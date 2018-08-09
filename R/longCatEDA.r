setClass('longCat',
         representation(   y                  = "matrix",
                           y.sorted           = "matrix",
                           dim                = "integer",
                           times              = "matrix",
                           times.sorted       = "matrix",
                           Labels             = "character",
                           factors            = "numeric",
                           IndTime            = "logical",
                           nfactors           = "integer",
                           sorted             = "logical",
                           ascending          = "logical",
                           group              = "matrix",
                           group.sorted       = "matrix",
                           groupLabels        = "character",
                           order.y            = "matrix",
                           order.y.sorted     = "matrix",
                           events             = "matrix",
                           event.times        = "matrix",
                           events.sorted      = "matrix",
                           event.times.sorted = "matrix",
                           eventLables        = "character")
)
setMethod("summary",
          signature(object = "longCat"),
          definition = function (object, ...)
          {
            if( object$sorted) temp <- object[c(3,8:15)]
            if(!object$sorted) temp <- object[c(3,8:12 )]
            temp$group <- table(temp$group)
            print(temp)
          }
)

alignTime <- function(y, times,
                      which.state=NULL, nth.state=NULL, not.state=FALSE,
                      events=NULL, event.times=NULL,
                      which.event=NULL, nth.event=NULL, not.event=FALSE)
{
  dimCheck <- nrow(y)==nrow(times) & ncol(times)==(ncol(y)+1)
  if(!dimCheck)
  {
    stop("The dimension of times does not equal c(nrow(y), ncol(y)+1)")
  }
  if( ( is.null(which.state) &  is.null(which.event)) |
      (!is.null(which.state) & !is.null(which.event)) )
  {
    stop('One of which.state or which.event must be given, and the other must be NULL')
  }
  if( ( is.null(nth.state) &  is.null(nth.event)) |
      (!is.null(nth.state) & !is.null(nth.event)) )
  {
    stop('One of nth.state or nth.event must be given, and the other must be NULL')
  }
  if( ( is.null(which.state) & !is.null(nth.state)) |
      (!is.null(which.state) &  is.null(nth.state)) )
  {
    stop('Both which.state and nth.state must be given')
  }
  if( ( is.null(which.event) & !is.null(nth.event)) |
      (!is.null(which.event) &  is.null(nth.event)) )
  {
    stop('Both which.event and nth.event must be given')
  }
  ### internal function for finding the n'th value in a vector
  #   w=which value
  #   n=nth value
  #   np1=(n) (p)lus 1, do for y/times but not for events
  #   notw=!w
  # if w is not in x at least n times, return the final position + np1
  # if all of x is missing, the first position
  getN <- function(x, w, n, np1=1, notw=FALSE)
  {
    if(!notw[1]) wn <- which(x==w)[n]
    if( notw[1]) wn <- which(x!=w)[n]
    if(length(wn)==0 | is.na(wn)) wn <- sum(!is.na(x)) + np1
    if(all(is.na(x))) wn <- 1
    wn
  }
  ### test cases
  #x <- c(1,2,1,3,3,4)
  #getN(x, 1, 1)
  #getN(x, 1, 2)
  #getN(x, 2, 1)
  #getN(x, 2, 2)
  #getN(x, 3, 1)
  #getN(x, 3, 2)
  #getN(x, 3, 3)
  #x <- c(1,2,1,3,NA,4)
  #getN(x, 3, 1)
  #getN(x, 3, 2)
  #x <- rep(NA,10)
  #getN(x, 1, 2)
  #getN(x, 5, 1)

  ### find the max of available times for each case
  maxna <- function(x)
  {
    if(all(is.na(x))) return(NA)
    else return(max(x, na.rm=TRUE))
  }
  max.times  <- apply(times, 1, maxna)
  if(!is.null(event.times))
  {
    max.event.times <- apply(event.times, 1, maxna)
    max.times <- apply(cbind(max.times, max.event.times), 1, maxna)
  }

  ### make aligned event times null by default
  aligned.event.times = NULL

  ### find the n'th event for each case
  if(!is.null(which.state))
  {
    if(!not.state) has.nth.state <- apply(y==which.state, 1, sum, na.rm=TRUE)>=nth.state
    if( not.state) has.nth.state <- apply(y!=which.state, 1, sum, na.rm=TRUE)>=nth.state
    nth.states <- apply(y, 1, getN, w=which.state, n=nth.state, np1=1, notw=not.state)
    # get the time where which.state occurs for the nth.state time
    nth.times  <- times[cbind(seq_along(nth.states), nth.states)]
    # for those who !has.nth.state, use the max of all time values
    nth.times[!has.nth.state] <- max.times[!has.nth.state]
    # subtract out nth.times
    aligned.times <- times - nth.times
    aligned.event.times <- event.times - nth.times
  }
  ### find the n'th event for each case
  if(!is.null(which.event))
  {
    if(!not.event) has.nth.event <- apply(events==which.event, 1, sum, na.rm=TRUE)>=nth.event
    if( not.event) has.nth.event <- apply(events!=which.event, 1, sum, na.rm=TRUE)>=nth.event
    nth.events <- apply(events, 1, getN, w=which.event, n=nth.event, np1=0, notw=not.event)
    # get the time where which.event occurs for the nth.event time
    nth.times  <- event.times[cbind(seq_along(nth.events), nth.events)]
    # for those who !has.nth.event, use the max of all time values
    nth.times[!has.nth.event] <- max.times[!has.nth.event]
    # subtract out nth.times
    aligned.times <- times - nth.times
    aligned.event.times <- event.times - nth.times
  }
  list(aligned.times=aligned.times,
       aligned.event.times=aligned.event.times)
}


longContPlot <- function(y, times=NULL, jog=FALSE, ylim=NULL, xlim=NULL, ...)
{
  # check the inputs and set graphing parameters
  if( is.null(ylim) ){ ylim=range(y, na.rm=T) }
  if( is.null(times) )
  {
    txx <- 1:ncol(y)
    xlim=range(txx)
    times <- data.frame( matrix(txx, 1, ncol(y)) )
  }
  if( !is.null(times) & is.null(dim(times)) )
  {
    if(length(times)==(ncol(y)+1))
    {
      times <- matrix(times[1:(length(times)-1)], nrow(y), ncol(y), byrow=TRUE)
    }
    if(length(times)==ncol(y))
    {
      times <- matrix(times, nrow(y), ncol(y), byrow=TRUE)
    }
  }
  if( !is.null(times) & !is.null(dim(times)) )
  {
    xlim <- range(times, na.rm=TRUE)
  }
  if( all( dim(y)==dim(times) ) & !is.null(times) ){ txx <- times[1,] }
  if( any( dim(y)!=dim(times) ) & !is.null(times) ){ txx <- times[1,] }
  if( !is.null(times) & is.null(xlim) ){ xlim=range(as.numeric(times)) }
  if(  is.null(times) & is.null(xlim) ){ xlim=c(1,(ncol(y)-1)) }

  if(jog)
  {
    j <- matrix( runif(nrow(y), -.25, .25), nrow(y), ncol(y) )
    y <- y + j
  }

  #
  y   <- as.matrix(y)
  txx <- unlist(c(txx))

  # initiate a blank plot
  plot( txx, y[1,], col='transparent', ylim=ylim, xlim=xlim, type='n', ...)

  # loop through subjects adding them to the plot
  if( any( dim(y)!=dim(times) ) )
  {
    for(r in 1:nrow(y)){ lines(txx, y[r,])  }
  }
  if( all( dim(y)==dim(times) ) )
  {
    for(r in 1:nrow(y))
    {
      txx <- times[r,]
      lines(txx, y[r,])
    }
  }
}

lunique <- function(y)
{
  u <- unique(y)
  sum(!is.na(u))
}
levelCheck <- function(y)
{
  lu <- apply(y, 2, lunique)
  maxlu <- max(lu)
  if( maxlu > 9 )
  {
    warning('One or more variables in y has 10 or more categories.\nConsider using continuous methods\nsuch as longContPlot().')
  }
  # determine unique values
  factors <- as.numeric(levels(factor(unlist(y))))
  return( factors )
}

longCat <- function(y, times=NULL, Labels=NULL, tLabels=NULL,
                    id=NULL, events=NULL,
                    event.times=NULL, eventLabels=NULL)
{
  # convert y frame to matrix and character to numeric
  if(is.factor(y[,1]))
  {
    if(is.null(Labels)) Labels <- levels(unlist(c(y)))
  }
  y <- data.matrix(y)

  # check the times input, that it is monotonically increasing, and force to data.frame
  IndTime <- NULL
  if( is.null(times) )
  {
    IndTime <- FALSE
    times   <- data.frame( matrix(0:ncol(y), nrow(y), (1+ncol(y)), byrow=TRUE) )
  }
  if(!is.null(times) )
  {
    # if times is a vector
    if( is.null(dim(times)) )
    {
      IndTime <- FALSE
      if(length(times)!=(ncol(y)+1)) stop('times must be of length ncol(y)+1')
      if(!all(times==cummax(times))) stop('times must be monotonically increasing')
      times <- data.frame( matrix(times, nrow(y), (1+ncol(y)), byrow=TRUE) )
    }
    # if times is a matrix
    dimCheck <- nrow(y)==nrow(times) & ncol(times)==(ncol(y)+1)
    if(dimCheck)
    {
      if(is.null(IndTime)) IndTime <- TRUE
      times <- as.matrix(times)
      cummax2 <- function(x)
	  {
	    x <- x[!is.na(x)]
	    all(x==cummax(x))
	  }
	  if(!all( apply(times, 1, cummax2) ))
	  {
	    stop('times must be monotonically increasing')
	  }
    }
    if(!dimCheck)
    {
      stop("The dimension of times does not equal c(nrow(y), ncol(y)+1)")
    }
  }

  # check id
  if( !is.null(id) & nrow(y) != length(id) )
  {
    stop('The number of IDs length(id) does not match the number of rows in y nrow(y)')
  }

  # create the order.y matrix
  order.y <- as.data.frame( matrix(NA, nrow(y), 4) )
  # the following line is retained for reference, but not used
  #colnames(order.y) <- c("id", "pattern", "customSort", "o")
  if( !is.null(id) ) order.y[,1] <- id
  if(  is.null(id) ) order.y[,1] <- 1:nrow(y)

  # rescale inputs to positive sequential integers from 1 to the maximum
  # number of categories
  u <- unique(c(y))
  u <- u[ !is.na(u) ]
  u <- u[order(u)]
  if( !all(u == 1:length(u)) )
  {
    temp <- y
    for(i in 1:length(u))
    {
      temp[y==u[i]] <- i
    }
    y <- temp; rm(temp)
  }
  rm(u)

  # check the levels using levelCheck()
  factors <- levelCheck(y)

  # create Labels if not provided
  if(is.null(Labels)){ Labels <- factors }

  # count the number of factors returned by leveCheck()
  nfactors <- length(factors)

  # check that Labels, nfactors, tLabels, and times conform
  if( length(Labels) != nfactors )
  {
    warning(paste('The number of labels in Labels does not equal the\n',
                  'number of unique values in the y set.'))
  }
  if( !is.null(tLabels) & ncol(y) != length(tLabels) )
  {
    warning(paste('The number of labels in tLabels does not equal the\n',
                  'number of unique values in times.'))
  }

  # if individually varying times of observation are given, make tLabels NULL
  if( (length(unique(times))-1) != ncol(y) ) tLabels <- NULL

  # if events are provided, event times must also be provided
  if(!is.null(events))
  {
    if(nrow(events)!=nrow(y)) stop("events must be a matrix with as many rows as y")
    if(ncol(events)!=ncol(event.times)) stop("events and event.times must be matrices (or data.frames) of equal dimension")
    if(nrow(events)!=nrow(event.times)) stop("events and event.times must be matrices (or data.frames) of equal dimension")

          events <- as.matrix(events)
    event.times  <- as.matrix(event.times)
    if(is.null(eventLabels))
    {
      eventLabels <- unique(c(events))
      eventLabels <- eventLabels[!is.na(eventLabels)]
    }

    # rescale to positive consecutive integers starting at 1
    u <- unique(c(events))
    u <- u[ !is.na(u) ]
    u <- u[order(u)]
    if( !all(u == 1:length(u)) )
    {
      temp <- events
      for(i in 1:length(u))
      {
        temp[events==u[i]] <- i
      }
      events <- temp; rm(temp)
    }
    rm(u)
  }


  # apply class and return output
  lc =  list(y=y,
             y.sorted=NULL,
             dim=dim(y),
             times=times,
             times.sorted=NULL,
             Labels=Labels,
             tLabels=tLabels,
             factors=factors,
             IndTime=IndTime,
             nfactors=nfactors,
             sorted=FALSE,
             ascending = NULL,
             group = NULL,
             group.sorted = NULL,
             groupLabels = NULL,
             order.y = order.y,
             order.y.sorted = NULL,
             events = events,
             event.times = event.times,
             events.sorted = NULL,
             event.times.sorted = NULL,
             eventLabels = eventLabels)
  class(lc) = 'longCat'
  return(lc)
}

norpt <- function( alist = c(1,2,2,3,3,3,4,4,4,4,5) )
{
  outlist <- alist[1]
  for(i in 2:length(alist))
  {
    if( !is.na(alist[i-1]) & !is.na(alist[i]) )
    {
      if(alist[i-1] != alist[i]){ outlist <- c(outlist, alist[i]) }
    }
  }
  outlist <- c(outlist, rep(NA, (length(alist)-length(outlist))))
  outlist
}
makePatterns <- function(dat, times=NULL, num=TRUE, mindur=NULL, igrpt=FALSE)
{
  # set times if null
  if( is.null(times) ) times <- 1:ncol(dat)
  # reduce the effect of short durations
  if(!is.null(mindur) & length(dim(times))==2)
  {
    times <- times - times[,1]
    mintime <- times <= mindur & !is.na(times)
    dat[mintime] <- NA
  }
  # if desired, (ig)nore (r)e(p)ea(t) observations,
  #   i.e., c(1, 2, 2, 3) becomes c(1, 2, 3)
  if(igrpt){ dat <- t( apply(dat, 1, norpt) )  }
  # concatenate rows into a string
  out <- apply(dat, 1, paste, collapse="")
  # if desired, turn to numeric and reduce the scale
  if(num)
  {
    g    <- gsub('NA', '', out)
    nc   <- nchar(g)
    tens <- 10^nc
    out  <- ( as.numeric(g)/tens )*10
  }
  as.matrix(out, nrow(dat), 1)
}
sort1  <- function(id1, y1, times1, events1, event.times1, group1,
                   ascending=TRUE, whichColumns=NULL,
				           initFirst=FALSE)
{

  if( initFirst) init <- y1[,1]
  if(!initFirst) init <- rep(1, nrow(y1))

  o <- order(init, id1[,3], id1[,2], decreasing = !ascending)

  id.s          <- id1[o,]
  y.s           <- y1[o,]
  times.s       <- times1[o,]
  if(!is.null(events1))
  {
    events.s      <- events1[o,]
    event.times.s <- event.times1[o,]
  }
  if( is.null(events1) )
  {
    events.s      <- NULL
    event.times.s <- NULL
  }
  if( !is.null(group1) ) group.s <- matrix( group1[o] )
  if(  is.null(group1) ) group.s <- NULL

  # vector to matrix
  vtom <- function(x)
  {
    if(!is.null(x))
    {
      if(is.null(dim(x))) x <- t(as.matrix(x))
      if(!is.null(dim(x)))
      {
        if(ncol(x)==1 & nrow(x)>1)  x <- t(as.matrix(x))
      }
    }
    return(x)
  }
  y.s           <- vtom(y.s          )
  times.s       <- vtom(times.s      )
  events.s      <- vtom(events.s     )
  event.times.s <- vtom(event.times.s)
  return(list(id.s          = id.s,
              y.s           = y.s,
              times.s       = times.s,
              events.s      = events.s,
              event.times.s = event.times.s,
              group.s       = group.s))

}

sorter <- function(lc,
                   ascending    = TRUE  ,
                   whichColumns = NULL  ,
                   num          = TRUE  ,
                   mindur       = NULL  ,
                   igrpt        = FALSE ,
                   customSort   = NULL  ,
                   initFirst    = FALSE ,
                   group        = NULL  ,
                   groupLabels  = NULL  ,
                   ggap         = NULL  )
{
  # check ggap
  if( !is.null(ggap) )
  {
    if(ggap<0 | ggap>1) stop("ggap must be in [0,1]")
    ggap=ceiling(.05*lc$dim[1])
  }

  # make group numeric, extracting labels first if not given
  if(is.character(group)) group <- as.factor(group)
  if(is.factor(group))
  {
    if(is.null(groupLabels)) groupLabels <- levels(group)
    group <- as.numeric(group)
  }

  # if object is already sorted, don't attempt to resort
  if(lc$sorted)
  {
    sortWarn <- paste('lc object already sorted, use the following approach\n',
                      'lc <- (y)\n',
                      'lc_sort1 <- sorter(lc, group=g1)\n',
                      'lc_sort2 <- sorter(lc, group=g2)\n',
                      'lc_customSort <- sorter(lc, customSort=cs)\n'
    )
    stop(sortWarn)
  }

  # if customSort contains missing y, stop
  if( !is.null(customSort) )
  {
    if( any(is.na(customSort)) )
    {
      stp <- paste('\ncustomSort cannot contain missing values \n',
                   'consider trying one of:\n',
                   '1. Subsetting the y first\n',
                   '2. Setting missing to an extreme value and adding a group variable, such as\n',
                   '     lc <- longCat(y)\n',
                   '     customSort[is.na(customSort)] <- -99\n',
                   '     group <- customSort==-99\n',
                   "     lc_customSort <- sorter(lc, group=group, \n",
                   "          groupLabels=c('nonMissing customSort', 'Missing customSort'),\n",
                   "          customSort=customSort\n")
      stop(stp)
    }
  }

  # check for missing values on the grouping variable and get the number of groups
  if( !is.null(group) )
  {
    if( any(is.na(group)) )
    {
      ndeleted   <- sum(is.na(group))
      w <- paste(ndeleted,
                 ' row(s) with missing membership on the group variable\n',
                 'have been deleted.\n\n',
                 'If a large number of cases have missing y on the\n',
                 'group variable, consider recoding the missings into\n',
                 'their own group, e.g.:\n\n',
                 '     group[is.na(group)] <- -999 \n\n',
                 'and add a missing label to groupLabels, e.g.:\n\n',
                 "     groupLabels=c('Missing', 'Group1', 'Group2', 'Etc.')\n",
                 sep='')
      warning(w)
    }
    ngroups <- length(table(group))
    if(is.null(ggap)) ggap=ceiling(.05*lc$dim[1])
  }
  if( is.null(group) )
  {
    group <- rep(1, lc$dim[1])
    ngroups  <- 1
    if(is.null(ggap)) ggap=0
  }
  if(is.null(ggap)) ggap=ceiling(ggap*lc$dim[1])
  u.g <- names(table(group))

  # check inputs and set additional sorting parameters
  if(is.null(whichColumns)) whichColumns <- 1:lc$dim[2]
  if(is.null(customSort))   customSort   <- rep(1, lc$dim[1])

  # concatinate the data
  lc$order.y[,2] <- makePatterns(lc$y[,whichColumns], lc$times[,whichColumns],
                                 num, mindur, igrpt)
  lc$order.y[,3] <- customSort

  # split the data into groups and sort within group
  id.g.l <- y.g.l <- times.g.l <- events.g.l <- event.times.g.l <-
    group.g.l <- vector('list', ngroups)
  for(g in 1:ngroups)
  {
    # extract group specific data
    # extract group specific data
    w.g     <- group==u.g[g]
    id.g    <- lc$order.y[w.g,]
    y.g     <- lc$y[w.g,]
    times.g <- lc$times[w.g,]

    if(!is.null(lc$events))
    {
      events.g      <- lc$events[w.g,]
      event.times.g <- lc$event.times[w.g,]

      # if a one column events matrix is provided, the above row extraction
      # results in vectors instead of matrices
      if(!class(events.g)      %in% 'matrix') events.g      <- as.matrix(events.g)
      if(!class(event.times.g) %in% 'matrix') event.times.g <- as.matrix(event.times.g)
    }
    if( is.null(lc$events))
    {
      events.g      <- NULL
      event.times.g <- NULL
    }
    group.g <- group[w.g]

    # vector to matrix
    vtom <- function(x)
    {
      if(!is.null(x))
      {
        if(is.null(dim(x))) x <- t(as.matrix(x))
        if(!is.null(dim(x)))
        {
          if(ncol(x)==1 & nrow(x)>1)  x <- t(as.matrix(x))
        }
      }
      return(x)
    }
    y.g           <- vtom(y.g)
    times.g       <- vtom(times.g)
    events.g      <- vtom(events.g)
    event.times.g <- vtom(event.times.g)

    # sort
    sorted.dat <- sort1(id1          = id.g          ,
                        y1           = y.g           ,
                        times1       = times.g       ,
                        events1      = events.g      ,
                        event.times1 = event.times.g ,
                        group1       = group.g       ,
                        ascending    = ascending     ,
                        whichColumns = whichColumns  ,
                        initFirst    = initFirst     )

    # populate storage lists
    id.g.l[[g]]          <- sorted.dat$id.s
    y.g.l[[g]]           <- sorted.dat$y.s
    times.g.l[[g]]       <- sorted.dat$times.s
    events.g.l[[g]]      <- sorted.dat$events.s
    event.times.g.l[[g]] <- sorted.dat$event.times.s
    group.g.l[[g]]       <- sorted.dat$group.s

  	# add empty lines if ngroups > 1
  	if(ngroups>1)
  	{
  	  dat.id    <- data.frame(id.g.l[[g]])
  	  dat.y     <- data.frame(y.g.l[[g]])
  	  dat.times <- data.frame(times.g.l[[g]])
  	  dat.group <- data.frame(group.g.l[[g]])

  	  na.id    <- data.frame(matrix(NA, ggap, ncol(id.g.l[[g]]   )))
  	  na.y     <- data.frame(matrix(NA, ggap, ncol(y.g.l[[g]]    )))
  	  na.times <- data.frame(matrix(NA, ggap, ncol(times.g.l[[g]])))
  	  na.group <- data.frame(matrix(NA, ggap, 1))

  	  names(na.id   ) <- names(dat.id   )
  	  names(na.y    ) <- names(dat.y    )
  	  names(na.times) <- names(dat.times)
  	  names(na.group) <- names(dat.group)

  	  id.g.l[[g]]          <- rbind(do.call(data.frame, dat.id), na.id)#rbind(dat.id    , na.id   )
  		y.g.l[[g]]           <- rbind(dat.y     , na.y    )
  		times.g.l[[g]]       <- rbind(dat.times , na.times)
  		group.g.l[[g]]       <- rbind(dat.group , na.group)
  		if(!is.null(lc$events))
  		{
  			events.g.l[[g]]      <- rbind(events.g.l[[g]]     , matrix(NA, ggap, ncol(events.g.l[[g]]     )))
  			event.times.g.l[[g]] <- rbind(event.times.g.l[[g]], matrix(NA, ggap, ncol(event.times.g.l[[g]])))
  		}
  	}

    # rm objects
    rm(w.g, id.g, y.g, times.g, sorted.dat)
    if(!is.null(lc$events)) rm(events.g, event.times.g)
    if(!is.null(group))     rm(group.g)
  }

  # restack the data
  order.y.sorted <- do.call(rbind, id.g.l)
  y.sorted <- do.call(rbind, y.g.l)
  times.sorted <- do.call(rbind, times.g.l)
  if(!is.null(lc$events))
  {
    events.sorted      <- do.call(rbind, events.g.l)
    event.times.sorted <- do.call(rbind, event.times.g.l)

    if(nrow(lc$events)!=nrow(events.sorted) & nrow(events.sorted)==1)
    {
      events.sorted <- t(events.sorted)
      event.times.sorted <- t(event.times.sorted)
    }
  }
  if( is.null(lc$events))
  {
    events.sorted      <- NULL
    event.times.sorted <- NULL
  }
  group.sorted <- do.call(rbind, group.g.l)
  rm(id.g.l, y.g.l, times.g.l, events.g.l, event.times.g.l, group.g.l)

  # return modified lc object
  lc = list( y = lc$y,
             y.sorted = y.sorted,
             dim = lc$dim,
             times = lc$times,
             times.sorted = times.sorted,
             Labels = lc$Labels,
             tLabels = lc$tLabels,
             factors = lc$factors,
             IndTime = lc$IndTime,
             nfactors = lc$nfactors,
             sorted = TRUE,
             ascending = ascending,
             group = group,
             group.sorted = group.sorted,
             groupLabels = groupLabels,
             order.y = lc$order.y,
             order.y.sorted = order.y.sorted,
             events = lc$events,
             event.times = lc$event.times,
             events.sorted = events.sorted,
             event.times.sorted = event.times.sorted,
             eventLabels = lc$eventLabels)
  class(lc) = 'longCat'
  return(lc)
}

clean.events <- function(events, event.times)
{
  nc <- ncol(events)
  for(i in 1:nrow(events))
  {
    w <- !is.na(events[i,])
    temp.events <- events[i,w]
    temp.event.times <- event.times[i,w]
    nas <- rep(NA, (nc-length(temp.events)))
    events[i,] <- c(temp.events, nas)
    event.times[i,] <- c(temp.event.times, nas)
    rm(w, temp.events, temp.event.times, nas)
  }
  rm(nc)
  list(events=events, event.times=event.times)
}


colChoose <- function(colScheme, nfactors, reverse=FALSE)
{
  if(colScheme=='gray'){cols <- gray((nfactors-1):0/nfactors)}
  if(colScheme=='rainbow'){cols <- rainbow(nfactors)}
  if(colScheme=='heat'){cols <- heat.colors(nfactors, alpha = 1)[nfactors:1]}
  if(colScheme=='terrain'){cols <- terrain.colors(nfactors, alpha = 1)}
  if(colScheme=='topo'){cols <- topo.colors(nfactors, alpha = 1)}
  if(colScheme=='cm'){cols <- cm.colors(nfactors, alpha = 1)}
  # some finessing to make sure contrast is sufficient when nFactors < 9
  if(reverse) cols <- cols[nfactors:1]
  return(cols)
}

longCatPlot <- function(lc,
                        xlab            =  "Days" ,
                        ylab            =  NULL   ,
                        cols            =  NULL   ,
                        colScheme       =  'heat' ,
                        reverse         =  FALSE  ,
                        lwd             =  .5     ,
                        lcex            =  1      ,
                        llwd            =  3      ,
                        legendBuffer    =  .12    ,
                        groupBuffer     =  0      ,
                        groupRotation   =  90     ,
                        gcex            =  1      ,
                        seg.len         =  1      ,
                        xlas            =  0      ,
                        xcex            =  1      ,
                        ecex            =  .5     ,
                        event.col       =  1      ,
                        plot.events     =  TRUE   ,
                        which.events    =  NULL   ,
                        n.events        =  NULL   ,
                        event.pch       =  NULL   ,
                        texclude        =  NULL   ,
                        sort            =  TRUE   ,
                        which.state     =  NULL   ,
                        nth.state       =  NULL   ,
                        not.state       =  FALSE  ,
                        which.event     =  NULL   ,
                        nth.event       =  NULL   ,
                        not.event       =  FALSE  ,
                        ...)
{
  if( is(lc) != 'longCat' ){ stop('longCatPlot requires an object of class longCat.')  }
  if(is.null(cols)){ cols <- colChoose(colScheme, lc$nfactors, reverse) }
  if(legendBuffer < 0 | legendBuffer > 1){stop('legendBuffer must be in [0,1]')}
  if(groupBuffer < 0 | groupBuffer > 1){stop('groupBuffer must be in [0,1]')}
  if(is.null(ylab)) ylab = paste("Each Line Represents a Participant, N =", lc$dim[1])
  if(!is.null(lc$events))
  {
    if(ncol(lc$events)==1) # override if the user has one column event matrix
    {
      which.events=NULL
      n.events=NULL
    }
  }

  # on the fly sorting
  if(sort==TRUE)
  {
    if( !lc$sorted & ( any(is.na(lc$y)) |  lc$IndTime) ) lc <- sorter(lc, num=TRUE)
    if( !lc$sorted &  !any(is.na(lc$y)) & !lc$IndTime  ) lc <- sorter(lc, num=FALSE)
  }

  # populate empty objects to avoid if checks below
  if(is.null(lc$y.sorted          )) lc$y.sorted           = lc$y
  if(is.null(lc$times.sorted      )) lc$times.sorted       = lc$times
  if(is.null(lc$events.sorted     )) lc$events.sorted      = lc$events
  if(is.null(lc$event.times.sorted)) lc$event.times.sorted = lc$event.times

  # check whether a subset of events is wanted
  if(plot.events & !is.null(lc$events))
  {
    u.events <- unique( unlist(c(lc$events.sorted)) )
    u.events <- u.events[!is.na(u.events)]
    if(!is.null(which.events))
    {
      if(!all(which.events%in%u.events))
      {
        stop('which.events contains values not in the events matrix')
      }
      if( all(which.events%in%u.events))
      {
        not.events <- u.events[!u.events%in%which.events]
        for(i in 1:length(not.events))
        {
          lc$events.sorted[lc$events.sorted==not.events[i]] <- NA
        }
        rm(not.events)
      }
    }
    rm(u.events)
    # clean the events by removing NAs and collapsing
    temp.events <- clean.events(lc$events.sorted, lc$event.times.sorted)
    lc$events.sorted <- temp.events$events
    lc$event.times.sorted <- temp.events$event.times
    rm(temp.events)
  }

  # restrict range if requested
  if(!is.null(texclude))
  {
    if(length(texclude)!=2) stop("texclude must be a vector of length 2")
    lc$times.sorted[lc$times.sorted<texclude[1]] <- texclude[1]
    lc$times[lc$times<texclude[1]] <- texclude[1]
    lc$times.sorted[lc$times.sorted>texclude[2]] <- texclude[2]
    lc$times[lc$times>texclude[2]] <- texclude[2]
    if(!is.null(lc$event.times.sorted))
    {
      lc$event.times.sorted[lc$event.times.sorted<texclude[1]] <- NA
      lc$event.times.sorted[lc$event.times.sorted>texclude[2]] <- NA
    }
  }

  # apply time alignment if any alignment options are non-NULL
  if(!is.null(which.state) |
     !is.null(nth.state  ) |
     !is.null(which.event) |
     !is.null(nth.event  ) )
  {
    temp.times <- alignTime(y=lc$y.sorted,
                            times=lc$times.sorted,
                            which.state=which.state,
                            nth.state=nth.state,
                            not.state=not.state,
                            events=lc$events.sorted,
                            event.times=lc$event.times.sorted,
                            which.event=which.event,
                            nth.event=nth.event,
                            not.event=not.event)
    lc$times.sorted <- temp.times$aligned.times
    lc$event.times.sorted <- temp.times$aligned.event.times
    rm(temp.times)
  }

  # restrict to the first n events if requested
  if(!is.null(n.events))
  {
    if(!is.numeric(n.events)) stop('n.events must be numeric')
    if(n.events > ncol(lc$events.sorted)) stop('n.events cannot exceed ncol(lc$events.sorted)')
    lc$events.sorted[,(n.events+1):ncol(lc$events.sorted)] <- NA
  }

  # set up plot, pre-allocating a region for labels using ymax
  lo = min(lc$times.sorted, na.rm=T)
  up = max(lc$times.sorted, na.rm=T)
  xrange = lo:up

  # reps is used to automatically scale the x-axis
  reps = nrow(lc$y.sorted)-length(xrange)
  # fix reps if negative, will occur when the number of cases is fewer than
  # the number of time points. This happens when plotting a small # of cases
  if( reps < 0 ) reps <- 0

  # set additional plotting parameters
  xbuffer <- .25*mean(xrange[2:length(xrange)]-xrange[1:(length(xrange)-1)])
  groupBuffer <- ceiling( groupBuffer*up )
  tx <- c(lo-.5, xrange, rep( lo, reps), up+.5 )
  if( !is.null(lc$group) ){ tx[1] <- tx[1] - groupBuffer*xbuffer }
  ymax <- nrow(lc$y.sorted) + ceiling( legendBuffer*nrow(lc$y.sorted) )

  # initiate the empty plot
  plot(tx,y=rep(NA,length(tx)),col='transparent', ylim=c(0,ymax),
       xlab=xlab, ylab='', axes=FALSE, ...)
  if(groupBuffer >0 | !is.null(lc$group)) title(ylab=ylab, mgp=c(1   ,1,0))
  if(groupBuffer==0 &  is.null(lc$group)) title(ylab=ylab, mgp=c(0.25,1,0))

  # add axes
  tat <- unique(lc$times)
  tat <- tat[1:(length(tat)-1)]
  if(!is.null(lc$tLabels) ) axis( 1, at = tat,
                                 labels=lc$tLabels, las=xlas, cex.axis=xcex )
  if( is.null(lc$tLabels) ) axis( 1, at = NULL )

  # plot loops
  for(r in nrow(lc$y.sorted):1) # loop over cases
  {
    # select plotting y for the r^th case
    pdat  <- as.numeric(lc$y.sorted[r,])
    txx   <- as.numeric(lc$times.sorted[r,])
    tempy <- rep(r,2)
    # loop over observations
    for(j in 1:length(pdat))
    {
      # if observation is NA, skip
      if( !is.na( pdat[j] ) )
      {
        tempx <- c(txx[j], txx[j+1])
        # horizontal line plot
        lines(tempx, tempy, lwd=lwd, col=cols[ as.numeric(pdat[j]) ] )
      }
    }
  }

  # if event.pch are given, convert lc$events.sorted
  if(!is.null(event.pch))
  {
    u.events  <- unique( unlist(c(lc$events.sorted)) )
    u.events  <- sort(u.events[!is.na(u.events)])
    event.pch <- event.pch[1:length(u.events)]
    for(i in 1:length(event.pch))
    {
      lc$events.sorted[lc$events.sorted==u.events[i]] <- event.pch[i]
    }
  }

  # plot event points
  if(!is.null(lc$events) & plot.events)
  {
    for(i in 1:ncol(lc$events))
    {
      points(lc$event.times.sorted[,i], 1:nrow(lc$events.sorted),
             pch=lc$events.sorted[,i], cex=ecex, col=event.col)
    }
  }

  # add legend at top
  if(legendBuffer > 0)
  {
    legend(min(lc$times.sorted, na.rm=T), ymax + .1, legend=lc$Labels, lty=1,
           cex=lcex, col=cols, bty='n', lwd=llwd, seg.len=seg.len, horiz=T)
  }

  # if there is grouping add group labels
  if(!is.null(lc$group))
  {
    if( var(as.numeric(lc$group), na.rm=TRUE)!=0 )
    {
      #g <- data.frame(g=lc$group.sorted,
      #                gn=1:length(lc$group.sorted))
      g   <- as.numeric(unlist( lc$group.sorted ))
      gn  <- 1:length(g)
      gag <- aggregate(gn, list(group=g), mean, na.rm=T)
      u.g <- unique(g[!is.na(g)])
      for(i in 1:length(u.g))
      {
        text(tx[1], gag$x[i], labels = lc$groupLabels[i], srt=groupRotation, cex=gcex)
      }
    }
  }
  # return colors
  invisible(cols)
}
