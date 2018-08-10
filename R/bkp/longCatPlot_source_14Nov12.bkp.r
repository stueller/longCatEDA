################################################################################
# set class and summary method
################################################################################
setClass('longCat',
    representation(		data        = "matrix",
                      data.sorted = "matrix",
                      dim         = "integer",
                      times       = "matrix",
                      times.sorted= "matrix",
                      labels      = "character",
                      factors     = "numeric",
                      IndTime     = "logical",
                      nfactors    = "integer",
                      sorted      = "logical",
                      ascending	  = "logical",			   
                      group       = "matrix",
                      groupLabels = "character" ) 
)
setMethod("summary",
    signature(object = "longCat"),
    definition = function (object, ...) 
    {
      if( object$sorted) temp <- object[c(3,6:13)]
      if(!object$sorted) temp <- object[c(3,6:10 )]
      temp$group <- table(temp$group)
      print(temp)
    }
)

################################################################################
# longContPlot - used to illustrate the limitation of plotting
#   categorical longitudinal and time series data using continuous methods.
#   This plot function is appropriate for continuous data, but has limited
#   functionality and is included for illustrative purposes only.
#
# INPUTS
#   y - a matrix of longitudinal data where rows are cases/participtants and
#       columns are repeated observations
#   times - a vector of observation times. Individually varying times of
#       observation are not allowed, but this function could be generalized.
#   ylim, xlim - see ?par. Default is null and values are determined from the
#       data if not specified by the user
#   ... - additional options to be passed to the generic plot() function, e.g.,
#       ylab, xlab, main, etc.
################################################################################
longContPlot <- function(y, times=NULL, ylim=NULL, xlim=NULL, ...)
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
    times <- data.frame( matrix(times, 1, ncol(y)) ) 
  }
  if( all( dim(y)==dim(times) ) & !is.null(times) ){ txx <- times[1,] }
  if( any( dim(y)!=dim(times) ) & !is.null(times) ){ txx <- times[1,] }
  if( !is.null(times) & is.null(xlim) ){ xlim=range(times) }
  if(  is.null(times) & is.null(xlim) ){ xlim=c(1,ncol(y)) }
  
  # initiate a blank plot
  plot( unlist(txx), unlist(y[1,]), col='white', ylim=ylim, xlim=xlim, ...)
  
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
################################################################################
# Barplot - preps data and calls native are function barplot (note
#   lower case 'b'). See ?barplot. Note that this function is not appropriate
#   for date with individually varying times of observation. It implements the
#   approach used by Feldma, Masyn, & Conger (2009). See
#   http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2791967/
#
# INPUTS
#   y - a list containing data, times, and labels
#   cols, colScheme - see INPUTS section for longCatPlot()
#   xlab - time scale as a character expression; default is 'Day'
#   legx - x-axis position of the legend; default is null and will be determined
#       from the 'times' argument.
#   legy - y-axis position of the legend; default is .5
#   ... - other arguments passed to barplot(). Examples include ylab and main.
################################################################################
Barplot <- function(y, cols=NULL, colScheme=0,
                    xlab='Day', legx=NULL, legy=.5,  ...)
{
  # setting graphing parameters
  if(is.null(legx)){ legx=min(y$times)+1 }
  if(is.null(cols)){ cols <- colChoose(colScheme, y$nfactors) }
  
  # prep data for plotting
  uy <- unique(c(y$data))
  uy <- uy[ !is.na(uy) ]
  props <- matrix(NA,length(uy),ncol(y$data))
  for(i in 1:ncol(y$data))
  {
    props[,i] <- table( y$data[,i] )/sum( !is.na(y$data[,i]) )
  }
  
  # plot the data
  barplot(props, col=cols, names.arg=paste(xlab, y$times),
          legend.text = y$labels,
          args.legend=list(x=legx,y=legy,bg='white'), ...)
          
  # return the proportions
  return(props)
}
################################################################################
# Helper functions for longCat function
# 19Mar12 by Stephen Tueller
# stueller@rti.org
# function::lunique - called by levelCheck, returns number of unique 
#   non-missilong values; not used directly
# function::levelCheck - stops computation if > 9 levels for any column 
#   in the input data; users can apply levelCheck directly
# function::dimCheck - checks dimensions of data and times inputs and 
#   provides error messages if inputs do not conform
################################################################################
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
    stop('One or more variables in y has 10 or more categories')
  }
  # determine unique values
  factors <- as.numeric(levels(factor(unlist(y))))
  return( factors )
}
dimCheck <- function(y, times)
{
  dimErr1 <- "The dimension of times does not equal the dimension of y"
  dimErr2 <- "The length of times does not equal ncol(y)"
  ry <- nrow(y)
  cy <- ncol(y)
  rt <- nrow(times)
  ct <- ncol(times)

  if( rt>1 & rt!=ry ) stop(dimErr1) 
  if( cy!=ct ) stop(dimErr2)
  if( ry==rt & cy==ct )
  {
    IndTime <- T
  }
  if( rt==1 & cy==ct )
  {
    IndTime <- F
  }
  return(IndTime)
}
################################################################################
# longCat - 01May12 by Stephen Tueller stueller@rti.org
# function to prepare data for longCatPlot
#
# INPUTS
#   y - a matrix or df of observed categorical data; current version supports up
#       to nine levels, if more than this, an error is returned. See function
#       levelCheck. The program assumes y is numeric (or can be coerced to
#       numeric) and that missing values are represented by NA.
#   times - either a vector of length ncol(y) or a matrix of the same dimension
#       as y containing the times of observation. Default is NULL, and
#       1:ncol(y) is used for times.
#   Labels - a vector of up to length 9 containing labels for the factor
#       levels in y. No label is needed for missing values (NA). If not provided
#       numeric values will be used. Default is NULL.
#   tLabels - a vector of labels for the time values. This can only be used if
#       times is a vector of length ncol(y) - i.e., not use with individually
#       varying times of observation. This is useful with non longitudinal 
#       data (e.g., for looking at IRT or categorical CFA/IFA data) 
#
# OUTPUT
#   a list containing y, times, and Labels. y has been checked and recoded if
#   neccessary. times and Labels are checked if provided, if they are NULL they
#   are created. Null placeholders for sorting information are included, see
#   sorter(). Note that data are tacitly sorted in longCatPlot if sorter()
#   is not invoked by the user.
################################################################################
longCat <- function(y, times=NULL, Labels=NULL, tLabels=NULL)
{
  # convert data frame to matrix
  y <- as.matrix(y)
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
  
  # check the times input and force to data.frame
  if( is.null(times)){ times <- data.frame( matrix(1:ncol(y), 1, ncol(y)) ) }
  if(!is.null(times))
  { 
    if( is.null(dim(times))) times <- data.frame( matrix(times, 1, ncol(y)) )
    if(!is.null(dim(times))) times <- data.frame( times )  
  }
  
  # check the dimensions of the data (y) and the times input
  IndTime <- dimCheck(y, times)
  
  # create Labels if not provided
  if(is.null(Labels)){ Labels <- factors }
  
  # count the number of factors returned by leveCheck()
  nfactors <- length(factors)
  
  # check that Labels, nfactors, tLabels, and times conform
  if( length(Labels) != nfactors )
  {
    warning(paste('The number of labels in Labels does not equal the\n',
     'number of unique values in the data set.'))
  }
  if( !is.null(tLabels) & length(unique(times)) != length(tLabels)  )
  {
    warning(paste('The number of labels in tLabels does not equal the\n',
      'number of unique values in times.'))
  }
  
  # if individually varying times of observation, make sure times is a matrix
  if(!is.null(times) & IndTime) times <- as.matrix(times)

  # apply class and return output
  lc =  list(data=y,
        data.sorted=NULL,
        dim=dim(y),
        times=times,
		    times.sorted=NULL,
        labels=Labels,
        tLabels=tLabels,
        factors=factors,
        IndTime=IndTime,
        nfactors=nfactors,
        sorted=FALSE,
        ascending = NULL,					   
        group = NULL,
        groupLabels = NULL)
  class(lc) = 'longCat'
  return(lc) 
}

################################################################################
# sorter - 17Jul12 by Stephen Tueller - stueller@rti.org
#
# function::makePatterns (helper function for sorter)
#   INPUT
#     dat - a numeric matrix
#   OUTPUT
#     out - an nrow(dat) by 1 matrix where each row of dat is concatenated
#       into a single character
#
# function::sorter - function to sort data. Note that sorter is tacitly
#     used by longCatPlot if the user does not explicitly invoke sorter
#   INPUT
#     lc - a longCat object created by longCat()
#     ascending - if TRUE, data are sorted ascending, if FALSE, descending,
#       default is TRUE
#     whichColumns - if only a subset of lc$data are to be sorted on, these
#       columns can be listed, e.g., c(1:10,12,25)
#     num - parameter passed to makePatterns, return numeric version of 
#       patterns? This can drastically improve sorting especially when each 
#       case has a different number of observations. Default is TRUE. If FALSE
#       only the character version of the concatenated observations are 
#       return, one per subject in a vector
#     mindur - minimum duration to be considered for sorting. Used to reduce 
#       the effect of small individually varyring times of observation on
#       sorting. For example, if time is in days, and mindur=10, and duration
#       10 or fewer days will be ignored when sorting on the times matrix
#     customOrder - a vector with a user created ordering of variables. If
#       group is not null, customOrder should be ordered on group first, then
#       by the custom ordering within group. Users should understand the 
#       difference between ?order() and ?sort(). Pre-sorting the data is not
#       sufficient unless the user overrides the $sorted option, e.g.,
#
#         lc$sorted <- TRUE; lc$data.sorted <- lc$data
#
#     group - if sorting should be stratified by group, group is a vector of
#       group membership indicators of the same length and in the same order
#       as lc$data. Warning: no matching is done, it is up to the user to ensure
#       the vector entered for group is in the correct order. This becomes a
#       convenience duplicate lc with a different grouping variable. e.g.
#
#         lc  <- longCat(yourdata)
#         lc1 <- sorter(lc, group1)
#         lc2 <- sorter(lc, group2)
#
#       group is added to the lc object output by the sorter function
#     groupLabels - a list of labels for the group indicators in group. 
#       length(groupLabels)==length(unique(group)) must be true. Default is
#       null and the numeric values in unique(group) will be used
#     ggap - the number of empty rows placed in lc$data.sorted to give
#       visual differentiation between groups in longCatPlot. 
#   OUTPUT
#     an lc object now including non-null lc$data.sorted, sorting information, 
#     and, if supplied, grouping data 
################################################################################
makePatterns <- function(dat, times, num=TRUE, mindur=NULL)
{
  # first, reduce the effect of short durations
  if(!is.null(mindur) & length(dim(times))==2)
  {
    times <- times - times[,1]
    mintime <- times <= mindur & !is.na(times)
    dat[mintime] <- NA
  }
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
sorter <- function(lc, ascending=TRUE, whichColumns=NULL, num=TRUE, mindur=NULL, 
            customOrder=NULL, group=NULL, groupLabels=NULL, ggap=10)
{
  # check for missing values on the grouping variable
  if( !is.null(group) )
  {
    if( any(is.na(group)) ) 
    {
      lc$data <- lc$data[!is.na(group),]
      group <- group[!is.na(group)]
      w <- paste('WARNING: rOWS in ', quote(lc),
                 '$data with missing membership on group variable ', quote(group), 
                 ' have been deleted\n', sep='')
      cat(rep('*',40),'\n',w,rep('*',40),'\n')
    }
  }
  
  # check inputs and set additional sorting parameters
  if(is.null(whichColumns)) whichColumns = 1:ncol(lc$data)
  if( lc$IndTime) pats <- makePatterns(lc$data[,whichColumns], lc$times[,whichColumns], num, mindur)
  if(!lc$IndTime) pats <- makePatterns(lc$data[,whichColumns], NULL, num, mindur)
  if(lc$IndTime) tpat <- do.call(order, data.frame(lc$times[,whichColumns]) )
  if( is.null(group) & !lc$IndTime) o <- order(pats, decreasing = !ascending)
  if(!is.null(group) & !lc$IndTime) o <- order(group, pats, decreasing = !ascending)
  if( is.null(group) &  lc$IndTime) o <- order(pats, tpat, decreasing = !ascending)
  if(!is.null(group) &  lc$IndTime) o <- order(group, pats, tpat, decreasing = !ascending)
  if(!is.null(customOrder) &  is.null(group) ) o <- customOrder 
  if(!is.null(customOrder) & !is.null(group) ) o <- order(group, customOrder, decreasing = !ascending)
  data.sorted <- lc$data[o,]
  group <- group[o]
  if(lc$IndTime) times.sorted <- lc$times[o,]

  # check grouping parameters
  if( !is.null(group) )
  {
    if( nrow(as.matrix(group)) != nrow(lc$data) )
    {
      stop('group has a length that does not equal the number of rows in y')
    }
    group <- as.numeric(group)
    u <- unique(group)
    if( is.null(groupLabels) ) groupLabels <- paste('Group',u,sep='')
    if( length( u ) != length(groupLabels) )
    {
      stop(paste('The number of labels in groupLabels does not equal the\n',
        'number of unique values in the variable group.'))
    }
  }
  # if grouping/stratification is present, augment the data with empty rows
  # which will visually dilineate groups
  if( !is.null(group) )
  {
    temp <- vector('list', length(u) )
    gtemp <- vector('list', length(u) )
    ttemp <- vector('list', length(u) )
    blines <- matrix(NA, ggap, ncol(data.sorted))
    glines <- blines[,1]
    for(i in 1:length(u))
    {
      gdat <- data.sorted[group==u[i],]
      pats <- makePatterns(gdat[,whichColumns], NULL, num, mindur)
      o    <- order(pats, decreasing = !ascending)
      if(lc$IndTime)
      {
        tdat <- times.sorted[group==u[i],]
        pats <- makePatterns(gdat[,whichColumns], tdat[,whichColumns], num, mindur)
        tpat <- do.call(order, data.frame(tdat) )
        o    <- order(pats, tpat, decreasing = !ascending)
      }     
      temp[[i]] <- rbind(blines, gdat[o,]) 
      gtemp[[i]] <- as.matrix(c(glines, group[group==u[i]]))
      if(lc$IndTime) 
      {
        ttemp[[i]] <- rbind(blines, tdat[o,]  )
      }
    }
    data.sorted <- do.call(rbind, temp); rm(temp)
    group <- do.call(rbind, gtemp); rm(gtemp)
    if(lc$IndTime) lc$times <- do.call(rbind, ttemp); rm(ttemp)
  }                                          
  
  # return modified lc object
  if(!lc$IndTime) times.sorted = NULL
  
  lc = list( data = lc$data,
        data.sorted = data.sorted,
        dim = lc$dim,
        times = lc$times,
		times.sorted = times.sorted,
        labels = lc$labels,
        tLabels = lc$tLabels,
        factors = lc$factors,
        IndTime = lc$IndTime,
        nfactors = lc$nfactors,
        sorted = TRUE,
        ascending = ascending,			   
        group = group,
        groupLabels = groupLabels )
  class(lc) = 'longCat'
  return(lc)        
}

################################################################################
# function::colScheme - expandable bank of color schemes, currently
#   0 = 1:8 + 'darkgreen' (9==1, so you can't use 1:9)
#   1 = roygbiv
#   2 = orange to blue
#   3 = green to red
#   'gray' = a spectrum of greys
#   'oldheat' = a manual heat map spectrum
#   'coldheat' = a manual heat map starting with cold colors
#   'rainbow' see ?rainbow
#   'heat' see ?heat.colors
#   'terrain' see ?terrain.colors
#   'topo' see ?topo.colors
#    'cm' see ?cm.colors
#
#   in longCatPlot, cols can be supplied directly for user defined schemes
################################################################################
colChoose <- function(colScheme, nfactors, reverse=FALSE)
{
	if(colScheme==0) cols <- c(1:8, 'darkgreen')
  if(colScheme==1){
	cols <- c("darkred",
			  "darkorange",
			  "darkgoldenrod2",
			  "darkgreen",
			  "lightblue",
			  "darkblue",
			  "blueviolet",
			  "black",
			  "hotpink")}
	if(colScheme==2){
	cols <- c("orange4",
			  "orange",
			  "olivedrab",
			  "olivedrab1",
			  "mediumorchid4",
			  "mediumorchid1",
			  "royalblue4",
			  "royalblue1",
			  "black")}
	if(colScheme==3){
	cols <- c("forestgreen",
			  "green",
			  "deeppink4",
			  "hotpink1",
			  "chocolate4",
			  "darkorange",
			  "red4",
			  "orangered",
			  "black")}
  if(colScheme=='gray'){cols <- gray(seq(0,.9,len=nfactors))}
  if(colScheme=='oldheat'){
  cols <- c("purple4",
        "royalblue",
        "paleturquoise3",
        "palegreen",
        "yellow",
        "orange",
        "orangered",
        "maroon",
        "red4")
  }
  if(colScheme=='coldheat'){
  cols <- c("blue4",
        "dodgerblue2",
        "green4",
        "darkseagreen3",
        "yellow4",
        "tan1",
        "orange",
        "orangered",
        "red2")
  }
  if(colScheme=='rainbow'){cols <- rainbow(9)}
  if(colScheme=='heat'){cols <- heat.colors(9, alpha = 1)[9:1]}
  if(colScheme=='terrain'){cols <- terrain.colors(9, alpha = 1)}
  if(colScheme=='topo'){cols <- topo.colors(9, alpha = 1)}
  if(colScheme=='cm'){cols <- cm.colors(9, alpha = 1)}
             
  # some finessing to make sure contrast is sufficient when nFactors < 9
  if(reverse) cols <- cols[9:1]
  if( colScheme > 0 )
  {
    if(nfactors <= 5 & nfactors > 3) cols <- cols[c(1,3,5,7,9)]
    if(nfactors <= 3) cols <- cols[c(1,5,9)]
  }
	return(cols)
}
################################################################################
# longCatPlot - plotting function for categorical longitudinal data
# 19Mar12 by Stephen Tueller
# stueller@rti.org
#
# INPUTS
#   lc - an lc object created by longCat()
#   xlab - default is "Day"; can take on any arbitrary string value and
#       is used only as a label; see xlab under ?plot
#   ylab - default is "Each Line Represents a Participant"; see xlab
#   cols - a vector of length length(Labels). Can take on numeric or
#       character values. See col under ?par. R color charts with both numeric
#       and character can values can be found by searching the internet for
#       "#R color" without quotes.
#   colScheme - see options for helper function colChoose. Used only if
#       cols is NULL. The default is 'heat'.
#   lwd - the width of horizontal lines. See ?par. If the number of subjects is
#       large, lwd must compensate by getting smaller, otherwise lines will
#       overlap and obscure each other, especially in the presence of missing
#       data.
#   lcex - cex parameter for the legend, default 1, see ?legend
#   legendBuffer - a proportion of the plot to retain for legends, must be in
#       [0,1]. Note that the legend is very sensitive to the scaling of the
#       graphics device. Users are advised to maximize their device and rerun.
#       Default is .1 (i.e., 10% of the plot area retained for the legend)
#   groupBuffer - similar to legendBuffer, but for group labels on the left of
#       of the plot. Default is .25.
#   groupRotation - if groupLabels are long, rotation of the labels can be used
#       reduce the needed size of groupBuffer
#   ... - other graphing parameters (e.g., main) to be passed to plot,
#         see ?plot and ?par
################################################################################
longCatPlot <- function(lc, xlab="Day",
                ylab="Each Line Represents a Participant", cols=NULL,
                colScheme='heat', reverse=FALSE, lwd=.5, lcex=1, llwd=1, 
                legendBuffer=.1, groupBuffer=.25, groupRotation=90, ...)
{
  if(is.null(cols)){ cols <- colChoose(colScheme, lc$nfactors, reverse) }
  if(legendBuffer < 0 | legendBuffer > 1){stop('legendBuffer must be in [0,1]')}

  # on the fly sorting
  if( !lc$sorted ) lc <- sorter(lc)

  # set up plot, pre-allocating a region for labels using ymax
  lo = min(lc$times, na.rm=T)
  up = max(lc$times, na.rm=T)
  xrange = lo:up
  
  # reps is used to automatically scale the x-axis
  reps = nrow(lc$data.sorted)-length(xrange)    
  # fix reps if negative, will occur when the number of cases is fewer than
  # the number of time points. This happens when plotting an individual
  if( reps < 0 ) reps <- 0
  
  # set additional plotting parameters
  xbuffer <- .5*mean(xrange[2:length(xrange)]-xrange[1:(length(xrange)-1)])
  tx <- c(lo-.5, xrange, rep( lo, reps), up+.5 )
  if( !is.null(lc$group) ){ tx[1] <- tx[1] - groupBuffer*xbuffer }
  ymax <- nrow(lc$data.sorted) + ceiling( legendBuffer*nrow(lc$data.sorted) )
  
  # initiate the empty plot
  plot(tx,y=rep(NA,length(tx)),col='white',ylim=c(0,ymax), 
        xlab=xlab,ylab=ylab, axes=FALSE, ...)

  # add axes
  if(!is.null(lc$tLabels)) axis( 1, at = unique(lc$times), labels=lc$tLabels )
  if( is.null(lc$tLabels)) axis( 1, at = NULL )

  # plot loops
  for(r in 1:nrow(lc$data.sorted)) # loop over cases
  {
    # select plotting data for the r^th case
    pdat <- lc$data.sorted[r,]
    if( lc$IndTime & !lc$sorted) txx <- lc$times[r,]
	if( lc$IndTime &  lc$sorted) txx <- lc$times.sorted[r,]
    if(!lc$IndTime) txx <- lc$times
    tempy <- rep(r,2)
    for(j in 1:length(pdat)) # loop over observations
    {
       if( !is.na( pdat[j] ) ) # if observation is NA, skip
       {
         # define the x-values for any but the last segment
         if( j <length(pdat) )
         { 
           tempx <- c(txx[j]-xbuffer, txx[j+1]-xbuffer)
           # correct for missing endpoint
           if(is.na(txx[j+1])) tempx[2] <- txx[j]+xbuffer 
         }
         # define the x-values for the last segment
         if( j==length(pdat) ){ tempx <- c(txx[j]-xbuffer, txx[j]+xbuffer) }
         # horizontal line plot
         lines(tempx, tempy, lwd=lwd, col=cols[ unlist(pdat[j]) ] )
       }
    }
  }

  # add legend
  legMax <- max(lc$times, na.rm=T)-(max(lc$times, na.rm=T)-min(lc$times, na.rm=T))/lc$nfactors
  legPoints <- seq(from=min(lc$times, na.rm=T), to=legMax, length.out=lc$nfactors)
  for(l in 1:length(legPoints))
  {
    legend(legPoints[l], ymax, legend=lc$labels[l], lty=1,
           cex=lcex, col=cols[l], bty='n', lwd=llwd)
  }
  
  # if there is grouping add group labels
  if( !is.null(lc$group) )
  {
    g <- cbind(lc$group, 1:length(lc$group))
    gag <- aggregate(g[,2] ~ g[,1], g, mean, na.rm=T)
    u <- unique(lc$group)
    for(i in 1:length(u))
    {
      text(tx[1], gag[i,2], labels = lc$groupLabels[i], srt=groupRotation)
    }
  }
}

