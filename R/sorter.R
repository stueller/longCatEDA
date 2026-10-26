#' General Sorting Function
#'
#' A function to sort an \code{\link{longCat}} object created by \code{\link{longCat}}. \code{sorter} must be used directly when stratified plots of subgroups are desired, or when sorting other than the default is desired. Otherwise, \code{sorter} is used internally with the defaults by \code{\link{longCatPlot}} if \code{lc$sorted=FALSE}. If an object has already been sorted (\code{lc$sorted=TRUE}), \code{sorter} will not resort it but will print a code example of how to use multiple sortings.
#'
#' @param lc An object of class \code{\link{longCat}} created by \code{\link{longCat}}.
#' @param ascending Logical indicating if sorting should be done in ascending order. Default is \code{TRUE}.
#' @param whichColumns A numeric vector indicating which columns in \code{lc$y} should be used for sorting (e.g., \code{c(1, 5, 7)}). Useful if an intervention occurs after data collection has started and the user is not interested in sorting based on pre-intervention observations.
#' @param num How missing values affect the order (see Details). Default is \code{TRUE}.
#' @param mindur Minimum duration, or \code{NULL} (the default). States observed for less than \code{mindur} (\code{times[, j + 1] - times[, j] < mindur}) are treated as missing for sorting, so short-lived states do not drive the order. Durations are measured on all of \code{lc$times}, before \code{whichColumns} is applied.
#' @param igrpt Should \code{sorter} ignore repeated values for each row in \code{lc$y} for sorting purposes? If \code{TRUE}, each run of a repeated state is reduced to one value before sorting; a missing value ends a run and is then dropped, so \code{c(1, 2, 2, NA, 2, 3)} sorts as \code{c(1, 2, 2, 3)}.
#' @param customSort A vector of the same length as the number of rows in \code{lc$y} providing a user-defined variable on which to sort the data prior to secondarily applying the default sort. If \code{group} is not \code{NULL}, group will be sorted prior to the \code{customSort} variable.
#' @param initFirst If \code{customSort} is not \code{NULL}, setting \code{initFirst=TRUE} will sort on initial values prior to the custom sorting variable.
#' @param group A vector indicating group membership, of the same length as the number of rows in \code{lc$y}. Default is \code{NULL}.
#' @param groupLabels Numeric or character labels for the groups specified in \code{group}. Default is \code{NULL}.
#' @param ggap A number between zero and one indicating the proportion of blank rows to be plotted between groups when \code{group} is specified. The default of \code{NULL} is set to 0.05 when groups are present, and 0.0 when there are no groups.
#'
#' @return Returns an object of class \code{longCat} where \code{lc$sorted=TRUE}.
#'
#' @details
#' Within each group, cases are sorted on their initial state (if \code{initFirst = TRUE}), then on \code{customSort}, then on their sequence of states in the \code{whichColumns} columns (after applying \code{mindur} and \code{igrpt}). Sequences are compared state by state from the first column, as whole numbers, so states of 10 or more and any number of time points are handled exactly. Ties keep their original order.
#'
#' With \code{num = TRUE}, missing values are skipped: the observed states are compared from the left and a sequence that ends sooner sorts first, so \code{c(1, NA, 2)} sorts with \code{c(1, 2)} and before \code{c(1, 2, 3)}. Cases with no observed states sort last. This gives the order of the numeric patterns from \code{\link{makePatterns}} (\code{1.2} before \code{1.23}) and is recommended when data are missing or times vary across cases.
#'
#' With \code{num = FALSE}, states are compared in place and a missing value sorts after every state, the order of the string patterns from \code{\link{makePatterns}}.
#'
#' The patterns themselves are stored in the second column of \code{order.y} for reference.
#'
#' @author Stephen J. Tueller
#' @references Tueller, S. J., Van Dorn, R. A., & Bobashev, G. V. (2016). Visualization of categorical longitudinal and times series data (Report No. MR-0033-1602). Research Triangle Park, NC: RTI Press. \url{http://www.rti.org/publication/visualization-categorical-longitudinal-and-times-series-data}
#' @seealso \code{\link{longCat}} and \code{\link{longCatPlot}} for further functionality.
#' @examples
#'
#' ### create a plot like that in Figure 3 from Tueller, Van Dorn, & Bobashev (2016)
#' par(mfrow=c(1,2), bg='cornsilk3')
#' times <- c(1,100,200,300,400,500,600)
#' f3lc <- longCat(example3, times)
#' longCatPlot(f3lc, main='Unsorted', colScheme='heat', lwd=2, legendBuffer=.2,
#'             sort=FALSE)
#' longCatPlot(f3lc, main='Sorted', colScheme='heat', lwd=2, legendBuffer=.2)
#'
#' ### sort with a grouping variable and plot
#' par(mfrow=c(1,1), bg='cornsilk3', mar=c(5.1, 4.1, 4.1, 9.1), xpd=TRUE)
#' times <- c(1,100,200,300,400,500,600)
#' lc <- longCat(example3, times)
#' group <- sample(1:3, nrow(example3), replace=TRUE)
#' grouplc <- sorter(lc, group=group, groupLabels=1:3)
#' cols <- longCatPlot(grouplc, groupBuffer=.15, main='Grouped Data', colScheme='heat',
#'                     lwd=2, legendBuffer=0)
#' legend(610, 130, legend=1:5, col=cols, lty=1, lwd=2)
#' par(bg='transparent', mar = c(5, 4, 4, 2) + 0.1, xpd=FALSE)
#'
#' ### using the sorted data from the previous plot, repeate using ggplot2
#' #   following the example of Figure 4 of bdemarest's answer on
#' #   https://stackoverflow.com/questions/11513149/
#' #   good-ways-to-visualize-longitudinal-categorical-data-in-r/
#' grouplc.df <- data.frame(id=1:nrow(grouplc$group.sorted),
#'                          group=grouplc$group.sorted[,1], grouplc$y.sorted)
#' grouplc.long <- reshape(grouplc.df,
#'                         varying = names(grouplc$y.sorted),
#'                         v.names = "score",
#'                         timevar = "time",
#'                         times = times[1:ncol(grouplc$y.sorted)],
#'                         direction = "long")
#' grouplc.long$score <- factor(grouplc.long$score)
#' grouplc.long$group <- factor(grouplc.long$group, levels=3:1)
#' # remove NA's introduced using group option in sorter
#' grouplc.long <- na.omit(grouplc.long)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   print(
#'     ggplot2::ggplot(grouplc.long,
#'                     ggplot2::aes(x=time, y=id, fill=score)) +
#'       ggplot2::geom_tile(colour="transparent") +
#'       ggplot2::scale_fill_manual(values=cols) +
#'       ggplot2::facet_grid(group ~ ., space="free_y", scales="free_y")
#'   )
#' }
#'
#' ### sort with a grouping variable and events and plot
#' times <- c(1,100,200,300,400,500,600)
#' set.seed(45962)
#' events <- matrix(sample(1:3, nrow(example3)*2, replace=TRUE), nrow(example3), 2)
#' set.seed(23498)
#' event.times <- matrix(sample(min(times):max(times), nrow(example3)*2, replace=TRUE),
#'                       nrow(example3), 2)
#' labels <- c('Street', 'Drug Tx', 'Jail', 'Prison', 'Unknown')
#' eventLabels=c('Arrest', 'Drug Test', 'Hearing')
#' eventlc <- longCat(example3, times=times, Labels=labels,
#'                    events=events, event.times=event.times,
#'                    eventLabels=eventLabels)
#' set.seed(4290)
#' groupevent <- sample(1:3, nrow(example3), replace=TRUE)
#' groupeventlc <- sorter(eventlc, group=groupevent)
#' par(mfrow=c(1,1), bg='cornsilk3', mar=c(5.1, 4.1, 4.1, 12.1), xpd=TRUE)
#' cols <- longCatPlot(groupeventlc, legendBuffer=0, groupBuffer=0.15,
#'                     main='Grouping and Events')
#' legend(610, 130, legend=groupeventlc$Labels, lty=1, col=cols, lwd=2)
#' legend(610, 60, legend=groupeventlc$eventLabels,
#'        pch=1:length(groupeventlc$eventLabels))
#' par(bg='transparent', mar = c(5, 4, 4, 2) + 0.1, xpd=FALSE)
#' @export
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
  validate_longCat(lc)

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

  # prepare the states to sort on: blank short durations (using the full
  # times so each cell's own interval is measured), select the sorting
  # columns, and collapse repeats; drop = FALSE keeps one column a matrix
  y.sort <- lc$y
  if(!is.null(mindur)) y.sort <- mask_short_durations(y.sort, lc$times, mindur)
  y.sort <- y.sort[, whichColumns, drop = FALSE]
  if(igrpt) y.sort <- collapse_repeats_rows(y.sort)

  # store the patterns for reference; sorting compares the states column by
  # column (see sequence_sort_keys())
  lc$order.y[,2] <- makePatterns(y.sort, num = num)
  lc$order.y[,3] <- customSort
  sort.keys <- sequence_sort_keys(y.sort, num)

  # split the data into groups and sort within group
  id.g.l <- y.g.l <- times.g.l <- events.g.l <- event.times.g.l <-
    group.g.l <- vector('list', ngroups)
  for(g in 1:ngroups)
  {
    # extract group specific data
    w.g     <- group==u.g[g]
    id.g    <- lc$order.y[w.g,]
    keys.g  <- sort.keys[w.g, , drop = FALSE]
    y.g     <- lc$y[w.g,]
    times.g <- lc$times[w.g,]

    if(!is.null(lc$events))
    {
      events.g      <- lc$events[w.g,]
      event.times.g <- lc$event.times[w.g,]

      # if a one column events matrix is provided, the above row extraction
      # results in vectors instead of matrices
      if(! 'matrix' %in% class(events.g)     ) events.g      <- as.matrix(events.g)
      if(! 'matrix' %in% class(event.times.g)) event.times.g <- as.matrix(event.times.g)
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
                        initFirst    = initFirst     ,
                        keys1        = keys.g        )

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
  new_longCat(list( y = lc$y,
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
             eventLabels = lc$eventLabels))
}

#' Helper function for \code{\link{sorter}}, doing within group sorting
#'
#' This function is a helper for \code{\link{sorter}}, designed to handle sorting within individual groups as \code{\link{sorter}} loops through groups (even if there is only one group) using \code{sort1}.
#'
#' @param id1 The identification variable for one group, analogous to \code{id} in \code{\link{longCat}}.
#' @param y1 The longitudinal data for one group, analogous to \code{y} in \code{\link{longCat}}.
#' @param times1 The time data for one group, analogous to \code{times} in \code{\link{longCat}}.
#' @param events1 The events data for one group, analogous to \code{events} in \code{\link{longCat}}.
#' @param event.times1 The event times for one group, analogous to \code{event.times} in \code{\link{longCat}}.
#' @param group1 The group identification variable for one group, analogous to \code{group} in \code{\link{sorter}}.
#' @param ascending Logical, indicating if sorting should be in ascending order. This parameter is consistent with the \code{ascending} option in \code{\link{sorter}}.
#' @param whichColumns Specifies which columns of \code{y1} should be considered in the sorting process, consistent with the \code{whichColumns} option in \code{\link{sorter}}.
#' @param initFirst Logical, indicating whether to initialize sorting with the first column, consistent with the \code{initFirst} option in \code{\link{sorter}}.
#' @param keys1 Sort keys for the group's state sequences, as built internally by \code{\link{sorter}}. If \code{NULL} (the default), cases are sorted on the pattern in \code{id1[, 2]} instead.
#'
#' @author Stephen J. Tueller
#' @export
sort1  <- function(id1, y1, times1, events1, event.times1, group1,
                   ascending=TRUE, whichColumns=NULL,
                   initFirst=FALSE, keys1=NULL)
{

  if( initFirst) init <- y1[,1]
  if(!initFirst) init <- rep(1, nrow(y1))

  if(is.null(keys1)) {
    o <- order(init, id1[,3], id1[,2], decreasing = !ascending)
  } else {
    o <- order_cases(init, id1[,3], keys1, ascending)
  }

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


