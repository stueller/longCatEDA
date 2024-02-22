#' longCat Class for Longitudinal Categorical Data
#'
#' This S4 class represents longitudinal categorical data, including both raw and sorted data,
#' along with associated metadata such as time points, labels, and grouping information. It is
#' designed to facilitate the analysis and visualization of such data.
#'
#' @slot y A matrix representing the raw longitudinal categorical data with cases in rows and repeated observations in columns.
#' @slot y.sorted A matrix representing the sorted longitudinal categorical data, analogous to \code{y}.
#' @slot dim An integer vector indicating the dimension of the \code{y} matrix.
#' @slot times A matrix of time points corresponding to each observation in \code{y}.
#' @slot times.sorted A matrix of sorted time points, analogous to \code{times}.
#' @slot Labels Character vector of labels for the categorical states represented in \code{y}.
#' @slot factors Numeric vector containing the unique values in \code{y}.
#' @slot IndTime Logical indicating if \code{times} represents individually varying times of observation.
#' @slot nfactors Integer indicating the number of unique factors in \code{y}.
#' @slot sorted Logical indicating if the data in \code{y} has been sorted.
#' @slot ascending Logical indicating the direction of sorting.
#' @slot group A matrix indicating group membership for each case.
#' @slot group.sorted A matrix of sorted group membership, analogous to \code{group}.
#' @slot groupLabels Character vector of labels for the groups.
#' @slot order.y A matrix with identification and sorting information for each case.
#' @slot order.y.sorted A matrix of sorted identification and sorting information, analogous to \code{order.y}.
#' @slot events A matrix representing events data with cases in rows and events in columns.
#' @slot event.times A matrix of times corresponding to each event in \code{events}.
#' @slot events.sorted A matrix of sorted events data, analogous to \code{events}.
#' @slot event.times.sorted A matrix of sorted event times, analogous to \code{event.times}.
#' @slot eventLables (Typo in original, should be `eventLabels`) Character vector of labels for the events represented in \code{events}.
#'
#' @author Stephen Tueller
#' @exportClass longCat
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

#' Summary Method for longCat Objects
#'
#' Provides a summary of an \code{longCat} object, including information about its dimensions,
#' factor details, sorting status, and group composition. If the object is sorted, additional
#' information relevant to sorting is also displayed.
#'
#' @param object An object of class \code{longCat} to summarize.
#' @param ... Additional arguments affecting the summary produced.
#'
#' @return Prints a summary of the \code{longCat} object, including dimensions, the number of unique factors,
#' whether the data is sorted, group information, and possibly sorted group information. The function
#' itself invisibly returns \code{NULL}.
#'
#' @exportMethod summary
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

#' Creation of Objects of Class longCat
#'
#' Function to create objects of class \code{longCat}.
#'
#' @param y A data matrix or data frame of numeric states in wide (as opposed to long) format with cases in rows and repeated observations in columns. It is recommended that \code{y} have 9 or fewer unique non-missing levels. Labels for the numeric states are given in \code{Labels}.
#' @param times The \code{times} object designates start and stop points for each plotted interval. It is either a vector with length being the number of columns in \code{y} plus one, \code{NULL}, or a matrix with the same number of rows as \code{y} and one more column than in \code{y}. Negative values are allowed such as would be the case if time is centered at an intervention point, negative values represent times prior to the intervention, and positive times represent times after the intervention. If \code{times} is a vector, it is assumed that cases in each row in \code{y} are observed at the same time points for the same durations...
#' @param Labels A vector of numeric or character labels for the response options in \code{y}. Must be the same length as the number of unique non-missing values in \code{y}. Default is \code{NULL} and is assigned the values \code{1:max(unique(y))}.
#' @param tLabels Numeric or character labels for the time points in \code{times}. Default is \code{NULL} and is assigned the values \code{1:ncol(y)}.
#' @param id An optional variable identifying or naming the rows of \code{y}. Returned as the first column of the matrix \code{order.y} (see \code{order.y} in the value section below).
#' @param events An event \code{matrix} or \code{data.frame} which may be numeric or character (see \code{eventLabels}). Whereas the data in \code{y} are states in which each case resides for some period of time, \code{events} are instantaneous events (or very short-lived states) that can be attached to a single point in time at \code{event.times}. The number of rows in \code{events} and \code{event.times} must equal the number of rows in \code{y}, but can have as many columns as needed to capture all events of interest.
#' @param event.times A \code{matrix} or \code{data.frame} of event times corresponding to each event in \code{events}.
#' @param eventLabels If \code{events} is a character matrix, \code{eventLabels} should be left \code{NULL} and labels will be pulled from the data in \code{events}. If \code{events} is numeric, corresponding \code{eventLabels} can be supplied by the user as a character vector.
#' @return An object of class \code{longCat} which is a list containing at least the following components: \code{y}, \code{y.sorted}, \code{dim}, \code{times}, \code{endt}, \code{times.sorted}, \code{endt.sorted}, \code{labels}, \code{tLabels}, \code{factors}, \code{IndTime}, \code{nfactors}, \code{sorted}, \code{ascending}, \code{group}, \code{groupLabels}, \code{order.y}, \code{events}, \code{event.times}, and \code{eventLabels}.
#' @references Tueller, S. J., Van Dorn, R. A., & Bobashev, G. V. (2016). Visualization of categorical longitudinal and times series data (Report No. MR-0033-1602). Research Triangle Park, NC: RTI Press. \url{http://www.rti.org/publication/visualization-categorical-longitudinal-and-times-series-data}
#' @author Stephen Tueller
#' @seealso \code{\link{longCatPlot}} to plot \code{longCat} objects created by the \code{\link{longCat}} function.
#' @examples
#' #create the longcat object similar to Figure 2 in Tueller (2016)
#' times <- c(1,100,200,300,400,500,600)
#' f2lc <- longCat(example2cat, times)
#'
#' # object summary
#' summary(f2lc)
#'
#' # compare growth curves to longCat
#' par(mfrow=c(1,2), bg='cornsilk3')
#' longContPlot(example2cat, times, ylim=c(1,5),
#'              main='Growth Curves', ylab='', xlab='Days')
#' longCatPlot(f2lc, lwd=4, main='Horizontal Line Plot', colScheme='heat', legendBuffer=.2)
#' par(mfrow=c(1,1), bg='transparent')
#'
#' # illustrate individually varying times of observation
#' set.seed(642531)
#' y <- matrix(sample(1:5, 500, replace=TRUE), 100, 5)
#' set.seed(963854)
#' times <- matrix(runif(600, 1, 3), 100, 6)
#' # times must be cumulative
#' times <- t(apply(times, 1, cumsum))
#' lc <- longCat(y, times=times)
#' par(mfrow=c(1,1), bg='cornsilk3', mar=c(5.1, 4.1, 4.1, 10.1), xpd=TRUE)
#' cols <- longCatPlot(lc, legendBuffer=0, groupBuffer=0,
#'                     main='Individually Varying Times of Observation')
#' legend(15.5, 100, legend=lc$Labels, lty=1, col=cols, lwd=2)
#' par(bg='transparent', mar = c(5, 4, 4, 2) + 0.1, xpd=FALSE)
#'
#' # illustrate the adding event indicators
#' set.seed(45962)
#' events <- matrix(sample(1:3, 200, replace=TRUE), 100, 2)
#' set.seed(23498)
#' event.times <- matrix(sample(c(times), 200, replace=FALSE), 100, 2)
#' labels <- c('Street', 'Drug Tx', 'Jail', 'Prison', 'Unknown')
#' eventLabels=c('Arrest', 'Drug Test', 'Hearing')
#' lc <- longCat(y, times=times, Labels=labels,
#'               events=events, event.times=event.times,
#'               eventLabels=eventLabels)
#' par(mfrow=c(1,1), bg='cornsilk3', mar=c(5.1, 4.1, 4.1, 12.1), xpd=TRUE)
#' cols <- longCatPlot(lc, legendBuffer=0, groupBuffer=0,
#'                     main='Superimpose Events Over States')
#' legend(15.5, 100, legend=lc$Labels, lty=1, col=cols, lwd=2)
#' legend(15.5, 40, legend=lc$eventLabels, pch=1:length(lc$eventLabels))
#' par(bg='transparent', mar = c(5, 4, 4, 2) + 0.1, xpd=FALSE)
#'
#' \dontrun{
#'   # illustrate handling non time-ordered input (e.g., factor analysis data)
#'   y <- matrix(sample(c('1', '2', '3', '4', '5'), 500, replace=TRUE), 100, 5)
#'   lc <- longCat(y)
#'   par(mfrow=c(1,1), bg='cornsilk3', mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
#'   cols <- longCatPlot(lc, legendBuffer=0)
#'   legend(6, 100, legend=lc$factors, lty=1, col=cols, lwd=2)
#'   par(bg='transparent', mar = c(5, 4, 4, 2) + 0.1, xpd=FALSE)
#'
#'   # illustrate plotting with more than 9 categories
#'   # (a warning is issued)
#'   y <- matrix(sample(1:18, 500, replace=TRUE), 100, 5)
#'   lc <- longCat(y)
#'   par(mfrow=c(1,1), bg='cornsilk3', mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
#'   cols <- longCatPlot(lc, legendBuffer=0)
#'   legend(6, 100, legend=lc$factors, lty=1, col=cols, lwd=2)
#'   par(bg='transparent', mar = c(5, 4, 4, 2) + 0.1, xpd=FALSE)
#' }
#'
#' @export
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
