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
#' @return An S3 object of class \code{longCat}: a named list that always
#' contains every one of the components below (unused components are
#' \code{NULL}). Objects are checked by \code{\link{validate_longCat}} when
#' created and when passed to \code{\link{sorter}} or \code{\link{longCatPlot}}.
#' \describe{
#'   \item{\code{y}}{Numeric matrix of states recoded to the integers
#'     \code{1:nfactors}; cases in rows, repeated observations in columns.}
#'   \item{\code{dim}}{\code{dim(y)}.}
#'   \item{\code{times}}{Numeric matrix with \code{nrow(y)} rows and
#'     \code{ncol(y) + 1} columns giving interval start and stop times.}
#'   \item{\code{Labels}, \code{tLabels}}{Labels for the states and time
#'     points.}
#'   \item{\code{factors}, \code{nfactors}}{The recoded state values and their
#'     number.}
#'   \item{\code{IndTime}}{\code{TRUE} if times vary across cases.}
#'   \item{\code{order.y}}{Data frame of case IDs and sorting keys.}
#'   \item{\code{events}, \code{event.times}, \code{eventLabels}}{Event data,
#'     or \code{NULL}.}
#'   \item{\code{sorted}, \code{ascending}}{Sorting status; set by
#'     \code{\link{sorter}}.}
#'   \item{\code{group}, \code{groupLabels}}{Grouping used by
#'     \code{\link{sorter}}, or \code{NULL}.}
#'   \item{\code{y.sorted}, \code{times.sorted}, \code{order.y.sorted},
#'     \code{group.sorted}, \code{events.sorted},
#'     \code{event.times.sorted}}{Sorted copies set by \code{\link{sorter}};
#'     \code{NULL} until the object is sorted.}
#' }
#' @references Tueller, S. J., Van Dorn, R. A., & Bobashev, G. V. (2016). Visualization of categorical longitudinal and times series data (Report No. MR-0033-1602). Research Triangle Park, NC: RTI Press. \url{http://www.rti.org/publication/visualization-categorical-longitudinal-and-times-series-data}
#' @author Stephen J. Tueller
#' @seealso \code{\link{longCatPlot}} to plot \code{longCat} objects created by the \code{\link{longCat}} function;
#' \code{\link{longCat-methods}} for \code{print} and \code{summary}.
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

  # recode states to the integers 1 to the number of categories, in sorted
  # order of the observed values; this also converts character matrices
  # (which data.matrix() leaves unchanged) to numeric
  u <- sort(unique(c(y)))
  y <- matrix(match(y, u), nrow(y), ncol(y), dimnames = dimnames(y))
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
  if( !is.null(tLabels) && ncol(y) != length(tLabels) )
  {
    stop('`tLabels` must have one label per column of `y` (', ncol(y),
         '), not ', length(tLabels), '.', call. = FALSE)
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


  # apply class, validate, and return output
  new_longCat(list(y=y,
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
             eventLabels = eventLabels))
}
