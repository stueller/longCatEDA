

#' Align Time Series Data to Specific Events or States
#'
#' This function aligns time series data based on the occurrence of specific events or states.
#' It adjusts the time series to start from the nth occurrence of a given state or event.
#'
#' @param y Matrix of states over time for each case.
#' @param times Matrix of times corresponding to each state in `y`.
#' @param which.state Integer specifying the state to align by. NULL if aligning by event.
#' @param nth.state Integer specifying the nth occurrence of the state to align by. Required if `which.state` is not NULL.
#' @param not.state Logical indicating whether to align by the absence (`TRUE`) of `which.state`.
#' @param events Matrix of events over time for each case. NULL if aligning by state.
#' @param event.times Matrix of times corresponding to each event in `events`.
#' @param which.event Integer specifying the event to align by. NULL if aligning by state.
#' @param nth.event Integer specifying the nth occurrence of the event to align by. Required if `which.event` is not NULL.
#' @param not.event Logical indicating whether to align by the absence (`TRUE`) of `which.event`.
#' @return A list containing `aligned.times` and `aligned.event.times`, the matrices of aligned times and event times, respectively.
#' @examples
#' # illustrate individually varying times of observation
#' set.seed(642531)
#' y <- matrix(sample(1:5, 500, replace=TRUE), 100, 5)
#' set.seed(963854)
#' times <- matrix(runif(600, 1, 3), 100, 6)
#' # times must be cumulative
#' times <- t(apply(times, 1, cumsum))
#' # align at the 2nd instance of state 3
#' times23 <- alignTime(y, times, which.state=3, nth.state=2)$aligned.times
#' # plotting
#' labels <- c('Street', 'Drug Tx', 'Jail', 'Prison', 'Unknown')
#' lc   <- longCat(y, times=times, Labels=labels)
#' lc23 <- longCat(y, times=times23, Labels=labels)
#' par(mfrow=c(3,1), bg='cornsilk3')
#' longCatPlot(lc, main='Raw Times', legendBuffer=.5, ylab='')
#' longCatPlot(lc23, main='Aligned: 2nd Intsance of Jail',
#'             xlab='Days Since 2nd Instance of Jail (via alignTime)',
#'             legendBuffer=.5, ylab='')
#' # repeat calling alignment from longCatPlot, not quite identical due to sorting
#' # on unaligned data
#' longCatPlot(lc, main='Aligned: 2nd Instance of Jail (via longCatPlot)', legendBuffer=.5,
#'             which.state=3, nth.state=2, ylab='', xlab='Days Since 2nd Instance of Jail')
#' par(mfrow=c(1,1), bg='transparent')
#'
#' # illustrate the adding event indicators
#' set.seed(45962)
#' events <- matrix(sample(1:3, 200, replace=TRUE), 100, 2)
#' set.seed(23498)
#' event.times <- matrix(sample(c(times), 200, replace=FALSE), 100, 2)
#' # align at the 1st instance of event 2
#' alignedTimes <- alignTime(y, times, events=events, event.times=event.times,
#'                           which.event=2, nth.event=1)
#' times12       <- alignedTimes$aligned.times
#' event.times12 <- alignedTimes$aligned.event.times
#' # plotting
#' eventLabels=c('Arrest', 'Drug Test', 'Hearing')
#' lc <- longCat(y, times=times, Labels=labels,
#'               events=events, event.times=event.times,
#'               eventLabels=eventLabels)
#' lc12 <- longCat(y, times=times12, Labels=labels,
#'                 events=events, event.times=event.times12,
#'                 eventLabels=eventLabels)
#' par(mfrow=c(2,1), bg='cornsilk3', mar=c(5.1, 4.1, 4.1, 12.1), xpd=TRUE)
#' cols <- longCatPlot(lc, legendBuffer=.5,
#'                     main='Superimpose Events', ylab='')
#' cols <- longCatPlot(lc12, , legendBuffer=.5,
#'                     main='Aligned: 1st Drug Test',
#'                     xlab='Days Since 1st Drug Test', ylab='')
#' legend(15.5, 50, legend=lc$eventLabels, pch=1:length(lc$eventLabels))
#' par(mfrow=c(1,1), bg='transparent', mar = c(5, 4, 4, 2) + 0.1, xpd=FALSE)
#' @export
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



#' #' Find the Nth Occurrence of a Value in a Vector
#'
#' This function searches for the nth occurrence of a specified value (`w`) in a vector (`x`).
#' It can also find the nth occurrence where `x` does not equal `w` if `notw` is TRUE.
#' If the nth occurrence is not found, it returns a specified position beyond the last non-NA value.
#'
#' @param x Numeric vector in which to search for the value.
#' @param w The value to search for in `x`.
#' @param n The occurrence number of the value to find.
#' @param np1 Offset to add to the final position if `n` is not found.
#'        Default is 1, indicating the next position after the last non-NA value in `x`.
#' @param notw Logical; if TRUE, search for the nth occurrence where `x` does not equal `w`.
#'
#' @return Integer position of the nth occurrence of `w` in `x`, adjusted by `np1` if not found,
#'         or 1 if `x` contains all NAs.
#' @examples
#' x <- c(1, 2, 3, 2, 4, 2, NA, NA)
#' getN(x, 2, 2) # Returns 4, the position of the second occurrence of 2
#' getN(x, 2, 3) # Returns 6, the position of the third occurrence of 2
#' getN(x, 5, 1) # Returns 9, one position after the last non-NA value, since 5 is not found
#' getN(x, 2, 1, np1 = 0, notw = TRUE) # Returns 1, the position of the first value not equal to 2
#' @export
getN <- function(x, w, n, np1 = 1, notw = FALSE) {
  # Input validation
  stopifnot(is.numeric(x), is.numeric(w), is.numeric(n), is.numeric(np1), is.logical(notw))
  stopifnot(length(w) == 1, length(n) == 1, length(np1) == 1, length(notw) == 1)

  # Find the nth occurrence
  wn <- if(!notw) which(x == w)[n] else which(x != w)[n]

  # Adjust if nth occurrence is not found or if x is all NAs
  if(length(wn) == 0 || is.na(wn)) wn <- sum(!is.na(x)) + np1
  if(all(is.na(x))) wn <- 1

  wn
}

