#' Simulated Data for Illustrating \code{\link{longContPlot}}
#'
#' Simulated data for illustrating \code{\link{longContPlot}}.
#'
#' @format Data frame with 20 subjects and 6 time points
#' @docType data
#' @usage example2cont
#' @keywords datasets
"example2cont"

#' Plot Longitudinal Continuous Data
#'
#' This function creates a plot for longitudinal continuous data, optionally adding a random "jog" to each data point
#' to reduce overplotting. It allows specifying the x and y limits and uses either provided or automatically
#' calculated times for plotting.
#'
#' @param y Numeric matrix of data to plot.
#' @param times Optional; numeric vector or matrix of times corresponding to `y` values.
#' @param jog Logical; if TRUE, adds a random jog to the data to reduce overplotting.
#' @param ylim Numeric vector; the range of y values to be plotted.
#' @param xlim Numeric vector; the range of x values (times) to be plotted.
#' @param ... Additional arguments to be passed to the plot function.
#'
#' @examples
#' # longitudinal plot
#' times <- c(1,100,200,300,400,500)
#' par(mfrow=c(1,1), bg='cornsilk3')
#' longContPlot(example2cont, times, ylim=c(-2,6), main='', ylab='', xlab='Day')
#' par(mfrow=c(1,1), bg='transparent')
#'
#' # jogging example
#' times <- c(1,100,200,300,400,500)
#' par(mfrow=c(1,2), bg='cornsilk3')
#' longContPlot(example2cat, times,           ylim=c(0,6),
#'              main='Growth Curves', ylab='', xlab='Days')
#' longContPlot(example2cat, times, jog=TRUE, ylim=c(0,6),
#'              main='Growth Curves + Jogging',
#'              ylab='', xlab='Days')
#' par(mfrow=c(1,1), bg='transparent')# compare growth curves to longCat
#' @export
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


