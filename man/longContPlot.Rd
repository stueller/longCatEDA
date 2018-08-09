\name{longContPlot}
\alias{longContPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Continuous Longitudinal Data
}
\description{
Function to plot continuous longitudinal or time-series data.
}
\usage{
longContPlot(y, times = NULL, jog=FALSE, ylim = NULL, xlim = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{a data matrix or data frame in wide (as opposed to long) format with cases in rows and repeated observations in columns.}
  \item{times}{time points used for the x-axis in plotting. Either a vector of the same length as the number of columns in \code{y} (i.e., all cases have the same times of observation), or a matrix of the same dimension as \code{y} (i.e., individually varying times of observation). Default is \code{NULL} and is assigned the value 1:\code{ncol(y)}.}  
  \item{jog}{
When \code{y} is integer data, it can be useful to jog all values by a small amount. When \code{jog=TRUE}, a random uniform variate in [-.25, .25] is added to each row in \code{y}.
}
  \item{ylim}{
see \code{\link{par}}. Default is \code{NULL} and calculated from \code{y}.
}
  \item{xlim}{
see \code{\link{par}}. Default is \code{NULL} and calculated from \code{y}.
}
  \item{\dots}{
Arguments to be passed to \code{\link{plot}}. See \code{\link{par}}.
}
}

\references{
Tueller, S. J., Van Dorn, R. A., & Bobashev, G. V. (2016). Visualization of categorical longitudinal and times series data (Report No. MR-0033-1602). Research Triangle Park, NC: RTI Press. http://www.rti.org/publication/visualization-categorical-longitudinal-and-times-series-data}
\author{Stephen Tueller}

\seealso{
\code{\link{longCatPlot}}.
}
\examples{
# longitudinal plot
times <- c(1,100,200,300,400,500)
par(mfrow=c(1,1), bg='cornsilk3')
longContPlot(example2cont, times, ylim=c(-2,6), main='', ylab='', xlab='Day')
par(mfrow=c(1,1), bg='transparent')

# jogging example
times <- c(1,100,200,300,400,500)
par(mfrow=c(1,2), bg='cornsilk3')
longContPlot(example2cat, times,           ylim=c(0,6), 
  main='Growth Curves', ylab='', xlab='Days')
longContPlot(example2cat, times, jog=TRUE, ylim=c(0,6), 
  main='Growth Curves + Jogging', 
	ylab='', xlab='Days')
par(mfrow=c(1,1), bg='transparent')# compare growth curves to longCat

}
