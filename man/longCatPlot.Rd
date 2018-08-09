\name{longCatPlot}
\alias{longCatPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting of lc objects
}
\description{
Function to plot \code{longCat} objects created by \code{\link{longCat}}.  
}
\usage{longCatPlot(lc, xlab = "Days", 
  ylab = NULL, cols = NULL, 
  colScheme = "heat", reverse = FALSE, lwd = 0.5, lcex = 1, llwd = 3, 
  legendBuffer = 0.12, groupBuffer = 0, groupRotation = 90, gcex = 1, 
  seg.len = 1, xlas = 0, xcex = 1, ecex = .5, event.col=1, 
  plot.events=TRUE, which.events=NULL, 
  n.events=NULL, event.pch=NULL, 
  texclude=NULL, sort=TRUE,
  which.state=NULL, nth.state=NULL, not.state=FALSE,
  which.event=NULL, nth.event=NULL, not.event=FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lc}{
an object of class \code{\link{longCat}} created by \code{\link{longCat}}. See \code{\link{par}}.
}
  \item{xlab}{
a label for the x-axis. Default is "Days". See \code{\link{par}}.
}
  \item{ylab}{
a label for the y-axis. Default is \code{NULL} which is changed to "Each Line Represents a Participant" with the sample size appended (\code{lc$dim[1]}, see \code{\link{longCat}}). See \code{\link{par}}.
}
  \item{cols}{
a numeric or character list of colors. See \code{\link{par}}. Default is \code{NULL}. To use internal color schemes, use \code{colScheme}.
}
  \item{colScheme}{
select a color scheme. See \code{\link{colChoose}} for available options.
}
  \item{reverse}{
color schemes are applied from the lowest to highest level of categorical data in \code{lc$y} or \code{lc$y.sorted}. Set \code{reverse=TRUE} to reverse this. Default is \code{FALSE}.
}
  \item{lwd}{
set the width of horizontal lines. Default is .5. \code{lwd} should be reduced proportionally to the number of rows in \code{lc$y} to avoid overlap in plotting. 
}
  \item{lcex}{
character expansion factor for the legend text. Default is 1. See \code{\link{par}}.
}
  \item{llwd}{
set the width of lines in the legend. default is 3. See \code{lwd}.
}
  \item{legendBuffer}{
set proportion of the plot to retain for legends, must be in [0,1]. Note that the legend is very sensitive to the scaling of the graphics device. Users are advised to maximize their device and rerun, or call \code{dev.new()} and resize prior to running \code{longCatPlot}. Default is .12 (i.e., 12\% of the vertical plot area is retained for the legend). Set to 0 if no legend is desired. See the examples for moving the legend outside of the plotting margins.
}
  \item{groupBuffer}{
similar to legendBuffer, but for group labels on the left side of the plot. Default is 0 (i.e., 0\% of the horizontal plot area is retained for group labels). Can take on any value in [0,1]. 
}
  \item{groupRotation}{
if \code{lc$groupLabels} are long (see \code{\link{longCat}}), rotation of the labels can be used reduce the needed size of \code{groupBuffer}. The value is in degrees ranging from -360 to +360.
}
  \item{gcex}{character expansion factor for group labels. Default is 1. See \code{\link{par}}.
}
  \item{seg.len}{Length of lines in the upper legend. Default is 1. See \code{\link{legend}}.}
  \item{xlas}{see \code{las} in \code{\link{par}}. Applied to the x-axis when \code{tLabels} are provided to \code{\link{longCat}}.}
  \item{xcex}{see \code{axis.cex} in \code{\link{par}} and  \code{\link{axis}}. Applied to the x-axis when \code{tLabels} are provided to \code{\link{longCat}}.}
  \item{ecex}{see \code{cex} in \code{\link{points}}. This is used to size the points used to plot event points if \code{events} is not \code{NULL}.}
  \item{event.col}{color for ploting event indicators.}
  \item{plot.events}{logical - should events be plotted?}
  \item{which.events}{numeric vector - which events should be plotted? For example, if the events have values \code{c(1,2,3,4,5)}, you can specify that only \code{which.events=c(2,5)} be plotted.}
  \item{n.events}{how many events should be plotted, e.g., \code{n.events=3} plots the first three events for each participant.}
  \item{event.pch}{what plotting characters should be used. See \code{\link{points}}.}
  \item{texclude}{a vector of length 2 indicating the range of \code{times} and \code{event.times} to be plotted, e.g., \code{texclude=c(10,20)} will plot data for time points 10 and larger up to and including 20.}
  \item{sort}{logical - should \code{longCatPlot} sort the data on the fly using intelligent defaults? If data are already sorted via \code{sorter}, this will be ignored.}
  \item{which.state }{see \code{\link{alignTime}}.}
  \item{nth.state   }{see \code{\link{alignTime}}.}
  \item{not.state   }{see \code{\link{alignTime}}.}
  \item{which.event }{see \code{\link{alignTime}}.}
  \item{nth.event   }{see \code{\link{alignTime}}.}
  \item{not.event   }{see \code{\link{alignTime}}.}
  \item{\dots}{
Arguments to be passed to \code{\link{plot}} (see \code{\link{par}}).
}
}
\references{
Tueller, S. J., Van Dorn, R. A., & Bobashev, G. V. (2016). Visualization of categorical longitudinal and times series data (Report No. MR-0033-1602). Research Triangle Park, NC: RTI Press. http://www.rti.org/publication/visualization-categorical-longitudinal-and-times-series-data}
\author{Stephen Tueller}

\seealso{
\code{\link{longCat}}.
}
\examples{
# Illustrate longCatPlot with the legend outside the plot
par(mfrow=c(1,1), bg='cornsilk3', mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
cols <- longCatPlot(
  longCat(example3),
  legendBuffer=0,
  main='Horizontal Line Plot')
legend(7.1, 100, legend=1:5, col=cols, lty=1, lwd=2)
par(bg='transparent', mar = c(5, 4, 4, 2) + 0.1, xpd=FALSE)

### visualizing multivariate data: 3 items at 4 time points
library(MASS)
Sigma <- matrix(.25, 12, 12)
diag(Sigma) <- 1
set.seed(9845)
mu <- rep(c(-.5, 0, .5), 4) + rnorm(12, 0, .25)
set.seed(539)
ymv <- apply(mvrnorm(n=100, mu=mu, Sigma = Sigma), 2, cut, breaks=c(-Inf, 0, Inf), labels=c(0,1))
apply(ymv, 2, table)
(items <- rep(1:3, 4))
(times <- sort(rep(1:4, 3)))
tLabels <- paste('Time', 1:4)
 Labels <- paste('Item', 1:3)
 
# plot time points within items
par(mfrow=c(2,2), bg='cornsilk3', mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)

item1  <- longCat(y=ymv[,items==1], tLabels=tLabels)
cols <- longCatPlot(item1, ylab='', main='Item 1', legendBuffer=0, xlab="", xlas=2)
legend(length(unique(times))+.1, nrow(ymv), legend=0:1, col=cols, lty=1, lwd=2, title='Response')

item2 <- longCat(y=ymv[,items==2], tLabels=tLabels)
longCatPlot(item2, ylab='', main='Item 2', legendBuffer=0, xlab="", xlas=2)
legend(length(unique(times))+.1, nrow(ymv), legend=0:1, col=cols, lty=1, lwd=2, title='Response')

item3 <- longCat(y=ymv[,items==3], tLabels=tLabels)
longCatPlot(item3, ylab='', main='Item 3', legendBuffer=0, xlab="", xlas=2)
legend(length(unique(times))+.1, nrow(ymv), legend=0:1, col=cols, lty=1, lwd=2, title='Response')

par(bg='transparent', mar = c(5, 4, 4, 2) + 0.1, xpd=FALSE)

# plot items within time points
par(mfrow=c(2,2), bg='cornsilk3', mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)

time1  <- longCat(y=ymv[,times==1], tLabels=Labels)
cols <- longCatPlot(time1, ylab='', main='Time 1', legendBuffer=0, xlab="")
legend(length(unique(times))+.1, nrow(ymv), legend=0:1, col=cols, lty=1, lwd=2, title='Response')

time2  <- longCat(y=ymv[,times==2], tLabels=Labels)
cols <- longCatPlot(time2, ylab='', main='Time 2', legendBuffer=0, xlab="")
legend(length(unique(times))+.1, nrow(ymv), legend=0:1, col=cols, lty=1, lwd=2, title='Response')

time3  <- longCat(y=ymv[,times==3], tLabels=Labels)
cols <- longCatPlot(time3, ylab='', main='Time 3', legendBuffer=0, xlab="")
legend(length(unique(times))+.1, nrow(ymv), legend=0:1, col=cols, lty=1, lwd=2, title='Response')

time4  <- longCat(y=ymv[,times==4], tLabels=Labels)
cols <- longCatPlot(time4, ylab='', main='Time 4', legendBuffer=0, xlab="")
legend(length(unique(times))+.1, nrow(ymv), legend=0:1, col=cols, lty=1, lwd=2, title='Response')

par(mfrow=c(1,1), bg='transparent', mar = c(5, 4, 4, 2) + 0.1, xpd=FALSE)

\dontrun{
# for data sets with many rows, writing directly to a file 
# is much faster and unaffected by device resizing, see ?pdf
pdf('C:/mydir/mysubdir/myfile.pdf')
par(bg='cornsilk3')
longCatPlot(f3lc, main='Sorted', colScheme='heat', lwd=2)
par(mfrow=c(1,1), bg='transparent')
dev.off()
# see ?jpeg for picture file options
}
}
