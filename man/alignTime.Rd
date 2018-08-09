\name{alignTime}
\alias{alignTime}
\title{Align time data at a given state or event}
\description{Align \code{time} and \code{event.times} at a given state in \code{y} or \code{event}. All options can also be called via \code{\link{longCatPlot}}.}
\usage{
alignTime(y, times, 
  which.state=NULL, nth.state=NULL, not.state=FALSE,
  events=NULL, event.times=NULL, 
  which.event=NULL, nth.event=NULL, not.event=FALSE)
}
\arguments{
  \item{y}{see \code{y} for \code{\link{longCat}}.}
  \item{times}{see \code{times} for \code{\link{longCat}}.}
  \item{which.state}{the state in \code{y} on which to align \code{times} (and if given, \code{event.times}). If \code{which.state} is given, \code{which.event} must be \code{NULL}.}
  \item{nth.state}{the nth occurrence of \code{which.state} on which to align \code{times} (and if given, \code{event.times}). If \code{nth.state} is given, \code{nth.event} must be \code{NULL}.}
  \item{not.state}{instead of aligning at the nth state, align at the nth non-instance of \code{which.state}.}
  \item{events}{see \code{events} for \code{\link{longCat}}.}
  \item{event.times}{see \code{events.times} for \code{\link{longCat}}.}
  \item{which.event}{the event in \code{events} on which to align \code{times} and \code{event.times}. If \code{which.event} is given, \code{which.state} must be \code{NULL}.}
  \item{nth.event}{the nth occurrence of \code{which.event} on which to align \code{times} and \code{event.times}. If \code{nth.event} is given, \code{nth.state} must be \code{NULL}.}
  \item{not.event}{instead of aligning at the nth event, align at the nth non-instance of \code{which.event}.}
}
\value{
\code{alignTime} returns a list with two objects:
\item{aligned.times}{The \code{times} matrix aligned at the \code{nth.state} of \code{which.state}.}
\item{aligned.event.times}{The \code{event.times} matrix aligned at the \code{nth.event} of \code{which.event}.}
}
\references{
Tueller, S. J., Van Dorn, R. A., & Bobashev, G. V. (2016). Visualization of categorical longitudinal and times series data (Report No. MR-0033-1602). Research Triangle Park, NC: RTI Press. http://www.rti.org/publication/visualization-categorical-longitudinal-and-times-series-data}
\author{Stephen Tueller}

\seealso{
\code{\link{longCatPlot}} to plot \code{longCat} objects created by the \code{\link{longCat}} function.
}
\examples{
# illustrate individually varying times of observation 
set.seed(642531)
y <- matrix(sample(1:5, 500, replace=TRUE), 100, 5)
set.seed(963854)
times <- matrix(runif(600, 1, 3), 100, 6)
# times must be cumulative
times <- t(apply(times, 1, cumsum))
# align at the 2nd instance of state 3
times23 <- alignTime(y, times, which.state=3, nth.state=2)$aligned.times
# plotting
labels <- c('Street', 'Drug Tx', 'Jail', 'Prison', 'Unknown')
lc   <- longCat(y, times=times, Labels=labels)
lc23 <- longCat(y, times=times23, Labels=labels)
par(mfrow=c(3,1), bg='cornsilk3')
longCatPlot(lc, main='Raw Times', legendBuffer=.5, ylab='')
longCatPlot(lc23, main='Aligned: 2nd Intsance of Jail',
            xlab='Days Since 2nd Instance of Jail (via alignTime)', 
            legendBuffer=.5, ylab='')
# repeat calling alignment from longCatPlot, not quite identical due to sorting
# on unaligned data
longCatPlot(lc, main='Aligned: 2nd Instance of Jail (via longCatPlot)', legendBuffer=.5, 
            which.state=3, nth.state=2, ylab='', xlab='Days Since 2nd Instance of Jail')
par(mfrow=c(1,1), bg='transparent')            

# illustrate the adding event indicators
set.seed(45962)
events <- matrix(sample(1:3, 200, replace=TRUE), 100, 2)
set.seed(23498)
event.times <- matrix(sample(c(times), 200, replace=FALSE), 100, 2)
# align at the 1st instance of event 2
alignedTimes <- alignTime(y, times, events=events, event.times=event.times, 
                           which.event=2, nth.event=1)
times12       <- alignedTimes$aligned.times
event.times12 <- alignedTimes$aligned.event.times
# plotting
eventLabels=c('Arrest', 'Drug Test', 'Hearing')
lc <- longCat(y, times=times, Labels=labels,  
              events=events, event.times=event.times, 
              eventLabels=eventLabels)
lc12 <- longCat(y, times=times12, Labels=labels,  
                events=events, event.times=event.times12, 
                eventLabels=eventLabels)
par(mfrow=c(2,1), bg='cornsilk3', mar=c(5.1, 4.1, 4.1, 12.1), xpd=TRUE)
cols <- longCatPlot(lc, legendBuffer=.5,
                    main='Superimpose Events', ylab='')
cols <- longCatPlot(lc12, , legendBuffer=.5,
                    main='Aligned: 1st Drug Test',
                    xlab='Days Since 1st Drug Test', ylab='')
legend(15.5, 50, legend=lc$eventLabels, pch=1:length(lc$eventLabels))
par(mfrow=c(1,1), bg='transparent', mar = c(5, 4, 4, 2) + 0.1, xpd=FALSE)
}



