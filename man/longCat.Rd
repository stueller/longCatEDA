\name{longCat}
\alias{longCat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Creation of Objects of Class longCat}
\description{Function to create objects of class \code{longCat}.}
\usage{
longCat(y, times = NULL, Labels = NULL, tLabels = NULL, id = NULL, 
        events = NULL, event.times = NULL, eventLabels = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{a data matrix or data frame of numeric states in wide (as opposed to long) format with cases in rows and repeated observations in columns. It is reccomended that \code{y} have 9 or fewer unique non-missing levels. Labels for the numeric states are given in \code{Labels}.}
  \item{times}{The \code{times} object designates start and stop points for each plotted interval. It is either a vector with length being the number of columns in \code{y} plus one, \code{NULL}, or a matrix with the same number of rows as \code{y} and one more column than in \code{y}. Negative values are allowed such as would be the case if time is centered at an intervention point, negative values represent times prior to the intervention, and positive times represent times after the intervention. 
  
  If \code{times} is a vector, it is assumed that cases in each row in \code{y} is observed at the same time points for the same durations. For example, if \code{times=c(0,6,12,13)}, this indicates a design were cases were observed at 0, 6, and 12 time units. When applying \code{longCatPlot}, the first observation for each case with extend from 0 to 6, the second observation for each case will extend from 6 to 12, and the third observation will extend from 12 to 13. The value selected at the end may be arbitrary as cases may not have been followed for any additional time. A value may be selected to maintain consistent interval sizes. Continuing the example, one could use \code{times=c(0,6,12,18)} instead of \code{times=c(0,6,12,13)} even though cases weren't actually followed past month 12. Unequal spacing is allow, for example \code{times=c(0,3,12,18)}. In this case, participants are observed for at baseline, 3 time units, and 12 time units, where the final status is either intentionally or arbitrarily extended 18-12=6 time units at the right end of the plot. Missing values are not allowed. 
  
  If \code{times=NULL} (the default), \code{times=0:ncol(y)} will be assigned (i.e., starting at 0 times units and increase to 1 time unit, 2 time units, etc.). 
  
  When \code{times} is a matrix, each case has a unique set of observation times. In this case, missing values are allowed, but each case should have at least one observation and a start and stop point for that observation. If available timing data is a matrix of the same size as \code{y} and represents start points, the user must add a column at the end designating how far to the right time points should be extended. As noted above, this value may be arbitrary and should be large enough to show what state cases end in. If available timing data is a matrix of the same size as \code{y} and represents end points, the user must add a column at the beginning of the matrix designating designating start points. If available timing data represents duration instead of time points, the user must recode the data into cumulative time. For example, if a participant was in their first three states for 5, 7, and 11 time units, their row of \code{times} should be recoded to be \code{c(0,5,12,23)}. 
  
The \code{times} and \code{event.times} matrices can be realigned using the \code{\link{alignTime}} function, or this can be done on the fly by accessing the \code{alginTime} options in the \code{\link{longCatPlot}} function.
}  
  \item{Labels}{a vector of numeric or character labels for the response options in \code{y}. Must be the same length as the number of unique non-missing values in \code{y}. Default is \code{NULL} and is assigned the values \code{1:max(unique(y))}.}
  \item{tLabels}{numeric or character labels for the time points in \code{times}. Default is \code{NULL} and is assigned the values \code{1:ncol(y)}.}
  \item{id}{An optional variable identifying or naming the rows of \code{y}. Returned as the first column of the matrix \code{order.y} (see \code{order.y} in the value section below).}
  \item{events}{An event \code{matrix} or \code{y.frame} which may be numeric or character (see \code{eventLabels}). Whereas the data in \code{y} are states in which each case resides for some period of time, \code{events} are instantaneous events (or very short lived states) that can be attached to a single point in time at \code{event.times}. The number of rows in \code{events} and \code{event.times} must equal the number of rows in \code{y}, but can have as many columns as needed to capture all events of interest. Large numbers of events may cloud the resulting figures created by \code{\link{longCatPlot}}.}
  \item{event.times}{A \code{matrix} or \code{data.frame} of event times corresponding to each event in \code{events}. As opposed to the \code{times} matrix, which contains durations (except possibly the first column as described above), the \code{event.times} matrix is the time the event takes place (i.e., cumulative time).
  
The \code{times} and \code{event.times} matrices can be realigned using the \code{\link{alignTime}} function, or this can be done on the fly by accessing the \code{alginTime} options in the \code{\link{longCatPlot}} function.}
  \item{eventLabels}{If \code{events} is a character matrix, \code{eventLabels} should be left \code{NULL} and labels will be pulled from the data in \code{events}. If \code{events} is numeric, corresponding \code{eventLabels} can be supplied by the user as a character vector, for example, \code{c('event1', 'event2', 'etc.')}. The number of unique events (and correpsonding event labels) should be kept small if possible, otherwise the event legend on Figures produced by \code{\link{longCatPlot}} may be truncated.}
}
\value{
\code{longCat} returns an object of class \code{longCat} which is a list containing at least the following components:
  \item{y }{\code{y}}
  \item{y.sorted }{\code{y} sorted (default is \code{NULL} unless \code{\link{sorter}} has been applied to the \code{longCat} object).}
  \item{dim }{the dimension of \code{y}.}
  \item{times }{the \code{times} object as described above.}
  \item{endt}{the \code{endt} object as described above.}
  \item{times.sorted }{if \code{times} is a matrix of the same dimension as \code{y}, \code{times.sorted} contains a matrix of individually varying times of observation with the same sorting as \code{y.sorted.}}
  \item{endt.sorted}{\code{endt} sorted after an \code{lc} object is passed to \code{\link{sorter}}}
  \item{labels }{the \code{labels} vector as described above}
  \item{tLabels }{the \code{tLabels} vector as described above}
  \item{factors }{a vector containing the unique values in \code{y}. Not that if the unique values in \code{y} were not sequential integers starting at 1, both \code{factors} and \code{y} are recoded such that they contain sequential integers starting at 1.}
  \item{IndTime }{a logical indicator of whether \code{times} is a matrix of the same dimension as \code{y}. If \code{TRUE}, \code{\link{longCatPlot}} treats these times as individually varying times of observation.}
  \item{nfactors}{the number of unique values in \code{y}, and is the same as the length of the \code{factors} vector. \code{nfactors} is determined by \code{longCat}. If users have data with more than 9 categories, continuous plotting methods are recommended via warning (e.g., try \code{\link{longContPlot}}.)}
  \item{sorted }{a logical indicator of whether \code{y} has been sorted by the \code{\link{sorter}} function. If \code{TRUE}, \code{y.sorted} (and \code{times.sorted} if \code{IndTime} is \code{TRUE}) will not be \code{NULL}. }
  \item{ascending }{logical indicator. If \code{sorted} is \code{TRUE}, this will indicate whether sorting was done ascending. (default is \code{NULL} unless \code{\link{sorter}} has been applied to the \code{longCat} object).}
  \item{group }{a vector of the same length as the number of rows in \code{y} (default is \code{NULL} unless \code{\link{sorter}} has been applied to the \code{longCat} object along with a grouping variable).}
  \item{groupLabels }{a optional vector of character or numeric labels for the \code{group} variable (see \code{\link{sorter}}).}
  \item{order.y}{A matrix with identification (see input \code{id} above) and sorting information. Rows of the matrix correspond to rows of \code{y} and the columns are \code{id}, and order variable, and a variable representing the unique data patterns in \code{y}. The former is returned only if \code{id} is provided to \code{longCat}. The latter two are only returned by {\link{sorter}}. If only unique data patterns are desired, use \code{\link{makePatterns}}; see \code{example(makePatterns)}.}
  \item{events}{A matrix of events.}
  \item{event.times}{A matrix of event times.}
  \item{eventLabels}{A vector of event labels.}
%% ...
}
\references{
Tueller, S. J., Van Dorn, R. A., & Bobashev, G. V. (2016). Visualization of categorical longitudinal and times series data (Report No. MR-0033-1602). Research Triangle Park, NC: RTI Press. http://www.rti.org/publication/visualization-categorical-longitudinal-and-times-series-data}
\author{Stephen Tueller}

\seealso{
\code{\link{longCatPlot}} to plot \code{longCat} objects created by the \code{\link{longCat}} function.
}
\examples{
# create the longcat object similar to Figure 2 in Tueller (2016)
times <- c(1,100,200,300,400,500,600)
f2lc <- longCat(example2cat, times)

# object summary
summary(f2lc)

# compare growth curves to longCat
par(mfrow=c(1,2), bg='cornsilk3')
longContPlot(example2cat, times, ylim=c(1,5), 
  main='Growth Curves', ylab='', xlab='Days')
longCatPlot(f2lc, lwd=4, main='Horizontal Line Plot', colScheme='heat', legendBuffer=.2)
par(mfrow=c(1,1), bg='transparent')

# illustrate individually varying times of observation 
set.seed(642531)
y <- matrix(sample(1:5, 500, replace=TRUE), 100, 5)
set.seed(963854)
times <- matrix(runif(600, 1, 3), 100, 6)
# times must be cumulative
times <- t(apply(times, 1, cumsum))
lc <- longCat(y, times=times)
par(mfrow=c(1,1), bg='cornsilk3', mar=c(5.1, 4.1, 4.1, 10.1), xpd=TRUE)
cols <- longCatPlot(lc, legendBuffer=0, groupBuffer=0, 
	main='Individually Varying Times of Observation')
legend(15.5, 100, legend=lc$Labels, lty=1, col=cols, lwd=2)
par(bg='transparent', mar = c(5, 4, 4, 2) + 0.1, xpd=FALSE)

# illustrate the adding event indicators
set.seed(45962)
events <- matrix(sample(1:3, 200, replace=TRUE), 100, 2)
set.seed(23498)
event.times <- matrix(sample(c(times), 200, replace=FALSE), 100, 2)
labels <- c('Street', 'Drug Tx', 'Jail', 'Prison', 'Unknown')
eventLabels=c('Arrest', 'Drug Test', 'Hearing')
lc <- longCat(y, times=times, Labels=labels,  
              events=events, event.times=event.times, 
              eventLabels=eventLabels)
par(mfrow=c(1,1), bg='cornsilk3', mar=c(5.1, 4.1, 4.1, 12.1), xpd=TRUE)
cols <- longCatPlot(lc, legendBuffer=0, groupBuffer=0, 
                    main='Superimpose Events Over States')
legend(15.5, 100, legend=lc$Labels, lty=1, col=cols, lwd=2)
legend(15.5, 40, legend=lc$eventLabels, pch=1:length(lc$eventLabels))
par(bg='transparent', mar = c(5, 4, 4, 2) + 0.1, xpd=FALSE)

\dontrun{
# illustrate handling non time-ordered input (e.g., factor analysis data)
y <- matrix(sample(c('1', '2', '3', '4', '5'), 500, replace=TRUE), 100, 5)
lc <- longCat(y)
par(mfrow=c(1,1), bg='cornsilk3', mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
cols <- longCatPlot(lc, legendBuffer=0)
legend(6, 100, legend=lc$factors, lty=1, col=cols, lwd=2)
par(bg='transparent', mar = c(5, 4, 4, 2) + 0.1, xpd=FALSE)

# illustrate plotting with more than 9 categories 
# (a warning is issued) 
y <- matrix(sample(1:18, 500, replace=TRUE), 100, 5)
lc <- longCat(y)
par(mfrow=c(1,1), bg='cornsilk3', mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
cols <- longCatPlot(lc, legendBuffer=0)
legend(6, 100, legend=lc$factors, lty=1, col=cols, lwd=2)
par(bg='transparent', mar = c(5, 4, 4, 2) + 0.1, xpd=FALSE)
}

}