\name{sorter}
\alias{sorter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
General Sorting Function
}
\description{
A function to sort an \code{\link{longCat}} object created by \code{\link{longCat}}. \code{sorter} must be used directly when stratified plots of subgroups is desired, or when sorting other than the default sorting is desired. Otherwise, \code{sorter} is used internally with the defaults by \code{\link{longCatPlot}} if \code{lc$sorted=FALSE}. If an object has already been sorted (\code{lc$sort=TRUE}), \code{sorter} will not resort it, but will print a code example of how to use multiple sortings.
}
\usage{
sorter(lc, ascending = TRUE, whichColumns = NULL, num = TRUE,
                 mindur = NULL, igrpt = FALSE, customSort = NULL,
                 initFirst = FALSE, group = NULL, groupLabels = NULL,
                 ggap = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lc}{
an object of class \code{\link{longCat}} created by \code{\link{longCat}}.
}
  \item{ascending}{
logical - should sorting be done ascending. Default is \code{TRUE}.
}
  \item{whichColumns}{
a numeric list indicating which columns in \code{lc$y} should be used for sorting (.e.g., \code{c(1, 5, 7)}). Useful if, for example, an intervention occurs after data collection has started, and the user is not interested in sorting on pre-intervention observations.
}
  \item{num}{
see \code{\link{makePatterns}} for details.
}
  \item{mindur}{
see \code{\link{makePatterns}}.
}
  \item{igrpt}{should \code{sorter} (ig)nore (r)e(p)ea(t)ed values for each row in \code{lc$y} for sorting purposes? See \code{\link{norpt}}.}
  \item{customSort}{
a vector of the same length as the number of rows in \code{lc$y} providing a user defined variable on which to sort the data prior to secondarily applying the default sort. If \code{group} is not \code{NULL}, group will be sorted on prior to the \code{customSort} variable. Alternatively, \code{lc$y} can be sorted without calling \code{sorter} using \code{lc$y.sorted <- lc$y[o, ]} where \code{o} is the \code{\link{order}} (e.g., use \code{o <- \link{order}(customSort)}). The user must also set \code{lc$sorted <- TRUE} to prevent on-the-fly default sorting from being carried out by \code{\link{longCatPlot}}. Users unfamiliar with sorting in \code{R} should take care not to confuse \code{\link{order}} with \code{\link{sort}}. Default is \code{NULL}. If any values on \code{customSort} are missing, the function will return an error message.
}
  \item{initFirst}{if \code{customSort} is not \code{NULL}, setting \code{initFirst=TRUE} will sort on initial values prior to the custom sorting variable.}
  \item{group}{
a vector of the same length as the number of rows in \code{lc$y} indicating group membership. Default is \code{NULL}. If \code{group} is \code{NA}, corresponding rows in \code{lc$y} will be deleted prior to completing the sorting, and a warning indicating this has been done is printed to the console. If a large number of cases have missing data on the grouping variable, consider recoding the missings into their own group, e.g., \code{group[is.na(group)] <- -999} and add a missing label to \code{groupLabels}, e.g. \code{groupLabels=c('Missing', 'Group1', 'Group2', 'Etc.')}.
}
  \item{groupLabels}{
a vector of numeric or character labels of the same length as the number of unique values in \code{group}. Default is \code{NULL}. If \code{group} is not \code{NULL} and \code{groupLabels} is not provided, then the numeric values in \code{group} are used as the labels.
}
  \item{ggap}{
a number zero to 1. The proportion of blank rows to be plotted between groups when \code{group} is specified. The default of \code{NULL} is set to 0.05 when groups are present, 0.0 when there are no groups.}
}


\value{
Returns an object of class \code{longCat} where \code{lc$sorted=TRUE}. See \code{\link{longCat}} for values.
}
\references{
Tueller, S. J., Van Dorn, R. A., & Bobashev, G. V. (2016). Visualization of categorical longitudinal and times series data (Report No. MR-0033-1602). Research Triangle Park, NC: RTI Press. http://www.rti.org/publication/visualization-categorical-longitudinal-and-times-series-data}
\author{
Stephen Tueller
}

\seealso{
\code{\link{longCat}} and \code{\link{longCatPlot}}.
}
\examples{
### create a plot like that in Figure 3 from Tueller, Van Dorn, & Bobashev (2016)
par(mfrow=c(1,2), bg='cornsilk3')
times <- c(1,100,200,300,400,500,600)
f3lc <- longCat(example3, times); f3lc$sorted <- TRUE; f3lc$y.sorted <- f3lc$y
longCatPlot(f3lc, main='Unsorted', colScheme='heat', lwd=2, legendBuffer=.2)
f3lc <- longCat(example3, times)
longCatPlot(f3lc, main='Sorted', colScheme='heat', lwd=2, legendBuffer=.2)

### sort with a grouping variable and plot
par(mfrow=c(1,1), bg='cornsilk3', mar=c(5.1, 4.1, 4.1, 9.1), xpd=TRUE)
times <- c(1,100,200,300,400,500,600)
lc <- longCat(example3, times)
group <- sample(1:3, nrow(example3), replace=TRUE)
grouplc <- sorter(lc, group=group, groupLabels=1:3)
cols <- longCatPlot(grouplc, groupBuffer=.15, main='Grouped Data', colScheme='heat', 
                    lwd=2, legendBuffer=0)
legend(610, 130, legend=1:5, col=cols, lty=1, lwd=2)
par(bg='transparent', mar = c(5, 4, 4, 2) + 0.1, xpd=FALSE)

### using the sorted data from the previous plot, repeate using ggplot2 
#   following the example of Figure 4 of bdemarest's answer on 
#   https://stackoverflow.com/questions/11513149/
#   good-ways-to-visualize-longitudinal-categorical-data-in-r/
grouplc.df <- data.frame(id=1:nrow(grouplc$group.sorted), 
                group=grouplc$group.sorted[,1], grouplc$y.sorted)
grouplc.long <- reshape(grouplc.df,
                        varying = names(grouplc$y.sorted),
                        v.names = "score",
                        timevar = "time",
                        times = times[1:ncol(grouplc$y.sorted)],
                        direction = "long")
grouplc.long$score <- factor(grouplc.long$score)
grouplc.long$group <- factor(grouplc.long$group, level=3:1)
# remove NA's introduced using group option in sorter
grouplc.long <- na.omit(grouplc.long) 
library(ggplot2)
ggplot(grouplc.long, aes(x=time, y=id, fill=score)) + 
  geom_tile(colour="transparent") +
  scale_fill_manual(values=cols) +
  facet_grid(group ~ ., space="free_y", scales="free_y")

### sort with a grouping variable and events and plot
times <- c(1,100,200,300,400,500,600)
set.seed(45962)
events <- matrix(sample(1:3, nrow(example3)*2, replace=TRUE), nrow(example3), 2)
set.seed(23498)
event.times <- matrix(sample(min(times):max(times), nrow(example3)*2, replace=TRUE), 
nrow(example3), 2)
labels <- c('Street', 'Drug Tx', 'Jail', 'Prison', 'Unknown')
eventLabels=c('Arrest', 'Drug Test', 'Hearing')
eventlc <- longCat(example3, times=times, Labels=labels,  
              events=events, event.times=event.times, 
              eventLabels=eventLabels)
set.seed(4290)              
groupevent <- sample(1:3, nrow(example3), replace=TRUE)
groupeventlc <- sorter(eventlc, group=groupevent)            
par(mfrow=c(1,1), bg='cornsilk3', mar=c(5.1, 4.1, 4.1, 12.1), xpd=TRUE)
cols <- longCatPlot(groupeventlc, legendBuffer=0, groupBuffer=0.15, 
                    main='Grouping and Events')
legend(610, 130, legend=groupeventlc$Labels, lty=1, col=cols, lwd=2)
legend(610, 60, legend=groupeventlc$eventLabels, 
       pch=1:length(groupeventlc$eventLabels))
par(bg='transparent', mar = c(5, 4, 4, 2) + 0.1, xpd=FALSE)

}

