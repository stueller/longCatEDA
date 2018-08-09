\name{makePatterns}
\alias{makePatterns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Concatenate Multivariate Data into Numeric or Character Patterns
}
\description{
Function to concatenate the columns of a matrix or data frame for each row into a single character variable, which can optionally be reconverted to numeric. Called internally by \code{\link{sorter}}. For example, a row of a matrix containing \code{c(1, 2, 3, 5)} will be concatenated to \verb{"1235"}. 
}
\usage{
makePatterns(dat, times, num = TRUE, mindur = NULL, igrpt = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
a matrix or data frame such as \code{lc$y} from an \code{\link{longCat}} object created by \code{\link{longCat}}.
}
  \item{times}{
see \code{times} in \code{\link{longCat}}.
}
  \item{num}{
logical indicator, should a numeric version of the concatenate rows be return. Default is \code{TRUE}. When \code{num=TRUE}, the return is rescaled by moving a decimal point between the first and second digits. This ensures that, under different numbers of observations or missing data, ordering is not unduly impacted by patterns of missing data. Users are encouraged to try sorting with \code{num=TRUE} and \code{num=FALSE} when experimenting to find the sorting that leads to the clearest plot. When \code{lc$sorted=FALSE} and there is no missing data in \code{lc$y} and \code{lc$IntTime=FALSE}, \code{\link{longCatPlot}} will change \code{num} to \code{FALSE}. 
}
  \item{mindur}{
minimum duration. If \code{times} is a matrix or data frame of individually varying times of observation of the same dimension as \code{dat}, selecting \code{mindur > 0} results in all cells in \code{y} corresponding to cells in \code{times - times[,1] < mindur} being changed to \code{NA} (where \code{times - times[,1]} changes the \code{times} from a matrix of observed times to a matrix of durations for each state in \code{dat}). This minimizes the effect of short durations on the sorting algorithm in \code{\link{sorter}}. Default is \code{NULL}.
}
  \item{igrpt}{Option to ignore repeated values when sorting, allowing the sorting algorithm in \code{\link{sorter}} to smooth over regions of no change for each row in \code{lc$y}. Default is \code{FALSE}. See \code{\link{norpt}}.}
}
\value{
	\item{out }{A vector of patterns of length \code{nrow(dat)}}.

}
\references{
Tueller, S. J., Van Dorn, R. A., & Bobashev, G. V. (2016). Visualization of categorical longitudinal and times series data (Report No. MR-0033-1602). Research Triangle Park, NC: RTI Press. http://www.rti.org/publication/visualization-categorical-longitudinal-and-times-series-data}
\author{Stephen Tueller}

\seealso{
\code{\link{sorter}}
}
\examples{
# create an arbitrary matrix and demonstrate
temp <- matrix( sample(1:9, 40, replace=TRUE), 10, 4)
print(temp)
makePatterns(temp, num=FALSE)

# examine the unique patterns of data
bindat <- matrix( sample(0:1, 500, replace=TRUE), 100, 5)
uniquePatterns <- makePatterns( bindat, num=FALSE)
as.matrix( table( uniquePatterns ) )

}

