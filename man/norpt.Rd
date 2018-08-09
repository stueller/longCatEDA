\name{norpt}
\alias{norpt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to take out repeated observations in a vector of data for sorting purposes.
}
\description{
See the example.
}
\usage{
norpt(alist = c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alist}{
%%     ~~Describe \code{alist} here~~
}
}

\value{
A vector excluding repeated values with trailing \code{NA}'s to fill the vector to original length.
}
\references{
Tueller, S. J., Van Dorn, R. A., & Bobashev, G. V. (2016). Visualization of categorical longitudinal and times series data (Report No. MR-0033-1602). Research Triangle Park, NC: RTI Press. http://www.rti.org/publication/visualization-categorical-longitudinal-and-times-series-data}
\author{
Stephen Tueller
}

\seealso{
\code{igrpt} input to \code{\link{makePatterns}}
}
\examples{
(alist = c(1,2,2,3,3,3,4,4,4,4,5))
norpt(alist)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
