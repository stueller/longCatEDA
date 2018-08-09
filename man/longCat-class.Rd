\name{longCat-class}
\Rdversion{0.31}
\docType{class}
\alias{longCat-class}

\title{Class \code{"longCat"}}
\description{
An object of class \code{\link{longCat}.} 
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("longCat", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{y}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{y.sorted}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{dim}:}{Object of class \code{"integer"} ~~ }
    \item{\code{times}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{times.sorted}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{Labels}:}{Object of class \code{"character"} ~~ }
    \item{\code{factors}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{IndTime}:}{Object of class \code{"logical"} ~~ }
    \item{\code{nfactors}:}{Object of class \code{"integer"} ~~ }
    \item{\code{sorted}:}{Object of class \code{"logical"} ~~ }
    \item{\code{ascending}:}{Object of class \code{"logical"} ~~ }
    \item{\code{group}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{group.sorted}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{groupLabels}:}{Object of class \code{"character"} ~~ }
    \item{\code{order.y}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{order.y.sorted}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{events}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{event.times}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{events.sorted}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{event.times.sorted}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{eventLables}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "longCat")}: ... }
	 }
}
\references{
Tueller, S. J., Van Dorn, R. A., & Bobashev, G. V. (2016). Visualization of categorical longitudinal and times series data (Report No. MR-0033-1602). Research Triangle Park, NC: RTI Press. http://www.rti.org/publication/visualization-categorical-longitudinal-and-times-series-data}
\examples{
showClass("longCat")
}
\keyword{classes}
