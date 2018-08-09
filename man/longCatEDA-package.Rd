\name{longCatEDA-package}
\alias{longCatEDA-package}
\alias{longCatEDA}
\docType{package}
\title{
Plot Categorical Longitudinal and Time-Series Data
}
\description{
Package to implement horizontal line plots for categorical (ordinal or nominal) longitudinal or time-series data. This is done by mapping individuals to the vertical space (each horizontal line represents a participant), time (or repeated measures) to the horizontal space, categorical (or discrete) states as facets using color or shade, and events to points. Sorting individuals in the vertical space can reveal patterns in changes over time. 
}
\details{
\tabular{ll}{
Package: \tab longCatEDA\cr
Type: \tab Package\cr
Version: \tab 0.31\cr
Date: \tab 29Oct2016\cr
License: \tab GNU GPL\cr
Depends: \tab methods\cr
}
Example usage is \code{\link{longCatPlot}}(\code{\link{longCat}}(y)) where \code{y} is a matrix or data frame in wide format with participants in rows and repeated observations in columns. The function \code{\link{longCat}} returns an object of class \code{longCat} which can be plotted using \code{\link{longCatPlot}}. Options for sorting and/or stratifying by groups are implemented using \code{\link{sorter}} which returns a sorted and/or stratified object of class \code{longCat}. 
}
\author{
Stephen Tueller

Maintainer: Stephen Tueller <stueller@rti.org>
}
\references{
Tueller, S. J., Van Dorn, R. A., & Bobashev, G. V. (2016). Visualization of categorical longitudinal and times series data (Report No. MR-0033-1602). Research Triangle Park, NC: RTI Press. http://www.rti.org/publication/visualization-categorical-longitudinal-and-times-series-data}
\keyword{ package }
\examples{
par(bg='cornsilk3')
longCatPlot( longCat( example3 ) )
par(bg='transparent')
}
