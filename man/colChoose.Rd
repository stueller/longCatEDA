\name{colChoose}
\alias{colChoose}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Internal Function for Selecting Color Schemes Used by longCatPlot 
}
\description{
Internal function used by \code{\link{longCatPlot}}.
}
\usage{
colChoose(colScheme, nfactors, reverse = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{colScheme}{
can be one of
       \itemize{
		\item 'gray' = a grayscale spectrum
		\item 'rainbow' see \code{\link{rainbow}}
		\item 'heat' see \code{\link{heat.colors}}
		\item 'terrain' see \code{\link{terrain.colors}}
		\item 'topo' see \code{\link{topo.colors}}
		\item 'cm' see \code{\link{cm.colors}}
       }
No default is given in the function definition, but the default in \code{\link{longCatPlot}} passed to \code{colChoose} is \code{'heat'}.
}
  \item{nfactors}{
see \code{nfactors} in values returned by \code{\link{longCat}}.
}
  \item{reverse}{
logical - should color scheme be applied in reverse order to the levels of the categorical variable? Default is \code{FALSE}. See \code{reverse} input to \code{\link{longCatPlot}}.
}
}

\references{
Tueller, S. J., Van Dorn, R. A., & Bobashev, G. V. (2016). Visualization of categorical longitudinal and times series data (Report No. MR-0033-1602). Research Triangle Park, NC: RTI Press. http://www.rti.org/publication/visualization-categorical-longitudinal-and-times-series-data}
\author{Stephen Tueller}


\seealso{
\code{\link{longCatPlot}}.
}
\examples{
# color examples
par(mfrow=c(2,3), bg='wheat')
times <- c(1,100,200,300,400,500,600)
f3lc <- longCat( example3, times, Labels=rep('',5) )
longCatPlot(f3lc, main='colScheme=gray', colScheme='gray', 
  lwd=.1, ylab='', legendBuffer = .25)
longCatPlot(f3lc, main='colScheme=rainbow', colScheme='rainbow', 
  lwd=.1, ylab='', legendBuffer = .25)
longCatPlot(f3lc, main='colScheme=heat', colScheme='heat', 
  lwd=.1, ylab='', legendBuffer = .25)
longCatPlot(f3lc, main='colScheme=terrain', colScheme='terrain', 
  lwd=.1, ylab='', legendBuffer = .25)
longCatPlot(f3lc, main='colScheme=topo', colScheme='topo', 
  lwd=.1, ylab='', legendBuffer = .25)
longCatPlot(f3lc, main='colScheme=cm', colScheme='cm', 
  lwd=.1, ylab='', legendBuffer = .25)
par(mfrow=c(1,1), bg='transparent')

\dontrun{
# illustrate the use of colors from the package RColorBrewer
library(RColorBrewer)
par(mfrow=c(2,3), bg='cornsilk3')
longCatPlot(f3lc, main='RColorBrewer: Blues', cols=brewer.pal(f3lc$nfactors, "Blues"), 
            lwd=.1, ylab='', legendBuffer = .25)
longCatPlot(f3lc, main='RColorBrewer: Greens', cols=brewer.pal(f3lc$nfactors, "Greens"), 
            lwd=.1, ylab='', legendBuffer = .25)
longCatPlot(f3lc, main='RColorBrewer: PuBuGn', cols=brewer.pal(f3lc$nfactors, "PuBuGn"), 
            lwd=.1, ylab='', legendBuffer = .25)
longCatPlot(f3lc, main='RColorBrewer: YlOrRd', cols=brewer.pal(f3lc$nfactors, "YlOrRd"), 
            lwd=.1, ylab='', legendBuffer = .25)
longCatPlot(f3lc, main='RColorBrewer: Spectral', cols=brewer.pal(f3lc$nfactors, "Spectral"), 
            lwd=.1, ylab='', legendBuffer = .25)
longCatPlot(f3lc, main='RColorBrewer: Accent', cols=brewer.pal(f3lc$nfactors, "Accent"), 
            lwd=.1, ylab='', legendBuffer = .25)
par(mfrow=c(1,1), bg='transparent')

# illustrate the use of colors from the package colorspace
library(colorspace)
par(mfrow=c(2,3), bg='cornsilk3')
longCatPlot(f3lc, main='colorspace: rainbow_hcl', cols=rainbow_hcl(f3lc$nfactors), 
            lwd=.1, ylab='', legendBuffer = .25)
longCatPlot(f3lc, main='colorspace: sequential_hcl', cols=sequential_hcl(f3lc$nfactors), 
            lwd=.1, ylab='', legendBuffer = .25)
longCatPlot(f3lc, main='colorspace: heat_hcl', cols=heat_hcl(f3lc$nfactors), 
            lwd=.1, ylab='', legendBuffer = .25)
longCatPlot(f3lc, main='colorspace: terrain_hcl', cols=terrain_hcl(f3lc$nfactors), 
            lwd=.1, ylab='', legendBuffer = .25)
longCatPlot(f3lc, main='colorspace: diverge_hcl', cols=diverge_hcl(f3lc$nfactors), 
            lwd=.1, ylab='', legendBuffer = .25)
longCatPlot(f3lc, main='colorspace: diverge_hsv', cols=diverge_hsv(f3lc$nfactors), 
            lwd=.1, ylab='', legendBuffer = .25)
par(mfrow=c(1,1), bg='transparent')
}
}
