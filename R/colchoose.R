################################################################################
# function::colScheme - expandable bank of color schemes, currently
#   0 = 1:8 + 'darkgreen' (9==1, so you can't use 1:9)
#   1 = roygbiv
#   2 = orange to blue
#   3 = green to red
#   'gray' = a spectrum of greys
#   'oldheat' = a manual heat map spectrum
#   'coldheat' = a manual heat map starting with cold colors
#   'rainbow' see ?rainbow
#   'heat' see ?heat.colors
#   'terrain' see ?terrain.colors
#   'topo' see ?topo.colors
#    'cm' see ?cm.colors
#
#   in longCatPlot, cols can be supplied directly for user defined schemes
################################################################################
#' Internal Function for Selecting Color Schemes Used by \code{longCatPlot}
#'
#' Internal function used by \code{\link{longCatPlot}} to select color schemes
#' based on the specified parameters.
#'
#' @param colScheme A character string specifying the color scheme to use.
#' Options include:
#' \itemize{
#'   \item \code{"gray"}: A grayscale spectrum.
#'   \item \code{"rainbow"}: See \code{\link{rainbow}}.
#'   \item \code{"heat"}: See \code{\link{heat.colors}}.
#'   \item \code{"terrain"}: See \code{\link{terrain.colors}}.
#'   \item \code{"topo"}: See \code{\link{topo.colors}}.
#'   \item \code{"cm"}: See \code{\link{cm.colors}}.
#' }
#' Although no default is specified, \code{"heat"} is the default used when
#' passed to \code{colChoose} by \code{\link{longCatPlot}}.
#' @param nfactors The number of unique factors (levels) in the data.
#' This parameter helps determine the number of colors needed.
#' @param reverse Logical indicating whether the color scheme should be applied
#' in reverse order. Default is \code{FALSE}.
#'
#' @return Returns a vector of colors based on the specified color scheme,
#' number of factors, and whether the order is reversed.
#'
#' @examples
#' # Example usage within longCatPlot
#' times <- c(1,100,200,300,400,500,600)
#' f3lc <- longCat(example3, times, Labels=rep('',5))
#' longCatPlot(f3lc, main='colScheme=gray', colScheme='gray', lwd=.1, ylab='',
#' legendBuffer = .25)
#'
#' # More examples showing different color schemes
#' par(mfrow=c(2,3), bg='wheat')
#' longCatPlot(f3lc, main='colScheme=rainbow', colScheme='rainbow', lwd=.1,
#' ylab='', legendBuffer = .25)
#' longCatPlot(f3lc, main='colScheme=heat', colScheme='heat', lwd=.1, ylab='',
#' legendBuffer = .25)
#' par(mfrow=c(1,1), bg='transparent')
#'
#' @seealso \code{\link{longCatPlot}} for how this function is used within plotting.
#' @references Tueller, S. J., Van Dorn, R. A., & Bobashev, G. V. (2016).
#' Visualization of categorical longitudinal and times series data
#' (Report No. MR-0033-1602). Research Triangle Park, NC: RTI Press.
#' \url{http://www.rti.org/publication/visualization-categorical-longitudinal-and-times-series-data}
#' @author Stephen J. Tueller
#' @importFrom grDevices cm.colors gray heat.colors rainbow terrain.colors topo.colors
#' @importFrom graphics axis legend lines points text title
#' @importFrom methods is
#' @importFrom stats aggregate runif var
#' @export

colChoose <- function(colScheme, nfactors, reverse=FALSE)
{
  if(colScheme==0) cols <- c(1:8, 'darkgreen')
  if(colScheme==1){
    cols <- c("darkred",
              "darkorange",
              "darkgoldenrod2",
              "darkgreen",
              "lightblue",
              "darkblue",
              "blueviolet",
              "black",
              "hotpink")}
  if(colScheme==2){
    cols <- c("orange4",
              "orange",
              "olivedrab",
              "olivedrab1",
              "mediumorchid4",
              "mediumorchid1",
              "royalblue4",
              "royalblue1",
              "black")}
  if(colScheme==3){
    cols <- c("forestgreen",
              "green",
              "deeppink4",
              "hotpink1",
              "chocolate4",
              "darkorange",
              "red4",
              "orangered",
              "black")}
  if(colScheme=='gray'){cols <- gray(seq(0,.9,len=nfactors))}
  if(colScheme=='oldheat'){
    cols <- c("purple4",
              "royalblue",
              "paleturquoise3",
              "palegreen",
              "yellow",
              "orange",
              "orangered",
              "maroon",
              "red4")
  }
  if(colScheme=='coldheat'){
    cols <- c("blue4",
              "dodgerblue2",
              "green4",
              "darkseagreen3",
              "yellow4",
              "tan1",
              "orange",
              "orangered",
              "red2")
  }
  if(colScheme=='rainbow'){cols <- rainbow(9)}
  if(colScheme=='heat'){cols <- heat.colors(9, alpha = 1)[9:1]}
  if(colScheme=='terrain'){cols <- terrain.colors(9, alpha = 1)}
  if(colScheme=='topo'){cols <- topo.colors(9, alpha = 1)}
  if(colScheme=='cm'){cols <- cm.colors(9, alpha = 1)}

  # some finessing to make sure contrast is sufficient when nFactors < 9
  if(reverse) cols <- cols[9:1]
  if( colScheme > 0 )
  {
    if(nfactors <= 5 & nfactors > 3) cols <- cols[c(1,3,5,7,9)]
    if(nfactors <= 3) cols <- cols[c(1,5,9)]
  }
  return(cols)
}
