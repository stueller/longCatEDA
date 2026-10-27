#' Deprecated functions in longCatEDA
#'
#' These functions still work but will be removed in a future release. Use
#' the [plot()] methods instead:
#'
#' * `longCatPlot(lc, ...)` is now `plot(lc, ...)`; see [plot.longCat()].
#' * `longContPlot(y, times, ...)` is now `plot(longCont(y, times), ...)`;
#'   see [longCont()] and [plot.longCont()].
#'
#' @param lc An object of class `longCat` created by [longCat()].
#' @param y,times Data and observation times, as for [longCont()].
#' @param jog,xlim,ylim As for [plot.longCont()].
#' @param ... Further arguments passed to [plot.longCat()] or
#'   [plot.longCont()].
#' @return As for [plot.longCat()] and [plot.longCont()].
#' @md
#' @name longCatEDA-deprecated
#' @keywords internal
NULL

#' @rdname longCatEDA-deprecated
#' @md
#' @export
longCatPlot <- function(lc, ...) {
  .Deprecated(msg = "longCatPlot() is deprecated; use plot() instead.")
  plot.longCat(lc, y = NULL, ...)
}

#' @rdname longCatEDA-deprecated
#' @md
#' @export
longContPlot <- function(y, times = NULL, jog = FALSE, ylim = NULL, xlim = NULL,
                         ...) {
  .Deprecated(msg = paste("longContPlot() is deprecated; use",
                          "plot(longCont(y, times)) instead."))
  plot.longCont(longCont(y, times), jog = jog, xlim = xlim, ylim = ylim, ...)
}
