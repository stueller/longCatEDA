# ggplot2 colour and fill scales using the colChoose() schemes.

#' ggplot2 colour scales from the longCatEDA color schemes
#'
#' Discrete ggplot2 scales that color states with a scheme from
#' [colChoose()], so plots built with [ggplot2::ggplot()] from
#' [as.data.frame()][as.data.frame.longCat()] or the data frame method of
#' [sorter()] get the same colors as [plot.longCat()]. The number of colors
#' is the number of states in the data, so it need not be given.
#'
#' States must be mapped as a factor or text (e.g., `colour = state` for the
#' `state` column of [as.data.frame.longCat()], or `colour =
#' factor(status)` for numeric states). With `drop = FALSE` (the default),
#' the colors are chosen for all the levels of a factor, whether or not
#' they occur in the data: as in [plot.longCat()], each state then keeps its
#' color in a subset of the cases, and every state appears in the legend.
#' With `drop = TRUE`, colors are chosen for the states present only.
#'
#' These functions need the ggplot2 package, which longCatEDA suggests but
#' does not require.
#'
#' @param colScheme A color scheme from [colChoose()]. Default is `"heat"`.
#' @param reverse Logical. Reverse the color scheme? Default is `FALSE`.
#' @param ... Further arguments passed to [ggplot2::discrete_scale()], such
#'   as `name`, `labels`, or `na.value`.
#' @param drop Logical. Leave out unused factor levels? Default is `FALSE`
#'   (see Details).
#' @param aesthetics The aesthetics the scale applies to. Default is
#'   `"colour"` for `scale_colour_longcat()` and `"fill"` for
#'   `scale_fill_longcat()`.
#' @return A ggplot2 scale, to add to a plot with `+`.
#' @seealso [colChoose()] for the schemes; [autoplot.longCat()] for a
#'   complete plot.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   d <- as.data.frame(sorter(longCat(example3)), na.rm = TRUE)
#'   p <- ggplot2::ggplot(d, ggplot2::aes(x = start, xend = stop, y = row,
#'                                         yend = row, colour = state)) +
#'     ggplot2::geom_segment()
#'   print(p + scale_colour_longcat())
#'   print(p + scale_colour_longcat("topo", reverse = TRUE, name = "State"))
#'
#'   # fill, e.g. for tiles
#'   print(ggplot2::ggplot(d, ggplot2::aes(x = start, y = row, fill = state)) +
#'           ggplot2::geom_tile() +
#'           scale_fill_longcat("gray"))
#' }
#' @md
#' @export
scale_colour_longcat <- function(colScheme = "heat", reverse = FALSE, ...,
                                 drop = FALSE, aesthetics = "colour") {
  longcat_scale(aesthetics, colScheme, reverse, drop, ...)
}

#' @rdname scale_colour_longcat
#' @md
#' @export
scale_color_longcat <- scale_colour_longcat

#' @rdname scale_colour_longcat
#' @md
#' @export
scale_fill_longcat <- function(colScheme = "heat", reverse = FALSE, ...,
                               drop = FALSE, aesthetics = "fill") {
  longcat_scale(aesthetics, colScheme, reverse, drop, ...)
}

longcat_scale <- function(aesthetics, colScheme, reverse, drop, ...) {
  if (!has_ggplot2()) {
    stop("The longCatEDA scales need the ggplot2 package.", call. = FALSE)
  }
  if (!is_flag(drop)) stop("`drop` must be TRUE or FALSE.", call. = FALSE)
  # check the scheme now, rather than when the plot is drawn
  colChoose(colScheme, 1, reverse)
  palette <- function(n) {
    if (n == 0) character(0) else colChoose(colScheme, n, reverse)
  }
  ggplot2::discrete_scale(aesthetics, palette = palette, drop = drop, ...)
}

# Separate so that tests can pretend ggplot2 is not installed.
has_ggplot2 <- function() requireNamespace("ggplot2", quietly = TRUE)
