# ggplot2 fortify() methods, so that ggplot() takes longCat and longCont
# objects as data. Registered when ggplot2 (suggested) is installed.

#' Use longCat and longCont objects as ggplot2 data
#'
#' `fortify()` methods that let [ggplot2::ggplot()] and its layers take a
#' [longCat()] or [longCont()] object as their data, e.g.
#' `ggplot(lc, aes(...))`. ggplot2 calls them itself; they are available
#' when ggplot2 is installed.
#'
#' For a `longCat` object, the data are those of
#' [as.data.frame()][as.data.frame.longCat()], ready to plot: an unsorted
#' object is first sorted with [sorter()], as [plot.longCat()] does (unless
#' `sort = FALSE`, when the cases keep their original order), and missing
#' states are left out. Columns giving the vertical position of each case,
#' as in [plot.longCat()], are added:
#'
#' * `row`: the plot row, counted from the bottom (for unsorted data, the
#'   row of `y`).
#' * `y`: the center of the case's line or, with `weights`, of its band.
#' * `ymin`, `ymax`: the edges of the case's band, a height of 1 without
#'   weights or proportional to the weight with them.
#'
#' So `aes(x = start, xend = stop, y = y, yend = y, colour = state)` with
#' [ggplot2::geom_segment()] draws lines, and `aes(xmin = start, xmax =
#' stop, ymin = ymin, ymax = ymax, fill = state)` with
#' [ggplot2::geom_rect()] draws bands. Give `what = "events"` to
#' [ggplot2::ggplot()] for the events, or use
#' `fortify(lc, what = "events")` as the data of a layer.
#'
#' For a `longCont` object, the data are those of
#' [as.data.frame()][as.data.frame.longCat()]: `id`, `occasion`, `time`, and
#' `value`.
#'
#' @param model An object of class `longCat` or `longCont`.
#' @param data Unused; included for consistency with the generic.
#' @param what `"states"` (the default) or `"events"`, as in
#'   [as.data.frame.longCat()].
#' @param sort Logical. Sort an unsorted object with [sorter()] first?
#'   Default is `TRUE`.
#' @param na.rm Logical. Leave out missing states or values? Default is
#'   `TRUE` for `longCat` objects (missing states are not drawn) and
#'   `FALSE` for `longCont` objects (missing values break the lines, as in
#'   [plot.longCont()]).
#' @param ... Unused; unknown arguments are an error.
#' @return A data frame.
#' @seealso [as.data.frame.longCat()]; [autoplot.longCat()] for a complete
#'   plot; [scale_colour_longcat()] for the colors of [plot.longCat()].
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'   lc <- longCat(example3)
#'   print(ggplot(lc, aes(x = start, xend = stop, y = y, yend = y,
#'                        colour = state)) +
#'           geom_segment() +
#'           scale_colour_longcat())
#'
#'   # weighted bands, with events from a second call
#'   set.seed(1)
#'   events <- matrix(sample(1:2, 200, TRUE), 100, 2)
#'   event.times <- matrix(runif(200, 0, 6), 100, 2)
#'   lcw <- longCat(example3, weights = rexp(100), events = events,
#'                  event.times = event.times)
#'   print(ggplot(lcw) +
#'           geom_rect(aes(xmin = start, xmax = stop, ymin = ymin,
#'                         ymax = ymax, fill = state)) +
#'           geom_point(aes(x = time, y = y, shape = event),
#'                      data = fortify(lcw, what = "events")) +
#'           scale_fill_longcat())
#'
#'   # growth curves
#'   print(ggplot(longCont(example2cont), aes(time, value, group = id)) +
#'           geom_line())
#' }
#' @md
#' @name fortify.longCat
NULL

#' @rdname fortify.longCat
#' @md
#' @exportS3Method ggplot2::fortify
fortify.longCat <- function(model, data, what = c("states", "events"),
                            sort = TRUE, na.rm = TRUE, ...) {
  check_no_dots("fortify() of a longCat object", ...)
  validate_longCat(model, "model")
  what <- match.arg(what)
  if (!is_flag(sort)) stop("`sort` must be TRUE or FALSE.", call. = FALSE)
  if (!is_flag(na.rm)) stop("`na.rm` must be TRUE or FALSE.", call. = FALSE)
  if (sort && !model$sorted) {
    model <- sorter(model, num = anyNA(model$y) || model$IndTime)
  }
  positioned_table(model, what, na.rm)
}

#' @rdname fortify.longCat
#' @md
#' @exportS3Method ggplot2::fortify
fortify.longCont <- function(model, data, na.rm = FALSE, ...) {
  check_no_dots("fortify() of a longCont object", ...)
  as.data.frame(model, na.rm = na.rm)
}

# The states (what = "states") or events of the longCat object `lc` in long
# format, as in as.data.frame(), with each case's plot row `row` (for an
# unsorted object, its row of `y`) and the vertical extent of its line or
# band, as in plot.longCat(): `y` at the center, `ymin` and `ymax` at the
# edges.
positioned_table <- function(lc, what, na.rm = TRUE) {
  cases <- case_table(lc, lc$sorted)
  if (is.null(cases$row)) {
    rest <- setdiff(names(cases), c("index", "id"))
    cases <- cbind(cases[c("index", "id")], row = cases$index, cases[rest])
  }
  d <- if (what == "states") {
    state_table(lc, cases, lc$sorted, na.rm)
  } else {
    event_table(lc, cases, lc$sorted)
  }
  pd <- plot_data(lc)
  height <- if (is.null(pd$row_height)) 1 else pd$row_height[d$row]
  d$y <- as.numeric(pd$row_center[d$row])
  d$ymin <- d$y - height / 2
  d$ymax <- d$y + height / 2
  d
}
