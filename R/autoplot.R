# ggplot2 versions of the plots, registered as autoplot() methods when
# ggplot2 is installed (it is only suggested).

#' Plot longCat and longCont objects with ggplot2
#'
#' `autoplot()` methods that draw the plots of [plot.longCat()] and
#' [plot.longCont()] with ggplot2 and return the `ggplot` object, so the
#' plot can be changed with further layers, scales, labels, and themes.
#' They are available when ggplot2 is installed; call them with
#' `ggplot2::autoplot()` or, after `library(ggplot2)`, `autoplot()`.
#'
#' The data are those of [as.data.frame()][as.data.frame.longCat()]. For a
#' `longCat` object, each case is a horizontal line (with `weights`, a band
#' with a height proportional to its weight) in the plot order of
#' [sorter()], colored by state, with the events as points. Groups are drawn
#' as facets, with the first group at the bottom as in [plot.longCat()].
#' The y-axis has no scale, since its positions are only plot rows.
#'
#' Features of [plot.longCat()] such as `texclude` and the alignment of
#' times have no counterpart here; filter or change the data of
#' [as.data.frame()][as.data.frame.longCat()] and build the plot with
#' [ggplot2::ggplot()] instead.
#'
#' @param object An object of class `longCat` or `longCont`.
#' @param cols,colScheme,reverse The colors of the states: `cols`, one per
#'   state, or the scheme `colScheme` from [colChoose()] (default `"heat"`),
#'   reversed with `reverse = TRUE`; as in [plot.longCat()].
#' @param linewidth The width of the lines (ignored for weighted bands).
#'   Default is 0.5.
#' @param events Logical. Plot the events as points? Default is `TRUE`.
#' @param sort Logical. Sort an unsorted object with [sorter()] first?
#'   Default is `TRUE`, as in [plot.longCat()].
#' @param xlab,ylab Labels of the axes. For `longCat` objects the defaults
#'   are those of [plot.longCat()].
#' @param ... For `longCat` objects, unused (an error if given). For
#'   `longCont` objects, further arguments to [ggplot2::geom_line()], such
#'   as `colour` or `alpha`.
#' @return A `ggplot` object.
#' @seealso [plot.longCat()] and [plot.longCont()] for the base graphics
#'   plots; [as.data.frame.longCat()] for the data.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   lc <- longCat(example3)
#'   print(ggplot2::autoplot(lc, xlab = "Time"))
#'
#'   # groups as facets, changed like any ggplot
#'   s <- sorter(lc, group = rep(c("Treatment", "Control"), 50))
#'   print(ggplot2::autoplot(s, colScheme = "topo") +
#'           ggplot2::theme_minimal() +
#'           ggplot2::theme(axis.text.y = ggplot2::element_blank()))
#'
#'   print(ggplot2::autoplot(longCont(example2cont), alpha = 0.3))
#' }
#' @md
#' @name autoplot.longCat
NULL

#' @rdname autoplot.longCat
#' @md
#' @exportS3Method ggplot2::autoplot
autoplot.longCat <- function(object, cols = NULL, colScheme = "heat",
                             reverse = FALSE, linewidth = 0.5, events = TRUE,
                             sort = TRUE, xlab = "Days", ylab = NULL, ...) {
  check_no_dots("autoplot() of a longCat object", ...)
  validate_longCat(object, "object")
  if (!is_flag(events)) stop("`events` must be TRUE or FALSE.", call. = FALSE)
  if (!is_flag(sort)) stop("`sort` must be TRUE or FALSE.", call. = FALSE)
  cols <- state_colors(cols, colScheme, object$nfactors, reverse)
  if (sort && !object$sorted) {
    object <- sorter(object, num = anyNA(object$y) || object$IndTime)
  }

  frames <- autoplot_data(object, events)
  states <- frames$states
  weighted <- !is.null(object$weights)
  if (is.null(ylab)) {
    ylab <- default_ylab(length(unique(states$row)),
                         if (weighted) sum(object$weights[frames$cases]))
  }

  .data <- ggplot2::.data
  aes <- ggplot2::aes
  p <- ggplot2::ggplot(states)
  p <- if (weighted) {
    p + ggplot2::geom_rect(aes(xmin = .data$start, xmax = .data$stop,
                               ymin = .data$ymin, ymax = .data$ymax,
                               fill = .data$state)) +
      ggplot2::scale_fill_manual(values = cols[seq_len(object$nfactors)],
                                 drop = FALSE, name = NULL)
  } else {
    p + ggplot2::geom_segment(aes(x = .data$start, xend = .data$stop,
                                  y = .data$y, yend = .data$y,
                                  colour = .data$state),
                              linewidth = linewidth) +
      ggplot2::scale_colour_manual(values = cols[seq_len(object$nfactors)],
                                   drop = FALSE, name = NULL)
  }
  if (!is.null(frames$events) && nrow(frames$events) > 0L) {
    p <- p + ggplot2::geom_point(aes(x = .data$time, y = .data$y,
                                     shape = .data$event),
                                 data = frames$events) +
      ggplot2::scale_shape_manual(values = seq_along(object$eventLabels),
                                  drop = FALSE, name = NULL)
  }
  if ("group" %in% names(states)) {
    p <- p + ggplot2::facet_grid(rows = ggplot2::vars(.data$group),
                                 scales = "free_y", space = "free_y",
                                 switch = "y")
  }
  breaks <- time_breaks(object)
  if (!is.null(breaks)) {
    p <- p + ggplot2::scale_x_continuous(breaks = breaks, labels = object$tLabels)
  }
  p + ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank(),
                   legend.position = "top")
}

#' @rdname autoplot.longCat
#' @md
#' @exportS3Method ggplot2::autoplot
autoplot.longCont <- function(object, linewidth = 0.5, xlab = "Time",
                              ylab = NULL, ...) {
  validate_longCont(object, "object")
  d <- as.data.frame(object, na.rm = TRUE)
  if (nrow(d) == 0L) {
    stop("`object` has no non-missing values to plot.", call. = FALSE)
  }
  .data <- ggplot2::.data
  ggplot2::ggplot(d, ggplot2::aes(x = .data$time, y = .data$value,
                                  group = .data$id)) +
    ggplot2::geom_line(linewidth = linewidth, ...) +
    ggplot2::labs(x = xlab, y = ylab)
}

# The states and events of positioned_table(), with the groups in reverse
# order: facets run from the top, and the first group belongs at the
# bottom. Unsorted objects are drawn in their original order. `cases` are
# the rows of `y` plotted, for the weighted N.
autoplot_data <- function(lc, events) {
  facet_order <- function(d) {
    if (!is.null(d$group)) d$group <- factor(d$group, levels = rev(levels(d$group)))
    d
  }
  plotted <- if (lc$sorted) {
    lc$order.y.sorted[!is.na(lc$group.sorted[, 1]), 4]
  } else {
    seq_len(nrow(lc$y))
  }
  list(
    states = facet_order(positioned_table(lc, "states")),
    events = if (events && !is.null(lc$events)) {
      facet_order(positioned_table(lc, "events"))
    },
    cases = plotted
  )
}

# Breaks at the starts of the intervals, labelled with tLabels, when the
# object has them (every case then shares the times).
time_breaks <- function(lc) {
  if (is.null(lc$tLabels)) return(NULL)
  lc$times[1, -ncol(lc$times)]
}
