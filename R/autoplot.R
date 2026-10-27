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
#' As in [plot.longCat()], the events can be selected (`which.events`,
#' `n.events`), the plot cut to a time range (`texclude`), and the times
#' aligned at the nth occurrence of a state or event (see [alignTime()]).
#' The cases are sorted before the times are aligned, and `texclude` cuts
#' the original times, before they are aligned. With aligned times, the
#' x-axis shows the aligned times rather than `tLabels`.
#'
#' @param object An object of class `longCat` or `longCont`.
#' @param cols,colScheme,reverse The colors of the states: `cols`, one per
#'   state, or the scheme `colScheme` from [colChoose()] (default `"heat"`),
#'   reversed with `reverse = TRUE`; as in [plot.longCat()].
#' @param linewidth The width of the lines (ignored for weighted bands).
#'   Default is 0.5.
#' @param lty Line types for the states, one per state (recycled), as in
#'   [plot.longCat()]. Default is 1 (solid). Other line types are shown in
#'   the legend with the colors. Ignored for weighted bands.
#' @param events Logical. Plot the events as points? Default is `TRUE`.
#' @param which.events,n.events Codes of the events to plot, and how many
#'   of each case's first events, as in [plot.longCat()]. Default is all.
#' @param texclude A vector of two times: only the part of the plot between
#'   them is drawn, as in [plot.longCat()].
#' @param which.state,nth.state,not.state,which.event,nth.event,not.event
#'   Align the times at the nth occurrence of a state or event; see
#'   [alignTime()].
#' @param sort Logical. Sort an unsorted object with [sorter()] first?
#'   Default is `TRUE`, as in [plot.longCat()].
#' @param jog For `longCont` objects: jitter each case's line vertically, as
#'   in [plot.longCont()]: `FALSE` (the default), `TRUE` (by up to 0.25), or
#'   the largest shift.
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
#'   # aligned at the first time in state 5, between -3 and 3
#'   print(ggplot2::autoplot(lc, which.state = 5, nth.state = 1,
#'                           texclude = c(-3, 3), xlab = "Time Since State 5"))
#'
#'   # line types for grayscale
#'   print(ggplot2::autoplot(lc, colScheme = "gray", lty = c(1, 2)))
#'
#'   set.seed(1)
#'   print(ggplot2::autoplot(longCont(example2cont), jog = TRUE, alpha = 0.3))
#' }
#' @md
#' @name autoplot.longCat
NULL

#' @rdname autoplot.longCat
#' @md
#' @exportS3Method ggplot2::autoplot
autoplot.longCat <- function(object, cols = NULL, colScheme = "heat",
                             reverse = FALSE, linewidth = 0.5, lty = 1,
                             events = TRUE, which.events = NULL,
                             n.events = NULL, texclude = NULL,
                             which.state = NULL, nth.state = NULL,
                             not.state = FALSE, which.event = NULL,
                             nth.event = NULL, not.event = FALSE,
                             sort = TRUE, xlab = "Days", ylab = NULL, ...) {
  check_no_dots("autoplot() of a longCat object", ...)
  validate_longCat(object, "object")
  if (!is_flag(events)) stop("`events` must be TRUE or FALSE.", call. = FALSE)
  if (!is_flag(sort)) stop("`sort` must be TRUE or FALSE.", call. = FALSE)
  cols <- state_colors(cols, colScheme, object$nfactors, reverse)
  lty <- rep_len(lty, object$nfactors)
  if (sort && !object$sorted) {
    object <- sorter(object, num = anyNA(object$y) || object$IndTime)
  }

  pd <- prepare_plot_data(object, events, which.events, n.events, texclude,
                          list(which.state = which.state, nth.state = nth.state,
                               not.state = not.state,
                               which.event = which.event, nth.event = nth.event,
                               not.event = not.event))
  frames <- autoplot_data(use_prepared_data(object, pd), pd$plot_events)
  states <- frames$states
  weighted <- !is.null(object$weights)
  if (is.null(ylab)) {
    ylab <- default_ylab(length(frames$cases),
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
  } else if (all(lty == 1)) {
    p + ggplot2::geom_segment(aes(x = .data$start, xend = .data$stop,
                                  y = .data$y, yend = .data$y,
                                  colour = .data$state),
                              linewidth = linewidth) +
      ggplot2::scale_colour_manual(values = cols[seq_len(object$nfactors)],
                                   drop = FALSE, name = NULL)
  } else {
    # line types by state, in the same legend as the colors
    p + ggplot2::geom_segment(aes(x = .data$start, xend = .data$stop,
                                  y = .data$y, yend = .data$y,
                                  colour = .data$state,
                                  linetype = .data$state),
                              linewidth = linewidth) +
      ggplot2::scale_colour_manual(values = cols[seq_len(object$nfactors)],
                                   drop = FALSE, name = NULL) +
      ggplot2::scale_linetype_manual(values = lty, drop = FALSE, name = NULL)
  }
  if (!is.null(frames$events) && nrow(frames$events) > 0L) {
    # each event keeps the shape of its code; the legend shows only the
    # events drawn
    shapes <- stats::setNames(seq_along(object$eventLabels), object$eventLabels)
    drawn <- levels(droplevels(frames$events$event))
    p <- p + ggplot2::geom_point(aes(x = .data$time, y = .data$y,
                                     shape = .data$event),
                                 data = frames$events) +
      ggplot2::scale_shape_manual(values = shapes, breaks = drawn,
                                  drop = FALSE, name = NULL) +
      # the states' legend first, then the events'
      ggplot2::guides(colour = ggplot2::guide_legend(order = 1),
                      fill = ggplot2::guide_legend(order = 1),
                      linetype = ggplot2::guide_legend(order = 1),
                      shape = ggplot2::guide_legend(order = 2))
  }
  if ("group" %in% names(states)) {
    p <- p + ggplot2::facet_grid(rows = ggplot2::vars(.data$group),
                                 scales = "free_y", space = "free_y",
                                 switch = "y")
  }
  breaks <- time_breaks(object, pd$aligned, texclude)
  if (!is.null(breaks)) {
    p <- p + ggplot2::scale_x_continuous(breaks = breaks, labels = names(breaks))
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
autoplot.longCont <- function(object, linewidth = 0.5, jog = FALSE,
                              xlab = "Time", ylab = NULL, ...) {
  validate_longCont(object, "object")
  object$y <- jog_cases(object$y, jog_amount(jog))
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
# bottom. Unsorted objects are drawn in their original order. Intervals
# of no length (e.g., outside `texclude`) and events without a time are
# left out. `cases` are the rows of `y` plotted, for the weighted N.
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
  states <- positioned_table(lc, "states")
  states <- states[states$start < states$stop, , drop = FALSE]
  list(
    states = facet_order(states),
    events = if (events) {
      e <- positioned_table(lc, "events")
      facet_order(e[!is.na(e$time), , drop = FALSE])
    },
    cases = plotted
  )
}

# `lc` with the times and events of `pd`, from prepare_plot_data(), in
# place of its own (sorted ones, when it is sorted).
use_prepared_data <- function(lc, pd) {
  suffix <- if (lc$sorted) ".sorted" else ""
  lc[paste0(c("times", "events", "event.times"), suffix)] <-
    list(pd$times, pd$events, pd$event.times)
  lc
}

# Breaks at the starts of the intervals, named with tLabels, when the
# object has them (every case then shares the times) and the times are
# not aligned; only those within `texclude`, if given.
time_breaks <- function(lc, aligned = FALSE, texclude = NULL) {
  if (is.null(lc$tLabels) || aligned) return(NULL)
  starts <- stats::setNames(lc$times[1, -ncol(lc$times)], lc$tLabels)
  if (!is.null(texclude)) {
    starts <- starts[starts >= texclude[1] & starts <= texclude[2]]
  }
  starts
}
