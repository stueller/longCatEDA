# ggplot2 versions of the plots, registered as autoplot() methods when
# ggplot2 is installed (it is only suggested).

#' Horizontal line plots with ggplot2
#'
#' `autoplot()` draws the plots of [plot.longCat()] and [plot.longCont()]
#' with ggplot2 and returns the `ggplot` object, so the plot can be changed
#' with further layers, scales, labels, and themes. `autoplot()` is the
#' ggplot2 function for drawing a complete plot of an object, as `plot()`
#' is in base graphics. It is available when ggplot2 is installed; call it
#' with `ggplot2::autoplot()` or, after `library(ggplot2)`, `autoplot()`.
#' This page is also found as `?longCat-ggplot2`; see
#' `vignette("longCatEDA-ggplot2")` for an overview of the ggplot2
#' features.
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
#' **Two variables.** With `y`, a second `longCat` object of the same cases,
#' the two variables are drawn side by side as facets of one plot, in the
#' same case order, as in `plot(x, y)` (see [plot.longCat()]): by the order
#' of `object` (`order = "first"`) or sorted on both (`order = "joint"`), with
#' the groups and weights of `object`. Each case's row is the same in both
#' panels. The options above apply to both panels; `y_args` sets those of
#' the second (e.g., `list(colScheme = "topo")`). The panels are titled
#' with the names of the objects (or `titles`), and the legend lists the
#' states of each, prefixed with its title.
#'
#' @param object An object of class `longCat` or `longCont`.
#' @param y Optional second `longCat` object with the same cases (the same
#'   IDs in the same order), drawn in a second panel. Default is `NULL`.
#' @param order With `y`: the shared case order, `"first"` (the default) or
#'   `"joint"`, as in [plot.longCat()].
#' @param y_args With `y`: a named list of options for the second panel,
#'   replacing those given for both: `cols`, `colScheme`, `reverse`, `lty`,
#'   `show_events`, `which.events`, `n.events`, `texclude`, and the alignment
#'   options.
#' @param titles With `y`: the titles of the two panels. By default, the
#'   names of `object` and `y` when they are plain names, otherwise `"x"`
#'   and `"y"`.
#' @param cols,colScheme,reverse The colors of the states: `cols`, one per
#'   state, or the scheme `colScheme` from [colChoose()] (default `"heat"`),
#'   reversed with `reverse = TRUE`; as in [plot.longCat()].
#' @param linewidth The width of the lines (ignored for weighted bands).
#'   Default is 0.5.
#' @param lty Line types for the states, one per state (recycled), as in
#'   [plot.longCat()]. Default is 1 (solid). Other line types are shown in
#'   the legend with the colors. Ignored for weighted bands.
#' @param show_events Logical. Plot the events as points? Default is `TRUE`.
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
#'   # aligned at the first time in state 5, using days 1 to 5
#'   print(ggplot2::autoplot(lc, which.state = 5, nth.state = 1,
#'                           texclude = c(1, 5), xlab = "Time Since State 5"))
#'
#'   # line types for grayscale
#'   print(ggplot2::autoplot(lc, colScheme = "gray", lty = c(1, 2)))
#'
#'   # two variables of the same people, with rows lined up
#'   set.seed(3)
#'   use <- longCat(pmax(pmin(example3 + matrix(sample(-1:1, 600, TRUE), 100),
#'                            5), 1))
#'   print(ggplot2::autoplot(s, use, y_args = list(colScheme = "topo"),
#'                           titles = c("Living situation", "Drug use")))
#'
#'   set.seed(1)
#'   print(ggplot2::autoplot(longCont(example2cont), jog = TRUE, alpha = 0.3))
#' }
#' @md
#' @name autoplot.longCat
#' @aliases longCat-ggplot2 ggplot2-longCat
NULL

#' @rdname autoplot.longCat
#' @md
#' @exportS3Method ggplot2::autoplot
autoplot.longCat <- function(object, y = NULL, cols = NULL, colScheme = "heat",
                             reverse = FALSE, linewidth = 0.5, lty = 1,
                             show_events = TRUE, which.events = NULL,
                             n.events = NULL, texclude = NULL,
                             which.state = NULL, nth.state = NULL,
                             not.state = FALSE, which.event = NULL,
                             nth.event = NULL, not.event = FALSE,
                             sort = TRUE, xlab = "Days", ylab = NULL,
                             order = c("first", "joint"), y_args = list(),
                             titles = NULL, ...) {
  check_no_dots("autoplot() of a longCat object", ...)
  validate_longCat(object, "object")
  if (!is_flag(sort)) stop("`sort` must be TRUE or FALSE.", call. = FALSE)
  # the options that can differ between the panels of two variables
  panel_args <- list(cols = cols, colScheme = colScheme, reverse = reverse,
                     lty = lty, show_events = show_events,
                     which.events = which.events,
                     n.events = n.events, texclude = texclude,
                     which.state = which.state, nth.state = nth.state,
                     not.state = not.state, which.event = which.event,
                     nth.event = nth.event, not.event = not.event)

  if (is.null(y)) {
    if (sort && !object$sorted) {
      object <- sorter(object, num = anyNA(object$y) || object$IndTime)
    }
    panels <- list(autoplot_panel(object, panel_args))
  } else {
    check_y_args(y_args)
    unknown <- setdiff(names(y_args), names(panel_args))
    if (length(unknown) > 0L) {
      stop(sprintf(paste("`y_args` can only set options of each panel (%s),",
                         "not %s."),
                   paste(names(panel_args), collapse = ", "),
                   paste(unknown, collapse = ", ")), call. = FALSE)
    }
    titles <- check_titles(titles, list(substitute(object), substitute(y)))
    pair <- pair_objects(object, y, sort, match.arg(order))
    second <- panel_args
    second[names(y_args)] <- y_args
    panels <- list(autoplot_panel(pair$x, panel_args),
                   autoplot_panel(pair$y, second))
    object <- pair$x
  }

  cases <- panels[[1]]$cases
  weighted <- !is.null(object$weights)
  if (is.null(ylab)) {
    ylab <- default_ylab(length(cases),
                         if (weighted) sum(object$weights[cases]))
  }
  draw_autoplot(panels, titles, weighted, linewidth) +
    ggplot2::labs(x = xlab, y = ylab)
}

# The data and scales of one panel of autoplot(): the states and events of
# the (sorted or unsorted) longCat object `lc` as drawn with the options
# `a` (see autoplot.longCat()), the plotted cases, the colors and line
# types of the states, and the tLabels breaks of the x-axis (if any).
autoplot_panel <- function(lc, a) {
  if (!is_flag(a$show_events)) {
    stop("`show_events` must be TRUE or FALSE.", call. = FALSE)
  }
  align <- a[c("which.state", "nth.state", "not.state", "which.event",
               "nth.event", "not.event")]
  pd <- prepare_plot_data(lc, a$show_events, a$which.events, a$n.events,
                          a$texclude, align)
  frames <- autoplot_data(use_prepared_data(lc, pd), pd$plot_events)
  list(states = frames$states, events = frames$events, cases = frames$cases,
       cols = state_colors(a$cols, a$colScheme, lc$nfactors,
                           a$reverse)[seq_len(lc$nfactors)],
       lty = rep_len(a$lty, lc$nfactors),
       breaks = time_breaks(lc, pd$aligned, a$texclude),
       aligned = pd$aligned)
}

# Titles of the panels of two variables: `titles`, or the names of the
# objects (`exprs`) when they are plain names, else "x" and "y"; made
# unique.
check_titles <- function(titles, exprs) {
  if (is.null(titles)) {
    titles <- pair_titles(exprs, fallback = c("x", "y"))
  } else if (!is.character(titles) || length(titles) != 2L || anyNA(titles)) {
    stop("`titles` must be two panel titles.", call. = FALSE)
  }
  make.unique(titles, sep = " ")
}

# The ggplot of one panel or, with two (and `titles`), of two panels side
# by side, as facets with the same rows.
draw_autoplot <- function(panels, titles, weighted, linewidth) {
  pair <- length(panels) == 2L
  # the states and events of all panels, keyed by panel and state (or
  # event) so that each panel keeps its own colors (or shapes)
  keyed <- lapply(seq_along(panels), function(i) {
    prefix <- if (pair) paste0(titles[i], ": ") else ""
    panel <- panels[[i]]
    states <- panel$states
    states$.key <- paste0(prefix, states$state)
    states$.panel <- titles[i]
    events <- panel$events
    if (!is.null(events)) {
      events$.key <- paste0(prefix, events$event)
      events$.panel <- titles[i]
    }
    list(states = states, events = events,
         state_keys = paste0(prefix, levels(states$state)),
         event_keys = if (!is.null(events)) paste0(prefix, levels(events$event)),
         drawn_events = if (!is.null(events)) unique(events$.key))
  })
  state_keys <- unlist(lapply(keyed, `[[`, "state_keys"))
  states <- bind_rows_common(lapply(keyed, `[[`, "states"))
  states$.key <- factor(states$.key, levels = state_keys)
  if (pair) states$.panel <- factor(states$.panel, levels = titles)
  cols <- stats::setNames(unlist(lapply(panels, `[[`, "cols")), state_keys)
  lty <- stats::setNames(unlist(lapply(panels, `[[`, "lty")), state_keys)

  .data <- ggplot2::.data
  aes <- ggplot2::aes
  p <- ggplot2::ggplot(states)
  p <- if (weighted) {
    p + ggplot2::geom_rect(aes(xmin = .data$start, xmax = .data$stop,
                               ymin = .data$ymin, ymax = .data$ymax,
                               fill = .data$.key)) +
      ggplot2::scale_fill_manual(values = cols, drop = FALSE, name = NULL)
  } else if (all(lty == 1)) {
    p + ggplot2::geom_segment(aes(x = .data$start, xend = .data$stop,
                                  y = .data$y, yend = .data$y,
                                  colour = .data$.key),
                              linewidth = linewidth) +
      ggplot2::scale_colour_manual(values = cols, drop = FALSE, name = NULL)
  } else {
    # line types by state, in the same legend as the colors
    p + ggplot2::geom_segment(aes(x = .data$start, xend = .data$stop,
                                  y = .data$y, yend = .data$y,
                                  colour = .data$.key,
                                  linetype = .data$.key),
                              linewidth = linewidth) +
      ggplot2::scale_colour_manual(values = cols, drop = FALSE, name = NULL) +
      ggplot2::scale_linetype_manual(values = lty, drop = FALSE, name = NULL)
  }

  events <- bind_rows_common(lapply(keyed, `[[`, "events"))
  if (!is.null(events) && nrow(events) > 0L) {
    # each event keeps the shape of its code in its panel; the legend shows
    # only the events drawn
    event_keys <- unlist(lapply(keyed, `[[`, "event_keys"))
    shapes <- stats::setNames(unlist(lapply(keyed, function(k) {
      seq_along(k$event_keys)
    })), event_keys)
    events$.key <- factor(events$.key, levels = event_keys)
    if (pair) events$.panel <- factor(events$.panel, levels = titles)
    drawn <- event_keys[event_keys %in% events$.key]
    p <- p + ggplot2::geom_point(aes(x = .data$time, y = .data$y,
                                     shape = .data$.key),
                                 data = events) +
      ggplot2::scale_shape_manual(values = shapes, breaks = drawn,
                                  drop = FALSE, name = NULL)
  }
  # the states' legend first, then the events'; with two panels, one
  # column of states per panel
  rows <- if (pair) max(lengths(lapply(keyed, `[[`, "state_keys")))
  state_guide <- ggplot2::guide_legend(order = 1, nrow = rows)
  p <- p + ggplot2::guides(colour = state_guide, fill = state_guide,
                           linetype = state_guide,
                           shape = ggplot2::guide_legend(order = 2))

  grouped <- "group" %in% names(states)
  if (grouped || pair) {
    p <- p + ggplot2::facet_grid(
      rows = if (grouped) ggplot2::vars(.data$group),
      cols = if (pair) ggplot2::vars(.data$.panel),
      scales = if (pair) "free" else "free_y", space = "free_y",
      switch = "y"
    )
  }
  # tLabels only when every panel would show the same ones
  breaks <- lapply(panels, `[[`, "breaks")
  if (!is.null(breaks[[1]]) &&
      all(vapply(breaks, identical, logical(1), breaks[[1]]))) {
    p <- p + ggplot2::scale_x_continuous(breaks = breaks[[1]],
                                         labels = names(breaks[[1]]))
  }
  p + ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     panel.grid.major.y = ggplot2::element_blank(),
                     panel.grid.minor.y = ggplot2::element_blank(),
                     legend.position = "top")
}

# The rows of the data frames in `frames` (NULLs dropped) in one data frame,
# with the columns they all have; NULL when there are none.
bind_rows_common <- function(frames) {
  frames <- Filter(Negate(is.null), frames)
  if (length(frames) == 0L) return(NULL)
  common <- Reduce(intersect, lapply(frames, names))
  out <- do.call(rbind, lapply(frames, function(f) f[common]))
  rownames(out) <- NULL
  out
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
