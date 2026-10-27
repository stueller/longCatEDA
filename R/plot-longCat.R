#' Horizontal line plot of a longCat object
#'
#' Plots each case as a horizontal line (cases in rows, time on the x-axis),
#' colored by state, with events drawn as points. Unsorted objects are
#' sorted on the fly with [sorter()]; sort them yourself to use groups or
#' other sorting options.
#'
#' When the object has `weights` (see [longCat()]), each case is drawn as a
#' band whose height is proportional to its weight instead of as a line, so
#' the plot shows the weighted distribution of the states; `lwd` is then
#' not used, and `lty` applies only to the legend.
#'
#' **Two variables.** With `y`, the two objects are drawn in two panels
#' (side by side, unless a multi-figure layout is already set with
#' [par()]) with the same plot rows. All arguments apply to both panels
#' except those in `y_args`. Panel titles default to the objects' names
#' (when they are given as names), and the second panel has no y-axis
#' label. The groups, blank rows, and weights of `x` are used for both. To
#' compare individuals, look along a row; to compare groups, sort `x` with
#' groups first.
#'
#' @param x An object of class `longCat` created by [longCat()].
#' @param y Optional second `longCat` object with the same cases (the same
#'   IDs in the same order), e.g., a second variable measured on the same
#'   participants. Both are then plotted side by side with the same case
#'   order, so that each case's row lines up across the two panels (see
#'   Details).
#' @param xlab Label for the x-axis. Default is `"Days"`.
#' @param ylab Label for the y-axis. The default, `NULL`, gives "Each Line
#'   Represents a Participant, N = " followed by the number of cases plotted
#'   (and, with weights, their total weight).
#' @param cols Colors for the states, one per state (more are ignored).
#'   Overrides `colScheme`.
#' @param colScheme A color scheme from [colChoose()]. Default is `"heat"`.
#' @param reverse Logical. Reverse the color scheme? Default is `FALSE`.
#' @param lwd Width of the horizontal lines. Default is 0.5.
#' @param lcex Character expansion for the legend text. Default is 1.
#' @param llwd Width of the lines in the legend. Default is 3.
#' @param legendBuffer Proportion of the plot height (relative to the number
#'   of rows) kept free above the lines for the legend, between 0 and 1.
#'   With 0, no legend is drawn. Default is 0.12.
#' @param groupBuffer Proportion of the plot width (relative to the time
#'   range) kept free left of the lines for group labels, between 0 and 1.
#'   Default is 0.
#' @param groupRotation Rotation of the group labels in degrees. Default is
#'   90.
#' @param gcex Character expansion for the group labels. Default is 1.
#' @param seg.len Length of the lines in the legend. Default is 1.
#' @param xlas,xcex Orientation (see `las` in [par()]) and character
#'   expansion of the x-axis labels. Defaults are 0 and 1.
#' @param ecex Character expansion for the event points. Default is 0.5.
#' @param event.col Colors for the event points, one per event (recycled).
#'   Default is 1 (black).
#' @param plot.events Logical. Plot events? Default is `TRUE`.
#' @param which.events Codes of the events to plot (positions in
#'   `x$eventLabels`). Default is all events.
#' @param n.events Plot only each case's first `n.events` events (after
#'   `which.events`). Default is all.
#' @param event.pch Plotting characters for the events, one per event
#'   (recycled; see [points()]). The default uses the event codes, i.e.,
#'   `seq_along(lc$eventLabels)`.
#' @param texclude A vector of two times: only the part of the plot between
#'   them is drawn. Intervals are cut at these times and events outside
#'   them are dropped.
#' @param sort Logical. Sort an unsorted object before plotting? Default is
#'   `TRUE`. Objects already sorted with [sorter()] are plotted in their
#'   sorted order either way.
#' @param which.state,nth.state,not.state,which.event,nth.event,not.event
#'   Align the times at the nth occurrence of a state or event; see
#'   [alignTime()].
#' @param lty Line types for the states, one per state (recycled; see
#'   `lty` in [par()]). Default is 1 (solid).
#' @param xlim,ylim Ranges of the axes. By default the x-axis covers the
#'   times (plus `groupBuffer`) and the y-axis covers the rows (plus
#'   `legendBuffer`).
#' @param order With `y`: the shared case order. `"first"` (the default) uses
#'   the order of `x` (sorted as it would be plotted alone, or as sorted with
#'   [sorter()]) for both; `"joint"` sorts the cases on the states of `x` and
#'   then on those of `y`, within the groups of `x`; `y` then orders the
#'   cases whose states in `x` are identical.
#' @param y_args With `y`: a named list of arguments that replace those
#'   given for the second panel, e.g., `list(main = "Employment",
#'   colScheme = "topo")`.
#' @param ... Further graphical parameters passed to [plot()], such as
#'   `main`.
#' @return The colors of the states, invisibly, for use in a legend; with
#'   `y`, a list of the colors of each panel (`x` and `y`).
#' @examples
#' # the legend outside the plot
#' op <- par(bg = "cornsilk3", mar = c(5.1, 4.1, 4.1, 8.1), xpd = TRUE)
#' cols <- plot(longCat(example3), legendBuffer = 0,
#'              main = "Horizontal Line Plot")
#' legend(7.1, 100, legend = 1:5, col = cols, lty = 1, lwd = 2)
#' par(op)
#'
#' # weighted data: each case is a band with a height proportional to its
#' # weight
#' set.seed(2)
#' w <- rexp(nrow(example3))
#' plot(longCat(example3, weights = w), main = "Weighted")
#'
#' # two variables of the same participants side by side (#7)
#' set.seed(3)
#' lc1 <- longCat(example3)
#' lc2 <- longCat(pmin(example3 + matrix(sample(-1:1, 600, TRUE), 100), 5))
#' plot(lc1, lc2, y_args = list(colScheme = "topo"))
#' # with ties in the first variable, "joint" sorts them on the second
#' plot(longCat(example3[, 1:2]), lc2, order = "joint")
#'
#' # line types distinguish states in grayscale
#' plot(longCat(example3), colScheme = "gray", lty = c(1, 2, 1, 2, 1),
#'      lwd = 1, llwd = 1.5, main = "Gray with line types")
#'
#' # multivariate data: 3 items at 4 time points
#' Sigma <- matrix(.25, 12, 12)
#' diag(Sigma) <- 1
#' set.seed(9845)
#' mu <- rep(c(-.5, 0, .5), 4) + rnorm(12, 0, .25)
#' set.seed(539)
#' # 100 draws from a multivariate normal with mean mu and covariance Sigma
#' z <- matrix(rnorm(100 * 12), 100, 12) %*% chol(Sigma) +
#'   matrix(mu, 100, 12, byrow = TRUE)
#' ymv <- apply(z, 2, cut, breaks = c(-Inf, 0, Inf), labels = c(0, 1))
#' items <- rep(1:3, 4)
#' times <- sort(rep(1:4, 3))
#' tLabels <- paste("Time", 1:4)
#'
#' # time points within items
#' op <- par(mfrow = c(2, 2), bg = "cornsilk3", mar = c(5.1, 4.1, 4.1, 8.1),
#'           xpd = TRUE)
#' for (i in 1:3) {
#'   cols <- plot(longCat(ymv[, items == i], tLabels = tLabels),
#'                ylab = "", main = paste("Item", i), legendBuffer = 0,
#'                xlab = "", xlas = 2)
#'   legend(4.1, nrow(ymv), legend = 0:1, col = cols, lty = 1, lwd = 2,
#'          title = "Response")
#' }
#' par(op)
#'
#' # for data sets with many rows, writing directly to a file
#' # is much faster and unaffected by device resizing, see ?pdf
#' # (and ?png for picture file options)
#' f <- tempfile(fileext = ".pdf")
#' pdf(f)
#' plot(longCat(example3), main = "Sorted", colScheme = "heat", lwd = 2)
#' dev.off()
#' unlink(f)
#' @seealso [longCat()] to create the object; [sorter()] for groups and
#'   sorting options; [alignTime()] for alignment;
#'   [autoplot.longCat()] for the same plot with ggplot2.
#' @md
#' @export
plot.longCat <- function(x,
                         y             = NULL,
                         xlab          = "Days",
                         ylab          = NULL,
                         cols          = NULL,
                         colScheme     = "heat",
                         reverse       = FALSE,
                         lwd           = .5,
                         lcex          = 1,
                         llwd          = 3,
                         legendBuffer  = .12,
                         groupBuffer   = 0,
                         groupRotation = 90,
                         gcex          = 1,
                         seg.len       = 1,
                         xlas          = 0,
                         xcex          = 1,
                         ecex          = .5,
                         event.col     = 1,
                         plot.events   = TRUE,
                         which.events  = NULL,
                         n.events      = NULL,
                         event.pch     = NULL,
                         texclude      = NULL,
                         sort          = TRUE,
                         which.state   = NULL,
                         nth.state     = NULL,
                         not.state     = FALSE,
                         which.event   = NULL,
                         nth.event     = NULL,
                         not.event     = FALSE,
                         lty           = 1,
                         xlim          = NULL,
                         ylim          = NULL,
                         order         = c("first", "joint"),
                         y_args        = list(),
                         ...) {
  validate_longCat(x, "x")
  if (!is.null(y)) {
    own <- setdiff(names(formals(plot.longCat)),
                   c("x", "y", "order", "y_args", "..."))
    # panel titles from the objects' names, when they are plain names
    titles <- pair_titles(list(substitute(x), substitute(y)))
    return(plot_pair(x, y, mget(own), list(...), match.arg(order), y_args,
                     titles))
  }
  check_proportion(legendBuffer, "legendBuffer")
  check_proportion(groupBuffer, "groupBuffer")
  if (!is_flag(sort)) stop("`sort` must be TRUE or FALSE.", call. = FALSE)
  if (!is_flag(plot.events)) {
    stop("`plot.events` must be TRUE or FALSE.", call. = FALSE)
  }
  cols <- state_colors(cols, colScheme, x$nfactors, reverse)
  lty <- rep_len(lty, x$nfactors)

  if (sort && !x$sorted) x <- sorter(x, num = anyNA(x$y) || x$IndTime)
  pd <- prepare_plot_data(x, plot.events, which.events, n.events, texclude,
                          list(which.state = which.state, nth.state = nth.state,
                               not.state = not.state,
                               which.event = which.event, nth.event = nth.event,
                               not.event = not.event))
  plot.events <- pd$plot_events
  aligned <- pd$aligned

  layout <- plot_layout(pd, legendBuffer, groupBuffer, plot.events, xlim, ylim)
  if (is.null(ylab)) {
    ylab <- default_ylab(layout$n_cases,
                         if (!is.null(pd$weights)) sum(pd$weights, na.rm = TRUE))
  }

  plot(NA, type = "n", xlim = layout$xlim, ylim = layout$ylim,
       xlab = xlab, ylab = "", axes = FALSE, ...)
  title(ylab = ylab, line = if (layout$n_groups > 1L || groupBuffer > 0) 1 else 0.25)
  draw_time_axis(x, layout$time_range, aligned, texclude, xlas, xcex)
  draw_states(pd, cols, lwd, lty)
  if (plot.events) {
    draw_events(pd, event.pch, event.col, ecex, n_types = length(x$eventLabels))
  }
  if (legendBuffer > 0) {
    legend(layout$time_range[1], layout$legend_y, legend = x$Labels,
           lty = lty, cex = lcex, col = cols[seq_len(x$nfactors)],
           bty = "n", lwd = llwd, seg.len = seg.len, horiz = TRUE)
  }
  if (layout$n_groups > 1L) {
    draw_group_labels(pd, x$groupLabels, layout$xlim[1],
                      groupRotation, gcex)
  }
  invisible(cols)
}

# ---- preparing the data -------------------------------------------------------

# The data to draw for the (sorted or unsorted) longCat object `x`, as in
# plot_data(), after selecting events, cutting the times at `texclude`,
# aligning them (`align`: the alignTime() arguments), and keeping each
# case's first `n.events` events. `plot_events` is whether events are
# drawn and `aligned` whether the times were aligned.
prepare_plot_data <- function(x, plot.events, which.events, n.events,
                              texclude, align) {
  pd <- plot_data(x)
  plot.events <- plot.events && !is.null(pd$events)
  if (plot.events) {
    pd <- select_events(pd, which.events, n_types = length(x$eventLabels))
  }
  if (!is.null(texclude)) pd <- restrict_times(pd, texclude)
  aligned <- !is.null(align$which.state) || !is.null(align$nth.state) ||
    !is.null(align$which.event) || !is.null(align$nth.event)
  if (aligned) {
    shifted <- do.call(alignTime, c(list(pd$y, pd$times, events = pd$events,
                                         event.times = pd$event.times),
                                    align))
    pd$times <- shifted$aligned.times
    pd$event.times <- shifted$aligned.event.times
  }
  if (plot.events && !is.null(n.events)) pd <- first_events(pd, n.events)
  pd$plot_events <- plot.events
  pd$aligned <- aligned
  pd
}

check_proportion <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0 || x > 1) {
    stop(sprintf("`%s` must be a single number between 0 and 1.", arg),
         call. = FALSE)
  }
}

state_colors <- function(cols, colScheme, nfactors, reverse) {
  if (is.null(cols)) return(colChoose(colScheme, nfactors, reverse))
  if (length(cols) < nfactors) {
    stop(sprintf("`cols` must have one color per state (%d), not %d.",
                 nfactors, length(cols)), call. = FALSE)
  }
  cols
}

# The data to draw: the sorted components if the object is sorted, otherwise
# the unsorted ones. `group` is NULL for unsorted objects and NA on the blank
# rows between groups.
plot_data <- function(lc) {
  pd <- if (lc$sorted) {
    list(y = lc$y.sorted, times = lc$times.sorted, events = lc$events.sorted,
         event.times = lc$event.times.sorted, group = lc$group.sorted[, 1],
         weights = lc$weights[lc$order.y.sorted[, 4]])
  } else {
    list(y = lc$y, times = lc$times, events = lc$events,
         event.times = lc$event.times, group = NULL, weights = lc$weights)
  }
  c(pd, row_geometry(nrow(pd$y), pd$weights))
}

# The center and height of each plot row. Without weights, row r is drawn
# at height r. With weights, each case is a band of height proportional to
# its weight, scaled so that the mean height is 1 (the layout is the same
# as without weights), and blank rows (NA weight) have height 1.
row_geometry <- function(n_rows, weights) {
  if (is.null(weights)) {
    return(list(row_center = seq_len(n_rows), row_height = NULL))
  }
  height <- weights / mean(weights, na.rm = TRUE)
  height[is.na(height)] <- 1
  top <- cumsum(height) + 0.5
  list(row_center = top - height / 2, row_height = height)
}

# Keep only `which.events`, then move each case's events to the left so that
# its first events are in its first columns.
select_events <- function(pd, which.events, n_types) {
  if (!is.null(which.events)) {
    if (!is.numeric(which.events) || anyNA(which.events) ||
        !all(which.events %in% seq_len(n_types))) {
      stop(sprintf("`which.events` must be event codes between 1 and %d.",
                   n_types), call. = FALSE)
    }
    pd$events[!pd$events %in% which.events] <- NA
  }
  packed <- pack_events(pd$events, pd$event.times)
  pd$events <- packed$events
  pd$event.times <- packed$event.times
  pd
}

# Keep each case's first `n` (packed) events.
first_events <- function(pd, n) {
  if (!is_whole_scalar(n) || n < 1) {
    stop("`n.events` must be a single positive integer.", call. = FALSE)
  }
  if (n < ncol(pd$events)) {
    drop <- seq.int(n + 1L, ncol(pd$events))
    pd$events[, drop] <- NA
    pd$event.times[, drop] <- NA
  }
  pd
}

# Cut intervals at the limits and drop events outside them.
restrict_times <- function(pd, texclude) {
  if (!is.numeric(texclude) || length(texclude) != 2L || anyNA(texclude) ||
      texclude[1] >= texclude[2]) {
    stop("`texclude` must be two increasing times.", call. = FALSE)
  }
  pd$times <- pmin(pmax(pd$times, texclude[1]), texclude[2])
  if (!is.null(pd$event.times)) {
    outside <- pd$event.times < texclude[1] | pd$event.times > texclude[2]
    pd$event.times[!is.na(outside) & outside] <- NA
  }
  pd
}

# ---- layout -------------------------------------------------------------------

plot_layout <- function(pd, legendBuffer, groupBuffer, with_events,
                        xlim = NULL, ylim = NULL) {
  n_rows <- nrow(pd$y)
  all_times <- c(pd$times, if (with_events) pd$event.times)
  time_range <- range(all_times, na.rm = TRUE, finite = TRUE)
  if (!all(is.finite(time_range))) time_range <- c(0, 1)
  width <- diff(time_range)
  if (width == 0) width <- 1

  groups <- if (is.null(pd$group)) numeric(0) else unique(pd$group[!is.na(pd$group)])
  legend_y <- n_rows + ceiling(legendBuffer * n_rows)
  list(
    time_range = time_range,
    xlim = if (is.null(xlim)) time_range - c(groupBuffer * width, 0) else xlim,
    ylim = if (is.null(ylim)) c(0, legend_y) else ylim,
    legend_y = legend_y + 0.1,
    n_groups = length(groups),
    n_cases = if (is.null(pd$group)) n_rows else sum(!is.na(pd$group))
  )
}

default_ylab <- function(n_cases, total_weight = NULL) {
  if (is.null(total_weight)) {
    return(paste("Each Line Represents a Participant, N =", format_count(n_cases)))
  }
  sprintf("Each Band Represents a Participant, N = %s (Weighted N = %s)",
          format_count(n_cases), format_weight(total_weight))
}

# ---- drawing ------------------------------------------------------------------

# One segment per observed cell, at the center of its plot row; with
# weights, `half` is half the height of the row's band.
state_segments <- function(pd) {
  y <- pd$y
  observed <- !is.na(y)
  n_col <- ncol(y)
  rows <- row(y)[observed]
  s <- data.frame(x0 = pd$times[, -(n_col + 1L), drop = FALSE][observed],
                  x1 = pd$times[, -1L, drop = FALSE][observed],
                  y = pd$row_center[rows],
                  state = y[observed])
  if (!is.null(pd$row_height)) s$half <- pd$row_height[rows] / 2
  s
}

draw_states <- function(pd, cols, lwd, lty) {
  s <- state_segments(pd)
  if (is.null(s$half)) {
    segments(s$x0, s$y, s$x1, s$y, col = cols[s$state], lwd = lwd,
             lty = lty[s$state])
  } else {
    rect(s$x0, s$y - s$half, s$x1, s$y + s$half, col = cols[s$state],
         border = NA)
  }
}

event_points <- function(pd, event.pch, event.col, n_types) {
  codes <- pd$events
  shown <- !is.na(codes) & !is.na(pd$event.times)
  code <- codes[shown]
  pch <- if (is.null(event.pch)) seq_len(n_types) else rep_len(event.pch, n_types)
  data.frame(x = pd$event.times[shown], y = pd$row_center[row(codes)[shown]],
             code = code,
             pch = pch[code], col = rep_len(event.col, n_types)[code],
             stringsAsFactors = FALSE)
}

draw_events <- function(pd, event.pch, event.col, ecex, n_types) {
  p <- event_points(pd, event.pch, event.col, n_types)
  if (nrow(p) > 0L) points(p$x, p$y, pch = p$pch, col = p$col, cex = ecex)
}

# Label the time points with tLabels when every case shares the plotted
# times; otherwise draw default ticks within the time range (not in the
# margin left for group labels).
draw_time_axis <- function(lc, time_range, aligned, texclude, xlas, xcex) {
  if (is.null(lc$tLabels) || aligned) {
    ticks <- graphics::axTicks(1)
    # the default 4% axis extension beyond the data
    tolerance <- 0.04 * diff(time_range)
    ticks <- ticks[ticks >= time_range[1] - tolerance &
                     ticks <= time_range[2] + tolerance]
    axis(1, at = ticks, las = xlas, cex.axis = xcex)
    return(invisible())
  }
  starts <- lc$times[1, -ncol(lc$times)]
  keep <- if (is.null(texclude)) TRUE else starts >= texclude[1] & starts <= texclude[2]
  axis(1, at = starts[keep], labels = lc$tLabels[keep], las = xlas,
       cex.axis = xcex)
}

# The center and label of each group present in `group` (group codes by
# plot row, NA for blank rows): the middle of the span from the bottom of
# its first row to the top of its last. Labels are looked up by code, so
# groups missing from a subset do not shift the labels of the others.
group_label_positions <- function(group, groupLabels,
                                  row_center = seq_along(group),
                                  row_height = NULL) {
  if (is.null(row_height)) row_height <- rep(1, length(group))
  in_group <- !is.na(group)
  bottom <- tapply((row_center - row_height / 2)[in_group], group[in_group], min)
  top <- tapply((row_center + row_height / 2)[in_group], group[in_group], max)
  centers <- (bottom + top) / 2
  codes <- as.numeric(names(centers))
  labels <- if (is.null(groupLabels)) names(centers) else groupLabels[codes]
  data.frame(center = as.vector(centers), label = labels)
}

draw_group_labels <- function(pd, groupLabels, x, rotation, cex) {
  pos <- group_label_positions(pd$group, groupLabels, pd$row_center,
                               pd$row_height)
  text(x, pos$center, labels = pos$label, srt = rotation, cex = cex)
}
