# Frequencies of state sequences (patterns), with print(), plot(), and
# autoplot() methods.

#' Frequencies of state sequences
#'
#' Counts how many cases follow each sequence of states (pattern), from the
#' most to the least common. By default, repeated states are collapsed into
#' spells, so a case in jail for two waves and then at work has the pattern
#' "Jail > Work". The result prints as a table and can be drawn with
#' [plot()] or, with ggplot2, [ggplot2::autoplot()]: each of the most
#' common patterns as a row of colored spells, with its count and percent.
#'
#' Missing states are skipped, so `c(1, NA, 2)` has the pattern of
#' `c(1, 2)`; a case with no observed states has the pattern `NA`. With
#' `mindur`, states observed for less than `mindur` are treated as missing
#' first, as in [sorter()].
#'
#' With weights (the `weights` of [longCat()], or `weight` for long data),
#' the percentages are of the total weight, the patterns are ordered by
#' their weights, and a `weight` column gives the weighted counts.
#'
#' With groups (`group`, or the groups of an object sorted with [sorter()]),
#' the patterns are counted within each group, and the percentages are of
#' the group.
#'
#' @param x A [longCat()] object, or a data frame in long format (one row
#'   per case and time).
#' @param collapse Logical. Collapse repeated states into one spell? Default
#'   is `TRUE`. With `FALSE`, each time point is one state of the pattern.
#' @param mindur Minimum duration, or `NULL` (the default): states observed
#'   for less than `mindur` are treated as missing, as in [sorter()].
#' @param group Groups to count the patterns in: for a `longCat` object,
#'   a vector with one value per case; for long data, the name of a column
#'   that is the same in every row of a case, as in [sorter()]. By default,
#'   the groups of an object sorted with [sorter()] (if any). Cases with a
#'   missing group are left out, with a warning.
#' @param sep The separator between the states of a pattern. Default is
#'   `" > "`.
#' @param id,time,state,stop,last_stop For long data: the names of the
#'   columns holding the case IDs, the (start) times, the states, and,
#'   optionally, the stop times of intervals, and when the last state ends;
#'   as in [as.longCat()].
#' @param weight For long data: the name of a column of case weights, or
#'   `NULL` (the default).
#' @param ... Unused; unknown arguments are an error.
#' @return A data frame of class `longCatPatterns`, one row per pattern
#'   (within each group), from the most to the least common (ties in the
#'   order of [sorter()]), with columns
#'   * `group`: the group (only with groups).
#'   * `pattern`: the labels of the states, joined by `sep`.
#'   * `length`: the number of states (spells) in the pattern.
#'   * `n`: the number of cases.
#'   * `weight`: the total weight of the cases (only with weights).
#'   * `percent`, `cumulative`: the percent of the cases (or of their
#'     weight) and the cumulative percent, within the group.
#'   * `states`: a list of the state codes of each pattern (positions in the
#'     labels of the states).
#' @references Tueller, S. J., Van Dorn, R. A., & Bobashev, G. V. (2016).
#'   Visualization of categorical longitudinal and times series data (Report
#'   No. MR-0033-1602). Research Triangle Park, NC: RTI Press.
#'   \doi{10.3768/rtipress.2016.mr.0033.1602}
#' @seealso [makePatterns()] for one pattern per case;
#'   [plot.longCatPatterns()] to draw the patterns.
#' @examples
#' lc <- longCat(example3,
#'               Labels = c("Home", "School", "Work", "Jail", "Other"))
#' p <- count_patterns(lc)
#' p
#' plot(p)
#'
#' # without collapsing repeated states
#' count_patterns(lc, collapse = FALSE)
#'
#' # within groups
#' group <- rep(c("Treatment", "Control"), 50)
#' count_patterns(lc, group = group)
#'
#' # long data, with ggplot2
#' long <- data.frame(id = rep(1:100, each = 6), wave = rep(1:6, 100),
#'                    status = c(t(example3)))
#' p <- count_patterns(long, time = "wave", state = "status")
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   print(ggplot2::autoplot(p, n = 5, colScheme = "topo"))
#' }
#' @md
#' @export
count_patterns <- function(x, ...) UseMethod("count_patterns")

#' @rdname count_patterns
#' @md
#' @export
count_patterns.longCat <- function(x, collapse = TRUE, mindur = NULL,
                                   group = NULL, sep = " > ", ...) {
  check_no_dots("count_patterns()", ...)
  validate_longCat(x, "x")
  if (!is_flag(collapse)) {
    stop("`collapse` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.character(sep) || length(sep) != 1L || is.na(sep)) {
    stop("`sep` must be a single string.", call. = FALSE)
  }
  y <- x$y
  if (!is.null(mindur)) y <- mask_short_durations(y, x$times, mindur)
  spells <- if (collapse) {
    collapse_repeats_rows(y)
  } else {
    left_pack_rows(y, !is.na(y))
  }
  pattern_table(spells, x$Labels, x$weights, pattern_groups(x, group), sep,
                collapse)
}

#' @rdname count_patterns
#' @md
#' @export
count_patterns.data.frame <- function(x, id = "id", time = "time",
                                      state = "state", stop = NULL,
                                      last_stop = NULL, weight = NULL,
                                      group = NULL, collapse = TRUE,
                                      mindur = NULL, sep = " > ", ...) {
  check_no_dots("count_patterns()", ...)
  lc <- as.longCat(x, id = id, time = time, state = state, stop = stop,
                   last_stop = last_stop, weight = weight)
  # the cases of as.longCat(), in the order their IDs first appear
  cases <- case_index(x[[id]], "x")
  group <- case_column(x, group, cases$case, length(cases$ids), "group",
                       data_arg = "x")
  count_patterns.longCat(lc, collapse = collapse, mindur = mindur,
                         group = group, sep = sep)
}

# The group of each case of `lc`, as a factor, or NULL: `group`, or the groups
# of a sorted object.
pattern_groups <- function(lc, group) {
  n <- nrow(lc$y)
  if (is.null(group)) {
    if (!lc$sorted || is.null(lc$groupLabels) || length(lc$groupLabels) < 2L) {
      return(NULL)
    }
    return(factor(lc$group, levels = seq_along(lc$groupLabels),
                  labels = lc$groupLabels))
  }
  if (!is.atomic(group) || length(group) != n) {
    stop(sprintf("`group` must have one value per case (%d), not %d.", n,
                 length(group)), call. = FALSE)
  }
  if (is.factor(group)) droplevels(group) else factor(group)
}

# The table of patterns of the left-packed state codes `spells` (one row per
# case), with state labels `labels`, case weights `weights` (or NULL), and
# groups `group` (a factor, or NULL).
pattern_table <- function(spells, labels, weights, group, sep, collapse) {
  if (!is.null(group) && anyNA(group)) {
    warning(sprintf("%d case(s) with a missing `group` are left out.",
                    sum(is.na(group))), call. = FALSE)
    keep <- !is.na(group)
    spells <- spells[keep, , drop = FALSE]
    weights <- weights[keep]
    group <- droplevels(group[keep])
  }
  grouped <- !is.null(group)
  key <- do.call(paste, c(unname(as.data.frame(spells)), sep = "-"))
  if (grouped) key <- paste(as.integer(group), key, sep = "|")
  first <- !duplicated(key)
  pattern <- match(key, key[first])
  k <- sum(first)

  reps <- spells[first, , drop = FALSE]
  states <- lapply(seq_len(k), function(i) {
    codes <- reps[i, ]
    as.integer(codes[!is.na(codes)])
  })
  n <- tabulate(pattern, nbins = k)
  weight <- if (!is.null(weights)) {
    as.vector(tapply(weights, factor(pattern, levels = seq_len(k)), sum))
  }
  amount <- if (is.null(weight)) n else weight
  pattern_group <- if (grouped) group[first] else factor(rep(1L, k))
  total <- as.vector(stats::ave(amount, pattern_group, FUN = sum))
  percent <- 100 * amount / total

  # by group, then from the most common, then in the order of sorter()
  # (shorter patterns first among equals), with no observed states last
  n_states <- lengths(states)
  ord <- do.call(order, c(list(as.integer(pattern_group), -amount,
                               n_states == 0L),
                          unname(as.list(as.data.frame(reps))),
                          list(na.last = FALSE)))
  out <- data.frame(
    pattern = vapply(states, function(s) {
      if (length(s) == 0L) NA_character_ else paste(labels[s], collapse = sep)
    }, character(1)),
    length = n_states, n = n, stringsAsFactors = FALSE
  )
  if (grouped) out <- cbind(group = group[first], out)
  if (!is.null(weight)) out$weight <- weight
  out$percent <- percent
  out <- out[ord, , drop = FALSE]
  out$cumulative <- as.vector(stats::ave(out$percent, pattern_group[ord],
                                  FUN = cumsum))
  out$states <- states[ord]
  rownames(out) <- NULL
  structure(out, class = c("longCatPatterns", "data.frame"), Labels = labels,
            n_cases = nrow(spells), n_patterns = k, collapse = collapse)
}

#' @export
`[.longCatPatterns` <- function(x, ...) {
  out <- NextMethod()
  if (!is.data.frame(out)) return(out)
  if (!all(c("pattern", "states") %in% names(out))) {
    class(out) <- "data.frame"
    return(out)
  }
  for (a in c("Labels", "n_cases", "n_patterns", "collapse")) {
    attr(out, a) <- attr(x, a)
  }
  out
}

#' @rdname plot.longCatPatterns
#' @md
#' @export
print.longCatPatterns <- function(x, n = 10, ...) {
  shown <- top_patterns(x, n)
  n_patterns <- attr(x, "n_patterns")
  if (is.null(n_patterns)) n_patterns <- nrow(x)
  collapsed <- isTRUE(attr(x, "collapse"))
  cat(sprintf("%s distinct pattern%s in %s cases%s\n\n",
              format_count(n_patterns), if (n_patterns == 1) "" else "s",
              format_count(attr(x, "n_cases")),
              if (collapsed) " (repeated states collapsed)" else ""))
  d <- x[shown, setdiff(names(x), "states"), drop = FALSE]
  d$percent <- round(d$percent, 1)
  d$cumulative <- round(d$cumulative, 1)
  if (!is.null(d$weight)) d$weight <- signif(d$weight, 4)
  print(d, row.names = FALSE)
  if (nrow(x) > length(shown)) {
    cat(sprintf("... and %s more\n", format_count(nrow(x) - length(shown))))
  }
  invisible(x)
}

#' Plot pattern frequencies
#'
#' Draws the most common patterns from [count_patterns()], each as a row of
#' colored spells (the states of the pattern, in order), with the most
#' common at the top and its count (or weight) and percent on the right.
#' `plot()` uses base graphics; `autoplot()`, available when ggplot2 is
#' installed, returns a `ggplot` object. With groups, each group is drawn
#' in its own panel. `print()` shows the patterns as a table.
#'
#' @param x,object An object of class `longCatPatterns` from [count_patterns()].
#' @param n The number of patterns to draw (or print), the most common
#'   ones, in each group. Default is 10.
#' @param cols,colScheme,reverse The colors of the states, as in
#'   [plot.longCat()].
#' @param main,xlab,ylab Titles of the plot and axes. `main` defaults to
#'   the group, with groups.
#' @param ... For `plot()`, further graphical parameters passed to
#'   [plot()]. Unused otherwise; for `autoplot()`, unknown arguments are an
#'   error.
#' @return `plot()` returns the colors of the states, invisibly;
#'   `autoplot()` a `ggplot` object; `print()` its argument, invisibly.
#' @seealso [count_patterns()]
#' @examples
#' p <- count_patterns(longCat(example3))
#' print(p, n = 5)
#' plot(p, n = 5, colScheme = "topo")
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   print(ggplot2::autoplot(p))
#' }
#' @md
#' @export
plot.longCatPatterns <- function(x, n = 10, cols = NULL, colScheme = "heat",
                                 reverse = FALSE, main = NULL, xlab = "Spell",
                                 ylab = "Pattern (most common first)", ...) {
  labels <- attr(x, "Labels")
  cols <- state_colors(cols, colScheme, length(labels), reverse)
  panels <- pattern_panels(x, n)
  if (length(panels) > 1L && identical(graphics::par("mfrow"), c(1L, 1L))) {
    op <- graphics::par(mfrow = c(1L, length(panels)))
    on.exit(graphics::par(op), add = TRUE)
  }
  for (i in seq_along(panels)) {
    rows <- panels[[i]]
    title <- main
    if (is.null(main) && length(panels) > 1L) title <- names(panels)[i]
    draw_pattern_panel(rows, cols, labels, title, xlab, ylab, ...)
  }
  invisible(cols)
}

#' @rdname plot.longCatPatterns
#' @md
#' @exportS3Method ggplot2::autoplot
autoplot.longCatPatterns <- function(object, n = 10, cols = NULL,
                                     colScheme = "heat", reverse = FALSE,
                                     xlab = "Spell",
                                     ylab = "Pattern (most common first)",
                                     ...) {
  check_no_dots("autoplot() of patterns", ...)
  labels <- attr(object, "Labels")
  cols <- state_colors(cols, colScheme, length(labels), reverse)
  cols <- cols[seq_along(labels)]
  panels <- pattern_panels(object, n)
  panel_names <- if (is.null(names(panels))) "" else names(panels)
  spells <- do.call(rbind, lapply(seq_along(panels), function(j) {
    rows <- panels[[j]]
    name <- panel_names[j]
    do.call(rbind, lapply(seq_len(nrow(rows)), function(i) {
      s <- rows$states[[i]]
      if (length(s) == 0L) return(NULL)
      data.frame(panel = name, rank = i, position = seq_along(s),
                 state = factor(labels[s], levels = labels))
    }))
  }))
  counts <- do.call(rbind, lapply(seq_along(panels), function(j) {
    rows <- panels[[j]]
    data.frame(panel = panel_names[j], rank = seq_len(nrow(rows)),
               label = pattern_count_labels(rows))
  }))
  longest <- max(1L, lengths(object$states[top_patterns(object, n)]))
  panel_levels <- panel_names
  spells$panel <- factor(spells$panel, levels = panel_levels)
  counts$panel <- factor(counts$panel, levels = panel_levels)

  .data <- ggplot2::.data
  aes <- ggplot2::aes
  p <- ggplot2::ggplot(spells) +
    ggplot2::geom_rect(aes(xmin = .data$position - 1, xmax = .data$position,
                           ymin = -.data$rank - 0.4, ymax = -.data$rank + 0.4,
                           fill = .data$state)) +
    ggplot2::geom_text(aes(x = longest + 0.2, y = -.data$rank,
                           label = .data$label),
                       data = counts, hjust = 0) +
    ggplot2::scale_fill_manual(values = stats::setNames(cols, labels),
                               drop = FALSE, name = NULL) +
    ggplot2::scale_x_continuous(breaks = seq_len(longest) - 0.5,
                                labels = seq_len(longest),
                                limits = c(0, longest + 2)) +
    ggplot2::scale_y_continuous(breaks = function(lim) {
      -seq_len(max(1, floor(-lim[1])))
    }, labels = function(b) -b) +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   legend.position = "top")
  if (length(panels) > 1L) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$panel), scales = "free_y")
  }
  p
}

# The rows of `x` shown by print() and plot(): the first `n` of each group.
top_patterns <- function(x, n) {
  if (!is_whole_scalar(n) || n < 1) {
    stop("`n` must be a single positive integer.", call. = FALSE)
  }
  rows <- seq_len(nrow(x))
  if (!"group" %in% names(x)) return(first_n(rows, n))
  unlist(lapply(split(rows, x$group), first_n, n), use.names = FALSE)
}

first_n <- function(x, n) x[seq_len(min(n, length(x)))]

# The rows of `x` to draw, as a list of data frames named by group (one
# element, named "", without groups); patterns with no observed states are
# left out.
pattern_panels <- function(x, n) {
  observed <- lengths(x$states) > 0L
  rows <- top_patterns(x[observed, , drop = FALSE], n)
  shown <- x[observed, , drop = FALSE][rows, , drop = FALSE]
  if (nrow(shown) == 0L) {
    stop("There are no patterns with observed states to plot.", call. = FALSE)
  }
  if (!"group" %in% names(shown)) return(list(shown))
  panels <- split(shown, shown$group, drop = TRUE)
  lapply(panels, function(p) {
    for (a in c("Labels", "n_cases", "collapse")) attr(p, a) <- attr(x, a)
    p
  })
}

# "count (percent%)" for each pattern, using the weights when there are
# any.
pattern_count_labels <- function(rows) {
  amount <- if (is.null(rows$weight)) {
    formatC(rows$n, format = "d", big.mark = ",")
  } else {
    formatC(round(rows$weight, 1), format = "f", digits = 1, big.mark = ",")
  }
  sprintf("%s (%s%%)", amount, format(round(rows$percent, 1), trim = TRUE))
}

draw_pattern_panel <- function(rows, cols, labels, main, xlab, ylab, ...) {
  k <- nrow(rows)
  longest <- max(1L, lengths(rows$states))
  y <- rev(seq_len(k))
  plot(NA, type = "n", xlim = c(0, longest + 2), ylim = c(0.5, k + 1.5),
       axes = FALSE, xlab = xlab, ylab = ylab, main = main, ...)
  spell_rank <- rep(seq_len(k), lengths(rows$states))
  spell_pos <- sequence(lengths(rows$states))
  spell_state <- unlist(rows$states)
  rect(spell_pos - 1, y[spell_rank] - 0.4, spell_pos, y[spell_rank] + 0.4,
       col = cols[spell_state], border = NA)
  text(longest + 0.2, y, pattern_count_labels(rows), adj = 0)
  axis(1, at = seq_len(longest) - 0.5, labels = seq_len(longest))
  axis(2, at = y, labels = seq_len(k), las = 1)
  legend(0, k + 1.5, legend = labels, fill = cols[seq_along(labels)],
         border = NA, bty = "n", horiz = TRUE, cex = 0.8)
}
