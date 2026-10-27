#' Sort a longCat object or long-format data
#'
#' Orders the cases of a [longCat()] object so that similar trajectories
#' are plotted next to each other, optionally stratified by group.
#' [plot.longCat()] sorts with the defaults when given an unsorted object;
#' call `sorter()` directly to plot groups or to change how cases are sorted.
#' An object can be sorted only once: sort the unsorted object again to try
#' other settings.
#'
#' Data in long format (one row per case and time) are sorted the same way
#' without reshaping them: the data frame method returns the data with each
#' case's plot row added, for plotting with ggplot2 or other tools.
#'
#' @param lc An unsorted object of class `longCat` created by [longCat()],
#'   or, for the data frame method, a data frame in long format.
#' @param ascending Logical. Sort in ascending order? Default is `TRUE`.
#'   Groups are always in ascending order.
#' @param whichColumns Which columns of `lc$y` to sort on (e.g., `c(1, 5,
#'   7)`). Useful when, for example, only observations after an intervention
#'   should drive the order. Default is all columns.
#' @param num How missing values affect the order (see Details). Default is
#'   `TRUE`.
#' @param mindur Minimum duration, or `NULL` (the default). States observed
#'   for less than `mindur` (`times[, j + 1] - times[, j] < mindur`) are
#'   treated as missing for sorting, so short-lived states do not drive the
#'   order. Durations are measured on all of `lc$times`, before
#'   `whichColumns` is applied.
#' @param igrpt Logical. Ignore repeated values? If `TRUE`, each run of a
#'   repeated state is reduced to one value before sorting; a missing value
#'   ends a run and is then dropped, so `c(1, 2, 2, NA, 2, 3)` sorts as
#'   `c(1, 2, 2, 3)`. Default is `FALSE`.
#' @param customSort A vector with one value per case, sorted on before the
#'   states (after `initFirst`, within groups). Must not contain missing
#'   values. For the data frame method, the name of a column of `lc` that
#'   is the same in every row of a case.
#' @param initFirst Logical. If `TRUE`, sort on each case's first state
#'   before `customSort` and the state sequence. Default is `FALSE`.
#' @param group A vector with one value per case giving group membership.
#'   Groups are plotted in separate blocks in the order of their values (or
#'   factor levels). Cases with a missing group are dropped from the sorted
#'   data, with a warning. Default is `NULL` (one group). For the data frame
#'   method, the name of a column of `lc` that is the same in every row of
#'   a case.
#' @param groupLabels Labels for the groups, one per group in the order
#'   above. Defaults to the group values (or factor levels).
#' @param ggap The number of blank rows plotted after each group, as a
#'   proportion of the number of cases (between 0 and 1). The default,
#'   `NULL`, is 0.05 when there are two or more groups and 0 otherwise.
#' @param sortDurations Logical. Break ties between identical state
#'   sequences by the time spent in each state (see Details)? The default,
#'   `NULL`, is `TRUE` when times vary across cases (`lc$IndTime`).
#' @param id,time,state,stop,end For the data frame method: the names of the
#'   columns of `lc` holding the case IDs, the (start) times, the states,
#'   and, optionally, the stop times of intervals, and when the last state
#'   ends; as in [as.longCat()].
#' @param weight For the data frame method: the name of a column of `lc`
#'   holding case weights (the same in every row of a case), or `NULL` (the
#'   default). With weights, the band edges of each case are added as
#'   `ymin` and `ymax`.
#' @param into For the data frame method: the name of the column to add,
#'   holding each case's plot row. Default is `"row"`.
#' @param ... For the data frame method, the other arguments of the
#'   `longCat` method (`ascending`, `whichColumns`, `num`, `mindur`,
#'   `igrpt`, `initFirst`, `groupLabels`, `ggap`, and `sortDurations`).
#'   Unused otherwise; unknown arguments are an error.
#'
#' @return For a `longCat` object, the object `lc` with `sorted = TRUE` and
#'   the sorted components (`y.sorted`, `times.sorted`, `order.y.sorted`,
#'   `group.sorted`, and, with events, `events.sorted` and
#'   `event.times.sorted`) filled in. Sorted components are matrices, except
#'   `order.y.sorted`, which is a data frame. Blank rows between groups are
#'   rows of `NA`.
#'
#'   For a data frame, the data frame `lc` with all its columns and the
#'   column `into` added: the plot row of each case, counted from the
#'   bottom, the same as the `row` of [as.data.frame.longCat()]. Rows are in
#'   plot order (by plot row, then time), without row names. With groups,
#'   the plot rows skip the blank rows between groups. Cases left out
#'   because their group is missing are dropped.
#'
#'   With `weight`, the columns `ymin` and `ymax` are added too: the bottom
#'   and top of each case's band, whose height is proportional to its
#'   weight, as drawn by [plot.longCat()] (the mean height is 1, and the
#'   blank rows between groups have height 1). Draw the bands with
#'   `ggplot2::geom_rect()`; their centers, `(ymin + ymax) / 2`, place
#'   points or lines on them.
#'
#' @details
#' Within each group, cases are sorted on their initial state (if
#' `initFirst = TRUE`), then on `customSort`, then on their sequence of
#' states in the `whichColumns` columns (after applying `mindur` and
#' `igrpt`), and then, if `sortDurations = TRUE`, on the time spent in each
#' state of the sequence. Ties keep their original order.
#'
#' **Long-format data.** The data frame method converts `lc` with
#' [as.longCat()], sorts it with the `longCat` method, and adds each case's
#' plot row to the original rows, so the result does not depend on how the
#' times are laid out and other columns (covariates) are kept. Ties are
#' broken by case ID (in the order of [order()], using the factor levels
#' for factors), so the result does not depend on the order of the rows of
#' `lc`.
#'
#' Sequences are compared state by state from the first column, as whole
#' numbers, so states of 10 or more and any number of time points are
#' handled exactly.
#'
#' * With `num = TRUE`, missing values are skipped: the observed states are
#'   compared from the left and a sequence that ends sooner sorts first, so
#'   `c(1, NA, 2)` sorts with `c(1, 2)` and before `c(1, 2, 3)`. Cases with
#'   no observed states sort last. This gives the order of the numeric
#'   patterns from [makePatterns()] (`1.2` before `1.23`) and is recommended
#'   when data are missing or times vary across cases.
#' * With `num = FALSE`, states are compared in place and a missing value
#'   sorts after every state, the order of the string patterns from
#'   [makePatterns()].
#'
#' With individually varying times of observation, cases with the same
#' sequence of states are then "secondarily sorted by each participant's
#' pattern of time (or duration) in each state" (Tueller, Van Dorn, &
#' Bobashev, 2016, p. 7): by the duration of the first state in the
#' sequence, then the second, and so on, in the direction set by
#' `ascending`. With `igrpt = TRUE`, the duration of a run is the total time
#' of its intervals.
#'
#' The patterns from [makePatterns()] are stored in the second column of
#' `order.y` for reference, `customSort` in the third, and the row number
#' of each case in the fourth, so that `order.y.sorted[, 4]` gives the row
#' of `y` plotted in each row (`NA` for blank rows).
#'
#' @author Stephen J. Tueller
#' @references Tueller, S. J., Van Dorn, R. A., & Bobashev, G. V. (2016).
#'   Visualization of categorical longitudinal and times series data (Report
#'   No. MR-0033-1602). Research Triangle Park, NC: RTI Press.
#'   \doi{10.3768/rtipress.2016.mr.0033.1602}
#' @seealso [longCat()] and [plot.longCat()].
#' @examples
#' # a plot like Figure 3 of Tueller, Van Dorn, & Bobashev (2016)
#' times <- c(1, 100, 200, 300, 400, 500, 600)
#' f3lc <- longCat(example3, times)
#' op <- par(mfrow = c(1, 2), bg = "cornsilk3")
#' plot(f3lc, main = "Unsorted", colScheme = "heat", lwd = 2,
#'      legendBuffer = .2, sort = FALSE)
#' plot(f3lc, main = "Sorted", colScheme = "heat", lwd = 2,
#'      legendBuffer = .2)
#' par(op)
#'
#' # sort with a grouping variable and plot
#' set.seed(1)
#' group <- sample(c("A", "B", "C"), nrow(example3), replace = TRUE)
#' grouplc <- sorter(f3lc, group = group)
#' op <- par(bg = "cornsilk3", mar = c(5.1, 4.1, 4.1, 9.1), xpd = TRUE)
#' cols <- plot(grouplc, groupBuffer = .15, main = "Grouped Data",
#'              colScheme = "heat", lwd = 2, legendBuffer = 0)
#' legend(610, 130, legend = 1:5, col = cols, lty = 1, lwd = 2)
#' par(op)
#'
#' # the same sorted data in ggplot2, as tiles like Figure 4 of
#' # bdemarest's answer at https://stackoverflow.com/questions/11513149/:
#' # ggplot() takes the object as data, with each case's plot row and band
#' # (see fortify.longCat())
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   print(
#'     ggplot2::ggplot(grouplc, ggplot2::aes(xmin = start, xmax = stop,
#'                                           ymin = ymin, ymax = ymax,
#'                                           fill = state)) +
#'       ggplot2::geom_rect() +
#'       scale_fill_longcat() +
#'       ggplot2::facet_grid(group ~ ., space = "free_y", scales = "free_y")
#'   )
#'   # or the complete plot in one call
#'   print(ggplot2::autoplot(grouplc))
#' }
#'
#' # sort with a grouping variable and events
#' set.seed(45962)
#' events <- matrix(sample(1:3, nrow(example3) * 2, replace = TRUE),
#'                  nrow(example3), 2)
#' set.seed(23498)
#' event.times <- matrix(sample(min(times):max(times), nrow(example3) * 2,
#'                              replace = TRUE), nrow(example3), 2)
#' labels <- c("Street", "Drug Tx", "Jail", "Prison", "Unknown")
#' eventLabels <- c("Arrest", "Drug Test", "Hearing")
#' eventlc <- longCat(example3, times = times, Labels = labels,
#'                    events = events, event.times = event.times,
#'                    eventLabels = eventLabels)
#' set.seed(4290)
#' groupevent <- sample(1:3, nrow(example3), replace = TRUE)
#' groupeventlc <- sorter(eventlc, group = groupevent)
#' op <- par(bg = "cornsilk3", mar = c(5.1, 4.1, 4.1, 12.1), xpd = TRUE)
#' cols <- plot(groupeventlc, legendBuffer = 0, groupBuffer = 0.15,
#'              main = "Grouping and Events")
#' legend(610, 130, legend = groupeventlc$Labels, lty = 1, col = cols, lwd = 2)
#' legend(610, 60, legend = groupeventlc$eventLabels,
#'        pch = seq_along(groupeventlc$eventLabels))
#' par(op)
#'
#' # long-format data: sorted without reshaping, covariates kept
#' set.seed(2)
#' long <- data.frame(pid = rep(sprintf("P%03d", 1:100), each = 6),
#'                    wave = rep(1:6, 100),
#'                    status = c(t(example3)),
#'                    site = rep(sample(c("North", "South"), 100, TRUE),
#'                               each = 6))
#' d <- sorter(long, id = "pid", time = "wave", state = "status",
#'             group = "site")
#' head(d)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   print(
#'     ggplot2::ggplot(d, ggplot2::aes(x = wave, xend = wave + 1, y = row,
#'                                     yend = row, colour = factor(status))) +
#'       ggplot2::geom_segment() +
#'       ggplot2::facet_grid(site ~ ., scales = "free_y", space = "free_y")
#'   )
#' }
#'
#' # weighted cases: bands as tall as their weights
#' long$w <- rep(stats::rexp(100), each = 6)
#' dw <- sorter(long, id = "pid", time = "wave", state = "status",
#'              weight = "w")
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   print(
#'     ggplot2::ggplot(dw, ggplot2::aes(xmin = wave, xmax = wave + 1,
#'                                      ymin = ymin, ymax = ymax,
#'                                      fill = factor(status))) +
#'       ggplot2::geom_rect() +
#'       scale_fill_longcat()
#'   )
#' }
#' @md
#' @export
sorter <- function(lc, ...) UseMethod("sorter")

#' @rdname sorter
#' @md
#' @export
sorter.longCat <- function(lc,
                           ascending     = TRUE,
                           whichColumns  = NULL,
                           num           = TRUE,
                           mindur        = NULL,
                           igrpt         = FALSE,
                           customSort    = NULL,
                           initFirst     = FALSE,
                           group         = NULL,
                           groupLabels   = NULL,
                           ggap          = NULL,
                           sortDurations = NULL,
                           ...) {
  check_no_dots("sorter()", ...)
  validate_longCat(lc)
  if (lc$sorted) {
    stop("`lc` is already sorted. To sort it another way, sort the unsorted ",
         "object again, e.g.:\n",
         "  lc <- longCat(y)\n",
         "  lc_by_g1 <- sorter(lc, group = g1)\n",
         "  lc_by_cs <- sorter(lc, customSort = cs)", call. = FALSE)
  }
  for (arg in c("ascending", "num", "igrpt", "initFirst")) {
    if (!is_flag(get(arg))) {
      stop(sprintf("`%s` must be TRUE or FALSE.", arg), call. = FALSE)
    }
  }
  if (is.null(sortDurations)) sortDurations <- lc$IndTime
  if (!is_flag(sortDurations)) {
    stop("`sortDurations` must be NULL, TRUE, or FALSE.", call. = FALSE)
  }

  n <- nrow(lc$y)
  whichColumns <- check_which_columns(whichColumns, ncol(lc$y))
  customSort <- check_custom_sort(customSort, n)
  groups <- prepare_groups(group, groupLabels, n)
  gap <- gap_rows(ggap, n, groups$n)

  # the states to sort on: blank short durations (using the full times so
  # each cell's own interval is measured), select the sorting columns, and
  # collapse repeats
  y_masked <- lc$y
  if (!is.null(mindur)) y_masked <- mask_short_durations(y_masked, lc$times, mindur)
  y_masked <- y_masked[, whichColumns, drop = FALSE]
  y_sort <- if (igrpt) collapse_repeats_rows(y_masked) else y_masked

  keys <- sequence_sort_keys(y_sort, num)
  if (sortDurations) {
    durations <- interval_durations(lc$times, n, ncol(lc$y))[, whichColumns, drop = FALSE]
    keys <- cbind(keys, duration_sort_keys(y_masked, durations, num, igrpt))
  }

  order_y <- lc$order.y
  order_y[, 2] <- makePatterns(y_sort, num = num)
  order_y[, 3] <- customSort

  init <- if (initFirst) lc$y[, 1] else rep(1, n)
  rows <- sorted_rows(groups$codes, init, customSort, keys, ascending, gap)

  lc <- unclass(lc)
  lc$order.y <- order_y
  lc$ascending <- ascending
  lc$group <- groups$codes
  lc$groupLabels <- groups$labels
  new_longCat(set_sorted_fields(lc, rows))
}

#' @rdname sorter
#' @md
#' @export
sorter.data.frame <- function(lc, id = "id", time = "time", state = "state",
                              stop = NULL, end = NULL, group = NULL,
                              customSort = NULL, weight = NULL, into = "row",
                              ...) {
  x <- lc
  check_column_names(x, list(id = id, time = time, state = state), "lc")
  check_new_column(x, into, "lc", "the plot rows")
  if (!is.null(weight)) {
    clash <- intersect(c("ymin", "ymax"), names(x))
    if (length(clash) > 0L) {
      stop(sprintf(paste("`lc` already has a column \"%s\"; rename it to add",
                         "the band edges `ymin` and `ymax`."), clash[1]),
           call. = FALSE)
    }
  }

  # number the cases in ID order, so that ties do not depend on the order
  # of the rows of `x`
  ids <- unique(x[[id]][order(x[[id]], method = "radix")])
  if (anyNA(ids)) stop("The case IDs in `lc` must not be missing.", call. = FALSE)
  case <- match(x[[id]], ids)
  by_id <- x[order(case), , drop = FALSE]
  wide <- as.longCat(by_id, id = id, time = time, state = state, stop = stop,
                     end = end, weight = weight)
  sorted <- sorter.longCat(
    wide,
    group = case_column(x, group, case, length(ids), "group"),
    customSort = case_column(x, customSort, case, length(ids), "customSort"),
    ...
  )

  # each case's plot row; NA for cases left out for a missing group
  plot_row <- match(seq_along(ids), sorted$order.y.sorted[, 4])
  x[[into]] <- plot_row[case]
  if (!is.null(weight)) {
    # the bands of plot.longCat()
    pd <- plot_data(sorted)
    center <- pd$row_center[x[[into]]]
    half <- pd$row_height[x[[into]]] / 2
    x$ymin <- center - half
    x$ymax <- center + half
  }
  keep <- !is.na(x[[into]])
  out <- x[keep, , drop = FALSE]
  out <- out[order(out[[into]], x[[time]][keep]), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' @rdname sorter
#' @md
#' @export
sorter.default <- function(lc, ...) {
  stop("`lc` must be a longCat object or a data frame in long format.",
       call. = FALSE)
}

# One value per case from the column `column` of the long data `x`, whose
# rows belong to cases `case` (1 to n_cases); NULL when `column` is NULL.
# The column must be the same in every row of a case.
case_column <- function(x, column, case, n_cases, arg, data_arg = "lc") {
  if (is.null(column)) return(NULL)
  check_column_names(x, stats::setNames(list(column), arg), data_arg)
  values <- x[[column]]
  per_case <- values[match(seq_len(n_cases), case)]
  if (!identical(per_case[case], values)) {
    stop(sprintf("Column \"%s\" (`%s`) must be the same in every row of a case.",
                 column, arg), call. = FALSE)
  }
  per_case
}

# Mark the (unclassed) longCat list `lc` as sorted with plot rows `rows`
# (rows of `y`, NA for blank rows), filling the sorted copies of its fields.
# `lc$group`, `lc$ascending`, and the first columns of `order.y` must
# already be set; the fourth column of `order.y` becomes the row numbers.
set_sorted_fields <- function(lc, rows) {
  take <- function(x) if (!is.null(x)) unname_rows(x[rows, , drop = FALSE])
  lc$sorted <- TRUE
  lc$order.y[, 4] <- seq_len(nrow(lc$y))
  lc$y.sorted <- take(lc$y)
  lc$times.sorted <- take(lc$times)
  lc$order.y.sorted <- take(lc$order.y)
  lc$group.sorted <- matrix(lc$group[rows], ncol = 1)
  lc$events.sorted <- take(lc$events)
  lc$event.times.sorted <- take(lc$event.times)
  lc
}

# The number of blank rows after each group in the sorted object `lc`.
blank_rows_per_group <- function(lc) {
  plotted_groups <- lc$group.sorted[, 1]
  n_groups <- length(unique(plotted_groups[!is.na(plotted_groups)]))
  if (n_groups > 1L) sum(is.na(plotted_groups)) %/% n_groups else 0L
}

# The (unclassed) longCat list `lc` without any sorting.
unsorted_fields <- function(lc) {
  for (field in c("y.sorted", "times.sorted", "order.y.sorted", "group.sorted",
                  "events.sorted", "event.times.sorted", "ascending", "group",
                  "groupLabels")) {
    lc[field] <- list(NULL)
  }
  lc$sorted <- FALSE
  lc$order.y[, 2:4] <- NA
  lc
}

# ---- argument checks ----------------------------------------------------------

check_which_columns <- function(whichColumns, n_col) {
  if (is.null(whichColumns)) return(seq_len(n_col))
  if (!is.numeric(whichColumns) || length(whichColumns) == 0L ||
      anyNA(whichColumns) || any(whichColumns != round(whichColumns)) ||
      any(whichColumns < 1 | whichColumns > n_col)) {
    stop(sprintf("`whichColumns` must be column numbers between 1 and %d.",
                 n_col), call. = FALSE)
  }
  as.integer(whichColumns)
}

check_custom_sort <- function(customSort, n) {
  if (is.null(customSort)) return(rep(1, n))
  if (!is.atomic(customSort) || length(customSort) != n) {
    stop(sprintf("`customSort` must have one value per case (%d), not %d.",
                 n, length(customSort)), call. = FALSE)
  }
  if (anyNA(customSort)) {
    stop("`customSort` must not contain missing values. Consider sorting a ",
         "subset of the cases, or giving the missing values their own group:\n",
         "  missing <- is.na(customSort)\n",
         "  customSort[missing] <- min(customSort, na.rm = TRUE)\n",
         "  sorter(lc, customSort = customSort, group = missing,\n",
         "         groupLabels = c(\"Observed\", \"Missing\"))", call. = FALSE)
  }
  customSort
}

# Group codes (numeric, in group order; NA for a missing group), labels, and
# the number of groups.
prepare_groups <- function(group, groupLabels, n) {
  if (is.null(group)) {
    if (!is.null(groupLabels)) {
      stop("`groupLabels` requires `group`.", call. = FALSE)
    }
    return(list(codes = rep(1, n), labels = NULL, n = 1L))
  }
  if (!is.atomic(group) || length(group) != n) {
    stop(sprintf("`group` must have one value per case (%d), not %d.",
                 n, length(group)), call. = FALSE)
  }
  if (anyNA(group)) {
    warning(sprintf(paste0(
      "%d case(s) with a missing `group` are left out of the sorted data. ",
      "To keep them, give them their own group, e.g.:\n",
      "  group[is.na(group)] <- \"Missing\""), sum(is.na(group))),
      call. = FALSE)
  }
  if (is.numeric(group)) {
    codes <- as.numeric(group)
    values <- sort(unique(codes))
  } else {
    group <- droplevels(as.factor(group))
    codes <- as.numeric(group)
    values <- levels(group)
  }
  if (length(values) == 0L) stop("`group` has no observed values.", call. = FALSE)
  labels <- match_labels(groupLabels, values, "groupLabels", "group")
  list(codes = codes, labels = labels, n = length(values))
}

gap_rows <- function(ggap, n, n_groups) {
  if (is.null(ggap)) ggap <- if (n_groups > 1L) 0.05 else 0
  if (!is.numeric(ggap) || length(ggap) != 1L || is.na(ggap) ||
      ggap < 0 || ggap > 1) {
    stop("`ggap` must be a single number between 0 and 1.", call. = FALSE)
  }
  as.integer(ceiling(ggap * n))
}

# ---- ordering -------------------------------------------------------------------

# Row indices of the sorted data: cases ordered within groups, groups in
# ascending order, `gap` NA indices after each group when there are two or
# more groups. Cases with a missing group are left out.
sorted_rows <- function(group, init, custom, keys, ascending, gap) {
  in_group <- which(!is.na(group))
  ord <- in_group[order_cases(init[in_group], custom[in_group],
                              keys[in_group, , drop = FALSE], ascending,
                              group = group[in_group])]
  insert_gaps(ord, group, gap)
}

# The rows `ord` (in plot order, grouped) with `gap` NA rows after each
# group, when there are two or more groups.
insert_gaps <- function(ord, group, gap) {
  group_values <- unique(group[ord])
  if (length(group_values) < 2L || gap == 0L) return(ord)
  blocks <- lapply(group_values, function(g) {
    c(ord[group[ord] == g], rep(NA_integer_, gap))
  })
  unlist(blocks, use.names = FALSE)
}

#' Duration keys for the secondary sort
#'
#' The time spent in each state of the sequence that the state keys compare,
#' in the same positions: left-packed when `num = TRUE`, in place when
#' `num = FALSE`, and summed over each run when `igrpt = TRUE`.
#'
#' @param y_masked States in the sort columns after `mindur`, before
#'   collapsing repeats.
#' @param durations Interval durations for the same cells.
#' @inheritParams sequence_sort_keys
#' @param igrpt Logical; whether runs were collapsed.
#' @return A numeric matrix the shape of `y_masked`; `NA` where there is no
#'   state.
#' @keywords internal
#' @md
#' @noRd
duration_sort_keys <- function(y_masked, durations, num, igrpt) {
  observed <- !is.na(y_masked)
  durations[!observed] <- NA
  if (!igrpt) {
    return(if (num) left_pack_rows(durations, observed) else durations)
  }
  # total duration of each run, placed at the run's position in its row (the
  # r-th run of a row is at column r, as in collapse_repeats_rows())
  starts <- run_starts(y_masked)
  run_index <- matrix(as.integer(starts), nrow(starts), ncol(starts))
  for (j in seq_len(ncol(starts))[-1L]) {
    run_index[, j] <- run_index[, j - 1L] + run_index[, j]
  }
  n_col <- ncol(y_masked)
  cell <- (row(y_masked)[observed] - 1L) * n_col + run_index[observed]
  totals <- rowsum(durations[observed], cell, reorder = FALSE)
  cell <- as.integer(rownames(totals))

  out <- durations
  out[] <- NA
  out[cbind((cell - 1L) %/% n_col + 1L, (cell - 1L) %% n_col + 1L)] <- totals[, 1L]
  out
}

unname_rows <- function(x) {
  rownames(x) <- NULL
  x
}
