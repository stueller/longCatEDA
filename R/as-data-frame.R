# Long-format data frames from longCat and longCont objects.

#' Convert longCat and longCont objects to long-format data frames
#'
#' Returns the data of a [longCat()] or [longCont()] object in long format,
#' one row per case and time point, for use with other tools such as
#' ggplot2, dplyr, or modelling functions.
#'
#' For a `longCat` object, `what = "states"` gives one row per interval
#' (cell of `y`) and `what = "events"` one row per observed event. When the
#' object has been sorted with [sorter()], rows come in plot order and
#' include the plot row and, with groups, the group; cases that
#' [sorter()] left out because their group is missing are not included.
#' Use `sorted = FALSE` to get the unsorted data in their original order.
#'
#' @param x An object of class `longCat` or `longCont`.
#' @param row.names `NULL` (the default) or a character vector of row names.
#' @param optional Unused; included for consistency with the generic.
#' @param ... Unused; included for consistency with the generic.
#' @param what `"states"` (the default) for the states in each interval, or
#'   `"events"` for the events.
#' @param sorted Logical. Use the sorted data, in plot order? Defaults to
#'   `TRUE` for sorted objects. Must be `FALSE` for unsorted ones.
#' @param na.rm Logical. Leave out intervals or time points whose state or
#'   value is missing? Default is `FALSE`. Missing events are always left
#'   out.
#' @return A data frame. For `what = "states"`, the columns are
#'   * `id`: the case ID (the `id` given to [longCat()], or the row of `y`).
#'   * `row`: the plot row, counted from the bottom (sorted data only).
#'   * `group`: the group, as a factor labelled with `groupLabels` (sorted
#'     data with groups only).
#'   * `weight`: the case weight (only when the object has `weights`).
#'   * `occasion`: the time point (measurement occasion), numbered from 1:
#'     the column of `y`.
#'   * `tLabel`: the label of the column, as a factor (only when the object
#'     has `tLabels`).
#'   * `start`, `stop`: the start and stop times of the interval.
#'   * `state`: the state, as a factor labelled with `Labels`.
#'
#'   For `what = "events"`, the columns are `id`, `row`, `group`, and
#'   `weight` (as above), `event` (a factor labelled with `eventLabels`),
#'   and `time`.
#'
#'   For a `longCont` object, the columns are `id` (the row of `y`),
#'   `occasion`, `time`, and `value`.
#' @seealso [longCat()], [longCont()]
#' @examples
#' lc <- longCat(example3[1:3, ], times = c(0, 1, 2, 3, 4, 5, 6))
#' head(as.data.frame(lc))
#'
#' # sorted and grouped: rows in plot order
#' s <- sorter(lc, group = c("a", "b", "a"))
#' as.data.frame(s, na.rm = TRUE)[1:8, ]
#'
#' # continuous data
#' head(as.data.frame(longCont(example2cont)))
#'
#' # a horizontal line plot drawn with ggplot2
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   d <- as.data.frame(sorter(longCat(example3)), na.rm = TRUE)
#'   print(ggplot2::ggplot(d, ggplot2::aes(x = start, xend = stop, y = row,
#'                                         yend = row, colour = state)) +
#'           ggplot2::geom_segment() +
#'           ggplot2::scale_colour_manual(values = colChoose("heat", 5)))
#' }
#' @md
#' @name as.data.frame.longCat
NULL

#' @rdname as.data.frame.longCat
#' @md
#' @export
as.data.frame.longCat <- function(x, row.names = NULL, optional = FALSE, ...,
                                  what = c("states", "events"),
                                  sorted = x$sorted, na.rm = FALSE) {
  validate_longCat(x, "x")
  what <- match.arg(what)
  if (!is_flag(sorted)) stop("`sorted` must be TRUE or FALSE.", call. = FALSE)
  if (sorted && !x$sorted) {
    stop("`x` is not sorted; use `sorted = FALSE` or sort it with sorter().",
         call. = FALSE)
  }
  if (!is_flag(na.rm)) stop("`na.rm` must be TRUE or FALSE.", call. = FALSE)
  cases <- case_table(x, sorted)
  out <- if (what == "states") {
    state_table(x, cases, sorted, na.rm)
  } else {
    event_table(x, cases, sorted)
  }
  set_row_names(out, row.names)
}

#' @rdname as.data.frame.longCat
#' @md
#' @export
as.data.frame.longCont <- function(x, row.names = NULL, optional = FALSE, ...,
                                   na.rm = FALSE) {
  validate_longCont(x, "x")
  if (!is_flag(na.rm)) stop("`na.rm` must be TRUE or FALSE.", call. = FALSE)
  cell <- cell_index(nrow(x$y), ncol(x$y))
  out <- data.frame(id = cell[, 1], occasion = cell[, 2], time = x$times[cell],
                    value = x$y[cell])
  if (na.rm) out <- out[!is.na(out$value), , drop = FALSE]
  set_row_names(out, row.names)
}

# Row and column indices of the cells of an n_row x n_col matrix, case by
# case (all columns of row 1, then row 2, ...).
cell_index <- function(n_row, n_col) {
  cbind(rep(seq_len(n_row), each = n_col), rep(seq_len(n_col), times = n_row))
}

# The cases to output, one row each: `index` is the row of the (sorted or
# unsorted) data, `id` the case ID, and for sorted data `row` the plot row
# and `group` the group. Blank rows between groups are dropped.
case_table <- function(x, sorted) {
  if (!sorted) {
    cases <- data.frame(index = seq_len(nrow(x$y)), id = x$order.y[, 1])
    if (!is.null(x$weights)) cases$weight <- x$weights
    return(cases)
  }
  ids <- x$order.y.sorted[, 1]
  keep <- which(!is.na(x$group.sorted[, 1]))
  cases <- data.frame(index = keep, id = ids[keep], row = keep)
  if (!is.null(x$groupLabels)) {
    cases$group <- factor(x$group.sorted[keep, 1],
                          levels = seq_along(x$groupLabels),
                          labels = x$groupLabels)
  }
  if (!is.null(x$weights)) {
    cases$weight <- x$weights[x$order.y.sorted[keep, 4]]
  }
  cases
}

# The case columns (all but `index`) repeated for the rows `i` of `cases`;
# indexing each column is much faster than indexing the data frame.
case_columns <- function(cases, i) {
  columns <- setdiff(names(cases), "index")
  data.frame(lapply(cases[columns], function(column) column[i]))
}

state_table <- function(x, cases, sorted, na.rm) {
  y <- if (sorted) x$y.sorted else x$y
  times <- if (sorted) x$times.sorted else x$times
  n_col <- ncol(y)
  cell <- cell_index(nrow(cases), n_col)
  rows <- cases$index[cell[, 1]]
  out <- case_columns(cases, cell[, 1])
  out$occasion <- cell[, 2]
  if (!is.null(x$tLabels)) {
    out$tLabel <- factor(x$tLabels[cell[, 2]], levels = unique(x$tLabels))
  }
  out$start <- times[cbind(rows, cell[, 2])]
  out$stop <- times[cbind(rows, cell[, 2] + 1L)]
  out$state <- factor(y[cbind(rows, cell[, 2])], levels = seq_len(x$nfactors),
                      labels = x$Labels)
  if (na.rm) out <- out[!is.na(out$state), , drop = FALSE]
  out
}

event_table <- function(x, cases, sorted) {
  events <- if (sorted) x$events.sorted else x$events
  event_times <- if (sorted) x$event.times.sorted else x$event.times
  columns <- c(setdiff(names(cases), "index"), "event", "time")
  if (is.null(events)) {
    out <- case_columns(cases, integer(0))
    out$event <- factor(character(0))
    out$time <- numeric(0)
    return(out[columns])
  }
  cell <- cell_index(nrow(cases), ncol(events))
  rows <- cases$index[cell[, 1]]
  codes <- events[cbind(rows, cell[, 2])]
  observed <- !is.na(codes)
  out <- case_columns(cases, cell[observed, 1])
  out$event <- factor(codes[observed], levels = seq_along(x$eventLabels),
                      labels = x$eventLabels)
  out$time <- event_times[cbind(rows, cell[, 2])][observed]
  out
}

set_row_names <- function(out, row.names) {
  if (is.null(row.names)) {
    rownames(out) <- NULL
  } else {
    if (length(row.names) != nrow(out)) {
      stop(sprintf("`row.names` must have length %d.", nrow(out)), call. = FALSE)
    }
    rownames(out) <- row.names
  }
  out
}
