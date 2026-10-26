# Building longCat and longCont objects from long-format data.

#' Create longCat and longCont objects from long-format data
#'
#' `as.longCat()` and `as.longCont()` convert data in long format (one row
#' per case and time) to the wide format used by [longCat()] and
#' [longCont()]. They reverse [as.data.frame.longCat()].
#'
#' Cases are kept in the order in which their IDs first appear.
#'
#' **Observation times.** When only `time` is given, each row is an
#' observation at that time:
#'
#' * If the cases share one schedule (there are no more distinct times than
#'   the most times observed for any one case), the columns are the
#'   distinct times, and a case not observed at a time is missing there.
#'   For `as.longCat()`, the state at each time lasts until the next time
#'   in the schedule, and the last until `end`.
#' * Otherwise times vary across cases: the columns are each case's first,
#'   second, ... observation. For `as.longCat()`, each state lasts until
#'   the case's next observation, and the last until `end`.
#'
#' **Intervals.** With `stop`, each row of an `as.longCat()` data set is an
#' interval from `time` to `stop`, as returned by [as.data.frame.longCat()].
#' A gap between one interval's stop and the case's next start becomes a
#' missing interval, and overlapping intervals are an error.
#'
#' **Round trips.** `as.longCat(as.data.frame(lc), time = "start", stop =
#' "stop")` gives back the states, times, and IDs of `lc` (and its events,
#' with `events = as.data.frame(lc, what = "events")`, in time order). The
#' labels come back as text, since [as.data.frame.longCat()] returns states
#' as factors. Intervals left out with `na.rm = TRUE` come back as missing
#' intervals, except at the end of a case, where they are lost.
#'
#' @param x A data frame in long format, or an object to convert.
#' @param id,time,state,value,stop Names of the columns of `x` holding the
#'   case IDs, the (start) times, the states (for `as.longCat()`) or values
#'   (for `as.longCont()`), and, optionally, the stop times of intervals.
#' @param end For observation times, when the last state ends: a single
#'   number, or `NULL` (the default) for one time step (the last spacing
#'   between times) after the last time.
#' @param events Optional data frame of events in long format, with the
#'   columns `id` and `event_time` and a column `event` of event codes.
#' @param event,event_time Names of the columns of `events` holding the
#'   events and their times.
#' @param ... Further arguments passed to [longCat()], such as `Labels` or
#'   `tLabels`.
#' @return A `longCat` or `longCont` object. The case IDs are stored as the
#'   `id` of [longCat()]. When the states, values, or events are factors,
#'   their levels give the order of the states and the default labels.
#' @seealso [as.data.frame.longCat()] for the reverse conversion.
#' @examples
#' # observations of a shared schedule (case 3 missed time 2)
#' d <- data.frame(id    = c(1, 1, 1, 2, 2, 2, 3, 3),
#'                 time  = c(0, 1, 2, 0, 1, 2, 0, 1),
#'                 state = c("a", "b", "b", "b", "a", "c", "c", "c"))
#' lc <- as.longCat(d)
#' lc$y
#' lc$times
#'
#' # a round trip through long format
#' lc <- longCat(example3)
#' d <- as.data.frame(lc)
#' lc2 <- as.longCat(d, time = "start", stop = "stop")
#' all.equal(lc$y, lc2$y, check.attributes = FALSE)
#'
#' # continuous data
#' as.longCont(data.frame(id = c(1, 1, 2, 2), time = c(1, 2, 1, 2),
#'                        value = c(0.5, 1.5, 2, 1)))
#' @md
#' @export
as.longCat <- function(x, ...) UseMethod("as.longCat")

#' @rdname as.longCat
#' @md
#' @export
as.longCat.longCat <- function(x, ...) validate_longCat(x, "x")

#' @rdname as.longCat
#' @md
#' @export
as.longCat.data.frame <- function(x, id = "id", time = "time", state = "state",
                                  stop = NULL, end = NULL, events = NULL,
                                  event = "event", event_time = "time", ...) {
  check_column_names(x, list(id = id, time = time, state = state), "x")
  if (!is.null(stop)) check_column_names(x, list(stop = stop), "x")
  cases <- case_index(x[[id]], "x")
  start <- check_long_times(x[[time]], time)
  layout <- if (is.null(stop)) {
    observation_layout(cases$case, start, end)
  } else {
    interval_layout(cases$case, start, check_long_times(x[[stop]], stop),
                    length(cases$ids))
  }
  check_one_per_cell(cases, layout)
  args <- list(...)
  y <- widen(x[[state]], cases$case, layout$column, length(cases$ids),
             layout$n_columns)
  ev <- if (!is.null(events)) {
    long_events(events, cases$ids, id, event, event_time)
  }
  do.call(longCat, c(list(y = y, times = layout$times, id = cases$ids,
                          events = ev$events, event.times = ev$event.times),
                     args))
}

#' @rdname as.longCat
#' @md
#' @export
as.longCont <- function(x, ...) UseMethod("as.longCont")

#' @rdname as.longCat
#' @md
#' @export
as.longCont.longCont <- function(x, ...) validate_longCont(x, "x")

#' @rdname as.longCat
#' @md
#' @export
as.longCont.data.frame <- function(x, id = "id", time = "time",
                                   value = "value", ...) {
  check_column_names(x, list(id = id, time = time, value = value), "x")
  cases <- case_index(x[[id]], "x")
  times <- check_long_times(x[[time]], time)
  layout <- observation_layout(cases$case, times, end = NULL)
  check_one_per_cell(cases, layout)
  y <- widen(x[[value]], cases$case, layout$column, length(cases$ids),
             layout$n_columns)
  # longCont() wants the observation times, without the end of the last
  # interval that observation_layout() adds for longCat()
  obs_times <- if (is.null(dim(layout$times))) {
    layout$times[seq_len(layout$n_columns)]
  } else {
    layout$times[, seq_len(layout$n_columns), drop = FALSE]
  }
  longCont(y, obs_times)
}

# ---- helpers ----------------------------------------------------------------

# Each element of `columns` must be a single string naming a column of `x`.
check_column_names <- function(x, columns, arg) {
  for (nm in names(columns)) {
    column <- columns[[nm]]
    if (!is.character(column) || length(column) != 1L || is.na(column)) {
      stop(sprintf("`%s` must be a single column name.", nm), call. = FALSE)
    }
    if (!column %in% names(x)) {
      stop(sprintf("`%s` has no column \"%s\" (given as `%s`).", arg, column,
                   nm), call. = FALSE)
    }
  }
}

# The distinct case IDs in order of appearance, and each row's case number.
case_index <- function(id, arg) {
  if (anyNA(id)) stop(sprintf("The case IDs in `%s` must not be missing.", arg),
                      call. = FALSE)
  ids <- unique(id)
  list(ids = if (is.factor(ids)) as.character(ids) else ids,
       case = match(id, ids))
}

check_long_times <- function(times, column) {
  if (!is.numeric(times)) {
    stop(sprintf("Column \"%s\" must be numeric.", column), call. = FALSE)
  }
  if (anyNA(times)) {
    stop(sprintf("Column \"%s\" must not have missing values.", column),
         call. = FALSE)
  }
  as.numeric(times)
}

# Columns and interval times for observations at `times`: a shared
# schedule when there are no more distinct times than the most times of
# any case, otherwise each case's observations in time order. The result
# has each row's `column`, `n_columns`, and `times` for longCat() (a
# vector for a shared schedule, otherwise a matrix), including the end
# of the last interval.
observation_layout <- function(case, times, end) {
  schedule <- sort(unique(times))
  per_case <- tabulate(case)
  if (length(schedule) <= max(per_case)) {
    return(list(column = match(times, schedule),
                n_columns = length(schedule),
                times = c(schedule, last_end(schedule[length(schedule)],
                                             schedule[length(schedule) - 1L],
                                             end))))
  }
  ord <- order(case, times)
  column <- integer(length(case))
  column[ord] <- sequence(per_case)
  n_columns <- max(per_case)
  bounds <- matrix(NA_real_, length(per_case), n_columns + 1L)
  bounds[cbind(case, column)] <- times
  rows <- seq_along(per_case)
  last <- bounds[cbind(rows, per_case)]
  previous <- bounds[cbind(rows, pmax(per_case - 1L, 1L))]
  previous[per_case == 1L] <- NA_real_
  bounds[cbind(rows, per_case + 1L)] <- last_end(last, previous, end)
  list(column = column, n_columns = n_columns, times = fill_right(bounds))
}

# The end of the last interval for each case with last and previous times
# `last` and `previous`: `end`, or one step (the last spacing, or 1 when
# there is none) after the last time.
last_end <- function(last, previous, end) {
  if (length(previous) == 0L) previous <- NA_real_
  if (!is.null(end)) {
    if (!is.numeric(end) || length(end) != 1L || is.na(end) ||
        any(end < last)) {
      stop("`end` must be a single number, no earlier than the last time.",
           call. = FALSE)
    }
    return(rep(end, length(last)))
  }
  step <- last - previous
  ifelse(is.na(step) | step <= 0, last + 1, last + step)
}

# Columns and interval times for intervals from `start` to `stop`. Each
# case's intervals are put in time order, with a missing interval for each
# gap; shorter cases are padded with zero-length missing intervals.
interval_layout <- function(case, start, stop, n_cases) {
  if (any(stop < start)) {
    stop("Each interval's stop time must not be before its start.",
         call. = FALSE)
  }
  ord <- order(case, start)
  case_o <- case[ord]
  start_o <- start[ord]
  stop_o <- stop[ord]
  same_case <- c(FALSE, case_o[-1] == case_o[-length(case_o)])
  previous_stop <- c(NA, stop_o[-length(stop_o)])
  if (any(same_case & start_o < previous_stop)) {
    stop("A case's intervals must not overlap.", call. = FALSE)
  }
  gap <- same_case & start_o > previous_stop
  # each interval takes one column, plus one before it for a gap
  column_o <- stats::ave(1L + gap, case_o, FUN = cumsum)
  column <- integer(length(case))
  column[ord] <- column_o
  n_columns <- max(column_o)

  bounds <- matrix(NA_real_, n_cases, n_columns + 1L)
  bounds[cbind(case_o, column_o)] <- start_o
  bounds[cbind(case_o, column_o + 1L)] <- stop_o
  bounds <- fill_right(bounds)
  shared <- all(bounds == rep(bounds[1, ], each = n_cases))
  times <- if (shared) bounds[1, ] else bounds
  list(column = column, n_columns = n_columns, times = times)
}

# Fill missing values with the last non-missing value to their left.
fill_right <- function(m) {
  for (j in seq_len(ncol(m))[-1]) {
    m[, j] <- ifelse(is.na(m[, j]), m[, j - 1L], m[, j])
  }
  m
}

check_one_per_cell <- function(cases, layout) {
  dup <- anyDuplicated(cbind(cases$case, layout$column))
  if (dup > 0L) {
    stop(sprintf("Case %s has more than one row for the same time.",
                 format(cases$ids[cases$case[dup]])), call. = FALSE)
  }
}

# A wide n_row x n_col version of `values` placed at (case, column). Factors
# become a data frame of factor columns with the same levels, so that
# longCat() keeps their order.
widen <- function(values, case, column, n_row, n_col) {
  if (is.factor(values)) {
    codes <- matrix(NA_integer_, n_row, n_col)
    codes[cbind(case, column)] <- as.integer(values)
    columns <- lapply(seq_len(n_col), function(j) {
      factor(levels(values)[codes[, j]], levels = levels(values))
    })
    return(stats::setNames(as.data.frame(columns), paste0("T", seq_len(n_col))))
  }
  if (!is.atomic(values)) {
    stop("States and values must be atomic.", call. = FALSE)
  }
  wide <- matrix(values[NA_integer_], n_row, n_col)
  wide[cbind(case, column)] <- values
  wide
}

# Wide `events` and `event.times` from a long data frame of events.
long_events <- function(events, ids, id, event, event_time) {
  if (!is.data.frame(events)) {
    stop("`events` must be a data frame.", call. = FALSE)
  }
  check_column_names(events,
                     list(id = id, event = event, event_time = event_time),
                     "events")
  event_ids <- events[[id]]
  if (is.factor(event_ids)) event_ids <- as.character(event_ids)
  case <- match(event_ids, ids)
  if (anyNA(case)) {
    stop("Every case ID in `events` must also be in `x`.", call. = FALSE)
  }
  times <- check_long_times(events[[event_time]], event_time)
  ord <- order(case, times)
  column <- integer(length(case))
  per_case <- tabulate(case, nbins = length(ids))
  column[ord] <- sequence(per_case[per_case > 0L])
  n_col <- max(1L, column)
  event_times <- matrix(NA_real_, length(ids), n_col)
  event_times[cbind(case, column)] <- times
  list(events = widen(events[[event]], case, column, length(ids), n_col),
       event.times = event_times)
}
