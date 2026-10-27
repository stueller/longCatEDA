#' Align times at the nth occurrence of a state or event
#'
#' Shifts each case's times so that time 0 is the start of the nth
#' occurrence of a state in `y` (or the time of the nth occurrence of an
#' event in `events`). For example, aligning at the 2nd occurrence of a
#' "jail" state puts every case's second jail spell at time 0. Event times
#' are shifted by the same amount.
#'
#' Give either `which.state` and `nth.state`, or `which.event` and
#' `nth.event`. Occurrences are counted per cell from left to right, so a
#' state lasting several consecutive intervals counts once per interval.
#' Cases with fewer than `nth` occurrences are aligned so that their last
#' observed time (of states or events) is 0, placing them entirely at or
#' before time 0.
#'
#' @param y A matrix or data frame of states, cases in rows (as `lc$y`).
#' @param times Interval start and stop times for `y`: a matrix or data frame
#'   with `nrow(y)` rows and `ncol(y) + 1` columns, or a vector of length
#'   `ncol(y) + 1` shared by all cases (as `times` in [longCat()]).
#' @param which.state The state to align at, or `NULL` to align at an event.
#' @param nth.state Which occurrence of `which.state` to align at (a positive
#'   integer).
#' @param not.state Logical. If `TRUE`, align at the nth cell that is *not*
#'   `which.state`.
#' @param events,event.times Matrices (or data frames) of events and their
#'   times, with `nrow(y)` rows; required when aligning at an event.
#' @param which.event The event to align at, or `NULL` to align at a state.
#' @param nth.event Which occurrence of `which.event` to align at (a
#'   positive integer).
#' @param not.event Logical. If `TRUE`, align at the nth event that is *not*
#'   `which.event`.
#' @return A list with
#'   * `aligned.times`: a numeric matrix the shape of `times`, shifted.
#'   * `aligned.event.times`: `event.times` as a matrix, shifted by the same
#'     amount, or `NULL` if `event.times` is `NULL`.
#'
#' @seealso [plot.longCat()], which can align on the fly with the same
#'   arguments.
#' @examples
#' # individually varying times of observation
#' set.seed(642531)
#' y <- matrix(sample(1:5, 500, replace = TRUE), 100, 5)
#' set.seed(963854)
#' times <- matrix(runif(600, 1, 3), 100, 6)
#' times <- t(apply(times, 1, cumsum))  # times must be cumulative
#'
#' # align at the 2nd instance of state 3
#' times23 <- alignTime(y, times, which.state = 3, nth.state = 2)$aligned.times
#' labels <- c("Street", "Drug Tx", "Jail", "Prison", "Unknown")
#' lc   <- longCat(y, times = times, Labels = labels)
#' lc23 <- longCat(y, times = times23, Labels = labels)
#' op <- par(mfrow = c(3, 1), bg = "cornsilk3")
#' plot(lc, main = "Raw Times", legendBuffer = .5, ylab = "")
#' plot(lc23, main = "Aligned: 2nd Instance of Jail",
#'      xlab = "Days Since 2nd Instance of Jail (via alignTime)",
#'      legendBuffer = .5, ylab = "")
#' # the same alignment from plot(); the order differs slightly because
#' # plot() sorts on the unaligned data
#' plot(lc, main = "Aligned: 2nd Instance of Jail (via plot)",
#'      legendBuffer = .5, which.state = 3, nth.state = 2, ylab = "",
#'      xlab = "Days Since 2nd Instance of Jail")
#' par(op)
#'
#' # align at the 1st instance of event 2
#' set.seed(45962)
#' events <- matrix(sample(1:3, 200, replace = TRUE), 100, 2)
#' set.seed(23498)
#' event.times <- matrix(sample(c(times), 200, replace = FALSE), 100, 2)
#' aligned <- alignTime(y, times, events = events, event.times = event.times,
#'                      which.event = 2, nth.event = 1)
#' eventLabels <- c("Arrest", "Drug Test", "Hearing")
#' lc <- longCat(y, times = times, Labels = labels,
#'               events = events, event.times = event.times,
#'               eventLabels = eventLabels)
#' lc12 <- longCat(y, times = aligned$aligned.times, Labels = labels,
#'                 events = events, event.times = aligned$aligned.event.times,
#'                 eventLabels = eventLabels)
#' op <- par(mfrow = c(2, 1), bg = "cornsilk3", mar = c(5.1, 4.1, 4.1, 12.1),
#'           xpd = TRUE)
#' plot(lc, legendBuffer = .5, main = "Superimpose Events", ylab = "")
#' plot(lc12, legendBuffer = .5, main = "Aligned: 1st Drug Test",
#'      xlab = "Days Since 1st Drug Test", ylab = "")
#' legend(15.5, 50, legend = lc$eventLabels, pch = seq_along(lc$eventLabels))
#' par(op)
#' @md
#' @export
alignTime <- function(y, times,
                      which.state = NULL, nth.state = NULL, not.state = FALSE,
                      events = NULL, event.times = NULL,
                      which.event = NULL, nth.event = NULL, not.event = FALSE) {
  y <- as_case_matrix(y, "y")
  times <- as_times_matrix(times, nrow(y), ncol(y), "y")
  if (!is.null(event.times)) {
    event.times <- as_case_matrix(event.times, "event.times")
    if (nrow(event.times) != nrow(y)) {
      stop("`event.times` must have nrow(y) rows.", call. = FALSE)
    }
  }

  by_state <- check_alignment_target(which.state, nth.state, not.state,
                                     which.event, nth.event, not.event)
  if (by_state) {
    position <- nth_match_col(y, which.state, nth.state, not.state)
    anchor_times <- times
  } else {
    if (is.null(events) || is.null(event.times)) {
      stop("Aligning at an event requires `events` and `event.times`.",
           call. = FALSE)
    }
    events <- as_case_matrix(events, "events")
    if (!identical(dim(events), dim(event.times))) {
      stop("`events` and `event.times` must have the same dimension.",
           call. = FALSE)
    }
    position <- nth_match_col(events, which.event, nth.event, not.event)
    anchor_times <- event.times
  }

  # time of the nth occurrence; cases without one are aligned at their last
  # observed time
  anchor <- anchor_times[cbind(seq_len(nrow(y)), position)]
  missing_anchor <- is.na(position)
  anchor[missing_anchor] <- last_time(times, event.times)[missing_anchor]

  list(aligned.times = times - anchor,
       aligned.event.times = if (!is.null(event.times)) event.times - anchor)
}

# ---- helpers ----------------------------------------------------------------

# Check that exactly one of a state or an event target is fully specified;
# returns TRUE when aligning at a state.
check_alignment_target <- function(which.state, nth.state, not.state,
                                   which.event, nth.event, not.event) {
  by_state <- !is.null(which.state)
  by_event <- !is.null(which.event)
  if (by_state == by_event) {
    stop("Give exactly one of `which.state` or `which.event`.", call. = FALSE)
  }
  which <- if (by_state) "state" else "event"
  value <- if (by_state) which.state else which.event
  nth <- if (by_state) nth.state else nth.event
  other_nth <- if (by_state) nth.event else nth.state
  negate <- if (by_state) not.state else not.event

  if (!is.atomic(value) || length(value) != 1L || is.na(value)) {
    stop(sprintf("`which.%s` must be a single value.", which), call. = FALSE)
  }
  if (!is_whole_scalar(nth) || nth < 1) {
    stop(sprintf("`nth.%s` must be a single positive integer.", which),
         call. = FALSE)
  }
  if (!is.null(other_nth)) {
    stop(sprintf("`nth.%s` cannot be used when aligning at %s %s.",
                 if (by_state) "event" else "state",
                 if (by_state) "a" else "an", which), call. = FALSE)
  }
  if (!is_flag(negate)) {
    stop(sprintf("`not.%s` must be TRUE or FALSE.", which), call. = FALSE)
  }
  by_state
}

#' Column of the nth match in each row of a matrix
#'
#' @param m A matrix.
#' @param value The value to match.
#' @param n Which match to find.
#' @param negate If `TRUE`, match cells that are observed and not `value`.
#' @return An integer vector with one element per row: the column of the nth
#'   match, or `NA` if the row has fewer than `n` matches. Missing cells never
#'   match.
#' @keywords internal
#' @md
#' @noRd
nth_match_col <- function(m, value, n, negate = FALSE) {
  hits <- if (negate) m != value else m == value
  hits[is.na(hits)] <- FALSE
  count <- matrix(as.integer(hits), nrow(hits), ncol(hits))
  for (j in seq_len(ncol(count))[-1L]) count[, j] <- count[, j - 1L] + count[, j]

  at <- which(hits & count == n, arr.ind = TRUE)
  position <- rep(NA_integer_, nrow(m))
  position[at[, 1L]] <- at[, 2L]
  position
}

# Latest time per row across `times` and (if given) `event.times`; NA when a
# row has no times at all.
last_time <- function(times, event.times = NULL) {
  all_times <- cbind(times, event.times)
  columns <- lapply(seq_len(ncol(all_times)), function(j) all_times[, j])
  do.call(pmax, c(columns, na.rm = TRUE))
}
