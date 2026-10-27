#' Create a longCat object
#'
#' Collects categorical longitudinal data, their observation times, and
#' optional events into an object of class `longCat` for sorting with
#' [sorter()] and plotting with [plot.longCat()].
#'
#' States are recoded to the integers `1` to the number of states. The order
#' of the states (which sets their colors and legend order) is:
#'
#' * for factor columns, the order of their levels (levels that never occur
#'   are dropped);
#' * for numbers, increasing order;
#' * for text, increasing numeric order if every value looks like a number
#'   (so `"9"` comes before `"10"`), otherwise alphabetical order.
#'
#' All columns are recoded together, so a state has the same code in every
#' column. Events are recoded in the same way.
#'
#' @param y A matrix or data frame of states in wide format, with cases in
#'   rows and repeated observations in columns. States may be numbers, text,
#'   or factors. At most 9 distinct states are recommended (see
#'   [colChoose()]).
#' @param times Start and stop times of the intervals in `y`: `NULL` (the
#'   default; intervals `0:ncol(y)`), a vector of length `ncol(y) + 1` shared
#'   by all cases, or a matrix (or data frame) with `nrow(y)` rows and
#'   `ncol(y) + 1` columns for individually varying times. Each case's times
#'   must not decrease. Negative times are allowed, e.g., when time is
#'   centered at an intervention.
#' @param Labels Labels for the states, one per state in the order described
#'   above. Defaults to the states' original values.
#' @param tLabels Labels for the time points, one per column of `y`. Ignored
#'   (with a warning) when times vary across cases.
#' @param id Optional identifiers for the cases, one per row of `y`. Stored
#'   in the first column of `order.y`. Defaults to `1:nrow(y)`.
#' @param events Optional matrix or data frame of instantaneous events (or
#'   very short-lived states), with one row per case and as many columns as
#'   needed. Events may be numbers, text, or factors and are recoded like the
#'   states.
#' @param event.times The times of `events`: numeric, with the same dimension
#'   as `events`. Required with `events`.
#' @param eventLabels Labels for the events, one per distinct event in the
#'   order described above. Defaults to the events' original values.
#' @param weights Optional case weights, e.g., survey weights: one finite,
#'   non-negative number per row of `y`. [plot.longCat()] draws each case as
#'   a band whose height is proportional to its weight. See also
#'   [expand_weights()].
#' @return An S3 object of class `longCat`: a named list that always
#'   contains every one of the components below (unused components are
#'   `NULL`). Objects are checked by [validate_longCat()] when created and
#'   when passed to [sorter()] or [plot.longCat()].
#'
#'   * `y`: integer matrix of states recoded to `1:nfactors`.
#'   * `dim`: `dim(y)`.
#'   * `times`: numeric matrix with `nrow(y)` rows and `ncol(y) + 1` columns
#'     of interval start and stop times.
#'   * `Labels`, `tLabels`: labels for the states and time points.
#'   * `factors`, `nfactors`: the recoded state values (`1:nfactors`) and
#'     their number.
#'   * `IndTime`: `TRUE` if `times` was given as a matrix.
#'   * `order.y`: data frame of case IDs and sorting keys.
#'   * `events`, `event.times`, `eventLabels`: event codes (integers),
#'     times, and labels, or `NULL`.
#'   * `weights`: the case weights (numeric), or `NULL`.
#'   * `sorted`, `ascending`: sorting status; set by [sorter()].
#'   * `group`, `groupLabels`: grouping used by [sorter()], or `NULL`.
#'   * `y.sorted`, `times.sorted`, `order.y.sorted`, `group.sorted`,
#'     `events.sorted`, `event.times.sorted`: sorted copies set by
#'     [sorter()]; `NULL` until the object is sorted.
#' @references Tueller, S. J., Van Dorn, R. A., & Bobashev, G. V. (2016).
#'   Visualization of categorical longitudinal and times series data (Report
#'   No. MR-0033-1602). Research Triangle Park, NC: RTI Press.
#'   \doi{10.3768/rtipress.2016.mr.0033.1602}
#' @author Stephen J. Tueller
#' @seealso [plot.longCat()] to plot `longCat` objects; [longCat-methods] for
#'   `print()` and `summary()`.
#' @examples
#' # create the longCat object similar to Figure 2 in Tueller et al. (2016)
#' times <- c(1, 100, 200, 300, 400, 500, 600)
#' f2lc <- longCat(example2cat, times)
#' f2lc
#' summary(f2lc)
#'
#' # compare growth curves to the horizontal line plot
#' op <- par(mfrow = c(1, 2), bg = "cornsilk3")
#' plot(longCont(example2cat, times), ylim = c(1, 5), main = "Growth Curves",
#'      xlab = "Days")
#' plot(f2lc, lwd = 4, main = "Horizontal Line Plot",
#'      colScheme = "heat", legendBuffer = .2)
#' par(op)
#'
#' # individually varying times of observation
#' set.seed(642531)
#' y <- matrix(sample(1:5, 500, replace = TRUE), 100, 5)
#' set.seed(963854)
#' times <- matrix(runif(600, 1, 3), 100, 6)
#' times <- t(apply(times, 1, cumsum))  # times must be cumulative
#' lc <- longCat(y, times = times)
#' op <- par(bg = "cornsilk3", mar = c(5.1, 4.1, 4.1, 10.1), xpd = TRUE)
#' cols <- plot(lc, legendBuffer = 0, groupBuffer = 0,
#'              main = "Individually Varying Times of Observation")
#' legend(15.5, 100, legend = lc$Labels, lty = 1, col = cols, lwd = 2)
#' par(op)
#'
#' # superimpose events over states
#' set.seed(45962)
#' events <- matrix(sample(1:3, 200, replace = TRUE), 100, 2)
#' set.seed(23498)
#' event.times <- matrix(sample(c(times), 200, replace = FALSE), 100, 2)
#' labels <- c("Street", "Drug Tx", "Jail", "Prison", "Unknown")
#' eventLabels <- c("Arrest", "Drug Test", "Hearing")
#' lc <- longCat(y, times = times, Labels = labels,
#'               events = events, event.times = event.times,
#'               eventLabels = eventLabels)
#' op <- par(bg = "cornsilk3", mar = c(5.1, 4.1, 4.1, 12.1), xpd = TRUE)
#' cols <- plot(lc, legendBuffer = 0, groupBuffer = 0,
#'              main = "Superimpose Events Over States")
#' legend(15.5, 100, legend = lc$Labels, lty = 1, col = cols, lwd = 2)
#' legend(15.5, 40, legend = lc$eventLabels, pch = seq_along(lc$eventLabels))
#' par(op)
#'
#' # text states that are not ordered in time (e.g., item responses)
#' set.seed(1)
#' y <- matrix(sample(c("agree", "neutral", "disagree"), 300, replace = TRUE),
#'             100, 3)
#' lc <- longCat(y)
#' lc$Labels  # alphabetical
#'
#' # factor levels set the order of the states
#' ylev <- as.data.frame(lapply(as.data.frame(y), factor,
#'                              levels = c("disagree", "neutral", "agree")))
#' longCat(ylev)$Labels
#'
#' # more than 9 states gives a warning: colors become hard to tell apart
#' y <- matrix(sample(1:12, 500, replace = TRUE), 100, 5)
#' lc <- longCat(y)
#' @md
#' @export
longCat <- function(y, times = NULL, Labels = NULL, tLabels = NULL,
                    id = NULL, events = NULL,
                    event.times = NULL, eventLabels = NULL, weights = NULL) {
  states <- recode_values(y, "y")
  y <- states$codes
  factors <- check_levels(y)
  nfactors <- length(factors)
  Labels <- match_labels(Labels, states$values, "Labels", "state in `y`")

  # times given per case (a matrix) mark individually varying times
  IndTime <- !is.null(dim(times))
  times <- as_times_matrix(if (is.null(times)) 0:ncol(y) else times,
                           nrow(y), ncol(y), "y")
  if (!rows_nondecreasing(times)) {
    stop("Each case's `times` must not decrease.", call. = FALSE)
  }
  tLabels <- check_tLabels(tLabels, times)

  ev <- recode_events(events, event.times, eventLabels, nrow(y))

  new_longCat(list(
    y = y,
    dim = dim(y),
    times = times,
    Labels = Labels,
    tLabels = tLabels,
    factors = factors,
    IndTime = IndTime,
    nfactors = nfactors,
    sorted = FALSE,
    order.y = new_order_y(id, nrow(y)),
    events = ev$events,
    event.times = ev$event.times,
    eventLabels = ev$eventLabels,
    weights = check_weights(weights, nrow(y))
  ))
}

# ---- helpers ----------------------------------------------------------------

#' Recode states (or events) to integer codes
#'
#' @param x A matrix or data frame of states or events.
#' @param arg Name of `x` for error messages.
#' @return A list with `codes`, an integer matrix the shape of `x` with
#'   values `1:length(values)` (or `NA`), and `values`, the distinct observed
#'   values in code order (see [longCat()] for the ordering rules).
#' @keywords internal
#' @md
#' @noRd
recode_values <- function(x, arg) {
  if (!is.matrix(x) && !is.data.frame(x)) {
    stop(sprintf("`%s` must be a matrix or data frame with cases in rows.", arg),
         call. = FALSE)
  }
  if (nrow(x) == 0L || ncol(x) == 0L) {
    stop(sprintf("`%s` must have at least one row and one column.", arg),
         call. = FALSE)
  }

  level_order <- NULL
  if (is.data.frame(x)) {
    is_factor <- vapply(x, is.factor, logical(1))
    level_order <- unique(unlist(lapply(x[is_factor], levels)))
    values <- flatten_columns(x, is_factor)
  } else {
    if (!is.atomic(x)) stop(sprintf("`%s` must be atomic.", arg), call. = FALSE)
    values <- x
  }

  observed <- unique(values[!is.na(values)])
  if (length(observed) == 0L) {
    stop(sprintf("`%s` has no observed (non-missing) values.", arg),
         call. = FALSE)
  }
  in_levels <- level_order[level_order %in% observed]
  states <- c(in_levels, sort_values(setdiff(observed, in_levels)))

  codes <- matrix(match(values, states), nrow(values), ncol(values),
                  dimnames = dimnames(values))
  list(codes = codes, values = states)
}

# A data frame's columns as one matrix. Numeric (and logical) columns stay
# numeric; otherwise every column becomes text, column by column, so that
# numbers are not padded as as.matrix() would pad them.
flatten_columns <- function(x, is_factor) {
  simple <- vapply(x, function(col) is.numeric(col) || is.logical(col),
                   logical(1))
  row_names <- if (.row_names_info(x) > 0L) rownames(x)
  if (all(simple)) {
    columns <- lapply(x, as.numeric)
  } else {
    columns <- lapply(x, as.character)
  }
  matrix(unlist(columns, use.names = FALSE), nrow(x), ncol(x),
         dimnames = list(row_names, names(x)))
}

# Sort distinct values: numerically if they are numbers or text that all
# looks like numbers, otherwise as text.
sort_values <- function(values) {
  if (is.character(values)) {
    as_numbers <- suppressWarnings(as.numeric(values))
    if (!anyNA(as_numbers)) return(values[order(as_numbers)])
  }
  sort(values)
}

# Use the supplied labels, or default to the original values; one label per
# distinct value. `what` names one value, e.g. "state in `y`".
match_labels <- function(labels, values, arg, what) {
  if (is.null(labels)) return(values)
  if (!is.atomic(labels) || length(labels) != length(values)) {
    stop(sprintf("`%s` must have one label per %s (%d), not %d.", arg, what,
                 length(values), length(labels)),
         call. = FALSE)
  }
  labels
}

# tLabels apply only when every case has the same times.
check_tLabels <- function(tLabels, times) {
  if (is.null(tLabels)) return(NULL)
  if (!is.atomic(tLabels) || length(tLabels) != ncol(times) - 1L) {
    stop(sprintf("`tLabels` must have one label per column of `y` (%d), not %d.",
                 ncol(times) - 1L, length(tLabels)), call. = FALSE)
  }
  if (nrow(unique(times)) > 1L) {
    warning("`tLabels` are ignored because `times` vary across cases.",
            call. = FALSE)
    return(NULL)
  }
  tLabels
}

new_order_y <- function(id, n) {
  if (!is.null(id) && length(id) != n) {
    stop(sprintf("`id` must have one value per row of `y` (%d), not %d.",
                 n, length(id)), call. = FALSE)
  }
  order_y <- as.data.frame(matrix(NA, n, 4))
  order_y[, 1] <- if (is.null(id)) seq_len(n) else id
  order_y
}

# Check and recode events; returns NULLs when there are none.
recode_events <- function(events, event.times, eventLabels, n) {
  if (is.null(events) && is.null(event.times)) {
    return(list(events = NULL, event.times = NULL, eventLabels = NULL))
  }
  if (is.null(events) || is.null(event.times)) {
    stop("`events` and `event.times` must be given together.", call. = FALSE)
  }
  recoded <- recode_values(events, "events")
  event.times <- as_case_matrix(event.times, "event.times")
  if (!is.numeric(event.times)) {
    stop("`event.times` must be numeric.", call. = FALSE)
  }
  if (nrow(recoded$codes) != n) {
    stop(sprintf("`events` must have one row per row of `y` (%d), not %d.",
                 n, nrow(recoded$codes)), call. = FALSE)
  }
  if (!identical(dim(event.times), dim(recoded$codes))) {
    stop("`events` and `event.times` must have the same dimension.",
         call. = FALSE)
  }
  list(events = recoded$codes,
       event.times = event.times,
       eventLabels = match_labels(eventLabels, recoded$values, "eventLabels",
                                  "distinct event"))
}

# Case weights as a double vector with one value per case, or NULL.
check_weights <- function(weights, n, data_arg = "y") {
  if (is.null(weights)) return(NULL)
  if (!is.numeric(weights) || !is.null(dim(weights)) || length(weights) != n ||
      anyNA(weights) || any(!is.finite(weights)) || any(weights < 0) ||
      all(weights == 0)) {
    stop(sprintf(paste("`weights` must be one finite, non-negative number per",
                       "row of `%s` (%d), not all zero."), data_arg, n),
         call. = FALSE)
  }
  as.numeric(weights)
}
