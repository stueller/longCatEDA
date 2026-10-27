# Input checks shared by functions that take case-by-time data.

# `x` as a matrix, with cases in rows.
as_case_matrix <- function(x, arg) {
  if (!is.matrix(x) && !is.data.frame(x)) {
    stop(sprintf("`%s` must be a matrix or data frame with cases in rows.", arg),
         call. = FALSE)
  }
  as.matrix(x)
}

# Interval start and stop times as an n_row x (n_col + 1) numeric matrix.
# `times` may be such a matrix (or data frame) or a vector of n_col + 1 times
# shared by all rows. `data_arg` names the data in error messages.
as_times_matrix <- function(times, n_row, n_col, data_arg) {
  if (is.data.frame(times)) times <- as.matrix(times)
  if (!is.numeric(times)) stop("`times` must be numeric.", call. = FALSE)
  if (is.null(dim(times))) {
    if (length(times) != n_col + 1L) {
      stop(sprintf("`times` must have length ncol(%s) + 1.", data_arg),
           call. = FALSE)
    }
    times <- matrix(times, n_row, n_col + 1L, byrow = TRUE)
  } else if (!identical(dim(times),
                        c(as.integer(n_row), as.integer(n_col) + 1L))) {
    stop(sprintf("`times` must have nrow(%s) rows and ncol(%s) + 1 columns.",
                 data_arg, data_arg), call. = FALSE)
  }
  storage.mode(times) <- "double"
  times
}

# An error naming any arguments in `...`, for methods that must take `...`
# (to match their generic) but use no further arguments; `what` names the
# function in the message.
check_no_dots <- function(what, ...) {
  if (...length() == 0L) return(invisible())
  nms <- names(list(...))
  if (is.null(nms)) nms <- rep("", ...length())
  nms[nms == ""] <- "(unnamed)"
  stop(sprintf("Unknown arguments to %s: %s.", what,
               paste(nms, collapse = ", ")), call. = FALSE)
}

# `into` must be a single new column name for the data frame `x` (named
# `arg` in messages); `what` describes the column in the message.
check_new_column <- function(x, into, arg, what) {
  if (!is.character(into) || length(into) != 1L || is.na(into) ||
      !nzchar(into)) {
    stop("`into` must be a single column name.", call. = FALSE)
  }
  if (into %in% names(x)) {
    stop(sprintf(paste("`%s` already has a column \"%s\"; give another",
                       "name for %s with `into`."),
                 arg, into, what), call. = FALSE)
  }
}
