# S3 class for continuous longitudinal data, plotted as growth curves.

longcont_fields <- c("y", "times", "IndTime", "dim")

#' Create a longCont object
#'
#' Collects continuous longitudinal data and their observation times into an
#' object of class `longCont`, which [plot()] draws as growth curves (one
#' line per case). It is the continuous counterpart of [longCat()]; plotting
#' both side by side compares growth curves with a horizontal line plot.
#'
#' @param y A numeric matrix or data frame with cases in rows and time points
#'   in columns. Missing values break a case's line.
#' @param times Observation times of the columns of `y`: `NULL` (the
#'   default, giving `1:ncol(y)`), a vector of times shared by all cases, or
#'   a matrix with one row per case for individually varying times. Each
#'   case's times must not decrease. A vector or matrix with `ncol(y) + 1`
#'   times is also accepted, so the interval times given to [longCat()] can
#'   be reused: the last time (the end of the last interval) is dropped.
#' @return An S3 object of class `longCont`: a named list with
#'   * `y`: numeric matrix of the data.
#'   * `times`: numeric matrix of observation times, the same dimension as
#'     `y`.
#'   * `IndTime`: `TRUE` if `times` was given as a matrix.
#'   * `dim`: `dim(y)`.
#' @seealso [plot.longCont()] to plot `longCont` objects; [longCont-methods]
#'   for `print()` and `summary()`; [validate_longCont()].
#' @examples
#' lcont <- longCont(example2cont, times = c(1, 100, 200, 300, 400, 500))
#' lcont
#' summary(lcont)
#' plot(lcont, xlab = "Day")
#' @md
#' @export
longCont <- function(y, times = NULL) {
  y <- as_case_matrix(y, "y")
  if (!is.numeric(y)) stop("`y` must be numeric.", call. = FALSE)
  storage.mode(y) <- "double"
  IndTime <- !is.null(dim(times))
  times <- as_observation_times(if (is.null(times)) seq_len(ncol(y)) else times,
                                nrow(y), ncol(y))
  if (!rows_nondecreasing(times)) {
    stop("Each case's `times` must not decrease.", call. = FALSE)
  }
  new_longCont(list(y = unname(y), times = unname(times), IndTime = IndTime,
                    dim = dim(y)))
}

# Observation times as an n_row x n_col numeric matrix. `times` may be a
# vector shared by all rows or a matrix (or data frame) with one row per
# case, with either n_col times or n_col + 1 interval times (the last is
# dropped).
as_observation_times <- function(times, n_row, n_col) {
  if (is.data.frame(times)) times <- as.matrix(times)
  if (!is.numeric(times)) stop("`times` must be numeric.", call. = FALSE)
  if (is.null(dim(times))) {
    if (!length(times) %in% c(n_col, n_col + 1L)) {
      stop("`times` must have length ncol(y) or ncol(y) + 1.", call. = FALSE)
    }
    times <- matrix(times[seq_len(n_col)], n_row, n_col, byrow = TRUE)
  } else {
    if (nrow(times) != n_row || !ncol(times) %in% c(n_col, n_col + 1L)) {
      stop("`times` must have nrow(y) rows and ncol(y) or ncol(y) + 1 columns.",
           call. = FALSE)
    }
    times <- times[, seq_len(n_col), drop = FALSE]
  }
  storage.mode(times) <- "double"
  times
}

new_longCont <- function(x) {
  validate_longCont(structure(x[longcont_fields], class = "longCont"))
}

#' Validate a longCont object
#'
#' Checks that an object has the structure produced by [longCont()]. All
#' problems are collected and reported in a single error. Call it after
#' editing an object by hand.
#'
#' @param x Object to validate.
#' @param arg Name used for `x` in error messages.
#' @return `x`, invisibly, if it is valid. Otherwise an error listing every
#'   problem found.
#' @seealso [longCont()]
#' @examples
#' lcont <- longCont(example2cont)
#' validate_longCont(lcont)
#'
#' # hand edits are checked
#' lcont$times <- lcont$times[, -1]
#' try(validate_longCont(lcont))
#' @md
#' @export
validate_longCont <- function(x, arg = "x") {
  if (!inherits(x, "longCont")) {
    stop(sprintf("`%s` must be a longCont object created by longCont().", arg),
         call. = FALSE)
  }
  if (!setequal(names(x), longcont_fields)) {
    longcat_abort(arg, paste0("fields must be exactly: ",
                              paste(longcont_fields, collapse = ", ")),
                  "longCont")
  }
  problems <- c(
    if (!is.matrix(x$y) || !is.double(x$y)) "`y` must be a numeric matrix.",
    if (!is.matrix(x$times) || !is.double(x$times)) {
      "`times` must be a numeric matrix."
    } else if (!identical(dim(x$times), dim(x$y))) {
      "`times` must have the same dimension as `y`."
    } else if (!rows_nondecreasing(x$times)) {
      "each row of `times` must not decrease."
    },
    if (!is_flag(x$IndTime)) "`IndTime` must be TRUE or FALSE.",
    if (!identical(x$dim, dim(x$y))) "`dim` must equal dim(y)."
  )
  if (length(problems) > 0) longcat_abort(arg, problems, "longCont")
  invisible(x)
}

# ---- plot -----------------------------------------------------------------

#' Plot growth curves of a longCont object
#'
#' Draws each case of a [longCont()] object as a line through its observed
#' values. With `jog`, each case is shifted up or down by a random amount,
#' which separates the lines of cases with equal values (Tueller, Van Dorn,
#' & Bobashev, 2016), e.g., when the data are categorical.
#'
#' @param x An object of class `longCont` created by [longCont()].
#' @param jog `FALSE` (the default), `TRUE`, or a positive number. If not
#'   `FALSE`, each case is shifted by a uniform random amount between `-jog`
#'   and `jog` (0.25 for `TRUE`). Use [set.seed()] for reproducible plots.
#' @param col,lty,lwd Color, type, and width of the lines, recycled across
#'   cases (see [par()]). Defaults are black, solid, and 1.
#' @param xlim,ylim Ranges of the axes. By default they cover the times and
#'   the (jogged) values.
#' @param xlab,ylab Axis labels. Defaults are `"Time"` and none.
#' @param ... Further graphical parameters passed to [plot()], such as
#'   `main`.
#' @return `x`, invisibly.
#' @references Tueller, S. J., Van Dorn, R. A., & Bobashev, G. V. (2016).
#'   Visualization of categorical longitudinal and times series data (Report
#'   No. MR-0033-1602). Research Triangle Park, NC: RTI Press.
#'   \doi{10.3768/rtipress.2016.mr.0033.1602}
#' @seealso [longCont()]; [plot.longCat()] for horizontal line plots of
#'   categorical data.
#' @examples
#' times <- c(1, 100, 200, 300, 400, 500)
#' op <- par(bg = "cornsilk3")
#' plot(longCont(example2cont, times), ylim = c(-2, 6), xlab = "Day")
#' par(op)
#'
#' # jogging separates the lines of categorical data
#' lcont <- longCont(example2cat, times)
#' op <- par(mfrow = c(1, 2), bg = "cornsilk3")
#' plot(lcont, main = "Growth Curves", xlab = "Days")
#' set.seed(1)
#' plot(lcont, jog = TRUE, main = "Growth Curves + Jogging", xlab = "Days")
#' par(op)
#'
#' # compare growth curves to the horizontal line plot
#' op <- par(mfrow = c(1, 2), bg = "cornsilk3")
#' plot(lcont, main = "Growth Curves", xlab = "Days")
#' plot(longCat(example2cat, c(times, 600)), lwd = 4, legendBuffer = .2,
#'      main = "Horizontal Line Plot")
#' par(op)
#' @md
#' @export
plot.longCont <- function(x, jog = FALSE, col = 1, lty = 1, lwd = 1,
                          xlim = NULL, ylim = NULL, xlab = "Time", ylab = "",
                          ...) {
  validate_longCont(x, "x")
  y <- jog_cases(x$y, jog_amount(jog))
  if (all(is.na(y))) stop("`x` has no non-missing values to plot.", call. = FALSE)
  if (is.null(xlim)) xlim <- range(x$times, na.rm = TRUE)
  if (is.null(ylim)) ylim <- range(y, na.rm = TRUE)
  plot(NA, type = "n", xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, ...)
  matlines(t(x$times), t(y), col = col, lty = lty, lwd = lwd)
  invisible(x)
}

# The half-width of the jog: 0 for FALSE, 0.25 for TRUE, or a number.
jog_amount <- function(jog) {
  if (is_flag(jog)) return(if (jog) 0.25 else 0)
  if (!is.numeric(jog) || length(jog) != 1L || is.na(jog) || jog <= 0) {
    stop("`jog` must be TRUE, FALSE, or a single positive number.",
         call. = FALSE)
  }
  jog
}

# Shift each row of `y` by its own uniform random amount in [-amount, amount].
jog_cases <- function(y, amount) {
  if (amount == 0) return(y)
  y + stats::runif(nrow(y), -amount, amount)
}

# ---- methods --------------------------------------------------------------

#' Print and summarize longCont objects
#'
#' `print()` gives a compact description of a [longCont()] object.
#' `summary()` returns the case and time-point counts and, for each time
#' point, the number of observed values and their minimum, mean, and
#' maximum.
#'
#' @param x,object An object of class `longCont` (or `summary.longCont` for
#'   the print method of the summary).
#' @param ... Unused; included for consistency with the generics.
#' @return `print()` methods return `x` invisibly. `summary()` returns an
#'   object of class `summary.longCont`, a list with elements `dim`,
#'   `IndTime`, and `columns` (a matrix with one row per time point and
#'   columns `n`, `min`, `mean`, and `max`).
#' @examples
#' lcont <- longCont(example2cont)
#' lcont
#' summary(lcont)
#' @md
#' @name longCont-methods
NULL

#' @rdname longCont-methods
#' @md
#' @export
print.longCont <- function(x, ...) {
  cat(sprintf("<longCont> %s cases x %s time points\n",
              format_count(x$dim[1]), format_count(x$dim[2])))
  cat("Times: ", if (x$IndTime) "individually varying" else "common to all cases",
      "\n", sep = "")
  if (all(is.na(x$y))) {
    cat("Values: all missing\n")
  } else {
    cat(sprintf("Values: %s to %s, %s missing\n",
                format(min(x$y, na.rm = TRUE)), format(max(x$y, na.rm = TRUE)),
                format_count(sum(is.na(x$y)))))
  }
  invisible(x)
}

#' @rdname longCont-methods
#' @md
#' @export
summary.longCont <- function(object, ...) {
  observed <- !is.na(object$y)
  n <- colSums(observed)
  column_stat <- function(f) {
    vapply(seq_len(ncol(object$y)), function(j) {
      if (n[j] == 0) NA_real_ else f(object$y[observed[, j], j])
    }, numeric(1))
  }
  columns <- cbind(n = n, min = column_stat(min), mean = column_stat(mean),
                   max = column_stat(max))
  rownames(columns) <- paste0("T", seq_len(ncol(object$y)))
  structure(list(dim = object$dim, IndTime = object$IndTime, columns = columns),
            class = "summary.longCont")
}

#' @rdname longCont-methods
#' @md
#' @export
print.summary.longCont <- function(x, ...) {
  cat(sprintf("longCont object: %s cases x %s time points\n",
              format_count(x$dim[1]), format_count(x$dim[2])))
  cat("Times:", if (x$IndTime) "individually varying" else "common to all cases",
      "\n")
  cat("\nValues by time point:\n")
  print(x$columns)
  invisible(x)
}
