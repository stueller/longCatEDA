#' Concatenate the rows of a matrix into sorting patterns
#'
#' Concatenates the values in each row of `dat` into a single string, which
#' can optionally be converted to a number. For example, a row containing
#' `c(1, 2, 3, 5)` becomes `"1235"`. [sorter()] stores these patterns in
#' `order.y`; it sorts cases by comparing the same values column by column,
#' which gives the order of the patterns without their limits (values of 10
#' or more, and many time points; see [sorter()]).
#'
#' @param dat A matrix or data frame such as `lc$y` from a [longCat()]
#'   object, with cases in rows.
#' @param times `NULL`, or the start and stop times of the intervals in
#'   `dat`, as for `times` in [longCat()]: a vector of length `ncol(dat) + 1`
#'   or a matrix (or data frame) with `nrow(dat)` rows and `ncol(dat) + 1`
#'   columns. Only used with `mindur`.
#' @param num Logical. If `TRUE` (the default), each pattern is converted to a
#'   number with missing values removed and the decimal point placed after the
#'   first digit, so `c(1, 2, 3, NA, 5)` becomes `1.235` and
#'   `c(5, 4, 3, 3, 2)` becomes `5.4332`. This keeps rows with missing data
#'   from sorting apart from complete rows only because their patterns are
#'   shorter. If `FALSE`, the patterns are returned as strings, with missing
#'   values written as `"NA"`.
#' @param mindur Minimum duration, or `NULL` (the default). If given, each
#'   cell of `dat` whose interval is shorter than `mindur`
#'   (`times[, j + 1] - times[, j] < mindur`) is set to `NA` before the
#'   patterns are built, so that short-lived states do not drive the sort
#'   order. Requires `times`.
#' @param igrpt Logical. If `TRUE`, ignore repeated values so that sorting
#'   smooths over regions of no change. Each run of a repeated value is
#'   reduced to one value. A missing value (including one set by `mindur`)
#'   ends a run and is then dropped, so a row `c(1, 2, 2, NA, 2, 3)` becomes
#'   `c(1, 2, 2, 3, NA, NA)`. Default is `FALSE`.
#'
#' @return A vector with one pattern per row of `dat`: numeric if `num` is
#'   `TRUE`, otherwise character. A row with no observed values gives `NA`
#'   when `num` is `TRUE`.
#'
#' @references Tueller, S. J., Van Dorn, R. A., & Bobashev, G. V. (2016).
#'   Visualization of categorical longitudinal and times series data (Report
#'   No. MR-0033-1602). Research Triangle Park, NC: RTI Press.
#'   \doi{10.3768/rtipress.2016.mr.0033.1602}
#'
#' @seealso [sorter()], which sorts cases by these patterns.
#'
#' @examples
#' # concatenate the rows of a matrix
#' set.seed(1)
#' temp <- matrix(sample(1:9, 40, replace = TRUE), 10, 4)
#' temp
#' makePatterns(temp, num = FALSE)
#'
#' # numeric patterns, as in Tueller, Van Dorn, & Bobashev (2016)
#' makePatterns(rbind(c(1, 2, 3, NA, 5), c(5, 4, 3, 3, 2)))
#'
#' # examine the unique patterns of data
#' bindat <- matrix(sample(0:1, 500, replace = TRUE), 100, 5)
#' uniquePatterns <- makePatterns(bindat, num = FALSE)
#' as.matrix(table(uniquePatterns))
#'
#' @author Stephen J. Tueller
#' @md
#' @export
makePatterns <- function(dat, times = NULL, num = TRUE, mindur = NULL,
                         igrpt = FALSE) {
  dat <- as_pattern_matrix(dat)
  if (!is_flag(num)) stop("`num` must be TRUE or FALSE.", call. = FALSE)
  if (!is_flag(igrpt)) stop("`igrpt` must be TRUE or FALSE.", call. = FALSE)

  if (!is.null(mindur)) dat <- mask_short_durations(dat, times, mindur)
  if (igrpt) dat <- collapse_repeats_rows(dat)

  patterns <- pattern_strings(dat)
  if (num) patterns <- pattern_numbers(patterns)
  patterns
}

# ---- helpers ----------------------------------------------------------------

as_pattern_matrix <- function(dat) {
  if (!is.matrix(dat) && !is.data.frame(dat)) {
    stop("`dat` must be a matrix or data frame with cases in rows.",
         call. = FALSE)
  }
  if (ncol(dat) == 0L) stop("`dat` must have at least one column.", call. = FALSE)
  as.matrix(dat)
}

# Set to NA each cell whose interval is shorter than `mindur` (Tueller, Van
# Dorn, & Bobashev, 2016, p. 7). Cells with an unknown duration are kept.
mask_short_durations <- function(dat, times, mindur) {
  if (!is.numeric(mindur) || length(mindur) != 1L || is.na(mindur) ||
      mindur < 0) {
    stop("`mindur` must be a single non-negative number.", call. = FALSE)
  }
  if (is.null(times)) {
    stop("`mindur` requires `times`.", call. = FALSE)
  }
  durations <- interval_durations(times, nrow(dat), ncol(dat))
  dat[!is.na(durations) & durations < mindur] <- NA
  dat
}

# Durations of the `n_col` intervals bounded by `times`, as an n_row x n_col
# matrix. `times` is a vector of n_col + 1 boundaries shared by all rows, or a
# matrix with n_row rows and n_col + 1 columns.
interval_durations <- function(times, n_row, n_col) {
  times <- as_times_matrix(times, n_row, n_col, "dat")
  times[, -1L, drop = FALSE] - times[, -(n_col + 1L), drop = FALSE]
}

# Paste each row's values together; missing values are written as "NA".
pattern_strings <- function(dat) {
  columns <- lapply(seq_len(ncol(dat)), function(j) dat[, j])
  do.call(paste0, unname(columns))
}

# Drop the "NA"s and read the digits as a number with one digit before the
# decimal point.
pattern_numbers <- function(patterns) {
  digits <- gsub("NA", "", patterns, fixed = TRUE)
  (as.numeric(digits) / 10^nchar(digits)) * 10
}
