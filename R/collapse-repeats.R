#' Collapse runs of repeated values
#'
#' Used by [makePatterns()] when `igrpt = TRUE` so that sorting smooths over
#' regions of no change. A missing value ends a run, so the same state before
#' and after a gap (for example, a short spell set to `NA` by `mindur`) counts
#' as two runs. Missing values are then dropped and the result is padded with
#' trailing `NA`s to the length of `x` so that rows of a matrix stay aligned.
#' For example, `c(1, 2, 2, NA, 2, 3)` becomes `c(1, 2, 2, 3, NA, NA)`.
#'
#' @param x An atomic vector (not a factor).
#' @return A vector of the same type and length as `x`: the first value of
#'   each run of observed values, followed by `NA`s.
#' @keywords internal
#' @md
#' @noRd
collapse_repeats <- function(x) {
  if (!is.atomic(x) || is.factor(x)) {
    stop("`x` must be an atomic vector (not a factor).", call. = FALSE)
  }
  # rle() never merges NAs with neighbouring values, so each NA ends a run
  runs <- rle(x)$values
  runs <- runs[!is.na(runs)]
  out <- rep(x[NA_integer_], length(x))  # NAs of the same type as `x`
  out[seq_along(runs)] <- runs
  out
}
