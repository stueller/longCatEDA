#' Collapse runs of repeated values
#'
#' Reference implementation for one vector. [makePatterns()] uses the
#' vectorized `collapse_repeats_rows()`, which applies the same rule to every
#' row of a matrix and is tested against this function.
#'
#' Collapsing smooths over regions of no change when sorting
#' (`igrpt = TRUE`). A missing value ends a run, so the same state before
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

#' Collapse runs of repeated values in every row of a matrix
#'
#' Applies the rule of `collapse_repeats()` to each row at once: a cell starts
#' a run if it is observed and differs from the cell to its left (or that cell
#' is missing); run starts are then moved to the left of their row in order
#' and the rest of the row is set to `NA`.
#'
#' Unlike `t(apply(m, 1, collapse_repeats))`, the result always has the same
#' dimension as `m` (including one-column and zero-row input), and no R
#' function is called per row.
#'
#' @param m An atomic matrix.
#' @return A matrix with the same dimension, type, and dimnames as `m`.
#' @keywords internal
#' @md
#' @noRd
collapse_repeats_rows <- function(m) {
  if (!is.matrix(m) || !is.atomic(m)) {
    stop("`m` must be an atomic matrix.", call. = FALSE)
  }
  if (nrow(m) == 0L || ncol(m) == 0L) return(m)
  left_pack_rows(m, run_starts(m))
}

# Logical matrix marking the cells that start a run: observed cells that
# differ from the cell to their left, or whose left cell is missing.
run_starts <- function(m) {
  n_col <- ncol(m)
  observed <- !is.na(m)
  starts <- observed
  if (n_col > 1L) {
    left <- m[, -n_col, drop = FALSE]
    right <- m[, -1L, drop = FALSE]
    continues <- !is.na(left) & observed[, -1L, drop = FALSE] & left == right
    starts[, -1L] <- observed[, -1L, drop = FALSE] & !continues
  }
  starts
}

#' Move selected cells to the left of their row
#'
#' For each row of `m`, the cells where `keep` is `TRUE` are moved, in column
#' order, to the start of the row; the remaining cells become `NA`.
#'
#' @param m An atomic matrix.
#' @param keep A logical matrix with the same dimension as `m`.
#' @return A matrix with the same dimension, type, and dimnames as `m`.
#' @keywords internal
#' @md
#' @noRd
left_pack_rows <- function(m, keep) {
  rows <- row(m)[keep]
  cols <- col(m)[keep]
  values <- m[keep]
  ord <- order(rows, cols)
  rows <- rows[ord]
  position <- sequence(tabulate(rows, nbins = nrow(m)))

  out <- m
  out[] <- NA
  out[cbind(rows, position)] <- values[ord]
  out
}
