# Sorting cases by their state sequences.
#
# sorter() used to sort on the patterns from makePatterns(): concatenated
# strings (num = FALSE) or those strings read as numbers (num = TRUE). Both
# have limits: "1" followed by "12" and "11" followed by "2" give the same
# string, and a number holds only about 15 significant digits, so rows that
# differ only after the 15th time point tied. Comparing the values column by
# column gives the order those patterns were meant to give, with no limits.

#' Build column-wise sort keys for state sequences
#'
#' The keys reproduce the order of the [makePatterns()] patterns:
#'
#' * `num = TRUE`: observed values are moved to the left of each row and a
#'   missing value sorts below every state, so a row that is a prefix of
#'   another sorts first (as `1.23` sorts before `1.235`). Rows with no
#'   observed values are flagged so that they sort last (their numeric
#'   pattern was `NA`).
#' * `num = FALSE`: values are compared in place and a missing value sorts
#'   above every state (as the text `"NA"` sorted after digits).
#'
#' @param values A numeric matrix of states (after `mindur` and `igrpt` have
#'   been applied).
#' @param num Logical; see [makePatterns()].
#' @return A numeric matrix with `nrow(values)` rows: column 1 is 1 for rows
#'   with no observed values (always 0 when `num = FALSE`), and the remaining
#'   columns are the values to compare, with no `NA`s.
#' @keywords internal
#' @md
#' @noRd
sequence_sort_keys <- function(values, num) {
  if (!is.matrix(values) || !is.numeric(values)) {
    stop("`values` must be a numeric matrix.", call. = FALSE)
  }
  if (num) {
    keys <- left_pack_rows(values, !is.na(values))
    all_missing <- is.na(keys[, 1])
    keys[is.na(keys)] <- -Inf
  } else {
    keys <- values
    all_missing <- rep(FALSE, nrow(values))
    keys[is.na(keys)] <- Inf
  }
  storage.mode(keys) <- "double"
  cbind(all_missing = as.numeric(all_missing), unname(keys))
}

#' Order cases by initial state, custom sort, and state sequence
#'
#' @param init Values sorted on first (constant unless `initFirst = TRUE`).
#' @param custom The user's `customSort` values, sorted on second.
#' @param keys A key matrix from `sequence_sort_keys()`.
#' @param ascending Logical; sort direction for everything except the
#'   all-missing flag, which always puts rows with no observed values last.
#' @return An integer permutation, as from [order()]. Ties keep their
#'   original order.
#' @keywords internal
#' @md
#' @noRd
order_cases <- function(init, custom, keys, ascending) {
  descending <- !ascending
  key_columns <- lapply(seq_len(ncol(keys)), function(j) keys[, j])
  args <- c(list(init, custom), key_columns)
  decreasing <- c(descending, descending, FALSE,
                  rep(descending, ncol(keys) - 1L))
  do.call(order, c(unname(args), list(decreasing = decreasing,
                                      method = "radix", na.last = TRUE)))
}
