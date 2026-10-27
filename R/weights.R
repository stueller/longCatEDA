# Duplicating rows of weighted data.

#' Duplicate rows of weighted data
#'
#' Repeats each row of `data` a number of times proportional to its weight,
#' so that tools without support for weights can treat the result as
#' unweighted data. With `rescale = TRUE`, the weights are first divided by
#' the smallest positive weight, so that the lightest case appears once and
#' the proportions between weights are kept; the counts are then rounded.
#'
#' Rounding distorts small weights (after rescaling, weights of 1.4 and 0.6
#' both give 1 row), and large weights can give very large data. For
#' plotting, giving the weights to [longCat()] is exact and adds no rows;
#' use `expand_weights()` when the duplicated data are needed elsewhere.
#'
#' Rows are selected with `[`, so the columns of a data frame keep their
#' classes and attributes (e.g., factor levels).
#'
#' @param data A data frame or matrix with one row per case.
#' @param weights The weights: a numeric vector with one finite,
#'   non-negative value per row of `data`, or the name of such a column of
#'   `data`.
#' @param rescale Logical. Divide the weights by the smallest positive
#'   weight before rounding? Default is `TRUE`.
#' @param max_rows The largest number of rows to create, as a safeguard
#'   against running out of memory. Default is 10 million.
#' @return `data` with each row repeated `round(weight)` times (after
#'   rescaling), in the original order, without row names. Rows whose count
#'   rounds to 0 are left out, with a warning.
#' @references Tueller, S. J., Van Dorn, R. A., & Bobashev, G. V. (2016).
#'   Visualization of categorical longitudinal and times series data (Report
#'   No. MR-0033-1602). Research Triangle Park, NC: RTI Press.
#'   \doi{10.3768/rtipress.2016.mr.0033.1602}
#' @seealso [longCat()], whose `weights` argument plots weighted data
#'   without duplicating rows.
#' @examples
#' d <- data.frame(t1 = factor(c("low", "high", "low"), c("low", "high")),
#'                 t2 = factor(c("high", "high", "low"), c("low", "high")),
#'                 w = c(150, 300, 75))
#' expand_weights(d, "w")
#' str(expand_weights(d, "w"))  # the factors are kept
#'
#' # the same plot from duplicated rows and from weights
#' lc_dup <- longCat(expand_weights(d, "w")[, 1:2])
#' lc_w <- longCat(d[, 1:2], weights = d$w)
#' op <- par(mfrow = c(1, 2))
#' plot(lc_dup, main = "Duplicated rows")
#' plot(lc_w, main = "Weights")
#' par(op)
#' @md
#' @export
expand_weights <- function(data, weights, rescale = TRUE, max_rows = 1e7) {
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("`data` must be a data frame or matrix.", call. = FALSE)
  }
  if (is.character(weights) && length(weights) == 1L) {
    if (!is.data.frame(data) || !weights %in% names(data)) {
      stop(sprintf("`data` has no column \"%s\".", weights), call. = FALSE)
    }
    weights <- data[[weights]]
  }
  weights <- check_weights(weights, nrow(data), "data")
  if (!is_flag(rescale)) stop("`rescale` must be TRUE or FALSE.", call. = FALSE)
  if (!is.numeric(max_rows) || length(max_rows) != 1L || is.na(max_rows) ||
      max_rows < 1) {
    stop("`max_rows` must be a single positive number.", call. = FALSE)
  }

  if (rescale) weights <- weights / min(weights[weights > 0])
  counts <- round(weights)
  dropped <- sum(counts == 0 & weights > 0)
  if (dropped > 0) {
    warning(sprintf(paste("%d row(s) with weights that round to 0 are left",
                          "out; consider `rescale = TRUE`."), dropped),
            call. = FALSE)
  }
  if (sum(counts) > max_rows) {
    stop(sprintf(paste(
      "Duplicating would create %s rows, more than `max_rows` (%s). Give the",
      "weights to longCat(weights = ) instead, or raise `max_rows`."),
      format_count(sum(counts)), format_count(max_rows)), call. = FALSE)
  }
  out <- data[rep(seq_len(nrow(data)), counts), , drop = FALSE]
  rownames(out) <- NULL
  out
}
