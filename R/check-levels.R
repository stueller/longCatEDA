# Number of colors that define the built-in color schemes (see colChoose()).
# Schemes are extended for more states, but the colors become hard to tell
# apart.
longcat_max_states <- 9L

#' Find the distinct states in a numeric state matrix
#'
#' Returns the sorted distinct non-missing values in `y` and warns when there
#' are more of them than the built-in color schemes are designed for. The
#' count is taken over all of `y`, not column by column, because every column
#' shares one color scheme.
#'
#' @param y A numeric vector or matrix of states.
#' @param max_states The number of states the built-in color schemes are
#'   designed for.
#' @return A sorted numeric vector of the distinct non-missing values in `y`
#'   (length 0 if `y` is entirely missing).
#' @keywords internal
#' @noRd
check_levels <- function(y, max_states = longcat_max_states) {
  if (!is.numeric(y)) {
    stop("`y` must be numeric; recode states to numbers before checking levels.",
         call. = FALSE)
  }
  states <- as.numeric(sort(unique(as.vector(y))))
  if (length(states) > max_states) {
    warning(sprintf(paste0(
      "`y` has %d distinct states, but the built-in color schemes are ",
      "designed for at most %d,\nso some states may be hard to tell apart. ",
      "Consider supplying `cols` to plot(), combining states, or ",
      "plotting growth curves with plot(longCont(y))."),
      length(states), max_states), call. = FALSE)
  }
  states
}
