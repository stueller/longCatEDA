# Number of colors in the built-in color schemes (see colChoose()). States
# beyond this number are not drawn unless the user supplies `cols`.
longcat_max_states <- 9L

#' Find the distinct states in a numeric state matrix
#'
#' Returns the sorted distinct non-missing values in `y` and warns when there
#' are more of them than the built-in color schemes can show. The count is
#' taken over all of `y`, not column by column, because every column shares
#' one color scheme.
#'
#' @param y A numeric vector or matrix of states.
#' @param max_states The largest number of states that can be drawn with the
#'   built-in color schemes.
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
      "`y` has %d distinct states, but the built-in color schemes have only ",
      "%d colors.\nStates above %d will not be drawn unless you supply `cols` ",
      "to longCatPlot(); also consider continuous methods such as ",
      "longContPlot()."),
      length(states), max_states, max_states), call. = FALSE)
  }
  states
}
