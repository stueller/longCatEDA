#' Check Levels in a Data Frame or Matrix
#'
#' This function checks each variable in a data frame or matrix `y` for the number of unique levels.
#' If any variable contains 10 or more unique levels, a warning is issued suggesting consideration
#' of continuous methods. It returns the unique values across all variables as a numeric vector.
#'
#' @param y A data frame or matrix to check for levels of variables.
#'
#' @return Numeric vector of all unique values across all variables in `y`.
#' @examples
#' y <- data.frame(a = 1:5, b = factor(c("yes", "no", "yes", "maybe", "no")))
#' levelCheck(y)
#'
#' @export
levelCheck <- function(y)
{
  lu <- apply(y, 2, lunique)
  maxlu <- max(lu)
  if( maxlu > 9 )
  {
    warning('One or more variables in y has 10 or more categories.",
            "\nConsider using continuous methods\nsuch as longContPlot().')
  }
  # determine unique values
  factors <- as.numeric(levels(factor(unlist(y))))
  return( factors )
}


#' Count Unique Non-NA Values
#'
#' This function calculates the number of unique non-NA values in a vector, matrix, or data frame.
#'
#' @param y Input vector, matrix, or data frame.
#'
#' @return Integer representing the number of unique non-NA values.
#'
#' @examples
#' vec <- c(1, 2, 3, NA, 3, 4, NA)
#' lunique(vec) # Returns 4
#' @export
lunique <- function(y)
{
  u <- unique(y)
  sum(!is.na(u))
}
