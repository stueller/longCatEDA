# Small fixtures shared across test files.

small_y <- function() {
  matrix(c(1, 2, 3,
           2, 2, 1,
           3, 1, NA,
           1, 3, 2),
         nrow = 4, byrow = TRUE)
}

small_lc <- function(...) longCat(small_y(), ...)

small_events <- function() {
  list(events = matrix(c(1, 2, 2, NA, 1, 1, 2, NA), nrow = 4, byrow = TRUE),
       event.times = matrix(c(0.5, 1.5, 1.2, NA, 0.1, 2.2, 2.5, NA),
                            nrow = 4, byrow = TRUE))
}

# Expect `validate_longCat()` to fail with a message matching `pattern`.
expect_invalid <- function(x, pattern) {
  expect_error(validate_longCat(x), pattern)
}
