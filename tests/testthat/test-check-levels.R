check_levels <- longCatEDA:::check_levels

test_that("check_levels() returns sorted distinct non-missing states", {
  expect_identical(check_levels(c(3, 1, NA, 2, 3, 1)), c(1, 2, 3))
  expect_identical(check_levels(matrix(c(2L, 5L, NA, 2L), 2)), c(2, 5))
})

test_that("check_levels() counts distinct values, not unique rows", {
  # unique() on a matrix returns unique rows; the old lunique() counted the
  # non-missing cells of those rows and reported 4 here
  expect_length(check_levels(matrix(c(1, 2, 1, 2), 2)), 2)
})

test_that("check_levels() handles input with no observed states", {
  expect_identical(check_levels(matrix(NA_real_, 2, 2)), numeric(0))
  expect_identical(check_levels(numeric(0)), numeric(0))
})

test_that("check_levels() warns only above the color-scheme limit", {
  expect_silent(check_levels(1:9))
  expect_warning(check_levels(1:10), "10 distinct states")
  expect_warning(check_levels(1:10), "designed for at most 9")
  expect_silent(check_levels(1:10, max_states = 10))
})

test_that("check_levels() counts states across all columns together", {
  # 5 states per column but 10 overall; the old check was per column and
  # did not warn
  expect_warning(check_levels(cbind(1:5, 6:10)), "10 distinct states")
})

test_that("check_levels() rejects non-numeric input", {
  expect_error(check_levels(c("1", "2")), "`y` must be numeric")
  expect_error(check_levels(factor(1:2)), "`y` must be numeric")
})

test_that("longCat() warns once for more than 9 states and records them", {
  y <- matrix(1:12, 3, 4)
  expect_warning(lc <- longCat(y), "12 distinct states")
  expect_identical(lc$factors, as.numeric(1:12))
  expect_identical(lc$nfactors, 12L)
})
