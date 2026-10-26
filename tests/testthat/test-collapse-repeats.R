collapse_repeats <- longCatEDA:::collapse_repeats

test_that("collapse_repeats() keeps the first value of each run, padded with NA", {
  expect_identical(collapse_repeats(c(1, 2, 2, 3, 3, 3)),
                   c(1, 2, 3, NA, NA, NA))
  expect_identical(collapse_repeats(c(1, 2, 1)), c(1, 2, 1))
  expect_identical(collapse_repeats(c(4, 4, 4)), c(4, NA, NA))
})

test_that("collapse_repeats() keeps the value after a missing value", {
  # the old norpt() returned c(1, 3, NA, NA, NA), losing the 2s
  expect_identical(collapse_repeats(c(1, NA, 2, 2, 3)),
                   c(1, 2, 3, NA, NA))
})

test_that("collapse_repeats() handles leading and trailing missing values", {
  # the old norpt() returned c(NA, 2, NA, NA), keeping the NA and losing the 1s
  expect_identical(collapse_repeats(c(NA, 1, 1, 2)), c(1, 2, NA, NA))
  expect_identical(collapse_repeats(c(1, 1, NA, NA)), c(1, NA, NA, NA))
})

test_that("collapse_repeats() treats a missing value as the end of a run", {
  expect_identical(collapse_repeats(c(1, NA, 1, 2)), c(1, 1, 2, NA))
  expect_identical(collapse_repeats(c(1, 1, NA, NA, 1)), c(1, 1, NA, NA, NA))
  expect_identical(collapse_repeats(c(1, 2, 2, NA, 2, 3)),
                   c(1, 2, 2, 3, NA, NA))
})

test_that("collapse_repeats() handles short and empty input", {
  # the old norpt() errored on both
  expect_identical(collapse_repeats(5), 5)
  expect_identical(collapse_repeats(numeric(0)), numeric(0))
  expect_identical(collapse_repeats(c(NA_real_, NA_real_)), c(NA_real_, NA_real_))
})

test_that("collapse_repeats() preserves the type of its input", {
  expect_identical(collapse_repeats(c(1L, 1L, 2L)), c(1L, 2L, NA))
  expect_identical(collapse_repeats(c("a", "a", "b")), c("a", "b", NA))
  expect_identical(collapse_repeats(c(NA_integer_, NA_integer_)),
                   c(NA_integer_, NA_integer_))
})

test_that("collapse_repeats() rejects factors and non-atomic input", {
  expect_error(collapse_repeats(factor(c("a", "a"))), "atomic vector")
  expect_error(collapse_repeats(list(1, 1)), "atomic vector")
})

test_that("makePatterns(igrpt = TRUE) keeps values after missing data", {
  dat <- rbind(c(1, NA, 2, 2),
               c(2, 2, 1, 1))
  # old result for row 1 was "1NANANA"
  expect_identical(as.vector(makePatterns(dat, igrpt = TRUE, num = FALSE)),
                   c("12NANA", "21NANA"))
})
