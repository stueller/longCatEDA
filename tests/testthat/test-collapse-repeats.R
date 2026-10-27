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

# ---- matrix version ---------------------------------------------------------

collapse_repeats_rows <- longCatEDA:::collapse_repeats_rows

# row-by-row reference built from the vector version
collapse_by_row <- function(m) {
  matrix(apply(m, 1, collapse_repeats), nrow(m), ncol(m), byrow = TRUE)
}

test_that("collapse_repeats_rows() matches collapse_repeats() on every row", {
  set.seed(20261026)
  for (i in 1:300) {
    n <- sample(1:6, 1)
    k <- sample(1:6, 1)
    m <- matrix(sample(c(1:3, NA), n * k, replace = TRUE), n, k)
    expect_identical(collapse_repeats_rows(m), collapse_by_row(m))
  }
})

test_that("collapse_repeats_rows() keeps the shape of one-column input", {
  # t(apply(m, 1, f)) turns an n x 1 matrix into 1 x n
  m <- matrix(c(1, 1, NA, 2), ncol = 1)
  expect_identical(collapse_repeats_rows(m), m)
})

test_that("collapse_repeats_rows() handles one-row, empty, and all-NA input", {
  expect_identical(collapse_repeats_rows(matrix(c(1, 1, 2), 1)),
                   matrix(c(1, 2, NA), 1))
  empty <- matrix(numeric(0), 0, 3)
  expect_identical(collapse_repeats_rows(empty), empty)
  all_na <- matrix(NA_real_, 2, 3)
  expect_identical(collapse_repeats_rows(all_na), all_na)
})

test_that("collapse_repeats_rows() preserves type and dimnames", {
  # rows are c("a", "a") and c("a", "b")
  m <- matrix(c("a", "a", "a", "b"), 2, 2,
              dimnames = list(c("r1", "r2"), c("t1", "t2")))
  out <- collapse_repeats_rows(m)
  expect_identical(dimnames(out), dimnames(m))
  expect_identical(out, matrix(c("a", "a", NA, "b"), 2, 2,
                               dimnames = dimnames(m)))
  expect_type(collapse_repeats_rows(matrix(c(1L, 1L), 1)), "integer")
})

test_that("collapse_repeats_rows() rejects non-matrix input", {
  expect_error(collapse_repeats_rows(1:3), "atomic matrix")
  expect_error(collapse_repeats_rows(data.frame(a = 1)), "atomic matrix")
})
