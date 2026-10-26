sequence_sort_keys <- longCatEDA:::sequence_sort_keys
order_cases <- longCatEDA:::order_cases

# order rows of `y` the way sorter() does with no initFirst or customSort
order_rows <- function(y, num, ascending = TRUE) {
  ones <- rep(1, nrow(y))
  order_cases(ones, ones, sequence_sort_keys(y, num), ascending)
}

test_that("ordering matches the numeric patterns for single-digit states", {
  set.seed(11)
  for (i in 1:200) {
    y <- matrix(sample(c(1:5, NA), 5 * 4, replace = TRUE), 5, 4)
    for (asc in c(TRUE, FALSE)) {
      expect_identical(order_rows(y, num = TRUE, ascending = asc),
                       order(makePatterns(y), decreasing = !asc))
    }
  }
})

test_that("ordering matches the string patterns for single-digit states", {
  set.seed(12)
  for (i in 1:200) {
    y <- matrix(sample(c(1:5, NA), 5 * 4, replace = TRUE), 5, 4)
    for (asc in c(TRUE, FALSE)) {
      expect_identical(order_rows(y, num = FALSE, ascending = asc),
                       order(makePatterns(y, num = FALSE), decreasing = !asc,
                             method = "radix"))
    }
  }
})

test_that("states of 10 or more are compared as whole numbers", {
  # both rows give the pattern "112"; by value, (1, 12) comes first
  y <- rbind(c(11, 2),
             c(1, 12))
  expect_identical(makePatterns(y, num = FALSE), c("112", "112"))
  expect_identical(order_rows(y, num = FALSE), c(2L, 1L))
  expect_identical(order_rows(y, num = TRUE), c(2L, 1L))
})

test_that("differences after the 15th time point still affect the order", {
  # numeric patterns of 20 digits cannot tell these rows apart
  y <- rbind(c(rep(1, 19), 2),
             c(rep(1, 19), 1))
  keys <- makePatterns(y)
  expect_identical(keys[1], keys[2])
  expect_identical(order_rows(y, num = TRUE), c(2L, 1L))
  expect_identical(order_rows(y, num = TRUE, ascending = FALSE), c(1L, 2L))
})

test_that("with num = TRUE, missing values sort low and empty rows sort last", {
  y <- rbind(c(NA, NA, NA),
             c(1, 2, 3),
             c(1, NA, 2),
             c(NA, 1, 2))
  # observed sequences: (), (1, 2, 3), (1, 2), (1, 2)
  expect_identical(order_rows(y, num = TRUE), c(3L, 4L, 2L, 1L))
  expect_identical(order_rows(y, num = TRUE, ascending = FALSE),
                   c(2L, 3L, 4L, 1L))
})

test_that("with num = FALSE, missing values sort high in place", {
  y <- rbind(c(1, NA),
             c(1, 2),
             c(NA, 1))
  expect_identical(order_rows(y, num = FALSE), c(2L, 1L, 3L))
  expect_identical(order_rows(y, num = FALSE, ascending = FALSE),
                   c(3L, 1L, 2L))
})

test_that("order_cases() sorts on init, then custom, then the sequence", {
  y <- rbind(c(1, 1), c(2, 2), c(1, 2), c(2, 1))
  keys <- sequence_sort_keys(y, num = TRUE)
  init <- y[, 1]
  custom <- c(2, 1, 1, 1)
  # init 1: rows 1 (custom 2), 3 (custom 1) -> 3, 1; init 2: rows 2, 4 by sequence
  expect_identical(order_cases(init, custom, keys, TRUE), c(3L, 1L, 4L, 2L))
})

test_that("ties keep their original order", {
  y <- rbind(c(1, 2), c(1, 2), c(1, 2))
  expect_identical(order_rows(y, num = TRUE), 1:3)
  expect_identical(order_rows(y, num = TRUE, ascending = FALSE), 1:3)
})

test_that("sequence_sort_keys() rejects non-numeric input", {
  expect_error(sequence_sort_keys(matrix("a"), TRUE), "numeric matrix")
})

# ---- through sorter() -----------------------------------------------------

test_that("sorter() orders by whole state values and all time points", {
  y <- rbind(c(rep(1, 19), 2),
             c(rep(1, 19), 1))
  s <- sorter(longCat(y))
  expect_identical(s$order.y.sorted[, 1], c(2L, 1L))
})

test_that("sorter(mindur) measures each cell's own interval", {
  y <- rbind(c(3, 1, 2),
             c(1, 2, 3))
  # row 1 spends only 0.1 in state 3, so with mindur = 0.5 it sorts as
  # (1, 2), ahead of (1, 2, 3)
  times <- rbind(c(0, 0.1, 1, 2),
                 c(0, 1, 2, 3))
  lc <- longCat(y, times = times)
  expect_identical(sorter(lc)$order.y.sorted[, 1], c(2L, 1L))
  expect_identical(sorter(lc, mindur = 0.5)$order.y.sorted[, 1], c(1L, 2L))
})

test_that("sorter(mindur) uses the full times when sorting on some columns", {
  y <- rbind(c(1, 2, 3),
             c(1, 3, 2))
  times <- rbind(c(0, 1, 2, 2.1),
                 c(0, 1, 2, 3))
  lc <- longCat(y, times = times)
  # column 3 of row 1 lasts 0.1; measuring it needs times[, 4], which is not
  # among the columns selected by whichColumns
  s <- sorter(lc, whichColumns = 2:3, mindur = 0.5)
  expect_identical(s$order.y[, 2], makePatterns(rbind(c(2, NA), c(3, 2))))
})
