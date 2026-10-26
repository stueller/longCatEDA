nth_match_col <- longCatEDA:::nth_match_col

# three cases, four intervals
align_y <- rbind(c(1, 3, 2, 3),
                 c(2, 2, 2, 2),
                 c(3, NA, 3, 1))
align_times <- rbind(c(0, 1, 2, 3, 4),
                     c(0, 2, 4, 6, 8),
                     c(10, 11, 12, 13, 14))

# ---- nth_match_col() ------------------------------------------------------

test_that("nth_match_col() finds the column of the nth match per row", {
  expect_identical(nth_match_col(align_y, 3, 1), c(2L, NA, 1L))
  expect_identical(nth_match_col(align_y, 3, 2), c(4L, NA, 3L))
  expect_identical(nth_match_col(align_y, 2, 3), c(NA, 3L, NA))
})

test_that("nth_match_col() never matches missing cells, even when negated", {
  # row 3 is c(3, NA, 3, 1): the first cell that is not 3 is column 4
  expect_identical(nth_match_col(align_y, 3, 1, negate = TRUE), c(1L, 1L, 4L))
  expect_identical(nth_match_col(matrix(NA, 2, 3), 1, 1), c(NA_integer_, NA_integer_))
})

test_that("nth_match_col() works with one column, one row, and characters", {
  expect_identical(nth_match_col(matrix(c(1, 2, 1), ncol = 1), 1, 1), c(1L, NA, 1L))
  expect_identical(nth_match_col(matrix(c(2, 1, 1), nrow = 1), 1, 2), 3L)
  expect_identical(nth_match_col(matrix(c("a", "b", "b", "a"), 2), "b", 1), c(2L, 1L))
})

# ---- alignTime(): states ----------------------------------------------------

test_that("alignTime() puts the start of the nth state at time 0", {
  out <- alignTime(align_y, align_times, which.state = 3, nth.state = 2)
  # row 1: 2nd 3 starts at times[1, 4] = 3; row 3: at times[3, 3] = 12
  expect_identical(out$aligned.times[1, ], c(-3, -2, -1, 0, 1))
  expect_identical(out$aligned.times[3, ], c(-2, -1, 0, 1, 2))
})

test_that("alignTime() aligns cases without the nth state at their last time", {
  out <- alignTime(align_y, align_times, which.state = 3, nth.state = 2)
  expect_identical(out$aligned.times[2, ], c(-8, -6, -4, -2, 0))
})

test_that("alignTime(not.state = TRUE) aligns at the nth other state", {
  out <- alignTime(align_y, align_times, which.state = 2, nth.state = 2,
                   not.state = TRUE)
  # row 1: cells that are not 2 are columns 1, 2, 4; the 2nd starts at 1
  expect_identical(out$aligned.times[1, ], c(-1, 0, 1, 2, 3))
})

test_that("alignTime() returns NULL event times when there are no events", {
  # previously numeric(0)
  out <- alignTime(align_y, align_times, which.state = 1, nth.state = 1)
  expect_null(out$aligned.event.times)
})

test_that("alignTime() accepts vector times and data frames, returning matrices", {
  out_vec <- alignTime(align_y, 0:4, which.state = 3, nth.state = 1)
  expect_true(is.matrix(out_vec$aligned.times))
  expect_identical(out_vec$aligned.times[1, ], c(-1, 0, 1, 2, 3))

  out_df <- alignTime(as.data.frame(align_y), as.data.frame(align_times),
                      which.state = 3, nth.state = 1)
  expect_true(is.matrix(out_df$aligned.times))
  expect_equal(unname(out_df$aligned.times),
               alignTime(align_y, align_times, which.state = 3,
                         nth.state = 1)$aligned.times)
})

# ---- alignTime(): events ----------------------------------------------------

test_that("alignTime() aligns states and events at the nth event", {
  events <- rbind(c(1, 2, 2),
                  c(2, NA, 1),
                  c(1, 1, NA))
  event.times <- rbind(c(0.5, 1.5, 3.5),
                       c(1, NA, 7),
                       c(10.5, 13.5, NA))
  out <- alignTime(align_y, align_times, events = events,
                   event.times = event.times, which.event = 2, nth.event = 2)
  # row 1: 2nd event 2 at 3.5; row 2: only one event 2, so aligned at its
  # last time (8); row 3: no event 2, aligned at its last time (14)
  expect_identical(out$aligned.times[, 1], c(-3.5, -8, -4))
  expect_identical(out$aligned.event.times[1, ], c(-3, -2, 0))
  expect_identical(out$aligned.event.times[2, ], c(-7, NA, -1))
})

test_that("alignTime() aligns at events when they are the last time", {
  events <- matrix(c(1, 1, 1), ncol = 1)
  event.times <- matrix(c(9, 1, 20), ncol = 1)
  out <- alignTime(align_y, align_times, events = events,
                   event.times = event.times, which.event = 2, nth.event = 1)
  # no case has event 2; the latest time across states and events is used
  expect_identical(out$aligned.event.times[, 1], c(0, -7, 0))
})

# ---- alignTime(): argument checks -------------------------------------------

test_that("alignTime() needs exactly one complete alignment target", {
  expect_error(alignTime(align_y, align_times), "exactly one of")
  expect_error(alignTime(align_y, align_times, which.state = 1, nth.state = 1,
                         which.event = 1, nth.event = 1), "exactly one of")
  expect_error(alignTime(align_y, align_times, which.state = 1),
               "`nth.state` must be a single positive integer")
  expect_error(alignTime(align_y, align_times, which.state = 1, nth.state = 0),
               "`nth.state` must be a single positive integer")
  expect_error(alignTime(align_y, align_times, which.state = 1, nth.state = 1,
                         nth.event = 1),
               "`nth.event` cannot be used when aligning at a state")
  expect_error(alignTime(align_y, align_times, which.state = 1:2, nth.state = 1),
               "`which.state` must be a single value")
  expect_error(alignTime(align_y, align_times, which.state = 1, nth.state = 1,
                         not.state = NA),
               "`not.state` must be TRUE or FALSE")
})

test_that("alignTime() checks data shapes and events", {
  expect_error(alignTime(1:4, align_times, which.state = 1, nth.state = 1),
               "`y` must be a matrix or data frame")
  expect_error(alignTime(align_y, align_times[, -1], which.state = 1, nth.state = 1),
               "nrow\\(y\\) rows and ncol\\(y\\) \\+ 1 columns")
  # previously: "dim(X) must have a positive length"
  expect_error(alignTime(align_y, align_times, which.event = 1, nth.event = 1),
               "requires `events` and `event.times`")
  expect_error(alignTime(align_y, align_times, events = matrix(1, 3, 2),
                         event.times = matrix(1, 3, 1), which.event = 1,
                         nth.event = 1),
               "same dimension")
})

test_that("longCatPlot() aligns on the fly", {
  lc <- longCat(align_y, times = align_times)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_silent(longCatPlot(lc, which.state = 3, nth.state = 1))
})
