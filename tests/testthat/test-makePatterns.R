test_that("makePatterns() concatenates each row into a string", {
  dat <- rbind(c(1, 2, 3, 5),
               c(2, NA, 1, 1))
  expect_identical(makePatterns(dat, num = FALSE), c("1235", "2NA11"))
})

test_that("makePatterns() rescales numeric patterns as in the paper", {
  # Tueller, Van Dorn, & Bobashev (2016), p. 7
  dat <- rbind(c(1, 2, 3, NA, 5),
               c(5, 4, 3, 3, 2))
  expect_equal(makePatterns(dat), c(1.235, 5.4332))
})

test_that("makePatterns() returns a plain vector with one pattern per row", {
  out <- makePatterns(matrix(1:6, 3))
  expect_null(dim(out))
  expect_length(out, 3)
  expect_type(out, "double")
  expect_type(makePatterns(matrix(1:6, 3), num = FALSE), "character")
})

test_that("makePatterns() accepts data frames and matrices alike", {
  m <- matrix(c(1, 2, NA, 3, 3, 1), 3)
  expect_identical(makePatterns(as.data.frame(m)), makePatterns(m))
})

test_that("makePatterns() gives NA for a row with no observed values", {
  expect_equal(makePatterns(rbind(c(NA, NA), c(1, 2))), c(NA, 1.2))
})

test_that("makePatterns() handles one-column and one-row input", {
  one_col <- matrix(c(1, 1, 2), ncol = 1)
  # with igrpt = TRUE the old code returned a single pattern "112"
  expect_identical(makePatterns(one_col, num = FALSE, igrpt = TRUE),
                   c("1", "1", "2"))
  expect_identical(makePatterns(one_col, num = FALSE), c("1", "1", "2"))
  expect_identical(makePatterns(matrix(c(1, 1, 2), 1), num = FALSE,
                                igrpt = TRUE),
                   "12NA")
})

test_that("makePatterns(igrpt = TRUE) collapses runs in every row", {
  dat <- rbind(c(1, 1, 2, 2),
               c(3, NA, 3, 3))
  expect_identical(makePatterns(dat, num = FALSE, igrpt = TRUE),
                   c("12NANA", "33NANA"))
})

test_that("makePatterns(mindur) blanks cells with short durations", {
  dat <- rbind(c(1, 2, 3, 4),
               c(1, 2, 3, 4))
  # durations: row 1 = 1, 1, 0.2, 0.8; row 2 = 3, 1, 1, 1
  times <- rbind(c(0, 1, 2, 2.2, 3),
                 c(0, 3, 4, 5, 6))
  expect_identical(makePatterns(dat, times, num = FALSE, mindur = 0.5),
                   c("12NA4", "1234"))
  expect_identical(makePatterns(dat, times, num = FALSE, mindur = 1.5),
                   c("NANANANA", "1NANANA"))
})

test_that("makePatterns(mindur) keeps a duration equal to mindur", {
  # "durations less than a specified amount" (Tueller et al., 2016, p. 7)
  expect_identical(makePatterns(matrix(1:2, 1), times = c(0, 1, 3),
                                num = FALSE, mindur = 1),
                   "12")
})

test_that("makePatterns(mindur) accepts times shared by all rows", {
  dat <- rbind(c(1, 2, 3),
               c(3, 2, 1))
  expect_identical(makePatterns(dat, times = c(0, 2, 2.5, 5), num = FALSE,
                                mindur = 1),
                   c("1NA3", "3NA1"))
})

test_that("makePatterns(mindur) keeps cells whose duration is unknown", {
  times <- rbind(c(0, 0.1, NA))
  expect_identical(makePatterns(matrix(1:2, 1), times = times, num = FALSE,
                                mindur = 1),
                   "NA2")
})

test_that("makePatterns(mindur) requires times", {
  expect_error(makePatterns(matrix(1:4, 2), mindur = 1), "`mindur` requires `times`")
})

test_that("makePatterns() validates its arguments", {
  expect_error(makePatterns(1:3), "matrix or data frame")
  expect_error(makePatterns(matrix(numeric(0), 2, 0)), "at least one column")
  expect_error(makePatterns(matrix(1:4, 2), num = NA), "`num` must be TRUE or FALSE")
  expect_error(makePatterns(matrix(1:4, 2), igrpt = "yes"), "`igrpt` must be TRUE or FALSE")
  expect_error(makePatterns(matrix(1:4, 2), times = matrix(1:6, 2), mindur = -1),
               "single non-negative number")
  expect_error(makePatterns(matrix(1:4, 2), times = matrix(1:4, 2), mindur = 1),
               "nrow\\(dat\\) rows and ncol\\(dat\\) \\+ 1 columns")
  expect_error(makePatterns(matrix(1:4, 2), times = 1:2, mindur = 1),
               "length ncol\\(dat\\) \\+ 1")
  expect_error(makePatterns(matrix(1:4, 2), times = matrix("a", 2, 3), mindur = 1),
               "`times` must be numeric")
})

test_that("sorter() works when sorting on a single column", {
  # lc$y[, 1] used to drop to a vector and makePatterns() errored
  lc <- small_lc()
  expect_silent(s <- sorter(lc, whichColumns = 1))
  expect_identical(s$order.y[, 2], makePatterns(lc$y[, 1, drop = FALSE],
                                                lc$times[, 1, drop = FALSE],
                                                num = TRUE))
})
