# ---- states and labels ------------------------------------------------------

test_that("longCat() recodes states to 1:k in increasing order", {
  lc <- longCat(matrix(c(7, 2, 2, NA, 11, 7), 2))
  expect_equal(unname(lc$y), matrix(c(2, 1, 1, NA, 3, 2), 2))
  expect_identical(lc$factors, c(1, 2, 3))
  expect_identical(lc$nfactors, 3L)
})

test_that("default Labels are the original state values", {
  # previously 1 2 for 0/1 data
  expect_identical(longCat(matrix(c(0, 1, 1, 0), 2))$Labels, c(0, 1))
  # previously state 5 was labelled 4 when state 4 never occurs
  expect_identical(longCat(matrix(c(1, 2, 3, 5), 2))$Labels, c(1, 2, 3, 5))
})

test_that("text states that look like numbers sort numerically", {
  # previously "10" < "11" < "9"
  lc <- longCat(matrix(c("9", "10", "9", "11"), 2))
  expect_identical(lc$Labels, c("9", "10", "11"))
  expect_equal(unname(lc$y), matrix(c(1, 2, 1, 3), 2))
})

test_that("other text states sort alphabetically", {
  lc <- longCat(matrix(c("neutral", "agree", "disagree", "agree"), 2))
  expect_identical(lc$Labels, c("agree", "disagree", "neutral"))
})

test_that("factor columns share codes, in level order", {
  # default levels: a = c("hi", "lo"), b = "lo"; the old code coded "lo" as
  # 2 in column a and 1 in column b
  d <- data.frame(a = factor(c("lo", "hi")), b = factor(c("lo", "lo")))
  lc <- longCat(d)
  expect_identical(lc$Labels, c("hi", "lo"))
  expect_equal(unname(lc$y), matrix(c(2, 1, 2, 2), 2))

  d2 <- data.frame(a = factor(c("lo", "hi"), levels = c("lo", "mid", "hi")),
                   b = factor(c("hi", "hi"), levels = c("lo", "mid", "hi")))
  lc2 <- longCat(d2)
  # unused level "mid" is dropped
  expect_identical(lc2$Labels, c("lo", "hi"))
  expect_equal(unname(lc2$y), matrix(c(1, 2, 2, 2), 2))
})

test_that("mixed factor and numeric columns keep the factor labels", {
  d <- data.frame(a = factor(c("lo", "hi")), b = c(1, 2))
  lc <- longCat(d)
  expect_identical(lc$Labels, c("hi", "lo", "1", "2"))
  expect_equal(unname(lc$y), matrix(c(2, 1, 3, 4), 2))
})

test_that("numeric data frames give the same object as matrices", {
  m <- matrix(c(1, 3, NA, 2, 2, 1), 3)
  expect_equal(unname(longCat(as.data.frame(m))$y), unname(longCat(m)$y))
  expect_identical(longCat(as.data.frame(m))$Labels, longCat(m)$Labels)
})

test_that("user Labels must have one label per state", {
  expect_identical(longCat(matrix(1:4, 2), Labels = letters[1:4])$Labels,
                   letters[1:4])
  # previously only a warning, followed by a mislabelled legend
  expect_error(longCat(matrix(1:4, 2), Labels = c("a", "b")),
               "one label per state in `y` \\(4\\), not 2")
})

test_that("longCat() rejects unusable y", {
  expect_error(longCat(1:4), "`y` must be a matrix or data frame")
  expect_error(longCat(matrix(numeric(0), 0, 2)), "at least one row")
  expect_error(longCat(matrix(NA, 2, 2)), "`y` has no observed")
})

# ---- times ---------------------------------------------------------------

test_that("longCat() builds times from NULL, a vector, or a matrix", {
  y <- matrix(1:4, 2)
  lc_null <- longCat(y)
  expect_identical(lc_null$times, matrix(c(0, 0, 1, 1, 2, 2), 2))
  expect_false(lc_null$IndTime)

  lc_vec <- longCat(y, times = c(0, 5, 10))
  expect_identical(lc_vec$times[2, ], c(0, 5, 10))
  expect_false(lc_vec$IndTime)

  tm <- rbind(c(0, 1, 2), c(0, 2, 4))
  lc_mat <- longCat(y, times = tm)
  expect_identical(lc_mat$times, tm)
  expect_true(lc_mat$IndTime)
  expect_true(longCat(y, times = as.data.frame(tm))$IndTime)
})

test_that("longCat() checks the shape and order of times", {
  y <- matrix(1:4, 2)
  expect_error(longCat(y, times = 0:1), "length ncol\\(y\\) \\+ 1")
  expect_error(longCat(y, times = matrix(0:3, 2)), "ncol\\(y\\) \\+ 1 columns")
  expect_error(longCat(y, times = c(0, 2, 1)), "must not decrease")
  # equal times and missing times are allowed
  expect_silent(longCat(y, times = rbind(c(0, 0, 1), c(0, NA, 1))))
})

test_that("tLabels are kept with common times and dropped with a warning otherwise", {
  y <- matrix(1:4, 2)
  expect_identical(longCat(y, tLabels = c("pre", "post"))$tLabels,
                   c("pre", "post"))
  # previously dropped silently
  expect_warning(lc <- longCat(y, times = rbind(0:2, c(0, 2, 3)),
                               tLabels = c("pre", "post")),
                 "`tLabels` are ignored")
  expect_null(lc$tLabels)
  # a matrix whose rows are all equal still counts as common times
  expect_identical(longCat(y, times = rbind(0:2, 0:2),
                           tLabels = c("pre", "post"))$tLabels,
                   c("pre", "post"))
})

# ---- ids ----------------------------------------------------------------

test_that("longCat() stores ids in the first column of order.y", {
  expect_identical(longCat(matrix(1:4, 2))$order.y[, 1], 1:2)
  expect_identical(longCat(matrix(1:4, 2), id = c("a", "b"))$order.y[, 1],
                   c("a", "b"))
  expect_error(longCat(matrix(1:4, 2), id = 1:3), "one value per row of `y`")
})

# ---- events -------------------------------------------------------------

test_that("eventLabels default to the events in sorted order", {
  lc <- longCat(matrix(1:2, 2, 2), events = matrix(c(3, 1), 2),
                event.times = matrix(c(0.5, 0.5), 2))
  # previously c(3, 1): event 1 (code 1) was labelled "3"
  expect_identical(lc$eventLabels, c(1, 3))
  expect_equal(unname(lc$events), matrix(c(2, 1), 2))
})

test_that("text events are recoded to numeric codes", {
  lc <- longCat(matrix(1:2, 2, 2), events = matrix(c("test", "arrest"), 2),
                event.times = matrix(c(0.5, 0.5), 2))
  # previously the codes stayed text ("1", "2") and plotted as digits
  expect_true(is.numeric(lc$events))
  expect_identical(lc$eventLabels, c("arrest", "test"))
  expect_equal(unname(lc$events), matrix(c(2, 1), 2))
})

test_that("user eventLabels must have one label per distinct event", {
  ev <- small_events()
  expect_identical(small_lc(events = ev$events, event.times = ev$event.times,
                            eventLabels = c("Arrest", "Test"))$eventLabels,
                   c("Arrest", "Test"))
  expect_error(small_lc(events = ev$events, event.times = ev$event.times,
                        eventLabels = "Arrest"),
               "one label per distinct event \\(2\\), not 1")
})

test_that("events and event.times must be given together and match", {
  y <- matrix(1:2, 2, 2)
  # previously: "argument is of length zero"
  expect_error(longCat(y, events = matrix(1, 2)), "must be given together")
  expect_error(longCat(y, event.times = matrix(1, 2)), "must be given together")
  expect_error(longCat(y, events = matrix(1, 3), event.times = matrix(1, 3)),
               "one row per row of `y`")
  expect_error(longCat(y, events = matrix(1, 2, 2), event.times = matrix(1, 2)),
               "same dimension")
  expect_error(longCat(y, events = matrix(1, 2), event.times = matrix("a", 2)),
               "`event.times` must be numeric")
})

test_that("longCat() objects with every kind of input plot without error", {
  d <- data.frame(a = factor(c("lo", "hi", "lo")), b = factor(c("hi", "hi", "lo")))
  lc <- longCat(d, times = c(0, 1, 3), tLabels = c("t1", "t2"),
                events = matrix(c("x", "y", NA), 3),
                event.times = matrix(c(0.5, 2, NA), 3))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_silent(longCatPlot(lc))
})
