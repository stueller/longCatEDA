duration_sort_keys <- longCatEDA:::duration_sort_keys

# six cases, three intervals
sorter_y <- rbind(c(1, 2, 2),
                  c(2, 1, 1),
                  c(1, 2, 2),
                  c(1, 1, 2),
                  c(2, 2, 2),
                  c(1, 2, 2))

# ---- issue #6: one-column events ------------------------------------------

test_that("one-column events sort and plot (issue #6)", {
  # previously: "subscript out of bounds" / "incorrect number of dimensions"
  lc <- longCat(sorter_y, events = matrix(c(1, 2, NA, 1, 2, 1), ncol = 1),
                event.times = matrix(c(0.5, 1, NA, 2, 2.5, 0.1), ncol = 1))
  s <- sorter(lc)
  expect_identical(dim(s$events.sorted), c(6L, 1L))
  expect_identical(s$events.sorted[, 1], lc$events[s$order.y.sorted[, 1], 1])

  grouped <- sorter(lc, group = c(1, 1, 2, 2, 2, 1))
  expect_identical(ncol(grouped$events.sorted), 1L)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_silent(plot(lc))
  expect_silent(plot(grouped))
})

test_that("sorting on one column and one-row groups keep their shape", {
  lc <- longCat(sorter_y)
  expect_identical(dim(sorter(lc, whichColumns = 2)$y.sorted), c(6L, 3L))
  s <- sorter(lc, group = c(1, 2, 2, 2, 2, 2), ggap = 0)
  expect_identical(dim(s$y.sorted), c(6L, 3L))
  expect_identical(s$group.sorted[, 1], c(1, 2, 2, 2, 2, 2))
})

# ---- groups and gaps (issue #1) --------------------------------------------

test_that("a user-supplied ggap sets the blank rows after each group (issue #1)", {
  lc <- longCat(sorter_y)
  group <- c(1, 2, 1, 2, 1, 2)
  # previously any ggap was replaced by 0.05
  s <- sorter(lc, group = group, ggap = 0.5)
  expect_identical(nrow(s$y.sorted), 6L + 2L * 3L)
  expect_identical(s$group.sorted[, 1], c(1, 1, 1, NA, NA, NA, 2, 2, 2, NA, NA, NA))
  expect_true(all(is.na(s$y.sorted[4:6, ])))

  expect_identical(nrow(sorter(lc, group = group, ggap = 0)$y.sorted), 6L)
  # default: ceiling(0.05 * 6) = 1 blank row per group
  expect_identical(nrow(sorter(lc, group = group)$y.sorted), 8L)
  # no blank rows with a single group
  expect_identical(nrow(sorter(lc, ggap = 0.5)$y.sorted), 6L)
  expect_error(sorter(lc, group = group, ggap = 2), "between 0 and 1")
})

test_that("cases with a missing group are left out, with a warning", {
  lc <- longCat(sorter_y)
  group <- c(1, NA, 2, 1, NA, 2)
  expect_warning(s <- sorter(lc, group = group, ggap = 0),
                 "2 case\\(s\\) with a missing `group`")
  # previously each such case added a row of NAs to every group
  expect_identical(nrow(s$y.sorted), 4L)
  expect_false(any(is.na(s$y.sorted)))
  expect_setequal(s$order.y.sorted[, 1], c(1, 3, 4, 6))
})

test_that("groups come in ascending order with default labels", {
  lc <- longCat(sorter_y)
  s <- sorter(lc, group = c(3, 1, 3, 1, 3, 1), ggap = 0, ascending = FALSE)
  expect_identical(s$group.sorted[, 1], c(1, 1, 1, 3, 3, 3))
  expect_identical(s$groupLabels, c(1, 3))

  s_chr <- sorter(lc, group = c("b", "a", "b", "a", "b", "a"), ggap = 0)
  expect_identical(s_chr$groupLabels, c("a", "b"))
  expect_identical(s_chr$group.sorted[, 1], c(1, 1, 1, 2, 2, 2))

  fct <- factor(c("hi", "lo", "hi", "lo", "hi", "lo"), levels = c("lo", "mid", "hi"))
  s_fct <- sorter(lc, group = fct, ggap = 0)
  expect_identical(s_fct$groupLabels, c("lo", "hi"))  # unused level dropped
})

test_that("group arguments are checked", {
  lc <- longCat(sorter_y)
  expect_error(sorter(lc, group = 1:3), "one value per case \\(6\\), not 3")
  expect_error(sorter(lc, group = rep(1:2, 3), groupLabels = "a"),
               "one label per group \\(2\\), not 1")
  expect_error(sorter(lc, groupLabels = "a"), "requires `group`")
})

# ---- other arguments --------------------------------------------------------

test_that("sorter() checks its other arguments", {
  lc <- longCat(sorter_y)
  expect_error(sorter(sorter(lc)), "already sorted")
  expect_error(sorter(lc, whichColumns = 4), "between 1 and 3")
  expect_error(sorter(lc, whichColumns = c(1, NA)), "between 1 and 3")
  expect_error(sorter(lc, customSort = 1:2), "one value per case")
  expect_error(sorter(lc, customSort = c(1:5, NA)), "must not contain missing")
  expect_error(sorter(lc, ascending = NA), "`ascending` must be TRUE or FALSE")
  expect_error(sorter(lc, sortDurations = "yes"), "`sortDurations` must be")
})

test_that("sorted components are matrices, and the unsorted data are kept", {
  lc <- longCat(sorter_y)
  s <- sorter(lc, group = rep(1:2, 3))
  # previously data frames when grouped
  expect_true(is.matrix(s$y.sorted))
  expect_true(is.matrix(s$times.sorted))
  expect_true(is.matrix(s$group.sorted))
  expect_true(is.data.frame(s$order.y.sorted))
  expect_identical(s$y, lc$y)
  expect_identical(s$times, lc$times)
  expect_true(s$sorted)
})

test_that("customSort is stored in order.y and sorted on before states", {
  lc <- longCat(sorter_y)
  s <- sorter(lc, customSort = c(2, 2, 2, 1, 1, 1))
  expect_identical(s$order.y[, 3], c(2, 2, 2, 1, 1, 1))
  expect_identical(s$order.y.sorted[, 1], c(4L, 6L, 5L, 1L, 3L, 2L))
})

# ---- secondary sort on durations ---------------------------------------------

test_that("duration_sort_keys() aligns durations with the state keys", {
  y <- rbind(c(1, NA, 2),
             c(1, 1, 2))
  d <- rbind(c(1, 5, 2),
             c(3, 4, 6))
  expect_identical(duration_sort_keys(y, d, num = FALSE, igrpt = FALSE),
                   rbind(c(1, NA, 2), c(3, 4, 6)))
  expect_identical(duration_sort_keys(y, d, num = TRUE, igrpt = FALSE),
                   rbind(c(1, 2, NA), c(3, 4, 6)))
  # with igrpt, the run c(1, 1) in row 2 lasts 3 + 4
  expect_identical(duration_sort_keys(y, d, num = TRUE, igrpt = TRUE),
                   rbind(c(1, 2, NA), c(7, 6, NA)))
})

test_that("identical state sequences are ordered by their durations", {
  # cases 1, 3, and 6 have the sequence (1, 2, 2) but spend 3, 1, and 2 in
  # state 1
  times <- rbind(c(0, 3, 4, 5),
                 c(0, 1, 2, 3),
                 c(0, 1, 2, 3),
                 c(0, 1, 2, 3),
                 c(0, 1, 2, 3),
                 c(0, 2, 3, 4))
  lc <- longCat(sorter_y, times = times)
  ids <- function(s) s$order.y.sorted[, 1]
  # IndTime is TRUE, so sortDurations defaults to TRUE
  expect_identical(ids(sorter(lc)), c(4L, 3L, 6L, 1L, 2L, 5L))
  expect_identical(ids(sorter(lc, ascending = FALSE)), c(5L, 2L, 1L, 6L, 3L, 4L))
  # without it, ties keep their original order
  expect_identical(ids(sorter(lc, sortDurations = FALSE)),
                   c(4L, 1L, 3L, 6L, 2L, 5L))
})

test_that("durations are not used by default when times are common", {
  lc <- longCat(sorter_y, times = c(0, 1, 5, 6))
  expect_false(lc$IndTime)
  expect_identical(sorter(lc)$order.y.sorted[, 1],
                   sorter(lc, sortDurations = FALSE)$order.y.sorted[, 1])
})

test_that("durations use the full times with whichColumns and mindur", {
  # sort on column 3 only; its duration is times[, 4] - times[, 3]
  times <- rbind(c(0, 1, 2, 9),
                 c(0, 1, 2, 3))
  lc <- longCat(rbind(c(1, 1, 2), c(1, 1, 2)), times = times)
  expect_identical(sorter(lc, whichColumns = 3)$order.y.sorted[, 1], c(2L, 1L))
})
