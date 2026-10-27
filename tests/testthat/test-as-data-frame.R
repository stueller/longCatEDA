events_lc <- function() {
  ev <- small_events()
  longCat(small_y(), events = ev$events, event.times = ev$event.times)
}

# ---- states -------------------------------------------------------------------

test_that("as.data.frame() gives one row per interval, case by case", {
  d <- as.data.frame(small_lc())
  expect_named(d, c("id", "occasion", "start", "stop", "state"))
  expect_identical(d$id, rep(1:4, each = 3))
  expect_identical(d$occasion, rep(1:3, times = 4))
  expect_identical(d$start, rep(c(0, 1, 2), times = 4))
  expect_identical(d$stop, rep(c(1, 2, 3), times = 4))
  expect_identical(d$state, factor(c(1, 2, 3, 2, 2, 1, 3, 1, NA, 1, 3, 2)))
  expect_identical(rownames(d), as.character(1:12))
  # na.rm drops the missing interval of case 3
  d <- as.data.frame(small_lc(), na.rm = TRUE)
  expect_identical(nrow(d), 11L)
  expect_false(anyNA(d$state))
})

test_that("as.data.frame() uses labels, ids, tLabels, and individual times", {
  lc <- longCat(small_y(), times = rbind(0:3, 10:13, 0:3, c(0, 5, 6, 7)),
                Labels = c("low", "mid", "high"), id = c(11, 12, 13, 14))
  d <- as.data.frame(lc)
  expect_named(d, c("id", "occasion", "start", "stop", "state"))
  expect_identical(d$id, rep(c(11, 12, 13, 14), each = 3))
  expect_identical(levels(d$state), c("low", "mid", "high"))
  expect_identical(as.character(d$state[1:3]), c("low", "mid", "high"))
  expect_identical(d$start[4:6], c(10, 11, 12))
  expect_identical(d$stop[10:12], c(5, 6, 7))

  d <- as.data.frame(small_lc(tLabels = c("c", "b", "a")))
  expect_named(d, c("id", "occasion", "tLabel", "start", "stop", "state"))
  # levels in column order, not alphabetical
  expect_identical(d$tLabel, factor(rep(c("c", "b", "a"), 4),
                                    levels = c("c", "b", "a")))
})

test_that("sorted objects come out in plot order", {
  s <- sorter(small_lc())
  d <- as.data.frame(s)
  expect_named(d, c("id", "row", "occasion", "start", "stop", "state"))
  # ascending sequences: (1,2,3), (1,3,2), (2,2,1), (3,1)
  expect_identical(d$id, rep(c(1L, 4L, 2L, 3L), each = 3))
  expect_identical(d$row, rep(1:4, each = 3))
  expect_identical(d$state[d$row == 2], factor(c(1, 3, 2), levels = 1:3))
  # sorted = FALSE gives the original order without plot rows
  expect_identical(as.data.frame(s, sorted = FALSE), as.data.frame(small_lc()))
})

test_that("groups and the blank rows between them are handled", {
  s <- sorter(small_lc(), group = c("b", "a", "b", "a"), ggap = 0.25)
  d <- as.data.frame(s)
  expect_named(d, c("id", "row", "group", "occasion", "start", "stop", "state"))
  expect_identical(levels(d$group), c("a", "b"))
  # one blank row after group "a" (rows 1-2), so group "b" starts at row 4
  expect_identical(unique(d$row), c(1L, 2L, 4L, 5L))
  expect_identical(as.character(unique(d$group)), c("a", "b"))
  # every state matches the sorted data the plot draws
  expect_identical(as.integer(d$state),
                   s$y.sorted[cbind(d$row, d$occasion)])
  expect_identical(unique(d$id), s$order.y.sorted[unique(d$row), 1])
})

test_that("cases with a missing group are left out like in the plot", {
  s <- suppressWarnings(sorter(small_lc(), group = c(1, NA, 1, 2)))
  d <- as.data.frame(s)
  expect_false(2 %in% d$id)
  expect_identical(sort(unique(d$id)), c(1L, 3L, 4L))
})

# ---- events -------------------------------------------------------------------

test_that("what = 'events' gives one row per observed event", {
  d <- as.data.frame(events_lc(), what = "events")
  expect_named(d, c("id", "event", "time"))
  expect_identical(d$id, c(1L, 1L, 2L, 3L, 3L, 4L))
  expect_identical(as.integer(d$event), c(1L, 2L, 2L, 1L, 1L, 2L))
  expect_identical(d$time, c(0.5, 1.5, 1.2, 0.1, 2.2, 2.5))

  s <- sorter(events_lc())
  d <- as.data.frame(s, what = "events")
  expect_named(d, c("id", "row", "event", "time"))
  expect_identical(d$id, c(1L, 1L, 4L, 2L, 3L, 3L))
  expect_identical(d$row, c(1L, 1L, 2L, 3L, 4L, 4L))
})

test_that("objects without events give an empty events table", {
  d <- as.data.frame(small_lc(), what = "events")
  expect_identical(nrow(d), 0L)
  expect_named(d, c("id", "event", "time"))
})

# ---- longCont -------------------------------------------------------------------

test_that("as.data.frame() of a longCont object gives one row per value", {
  lcont <- longCont(rbind(c(1, 2, 3), c(2, NA, 1)), times = c(0, 5, 10))
  d <- as.data.frame(lcont)
  expect_identical(d, data.frame(id = rep(1:2, each = 3),
                                 occasion = rep(1:3, 2),
                                 time = rep(c(0, 5, 10), 2),
                                 value = c(1, 2, 3, 2, NA, 1)))
  expect_identical(nrow(as.data.frame(lcont, na.rm = TRUE)), 5L)
})

# ---- arguments ----------------------------------------------------------------

test_that("as.data.frame() checks its arguments", {
  lc <- small_lc()
  expect_error(as.data.frame(lc, sorted = TRUE), "not sorted")
  expect_error(as.data.frame(lc, sorted = NA), "`sorted` must be")
  expect_error(as.data.frame(lc, na.rm = "yes"), "`na.rm` must be")
  expect_error(as.data.frame(lc, what = "times"), "should be one of")
  expect_error(as.data.frame(lc, row.names = "a"), "length 12")
  expect_identical(rownames(as.data.frame(lc, row.names = letters[1:12])),
                   letters[1:12])
  expect_error(as.data.frame(longCont(1:2 + matrix(0, 2, 2)), na.rm = NA),
               "`na.rm` must be")
})
