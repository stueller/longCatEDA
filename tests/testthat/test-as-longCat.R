schedule_data <- function() {
  data.frame(id    = c(1, 1, 1, 2, 2, 2, 3, 3),
             time  = c(0, 1, 2, 0, 1, 2, 0, 1),
             state = c("a", "b", "b", "b", "a", "c", "c", "c"))
}

# ---- observation times --------------------------------------------------------

test_that("observations of a shared schedule become one column per time", {
  lc <- as.longCat(schedule_data())
  expect_s3_class(lc, "longCat")
  expect_identical(lc$Labels, c("a", "b", "c"))
  expect_identical(unname(lc$y), rbind(c(1L, 2L, 2L), c(2L, 1L, 3L),
                                       c(3L, 3L, NA)))
  # the last state lasts one time step
  expect_identical(lc$times[1, ], c(0, 1, 2, 3))
  expect_false(lc$IndTime)
  expect_identical(lc$order.y[, 1], c(1, 2, 3))
  expect_identical(as.longCat(schedule_data(), end = 10)$times[1, ],
                   c(0, 1, 2, 10))
})

test_that("rows may come in any order; cases keep their first appearance", {
  d <- schedule_data()[c(8, 3, 1, 7, 5, 2, 6, 4), ]
  lc <- as.longCat(d)
  expect_identical(lc$order.y[, 1], c(3, 1, 2))
  expect_identical(unname(lc$y[2, ]), c(1L, 2L, 2L))
})

test_that("individually varying observation times become a times matrix", {
  d <- data.frame(id = c(1, 1, 1, 2, 2, 3), time = c(0, 2, 5, 1, 4, 3),
                  state = c(1, 2, 1, 2, 2, 1))
  lc <- as.longCat(d)
  expect_true(lc$IndTime)
  expect_identical(unname(lc$y), rbind(c(1L, 2L, 1L), c(2L, 2L, NA),
                                       c(1L, NA, NA)))
  # each state lasts until the next observation, the last one step more;
  # with one observation the step is 1; shorter cases end in zero-length
  # missing intervals
  expect_identical(lc$times, rbind(c(0, 2, 5, 8), c(1, 4, 7, 7), c(3, 4, 4, 4)))
  expect_identical(as.longCat(d, end = 10)$times,
                   rbind(c(0, 2, 5, 10), c(1, 4, 10, 10), c(3, 10, 10, 10)))
})

test_that("factor states keep their level order and drop unused levels", {
  d <- schedule_data()
  d$state <- factor(d$state, levels = c("c", "w", "b", "a"))
  lc <- as.longCat(d)
  expect_identical(lc$Labels, c("c", "b", "a"))
  expect_identical(unname(lc$y[1, ]), c(3L, 2L, 2L))
  # further arguments go to longCat()
  expect_identical(as.longCat(d, Labels = c("C", "B", "A"))$Labels,
                   c("C", "B", "A"))
})

# ---- intervals ----------------------------------------------------------------

test_that("intervals become columns, with gaps as missing intervals", {
  d <- data.frame(id = c(1, 1, 2, 2), start = c(0, 1, 0, 2),
                  stop = c(1, 3, 1, 3), state = c("a", "b", "b", "a"))
  lc <- as.longCat(d, time = "start", stop = "stop")
  expect_identical(unname(lc$y), rbind(c(1L, 2L, NA), c(2L, NA, 1L)))
  expect_identical(lc$times, rbind(c(0, 1, 3, 3), c(0, 1, 2, 3)))
  expect_true(lc$IndTime)
})

test_that("overlapping or reversed intervals are errors", {
  d <- data.frame(id = c(1, 1), start = c(0, 1), stop = c(2, 3), state = 1:2)
  expect_error(as.longCat(d, time = "start", stop = "stop"), "must not overlap")
  d <- data.frame(id = 1, start = 2, stop = 1, state = 1)
  expect_error(as.longCat(d, time = "start", stop = "stop"),
               "stop time must not be before its start")
})

# ---- round trips ----------------------------------------------------------------

test_that("as.longCat() reverses as.data.frame()", {
  round_trip <- function(lc) {
    as.longCat(as.data.frame(lc), time = "start", stop = "stop")
  }
  lc <- longCat(example3)
  lc2 <- round_trip(lc)
  expect_identical(unname(lc2$y), unname(lc$y))
  expect_identical(lc2$times, lc$times)
  expect_identical(lc2$IndTime, lc$IndTime)
  expect_identical(lc2$order.y[, 1], lc$order.y[, 1])

  # individually varying times, labels, and missing states
  lc <- longCat(small_y(), times = rbind(0:3, 10:13, 0:3, c(0, 5, 6, 7)),
                Labels = c("low", "mid", "high"), id = c(11, 12, 13, 14))
  lc2 <- round_trip(lc)
  expect_identical(unname(lc2$y), unname(lc$y))
  expect_identical(lc2$times, lc$times)
  expect_identical(lc2$Labels, lc$Labels)
  expect_identical(lc2$order.y[, 1], lc$order.y[, 1])
})

test_that("events round-trip through the events table", {
  ev <- small_events()
  lc <- longCat(small_y(), events = ev$events, event.times = ev$event.times,
                eventLabels = c("x", "y"))
  lc2 <- as.longCat(as.data.frame(lc), time = "start", stop = "stop",
                    events = as.data.frame(lc, what = "events"))
  expect_identical(unname(lc2$events), unname(lc$events))
  expect_identical(unname(lc2$event.times), unname(lc$event.times))
  expect_identical(lc2$eventLabels, lc$eventLabels)
})

test_that("events are put in time order within each case", {
  events <- data.frame(id = c(2, 1, 1), event = c("x", "y", "x"),
                       time = c(0.5, 1.5, 0.2))
  lc <- as.longCat(schedule_data(), events = events)
  expect_identical(lc$eventLabels, c("x", "y"))
  expect_identical(unname(lc$events), rbind(c(1L, 2L), c(1L, NA), c(NA, NA)))
  expect_identical(unname(lc$event.times),
                   rbind(c(0.2, 1.5), c(0.5, NA), c(NA, NA)))
  expect_error(as.longCat(schedule_data(),
                          events = data.frame(id = 9, event = 1, time = 1)),
               "Every case ID in `events`")
})

# ---- longCont -----------------------------------------------------------------

test_that("as.longCont() builds a schedule or individual times", {
  d <- data.frame(id = c("p", "p", "q"), time = c(1, 2, 2), value = c(0.5, 1, 2))
  lcont <- as.longCont(d)
  expect_s3_class(lcont, "longCont")
  expect_identical(lcont$y, rbind(c(0.5, 1), c(NA, 2)))
  expect_identical(lcont$times[1, ], c(1, 2))
  expect_false(lcont$IndTime)

  d <- data.frame(id = c(1, 1, 2, 2), time = c(1, 2, 1.5, 3), value = 1:4)
  lcont <- as.longCont(d)
  expect_true(lcont$IndTime)
  expect_identical(lcont$y, rbind(c(1, 2), c(3, 4)))
  expect_identical(lcont$times, rbind(c(1, 2), c(1.5, 3)))
})

test_that("as.longCont() reverses as.data.frame()", {
  lcont <- longCont(example2cont, times = c(1, 100, 200, 300, 400, 500))
  lcont2 <- as.longCont(as.data.frame(lcont))
  expect_identical(lcont2$y, lcont$y)
  expect_identical(lcont2$times, lcont$times)
})

# ---- arguments ----------------------------------------------------------------

test_that("as.longCat() and as.longCont() check their input", {
  d <- schedule_data()
  expect_error(as.longCat(d, state = "status"), "no column \"status\"")
  expect_error(as.longCat(d, id = c("id", "time")), "`id` must be a single")
  expect_error(as.longCat(d, stop = "end"), "no column \"end\"")
  d$id[2] <- NA
  expect_error(as.longCat(d), "must not be missing")
  d <- schedule_data()
  d$time[1] <- NA
  expect_error(as.longCat(d), "must not have missing values")
  d$time <- as.character(schedule_data()$time)
  expect_error(as.longCat(d), "must be numeric")
  d <- rbind(schedule_data(), schedule_data()[2, ])
  expect_error(as.longCat(d), "Case 1 has more than one row")
  expect_error(as.longCat(schedule_data(), end = 1), "`end` must be")
  expect_error(as.longCont(data.frame(id = 1, time = 1, y = 2)),
               "no column \"value\"")
  expect_error(as.longCat(list()), "no applicable method")
})

test_that("objects of the class itself are returned unchanged", {
  lc <- small_lc()
  expect_identical(as.longCat(lc), lc)
  lcont <- longCont(example2cont)
  expect_identical(as.longCont(lcont), lcont)
})
