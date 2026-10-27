# Three people observed weekly; person 3 is never in jail.
weekly <- function() {
  data.frame(id = rep(1:3, each = 4), day = rep(c(0, 7, 14, 21), 3),
             status = c("Home", "Jail", "Home", "Jail",
                        "Jail", "Home", "Home", "Home",
                        "Home", "Home", "Home", "Home"),
             age = rep(c(30, 41, 25), each = 4))
}

align_jail <- function(d, nth = 1, ...) {
  alignTime(d, time = "day", state = "status", which.state = "Jail",
            nth.state = nth, ...)
}

test_that("long data get aligned times, keeping all columns and rows", {
  d <- weekly()
  out <- align_jail(d)
  expect_identical(out[names(d)], d)
  expect_named(out, c(names(d), "aligned_time"))
  # person 1 at their first jail (day 7), person 2 at day 0; person 3,
  # never in jail, at the end of their last week (day 28)
  expect_identical(out$aligned_time,
                   c(-7, 0, 7, 14, 0, 7, 14, 21, -28, -21, -14, -7))
  expect_identical(align_jail(d, nth = 2)$aligned_time[1:4],
                   c(-21, -14, -7, 0))
})

test_that("the order of the rows does not matter", {
  d <- weekly()
  set.seed(4)
  shuffled <- d[sample(nrow(d)), ]
  out <- align_jail(shuffled)
  expect_identical(rownames(out), rownames(shuffled))
  expect_identical(out[order(as.integer(rownames(out))), ], align_jail(d))
})

test_that("the alignment matches alignTime() of the wide data", {
  set.seed(642531)
  y <- matrix(sample(1:5, 120, replace = TRUE), 20, 6)
  times <- t(apply(matrix(runif(140, 1, 3), 20, 7), 1, cumsum))
  long <- data.frame(id = rep(1:20, each = 6), time = c(t(times[, 1:6])),
                     stop = c(t(times[, -1])), state = c(t(y)))
  wide <- alignTime(y, times, which.state = 3, nth.state = 2)$aligned.times
  out <- alignTime(long, stop = "stop", which.state = 3, nth.state = 2)
  expect_equal(out$aligned_time, c(t(wide[, 1:6])))
  expect_equal(out$aligned_time_stop, c(t(wide[, -1])))
  # with not.state
  wide <- alignTime(y, times, which.state = 1, nth.state = 3,
                    not.state = TRUE)$aligned.times
  out <- alignTime(long, stop = "stop", which.state = 1, nth.state = 3,
                   not.state = TRUE)
  expect_equal(out$aligned_time, c(t(wide[, 1:6])))
})

test_that("factor states are matched by their values", {
  d <- weekly()
  d$status <- factor(d$status, levels = c("Jail", "Home"))
  expect_identical(align_jail(d)$aligned_time, align_jail(weekly())$aligned_time)
})

test_that("cases can be aligned at an event from a long events table", {
  d <- weekly()
  ev <- data.frame(id = c(1, 1, 2, 3), time = c(3, 10, 12, 20),
                   event = c("Arrest", "Hearing", "Hearing", "Arrest"))
  out <- alignTime(d, time = "day", state = "status", events = ev,
                   which.event = "Hearing", nth.event = 1)
  # person 1 at day 10, person 2 at day 12; person 3, without a hearing, at
  # their last time (day 28, after their arrest at day 20)
  expect_identical(unique(out$day - out$aligned_time), c(10, 12, 28))
})

test_that("arguments for the alignment are checked", {
  d <- weekly()
  expect_error(alignTime(d, time = "day", state = "status"),
               "Give exactly one of `which.state` or `which.event`")
  expect_error(align_jail(d, foo = 1), "Unknown arguments to alignTime\\(\\): foo")
  expect_error(align_jail(d, event.times = matrix(1)),
               "give the events as a long data frame")
  expect_error(alignTime(d, time = "day", state = "status", which.event = 1,
                         nth.event = 1),
               "requires `events` and `event.times`")
})

test_that("alignTime() checks the long data and the new column", {
  d <- weekly()
  expect_error(alignTime(d, state = "status", which.state = "Jail",
                         nth.state = 1),
               "`y` has no column \"time\"")
  expect_error(align_jail(d, into = "age"), "already has a column \"age\"")
  expect_error(align_jail(d, into = c("a", "b")), "`into` must be a single")
  out <- align_jail(d, into = "since_jail")
  expect_true("since_jail" %in% names(out))
  d$day[2] <- 0
  expect_error(align_jail(d), "more than one row for the same time")
})

test_that("wide data frames with times are aligned as before", {
  y <- data.frame(t1 = c(1, 2), t2 = c(2, 2))
  out <- alignTime(y, times = 0:2, which.state = 2, nth.state = 1)
  expect_identical(out$aligned.times, rbind(c(-1, 0, 1), c(0, 1, 2)))
  expect_null(out$aligned.event.times)
})
