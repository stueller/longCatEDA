pack_events <- longCatEDA:::pack_events

test_that("pack_events() moves each row's events left with their times", {
  events <- rbind(c(NA, 2, NA, 1),
                  c(NA, NA, NA, NA),
                  c(3, NA, 3, NA))
  event.times <- rbind(c(9, 2, 9, 4),
                       c(NA, NA, NA, NA),
                       c(1, 9, 3, NA))
  out <- pack_events(events, event.times)
  expect_identical(out$events, rbind(c(2, 1, NA, NA),
                                     c(NA, NA, NA, NA),
                                     c(3, 3, NA, NA)))
  # times of missing events (the 9s) are dropped
  expect_identical(out$event.times, rbind(c(2, 4, NA, NA),
                                          c(NA, NA, NA, NA),
                                          c(1, 3, NA, NA)))
})

test_that("pack_events() keeps the shape of one-column and data frame input", {
  out <- pack_events(matrix(c(NA, 1), 2), matrix(c(NA, 5), 2))
  expect_identical(out$events, matrix(c(NA, 1), 2))
  expect_identical(out$event.times, matrix(c(NA, 5), 2))

  out_df <- pack_events(data.frame(a = c(NA, 1), b = c(2, 3)),
                        data.frame(a = c(0, 1), b = c(2, 3)))
  expect_true(is.matrix(out_df$events))
  expect_identical(unname(out_df$events), rbind(c(2, NA), c(1, 3)))
})

test_that("pack_events() requires matching dimensions", {
  expect_error(pack_events(matrix(1, 2, 2), matrix(1, 2, 3)), "same dimension")
  expect_error(pack_events(1:2, 1:2), "`events` must be a matrix")
})

test_that("plot(which.events, n.events) plots with packed events", {
  lc <- small_lc(events = small_events()$events,
                 event.times = small_events()$event.times)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_silent(plot(lc, which.events = 2))
  expect_silent(plot(lc, n.events = 1))
})
