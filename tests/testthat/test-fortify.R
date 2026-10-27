skip_if_not_installed("ggplot2")

fortify <- ggplot2::fortify
ggplot <- ggplot2::ggplot
aes <- ggplot2::aes

events_lc <- function(...) {
  ev <- small_events()
  longCat(small_y(), events = ev$events, event.times = ev$event.times, ...)
}

# ---- longCat ----------------------------------------------------------------------

test_that("ggplot() takes a longCat object as its data", {
  lc <- small_lc()
  d <- fortify(lc)
  expected <- as.data.frame(sorter(lc), na.rm = TRUE)
  expect_identical(d[names(expected)], expected)
  expect_named(d, c(names(expected), "y", "ymin", "ymax"))
  # without weights, a line on each plot row and a band of height 1
  expect_equal(d$y, d$row)
  expect_equal(d$ymin, d$row - 0.5)
  expect_equal(d$ymax, d$row + 0.5)
  expect_identical(ggplot(lc)$data, d)
  p <- ggplot(lc, aes(x = start, xend = stop, y = y, yend = y,
                      colour = state)) + ggplot2::geom_segment()
  expect_equal(ggplot2::layer_data(p, 1)$y, d$y)
})

test_that("a sorted object keeps its order; sort = FALSE keeps the original", {
  s <- sorter(small_lc(), ascending = FALSE)
  expect_identical(fortify(s)$row, as.data.frame(s, na.rm = TRUE)$row)
  d <- fortify(small_lc(), sort = FALSE)
  expect_named(d, c("id", "row", "column", "start", "stop", "state", "y",
                    "ymin", "ymax"))
  expect_identical(unique(d$row), 1:4)
  # arguments given to ggplot() reach fortify()
  expect_identical(ggplot(small_lc(), sort = FALSE)$data, d)
})

test_that("missing states are kept with na.rm = FALSE", {
  y <- small_y()
  y[2, 2] <- NA
  lc <- longCat(y)
  expect_false(anyNA(fortify(lc)$state))
  d <- fortify(lc, na.rm = FALSE)
  # small_y() already has one missing state
  expect_identical(sum(is.na(d$state)), 2L)
  expect_identical(nrow(d), length(y))
})

test_that("weighted cases have bands proportional to their weights", {
  lc <- small_lc(weights = c(1, 2, 3, 2))
  d <- fortify(lc)
  pd <- longCatEDA:::plot_data(sorter(lc))
  expect_equal(d$y, pd$row_center[d$row])
  expect_equal(d$ymax - d$ymin, pd$row_height[d$row])
  # bands touch: the bottom of each band is the top of the one below
  bands <- unique(d[c("row", "ymin", "ymax")])
  bands <- bands[order(bands$row), ]
  expect_equal(bands$ymin[-1], bands$ymax[-nrow(bands)])
  expect_identical(unique(d[c("id", "weight")])$weight[order(unique(d$id))],
                   c(1, 2, 3, 2))
})

test_that("events have the vertical position of their case", {
  lc <- events_lc()
  e <- fortify(lc, what = "events")
  expected <- as.data.frame(sorter(lc), what = "events")
  expect_identical(e[names(expected)], expected)
  states <- fortify(lc)
  expect_equal(e$y, states$y[match(e$id, states$id)])
  expect_identical(ggplot(lc, what = "events")$data, e)
  # a layer given the object as data gets the states
  p <- ggplot(lc) +
    ggplot2::geom_point(aes(x = time, y = y), data = e) +
    ggplot2::geom_segment(aes(x = start, xend = stop, y = y, yend = y),
                          data = lc)
  expect_identical(nrow(ggplot2::layer_data(p, 2)), nrow(states))
  expect_identical(nrow(fortify(small_lc(), what = "events")), 0L)
})

test_that("groups keep their order, with the blank rows skipped", {
  s <- sorter(small_lc(), group = c("b", "a", "b", "a"), ggap = 0.25)
  d <- fortify(s)
  expect_identical(levels(d$group), c("a", "b"))
  expect_identical(unique(d$row), c(1L, 2L, 4L, 5L))
  expect_equal(unique(d$y), c(1, 2, 4, 5))
})

test_that("fortify() of a longCat object checks its arguments", {
  lc <- small_lc()
  expect_error(fortify(lc, colour = "red"),
               "Unknown arguments to fortify\\(\\) of a longCat object: colour")
  expect_error(ggplot(lc, foo = 1), "Unknown arguments to fortify")
  expect_error(fortify(lc, sort = NA), "`sort` must be TRUE or FALSE")
  expect_error(fortify(lc, na.rm = "yes"), "`na.rm` must be TRUE or FALSE")
  expect_error(fortify(lc, what = "times"), "should be one of")
  bad <- lc
  bad$dim <- 1
  expect_error(fortify(bad), "model")
})

# ---- longCont ---------------------------------------------------------------------

test_that("ggplot() takes a longCont object as its data", {
  lcont <- longCont(rbind(c(1, 2, NA), c(3, 4, 5)), times = c(0, 1, 2))
  expect_identical(fortify(lcont), as.data.frame(lcont))
  expect_identical(fortify(lcont, na.rm = TRUE),
                   as.data.frame(lcont, na.rm = TRUE))
  p <- ggplot(lcont, aes(time, value, group = id)) + ggplot2::geom_line()
  d <- suppressWarnings(ggplot2::layer_data(p, 1))
  expect_equal(d$y[!is.na(d$y)], c(1, 2, 3, 4, 5))
  expect_error(fortify(lcont, sort = TRUE),
               "Unknown arguments to fortify\\(\\) of a longCont object: sort")
})
