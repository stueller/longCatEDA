skip_if_not_installed("ggplot2")

autoplot <- ggplot2::autoplot
layer_data <- ggplot2::layer_data

events_lc <- function(...) {
  ev <- small_events()
  longCat(small_y(), events = ev$events, event.times = ev$event.times, ...)
}

geoms <- function(p) {
  vapply(p$layers, function(l) class(l$geom)[1], character(1), USE.NAMES = FALSE)
}

# ---- longCat ----------------------------------------------------------------------

test_that("autoplot() is registered for longCat and longCont objects", {
  expect_s3_class(autoplot(small_lc()), "ggplot")
  expect_s3_class(autoplot(longCont(example2cont)), "ggplot")
})

test_that("each state is a segment on the case's plot row", {
  lc <- small_lc()
  p <- autoplot(lc)
  expect_identical(geoms(p), "GeomSegment")
  d <- layer_data(p, 1)
  s <- sorter(lc)
  # the same intervals as as.data.frame() of the sorted object
  expected <- as.data.frame(s, na.rm = TRUE)
  expect_equal(d$x, expected$start)
  expect_equal(d$xend, expected$stop)
  expect_equal(d$y, expected$row)
  expect_identical(d$colour, colChoose("heat", 3)[as.integer(expected$state)])
  expect_equal(unique(d$linewidth), 0.5)
  expect_identical(p$labels$x, "Days")
  expect_identical(p$labels$y, "Each Line Represents a Participant, N = 4")
})

test_that("colors and labels can be chosen", {
  lc <- small_lc(Labels = c("low", "mid", "high"))
  p <- autoplot(lc, cols = c("red", "green", "blue"), linewidth = 2,
                xlab = "Weeks", ylab = "Cases")
  d <- layer_data(p, 1)
  expect_setequal(unique(d$colour), c("red", "green", "blue"))
  expect_equal(unique(d$linewidth), 2)
  expect_identical(p$labels$x, "Weeks")
  expect_identical(p$labels$y, "Cases")
  expect_identical(ggplot2::get_guide_data(p, "colour")$.label,
                   c("low", "mid", "high"))
  rev_cols <- layer_data(autoplot(lc, colScheme = "topo", reverse = TRUE), 1)
  expect_setequal(unique(rev_cols$colour), colChoose("topo", 3, TRUE))
  expect_error(autoplot(lc, cols = "red"), "one color per state")
})

test_that("with sort = FALSE unsorted cases keep their order", {
  d <- layer_data(autoplot(small_lc(), sort = FALSE), 1)
  expect_equal(unique(d$y), 1:4)
  # an object sorted with sorter() keeps its order
  s <- sorter(small_lc(), ascending = FALSE)
  expect_equal(layer_data(autoplot(s, sort = FALSE), 1)$y,
               as.data.frame(s, na.rm = TRUE)$row)
})

test_that("events are points with one shape per event type", {
  lc <- events_lc()
  p <- autoplot(lc)
  expect_identical(geoms(p), c("GeomSegment", "GeomPoint"))
  pts <- layer_data(p, 2)
  e <- as.data.frame(sorter(lc), what = "events")
  expect_equal(pts$x, e$time)
  expect_equal(pts$y, e$row)
  expect_equal(pts$shape, as.integer(e$event))
  expect_identical(geoms(autoplot(lc, events = FALSE)), "GeomSegment")
  expect_identical(geoms(autoplot(small_lc())), "GeomSegment")
})

test_that("groups are facets with the first group at the bottom", {
  s <- sorter(small_lc(), group = c("a", "b", "a", "b"))
  p <- autoplot(s)
  expect_s3_class(p$facet, "FacetGrid")
  d <- layer_data(p, 1)
  # panels run from the top: group "b" in panel 1, group "a" in panel 2
  expected <- as.data.frame(s, na.rm = TRUE)
  expect_identical(as.integer(d$PANEL), ifelse(expected$group == "a", 2L, 1L))
  expect_equal(d$y, expected$row)
  expect_null(autoplot(small_lc())$facet$params$rows)
})

test_that("weighted cases are bands as in plot()", {
  lc <- small_lc(weights = c(1, 2, 3, 2))
  p <- autoplot(lc)
  expect_identical(geoms(p), "GeomRect")
  d <- layer_data(p, 1)
  s <- sorter(lc)
  pd <- longCatEDA:::plot_data(s)
  rows <- as.data.frame(s, na.rm = TRUE)$row
  expect_equal(d$ymin, pd$row_center[rows] - pd$row_height[rows] / 2)
  expect_equal(d$ymax, pd$row_center[rows] + pd$row_height[rows] / 2)
  expect_identical(p$labels$y,
                   "Each Band Represents a Participant, N = 4 (Weighted N = 8)")
  # the weighted N counts only the plotted cases
  g <- suppressWarnings(sorter(lc, group = c(1, NA, 1, 2)))
  expect_match(autoplot(g)$labels$y,
               "N = 3 \\(Weighted N = 6\\)")
})

test_that("tLabels label the starts of the intervals", {
  lc <- small_lc(times = c(0, 10, 20, 30), tLabels = c("A", "B", "C"))
  x <- ggplot2::get_guide_data(autoplot(lc), "x")
  expect_equal(x$.value, c(0, 10, 20))
  expect_identical(x$.label, c("A", "B", "C"))
})

test_that("autoplot() of a longCat object checks its arguments", {
  lc <- small_lc()
  expect_error(autoplot(lc, colour = "red"),
               "Unknown arguments to autoplot\\(\\) of a longCat object: colour")
  expect_error(autoplot(lc, events = NA), "`events` must be TRUE or FALSE")
  expect_error(autoplot(lc, sort = "yes"), "`sort` must be TRUE or FALSE")
  bad <- lc
  bad$dim <- 1
  expect_error(autoplot(bad), "object")
})

# ---- longCont ---------------------------------------------------------------------

test_that("autoplot() of a longCont object draws one line per case", {
  lcont <- longCont(rbind(c(1, 2, NA), c(3, 4, 5)), times = c(0, 1, 2))
  p <- autoplot(lcont, alpha = 0.5, xlab = "Year")
  expect_identical(geoms(p), "GeomLine")
  d <- layer_data(p, 1)
  expect_equal(d$x, c(0, 1, 0, 1, 2))
  expect_equal(d$y, c(1, 2, 3, 4, 5))
  expect_identical(d$group, c(1L, 1L, 2L, 2L, 2L))
  expect_equal(unique(d$alpha), 0.5)
  expect_identical(p$labels$x, "Year")
  expect_error(autoplot(longCont(matrix(NA_real_, 2, 2))), "no non-missing values")
})
