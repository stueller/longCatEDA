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

# ---- the plot() options -----------------------------------------------------------

# The starts of each case's states, by case ID, in the plotted data.
starts_by_id <- function(p) as.vector(tapply(p$data$start, p$data$id, min))

test_that("texclude cuts the intervals and drops events outside", {
  lc <- events_lc(tLabels = c("A", "B", "C"))
  p <- autoplot(lc, texclude = c(0.5, 2))
  d <- layer_data(p, 1)
  expect_true(all(d$x >= 0.5 & d$xend <= 2 & d$x < d$xend))
  # events at 0.1, 2.2, and 2.5 are outside
  expect_setequal(layer_data(p, 2)$x, c(0.5, 1.5, 1.2))
  # tLabels only within the range
  expect_identical(ggplot2::get_guide_data(p, "x")$.label, c("B", "C"))
  # the N counts every case, also those cut away
  expect_identical(p$labels$y, "Each Line Represents a Participant, N = 4")
  expect_error(autoplot(lc, texclude = c(2, 1)), "`texclude` must be two")
})

test_that("events can be selected", {
  lc <- events_lc()
  expect_setequal(layer_data(autoplot(lc, which.events = 2), 2)$x,
                  c(1.5, 1.2, 2.5))
  # each case's first event
  expect_setequal(layer_data(autoplot(lc, n.events = 1), 2)$x,
                  c(0.5, 1.2, 0.1, 2.5))
  expect_setequal(layer_data(autoplot(lc, which.events = 1, n.events = 1), 2)$x,
                  c(0.5, 0.1))
  # the legend shows only the events drawn, each with its own shape
  p <- autoplot(lc, which.events = 2)
  expect_identical(ggplot2::get_guide_data(p, "shape")$.label, "2")
  expect_identical(unique(layer_data(p, 2)$shape), 2L)
  expect_error(autoplot(lc, which.events = 5), "`which.events` must be")
  expect_error(autoplot(lc, n.events = 0), "`n.events` must be")
})

test_that("times can be aligned at a state or an event", {
  lc <- events_lc(tLabels = c("A", "B", "C"))
  p <- autoplot(lc, which.state = 3, nth.state = 1)
  # state 3 starts at 2, 0, and 1 for cases 1, 3, and 4; case 2 never has
  # it and ends at time 0
  expect_equal(starts_by_id(p), c(-2, -3, 0, -1))
  # events move with their case: case 1's events at 0.5 and 1.5
  e <- layer_data(p, 2)
  expect_true(all(c(-1.5, -0.5) %in% e$x))
  # aligned times are not labelled with tLabels
  expect_false(any(c("A", "B", "C") %in%
                     ggplot2::get_guide_data(p, "x")$.label))
  # at the first event 2: cases 1, 2, and 4 at 1.5, 1.2, and 2.5
  p <- autoplot(lc, which.event = 2, nth.event = 1)
  expect_equal(starts_by_id(p), c(-1.5, -1.2, -3, -2.5))
  expect_error(autoplot(lc, which.state = 1, which.event = 1),
               "exactly one of `which.state` or `which.event`")
})

test_that("the options match the data plot() draws", {
  set.seed(7)
  lc <- longCat(example3, times = 0:6,
                events = matrix(sample(1:3, 200, TRUE), 100, 2),
                event.times = matrix(runif(200, 0, 6), 100, 2))
  s <- sorter(lc)
  pd <- longCatEDA:::prepare_plot_data(
    s, TRUE, which.events = c(1, 3), n.events = 1, texclude = c(1, 5),
    align = list(which.state = 2, nth.state = 1, not.state = FALSE,
                 which.event = NULL, nth.event = NULL, not.event = FALSE))
  p <- autoplot(lc, which.events = c(1, 3), n.events = 1, texclude = c(1, 5),
                which.state = 2, nth.state = 1)
  starts <- pd$times[, 1:6][!is.na(pd$y) & pd$times[, 1:6] < pd$times[, -1]]
  expect_setequal(layer_data(p, 1)$x, starts)
  expect_setequal(layer_data(p, 2)$x, stats::na.omit(c(pd$event.times)))
})

test_that("line types distinguish the states", {
  lc <- small_lc()
  p <- autoplot(lc, lty = c(1, 2))
  d <- layer_data(p, 1)
  expect_equal(d$linetype, c(1, 2, 1)[as.integer(p$data$state)])
  expect_true(all(layer_data(autoplot(lc), 1)$linetype == 1))
  # bands have no line types
  expect_identical(geoms(autoplot(small_lc(weights = 1:4), lty = 2)),
                   "GeomRect")
})

test_that("longCont lines can be jogged", {
  lcont <- longCont(rbind(c(1, 2, 3), c(4, 5, 6)))
  set.seed(1)
  d <- autoplot(lcont, jog = TRUE)$data
  shift <- d$value - c(1, 2, 3, 4, 5, 6)
  # one shift per case, of at most 0.25
  expect_equal(as.vector(tapply(shift, d$id, stats::sd)), c(0, 0))
  expect_true(all(abs(shift) <= 0.25 & shift != 0))
  set.seed(1)
  expect_true(all(abs(autoplot(lcont, jog = 2)$data$value -
                        c(1, 2, 3, 4, 5, 6)) <= 2))
  expect_identical(autoplot(lcont)$data$value, c(1, 2, 3, 4, 5, 6))
  expect_error(autoplot(lcont, jog = -1), "`jog` must be")
})
