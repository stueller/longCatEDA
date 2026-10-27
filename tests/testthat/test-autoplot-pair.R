skip_if_not_installed("ggplot2")

autoplot <- ggplot2::autoplot

# x ties for cases 1, 2, and 4, which the second variable separates
tie_x <- function(...) longCat(rbind(c(1, 1), c(1, 1), c(2, 2), c(1, 1)), ...)
tie_y <- function(...) longCat(rbind(c(2, 2), c(1, 2), c(1, 1), c(1, 1)), ...)

# Each case's plot row in each panel, as a list by panel title.
rows_by_panel <- function(p) {
  d <- unique(p$data[c(".panel", "id", "row")])
  lapply(split(d, d$.panel), function(x) x$row[order(x$id)])
}

test_that("two variables are facets of one plot with the same rows", {
  lc1 <- tie_x()
  lc2 <- tie_y()
  p <- autoplot(lc1, lc2)
  expect_s3_class(p, "ggplot")
  rows <- rows_by_panel(p)
  expect_named(rows, c("lc1", "lc2"))
  expect_identical(rows$lc1, rows$lc2)
  # the order of x: (1,1) cases 1, 2, 4 in their order, then case 3
  expect_identical(rows$lc1, c(1L, 2L, 4L, 3L))
  expect_identical(levels(p$data$.panel), c("lc1", "lc2"))
  expect_s3_class(p$facet, "FacetGrid")
  # the legend lists each panel's states, prefixed with its title
  expect_identical(ggplot2::get_guide_data(p, "colour")$.label,
                   c("lc1: 1", "lc1: 2", "lc2: 1", "lc2: 2"))
})

test_that("order = 'joint' sorts ties of x on y, as plot() does", {
  lc1 <- tie_x()
  lc2 <- tie_y()
  p <- autoplot(lc1, lc2, order = "joint")
  shared <- longCatEDA:::shared_plot_rows(lc1, lc2, "joint")$rows
  expect_identical(rows_by_panel(p)$lc1, match(1:4, shared))
  expect_identical(rows_by_panel(p)$lc2, match(1:4, shared))
  expect_error(autoplot(lc1, lc2, order = "y"), "should be one of")
})

test_that("each panel has its own colors; y_args set the second panel", {
  lc1 <- small_lc()
  lc2 <- small_lc()
  p <- autoplot(lc1, lc2, colScheme = "gray",
                y_args = list(colScheme = "topo", reverse = TRUE))
  d <- ggplot2::layer_data(p, 1)
  first <- p$data$.panel == "lc1"
  expect_identical(d$colour[first],
                   colChoose("gray", 3)[as.integer(p$data$state[first])])
  expect_identical(d$colour[!first],
                   colChoose("topo", 3, reverse = TRUE)[as.integer(p$data$state[!first])])
  # line types for the second panel only
  d <- ggplot2::layer_data(autoplot(lc1, lc2, y_args = list(lty = 2)), 1)
  expect_identical(sort(unique(d$linetype[!first])), 2)
  expect_identical(sort(unique(d$linetype[first])), 1)
})

test_that("events and times can differ between the panels", {
  ev <- small_events()
  lc1 <- longCat(small_y(), events = ev$events, event.times = ev$event.times)
  lc2 <- small_lc()
  p <- autoplot(lc1, lc2)
  e <- ggplot2::layer_data(p, 2)
  expect_identical(nrow(e), 6L)
  # no events in the second panel; the second panel cut to times 1 to 2
  lc2 <- lc1
  p <- autoplot(lc1, lc2, y_args = list(events = FALSE, texclude = c(1, 2)))
  expect_identical(nrow(ggplot2::layer_data(p, 2)), 6L)
  second <- p$data[p$data$.panel == "lc2", ]
  expect_true(all(second$start >= 1 & second$stop <= 2))
  # aligned in the second panel only
  p <- autoplot(lc1, lc2, y_args = list(which.state = 3, nth.state = 1))
  second <- p$data[p$data$.panel == "lc2", ]
  expect_true(any(second$start < 0))
  expect_false(any(p$data$start[p$data$.panel == "lc1"] < 0))
})

test_that("the groups and weights of x are used for both panels", {
  lc1 <- sorter(tie_x(weights = c(1, 2, 3, 4)), group = c("a", "a", "b", "b"),
                ggap = 0.25)
  lc2 <- tie_y(weights = c(9, 9, 9, 9))
  p <- autoplot(lc1, lc2)
  expect_s3_class(p$layers[[1]]$geom, "GeomRect")
  bands <- unique(p$data[c(".panel", "id", "ymin", "ymax", "group")])
  first <- bands[bands$.panel == "lc1", ]
  second <- bands[bands$.panel == "lc2", ]
  expect_equal(second[order(second$id), c("ymin", "ymax")],
               first[order(first$id), c("ymin", "ymax")],
               ignore_attr = TRUE)
  expect_identical(as.character(second$group[order(second$id)]),
                   c("a", "a", "b", "b"))
  expect_identical(p$labels$y,
                   "Each Band Represents a Participant, N = 4 (Weighted N = 10)")
})

test_that("with sort = FALSE both panels keep the original order", {
  p <- autoplot(tie_x(), sorter(tie_y()), sort = FALSE,
                titles = c("a", "b"))
  rows <- rows_by_panel(p)
  expect_identical(rows$a, 1:4)
  expect_identical(rows$b, 1:4)
})

test_that("titles come from names, `titles`, or 'x' and 'y'", {
  lc1 <- tie_x()
  expect_identical(levels(autoplot(lc1, tie_y())$data$.panel), c("lc1", "y"))
  expect_identical(levels(autoplot(tie_x(), tie_y())$data$.panel), c("x", "y"))
  expect_identical(levels(autoplot(lc1, lc1)$data$.panel), c("lc1", "lc1 1"))
  p <- autoplot(lc1, tie_y(), titles = c("Housing", "Drug use"))
  expect_identical(levels(p$data$.panel), c("Housing", "Drug use"))
  expect_error(autoplot(lc1, lc1, titles = "one"), "`titles` must be two")
})

test_that("tLabels label the x-axis only when both panels share them", {
  lc1 <- small_lc(tLabels = c("A", "B", "C"))
  p <- autoplot(lc1, lc1)
  expect_identical(ggplot2::get_guide_data(p, "x", panel = 1)$.label,
                   c("A", "B", "C"))
  p <- autoplot(lc1, small_lc())
  expect_false("A" %in% ggplot2::get_guide_data(p, "x", panel = 1)$.label)
})

test_that("autoplot(x, y) checks y and y_args", {
  lc <- small_lc()
  expect_error(autoplot(lc, 1), "`y` must be NULL or a longCat object")
  expect_error(autoplot(lc, longCat(small_y()[1:3, ])), "must have the same cases")
  expect_error(autoplot(lc, lc, y_args = list(1)), "`y_args` must be a named list")
  expect_error(autoplot(lc, lc, y_args = list(xlab = "Weeks")),
               "`y_args` can only set options of each panel .* not xlab")
  expect_error(autoplot(lc, lc, y_args = list(events = NA)),
               "`events` must be TRUE or FALSE")
})
