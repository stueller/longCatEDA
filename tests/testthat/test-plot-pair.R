shared_plot_rows <- longCatEDA:::shared_plot_rows

# x ties for cases 1, 2, and 4, which the second variable separates
tie_x <- function(...) longCat(rbind(c(1, 1), c(1, 1), c(2, 2), c(1, 1)), ...)
tie_y <- function(...) longCat(rbind(c(2, 2), c(1, 2), c(1, 1), c(1, 1)), ...)

# Record the objects and arguments that plot_pair() draws.
capture_panels <- function(env = parent.frame()) {
  panels <- new.env()
  panels$calls <- list()
  local_mocked_bindings(draw_panel = function(lc, args) {
    panels$calls[[length(panels$calls) + 1L]] <- list(x = lc, args = args)
    invisible(length(panels$calls))
  }, .env = env)
  panels
}

# ---- shared order -------------------------------------------------------------

test_that("order = 'x' uses the plot order of x", {
  shared <- shared_plot_rows(small_lc(), small_lc(), "x")
  # ascending sequences: (1,2,3), (1,3,2), (2,2,1), (3,1)
  expect_identical(shared$rows, c(1L, 4L, 2L, 3L))
  s <- sorter(small_lc(), ascending = FALSE)
  expect_identical(shared_plot_rows(s, small_lc(), "x")$rows,
                   s$order.y.sorted[, 4])
})

test_that("order = 'joint' breaks ties of x with y", {
  expect_identical(shared_plot_rows(tie_x(), tie_y(), "x")$rows,
                   c(1L, 2L, 4L, 3L))
  expect_identical(shared_plot_rows(tie_x(), tie_y(), "joint")$rows,
                   c(4L, 2L, 1L, 3L))
  s <- sorter(tie_x(), ascending = FALSE)
  expect_identical(shared_plot_rows(s, tie_y(), "joint")$rows,
                   c(3L, 1L, 2L, 4L))
  # within the groups of x, with its blank rows
  g <- sorter(tie_x(), group = c(1, 1, 2, 2), ggap = 0.25)
  expect_identical(shared_plot_rows(g, tie_y(), "joint")$rows,
                   c(2L, 1L, NA, 4L, 3L, NA))
})

test_that("cases with no observed states of y sort last in both directions", {
  y <- longCat(rbind(c(NA, NA), c(1, 2), c(1, 1), c(2, 2)))
  # among the ties: (1,2), (2,2), then the case with no observed states
  expect_identical(shared_plot_rows(tie_x(), y, "joint")$rows,
                   c(2L, 4L, 1L, 3L))
  s <- sorter(tie_x(), ascending = FALSE)
  expect_identical(shared_plot_rows(s, y, "joint")$rows, c(3L, 4L, 2L, 1L))
})

# ---- plot(x, y) ---------------------------------------------------------------

test_that("plot(x, y) draws both panels with the same rows", {
  lc1 <- tie_x()
  lc2 <- sorter(tie_y())   # its own sort is replaced by the shared one
  panels <- capture_panels()
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  out <- plot(lc1, lc2, order = "joint", lwd = 2)
  expect_length(panels$calls, 2L)
  first <- panels$calls[[1]]
  second <- panels$calls[[2]]
  expect_identical(first$x$order.y.sorted[, 4], c(4L, 2L, 1L, 3L))
  expect_identical(second$x$order.y.sorted[, 4], c(4L, 2L, 1L, 3L))
  expect_identical(second$x$y, lc2$y)
  # titles from the object names; no y-axis label on the second panel
  expect_identical(first$args$main, "lc1")
  expect_identical(second$args$main, "lc2")
  expect_null(first$args$ylab)
  expect_identical(second$args$ylab, "")
  expect_identical(first$args$lwd, 2)
  expect_identical(out, list(x = 1L, y = 2L))
})

test_that("y_args override the arguments of the second panel", {
  panels <- capture_panels()
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  plot(tie_x(), tie_y(), main = "Both", colScheme = "gray",
       y_args = list(colScheme = "topo", ylab = "Cases"))
  expect_identical(panels$calls[[1]]$args$main, "Both")
  expect_identical(panels$calls[[2]]$args$main, "Both")
  expect_identical(panels$calls[[1]]$args$colScheme, "gray")
  expect_identical(panels$calls[[2]]$args$colScheme, "topo")
  expect_identical(panels$calls[[2]]$args$ylab, "Cases")
})

test_that("the groups and weights of x are used for both panels", {
  panels <- capture_panels()
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  g <- sorter(tie_x(weights = c(1, 2, 3, 4)), group = c("a", "a", "b", "b"),
              ggap = 0.25)
  plot(g, tie_y(weights = c(9, 9, 9, 9)))
  second <- panels$calls[[2]]$x
  expect_identical(second$order.y.sorted[, 4], g$order.y.sorted[, 4])
  expect_identical(second$group.sorted, g$group.sorted)
  expect_identical(second$groupLabels, c("a", "b"))
  expect_identical(second$weights, c(1, 2, 3, 4))
})

test_that("with sort = FALSE both panels keep the original order", {
  panels <- capture_panels()
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  plot(tie_x(), sorter(tie_y()), sort = FALSE)
  expect_false(panels$calls[[1]]$x$sorted)
  expect_false(panels$calls[[2]]$x$sorted)
})

test_that("plot(x, y) sets and restores a two-panel layout", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_silent(cols <- plot(small_lc(), small_lc(), legendBuffer = 0.2))
  expect_named(cols, c("x", "y"))
  expect_identical(cols$x, colChoose("heat", 3))
  expect_identical(graphics::par("mfrow"), c(1L, 1L))
  # an existing multi-figure layout is left alone
  op <- graphics::par(mfrow = c(2, 2))
  plot(small_lc(), small_lc())
  expect_identical(graphics::par("mfrow"), c(2L, 2L))
  graphics::par(op)
  expect_silent(plot(sorter(small_lc(), group = c(1, 2, 1, 2)), small_lc(),
                     order = "joint", groupBuffer = 0.1))
})

test_that("plot(x, y) checks y", {
  lc <- small_lc()
  expect_error(plot(lc, 1), "`y` must be NULL or a longCat object")
  expect_error(plot(lc, longCat(small_y()[1:3, ])), "must have the same cases")
  expect_error(plot(lc, small_lc(id = 4:1)), "must have the same cases")
  expect_error(plot(lc, lc, y_args = list(1)), "`y_args` must be a named list")
  expect_error(plot(lc, lc, order = "y"), "should be one of")
})

test_that("longCatPlot() still takes xlab as its second argument", {
  captured <- NULL
  local_mocked_bindings(plot.longCat = function(x, y = NULL, xlab = "Days", ...) {
    captured <<- list(y = y, xlab = xlab)
  })
  suppressWarnings(longCatPlot(small_lc(), "Months"))
  expect_identical(captured, list(y = NULL, xlab = "Months"))
})

test_that("panels get titles only from plain object names", {
  panels <- capture_panels()
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  lc1 <- tie_x()
  plot(lc1, sorter(tie_y()))
  expect_identical(panels$calls[[1]]$args$main, "lc1")
  expect_null(panels$calls[[2]]$args$main)
})
