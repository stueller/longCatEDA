plot_layout <- longCatEDA:::plot_layout
plot_data <- longCatEDA:::plot_data
state_segments <- longCatEDA:::state_segments
select_events <- longCatEDA:::select_events
first_events <- longCatEDA:::first_events
restrict_times <- longCatEDA:::restrict_times
event_points <- longCatEDA:::event_points

plot_events_lc <- function() {
  longCat(rbind(c(1, 2), c(2, 1), c(1, 1)), times = c(0, 1, 2),
          events = rbind(c(1, 2, 3), c(3, NA, 1), c(NA, 2, NA)),
          event.times = rbind(c(0.2, 0.5, 1.5), c(0.1, NA, 1.9), c(NA, 1, NA)),
          eventLabels = c("a", "b", "c"))
}

# ---- issues #2 and #3 -------------------------------------------------------

test_that("the default y-axis label uses thousands separators (issue #2)", {
  expect_identical(longCatEDA:::default_ylab(1234),
                   "Each Line Represents a Participant, N = 1,234")
  expect_identical(longCatEDA:::default_ylab(12),
                   "Each Line Represents a Participant, N = 12")
})

test_that("lty sets a line type per state (issue #3)", {
  captured <- NULL
  local_mocked_bindings(draw_states = function(pd, cols, lwd, lty) {
    captured <<- lty
  })
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  plot(longCat(example3), lty = c(1, 2))
  expect_identical(captured, c(1, 2, 1, 2, 1))
  plot(longCat(example3), lty = "dashed")
  expect_identical(captured, rep("dashed", 5))
})

# ---- data preparation -------------------------------------------------------

test_that("plot_data() uses sorted components when the object is sorted", {
  lc <- small_lc()
  expect_identical(plot_data(lc)$y, lc$y)
  expect_null(plot_data(lc)$group)
  s <- sorter(lc, group = c(1, 2, 1, 2), ggap = 0.5)
  pd <- plot_data(s)
  expect_identical(pd$y, s$y.sorted)
  expect_identical(pd$group, s$group.sorted[, 1])
})

test_that("select_events() keeps which.events and packs each case's events", {
  pd <- plot_data(plot_events_lc())
  out <- select_events(pd, which.events = c(1, 3), n_types = 3)
  expect_equal(unname(out$events), rbind(c(1, 3, NA), c(3, 1, NA), c(NA, NA, NA)))
  expect_identical(unname(out$event.times),
                   rbind(c(0.2, 1.5, NA), c(0.1, 1.9, NA), c(NA, NA, NA)))
  expect_error(select_events(pd, which.events = 4, n_types = 3),
               "event codes between 1 and 3")
})

test_that("first_events() keeps the first n events, for any n", {
  pd <- select_events(plot_data(plot_events_lc()), NULL, n_types = 3)
  expect_equal(unname(first_events(pd, 1)$events[, 1]), c(1, 3, 2))
  expect_true(all(is.na(first_events(pd, 1)$events[, 2:3])))
  # previously n.events equal to the number of event columns failed with
  # "subscript out of bounds"
  expect_identical(first_events(pd, 3)$events, pd$events)
  expect_identical(first_events(pd, 10)$events, pd$events)
  expect_error(first_events(pd, 0), "single positive integer")
})

test_that("restrict_times() cuts intervals and drops events outside", {
  pd <- plot_data(plot_events_lc())
  out <- restrict_times(pd, c(0.5, 1.6))
  expect_identical(unname(out$times[1, ]), c(0.5, 1, 1.6))
  expect_true(is.na(out$event.times[1, 1]))   # 0.2
  expect_identical(out$event.times[1, 3], 1.5)
  expect_true(is.na(out$event.times[2, 3]))   # 1.9
  expect_error(restrict_times(pd, c(2, 1)), "two increasing times")
  expect_error(restrict_times(pd, 1), "two increasing times")
})

# ---- layout -------------------------------------------------------------------

test_that("plot_layout() spans the times, whatever their scale", {
  lc <- longCat(example3, times = seq(0, 0.6, 0.1))
  layout <- plot_layout(plot_data(lc), legendBuffer = 0.12, groupBuffer = 0,
                        with_events = FALSE)
  # previously the x range was built from lo:up, so 0 to 0.6 became 0 to 1
  expect_equal(layout$xlim, c(0, 0.6))
  expect_identical(layout$ylim, c(0, 100 + 12))
})

test_that("plot_layout() leaves room for group labels and counts cases", {
  s <- sorter(longCat(rbind(c(1, 2), c(2, 1), c(1, 1), c(2, 2)),
                      times = c(0, 5, 10)),
              group = c(1, 2, 1, 2), ggap = 0.5)
  layout <- plot_layout(plot_data(s), legendBuffer = 0, groupBuffer = 0.2,
                        with_events = FALSE)
  expect_equal(layout$xlim, c(-2, 10))
  expect_identical(layout$n_groups, 2L)
  expect_identical(layout$n_cases, 4L)  # blank rows are not counted
})

test_that("plot_layout() honours user xlim and ylim", {
  layout <- plot_layout(plot_data(small_lc()), 0.12, 0, FALSE,
                        xlim = c(-1, 5), ylim = c(0, 10))
  expect_identical(layout$xlim, c(-1, 5))
  expect_identical(layout$ylim, c(0, 10))
})

# ---- drawing data -------------------------------------------------------------

test_that("state_segments() gives one segment per observed cell", {
  lc <- longCat(rbind(c(1, NA, 2), c(2, 2, 1)), times = c(0, 1, 3, 4))
  s <- state_segments(plot_data(lc))
  expect_identical(nrow(s), 5L)
  expect_identical(s$x0, c(0, 0, 1, 3, 3))
  expect_identical(s$x1, c(1, 1, 3, 4, 4))
  expect_identical(s$y, c(1L, 2L, 2L, 1L, 2L))
  expect_identical(s$state, c(1L, 2L, 2L, 2L, 1L))
})

test_that("event_points() maps event.pch and event.col by event code", {
  pd <- select_events(plot_data(plot_events_lc()), c(1, 3), n_types = 3)
  p <- event_points(pd, event.pch = c(16, 17, 18), event.col = c("red", "green", "blue"),
                    n_types = 3)
  # code 3 gets the 3rd character even though code 2 is not shown; the old
  # code mapped characters to the codes present, giving code 3 the 2nd
  expect_identical(p$pch[p$code == 3], c(18, 18))
  expect_identical(unique(p$col[p$code == 1]), "red")
  # default: the code itself
  expect_identical(event_points(pd, NULL, 1, 3)$pch, event_points(pd, NULL, 1, 3)$code)
})

# ---- plot() -----------------------------------------------------------------

test_that("plot() accepts ylim and xlim", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  # previously: formal argument "ylim" matched by multiple actual arguments
  expect_silent(plot(longCat(example3), ylim = c(0, 200)))
  expect_equal(graphics::par("usr")[3:4], c(0, 200) + c(-8, 8))
  expect_silent(plot(longCat(example3), xlim = c(0, 10)))
  expect_equal(graphics::par("usr")[1:2], c(0, 10) + c(-0.4, 0.4))
})

test_that("plot() returns the state colors invisibly", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_invisible(cols <- plot(longCat(example3), colScheme = "gray"))
  expect_identical(cols, colChoose("gray", 5))
  expect_identical(plot(longCat(example3), cols = rainbow(6)),
                   rainbow(6))
})

test_that("plot() checks its arguments", {
  lc <- longCat(example3)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_error(plot(lc, cols = c("red", "blue")),
               "one color per state \\(5\\), not 2")
  expect_error(plot(lc, legendBuffer = 2), "`legendBuffer` must be")
  expect_error(plot(lc, groupBuffer = -1), "`groupBuffer` must be")
  expect_error(plot(lc, sort = NA), "`sort` must be TRUE or FALSE")
})

test_that("plot() handles events, texclude with tLabels, and alignment", {
  lc <- plot_events_lc()
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_silent(plot(lc, which.events = c(1, 3), n.events = 3,
                     event.pch = c(16, 17, 18)))
  expect_silent(plot(lc, plot.events = FALSE))
  expect_silent(plot(longCat(example3, tLabels = paste("T", 1:6)),
                     texclude = c(1, 4)))
  expect_silent(plot(lc, which.event = 3, nth.event = 1))
  expect_silent(plot(lc, sort = FALSE))
})

test_that("group labels are matched to groups by code", {
  group_label_positions <- longCatEDA:::group_label_positions
  pos <- group_label_positions(c(1, 1, NA, 2, NA), c("A", "B"))
  expect_identical(pos$center, c(1.5, 4))
  expect_identical(pos$label, c("A", "B"))
  # a subset without group 2: group 3 keeps its own label
  pos <- group_label_positions(c(1, NA, 3, 3, NA), c("A", "B", "C"))
  expect_identical(pos$label, c("A", "C"))
  expect_identical(pos$center, c(1, 3.5))
})
