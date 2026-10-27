plot_data <- longCatEDA:::plot_data
row_geometry <- longCatEDA:::row_geometry
state_segments <- longCatEDA:::state_segments
event_points <- longCatEDA:::event_points
group_label_positions <- longCatEDA:::group_label_positions

# ---- longCat(weights = ) --------------------------------------------------------

test_that("longCat() stores case weights", {
  lc <- small_lc(weights = 1:4)
  expect_identical(lc$weights, c(1, 2, 3, 4))
  expect_null(small_lc()$weights)
  expect_output(print(lc), "Weights: total 10 \\(1 to 4\\)")
  expect_output(print(small_lc(weights = c(0.5, 1, 1, 1234.5))),
                "total 1,237 \\(0.5 to 1,234.5\\)")
})

test_that("longCat() and validate_longCat() check weights", {
  for (bad in list(1:3, c(1, NA, 1, 1), c(1, -1, 1, 1), rep(0, 4),
                   c(1, Inf, 1, 1), letters[1:4], matrix(1, 4, 1))) {
    expect_error(small_lc(weights = bad), "`weights` must be one finite")
  }
  lc <- small_lc(weights = 1:4)
  lc$weights <- 1:3
  expect_invalid(lc, "`weights` must be NULL or one finite")
})

test_that("weights follow the cases through sorter() and [", {
  s <- sorter(small_lc(weights = c(1, 2, 3, 4)), group = c(1, 2, 1, 2),
              ggap = 0.25)
  expect_identical(s$weights, c(1, 2, 3, 4))
  # plot rows: cases 1, 3, gap, 4, 2, gap
  expect_identical(plot_data(s)$weights, c(1, 3, NA, 4, 2, NA))
  u <- s[c(4, 1), ]
  expect_identical(u$weights, c(4, 1))
  expect_identical(plot_data(u)$weights, c(1, NA, 4, NA))
  expect_null(plot_data(sorter(small_lc()))$weights)
})

# ---- weighted plot geometry -------------------------------------------------------

test_that("row_geometry() gives bands proportional to the weights", {
  g <- row_geometry(3, NULL)
  expect_identical(g$row_center, 1:3)
  expect_null(g$row_height)
  # equal weights: the same layout as without weights
  g <- row_geometry(3, c(5, 5, 5))
  expect_equal(g$row_center, c(1, 2, 3))
  expect_equal(g$row_height, c(1, 1, 1))
  # mean weight 2: heights 0.5 and 1.5, stacked from 0.5
  g <- row_geometry(2, c(1, 3))
  expect_equal(g$row_height, c(0.5, 1.5))
  expect_equal(g$row_center, c(0.75, 1.75))
  # blank rows have the mean height
  g <- row_geometry(4, c(1, 3, NA, 2))
  expect_equal(g$row_height, c(0.5, 1.5, 1, 1))
  expect_equal(g$row_center, c(0.75, 1.75, 3, 4))
})

test_that("weighted states are drawn as bands", {
  lc <- longCat(rbind(c(1, 2), c(2, 1)), times = c(0, 1, 2), weights = c(1, 3))
  s <- state_segments(plot_data(lc))
  expect_equal(s$y, c(0.75, 1.75, 0.75, 1.75))
  expect_equal(s$half, c(0.25, 0.75, 0.25, 0.75))
  expect_null(state_segments(plot_data(longCat(rbind(c(1, 2)))))$half)
})

test_that("events and group labels are placed on the bands", {
  ev <- small_events()
  lc <- longCat(small_y(), events = ev$events, event.times = ev$event.times,
                weights = c(1, 1, 1, 5))
  p <- event_points(plot_data(lc), NULL, 1, 2)
  # mean weight 2: heights 0.5, 0.5, 0.5, 2.5; case 4 spans 2 to 4.5
  expect_equal(p$y[p$x == 2.5], 3.25)
  expect_equal(unique(p$y[p$x %in% c(0.5, 1.5)]), 0.75)

  pos <- group_label_positions(c(1, 1, NA, 2), c("A", "B"),
                               row_center = c(0.75, 1.75, 3, 4),
                               row_height = c(0.5, 1.5, 1, 1))
  # group A spans 0.5 to 2.5, group B 3.5 to 4.5
  expect_equal(pos$center, c(1.5, 4))
})

test_that("weighted plots use the unweighted layout and label the total", {
  expect_identical(longCatEDA:::default_ylab(3, 1234.5),
                   "Each Band Represents a Participant, N = 3 (Weighted N = 1,234.5)")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  plot(longCat(example3))
  usr <- graphics::par("usr")
  set.seed(1)
  w <- stats::rexp(nrow(example3))
  expect_silent(plot(longCat(example3, weights = w)))
  expect_identical(graphics::par("usr"), usr)
  s <- sorter(longCat(example3, weights = w), group = rep(1:2, 50))
  expect_silent(plot(s, groupBuffer = 0.1))
  expect_silent(plot(longCat(example3, weights = w), sort = FALSE))
})

# ---- long format ------------------------------------------------------------------

test_that("as.data.frame() and as.longCat() carry the weights", {
  lc <- small_lc(weights = c(1, 2, 3, 4))
  d <- as.data.frame(lc)
  expect_named(d, c("id", "weight", "column", "start", "stop", "state"))
  expect_identical(d$weight, rep(c(1, 2, 3, 4), each = 3))
  d_s <- as.data.frame(sorter(lc))
  expect_identical(unique(d_s[c("id", "weight")]$weight), c(1, 4, 2, 3))
  expect_identical(as.data.frame(lc, what = "events")$weight, numeric(0))

  lc2 <- as.longCat(d, time = "start", stop = "stop", weight = "weight")
  expect_identical(lc2$weights, lc$weights)
  d$weight[2] <- 9
  expect_error(as.longCat(d, time = "start", stop = "stop", weight = "weight"),
               "same in every row of a case")
  expect_error(as.longCat(d, time = "start", stop = "stop", weight = "w"),
               "no column \"w\"")
})

# ---- expand_weights() -------------------------------------------------------------

weighted_data <- function() {
  data.frame(t1 = factor(c("low", "high", "low"), c("low", "high")),
             t2 = factor(c("high", "high", "low"), c("low", "high")),
             w = c(150, 300, 75))
}

test_that("expand_weights() repeats rows by rescaled weights, keeping factors", {
  d <- weighted_data()
  out <- expand_weights(d, "w")
  # weights / 75 = 2, 4, 1
  expect_identical(out, d[c(1, 1, 2, 2, 2, 2, 3), ], ignore_attr = "row.names")
  expect_identical(rownames(out), as.character(1:7))
  expect_identical(levels(out$t1), c("low", "high"))
  # a weight vector and a matrix
  # divided by 0.6: 2.33, 1, 3.33, rounded to 2, 1, 3
  expect_identical(nrow(expand_weights(d, c(1.4, 0.6, 2))), 6L)
  m <- matrix(1:6, 3)
  expect_identical(expand_weights(m, c(2, 1, 1)), m[c(1, 1, 2, 3), ])
})

test_that("expand_weights() without rescaling rounds the weights", {
  d <- weighted_data()
  expect_warning(out <- expand_weights(d, c(2.4, 0.4, 1), rescale = FALSE),
                 "1 row\\(s\\) with weights that round to 0")
  expect_identical(nrow(out), 3L)
  expect_identical(expand_weights(d, c(2, 0, 1), rescale = FALSE)$w,
                   c(150, 150, 75))
})

test_that("duplicated rows and weights give the same weighted states", {
  d <- weighted_data()
  dup <- as.data.frame(longCat(expand_weights(d, "w")[, 1:2]))
  wtd <- as.data.frame(longCat(d[, 1:2], weights = d$w / 75))
  expect_equal(c(table(dup$state)),
               c(tapply(wtd$weight, wtd$state, sum)))
})

test_that("expand_weights() checks its arguments", {
  d <- weighted_data()
  expect_error(expand_weights(1:3, 1:3), "data frame or matrix")
  expect_error(expand_weights(d, "weight"), "no column \"weight\"")
  expect_error(expand_weights(d, c(1, -1, 1)), "row of `data` \\(3\\)")
  expect_error(expand_weights(d, "w", rescale = NA), "`rescale` must be")
  expect_error(expand_weights(d, "w", max_rows = 5),
               "would create 7 rows, more than `max_rows` \\(5\\)")
  expect_error(expand_weights(d, "w", max_rows = NA), "`max_rows` must be")
})
