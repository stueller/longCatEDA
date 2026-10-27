skip_if_not_installed("ggplot2")

layer_data <- ggplot2::layer_data

# Segments for states given as a factor with three levels.
state_plot <- function(states = c("1", "2", "3", "2")) {
  d <- data.frame(start = seq_along(states), row = 1,
                  state = factor(states, levels = c("1", "2", "3")))
  ggplot2::ggplot(d, ggplot2::aes(x = start, xend = start + 1, y = row,
                                  yend = row, colour = state)) +
    ggplot2::geom_segment()
}

test_that("states get the colors of colChoose()", {
  d <- layer_data(state_plot() + scale_colour_longcat(), 1)
  expect_identical(d$colour, colChoose("heat", 3)[c(1, 2, 3, 2)])
  d <- layer_data(state_plot() + scale_colour_longcat("topo", reverse = TRUE), 1)
  expect_identical(d$colour, colChoose("topo", 3, reverse = TRUE)[c(1, 2, 3, 2)])
  # the same colors as plot() and autoplot() for a longCat object
  lc <- sorter(longCat(example3))
  d <- as.data.frame(lc, na.rm = TRUE)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = start, xend = stop, y = row,
                                       yend = row, colour = state)) +
    ggplot2::geom_segment() + scale_colour_longcat()
  expect_identical(layer_data(p, 1)$colour,
                   layer_data(ggplot2::autoplot(lc), 1)$colour)
})

test_that("unused states keep their colors unless drop = TRUE", {
  p <- state_plot(c("1", "3", "3"))
  d <- layer_data(p + scale_colour_longcat(), 1)
  expect_identical(d$colour, colChoose("heat", 3)[c(1, 3, 3)])
  expect_identical(ggplot2::get_guide_data(p + scale_colour_longcat(),
                                           "colour")$.label,
                   c("1", "2", "3"))
  d <- layer_data(p + scale_colour_longcat(drop = TRUE), 1)
  expect_identical(d$colour, colChoose("heat", 2)[c(1, 2, 2)])
})

test_that("text states and the fill aesthetic work", {
  d <- data.frame(x = 1:3, y = 1, state = c("jail", "home", "jail"))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, fill = state)) +
    ggplot2::geom_tile() + scale_fill_longcat("gray")
  # "home" before "jail"
  expect_identical(layer_data(p, 1)$fill, colChoose("gray", 2)[c(2, 1, 2)])
})

test_that("further arguments go to discrete_scale()", {
  p <- state_plot() +
    scale_colour_longcat(name = "Status", labels = c("A", "B", "C"))
  guide <- ggplot2::get_guide_data(p, "colour")
  expect_identical(guide$.label, c("A", "B", "C"))
  expect_identical(ggplot2::get_labs(p)$colour, "Status")
  s <- scale_colour_longcat(aesthetics = c("colour", "fill"))
  expect_identical(s$aesthetics, c("colour", "fill"))
  expect_identical(scale_fill_longcat()$aesthetics, "fill")
  expect_identical(scale_color_longcat, scale_colour_longcat)
  expect_identical(scale_colour_longcat()$palette(0), character(0))
})

test_that("the scales check their arguments when created", {
  expect_error(scale_colour_longcat("plaid"), "colScheme")
  expect_error(scale_fill_longcat(reverse = NA), "`reverse` must be TRUE or FALSE")
  expect_error(scale_colour_longcat(drop = "no"), "`drop` must be TRUE or FALSE")
  local_mocked_bindings(has_ggplot2 = function() FALSE)
  expect_error(scale_colour_longcat(), "need the ggplot2 package")
})
