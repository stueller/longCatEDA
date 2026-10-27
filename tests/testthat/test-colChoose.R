all_schemes <- names(longCatEDA:::longcat_color_schemes)

is_color <- function(x) {
  !inherits(try(grDevices::col2rgb(x), silent = TRUE), "try-error")
}

test_that("colChoose() returns exactly nfactors valid colors for every scheme", {
  for (scheme in all_schemes) {
    for (n in c(1:12, 20)) {
      for (rev in c(FALSE, TRUE)) {
        cols <- colChoose(scheme, n, rev)
        expect_length(cols, n)
        expect_false(anyNA(cols), label = paste(scheme, n, rev))
        expect_true(is_color(cols), label = paste(scheme, n, rev))
      }
    }
  }
})

test_that("colChoose('gray') no longer returns NA for few states", {
  # previously c("#000000", NA, NA)
  expect_identical(colChoose("gray", 3), gray(c(0, 0.45, 0.9)))
  expect_identical(colChoose("gray", 3, reverse = TRUE), gray(c(0.9, 0.45, 0)))
  expect_identical(colChoose("gray", 7, reverse = TRUE),
                   rev(gray(seq(0, 0.9, length.out = 7))))
})

test_that("colChoose() spreads 9-color schemes over 5 or fewer states", {
  heat9 <- rev(heat.colors(9, alpha = 1))
  expect_identical(colChoose("heat", 3), heat9[c(1, 5, 9)])
  expect_identical(colChoose("heat", 2), heat9[c(1, 5)])
  expect_identical(colChoose("heat", 4), heat9[c(1, 3, 5, 7)])
  expect_identical(colChoose("heat", 5), heat9[c(1, 3, 5, 7, 9)])
  expect_identical(colChoose("heat", 7), heat9[1:7])
  expect_identical(colChoose("heat", 5, reverse = TRUE),
                   rev(heat9)[c(1, 3, 5, 7, 9)])
})

test_that("colChoose(0) uses its colors in order without spreading", {
  expect_identical(colChoose(0, 3), c("1", "2", "3"))
  expect_identical(colChoose(0, 3, reverse = TRUE), c("darkgreen", "8", "7"))
})

test_that("colChoose() extends schemes to distinct colors beyond 9 states", {
  for (scheme in all_schemes) {
    expect_length(unique(colChoose(scheme, 12)), 12)
  }
  # fixed schemes keep their end colors when interpolated
  cols <- colChoose("oldheat", 15)
  expect_identical(toupper(cols[c(1, 15)]),
                   toupper(grDevices::rgb(t(grDevices::col2rgb(c("purple4", "red4"))),
                                          maxColorValue = 255)))
  expect_identical(colChoose("rainbow", 12), rainbow(12))
})

test_that("colChoose() accepts numbered schemes as numbers or strings", {
  expect_identical(colChoose(2, 5), colChoose("2", 5))
  expect_identical(colChoose(3L, 9), colChoose("3", 9))
})

test_that("colChoose() defaults to the heat scheme", {
  expect_identical(colChoose(nfactors = 4), colChoose("heat", 4))
})

test_that("colChoose() validates its arguments", {
  # previously: "object 'cols' not found"
  expect_error(colChoose("blue", 3), "`colScheme` must be one of: \"heat\"")
  expect_error(colChoose(4, 3), "`colScheme` must be one of")
  expect_error(colChoose(c("heat", "gray"), 3), "`colScheme` must be one of")
  expect_error(colChoose("heat", 0), "`nfactors` must be a single positive integer")
  expect_error(colChoose("heat", 2.5), "`nfactors` must be a single positive integer")
  expect_error(colChoose("heat", 3, reverse = NA), "`reverse` must be TRUE or FALSE")
})

test_that("plot() gives every state a color when there are more than 9", {
  y <- matrix(rep(1:12, each = 2), ncol = 2)
  expect_warning(lc <- longCat(y), "12 distinct states")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  cols <- plot(lc)
  expect_length(cols, 12)
  expect_false(anyNA(cols))
})
