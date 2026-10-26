test_that("longCatPlot() is deprecated in favour of plot()", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  lc <- longCat(example3)
  expect_warning(cols <- longCatPlot(lc, colScheme = "gray"),
                 "longCatPlot\\(\\) is deprecated; use plot\\(\\) instead")
  expect_identical(cols, plot(lc, colScheme = "gray"))
  expect_error(suppressWarnings(longCatPlot(list())), "must be a longCat object")
})

test_that("longContPlot() is deprecated in favour of plot(longCont())", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  times <- c(1, 100, 200, 300, 400, 500)
  expect_warning(longContPlot(example2cont, times, xlim = c(0, 600)),
                 "use plot\\(longCont\\(y, times\\)\\) instead")
  old_usr <- graphics::par("usr")
  plot(longCont(example2cont, times), xlim = c(0, 600))
  expect_identical(old_usr, graphics::par("usr"))
})
