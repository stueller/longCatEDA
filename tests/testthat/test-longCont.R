jog_amount <- longCatEDA:::jog_amount
jog_cases <- longCatEDA:::jog_cases

cont_y <- function() rbind(c(1, 2, 3), c(2, NA, 1))

# ---- longCont() ---------------------------------------------------------------

test_that("longCont() stores numeric data and observation times", {
  lcont <- longCont(cont_y())
  expect_s3_class(lcont, "longCont")
  expect_named(lcont, c("y", "times", "IndTime", "dim"))
  expect_identical(lcont$y, cont_y())
  expect_identical(lcont$times, rbind(c(1, 2, 3), c(1, 2, 3)))
  expect_false(lcont$IndTime)
  expect_identical(lcont$dim, c(2L, 3L))
  # integer data and data frames are converted
  expect_identical(longCont(as.data.frame(matrix(1:6, 2)))$y,
                   matrix(as.numeric(1:6), 2))
})

test_that("longCont() accepts n or n + 1 times, as a vector or a matrix", {
  expect_identical(longCont(cont_y(), c(0, 10, 20))$times[2, ], c(0, 10, 20))
  # the interval times of longCat(): the end of the last interval is dropped
  expect_identical(longCont(cont_y(), c(0, 10, 20, 30))$times[1, ],
                   c(0, 10, 20))
  tm <- rbind(c(0, 1, 2, 3), c(5, 6, 7, 8))
  lcont <- longCont(cont_y(), tm)
  expect_true(lcont$IndTime)
  expect_identical(lcont$times, tm[, 1:3])
  expect_identical(longCont(cont_y(), tm[, 1:3])$times, tm[, 1:3])
})

test_that("longCont() checks its inputs", {
  expect_error(longCont(1:3), "matrix or data frame")
  expect_error(longCont(matrix(letters[1:4], 2)), "`y` must be numeric")
  expect_error(longCont(cont_y(), 1:2), "length ncol\\(y\\) or ncol\\(y\\) \\+ 1")
  expect_error(longCont(cont_y(), matrix(1:6, 3)), "nrow\\(y\\) rows")
  expect_error(longCont(cont_y(), c("a", "b", "c")), "`times` must be numeric")
  expect_error(longCont(cont_y(), c(3, 2, 1)), "must not decrease")
})

test_that("validate_longCont() reports every problem", {
  lcont <- longCont(cont_y())
  expect_invisible(validate_longCont(lcont))
  expect_error(validate_longCont(unclass(lcont)), "must be a longCont object")
  bad <- lcont
  bad$extra <- 1
  expect_error(validate_longCont(bad), "fields must be exactly")
  bad <- lcont
  bad$times <- bad$times[, -1]
  bad$IndTime <- NA
  expect_error(validate_longCont(bad, "obj"),
               "`obj` is not a valid longCont object.*same dimension.*IndTime")
})

# ---- plot.longCont() ----------------------------------------------------------

test_that("jog_amount() maps jog to a half-width", {
  expect_identical(jog_amount(FALSE), 0)
  expect_identical(jog_amount(TRUE), 0.25)
  expect_identical(jog_amount(0.5), 0.5)
  for (bad in list(-1, 0, "a", c(1, 2), NA, NA_real_)) {
    expect_error(jog_amount(bad), "`jog` must be")
  }
})

test_that("jog_cases() shifts each case by one random amount", {
  y <- matrix(0, 50, 4)
  set.seed(1)
  out <- jog_cases(y, 0.25)
  expect_true(all(apply(out, 1, function(r) length(unique(r)) == 1L)))
  expect_true(all(abs(out) <= 0.25))
  # same draws as the old longContPlot(jog = TRUE)
  set.seed(1)
  expect_identical(out[, 1], stats::runif(50, -0.25, 0.25))
  expect_identical(jog_cases(y, 0), y)
})

test_that("plot() of a longCont object sets the axis ranges", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  usr <- function() graphics::par("usr")
  pad <- function(r) r + c(-1, 1) * 0.04 * diff(r)

  expect_invisible(plot(longCont(cont_y(), c(0, 10, 20))))
  expect_equal(usr(), c(pad(c(0, 20)), pad(c(1, 3))))
  # previously a user xlim was always replaced by the range of the times
  plot(longCont(cont_y(), c(0, 10, 20)), xlim = c(-5, 50), ylim = c(0, 10))
  expect_equal(usr(), c(pad(c(-5, 50)), pad(c(0, 10))))
  # previously ylim was set before jogging, so jogged lines were clipped
  set.seed(3)
  shift <- stats::runif(2, -1, 1)
  set.seed(3)
  plot(longCont(cont_y()), jog = 1)
  expect_equal(usr()[3:4], pad(range(cont_y() + shift, na.rm = TRUE)))
  # individually varying times, including longCat()-style interval times
  # (previously "'x' and 'y' lengths differ")
  tm <- rbind(c(0, 1, 2, 3), c(5, 6, 7, 8))
  expect_silent(plot(longCont(cont_y(), tm)))
  expect_equal(usr()[1:2], pad(c(0, 7)))
})

test_that("plot() of a longCont object checks its input", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_error(longCatEDA:::plot.longCont(list()), "must be a longCont object")
  expect_error(plot(longCont(matrix(NA_real_, 2, 2))), "no non-missing values")
  expect_error(plot(longCont(cont_y()), jog = -1), "`jog` must be")
  expect_silent(plot(longCont(cont_y()[1, , drop = FALSE]), col = "red",
                     lty = 2, lwd = 3, main = "One case"))
})

# ---- methods --------------------------------------------------------------

test_that("print() describes a longCont object", {
  expect_output(print(longCont(cont_y())),
                "<longCont> 2 cases x 3 time points\nTimes: common to all cases\nValues: 1 to 3, 1 missing")
  expect_output(print(longCont(cont_y(), rbind(1:3, 2:4))), "individually varying")
  expect_output(print(longCont(matrix(NA_real_, 1, 2))), "Values: all missing")
})

test_that("summary() gives counts and values by time point", {
  s <- summary(longCont(rbind(c(1, NA), c(3, NA))))
  expect_s3_class(s, "summary.longCont")
  expect_identical(s$columns["T1", ], c(n = 2, min = 1, mean = 2, max = 3))
  expect_identical(s$columns["T2", ], c(n = 0, min = NA, mean = NA, max = NA))
  expect_output(print(s), "Values by time point")
})
