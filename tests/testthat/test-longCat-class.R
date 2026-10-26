# ---- construction ---------------------------------------------------------

test_that("longCat() returns a valid S3 object with every field", {
  lc <- small_lc()
  expect_s3_class(lc, "longCat")
  expect_false(isS4(lc))
  expect_identical(names(lc), longCatEDA:::longcat_fields)
  expect_identical(validate_longCat(lc), lc)
  expect_invisible(validate_longCat(lc))
})

test_that("sorter() output is valid, with and without groups and events", {
  ev <- small_events()
  lc <- small_lc(events = ev$events, event.times = ev$event.times)
  expect_silent(validate_longCat(sorter(lc)))
  expect_silent(validate_longCat(sorter(lc, group = c(1, 2, 1, 2))))
  expect_silent(validate_longCat(sorter(longCat(example3))))
})

test_that("new_longCat() fills absent fields and rejects unknown ones", {
  lc <- small_lc()
  partial <- unclass(lc)[c("y", "dim", "times", "Labels", "factors", "IndTime",
                           "nfactors", "sorted", "order.y")]
  rebuilt <- longCatEDA:::new_longCat(partial)
  expect_identical(names(rebuilt), longCatEDA:::longcat_fields)
  expect_null(rebuilt$events)

  expect_error(longCatEDA:::new_longCat(c(partial, list(eventLables = "a"))),
               "Unknown longCat field\\(s\\): eventLables")
  expect_error(longCatEDA:::new_longCat(list(1)), "named list")
})

test_that("longCat() stores y as a numeric matrix for character input", {
  y_chr <- matrix(c("0", "1", "1", NA, "0", "1"), nrow = 3)
  lc <- longCat(y_chr)
  expect_true(is.numeric(lc$y))
  expect_equal(lc$y, matrix(c(1, 2, 2, NA, 1, 2), nrow = 3))
})

test_that("longCat() errors when tLabels do not match the columns of y", {
  expect_error(small_lc(tLabels = c("a", "b")), "one label per column")
  expect_silent(small_lc(tLabels = c("a", "b", "c")))
})

# ---- validator: class and structure ---------------------------------------

test_that("validate_longCat() rejects non-longCat input, naming the argument", {
  expect_error(validate_longCat(list()), "`lc` must be a longCat object")
  expect_error(validate_longCat(matrix(1), arg = "obj"),
               "`obj` must be a longCat object")
})

test_that("validate_longCat() reports missing and unknown fields", {
  lc <- small_lc()
  lc$events <- NULL  # `$<-` with NULL removes the element
  expect_invalid(lc, "missing field\\(s\\): events")

  lc <- small_lc()
  lc$eventLables <- "typo"
  expect_invalid(lc, "unknown field\\(s\\): eventLables")
})

test_that("validate_longCat() collects every problem in one error", {
  lc <- small_lc()
  lc$IndTime <- NA
  lc$dim <- c(1L, 1L)
  err <- expect_error(validate_longCat(lc))
  expect_match(conditionMessage(err), "`dim` must equal")
  expect_match(conditionMessage(err), "`IndTime` must be TRUE or FALSE")
})

# ---- validator: field contents --------------------------------------------

test_that("validate_longCat() checks y, factors, and nfactors", {
  lc <- small_lc()
  lc$y <- as.data.frame(lc$y)
  expect_invalid(lc, "`y` must be a numeric matrix")

  lc <- small_lc()
  lc$y[1, 1] <- 7
  expect_invalid(lc, "integers 1 to `nfactors`")

  lc <- small_lc()
  lc$nfactors <- 0L
  expect_invalid(lc, "`nfactors` must be a single positive integer")

  lc <- small_lc()
  lc$factors <- 1:2
  expect_invalid(lc, "`factors` must be numeric of length `nfactors`")
})

test_that("validate_longCat() checks times", {
  lc <- small_lc()
  lc$times <- lc$times[, -1]
  expect_invalid(lc, "c\\(nrow\\(y\\), ncol\\(y\\) \\+ 1\\)")

  lc <- small_lc()
  lc$times[2, ] <- rev(lc$times[2, ])
  expect_invalid(lc, "non-decreasing")

  lc <- small_lc()
  lc$times[2, 2] <- NA  # NAs are ignored in the monotonicity check
  expect_silent(validate_longCat(lc))
})

test_that("validate_longCat() checks labels, ids, and groups", {
  lc <- small_lc()
  lc$Labels <- character(0)
  expect_invalid(lc, "`Labels` must be a non-empty atomic vector")

  lc <- small_lc()
  lc$tLabels <- c("a", "b")
  expect_invalid(lc, "`tLabels` must be NULL or an atomic vector of length ncol")

  lc <- small_lc()
  lc$order.y <- lc$order.y[-1, ]
  expect_invalid(lc, "`order.y` must be a data frame with nrow\\(y\\) rows")

  lc <- small_lc()
  lc["group"] <- list(1:3)
  expect_invalid(lc, "`group` must be NULL or an atomic vector of length nrow")
})

test_that("validate_longCat() checks events and event times together", {
  ev <- small_events()
  lc <- small_lc(events = ev$events, event.times = ev$event.times)
  expect_silent(validate_longCat(lc))

  bad <- lc
  bad$event.times <- bad$event.times[, 1, drop = FALSE]
  expect_invalid(bad, "same dimension as `events`")

  bad <- lc
  bad$events <- bad$events[-1, ]
  expect_invalid(bad, "`events` must be NULL or a matrix with nrow\\(y\\) rows")

  bad <- small_lc()
  bad["event.times"] <- list(ev$event.times)
  expect_invalid(bad, "`event.times` must be NULL when `events` is NULL")
})

test_that("validate_longCat() checks consistency of sorted fields", {
  lc <- small_lc()
  lc$sorted <- TRUE
  expect_invalid(lc, "sort = FALSE")

  lc <- small_lc()
  lc["y.sorted"] <- list(lc$y)
  expect_invalid(lc, "`sorted` is FALSE but sorted field\\(s\\) are set: y.sorted")

  s <- sorter(small_lc())
  s$times.sorted <- s$times.sorted[-1, ]
  expect_invalid(s, "`times.sorted` must have the same number of rows as `y.sorted`")

  s <- sorter(small_lc())
  s$ascending <- NA
  expect_invalid(s, "`ascending` must be TRUE or FALSE")
})

test_that("functions taking a longCat object validate it on entry", {
  expect_error(sorter(list(y = 1)), "must be a longCat object")
  expect_error(longCatPlot(list(y = 1)), "must be a longCat object")
})

# ---- methods --------------------------------------------------------------

test_that("print.longCat() describes the object and returns it invisibly", {
  lc <- small_lc(Labels = c("Low", "Mid", "High"))
  expect_output(print(lc), "<longCat> 4 cases x 3 time points")
  expect_output(print(lc), "States \\(3\\): Low, Mid, High")
  expect_output(print(lc), "Events: none")
  expect_output(print(lc), "Sorted: no")
  expect_output(vis <- withVisible(print(lc)))
  expect_false(vis$visible)
  expect_identical(vis$value, lc)

  grouped <- sorter(lc, group = c(1, 2, 1, 2))
  expect_output(print(grouped), "Sorted: yes \\(ascending\\), 2 groups")
})

test_that("print.longCat() uses thousands separators for large data", {
  lc <- longCat(matrix(rep(1:2, length.out = 2400), ncol = 2))
  expect_output(print(lc), "1,200 cases")
})

test_that("summary.longCat() counts states, groups, and events", {
  ev <- small_events()
  lc <- small_lc(Labels = c("Low", "Mid", "High"),
                 events = ev$events, event.times = ev$event.times)
  s <- summary(lc)
  expect_s3_class(s, "summary.longCat")
  expect_equal(unname(as.vector(s$states)), c(4, 4, 3, 1))
  expect_identical(names(s$states)[1:3], c("Low", "Mid", "High"))
  expect_null(s$groups)
  expect_equal(sum(s$events), 6)

  sg <- summary(sorter(lc, group = c("a", "b", "a", "a")))
  expect_equal(as.vector(sg$groups), c(3, 1))

  expect_output(print(s), "State frequencies")
  expect_output(print(sg), "Cases per group")
  expect_output(vis <- withVisible(print(s)))
  expect_false(vis$visible)
})
