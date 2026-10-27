events_lc <- function(...) {
  ev <- small_events()
  longCat(small_y(), events = ev$events, event.times = ev$event.times, ...)
}

# ---- [.longCat ------------------------------------------------------------------

test_that("x[i, ] keeps the selected cases with their times and events", {
  lc <- events_lc(times = rbind(0:3, 10:13, 0:3, c(0, 5, 6, 7)))
  s <- lc[c(4, 2), ]
  expect_s3_class(s, "longCat")
  expect_identical(s$y, lc$y[c(4, 2), ])
  expect_identical(s$dim, c(2L, 3L))
  expect_identical(s$times, lc$times[c(4, 2), ])
  expect_identical(s$events, lc$events[c(4, 2), ])
  expect_identical(s$event.times, lc$event.times[c(4, 2), ])
  expect_identical(s$order.y[, 1], c(4L, 2L))
  expect_true(s$IndTime)
})

test_that("states and labels are kept when they no longer occur", {
  lc <- small_lc(Labels = c("low", "mid", "high"))
  s1 <- lc[2, ]        # states 2, 2, 1: no "high"
  expect_identical(s1$Labels, lc$Labels)
  expect_identical(s1$nfactors, 3L)
  expect_identical(s1$factors, lc$factors)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_identical(plot(s1), plot(lc))  # the same colors
})

test_that("cases can be selected by logical, negative, or ID", {
  lc <- small_lc(id = c("a", "b", "c", "d"))
  expect_identical(lc[c(TRUE, FALSE, TRUE, FALSE), ]$order.y[, 1], c("a", "c"))
  expect_identical(lc[-1, ]$order.y[, 1], c("b", "c", "d"))
  expect_identical(lc[c("d", "a"), ]$y, lc$y[c(4, 1), ])
  # repeated cases are allowed
  expect_identical(lc[c(1, 1), ]$dim, c(2L, 3L))
  expect_identical(lc[, ], lc)
})

test_that("sorter() records each case's row in order.y", {
  s <- sorter(small_lc(), group = c(1, 2, 1, 2), ggap = 0.25)
  expect_identical(s$order.y[, 4], 1:4)
  # plot rows: group 1 is cases 1, 3; group 2 is cases 4, 2; one blank row
  # after each group
  expect_identical(s$order.y.sorted[, 4], c(1L, 3L, NA, 4L, 2L, NA))
  bad <- s
  bad$order.y[, 4] <- 4:1
  expect_invalid(bad, "order.y\\[, 4\\]` must be the row numbers")
})

test_that("subsets of sorted objects keep their plot order and gaps", {
  s <- sorter(small_lc(), group = c(1, 2, 1, 2), ggap = 0.25)
  u <- s[c(2, 3, 4), ]
  expect_true(u$sorted)
  expect_identical(u$y, s$y[2:4, ])
  expect_identical(u$group, c(2, 1, 2))
  expect_identical(u$order.y[, 4], 1:3)
  # the plot rows of s without case 1
  expect_identical(u$y.sorted, s$y.sorted[-1, ])
  expect_identical(u$order.y.sorted[, 1], c(3L, NA, 4L, 2L, NA))
  expect_identical(u$order.y.sorted[, 4], c(2L, NA, 3L, 1L, NA))
  expect_identical(u$group.sorted[, 1], c(1, NA, 2, 2, NA))
  expect_identical(u$groupLabels, s$groupLabels)

  # one group left: no blank rows
  one <- s[c(3, 1), ]
  expect_identical(one$y.sorted, s$y.sorted[1:2, ])
  # repeated cases are plotted next to each other
  expect_identical(s[c(2, 2), ]$order.y.sorted[, 1], c(2L, 2L))
  # subsets of subsets, and the same object for x[, ]
  expect_identical(u[c(1, 3), ]$y.sorted, s[c(2, 4), ]$y.sorted)
  expect_identical(s[, ], s)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_silent(plot(u, groupBuffer = 0.1))
  # the same long data without case 1, with plot rows renumbered
  d_u <- as.data.frame(u)
  d_s <- as.data.frame(s)
  d_s <- d_s[d_s$id != 1, ]
  expect_identical(unique(d_u$row), c(1L, 3L, 4L))
  expect_identical(d_u[names(d_u) != "row"], d_s[names(d_s) != "row"],
                   ignore_attr = TRUE)
})

test_that("subsets of sorted objects match the filtered plot rows", {
  set.seed(1026)
  for (k in 1:100) {
    n <- sample(4:30, 1)
    y <- matrix(sample(1:3, n * 3, replace = TRUE), n, 3)
    y[1, 1] <- 1
    y[2, 1] <- 2
    y[3, 1] <- 3
    group <- sample(c(1:3, NA), n, replace = TRUE, prob = c(.3, .3, .3, .1))
    s <- suppressWarnings(sorter(longCat(y), group = group, ggap = 0.1))
    keep <- sample(c(TRUE, FALSE), n, replace = TRUE)
    keep[1] <- TRUE
    u <- s[keep, ]
    # expected: the plot rows of the kept cases, with each group's blank
    # rows kept while two or more groups have cases
    plotted <- s$order.y.sorted[, 4]
    kept_rows <- which(plotted %in% which(keep))
    groups_left <- unique(s$group.sorted[kept_rows, 1])
    if (length(groups_left) > 1) {
      gap <- sum(is.na(plotted)) %/% length(unique(stats::na.omit(s$group.sorted[, 1])))
      expected <- unlist(lapply(groups_left, function(g) {
        c(kept_rows[s$group.sorted[kept_rows, 1] == g], rep(NA, gap))
      }))
    } else {
      expected <- kept_rows
    }
    expect_identical(u$y.sorted, unname(s$y.sorted[expected, , drop = FALSE]))
  }
})

test_that("[.longCat checks its arguments", {
  lc <- small_lc()
  expect_error(lc[1], "Select cases with `x\\[i, \\]`")
  expect_error(lc[1, 2], "Only cases can be selected")
  expect_error(lc[c(TRUE, FALSE), ], "one non-missing value per case \\(4\\)")
  expect_error(lc[c(1, NA), ], "case numbers between 1 and 4")
  expect_error(lc[5, ], "case numbers between 1 and 4")
  expect_error(lc[c(-1, 2), ], "all positive or all negative")
  expect_error(lc["a", ], "needs character case IDs")
  expect_error(small_lc(id = c("a", "b", "c", "d"))["z", ], "Unknown case ID")
  expect_error(lc[rep(FALSE, 4), ], "at least one case")
  expect_error(lc[list(1), ], "must be logical, numeric, or character")
})

test_that("base tools still see a longCat object as a list", {
  lc <- small_lc()
  expect_output(str(lc), "List of 22")
  expect_true(isTRUE(all.equal(lc, lc)))
  expect_identical(lc[["y"]], lc$y)
  expect_length(lapply(lc, class), 22L)
})

# ---- [.longCont -----------------------------------------------------------------

test_that("x[i, ] subsets a longCont object", {
  lcont <- longCont(rbind(c(1, 2), c(3, 4), c(5, 6)), rbind(1:2, 2:3, 3:4))
  s <- lcont[c(3, 1), ]
  expect_s3_class(s, "longCont")
  expect_identical(s$y, rbind(c(5, 6), c(1, 2)))
  expect_identical(s$times, rbind(c(3, 4), c(1, 2)))
  expect_identical(s$dim, c(2L, 2L))
  expect_identical(lcont[-2, ]$y, lcont$y[-2, ])
  expect_error(lcont["a", ], "needs character case IDs")
  expect_error(lcont[1], "Select cases")
})

# ---- as.longCont() of a longCat object ------------------------------------------

test_that("as.longCont() turns states into values at the interval starts", {
  lc <- small_lc(times = c(0, 10, 20, 30))
  lcont <- as.longCont(lc)
  expect_s3_class(lcont, "longCont")
  # numeric labels are the original values
  expect_identical(lcont$y, unname(small_y()))
  expect_identical(lcont$times[1, ], c(0, 10, 20))
  expect_false(lcont$IndTime)

  lc <- longCat(small_y() - 1)      # states 0, 1, 2
  expect_identical(as.longCont(lc)$y, unname(small_y() - 1))
  lc <- small_lc(Labels = c("low", "mid", "high"))
  expect_identical(as.longCont(lc)$y, unname(small_y()))   # codes
  expect_identical(as.longCont(lc, values = c(0, 5, 10))$y,
                   unname((small_y() - 1) * 5))
})

test_that("as.longCont() keeps individually varying times", {
  tm <- rbind(0:3, 10:13, 0:3, c(0, 5, 6, 7))
  lcont <- as.longCont(small_lc(times = tm))
  expect_true(lcont$IndTime)
  expect_identical(lcont$times, tm[, 1:3])
})

test_that("as.longCont() checks values", {
  lc <- small_lc()
  expect_error(as.longCont(lc, values = 1:2), "a number for each of the 3 states")
  expect_error(as.longCont(lc, values = c("a", "b", "c")), "`values` must")
  expect_error(as.longCont(lc, values = c(1, NA, 3)), "`values` must")
})
