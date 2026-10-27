# Long-format data: three cases, "a" and "b" tie, "c" has a covariate that
# changes across cases.
tie_long <- function() {
  data.frame(id = c("b", "b", "a", "a", "c", "c"),
             time = c(1, 2, 1, 2, 1, 2),
             state = c(1, 2, 1, 2, 1, 1),
             site = c("x", "x", "y", "y", "x", "x"),
             score = c(10, 11, 20, 21, 30, 31))
}

# example3 in long format, with a case-level group
example_long <- function() {
  set.seed(2)
  data.frame(pid = rep(sprintf("P%03d", 1:100), each = 6),
             wave = rep(1:6, 100),
             status = c(t(example3)),
             site = rep(sample(c("North", "South"), 100, TRUE), each = 6))
}

sort_example <- function(d, ...) {
  sorter(d, id = "pid", time = "wave", state = "status", ...)
}

# The plot rows of the wide sort of example3, by case ID.
wide_rows <- function(...) {
  s <- sorter(longCat(example3, id = sprintf("P%03d", 1:100)), ...)
  plotted <- !is.na(s$order.y.sorted[, 1])
  stats::setNames(which(plotted), s$order.y.sorted[plotted, 1])
}

# ---- sorter.data.frame() ----------------------------------------------------------

test_that("long data get each case's plot row, keeping all columns", {
  d <- sorter(tie_long())
  # (1, 1) sorts before (1, 2); the tie of "a" and "b" is broken by ID
  expect_identical(d$id, c("c", "c", "a", "a", "b", "b"))
  expect_identical(d$row, c(1L, 1L, 2L, 2L, 3L, 3L))
  expect_named(d, c("id", "time", "state", "site", "score", "row"))
  expect_identical(d$score, c(30, 31, 20, 21, 10, 11))
  expect_identical(rownames(d), as.character(1:6))
  # in plot order, then time order
  expect_identical(d$time, c(1, 2, 1, 2, 1, 2))
})

test_that("the order matches sorter() of the wide data", {
  d <- sort_example(example_long())
  rows <- wide_rows()
  expect_identical(d$row, unname(rows[d$pid]))
  # and the row of as.data.frame() after as.longCat()
  lc <- as.longCat(example_long(), id = "pid", time = "wave", state = "status")
  expect_identical(unique(d$row),
                   unique(as.data.frame(sorter(lc))$row))
})

test_that("the order of the rows of the data does not matter", {
  d <- example_long()
  set.seed(9)
  shuffled <- d[sample(nrow(d)), ]
  expect_identical(sort_example(shuffled), sort_example(d))
  # cases with the same sequence (39 cases of example3 repeat another
  # case's sequence) are in ID order
  out <- sort_example(d)
  first <- !duplicated(out$row)
  sequences <- tapply(out$status, out$row, paste, collapse = "-")
  pids <- out$pid[first]
  for (s in unique(sequences[duplicated(sequences)])) {
    expect_false(is.unsorted(pids[sequences == s]))
  }
  expect_true(any(duplicated(sequences)))
})

test_that("factor IDs break ties in the order of their levels", {
  d <- tie_long()
  d$id <- factor(d$id, levels = c("c", "b", "a"))
  out <- sorter(d)
  expect_identical(as.character(unique(out$id)), c("c", "b", "a"))
  expect_identical(levels(out$id), c("c", "b", "a"))
})

test_that("options are passed to the longCat method", {
  d <- sort_example(example_long(), ascending = FALSE, whichColumns = 4:6)
  rows <- wide_rows(ascending = FALSE, whichColumns = 4:6)
  expect_identical(d$row, unname(rows[d$pid]))
  expect_error(sort_example(example_long(), acending = FALSE),
               "Unknown arguments to sorter\\(\\): acending")
})

test_that("groups come from a column, with blank rows between them", {
  d <- example_long()
  out <- sort_example(d, group = "site", ggap = 0.1)
  g <- d$site[seq(1, 600, by = 6)]
  rows <- wide_rows(group = g, ggap = 0.1)
  expect_identical(out$row, unname(rows[out$pid]))
  # North (first in order) at the bottom, then 10 blank rows
  n_north <- sum(g == "North")
  expect_identical(unique(out$site), c("North", "South"))
  expect_identical(min(out$row[out$site == "South"]), n_north + 11L)
  # factor groups in the order of their levels
  d$site <- factor(d$site, levels = c("South", "North"))
  expect_identical(levels(sort_example(d, group = "site")$site),
                   c("South", "North"))
  expect_identical(as.character(sort_example(d, group = "site")$site[1]),
                   "South")
})

test_that("cases with a missing group are dropped", {
  d <- tie_long()
  d$site[d$id == "a"] <- NA
  expect_warning(out <- sorter(d, group = "site"), "missing `group`")
  expect_identical(unique(out$id), c("c", "b"))
  expect_false(anyNA(out$row))
})

test_that("customSort comes from a column", {
  d <- tie_long()
  d$rank <- c(1, 1, 2, 2, 3, 3)
  expect_identical(unique(sorter(d, customSort = "rank")$id), c("b", "a", "c"))
})

test_that("case-level columns must be the same within each case", {
  d <- tie_long()
  expect_error(sorter(d, group = "score"),
               "Column \"score\" \\(`group`\\) must be the same in every row")
  expect_error(sorter(d, customSort = "score"), "\\(`customSort`\\)")
  expect_error(sorter(d, group = "region"), "`lc` has no column \"region\"")
})

test_that("intervals and individually varying times are supported", {
  d <- data.frame(id = c(1, 1, 2, 2, 3),
                  start = c(0, 5, 0, 1, 0), stop = c(5, 6, 1, 6, 6),
                  state = c(1, 2, 1, 2, 2))
  out <- sorter(d, time = "start", stop = "stop")
  # cases 1 and 2 share the sequence (1, 2); with varying times the shorter
  # first state sorts first
  expect_identical(unique(out$id), c(2, 1, 3))
  expect_identical(unique(sorter(d, time = "start", stop = "stop",
                                 sortDurations = FALSE)$id), c(1, 2, 3))
})

test_that("the plot row column can be named with `into`", {
  d <- tie_long()
  expect_named(sorter(d, into = "plot_row"),
               c("id", "time", "state", "site", "score", "plot_row"))
  d$row <- 1
  expect_error(sorter(d), "already has a column \"row\"")
  expect_error(sorter(d, into = NA_character_), "`into` must be a single")
  expect_error(sorter(d, into = c("a", "b")), "`into` must be a single")
})

test_that("sorter.data.frame() checks the data", {
  d <- tie_long()
  expect_error(sorter(d, state = "status"), "`lc` has no column \"status\"")
  d$id[1] <- NA
  expect_error(sorter(d), "case IDs in `lc` must not be missing")
  d <- tie_long()
  d$time[1] <- 2
  expect_error(sorter(d), "more than one row for the same time")
})

# ---- the generic ------------------------------------------------------------------

test_that("sorter() takes longCat objects and data frames only", {
  expect_error(sorter(1:3), "must be a longCat object or a data frame")
  expect_error(sorter(small_lc(), acending = FALSE),
               "Unknown arguments to sorter\\(\\): acending")
  expect_error(sorter(small_lc(), FALSE, NULL, TRUE, NULL, FALSE, NULL, FALSE,
                      NULL, NULL, NULL, NULL, 1),
               "\\(unnamed\\)")
})
