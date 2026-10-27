# Five cases with states a (1) and b (2). With repeats collapsed: cases 1
# and 2 are "a > b", case 3 "b > a" (its missing state skipped), case 4
# "a > b > a", and case 5 "b".
pattern_y <- function() {
  rbind(c(1, 1, 2), c(1, 2, 2), c(2, NA, 1), c(1, 2, 1), c(2, 2, 2))
}
pattern_lc <- function(...) longCat(pattern_y(), Labels = c("a", "b"), ...)

# pattern_y() in long format, with a case-level group and weight
pattern_long <- function() {
  y <- pattern_y()
  d <- data.frame(id = rep(1:5, each = 3), time = rep(1:3, 5), state = c(t(y)),
                  site = rep(c("g", "g", "h", "h", "h"), each = 3),
                  w = rep(c(1, 1, 1, 1, 6), each = 3))
  d$state <- factor(d$state, levels = 1:2, labels = c("a", "b"))
  d[!is.na(d$state), ]
}

# ---- the table ----------------------------------------------------------------------

test_that("patterns() counts the sequences of spells, most common first", {
  p <- patterns(pattern_lc())
  expect_s3_class(p, "longCatPatterns")
  expect_s3_class(p, "data.frame")
  # ties in the order of sorter(): a > b > a, then b before b > a
  expect_identical(p$pattern, c("a > b", "a > b > a", "b", "b > a"))
  expect_identical(p$length, c(2L, 3L, 1L, 2L))
  expect_identical(p$n, c(2L, 1L, 1L, 1L))
  expect_equal(p$percent, c(40, 20, 20, 20))
  expect_equal(p$cumulative, c(40, 60, 80, 100))
  expect_identical(p$states, list(1:2, c(1L, 2L, 1L), 2L, 2:1))
  expect_named(p, c("pattern", "length", "n", "percent", "cumulative", "states"))
})

test_that("collapse = FALSE keeps every time point", {
  p <- patterns(pattern_lc(), collapse = FALSE)
  expect_identical(p$pattern, c("a > a > b", "a > b > a", "a > b > b",
                                "b > a", "b > b > b"))
  expect_identical(p$n, rep(1L, 5))
})

test_that("mindur drops short states before collapsing", {
  # the second interval lasts 0.5: a missing state ends a spell, so case 4
  # (1, NA, 1) is two spells of a and case 5 (2, NA, 2) two of b
  p <- patterns(pattern_lc(times = c(0, 1, 1.5, 3)), mindur = 1)
  expect_identical(p$pattern, c("a > b", "a > a", "b > a", "b > b"))
  expect_identical(p$n, c(2L, 1L, 1L, 1L))
})

test_that("weights give weighted counts and percents", {
  p <- patterns(pattern_lc(weights = c(1, 1, 1, 1, 6)))
  expect_identical(p$pattern, c("b", "a > b", "a > b > a", "b > a"))
  expect_equal(p$weight, c(6, 2, 1, 1))
  expect_identical(p$n, c(1L, 2L, 1L, 1L))
  expect_equal(p$percent, c(60, 20, 10, 10))
  expect_named(p, c("pattern", "length", "n", "weight", "percent",
                    "cumulative", "states"))
})

test_that("patterns are counted within groups", {
  p <- patterns(pattern_lc(), by = c("g", "g", "h", "h", "h"))
  expect_identical(as.character(p$group), c("g", "h", "h", "h"))
  expect_identical(p$pattern, c("a > b", "a > b > a", "b", "b > a"))
  expect_equal(p$percent, c(100, 100 / 3, 100 / 3, 100 / 3))
  expect_equal(p$cumulative, c(100, 100 / 3, 200 / 3, 100))
  # the groups of a sorted object by default
  s <- sorter(pattern_lc(), group = c("g", "g", "h", "h", "h"))
  expect_identical(patterns(s)[names(p)], p)
  expect_false("group" %in% names(patterns(sorter(pattern_lc()))))
  # missing groups are left out
  expect_warning(p <- patterns(pattern_lc(), by = c("g", NA, "h", "h", "h")),
                 "1 case\\(s\\) with a missing `by` are left out")
  expect_identical(sum(p$n), 4L)
  expect_identical(attr(p, "n_cases"), 4L)
})

test_that("a case with no observed states has the pattern NA, last", {
  p <- patterns(longCat(rbind(c(1, 2), c(NA, NA), c(2, 2)), Labels = c("a", "b")))
  expect_identical(p$pattern, c("a > b", "b", NA))
  expect_identical(p$length, c(2L, 1L, 0L))
})

test_that("labels and separators are used for the patterns", {
  p <- patterns(longCat(rbind(c(1, 12), c(11, 2))), sep = "-")
  # states 10 or more stay distinct
  expect_setequal(p$pattern, c("1-12", "11-2"))
})

# ---- long data ----------------------------------------------------------------------

test_that("long data give the same table", {
  d <- pattern_long()
  expect_identical(patterns(d), patterns(pattern_lc()))
  expect_identical(patterns(d, by = "site"),
                   patterns(pattern_lc(), by = c("g", "g", "h", "h", "h")))
  expect_identical(patterns(d, weight = "w"),
                   patterns(pattern_lc(weights = c(1, 1, 1, 1, 6))))
  # the order of the rows does not matter
  set.seed(2)
  expect_identical(patterns(d[sample(nrow(d)), ])$n, patterns(d)$n)
  d$site[1] <- "h"
  expect_error(patterns(d, by = "site"), "must be the same in every row")
  expect_error(patterns(d, by = "region"), "`x` has no column \"region\"")
})

# ---- methods ----------------------------------------------------------------------

test_that("subsets keep the class while they keep the patterns", {
  p <- patterns(pattern_lc())
  top <- head(p, 2)
  expect_s3_class(top, "longCatPatterns")
  expect_identical(attr(top, "Labels"), c("a", "b"))
  expect_false(inherits(p[, c("pattern", "n")], "longCatPatterns"))
  expect_identical(p$n, c(2L, 1L, 1L, 1L))
})

test_that("print() shows the most common patterns", {
  p <- patterns(pattern_lc())
  expect_output(print(p), "4 distinct patterns in 5 cases \\(repeated states collapsed\\)")
  expect_output(print(p, n = 2), "\\.\\.\\. and 2 more")
  # the list of state codes is not printed
  out <- capture.output(print(p))
  header <- out[grepl("cumulative", out)]
  expect_false(grepl("states", header))
  expect_output(print(patterns(pattern_lc(), collapse = FALSE)), "in 5 cases\n")
  expect_error(print(p, n = 0), "`n` must be a single positive integer")
})

test_that("plot() draws the patterns and restores the layout", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  p <- patterns(pattern_lc())
  expect_identical(plot(p), colChoose("heat", 2))
  expect_silent(plot(p, n = 2, colScheme = "topo", main = "Top 2"))
  expect_silent(plot(patterns(pattern_lc(weights = c(1, 1, 1, 1, 6)))))
  pg <- patterns(pattern_lc(), by = c("g", "g", "h", "h", "h"))
  expect_silent(plot(pg))
  expect_identical(graphics::par("mfrow"), c(1L, 1L))
  expect_error(plot(patterns(longCat(rbind(c(NA, NA), c(NA, 1))))[2, ]),
               "no patterns with observed states")
})

test_that("pattern labels show counts, or weights, and percents", {
  p <- patterns(pattern_lc())
  expect_identical(longCatEDA:::pattern_count_labels(p),
                   c("2 (40%)", "1 (20%)", "1 (20%)", "1 (20%)"))
  p <- patterns(pattern_lc(weights = c(1, 1, 1, 1, 6) * 1000.04))
  expect_identical(longCatEDA:::pattern_count_labels(p)[1], "6,000.2 (60%)")
})

test_that("autoplot() draws each pattern as a row of spells", {
  skip_if_not_installed("ggplot2")
  p <- patterns(pattern_lc())
  g <- ggplot2::autoplot(p)
  rects <- ggplot2::layer_data(g, 1)
  # 2 + 3 + 1 + 2 spells; pattern 2 (a > b > a) in the second row
  expect_identical(nrow(rects), 8L)
  second <- rects[rects$ymax == -2 + 0.4, ]
  expect_equal(second$xmin, c(0, 1, 2))
  expect_identical(second$fill, colChoose("heat", 2)[c(1, 2, 1)])
  expect_identical(ggplot2::layer_data(g, 2)$label,
                   c("2 (40%)", "1 (20%)", "1 (20%)", "1 (20%)"))
  expect_identical(nrow(ggplot2::layer_data(ggplot2::autoplot(p, n = 2), 1)), 5L)
  # groups as facets
  pg <- patterns(pattern_lc(), by = c("g", "g", "h", "h", "h"))
  g <- ggplot2::autoplot(pg, colScheme = "topo")
  expect_s3_class(g$facet, "FacetWrap")
  expect_identical(levels(ggplot2::layer_data(g, 1)$PANEL), c("1", "2"))
  expect_error(ggplot2::autoplot(p, fill = "red"),
               "Unknown arguments to autoplot\\(\\) of patterns: fill")
})

test_that("patterns() checks its arguments", {
  lc <- pattern_lc()
  expect_error(patterns(lc, collapse = NA), "`collapse` must be TRUE or FALSE")
  expect_error(patterns(lc, sep = 1), "`sep` must be a single string")
  expect_error(patterns(lc, by = 1:2), "`by` must have one value per case \\(5\\)")
  expect_error(patterns(lc, colapse = FALSE), "Unknown arguments to patterns\\(\\)")
  expect_error(patterns(lc, mindur = -1), "`mindur` must be")
  expect_error(patterns(1:3), "no applicable method")
})
