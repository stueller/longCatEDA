# Subsetting cases of longCat and longCont objects, and converting longCat
# objects to longCont.

#' Subset the cases of longCat and longCont objects
#'
#' `x[i, ]` keeps the cases (rows) selected by `i`, with their times,
#' events, and IDs. The states, labels, and colors of a `longCat` object are
#' kept even when some states no longer occur, so plots of subsets use the
#' same colors and legend.
#'
#' A subset of a sorted `longCat` object stays sorted: its cases keep their
#' plot order and groups, with the same number of blank rows after each
#' group. To sort a subset afresh (e.g., so that each subgroup gets its own
#' gap size), subset the unsorted object and then sort it, e.g.,
#' `sorter(lc[keep, ])`.
#'
#' @param x An object of class `longCat` or `longCont`.
#' @param i The cases to keep: a logical vector with one value per case,
#'   positive or negative case numbers (rows of `y`), or, for `longCat`
#'   objects with character IDs, the IDs of the cases. Missing `i` keeps all
#'   cases.
#' @param j Not supported; only cases can be selected.
#' @param ... Unused.
#' @param drop Unused; subsets are always objects of the same class.
#' @return An object of the same class with the selected cases, in the order
#'   given by `i` (and, for sorted objects, in plot order in the sorted
#'   components).
#' @examples
#' lc <- longCat(example3)
#' lc[1:10, ]
#' lc[lc$y[, 1] == 1, ]
#'
#' # subsets of sorted objects keep their order and groups
#' set.seed(1)
#' group <- sample(c("A", "B", "C"), nrow(example3), replace = TRUE)
#' s <- sorter(lc, group = group)
#' op <- par(mfrow = c(1, 2))
#' plot(s, groupBuffer = .15, main = "All groups")
#' plot(s[group != "B", ], groupBuffer = .15, main = "Without group B")
#' par(op)
#'
#' longCont(example2cont)[-1, ]
#' @md
#' @name subset-longCat
NULL

#' @rdname subset-longCat
#' @md
#' @export
`[.longCat` <- function(x, i, j, ..., drop = FALSE) {
  validate_longCat(x, "x")
  check_case_subset_call(nargs(), missing(j))
  if (missing(i)) return(x)
  ids <- x$order.y[, 1]
  rows <- case_rows(i, nrow(x$y), if (is.character(ids)) ids)

  out <- unclass(x)
  case_fields <- c("y", "times", "events", "event.times")
  out[case_fields] <- lapply(out[case_fields], function(m) {
    if (!is.null(m)) m[rows, , drop = FALSE]
  })
  out$dim <- c(length(rows), ncol(x$y))
  out$order.y <- unname_rows(x$order.y[rows, , drop = FALSE])
  if (x$sorted) {
    out$order.y[, 4] <- seq_along(rows)
    out$group <- x$group[rows]
    listing <- sorted_subset_rows(x, rows)
    take <- function(m) if (!is.null(m)) unname_rows(m[listing, , drop = FALSE])
    out$y.sorted <- take(out$y)
    out$times.sorted <- take(out$times)
    out$order.y.sorted <- take(out$order.y)
    out$group.sorted <- matrix(out$group[listing], ncol = 1)
    out$events.sorted <- take(out$events)
    out$event.times.sorted <- take(out$event.times)
  }
  new_longCat(out)
}

# The plot rows of a subset of a sorted object: the selected cases `rows`
# (as positions in the subset) in their sorted order, with the same number
# of blank rows after each group as before. Cases that sorter() left out
# (with a missing group) stay out.
sorted_subset_rows <- function(x, rows) {
  pos <- match(rows, x$order.y.sorted[, 4])
  kept <- which(!is.na(pos))
  ord <- kept[order(pos[kept])]
  plotted_groups <- x$group.sorted[, 1]
  n_groups <- length(unique(plotted_groups[!is.na(plotted_groups)]))
  gap <- if (n_groups > 1L) sum(is.na(plotted_groups)) %/% n_groups else 0L
  insert_gaps(ord, x$group[rows], gap)
}

#' @rdname subset-longCat
#' @md
#' @export
`[.longCont` <- function(x, i, j, ..., drop = FALSE) {
  validate_longCont(x, "x")
  check_case_subset_call(nargs(), missing(j))
  if (missing(i)) return(x)
  rows <- case_rows(i, nrow(x$y), ids = NULL)
  new_longCont(list(y = x$y[rows, , drop = FALSE],
                    times = x$times[rows, , drop = FALSE],
                    IndTime = x$IndTime, dim = c(length(rows), ncol(x$y))))
}

# Only x[i, ] (or x[, ]) is supported: x[i] would usually mean list
# elements, and x[i, j] would select time points.
check_case_subset_call <- function(n_args, j_missing) {
  if (n_args < 3L) {
    stop("Select cases with `x[i, ]`; use `x$name` or `x[[\"name\"]]` for ",
         "components.", call. = FALSE)
  }
  if (!j_missing) {
    stop("Only cases can be selected: use `x[i, ]`.", call. = FALSE)
  }
}

# The rows selected by `i` among `n` cases; character `i` is matched
# against `ids` when there are character IDs.
case_rows <- function(i, n, ids) {
  if (is.logical(i)) {
    if (length(i) != n || anyNA(i)) {
      stop(sprintf("A logical `i` must have one non-missing value per case (%d).",
                   n), call. = FALSE)
    }
    rows <- which(i)
  } else if (is.character(i)) {
    if (is.null(ids)) {
      stop("Character `i` needs character case IDs (the `id` of longCat()).",
           call. = FALSE)
    }
    rows <- match(i, ids)
    if (anyNA(rows)) {
      stop("Unknown case ID(s): ", paste(i[is.na(rows)], collapse = ", "),
           call. = FALSE)
    }
  } else if (is.numeric(i)) {
    if (anyNA(i) || any(i != round(i)) || any(abs(i) > n) ||
        (any(i < 0) && any(i > 0))) {
      stop(sprintf(paste("A numeric `i` must be case numbers between 1 and",
                         "%d, all positive or all negative."), n),
           call. = FALSE)
    }
    rows <- seq_len(n)[i[i != 0]]
  } else {
    stop("`i` must be logical, numeric, or character.", call. = FALSE)
  }
  if (length(rows) == 0L) {
    stop("The subset must keep at least one case.", call. = FALSE)
  }
  rows
}

#' @rdname as.longCat
#' @param values For `as.longCont()` of a `longCat` object: the numeric value
#'   of each state, in the order of `x$Labels`. The default, `NULL`, uses
#'   the labels when they are numbers and the state codes `1`, `2`, ...
#'   otherwise.
#' @details **From longCat to longCont.** `as.longCont()` of a `longCat`
#'   object gives growth curves of the states, observed at the start of
#'   each interval, for comparison with the horizontal line plot (Tueller,
#'   Van Dorn, & Bobashev, 2016). Use `values` to give the states scores.
#' @md
#' @export
as.longCont.longCat <- function(x, values = NULL, ...) {
  validate_longCat(x, "x")
  if (is.null(values)) {
    values <- if (is.numeric(x$Labels)) x$Labels else seq_len(x$nfactors)
  }
  if (!is.numeric(values) || length(values) != x$nfactors || anyNA(values)) {
    stop(sprintf("`values` must give a number for each of the %d states.",
                 x$nfactors), call. = FALSE)
  }
  y <- matrix(values[x$y], nrow(x$y), ncol(x$y))
  times <- if (x$IndTime) x$times else x$times[1, ]
  longCont(y, times)
}
