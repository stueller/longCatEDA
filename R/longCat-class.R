# S3 class infrastructure for longCat objects.
#
# A longCat object is a named list with class "longCat". Every field in
# `longcat_fields` is always present (possibly NULL) so that `$` never falls
# back to partial matching (e.g., `lc$events` silently returning
# `lc$events.sorted` when `events` is absent).

longcat_fields <- c(
  "y", "y.sorted", "dim", "times", "times.sorted", "Labels", "tLabels",
  "factors", "IndTime", "nfactors", "sorted", "ascending", "group",
  "group.sorted", "groupLabels", "order.y", "order.y.sorted", "events",
  "event.times", "events.sorted", "event.times.sorted", "eventLabels",
  "weights"
)

#' Construct and validate a longCat object
#'
#' Low-level constructor used by [longCat()] and [sorter()]. Fills any absent
#' field with `NULL`, puts fields in canonical order, sets the class, and runs
#' [validate_longCat()].
#'
#' @param x A named list of longCat fields.
#' @return A validated object of class `longCat`.
#' @keywords internal
#' @noRd
new_longCat <- function(x) {
  if (!is.list(x) || is.null(names(x))) {
    stop("`x` must be a named list.", call. = FALSE)
  }
  unknown <- setdiff(names(x), longcat_fields)
  if (length(unknown) > 0) {
    stop("Unknown longCat field(s): ", paste(unknown, collapse = ", "),
         call. = FALSE)
  }
  out <- stats::setNames(vector("list", length(longcat_fields)), longcat_fields)
  for (nm in names(x)) {
    if (!is.null(x[[nm]])) out[[nm]] <- x[[nm]]
  }
  validate_longCat(structure(out, class = "longCat"))
}

#' Validate a longCat object
#'
#' Checks that an object has the structure produced by [longCat()] and
#' [sorter()]. All problems are collected and reported in a single error, so a
#' modified object can be fixed in one pass. Called on entry by functions that
#' accept a `longCat` object; call it yourself after editing an object by hand.
#'
#' @param x Object to validate.
#' @param arg Name used for `x` in error messages.
#' @return `x`, invisibly, if it is valid. Otherwise an error listing every
#'   problem found.
#' @seealso [longCat()]
#' @examples
#' lc <- longCat(example3)
#' validate_longCat(lc)
#'
#' # hand edits are checked
#' lc$sorted <- TRUE
#' try(validate_longCat(lc))
#' @md
#' @export
validate_longCat <- function(x, arg = "lc") {
  if (!inherits(x, "longCat")) {
    stop(sprintf("`%s` must be a longCat object created by longCat().", arg),
         call. = FALSE)
  }

  # structural problems make the remaining checks meaningless, so stop early
  missing <- setdiff(longcat_fields, names(x))
  unknown <- setdiff(names(x), longcat_fields)
  structural <- c(
    if (length(missing) > 0) {
      paste0("missing field(s): ", paste(missing, collapse = ", "))
    },
    if (length(unknown) > 0) {
      paste0("unknown field(s): ", paste(unknown, collapse = ", "))
    }
  )
  if (length(structural) > 0) longcat_abort(arg, structural)

  problems <- c(
    check_core_fields(x),
    check_label_fields(x),
    check_group_fields(x),
    check_event_fields(x),
    check_weight_fields(x),
    check_sorted_fields(x)
  )
  if (length(problems) > 0) longcat_abort(arg, problems)
  invisible(x)
}

longcat_abort <- function(arg, problems, class = "longCat") {
  stop(sprintf("`%s` is not a valid %s object:\n%s", arg, class,
               paste0("* ", problems, collapse = "\n")),
       call. = FALSE)
}

# ---- field checks: each returns a character vector of problems -------------

check_core_fields <- function(x) {
  y <- x$y
  if (!is.matrix(y) || !is.numeric(y)) {
    return("`y` must be a numeric matrix.")
  }
  problems <- character(0)

  if (!is_whole_scalar(x$nfactors) || x$nfactors < 1) {
    problems <- c(problems, "`nfactors` must be a single positive integer.")
  } else {
    if (!is.numeric(x$factors) || length(x$factors) != x$nfactors) {
      problems <- c(problems, "`factors` must be numeric of length `nfactors`.")
    }
    if (!all(y %in% c(NA, seq_len(x$nfactors)))) {
      problems <- c(problems,
                    "`y` may only contain NA or integers 1 to `nfactors`.")
    }
  }

  if (!is.numeric(x$dim) || !identical(as.integer(x$dim), dim(y))) {
    problems <- c(problems, "`dim` must equal `dim(y)`.")
  }

  times <- x$times
  if (!is.matrix(times) || !is.numeric(times)) {
    problems <- c(problems, "`times` must be a numeric matrix.")
  } else if (!identical(dim(times), dim(y) + c(0L, 1L))) {
    problems <- c(problems,
                  "`times` must have dimension c(nrow(y), ncol(y) + 1).")
  } else if (!rows_nondecreasing(times)) {
    problems <- c(problems,
                  "Each row of `times` must be non-decreasing (ignoring NA).")
  }

  if (!is_flag(x$IndTime)) problems <- c(problems, "`IndTime` must be TRUE or FALSE.")
  if (!is_flag(x$sorted)) problems <- c(problems, "`sorted` must be TRUE or FALSE.")

  order_y <- x$order.y
  if (!is.data.frame(order_y) || nrow(order_y) != nrow(y) ||
      ncol(order_y) != 4) {
    problems <- c(problems,
                  "`order.y` must be a data frame with nrow(y) rows and 4 columns.")
  }
  problems
}

check_label_fields <- function(x) {
  problems <- character(0)
  if (!is.atomic(x$Labels) || length(x$Labels) == 0) {
    problems <- c(problems, "`Labels` must be a non-empty atomic vector.")
  }
  if (!is.null(x$tLabels) &&
      (!is.atomic(x$tLabels) || length(x$tLabels) != ncol(x$y))) {
    problems <- c(problems,
                  "`tLabels` must be NULL or an atomic vector of length ncol(y).")
  }
  problems
}

check_group_fields <- function(x) {
  problems <- character(0)
  if (!is.null(x$group) &&
      (!is.atomic(x$group) || length(x$group) != nrow(x$y))) {
    problems <- c(problems,
                  "`group` must be NULL or an atomic vector of length nrow(y).")
  }
  if (!is.null(x$groupLabels) && !is.atomic(x$groupLabels)) {
    problems <- c(problems, "`groupLabels` must be NULL or an atomic vector.")
  }
  problems
}

check_event_fields <- function(x) {
  events <- x$events
  event_times <- x$event.times
  if (is.null(events)) {
    if (!is.null(event_times)) {
      return("`event.times` must be NULL when `events` is NULL.")
    }
    return(character(0))
  }

  problems <- character(0)
  if (!is.matrix(events) || !is.atomic(events) || nrow(events) != nrow(x$y)) {
    problems <- c(problems,
                  "`events` must be NULL or a matrix with nrow(y) rows.")
  }
  if (!is.matrix(event_times) || !is.numeric(event_times) ||
      !identical(dim(event_times), dim(events))) {
    problems <- c(problems,
                  "`event.times` must be a numeric matrix with the same dimension as `events`.")
  }
  if (!is.null(x$eventLabels) && !is.atomic(x$eventLabels)) {
    problems <- c(problems, "`eventLabels` must be NULL or an atomic vector.")
  }
  problems
}

check_weight_fields <- function(x) {
  w <- x$weights
  if (is.null(w)) return(character(0))
  if (!is.numeric(w) || length(w) != nrow(x$y) || anyNA(w) ||
      any(!is.finite(w)) || any(w < 0) || all(w == 0)) {
    return(paste("`weights` must be NULL or one finite, non-negative weight",
                 "per case, not all zero."))
  }
  character(0)
}

check_sorted_fields <- function(x) {
  sorted_fields <- c("y.sorted", "times.sorted", "order.y.sorted",
                     "group.sorted", "events.sorted", "event.times.sorted")
  if (!isTRUE(x$sorted)) {
    present <- sorted_fields[!vapply(sorted_fields, function(f) is.null(x[[f]]),
                                     logical(1))]
    if (length(present) > 0) {
      return(paste0("`sorted` is FALSE but sorted field(s) are set: ",
                    paste(present, collapse = ", "),
                    ". Use plot(lc, sort = FALSE) to plot unsorted data."))
    }
    if (!is.null(x$ascending)) {
      return("`ascending` must be NULL when `sorted` is FALSE.")
    }
    return(character(0))
  }

  problems <- character(0)
  if (!is_flag(x$ascending)) {
    problems <- c(problems, "`ascending` must be TRUE or FALSE when `sorted` is TRUE.")
  }
  if (!is_2d(x$y.sorted) || ncol(x$y.sorted) != ncol(x$y)) {
    return(c(problems, paste(
      "`y.sorted` must be a matrix or data frame with ncol(y) columns when",
      "`sorted` is TRUE. Use plot(lc, sort = FALSE) to plot unsorted data."
    )))
  }

  n_sorted <- nrow(x$y.sorted)
  expect_rows <- function(field, n_col) {
    value <- x[[field]]
    if (is.null(value)) return(sprintf("`%s` must be set when `sorted` is TRUE.", field))
    if (!is_2d(value) || nrow(value) != n_sorted ||
        (!is.null(n_col) && ncol(value) != n_col)) {
      return(sprintf("`%s` must have the same number of rows as `y.sorted`%s.",
                     field,
                     if (is.null(n_col)) "" else sprintf(" and %d column(s)", n_col)))
    }
    character(0)
  }
  rows <- x$order.y[, 4]
  if (!is.numeric(rows) || !identical(as.integer(rows), seq_len(nrow(x$y)))) {
    problems <- c(problems, paste(
      "`order.y[, 4]` must be the row numbers 1 to nrow(y) when `sorted` is",
      "TRUE."))
  }
  problems <- c(problems,
                expect_rows("times.sorted", ncol(x$y) + 1L),
                expect_rows("order.y.sorted", 4L),
                expect_rows("group.sorted", 1L))
  if (!is.null(x$events)) {
    problems <- c(problems,
                  expect_rows("events.sorted", ncol(x$events)),
                  expect_rows("event.times.sorted", ncol(x$events)))
  } else if (!is.null(x$events.sorted) || !is.null(x$event.times.sorted)) {
    problems <- c(problems,
                  "`events.sorted` and `event.times.sorted` must be NULL when `events` is NULL.")
  }
  problems
}

# ---- small predicates -----------------------------------------------------

is_flag <- function(x) is.logical(x) && length(x) == 1L && !is.na(x)

is_whole_scalar <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && x == round(x)
}

is_2d <- function(x) is.matrix(x) || is.data.frame(x)

# TRUE if the non-missing values of every row of `m` do not decrease. Works
# column by column (comparing each value with the row's last non-missing
# one), which is much faster than apply() over rows for many cases.
rows_nondecreasing <- function(m) {
  if (ncol(m) < 2L) return(TRUE)
  last <- m[, 1]
  for (j in 2:ncol(m)) {
    current <- m[, j]
    if (any(current < last, na.rm = TRUE)) return(FALSE)
    last <- ifelse(is.na(current), last, current)
  }
  TRUE
}

# ---- methods --------------------------------------------------------------

#' Print and summarize longCat objects
#'
#' `print()` gives a compact description of a [longCat()] object. `summary()`
#' returns the case and time-point counts, the frequency of each state (all
#' cells pooled), and group and event counts when present.
#'
#' @param x,object An object of class `longCat` (or `summary.longCat` for the
#'   print method of the summary).
#' @param ... Unused; included for consistency with the generics.
#' @return `print()` methods return `x` invisibly. `summary()` returns an
#'   object of class `summary.longCat`, a list with elements `dim`,
#'   `IndTime`, `sorted`, `ascending`, `states` (a table of state counts
#'   including `NA`), `groups` (a table, or `NULL`), and `events` (a table, or
#'   `NULL`).
#' @examples
#' lc <- longCat(example3)
#' lc
#' summary(lc)
#' @md
#' @name longCat-methods
NULL

#' @rdname longCat-methods
#' @md
#' @export
print.longCat <- function(x, ...) {
  cat(sprintf("<longCat> %s cases x %s time points\n",
              format_count(x$dim[1]), format_count(x$dim[2])))
  cat(sprintf("States (%d): %s\n", x$nfactors, collapse_labels(x$Labels)))
  cat("Times: ", if (x$IndTime) "individually varying" else "common to all cases",
      "\n", sep = "")
  if (is.null(x$events)) {
    cat("Events: none\n")
  } else {
    cat(sprintf("Events: up to %d per case; labels: %s\n", ncol(x$events),
                collapse_labels(x$eventLabels)))
  }
  if (!is.null(x$weights)) {
    cat(sprintf("Weights: total %s (%s to %s)\n", format_weight(sum(x$weights)),
                format_weight(min(x$weights)), format_weight(max(x$weights))))
  }
  if (x$sorted) {
    n_groups <- length(unique(stats::na.omit(x$group)))
    cat(sprintf("Sorted: yes (%s)%s\n",
                if (x$ascending) "ascending" else "descending",
                if (n_groups > 1) sprintf(", %d groups", n_groups) else ""))
  } else {
    cat("Sorted: no\n")
  }
  invisible(x)
}

#' @rdname longCat-methods
#' @md
#' @export
summary.longCat <- function(object, ...) {
  states <- table(factor(object$y, levels = seq_len(object$nfactors)),
                  useNA = "always")
  if (length(object$Labels) == object$nfactors) {
    names(states) <- c(as.character(object$Labels), NA)
  }
  structure(
    list(
      dim = object$dim,
      IndTime = object$IndTime,
      sorted = object$sorted,
      ascending = object$ascending,
      states = states,
      groups = if (object$sorted && !is.null(object$group)) {
        table(object$group, useNA = "ifany")
      },
      events = if (!is.null(object$events)) table(object$events)
    ),
    class = "summary.longCat"
  )
}

#' @rdname longCat-methods
#' @md
#' @export
print.summary.longCat <- function(x, ...) {
  cat(sprintf("longCat object: %s cases x %s time points\n",
              format_count(x$dim[1]), format_count(x$dim[2])))
  cat("Times:", if (x$IndTime) "individually varying" else "common to all cases",
      "\n")
  cat("Sorted:", if (!x$sorted) "no" else if (x$ascending) "ascending" else "descending",
      "\n")
  cat("\nState frequencies (all cells):\n")
  print(x$states)
  if (!is.null(x$groups)) {
    cat("\nCases per group:\n")
    print(x$groups)
  }
  if (!is.null(x$events)) {
    cat("\nEvent frequencies:\n")
    print(x$events)
  }
  invisible(x)
}

format_count <- function(n) format(n, big.mark = ",", scientific = FALSE)

format_weight <- function(w) {
  format(signif(w, 6), big.mark = ",", scientific = FALSE, drop0trailing = TRUE)
}

collapse_labels <- function(labels, max = 6) {
  if (length(labels) == 0) return("(none)")
  labels <- as.character(labels)
  if (length(labels) > max) {
    labels <- c(labels[seq_len(max)], sprintf("... (%d more)", length(labels) - max))
  }
  paste(labels, collapse = ", ")
}
