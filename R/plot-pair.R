# Plotting two categorical variables of the same cases side by side (#7).

# Draw `x` and `y` in two panels with the same plot rows. `args` are the
# values of plot.longCat()'s arguments and `dots` its further arguments;
# `y_args` override both for the second panel.
plot_pair <- function(x, y, args, dots, order, y_args, titles) {
  pair <- pair_objects(x, y, args$sort, order)
  check_y_args(y_args)
  x <- pair$x
  y <- pair$y

  x_args <- c(args, dots)
  if (is.null(x_args$main) && !is.na(titles[1])) x_args$main <- titles[1]
  second <- c(args, dots)
  second[names(y_args)] <- y_args
  if (is.null(second$main) && !is.na(titles[2])) second$main <- titles[2]
  if (!"ylab" %in% names(y_args)) second["ylab"] <- list("")

  if (identical(graphics::par("mfrow"), c(1L, 1L))) {
    op <- graphics::par(mfrow = c(1L, 2L))
    on.exit(graphics::par(op), add = TRUE)
  }
  invisible(list(x = draw_panel(x, x_args), y = draw_panel(y, second)))
}

# `x` and `y` with the same plot rows and the weights of `x`, for two
# panels: sorted together (see shared_plot_rows()) when `sort` is TRUE or
# `x` is sorted, otherwise both unsorted. Checks that `y` is a longCat
# object with the same cases.
pair_objects <- function(x, y, sort, order) {
  if (!inherits(y, "longCat")) {
    stop("`y` must be NULL or a longCat object with the same cases as `x`.",
         call. = FALSE)
  }
  validate_longCat(y, "y")
  if (nrow(y$y) != nrow(x$y) ||
      !identical(as.vector(y$order.y[, 1]), as.vector(x$order.y[, 1]))) {
    stop("`x` and `y` must have the same cases (the same IDs in the same ",
         "order).", call. = FALSE)
  }
  y <- unclass(y)
  y$weights <- x$weights
  if (sort || x$sorted) {
    shared <- shared_plot_rows(x, y, order)
    x <- new_longCat(set_sorted_fields(shared_order(unclass(x), shared),
                                       shared$rows))
    y <- new_longCat(set_sorted_fields(shared_order(y, shared), shared$rows))
  } else {
    y <- new_longCat(unsorted_fields(y))
  }
  list(x = x, y = y)
}

check_y_args <- function(y_args) {
  if (!is.list(y_args) || (length(y_args) > 0 && is.null(names(y_args)))) {
    stop("`y_args` must be a named list of arguments.", call. = FALSE)
  }
}

# Panel titles from the expressions of the two objects, when they are plain
# names; `fallback` otherwise.
pair_titles <- function(exprs, fallback = c(NA_character_, NA_character_)) {
  titles <- vapply(exprs, function(e) {
    if (is.name(e)) as.character(e) else NA_character_
  }, character(1))
  ifelse(is.na(titles), fallback, titles)
}

# One panel: plot.longCat() of the sorted object `lc` with the list `args`.
draw_panel <- function(lc, args) do.call(plot.longCat, c(list(lc), args))

# `lc` (unclassed) with the group, direction, and labels of `shared`.
shared_order <- function(lc, shared) {
  lc <- unsorted_fields(lc)
  lc$ascending <- shared$ascending
  lc$group <- shared$group
  lc$groupLabels <- shared$groupLabels
  lc
}

# The plot rows shared by both panels: those of `x` (sorted as plot() would
# sort it, unless already sorted) for order = "first"; for order = "joint",
# the cases sorted on the states of `x` and then those of `y`, within the
# groups of `x` and with its blank rows between groups.
shared_plot_rows <- function(x, y, order) {
  xs <- if (x$sorted) x else sorter(x, num = anyNA(x$y) || x$IndTime)
  shared <- list(rows = xs$order.y.sorted[, 4], group = xs$group,
                 groupLabels = xs$groupLabels, ascending = xs$ascending)
  if (order == "first") return(shared)

  num <- anyNA(x$y) || anyNA(y$y) || x$IndTime
  keys_y <- sequence_sort_keys(y$y, num)
  # order_cases() sorts the all-missing flag of the first variable last in
  # either direction; do the same for the second variable's flag, which it
  # sorts in the direction of the other keys
  if (!shared$ascending) keys_y[, 1] <- -keys_y[, 1]
  keys <- cbind(sequence_sort_keys(x$y, num), keys_y)
  group <- shared$group
  in_group <- which(!is.na(group))
  ones <- rep(1, length(in_group))
  ord <- in_group[order_cases(ones, ones, keys[in_group, , drop = FALSE],
                              shared$ascending, group = group[in_group])]
  shared$rows <- insert_gaps(ord, group, blank_rows_per_group(xs))
  shared
}
