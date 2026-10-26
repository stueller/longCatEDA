#' Move each case's events to the left, with their times
#'
#' For each row, the observed (non-missing) events are moved, in column
#' order, to the first columns, and their times move with them; the rest of
#' the row becomes `NA`. Times of missing events are dropped. After packing,
#' "the first `k` events" of a case are its first `k` columns, which is what
#' [longCatPlot()]'s `n.events` relies on after `which.events` has removed
#' some events.
#'
#' @param events,event.times Matrices (or data frames) of the same dimension.
#' @return A list with matrices `events` and `event.times`, each the same
#'   dimension as the input.
#' @keywords internal
#' @md
#' @noRd
pack_events <- function(events, event.times) {
  events <- as_case_matrix(events, "events")
  event.times <- as_case_matrix(event.times, "event.times")
  if (!identical(dim(events), dim(event.times))) {
    stop("`events` and `event.times` must have the same dimension.",
         call. = FALSE)
  }
  keep <- !is.na(events)
  list(events = left_pack_rows(events, keep),
       event.times = left_pack_rows(event.times, keep))
}
