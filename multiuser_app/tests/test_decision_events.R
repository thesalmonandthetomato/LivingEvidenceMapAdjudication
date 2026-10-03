`%||%` <- function(x, y) if (is.null(x)) y else x

source("R/decision_events.R")

events <- list(
  list(
    decision_id = "d1",
    review_case_id = "case-1",
    reviewer = "usr-a",
    decision = "include",
    resolved_at_utc = "2026-10-03T08:00:00Z",
    supersedes_decision_id = ""
  ),
  list(
    decision_id = "d2",
    review_case_id = "case-2",
    reviewer = "usr-b",
    decision = "exclude",
    resolved_at_utc = "2026-10-03T08:01:00Z",
    supersedes_decision_id = ""
  ),
  list(
    decision_id = "d3",
    review_case_id = "case-1",
    reviewer = "usr-a",
    decision = "exclude",
    resolved_at_utc = "2026-10-03T08:02:00Z",
    supersedes_decision_id = "d1"
  )
)

xs <- normalise_decision_events(events, case_fields = c("case_id", "review_case_id"))
stopifnot(
  length(xs) == 3L,
  identical(xs[[1L]]$case_id, "case-1"),
  identical(xs[[1L]]$user_id, "usr-a"),
  identical(xs[[1L]]$version, 1L),
  identical(xs[[3L]]$version, 2L),
  identical(xs[[1L]]$active, FALSE),
  identical(xs[[2L]]$active, TRUE),
  identical(xs[[3L]]$active, TRUE),
  identical(xs[[3L]]$event_at_utc, "2026-10-03T08:02:00Z")
)

active <- active_decision_events(events, case_fields = c("case_id", "review_case_id"))
stopifnot(
  length(active) == 2L,
  setequal(vapply(active, function(x) x$decision_id, character(1)), c("d2", "d3"))
)

independent <- normalise_decision_events(
  c(
    events,
    list(list(
      decision_id = "d4b",
      review_case_id = "case-1",
      reviewer = "usr-b",
      decision = "include",
      resolved_at_utc = "2026-10-03T08:02:30Z",
      supersedes_decision_id = ""
    ))
  ),
  case_fields = c("case_id", "review_case_id")
)
independent_active <- independent[vapply(independent, function(x) isTRUE(x$active), logical(1))]
case1_active <- Filter(function(x) identical(x$case_id, "case-1"), independent_active)
stopifnot(
  length(case1_active) == 2L,
  setequal(vapply(case1_active, function(x) x$user_id, character(1)), c("usr-a","usr-b")),
  identical(Filter(function(x) identical(x$user_id, "usr-b"), case1_active)[[1L]]$version, 1L)
)

saved <- normalise_saved_decision_event(
  list(
    decision_id = "d4",
    review_case_id = "case-1",
    reviewer = "usr-a",
    decision = "include",
    resolved_at_utc = "2026-10-03T08:03:00Z",
    supersedes_decision_id = "d3"
  ),
  prior_decision = xs[[3L]],
  case_fields = c("case_id", "review_case_id")
)
stopifnot(
  identical(saved$version, 3L),
  identical(saved$case_id, "case-1"),
  identical(saved$user_id, "usr-a"),
  isTRUE(saved$active)
)

w08 <- normalise_decision_events(
  list(
    list(
      decision_id = "w08-1",
      record_id = "rec-9",
      reviewer = "usr-r",
      resolved_at_utc = "2026-10-03T08:04:00Z"
    )
  ),
  case_fields = c("case_id", "record_id")
)
stopifnot(
  identical(w08[[1L]]$case_id, "rec-9"),
  identical(w08[[1L]]$user_id, "usr-r"),
  identical(w08[[1L]]$version, 1L),
  isTRUE(w08[[1L]]$active)
)

cat("PASS: canonical decision event normalisation\n")
