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
  identical(xs[[1L]]$version, 1L),
  identical(xs[[3L]]$version, 2L),
  identical(xs[[1L]]$active, FALSE),
  identical(xs[[2L]]$active, TRUE),
  identical(xs[[3L]]$active, TRUE)
)

cross_reviewer <- c(
  events,
  list(list(
    decision_id = "d4",
    review_case_id = "case-1",
    reviewer = "usr-b",
    decision = "include",
    resolved_at_utc = "2026-10-03T08:03:00Z",
    supersedes_decision_id = "d3"
  ))
)

case_scope <- active_decision_events(
  cross_reviewer,
  case_fields = c("case_id", "review_case_id"),
  identity_scope = "case"
)
case1 <- Filter(function(x) identical(x$case_id, "case-1"), case_scope)
stopifnot(
  length(case1) == 1L,
  identical(case1[[1L]]$decision_id, "d4")
)

blind_scope <- active_decision_events(
  cross_reviewer,
  case_fields = c("case_id", "review_case_id"),
  identity_scope = "case_user"
)
case1_blind <- Filter(function(x) identical(x$case_id, "case-1"), blind_scope)
stopifnot(
  length(case1_blind) == 2L,
  setequal(vapply(case1_blind, function(x) x$user_id, character(1)), c("usr-a","usr-b"))
)

saved <- normalise_saved_decision_event(
  list(
    decision_id = "d5",
    review_case_id = "case-1",
    reviewer = "usr-a",
    decision = "include",
    resolved_at_utc = "2026-10-03T08:04:00Z",
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

cat("PASS: canonical decision identity supports case authority and blind review\n")
