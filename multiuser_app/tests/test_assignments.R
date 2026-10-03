`%||%` <- function(x, y) if (is.null(x)) y else x

source("R/adjudication_schema.R")
source("R/users.R")
source("R/assignments.R")

users <- list(
  list(user_id="usr-admin", email="admin@example.org", display_name="Admin", role="administrator", active=TRUE),
  list(user_id="usr-a", email="a@example.org", display_name="Reviewer A", role="reviewer", active=TRUE),
  list(user_id="usr-b", email="b@example.org", display_name="Reviewer B", role="reviewer", active=TRUE)
)

assignments <- list(
  list(assignment_id="a1", workflow="04", batch_id="batch-1", case_id="case-1", user_id="usr-a", blind_group="g1", status="assigned"),
  list(assignment_id="a2", workflow="04", batch_id="batch-1", case_id="case-1", user_id="usr-b", blind_group="g1", status="assigned"),
  list(assignment_id="a3", workflow="04", batch_id="batch-1", case_id="case-2", user_id="usr-a", blind_group="g2", status="assigned")
)

validate_assignment_registry(assignments)

cases <- list(
  list(review_case_id="case-1"),
  list(review_case_id="case-2"),
  list(review_case_id="case-3")
)

reviewer_a <- find_user_by_id(users, "usr-a")
admin <- find_user_by_id(users, "usr-admin")

visible_a <- cases_for_assignment_user(cases, assignments, "04", "batch-1", reviewer_a)
stopifnot(
  length(visible_a) == 2L,
  setequal(vapply(visible_a, function(x) x$review_case_id, character(1)), c("case-1","case-2"))
)

visible_admin <- cases_for_assignment_user(cases, assignments, "04", "batch-1", admin)
stopifnot(length(visible_admin) == 3L)

events <- list(
  list(case_id="case-1", user_id="usr-a", event_at_utc="2026-10-03T09:00:00Z"),
  list(case_id="case-1", user_id="usr-b", event_at_utc="2026-10-03T09:01:00Z")
)

p <- assignment_progress(assignments, events, users)
stopifnot(
  identical(p$assigned, 3L),
  identical(p$completed, 2L),
  identical(p$remaining, 1L)
)

pa <- Filter(function(x) identical(x$user_id, "usr-a"), p$by_user)[[1L]]
pb <- Filter(function(x) identical(x$user_id, "usr-b"), p$by_user)[[1L]]
stopifnot(
  identical(pa$assigned, 2L),
  identical(pa$completed, 1L),
  identical(pa$remaining, 1L),
  identical(pb$assigned, 1L),
  identical(pb$completed, 1L),
  identical(pb$remaining, 0L)
)

cat("PASS: assignment filtering and progress\n")
