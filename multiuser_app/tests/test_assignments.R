`%||%` <- function(x, y) if (is.null(x)) y else x

source("R/adjudication_schema.R")
source("R/users.R")
source("R/assignments.R")

users <- list(
  list(user_id="usr-admin", email="admin@example.org", display_name="Admin", role="administrator", active=TRUE),
  list(user_id="usr-a", email="a@example.org", display_name="Reviewer A", role="reviewer", active=TRUE),
  list(user_id="usr-b", email="b@example.org", display_name="Reviewer B", role="reviewer", active=TRUE)
)

shared <- list(
  list(assignment_id="s1", workflow="01", task_type="deduplication", batch_id="batch-1", case_id="case-1", user_id="usr-a", blind_group="pool", status="assigned"),
  list(assignment_id="s2", workflow="01", task_type="deduplication", batch_id="batch-1", case_id="case-1", user_id="usr-b", blind_group="pool", status="assigned"),
  list(assignment_id="s3", workflow="01", task_type="deduplication", batch_id="batch-1", case_id="case-2", user_id="usr-a", blind_group="pool", status="assigned")
)

blind <- list(
  list(assignment_id="b1", workflow="04", task_type="manual_screening", batch_id="batch-4", case_id="case-9", user_id="usr-a", blind_group="blind-1", status="assigned"),
  list(assignment_id="b2", workflow="04", task_type="manual_screening", batch_id="batch-4", case_id="case-9", user_id="usr-b", blind_group="blind-1", status="assigned")
)

validate_assignment_registry(shared)
validate_assignment_registry(blind)

stopifnot(
  identical(assignment_mode_for("01","deduplication"), "shared_work_pool"),
  identical(assignment_mode_for("04","manual_screening"), "independent_blind_review"),
  identical(assignment_mode_for("08","annotation"), "single_reviewer")
)

cases <- list(
  list(review_case_id="case-1"),
  list(review_case_id="case-2"),
  list(review_case_id="case-3")
)

reviewer_a <- find_user_by_id(users, "usr-a")
reviewer_b <- find_user_by_id(users, "usr-b")
admin <- find_user_by_id(users, "usr-admin")

events_shared <- list(
  list(case_id="case-1", user_id="usr-a", decision="duplicate", event_at_utc="2026-10-03T09:00:00Z")
)

visible_b <- cases_for_assignment_user(
  cases, shared, "01", "batch-1", reviewer_b,
  task_type="deduplication", active_events=events_shared
)
stopifnot(length(visible_b) == 0L)

visible_a <- cases_for_assignment_user(
  cases, shared, "01", "batch-1", reviewer_a,
  task_type="deduplication", active_events=events_shared
)
stopifnot(
  length(visible_a) == 2L,
  setequal(vapply(visible_a, function(x) x$review_case_id, character(1)), c("case-1","case-2"))
)

visible_admin <- cases_for_assignment_user(
  cases, shared, "01", "batch-1", admin,
  task_type="deduplication", active_events=events_shared
)
stopifnot(length(visible_admin) == 3L)

p_shared <- assignment_progress(shared, events_shared, users)
stopifnot(
  identical(p_shared$assigned, 3L),
  identical(p_shared$completed, 1L),
  identical(p_shared$resolved_elsewhere, 1L),
  identical(p_shared$remaining, 1L),
  identical(p_shared$cases, 2L)
)

blind_events_one <- list(
  list(case_id="case-9", user_id="usr-a", event_at_utc="2026-10-03T09:10:00Z")
)
p_blind_one <- assignment_progress(blind, blind_events_one, users)
stopifnot(
  identical(p_blind_one$completed, 1L),
  identical(p_blind_one$resolved_elsewhere, 0L),
  identical(p_blind_one$remaining, 1L)
)

blind_events_two <- c(
  blind_events_one,
  list(list(case_id="case-9", user_id="usr-b", event_at_utc="2026-10-03T09:11:00Z"))
)
p_blind_two <- assignment_progress(blind, blind_events_two, users)
stopifnot(
  identical(p_blind_two$completed, 2L),
  identical(p_blind_two$resolved_elsewhere, 0L),
  identical(p_blind_two$remaining, 0L)
)

allocation_cases <- lapply(seq_len(10), function(i) list(review_case_id = paste0("alloc-", i)))
allocation_events <- list(
  list(case_id="alloc-1", user_id="usr-a", decision="uncertain", event_at_utc="2026-10-03T10:00:00Z"),
  list(case_id="alloc-2", user_id="usr-a", decision="duplicate", event_at_utc="2026-10-03T10:01:00Z")
)

plan_number <- plan_shared_pool_assignment(
  cases = allocation_cases,
  assignments = list(),
  active_events = allocation_events,
  workflow = "01",
  batch_id = "batch-alloc",
  task_type = "deduplication",
  user_ids = c("usr-a","usr-b"),
  allocation_type = "number",
  amount = 3
)
stopifnot(
  identical(plan_number$available, 9L),
  identical(plan_number$allocated, 3L),
  identical(unname(plan_number$by_user), c(2L,1L)),
  "alloc-1" %in% plan_number$case_ids,
  !"alloc-2" %in% plan_number$case_ids
)

plan_percent <- plan_shared_pool_assignment(
  cases = allocation_cases,
  assignments = list(),
  active_events = allocation_events,
  workflow = "01",
  batch_id = "batch-percent",
  task_type = "deduplication",
  user_ids = c("usr-a","usr-b"),
  allocation_type = "percentage",
  amount = 50
)
stopifnot(
  identical(plan_percent$available, 9L),
  identical(plan_percent$allocated, 5L),
  identical(unname(plan_percent$by_user), c(3L,2L))
)

after_first <- c(plan_number$new_assignments)
plan_again <- plan_shared_pool_assignment(
  cases = allocation_cases,
  assignments = after_first,
  active_events = allocation_events,
  workflow = "01",
  batch_id = "batch-alloc",
  task_type = "deduplication",
  user_ids = c("usr-a","usr-b"),
  allocation_type = "number",
  amount = 3
)
stopifnot(
  identical(plan_again$available, 6L),
  length(intersect(plan_number$case_ids, plan_again$case_ids)) == 0L
)

removal_assignments <- list(
  list(assignment_id="rm-1", workflow="01", task_type="deduplication", batch_id="batch-rm", case_id="rm-case-1", user_id="usr-a", blind_group="pool", status="assigned"),
  list(assignment_id="rm-2", workflow="01", task_type="deduplication", batch_id="batch-rm", case_id="rm-case-2", user_id="usr-b", blind_group="pool", status="assigned")
)
removal_events <- list(
  list(case_id="rm-case-2", user_id="usr-b", decision="duplicate", event_at_utc="2026-10-03T10:15:00Z")
)

cancellable <- cancellable_assignments(
  removal_assignments, removal_events, "01", "batch-rm", "deduplication"
)
stopifnot(
  length(cancellable) == 1L,
  identical(normalise_assignment_row(cancellable[[1L]])$assignment_id, "rm-1")
)

removed <- cancel_assignment_ids(
  removal_assignments, "rm-1", removal_events, "01", "batch-rm", "deduplication"
)
stopifnot(
  identical(removed$cancelled, 1L),
  identical(normalise_assignment_row(removed$assignments[[1L]])$status, "cancelled"),
  length(active_assignments_for_batch(
    removed$assignments, "01", "batch-rm", "deduplication"
  )) == 1L
)

blocked_remove <- tryCatch(
  {
    cancel_assignment_ids(
      removal_assignments, "rm-2", removal_events, "01", "batch-rm", "deduplication"
    )
    FALSE
  },
  error = function(e) grepl("completed, resolved, or no longer active", conditionMessage(e), fixed = TRUE)
)
stopifnot(isTRUE(blocked_remove))

reassign_cases <- list(
  list(review_case_id="rm-case-1"),
  list(review_case_id="rm-case-2")
)
replan <- plan_shared_pool_assignment(
  cases = reassign_cases,
  assignments = removed$assignments,
  active_events = removal_events,
  workflow = "01",
  batch_id = "batch-rm",
  task_type = "deduplication",
  user_ids = "usr-a",
  allocation_type = "number",
  amount = 1
)
stopifnot(
  identical(replan$allocated, 1L),
  identical(replan$case_ids, "rm-case-1"),
  !identical(
    normalise_assignment_row(replan$new_assignments[[1L]])$assignment_id,
    "rm-1"
  )
)

cat("PASS: allocation planning and safe unfinished-assignment cancellation\n")
