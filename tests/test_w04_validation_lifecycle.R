suppressPackageStartupMessages(library(digest))

source("R/w01_contract.R")
source("R/adjudication_schema.R")
source("R/users.R")
source("R/assignments.R")
source("R/w04_validation_lifecycle.R")

cases <- lapply(seq_len(4L), function(i) {
  list(
    review_case_id = paste0("w04-val-", i),
    record_id = paste0("record-", i),
    bibliographic = list(
      title = paste("Title", i),
      abstract = paste("Abstract", i)
    )
  )
})

batch_id <- "w04-validation-test-batch"
queue_lines <- vapply(
  cases,
  function(x) jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"),
  character(1)
)
queue_sha <- digest::digest(
  paste0(paste(queue_lines, collapse = "\n"), "\n"),
  algo = "sha256",
  serialize = FALSE
)

plan <- plan_w04_manual_assignment(
  cases = cases,
  assignments = list(),
  active_events = list(),
  batch_id = batch_id,
  user_ids = c("reviewer-a", "reviewer-b"),
  review_mode = "validation_set",
  allocation_type = "all"
)

stopifnot(
  identical(plan$review_mode, "validation_set"),
  identical(plan$allocated, 4L),
  length(plan$new_assignments) == 4L,
  all(vapply(
    plan$new_assignments,
    function(x) identical(normalise_assignment_row(x)$blind_group, "w04-validation-set"),
    logical(1)
  ))
)

assigned_ids <- vapply(
  plan$new_assignments,
  function(x) normalise_assignment_row(x)$case_id,
  character(1)
)
stopifnot(!anyDuplicated(assigned_ids), setequal(assigned_ids, vapply(
  cases, function(x) x$review_case_id, character(1)
)))

decisions <- lapply(seq_along(plan$new_assignments), function(i) {
  a <- normalise_assignment_row(plan$new_assignments[[i]])
  list(
    review_case_id = a$case_id,
    record_id = cases[[match(a$case_id, vapply(cases, function(x) x$review_case_id, character(1)))]]$record_id,
    reviewer = a$user_id,
    decision = c("retain", "exclude", "uncertain", "retain")[[i]],
    queue_sha256 = queue_sha,
    resolved_at_utc = sprintf("2026-10-04T12:%02d:00Z", i)
  )
})

ready <- w04_validation_lifecycle(
  cases, plan$new_assignments, decisions, batch_id, queue_sha
)
stopifnot(
  isTRUE(ready$ready),
  identical(ready$reason, "ready"),
  identical(ready$cases, 4L),
  identical(ready$assigned_cases, 4L),
  identical(ready$decided_cases, 4L),
  length(ready$unassigned_case_ids) == 0L,
  length(ready$missing_decision_case_ids) == 0L
)

missing <- w04_validation_lifecycle(
  cases, plan$new_assignments, decisions[-1L], batch_id, queue_sha
)
stopifnot(
  isFALSE(missing$ready),
  identical(missing$reason, "incomplete_or_ambiguous"),
  identical(missing$missing_decision_case_ids, assigned_ids[[1L]])
)

wrong_sha_decisions <- decisions
wrong_sha_decisions[[1L]]$queue_sha256 <- paste(rep("0", 64L), collapse = "")
wrong_sha <- w04_validation_lifecycle(
  cases, plan$new_assignments, wrong_sha_decisions, batch_id, queue_sha
)
stopifnot(
  isFALSE(wrong_sha$ready),
  identical(wrong_sha$wrong_sha_case_ids, assigned_ids[[1L]])
)


wrong_reviewer_decisions <- decisions
wrong_reviewer_decisions[[1L]]$reviewer <- "reviewer-z"
wrong_reviewer <- w04_validation_lifecycle(
  cases, plan$new_assignments, wrong_reviewer_decisions, batch_id, queue_sha
)
stopifnot(
  isFALSE(wrong_reviewer$ready),
  identical(wrong_reviewer$mismatched_reviewer_case_ids, assigned_ids[[1L]])
)

wrong_record_decisions <- decisions
wrong_record_decisions[[1L]]$record_id <- "record-wrong"
wrong_record <- w04_validation_lifecycle(
  cases, plan$new_assignments, wrong_record_decisions, batch_id, queue_sha
)
stopifnot(
  isFALSE(wrong_record$ready),
  identical(wrong_record$mismatched_record_case_ids, assigned_ids[[1L]])
)

duplicate_assignments <- c(
  plan$new_assignments,
  list(within(plan$new_assignments[[1L]], {
    assignment_id <- "duplicate-assignment"
    user_id <- "reviewer-c"
  }))
)
duplicate_assignment <- w04_validation_lifecycle(
  cases, duplicate_assignments, decisions, batch_id, queue_sha
)
stopifnot(
  isFALSE(duplicate_assignment$ready),
  identical(duplicate_assignment$duplicate_assignment_case_ids, assigned_ids[[1L]])
)

consistency_plan <- plan_w04_manual_assignment(
  cases = cases,
  assignments = list(),
  active_events = list(),
  batch_id = batch_id,
  user_ids = c("reviewer-a", "reviewer-b"),
  review_mode = "reviewer_consistency",
  allocation_type = "all"
)
consistency_decisions <- unlist(lapply(seq_along(cases), function(i) {
  cid <- cases[[i]]$review_case_id
  list(
    list(
      review_case_id = cid,
      record_id = cases[[i]]$record_id,
      reviewer = "reviewer-a",
      decision = if (i == 2L) "retain" else "exclude",
      queue_sha256 = queue_sha
    ),
    list(
      review_case_id = cid,
      record_id = cases[[i]]$record_id,
      reviewer = "reviewer-b",
      decision = "exclude",
      queue_sha256 = queue_sha
    )
  )
}), recursive = FALSE)

consistency <- w04_validation_lifecycle(
  cases,
  consistency_plan$new_assignments,
  consistency_decisions,
  batch_id,
  queue_sha
)
stopifnot(
  isFALSE(consistency$ready),
  identical(consistency$mode, "reviewer_consistency"),
  identical(consistency$reason, "reviewer_consistency_not_finalisable_as_validation")
)

cat("PASS: W04 validation lifecycle is fail-closed and distinct from reviewer consistency\n")
