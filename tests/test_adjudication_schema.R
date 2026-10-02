source("R/adjudication_schema.R")

sha <- paste(rep("a", 64), collapse = "")

stopifnot(identical(
  ADJUDICATION_ROLES,
  c("administrator", "reviewer")
))
stopifnot("locked" %in% ADJUDICATION_BATCH_STATES)
stopifnot(is_sha256(sha))
stopifnot(!is_sha256("not-a-sha"))

validate_user_contract(list(
  user_id = "usr-neal",
  email = "neal@example.org",
  display_name = "Neal",
  role = "administrator",
  active = TRUE
))

validate_user_contract(list(
  user_id = "usr-reviewer",
  email = "reviewer@example.org",
  display_name = "Reviewer",
  role = "reviewer",
  active = TRUE
))

validate_review_batch_contract(list(
  batch_id = "batch-001",
  workflow = "04",
  queue_sha256 = sha,
  status = "awaiting_review",
  created_at_utc = "2026-10-02T00:00:00Z",
  locked_at_utc = ""
))

validate_review_case_contract(list(
  case_id = "case-001",
  record_id = "record-001",
  batch_id = "batch-001",
  case_index = 1L,
  case_json = "{\"record_id\":\"record-001\"}"
))

validate_assignment_contract(list(
  assignment_id = "asg-001",
  case_id = "case-001",
  user_id = "usr-neal",
  blind_group = "A",
  status = "assigned"
))

validate_decision_event_contract(list(
  decision_id = "dec-001",
  case_id = "case-001",
  user_id = "usr-neal",
  decision = "include",
  version = 1L,
  active = TRUE,
  event_at_utc = "2026-10-02T00:01:00Z",
  queue_sha256 = sha,
  supersedes_decision_id = ""
))

validate_decision_event_contract(list(
  decision_id = "dec-002",
  case_id = "case-001",
  user_id = "usr-neal",
  decision = "exclude",
  version = 2L,
  active = TRUE,
  event_at_utc = "2026-10-02T00:02:00Z",
  queue_sha256 = sha,
  supersedes_decision_id = "dec-001"
))

validate_batch_transition("ready", "running")
validate_batch_transition("awaiting_review", "ready_for_export")
validate_batch_transition("export_pending", "locked")

must_fail <- function(expr) {
  ok <- FALSE
  tryCatch(
    force(expr),
    error = function(e) ok <<- TRUE
  )
  stopifnot(ok)
}

must_fail(validate_user_contract(list(
  user_id = "usr-x",
  email = "x@example.org",
  display_name = "X",
  role = "resolver",
  active = TRUE
)))

must_fail(validate_review_batch_contract(list(
  batch_id = "batch-locked",
  workflow = "04",
  queue_sha256 = sha,
  status = "locked",
  created_at_utc = "2026-10-02T00:00:00Z",
  locked_at_utc = ""
)))

must_fail(validate_decision_event_contract(list(
  decision_id = "dec-bad",
  case_id = "case-001",
  user_id = "usr-neal",
  decision = "include",
  version = 2L,
  active = TRUE,
  event_at_utc = "2026-10-02T00:03:00Z",
  queue_sha256 = sha,
  supersedes_decision_id = ""
)))

must_fail(validate_batch_transition("locked", "running"))

cat("PASS: lean adjudication schema contract\n")
