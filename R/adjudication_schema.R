# Lean adjudication data contract
#
# This file defines prospective schemas and validation rules for the redesigned
# adjudication app. It deliberately does not alter the current storage or UI
# behaviour. Later migration phases should use these helpers at the storage
# boundaries.

ADJUDICATION_ROLES <- c("administrator", "resolver", "reviewer")

ADJUDICATION_BATCH_STATES <- c(
  "ready",
  "running",
  "awaiting_review",
  "ready_for_export",
  "export_pending",
  "locked",
  "failed"
)

ADJUDICATION_SCHEMA <- list(
  users = c(
    "user_id", "email", "display_name", "role", "active"
  ),
  review_batches = c(
    "batch_id", "workflow", "queue_sha256", "status",
    "created_at_utc", "locked_at_utc"
  ),
  review_cases = c(
    "case_id", "record_id", "batch_id", "case_index", "case_json"
  ),
  assignments = c(
    "assignment_id", "case_id", "user_id", "blind_group", "status"
  ),
  decision_events = c(
    "decision_id", "case_id", "user_id", "decision", "version",
    "active", "event_at_utc", "queue_sha256", "supersedes_decision_id"
  )
)

ADJUDICATION_ASSIGNMENT_STATES <- c("assigned", "complete")
ADJUDICATION_DECISIONS <- c("include", "exclude", "uncertain")

is_sha256 <- function(x) {
  length(x) == 1L &&
    !is.na(x) &&
    grepl("^[0-9a-fA-F]{64}$", as.character(x))
}

is_scalar_text <- function(x, allow_empty = FALSE) {
  if (length(x) != 1L || is.na(x)) return(FALSE)
  z <- as.character(x)
  if (allow_empty) TRUE else nzchar(trimws(z))
}

is_scalar_logical <- function(x) {
  length(x) == 1L && !is.na(x) && is.logical(x)
}

require_contract_fields <- function(x, entity) {
  if (!entity %in% names(ADJUDICATION_SCHEMA)) {
    stop("Unknown adjudication entity: ", entity, call. = FALSE)
  }
  missing <- setdiff(ADJUDICATION_SCHEMA[[entity]], names(x))
  if (length(missing)) {
    stop(
      entity, " missing required field(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

validate_user_contract <- function(x) {
  require_contract_fields(x, "users")
  if (!is_scalar_text(x$user_id)) stop("user_id is required", call. = FALSE)
  if (!is_scalar_text(x$email)) stop("email is required", call. = FALSE)
  if (!is_scalar_text(x$display_name)) stop("display_name is required", call. = FALSE)
  if (!is_scalar_text(x$role) || !x$role %in% ADJUDICATION_ROLES) {
    stop("Invalid user role", call. = FALSE)
  }
  if (!is_scalar_logical(x$active)) stop("active must be TRUE or FALSE", call. = FALSE)
  invisible(TRUE)
}

validate_review_batch_contract <- function(x) {
  require_contract_fields(x, "review_batches")
  if (!is_scalar_text(x$batch_id)) stop("batch_id is required", call. = FALSE)
  if (!is_scalar_text(x$workflow)) stop("workflow is required", call. = FALSE)
  if (!is_sha256(x$queue_sha256)) stop("queue_sha256 must be SHA-256", call. = FALSE)
  if (!is_scalar_text(x$status) || !x$status %in% ADJUDICATION_BATCH_STATES) {
    stop("Invalid batch status", call. = FALSE)
  }
  if (!is_scalar_text(x$created_at_utc)) stop("created_at_utc is required", call. = FALSE)
  if (!is_scalar_text(x$locked_at_utc, allow_empty = TRUE)) {
    stop("locked_at_utc must be a scalar value", call. = FALSE)
  }
  if (identical(x$status, "locked") && !nzchar(as.character(x$locked_at_utc))) {
    stop("locked batches require locked_at_utc", call. = FALSE)
  }
  invisible(TRUE)
}

validate_review_case_contract <- function(x) {
  require_contract_fields(x, "review_cases")
  for (nm in c("case_id", "record_id", "batch_id", "case_json")) {
    if (!is_scalar_text(x[[nm]])) stop(nm, " is required", call. = FALSE)
  }
  idx <- suppressWarnings(as.integer(x$case_index))
  if (length(idx) != 1L || is.na(idx) || idx < 1L) {
    stop("case_index must be a positive integer", call. = FALSE)
  }
  invisible(TRUE)
}

validate_assignment_contract <- function(x) {
  require_contract_fields(x, "assignments")
  for (nm in c("assignment_id", "case_id", "user_id", "blind_group")) {
    if (!is_scalar_text(x[[nm]])) stop(nm, " is required", call. = FALSE)
  }
  if (!is_scalar_text(x$status) || !x$status %in% ADJUDICATION_ASSIGNMENT_STATES) {
    stop("Invalid assignment status", call. = FALSE)
  }
  invisible(TRUE)
}

validate_decision_event_contract <- function(x) {
  require_contract_fields(x, "decision_events")
  for (nm in c("decision_id", "case_id", "user_id", "event_at_utc")) {
    if (!is_scalar_text(x[[nm]])) stop(nm, " is required", call. = FALSE)
  }
  if (!is_scalar_text(x$decision) || !x$decision %in% ADJUDICATION_DECISIONS) {
    stop("Invalid decision", call. = FALSE)
  }
  version <- suppressWarnings(as.integer(x$version))
  if (length(version) != 1L || is.na(version) || version < 1L) {
    stop("version must be a positive integer", call. = FALSE)
  }
  if (!is_scalar_logical(x$active)) stop("active must be TRUE or FALSE", call. = FALSE)
  if (!is_sha256(x$queue_sha256)) stop("queue_sha256 must be SHA-256", call. = FALSE)
  if (!is_scalar_text(x$supersedes_decision_id, allow_empty = TRUE)) {
    stop("supersedes_decision_id must be a scalar value", call. = FALSE)
  }
  if (version == 1L && nzchar(as.character(x$supersedes_decision_id))) {
    stop("Version 1 decision cannot supersede another decision", call. = FALSE)
  }
  if (version > 1L && !nzchar(as.character(x$supersedes_decision_id))) {
    stop("Revised decisions must identify the superseded decision", call. = FALSE)
  }
  invisible(TRUE)
}

validate_batch_transition <- function(from, to) {
  if (!from %in% ADJUDICATION_BATCH_STATES) stop("Invalid source batch state", call. = FALSE)
  if (!to %in% ADJUDICATION_BATCH_STATES) stop("Invalid target batch state", call. = FALSE)

  allowed <- list(
    ready = c("running", "failed"),
    running = c("awaiting_review", "ready_for_export", "failed"),
    awaiting_review = c("ready_for_export", "failed"),
    ready_for_export = c("export_pending", "failed"),
    export_pending = c("locked", "ready_for_export", "failed"),
    locked = character(),
    failed = c("ready", "running", "awaiting_review", "ready_for_export")
  )

  if (!to %in% allowed[[from]]) {
    stop("Invalid batch transition: ", from, " -> ", to, call. = FALSE)
  }
  invisible(TRUE)
}
