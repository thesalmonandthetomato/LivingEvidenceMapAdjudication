w04_manual_review_mode <- function(assignments, batch_id) {
  xs <- active_assignments_for_batch(
    assignments %||% list(), "04", as.character(batch_id), "manual_screening"
  )
  if (!length(xs)) return("")

  groups <- unique(vapply(
    xs,
    function(x) normalise_assignment_row(x)$blind_group,
    character(1)
  ))

  if ("w04-validation-set" %in% groups && "w04-reviewer-consistency" %in% groups) {
    return("mixed")
  }
  if ("w04-validation-set" %in% groups) return("validation_set")
  if ("w04-reviewer-consistency" %in% groups) return("reviewer_consistency")
  ""
}

w04_validation_lifecycle <- function(
  cases,
  assignments,
  decisions,
  batch_id,
  queue_sha256
) {
  cases <- cases %||% list()
  assignments <- assignments %||% list()
  decisions <- decisions %||% list()
  batch_id <- as.character(batch_id %||% "")
  queue_sha256 <- tolower(as.character(queue_sha256 %||% ""))

  mode <- w04_manual_review_mode(assignments, batch_id)
  case_ids <- unique(vapply(
    cases,
    function(x) as.character(x$review_case_id %||% x$case_id %||% x$record_id %||% ""),
    character(1)
  ))
  case_ids <- case_ids[nzchar(case_ids)]

  result <- list(
    ready = FALSE,
    mode = mode,
    batch_id = batch_id,
    queue_sha256 = queue_sha256,
    cases = length(case_ids),
    assigned_cases = 0L,
    decided_cases = 0L,
    unassigned_case_ids = case_ids,
    missing_decision_case_ids = case_ids,
    duplicate_assignment_case_ids = character(),
    duplicate_decision_case_ids = character(),
    invalid_decision_case_ids = character(),
    wrong_sha_case_ids = character(),
    mismatched_reviewer_case_ids = character(),
    mismatched_record_case_ids = character(),
    reason = ""
  )

  if (!nzchar(batch_id) || !grepl("^[0-9a-f]{64}$", queue_sha256)) {
    result$reason <- "invalid_batch_identity"
    return(result)
  }
  if (!length(case_ids) || length(case_ids) != length(cases)) {
    result$reason <- "invalid_case_identity"
    return(result)
  }
  if (!identical(mode, "validation_set")) {
    result$reason <- if (identical(mode, "reviewer_consistency")) {
      "reviewer_consistency_not_finalisable_as_validation"
    } else if (identical(mode, "mixed")) {
      "mixed_manual_review_modes"
    } else {
      "validation_set_not_assigned"
    }
    return(result)
  }

  batch_assignments <- active_assignments_for_batch(
    assignments, "04", batch_id, "manual_screening"
  )
  assignment_ids <- vapply(
    batch_assignments,
    function(x) normalise_assignment_row(x)$case_id,
    character(1)
  )
  assignment_ids <- assignment_ids[assignment_ids %in% case_ids]
  assignment_counts <- table(factor(assignment_ids, levels = case_ids))
  result$assigned_cases <- as.integer(sum(assignment_counts > 0L))
  result$unassigned_case_ids <- case_ids[assignment_counts == 0L]
  result$duplicate_assignment_case_ids <- case_ids[assignment_counts > 1L]

  queue_decisions <- Filter(
    function(x) {
      identical(
        tolower(as.character(x$queue_sha256 %||% "")),
        queue_sha256
      )
    },
    decisions
  )
  decision_ids <- vapply(queue_decisions, decision_case_id, character(1))
  in_queue <- decision_ids %in% case_ids
  queue_decisions <- queue_decisions[in_queue]
  decision_ids <- decision_ids[in_queue]

  decision_counts <- table(factor(decision_ids, levels = case_ids))
  result$decided_cases <- as.integer(sum(decision_counts > 0L))
  result$missing_decision_case_ids <- case_ids[decision_counts == 0L]
  result$duplicate_decision_case_ids <- case_ids[decision_counts > 1L]

  invalid <- vapply(
    queue_decisions,
    function(x) !tolower(as.character(x$decision %||% "")) %in% c("retain", "exclude", "uncertain"),
    logical(1)
  )
  result$invalid_decision_case_ids <- unique(decision_ids[invalid])

  assignment_user_by_case <- setNames(
    vapply(case_ids, function(cid) {
      hits <- Filter(
        function(x) identical(normalise_assignment_row(x)$case_id, cid),
        batch_assignments
      )
      if (length(hits) != 1L) return("")
      normalise_assignment_row(hits[[1L]])$user_id
    }, character(1)),
    case_ids
  )
  decision_user_by_case <- setNames(
    vapply(case_ids, function(cid) {
      hits <- Filter(function(x) identical(decision_case_id(x), cid), queue_decisions)
      if (length(hits) != 1L) return("")
      decision_user_id(hits[[1L]])
    }, character(1)),
    case_ids
  )
  result$mismatched_reviewer_case_ids <- case_ids[
    nzchar(assignment_user_by_case[case_ids]) &
      nzchar(decision_user_by_case[case_ids]) &
      assignment_user_by_case[case_ids] != decision_user_by_case[case_ids]
  ]

  case_record_ids <- setNames(
    vapply(cases, function(x) as.character(x$record_id %||% ""), character(1)),
    vapply(
      cases,
      function(x) as.character(x$review_case_id %||% x$case_id %||% x$record_id %||% ""),
      character(1)
    )
  )
  decision_record_by_case <- setNames(
    vapply(case_ids, function(cid) {
      hits <- Filter(function(x) identical(decision_case_id(x), cid), queue_decisions)
      if (length(hits) != 1L) return("")
      as.character(hits[[1L]]$record_id %||% "")
    }, character(1)),
    case_ids
  )
  result$mismatched_record_case_ids <- case_ids[
    nzchar(case_record_ids[case_ids]) &
      nzchar(decision_record_by_case[case_ids]) &
      case_record_ids[case_ids] != decision_record_by_case[case_ids]
  ]

  all_case_decisions <- Filter(
    function(x) decision_case_id(x) %in% case_ids,
    decisions
  )
  wrong_sha <- Filter(
    function(x) {
      !identical(
        tolower(as.character(x$queue_sha256 %||% "")),
        queue_sha256
      )
    },
    all_case_decisions
  )
  result$wrong_sha_case_ids <- unique(vapply(wrong_sha, decision_case_id, character(1)))

  blockers <- c(
    result$unassigned_case_ids,
    result$missing_decision_case_ids,
    result$duplicate_assignment_case_ids,
    result$duplicate_decision_case_ids,
    result$invalid_decision_case_ids,
    result$wrong_sha_case_ids,
    result$mismatched_reviewer_case_ids,
    result$mismatched_record_case_ids
  )
  result$ready <- !length(unique(blockers))
  result$reason <- if (isTRUE(result$ready)) "ready" else "incomplete_or_ambiguous"
  result
}
