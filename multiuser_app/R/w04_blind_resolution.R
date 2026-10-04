w04_blind_case_outcomes <- function(
  cases,
  assignments,
  decisions,
  batch_id,
  task_type = "manual_screening"
) {
  cases <- cases %||% list()
  decisions <- decisions %||% list()
  active_assignments <- active_assignments_for_batch(
    assignments %||% list(),
    "04",
    batch_id,
    task_type
  )

  case_ids <- vapply(
    cases,
    function(x) as.character(x$review_case_id %||% x$case_id %||% x$record_id %||% ""),
    character(1)
  )

  lapply(seq_along(cases), function(i) {
    case_id <- case_ids[[i]]
    case_assignments <- Filter(
      function(x) identical(normalise_assignment_row(x)$case_id, case_id),
      active_assignments
    )
    assigned_users <- unique(vapply(
      case_assignments,
      function(x) normalise_assignment_row(x)$user_id,
      character(1)
    ))
    assigned_users <- assigned_users[nzchar(assigned_users)]

    case_decisions <- Filter(
      function(x) {
        identical(decision_case_id(x), case_id) &&
          decision_user_id(x) %in% assigned_users
      },
      decisions
    )

    by_user <- lapply(assigned_users, function(uid) {
      hits <- Filter(
        function(x) identical(decision_user_id(x), uid),
        case_decisions
      )
      event <- if (length(hits)) hits[[1L]] else NULL
      list(
        user_id = uid,
        complete = !is.null(event),
        decision = if (is.null(event)) "" else as.character(event$decision %||% ""),
        event = event
      )
    })
    names(by_user) <- assigned_users

    completed_users <- assigned_users[vapply(
      by_user,
      function(x) isTRUE(x$complete),
      logical(1)
    )]
    all_complete <- length(assigned_users) > 0L &&
      length(completed_users) == length(assigned_users)

    completed_decisions <- if (length(completed_users)) {
      vapply(
        by_user[completed_users],
        function(x) as.character(x$decision %||% ""),
        character(1)
      )
    } else character()

    substantive <- completed_decisions %in% c("retain", "exclude")
    exact_agreement <- all_complete &&
      length(completed_decisions) > 0L &&
      all(substantive) &&
      length(unique(completed_decisions)) == 1L

    status <- if (!length(assigned_users)) {
      "unassigned"
    } else if (!all_complete) {
      "pending"
    } else if (exact_agreement) {
      "agreement"
    } else {
      "conflict"
    }

    list(
      case_id = case_id,
      record_id = as.character(cases[[i]]$record_id %||% ""),
      status = status,
      assigned_user_ids = assigned_users,
      completed_user_ids = completed_users,
      reviewer_decisions = by_user,
      final_decision = if (exact_agreement) unique(completed_decisions)[[1L]] else "",
      case = cases[[i]]
    )
  })
}

w04_blind_agreements <- function(outcomes) {
  Filter(function(x) identical(x$status, "agreement"), outcomes %||% list())
}

w04_blind_conflicts <- function(outcomes) {
  Filter(function(x) identical(x$status, "conflict"), outcomes %||% list())
}

w04_blind_pending <- function(outcomes) {
  Filter(function(x) identical(x$status, "pending"), outcomes %||% list())
}
