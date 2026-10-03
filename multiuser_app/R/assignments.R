ASSIGNMENT_MODES <- c(
  shared_work_pool = "shared_work_pool",
  independent_blind_review = "independent_blind_review",
  single_reviewer = "single_reviewer"
)

assignment_mode_for <- function(workflow, task_type) {
  workflow <- as.character(workflow)
  task_type <- as.character(task_type)

  if (identical(workflow, "04") && identical(task_type, "manual_screening")) {
    return(ASSIGNMENT_MODES[["independent_blind_review"]])
  }
  if (task_type %in% c("deduplication", "enrichment")) {
    return(ASSIGNMENT_MODES[["shared_work_pool"]])
  }
  ASSIGNMENT_MODES[["single_reviewer"]]
}

normalise_assignment_row <- function(x) {
  required <- ADJUDICATION_SCHEMA$assignments
  out <- setNames(vector("list", length(required)), required)
  for (nm in required) {
    value <- x[[nm]]
    if (is.null(value) || !length(value) || is.na(value[[1L]])) value <- ""
    out[[nm]] <- trimws(as.character(value[[1L]]))
  }
  out
}

validate_assignment_registry <- function(assignments) {
  if (is.null(assignments)) assignments <- list()
  if (!is.list(assignments)) stop("Assignment registry must be a list", call. = FALSE)
  if (!length(assignments)) return(invisible(TRUE))

  assignments <- lapply(assignments, normalise_assignment_row)
  invisible(lapply(assignments, validate_assignment_contract))

  ids <- vapply(assignments, function(x) x$assignment_id, character(1))
  if (anyDuplicated(ids)) stop("Assignment registry contains duplicate assignment_id", call. = FALSE)

  keys <- vapply(
    assignments,
    function(x) paste(x$workflow, x$task_type, x$batch_id, x$case_id, x$user_id, sep = "|"),
    character(1)
  )
  if (anyDuplicated(keys)) {
    stop("Assignment registry contains duplicate case/user assignments", call. = FALSE)
  }
  invisible(TRUE)
}

assignments_for_batch <- function(assignments, workflow, batch_id, task_type = NULL) {
  validate_assignment_registry(assignments)
  Filter(
    function(x) {
      a <- normalise_assignment_row(x)
      workflow_match <- identical(a$workflow, as.character(workflow))
      batch_match <- identical(a$batch_id, as.character(batch_id))
      task_match <- is.null(task_type) || identical(a$task_type, as.character(task_type))
      workflow_match && batch_match && task_match
    },
    assignments
  )
}

assignment_mode_active <- function(assignments, workflow, batch_id, task_type = NULL) {
  length(assignments_for_batch(assignments, workflow, batch_id, task_type)) > 0L
}

assignments_for_user <- function(assignments, workflow, batch_id, user_id, task_type = NULL) {
  xs <- assignments_for_batch(assignments, workflow, batch_id, task_type)
  Filter(
    function(x) identical(normalise_assignment_row(x)$user_id, as.character(user_id)),
    xs
  )
}

decision_case_id <- function(x) {
  as.character(x$case_id %||% x$review_case_id %||% x$record_id %||% "")
}

decision_user_id <- function(x) {
  as.character(x$user_id %||% x$reviewer %||% "")
}

case_authoritative_event <- function(events, case_id) {
  hits <- Filter(function(x) identical(decision_case_id(x), as.character(case_id)), events %||% list())
  if (!length(hits)) return(NULL)
  times <- vapply(hits, function(x) as.character(x$event_at_utc %||% x$resolved_at_utc %||% ""), character(1))
  versions <- vapply(hits, function(x) {
    z <- suppressWarnings(as.integer(x$version %||% 1L))
    if (is.na(z)) 1L else z
  }, integer(1))
  hits[[order(times, versions, seq_along(hits), decreasing = TRUE)[[1L]]]]
}

cases_for_assignment_user <- function(
  cases,
  assignments,
  workflow,
  batch_id,
  user,
  task_type = NULL,
  active_events = list()
) {
  if (!length(cases)) return(list())
  batch_assignments <- assignments_for_batch(assignments, workflow, batch_id, task_type)
  if (!length(batch_assignments)) return(cases)

  if (user_can(user, "manage_assignments")) return(cases)

  user_id <- as.character(normalise_user_row(user)$user_id)
  assigned <- assignments_for_user(assignments, workflow, batch_id, user_id, task_type)
  allowed <- unique(vapply(assigned, function(x) normalise_assignment_row(x)$case_id, character(1)))
  mode <- assignment_mode_for(workflow, task_type %||% normalise_assignment_row(batch_assignments[[1L]])$task_type)

  Filter(
    function(x) {
      case_id <- as.character(x$review_case_id %||% x$case_id %||% x$record_id %||% "")
      if (!case_id %in% allowed) return(FALSE)
      if (!identical(mode, ASSIGNMENT_MODES[["shared_work_pool"]])) return(TRUE)

      resolved <- case_authoritative_event(active_events, case_id)
      is.null(resolved) || identical(decision_user_id(resolved), user_id)
    },
    cases
  )
}

decision_events_for_user <- function(events, user_id) {
  if (!length(events)) return(list())
  Filter(
    function(x) identical(decision_user_id(x), as.character(user_id)),
    events
  )
}

assignment_progress <- function(
  assignments,
  active_events,
  users,
  workflow = NULL,
  batch_id = NULL,
  task_type = NULL
) {
  validate_assignment_registry(assignments)
  xs <- assignments
  if (!is.null(workflow) && !is.null(batch_id)) {
    xs <- assignments_for_batch(assignments, workflow, batch_id, task_type)
  }
  if (!length(xs)) {
    return(list(
      assigned = 0L,
      completed = 0L,
      resolved_elsewhere = 0L,
      remaining = 0L,
      cases = 0L,
      by_user = list()
    ))
  }

  events <- active_events %||% list()
  first <- normalise_assignment_row(xs[[1L]])
  mode <- assignment_mode_for(first$workflow, first$task_type)

  effective <- lapply(xs, function(x) {
    a <- normalise_assignment_row(x)
    if (identical(mode, ASSIGNMENT_MODES[["independent_blind_review"]])) {
      key_event <- Filter(
        function(e) identical(decision_case_id(e), a$case_id) && identical(decision_user_id(e), a$user_id),
        events
      )
      a$effective_status <- if (length(key_event)) "complete" else "assigned"
    } else {
      resolved <- case_authoritative_event(events, a$case_id)
      if (is.null(resolved)) {
        a$effective_status <- "assigned"
      } else if (identical(decision_user_id(resolved), a$user_id)) {
        a$effective_status <- "complete"
      } else {
        a$effective_status <- "resolved_elsewhere"
      }
    }
    a
  })

  user_ids <- unique(vapply(effective, function(x) x$user_id, character(1)))
  by_user <- lapply(user_ids, function(uid) {
    ua <- Filter(function(x) identical(x$user_id, uid), effective)
    completed <- sum(vapply(ua, function(x) identical(x$effective_status, "complete"), logical(1)))
    released <- sum(vapply(ua, function(x) identical(x$effective_status, "resolved_elsewhere"), logical(1)))
    remaining <- sum(vapply(ua, function(x) identical(x$effective_status, "assigned"), logical(1)))
    user <- find_user_by_id(users, uid, require_active = FALSE)
    user_events <- Filter(function(x) identical(decision_user_id(x), uid), events)
    last_activity <- ""
    if (length(user_events)) {
      times <- vapply(user_events, function(x) as.character(x$event_at_utc %||% x$resolved_at_utc %||% ""), character(1))
      times <- times[nzchar(times)]
      if (length(times)) last_activity <- max(times)
    }
    resolved_count <- completed + released
    list(
      user_id = uid,
      display_name = if (is.null(user)) uid else user$display_name,
      role = if (is.null(user)) "" else user$role,
      assigned = length(ua),
      completed = completed,
      resolved_elsewhere = released,
      remaining = remaining,
      progress = if (length(ua)) resolved_count / length(ua) else 0,
      workflows = unique(vapply(ua, function(x) x$workflow, character(1))),
      task_types = unique(vapply(ua, function(x) x$task_type, character(1))),
      last_activity = last_activity
    )
  })

  completed <- sum(vapply(effective, function(x) identical(x$effective_status, "complete"), logical(1)))
  released <- sum(vapply(effective, function(x) identical(x$effective_status, "resolved_elsewhere"), logical(1)))
  remaining <- sum(vapply(effective, function(x) identical(x$effective_status, "assigned"), logical(1)))

  list(
    assigned = length(effective),
    completed = completed,
    resolved_elsewhere = released,
    remaining = remaining,
    cases = length(unique(vapply(effective, function(x) x$case_id, character(1)))),
    by_user = by_user,
    mode = mode
  )
}


resolve_fixture_assignment_users <- function(assignments, users) {
  if (!length(assignments)) return(list())
  validate_user_registry(users)

  active <- active_users(users)
  admins <- Filter(function(x) identical(normalise_user_row(x)$role, "administrator"), active)
  reviewers <- Filter(function(x) identical(normalise_user_row(x)$role, "reviewer"), active)
  reviewers <- reviewers[order(vapply(reviewers, function(x) normalise_user_row(x)$display_name, character(1)))]

  resolve_id <- function(id) {
    if (identical(id, "fixture:administrator")) {
      if (!length(admins)) stop("Local assignment fixture requires an active administrator", call. = FALSE)
      return(normalise_user_row(admins[[1L]])$user_id)
    }
    if (grepl("^fixture:reviewer:[0-9]+$", id)) {
      n <- suppressWarnings(as.integer(sub("^fixture:reviewer:", "", id)))
      if (is.na(n) || n < 1L || n > length(reviewers)) {
        stop("Local assignment fixture reviewer alias cannot be resolved", call. = FALSE)
      }
      return(normalise_user_row(reviewers[[n]])$user_id)
    }
    id
  }

  resolved <- lapply(assignments, function(x) {
    a <- normalise_assignment_row(x)
    a$user_id <- resolve_id(a$user_id)
    a
  })
  validate_assignment_registry(resolved)
  resolved
}
