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

  active_assignments <- Filter(
    function(x) !identical(normalise_assignment_row(x)$status, "cancelled"),
    assignments
  )
  keys <- vapply(
    active_assignments,
    function(x) paste(x$workflow, x$task_type, x$batch_id, x$case_id, x$user_id, sep = "|"),
    character(1)
  )
  if (anyDuplicated(keys)) {
    stop("Assignment registry contains duplicate active case/user assignments", call. = FALSE)
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

active_assignments <- function(assignments) {
  Filter(
    function(x) !identical(normalise_assignment_row(x)$status, "cancelled"),
    assignments %||% list()
  )
}

active_assignments_for_batch <- function(assignments, workflow, batch_id, task_type = NULL) {
  active_assignments(assignments_for_batch(assignments, workflow, batch_id, task_type))
}

assignment_mode_active <- function(assignments, workflow, batch_id, task_type = NULL) {
  length(active_assignments_for_batch(assignments, workflow, batch_id, task_type)) > 0L
}

assignments_for_user <- function(assignments, workflow, batch_id, user_id, task_type = NULL) {
  xs <- active_assignments_for_batch(assignments, workflow, batch_id, task_type)
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

decision_resolves_case <- function(x) {
  decision <- tolower(trimws(as.character(x$decision %||% "")))
  if (nzchar(decision)) return(!identical(decision, "uncertain"))
  issue_json <- trimws(as.character(x$issue_decisions_json %||% ""))
  if (nzchar(issue_json)) return(TRUE)
  FALSE
}

case_authoritative_event <- function(events, case_id) {
  hits <- Filter(
    function(x) identical(decision_case_id(x), as.character(case_id)) && decision_resolves_case(x),
    events %||% list()
  )
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
  batch_assignments <- active_assignments_for_batch(assignments, workflow, batch_id, task_type)
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
    xs <- active_assignments_for_batch(assignments, workflow, batch_id, task_type)
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


assignment_case_ids <- function(assignments) {
  if (!length(assignments)) return(character())
  unique(vapply(assignments, function(x) normalise_assignment_row(x)$case_id, character(1)))
}

unresolved_unassigned_case_ids <- function(
  cases,
  assignments,
  active_events,
  workflow,
  batch_id,
  task_type
) {
  if (!length(cases)) return(character())
  case_ids <- vapply(
    cases,
    function(x) as.character(x$review_case_id %||% x$case_id %||% x$record_id %||% ""),
    character(1)
  )
  case_ids <- case_ids[nzchar(case_ids)]
  resolving_events <- Filter(decision_resolves_case, active_events %||% list())
  resolved_ids <- if (length(resolving_events)) {
    unique(vapply(resolving_events, decision_case_id, character(1)))
  } else character()
  batch_assignments <- active_assignments_for_batch(assignments, workflow, batch_id, task_type)
  assigned_ids <- assignment_case_ids(batch_assignments)
  case_ids[!case_ids %in% resolved_ids & !case_ids %in% assigned_ids]
}

unresolved_shared_pool_case_ids_for_users <- function(
  cases,
  assignments,
  active_events,
  workflow,
  batch_id,
  task_type,
  user_ids
) {
  if (!length(cases)) return(character())

  user_ids <- unique(as.character(user_ids))
  user_ids <- user_ids[nzchar(user_ids)]
  if (!length(user_ids)) return(character())

  case_ids <- vapply(
    cases,
    function(x) as.character(x$review_case_id %||% x$case_id %||% x$record_id %||% ""),
    character(1)
  )
  case_ids <- unique(case_ids[nzchar(case_ids)])

  resolving_events <- Filter(decision_resolves_case, active_events %||% list())
  resolved_ids <- if (length(resolving_events)) {
    unique(vapply(resolving_events, decision_case_id, character(1)))
  } else character()

  active_batch <- active_assignments_for_batch(
    assignments, workflow, batch_id, task_type
  )

  unresolved <- case_ids[!case_ids %in% resolved_ids]
  unresolved[vapply(
    unresolved,
    function(cid) {
      assigned_users <- unique(vapply(
        Filter(
          function(x) identical(normalise_assignment_row(x)$case_id, cid),
          active_batch
        ),
        function(x) normalise_assignment_row(x)$user_id,
        character(1)
      ))
      any(!user_ids %in% assigned_users)
    },
    logical(1)
  )]
}


plan_shared_pool_assignment <- function(
  cases,
  assignments,
  active_events,
  workflow,
  batch_id,
  task_type,
  user_ids,
  allocation_type = c("number", "percentage"),
  amount
) {
  allocation_type <- match.arg(allocation_type)
  user_ids <- unique(as.character(user_ids))
  user_ids <- user_ids[nzchar(user_ids)]
  if (!length(user_ids)) stop("Select at least one reviewer", call. = FALSE)

  amount <- suppressWarnings(as.numeric(amount))
  if (is.na(amount) || amount <= 0) stop("Assignment amount must be greater than zero", call. = FALSE)
  if (identical(allocation_type, "percentage") && amount > 100) {
    stop("Percentage cannot exceed 100", call. = FALSE)
  }

  eligible <- unresolved_shared_pool_case_ids_for_users(
    cases, assignments, active_events, workflow, batch_id, task_type, user_ids
  )
  n_available <- length(eligible)
  if (!n_available) {
    return(list(
      new_assignments = list(),
      available = 0L,
      requested = 0L,
      allocated = 0L,
      by_user = setNames(integer(length(user_ids)), user_ids),
      case_ids = character()
    ))
  }

  requested <- if (identical(allocation_type, "percentage")) {
    n <- floor(n_available * amount / 100 + 0.5)
    if (amount > 0 && n_available > 0) max(1L, n) else 0L
  } else {
    as.integer(floor(amount))
  }
  requested <- max(0L, min(as.integer(requested), n_available))
  chosen <- if (requested) eligible[seq_len(requested)] else character()

  rows <- list()
  by_user <- setNames(integer(length(user_ids)), user_ids)
  active_batch <- active_assignments_for_batch(
    assignments, workflow, batch_id, task_type
  )
  next_user_index <- 1L

  if (length(chosen)) {
    for (cid in chosen) {
      already_assigned <- unique(vapply(
        Filter(
          function(x) identical(normalise_assignment_row(x)$case_id, cid),
          active_batch
        ),
        function(x) normalise_assignment_row(x)$user_id,
        character(1)
      ))
      candidate_users <- user_ids[!user_ids %in% already_assigned]
      if (!length(candidate_users)) next

      rotated <- c(
        user_ids[seq.int(next_user_index, length(user_ids))],
        if (next_user_index > 1L) user_ids[seq_len(next_user_index - 1L)] else character()
      )
      uid <- rotated[rotated %in% candidate_users][[1L]]
      next_user_index <- match(uid, user_ids) %% length(user_ids) + 1L

      prior_same_key <- Filter(
        function(x) {
          a <- normalise_assignment_row(x)
          identical(a$workflow, as.character(workflow)) &&
            identical(a$task_type, as.character(task_type)) &&
            identical(a$batch_id, as.character(batch_id)) &&
            identical(a$case_id, cid) &&
            identical(a$user_id, uid)
        },
        assignments %||% list()
      )
      generation <- length(prior_same_key) + 1L
      assignment_id <- paste0(
        "asg-",
        substr(
          digest::digest(
            paste(workflow, task_type, batch_id, cid, uid, generation, sep = "|"),
            algo = "sha256",
            serialize = FALSE
          ),
          1L, 24L
        )
      )
      rows[[length(rows) + 1L]] <- list(
        assignment_id = assignment_id,
        workflow = as.character(workflow),
        task_type = as.character(task_type),
        batch_id = as.character(batch_id),
        case_id = cid,
        user_id = uid,
        blind_group = paste0("pool-", workflow, "-", task_type),
        status = "assigned"
      )
      by_user[[uid]] <- by_user[[uid]] + 1L
    }
  }

  combined <- c(assignments, rows)
  validate_assignment_registry(combined)

  list(
    new_assignments = rows,
    available = n_available,
    requested = requested,
    allocated = length(rows),
    by_user = by_user,
    case_ids = chosen
  )
}


cancellable_assignments <- function(
  assignments,
  active_events,
  workflow,
  batch_id,
  task_type
) {
  xs <- active_assignments_for_batch(assignments, workflow, batch_id, task_type)
  Filter(
    function(x) {
      a <- normalise_assignment_row(x)
      resolved <- case_authoritative_event(active_events, a$case_id)
      is.null(resolved)
    },
    xs
  )
}

cancel_assignment_ids <- function(
  assignments,
  assignment_ids,
  active_events,
  workflow,
  batch_id,
  task_type
) {
  assignment_ids <- unique(as.character(assignment_ids))
  assignment_ids <- assignment_ids[nzchar(assignment_ids)]
  if (!length(assignment_ids)) stop("Select at least one assignment to remove", call. = FALSE)

  cancellable <- cancellable_assignments(
    assignments, active_events, workflow, batch_id, task_type
  )
  allowed <- vapply(
    cancellable,
    function(x) normalise_assignment_row(x)$assignment_id,
    character(1)
  )
  blocked <- setdiff(assignment_ids, allowed)
  if (length(blocked)) {
    stop("One or more selected assignments are completed, resolved, or no longer active", call. = FALSE)
  }

  changed <- 0L
  out <- lapply(assignments, function(x) {
    a <- normalise_assignment_row(x)
    if (a$assignment_id %in% assignment_ids && !identical(a$status, "cancelled")) {
      a$status <- "cancelled"
      changed <<- changed + 1L
    }
    a
  })
  validate_assignment_registry(out)
  list(assignments = out, cancelled = changed)
}


cancellable_assignments_for_user <- function(
  assignments,
  active_events,
  workflow,
  batch_id,
  task_type,
  user_id
) {
  xs <- cancellable_assignments(
    assignments, active_events, workflow, batch_id, task_type
  )
  Filter(
    function(x) identical(normalise_assignment_row(x)$user_id, as.character(user_id)),
    xs
  )
}

cancel_user_assignments <- function(
  assignments,
  user_id,
  active_events,
  workflow,
  batch_id,
  task_type
) {
  user_id <- as.character(user_id %||% "")
  if (!nzchar(user_id)) stop("Select a reviewer", call. = FALSE)

  xs <- cancellable_assignments_for_user(
    assignments, active_events, workflow, batch_id, task_type, user_id
  )
  if (!length(xs)) {
    stop("This reviewer has no unfinished assignments to remove", call. = FALSE)
  }

  ids <- vapply(xs, function(x) normalise_assignment_row(x)$assignment_id, character(1))
  cancel_assignment_ids(
    assignments,
    ids,
    active_events,
    workflow,
    batch_id,
    task_type
  )
}


plan_single_reviewer_assignment <- function(
  cases,
  assignments,
  active_events,
  workflow,
  batch_id,
  task_type,
  user_ids,
  allocation_type = c("number", "percentage"),
  amount
) {
  allocation_type <- match.arg(allocation_type)
  user_ids <- unique(as.character(user_ids))
  user_ids <- user_ids[nzchar(user_ids)]
  if (!length(user_ids)) stop("Select at least one reviewer", call. = FALSE)

  amount <- suppressWarnings(as.numeric(amount))
  if (is.na(amount) || amount <= 0) {
    stop("Assignment amount must be greater than zero", call. = FALSE)
  }
  if (identical(allocation_type, "percentage") && amount > 100) {
    stop("Percentage cannot exceed 100", call. = FALSE)
  }

  eligible <- unresolved_unassigned_case_ids(
    cases, assignments, active_events, workflow, batch_id, task_type
  )
  n_available <- length(eligible)
  requested <- if (identical(allocation_type, "percentage")) {
    n <- floor(n_available * amount / 100 + 0.5)
    if (amount > 0 && n_available > 0) max(1L, n) else 0L
  } else {
    as.integer(floor(amount))
  }
  requested <- max(0L, min(as.integer(requested), n_available))
  chosen <- if (requested) eligible[seq_len(requested)] else character()

  rows <- list()
  by_user <- setNames(integer(length(user_ids)), user_ids)
  if (length(chosen)) {
    for (i in seq_along(chosen)) {
      uid <- user_ids[[((i - 1L) %% length(user_ids)) + 1L]]
      cid <- chosen[[i]]
      prior_same_key <- Filter(
        function(x) {
          a <- normalise_assignment_row(x)
          identical(a$workflow, as.character(workflow)) &&
            identical(a$task_type, as.character(task_type)) &&
            identical(a$batch_id, as.character(batch_id)) &&
            identical(a$case_id, cid) &&
            identical(a$user_id, uid)
        },
        assignments %||% list()
      )
      generation <- length(prior_same_key) + 1L
      assignment_id <- paste0(
        "asg-",
        substr(
          digest::digest(
            paste(workflow, task_type, batch_id, cid, uid, generation, sep = "|"),
            algo = "sha256",
            serialize = FALSE
          ),
          1L, 24L
        )
      )
      rows[[length(rows) + 1L]] <- list(
        assignment_id = assignment_id,
        workflow = as.character(workflow),
        task_type = as.character(task_type),
        batch_id = as.character(batch_id),
        case_id = cid,
        user_id = uid,
        blind_group = paste0("single-", workflow, "-", task_type),
        status = "assigned"
      )
      by_user[[uid]] <- by_user[[uid]] + 1L
    }
  }

  validate_assignment_registry(c(assignments, rows))
  list(
    new_assignments = rows,
    available = n_available,
    requested = requested,
    allocated = length(rows),
    by_user = by_user,
    case_ids = chosen
  )
}


plan_workflow_assignment <- function(
  cases,
  assignments,
  active_events,
  workflow,
  batch_id,
  task_type,
  user_ids,
  allocation_type = c("number", "percentage"),
  amount
) {
  mode <- assignment_mode_for(workflow, task_type)

  if (identical(mode, ASSIGNMENT_MODES[["shared_work_pool"]])) {
    return(plan_shared_pool_assignment(
      cases = cases,
      assignments = assignments,
      active_events = active_events,
      workflow = workflow,
      batch_id = batch_id,
      task_type = task_type,
      user_ids = user_ids,
      allocation_type = allocation_type,
      amount = amount
    ))
  }

  plan_single_reviewer_assignment(
    cases = cases,
    assignments = assignments,
    active_events = active_events,
    workflow = workflow,
    batch_id = batch_id,
    task_type = task_type,
    user_ids = user_ids,
    allocation_type = allocation_type,
    amount = amount
  )
}


user_has_active_assignment <- function(
  assignments,
  workflow,
  batch_id,
  task_type,
  case_id,
  user_id
) {
  xs <- assignments_for_user(
    assignments,
    workflow,
    batch_id,
    user_id,
    task_type
  )
  any(vapply(
    xs,
    function(x) identical(normalise_assignment_row(x)$case_id, as.character(case_id)),
    logical(1)
  ))
}
