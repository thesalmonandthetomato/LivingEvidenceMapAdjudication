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
  if (task_type %in% c("deduplication", "enrichment", "annotation")) {
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
  mode <- assignment_mode_for(workflow, task_type %||% "")
  explicit_user_scope <- !identical(mode, ASSIGNMENT_MODES[["shared_work_pool"]])

  batch_assignments <- active_assignments_for_batch(assignments, workflow, batch_id, task_type)
  if (!length(batch_assignments)) {
    if (isTRUE(explicit_user_scope)) return(list())
    return(cases)
  }

  if (
    user_can(user, "manage_assignments") &&
    !isTRUE(explicit_user_scope)
  ) return(cases)

  user_id <- as.character(normalise_user_row(user)$user_id)
  assigned <- assignments_for_user(assignments, workflow, batch_id, user_id, task_type)
  allowed <- unique(vapply(assigned, function(x) normalise_assignment_row(x)$case_id, character(1)))

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

  # Count direct adjudications even when an administrator completed a case
  # without a formal assignment. A previously cancelled formal assignment,
  # however, must not be resurrected as an implicit assignment in reporting.
  resolving_events <- Filter(decision_resolves_case, events)
  if (length(resolving_events)) {
    all_batch_assignments <- if (!is.null(workflow) && !is.null(batch_id)) {
      assignments_for_batch(assignments, workflow, batch_id, task_type)
    } else {
      assignments
    }
    existing_keys <- vapply(
      all_batch_assignments,
      function(x) {
        a <- normalise_assignment_row(x)
        paste(a$case_id, a$user_id, sep = "|")
      },
      character(1)
    )
    implicit <- list()
    for (e in resolving_events) {
      cid <- decision_case_id(e)
      uid <- decision_user_id(e)
      key <- paste(cid, uid, sep = "|")
      if (!nzchar(cid) || !nzchar(uid) || key %in% existing_keys) next
      implicit[[length(implicit) + 1L]] <- list(
        assignment_id = paste0("implicit-", substr(
          digest::digest(paste(first$workflow, first$task_type, first$batch_id, cid, uid, sep="|"),
                         algo="sha256", serialize=FALSE),
          1L, 24L
        )),
        workflow = first$workflow,
        task_type = first$task_type,
        batch_id = first$batch_id,
        case_id = cid,
        user_id = uid,
        blind_group = paste0("implicit-", first$workflow, "-", first$task_type),
        status = "assigned"
      )
      existing_keys <- c(existing_keys, key)
    }
    xs <- c(xs, implicit)
  }

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
  allocation_type = c("number", "percentage", "all"),
  amount = NA_real_,
  allocation_strategy = c("split", "shared")
) {
  allocation_type <- match.arg(allocation_type)
  allocation_strategy <- match.arg(allocation_strategy)
  user_ids <- unique(as.character(user_ids))
  user_ids <- user_ids[nzchar(user_ids)]
  if (!length(user_ids)) stop("Select at least one reviewer", call. = FALSE)

  if (!identical(allocation_type, "all")) {
    amount <- suppressWarnings(as.numeric(amount))
    if (is.na(amount) || amount <= 0) {
      stop("Assignment amount must be greater than zero", call. = FALSE)
    }
    if (identical(allocation_type, "percentage") && amount > 100) {
      stop("Percentage cannot exceed 100", call. = FALSE)
    }
  }

  eligible <- if (identical(allocation_strategy, "split")) {
    unresolved_unassigned_case_ids(
      cases, assignments, active_events, workflow, batch_id, task_type
    )
  } else {
    unresolved_shared_pool_case_ids_for_users(
      cases, assignments, active_events, workflow, batch_id, task_type, user_ids
    )
  }

  n_available <- length(eligible)
  if (!n_available) {
    return(list(
      new_assignments = list(),
      available = 0L,
      requested = 0L,
      selected_cases = 0L,
      allocated = 0L,
      by_user = setNames(integer(length(user_ids)), user_ids),
      case_ids = character(),
      allocation_strategy = allocation_strategy,
      allocation_type = allocation_type
    ))
  }

  requested <- if (identical(allocation_type, "all")) {
    n_available
  } else if (identical(allocation_type, "percentage")) {
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

  make_assignment <- function(cid, uid) {
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
    list(
      assignment_id = assignment_id,
      workflow = as.character(workflow),
      task_type = as.character(task_type),
      batch_id = as.character(batch_id),
      case_id = cid,
      user_id = uid,
      blind_group = paste0("pool-", workflow, "-", task_type),
      status = "assigned"
    )
  }

  if (identical(allocation_strategy, "split")) {
    if (length(chosen)) {
      for (i in seq_along(chosen)) {
        uid <- user_ids[[((i - 1L) %% length(user_ids)) + 1L]]
        cid <- chosen[[i]]
        rows[[length(rows) + 1L]] <- make_assignment(cid, uid)
        by_user[[uid]] <- by_user[[uid]] + 1L
      }
    }
  } else {
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
        for (uid in user_ids[!user_ids %in% already_assigned]) {
          rows[[length(rows) + 1L]] <- make_assignment(cid, uid)
          by_user[[uid]] <- by_user[[uid]] + 1L
        }
      }
    }
  }

  combined <- c(assignments, rows)
  validate_assignment_registry(combined)

  list(
    new_assignments = rows,
    available = n_available,
    requested = requested,
    selected_cases = length(chosen),
    allocated = length(rows),
    by_user = by_user,
    case_ids = chosen,
    allocation_strategy = allocation_strategy,
    allocation_type = allocation_type
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


plan_independent_blind_assignment <- function(
  cases,
  assignments,
  active_events,
  workflow,
  batch_id,
  task_type,
  user_ids,
  allocation_type = c("number", "percentage", "all"),
  amount = NA_real_
) {
  allocation_type <- match.arg(allocation_type)
  user_ids <- unique(as.character(user_ids))
  user_ids <- user_ids[nzchar(user_ids)]
  if (length(user_ids) < 2L) {
    stop("Independent blind review requires at least two reviewers", call. = FALSE)
  }

  if (!identical(allocation_type, "all")) {
    amount <- suppressWarnings(as.numeric(amount))
    if (is.na(amount) || amount <= 0) {
      stop("Assignment amount must be greater than zero", call. = FALSE)
    }
    if (identical(allocation_type, "percentage") && amount > 100) {
      stop("Percentage cannot exceed 100", call. = FALSE)
    }
  }

  case_ids <- unique(vapply(
    cases,
    function(x) as.character(x$review_case_id %||% x$case_id %||% x$record_id %||% ""),
    character(1)
  ))
  case_ids <- case_ids[nzchar(case_ids)]

  batch_assignments <- active_assignments_for_batch(
    assignments, workflow, batch_id, task_type
  )
  completed_keys <- if (length(active_events)) {
    unique(vapply(active_events, function(e) {
      paste(decision_case_id(e), decision_user_id(e), sep = "|")
    }, character(1)))
  } else character()

  missing_users_for_case <- function(cid) {
    assigned_users <- unique(vapply(
      Filter(
        function(x) identical(normalise_assignment_row(x)$case_id, cid),
        batch_assignments
      ),
      function(x) normalise_assignment_row(x)$user_id,
      character(1)
    ))
    completed_users <- user_ids[
      paste(cid, user_ids, sep = "|") %in% completed_keys
    ]
    setdiff(user_ids, union(assigned_users, completed_users))
  }

  eligible <- case_ids[vapply(
    case_ids,
    function(cid) length(missing_users_for_case(cid)) > 0L,
    logical(1)
  )]
  n_available <- length(eligible)
  if (!n_available) {
    return(list(
      new_assignments = list(),
      available = 0L,
      requested = 0L,
      selected_cases = 0L,
      allocated = 0L,
      by_user = setNames(integer(length(user_ids)), user_ids),
      case_ids = character(),
      allocation_type = allocation_type
    ))
  }

  requested <- if (identical(allocation_type, "all")) {
    n_available
  } else if (identical(allocation_type, "percentage")) {
    n <- floor(n_available * amount / 100 + 0.5)
    if (amount > 0 && n_available > 0) max(1L, n) else 0L
  } else {
    as.integer(floor(amount))
  }
  requested <- max(0L, min(as.integer(requested), n_available))
  chosen <- if (requested) eligible[seq_len(requested)] else character()

  rows <- list()
  by_user <- setNames(integer(length(user_ids)), user_ids)
  for (cid in chosen) {
    missing_users <- missing_users_for_case(cid)
    for (uid in missing_users) {
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
        blind_group = paste0("blind-", workflow, "-", task_type),
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
    selected_cases = length(chosen),
    allocated = length(rows),
    by_user = by_user,
    case_ids = chosen,
    allocation_type = allocation_type
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


plan_w04_manual_assignment <- function(
  cases,
  assignments,
  active_events,
  batch_id,
  user_ids,
  review_mode = c("reviewer_consistency", "validation_set"),
  allocation_type = c("number", "percentage", "all"),
  amount = NA_real_
) {
  review_mode <- match.arg(review_mode)
  allocation_type <- match.arg(allocation_type)
  user_ids <- unique(as.character(user_ids))
  user_ids <- user_ids[nzchar(user_ids)]
  if (!length(user_ids)) stop("Select at least one reviewer", call. = FALSE)

  # Stable pseudo-random order keeps assignment previews reproducible while
  # avoiding dependence on the incoming search-result order.
  if (length(cases)) {
    case_ids_for_order <- vapply(
      cases,
      function(x) as.character(x$review_case_id %||% x$case_id %||% x$record_id %||% ""),
      character(1)
    )
    random_key <- vapply(
      case_ids_for_order,
      function(cid) digest::digest(
        paste("w04-manual", batch_id, review_mode, cid, sep="|"),
        algo="sha256",
        serialize=FALSE
      ),
      character(1)
    )
    cases <- cases[order(random_key, case_ids_for_order)]
  }

  if (identical(review_mode, "reviewer_consistency")) {
    if (length(user_ids) < 2L) {
      stop("Reviewer consistency requires at least two reviewers", call. = FALSE)
    }
    plan <- plan_independent_blind_assignment(
      cases = cases,
      assignments = assignments,
      active_events = active_events,
      workflow = "04",
      batch_id = batch_id,
      task_type = "manual_screening",
      user_ids = user_ids,
      allocation_type = allocation_type,
      amount = amount
    )
    if (length(plan$new_assignments)) {
      plan$new_assignments <- lapply(plan$new_assignments, function(x) {
        x$blind_group <- "w04-reviewer-consistency"
        x
      })
    }
    plan$review_mode <- review_mode
    validate_assignment_registry(c(assignments, plan$new_assignments))
    return(plan)
  }

  eligible <- unresolved_unassigned_case_ids(
    cases, assignments, active_events,
    "04", batch_id, "manual_screening"
  )
  requested_amount <- amount
  requested_type <- allocation_type
  if (identical(allocation_type, "all")) {
    requested_amount <- length(eligible)
    requested_type <- "number"
  }
  if (!length(eligible)) {
    return(list(
      new_assignments = list(),
      available = 0L,
      requested = 0L,
      selected_cases = 0L,
      allocated = 0L,
      by_user = setNames(integer(length(user_ids)), user_ids),
      case_ids = character(),
      allocation_type = allocation_type,
      review_mode = review_mode
    ))
  }

  plan <- plan_single_reviewer_assignment(
    cases = cases,
    assignments = assignments,
    active_events = active_events,
    workflow = "04",
    batch_id = batch_id,
    task_type = "manual_screening",
    user_ids = user_ids,
    allocation_type = requested_type,
    amount = requested_amount
  )
  if (length(plan$new_assignments)) {
    plan$new_assignments <- lapply(plan$new_assignments, function(x) {
      x$blind_group <- "w04-validation-set"
      x
    })
  }
  plan$selected_cases <- length(plan$case_ids %||% character())
  plan$allocation_type <- allocation_type
  plan$review_mode <- review_mode
  validate_assignment_registry(c(assignments, plan$new_assignments))
  plan
}


plan_workflow_assignment <- function(
  cases,
  assignments,
  active_events,
  workflow,
  batch_id,
  task_type,
  user_ids,
  allocation_type = c("number", "percentage", "all"),
  amount = NA_real_,
  allocation_strategy = c("split", "shared")
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
      amount = amount,
      allocation_strategy = allocation_strategy
    ))
  }

  if (identical(mode, ASSIGNMENT_MODES[["independent_blind_review"]])) {
    return(plan_independent_blind_assignment(
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

  if (identical(allocation_type, "all")) {
    amount <- length(unresolved_unassigned_case_ids(
      cases, assignments, active_events, workflow, batch_id, task_type
    ))
    allocation_type <- "number"
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
