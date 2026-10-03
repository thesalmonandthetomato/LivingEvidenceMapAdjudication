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
    function(x) paste(x$workflow, x$batch_id, x$case_id, x$user_id, sep = "|"),
    character(1)
  )
  if (anyDuplicated(keys)) {
    stop("Assignment registry contains duplicate case/user assignments", call. = FALSE)
  }
  invisible(TRUE)
}

assignments_for_batch <- function(assignments, workflow, batch_id) {
  validate_assignment_registry(assignments)
  Filter(
    function(x) {
      a <- normalise_assignment_row(x)
      identical(a$workflow, as.character(workflow)) &&
        identical(a$batch_id, as.character(batch_id))
    },
    assignments
  )
}

assignment_mode_active <- function(assignments, workflow, batch_id) {
  length(assignments_for_batch(assignments, workflow, batch_id)) > 0L
}

assignments_for_user <- function(assignments, workflow, batch_id, user_id) {
  xs <- assignments_for_batch(assignments, workflow, batch_id)
  Filter(
    function(x) identical(normalise_assignment_row(x)$user_id, as.character(user_id)),
    xs
  )
}

cases_for_assignment_user <- function(cases, assignments, workflow, batch_id, user) {
  if (!length(cases)) return(list())
  batch_assignments <- assignments_for_batch(assignments, workflow, batch_id)
  if (!length(batch_assignments)) return(cases)

  # Administrators can inspect/adjudicate the full batch; reviewers only see
  # cases explicitly assigned to their stable user ID.
  if (user_can(user, "manage_assignments")) return(cases)

  user_id <- as.character(normalise_user_row(user)$user_id)
  assigned <- assignments_for_user(assignments, workflow, batch_id, user_id)
  allowed <- unique(vapply(assigned, function(x) normalise_assignment_row(x)$case_id, character(1)))

  Filter(
    function(x) {
      case_id <- as.character(x$review_case_id %||% x$case_id %||% x$record_id %||% "")
      case_id %in% allowed
    },
    cases
  )
}

decision_events_for_user <- function(events, user_id) {
  if (!length(events)) return(list())
  Filter(
    function(x) identical(
      as.character(x$user_id %||% x$reviewer %||% ""),
      as.character(user_id)
    ),
    events
  )
}

assignment_progress <- function(assignments, active_events, users, workflow = NULL, batch_id = NULL) {
  validate_assignment_registry(assignments)
  xs <- assignments
  if (!is.null(workflow) && !is.null(batch_id)) {
    xs <- assignments_for_batch(assignments, workflow, batch_id)
  }
  if (!length(xs)) {
    return(list(
      assigned = 0L,
      completed = 0L,
      remaining = 0L,
      by_user = list()
    ))
  }

  events <- active_events %||% list()
  complete_key <- if (length(events)) {
    unique(vapply(
      events,
      function(x) paste(
        as.character(x$case_id %||% x$review_case_id %||% x$record_id %||% ""),
        as.character(x$user_id %||% x$reviewer %||% ""),
        sep = "|"
      ),
      character(1)
    ))
  } else character()

  effective <- lapply(xs, function(x) {
    a <- normalise_assignment_row(x)
    key <- paste(a$case_id, a$user_id, sep = "|")
    a$effective_status <- if (key %in% complete_key) "complete" else "assigned"
    a
  })

  user_ids <- unique(vapply(effective, function(x) x$user_id, character(1)))
  by_user <- lapply(user_ids, function(uid) {
    ua <- Filter(function(x) identical(x$user_id, uid), effective)
    completed <- sum(vapply(ua, function(x) identical(x$effective_status, "complete"), logical(1)))
    user <- find_user_by_id(users, uid, require_active = FALSE)
    user_events <- Filter(
      function(x) identical(as.character(x$user_id %||% x$reviewer %||% ""), uid),
      events
    )
    last_activity <- ""
    if (length(user_events)) {
      times <- vapply(user_events, function(x) as.character(x$event_at_utc %||% x$resolved_at_utc %||% ""), character(1))
      times <- times[nzchar(times)]
      if (length(times)) last_activity <- max(times)
    }
    list(
      user_id = uid,
      display_name = if (is.null(user)) uid else user$display_name,
      role = if (is.null(user)) "" else user$role,
      assigned = length(ua),
      completed = completed,
      remaining = length(ua) - completed,
      progress = if (length(ua)) completed / length(ua) else 0,
      workflows = unique(vapply(ua, function(x) x$workflow, character(1))),
      last_activity = last_activity
    )
  })

  completed <- sum(vapply(effective, function(x) identical(x$effective_status, "complete"), logical(1)))
  list(
    assigned = length(effective),
    completed = completed,
    remaining = length(effective) - completed,
    by_user = by_user
  )
}
