read_local_decision_events <- function(path) {
  if (is.null(path) || !nzchar(as.character(path)) || !file.exists(path)) return(list())
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) return(list())
  lapply(lines, jsonlite::fromJSON, simplifyVector = FALSE)
}

decision_event_case_id <- function(x) {
  as.character(x$case_id %||% x$review_case_id %||% "")
}

decision_event_time <- function(x) {
  as.character(x$event_at_utc %||% x$resolved_at_utc %||% "")
}

active_local_decisions <- function(path) {
  active_decision_events(
    read_local_decision_events(path),
    case_fields = c("case_id", "review_case_id")
  )
}

# Backwards-compatible name used by the existing app/backend.
read_local_decisions <- function(path) {
  active_local_decisions(path)
}

write_local_decision <- function(path, decision) {
  events <- read_local_decision_events(path)
  case_id <- as.character(decision$review_case_id %||% decision$case_id %||% "")
  if (!nzchar(case_id)) stop("Local decision is missing review_case_id", call. = FALSE)

  user_id <- as.character(decision$reviewer %||% decision$user_id %||% "")
  case_events <- Filter(function(x) identical(decision_event_case_id(x), case_id), events)

  authority_user <- ""
  if (length(case_events)) {
    first_times <- vapply(case_events, decision_event_time, character(1))
    first_idx <- order(first_times, seq_along(case_events), decreasing = FALSE)[[1L]]
    authority_user <- as.character(case_events[[first_idx]]$user_id %||% case_events[[first_idx]]$reviewer %||% "")
    if (nzchar(authority_user) && !identical(authority_user, user_id)) {
      stop("This case has already been resolved by another reviewer", call. = FALSE)
    }
  }

  prior <- NULL
  if (length(case_events)) {
    prior_versions <- vapply(case_events, function(x) {
      z <- suppressWarnings(as.integer(x$version %||% NA_integer_))
      if (is.na(z)) 0L else z
    }, integer(1))
    prior_times <- vapply(case_events, decision_event_time, character(1))
    prior <- case_events[[order(prior_versions, prior_times, seq_along(case_events), decreasing = TRUE)[[1L]]]]
  }

  version <- if (is.null(prior)) 1L else {
    z <- suppressWarnings(as.integer(prior$version %||% NA_integer_))
    if (is.na(z) || z < 1L) 2L else z + 1L
  }

  event_at <- as.character(decision$resolved_at_utc %||% format(
    Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"
  ))
  supersedes <- if (is.null(prior)) "" else as.character(prior$decision_id %||% "")

  decision_id <- paste0(
    "w01-dec-",
    substr(
      digest::digest(
        paste(
          case_id,
          user_id,
          version,
          event_at,
          as.character(decision$decision %||% ""),
          as.character(decision$queue_sha256 %||% ""),
          sep = "|"
        ),
        algo = "sha256",
        serialize = FALSE
      ),
      1L, 24L
    )
  )

  event <- decision
  event$decision_id <- decision_id
  event$case_id <- case_id
  event$review_case_id <- case_id
  event$user_id <- user_id
  event$reviewer <- user_id
  event$version <- version
  event$active <- TRUE
  event$event_at_utc <- event_at
  event$resolved_at_utc <- event_at
  event$supersedes_decision_id <- supersedes

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  updated <- c(events, list(event))
  tmp <- tempfile(pattern = "w01-decision-events-", tmpdir = dirname(path), fileext = ".jsonl")
  con <- file(tmp, "wt", encoding = "UTF-8")
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  for (d in updated) {
    writeLines(
      jsonlite::toJSON(d, auto_unbox = TRUE, null = "null", na = "null"),
      con,
      useBytes = TRUE
    )
  }
  close(con)
  if (!file.rename(tmp, path)) stop("Could not atomically replace local decision event log", call. = FALSE)

  verify <- read_local_decision_events(path)
  hits <- Filter(function(x) identical(as.character(x$decision_id %||% ""), decision_id), verify)
  if (length(hits) != 1L) stop("Local decision event write could not be verified", call. = FALSE)
  hits[[1L]]
}


read_local_assignments <- function(path) {
  if (is.null(path) || !nzchar(as.character(path)) || !file.exists(path)) return(list())
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) return(list())
  assignments <- lapply(lines, jsonlite::fromJSON, simplifyVector = FALSE)
  assignments <- lapply(assignments, normalise_assignment_row)
  validate_assignment_registry(assignments)
  assignments
}


write_local_assignments <- function(path, assignments) {
  assignments <- lapply(assignments %||% list(), normalise_assignment_row)
  validate_assignment_registry(assignments)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = "assignments-", tmpdir = dirname(path), fileext = ".jsonl")
  con <- file(tmp, "wt", encoding = "UTF-8")
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  for (a in assignments) {
    writeLines(
      jsonlite::toJSON(a, auto_unbox = TRUE, null = "null", na = "null"),
      con,
      useBytes = TRUE
    )
  }
  close(con)
  if (!file.rename(tmp, path)) stop("Could not atomically replace local assignment registry", call. = FALSE)
  verify <- read_local_assignments(path)
  if (length(verify) != length(assignments)) {
    stop("Local assignment registry write could not be verified", call. = FALSE)
  }
  invisible(assignments)
}


read_local_users <- function(path) {
  if (is.null(path) || !nzchar(as.character(path)) || !file.exists(path)) return(list())
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) return(list())
  users <- lapply(lines, jsonlite::fromJSON, simplifyVector = FALSE)
  validate_user_registry(users)
  lapply(users, normalise_user_row)
}

write_local_users <- function(path, users) {
  validate_user_registry(users)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = "users-", tmpdir = dirname(path), fileext = ".jsonl")
  con <- file(tmp, "wt", encoding = "UTF-8")
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  for (u in users) {
    writeLines(
      jsonlite::toJSON(normalise_user_row(u), auto_unbox = TRUE, null = "null", na = "null"),
      con,
      useBytes = TRUE
    )
  }
  close(con)
  if (!file.rename(tmp, path)) stop("Could not atomically replace local user registry", call. = FALSE)
  invisible(TRUE)
}
