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
  events <- read_local_decision_events(path)
  if (!length(events)) return(list())

  case_ids <- vapply(events, decision_event_case_id, character(1))
  keep <- nzchar(case_ids)
  events <- events[keep]
  case_ids <- case_ids[keep]
  if (!length(events)) return(list())

  # Append-only semantics: the latest event for a case is the effective decision.
  # Version is authoritative where present; event time and file order break ties.
  versions <- vapply(events, function(x) {
    z <- suppressWarnings(as.integer(x$version %||% NA_integer_))
    if (is.na(z)) 0L else z
  }, integer(1))
  times <- vapply(events, decision_event_time, character(1))
  ord <- order(case_ids, versions, times, seq_along(events))
  events <- events[ord]
  case_ids <- case_ids[ord]

  latest <- !duplicated(case_ids, fromLast = TRUE)
  out <- events[latest]
  lapply(out, function(x) {
    x$active <- TRUE
    x
  })
}

# Backwards-compatible name used by the existing app/backend.
read_local_decisions <- function(path) {
  active_local_decisions(path)
}

write_local_decision <- function(path, decision) {
  events <- read_local_decision_events(path)
  case_id <- as.character(decision$review_case_id %||% decision$case_id %||% "")
  if (!nzchar(case_id)) stop("Local decision is missing review_case_id", call. = FALSE)

  prior_events <- Filter(function(x) identical(decision_event_case_id(x), case_id), events)
  prior <- NULL
  if (length(prior_events)) {
    prior_versions <- vapply(prior_events, function(x) {
      z <- suppressWarnings(as.integer(x$version %||% NA_integer_))
      if (is.na(z)) 0L else z
    }, integer(1))
    prior_times <- vapply(prior_events, decision_event_time, character(1))
    prior <- prior_events[[order(prior_versions, prior_times, seq_along(prior_events), decreasing = TRUE)[[1L]]]]
  }

  version <- if (is.null(prior)) 1L else {
    z <- suppressWarnings(as.integer(prior$version %||% NA_integer_))
    if (is.na(z) || z < 1L) 2L else z + 1L
  }

  event_at <- as.character(decision$resolved_at_utc %||% format(
    Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"
  ))
  user_id <- as.character(decision$reviewer %||% decision$user_id %||% "")
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

  # Preserve append-only history while still using an atomic file replacement.
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
