decision_event_scalar <- function(x, fields, default = "") {
  for (nm in fields) {
    value <- x[[nm]]
    if (!is.null(value) && length(value)) {
      value <- as.character(value[[1L]])
      if (!is.na(value) && nzchar(value)) return(value)
    }
  }
  default
}

normalise_decision_events <- function(
  events,
  case_fields = c("case_id", "review_case_id", "record_id"),
  identity_scope = c("case", "case_user")
) {
  identity_scope <- match.arg(identity_scope)
  if (!length(events)) return(list())

  out <- vector("list", length(events))
  versions <- new.env(parent = emptyenv())

  for (i in seq_along(events)) {
    x <- events[[i]]

    case_id <- decision_event_scalar(x, case_fields)
    if (!nzchar(case_id)) {
      stop("Decision event is missing a case identifier", call. = FALSE)
    }

    user_id <- decision_event_scalar(x, c("user_id", "reviewer"))
    event_key <- if (identical(identity_scope, "case_user")) {
      paste(case_id, user_id, sep = "|")
    } else {
      case_id
    }

    prior_version <- if (exists(event_key, envir = versions, inherits = FALSE)) {
      get(event_key, envir = versions, inherits = FALSE)
    } else {
      0L
    }
    version <- prior_version + 1L
    assign(event_key, version, envir = versions)

    event_at <- decision_event_scalar(x, c("event_at_utc", "resolved_at_utc"))

    x$case_id <- case_id
    x$user_id <- user_id
    x$version <- version
    x$event_at_utc <- event_at
    x$active <- FALSE
    out[[i]] <- x
  }

  event_keys <- vapply(
    out,
    function(x) {
      if (identical(identity_scope, "case_user")) {
        paste(as.character(x$case_id), as.character(x$user_id), sep = "|")
      } else {
        as.character(x$case_id)
      }
    },
    character(1)
  )
  latest <- !duplicated(event_keys, fromLast = TRUE)
  for (i in seq_along(out)) out[[i]]$active <- isTRUE(latest[[i]])

  out
}

active_decision_events <- function(
  events,
  case_fields = c("case_id", "review_case_id", "record_id"),
  identity_scope = c("case", "case_user")
) {
  identity_scope <- match.arg(identity_scope)
  xs <- normalise_decision_events(
    events,
    case_fields = case_fields,
    identity_scope = identity_scope
  )
  if (!length(xs)) return(list())
  xs[vapply(xs, function(x) isTRUE(x$active), logical(1))]
}


canonical_event_by_id <- function(
  events,
  decision_id,
  case_fields = c("case_id", "review_case_id", "record_id"),
  identity_scope = c("case", "case_user")
) {
  identity_scope <- match.arg(identity_scope)
  xs <- normalise_decision_events(
    events,
    case_fields = case_fields,
    identity_scope = identity_scope
  )
  hits <- Filter(
    function(x) identical(as.character(x$decision_id %||% ""), as.character(decision_id)),
    xs
  )
  if (length(hits) != 1L) stop("Decision event could not be uniquely resolved", call. = FALSE)
  hits[[1L]]
}


normalise_saved_decision_event <- function(event, prior_decision = NULL, case_fields = c("case_id", "review_case_id", "record_id")) {
  case_id <- decision_event_scalar(event, case_fields)
  if (!nzchar(case_id)) stop("Saved decision is missing a case identifier", call. = FALSE)

  prior_version <- 0L
  if (!is.null(prior_decision)) {
    z <- suppressWarnings(as.integer(prior_decision$version %||% NA_integer_))
    if (!is.na(z) && z > 0L) prior_version <- z
  }

  event$case_id <- case_id
  event$user_id <- decision_event_scalar(event, c("user_id", "reviewer"))
  event$version <- prior_version + 1L
  event$active <- TRUE
  event$event_at_utc <- decision_event_scalar(event, c("event_at_utc", "resolved_at_utc"))
  event
}
