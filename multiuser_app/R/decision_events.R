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

normalise_decision_events <- function(events, case_fields = c("case_id", "review_case_id", "record_id")) {
  if (!length(events)) return(list())

  out <- vector("list", length(events))
  versions <- new.env(parent = emptyenv())

  for (i in seq_along(events)) {
    x <- events[[i]]

    case_id <- decision_event_scalar(x, case_fields)
    if (!nzchar(case_id)) {
      stop("Decision event is missing a case identifier", call. = FALSE)
    }

    prior_version <- if (exists(case_id, envir = versions, inherits = FALSE)) {
      get(case_id, envir = versions, inherits = FALSE)
    } else {
      0L
    }
    version <- prior_version + 1L
    assign(case_id, version, envir = versions)

    user_id <- decision_event_scalar(x, c("user_id", "reviewer"))
    event_at <- decision_event_scalar(x, c("event_at_utc", "resolved_at_utc"))

    x$case_id <- case_id
    x$user_id <- user_id
    x$version <- version
    x$event_at_utc <- event_at
    x$active <- FALSE
    out[[i]] <- x
  }

  case_ids <- vapply(out, function(x) as.character(x$case_id), character(1))
  latest <- !duplicated(case_ids, fromLast = TRUE)
  for (i in seq_along(out)) out[[i]]$active <- isTRUE(latest[[i]])

  out
}

active_decision_events <- function(events, case_fields = c("case_id", "review_case_id", "record_id")) {
  xs <- normalise_decision_events(events, case_fields = case_fields)
  if (!length(xs)) return(list())
  xs[vapply(xs, function(x) isTRUE(x$active), logical(1))]
}


canonical_event_by_id <- function(events, decision_id, case_fields = c("case_id", "review_case_id", "record_id")) {
  xs <- normalise_decision_events(events, case_fields = case_fields)
  hits <- Filter(
    function(x) identical(as.character(x$decision_id %||% ""), as.character(decision_id)),
    xs
  )
  if (length(hits) != 1L) stop("Decision event could not be uniquely resolved", call. = FALSE)
  hits[[1L]]
}
