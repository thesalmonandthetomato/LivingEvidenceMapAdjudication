# User registry and permission helpers for the adjudication redesign.
#
# Phase 1A only: these functions are not yet wired into the Shiny login flow.

normalise_user_row <- function(x) {
  required <- ADJUDICATION_SCHEMA$users
  out <- setNames(vector("list", length(required)), required)
  for (nm in required) {
    value <- x[[nm]]
    if (is.null(value) || !length(value) || is.na(value[[1L]])) value <- ""
    out[[nm]] <- if (identical(nm, "active")) {
      if (is.logical(value)) isTRUE(value[[1L]]) else {
        tolower(trimws(as.character(value[[1L]]))) %in% c("true", "1", "yes", "y")
      }
    } else {
      trimws(as.character(value[[1L]]))
    }
  }
  out
}

validate_user_registry <- function(users) {
  if (is.null(users)) users <- list()
  if (!is.list(users)) stop("User registry must be a list", call. = FALSE)
  if (!length(users)) return(invisible(TRUE))

  users <- lapply(users, normalise_user_row)
  invisible(lapply(users, validate_user_contract))

  ids <- vapply(users, function(x) x$user_id, character(1))
  emails <- tolower(vapply(users, function(x) x$email, character(1)))

  if (anyDuplicated(ids)) stop("User registry contains duplicate user_id", call. = FALSE)
  if (anyDuplicated(emails)) stop("User registry contains duplicate email", call. = FALSE)
  invisible(TRUE)
}

active_users <- function(users) {
  validate_user_registry(users)
  Filter(function(x) isTRUE(normalise_user_row(x)$active), users)
}

find_user_by_id <- function(users, user_id, require_active = TRUE) {
  validate_user_registry(users)
  key <- trimws(as.character(user_id %||% ""))
  if (!nzchar(key)) return(NULL)

  hits <- Filter(
    function(x) identical(normalise_user_row(x)$user_id, key),
    users
  )
  if (!length(hits)) return(NULL)

  user <- normalise_user_row(hits[[1L]])
  if (require_active && !isTRUE(user$active)) return(NULL)
  user
}

find_user_by_email <- function(users, email, require_active = TRUE) {
  validate_user_registry(users)
  key <- tolower(trimws(as.character(email %||% "")))
  if (!nzchar(key)) return(NULL)

  hits <- Filter(
    function(x) identical(tolower(normalise_user_row(x)$email), key),
    users
  )
  if (!length(hits)) return(NULL)

  user <- normalise_user_row(hits[[1L]])
  if (require_active && !isTRUE(user$active)) return(NULL)
  user
}

user_can <- function(user, permission) {
  if (is.null(user)) return(FALSE)
  user <- normalise_user_row(user)
  if (!isTRUE(user$active)) return(FALSE)
  role_can(user$role, permission)
}

require_user_permission <- function(user, permission) {
  if (!user_can(user, permission)) {
    stop("User is not permitted to perform this action", call. = FALSE)
  }
  invisible(TRUE)
}
