source("R/w01_contract.R")
source("R/adjudication_schema.R")
suppressPackageStartupMessages(library(jsonlite))
source("R/users.R")
source("R/storage_local.R")

users <- list(
  list(
    user_id = "usr-admin",
    email = "admin@example.org",
    display_name = "Administrator",
    role = "administrator",
    active = TRUE
  ),
  list(
    user_id = "usr-reviewer",
    email = "reviewer@example.org",
    display_name = "Reviewer",
    role = "reviewer",
    active = TRUE
  ),
  list(
    user_id = "usr-inactive",
    email = "inactive@example.org",
    display_name = "Inactive",
    role = "reviewer",
    active = FALSE
  )
)

validate_user_registry(users)
stopifnot(length(active_users(users)) == 2L)

admin <- find_user_by_id(users, "usr-admin")
reviewer <- find_user_by_email(users, "REVIEWER@example.org")
inactive <- find_user_by_id(users, "usr-inactive")

stopifnot(identical(admin$role, "administrator"))
stopifnot(identical(reviewer$user_id, "usr-reviewer"))
stopifnot(is.null(inactive))

stopifnot(user_can(admin, "manage_users"))
stopifnot(user_can(admin, "resolve_conflicts"))
stopifnot(user_can(reviewer, "adjudicate_assigned"))
stopifnot(user_can(reviewer, "resolve_conflicts"))
stopifnot(!user_can(reviewer, "control_workflows"))
stopifnot(!user_can(reviewer, "export_to_github"))
stopifnot(!user_can(users[[3]], "resolve_conflicts"))

must_fail <- function(expr) {
  ok <- FALSE
  tryCatch(force(expr), error = function(e) ok <<- TRUE)
  stopifnot(ok)
}

must_fail(validate_user_registry(c(
  users,
  list(list(
    user_id = "usr-admin",
    email = "other@example.org",
    display_name = "Duplicate ID",
    role = "reviewer",
    active = TRUE
  ))
)))

must_fail(validate_user_registry(c(
  users,
  list(list(
    user_id = "usr-other",
    email = "ADMIN@example.org",
    display_name = "Duplicate email",
    role = "reviewer",
    active = TRUE
  ))
)))

must_fail(require_user_permission(reviewer, "manage_users"))

cat("PASS: user registry and permissions\n")


tmp_users <- tempfile(fileext = ".jsonl")
write_local_users(tmp_users, users)
roundtrip <- read_local_users(tmp_users)
stopifnot(length(roundtrip) == 3L)
stopifnot(identical(roundtrip[[1]]$user_id, "usr-admin"))
stopifnot(isTRUE(roundtrip[[1]]$active))
stopifnot(isFALSE(roundtrip[[3]]$active))
unlink(tmp_users)

cat("PASS: local user registry round-trip\n")
