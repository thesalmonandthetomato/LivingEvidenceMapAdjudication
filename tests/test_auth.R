suppressPackageStartupMessages({
  library(jsonlite)
  library(digest)
})
source("R/w01_contract.R")
source("R/adjudication_schema.R")
source("R/users.R")
source("R/storage_local.R")
source("R/storage_backend.R")
source("R/auth.R")

old_backend <- Sys.getenv("LEM_STORAGE_BACKEND", unset = NA)
old_users <- Sys.getenv("LEM_INITIAL_USERS_JSON", unset = NA)
old_hashes <- Sys.getenv("LEM_USER_ACCESS_KEY_HASHES_JSON", unset = NA)
old_legacy <- Sys.getenv("LEM_ACCESS_KEY_SHA256", unset = NA)
old_reviewer <- Sys.getenv("LEM_REVIEWER", unset = NA)

on.exit({
  restore <- function(name, value) {
    if (is.na(value)) Sys.unsetenv(name) else Sys.setenv(structure(value, names = name))
  }
  restore("LEM_STORAGE_BACKEND", old_backend)
  restore("LEM_INITIAL_USERS_JSON", old_users)
  restore("LEM_USER_ACCESS_KEY_HASHES_JSON", old_hashes)
  restore("LEM_ACCESS_KEY_SHA256", old_legacy)
  restore("LEM_REVIEWER", old_reviewer)
}, add = TRUE)

Sys.setenv(LEM_STORAGE_BACKEND = "local")

users_json <- jsonlite::toJSON(list(
  list(
    user_id = "usr-admin",
    email = "admin@example.org",
    display_name = "Admin User",
    role = "administrator",
    active = TRUE
  ),
  list(
    user_id = "usr-reviewer",
    email = "reviewer@example.org",
    display_name = "Review User",
    role = "reviewer",
    active = TRUE
  ),
  list(
    user_id = "usr-inactive",
    email = "inactive@example.org",
    display_name = "Inactive User",
    role = "reviewer",
    active = FALSE
  )
), auto_unbox = TRUE)

admin_key <- "admin-test-access-key"
reviewer_key <- "reviewer-test-access-key"
inactive_key <- "inactive-test-access-key"

hashes_json <- jsonlite::toJSON(list(
  "usr-admin" = hash_access_key(admin_key),
  "usr-reviewer" = hash_access_key(reviewer_key),
  "usr-inactive" = hash_access_key(inactive_key)
), auto_unbox = TRUE)

Sys.setenv(
  LEM_INITIAL_USERS_JSON = users_json,
  LEM_USER_ACCESS_KEY_HASHES_JSON = hashes_json
)

stopifnot(individual_auth_configured())

registry <- read_user_registry()
stopifnot(length(registry) == 3L)

admin <- authenticate_registered_user(registry, "ADMIN@example.org", admin_key)
reviewer <- authenticate_registered_user(registry, "reviewer@example.org", reviewer_key)

stopifnot(identical(admin$user_id, "usr-admin"))
stopifnot(identical(admin$role, "administrator"))
stopifnot(identical(reviewer$user_id, "usr-reviewer"))
stopifnot(identical(reviewer$role, "reviewer"))

stopifnot(is.null(authenticate_registered_user(registry, "reviewer@example.org", "wrong-key")))
stopifnot(is.null(authenticate_registered_user(registry, "unknown@example.org", reviewer_key)))
stopifnot(is.null(authenticate_registered_user(registry, "inactive@example.org", inactive_key)))

stopifnot(identical(authenticate_registered_user_result(registry, "admin@example.org", admin_key)$status, "ok"))
stopifnot(identical(authenticate_registered_user_result(registry, "unknown@example.org", admin_key)$status, "email_not_found_or_inactive"))
stopifnot(identical(authenticate_registered_user_result(registry, "inactive@example.org", inactive_key)$status, "email_not_found_or_inactive"))
stopifnot(identical(authenticate_registered_user_result(registry, "admin@example.org", "wrong-key")$status, "access_key_mismatch"))

Sys.setenv(
  LEM_ACCESS_KEY_SHA256 = hash_access_key("legacy-key"),
  LEM_REVIEWER = "legacy-reviewer"
)
stopifnot(access_key_valid("legacy-key"))
legacy <- legacy_session_user()
stopifnot(identical(legacy$user_id, "legacy-reviewer"))
stopifnot(identical(legacy$role, "administrator"))

cat("PASS: individual and legacy authentication helpers\n")
