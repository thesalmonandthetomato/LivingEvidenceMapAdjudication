hash_access_key <- function(key) {
  digest::digest(key, algo = "sha256", serialize = FALSE)
}

access_key_hash <- function() {
  configured <- Sys.getenv("LEM_ACCESS_KEY_SHA256", unset = "")
  if (!nzchar(configured)) stop("LEM_ACCESS_KEY_SHA256 is not configured", call. = FALSE)
  configured
}

access_key_valid <- function(candidate) {
  if (is.null(candidate) || !nzchar(candidate)) return(FALSE)
  identical(hash_access_key(candidate), access_key_hash())
}


individual_auth_configured <- function() {
  nzchar(Sys.getenv("LEM_USER_ACCESS_KEY_HASHES_JSON", unset = "")) &&
    (nzchar(Sys.getenv("LEM_INITIAL_USERS_JSON", unset = "")) ||
       identical(storage_backend(), "google_sheets"))
}

user_access_key_hashes <- function() {
  raw <- Sys.getenv("LEM_USER_ACCESS_KEY_HASHES_JSON", unset = "")
  if (!nzchar(raw)) return(list())

  parsed <- tryCatch(
    jsonlite::fromJSON(raw, simplifyVector = FALSE),
    error = function(e) stop("LEM_USER_ACCESS_KEY_HASHES_JSON is not valid JSON", call. = FALSE)
  )
  if (!is.list(parsed) || is.null(names(parsed)) || any(!nzchar(names(parsed)))) {
    stop("LEM_USER_ACCESS_KEY_HASHES_JSON must be a named JSON object keyed by user_id", call. = FALSE)
  }

  vals <- lapply(parsed, function(x) {
    z <- if (is.null(x) || !length(x)) "" else as.character(x[[1L]])
    if (!grepl("^[0-9a-fA-F]{64}$", z)) {
      stop("Every configured user access-key hash must be SHA-256", call. = FALSE)
    }
    tolower(z)
  })
  vals
}

registered_user_auth_diagnostic <- function(users, email, access_key) {
  if (is.null(email) || !nzchar(trimws(as.character(email)))) return("email_missing")
  user <- find_user_by_email(users, email, require_active = TRUE)
  if (is.null(user)) return("email_not_found_or_inactive")

  hashes <- user_access_key_hashes()
  expected <- hashes[[user$user_id]]
  if (is.null(expected) || !nzchar(expected)) return("hash_missing_for_user")

  if (is.null(access_key) || !nzchar(as.character(access_key))) return("access_key_missing")
  actual <- tolower(hash_access_key(access_key))
  if (!identical(actual, expected)) return("access_key_mismatch")
  "ok"
}

authenticate_registered_user <- function(users, email, access_key) {
  if (is.null(email) || !nzchar(trimws(as.character(email)))) return(NULL)
  if (is.null(access_key) || !nzchar(as.character(access_key))) return(NULL)

  user <- find_user_by_email(users, email, require_active = TRUE)
  if (is.null(user)) return(NULL)

  hashes <- user_access_key_hashes()
  expected <- hashes[[user$user_id]]
  if (is.null(expected) || !nzchar(expected)) return(NULL)

  actual <- tolower(hash_access_key(access_key))
  if (!identical(actual, expected)) return(NULL)
  user
}

legacy_session_user <- function() {
  rid <- Sys.getenv("LEM_REVIEWER", unset = "prototype-reviewer")
  list(
    user_id = rid,
    email = "",
    display_name = rid,
    role = "administrator",
    active = TRUE
  )
}
