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
