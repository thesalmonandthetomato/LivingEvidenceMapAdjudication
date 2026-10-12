# Review-wide, append-only formal search strategy registry.
# Pure file/Sheets persistence; no search dispatch or Zenodo access.
search_strategy_version_columns <- c("review_key","version","strategy_sha256",
                                     "search_string","created_by","created_at_utc")
search_strategy_review_key <- function(review_id) {
  if (!is.character(review_id) || length(review_id)!=1L || is.na(review_id) ||
      !nzchar(trimws(review_id))) stop("A review ID is required.",call.=FALSE)
  digest::digest(review_id,algo="sha256",serialize=FALSE)
}
search_strategy_log_path <- function() Sys.getenv("LEM_SEARCH_STRATEGY_LOG",
                                                  unset="local_state/search_strategy_versions.jsonl")
search_strategy_versions <- function(review_key) {
  if (!is.character(review_key) || length(review_key)!=1L || is.na(review_key) ||
      !nzchar(review_key)) stop("Invalid review key",call.=FALSE)
  if (identical(storage_backend(),"google_sheets")) {
    gs4_auth_from_env()
    ss <- sheet_id_from_env()
    tab <- "search_strategy_versions"
    if (!tab %in% sheet_names_cached(ss)) return(data.frame())
    rows <- googlesheets4::read_sheet(ss,sheet=tab,col_types="c")
  } else {
    path <- search_strategy_log_path()
    if (!file.exists(path)) return(data.frame())
    lines <- readLines(path,warn=FALSE)
    if (!length(lines)) return(data.frame())
    rows <- do.call(rbind,lapply(lines,function(line)
      as.data.frame(jsonlite::fromJSON(line,simplifyVector=TRUE),stringsAsFactors=FALSE)))
  }
  if (!all(search_strategy_version_columns %in% names(rows)))
    stop("Incompatible immutable strategy registry schema",call.=FALSE)
  rows[!is.na(rows$review_key) & rows$review_key==review_key,,drop=FALSE]
}
search_strategy_finalise <- function(review_key, search_string, created_by, expected_version=0L) {
  if (!is.character(search_string) || length(search_string)!=1L || is.na(search_string))
    stop("Strategy must be one string.",call.=FALSE)
  if (!is.character(created_by) || length(created_by)!=1L || is.na(created_by) ||
      !nzchar(trimws(created_by))) stop("Creator is required.",call.=FALSE)
  check <- search_boolean_validate(search_string)
  if (!isTRUE(check$valid)) stop(check$message,call.=FALSE)
  # One authorised administrator per review. Sheets checks are optimistic,
  # not atomic across simultaneous sessions; conflicting rows fail verification.
  backend <- storage_backend()
  if (!backend %in% c("local","google_sheets"))
    stop("Unsupported immutable strategy storage backend.",call.=FALSE)
  if (identical(backend,"local")) {
    lock_path <- paste0(search_strategy_log_path(),".lock")
    dir.create(dirname(lock_path),recursive=TRUE,showWarnings=FALSE)
    if (!dir.create(lock_path,showWarnings=FALSE))
      stop("Strategy registry is locked by another writer; retry after it completes.",call.=FALSE)
    on.exit(unlink(lock_path,recursive=TRUE),add=TRUE)
  }
  existing <- search_strategy_versions(review_key)
  versions <- if(nrow(existing)) suppressWarnings(as.integer(existing$version)) else integer()
  if(anyNA(versions) || anyDuplicated(versions) || !identical(versions,seq_along(versions)))
    stop("Strategy version history is inconsistent.",call.=FALSE)
  latest <- length(versions)
  if (length(expected_version)!=1L || is.na(expected_version) ||
      !identical(as.integer(expected_version),as.integer(latest)))
    stop("Strategy changed since it was loaded. Reload before finalising.",call.=FALSE)
  sha <- digest::digest(search_string,algo="sha256",serialize=FALSE)
  if(nrow(existing) && identical(as.character(existing$strategy_sha256[[latest]]),sha))
    stop("This strategy is already the latest finalised version.",call.=FALSE)
  row <- list(review_key=review_key,version=as.character(latest+1L),
              strategy_sha256=sha,search_string=search_string,created_by=created_by,
              created_at_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"))
  if (identical(backend,"google_sheets")) {
    # Recheck directly before append. Never automatically retry an uncertain append.
    current <- search_strategy_versions(review_key)
    if (nrow(current)!=latest)
      stop("Strategy version changed before writing. Reload.",call.=FALSE)
    gs4_auth_from_env()
    ss <- sheet_id_from_env()
    tab <- "search_strategy_versions"
    if (!tab %in% sheet_names_cached(ss)) {
      empty <- as.data.frame(setNames(replicate(length(search_strategy_version_columns),
        character(),simplify=FALSE),search_strategy_version_columns),stringsAsFactors=FALSE)
      googlesheets4::sheet_write(empty,ss=ss,sheet=tab)
      invalidate_sheet_names_cache(ss)
    }
    googlesheets4::sheet_append(ss,data=as.data.frame(row,stringsAsFactors=FALSE),sheet=tab)
  } else {
    path <- search_strategy_log_path()
    dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
    cat(as.character(jsonlite::toJSON(row,auto_unbox=TRUE)), "\n",
        file=path,append=TRUE,sep="")
  }
  verify <- search_strategy_versions(review_key)
  matching <- verify[as.character(verify$version)==row$version,,drop=FALSE]
  if(nrow(verify)!=latest+1L || nrow(matching)!=1L || !identical(as.character(matching$strategy_sha256[[1L]]),sha))
    stop("Finalisation write could not be verified; do not retry blindly.",call.=FALSE)
  row
}

# Each deployed review has one user registry. Require precisely one active
# administrator for configuration finalisation without changing adjudication roles.
search_review_require_sole_admin <- function(users, actor_id) {
  validate_user_registry(users)
  admins <- Filter(function(u) {
    u <- normalise_user_row(u)
    identical(u$role,"administrator") && isTRUE(u$active)
  },users)
  ids <- vapply(admins,function(u) as.character(normalise_user_row(u)$user_id),character(1))
  if(length(ids)!=1L)
    stop("Search finalisation requires exactly one active administrator for this review.",call.=FALSE)
  if(!identical(as.character(actor_id),ids[[1L]]))
    stop("Only the review administrator can finalise a search strategy.",call.=FALSE)
  invisible(TRUE)
}
