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


# Review-specific completed W00 scoping results. Separate from immutable formal strategies.
search_scoping_columns <- c("review_key","run_id","query","counts_json","completed_at_utc")
search_scoping_log_path <- function() Sys.getenv("LEM_SEARCH_SCOPING_LOG",
  unset="local_state/search_scoping_results.jsonl")

search_scoping_history <- function(review_key) {
  if (identical(storage_backend(),"google_sheets")) {
    gs4_auth_from_env()
    ss <- sheet_id_from_env()
    if (!"search_scoping_results" %in% sheet_names_cached(ss)) return(data.frame())
    rows <- googlesheets4::read_sheet(ss,sheet="search_scoping_results",col_types="c")
  } else {
    path <- search_scoping_log_path()
    if (!file.exists(path)) return(data.frame())
    lines <- readLines(path,warn=FALSE)
    if (!length(lines)) return(data.frame())
    rows <- do.call(rbind,lapply(lines,function(line)
      as.data.frame(jsonlite::fromJSON(line),stringsAsFactors=FALSE)))
  }
  if (!all(search_scoping_columns %in% names(rows)))
    stop("Incompatible scoping history schema",call.=FALSE)
  rows[!is.na(rows$review_key) & rows$review_key==review_key,,drop=FALSE]
}

search_scoping_save <- function(review_key,run_id,query,counts) {
  if (!grepl("^[0-9]+$",as.character(run_id)))
    stop("Scoping run ID missing",call.=FALSE)
  if (!is.character(query)||length(query)!=1L||!nzchar(trimws(query)))
    stop("Scoping query missing",call.=FALSE)
  if (!is.data.frame(counts)||!nrow(counts)||
      !all(c("source_slug","status","hits")%in%names(counts)))
    stop("Scoping counts incomplete",call.=FALSE)
  existing <- search_scoping_history(review_key)
  if (nrow(existing) && as.character(run_id)%in%as.character(existing$run_id))
    return(invisible(FALSE))
  row <- list(review_key=review_key,run_id=as.character(run_id),query=query,
    counts_json=as.character(jsonlite::toJSON(counts,dataframe="rows",auto_unbox=TRUE,na="null")),
    completed_at_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"))
  if (identical(storage_backend(),"google_sheets")) {
    gs4_auth_from_env()
    ss <- sheet_id_from_env()
    if (!"search_scoping_results"%in%sheet_names_cached(ss)) {
      empty <- as.data.frame(setNames(replicate(length(search_scoping_columns),
        character(),simplify=FALSE),search_scoping_columns),stringsAsFactors=FALSE)
      googlesheets4::sheet_write(empty,ss=ss,sheet="search_scoping_results")
      invalidate_sheet_names_cache(ss)
    }
    googlesheets4::sheet_append(ss,
      data=as.data.frame(row,stringsAsFactors=FALSE),sheet="search_scoping_results")
  } else {
    path <- search_scoping_log_path()
    dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
    cat(as.character(jsonlite::toJSON(row,auto_unbox=TRUE)),"\n",file=path,append=TRUE,sep="")
  }
  verify <- search_scoping_history(review_key)
  if (!as.character(run_id)%in%as.character(verify$run_id))
    stop("Scoping result write could not be verified",call.=FALSE)
  invisible(TRUE)
}

# Append-only, review-scoped W00 monitoring registry.
w00_monitor_columns <- c("review_key","request_id","run_id","event_type","timestamp_utc")
w00_monitor_path <- function() Sys.getenv("LEM_W00_MONITOR_LOG",
  unset="local_state/w00_monitor_events.jsonl")
w00_monitor_events <- function(review_key) {
  if (identical(storage_backend(),"google_sheets")) {
    gs4_auth_from_env()
    ss <- sheet_id_from_env()
    if (!"w00_monitor_events" %in% sheet_names_cached(ss)) return(data.frame())
    rows <- googlesheets4::read_sheet(ss,sheet="w00_monitor_events",col_types="c")
  } else {
    path <- w00_monitor_path()
    if(!file.exists(path)) return(data.frame())
    lines <- readLines(path,warn=FALSE)
    if(!length(lines)) return(data.frame())
    rows <- do.call(rbind,lapply(lines,function(x)
      as.data.frame(jsonlite::fromJSON(x),stringsAsFactors=FALSE)))
  }
  if(!all(w00_monitor_columns%in%names(rows)))
    stop("Incompatible W00 monitoring schema",call.=FALSE)
  rows[!is.na(rows$review_key)&rows$review_key==review_key,,drop=FALSE]
}
w00_monitor_record <- function(review_key,request_id,run_id="") {
  if(length(request_id)!=1L || is.na(request_id) ||
      !grepl("^[A-Za-z0-9._-]{8,80}$",request_id))
    stop("Invalid W00 request ID",call.=FALSE)
  if(length(run_id)!=1L || is.na(run_id) ||
      (nzchar(run_id) && !grepl("^[0-9]+$",run_id)))
    stop("Invalid W00 run ID",call.=FALSE)
  history <- w00_monitor_events(review_key)
  if(nrow(history) && any(as.character(history$request_id)==request_id &
      as.character(history$run_id)==run_id)) return(invisible(FALSE))
  if(nzchar(run_id) && (!nrow(history) ||
      !any(as.character(history$request_id)==request_id)))
    stop("W00 run cannot be linked without a recorded request",call.=FALSE)
  row <- list(review_key=review_key,request_id=request_id,run_id=run_id,
    event_type=if(nzchar(run_id)) "matched" else "dispatched",
    timestamp_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"))
  if(identical(storage_backend(),"google_sheets")) {
    gs4_auth_from_env()
    ss <- sheet_id_from_env()
    if(!"w00_monitor_events"%in%sheet_names_cached(ss)) {
      empty <- as.data.frame(setNames(replicate(length(w00_monitor_columns),
        character(),simplify=FALSE),w00_monitor_columns),stringsAsFactors=FALSE)
      googlesheets4::sheet_write(empty,ss=ss,sheet="w00_monitor_events")
      invalidate_sheet_names_cache(ss)
    }
    googlesheets4::sheet_append(ss,
      data=as.data.frame(row,stringsAsFactors=FALSE),sheet="w00_monitor_events")
  } else {
    path <- w00_monitor_path()
    dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
    cat(as.character(jsonlite::toJSON(row,auto_unbox=TRUE)),"\\n",
      file=path,append=TRUE,sep="")
  }
  verify <- w00_monitor_events(review_key)
  if(!any(as.character(verify$request_id)==request_id &
      as.character(verify$run_id)==run_id))
    stop("W00 monitoring persistence could not be verified",call.=FALSE)
  invisible(TRUE)
}
w00_monitor_latest <- function(review_key) {
  history <- w00_monitor_events(review_key)
  if(!nrow(history)) return(NULL)
  request_id <- as.character(tail(history$request_id,1L))
  rows <- history[as.character(history$request_id)==request_id,,drop=FALSE]
  ids <- as.character(rows$run_id)
  ids <- ids[!is.na(ids)&grepl("^[0-9]+$",ids)]
  list(request_id=request_id,run_id=if(length(ids)) tail(ids,1L) else "")
}
