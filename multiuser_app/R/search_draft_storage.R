# Draft-only storage. No W00 dispatch, Zenodo access or strategy finalisation.
search_draft_key <- function(review_id, user_id) {
  ids <- c(review_id, user_id)
  if (length(review_id)!=1L || length(user_id)!=1L ||
      anyNA(ids) || any(!nzchar(trimws(as.character(ids)))))
    stop("Review ID and user ID are required for search draft persistence.",call.=FALSE)
  digest::digest(paste(review_id,user_id,sep="|"),algo="sha256",serialize=FALSE)
}

search_draft_cols <- c("event_id","draft_key","search_string","saved_at_utc")
search_draft_local_path <- function() Sys.getenv("LEM_SEARCH_DRAFT_LOG",
  unset="local_state/search_drafts.jsonl")

search_draft_read <- function(key) {
  if (!nzchar(as.character(key))) stop("Missing draft key",call.=FALSE)
  if (identical(storage_backend(),"google_sheets")) {
    gs4_auth_from_env()
    ss <- sheet_id_from_env()
    tab <- "search_draft_events"
    if (!tab %in% sheet_names_cached(ss)) return(NULL)
    records <- googlesheets4::read_sheet(ss,sheet=tab,col_types="c")
  } else {
    path <- search_draft_local_path()
    if (!file.exists(path)) return(NULL)
    rows <- readLines(path,warn=FALSE)
    if (!length(rows)) return(NULL)
    records <- do.call(rbind,lapply(rows,function(s) {
      row <- jsonlite::fromJSON(s,simplifyVector=TRUE)
      as.data.frame(row,stringsAsFactors=FALSE)
    }))
  }
  if (!all(search_draft_cols %in% names(records)))
    stop("Search draft log has an incompatible schema",call.=FALSE)
  matching <- records[!is.na(records$draft_key) & records$draft_key==key,,drop=FALSE]
  if (!nrow(matching)) return(NULL)
  matching[nrow(matching),,drop=FALSE]
}

search_draft_save <- function(key, search_string) {
  if (length(search_string)!=1L || !is.character(search_string) || is.na(search_string))
    stop("Draft must be a single character string",call.=FALSE)
  if (!nzchar(as.character(key))) stop("Missing draft key",call.=FALSE)
  row <- list(
    event_id=paste0("draft-",digest::digest(paste(key,search_string,
      format(Sys.time(),"%Y%m%d%H%M%OS6",tz="UTC"),sep="|"),
      algo="sha256",serialize=FALSE)),
    draft_key=key,search_string=search_string,
    saved_at_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC")
  )
  if (identical(storage_backend(),"google_sheets")) {
    gs4_auth_from_env()
    ss <- sheet_id_from_env()
    tab <- "search_draft_events"
    if (!tab %in% sheet_names_cached(ss)) {
      sheet_add_cached(ss,tab)
      empty <- as.data.frame(setNames(replicate(length(search_draft_cols),
        character(),simplify=FALSE),search_draft_cols),stringsAsFactors=FALSE)
      googlesheets4::sheet_write(empty,ss=ss,sheet=tab)
    }
    googlesheets4::sheet_append(ss,data=as.data.frame(row,stringsAsFactors=FALSE),sheet=tab)
  } else {
    path <- search_draft_local_path()
    dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
    cat(as.character(jsonlite::toJSON(row,auto_unbox=TRUE)), "\n",
        file=path,append=TRUE,sep="")
  }
  check <- search_draft_read(key)
  if (is.null(check) || !identical(as.character(check$event_id[[1L]]),row$event_id))
    stop("Search draft write could not be verified. Do not retry blindly.",call.=FALSE)
  invisible(row)
}
