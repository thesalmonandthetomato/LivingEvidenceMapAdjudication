# Explicit, review-specific W07 completion handoff. Never dispatches W08.
w07_handoff_tab <- function() "w07_review_handoff_requests"
w07_handoff_columns <- c("event_id","review_id","run_id","user_id","sha256",
  "archive_filename","status","timestamp")
w07_handoff_records <- function(meta,create=FALSE) {
  gs4_auth_from_env()
  tab <- w07_handoff_tab()
  if(!tab %in% sheet_names_cached(meta$sheet_id)) {
    if(!create)return(data.frame())
    empty <- as.data.frame(setNames(rep(list(character()),length(w07_handoff_columns)),
       w07_handoff_columns),stringsAsFactors=FALSE)
    googlesheets4::sheet_write(empty,ss=meta$sheet_id,sheet=tab)
    invalidate_sheet_names_cache(meta$sheet_id)
  }
  x <- as.data.frame(googlesheets4::read_sheet(meta$sheet_id,sheet=tab,
                                                 col_types="c"),stringsAsFactors=FALSE)
  if(!identical(names(x),w07_handoff_columns))
    stop("Unrecognised W07 handoff ledger schema; nothing dispatched",call.=FALSE)
  x
}
w07_handoff_event <- function(meta,run_id,user_id,sha,filename,status) {
  if(!status %in% c("requested","dispatched"))
    stop("Invalid W07 handoff status",call.=FALSE)
  now <- format(Sys.time(),"%Y-%m-%dT%H:%M:%OS6Z",tz="UTC")
  row <- data.frame(event_id=digest::digest(paste(meta$review_id,run_id,
    user_id,sha,status,now,sep="|"),algo="sha256"),
    review_id=meta$review_id,run_id=as.character(run_id),user_id=user_id,
    sha256=sha,archive_filename=filename,status=status,timestamp=now,
    stringsAsFactors=FALSE)
  googlesheets4::sheet_append(meta$sheet_id,sheet=w07_handoff_tab(),data=row)
  invisible(row)
}
w07_handoff_archive <- function(meta,filename,filepath,file_sha) {
  dep <- w07_shiny_config_deposit(meta)
  if(is.null(dep) || isTRUE(dep$submitted) ||
     !identical(as.character(dep$metadata$access_right),"restricted"))
    stop("W07 handoff requires the matching restricted unpublished draft",call.=FALSE)
  filenames <- vapply(dep$files %||% list(),function(f)
    as.character(f$filename %||% ""),character(1))
  if(sum(filenames==filename)>1L)
    stop("Duplicate W07 handoff archive filename",call.=FALSE)
  if(!filename %in% filenames) {
    bucket <- as.character(dep$links$bucket %||% "")
    if(!startsWith(bucket,"https://zenodo.org/api/files/"))
      stop("Untrusted W07 handoff archive bucket",call.=FALSE)
    w04_prompt_zenodo_request(paste0(sub("/$","",bucket),"/",filename),
      "PUT",file=filepath,expected=c(200L,201L))
    dep <- w07_shiny_config_deposit(meta)
  }
  matching <- Filter(function(f)identical(as.character(f$filename),filename),
                     dep$files %||% list())
  if(length(matching)!=1L)
    stop("W07 archived handoff cannot be found uniquely",call.=FALSE)
  url <- as.character(matching[[1L]]$links$download %||% "")
  if(!startsWith(url,"https://zenodo.org/"))
    stop("Untrusted W07 handoff download URL",call.=FALSE)
  token <- Sys.getenv("ZENODO_ACCESS_TOKEN",unset="")
  if(!nzchar(token))stop("Zenodo token unavailable",call.=FALSE)
  req <- httr2::req_auth_bearer_token(httr2::request(url),token)
  actual <- httr2::resp_body_raw(httr2::req_perform(httr2::req_timeout(req,120)))
  if(!identical(digest::digest(actual,algo="sha256",serialize=FALSE),file_sha))
    stop("Archived W07 handoff checksum mismatch",call.=FALSE)
  invisible(as.character(dep$id))
}
w07_shiny_handoff <- function(meta,result,events,run_id,user_id) {
  payload <- w07_review_consolidate(result,events,meta$review_id,run_id,user_id)
  tmp <- tempfile(fileext=".json")
  on.exit(unlink(tmp),add=TRUE)
  jsonlite::write_json(payload,tmp,auto_unbox=TRUE,pretty=FALSE,null="null",
                       dataframe="rows")
  bytes <- readBin(tmp,what="raw",n=file.info(tmp)$size)
  file_sha <- digest::digest(bytes,algo="sha256",serialize=FALSE)
  filename <- paste0("w07-reviewed-",run_id,"-",file_sha,".json")
  current <- w07_handoff_records(meta,create=TRUE)
  match <- current$review_id==meta$review_id & current$run_id==as.character(run_id) &
    current$user_id==as.character(user_id) & current$sha256==file_sha
  if(any(match & current$status=="dispatched",na.rm=TRUE))
    return(list(status="already_dispatched",sha256=file_sha,filename=filename))
  deposit_id <- w07_handoff_archive(meta,filename,tmp,file_sha)
  if(!any(match & current$status=="requested",na.rm=TRUE))
    w07_handoff_event(meta,run_id,user_id,file_sha,filename,"requested")
  url <- sprintf("https://api.github.com/repos/%s/actions/workflows/w07_shiny_review_handoff.yml/dispatches",
                 w05_shiny_repo())
  w05_shiny_gh_request(url,"POST",list(ref="main",inputs=list(
    review_id=meta$review_id,source_run_id=as.character(run_id),
    config_deposit_id=deposit_id,archive_filename=filename,archive_sha256=file_sha)))
  w07_handoff_event(meta,run_id,user_id,file_sha,filename,"dispatched")
  list(status="dispatched",sha256=file_sha,filename=filename)
}
