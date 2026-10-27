# Durable, append-only W07 experimental review events in the review's restricted Zenodo draft.
# Separate from W04 screening and W08 final annotations.
w07_review_event_files <- function(meta) {
  dep <- w07_shiny_config_deposit(meta)
  if(is.null(dep))stop("W07 review storage has not been configured",call.=FALSE)
  fs <- Filter(function(f)grepl("^w07-review-[0-9]+-[A-Za-z0-9_-]+-[0-9a-f]{32}\\.json$",
    as.character(f$filename %||% "")),dep$files %||% list())
  list(deposit=dep,files=fs)
}
w07_review_events <- function(meta,run_id) {
  id <- as.character(run_id)
  if(!grepl("^[0-9]+$",id))stop("Invalid W07 run ID",call.=FALSE)
  fs <- w07_review_event_files(meta)$files
  fs <- Filter(function(f)startsWith(as.character(f$filename),paste0("w07-review-",id,"-")),fs)
  token <- Sys.getenv("ZENODO_ACCESS_TOKEN",unset="")
  events <- lapply(fs,function(f) {
    url <- as.character(f$links$download %||% "")
    if(!startsWith(url,"https://zenodo.org/"))
      stop("Untrusted W07 decision download URL",call.=FALSE)
    req <- httr2::req_auth_bearer_token(httr2::request(url),token)
    obj <- jsonlite::fromJSON(httr2::resp_body_string(httr2::req_perform(
      httr2::req_timeout(req,90))),simplifyVector=FALSE)
    if(!identical(obj$schema,"lem-w07-experimental-decision-v1")||
       !identical(obj$review_id,meta$review_id)||
       !identical(as.character(obj$run_id),id))
      stop("W07 review event scope mismatch",call.=FALSE)
    obj
  })
  events[order(vapply(events,function(e)as.character(e$timestamp),character(1)))]
}
w07_review_save <- function(meta,run_id,record_id,user_id,codes,relevance,reason) {
  id <- as.character(run_id)
  if(!grepl("^[0-9]+$",id))stop("Invalid W07 run ID",call.=FALSE)
  if(!nzchar(record_id)||!nzchar(user_id)||!nzchar(trimws(reason)))
    stop("Record, reviewer and justification are required",call.=FALSE)
  if(!relevance %in% c("CODED_IN_SCOPE","IN_SCOPE_ONTOLOGY_GAP","OUT_OF_SCOPE","INSUFFICIENT_INFORMATION"))
    stop("Invalid W07 relevance decision",call.=FALSE)
  if(nchar(codes)>4000||nchar(reason)>10000)stop("W07 review content too long",call.=FALSE)
  storage <- w07_review_event_files(meta)
  dep <- storage$deposit
  stamp <- format(Sys.time(),"%Y-%m-%dT%H:%M:%OS6Z",tz="UTC")
  salt <- paste(meta$review_id,id,record_id,user_id,stamp,runif(1),sep="|")
  suffix <- substr(digest::digest(salt,algo="sha256"),1,32)
  filename <- paste0("w07-review-",id,"-",substr(digest::digest(record_id,algo="sha256"),1,16),"-",suffix,".json")
  event <- list(schema="lem-w07-experimental-decision-v1",review_id=meta$review_id,
    run_id=id,record_id=record_id,user_id=user_id,codes=codes,
    relevance=relevance,reason=reason,timestamp=stamp,event_id=suffix)
  file <- tempfile(fileext=".json")
  on.exit(unlink(file),add=TRUE)
  jsonlite::write_json(event,file,auto_unbox=TRUE,pretty=TRUE)
  bucket <- as.character(dep$links$bucket %||% "")
  if(!startsWith(bucket,"https://zenodo.org/api/files/"))
    stop("Untrusted W07 decision storage",call.=FALSE)
  w04_prompt_zenodo_request(paste0(sub("/$","",bucket),"/",filename),
    "PUT",file=file,expected=c(200L,201L))
  fresh <- w07_review_event_files(meta)$files
  if(sum(vapply(fresh,function(f)identical(as.character(f$filename),filename),
                 logical(1)))!=1L)stop("W07 decision could not be verified after save",call.=FALSE)
  invisible(event)
}
w07_review_latest <- function(events) {
  if(!length(events))return(list())
  keys <- vapply(events,function(e)paste(e$record_id,e$user_id,sep="|"),character(1))
  events[!duplicated(keys,fromLast=TRUE)]
}
