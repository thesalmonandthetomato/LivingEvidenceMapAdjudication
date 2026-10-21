# Restricted Zenodo draft storage for W04 prompt versions.
# This is independent of W04 production screening and other Zenodo records.

w04_prompt_zenodo_request <- function(endpoint, method="GET", body=NULL, file=NULL,
                                      expected=c(200L)) {
  token <- Sys.getenv("ZENODO_ACCESS_TOKEN",unset="")
  if(!nzchar(token)) stop("Zenodo token is not configured",call.=FALSE)
  req <- httr2::req_headers(httr2::request(endpoint),
    Authorization=paste("Bearer",token))
  req <- httr2::req_method(req,method)
  if(identical(method,"POST") && is.list(body) && length(body)==0L) {
    req <- httr2::req_body_raw(req,charToRaw("{}"),type="application/json")
  } else if(!is.null(body)) req <- httr2::req_body_json(req,body,auto_unbox=TRUE)
  if(!is.null(file)) req <- httr2::req_body_file(req,file)
  # Retry only read-only requests. Never silently repeat a draft creation or
  # bucket upload: Zenodo may have persisted a write despite an HTTP 500.
  attempts <- if(identical(method,"GET"))2L else 1L
  resp <- NULL
  for(attempt in seq_len(attempts)) {
    resp <- tryCatch(httr2::req_perform(httr2::req_error(httr2::req_timeout(req,120),
      is_error=function(resp)FALSE)),error=function(e)e)
    status <- if(inherits(resp,"error"))NA_integer_ else httr2::resp_status(resp)
    if(status %in% expected)break
    transient <- inherits(resp,"error") || (!is.na(status) && status %in% c(500L,502L,503L,504L))
    if(attempt==attempts || !transient)break
    Sys.sleep(1)
  }
  if(inherits(resp,"error"))
    stop(paste("Zenodo W04 draft request failed:",conditionMessage(resp),
      "No further request was attempted."),call.=FALSE)
  status <- httr2::resp_status(resp)
  if(!status %in% expected)
    stop(sprintf("Zenodo W04 draft HTTP %d: %s%s",status,
      substr(httr2::resp_body_string(resp),1,240),
      if(identical(method,"GET"))" (read-only retry exhausted; no publication dispatched)" else
        " (write outcome must be checked before any retry; no publication dispatched)"),call.=FALSE)
  if(status==204L) return(NULL)
  httr2::resp_body_json(resp,simplifyVector=FALSE)
}
w04_prompt_deposit_endpoint <- function(id="") {
  if(nzchar(id) && !grepl("^[0-9]+$",id))
    stop("Invalid W04 Zenodo deposit ID",call.=FALSE)
  paste0("https://zenodo.org/api/deposit/depositions",if(nzchar(id)) paste0("/",id) else "")
}
w04_prompt_review_metadata <- function(force=FALSE) {
  if(!identical(storage_backend(),"google_sheets"))
    stop("W04 prompt archives require the review-specific Google Sheets backend",call.=FALSE)
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  data <- as.data.frame(read_review_metadata_cached(ss,force=force))
  if(!all(c("key","value")%in%names(data))) stop("Invalid review_metadata",call.=FALSE)
  if(anyDuplicated(data$key)) stop("Duplicate keys in review_metadata",call.=FALSE)
  review_id <- as.character(data$value[match("review_id",data$key)])
  if(length(review_id)!=1L||is.na(review_id)||
     !grepl("^[A-Za-z0-9][A-Za-z0-9._-]{2,127}$",review_id))
    stop("Missing review-specific review_id",call.=FALSE)
  list(sheet_id=ss,review_id=review_id,values=stats::setNames(as.character(data$value),
    as.character(data$key)))
}
w04_prompt_draft_id <- function(meta) {
  id <- if("w04_prompt_zenodo_draft_id" %in% names(meta$values))
    meta$values[["w04_prompt_zenodo_draft_id"]] else NULL
  if(is.null(id)||is.na(id)||!nzchar(id)) return("")
  if(!grepl("^[0-9]+$",id)) stop("Invalid stored W04 draft ID",call.=FALSE)
  id
}
w04_prompt_create_draft <- function(meta) {
  existing <- w04_prompt_draft_id(meta)
  if(nzchar(existing)) return(existing)
  # Re-read before creating to avoid needless duplicate records across sessions.
  fresh <- w04_prompt_review_metadata(force=TRUE)
  if(!identical(meta$review_id,fresh$review_id)) stop("Review changed",call.=FALSE)
  existing <- w04_prompt_draft_id(fresh)
  if(nzchar(existing)) return(existing)
  cfg <- jsonlite::fromJSON(read_github_text_file("user_input/zenodo_config.json"),
    simplifyVector=FALSE)
  if(!length(cfg$creators)) stop("Zenodo creators not configured",call.=FALSE)
  created <- w04_prompt_zenodo_request(w04_prompt_deposit_endpoint(),"POST",
    body=list(),expected=201L)
  id <- as.character(created$id)
  if(!grepl("^[0-9]+$",id)) stop("Zenodo created draft without ID",call.=FALSE)
  tryCatch({
    w04_prompt_zenodo_request(w04_prompt_deposit_endpoint(id),"PUT",
      body=list(metadata=list(
        title=paste("Living Evidence Map W04 prompt development |",meta$review_id),
        upload_type="dataset",publication_date=format(Sys.Date(),"%Y-%m-%d"),
        description=paste("Restricted W04 prompt development and validation archive for review",
          meta$review_id,". Separate from production screening results."),
        creators=cfg$creators,access_right="restricted",
        access_conditions="Development prompts and associated validation history are private.",
        keywords=c("Living Evidence Map","W04","prompt development",
                   paste0("review:",meta$review_id)))),
      expected=200L)
    # Append a new review-specific metadata key; never change review_id or other settings.
    googlesheets4::sheet_append(meta$sheet_id,
      data=data.frame(key="w04_prompt_zenodo_draft_id",value=id),
      sheet="review_metadata")
    verified <- w04_prompt_review_metadata(force=TRUE)
    if(!identical(verified$review_id,meta$review_id) ||
       !identical(w04_prompt_draft_id(verified),id))
      stop("Review metadata did not verify the Zenodo draft ID",call.=FALSE)
    id
  },error=function(e)stop(sprintf("W04 Zenodo draft %s created but not registered: %s. Reconcile this draft before retrying.",
        id,conditionMessage(e)),call.=FALSE))
}
w04_prompt_fetch_draft <- function(meta) {
  id <- w04_prompt_draft_id(meta)
  if(!nzchar(id)) return(NULL)
  x <- w04_prompt_zenodo_request(w04_prompt_deposit_endpoint(id))
  if(isTRUE(x$submitted))
    stop("W04 prompt archive is already published",call.=FALSE)
  expected <- paste("Living Evidence Map W04 prompt development |",meta$review_id)
  if(!identical(as.character(x$metadata$title),expected))
    stop("W04 Zenodo draft does not match the active review",call.=FALSE)
  x
}
w04_prompt_archive_versions <- function(meta) {
  deposit <- w04_prompt_fetch_draft(meta)
  if(is.null(deposit)) return(list())
  files <- deposit$files
  if(!length(files)) return(list())
  selected <- Filter(function(x) grepl("^w04-[0-9a-f]{20}[.]json$",
    as.character(x$filename %||% "")),files)
  if(!length(selected)) return(list())
  lapply(selected,function(f) {
    url <- as.character(f$links$download %||% "")
    if(!nzchar(url)) stop("Missing prompt download URL",call.=FALSE)
    token <- Sys.getenv("ZENODO_ACCESS_TOKEN",unset="")
    req <- httr2::req_headers(httr2::request(url),
      Authorization=paste("Bearer",token))
    resp <- httr2::req_perform(req)
    if(httr2::resp_status(resp)!=200L) stop("Cannot retrieve archived prompt",call.=FALSE)
    x <- jsonlite::fromJSON(httr2::resp_body_string(resp),simplifyVector=FALSE)
    if(!identical(x$review_id,meta$review_id) ||
       !identical(paste0(x$version_id,".json"),f$filename) ||
       !identical(w04_prompt_checksum(x$content),x$sha256))
      stop("Archived W04 prompt identity/checksum mismatch",call.=FALSE)
    x
  })
}
w04_prompt_archive_save <- function(meta,version) {
  if(!identical(meta$review_id,version$review_id) ||
     !identical(w04_prompt_checksum(version$content),version$sha256))
    stop("W04 prompt review/checksum mismatch",call.=FALSE)
  id <- w04_prompt_draft_id(meta)
  if(!nzchar(id)) id <- w04_prompt_create_draft(meta)
  deposit <- w04_prompt_fetch_draft(w04_prompt_review_metadata())
  if(is.null(deposit)||!identical(as.character(deposit$id),id))
    stop("Could not verify W04 prompt draft",call.=FALSE)
  filename <- paste0(version$version_id,".json")
  names <- vapply(deposit$files %||% list(),function(x)
    as.character(x$filename %||% ""),character(1))
  if(filename %in% names) stop("Prompt version already exists and is immutable",call.=FALSE)
  bucket <- as.character(deposit$links$bucket %||% "")
  if(!startsWith(bucket,"https://zenodo.org/api/files/"))
    stop("Unexpected Zenodo draft upload location",call.=FALSE)
  tmp <- tempfile(fileext=".json")
  on.exit(unlink(tmp),add=TRUE)
  writeLines(jsonlite::toJSON(version,auto_unbox=TRUE,pretty=TRUE,null="null"),
    tmp,useBytes=TRUE)
  w04_prompt_zenodo_request(paste0(bucket,"/",filename),"PUT",
    file=tmp,expected=c(200L,201L))
  # Confirm the archived version was persisted and can be retrieved by checksum.
  matches <- Filter(function(x) identical(x$version_id,version$version_id),
    w04_prompt_archive_versions(w04_prompt_review_metadata()))
  if(length(matches)!=1L||!identical(matches[[1]]$sha256,version$sha256))
    stop("W04 prompt upload was not verified; reconcile before retrying",call.=FALSE)
  # Rebuild the manifest from verified, immutable archived versions.
  current <- w04_prompt_archive_versions(w04_prompt_review_metadata())
  manifest <- list(schema="lem-w04-prompt-manifest-v1",review_id=meta$review_id,
                   deposition_id=id,versions=w04_prompt_manifest(current))
  manifest_path <- tempfile(fileext=".json")
  on.exit(unlink(manifest_path),add=TRUE)
  writeLines(jsonlite::toJSON(manifest,auto_unbox=TRUE,pretty=TRUE,null="null"),
    manifest_path,useBytes=TRUE)
  w04_prompt_zenodo_request(paste0(bucket,"/manifest.json"),"PUT",
    file=manifest_path,expected=c(200L,201L))
  verify <- w04_prompt_fetch_draft(w04_prompt_review_metadata())
  manifest_files <- Filter(function(x) identical(as.character(x$filename),"manifest.json"),
    verify$files %||% list())
  if(length(manifest_files)!=1L)
    stop("W04 draft prompt manifest was not listed after upload",call.=FALSE)
  url <- as.character(manifest_files[[1L]]$links$download %||% "")
  req <- httr2::req_headers(httr2::request(url),
    Authorization=paste("Bearer",Sys.getenv("ZENODO_ACCESS_TOKEN",unset="")))
  resp <- httr2::req_perform(req)
  if(httr2::resp_status(resp)!=200L) stop("Could not verify W04 prompt manifest",call.=FALSE)
  observed <- jsonlite::fromJSON(httr2::resp_body_string(resp),simplifyVector=FALSE)
  found <- Filter(function(x) identical(x$version_id,version$version_id),
    observed$versions %||% list())
  if(!identical(observed$review_id,meta$review_id)||length(found)!=1L||
     !identical(found[[1]]$sha256,version$sha256))
    stop("W04 manifest verification failed",call.=FALSE)
  if(exists("w04_summary_record_safe",mode="function"))
    w04_summary_record_safe("prompt",version$version_id,version$created_at_utc,
      w04_overview_chr(version$notes,w04_overview_chr(version$original_filename,version$version_id)),
      paste("Saved by",version$author),review_id=meta$review_id,
      metadata=list(author=as.character(version$author),status=as.character(version$status)))
  invisible(version$version_id)
}
