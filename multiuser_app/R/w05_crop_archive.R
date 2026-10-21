# W05 versioned private draft. Never publish the draft during upload or testing.
w05_crop_draft_id <- function(meta) {
  id <- if("w05_crop_zenodo_draft_id" %in% names(meta$values))
    as.character(meta$values[["w05_crop_zenodo_draft_id"]]) else ""
  if(length(id)!=1L || is.na(id))return("")
  if(nzchar(id) && !grepl("^[0-9]+$",id))stop("Invalid W05 Zenodo deposit ID",call.=FALSE)
  id
}
w05_crop_draft <- function(meta) {
  id <- w05_crop_draft_id(meta)
  if(!nzchar(id))return(NULL)
  dep <- w04_prompt_zenodo_request(w04_prompt_deposit_endpoint(id))
  if(!identical(as.character(dep$metadata$title),
                paste("Living Evidence Map W05 crop coding |",meta$review_id)))
    stop("W05 Zenodo draft belongs to another review",call.=FALSE)
  if(isTRUE(dep$submitted))stop("W05 coding archive has already been published",call.=FALSE)
  dep
}
w05_crop_archive_upload <- function(meta,path) {
  dict <- w05_crop_read(path)
  sha <- digest::digest(file=path,algo="sha256",serialize=FALSE)
  fname <- paste0("w05-crops-",substr(sha,1L,20L),".csv")
  dep <- w05_crop_draft(meta)
  if(is.null(dep)) {
    # Create a draft only after explicit upload, never when opening the W05 tab.
    creators <- jsonlite::fromJSON(read_github_text_file("user_input/zenodo_config.json"),
      simplifyVector=FALSE)$creators
    if(!length(creators))stop("Missing Zenodo creators",call.=FALSE)
    created <- w04_prompt_zenodo_request(w04_prompt_deposit_endpoint(),
      method="POST",body=list(),expected=201L)
    id <- as.character(created$id)
    if(!grepl("^[0-9]+$",id))stop("Zenodo did not identify created draft",call.=FALSE)
    tryCatch({
      w04_prompt_zenodo_request(w04_prompt_deposit_endpoint(id),"PUT",
        body=list(metadata=list(title=paste("Living Evidence Map W05 crop coding |",meta$review_id),
          upload_type="dataset",publication_date=format(Sys.Date(),"%Y-%m-%d"),
          description="Restricted draft of review-specific deterministic crop codes and experimental W05 configuration.",
          creators=creators,access_right="restricted",
          access_conditions="Private W05 coding configuration.",
          keywords=c("Living Evidence Map","W05","crop coding"))),expected=200L)
      googlesheets4::sheet_append(meta$sheet_id,
        data=data.frame(key="w05_crop_zenodo_draft_id",value=id),
        sheet="review_metadata")
      verified <- w04_prompt_review_metadata(force=TRUE)
      if(!identical(w05_crop_draft_id(verified),id))
        stop("W05 draft ID could not be verified in review metadata",call.=FALSE)
    },error=function(e)stop(paste("W05 Zenodo draft",id,
      "may exist but registration is incomplete. Do not create another draft:",
      conditionMessage(e)),call.=FALSE))
    dep <- w05_crop_draft(w04_prompt_review_metadata(force=TRUE))
  }
  old <- Filter(function(x)identical(as.character(x$filename),fname),
                dep$files %||% list())
  if(length(old)>1L)stop("Duplicate W05 version identity",call.=FALSE)
  if(!length(old)) {
    bucket <- as.character(dep$links$bucket %||% "")
    if(!startsWith(bucket,"https://zenodo.org/api/files/"))
      stop("Unexpected Zenodo draft bucket",call.=FALSE)
    w04_prompt_zenodo_request(paste0(bucket,"/",fname),"PUT",
       file=path,expected=c(200L,201L))
  }
  refreshed <- w05_crop_draft(w04_prompt_review_metadata())
  check <- Filter(function(x)identical(as.character(x$filename),fname),
                  refreshed$files %||% list())
  if(length(check)!=1L)stop("W05 draft upload is not verified; do not retry blindly",call.=FALSE)
  url <- as.character(check[[1L]]$links$download)
  if(!startsWith(url,"https://zenodo.org/"))stop("Untrusted W05 draft download",call.=FALSE)
  req <- httr2::req_headers(httr2::request(url),
          Authorization=paste("Bearer",Sys.getenv("ZENODO_ACCESS_TOKEN",unset="")))
  resp <- httr2::req_perform(httr2::req_timeout(req,120))
  if(httr2::resp_status(resp)!=200L ||
     !identical(digest::digest(httr2::resp_body_raw(resp),algo="sha256",serialize=FALSE),sha))
    stop("W05 draft checksum could not be verified",call.=FALSE)
  list(deposit_id=as.character(refreshed$id),filename=fname,sha256=sha,
       codes=nrow(dict))
}

# Approval is an immutable, review-scoped object in the same restricted draft.
# No W05 execution or draft publication occurs here.
w05_crop_approve_version <- function(meta,receipt) {
  sha <- as.character(receipt$sha256 %||% "")
  id <- as.character(receipt$deposit_id %||% "")
  file <- as.character(receipt$filename %||% "")
  if(!grepl("^[0-9a-f]{64}$",sha) || !grepl("^[0-9]+$",id) ||
     !grepl("^w05-crops-[0-9a-f]{20}\\.csv$",file) ||
     !identical(file,paste0("w05-crops-",substr(sha,1L,20L),".csv")))
    stop("Cannot approve an unverified W05 dictionary receipt",call.=FALSE)
  dep <- w05_crop_draft(meta)
  if(is.null(dep) || !identical(as.character(dep$id),id))
    stop("W05 approval deposit does not match this review",call.=FALSE)
  files <- dep$files %||% list()
  if(sum(vapply(files,function(x)identical(as.character(x$filename),file),logical(1)))!=1L)
    stop("W05 dictionary version is absent from the restricted draft",call.=FALSE)
  marker <- paste0("w05-approved-",sha,".json")
  existing <- Filter(function(x)identical(as.character(x$filename),marker),files)
  if(length(existing)>1L)stop("Duplicate W05 schema approval markers",call.=FALSE)
  if(!length(existing)) {
    path <- tempfile(fileext=".json")
    on.exit(unlink(path),add=TRUE)
    writeLines(jsonlite::toJSON(list(
      schema="lem-w05-shiny-schema-approval-v1",review_id=meta$review_id,
      deposit_id=id,dictionary_file=file,dictionary_sha256=sha,
      approved_at_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC")
    ),auto_unbox=TRUE,pretty=TRUE),path,useBytes=TRUE)
    bucket <- as.character(dep$links$bucket %||% "")
    if(!startsWith(bucket,"https://zenodo.org/api/files/"))
      stop("Unexpected W05 approval archive location",call.=FALSE)
    w04_prompt_zenodo_request(paste0(bucket,"/",marker),"PUT",
                              file=path,expected=c(200L,201L))
  }
  fresh <- w05_crop_draft(w04_prompt_review_metadata(force=TRUE))
  if(sum(vapply(fresh$files %||% list(),
                function(x)identical(as.character(x$filename),marker),
                logical(1)))!=1L)
    stop("W05 schema approval was not confirmed in restricted Zenodo draft",call.=FALSE)
  invisible(TRUE)
}

# Archive a separately materialised, verified W04-included corpus in the
# review-specific W05 draft. No workflow dispatch and no draft publication.
w05_shiny_archive_input <- function(meta, input_path, provenance_path, receipt) {
  if(!file.exists(input_path) || !file.exists(provenance_path))
    stop("Materialised W05 input and provenance are required",call.=FALSE)
  provenance <- jsonlite::fromJSON(provenance_path,simplifyVector=FALSE)
  w05_shiny_verify_w04_bridge(receipt,provenance,meta$review_id)
  sha <- digest::digest(file=input_path,algo="sha256",serialize=FALSE)
  if(!identical(sha,as.character(provenance$input_sha256)))
    stop("Materialised W05 input bytes do not match provenance",call.=FALSE)
  dep <- w05_crop_draft(meta)
  if(is.null(dep))stop("Upload a coding dictionary to create the W05 restricted draft first",call.=FALSE)
  name <- paste0("w05-shiny-w04-input-",substr(sha,1L,20L),".jsonl")
  files <- dep$files %||% list()
  matches <- Filter(function(x)identical(as.character(x$filename),name),files)
  if(length(matches)>1L)stop("Duplicate archived W05 input",call.=FALSE)
  if(!length(matches)) {
    bucket <- as.character(dep$links$bucket %||% "")
    if(!startsWith(bucket,"https://zenodo.org/api/files/"))
      stop("Unexpected W05 Zenodo draft bucket",call.=FALSE)
    w04_prompt_zenodo_request(paste0(bucket,"/",name),method="PUT",
                              file=input_path,expected=c(200L,201L))
  }
  fresh <- w05_crop_draft(w04_prompt_review_metadata(force=TRUE))
  verified <- Filter(function(x)identical(as.character(x$filename),name),
                     fresh$files %||% list())
  if(length(verified)!=1L)
    stop("W05 input upload is unverified; do not retry blindly",call.=FALSE)
  url <- as.character(verified[[1L]]$links$download %||% "")
  if(!startsWith(url,"https://zenodo.org/"))
    stop("Untrusted W05 input download URL",call.=FALSE)
  req <- httr2::req_headers(httr2::request(url),
    Authorization=paste("Bearer",Sys.getenv("ZENODO_ACCESS_TOKEN",unset="")))
  response <- httr2::req_perform(httr2::req_timeout(req,180))
  if(httr2::resp_status(response)!=200L ||
     !identical(digest::digest(httr2::resp_body_raw(response),
       algo="sha256",serialize=FALSE),sha))
    stop("Archived W05 input byte verification failed",call.=FALSE)
  list(deposit_id=as.character(fresh$id),filename=name,sha256=sha,
       record_count=as.integer(provenance$record_count),
       included_ids_sha256=as.character(provenance$included_ids_sha256))
}
