# W05 versioned private draft. Never publish the draft during upload or testing.
w05_crop_draft_id <- function(meta) {
  id <- as.character(meta$values[["w05_crop_zenodo_draft_id"]] %||% "")
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
