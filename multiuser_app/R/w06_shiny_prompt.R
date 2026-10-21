# Review-agnostic W06 prompt staging. No geographic rules or model dispatch.
w06_shiny_prompt_text <- function(path) {
  if(!file.exists(path) || is.na(file.info(path)$size) ||
     file.info(path)$size > 1024*1024)
    stop("W06 prompt must be a text file smaller than 1 MB",call.=FALSE)
  content <- paste(readLines(path,warn=FALSE,encoding="UTF-8"),collapse="\n")
  if(!nzchar(trimws(content)))stop("W06 prompt is empty",call.=FALSE)
  content
}
w06_shiny_prompt_draft_id <- function(meta) {
  id <- if("w06_prompt_zenodo_draft_id" %in% names(meta$values))
    as.character(meta$values[["w06_prompt_zenodo_draft_id"]]) else ""
  if(length(id)!=1L || is.na(id) ||
     (nzchar(id) && !grepl("^[0-9]+$",id)))
    stop("Invalid review W06 prompt draft",call.=FALSE)
  id
}
w06_shiny_prompt_deposit <- function(meta) {
  id <- w06_shiny_prompt_draft_id(meta)
  if(!nzchar(id))return(NULL)
  dep <- w04_prompt_zenodo_request(w04_prompt_deposit_endpoint(id))
  if(!identical(as.character(dep$metadata$title),
                 paste("Living Evidence Map W06 prompt development |",meta$review_id)) ||
     isTRUE(dep$submitted))
    stop("W06 prompt draft is not an unpublished draft for this review",call.=FALSE)
  dep
}
w06_shiny_prompt_create <- function(meta) {
  old <- w06_shiny_prompt_draft_id(w04_prompt_review_metadata(force=TRUE))
  if(nzchar(old))return(old)
  creators <- jsonlite::fromJSON(read_github_text_file("user_input/zenodo_config.json"),
    simplifyVector=FALSE)$creators
  if(!length(creators))stop("Missing Zenodo creators",call.=FALSE)
  created <- w04_prompt_zenodo_request(w04_prompt_deposit_endpoint(),"POST",
                                      body=list(),expected=201L)
  id <- as.character(created$id)
  tryCatch({
    w04_prompt_zenodo_request(w04_prompt_deposit_endpoint(id),"PUT",
      body=list(metadata=list(title=paste("Living Evidence Map W06 prompt development |",meta$review_id),
        upload_type="dataset",publication_date=format(Sys.Date(),"%Y-%m-%d"),
        description="Restricted review-specific W06 model prompts, approval and trials.",
        creators=creators,access_right="restricted",
        access_conditions="Private review configuration.")),expected=200L)
    googlesheets4::sheet_append(meta$sheet_id,
      data=data.frame(key="w06_prompt_zenodo_draft_id",value=id),
      sheet="review_metadata")
    verified <- w04_prompt_review_metadata(force=TRUE)
    if(!identical(w06_shiny_prompt_draft_id(verified),id))
      stop("W06 prompt draft registration not verified",call.=FALSE)
  },error=function(e)stop(sprintf("W06 draft %s may exist; reconcile before retry: %s",
                                  id,conditionMessage(e)),call.=FALSE))
  id
}
w06_shiny_prompt_save <- function(meta,path) {
  content <- w06_shiny_prompt_text(path)
  sha <- digest::digest(content,algo="sha256",serialize=FALSE)
  name <- paste0("w06-prompt-",sha,".json")
  id <- w06_shiny_prompt_draft_id(meta)
  if(!nzchar(id))id <- w06_shiny_prompt_create(meta)
  dep <- w06_shiny_prompt_deposit(w04_prompt_review_metadata(force=TRUE))
  existing <- Filter(function(f)identical(as.character(f$filename),name),dep$files %||% list())
  if(length(existing)>1L)stop("Duplicate W06 prompt version",call.=FALSE)
  if(!length(existing)) {
    version <- list(schema="lem-w06-topic-agnostic-prompt-v1",review_id=meta$review_id,
      sha256=sha,content=content,created_at_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"))
    file <- tempfile(fileext=".json")
    on.exit(unlink(file),add=TRUE)
    jsonlite::write_json(version,file,auto_unbox=TRUE,pretty=TRUE)
    bucket <- as.character(dep$links$bucket)
    if(!startsWith(bucket,"https://zenodo.org/api/files/"))
      stop("Untrusted W06 prompt draft bucket",call.=FALSE)
    w04_prompt_zenodo_request(paste0(sub("/$","",bucket),"/",name),
      "PUT",file=file,expected=c(200L,201L))
  }
  dep <- w06_shiny_prompt_deposit(w04_prompt_review_metadata(force=TRUE))
  found <- Filter(function(f)identical(as.character(f$filename),name),dep$files %||% list())
  if(length(found)!=1L)stop("W06 prompt archive not verified",call.=FALSE)
  url <- as.character(found[[1L]]$links$download)
  if(!startsWith(url,"https://zenodo.org/"))stop("Unexpected W06 prompt retrieval URL",call.=FALSE)
  token <- Sys.getenv("ZENODO_ACCESS_TOKEN",unset="")
  req <- httr2::request(url) |> httr2::req_headers(Authorization=paste("Bearer",token))
  resp <- httr2::req_perform(httr2::req_timeout(req,120))
  obj <- jsonlite::fromJSON(httr2::resp_body_string(resp),simplifyVector=FALSE)
  if(!identical(obj$review_id,meta$review_id) ||
     !identical(obj$sha256,sha) || !identical(obj$content,content))
    stop("W06 prompt archive contents do not match submitted bytes",call.=FALSE)
  list(draft_id=id,filename=name,sha256=sha,content=content)
}
w06_shiny_prompt_approve <- function(meta,receipt) {
  dep <- w06_shiny_prompt_deposit(meta)
  if(is.null(dep)||!identical(as.character(dep$id),receipt$draft_id))
    stop("W06 draft is not the approved review draft",call.=FALSE)
  name <- paste0("w06-approved-",receipt$sha256,".json")
  found <- Filter(function(f)identical(as.character(f$filename),name),dep$files %||% list())
  if(length(found)>1L)stop("Duplicate W06 approval",call.=FALSE)
  if(!length(found)) {
    tmp <- tempfile(fileext=".json")
    on.exit(unlink(tmp),add=TRUE)
    jsonlite::write_json(list(schema="lem-w06-prompt-approval-v1",
      review_id=meta$review_id,prompt_sha256=receipt$sha256,
      prompt_filename=receipt$filename),tmp,auto_unbox=TRUE,pretty=TRUE)
    bucket <- as.character(dep$links$bucket)
    if(!startsWith(bucket,"https://zenodo.org/api/files/"))
      stop("Untrusted W06 prompt draft bucket",call.=FALSE)
    w04_prompt_zenodo_request(paste0(sub("/$","",bucket),"/",name),"PUT",
      file=tmp,expected=c(200L,201L))
  }
  dep <- w06_shiny_prompt_deposit(meta)
  found <- Filter(function(f)identical(as.character(f$filename),name),dep$files %||% list())
  if(length(found)!=1L)stop("W06 prompt approval marker not persisted",call.=FALSE)
  invisible(TRUE)
}
