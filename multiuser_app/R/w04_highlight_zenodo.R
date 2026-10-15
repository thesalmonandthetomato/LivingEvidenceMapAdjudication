# Immutable include/exclude highlighting-term versions in the existing restricted W04 draft.
# Stored independently of prompt manifest.json and never alter prompt versions.
w04_highlight_parse <- function(text) {
  if(length(text)!=1L||is.na(text)) stop("Invalid highlight terms",call.=FALSE)
  terms <- trimws(unlist(strsplit(gsub("\r","\n",text,fixed=TRUE),"\n",fixed=TRUE)))
  terms <- terms[nzchar(terms)]
  unique(terms)
}
w04_highlight_version <- function(meta,include,exclude,author) {
  inc <- w04_highlight_parse(include)
  exc <- w04_highlight_parse(exclude)
  overlap <- intersect(tolower(inc),tolower(exc))
  if(length(overlap)) stop(paste("Terms cannot be both included and excluded:",paste(overlap,collapse=", ")),call.=FALSE)
  at <- format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%OS6Z")
  payload <- jsonlite::toJSON(list(include=inc,exclude=exc),auto_unbox=FALSE,null="null")
  sha <- digest::digest(payload,algo="sha256",serialize=FALSE)
  nonce <- digest::digest(paste(meta$review_id,at,sha,author,sep="|"),algo="sha256",serialize=FALSE)
  list(schema="lem-w04-highlight-terms-v1",review_id=meta$review_id,
       version_id=paste0("w04-highlight-",substr(nonce,1,20)),
       created_at_utc=at,author=as.character(author),
       include=as.list(inc),exclude=as.list(exc),sha256=sha)
}
w04_highlight_verify <- function(x,review_id) {
  if(!identical(x$review_id,review_id)||!identical(x$schema,"lem-w04-highlight-terms-v1"))
    stop("W04 highlight review mismatch",call.=FALSE)
  inc <- unlist(x$include,use.names=FALSE)
  exc <- unlist(x$exclude,use.names=FALSE)
  sha <- digest::digest(jsonlite::toJSON(list(include=inc,exclude=exc),auto_unbox=FALSE,null="null"),
                        algo="sha256",serialize=FALSE)
  if(!identical(sha,x$sha256)) stop("W04 highlight checksum mismatch",call.=FALSE)
  invisible(TRUE)
}
w04_highlight_history <- function(meta) {
  d <- w04_prompt_fetch_draft(meta)
  if(is.null(d)) return(list())
  files <- Filter(function(f)grepl("^w04-highlight-[0-9a-f]{20}[.]json$",
    as.character(f$filename %||% "")),d$files %||% list())
  result <- lapply(files,function(f) {
    req <- httr2::req_headers(httr2::request(as.character(f$links$download)),
       Authorization=paste("Bearer",Sys.getenv("ZENODO_ACCESS_TOKEN",unset="")))
    resp <- httr2::req_perform(httr2::req_timeout(req,120))
    if(httr2::resp_status(resp)!=200L) stop("Cannot retrieve highlight terms",call.=FALSE)
    x <- jsonlite::fromJSON(httr2::resp_body_string(resp),simplifyVector=FALSE)
    if(!identical(paste0(x$version_id,".json"),f$filename))stop("Highlight filename mismatch",call.=FALSE)
    w04_highlight_verify(x,meta$review_id)
    x
  })
  result[order(vapply(result,function(x)x$created_at_utc,character(1)),decreasing=TRUE)]
}
w04_highlight_latest <- function(meta) {
  x <- w04_highlight_history(meta)
  if(length(x)) x[[1L]] else NULL
}
w04_highlight_save <- function(meta,version) {
  w04_highlight_verify(version,meta$review_id)
  id <- w04_prompt_draft_id(meta)
  if(!nzchar(id)) id <- w04_prompt_create_draft(meta)
  d <- w04_prompt_fetch_draft(w04_prompt_review_metadata())
  bucket <- as.character(d$links$bucket %||% "")
  if(!startsWith(bucket,"https://zenodo.org/api/files/")) stop("Unexpected Zenodo bucket",call.=FALSE)
  file <- paste0(version$version_id,".json")
  if(any(vapply(d$files %||% list(),function(f)identical(f$filename,file),logical(1))))
    stop("Immutable highlight version already exists",call.=FALSE)
  path <- tempfile(fileext=".json")
  on.exit(unlink(path),add=TRUE)
  writeLines(jsonlite::toJSON(version,auto_unbox=TRUE,pretty=TRUE,null="null"),
             path,useBytes=TRUE)
  w04_prompt_zenodo_request(paste0(bucket,"/",file),"PUT",file=path,expected=c(200L,201L))
  found <- Filter(function(x)identical(x$version_id,version$version_id),
                  w04_highlight_history(w04_prompt_review_metadata()))
  if(length(found)!=1L)stop("Saved highlighting terms could not be verified",call.=FALSE)
  invisible(version$version_id)
}
