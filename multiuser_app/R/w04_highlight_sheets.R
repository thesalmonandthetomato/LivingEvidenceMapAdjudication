# Editable W04 highlighting configuration in an isolated review-specific Google Sheet.
# Prompt versions and the restricted Zenodo prompt draft are not modified.
w04_highlight_parse <- function(text) {
  if(length(text)!=1L||is.na(text)) stop("Invalid highlight terms",call.=FALSE)
  terms <- trimws(unlist(strsplit(gsub("\r","\n",text,fixed=TRUE),"\n",fixed=TRUE)))
  unique(terms[nzchar(terms)])
}
w04_highlight_config <- function(meta,include,exclude,author="") {
  inc <- w04_highlight_parse(include)
  exc <- w04_highlight_parse(exclude)
  if(length(intersect(tolower(inc),tolower(exc))))
    stop("A term cannot be highlighted in both green and red",call.=FALSE)
  payload <- jsonlite::toJSON(list(include=inc,exclude=exc),auto_unbox=FALSE,null="null")
  list(review_id=meta$review_id,include=inc,exclude=exc,
       sha256=digest::digest(payload,algo="sha256",serialize=FALSE),
       updated_by=as.character(author),
       updated_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%OS6Z"))
}
w04_highlight_tab <- function() "w04_highlight_terms"
w04_highlight_read <- function(meta) {
  gs4_auth_from_env()
  if(!w04_highlight_tab() %in% sheet_names_cached(meta$sheet_id))
    return(w04_highlight_config(meta,"",""))
  x <- as.data.frame(googlesheets4::read_sheet(meta$sheet_id,
    sheet=w04_highlight_tab(),col_types="c"))
  required <- c("review_id","include_json","exclude_json","sha256","updated_by","updated_at_utc")
  if(nrow(x)!=1L||!all(required%in%names(x))||
     !identical(as.character(x$review_id[[1L]]),meta$review_id))
    stop("Highlight terms sheet has an invalid review identity or schema",call.=FALSE)
  inc <- unlist(jsonlite::fromJSON(as.character(x$include_json[[1L]]),simplifyVector=FALSE),
                use.names=FALSE)
  exc <- unlist(jsonlite::fromJSON(as.character(x$exclude_json[[1L]]),simplifyVector=FALSE),
                use.names=FALSE)
  cfg <- w04_highlight_config(meta,paste(inc,collapse="\n"),paste(exc,collapse="\n"),
                               as.character(x$updated_by[[1L]]))
  if(!identical(cfg$sha256,as.character(x$sha256[[1L]])))
    stop("Highlight terms integrity checksum mismatch",call.=FALSE)
  cfg$updated_at_utc <- as.character(x$updated_at_utc[[1L]])
  cfg
}
w04_highlight_save <- function(meta,config,expected_sha256) {
  if(!identical(meta$review_id,config$review_id))
    stop("Cannot save highlighting terms to another review",call.=FALSE)
  current <- w04_highlight_read(meta)
  if(!identical(current$sha256,expected_sha256))
    stop("Highlighting terms changed since loading; reload before saving",call.=FALSE)
  row <- data.frame(review_id=meta$review_id,
    include_json=as.character(jsonlite::toJSON(config$include,auto_unbox=FALSE)),
    exclude_json=as.character(jsonlite::toJSON(config$exclude,auto_unbox=FALSE)),
    sha256=config$sha256,updated_by=config$updated_by,
    updated_at_utc=config$updated_at_utc,stringsAsFactors=FALSE)
  googlesheets4::sheet_write(row,ss=meta$sheet_id,sheet=w04_highlight_tab())
  invalidate_sheet_names_cache(meta$sheet_id)
  observed <- w04_highlight_read(meta)
  if(!identical(observed$sha256,config$sha256))
    stop("Highlighting terms were not confirmed saved",call.=FALSE)
  observed
}
