# W04 prompt improvement proposal. Each request and response is review-bound,
# immutable and stored in the existing restricted W04 Zenodo development draft.
w04_refinement_build_request <- function(meta,batch,original_prompt,cases,decisions,note="") {
  if(!identical(as.character(batch$review_id %||% meta$review_id),meta$review_id))
    stop("Model batch review does not match active review",call.=FALSE)
  if(!nzchar(original_prompt) || !identical(w04_prompt_checksum(original_prompt),
     as.character(batch$prompt_sha256)))stop("Archived source prompt checksum mismatch",call.=FALSE)
  if(length(cases)!=as.integer(batch$uncertain) || !length(cases))
    stop("Uncertainty queue does not match selected model run",call.=FALSE)
  if(nchar(note,type="chars")>5000L)stop("Refinement note exceeds 5000 characters",call.=FALSE)
  rows <- lapply(cases,function(z) {
    id <- as.character(z$review_case_id %||% "")
    rid <- as.character(z$record_id %||% "")
    relevant <- Filter(function(x) identical(as.character(x$review_case_id %||% ""),id),decisions)
    if(length(relevant)!=1L ||
       !as.character(relevant[[1L]]$decision %||% "") %in% c("retain","exclude"))
      stop("All uncertainties must have exactly one final human inclusion/exclusion decision",call.=FALSE)
    bibliographic <- z$bibliographic %||% list()
    list(record_id=rid,review_case_id=id,
      title=as.character(bibliographic$title %||% ""),
      abstract=as.character(bibliographic$abstract %||% ""),
      model_votes=z$screening$votes %||% list(),
      human_final_decision=as.character(relevant[[1L]]$decision),
      human_rationale=as.character(relevant[[1L]]$rationale %||% ""))
  })
  ids <- vapply(rows,function(x)x$record_id,character(1))
  if(anyNA(ids)||any(!nzchar(ids))||anyDuplicated(ids))
    stop("Refinement case identifiers must be unique",call.=FALSE)
  list(schema="lem-w04-prompt-refinement-request-v1",
    review_id=meta$review_id,source_run_id=as.character(batch$run_id),
    subset_id=as.character(batch$subset_id),
    prompt_version_id=as.character(batch$prompt_version_id),
    prompt_sha256=as.character(batch$prompt_sha256),
    original_prompt=original_prompt,cases=rows,user_note=note,
    requested_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"))
}
w04_refinement_archive_request <- function(meta,request) {
  draft <- w04_prompt_fetch_draft(meta)
  if(is.null(draft))stop("No review-specific W04 development draft",call.=FALSE)
  if(!identical(request$review_id,meta$review_id))stop("Review identity mismatch",call.=FALSE)
  tmp <- tempfile(fileext=".json");on.exit(unlink(tmp),add=TRUE)
  writeLines(jsonlite::toJSON(request,auto_unbox=TRUE,pretty=TRUE,null="null",digits=NA),
    tmp,useBytes=TRUE)
  sha <- digest::digest(file=tmp,algo="sha256",serialize=FALSE)
  name <- paste0("w04-refinement-request-",substr(sha,1L,20L),".json")
  bucket <- as.character(draft$links$bucket %||% "")
  if(!startsWith(bucket,"https://zenodo.org/api/files/"))
    stop("Unexpected restricted Zenodo upload location",call.=FALSE)
  matches <- Filter(function(x)identical(as.character(x$filename),name),draft$files %||% list())
  if(!length(matches)) w04_prompt_zenodo_request(paste0(bucket,"/",name),
    method="PUT",file=tmp,expected=c(200L,201L))
  else if(length(matches)!=1L)stop("Duplicate request file name",call.=FALSE)
  list(draft_id=as.character(draft$id),filename=name,sha256=sha,
    response_file=sub("request","response",name),review_id=meta$review_id,
    run_id=request$source_run_id,prompt_sha256=request$prompt_sha256)
}
w04_refinement_dispatch <- function(info) {
  assert_development_github_target()
  if(!grepl("^w04-refinement-request-[0-9a-f]{20}\\.json$",info$filename))
    stop("Invalid refinement filename",call.=FALSE)
  url <- sprintf("https://api.github.com/repos/%s/actions/workflows/w04_prompt_refinement.yml/dispatches",
    github_dispatch_repo())
  req <- httr2::request(url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(Authorization=paste("Bearer",github_dispatch_token()),
      Accept="application/vnd.github+json",
      `X-GitHub-Api-Version`="2022-11-28",
      `User-Agent`="LivingEvidenceMap-Adjudication") |>
    httr2::req_body_json(list(ref=github_scoping_ref(),inputs=list(
      review_id=info$review_id,draft_id=info$draft_id,
      request_file=info$filename,request_sha=info$sha256)))
  resp <- httr2::req_perform(req)
  if(httr2::resp_status(resp)!=204L)
    stop("W04 refinement GitHub dispatch not accepted",call.=FALSE)
  invisible(TRUE)
}
w04_refinement_load_response <- function(meta,info) {
  if(!identical(meta$review_id,info$review_id)||
     !identical(w04_prompt_draft_id(meta),info$draft_id))
    stop("Refinement response belongs to another review",call.=FALSE)
  draft <- w04_prompt_fetch_draft(meta)
  matches <- Filter(function(x)identical(as.character(x$filename),info$response_file),
    draft$files %||% list())
  if(!length(matches))return(NULL)
  if(length(matches)!=1L)stop("Duplicate refinement response",call.=FALSE)
  url <- as.character(matches[[1L]]$links$download %||% "")
  resp <- httr2::req_perform(httr2::req_headers(httr2::request(url),
    Authorization=paste("Bearer",Sys.getenv("ZENODO_ACCESS_TOKEN",unset=""))))
  if(httr2::resp_status(resp)!=200L)stop("Cannot read restricted refinement response",call.=FALSE)
  ans <- jsonlite::fromJSON(httr2::resp_body_string(resp),simplifyVector=FALSE)
  if(!identical(ans$schema,"lem-w04-prompt-refinement-response-v1") ||
     !identical(ans$review_id,info$review_id) ||
     !identical(ans$source_run_id,info$run_id) ||
     !identical(ans$prompt_sha256,info$prompt_sha256) ||
     !identical(ans$request_sha256,info$sha256) ||
     !identical(w04_prompt_checksum(ans$original_prompt),info$prompt_sha256))
    stop("Refinement response identity/checksum mismatch",call.=FALSE)
  ans
}
# Highlight additions/altered lines in red, while keeping the unchanged text
# visible. Plain text remains in the editable field rather than HTML.
w04_refinement_diff_ui <- function(original,proposal) {
  old <- strsplit(original,"\\n",fixed=FALSE)[[1L]]
  new <- strsplit(proposal,"\\n",fixed=FALSE)[[1L]]
  old_lines <- unique(old)
  shiny::tags$div(style="white-space:pre-wrap;overflow-wrap:anywhere;font-family:monospace;",
    lapply(seq_along(new),function(i) {
      changed <- !(new[[i]] %in% old_lines)
      shiny::tags$div(style=if(changed)"color:#b42318;font-weight:600;" else "",
        new[[i]])
    }))
}
