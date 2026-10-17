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
w04_refinement_find_existing <- function(meta,batch) {
  draft <- w04_prompt_fetch_draft(meta)
  if(is.null(draft))return(NULL)
  files <- Filter(function(f)grepl("^w04-refinement-request-[0-9a-f]{20}\\.json$",
    as.character(f$filename %||% "")),draft$files %||% list())
  if(!length(files))return(NULL)
  matches <- list()
  for(f in files) {
    url <- as.character(f$links$download %||% "")
    if(!nzchar(url))next
    resp <- httr2::req_perform(httr2::req_headers(httr2::request(url),
      Authorization=paste("Bearer",Sys.getenv("ZENODO_ACCESS_TOKEN",unset=""))))
    if(httr2::resp_status(resp)!=200L)next
    raw <- httr2::resp_body_raw(resp)
    request <- tryCatch(jsonlite::fromJSON(rawToChar(raw),simplifyVector=FALSE),error=function(e)NULL)
    if(is.null(request) || !identical(request$schema,"lem-w04-prompt-refinement-request-v1") ||
       !identical(as.character(request$review_id),as.character(meta$review_id)) ||
       !identical(as.character(request$source_run_id),as.character(batch$run_id)) ||
       !identical(as.character(request$prompt_sha256),as.character(batch$prompt_sha256)))
      next
    sha <- digest::digest(raw,algo="sha256",serialize=FALSE)
    if(!identical(substr(sha,1L,20L),
      sub("^w04-refinement-request-([0-9a-f]{20})\\.json$","\\1",as.character(f$filename))))next
    matches[[length(matches)+1L]] <- list(
      draft_id=as.character(draft$id),filename=as.character(f$filename),
      sha256=sha,response_file=sub("request","response",as.character(f$filename)),
      review_id=meta$review_id,run_id=as.character(batch$run_id),
      prompt_sha256=as.character(batch$prompt_sha256),
      requested_at_utc=as.character(request$requested_at_utc %||% ""))
  }
  if(!length(matches))return(NULL)
  timestamps <- vapply(matches,function(x)x$requested_at_utc,character(1))
  matches[[order(timestamps,decreasing=TRUE)[[1L]]]]
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

w04_refinement_run_status <- function(info) {
  assert_development_github_target()
  endpoint <- sprintf("https://api.github.com/repos/%s/actions/workflows/w04_prompt_refinement.yml/runs?branch=main&event=workflow_dispatch&per_page=100",
    github_dispatch_repo())
  runs <- github_api_get_json(endpoint)$workflow_runs %||% list()
  label <- paste("W04 prompt refinement",info$review_id,info$filename)
  matches <- Filter(function(x)identical(as.character(x$display_title %||% ""),label),runs)
  if(!length(matches))return(list(state="queued",url=""))
  z <- matches[[1L]]
  status <- as.character(z$status %||% "")
  conclusion <- as.character(z$conclusion %||% "")
  state <- if(identical(status,"completed")) {
    if(identical(conclusion,"success")) "completed" else "failed"
  } else if(status %in% c("in_progress","waiting","pending","requested")) "running" else "queued"
  list(state=state,url=as.character(z$html_url %||% ""),conclusion=conclusion)
}


# An archived request is not itself evidence that a paid call happened.
# Permit a fresh, separately archived attempt only if GitHub explicitly
# reports failure AND Zenodo contains neither a started marker nor response.
w04_refinement_can_retry_failed <- function(meta,info) {
  status <- w04_refinement_run_status(info)
  if(!identical(status$state,"failed") || !nzchar(as.character(status$url %||% "")))
    return(FALSE)
  draft <- w04_prompt_fetch_draft(meta)
  if(is.null(draft) || !identical(as.character(draft$id),as.character(info$draft_id)))
    stop("Cannot verify archived refinement state",call.=FALSE)
  names <- vapply(draft$files %||% list(),
    function(x)as.character(x$filename %||% ""),character(1))
  started <- sub("^w04-refinement-request-","w04-refinement-started-",info$filename)
  response <- as.character(info$response_file)
  if(started %in% names || response %in% names) return(FALSE)
  TRUE
}


# Another explicitly requested iteration is legitimate after the previous
# proposal has completed and its response is safely archived. Never unlock
# an active or uncertain request, even if its GitHub status cannot be read.
w04_refinement_can_start_new <- function(meta,prior) {
  if(is.null(prior)) return(TRUE)
  status <- w04_refinement_run_status(prior)
  if(!nzchar(as.character(status$url %||% ""))) return(FALSE)
  draft <- w04_prompt_fetch_draft(meta)
  if(is.null(draft) || !identical(as.character(draft$id),as.character(prior$draft_id)))
    stop("Cannot verify archived refinement state",call.=FALSE)
  names <- vapply(draft$files %||% list(),
    function(x)as.character(x$filename %||% ""),character(1))
  started <- sub("^w04-refinement-request-","w04-refinement-started-",prior$filename)
  completed <- identical(status$state,"completed") &&
    prior$response_file %in% names
  failed_before_call <- identical(status$state,"failed") &&
    !started %in% names && !prior$response_file %in% names
  completed || failed_before_call
}
