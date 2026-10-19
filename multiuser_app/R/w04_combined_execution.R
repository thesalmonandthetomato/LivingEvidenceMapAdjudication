# Pure planning only. Does not dispatch, write decisions or call an LLM.
# A run is identified by the review checkpoint, exact ordered membership and prompt.
w04_combined_execution_plan <- function(selection, prompt, batch_size=250L) {
  ids <- as.character(selection$record_ids %||% character())
  if(!length(ids) || anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids))
    stop("A nonempty, duplicate-free verified selection is required",call.=FALSE)
  if(!identical(as.integer(selection$selected_count),as.integer(length(ids))) ||
     !identical(as.character(selection$membership_sha256),
       digest::digest(paste(sort(ids),collapse="\\n"),algo="sha256",serialize=FALSE)))
    stop("Selected record membership does not match its checksum",call.=FALSE)
  checkpoint <- as.character(selection$source_checkpoint_sha256 %||% "")
  review_id <- as.character(selection$review_id %||% "")
  prompt_sha <- as.character(prompt$sha256 %||% "")
  prompt_id <- as.character(prompt$version_id %||% "")
  if(length(checkpoint)!=1L || !grepl("^[0-9a-f]{64}$",checkpoint) ||
     length(prompt_sha)!=1L || !grepl("^[0-9a-f]{64}$",prompt_sha) ||
     length(review_id)!=1L || !nzchar(review_id) ||
     length(prompt_id)!=1L || !nzchar(prompt_id) ||
     !identical(as.character(prompt$review_id %||% ""),review_id))
    stop("Selection and immutable prompt require matching review provenance",call.=FALSE)
  size <- as.numeric(batch_size)
  if(length(size)!=1L || !is.finite(size) || size<1 || size>1000 || size!=floor(size))
    stop("Batch size must be a whole number between 1 and 1000",call.=FALSE)
  size <- as.integer(size)
  # Always use deterministic canonical ordering, independent of Shiny UI order.
  ids <- sort(ids,method="radix")
  run_key <- digest::digest(paste(review_id,checkpoint,prompt_id,prompt_sha,
    selection$membership_sha256,size,sep="|"),algo="sha256",serialize=FALSE)
  groups <- split(ids,ceiling(seq_along(ids)/size))
  batches <- lapply(seq_along(groups),function(i) {
    part <- unname(groups[[i]])
    list(batch_index=i,record_count=length(part),record_ids=part,
      membership_sha256=digest::digest(paste(part,collapse="\\n"),
        algo="sha256",serialize=FALSE),
      batch_key=paste0(substr(run_key,1L,20L),"-",sprintf("%05d",i)))
  })
  list(schema="lem-w04-combined-plan-v1",review_id=review_id,
    source_checkpoint_sha256=checkpoint,prompt_version_id=prompt_id,
    prompt_sha256=prompt_sha,selection_sha256=selection$membership_sha256,
    run_key=run_key,batch_size=size,record_count=length(ids),
    batch_count=length(batches),batches=batches,dispatch_enabled=FALSE)
}

# Only verified terminal-success batches can be skipped during a resume.
# A failed/unknown/active batch always demands reconciliation before retry.
w04_combined_resume_plan <- function(plan, saved=list()) {
  if(!identical(plan$schema,"lem-w04-combined-plan-v1"))
    stop("Unsupported immutable execution plan",call.=FALSE)
  if(!is.list(saved))stop("Invalid saved execution states",call.=FALSE)
  ids <- vapply(plan$batches,function(b)b$batch_key,character(1))
  statuses <- stats::setNames(rep("new",length(ids)),ids)
  for(entry in saved) {
    key <- as.character(entry$batch_key %||% "")
    state <- as.character(entry$status %||% "")
    sha <- as.character(entry$membership_sha256 %||% "")
    if(length(key)!=1L || !key %in% ids || length(state)!=1L ||
       !state %in% c("succeeded","failed","queued","running") ||
       length(sha)!=1L)
      stop("Unrecognised saved execution state",call.=FALSE)
    expected <- plan$batches[[match(key,ids)]]$membership_sha256
    if(!identical(sha,expected) || statuses[[key]]!="new")
      stop("Conflicting or mismatched saved batch state",call.=FALSE)
    statuses[[key]] <- state
  }
  list(completed=names(statuses)[statuses=="succeeded"],
       ready=names(statuses)[statuses=="new"],
       needs_review=names(statuses)[statuses %in% c("failed","queued","running")],
       all_complete=all(statuses=="succeeded"))
}

# Archive an immutable proposed run manifest in the review's restricted Zenodo
# draft. This does not dispatch work or change any decisions.
w04_combined_manifest_archive <- function(meta,plan) {
  if(!identical(as.character(meta$review_id),as.character(plan$review_id)) ||
     !identical(as.character(plan$schema),"lem-w04-combined-plan-v1") ||
     !identical(plan$dispatch_enabled,FALSE))
    stop("Manifest has an invalid review, schema or dispatch flag",call.=FALSE)
  draft <- w04_prompt_fetch_draft(meta)
  if(is.null(draft))stop("Restricted W04 draft not available",call.=FALSE)
  bucket <- as.character(draft$links$bucket %||% "")
  if(!startsWith(bucket,"https://zenodo.org/api/files/"))
    stop("Unexpected Zenodo archive bucket",call.=FALSE)
  path <- tempfile(fileext=".json")
  on.exit(unlink(path),add=TRUE)
  writeLines(jsonlite::toJSON(plan,auto_unbox=TRUE,pretty=TRUE,
    null="null",digits=NA),path,useBytes=TRUE)
  sha <- digest::digest(file=path,algo="sha256",serialize=FALSE)
  filename <- paste0("w04-combined-plan-",sha,".json")
  existing <- Filter(function(x)identical(as.character(x$filename %||% ""),filename),
    draft$files %||% list())
  if(length(existing)>1L)stop("Duplicate manifest filenames in archive",call.=FALSE)
  if(!length(existing))
    w04_prompt_zenodo_request(paste0(bucket,"/",filename),
      method="PUT",file=path,expected=c(200L,201L))
  fresh <- w04_prompt_fetch_draft(meta)
  match <- Filter(function(x)identical(as.character(x$filename %||% ""),filename),
    fresh$files %||% list())
  if(length(match)!=1L)stop("Archived manifest missing after upload",call.=FALSE)
  url <- as.character(match[[1L]]$links$download %||% "")
  if(!startsWith(url,"https://zenodo.org/"))stop("Invalid archived manifest URL",call.=FALSE)
  resp <- httr2::req_perform(httr2::req_headers(httr2::request(url),
    Authorization=paste("Bearer",Sys.getenv("ZENODO_ACCESS_TOKEN",unset=""))))
  if(httr2::resp_status(resp)!=200L ||
     !identical(digest::digest(httr2::resp_body_raw(resp),
       algo="sha256",serialize=FALSE),sha))
    stop("Archived manifest checksum verification failed",call.=FALSE)
  list(deposit_id=as.character(fresh$id),filename=filename,sha256=sha,
       run_key=plan$run_key,batch_count=plan$batch_count)
}
