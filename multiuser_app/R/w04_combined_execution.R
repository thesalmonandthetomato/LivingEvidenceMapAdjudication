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
