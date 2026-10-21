# Review-scoped Shiny W04 subset launch. No alternative model screening code.
w04_shiny_model_bundle_build <- function(meta, subset, queue, prompt, decisions, edits=list()) {
  if(!identical(as.character(subset$subset_id),as.character(queue$subset_id)) ||
     !identical(as.character(meta$review_id),as.character(prompt$review_id)))
    stop("Subset, queue and prompt belong to different reviews",call.=FALSE)
  ids <- as.character(subset$record_ids)
  queue_ids <- vapply(queue$cases,function(x)as.character(x$record_id),character(1))
  if(!length(ids)||anyDuplicated(ids)||!identical(ids,queue_ids))
    stop("Registered subset members do not match the immutable W04 queue",call.=FALSE)
  case_ids <- vapply(queue$cases,function(x)as.character(x$review_case_id),character(1))
  expected_cases <- paste0(as.character(subset$subset_id),"|",ids)
  if(anyDuplicated(case_ids)||!identical(case_ids,expected_cases))
    stop("W04 subset case identifiers do not match registration",call.=FALSE)
  decision_cases <- unique(vapply(Filter(function(x)
    as.character(x$decision %||% "") %in% c("retain","exclude","uncertain"),decisions),
    function(x)as.character(x$review_case_id %||% ""),character(1)))
  if(length(setdiff(case_ids,decision_cases)))
    stop(sprintf("Human screening is incomplete for %d records",
      length(setdiff(case_ids,decision_cases))),call.=FALSE)
  if(!identical(w04_prompt_checksum(prompt$content),as.character(prompt$sha256)))
    stop("Selected immutable prompt checksum mismatch",call.=FALSE)
  if(!grepl("^w04-[0-9a-f]{20}$",as.character(prompt$version_id)))
    stop("Invalid W04 prompt version",call.=FALSE)
  accepted <- list()
  if(length(edits)) {
    edits <- Filter(function(e)
      identical(tolower(as.character(e$queue_sha256 %||% "")),
        tolower(as.character(queue$queue_sha256))),edits)
    edit_cases <- unique(vapply(edits,function(e)
      as.character(e$review_case_id %||% ""),character(1)))
    if(length(setdiff(edit_cases,case_ids)))
      stop("Abstract correction is outside the selected W04 queue",call.=FALSE)
    for(case_id in edit_cases) {
      matching <- Filter(function(e)identical(as.character(e$review_case_id),case_id),edits)
      texts <- unique(vapply(matching,function(e)
        trimws(as.character(e$abstract %||% "")),character(1)))
      if(length(texts)!=1L||!nzchar(texts[[1L]]))
        stop("Conflicting or empty abstracts for case ",case_id,call.=FALSE)
      id <- ids[[match(case_id,case_ids)]]
      if(any(vapply(matching,function(e)
         !identical(as.character(e$record_id %||% ""),id),logical(1))))
        stop("Abstract correction record ID mismatch",call.=FALSE)
      accepted[[length(accepted)+1L]] <- list(
        record_id=id,review_case_id=case_id,abstract=texts[[1L]],
        edit_provenance=lapply(matching,function(e)list(
          reviewer=as.character(e$reviewer %||% ""),
          edit_id=as.character(e$edit_id %||% ""),
          saved_at_utc=as.character(e$saved_at_utc %||% "")
        ))
      )
    }
  }
  list(schema="lem-w04-shiny-subset-input-v1",
    review_id=as.character(meta$review_id),
    review_sheet_id=as.character(meta$sheet_id),
    subset_id=as.character(subset$subset_id),
    queue_sha256=as.character(queue$queue_sha256),
    source_checkpoint_sha256=tolower(as.character(subset$source_checkpoint_sha256)),
    record_ids=as.list(ids),abstract_corrections=accepted,
    prompt_version_id=as.character(prompt$version_id),
    prompt_sha256=as.character(prompt$sha256),
    human_decision_count=length(decision_cases),
    prepared_by="shiny")
}


# Reconstructed model-only selections use the established archive and dispatch.
# The synthetic subset ID is a deterministic run grouping key, not a registered
# human-screening subset. Human decisions are never required or rewritten.
w04_shiny_model_selection_bundle_build <- function(meta,selection,prompt,source) {
  ids <- w04_subset_record_ids(selection$record_ids)
  member_sha <- digest::digest(paste(sort(ids),collapse="\n"),
                               algo="sha256",serialize=FALSE)
  if(!identical(member_sha,as.character(selection$membership_sha256)) ||
     !identical(as.integer(length(ids)),as.integer(selection$record_count)))
    stop("Reconstructed selection membership checksum mismatch",call.=FALSE)
  if(!identical(as.character(meta$review_id),as.character(prompt$review_id)) ||
     !identical(as.character(meta$review_id),as.character(source$review_id)))
    stop("Reconstructed W04 selection review identity mismatch",call.=FALSE)
  source_sha <- tolower(as.character(source$lean$lean_canonical_sha256 %||% ""))
  if(!grepl("^[0-9a-f]{64}$",source_sha))
    stop("Reconstructed W04 selection lacks a verified canonical checkpoint",call.=FALSE)
  if(!identical(w04_prompt_checksum(prompt$content),as.character(prompt$sha256)) ||
     !grepl("^w04-[0-9a-f]{20}$",as.character(prompt$version_id)))
    stop("Reconstructed W04 prompt provenance mismatch",call.=FALSE)
  key <- digest::digest(paste(meta$review_id,source_sha,member_sha,sep=":"),
                        algo="sha256",serialize=FALSE)
  list(schema="lem-w04-shiny-subset-input-v1",
       selection_type="reconstructed",
       membership_sha256=member_sha,
       review_id=as.character(meta$review_id),
       review_sheet_id=as.character(meta$sheet_id),
       subset_id=paste0("w04-subset-",substr(key,1L,20L)),
       queue_sha256=member_sha,
       source_checkpoint_sha256=source_sha,
       record_ids=as.list(ids),abstract_corrections=list(),
       prompt_version_id=as.character(prompt$version_id),
       prompt_sha256=as.character(prompt$sha256),
       human_decision_count=0L,prepared_by="shiny")
}

w04_shiny_model_bundle_archive <- function(meta,bundle) {
  if(!identical(meta$review_id,bundle$review_id))
    stop("Cannot archive subset for another review",call.=FALSE)
  deposit <- w04_prompt_fetch_draft(meta)
  if(is.null(deposit)) stop("No restricted W04 Zenodo prompt draft",call.=FALSE)
  bucket <- as.character(deposit$links$bucket %||% "")
  if(!startsWith(bucket,"https://zenodo.org/api/files/"))
    stop("Unexpected restricted Zenodo upload location",call.=FALSE)
  path <- tempfile(fileext=".json")
  on.exit(unlink(path),add=TRUE)
  writeLines(jsonlite::toJSON(bundle,auto_unbox=TRUE,pretty=TRUE,
    null="null",digits=NA),path,useBytes=TRUE)
  sha <- digest::digest(file=path,algo="sha256",serialize=FALSE)
  filename <- paste0("w04-model-input-",substr(sha,1L,20L),".json")
  existing <- Filter(function(x)identical(as.character(x$filename %||% ""),filename),
    deposit$files %||% list())
  if(!length(existing)) {
    w04_prompt_zenodo_request(paste0(bucket,"/",filename),
      method="PUT",file=path,expected=c(200L,201L))
  } else if(length(existing)!=1L) {
    stop("Duplicate archived Shiny input filename",call.=FALSE)
  }
  fresh <- w04_prompt_fetch_draft(w04_prompt_review_metadata())
  matches <- Filter(function(x)identical(as.character(x$filename %||% ""),filename),
    fresh$files %||% list())
  if(length(matches)!=1L)
    stop("Shiny model input could not be verified in Zenodo",call.=FALSE)
  url <- as.character(matches[[1L]]$links$download %||% "")
  if(!startsWith(url,"https://zenodo.org/"))
    stop("Unexpected Zenodo download URL",call.=FALSE)
  resp <- httr2::req_perform(httr2::req_headers(httr2::request(url),
    Authorization=paste("Bearer",Sys.getenv("ZENODO_ACCESS_TOKEN",unset=""))))
  if(httr2::resp_status(resp)!=200L ||
     !identical(digest::digest(httr2::resp_body_raw(resp),
      algo="sha256",serialize=FALSE),sha))
    stop("Archived W04 model input failed byte-level verification",call.=FALSE)
  list(deposit_id=as.character(fresh$id),filename=filename,sha256=sha)
}

# A GitHub startup_failure has no runnable jobs and can be safely retried.
w04_shiny_model_blocking_runs <- function(runs, label) {
  matching <- Filter(function(x)
    identical(as.character(x$display_title %||% ""), label), runs)
  # Completed attempts, whether successful or failed, must never prevent
  # intentionally rerunning the same immutable bundle. Only an active attempt
  # remains blocked to avoid accidentally dispatching twice concurrently.
  Filter(function(x) {
    status <- tolower(as.character(x$status %||% ""))
    status %in% c("queued", "in_progress", "waiting", "pending", "requested")
  }, matching)
}

w04_shiny_model_dispatch <- function(meta,source,subset,bundle_info) {
  if(!identical(meta$review_id,source$review_id) ||
     !identical(tolower(as.character(subset$source_checkpoint_sha256)),
       tolower(as.character(source$lean$lean_canonical_sha256))))
    stop("W04 model input does not match the review-bound W03 source",call.=FALSE)
  assert_development_github_target()
  label <- paste("W04 Shiny subset",meta$review_id,bundle_info$filename)
  run_url <- sprintf(
    "https://api.github.com/repos/%s/actions/workflows/w04_shiny_subset_launcher.yml/runs?branch=main&per_page=50",
    github_dispatch_repo()
  )
  recent <- github_api_get_json(run_url)$workflow_runs %||% list()
  blocking <- w04_shiny_model_blocking_runs(recent, label)
  if(length(blocking)) {
    runs <- vapply(blocking,function(x) {
      url <- as.character(x$html_url %||% "")
      status <- as.character(x$conclusion %||% x$status %||% "unknown")
      paste0(status,if(nzchar(url)) paste0(" (",url,")") else "")
    },character(1))
    stop(paste0("This exact subset/prompt/abstract bundle already has an active GitHub Actions attempt: ",
      paste(runs,collapse="; "),". Inspect that run before trying again."),
      call.=FALSE)
  }
  endpoint <- sprintf(
    "https://api.github.com/repos/%s/actions/workflows/w04_shiny_subset_launcher.yml/dispatches",
    github_dispatch_repo()
  )
  req <- httr2::request(endpoint) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Authorization=paste("Bearer",github_dispatch_token()),
      Accept="application/vnd.github+json",
      `X-GitHub-Api-Version`="2022-11-28",
      `User-Agent`="LivingEvidenceMap-Adjudication"
    ) |>
    httr2::req_body_json(list(ref=github_scoping_ref(),inputs=list(
      lean_pointer_path=as.character(source$lean_path),
      workflow03_pointer_path=as.character(source$w03_path),
      review_id=as.character(meta$review_id),
      prompt_draft_id=as.character(bundle_info$deposit_id),
      bundle_file=as.character(bundle_info$filename),
      bundle_sha256=as.character(bundle_info$sha256)
    )))
  resp <- httr2::req_perform(req)
  if(httr2::resp_status(resp)!=204L)
    stop(sprintf("GitHub W04 Shiny dispatch returned HTTP %s",
      httr2::resp_status(resp)),call.=FALSE)
  invisible(TRUE)
}

# Use the established restricted Zenodo archive transport for read-only W04
# finalisation input. This does not dispatch publication or mutate source data.
w04_finalise_bundle_build <- function(meta,source,snapshot) {
  if(!identical(as.character(snapshot$review_id),as.character(meta$review_id)) ||
     !identical(as.character(source$review_id),as.character(meta$review_id)) ||
     !identical(tolower(as.character(source$lean$lean_canonical_sha256)),
                tolower(as.character(snapshot$checkpoint_sha256))))
    stop("W04 finalisation archive checkpoint mismatch",call.=FALSE)
  rows <- snapshot$rows %||% list()
  if(!length(rows))stop("Empty finalisation reconciliation",call.=FALSE)
  ids <- vapply(rows,function(x)as.character(x$record_id %||% ""),character(1))
  if(anyNA(ids)||any(!nzchar(ids))||anyDuplicated(ids) ||
     any(vapply(rows,function(x)
       !identical(as.character(x$status %||% ""),"resolved") ||
       !as.character(x$decision %||% "") %in% c("retain","exclude"),logical(1))))
    stop("W04 finalisation archive requires complete substantive decisions",call.=FALSE)
  selected <- snapshot$selected_model_runs %||% list()
  if(!length(selected) || anyDuplicated(vapply(selected,function(x)
      as.character(x$subset_id %||% ""),character(1))) ||
     any(vapply(selected,function(x)
       !grepl("^[0-9]+$",as.character(x$run_id %||% "")) ||
       !grepl("^[0-9a-f]{64}$",tolower(as.character(x$prompt_sha256 %||% ""))),logical(1))))
    stop("Selected model run and prompt provenance is incomplete",call.=FALSE)
  payload <- vapply(rows,function(x)jsonlite::toJSON(x,auto_unbox=TRUE,
     null="null",na="null",digits=NA),character(1))
  sha <- digest::digest(paste(payload,collapse="\n"),algo="sha256",serialize=FALSE)
  list(schema="lem-w04-reconciled-finalise-input-v1",
       review_id=as.character(meta$review_id),
       source_checkpoint_sha256=tolower(as.character(snapshot$checkpoint_sha256)),
       source_lean_pointer=as.character(source$lean_path),
       source_w03_pointer=as.character(source$w03_path),
       selected_model_runs=selected,
       record_count=length(rows),records_sha256=sha,
       records=rows)
}

# Dispatch only the isolated, read-only W04 reconciliation validation job.
# This job has no OpenAI credentials and never publishes W04 or runs W05.
w04_finalise_validation_dispatch <- function(meta,source,archive) {
  if(!identical(as.character(meta$review_id),as.character(source$review_id)) ||
     !grepl("^[0-9a-f]{64}$",as.character(archive$sha256 %||% "")))
    stop("Invalid W04 finalisation validation provenance",call.=FALSE)
  assert_development_github_target()
  endpoint <- sprintf("https://api.github.com/repos/%s/actions/workflows/w04_shiny_reconciled_validation.yml/dispatches",
                      github_dispatch_repo())
  request <- httr2::request(endpoint) |>
    httr2::req_method("POST") |>
    httr2::req_headers(Authorization=paste("Bearer",github_dispatch_token()),
      Accept="application/vnd.github+json",
      `X-GitHub-Api-Version`="2022-11-28",
      `User-Agent`="LivingEvidenceMap-Adjudication") |>
    httr2::req_body_json(list(ref=github_scoping_ref(),inputs=list(
       lean_pointer_path=as.character(source$lean_path),
       workflow03_pointer_path=as.character(source$w03_path),
       review_id=as.character(meta$review_id),
       prompt_draft_id=as.character(archive$deposit_id),
       bundle_file=as.character(archive$filename),
       bundle_sha256=as.character(archive$sha256))))
  resp<-httr2::req_perform(request)
  if(httr2::resp_status(resp)!=204L)
    stop("GitHub offline W04 validation dispatch not confirmed",call.=FALSE)
  invisible(TRUE)
}
