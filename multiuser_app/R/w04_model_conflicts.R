# Rebuild saved W04 human-vs-model conflict sets from immutable model runs
# and the original human screening queue. No original decisions are changed.
w04_model_conflict_reconstruct <- function(set, assignments) {
  run <- sub("^w04-model-run-", "", as.character(set$parent_batch_id %||% ""))
  if (!grepl("^[0-9]+$",run) ||
      !identical(as.character(set$comparison_type %||% ""),"human_model"))
    stop("Not a saved human-model comparison",call.=FALSE)
  meta <- w04_prompt_review_metadata()
  batches <- w04_model_batch_registry_read(meta$review_id)
  hits <- Filter(function(x)identical(as.character(x$run_id),run),batches)
  if(length(hits)!=1L ||
     !identical(as.character(hits[[1L]]$model_sha256),
                as.character(set$parent_queue_sha256)))
    stop("Model-run checksum or identity mismatch",call.=FALSE)
  batch <- hits[[1L]]
  subset <- as.character(batch$subset_id)
  queue <- w04_subset_queue_read(subset,meta$review_id)
  if(is.null(queue))stop("Archived human subset queue unavailable",call.=FALSE)
  human_events <- w04_filter_batch_decisions(active_sheet_w04_decisions(),queue$queue_sha256)
  human <- w04_blind_case_outcomes(queue$cases,assignments,human_events,
    paste0("w04-human-",subset),"manual_screening")
  model <- w04_model_batch_results_read(batch)
  resolved <- list()
  if(as.integer(batch$uncertain)>0L) {
    uncertainty <- read_sheet_w04_resolution_queue(as.character(batch$resolution_tab))
    if(is.null(uncertainty) ||
       !identical(as.character(uncertainty$source_run_id),run) ||
       !identical(as.character(uncertainty$queue_sha256),
                  as.character(batch$resolution_queue_sha256)))
      stop("Model uncertainty provenance mismatch",call.=FALSE)
    events <- w04_filter_batch_decisions(active_sheet_w04_resolution_decisions(),
                                          uncertainty$queue_sha256)
    resolved <- Filter(function(e) as.character(e$decision %||% "") %in% c("retain","exclude"),events)
  }
  outcomes <- w04_model_consistency_outcomes(human,model,resolved)
  raters <- as.character(jsonlite::fromJSON(as.character(set$rater_ids_json %||% "[]")))
  ids <- as.character(jsonlite::fromJSON(as.character(set$conflict_case_ids_json %||% "[]")))
  if(length(raters)!=2L || sum(raters=="model")!=1L ||
     length(ids)<1L || anyDuplicated(ids))
    stop("Saved model conflict raters or IDs are invalid",call.=FALSE)
  actual <- w04_consistency_analysis(outcomes,raters)
  if(!setequal(actual$conflict_case_ids,ids))
    stop("Current decisions differ from the saved model conflict set; refusing mismatched adjudication",call.=FALSE)
  cases <- w04_conflict_cases_for_raters(outcomes,raters,ids,
    analysis_id=as.character(set$analysis_id),conflict_set_id=as.character(set$conflict_set_id))
  model_by_id <- stats::setNames(seq_len(nrow(model)),as.character(model$record_id))
  vote_results <- tryCatch(w04_model_vote_reasons_from_artifacts(run,
      as.character(batch$prompt_sha256)),error=function(e)e)
  vote_error <- if(inherits(vote_results,"error"))conditionMessage(vote_results) else ""
  if(inherits(vote_results,"error"))vote_results <- list()
  lapply(cases,function(z) {
    i <- model_by_id[[as.character(z$record_id)]]
    info <- tryCatch(jsonlite::fromJSON(as.character(model$result_json[[i]]),
      simplifyVector=FALSE),error=function(e)list())
    screening <- info$screening %||% list()
    z$model_reason <- as.character(screening$reason %||% info$reason %||% "")
    z$model_votes <- screening$votes %||% list()
    z$model_vote_details <- vote_results[[as.character(z$record_id)]] %||% list()
    z$model_vote_error <- vote_error
    z
  })
}

# Archived screening shards contain the individual pass explanations, even
# though the published consensus sheet contains only decisions and vote labels.
# Download only this run's shard archives. Do not call a model or rerun screening.
w04_model_vote_reasons_from_artifacts <- function(run_id,prompt_sha256) {
  run <- as.character(run_id)
  if(!grepl("^[0-9]+$",run))stop("Invalid GitHub model run ID",call.=FALSE)
  expected <- tolower(as.character(prompt_sha256))
  if(!grepl("^[0-9a-f]{64}$",expected))
    stop("Invalid expected model prompt checksum",call.=FALSE)
  repo <- github_dispatch_repo()
  endpoint <- sprintf("https://api.github.com/repos/%s/actions/runs/%s/artifacts?per_page=100",repo,run)
  response <- github_api_get_json(endpoint)
  all_artifacts <- response$artifacts %||% list()
  pattern <- paste0("^workflow04-incremental-shard-[0-9]+-",run,"$")
  artifacts <- Filter(function(a)grepl(pattern,as.character(a$name %||% "")) &&
    !isTRUE(a$expired),all_artifacts)
  if(!length(artifacts))
    stop("No unexpired model voting shard artifacts are available",call.=FALSE)
  seen <- character()
  out <- list()
  for(a in artifacts) {
    artifact_id <- as.character(a$id %||% "")
    if(!grepl("^[0-9]+$",artifact_id))stop("Invalid screening artifact ID",call.=FALSE)
    url <- sprintf("https://api.github.com/repos/%s/actions/artifacts/%s/zip",repo,artifact_id)
    req <- httr2::request(url) |>
      httr2::req_headers(Authorization=paste("Bearer",github_dispatch_token()),
        Accept="application/vnd.github+json",
        `User-Agent`="LivingEvidenceMap-Adjudication") |>
      httr2::req_timeout(90)
    bytes <- httr2::resp_body_raw(httr2::req_perform(req))
    if(length(bytes)>12000000L ||
       length(bytes)<4L || !identical(as.integer(bytes[1:2]),c(80L,75L)))
      stop("Unexpected screening artifact ZIP content",call.=FALSE)
    tmp <- tempfile(fileext=".zip")
    dir <- tempfile(pattern="w04-votes-")
    dir.create(dir)
    tryCatch({
      writeBin(bytes,tmp)
      members <- utils::unzip(tmp,list=TRUE)$Name
      passes <- intersect(c("pass1.jsonl","pass2.jsonl","pass3_conflicts.jsonl"),members)
      if(!length(passes)) stop("Screening artifact contains no vote records",call.=FALSE)
      utils::unzip(tmp,files=passes,exdir=dir)
      for(file in passes) {
        lines <- readLines(file.path(dir,file),warn=FALSE,encoding="UTF-8")
        for(line in lines[nzchar(trimws(lines))]) {
          x <- jsonlite::fromJSON(line,simplifyVector=FALSE)
          id <- as.character(x$record_id %||% "")
          pass <- suppressWarnings(as.integer(x$pass %||% NA_integer_))
          if(!nzchar(id) || is.na(pass) || !pass %in% 1:3 ||
             !identical(tolower(as.character(x$prompt_sha256 %||% "")),expected))
            stop("Individual model vote does not match archived run provenance",call.=FALSE)
          key <- paste(id,pass,sep="|")
          if(key %in% seen) stop("Duplicate model vote in saved artifacts",call.=FALSE)
          seen <- c(seen,key)
          out[[id]] <- c(out[[id]] %||% list(),
            list(list(pass=pass,decision=as.character(x$decision %||% ""),
                      reason=as.character(x$reason %||% ""))))
        }
      }
    },finally={
      unlink(tmp)
      unlink(dir,recursive=TRUE)
    })
  }
  out
}
