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
  lapply(cases,function(z) {
    i <- model_by_id[[as.character(z$record_id)]]
    info <- tryCatch(jsonlite::fromJSON(as.character(model$result_json[[i]]),
      simplifyVector=FALSE),error=function(e)list())
    screening <- info$screening %||% list()
    z$model_reason <- as.character(screening$reason %||% info$reason %||% "")
    z$model_votes <- screening$votes %||% list()
    z
  })
}
