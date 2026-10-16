# Batch-scoped Shiny W04 model return reader; Google Sheets is the source of truth.
w04_model_batch_registry_read <- function(review_id) {
  meta <- w04_prompt_review_metadata()
  if(!identical(as.character(meta$review_id),as.character(review_id)))
    stop("W04 model batch registry belongs to a different review",call.=FALSE)
  gs4_auth_from_env()
  ss <- meta$sheet_id
  if(!"w04_model_batch_registry" %in% sheet_names_cached(ss,refresh=TRUE)) return(list())
  rows <- as.data.frame(googlesheets4::read_sheet(ss,sheet="w04_model_batch_registry",col_types="c"))
  required <- c("review_id","run_id","subset_id","prompt_version_id","prompt_sha256",
    "model_sha256","result_tab","resolution_tab","resolution_queue_sha256",
    "total","retain","exclude","uncertain","published_at_utc")
  if(!all(required %in% names(rows)))stop("W04 model batch registry schema mismatch",call.=FALSE)
  rows <- rows[!is.na(rows$review_id)&rows$review_id==review_id,,drop=FALSE]
  if(!nrow(rows))return(list())
  ids <- as.character(rows$run_id)
  if(anyDuplicated(ids)||any(!grepl("^[0-9]+$",ids)))
    stop("W04 model batch registry contains invalid or duplicate run IDs",call.=FALSE)
  lapply(seq_len(nrow(rows)),function(i)as.list(rows[i,,drop=FALSE]))
}
w04_model_batch_results_read <- function(batch) {
  meta <- w04_prompt_review_metadata()
  ss <- meta$sheet_id
  run_id <- as.character(batch$run_id)
  tab <- paste0("w04_model_results_",run_id)
  if(!identical(as.character(batch$result_tab),tab))stop("W04 results tab identity mismatch",call.=FALSE)
  x <- as.data.frame(googlesheets4::read_sheet(ss,sheet=tab,col_types="c"))
  cols <- c("run_id","subset_id","record_id","decision","prompt_sha256","result_json")
  if(!all(cols %in% names(x)))stop("W04 model result columns missing",call.=FALSE)
  if(nrow(x)!=as.integer(batch$total) || any(as.character(x$run_id)!=run_id) ||
     any(as.character(x$subset_id)!=as.character(batch$subset_id)) ||
     any(as.character(x$prompt_sha256)!=as.character(batch$prompt_sha256)) ||
     anyDuplicated(as.character(x$record_id)))
    stop("W04 model results have inconsistent provenance or coverage",call.=FALSE)
  if(anyNA(x$record_id) || any(!nzchar(as.character(x$record_id))) ||
     anyNA(x$decision) ||
     any(!as.character(x$decision) %in% c("retain","exclude","uncertain")))
    stop("W04 model results contain missing IDs or invalid decisions",call.=FALSE)
  observed <- table(factor(as.character(x$decision),
    levels=c("retain","exclude","uncertain")))
  expected <- suppressWarnings(as.integer(unlist(batch[c("retain","exclude","uncertain")],use.names=FALSE)))
  if(length(expected)!=3L || anyNA(expected) ||
     !identical(as.integer(observed),expected) || sum(expected)!=nrow(x))
    stop("W04 model decision counts differ from published batch registry",call.=FALSE)
  x
}
w04_model_batch_comparison <- function(results, human_events, subset_id) {
  if(!nrow(results))return(data.frame())
  ids <- as.character(results$record_id)
  human <- Filter(function(z) {
    id <- as.character(z$record_id %||% "")
    case <- as.character(z$review_case_id %||% "")
    id %in% ids && identical(case,paste0(subset_id,"|",id))
  },human_events)
  reviewers <- unique(vapply(human,function(z)as.character(z$reviewer %||% ""),character(1)))
  out <- data.frame(record_id=ids,model=as.character(results$decision),stringsAsFactors=FALSE)
  for(uid in reviewers) {
    ds <- Filter(function(x)identical(as.character(x$reviewer %||% ""),uid),human)
    human_decision <- setNames(vapply(ds,function(z)as.character(z$decision %||% ""),character(1)),
      vapply(ds,function(z)as.character(z$record_id %||% ""),character(1)))
    out[[uid]] <- unname(human_decision[ids])
  }
  out
}
