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

# Overlay adjudicated model decisions on a read-only subset snapshot for the
# existing W04 kappa functions. Unresolved uncertainty is never treated as final.
w04_model_consistency_outcomes <- function(outcomes, results, resolution_events=list()) {
  if(!length(outcomes)) stop("No human subset outcomes loaded",call.=FALSE)
  ids <- vapply(outcomes,function(x)as.character(x$record_id %||% ""),character(1))
  mids <- as.character(results$record_id)
  if(anyDuplicated(ids)||any(!nzchar(ids))||!setequal(ids,mids)||
     length(ids)!=nrow(results)) stop("Model run does not match selected human subset",call.=FALSE)
  decisions <- stats::setNames(as.character(results$decision),mids)
  resolved <- list()
  for(event in resolution_events) {
    id <- as.character(event$record_id %||% "")
    decision <- as.character(event$decision %||% "")
    if(!id %in% ids || !identical(decisions[[id]],"uncertain") ||
       !decision %in% c("retain","exclude"))
      stop("Invalid model uncertainty resolution identity or decision",call.=FALSE)
    if(!is.null(resolved[[id]])) stop("Multiple active model resolutions for one record",call.=FALSE)
    resolved[[id]] <- decision
  }
  lapply(outcomes,function(x) {
    id <- as.character(x$record_id)
    value <- decisions[[id]]
    if(identical(value,"uncertain") && !is.null(resolved[[id]])) value <- resolved[[id]]
    # Unresolved model votes are pending, not a completed third category for κ.
    x$case$model_consistency_overlay <- TRUE
    x$case$model_decision <- if(identical(value,"uncertain")) "" else value
    x
  })
}

# Resolve already-screened canonical IDs from verified existing model returns.
# Uncertain outcomes are intentionally excluded: they require adjudication.
# Overlapping substantive model decisions must agree before marking as coded.
w04_model_coded_ids <- function(batches,eligible_ids,read_results=w04_model_batch_results_read) {
  eligible <- w04_subset_record_ids(eligible_ids)
  coded <- list()
  for(batch in batches) {
    results <- read_results(batch)
    ids <- as.character(results$record_id)
    votes <- as.character(results$decision)
    if(anyDuplicated(ids)||anyNA(ids)||any(!ids %in% eligible) ||
       any(!votes %in% c("retain","exclude","uncertain")))
      stop("Existing model run contains invalid canonical IDs or decisions",call.=FALSE)
    for(i in seq_along(ids)) {
      if(identical(votes[[i]],"uncertain")) next
      # This helper answers only whether an ID has been screened already.
      # Conflicting votes remain in the saved runs for W04 reconciliation;
      # they do not justify paid repeat screening of the same canonical ID.
      coded[[ids[[i]]]] <- TRUE
    }
  }
  sort(names(coded))
}


# Only completed, unambiguous human outcomes contribute to authoritative
# coverage. A human conflict or an uncertain response remains unresolved.
w04_human_coded_ids <- function(outcomes,eligible_ids) {
  eligible <- w04_subset_record_ids(eligible_ids)
  values <- list()
  for(x in outcomes) {
    id <- as.character(x$record_id %||% "")
    status <- as.character(x$status %||% "")
    decision <- as.character(x$final_decision %||% "")
    if(!nzchar(id) || !id %in% eligible)
      stop("Human outcome references an invalid canonical ID",call.=FALSE)
    if(!status %in% c("agreement","single_human")) next
    if(!decision %in% c("retain","exclude"))
      stop("Completed human outcome lacks a substantive decision",call.=FALSE)
    old <- values[[id]]
    if(!is.null(old) && !identical(old,decision))
      stop("Conflicting human outcomes for canonical ID ",id,call.=FALSE)
    values[[id]] <- decision
  }
  sort(names(values))
}


# Read-only reconciliation by canonical ID. Keep every contributing model run;
# human decisions have precedence, but model disagreements remain visible.
# resolution_events: explicitly verified (record_id, decision, source_run_id)
# entries. Caller must validate queue SHA and review identity before supplying.
w04_screening_coverage_reconcile <- function(eligible_ids,batches=list(),
    human_outcomes=list(),resolution_events=list(),
    read_results=w04_model_batch_results_read) {
  eligible <- w04_subset_record_ids(eligible_ids)
  human <- list()
  unresolved_human <- character()
  for(x in human_outcomes) {
    id <- as.character(x$record_id %||% "")
    if(length(id)!=1L || !id %in% eligible)
      stop("Human screening references an unknown canonical record",call.=FALSE)
    if(as.character(x$status %||% "") %in% c("conflict","pending"))
      unresolved_human <- union(unresolved_human,id)
    if(!as.character(x$status %||% "") %in% c("agreement","single_human")) next
    d <- as.character(x$final_decision %||% "")
    if(!d %in% c("retain","exclude"))
      stop("Completed human record lacks a final decision",call.=FALSE)
    human[[id]] <- unique(c(human[[id]],d))
  }
  models <- list()
  uncertain <- list()
  for(batch in batches) {
    run <- as.character(batch$run_id %||% "")
    if(!grepl("^[0-9]+$",run))stop("Invalid model run identity",call.=FALSE)
    rows <- read_results(batch)
    ids <- as.character(rows$record_id)
    decisions <- as.character(rows$decision)
    if(anyDuplicated(ids) || anyNA(ids) || any(!ids %in% eligible) ||
       anyNA(decisions) || any(!decisions %in% c("retain","exclude","uncertain")))
      stop("Invalid model result coverage or classification",call.=FALSE)
    for(i in seq_along(ids)) {
      id <- ids[[i]]
      if(identical(decisions[[i]],"uncertain")) {
        uncertain[[id]] <- unique(c(uncertain[[id]],run))
      } else {
        models[[id]] <- c(models[[id]],setNames(decisions[[i]],run))
      }
    }
  }
  resolved <- list()
  for(x in resolution_events) {
    id <- as.character(x$record_id %||% "")
    run <- as.character(x$source_run_id %||% "")
    d <- as.character(x$decision %||% "")
    if(!id %in% eligible || !grepl("^[0-9]+$",run) ||
       !run %in% (uncertain[[id]] %||% character()) ||
       !d %in% c("retain","exclude"))
      stop("Model uncertainty resolution has invalid provenance",call.=FALSE)
    resolved[[id]] <- c(resolved[[id]],setNames(d,paste0("resolution:",run)))
  }
  rows <- lapply(eligible,function(id) {
    h <- unique(human[[id]] %||% character())
    m <- models[[id]] %||% character()
    r <- resolved[[id]] %||% character()
    model_votes <- c(m,r)
    unresolved_runs <- setdiff(uncertain[[id]] %||% character(),
      sub("^resolution:","",names(r) %||% character()))
    disagree <- length(unique(unname(model_votes)))>1L
    human_model_disagree <- length(h)==1L && length(model_votes)>0L &&
      any(unname(model_votes)!=h[[1L]])
    human_conflict <- length(h)>1L || id %in% unresolved_human
    decision <- if(length(h)==1L)h[[1L]] else
      if(!human_conflict && length(model_votes) &&
         !disagree)unname(model_votes[[1L]]) else ""
    status <- if(human_conflict)"human_conflict" else
      if(length(h)==1L)"resolved" else
      if(disagree)"model_conflict" else
      if(length(unresolved_runs))"uncertain" else
      if(nzchar(decision))"resolved" else "missing"
    list(record_id=id,decision=decision,status=status,
         human_decisions=h,model_votes=m,adjudicated_model_votes=r,
         human_model_disagreement=isTRUE(human_model_disagree || disagree) && length(h)==1L,
         uncertain_run_ids=uncertain[[id]] %||% character(),
         unresolved_run_ids=unresolved_runs)
  })
  names(rows) <- eligible
  blocking <- vapply(rows,function(x)
    !nzchar(x$decision) || x$status %in% c("human_conflict","model_conflict","human_authoritative_model_conflict","uncertain"),logical(1))
  status <- vapply(rows,function(x)x$status,character(1))
  list(ready=!any(blocking),eligible=length(eligible),
       resolved=sum(!blocking),missing=eligible[status=="missing"],
       uncertain=eligible[status=="uncertain"],
       conflicts=eligible[status %in% c("human_conflict","model_conflict",
                                        "human_authoritative_model_conflict")],
       records=rows)
}

# Extract only queue-verified substantive resolutions from existing W04 Sheets.
# No mutation; unresolved cases remain explicit in the reconciliation.
w04_model_verified_resolution_events <- function(batches,
    read_queue=read_sheet_w04_resolution_queue,
    read_decisions=active_sheet_w04_resolution_decisions) {
  relevant <- Filter(function(b)as.integer(b$uncertain %||% 0L)>0L,batches)
  if(!length(relevant))return(list())
  events <- read_decisions()
  result <- list()
  for(batch in relevant) {
    run <- as.character(batch$run_id)
    queue <- read_queue(as.character(batch$resolution_tab))
    if(is.null(queue) ||
       !identical(as.character(queue$source_run_id),run) ||
       !identical(as.character(queue$queue_sha256),
                  as.character(batch$resolution_queue_sha256)))
      stop("Model uncertainty queue provenance mismatch for run ",run,call.=FALSE)
    cases <- queue$cases %||% list()
    ids <- vapply(cases,function(z)as.character(z$record_id %||% ""),character(1))
    caseids <- vapply(cases,function(z)as.character(z$review_case_id %||% ""),character(1))
    if(any(!nzchar(ids)) || anyDuplicated(ids) ||
       any(!nzchar(caseids)) || anyDuplicated(caseids) ||
       length(ids)!=as.integer(batch$uncertain))
      stop("Uncertainty queue membership mismatch",call.=FALSE)
    ds <- w04_filter_batch_decisions(events,queue$queue_sha256)
    for(d in ds) {
      caseid <- as.character(d$review_case_id %||% "")
      pos <- match(caseid,caseids)
      if(is.na(pos) || !identical(as.character(d$record_id %||% ""),ids[[pos]]))
        stop("Saved uncertainty decision has unmatched case identity",call.=FALSE)
      choice <- as.character(d$decision %||% "")
      if(!choice %in% c("retain","exclude"))next
      result[[length(result)+1L]] <- list(
        record_id=ids[[pos]],source_run_id=run,decision=choice)
    }
  }
  result
}

# Materialise the offline adapter's immutable decision input from the verified
# reconciliation and original Google Sheets result_json. Does not write remotely.
w04_reconciled_export_rows <- function(reconciliation,batches,
   human_outcomes=list(),read_results=w04_model_batch_results_read) {
  if(!isTRUE(reconciliation$ready) ||
     length(reconciliation$records)!=as.integer(reconciliation$eligible))
    stop("W04 reconciled export requires complete, conflict-free coverage",call.=FALSE)
  raw <- list()
  for(batch in batches) {
    run <- as.character(batch$run_id %||% "")
    results <- read_results(batch)
    if(!all(c("record_id","decision","result_json") %in% names(results)))
      stop("Model batch lacks original full screening JSON",call.=FALSE)
    for(i in seq_len(nrow(results))) {
      id <- as.character(results$record_id[[i]])
      payload <- as.character(results$result_json[[i]])
      z <- jsonlite::fromJSON(payload,simplifyVector=FALSE)
      if(!identical(as.character(z$record_id %||% ""),id) ||
         !identical(as.character(z$screening$decision %||% ""),
                    as.character(results$decision[[i]])))
        stop("Original screening JSON fails row-identity verification",call.=FALSE)
      raw[[id]] <- c(raw[[id]],list(list(run=run,decision=as.character(results$decision[[i]]),
                                         json=payload)))
    }
  }
  human <- list()
  for(x in human_outcomes) {
    if(!as.character(x$status %||% "") %in% c("agreement","single_human")) next
    id <- as.character(x$record_id %||% "")
    d <- as.character(x$final_decision %||% "")
    if(!d %in% c("retain","exclude"))stop("Invalid completed human decision",call.=FALSE)
    human[[id]] <- unique(c(human[[id]],d))
  }
  lapply(reconciliation$records,function(rec) {
    id <- as.character(rec$record_id)
    d <- as.character(rec$decision)
    if(!identical(as.character(rec$status),"resolved") ||
       !d %in% c("retain","exclude"))
      stop("Unresolved canonical decision cannot be exported",call.=FALSE)
    candidates <- raw[[id]] %||% list()
    # Prefer an original matching model decision. Otherwise preserve a
    # verified uncertain model row and its human-resolved disposition.
    exact <- Filter(function(z)identical(z$decision,d),candidates)
    uncertain <- Filter(function(z)identical(z$decision,"uncertain"),candidates)
    chosen <- if(length(exact))exact[[1L]] else
              if(length(uncertain))uncertain[[1L]] else
              if(length(human[[id]] %||% character())==1L && length(candidates))candidates[[1L]] else NULL
    hs <- human[[id]] %||% character()
    adjudicated <- rec$adjudicated_model_votes %||% character()
    explicit_human <- if(length(hs)==1L)hs[[1L]] else
      if(length(adjudicated) && all(unname(adjudicated)==d))d else ""
    if(is.null(chosen) && !nzchar(explicit_human))
      stop("Missing original model JSON or verified human authority: ",id,call.=FALSE)
    if(!is.null(chosen) && !identical(chosen$decision,d) &&
       !nzchar(explicit_human))
      stop("Model decision override is not human-authorised: ",id,call.=FALSE)
    contributing <- unique(c(vapply(candidates,function(z)z$run,character(1)),
      as.character(rec$uncertain_run_ids %||% character())))
    list(record_id=id,status="resolved",decision=d,
         model_result_json=if(is.null(chosen))"" else chosen$json,
         human_decision=explicit_human,
         source_run_ids=as.list(contributing))
  })
}

# Reconstructed selections encode (review, canonical SHA, exact member-ID SHA)
# in their synthetic identifier; registered subsets carry an archived source
# checksum and immutable membership. Reject unverifiable legacy batch inputs.
w04_model_verify_batch_checkpoints <- function(batches,review_id,canonical_sha,
    registered_subsets=list(),read_results=w04_model_batch_results_read) {
  sha <- tolower(as.character(canonical_sha))
  if(length(sha)!=1L || !grepl("^[0-9a-f]{64}$",sha) ||
     !nzchar(as.character(review_id)))
    stop("Valid review and canonical checkpoint are required",call.=FALSE)
  for(b in batches) {
    ids <- w04_subset_record_ids(as.character(read_results(b)$record_id))
    member <- digest::digest(paste(sort(ids),collapse="\n"),
                             algo="sha256",serialize=FALSE)
    subset_id <- as.character(b$subset_id %||% "")
    candidates <- Filter(function(s)identical(as.character(s$subset_id),subset_id),
                          registered_subsets)
    if(length(candidates)==1L) {
      s <- candidates[[1L]]
      if(!identical(as.character(s$review_id),as.character(review_id)) ||
         !identical(tolower(as.character(s$source_checkpoint_sha256)),sha) ||
         !setequal(as.character(s$record_ids),ids) ||
         !identical(as.character(s$membership_sha256),member))
        stop("Registered W04 batch/checkpoint provenance mismatch: ",b$run_id,call.=FALSE)
    } else if(length(candidates)==0L) {
      key <- digest::digest(paste(review_id,sha,member,sep=":"),
                            algo="sha256",serialize=FALSE)
      if(!identical(subset_id,paste0("w04-subset-",substr(key,1L,20L))))
        stop("Reconstructed W04 batch/checkpoint provenance mismatch: ",b$run_id,call.=FALSE)
    } else stop("Duplicate registered W04 subset identity",call.=FALSE)
  }
  invisible(TRUE)
}

# Explicit review-specific model-run choices. No implicit latest-run default.
# Every subset with completed model runs needs exactly one selected run.
w04_selected_model_batches <- function(batches,choices) {
  ids <- unique(vapply(batches,function(b)as.character(b$subset_id),character(1)))
  if(any(!nzchar(ids)) || anyDuplicated(vapply(batches,function(b)as.character(b$run_id),character(1))))
    stop("Invalid model batch registry",call.=FALSE)
  if(!length(ids))return(list())
  if(is.null(choices) || !setequal(names(choices),ids))
    stop("Select exactly one model run for every registered model subset",call.=FALSE)
  lapply(ids,function(id) {
    match <- Filter(function(b)identical(as.character(b$subset_id),id) &&
      identical(as.character(b$run_id),as.character(choices[[id]])),batches)
    if(length(match)!=1L)stop("Selected W04 model run is unavailable: ",id,call.=FALSE)
    match[[1L]]
  })
}
