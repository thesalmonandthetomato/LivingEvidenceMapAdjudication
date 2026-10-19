# Pure, read-only W04 model coverage and record-selection utilities.
# Decisions are not inferred from subset membership: only explicit canonical IDs.
w04_model_coded_ids <- function(events, eligible_ids, label="coding") {
  if(is.null(events)) events <- list()
  if(is.data.frame(events))events <- split(events,seq_len(nrow(events)))
  if(!is.list(events))stop(paste(label,"must be records"),call.=FALSE)
  eligible_ids <- w04_subset_record_ids(eligible_ids)
  if(!length(events))return(character())
  valid <- c("retain","exclude")
  coded <- character()
  for(e in events) {
    decision <- as.character(e$decision %||% "")
    if(length(decision)!=1L || is.na(decision))stop(paste(label,"contains invalid decision"),call.=FALSE)
    if(!decision %in% valid)next
    id <- as.character(e$record_id %||% "")
    if(length(id)!=1L || is.na(id) || !nzchar(id))
      stop(paste(label,"contains a coded event without explicit canonical record_id"),call.=FALSE)
    if(!id %in% eligible_ids)
      stop(paste(label,"contains coded record outside verified eligible corpus"),call.=FALSE)
    coded <- c(coded,id)
  }
  unique(coded)
}
w04_model_selection_preflight <- function(corpus, human_events=list(),
                                          model_events=list(),
                                          mode="uncoded",subset=NULL) {
  ids <- w04_subset_record_ids(corpus$record_ids)
  checkpoint <- tolower(as.character(corpus$checkpoint_sha256 %||% ""))
  review_id <- as.character(corpus$review_id %||% "")
  if(length(checkpoint)!=1L || !grepl("^[0-9a-f]{64}$",checkpoint) ||
     length(review_id)!=1L || !nzchar(review_id))
    stop("A verified review-specific canonical corpus is required",call.=FALSE)
  if(!mode %in% c("uncoded","subset","all"))
    stop("Unknown model selection mode",call.=FALSE)
  human <- w04_model_coded_ids(human_events,ids,"Human coding")
  model <- w04_model_coded_ids(model_events,ids,"Model coding")
  coded <- union(human,model)
  if(identical(mode,"subset")) {
    if(is.null(subset))stop("Select a registered subset",call.=FALSE)
    w04_subset_verify(subset)
    if(!identical(as.character(subset$review_id),review_id) ||
       !identical(tolower(as.character(subset$source_checkpoint_sha256)),checkpoint))
      stop("Subset does not belong to verified review checkpoint",call.=FALSE)
    selected <- as.character(subset$record_ids)
    if(!all(selected %in% ids))stop("Subset contains ineligible record IDs",call.=FALSE)
  } else if(identical(mode,"all")) {
    selected <- ids
  } else {
    selected <- ids[!ids %in% coded]
  }
  sha <- digest::digest(paste(sort(selected),collapse="\n"),
                        algo="sha256",serialize=FALSE)
  list(review_id=review_id,source_checkpoint_sha256=checkpoint,
       mode=mode,eligible=length(ids),human_coded=length(human),
       model_coded=length(model),already_coded=length(coded),
       selected_count=length(selected),record_ids=selected,
       membership_sha256=sha)
}

# Combine independently chosen sources without duplicate model calls.
# Coded IDs must be resolved from the authoritative saved event registries first.
w04_model_selection_combine <- function(corpus, include_remaining=TRUE,
                                       subsets=list(), coded_ids=NULL) {
  ids <- w04_subset_record_ids(corpus$record_ids)
  checkpoint <- tolower(as.character(corpus$checkpoint_sha256 %||% ""))
  review_id <- as.character(corpus$review_id %||% "")
  if(length(checkpoint)!=1L || !grepl("^[0-9a-f]{64}$",checkpoint) ||
     length(review_id)!=1L || !nzchar(review_id))
    stop("A verified review-specific canonical corpus is required",call.=FALSE)
  if(!is.logical(include_remaining) || length(include_remaining)!=1L ||
     is.na(include_remaining) || !is.list(subsets))
    stop("Invalid screening selection",call.=FALSE)
  if(!include_remaining && !length(subsets))
    stop("Select remaining uncoded records or at least one subset",call.=FALSE)
  if(include_remaining && is.null(coded_ids))
    stop("Authoritative record-level inclusion codes are not yet resolved",call.=FALSE)
  if(!is.null(coded_ids)) {
    coded_ids <- as.character(coded_ids)
    if(anyNA(coded_ids)||any(!nzchar(coded_ids))||!all(coded_ids %in% ids))
      stop("Previously coded record IDs are incomplete or outside the verified corpus",call.=FALSE)
  }
  subset_ids <- character()
  chosen <- character()
  for(z in subsets) {
    w04_subset_verify(z)
    if(!identical(as.character(z$review_id),review_id) ||
       !identical(tolower(as.character(z$source_checkpoint_sha256)),checkpoint))
      stop("Selected subset belongs to a different review or checkpoint",call.=FALSE)
    if(!all(z$record_ids %in% ids))
      stop("Selected subset contains ineligible records",call.=FALSE)
    subset_ids <- c(subset_ids,as.character(z$subset_id))
    chosen <- union(chosen,as.character(z$record_ids))
  }
  if(anyDuplicated(subset_ids))stop("Duplicate subset selected",call.=FALSE)
  remaining <- if(include_remaining) setdiff(ids,unique(coded_ids)) else character()
  selected <- ids[ids %in% union(remaining,chosen)]
  list(review_id=review_id,source_checkpoint_sha256=checkpoint,
       include_remaining=include_remaining,subset_ids=subset_ids,
       eligible=length(ids),already_coded=if(is.null(coded_ids)) NA_integer_ else length(unique(coded_ids)),
       remaining_uncoded=length(remaining),subset_records=length(chosen),
       overlap_removed=length(remaining)+length(chosen)-length(selected),
       selected_count=length(selected),record_ids=selected,
       membership_sha256=digest::digest(paste(sort(selected),collapse="\\n"),
                                        algo="sha256",serialize=FALSE))
}
