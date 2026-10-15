# W04 registered-subset to existing human-screening queue adapter.
# Fail closed: this never generates replacement case IDs or invents records.
w04_subset_queue_cases <- function(subset,queue_cases,queue_checkpoint_sha256,
                                   require_complete=TRUE) {
  w04_subset_verify(subset)
  checkpoint <- tolower(as.character(queue_checkpoint_sha256))
  if(length(checkpoint)!=1L ||
     !identical(checkpoint,tolower(as.character(subset$source_checkpoint_sha256))))
    stop("W04 queue canonical checkpoint is not the registered subset source",call.=FALSE)
  if(!is.list(queue_cases)) stop("W04 queue must contain case objects",call.=FALSE)
  ids <- vapply(queue_cases,function(x) as.character(x$record_id %||% ""),character(1))
  cases <- vapply(queue_cases,function(x) as.character(x$review_case_id %||% ""),character(1))
  if(anyNA(ids)||any(!nzchar(ids))||anyDuplicated(ids)||
     anyNA(cases)||any(!nzchar(cases))||anyDuplicated(cases))
    stop("W04 queue has missing or duplicate canonical/case IDs",call.=FALSE)
  wanted <- as.character(subset$record_ids)
  found <- match(wanted,ids)
  if(isTRUE(require_complete) && anyNA(found))
    stop(sprintf("W04 queue lacks %d/%d registered subset records; full source queue required",
      sum(is.na(found)),length(wanted)),call.=FALSE)
  selected <- queue_cases[found[!is.na(found)]]
  if(length(selected)!=length(wanted) && isTRUE(require_complete))
    stop("Partial W04 subset mapping refused",call.=FALSE)
  selected
}
w04_subset_queue_coverage <- function(subset,queue_cases,queue_checkpoint_sha256) {
  w04_subset_verify(subset)
  checkpoint_ok <- identical(tolower(as.character(queue_checkpoint_sha256)),
                             tolower(as.character(subset$source_checkpoint_sha256)))
  ids <- vapply(queue_cases,function(x)as.character(x$record_id %||% ""),character(1))
  if(anyNA(ids)||any(!nzchar(ids))||anyDuplicated(ids))
    stop("W04 queue has invalid canonical IDs",call.=FALSE)
  found <- sum(subset$record_ids %in% ids)
  list(subset_id=subset$subset_id,matched=as.integer(found),
       expected=as.integer(length(subset$record_ids)),
       missing=as.integer(length(subset$record_ids)-found),
       checkpoint_matches=isTRUE(checkpoint_ok),
       ready=isTRUE(checkpoint_ok)&&found==length(subset$record_ids))
}
