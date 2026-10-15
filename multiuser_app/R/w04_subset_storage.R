# Persistent W04 subset definitions; an isolated per-review registry tab.
# Does not mutate active queues, assignments, or review decisions.
w04_subset_registry_tab <- function() "w04_subset_registry"
w04_subset_registry_columns <- c("review_id","subset_id","parent_subset_id",
  "source_checkpoint_sha256","membership_sha256","actual_size",
  "created_at_utc","subset_json")
w04_subset_registry_read <- function(review_id) {
  if(!identical(storage_backend(),"google_sheets"))
    stop("Persistent W04 subsets require Google Sheets",call.=FALSE)
  gs4_auth_from_env()
  meta <- w04_prompt_review_metadata()
  if(!identical(meta$review_id,review_id))
    stop("W04 subset registry review identity mismatch",call.=FALSE)
  ss <- meta$sheet_id
  if(!w04_subset_registry_tab() %in% sheet_names_cached(ss)) return(list())
  rows <- as.data.frame(googlesheets4::read_sheet(ss,
    sheet=w04_subset_registry_tab(),col_types="c"))
  if(!nrow(rows)) return(list())
  if(!all(w04_subset_registry_columns %in% names(rows)))
    stop("W04 subset registry column mismatch",call.=FALSE)
  rows <- rows[!is.na(rows$review_id)&rows$review_id==review_id,,drop=FALSE]
  result <- lapply(seq_len(nrow(rows)),function(i) {
    x <- jsonlite::fromJSON(as.character(rows$subset_json[[i]]),
                            simplifyVector=FALSE)
    x$record_ids <- unlist(x$record_ids,use.names=FALSE)
    w04_subset_verify(x)
    if(!identical(x$review_id,review_id) ||
       !identical(x$subset_id,as.character(rows$subset_id[[i]]))||
       !identical(x$membership_sha256,as.character(rows$membership_sha256[[i]]))||
       !identical(x$source_checkpoint_sha256,
                  as.character(rows$source_checkpoint_sha256[[i]])))
      stop("W04 subset registry row identity mismatch",call.=FALSE)
    x
  })
  ids <- vapply(result,function(x)x$subset_id,character(1))
  if(anyDuplicated(ids)) stop("W04 registry contains duplicate subset IDs",call.=FALSE)
  result
}
w04_subset_registry_append <- function(subset) {
  w04_subset_verify(subset)
  meta <- w04_prompt_review_metadata()
  if(!identical(subset$review_id,meta$review_id))
    stop("Cannot save a subset for a different review",call.=FALSE)
  existing <- w04_subset_registry_read(meta$review_id)
  if(any(vapply(existing,function(x)identical(x$subset_id,subset$subset_id),
                logical(1))))
    stop("W04 subset already exists; duplicate registration refused",call.=FALSE)
  ss <- meta$sheet_id
  tab <- w04_subset_registry_tab()
  if(!tab %in% sheet_names_cached(ss)) {
    empty <- as.data.frame(stats::setNames(
      replicate(length(w04_subset_registry_columns),character(),simplify=FALSE),
      w04_subset_registry_columns),stringsAsFactors=FALSE)
    googlesheets4::sheet_write(empty,ss=ss,sheet=tab)
    invalidate_sheet_names_cache(ss)
  }
  row <- data.frame(
    review_id=subset$review_id,subset_id=subset$subset_id,
    parent_subset_id=subset$parent_subset_id,
    source_checkpoint_sha256=subset$source_checkpoint_sha256,
    membership_sha256=subset$membership_sha256,
    actual_size=as.character(subset$actual_size),
    created_at_utc=subset$created_at_utc,
    subset_json=as.character(jsonlite::toJSON(subset,auto_unbox=TRUE,
                                              null="null",digits=NA)),
    stringsAsFactors=FALSE)
  googlesheets4::sheet_append(ss,data=row,sheet=tab)
  verified <- w04_subset_registry_read(meta$review_id)
  hits <- Filter(function(x)identical(x$subset_id,subset$subset_id),verified)
  if(length(hits)!=1L||!identical(hits[[1]]$membership_sha256,subset$membership_sha256))
    stop("W04 subset registry persistence could not be verified",call.=FALSE)
  invisible(subset$subset_id)
}
