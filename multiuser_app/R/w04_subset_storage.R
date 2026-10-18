# Persistent W04 subset definitions; an isolated per-review registry tab.
# Does not mutate active queues, assignments, or review decisions.
w04_subset_registry_tab <- function() "w04_subset_registry"
w04_subset_registry_columns <- c("review_id","subset_id","parent_subset_id",
  "source_checkpoint_sha256","membership_sha256","actual_size",
  "created_at_utc","subset_json")
# Mutable display labels are maintained separately from immutable subset JSON.
# Each edit appends a small review-scoped event; subset membership never changes.
w04_subset_label_text <- function(x) {
  x <- trimws(as.character(x %||% ""))
  if(length(x)!=1L || is.na(x) || nchar(x,type="chars")>100L ||
     grepl("[\\r\\n]",x)) stop("Subset label must be one line (maximum 100 characters)",call.=FALSE)
  x
}
w04_subset_display <- function(subset, records=TRUE) {
  id <- as.character(subset$subset_id)
  label <- trimws(as.character(subset$label %||% ""))
  name <- if(nzchar(label)) paste0(label," · ",id) else id
  if(isTRUE(records)) sprintf("%s (%d records)",name,as.integer(subset$actual_size)) else name
}
w04_subset_label_tab <- function() "w04_subset_labels"
w04_subset_labels_read <- function(ss, review_id) {
  tab <- w04_subset_label_tab()
  if(!tab %in% sheet_names_cached(ss)) return(list())
  rows <- as.data.frame(googlesheets4::read_sheet(ss,sheet=tab,col_types="c"))
  required <- c("review_id","subset_id","label","changed_at_utc","actor_user_id")
  if(!all(required %in% names(rows))) stop("W04 subset label registry is malformed",call.=FALSE)
  if(!nrow(rows)) return(list())
  rows <- rows[!is.na(rows$review_id) & rows$review_id==review_id,,drop=FALSE]
  latest <- list()
  for(i in seq_len(nrow(rows))) {
    id <- as.character(rows$subset_id[[i]])
    if(nzchar(id)) latest[[id]] <- as.character(rows$label[[i]] %||% "")
  }
  latest
}
w04_subset_label_save <- function(review_id,subset_id,label,actor_user_id="") {
  label <- w04_subset_label_text(label)
  meta <- w04_prompt_review_metadata()
  if(!identical(as.character(meta$review_id),as.character(review_id)))
    stop("W04 subset label belongs to a different review",call.=FALSE)
  registered <- w04_subset_registry_read(review_id)
  if(!any(vapply(registered,function(x)identical(x$subset_id,subset_id),logical(1))))
    stop("Cannot label an unregistered subset",call.=FALSE)
  if(identical(as.character(Filter(function(x)identical(x$subset_id,subset_id),registered)[[1L]]$label %||% ""),label))
    return(invisible(registered))
  ss <- meta$sheet_id
  tab <- w04_subset_label_tab()
  if(!tab %in% sheet_names_cached(ss)) {
    cols <- c("review_id","subset_id","label","changed_at_utc","actor_user_id")
    empty <- as.data.frame(stats::setNames(replicate(length(cols),character(),simplify=FALSE),cols),stringsAsFactors=FALSE)
    googlesheets4::sheet_write(empty,ss=ss,sheet=tab)
    invalidate_sheet_names_cache(ss)
  }
  googlesheets4::sheet_append(ss,sheet=tab,data=data.frame(
    review_id=review_id,subset_id=subset_id,label=label,
    changed_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%OS6Z"),
    actor_user_id=as.character(actor_user_id),stringsAsFactors=FALSE))
  verified <- w04_subset_registry_read(review_id)
  found <- Filter(function(x)identical(x$subset_id,subset_id),verified)
  if(length(found)!=1L || !identical(as.character(found[[1L]]$label %||% ""),label))
    stop("W04 subset label update could not be verified",call.=FALSE)
  invisible(verified)
}
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
  labels <- w04_subset_labels_read(ss,review_id)
  for(i in seq_along(result))
    result[[i]]$label <- as.character(labels[[result[[i]]$subset_id]] %||% "")
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
  if(exists("w04_summary_record_safe",mode="function"))
    w04_summary_record_safe("subset",subset$subset_id,subset$created_at_utc,
      w04_overview_chr(subset$label,paste0("Subset ",substr(subset$subset_id,nchar(subset$subset_id)-5L,nchar(subset$subset_id)))),
      paste0(subset$actual_size," records"),review_id=subset$review_id,
      metadata=list(records=as.character(subset$actual_size),parent_subset_id=as.character(subset$parent_subset_id %||% "")))
  invisible(subset$subset_id)
}
