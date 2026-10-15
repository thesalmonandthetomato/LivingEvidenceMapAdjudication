# Dedicated per-subset W04 queue sheets, never the production model queue.
# A queue is append-only in the sense of creation: overwrite is forbidden.
w04_subset_queue_tab <- function(subset_id) {
  if(!grepl("^w04-subset-[0-9a-f]{20}$",subset_id))
    stop("Invalid subset queue identity",call.=FALSE)
  paste0("w04h_",sub("^w04-subset-","",subset_id))
}
w04_subset_queue_store <- function(queue,review_id) {
  rows <- w04_subset_screening_rows(queue)
  meta <- w04_prompt_review_metadata()
  if(!identical(meta$review_id,review_id))
    stop("Review identity mismatch when saving manual queue",call.=FALSE)
  subsets <- w04_subset_registry_read(review_id)
  match <- Filter(function(x)identical(x$subset_id,queue$subset_id),subsets)
  if(length(match)!=1L) stop("Subset not registered in the active review",call.=FALSE)
  subset <- match[[1L]]
  if(!identical(subset$source_checkpoint_sha256,queue$source_checkpoint_sha256)||
     !identical(as.character(subset$record_ids),
       vapply(queue$cases,function(x)x$record_id,character(1))))
    stop("Queue membership differs from registered subset",call.=FALSE)
  ss <- meta$sheet_id
  tab <- w04_subset_queue_tab(queue$subset_id)
  gs4_auth_from_env()
  if(tab %in% sheet_names_cached(ss,refresh=TRUE))
    stop("Subset queue already exists; overwrite refused",call.=FALSE)
  googlesheets4::sheet_write(rows,ss=ss,sheet=tab)
  invalidate_sheet_names_cache(ss)
  found <- w04_subset_queue_read(subset$subset_id,review_id)
  if(is.null(found)||!identical(found$queue_sha256,queue$queue_sha256)||
     length(found$cases)!=length(queue$cases))
    stop("New W04 subset queue could not be verified after saving",call.=FALSE)
  invisible(queue$queue_sha256)
}
w04_subset_queue_read <- function(subset_id,review_id) {
  meta <- w04_prompt_review_metadata()
  if(!identical(meta$review_id,review_id))
    stop("Queue belongs to a different review",call.=FALSE)
  ss <- meta$sheet_id
  tab <- w04_subset_queue_tab(subset_id)
  gs4_auth_from_env()
  if(!tab %in% sheet_names_cached(ss,refresh=TRUE)) return(NULL)
  rows <- googlesheets4::read_sheet(ss,sheet=tab,col_types="c")
  required <- c("batch_id","queue_sha256","case_index","review_case_id","case_json",
                "subset_id")
  if(!nrow(rows)||!all(required%in%names(rows)))
    stop("Subset queue schema or contents invalid",call.=FALSE)
  if(anyNA(rows$case_index)||anyDuplicated(rows$case_index))
    stop("Duplicate or missing case index",call.=FALSE)
  rows <- rows[order(as.integer(rows$case_index)),,drop=FALSE]
  sha <- unique(rows$queue_sha256)
  batch <- unique(rows$batch_id)
  if(length(sha)!=1L||length(batch)!=1L||
     !identical(batch,paste0("w04-human-",subset_id))||
     !all(rows$subset_id==subset_id)||
     !identical(digest::digest(paste0(paste(rows$case_json,collapse="\n"),"\n"),
                      algo="sha256",serialize=FALSE),sha))
    stop("Persisted W04 subset queue integrity mismatch",call.=FALSE)
  cases <- lapply(rows$case_json,jsonlite::fromJSON,simplifyVector=FALSE)
  if(!identical(vapply(cases,function(x)x$review_case_id,character(1)),
                as.character(rows$review_case_id))||
     anyDuplicated(rows$review_case_id))
    stop("Persisted W04 case identifiers mismatch",call.=FALSE)
  subsets <- w04_subset_registry_read(review_id)
  match <- Filter(function(x)identical(x$subset_id,subset_id),subsets)
  if(length(match)!=1L||!identical(vapply(cases,function(x)x$record_id,character(1)),
                                    as.character(match[[1L]]$record_ids)))
    stop("Persisted queue does not match immutable registered subset",call.=FALSE)
  list(batch_id=batch,queue_sha256=sha,cases=cases,subset_id=subset_id,
       review_mode="registered_human_subset",batch_status="active",
       highlight_include=character(),highlight_exclude=character())
}
