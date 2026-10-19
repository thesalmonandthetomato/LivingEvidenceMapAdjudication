# Read-only W04 production preflight. Does not dispatch, append, or mutate registry.
w04_production_preflight <- function(corpus, subsets, excluded_subset_ids, prompt=NULL) {
  ids <- w04_subset_record_ids(corpus$record_ids)
  checkpoint <- tolower(as.character(corpus$checkpoint_sha256 %||% ""))
  review <- as.character(corpus$review_id %||% "")
  if(!grepl("^[0-9a-f]{64}$",checkpoint) || !nzchar(review))
    stop("Verified review-bound W04 corpus is required",call.=FALSE)
  chosen <- unique(as.character(excluded_subset_ids))
  if(length(chosen)!=2L || any(!nzchar(chosen)))
    stop("Select exactly two distinct validation subsets",call.=FALSE)
  matches <- lapply(chosen,function(sid) {
    hit <- Filter(function(z)identical(as.character(z$subset_id %||% ""),sid),subsets)
    if(length(hit)!=1L)stop(paste("Validation subset not found uniquely:",sid),call.=FALSE)
    z <- hit[[1L]]
    w04_subset_verify(z)
    if(!identical(as.character(z$review_id %||% ""),review) ||
       !identical(tolower(as.character(z$source_checkpoint_sha256 %||% "")),checkpoint))
      stop("Validation subset does not match the verified review and source",call.=FALSE)
    z
  })
  excluded <- lapply(matches,function(z)as.character(z$record_ids))
  if(any(lengths(excluded)!=100L))stop("Each validation subset must contain exactly 100 records",call.=FALSE)
  if(length(intersect(excluded[[1L]],excluded[[2L]])))
    stop("Validation subsets overlap; refusing to exclude an ambiguous 200-record set",call.=FALSE)
  omitted <- unlist(excluded,use.names=FALSE)
  if(!all(omitted %in% ids))stop("Validation subset includes records outside the eligible corpus",call.=FALSE)
  remaining <- ids[!ids %in% omitted]
  if(!length(remaining))stop("No production records remain",call.=FALSE)
  membership_sha <- digest::digest(paste(sort(remaining),collapse="\n"),
    algo="sha256",serialize=FALSE)
  prompt_id <- as.character(prompt$version_id %||% "")
  prompt_sha <- as.character(prompt$sha256 %||% "")
  if(!is.null(prompt) && (!grepl("^w04-[0-9a-f]{20}$",prompt_id) ||
    !grepl("^[0-9a-f]{64}$",prompt_sha) ||
    !identical(w04_prompt_checksum(prompt$content),prompt_sha)))
      stop("Selected archived prompt failed verification",call.=FALSE)
  manifest <- list(schema="lem-w04-production-preflight-v1",review_id=review,
    source_checkpoint_sha256=checkpoint,w03_status_sha256=as.character(corpus$w03_status_sha256 %||% ""),
    excluded_subset_ids=chosen,excluded_record_ids=sort(omitted),
    record_ids=remaining,record_count=length(remaining),
    membership_sha256=membership_sha,prompt_version_id=prompt_id,prompt_sha256=prompt_sha)
  manifest$manifest_sha256 <- digest::digest(
    jsonlite::toJSON(manifest,auto_unbox=TRUE,null="null",digits=NA),
    algo="sha256",serialize=FALSE)
  list(eligible=length(ids),excluded=length(omitted),remaining=length(remaining),
    subset_ids=chosen,membership_sha256=membership_sha,manifest=manifest)
}
