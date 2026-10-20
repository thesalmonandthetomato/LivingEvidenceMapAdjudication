# W04 subset registry. Never sample reviewer queues or change screening decisions.
w04_subset_record_ids <- function(records) {
  ids <- if(is.character(records)) records else {
    if(!is.list(records)) stop("Eligible records must be canonical records",call.=FALSE)
    vapply(records,function(x)as.character(x$record_id %||% ""),character(1))
  }
  ids <- as.character(ids)
  if(!length(ids)||anyNA(ids)||any(!nzchar(ids))||anyDuplicated(ids))
    stop("Eligible canonical IDs must be complete and unique",call.=FALSE)
  ids
}
w04_subset_create <- function(review_id,eligible_ids,size,seed,parent=NULL,
                              source_checkpoint_sha256,created_by,existing=list()) {
  if(!grepl("^[A-Za-z0-9][A-Za-z0-9._-]{2,127}$",review_id))
    stop("Invalid review_id",call.=FALSE)
  if(!grepl("^[0-9a-fA-F]{64}$",source_checkpoint_sha256))
    stop("Eligible canonical corpus checksum is required",call.=FALSE)
  eligible_ids <- w04_subset_record_ids(eligible_ids)
  size <- as.integer(size)
  seed <- as.integer(seed)
  if(length(size)!=1L||is.na(size)||size<1L||
     length(seed)!=1L||is.na(seed)||seed<0L)
    stop("Subset size and non-negative integer seed required",call.=FALSE)
  if(!is.null(parent)) {
    if(!identical(parent$review_id,review_id)||
       !identical(parent$source_checkpoint_sha256,source_checkpoint_sha256))
      stop("Parent subset belongs to another review or source checkpoint",call.=FALSE)
    if(!all(parent$record_ids %in% eligible_ids))
      stop("Parent records are absent from eligible canonical corpus",call.=FALSE)
    pool <- as.character(parent$record_ids)
    parent_id <- as.character(parent$subset_id)
  } else {
    pool <- eligible_ids
    parent_id <- ""
  }
  if(size>length(pool)) stop("Requested subset exceeds eligible sampling pool",call.=FALSE)
  if(length(existing) && any(vapply(existing,function(x)
      identical(as.character(x$review_id),review_id) &&
      identical(as.character(x$source_checkpoint_sha256),source_checkpoint_sha256) &&
      anyDuplicated(x$record_ids)>0L,logical(1))))
    stop("Stored subset has duplicate canonical IDs",call.=FALSE)
  # Save and restore R's global random state, including the no-seed case.
  had_seed <- exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE)
  if(had_seed) old_seed <- get(".Random.seed",envir=.GlobalEnv)
  on.exit({
    if(had_seed) assign(".Random.seed",old_seed,envir=.GlobalEnv)
    else if(exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE))
      rm(".Random.seed",envir=.GlobalEnv)
  },add=TRUE)
  set.seed(seed)
  selected <- sample(pool,size=size,replace=FALSE)
  membership_sha <- digest::digest(paste(sort(selected),collapse="\n"),algo="sha256",
                                    serialize=FALSE)
  config <- paste(review_id,source_checkpoint_sha256,parent_id,size,seed,
                  membership_sha,sep=":")
  subset_id <- paste0("w04-subset-",substr(digest::digest(config,algo="sha256",
                                     serialize=FALSE),1L,20L))
  if(any(vapply(existing,function(x)identical(as.character(x$subset_id),subset_id),
                logical(1)))) stop("Subset already registered",call.=FALSE)
  list(schema="lem-w04-subset-v1",review_id=review_id,subset_id=subset_id,
       parent_subset_id=parent_id,source_checkpoint_sha256=tolower(source_checkpoint_sha256),
       random_seed=seed,requested_size=size,actual_size=length(selected),
       membership_sha256=membership_sha,record_ids=unname(selected),
       created_by=created_by,created_at_utc=format(Sys.time(),tz="UTC",
                                                  format="%Y-%m-%dT%H:%M:%OS6Z"),
       status="registered")
}
w04_subset_verify <- function(x) {
  ids <- w04_subset_record_ids(x$record_ids)
  if(!identical(as.integer(x$actual_size),as.integer(length(ids)))||
     !identical(x$membership_sha256,
       digest::digest(paste(sort(ids),collapse="\n"),algo="sha256",serialize=FALSE)))
    stop("Subset membership checksum or count mismatch",call.=FALSE)
  TRUE
}
w04_subset_pool_remainder <- function(eligible_ids,subsets) {
  ids <- w04_subset_record_ids(eligible_ids)
  used <- unique(unlist(lapply(subsets,function(x) {
    w04_subset_verify(x)
    x$record_ids
  }),use.names=FALSE))
  setdiff(ids,used)
}

# Reconstruct a W04 model selection without changing registered human subsets.
# Caller supplies canonical IDs with already completed model codes; no inference
# is made from subset membership or uncertain classifications.
w04_subset_reconstruct_selection <- function(eligible_ids, coded_ids=character(),
                                              selected_subsets=list(),
                                              include_remaining=TRUE) {
  eligible <- w04_subset_record_ids(eligible_ids)
  coded <- as.character(coded_ids)
  if(anyNA(coded)||any(!nzchar(coded))||!all(coded %in% eligible))
    stop("Coded record IDs must belong to the eligible canonical corpus",call.=FALSE)
  if(!is.logical(include_remaining)||length(include_remaining)!=1L||is.na(include_remaining))
    stop("Invalid include_remaining selection",call.=FALSE)
  if(!is.list(selected_subsets))
    stop("Selected subsets must be registered subset records",call.=FALSE)
  subset_ids <- character()
  for(s in selected_subsets) {
    w04_subset_verify(s)
    member_ids <- as.character(s$record_ids)
    if(!all(member_ids %in% eligible))
      stop("Selected subset contains records outside the eligible canonical corpus",call.=FALSE)
    subset_ids <- union(subset_ids,member_ids)
  }
  remaining <- if(include_remaining) setdiff(eligible,coded) else character()
  chosen <- eligible[eligible %in% union(remaining,subset_ids)]
  if(!length(chosen))stop("Selected screening set is empty",call.=FALSE)
  list(record_ids=chosen,record_count=length(chosen),
       remaining_count=length(remaining),selected_subset_count=length(subset_ids),
       duplicate_count=length(remaining)+length(subset_ids)-length(chosen),
       membership_sha256=digest::digest(paste(sort(chosen),collapse="\n"),
         algo="sha256",serialize=FALSE))
}
