# Offline contract tests for deterministic combined W04 planning.
`%||%` <- function(x,y)if(is.null(x))y else x
source("R/w04_subset_registry.R")
source("R/w04_model_selection.R")
source("R/w04_combined_execution.R")
ids <- sprintf("record-%03d",1:13)
corpus <- list(review_id="r-test",record_ids=ids,
 checkpoint_sha256=paste(rep("a",64),collapse=""))
selection <- w04_model_selection_combine(corpus,TRUE,list(),coded_ids=ids[c(2,4)])
prompt <- list(review_id="r-test",version_id="w04-12345678901234567890",
 sha256=paste(rep("b",64),collapse=""))
plan <- w04_combined_execution_plan(selection,prompt,5L)
stopifnot(plan$record_count==11L,plan$batch_count==3L,
 identical(vapply(plan$batches,function(x)x$record_count,integer(1)),c(5L,5L,1L)),
 !plan$dispatch_enabled)
reverse <- selection
reverse$record_ids <- rev(reverse$record_ids)
stopifnot(identical(plan,w04_combined_execution_plan(reverse,prompt,5L)))
resume <- w04_combined_resume_plan(plan,list(list(
 batch_key=plan$batches[[1]]$batch_key,
 status="succeeded",
 membership_sha256=plan$batches[[1]]$membership_sha256)))
stopifnot(length(resume$completed)==1L,length(resume$ready)==2L,
 !resume$all_complete)
failed <- function(x)inherits(tryCatch(force(x),error=identity),"error")
tampered <- selection
tampered$record_ids[[1]] <- "wrong"
stopifnot(failed(w04_combined_execution_plan(tampered,prompt,5L)),
 failed(w04_combined_execution_plan(selection,prompt,0)),
 failed(w04_combined_resume_plan(plan,list(list(
 batch_key=plan$batches[[1]]$batch_key,status="succeeded",
 membership_sha256="incorrect")))))
cat("W04 immutable combined execution plan tests passed\\n")

# Combined batches retain their own provenance: never masquerade as subsets.
meta <- list(review_id="r-test",sheet_id=paste(rep("S",30),collapse=""))
bundle <- w04_combined_batch_bundle(meta,plan,1L)
stopifnot(identical(bundle$schema,"lem-w04-combined-batch-input-v1"),
  identical(bundle$batch_membership_sha256,plan$batches[[1L]]$membership_sha256),
  identical(unlist(bundle$record_ids,use.names=FALSE),plan$batches[[1L]]$record_ids),
  !isTRUE(bundle$dispatch_enabled),is.null(bundle$subset_id))
stopifnot(failed(w04_combined_batch_bundle(meta,plan,0L)),
  failed(w04_combined_batch_bundle(modifyList(meta,list(review_id="wrong")),plan,1L)))
