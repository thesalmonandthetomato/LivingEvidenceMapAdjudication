# Offline W04 export contract. No Sheets, publication or model calls.
`%||%` <- function(x,y)if(is.null(x)||!length(x))y else x
source("R/w04_subset_registry.R")
source("R/w04_model_batches.R")
make_row <- function(id,decision) {
 jsonlite::toJSON(list(record_id=id,screening=list(decision=decision)),
                   auto_unbox=TRUE)
}
batch <- list(run_id="123")
read_results <- function(b)data.frame(record_id=c("r1","r2"),
 decision=c("retain","uncertain"),
 result_json=c(make_row("r1","retain"),make_row("r2","uncertain")),
 stringsAsFactors=FALSE)
human <- list(list(record_id="r2",status="agreement",final_decision="exclude"))
res <- list(list(record_id="r2",source_run_id="123",decision="exclude"))
reconciled <- w04_screening_coverage_reconcile(c("r1","r2"),list(batch),
 human,res,read_results)
stopifnot(reconciled$ready)
out <- w04_reconciled_export_rows(reconciled,list(batch),human,read_results)
stopifnot(length(out)==2L,
 identical(out[[1]]$decision,"retain"),identical(out[[2]]$human_decision,"exclude"),
 identical(jsonlite::fromJSON(out[[2]]$model_result_json)$screening$decision,"uncertain"))
bad <- function(r)inherits(tryCatch(
 w04_reconciled_export_rows(r,list(batch),human,read_results),error=identity),"error")
incomplete<-reconciled;incomplete$ready<-FALSE
stopifnot(bad(incomplete))
sha <- paste(rep("a",64),collapse="")
member <- digest::digest(paste(sort(c("r1","r2")),collapse="\n"),
 algo="sha256",serialize=FALSE)
key <- digest::digest(paste("review-x",sha,member,sep=":"),
 algo="sha256",serialize=FALSE)
synthetic <- paste0("w04-subset-",substr(key,1L,20L))
check_batch <- list(run_id="123",subset_id=synthetic)
stopifnot(isTRUE(w04_model_verify_batch_checkpoints(list(check_batch),
 "review-x",sha,read_results=read_results)))
wrong <- check_batch;wrong$subset_id <- "w04-subset-00000000000000000000"
stopifnot(inherits(tryCatch(w04_model_verify_batch_checkpoints(list(wrong),
 "review-x",sha,read_results=read_results),error=identity),"error"))
cat("PASS W04 reconciled model-JSON export\n")
