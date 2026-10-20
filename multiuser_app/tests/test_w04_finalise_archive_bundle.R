`%||%` <- function(x,y) if(is.null(x))y else x
source("R/w04_shiny_model_launch.R")
sha<-paste(rep("a",64),collapse="")
meta<-list(review_id="review-x")
source<-list(review_id="review-x",lean=list(lean_canonical_sha256=sha),
 lean_path="docs/compaction/zenodo/run-1.json",
 w03_path="docs/publication_status/zenodo/run-2.json")
snapshot<-list(review_id="review-x",checkpoint_sha256=sha,
 rows=list(list(record_id="r1",status="resolved",decision="retain"),
           list(record_id="r2",status="resolved",decision="exclude")))
bundle<-w04_finalise_bundle_build(meta,source,snapshot)
stopifnot(bundle$record_count==2L,identical(bundle$schema,
 "lem-w04-reconciled-finalise-input-v1"),grepl("^[0-9a-f]{64}$",bundle$records_sha256))
bad<-snapshot;bad$checkpoint_sha256<-paste(rep("b",64),collapse="")
stopifnot(inherits(tryCatch(w04_finalise_bundle_build(meta,source,bad),error=identity),"error"))
bad<-snapshot;bad$rows[[2]]$status<-"conflict"
stopifnot(inherits(tryCatch(w04_finalise_bundle_build(meta,source,bad),error=identity),"error"))
cat("PASS immutable W04 finalisation archive input\n")
