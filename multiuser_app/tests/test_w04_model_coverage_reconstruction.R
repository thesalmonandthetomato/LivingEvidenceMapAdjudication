source("R/w04_subset_registry.R")
source("R/w04_model_batches.R")
ids <- paste0("r",1:5)
runs <- list(list(run_id="1"),list(run_id="2"))
reader <- function(b) {
 if(b$run_id=="1") data.frame(record_id=c("r1","r2"),decision=c("retain","uncertain"))
 else data.frame(record_id=c("r1","r3"),decision=c("retain","exclude"))
}
stopifnot(identical(w04_model_coded_ids(runs,ids,reader),c("r1","r3")))
selection <- w04_subset_reconstruct_selection(ids,
 coded_ids=w04_model_coded_ids(runs,ids,reader))
stopifnot(identical(selection$record_ids,c("r2","r4","r5")))
conflicting <- function(b) data.frame(record_id="r1",
 decision=if(b$run_id=="1") "retain" else "exclude")
stopifnot(inherits(tryCatch(w04_model_coded_ids(runs,ids,conflicting),error=identity),"error"))
cat("PASS verified existing model coverage and reconstruction\n")
