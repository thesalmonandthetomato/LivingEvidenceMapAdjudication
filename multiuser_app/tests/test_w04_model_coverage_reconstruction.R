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
stopifnot(identical(w04_model_coded_ids(runs,ids,conflicting),"r1"))
stopifnot(!"r1" %in% w04_subset_reconstruct_selection(ids,
 coded_ids=w04_model_coded_ids(runs,ids,conflicting))$record_ids)
human <- list(list(record_id="r2",status="agreement",final_decision="retain"),
 list(record_id="r4",status="conflict",final_decision=""),
 list(record_id="r5",status="single_human",final_decision="exclude"))
stopifnot(identical(w04_human_coded_ids(human,ids),c("r2","r5")))
stopifnot(inherits(tryCatch(w04_human_coded_ids(c(human,list(
 list(record_id="r2",status="agreement",final_decision="exclude"))),ids),
 error=identity),"error"))
choices_batches <- list(
 list(run_id="11",subset_id="s1",prompt_version_id="old"),
 list(run_id="12",subset_id="s1",prompt_version_id="new"),
 list(run_id="21",subset_id="s2",prompt_version_id="chosen"))
chosen <- w04_selected_model_batches(choices_batches,c(s1="11",s2="21"))
stopifnot(identical(vapply(chosen,function(b)b$run_id,character(1)),c("11","21")))
stopifnot(inherits(tryCatch(w04_selected_model_batches(choices_batches,c(s1="",s2="21")),error=identity),"error"))
stopifnot(inherits(tryCatch(w04_selected_model_batches(choices_batches,c(s1="12")),error=identity),"error"))
cat("PASS verified existing model coverage and reconstruction\n")
