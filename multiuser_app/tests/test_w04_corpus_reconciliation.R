# Pure W04 reconciliation regression; no external services.
`%||%` <- function(x,y) if(is.null(x)||!length(x))y else x
source("R/w04_subset_registry.R")
source("R/w04_model_batches.R")
ids <- paste0("r",1:7)
batches <- list(list(run_id="101"),list(run_id="102"))
reader <- function(b) {
 if(b$run_id=="101")data.frame(record_id=c("r1","r2","r3","r4","r5"),
   decision=c("retain","uncertain","retain","retain","exclude"))
 else data.frame(record_id=c("r3","r4","r6"),
   decision=c("exclude","retain","uncertain"))
}
human <- list(list(record_id="r4",status="agreement",final_decision="exclude"))
x <- w04_screening_coverage_reconcile(ids,batches,human,read_results=reader)
stopifnot(!x$ready,identical(x$missing,"r7"),
          "r3" %in% x$conflicts,"r4" %in% x$conflicts,
          all(c("r2","r6") %in% x$uncertain),
          identical(x$records$r1$decision,"retain"))
resolutions <- list(list(record_id="r2",source_run_id="101",decision="exclude"),
                    list(record_id="r6",source_run_id="102",decision="retain"))
y <- w04_screening_coverage_reconcile(ids,batches,human,resolutions,reader)
stopifnot(identical(y$records$r2$decision,"exclude"),
          identical(y$records$r6$decision,"retain"))
bad <- tryCatch(w04_screening_coverage_reconcile(ids,batches,human,
 list(list(record_id="r1",source_run_id="101",decision="exclude")),reader),
 error=identity)
stopifnot(inherits(bad,"error"))
z <- w04_screening_coverage_reconcile(c("r1"),list(list(run_id="101")),
 read_results=function(b)data.frame(record_id="r1",decision="retain"))
stopifnot(z$ready,z$resolved==1L)
cat("PASS W04 canonical decision reconciliation and provenance guards\n")
