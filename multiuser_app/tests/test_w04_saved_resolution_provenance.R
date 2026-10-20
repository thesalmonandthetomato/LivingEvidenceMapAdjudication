# Offline W04 saved-resolution provenance tests.
`%||%` <- function(x,y)if(is.null(x)||!length(x))y else x
source("R/w04_model_batches.R")
w04_filter_batch_decisions <- function(xs,sha)Filter(function(x)
 identical(as.character(x$queue_sha256),sha),xs)
b <- list(run_id="123",uncertain="1",resolution_tab="queue_123",
          resolution_queue_sha256="sha-one")
queue <- function(tab)list(source_run_id="123",queue_sha256="sha-one",
 cases=list(list(record_id="canonical-1",review_case_id="case-1")))
event <- function()list(list(queue_sha256="sha-one",review_case_id="case-1",
 record_id="canonical-1",decision="retain"))
out <- w04_model_verified_resolution_events(list(b),queue,event)
stopifnot(length(out)==1L,identical(out[[1]]$source_run_id,"123"),
 identical(out[[1]]$record_id,"canonical-1"),identical(out[[1]]$decision,"retain"))
bad <- function(q,d)inherits(tryCatch(w04_model_verified_resolution_events(
 list(b),q,d),error=identity),"error")
stopifnot(bad(function(tab)list(source_run_id="wrong",queue_sha256="sha-one",
 cases=list(list(record_id="canonical-1",review_case_id="case-1"))),event),
 bad(queue,function()list(list(queue_sha256="sha-one",review_case_id="case-1",
 record_id="other-record",decision="retain"))))
cat("PASS verified model uncertainty resolution provenance\n")
