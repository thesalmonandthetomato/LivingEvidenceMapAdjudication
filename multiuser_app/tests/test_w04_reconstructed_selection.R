# Rscript tests/test_w04_reconstructed_selection.R from multiuser_app
`%||%` <- function(x,y) if(is.null(x)) y else x
source("R/w04_subset_registry.R")
ids <- paste0("r",seq_len(6))
sha <- paste(rep("a",64),collapse="")
mk <- function(xs){
 list(record_ids=xs,actual_size=length(xs),
      membership_sha256=digest::digest(paste(sort(xs),collapse="\n"),
       algo="sha256",serialize=FALSE))
}
s <- w04_subset_reconstruct_selection(ids,coded_ids=c("r1","r2"),
 selected_subsets=list(mk(c("r1","r3"))))
stopifnot(identical(s$record_ids,ids[c(1,3,4,5,6)]),
          s$record_count==5L,s$duplicate_count==1L)
sub <- w04_subset_reconstruct_selection(ids,selected_subsets=list(mk(c("r1","r3"))),
 include_remaining=FALSE)
stopifnot(identical(sub$record_ids,c("r1","r3")))
bad <- function(x)inherits(tryCatch(force(x),error=identity),"error")
stopifnot(bad(w04_subset_reconstruct_selection(ids,coded_ids="unknown")),
 bad(w04_subset_reconstruct_selection(ids,include_remaining=FALSE)))
cat("PASS W04 reconstructed selection\n")
