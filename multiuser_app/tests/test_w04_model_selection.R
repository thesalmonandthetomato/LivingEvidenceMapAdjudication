# Offline regression: Rscript tests/test_w04_model_selection.R from multiuser_app
`%||%` <- function(x,y) if(is.null(x))y else x
source("R/w04_subset_registry.R")
source("R/w04_model_selection.R")
ids <- sprintf("record-%03d",1:8)
corpus <- list(review_id="review-one",record_ids=ids,
  checkpoint_sha256=paste(rep("a",64),collapse=""))
events_human <- list(list(record_id=ids[[2]],decision="retain"),
                     list(record_id=ids[[3]],decision="exclude"))
events_model <- list(list(record_id=ids[[4]],decision="retain"),
                     list(record_id=ids[[5]],decision="uncertain"))
x <- w04_model_selection_preflight(corpus,events_human,events_model)
stopifnot(x$eligible==8L,x$already_coded==3L,x$selected_count==5L,
  identical(x$record_ids,ids[c(1,5,6,7,8)]))
all <- w04_model_selection_preflight(corpus,events_human,events_model,"all")
stopifnot(all$selected_count==8L)
failed <- function(expr)inherits(tryCatch(force(expr),error=identity),"error")
stopifnot(failed(w04_model_selection_preflight(corpus,
 list(list(decision="retain",review_case_id="untrusted|record-002")))),
 failed(w04_model_selection_preflight(corpus,
 list(list(decision="retain",record_id="record-outside")))))
cat("W04 canonical selection tests passed\n")

# Combined selection: remaining uncoded plus overlapping, explicitly selected subsets.
s1 <- w04_subset_create("review-one",ids,3L,4L,
  source_checkpoint_sha256=corpus$checkpoint_sha256,created_by="test")
s2 <- w04_subset_create("review-one",ids,4L,7L,
  source_checkpoint_sha256=corpus$checkpoint_sha256,created_by="test")
combined <- w04_model_selection_combine(corpus,TRUE,list(s1,s2),
  coded_ids=ids[c(2,3)])
stopifnot(setequal(combined$record_ids,
  union(setdiff(ids,ids[c(2,3)]),union(s1$record_ids,s2$record_ids))),
  combined$selected_count==length(unique(combined$record_ids)),
  combined$overlap_removed>=0L)
stopifnot(failed(w04_model_selection_combine(corpus,TRUE,list(s1),coded_ids=NULL)))
subset_only <- w04_model_selection_combine(corpus,FALSE,list(s1))
stopifnot(setequal(subset_only$record_ids,s1$record_ids))
