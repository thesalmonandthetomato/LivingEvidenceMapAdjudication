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
