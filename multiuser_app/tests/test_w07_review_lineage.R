source("multiuser_app/R/w07_shiny_review_store.R")
make_event <- function(id,version,parent="",user="reviewer") list(
  event_id=id,record_id="record1",run_id="trial1",user_id=user,
  version=version,supersedes_event_id=parent,timestamp=paste0("2026-10-27T09:00:0",version,"Z"))
stopifnot(length(w07_review_latest(list(make_event("a",1))))==1L)
chain <- list(make_event("a",1),make_event("b",2,"a"))
stopifnot(identical(w07_review_latest(chain)[[1]]$event_id,"b"))
fork <- c(chain,list(make_event("c",2,"a")))
stopifnot(inherits(try(w07_review_latest(fork),silent=TRUE),"try-error"))
missing <- list(make_event("b",2,"missing"))
stopifnot(inherits(try(w07_review_latest(missing),silent=TRUE),"try-error"))
two_users <- list(make_event("a",1,user="one"),make_event("b",1,user="two"))
stopifnot(length(w07_review_latest(two_users))==2L)
legacy <- list(make_event("a","",parent=""),make_event("b",2,"a"))
stopifnot(length(w07_review_latest(legacy))==1L)
competing_first <- list(make_event("a",1),make_event("b",1))
stopifnot(inherits(try(w07_review_latest(competing_first),silent=TRUE),"try-error"))
# An included W04 record must not be excluded by the W07 coding editor.
blocked <- try(w07_review_save(list(review_id="fixture",sheet_id="fixture"),
  "123","record1","reviewer","","OUT_OF_SCOPE","Model suspects mismatch"),silent=TRUE)
stopifnot(inherits(blocked,"try-error"),
  grepl("Invalid W07 relevance decision",as.character(blocked),fixed=TRUE))
cat("PASS W07 review lineage validation\n")
