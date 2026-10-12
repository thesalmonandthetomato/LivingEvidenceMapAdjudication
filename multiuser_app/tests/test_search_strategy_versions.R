source("R/search_boolean_validation.R")
source("R/search_strategy_versions.R")
storage_backend <- function() "local"
path <- tempfile(fileext=".jsonl")
Sys.setenv(LEM_SEARCH_STRATEGY_LOG=path)
k <- search_strategy_review_key("review-test")
other <- search_strategy_review_key("another-review")
stopifnot(nrow(search_strategy_versions(k))==0L)
v1 <- search_strategy_finalise(k,"salmon AND farming","tester",expected_version=0L)
stopifnot(identical(v1$version,"1"))
stopifnot(inherits(try(search_strategy_finalise(k,"salmon AND farming","tester",1L),
                         silent=TRUE),"try-error"))
stopifnot(inherits(try(search_strategy_finalise(k,"salmon OR feed","tester",0L),
                         silent=TRUE),"try-error"))
stopifnot(inherits(try(search_strategy_finalise(k,"salmon AND","tester",1L),
                         silent=TRUE),"try-error"))
v2 <- search_strategy_finalise(k,"salmon OR feed","tester",1L)
rows <- search_strategy_versions(k)
stopifnot(identical(as.character(rows$version),c("1","2")),
          identical(as.character(rows$search_string),c("salmon AND farming","salmon OR feed")),
          identical(as.character(rows$strategy_sha256[[1L]]),v1$strategy_sha256),
          identical(as.character(rows$strategy_sha256[[2L]]),v2$strategy_sha256),
          nrow(search_strategy_versions(other))==0L)
# Refuse non-atomic backend writes even when the version appears available.
storage_backend <- function() "google_sheets"
stopifnot(inherits(try(search_strategy_finalise(k,"another AND strategy","tester",2L),
                         silent=TRUE),"try-error"))
storage_backend <- function() "local"
lock <- paste0(path,".lock")
stopifnot(dir.create(lock))
stopifnot(inherits(try(search_strategy_finalise(k,"another AND strategy","tester",2L),
                         silent=TRUE),"try-error"))
unlink(lock,recursive=TRUE)
stopifnot(nrow(search_strategy_versions(k))==2L)
unlink(path)
cat("PASS: immutable strategy version ordering, stale-write rejection and isolation\n")
