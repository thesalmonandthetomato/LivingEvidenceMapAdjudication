suppressPackageStartupMessages({
  library(jsonlite)
  library(digest)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

source("R/decision_events.R")
source("R/storage_local.R")

tmp_dir <- tempfile("w01-events-test-")
dir.create(tmp_dir)
path <- file.path(tmp_dir, "decisions.jsonl")

d1 <- list(
  review_case_id = "case-001",
  decision = "duplicate",
  rationale = "test",
  reviewer = "usr-neal",
  resolved_at_utc = "2026-10-03T06:30:00Z",
  queue_sha256 = paste(rep("a", 64), collapse = "")
)

s1 <- write_local_decision(path, d1)
stopifnot(
  identical(as.integer(s1$version), 1L),
  identical(as.character(s1$user_id), "usr-neal"),
  identical(as.character(s1$supersedes_decision_id), "")
)

d2 <- d1
d2$decision <- "not_duplicate"
d2$resolved_at_utc <- "2026-10-03T06:31:00Z"

s2 <- write_local_decision(path, d2)
stopifnot(
  identical(as.integer(s2$version), 2L),
  identical(as.character(s2$supersedes_decision_id), as.character(s1$decision_id))
)

d_other <- d1
d_other$reviewer <- "usr-sini"
d_other$resolved_at_utc <- "2026-10-03T06:32:00Z"

blocked <- tryCatch(
  {
    write_local_decision(path, d_other)
    FALSE
  },
  error = function(e) grepl("already been resolved by another reviewer", conditionMessage(e), fixed = TRUE)
)
stopifnot(isTRUE(blocked))

events <- read_local_decision_events(path)
stopifnot(length(events) == 2L)

active <- read_local_decisions(path)
stopifnot(
  length(active) == 1L,
  identical(as.character(active[[1L]]$decision_id), as.character(s2$decision_id)),
  identical(as.character(active[[1L]]$user_id), "usr-neal"),
  identical(as.character(active[[1L]]$decision), "not_duplicate"),
  isTRUE(active[[1L]]$active)
)

cat("PASS: W01 first reviewer owns the case and may revise it; other reviewers are blocked\n")
