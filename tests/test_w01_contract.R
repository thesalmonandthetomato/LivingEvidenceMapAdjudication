suppressPackageStartupMessages({library(jsonlite);library(digest)})
source("R/decision_events.R")
source("R/storage_local.R")
source("R/w01_contract.R")

cases <- read_w01_cases("fixtures/w01_real_sample_2.jsonl")
stopifnot(length(cases) == 2L)
stopifnot(identical(cases[[1]]$review_case_id, "hr-f66acbf328538aa54bcf"))

queue_sha <- digest(file="fixtures/w01_real_sample_2.jsonl", algo="sha256", serialize=FALSE)
tmp <- tempfile(fileext=".jsonl")
out <- tempfile(fileext=".jsonl")

write_local_decision(tmp, list(
  review_case_id="hr-f66acbf328538aa54bcf",
  decision="duplicate",
  rationale="Schema round-trip test.",
  reviewer="test-reviewer",
  resolved_at_utc="2026-10-01T00:00:00Z",
  queue_sha256=queue_sha
))
write_local_decision(tmp, list(
  review_case_id="hr-0d7df583d4e6889b187d",
  decision="not_duplicate",
  rationale="Schema round-trip test.",
  reviewer="test-reviewer",
  resolved_at_utc="2026-10-01T00:00:01Z",
  queue_sha256=queue_sha
))

export_w01_decisions(tmp,out)
ds <- lapply(readLines(out,warn=FALSE), fromJSON, simplifyVector=FALSE)
stopifnot(length(ds)==2L)
stopifnot(setequal(vapply(ds,function(x)x$decision,character(1)),c("duplicate","not_duplicate")))
stopifnot(all(vapply(ds,function(x)identical(x$queue_sha256,queue_sha),logical(1))))

cat("PASS: W01 Shiny prototype contract round-trip\n")
