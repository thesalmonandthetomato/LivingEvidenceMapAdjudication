suppressPackageStartupMessages(library(jsonlite))

`%||%` <- function(x, y) if (is.null(x)) y else x

source("R/adjudication_schema.R")
source("R/assignments.R")
source("R/storage_local.R")

tmp_dir <- tempfile("assignment-write-test-")
dir.create(tmp_dir)
path <- file.path(tmp_dir, "assignments.jsonl")

rows <- list(
  list(
    assignment_id="asg-1",
    workflow="01",
    task_type="deduplication",
    batch_id="batch-1",
    case_id="case-1",
    user_id="usr-a",
    blind_group="pool-01-deduplication",
    status="assigned"
  ),
  list(
    assignment_id="asg-2",
    workflow="01",
    task_type="deduplication",
    batch_id="batch-1",
    case_id="case-2",
    user_id="usr-b",
    blind_group="pool-01-deduplication",
    status="assigned"
  )
)

write_local_assignments(path, rows)
back <- read_local_assignments(path)
stopifnot(
  length(back) == 2L,
  identical(back[[1L]]$assignment_id, "asg-1"),
  identical(back[[2L]]$user_id, "usr-b")
)

cat("PASS: local assignment registry writes atomically and reads back\n")
