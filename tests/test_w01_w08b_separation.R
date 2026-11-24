# Production review routing regression: monthly W01 and historical W08b must
# remain separate in the task dashboard and in export actions.
app <- paste(readLines("app.R",warn=FALSE,encoding="UTF-8"),collapse="\n")
stopifnot(
  grepl('"Citation-chasing deduplication"',app,fixed=TRUE),
  grepl('"W08b · Citation chasing"',app,fixed=TRUE),
  grepl('"Monthly bibliographic duplicate adjudication. Exports only to the current W01 checkpoint."',app,fixed=TRUE),
  grepl('w01_total,\n              w01_completed,\n              w01_remaining,',app,fixed=TRUE),
  grepl('if (length(w08b_pending_cases())>0L) "w08b_open_review" else NULL',app,fixed=TRUE),
  grepl('"W08b · historical citation chasing"',app,fixed=TRUE),
  !grepl("w01_total + if(is.null(w08b_review_queue_rv()))",app,fixed=TRUE),
  !grepl("w01_completed + length(w08b_review_decisions_rv())",app,fixed=TRUE),
  !grepl("w01_remaining + length(w08b_pending_cases())",app,fixed=TRUE)
)
dispatch <- paste(readLines("R/github_dispatch.R",warn=FALSE),collapse="\n")
w08b <- paste(readLines("R/w08b_review.R",warn=FALSE),collapse="\n")
stopifnot(
  grepl("dispatch_w01_export <- function",dispatch,fixed=TRUE),
  grepl("shiny-adjudication-dry-run-export-w01.yml",dispatch,fixed=TRUE),
  grepl("decisions_w08b_dedup",w08b,fixed=TRUE),
  grepl("queue_w08b_dedup_active",w08b,fixed=TRUE)
)
cat("PASS: W01 monthly review and W08b citation-chasing dashboard/exports are distinct\n")
