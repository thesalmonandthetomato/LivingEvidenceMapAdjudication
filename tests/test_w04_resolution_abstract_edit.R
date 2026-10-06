app <- paste(readLines("app.R", warn=FALSE, encoding="UTF-8"), collapse="\n")
storage <- paste(readLines("R/storage_sheets.R", warn=FALSE, encoding="UTF-8"), collapse="\n")

stopifnot(grepl('w04_resolution_edit_abstract', app, fixed=TRUE))
stopifnot(grepl('w04_resolution_save_abstract', app, fixed=TRUE))
stopifnot(grepl('w04_resolution_abstract_text', app, fixed=TRUE))
stopifnot(grepl('append_sheet_w04_resolution_abstract_edit', app, fixed=TRUE))
stopifnot(grepl('active_sheet_w04_resolution_abstract_edits', app, fixed=TRUE))
stopifnot(grepl('w04_resolution_abstract_edits', storage, fixed=TRUE))
stopifnot(grepl('supersedes_edit_id', storage, fixed=TRUE))
stopifnot(grepl('Saved abstracts are carried into the canonical record when W04 is sent to GitHub.', app, fixed=TRUE))

cat("PASS: W04 model-resolution abstract editing contract\n")

stopifnot(grepl('google_scholar_title_url <- function', app, fixed=TRUE))
stopifnot(grepl('google_scholar_button <- function', app, fixed=TRUE))
stopifnot(grepl('https://scholar.google.co.uk/scholar?start=0&q=', app, fixed=TRUE))
stopifnot(length(gregexpr('google_scholar_button\\(b\\$title', app, perl=TRUE)[[1L]]) == 3L)
stopifnot(grepl('if \\(!nzchar\\(url\\)\\) return\\(NULL\\)', app, perl=TRUE))

cat("PASS: W04 Google Scholar title-link contract\n")

app_src <- paste(readLines("app.R", warn=FALSE, encoding="UTF-8"), collapse="\n")
stopifnot(
  grepl("w04_resolution_resume_requested_rv <- reactiveVal(FALSE)", app_src, fixed=TRUE),
  grepl("Sent to GitHub. Workflow 04 finalisation has been requested.", app_src, fixed=TRUE),
  grepl("w04_resolution_resume_requested_rv(TRUE)", app_src, fixed=TRUE)
)
cat("PASS: W04 resolution GitHub send-state UI contract\n")

stopifnot(
  grepl("w02_resume_requested_rv <- reactiveVal(FALSE)", app_src, fixed=TRUE),
  grepl("Sent to GitHub. Workflow 02 resume has been requested.", app_src, fixed=TRUE),
  grepl("w08_resume_requested_rv <- reactiveVal(FALSE)", app_src, fixed=TRUE),
  grepl("Sent to GitHub. Workflow 08 resume has been requested.", app_src, fixed=TRUE)
)
cat("PASS: W02/W08 GitHub send-state UI contract\n")

stopifnot(
  grepl("w04_validation_finalize_requested_rv <- reactiveVal(FALSE)", app_src, fixed=TRUE),
  grepl("Sent to GitHub. Workflow 04 validation finalisation has been requested.", app_src, fixed=TRUE),
  grepl("w04_validation_finalize_request_exists", app_src, fixed=TRUE)
)
cat("PASS: W04 validation GitHub send-state UI contract\n")
