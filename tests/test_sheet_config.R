local({
  source("R/storage_sheets.R", local = TRUE)
  old <- Sys.getenv("LEM_GOOGLE_SHEET_ID", unset = NA_character_)
  on.exit({
    if (is.na(old)) Sys.unsetenv("LEM_GOOGLE_SHEET_ID") else Sys.setenv(LEM_GOOGLE_SHEET_ID = old)
  }, add = TRUE)
  Sys.setenv(LEM_GOOGLE_SHEET_ID = "test-sheet-id-A")
  stopifnot(identical(sheet_id_from_env(), "test-sheet-id-A"))
  Sys.setenv(LEM_GOOGLE_SHEET_ID = " test-sheet-id-B ")
  stopifnot(identical(sheet_id_from_env(), "test-sheet-id-B"))
  Sys.unsetenv("LEM_GOOGLE_SHEET_ID")
  err <- tryCatch(sheet_id_from_env(), error = function(e) conditionMessage(e))
  stopifnot(identical(err, "LEM_GOOGLE_SHEET_ID is not configured"))
  cat("PASS: configured Google Sheet ID required, trimmed, and never hard-coded\n")
})

# The Shiny scoping integration must not silently target the experimental pipeline.
# Current-run KPI reporting is an intentional exception: it reads the
# authoritative workflow branch, independently configurable at deployment.
for (path in c("R/github_dispatch.R","multiuser_app/R/github_dispatch.R")) {
  lines <- readLines(path,warn=FALSE)
  stopifnot(!any(grepl("workflow01-final-architecture",lines,fixed=TRUE)))
}
for (path in c("R/storage_sheets.R","multiuser_app/R/storage_sheets.R")) {
  lines <- readLines(path,warn=FALSE)
  stopifnot(any(grepl("LEM_PIPELINE_STATUS_REF",lines,fixed=TRUE)))
  stopifnot(any(grepl("workflow01-final-architecture",lines,fixed=TRUE)))
}
cat("PASS: Shiny scoping stays on its configured branch while KPI status uses active workflow branch\\n")
