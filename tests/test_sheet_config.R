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

# Only scoping is required to use its configurable branch. W01/W02/W08
# production resumes intentionally target workflow01-final-architecture.
# Do not ban that branch string throughout the entire dispatch file.
local({
  source("R/github_dispatch.R",local=TRUE)
  old_ref <- Sys.getenv("LEM_GITHUB_SCOPING_REF",unset=NA_character_)
  on.exit(if(is.na(old_ref))Sys.unsetenv("LEM_GITHUB_SCOPING_REF") else
    Sys.setenv(LEM_GITHUB_SCOPING_REF=old_ref),add=TRUE)
  Sys.unsetenv("LEM_GITHUB_SCOPING_REF")
  stopifnot(identical(github_scoping_ref(),"main"))
  Sys.setenv(LEM_GITHUB_SCOPING_REF="branch-for-scoping-test")
  stopifnot(identical(github_scoping_ref(),"branch-for-scoping-test"))
})
local({
  source("multiuser_app/R/github_dispatch.R",local=TRUE)
  old_ref <- Sys.getenv("LEM_GITHUB_SCOPING_REF",unset=NA_character_)
  on.exit(if(is.na(old_ref))Sys.unsetenv("LEM_GITHUB_SCOPING_REF") else
    Sys.setenv(LEM_GITHUB_SCOPING_REF=old_ref),add=TRUE)
  Sys.setenv(LEM_GITHUB_SCOPING_REF="main")
  stopifnot(identical(github_scoping_ref(),"main"))
  Sys.setenv(LEM_GITHUB_SCOPING_REF="branch-for-scoping-test")
  err <- tryCatch(github_scoping_ref(),error=function(e)conditionMessage(e))
  stopifnot(grepl("requires LEM_GITHUB_SCOPING_REF=main",err,fixed=TRUE))
})
for (path in c("R/storage_sheets.R","multiuser_app/R/storage_sheets.R")) {
  lines <- readLines(path,warn=FALSE)
  stopifnot(any(grepl("LEM_PIPELINE_STATUS_REF",lines,fixed=TRUE)))
  stopifnot(any(grepl("workflow01-final-architecture",lines,fixed=TRUE)))
}
cat("PASS: Shiny scoping stays on its configured branch while KPI status uses active workflow branch\\n")
