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

# The root production app fails closed on incorrect repository or branch configuration.
local({
  source("R/github_dispatch.R",local=TRUE)
  vars <- c("LEM_GITHUB_SCOPING_REF","LEM_GITHUB_DISPATCH_REPO")
  old <- Sys.getenv(vars,unset=NA_character_)
  on.exit({
    for (v in vars) {
      if (is.na(old[[v]])) Sys.unsetenv(v) else
        do.call(Sys.setenv,setNames(list(old[[v]]),v))
    }
  },add=TRUE)
  must_fail <- function(expr) {
    failed <- tryCatch({force(expr);FALSE},error=function(e) TRUE)
    stopifnot(failed)
  }
  Sys.unsetenv(vars)
  stopifnot(identical(github_scoping_ref(),"workflow01-final-architecture"))
  stopifnot(identical(github_production_ref(),"workflow01-final-architecture"))
  stopifnot(identical(github_dispatch_repo(),"thesalmonandthetomato/LivingEvidenceMap"))
  stopifnot(isTRUE(assert_production_github_target()))
  Sys.setenv(LEM_GITHUB_SCOPING_REF="workflow01-final-architecture")
  stopifnot(isTRUE(assert_production_github_target()))
  must_fail(assert_production_github_target("main"))
  must_fail(read_github_text_file("README.md",ref="main"))
  for (bad in c("main","branch-for-scoping-test","")) {
    Sys.setenv(LEM_GITHUB_SCOPING_REF=bad)
    must_fail(github_scoping_ref())
    must_fail(github_dispatch_token())
  }
  Sys.setenv(LEM_GITHUB_SCOPING_REF="workflow01-final-architecture")
  Sys.setenv(LEM_GITHUB_DISPATCH_REPO="wrong/repo")
  must_fail(github_dispatch_repo())
  must_fail(assert_production_github_target())
  cat("PASS: production branch and repository configuration fails closed\n")
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
# Production KPI status deliberately follows the authoritative workflow
# branch; the legacy development copy permits an environment override.
production_storage <- paste(readLines("R/storage_sheets.R",warn=FALSE),collapse="\\n")
development_storage <- paste(readLines("multiuser_app/R/storage_sheets.R",warn=FALSE),collapse="\\n")
stopifnot(
  grepl('read_latest_pipeline_status <- function',production_storage,fixed=TRUE),
  grepl('ref = "workflow01-final-architecture"',production_storage,fixed=TRUE),
  grepl('LEM_PIPELINE_STATUS_REF',development_storage,fixed=TRUE),
  grepl('workflow01-final-architecture',development_storage,fixed=TRUE)
)
cat("PASS: production KPI status uses the active workflow branch; development configuration remains separate\\n")
