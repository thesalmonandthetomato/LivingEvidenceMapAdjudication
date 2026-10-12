# Opt-in integration test using the existing review Google Sheet and credentials.
# Writes only unique integration-test review rows into search_strategy_versions.
# Retains test history for auditing. No W00 dispatch, no adjudication changes.
stopifnot(identical(Sys.getenv("LEM_ALLOW_SEARCH_SHEETS_TEST"),"YES"))
sheet_id <- trimws(Sys.getenv("LEM_GOOGLE_SHEET_ID"))
if (!nzchar(sheet_id) || !grepl("^[A-Za-z0-9_-]{20,}$",sheet_id))
  stop("LEM_GOOGLE_SHEET_ID must identify the existing review spreadsheet.",call.=FALSE)
if (!nzchar(Sys.getenv("GITHUB_RUN_ID")))
  stop("Unique GitHub test run identifier is required.",call.=FALSE)
Sys.setenv(LEM_STORAGE_BACKEND="google_sheets")
source("R/adjudication_schema.R")
source("R/users.R")
source("R/search_boolean_validation.R")
source("R/storage_sheets.R")
source("R/storage_backend.R")
source("R/search_strategy_versions.R")
key <- search_strategy_review_key(paste0("integration-test-",Sys.getenv("GITHUB_RUN_ID"),"-",Sys.getenv("GITHUB_RUN_ATTEMPT",unset="1")))
if(nrow(search_strategy_versions(key))!=0L)
  stop("Test review unexpectedly exists.",call.=FALSE)
v1 <- search_strategy_finalise(key,"salmon AND farm*","test-admin",0L)
v2 <- search_strategy_finalise(key,"salmon AND (farm* OR aquaculture)","test-admin",1L)
rows <- search_strategy_versions(key)
stopifnot(nrow(rows)==2L,
          identical(as.character(rows$version),c("1","2")),
          identical(as.character(rows$strategy_sha256),c(v1$strategy_sha256,v2$strategy_sha256)),
          identical(as.character(rows$search_string),c("salmon AND farm*","salmon AND (farm* OR aquaculture)")))
stopifnot(inherits(try(search_strategy_finalise(key,"salmon OR trout","test-admin",1L),
                         silent=TRUE),"try-error"))
cat("PASS: isolated Google Sheets version 1/2 write, read-back, immutable history and stale-write rejection\n")
