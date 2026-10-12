# Opt-in integration test. Requires a dedicated test Google Sheet and credentials.
# Never run against LEM_GOOGLE_SHEET_ID or without explicit opt-in.
stopifnot(identical(Sys.getenv("LEM_ALLOW_SEARCH_SHEETS_TEST"),"YES"))
test_id <- trimws(Sys.getenv("LEM_TEST_GOOGLE_SHEET_ID"))
production_id <- trimws(Sys.getenv("LEM_GOOGLE_SHEET_ID"))
if(!nzchar(test_id) || !nzchar(production_id) || identical(test_id,production_id))
  stop("A distinct test Sheets ID and production Sheets ID are mandatory.",call.=FALSE)
if(!grepl("^[A-Za-z0-9_-]{20,}$",test_id))
  stop("Invalid test Google Sheet ID.",call.=FALSE)
Sys.setenv(LEM_STORAGE_BACKEND="google_sheets",LEM_GOOGLE_SHEET_ID=test_id)
source("R/adjudication_schema.R")
source("R/users.R")
source("R/search_boolean_validation.R")
source("R/storage_sheets.R")
source("R/storage_backend.R")
source("R/search_strategy_versions.R")
key <- search_strategy_review_key(paste0("integration-test-",Sys.getenv("GITHUB_RUN_ID")))
if(!nzchar(Sys.getenv("GITHUB_RUN_ID")))
  stop("Unique GitHub test run identifier is required.",call.=FALSE)
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
