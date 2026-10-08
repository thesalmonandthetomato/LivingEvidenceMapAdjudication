app <- paste(readLines("app.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")

required_sources <- c(
  'source("R/adjudication_schema.R", local = TRUE)',
  'source("R/users.R", local = TRUE)',
  'source("R/assignments.R", local = TRUE)',
  'source("R/decision_events.R", local = TRUE)',
  'source("R/w04_blind_resolution.R", local = TRUE)',
  'source("R/w04_validation_lifecycle.R", local = TRUE)',
  'source("R/storage_local.R", local = TRUE)',
  'source("R/storage_sheets.R", local = TRUE)',
  'source("R/storage_backend.R", local = TRUE)',
  'source("R/auth.R", local = TRUE)',
  'source("R/github_dispatch.R", local = TRUE)'
)
for (needle in required_sources) stopifnot(grepl(needle, app, fixed = TRUE))

required_contracts <- c(
  'session_can("adjudicate_assigned")',
  'session_can("control_workflows")',
  'session_can("manage_assignments")',
  'dispatch_completed_w02 <- function()',
  'dispatch_completed_w04_validation <- function()',
  'dispatch_w04_resolution_resume',
  'dispatch_w08_resume',
  'reviewer_consistency',
  'validation_set',
  'Send validation set to GitHub'
)
for (needle in required_contracts) stopifnot(grepl(needle, app, fixed = TRUE))

# The production candidate must remain self-contained beneath multiuser_app/.
required_files <- c(
  "R/adjudication_schema.R",
  "R/users.R",
  "R/assignments.R",
  "R/decision_events.R",
  "R/w04_blind_resolution.R",
  "R/w04_validation_lifecycle.R",
  "R/storage_local.R",
  "R/storage_sheets.R",
  "R/storage_backend.R",
  "R/auth.R",
  "R/github_dispatch.R"
)
stopifnot(all(file.exists(required_files)))

# Ensure the multi-user manifest exists in-repo; CI separately regenerates it.
stopifnot(file.exists("manifest.json"))

cat("PASS: multi-user pre-production contract\n")


app_src <- paste(readLines("app.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
stopifnot(
  grepl('idle_text = "No records awaiting review"', app_src, fixed = TRUE),
  !grepl('"No active queue"', app_src, fixed = TRUE),
  !grepl('"No reviewer conflicts"', app_src, fixed = TRUE)
)


# Private-repository compatibility: production app must not depend on anonymous
# raw.githubusercontent.com reads from LivingEvidenceMap.
storage_src <- paste(readLines("R/storage_sheets.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
stopifnot(
  !grepl("raw.githubusercontent.com/thesalmonandthetomato/LivingEvidenceMap", app_src, fixed = TRUE),
  !grepl("raw.githubusercontent.com/thesalmonandthetomato/LivingEvidenceMap", storage_src, fixed = TRUE),
  grepl("read_github_text_file(registry_path, ref = ref)", app_src, fixed = TRUE),
  grepl('"docs/current_run/current_run_status.json"', storage_src, fixed = TRUE),
  grepl("LEM_GITHUB_DISPATCH_TOKEN is required to read the authoritative GitHub W04 kappa registry", storage_src, fixed = TRUE)
)

cat("PASS: private-repository GitHub reads are authenticated\n")
