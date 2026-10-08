`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
source("R/storage_sheets.R", local = FALSE)

tabs <- backend_reset_operational_tabs()
stopifnot(
  "queue_w01_active" %in% tabs,
  "queue_w02_active" %in% tabs,
  "queue_w04_validation_active" %in% tabs,
  "queue_w08_active" %in% tabs,
  "workflow_batch_status" %in% tabs,
  "assignments" %in% tabs,
  "pipeline_run_status" %in% tabs,
  !"users" %in% tabs
)

stopifnot(
  backend_reset_is_synthetic_batch("w08-test-assignment-smoke"),
  backend_reset_is_synthetic_batch("w04-test-validation"),
  backend_reset_is_synthetic_batch("test-w02-001"),
  !backend_reset_is_synthetic_batch("w08-run-37234697766")
)

src <- paste(readLines("app.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
stopifnot(
  grepl('"reset_backend_queue"', src, fixed = TRUE),
  grepl('"confirm_backend_reset"', src, fixed = TRUE),
  grepl('session_can("control_workflows")', src, fixed = TRUE),
  grepl('completed >= 11L', src, fixed = TRUE),
  grepl('RESET REPAIR', src, fixed = TRUE),
  grepl('w08-run-37553444492', src, fixed = TRUE),
  grepl('6d1d4ccb24c3e929eb4357b58f53e25a56438c749c32428ca812e51637766f85', src, fixed = TRUE),
  grepl('allowed_obsolete_batches = repair_override', src, fixed = TRUE)
)

storage_src <- paste(readLines("R/storage_sheets.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
stopifnot(
  grepl('ZENODO_ACCESS_TOKEN', storage_src, fixed = TRUE),
  grepl('access_right = "restricted"', storage_src, fixed = TRUE),
  grepl('if (!identical(status, "consumed"))', storage_src, fixed = TRUE),
  grepl('backend_reset_blockers <- function(allowed_obsolete_batches = list())', storage_src, fixed = TRUE),
  grepl('allowed_exact', storage_src, fixed = TRUE),
  grepl('googlesheets4::sheet_delete', storage_src, fixed = TRUE)
)

cat("PASS: guarded backend reset contract\n")
