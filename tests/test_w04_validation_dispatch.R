source("R/github_dispatch.R")

sha <- paste(rep("a", 64L), collapse = "")
payload <- w04_validation_dispatch_payload("batch-123", toupper(sha))

stopifnot(
  identical(payload$ref, "main"),
  identical(payload$inputs$batch_id, "batch-123"),
  identical(payload$inputs$queue_sha256, sha)
)

must_fail <- function(expr) {
  ok <- FALSE
  tryCatch(force(expr), error = function(e) ok <<- TRUE)
  stopifnot(ok)
}

must_fail(w04_validation_dispatch_payload("", sha))
must_fail(w04_validation_dispatch_payload("batch-123", "not-a-sha"))

dispatch_src <- paste(
  readLines("R/github_dispatch.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
stopifnot(grepl(
  "workflow_04_finalize_human_validation.yml/dispatches",
  dispatch_src,
  fixed = TRUE
))

cat("PASS: W04 validation dispatch contract\n")

# Production model-uncertainty resolution is distinct from manual validation.
# Inspect the actual dispatch body without a live GitHub request.
resolution_body <- paste(deparse(body(dispatch_w04_resolution_resume)), collapse=" ")
stopifnot(
  grepl("workflow_04_resume_after_shiny.yml/dispatches", resolution_body, fixed=TRUE),
  grepl('ref = "workflow01-final-architecture"', resolution_body, fixed=TRUE),
  !grepl('ref = "main"', resolution_body, fixed=TRUE)
)
cat("PASS: W04 production resolution dispatch targets production branch\n")
