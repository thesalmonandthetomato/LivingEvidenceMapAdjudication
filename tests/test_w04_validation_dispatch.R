source("R/github_dispatch.R")

sha <- paste(rep("a", 64L), collapse = "")
payload <- w04_validation_dispatch_payload("batch-123", toupper(sha))

stopifnot(
  identical(payload$ref, "workflow01-final-architecture"),
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
  grepl('ref = github_production_ref()', resolution_body, fixed=TRUE),
  !grepl('ref = "main"', resolution_body, fixed=TRUE)
)
cat("PASS: W04 production resolution dispatch targets production branch\n")

# All production dispatch functions must use the central configuration, not a literal branch.
for (fn in c("dispatch_w00_scoping","dispatch_w01_export","dispatch_w02_resume","w04_validation_dispatch_payload","dispatch_w08_resume","dispatch_w04_resolution_resume")) {
  src <- paste(deparse(body(get(fn))),collapse=" ")
  stopifnot(!grepl('(?:ref|branch)\\s*=\\s*"main"',src,perl=TRUE))
  stopifnot(grepl('github_(production|scoping)_ref\\(\\)',src,perl=TRUE))
}
cat("PASS: all production dispatch functions share guarded branch configuration\n")
