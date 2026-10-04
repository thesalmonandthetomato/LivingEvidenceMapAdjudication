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
