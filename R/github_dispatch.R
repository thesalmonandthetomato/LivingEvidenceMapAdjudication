suppressPackageStartupMessages(library(httr2))

github_dispatch_token <- function() {
  token <- Sys.getenv("LEM_GITHUB_DISPATCH_TOKEN", unset = "")
  if (!nzchar(token)) stop("LEM_GITHUB_DISPATCH_TOKEN is not configured", call. = FALSE)
  token
}

github_dispatch_repo <- function() {
  Sys.getenv("LEM_GITHUB_DISPATCH_REPO", unset = "thesalmonandthetomato/LivingEvidenceMap")
}

dispatch_w02_resume <- function(source_run_id, publish = TRUE) {
  source_run_id <- as.character(source_run_id)
  if (!grepl("^[0-9]+$", source_run_id)) stop("Invalid W02 source run ID", call. = FALSE)

  endpoint <- sprintf(
    "https://api.github.com/repos/%s/actions/workflows/workflow_02_resume_after_human_review.yml/dispatches",
    github_dispatch_repo()
  )

  req <- httr2::request(endpoint) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Authorization = paste("Bearer", github_dispatch_token()),
      Accept = "application/vnd.github+json",
      `X-GitHub-Api-Version` = "2022-11-28",
      `User-Agent` = "LivingEvidenceMap-Adjudication"
    ) |>
    httr2::req_body_json(list(
      ref = "workflow01-final-architecture",
      inputs = list(
        source_run_id = source_run_id,
        publish = if (isTRUE(publish)) "true" else "false"
      )
    ))

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  if (!identical(status, 204L)) {
    stop(sprintf("GitHub workflow dispatch failed with HTTP %d", status), call. = FALSE)
  }

  invisible(TRUE)
}
