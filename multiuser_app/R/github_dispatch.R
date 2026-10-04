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
  if (identical(status, 204L)) return(invisible(TRUE))

  # During development the resume workflow may live only on the integration
  # branch, in which case GitHub's workflow-dispatch endpoint returns 404
  # because the workflow is absent from the default branch. Fall back to a
  # branch commit that is watched by workflow_02_resume_request_listener.yml.
  if (!identical(status, 404L)) {
    stop(sprintf("GitHub workflow dispatch failed with HTTP %d", status), call. = FALSE)
  }

  stamp <- format(Sys.time(), tz = "UTC", format = "%Y%m%dT%H%M%SZ")
  marker_path <- sprintf(
    "docs/shiny_adjudication/w02_resume_requests/run-%s-%s.json",
    source_run_id, stamp
  )
  payload <- jsonlite::toJSON(list(
    source_run_id = source_run_id,
    publish = isTRUE(publish),
    requested_at_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
    requested_by = "LivingEvidenceMapAdjudication"
  ), auto_unbox = TRUE, pretty = TRUE)

  endpoint <- sprintf(
    "https://api.github.com/repos/%s/contents/%s",
    github_dispatch_repo(),
    marker_path
  )
  marker_req <- httr2::request(endpoint) |>
    httr2::req_method("PUT") |>
    httr2::req_headers(
      Authorization = paste("Bearer", github_dispatch_token()),
      Accept = "application/vnd.github+json",
      `X-GitHub-Api-Version` = "2022-11-28",
      `User-Agent` = "LivingEvidenceMap-Adjudication"
    ) |>
    httr2::req_body_json(list(
      message = sprintf("Request automatic W02 resume for run %s", source_run_id),
      content = jsonlite::base64_enc(charToRaw(payload)),
      branch = "workflow01-final-architecture"
    ))

  marker_resp <- httr2::req_perform(marker_req)
  marker_status <- httr2::resp_status(marker_resp)
  if (!marker_status %in% c(200L, 201L)) {
    stop(sprintf(
      "GitHub W02 resume fallback failed with HTTP %d after workflow dispatch returned 404",
      marker_status
    ), call. = FALSE)
  }

  invisible(TRUE)
}


w04_validation_dispatch_payload <- function(batch_id, queue_sha256) {
  batch_id <- as.character(batch_id)
  queue_sha256 <- tolower(as.character(queue_sha256))
  if (!nzchar(batch_id)) stop("Invalid W04 batch ID", call. = FALSE)
  if (!grepl("^[0-9a-f]{64}$", queue_sha256)) stop("Invalid W04 queue SHA-256", call. = FALSE)

  list(
    ref = "workflow01-final-architecture",
    inputs = list(
      batch_id = batch_id,
      queue_sha256 = queue_sha256
    )
  )
}

dispatch_w04_validation_finalize <- function(batch_id, queue_sha256) {
  payload <- w04_validation_dispatch_payload(batch_id, queue_sha256)

  endpoint <- sprintf(
    "https://api.github.com/repos/%s/actions/workflows/workflow_04_finalize_human_validation.yml/dispatches",
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
    httr2::req_body_json(payload)

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  if (!identical(status, 204L)) {
    stop(sprintf("W04 finalisation dispatch failed with HTTP %d", status), call. = FALSE)
  }

  invisible(TRUE)
}


dispatch_w08_resume <- function(source_run_id, batch_id, queue_sha256) {
  source_run_id <- as.character(source_run_id)
  batch_id <- as.character(batch_id)
  queue_sha256 <- tolower(as.character(queue_sha256))
  if (!grepl("^[0-9]+$", source_run_id)) stop("Invalid W08 source run ID", call. = FALSE)
  if (!nzchar(batch_id)) stop("Invalid W08 batch ID", call. = FALSE)
  if (!grepl("^[0-9a-f]{64}$", queue_sha256)) stop("Invalid W08 queue SHA-256", call. = FALSE)

  endpoint <- sprintf(
    "https://api.github.com/repos/%s/actions/workflows/workflow_08_resume_after_shiny.yml/dispatches",
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
        batch_id = batch_id,
        queue_sha256 = queue_sha256
      )
    ))

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  if (!identical(status, 204L)) {
    stop(sprintf("W08 resume dispatch failed with HTTP %d", status), call. = FALSE)
  }

  invisible(TRUE)
}


dispatch_w04_resolution_resume <- function(source_run_id, batch_id, queue_sha256) {
  source_run_id <- as.character(source_run_id)
  batch_id <- as.character(batch_id)
  queue_sha256 <- tolower(as.character(queue_sha256))
  if (!grepl("^[0-9]+$", source_run_id)) stop("Invalid W04 source run ID", call. = FALSE)
  if (!nzchar(batch_id)) stop("Invalid W04 batch ID", call. = FALSE)
  if (!grepl("^[0-9a-f]{64}$", queue_sha256)) stop("Invalid W04 queue SHA-256", call. = FALSE)

  endpoint <- sprintf(
    "https://api.github.com/repos/%s/actions/workflows/workflow_04_resume_after_shiny.yml/dispatches",
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
        batch_id = batch_id,
        queue_sha256 = queue_sha256
      )
    ))

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  if (!identical(status, 204L)) {
    stop(sprintf("W04 resolution resume dispatch failed with HTTP %d", status), call. = FALSE)
  }
  invisible(TRUE)
}
