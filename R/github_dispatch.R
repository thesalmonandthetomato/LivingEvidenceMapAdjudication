suppressPackageStartupMessages(library(httr2))

github_dispatch_token <- function() {
  token <- Sys.getenv("LEM_GITHUB_DISPATCH_TOKEN", unset = "")
  if (!nzchar(token)) stop("LEM_GITHUB_DISPATCH_TOKEN is not configured", call. = FALSE)
  token
}

github_dispatch_repo <- function() {
  Sys.getenv("LEM_GITHUB_DISPATCH_REPO", unset = "thesalmonandthetomato/LivingEvidenceMap")
}


github_scoping_ref <- function() {
  Sys.getenv("LEM_GITHUB_SCOPING_REF", unset = "workflow01-final-architecture")
}

github_api_get_json <- function(endpoint) {
  req <- httr2::request(endpoint) |>
    httr2::req_headers(
      Authorization = paste("Bearer", github_dispatch_token()),
      Accept = "application/vnd.github+json",
      `X-GitHub-Api-Version` = "2022-11-28",
      `User-Agent` = "LivingEvidenceMap-Adjudication"
    )
  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  if (status >= 400L) stop(sprintf("GitHub API GET failed with HTTP %d", status), call.=FALSE)
  httr2::resp_body_json(resp, simplifyVector=FALSE)
}

read_github_text_file <- function(path, ref = github_scoping_ref()) {
  endpoint <- sprintf(
    "https://api.github.com/repos/%s/contents/%s?ref=%s",
    github_dispatch_repo(),
    path,
    utils::URLencode(ref, reserved=TRUE)
  )
  x <- github_api_get_json(endpoint)
  encoding <- as.character(x$encoding %||% "")
  content <- gsub("\\s+", "", as.character(x$content %||% ""))
  if (!identical(encoding, "base64") || !nzchar(content)) {
    stop(sprintf("GitHub file %s did not return base64 content", path), call.=FALSE)
  }
  rawToChar(jsonlite::base64_dec(content))
}

dispatch_w00_scoping <- function(request_id, search_string_path = "user_input/scoping_search_string.txt") {
  request_id <- trimws(as.character(request_id))
  if (!grepl("^[A-Za-z0-9._-]{8,80}$", request_id)) {
    stop("Invalid scoping request ID", call.=FALSE)
  }
  endpoint <- sprintf(
    "https://api.github.com/repos/%s/actions/workflows/workflow_00_search_scoping.yml/dispatches",
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
      ref = github_scoping_ref(),
      inputs = list(
        search_string_path = as.character(search_string_path),
        request_id = request_id
      )
    ))
  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  if (!identical(status, 204L)) {
    stop(sprintf("W00 scoping dispatch failed with HTTP %d", status), call.=FALSE)
  }
  invisible(TRUE)
}

find_w00_scoping_run <- function(request_id) {
  endpoint <- sprintf(
    "https://api.github.com/repos/%s/actions/workflows/workflow_00_search_scoping.yml/runs?branch=%s&event=workflow_dispatch&per_page=30",
    github_dispatch_repo(),
    utils::URLencode(github_scoping_ref(), reserved=TRUE)
  )
  x <- github_api_get_json(endpoint)
  runs <- x$workflow_runs %||% list()
  wanted <- paste("W00 scope", as.character(request_id))
  hits <- Filter(function(r) identical(as.character(r$display_title %||% ""), wanted), runs)
  if (!length(hits)) return(NULL)
  hits[[1L]]
}

w00_scoping_run_jobs <- function(run_id) {
  endpoint <- sprintf(
    "https://api.github.com/repos/%s/actions/runs/%s/jobs?per_page=100",
    github_dispatch_repo(),
    as.character(run_id)
  )
  x <- github_api_get_json(endpoint)
  x$jobs %||% list()
}

w00_scoping_run_artifacts <- function(run_id) {
  endpoint <- sprintf(
    "https://api.github.com/repos/%s/actions/runs/%s/artifacts?per_page=100",
    github_dispatch_repo(),
    as.character(run_id)
  )
  x <- github_api_get_json(endpoint)
  x$artifacts %||% list()
}

download_github_artifact_zip <- function(artifact_id, destination) {
  endpoint <- sprintf(
    "https://api.github.com/repos/%s/actions/artifacts/%s/zip",
    github_dispatch_repo(),
    as.character(artifact_id)
  )
  req <- httr2::request(endpoint) |>
    httr2::req_headers(
      Authorization = paste("Bearer", github_dispatch_token()),
      Accept = "application/vnd.github+json",
      `X-GitHub-Api-Version` = "2022-11-28",
      `User-Agent` = "LivingEvidenceMap-Adjudication"
    )
  resp <- httr2::req_perform(req, path=destination)
  if (!file.exists(destination) || file.info(destination)$size <= 0L) {
    stop("Downloaded GitHub artifact is empty", call.=FALSE)
  }
  destination
}

read_w00_scoping_count_artifact <- function(artifact) {
  artifact_id <- as.character(artifact$id %||% "")
  artifact_name <- as.character(artifact$name %||% "")
  if (!grepl("^workflow00-scope-count-", artifact_name)) return(NULL)
  td <- tempfile("w00-scope-artifact-")
  dir.create(td, recursive=TRUE)
  zip_path <- file.path(td, "artifact.zip")
  on.exit(unlink(td, recursive=TRUE, force=TRUE), add=TRUE)
  download_github_artifact_zip(artifact_id, zip_path)
  utils::unzip(zip_path, exdir=td)
  csvs <- list.files(td, pattern="\\.csv$", recursive=TRUE, full.names=TRUE)
  if (length(csvs) != 1L) stop("Expected exactly one CSV in scoping count artifact", call.=FALSE)
  x <- utils::read.csv(csvs[[1L]], stringsAsFactors=FALSE, na.strings=c("", "NA"))
  if (nrow(x) != 1L) stop("Scoping count artifact must contain exactly one row", call.=FALSE)
  as.list(x[1L,,drop=FALSE])
}

read_w00_scoping_final_artifact <- function(artifact) {
  artifact_id <- as.character(artifact$id %||% "")
  artifact_name <- as.character(artifact$name %||% "")
  if (!grepl("^workflow00-search-scoping-", artifact_name)) return(NULL)
  td <- tempfile("w00-scope-final-")
  dir.create(td, recursive=TRUE)
  zip_path <- file.path(td, "artifact.zip")
  on.exit(unlink(td, recursive=TRUE, force=TRUE), add=TRUE)
  download_github_artifact_zip(artifact_id, zip_path)
  utils::unzip(zip_path, exdir=td)
  csvs <- list.files(td, pattern="search_scope_counts\\.csv$", recursive=TRUE, full.names=TRUE)
  if (length(csvs) != 1L) stop("Final scoping artifact is missing search_scope_counts.csv", call.=FALSE)
  utils::read.csv(csvs[[1L]], stringsAsFactors=FALSE, na.strings=c("", "NA"))
}


dispatch_w01_export <- function(batch_id, queue_sha256) {
  batch_id <- as.character(batch_id)
  queue_sha256 <- tolower(as.character(queue_sha256))
  if (!nzchar(batch_id)) stop("Invalid W01 batch ID", call.=FALSE)
  if (!grepl("^[0-9a-f]{64}$", queue_sha256)) stop("Invalid W01 queue SHA-256", call.=FALSE)

  endpoint <- sprintf(
    "https://api.github.com/repos/%s/actions/workflows/shiny-adjudication-dry-run-export-w01.yml/dispatches",
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
      ref = "workflow01-final-architecture"
    ))

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  if (identical(status, 204L)) return(invisible(TRUE))
  if (!identical(status, 404L)) {
    stop(sprintf("W01 export workflow dispatch failed with HTTP %d", status), call.=FALSE)
  }

  stamp <- format(Sys.time(), tz="UTC", format="%Y%m%dT%H%M%SZ")
  marker_path <- sprintf(
    "docs/deduplication/w01_export_requests/%s-%s.json",
    batch_id, stamp
  )
  payload <- jsonlite::toJSON(list(
    batch_id=batch_id,
    queue_sha256=queue_sha256,
    requested_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"),
    requested_by="LivingEvidenceMapAdjudication"
  ),auto_unbox=TRUE,pretty=TRUE)

  endpoint <- sprintf(
    "https://api.github.com/repos/%s/contents/%s",
    github_dispatch_repo(), marker_path
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
      message=sprintf("Request W01 export for %s",batch_id),
      content=jsonlite::base64_enc(charToRaw(payload)),
      branch="workflow01-final-architecture"
    ))

  marker_resp <- httr2::req_perform(marker_req)
  marker_status <- httr2::resp_status(marker_resp)
  if (!marker_status %in% c(200L,201L)) {
    stop(sprintf("W01 export request fallback failed with HTTP %d",marker_status),call.=FALSE)
  }
  invisible(TRUE)
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
