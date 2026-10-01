suppressPackageStartupMessages({
  library(googlesheets4)
  library(jsonlite)
})

gs4_auth_from_env <- function() {
  sa_json <- Sys.getenv("LEM_GOOGLE_SERVICE_ACCOUNT_JSON", unset = "")
  if (!nzchar(sa_json)) stop("LEM_GOOGLE_SERVICE_ACCOUNT_JSON is not set", call.=FALSE)

  credential_path <- sa_json
  cleanup <- FALSE
  if (!file.exists(credential_path)) {
    parsed <- tryCatch(jsonlite::fromJSON(sa_json, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(parsed) || is.null(parsed$type) || !identical(parsed$type, "service_account")) {
      stop("LEM_GOOGLE_SERVICE_ACCOUNT_JSON is neither a readable file path nor valid service-account JSON", call.=FALSE)
    }
    credential_path <- tempfile(pattern = "lem-google-service-account-", fileext = ".json")
    writeLines(sa_json, credential_path, useBytes = TRUE)
    Sys.chmod(credential_path, mode = "0600")
    cleanup <- TRUE
  }

  on.exit(if (cleanup && file.exists(credential_path)) unlink(credential_path), add = TRUE)
  googlesheets4::gs4_auth(path = credential_path, cache = FALSE)
  invisible(TRUE)
}

sheet_decision_tab <- function() Sys.getenv("LEM_GOOGLE_DECISIONS_TAB", unset = "decisions")

sheet_id_from_env <- function() {
  id <- Sys.getenv("LEM_GOOGLE_SHEET_ID", unset = "")
  if (!nzchar(id)) stop("LEM_GOOGLE_SHEET_ID is not set", call.=FALSE)
  id
}

normalise_sheet_rows <- function(df) {
  if (!nrow(df)) return(list())
  required <- c("decision_id","review_case_id","decision","rationale","reviewer","resolved_at_utc","queue_sha256","supersedes_decision_id")
  for (nm in required) if (!nm %in% names(df)) df[[nm]] <- ""
  lapply(seq_len(nrow(df)), function(i) {
    as.list(vapply(df[i, required, drop=FALSE], function(x) {
      y <- as.character(x[[1L]])
      if (is.na(y)) "" else y
    }, character(1)))
  })
}

read_sheet_decision_log <- function() {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- sheet_decision_tab()
  x <- tryCatch(
    googlesheets4::read_sheet(ss, sheet = tab, col_types = "c"),
    error = function(e) {
      if (grepl("Worksheet|sheet|range", conditionMessage(e), ignore.case=TRUE)) return(data.frame())
      stop(e)
    }
  )
  normalise_sheet_rows(x)
}

active_sheet_decisions <- function() {
  rows <- read_sheet_decision_log()
  if (!length(rows)) return(list())
  by_case <- split(rows, vapply(rows, function(x) x$review_case_id, character(1)))
  lapply(by_case, function(xs) {
    ord <- order(vapply(xs, function(x) x$resolved_at_utc, character(1)), decreasing = TRUE)
    xs[[ord[[1L]]]]
  })
}

append_sheet_decision <- function(decision, prior_decision = NULL) {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- sheet_decision_tab()

  decision_id <- paste0("dec-", digest::digest(
    paste(decision$review_case_id, decision$resolved_at_utc, decision$decision, sep="|"),
    algo="sha256", serialize=FALSE
  ))
  supersedes <- if (is.null(prior_decision)) "" else as.character(prior_decision$decision_id %||% "")

  row <- data.frame(
    decision_id = decision_id,
    review_case_id = as.character(decision$review_case_id),
    decision = as.character(decision$decision),
    rationale = as.character(decision$rationale),
    reviewer = as.character(decision$reviewer),
    resolved_at_utc = as.character(decision$resolved_at_utc),
    queue_sha256 = as.character(decision$queue_sha256),
    supersedes_decision_id = supersedes,
    stringsAsFactors = FALSE
  )

  googlesheets4::sheet_append(ss, data = row, sheet = tab)

  # Verify persistence with one post-write read. Avoid repeated authentication
  # and full-log reads inside a single adjudication action.
  x <- googlesheets4::read_sheet(ss, sheet = tab, col_types = "c")
  hits <- x[as.character(x$decision_id) == decision_id, , drop = FALSE]
  if (nrow(hits) != 1L) {
    stop("Google Sheets write could not be verified; case remains unsaved", call.=FALSE)
  }

  verify <- normalise_sheet_rows(hits)[[1L]]
  invisible(verify)
}

export_active_sheet_w01_decisions <- function(output_path) {
  active <- active_sheet_decisions()
  if (!length(active)) stop("No Google Sheets decisions found", call.=FALSE)
  ds <- lapply(active, function(x) list(
    review_case_id = x$review_case_id,
    decision = x$decision,
    rationale = x$rationale,
    reviewer = x$reviewer,
    resolved_at_utc = x$resolved_at_utc,
    queue_sha256 = x$queue_sha256
  ))
  dir.create(dirname(output_path), recursive=TRUE, showWarnings=FALSE)
  con <- file(output_path, "wt", encoding="UTF-8")
  on.exit(close(con), add=TRUE)
  for (d in ds) writeLines(jsonlite::toJSON(d, auto_unbox=TRUE, null="null", na="null"), con, useBytes=TRUE)
  invisible(output_path)
}


read_sheet_w01_queue <- function(
  tab = Sys.getenv("LEM_W01_QUEUE_TAB", unset = "")
) {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()

  if (!nzchar(tab)) {
    tabs <- googlesheets4::sheet_names(ss)
    tab <- if ("queue_w01_active" %in% tabs) "queue_w01_active" else "queue_w01_legacy_730"
  }

  x <- googlesheets4::read_sheet(ss, sheet = tab, col_types = "c")
  required <- c("batch_id","queue_sha256","case_index","review_case_id","case_json")
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("W01 queue tab missing field(s): ", paste(missing, collapse=", "), call.=FALSE)
  if (!nrow(x)) stop("W01 queue tab is empty", call.=FALSE)

  ord <- order(as.integer(x$case_index))
  x <- x[ord, required, drop=FALSE]

  hashes <- unique(x$queue_sha256)
  batches <- unique(x$batch_id)
  if (length(hashes) != 1L || !nzchar(hashes[[1L]])) stop("W01 queue has invalid queue_sha256", call.=FALSE)
  if (length(batches) != 1L || !nzchar(batches[[1L]])) stop("W01 queue has invalid batch_id", call.=FALSE)
  if (anyDuplicated(x$review_case_id)) stop("W01 queue contains duplicate review_case_id", call.=FALSE)

  reconstructed <- paste0(paste(x$case_json, collapse = "\n"), "\n")
  actual_sha <- digest::digest(reconstructed, algo = "sha256", serialize = FALSE)
  if (!identical(actual_sha, hashes[[1L]])) {
    stop("W01 queue SHA-256 validation failed", call.=FALSE)
  }

  cases <- lapply(x$case_json, jsonlite::fromJSON, simplifyVector = FALSE)
  ids <- vapply(cases, function(z) as.character(z$review_case_id %||% ""), character(1))
  if (!identical(ids, x$review_case_id)) stop("W01 queue case IDs do not match stored metadata", call.=FALSE)

  list(
    batch_id = batches[[1L]],
    queue_sha256 = hashes[[1L]],
    cases = cases
  )
}


w02_decision_tab <- function() {
  Sys.getenv("LEM_W02_DECISION_TAB", unset = "decisions_w02")
}

read_sheet_w02_queue <- function(
  tab = Sys.getenv("LEM_W02_QUEUE_TAB", unset = "queue_w02_active")
) {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tabs <- googlesheets4::sheet_names(ss)
  if (!tab %in% tabs) return(NULL)

  x <- googlesheets4::read_sheet(ss, sheet = tab, col_types = "c")
  required <- c("batch_id","queue_sha256","case_index","review_case_id","case_json")
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("W02 queue tab missing field(s): ", paste(missing, collapse=", "), call.=FALSE)
  if (!nrow(x)) return(NULL)

  ord <- order(as.integer(x$case_index))
  x <- x[ord, required, drop=FALSE]
  hashes <- unique(x$queue_sha256)
  batches <- unique(x$batch_id)
  if (length(hashes) != 1L || !nzchar(hashes[[1L]])) stop("W02 queue has invalid queue_sha256", call.=FALSE)
  if (length(batches) != 1L || !nzchar(batches[[1L]])) stop("W02 queue has invalid batch_id", call.=FALSE)
  if (anyDuplicated(x$review_case_id)) stop("W02 queue contains duplicate review_case_id", call.=FALSE)

  reconstructed <- paste0(paste(x$case_json, collapse = "\n"), "\n")
  actual_sha <- digest::digest(reconstructed, algo = "sha256", serialize = FALSE)
  if (!identical(actual_sha, hashes[[1L]])) stop("W02 queue SHA-256 validation failed", call.=FALSE)

  cases <- lapply(x$case_json, jsonlite::fromJSON, simplifyVector = FALSE)
  ids <- vapply(cases, function(z) as.character(z$review_case_id %||% ""), character(1))
  if (!identical(ids, x$review_case_id)) stop("W02 queue case IDs do not match stored metadata", call.=FALSE)

  list(
    batch_id = batches[[1L]],
    queue_sha256 = hashes[[1L]],
    cases = cases
  )
}

normalise_w02_sheet_rows <- function(x) {
  if (is.null(x) || !nrow(x)) return(list())
  lapply(seq_len(nrow(x)), function(i) {
    as.list(x[i, , drop=FALSE])
  })
}

read_sheet_w02_decision_log <- function() {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- w02_decision_tab()
  tabs <- googlesheets4::sheet_names(ss)
  if (!tab %in% tabs) return(list())
  x <- googlesheets4::read_sheet(ss, sheet = tab, col_types = "c")
  normalise_w02_sheet_rows(x)
}

active_sheet_w02_decisions <- function() {
  xs <- read_sheet_w02_decision_log()
  if (!length(xs)) return(list())

  resolved <- vapply(xs, function(x) as.character(x$resolved_at_utc %||% ""), character(1))
  ord <- order(resolved, seq_along(xs), decreasing = TRUE)
  xs <- xs[ord]
  ids <- vapply(xs, function(x) as.character(x$review_case_id %||% ""), character(1))
  xs[!duplicated(ids)]
}

append_sheet_w02_decision <- function(decision, prior_decision = NULL) {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- w02_decision_tab()
  tabs <- googlesheets4::sheet_names(ss)

  required_cols <- c(
    "decision_id","review_case_id","record_id","provider","field","reason",
    "decision","note","reviewer","resolved_at_utc","queue_sha256",
    "supersedes_decision_id"
  )

  if (!tab %in% tabs) {
    googlesheets4::sheet_add(ss, sheet = tab)
    empty <- as.data.frame(setNames(replicate(length(required_cols), character(), simplify=FALSE), required_cols))
    googlesheets4::sheet_write(empty, ss = ss, sheet = tab)
  }

  decision_id <- paste0("w02-dec-", digest::digest(
    paste(decision$review_case_id, decision$resolved_at_utc, decision$decision, sep="|"),
    algo="sha256", serialize=FALSE
  ))
  supersedes <- if (is.null(prior_decision)) "" else as.character(prior_decision$decision_id %||% "")

  row <- data.frame(
    decision_id = decision_id,
    review_case_id = as.character(decision$review_case_id),
    record_id = as.character(decision$record_id),
    provider = as.character(decision$provider),
    field = as.character(decision$field),
    reason = as.character(decision$reason),
    decision = as.character(decision$decision),
    note = as.character(decision$note %||% ""),
    reviewer = as.character(decision$reviewer),
    resolved_at_utc = as.character(decision$resolved_at_utc),
    queue_sha256 = as.character(decision$queue_sha256),
    supersedes_decision_id = supersedes,
    stringsAsFactors = FALSE
  )

  googlesheets4::sheet_append(ss, data = row, sheet = tab)
  x <- googlesheets4::read_sheet(ss, sheet = tab, col_types = "c")
  hits <- x[as.character(x$decision_id) == decision_id, , drop = FALSE]
  if (nrow(hits) != 1L) stop("W02 Google Sheets write could not be verified", call.=FALSE)
  as.list(hits[1, , drop=FALSE])
}
