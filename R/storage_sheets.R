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

append_sheet_decision <- function(decision) {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- sheet_decision_tab()

  current <- active_sheet_decisions()
  prior <- current[[as.character(decision$review_case_id)]]
  decision_id <- paste0("dec-", digest::digest(
    paste(decision$review_case_id, decision$resolved_at_utc, decision$decision, sep="|"),
    algo="sha256", serialize=FALSE
  ))
  supersedes <- if (is.null(prior)) "" else as.character(prior$decision_id)

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

  verify <- active_sheet_decisions()[[as.character(decision$review_case_id)]]
  if (is.null(verify) || !identical(as.character(verify$decision_id), decision_id)) {
    stop("Google Sheets write could not be verified; case remains unsaved", call.=FALSE)
  }
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
  tab = Sys.getenv("LEM_W01_QUEUE_TAB", unset = "queue_w01_legacy_730")
) {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()

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
