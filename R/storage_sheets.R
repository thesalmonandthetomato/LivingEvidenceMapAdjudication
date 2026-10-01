suppressPackageStartupMessages({
  library(googlesheets4)
  library(jsonlite)
})

gs4_auth_from_env <- function() {
  sa_json <- Sys.getenv("LEM_GOOGLE_SERVICE_ACCOUNT_JSON", unset = "")
  if (!nzchar(sa_json)) stop("LEM_GOOGLE_SERVICE_ACCOUNT_JSON is not set", call.=FALSE)
  googlesheets4::gs4_auth(path = sa_json, cache = FALSE)
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
