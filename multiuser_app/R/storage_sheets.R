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


batch_status_tab <- function() {
  Sys.getenv("LEM_BATCH_STATUS_TAB", unset = "workflow_batch_status")
}

read_batch_status_log <- function() {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- batch_status_tab()
  tabs <- googlesheets4::sheet_names(ss)
  if(!tab %in% tabs) return(data.frame())
  x <- googlesheets4::read_sheet(ss,sheet=tab,col_types="c")
  if(!nrow(x)) return(data.frame())
  required <- c("event_id","stage","batch_id","queue_sha256","status","event_at_utc","workflow_run_id","source_run_id","output_sha256","message")
  missing <- setdiff(required,names(x))
  if(length(missing)) stop("Batch-status tab missing field(s): ",paste(missing,collapse=", "),call.=FALSE)
  x[,required,drop=FALSE]
}

latest_batch_status <- function(stage,batch_id,queue_sha256) {
  x <- read_batch_status_log()
  if(!nrow(x)) return("")
  hit <- x[
    as.character(x$stage)==as.character(stage) &
    as.character(x$batch_id)==as.character(batch_id) &
    tolower(as.character(x$queue_sha256))==tolower(as.character(queue_sha256)),
    ,drop=FALSE
  ]
  if(!nrow(hit)) return("")
  as.character(hit$status[[nrow(hit)]])
}

append_batch_status <- function(stage,batch_id,queue_sha256,status,workflow_run_id="",source_run_id="",output_sha256="",message="") {
  if(!stage %in% c("01","02","04","08")) stop("Invalid batch-status stage",call.=FALSE)
  if(!status %in% c("published","review_complete","consumed")) stop("Invalid batch status",call.=FALSE)
  if(!nzchar(as.character(batch_id))) stop("Batch ID is required",call.=FALSE)
  if(!grepl("^[0-9a-fA-F]{64}$",as.character(queue_sha256))) stop("queue_sha256 must be SHA-256",call.=FALSE)

  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- batch_status_tab()
  tabs <- googlesheets4::sheet_names(ss)
  cols <- c("event_id","stage","batch_id","queue_sha256","status","event_at_utc","workflow_run_id","source_run_id","output_sha256","message")
  if(!tab %in% tabs){
    googlesheets4::sheet_add(ss,sheet=tab)
    empty <- as.data.frame(setNames(replicate(length(cols),character(),simplify=FALSE),cols),stringsAsFactors=FALSE)
    googlesheets4::sheet_write(empty,ss=ss,sheet=tab)
  }

  current <- latest_batch_status(stage,batch_id,queue_sha256)
  if(status=="review_complete" && nzchar(current) && !current %in% c("published","review_complete")) {
    stop("Cannot mark review_complete from status ",current,call.=FALSE)
  }
  if(status=="consumed" && !current %in% c("review_complete","consumed")) {
    stop("Cannot mark consumed before review_complete",call.=FALSE)
  }
  if(identical(current,status)) return(invisible(current))

  now <- format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
  event_id <- paste0("batch-status-",substr(digest::digest(
    paste(stage,batch_id,queue_sha256,status,now,sep="|"),
    algo="sha256",serialize=FALSE
  ),1,24))
  row <- data.frame(
    event_id=event_id,stage=stage,batch_id=as.character(batch_id),
    queue_sha256=tolower(as.character(queue_sha256)),status=status,event_at_utc=now,
    workflow_run_id=as.character(workflow_run_id),source_run_id=as.character(source_run_id),
    output_sha256=tolower(as.character(output_sha256)),message=as.character(message),
    stringsAsFactors=FALSE
  )
  googlesheets4::sheet_append(ss,data=row,sheet=tab)
  verify <- googlesheets4::read_sheet(ss,sheet=tab,col_types="c")
  if(sum(as.character(verify$event_id)==event_id)!=1L) stop("Batch-status write verification failed",call.=FALSE)
  invisible(status)
}

batch_is_consumed <- function(stage,batch_id,queue_sha256) {
  identical(latest_batch_status(stage,batch_id,queue_sha256),"consumed")
}

ensure_w01_decision_tab <- function() {
  ss <- sheet_id_from_env()
  tab <- sheet_decision_tab()
  required_cols <- c(
    "decision_id","review_case_id","decision","rationale","reviewer",
    "resolved_at_utc","queue_sha256","supersedes_decision_id"
  )

  tabs <- googlesheets4::sheet_names(ss)
  if (!tab %in% tabs) {
    googlesheets4::sheet_add(ss, sheet = tab)
    empty <- as.data.frame(
      setNames(replicate(length(required_cols), character(), simplify = FALSE), required_cols),
      stringsAsFactors = FALSE
    )
    googlesheets4::sheet_write(empty, ss = ss, sheet = tab)
  }

  invisible(TRUE)
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
  ensure_w01_decision_tab()
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
  ensure_w01_decision_tab()
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
    if ("queue_w01_active" %in% tabs) {
      tab <- "queue_w01_active"
    } else if ("queue_w01_legacy_730" %in% tabs) {
      tab <- "queue_w01_legacy_730"
    } else {
      return(NULL)
    }
  }

  tabs <- googlesheets4::sheet_names(ss)
  if (!tab %in% tabs) return(NULL)

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
  status <- latest_batch_status("01",batches[[1L]],hashes[[1L]])
  if(identical(status,"consumed")) return(NULL)

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
    cases = cases,
    batch_status = status
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
  status <- latest_batch_status("02",batches[[1L]],hashes[[1L]])
  if(identical(status,"consumed")) return(NULL)

  reconstructed <- paste0(paste(x$case_json, collapse = "\n"), "\n")
  actual_sha <- digest::digest(reconstructed, algo = "sha256", serialize = FALSE)
  if (!identical(actual_sha, hashes[[1L]])) stop("W02 queue SHA-256 validation failed", call.=FALSE)

  cases <- lapply(x$case_json, jsonlite::fromJSON, simplifyVector = FALSE)
  ids <- vapply(cases, function(z) as.character(z$review_case_id %||% ""), character(1))
  if (!identical(ids, x$review_case_id)) stop("W02 queue case IDs do not match stored metadata", call.=FALSE)

  list(
    batch_id = batches[[1L]],
    queue_sha256 = hashes[[1L]],
    cases = cases,
    batch_status = status
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


w02_resume_request_tab <- function() {
  Sys.getenv("LEM_W02_RESUME_REQUEST_TAB", unset = "w02_resume_requests")
}

w02_resume_request_exists <- function(queue_sha256, source_run_id) {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- w02_resume_request_tab()
  tabs <- googlesheets4::sheet_names(ss)
  if (!tab %in% tabs) return(FALSE)

  x <- googlesheets4::read_sheet(ss, sheet = tab, col_types = "c")
  if (!nrow(x)) return(FALSE)
  required <- c("queue_sha256","source_run_id","status")
  if (!all(required %in% names(x))) stop("W02 resume-request tab is malformed", call.=FALSE)

  any(
    as.character(x$queue_sha256) == as.character(queue_sha256) &
    as.character(x$source_run_id) == as.character(source_run_id) &
    as.character(x$status) %in% c("dispatching","dispatched")
  )
}

append_w02_resume_request <- function(queue_sha256, source_run_id, status, message = "") {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- w02_resume_request_tab()
  tabs <- googlesheets4::sheet_names(ss)

  required_cols <- c(
    "request_id","queue_sha256","source_run_id","status",
    "requested_at_utc","message"
  )

  if (!tab %in% tabs) {
    googlesheets4::sheet_add(ss, sheet = tab)
    empty <- as.data.frame(setNames(replicate(length(required_cols), character(), simplify=FALSE), required_cols))
    googlesheets4::sheet_write(empty, ss = ss, sheet = tab)
  }

  row <- data.frame(
    request_id = paste0("w02-resume-", digest::digest(
      paste(queue_sha256, source_run_id, status, format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%OS6Z"), sep="|"),
      algo="sha256", serialize=FALSE
    )),
    queue_sha256 = as.character(queue_sha256),
    source_run_id = as.character(source_run_id),
    status = as.character(status),
    requested_at_utc = format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"),
    message = as.character(message),
    stringsAsFactors = FALSE
  )
  googlesheets4::sheet_append(ss, data = row, sheet = tab)
  invisible(row)
}


w04_decision_tab <- function() {
  Sys.getenv("LEM_W04_DECISION_TAB", unset = "decisions_w04_validation")
}

w04_resolution_decision_tab <- function() {
  Sys.getenv("LEM_W04_RESOLUTION_DECISION_TAB", unset = "decisions_w04_resolution")
}

w04_conflict_decision_tab <- function() {
  Sys.getenv("LEM_W04_CONFLICT_DECISION_TAB", unset = "decisions_w04_conflict")
}

read_sheet_w04_queue_from_tab <- function(tab) {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tabs <- googlesheets4::sheet_names(ss)
  if (!tab %in% tabs) return(NULL)

  x <- googlesheets4::read_sheet(ss, sheet = tab, col_types = "c")
  required <- c("batch_id","queue_sha256","case_index","review_case_id","case_json")
  missing <- setdiff(required,names(x))
  if(length(missing)) stop("W04 queue tab missing field(s): ",paste(missing,collapse=", "),call.=FALSE)
  if(!nrow(x)) return(NULL)

  x <- x[order(as.integer(x$case_index)),,drop=FALSE]
  include_terms <- character()
  exclude_terms <- character()
  if("highlight_include_json" %in% names(x) && nzchar(as.character(x$highlight_include_json[[1L]] %||% ""))) {
    include_terms <- as.character(jsonlite::fromJSON(x$highlight_include_json[[1L]]))
  }
  if("highlight_exclude_json" %in% names(x) && nzchar(as.character(x$highlight_exclude_json[[1L]] %||% ""))) {
    exclude_terms <- as.character(jsonlite::fromJSON(x$highlight_exclude_json[[1L]]))
  }
  review_mode <- if("review_mode" %in% names(x)) as.character(x$review_mode[[1L]] %||% "") else ""
  source_run_id <- if("source_run_id" %in% names(x)) as.character(x$source_run_id[[1L]] %||% "") else ""

  core <- x[,required,drop=FALSE]
  hashes <- unique(core$queue_sha256)
  batches <- unique(core$batch_id)
  if(length(hashes)!=1L || !nzchar(hashes[[1L]])) stop("W04 queue has invalid queue_sha256",call.=FALSE)
  if(length(batches)!=1L || !nzchar(batches[[1L]])) stop("W04 queue has invalid batch_id",call.=FALSE)
  if(anyDuplicated(core$review_case_id)) stop("W04 queue contains duplicate review_case_id",call.=FALSE)
  status <- latest_batch_status("04",batches[[1L]],hashes[[1L]])
  if(identical(status,"consumed")) return(NULL)

  reconstructed <- paste0(paste(core$case_json,collapse="\n"),"\n")
  actual_sha <- digest::digest(reconstructed,algo="sha256",serialize=FALSE)
  if(!identical(actual_sha,hashes[[1L]])) stop("W04 queue SHA-256 validation failed",call.=FALSE)

  cases <- lapply(core$case_json,jsonlite::fromJSON,simplifyVector=FALSE)
  ids <- vapply(cases,function(z)as.character(z$review_case_id %||% ""),character(1))
  if(!identical(ids,core$review_case_id)) stop("W04 queue case IDs do not match stored metadata",call.=FALSE)

  list(
    batch_id=batches[[1L]],
    queue_sha256=hashes[[1L]],
    cases=cases,
    highlight_include=include_terms,
    highlight_exclude=exclude_terms,
    review_mode=review_mode,
    source_run_id=source_run_id,
    batch_status=status
  )
}

read_sheet_w04_queue <- function(tab = Sys.getenv("LEM_W04_QUEUE_TAB", unset = "queue_w04_validation_active")) {
  read_sheet_w04_queue_from_tab(tab)
}

read_sheet_w04_resolution_queue <- function(tab = Sys.getenv("LEM_W04_RESOLUTION_QUEUE_TAB", unset = "queue_w04_resolution_active")) {
  read_sheet_w04_queue_from_tab(tab)
}

read_sheet_w04_conflict_queue <- function(tab = Sys.getenv("LEM_W04_CONFLICT_QUEUE_TAB", unset = "queue_w04_conflict_active")) {
  read_sheet_w04_queue_from_tab(tab)
}

read_sheet_w04_decision_log_from_tab <- function(tab) {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tabs <- googlesheets4::sheet_names(ss)
  if(!tab %in% tabs) return(list())
  x <- googlesheets4::read_sheet(ss,sheet=tab,col_types="c")
  if(!nrow(x)) return(list())
  lapply(seq_len(nrow(x)),function(i)as.list(x[i,,drop=FALSE]))
}

active_w04_decisions_from_tab <- function(tab) {
  xs <- read_sheet_w04_decision_log_from_tab(tab)
  if(!length(xs)) return(list())
  resolved <- vapply(xs,function(x)as.character(x$resolved_at_utc %||% ""),character(1))
  ord <- order(resolved,seq_along(xs),decreasing=TRUE)
  xs <- xs[ord]
  ids <- vapply(xs,function(x)as.character(x$review_case_id %||% ""),character(1))
  xs[!duplicated(ids)]
}

read_sheet_w04_decision_log <- function() read_sheet_w04_decision_log_from_tab(w04_decision_tab())
active_sheet_w04_decisions <- function() active_w04_decisions_from_tab(w04_decision_tab())
active_sheet_w04_resolution_decisions <- function() active_w04_decisions_from_tab(w04_resolution_decision_tab())
active_sheet_w04_conflict_decisions <- function() active_w04_decisions_from_tab(w04_conflict_decision_tab())

append_w04_decision_to_tab <- function(decision, prior_decision=NULL, tab, prefix="w04-dec-") {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tabs <- googlesheets4::sheet_names(ss)

  required_cols <- c(
    "decision_id","review_case_id","record_id","decision","rationale",
    "reviewer","resolved_at_utc","queue_sha256","supersedes_decision_id"
  )
  if(!tab %in% tabs) {
    googlesheets4::sheet_add(ss,sheet=tab)
    empty <- as.data.frame(setNames(replicate(length(required_cols),character(),simplify=FALSE),required_cols))
    googlesheets4::sheet_write(empty,ss=ss,sheet=tab)
  }

  decision_id <- paste0(prefix,digest::digest(
    paste(decision$review_case_id,decision$resolved_at_utc,decision$decision,sep="|"),
    algo="sha256",serialize=FALSE
  ))
  supersedes <- if(is.null(prior_decision)) "" else as.character(prior_decision$decision_id %||% "")
  row <- data.frame(
    decision_id=decision_id,
    review_case_id=as.character(decision$review_case_id),
    record_id=as.character(decision$record_id),
    decision=as.character(decision$decision),
    rationale=as.character(decision$rationale %||% ""),
    reviewer=as.character(decision$reviewer),
    resolved_at_utc=as.character(decision$resolved_at_utc),
    queue_sha256=as.character(decision$queue_sha256),
    supersedes_decision_id=supersedes,
    stringsAsFactors=FALSE
  )
  googlesheets4::sheet_append(ss,data=row,sheet=tab)
  x <- googlesheets4::read_sheet(ss,sheet=tab,col_types="c")
  hits <- x[as.character(x$decision_id)==decision_id,,drop=FALSE]
  if(nrow(hits)!=1L) stop("W04 Google Sheets write could not be verified",call.=FALSE)
  as.list(hits[1,,drop=FALSE])
}

append_sheet_w04_decision <- function(decision, prior_decision=NULL) {
  append_w04_decision_to_tab(decision,prior_decision,w04_decision_tab(),"w04-val-dec-")
}
append_sheet_w04_resolution_decision <- function(decision, prior_decision=NULL) {
  append_w04_decision_to_tab(decision,prior_decision,w04_resolution_decision_tab(),"w04-res-dec-")
}
append_sheet_w04_conflict_decision <- function(decision, prior_decision=NULL) {
  append_w04_decision_to_tab(decision,prior_decision,w04_conflict_decision_tab(),"w04-conf-dec-")
}


w08_decision_tab <- function() {
  Sys.getenv("LEM_W08_DECISION_TAB", unset = "decisions_w08")
}

read_sheet_w08_queue <- function(
  tab = Sys.getenv("LEM_W08_QUEUE_TAB", unset = "queue_w08_active")
) {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tabs <- googlesheets4::sheet_names(ss)
  if (!tab %in% tabs) return(NULL)

  x <- googlesheets4::read_sheet(ss, sheet = tab, col_types = "c")
  required <- c("batch_id","queue_sha256","case_index","record_id","case_json")
  missing <- setdiff(required,names(x))
  if(length(missing)) stop("W08 queue tab missing field(s): ",paste(missing,collapse=", "),call.=FALSE)
  if(!nrow(x)) return(NULL)

  x <- x[order(as.integer(x$case_index)),,drop=FALSE]
  core <- x[,required,drop=FALSE]
  source_run_id <- if("source_run_id" %in% names(x)) as.character(x$source_run_id[[1L]] %||% "") else ""
  hashes <- unique(core$queue_sha256)
  batches <- unique(core$batch_id)
  if(length(hashes)!=1L || !nzchar(hashes[[1L]])) stop("W08 queue has invalid queue_sha256",call.=FALSE)
  if(length(batches)!=1L || !nzchar(batches[[1L]])) stop("W08 queue has invalid batch_id",call.=FALSE)
  if(anyDuplicated(core$record_id)) stop("W08 queue contains duplicate record_id",call.=FALSE)
  status <- latest_batch_status("08",batches[[1L]],hashes[[1L]])
  if(identical(status,"consumed")) return(NULL)

  reconstructed <- paste0(paste(core$case_json,collapse="\n"),"\n")
  actual_sha <- digest::digest(reconstructed,algo="sha256",serialize=FALSE)
  if(!identical(actual_sha,hashes[[1L]])) stop("W08 queue SHA-256 validation failed",call.=FALSE)

  cases <- lapply(core$case_json,jsonlite::fromJSON,simplifyVector=FALSE)
  ids <- vapply(cases,function(z)as.character(z$record_id %||% ""),character(1))
  if(!identical(ids,core$record_id)) stop("W08 queue record IDs do not match stored metadata",call.=FALSE)

  case_sha <- setNames(
    vapply(core$case_json,function(z)digest::digest(z,algo="sha256",serialize=FALSE),character(1)),
    core$record_id
  )

  species_options <- character()
  topic_options <- list()
  if("species_options_json" %in% names(x) && nzchar(as.character(x$species_options_json[[1L]] %||% ""))) {
    species_options <- as.character(jsonlite::fromJSON(x$species_options_json[[1L]]))
  }
  if("topic_options_json" %in% names(x) && nzchar(as.character(x$topic_options_json[[1L]] %||% ""))) {
    topic_options <- jsonlite::fromJSON(x$topic_options_json[[1L]],simplifyVector=FALSE)
  }

  list(
    batch_id=batches[[1L]],
    queue_sha256=hashes[[1L]],
    cases=cases,
    case_sha256=case_sha,
    species_options=species_options,
    topic_options=topic_options,
    source_run_id=source_run_id,
    batch_status=status
  )
}

read_sheet_w08_decision_log <- function() {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- w08_decision_tab()
  tabs <- googlesheets4::sheet_names(ss)
  if(!tab %in% tabs) return(list())
  x <- googlesheets4::read_sheet(ss,sheet=tab,col_types="c")
  if(!nrow(x)) return(list())
  lapply(seq_len(nrow(x)),function(i)as.list(x[i,,drop=FALSE]))
}

active_sheet_w08_decisions <- function() {
  rows <- read_sheet_w08_decision_log()
  if(!length(rows)) return(list())
  ord <- order(vapply(rows,function(x)as.character(x$resolved_at_utc %||% ""),character(1)),decreasing=TRUE)
  rows <- rows[ord]
  ids <- vapply(rows,function(x)as.character(x$record_id %||% ""),character(1))
  rows[!duplicated(ids)]
}

append_sheet_w08_decision <- function(decision, prior_decision=NULL) {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- w08_decision_tab()
  tabs <- googlesheets4::sheet_names(ss)
  cols <- c(
    "decision_id","record_id","queue_sha256","record_case_sha256",
    "issue_decisions_json","reviewer","resolved_at_utc","supersedes_decision_id"
  )

  if(!tab %in% tabs) {
    googlesheets4::sheet_add(ss,sheet=tab)
    empty <- as.data.frame(setNames(replicate(length(cols),character(),simplify=FALSE),cols),stringsAsFactors=FALSE)
    googlesheets4::sheet_write(empty,ss=ss,sheet=tab)
  }

  prior_id <- as.character(prior_decision$decision_id %||% "")
  decision_id <- paste0(
    "w08-rec-dec-",
    substr(digest::digest(
      paste(
        decision$record_id,
        decision$queue_sha256,
        decision$resolved_at_utc,
        decision$issue_decisions_json,
        sep="|"
      ),
      algo="sha256",serialize=FALSE
    ),1,24)
  )

  row <- data.frame(
    decision_id=decision_id,
    record_id=as.character(decision$record_id),
    queue_sha256=as.character(decision$queue_sha256),
    record_case_sha256=as.character(decision$record_case_sha256),
    issue_decisions_json=as.character(decision$issue_decisions_json),
    reviewer=as.character(decision$reviewer %||% ""),
    resolved_at_utc=as.character(decision$resolved_at_utc),
    supersedes_decision_id=prior_id,
    stringsAsFactors=FALSE
  )
  googlesheets4::sheet_append(ss,data=row,sheet=tab)

  verify <- googlesheets4::read_sheet(ss,sheet=tab,col_types="c")
  hit <- verify[verify$decision_id==decision_id,,drop=FALSE]
  if(nrow(hit)!=1L) stop("W08 decision write verification failed",call.=FALSE)
  as.list(hit[1,,drop=FALSE])
}


pipeline_status_tab <- function() {
  Sys.getenv("LEM_PIPELINE_STATUS_TAB", unset = "pipeline_run_status")
}

read_latest_pipeline_status <- function() {
  repo_url <- Sys.getenv(
    "LEM_CURRENT_RUN_STATUS_URL",
    unset = "https://raw.githubusercontent.com/thesalmonandthetomato/LivingEvidenceMap/workflow01-final-architecture/docs/current_run/current_run_status.json"
  )
  current <- tryCatch(
    jsonlite::fromJSON(repo_url, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (!is.null(current) && identical(as.character(current$schema), "living-evidence-map-current-run-status-v1")) {
    val <- function(x) if (is.null(x) || !length(x)) "" else as.character(x[[1L]])
    return(list(
      update_id = val(current$update_id),
      event_at_utc = val(current$last_updated_at_utc),
      stage = val(current$progress$current_stage),
      workflow_run_id = val(current$workflow_runs[[val(current$progress$current_stage)]]),
      last_search_date = val(current$search$search_date),
      canonical_existing = val(current$baseline$canonical_records),
      search_results_total = val(current$counts$search_results),
      deduplicated_records = val(current$counts$deduplicated_records),
      enriched_records = val(current$counts$enriched_records),
      retracted_records = val(current$counts$retraction_exclusions),
      screened_include = val(current$counts$screened_include),
      screened_exclude = val(current$counts$screened_exclude),
      geography_with = val(current$counts$geography$with),
      geography_without = val(current$counts$geography$without),
      topic_with = val(current$counts$topics$with),
      topic_without = val(current$counts$topics$without),
      completed_through = val(current$progress$completed_through),
      active_workflow = val(current$progress$active_position),
      status_label = val(current$progress$status_label)
    ))
  }

  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- pipeline_status_tab()
  tabs <- googlesheets4::sheet_names(ss)
  if(!tab %in% tabs) return(NULL)
  x <- googlesheets4::read_sheet(ss,sheet=tab,col_types="c")
  if(!nrow(x)) return(NULL)

  base <- c(
    "event_id","update_id","event_at_utc","stage","workflow_run_id",
    "last_search_date","canonical_existing","search_results_total",
    "deduplicated_records","enriched_records","retracted_records",
    "screened_include","screened_exclude",
    "completed_through","active_workflow","status_label"
  )
  miss <- setdiff(base,names(x))
  if(length(miss)) stop("pipeline_run_status missing field(s): ",paste(miss,collapse=", "),call.=FALSE)

  if(all(c("geography_with","geography_without","topic_with","topic_without") %in% names(x))) {
    cols <- c(base[1:13],"geography_with","geography_without","topic_with","topic_without",base[14:16])
    return(x[nrow(x),cols,drop=FALSE] |> as.list())
  }

  if(all(c("geography_coded","topic_coded") %in% names(x))) {
    z <- x[nrow(x),base,drop=FALSE] |> as.list()
    z$geography_with <- as.character(x$geography_coded[[nrow(x)]])
    z$geography_without <- ""
    z$topic_with <- as.character(x$topic_coded[[nrow(x)]])
    z$topic_without <- ""
    return(z)
  }

  NULL
}


user_registry_tab <- function() {
  Sys.getenv("LEM_GOOGLE_USERS_TAB", unset = "users")
}

ensure_sheet_user_registry <- function() {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- user_registry_tab()
  tabs <- googlesheets4::sheet_names(ss)
  cols <- ADJUDICATION_SCHEMA$users

  if (!tab %in% tabs) {
    googlesheets4::sheet_add(ss, sheet = tab)
    empty <- as.data.frame(
      setNames(replicate(length(cols), character(), simplify = FALSE), cols),
      stringsAsFactors = FALSE
    )
    googlesheets4::sheet_write(empty, ss = ss, sheet = tab)
  }
  invisible(TRUE)
}

read_sheet_users <- function() {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- user_registry_tab()
  tabs <- googlesheets4::sheet_names(ss)

  # Phase 1A is deliberately non-invasive: absence of the user tab is an
  # empty registry, not a reason to create or mutate the spreadsheet.
  if (!tab %in% tabs) return(list())

  x <- googlesheets4::read_sheet(ss, sheet = tab, col_types = "c")
  if (!nrow(x)) return(list())

  missing <- setdiff(ADJUDICATION_SCHEMA$users, names(x))
  if (length(missing)) {
    stop("User registry tab missing field(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }

  users <- lapply(seq_len(nrow(x)), function(i) {
    normalise_user_row(as.list(x[i, ADJUDICATION_SCHEMA$users, drop = FALSE]))
  })
  validate_user_registry(users)
  users
}
