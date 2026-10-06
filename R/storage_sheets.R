suppressPackageStartupMessages({
  library(googlesheets4)
  library(jsonlite)
})

.lem_gs4_cache <- new.env(parent = emptyenv())
.lem_gs4_cache$auth_fingerprint <- ""
.lem_gs4_cache$credential_path <- ""
.lem_gs4_cache$sheet_names <- list()
.lem_gs4_cache$sheet_names_at <- list()
reg.finalizer(
  .lem_gs4_cache,
  function(e) {
    path <- e$credential_path
    if (!is.null(path) && nzchar(as.character(path)) && file.exists(as.character(path))) {
      unlink(as.character(path))
    }
  },
  onexit = TRUE
)

gs4_auth_from_env <- function(force = FALSE) {
  sa_json <- Sys.getenv("LEM_GOOGLE_SERVICE_ACCOUNT_JSON", unset = "")
  if (!nzchar(sa_json)) stop("LEM_GOOGLE_SERVICE_ACCOUNT_JSON is not set", call.=FALSE)

  fingerprint <- digest::digest(sa_json, algo = "sha256", serialize = FALSE)
  if (
    !isTRUE(force) &&
    identical(.lem_gs4_cache$auth_fingerprint, fingerprint)
  ) {
    return(invisible(TRUE))
  }

  credential_path <- sa_json
  if (!file.exists(credential_path)) {
    parsed <- tryCatch(jsonlite::fromJSON(sa_json, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(parsed) || is.null(parsed$type) || !identical(parsed$type, "service_account")) {
      stop("LEM_GOOGLE_SERVICE_ACCOUNT_JSON is neither a readable file path nor valid service-account JSON", call.=FALSE)
    }

    cached_path <- .lem_gs4_cache$credential_path
    if (is.null(cached_path) || !nzchar(as.character(cached_path)) || !file.exists(as.character(cached_path))) {
      cached_path <- tempfile(pattern = "lem-google-service-account-", fileext = ".json")
      writeLines(sa_json, cached_path, useBytes = TRUE)
      Sys.chmod(cached_path, mode = "0600")
      .lem_gs4_cache$credential_path <- cached_path
    }
    credential_path <- as.character(cached_path)
  }

  googlesheets4::gs4_auth(path = credential_path, cache = FALSE)
  .lem_gs4_cache$auth_fingerprint <- fingerprint
  invisible(TRUE)
}

sheet_names_cache_ttl <- function() {
  ttl <- suppressWarnings(as.numeric(
    Sys.getenv("LEM_GOOGLE_SHEET_NAMES_CACHE_SECONDS", unset = "60")
  ))
  if (is.na(ttl) || ttl < 0) 60 else ttl
}

invalidate_sheet_names_cache <- function(ss = NULL) {
  if (is.null(ss)) {
    .lem_gs4_cache$sheet_names <- list()
    .lem_gs4_cache$sheet_names_at <- list()
    return(invisible(TRUE))
  }

  key <- as.character(ss)
  names_cache <- .lem_gs4_cache$sheet_names
  time_cache <- .lem_gs4_cache$sheet_names_at
  names_cache[[key]] <- NULL
  time_cache[[key]] <- NULL
  .lem_gs4_cache$sheet_names <- names_cache
  .lem_gs4_cache$sheet_names_at <- time_cache
  invisible(TRUE)
}

sheet_names_cached <- function(ss, refresh = FALSE) {
  gs4_auth_from_env()
  key <- as.character(ss)
  ttl <- sheet_names_cache_ttl()
  now <- as.numeric(Sys.time())
  names_cache <- .lem_gs4_cache$sheet_names
  time_cache <- .lem_gs4_cache$sheet_names_at
  cached <- names_cache[[key]]
  cached_at <- time_cache[[key]]

  if (
    !isTRUE(refresh) &&
    !is.null(cached) &&
    !is.null(cached_at) &&
    ttl > 0 &&
    (now - cached_at) <= ttl
  ) {
    return(cached)
  }

  tabs <- googlesheets4::sheet_names(ss)
  names_cache[[key]] <- tabs
  time_cache[[key]] <- now
  .lem_gs4_cache$sheet_names <- names_cache
  .lem_gs4_cache$sheet_names_at <- time_cache
  tabs
}

sheet_add_cached <- function(ss, sheet) {
  gs4_auth_from_env()
  out <- googlesheets4::sheet_add(ss, sheet = sheet)
  invalidate_sheet_names_cache(ss)
  invisible(out)
}

sheet_decision_tab <- function() Sys.getenv("LEM_GOOGLE_DECISIONS_TAB", unset = "decisions")

sheet_id_from_env <- function() {
  "1e9-V67-8lwPO6YBqeGJWn-SFanWSze5Gi2ToKR4bXvA"
}


batch_status_tab <- function() {
  Sys.getenv("LEM_BATCH_STATUS_TAB", unset = "workflow_batch_status")
}

read_batch_status_log <- function() {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- batch_status_tab()
  tabs <- sheet_names_cached(ss)
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
  tabs <- sheet_names_cached(ss)
  cols <- c("event_id","stage","batch_id","queue_sha256","status","event_at_utc","workflow_run_id","source_run_id","output_sha256","message")
  if(!tab %in% tabs){
    sheet_add_cached(ss, tab)
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

  tabs <- sheet_names_cached(ss)
  if (!tab %in% tabs) {
    sheet_add_cached(ss, tab)
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
  active_decision_events(
    read_sheet_decision_log(),
    case_fields = c("case_id", "review_case_id")
  )
}

append_sheet_decision <- function(decision, prior_decision = NULL) {
  gs4_auth_from_env()
  ensure_w01_decision_tab()
  ss <- sheet_id_from_env()
  tab <- sheet_decision_tab()

  # Shared-work-pool authority: once another reviewer has made a substantive
  # decision, later reviewers must not overwrite it. "uncertain" is explicitly
  # non-resolving, and the authoritative reviewer may revise their own decision.
  current_active <- active_sheet_decisions()
  case_id <- as.character(decision$review_case_id %||% "")
  user_id <- as.character(decision$reviewer %||% "")
  current_case <- Filter(
    function(x) identical(as.character(x$review_case_id %||% x$case_id %||% ""), case_id),
    current_active
  )
  if (length(current_case)) {
    current <- current_case[[1L]]
    current_decision <- tolower(trimws(as.character(current$decision %||% "")))
    current_user <- as.character(current$reviewer %||% current$user_id %||% "")
    if (
      nzchar(current_decision) &&
      !identical(current_decision, "uncertain") &&
      nzchar(current_user) &&
      !identical(current_user, user_id)
    ) {
      stop("This deduplication case has already been resolved by another reviewer", call. = FALSE)
    }
  }

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
  invisible(normalise_saved_decision_event(
    verify,
    prior_decision = prior_decision,
    case_fields = c("case_id", "review_case_id")
  ))
}

w01_repair_tab <- function() {
  Sys.getenv("LEM_W01_REPAIR_TAB", unset = "w01_data_quality_repairs")
}

ensure_w01_repair_tab <- function() {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- w01_repair_tab()
  cols <- c(
    "repair_id","review_case_id","source","source_record_id","action","value",
    "reason","reviewer","saved_at_utc","queue_sha256","supersedes_repair_id"
  )
  tabs <- sheet_names_cached(ss)
  if (!tab %in% tabs) {
    sheet_add_cached(ss, tab)
    empty <- as.data.frame(setNames(replicate(length(cols), character(), simplify=FALSE), cols), stringsAsFactors=FALSE)
    googlesheets4::sheet_write(empty, ss=ss, sheet=tab)
  }
  invisible(TRUE)
}

read_sheet_w01_repair_log <- function() {
  ensure_w01_repair_tab()
  ss <- sheet_id_from_env()
  tab <- w01_repair_tab()
  x <- googlesheets4::read_sheet(ss, sheet=tab, col_types="c")
  if (!nrow(x)) return(list())
  required <- c(
    "repair_id","review_case_id","source","source_record_id","action","value",
    "reason","reviewer","saved_at_utc","queue_sha256","supersedes_repair_id"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("W01 repair tab missing field(s): ", paste(missing, collapse=", "), call.=FALSE)
  lapply(seq_len(nrow(x)), function(i) as.list(x[i, required, drop=FALSE]))
}

active_sheet_w01_repairs <- function(queue_sha256 = "") {
  rows <- read_sheet_w01_repair_log()
  if (nzchar(as.character(queue_sha256))) {
    rows <- Filter(function(x) identical(
      tolower(as.character(x$queue_sha256 %||% "")),
      tolower(as.character(queue_sha256))
    ), rows)
  }
  if (!length(rows)) return(list())
  key <- vapply(rows, function(x) paste(
    as.character(x$source %||% ""),
    as.character(x$source_record_id %||% ""),
    sep="::"
  ), character(1))
  tm <- vapply(rows, function(x) as.character(x$saved_at_utc %||% ""), character(1))
  ord <- order(tm, seq_along(rows), decreasing=TRUE)
  rows <- rows[ord]; key <- key[ord]
  rows[!duplicated(key)]
}

append_sheet_w01_repair <- function(repair, prior_repair = NULL) {
  ensure_w01_repair_tab()
  action <- as.character(repair$action %||% "")
  if (!(action %in% c("replace_abstract","strip_abstract"))) {
    stop("Unsupported W01 Shiny abstract repair action", call.=FALSE)
  }
  if (identical(action,"replace_abstract") && !nzchar(trimws(as.character(repair$value %||% "")))) {
    stop("Replacement abstract must not be empty", call.=FALSE)
  }
  ss <- sheet_id_from_env()
  tab <- w01_repair_tab()
  saved_at <- as.character(repair$saved_at_utc %||% format(Sys.time(), tz="UTC", format="%Y-%m-%dT%H:%M:%SZ"))
  supersedes <- if (is.null(prior_repair)) "" else as.character(prior_repair$repair_id %||% "")
  repair_id <- paste0("w01-repair-", substr(digest::digest(
    paste(
      repair$review_case_id, repair$source, repair$source_record_id,
      repair$action, repair$value, repair$reviewer, saved_at, sep="|"
    ),
    algo="sha256", serialize=FALSE
  ), 1L, 24L))
  row <- data.frame(
    repair_id=repair_id,
    review_case_id=as.character(repair$review_case_id),
    source=as.character(repair$source),
    source_record_id=as.character(repair$source_record_id),
    action=action,
    value=as.character(repair$value %||% ""),
    reason=as.character(repair$reason %||% "human_abstract_correction_during_deduplication"),
    reviewer=as.character(repair$reviewer),
    saved_at_utc=saved_at,
    queue_sha256=tolower(as.character(repair$queue_sha256)),
    supersedes_repair_id=supersedes,
    stringsAsFactors=FALSE
  )
  googlesheets4::sheet_append(ss, data=row, sheet=tab)
  verify <- googlesheets4::read_sheet(ss, sheet=tab, col_types="c")
  hit <- verify[as.character(verify$repair_id)==repair_id,,drop=FALSE]
  if (nrow(hit)!=1L) stop("W01 abstract repair write could not be verified", call.=FALSE)
  as.list(hit[1,,drop=FALSE])
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
    tabs <- sheet_names_cached(ss)
    if ("queue_w01_active" %in% tabs) {
      tab <- "queue_w01_active"
    } else if ("queue_w01_legacy_730" %in% tabs) {
      tab <- "queue_w01_legacy_730"
    } else {
      return(NULL)
    }
  }

  tabs <- sheet_names_cached(ss)
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
  tabs <- sheet_names_cached(ss)
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
  tabs <- sheet_names_cached(ss)
  if (!tab %in% tabs) return(list())
  x <- googlesheets4::read_sheet(ss, sheet = tab, col_types = "c")
  normalise_w02_sheet_rows(x)
}

active_sheet_w02_decisions <- function() {
  active_decision_events(
    read_sheet_w02_decision_log(),
    case_fields = c("case_id", "review_case_id")
  )
}

append_sheet_w02_decision <- function(decision, prior_decision = NULL) {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- w02_decision_tab()
  tabs <- sheet_names_cached(ss)

  current_active <- active_sheet_w02_decisions()
  case_id <- as.character(decision$review_case_id %||% "")
  user_id <- as.character(decision$reviewer %||% "")
  current_case <- Filter(
    function(x) identical(as.character(x$review_case_id %||% ""), case_id),
    current_active
  )
  if (length(current_case)) {
    current <- current_case[[1L]]
    current_decision <- tolower(trimws(as.character(current$decision %||% "")))
    current_user <- as.character(current$reviewer %||% current$user_id %||% "")
    if (
      nzchar(current_decision) &&
      !identical(current_decision, "uncertain") &&
      nzchar(current_user) &&
      !identical(current_user, user_id)
    ) {
      stop("This enrichment case has already been resolved by another reviewer", call. = FALSE)
    }
  }

  required_cols <- c(
    "decision_id","review_case_id","record_id","provider","field","reason",
    "decision","note","reviewer","resolved_at_utc","queue_sha256",
    "supersedes_decision_id"
  )

  if (!tab %in% tabs) {
    sheet_add_cached(ss, tab)
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
  normalise_saved_decision_event(
    as.list(hits[1, , drop=FALSE]),
    prior_decision = prior_decision,
    case_fields = c("case_id", "review_case_id")
  )
}


w01_export_request_tab <- function() {
  Sys.getenv("LEM_W01_EXPORT_REQUEST_TAB", unset = "w01_export_requests")
}

w01_export_request_exists <- function(queue_sha256, batch_id) {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- w01_export_request_tab()
  tabs <- sheet_names_cached(ss)
  if (!tab %in% tabs) return(FALSE)

  x <- googlesheets4::read_sheet(ss, sheet = tab, col_types = "c")
  if (!nrow(x)) return(FALSE)
  required <- c("queue_sha256","batch_id","status")
  if (!all(required %in% names(x))) stop("W01 export-request tab is malformed", call.=FALSE)

  any(
    as.character(x$queue_sha256) == as.character(queue_sha256) &
    as.character(x$batch_id) == as.character(batch_id) &
    as.character(x$status) %in% c("dispatching","dispatched")
  )
}

append_w01_export_request <- function(queue_sha256, batch_id, status, message = "") {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- w01_export_request_tab()
  tabs <- sheet_names_cached(ss)

  required_cols <- c(
    "request_id","queue_sha256","batch_id","status",
    "requested_at_utc","message"
  )

  if (!tab %in% tabs) {
    sheet_add_cached(ss, tab)
    empty <- as.data.frame(setNames(replicate(length(required_cols), character(), simplify=FALSE), required_cols))
    googlesheets4::sheet_write(empty, ss=ss, sheet=tab)
  }

  row <- data.frame(
    request_id=paste0("w01-export-",digest::digest(
      paste(queue_sha256,batch_id,status,format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%OS6Z"),sep="|"),
      algo="sha256",serialize=FALSE
    )),
    queue_sha256=as.character(queue_sha256),
    batch_id=as.character(batch_id),
    status=as.character(status),
    requested_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"),
    message=as.character(message),
    stringsAsFactors=FALSE
  )
  googlesheets4::sheet_append(ss,data=row,sheet=tab)
  invisible(row)
}


w02_resume_request_tab <- function() {
  Sys.getenv("LEM_W02_RESUME_REQUEST_TAB", unset = "w02_resume_requests")
}

w02_resume_request_exists <- function(queue_sha256, source_run_id) {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- w02_resume_request_tab()
  tabs <- sheet_names_cached(ss)
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
  tabs <- sheet_names_cached(ss)

  required_cols <- c(
    "request_id","queue_sha256","source_run_id","status",
    "requested_at_utc","message"
  )

  if (!tab %in% tabs) {
    sheet_add_cached(ss, tab)
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


workflow_resume_request_exists <- function(tab, queue_sha256, source_run_id, batch_id) {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tabs <- sheet_names_cached(ss)
  if (!tab %in% tabs) return(FALSE)

  x <- googlesheets4::read_sheet(ss, sheet = tab, col_types = "c")
  if (!nrow(x)) return(FALSE)
  required <- c("queue_sha256","source_run_id","batch_id","status")
  if (!all(required %in% names(x))) {
    stop(tab, " resume-request tab is malformed", call. = FALSE)
  }

  any(
    as.character(x$queue_sha256) == as.character(queue_sha256) &
    as.character(x$source_run_id) == as.character(source_run_id) &
    as.character(x$batch_id) == as.character(batch_id) &
    as.character(x$status) %in% c("dispatching","dispatched")
  )
}

append_workflow_resume_request <- function(tab, prefix, queue_sha256, source_run_id, batch_id, status, message = "") {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tabs <- sheet_names_cached(ss)
  required_cols <- c(
    "request_id","queue_sha256","source_run_id","batch_id","status",
    "requested_at_utc","message"
  )

  if (!tab %in% tabs) {
    sheet_add_cached(ss, tab)
    empty <- as.data.frame(
      setNames(replicate(length(required_cols), character(), simplify = FALSE), required_cols),
      stringsAsFactors = FALSE
    )
    googlesheets4::sheet_write(empty, ss = ss, sheet = tab)
  }

  row <- data.frame(
    request_id = paste0(prefix, digest::digest(
      paste(
        queue_sha256, source_run_id, batch_id, status,
        format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%OS6Z"),
        sep="|"
      ),
      algo="sha256", serialize=FALSE
    )),
    queue_sha256=as.character(queue_sha256),
    source_run_id=as.character(source_run_id),
    batch_id=as.character(batch_id),
    status=as.character(status),
    requested_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"),
    message=as.character(message),
    stringsAsFactors=FALSE
  )
  googlesheets4::sheet_append(ss, data=row, sheet=tab)
  invisible(row)
}

w04_validation_finalize_request_tab <- function() {
  Sys.getenv("LEM_W04_VALIDATION_FINALIZE_REQUEST_TAB", unset = "w04_validation_finalize_requests")
}

w04_validation_finalize_request_exists <- function(queue_sha256, batch_id) {
  workflow_resume_request_exists(
    w04_validation_finalize_request_tab(), queue_sha256, "", batch_id
  )
}

append_w04_validation_finalize_request <- function(queue_sha256, batch_id, status, message = "") {
  append_workflow_resume_request(
    w04_validation_finalize_request_tab(), "w04-validation-finalize-",
    queue_sha256, "", batch_id, status, message
  )
}

w04_resolution_resume_request_tab <- function() {
  Sys.getenv("LEM_W04_RESOLUTION_RESUME_REQUEST_TAB", unset = "w04_resolution_resume_requests")
}

w04_resolution_resume_request_exists <- function(queue_sha256, source_run_id, batch_id) {
  workflow_resume_request_exists(
    w04_resolution_resume_request_tab(), queue_sha256, source_run_id, batch_id
  )
}

append_w04_resolution_resume_request <- function(queue_sha256, source_run_id, batch_id, status, message = "") {
  append_workflow_resume_request(
    w04_resolution_resume_request_tab(), "w04-resolution-resume-",
    queue_sha256, source_run_id, batch_id, status, message
  )
}

w08_resume_request_tab <- function() {
  Sys.getenv("LEM_W08_RESUME_REQUEST_TAB", unset = "w08_resume_requests")
}

w08_resume_request_exists <- function(queue_sha256, source_run_id, batch_id) {
  workflow_resume_request_exists(
    w08_resume_request_tab(), queue_sha256, source_run_id, batch_id
  )
}

append_w08_resume_request <- function(queue_sha256, source_run_id, batch_id, status, message = "") {
  append_workflow_resume_request(
    w08_resume_request_tab(), "w08-resume-",
    queue_sha256, source_run_id, batch_id, status, message
  )
}


w04_decision_tab <- function() {
  Sys.getenv("LEM_W04_DECISION_TAB", unset = "decisions_w04_validation")
}

w04_screening_note_tab <- function() {
  Sys.getenv("LEM_W04_SCREENING_NOTE_TAB", unset = "w04_screening_notes")
}

read_sheet_w04_screening_note_log <- function() {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- w04_screening_note_tab()
  if (!tab %in% sheet_names_cached(ss)) return(list())
  x <- googlesheets4::read_sheet(ss, sheet=tab, col_types="c")
  if (!nrow(x)) return(list())
  lapply(seq_len(nrow(x)), function(i) as.list(x[i,,drop=FALSE]))
}

active_sheet_w04_screening_notes <- function(queue_sha256 = "") {
  rows <- read_sheet_w04_screening_note_log()
  if (!length(rows)) return(list())
  sha <- tolower(as.character(queue_sha256 %||% ""))
  if (nzchar(sha)) {
    rows <- Filter(
      function(x) identical(tolower(as.character(x$queue_sha256 %||% "")), sha),
      rows
    )
  }
  if (!length(rows)) return(list())
  keys <- vapply(rows, function(x) paste(
    tolower(as.character(x$queue_sha256 %||% "")),
    as.character(x$review_case_id %||% ""),
    as.character(x$reviewer %||% ""),
    sep="::"
  ), character(1))
  tm <- vapply(rows, function(x) as.character(x$saved_at_utc %||% ""), character(1))
  ord <- order(tm, seq_along(rows), decreasing=TRUE)
  rows <- rows[ord]
  keys <- keys[ord]
  rows[!duplicated(keys)]
}

append_sheet_w04_screening_note <- function(note, prior_note=NULL) {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- w04_screening_note_tab()
  tabs <- sheet_names_cached(ss)
  cols <- c(
    "note_id","review_case_id","record_id","note","reviewer",
    "saved_at_utc","queue_sha256","supersedes_note_id"
  )

  if (!tab %in% tabs) {
    sheet_add_cached(ss, tab)
    empty <- as.data.frame(
      setNames(replicate(length(cols), character(), simplify=FALSE), cols),
      stringsAsFactors=FALSE
    )
    googlesheets4::sheet_write(empty, ss=ss, sheet=tab)
  }

  review_case_id <- as.character(note$review_case_id %||% "")
  reviewer <- as.character(note$reviewer %||% "")
  text <- trimws(as.character(note$note %||% ""))
  if (!nzchar(review_case_id)) stop("W04 screening note is missing review_case_id", call.=FALSE)
  if (!nzchar(reviewer)) stop("W04 screening note is missing reviewer", call.=FALSE)
  if (!nzchar(text)) stop("W04 screening note must not be empty", call.=FALSE)

  saved_at <- as.character(note$saved_at_utc %||% format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"))
  note_id <- paste0("w04-note-", substr(digest::digest(
    paste(review_case_id, reviewer, text, saved_at, sep="|"),
    algo="sha256", serialize=FALSE
  ),1L,24L))
  supersedes <- if (is.null(prior_note)) "" else as.character(prior_note$note_id %||% "")

  row <- data.frame(
    note_id=note_id,
    review_case_id=review_case_id,
    record_id=as.character(note$record_id %||% ""),
    note=text,
    reviewer=reviewer,
    saved_at_utc=saved_at,
    queue_sha256=as.character(note$queue_sha256 %||% ""),
    supersedes_note_id=supersedes,
    stringsAsFactors=FALSE
  )
  googlesheets4::sheet_append(ss, data=row, sheet=tab)

  verify <- googlesheets4::read_sheet(ss, sheet=tab, col_types="c")
  hit <- verify[as.character(verify$note_id)==note_id,,drop=FALSE]
  if (nrow(hit)!=1L) stop("W04 screening note write could not be verified", call.=FALSE)
  as.list(hit[1,,drop=FALSE])
}

w04_resolution_abstract_edit_tab <- function() {
  Sys.getenv("LEM_W04_RESOLUTION_ABSTRACT_EDIT_TAB", unset = "w04_resolution_abstract_edits")
}

read_sheet_w04_resolution_abstract_edit_log <- function() {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- w04_resolution_abstract_edit_tab()
  if (!tab %in% sheet_names_cached(ss)) return(list())
  x <- googlesheets4::read_sheet(ss, sheet=tab, col_types="c")
  if (!nrow(x)) return(list())
  lapply(seq_len(nrow(x)), function(i) as.list(x[i,,drop=FALSE]))
}

active_sheet_w04_resolution_abstract_edits <- function(queue_sha256 = "") {
  rows <- read_sheet_w04_resolution_abstract_edit_log()
  if (!length(rows)) return(list())
  sha <- tolower(as.character(queue_sha256 %||% ""))
  if (nzchar(sha)) {
    rows <- Filter(
      function(x) identical(tolower(as.character(x$queue_sha256 %||% "")), sha),
      rows
    )
  }
  if (!length(rows)) return(list())
  keys <- vapply(rows, function(x) paste(
    tolower(as.character(x$queue_sha256 %||% "")),
    as.character(x$review_case_id %||% ""),
    sep="::"
  ), character(1))
  tm <- vapply(rows, function(x) as.character(x$saved_at_utc %||% ""), character(1))
  ord <- order(tm, seq_along(rows), decreasing=TRUE)
  rows <- rows[ord]
  keys <- keys[ord]
  rows[!duplicated(keys)]
}

append_sheet_w04_resolution_abstract_edit <- function(edit, prior_edit=NULL) {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- w04_resolution_abstract_edit_tab()
  tabs <- sheet_names_cached(ss)
  cols <- c(
    "edit_id","review_case_id","record_id","abstract","reviewer",
    "saved_at_utc","queue_sha256","supersedes_edit_id"
  )
  if (!tab %in% tabs) {
    sheet_add_cached(ss, tab)
    empty <- as.data.frame(
      setNames(replicate(length(cols), character(), simplify=FALSE), cols),
      stringsAsFactors=FALSE
    )
    googlesheets4::sheet_write(empty, ss=ss, sheet=tab)
  }

  review_case_id <- as.character(edit$review_case_id %||% "")
  record_id <- as.character(edit$record_id %||% "")
  reviewer <- as.character(edit$reviewer %||% "")
  abstract <- trimws(as.character(edit$abstract %||% ""))
  queue_sha <- tolower(as.character(edit$queue_sha256 %||% ""))
  if (!nzchar(review_case_id) || !nzchar(record_id)) stop("W04 abstract edit is missing record identity", call.=FALSE)
  if (!nzchar(reviewer)) stop("W04 abstract edit is missing reviewer", call.=FALSE)
  if (!nzchar(abstract)) stop("W04 abstract edit must not be empty", call.=FALSE)
  if (!grepl("^[0-9a-f]{64}$",queue_sha)) stop("W04 abstract edit has invalid queue SHA-256", call.=FALSE)

  saved_at <- as.character(edit$saved_at_utc %||% format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"))
  edit_id <- paste0("w04-abs-", substr(digest::digest(
    paste(review_case_id, record_id, abstract, reviewer, saved_at, queue_sha, sep="|"),
    algo="sha256", serialize=FALSE
  ),1L,24L))
  supersedes <- if (is.null(prior_edit)) "" else as.character(prior_edit$edit_id %||% "")

  row <- data.frame(
    edit_id=edit_id,
    review_case_id=review_case_id,
    record_id=record_id,
    abstract=abstract,
    reviewer=reviewer,
    saved_at_utc=saved_at,
    queue_sha256=queue_sha,
    supersedes_edit_id=supersedes,
    stringsAsFactors=FALSE
  )
  googlesheets4::sheet_append(ss, data=row, sheet=tab)

  verify <- googlesheets4::read_sheet(ss, sheet=tab, col_types="c")
  hit <- verify[as.character(verify$edit_id)==edit_id,,drop=FALSE]
  if (nrow(hit)!=1L) stop("W04 abstract edit write could not be verified", call.=FALSE)
  as.list(hit[1,,drop=FALSE])
}

w04_resolution_decision_tab <- function() {
  Sys.getenv("LEM_W04_RESOLUTION_DECISION_TAB", unset = "decisions_w04_resolution")
}

w04_conflict_decision_tab <- function() {
  Sys.getenv("LEM_W04_CONFLICT_DECISION_TAB", unset = "decisions_w04_conflict")
}

w04_consistency_analysis_tab <- function() {
  Sys.getenv("LEM_W04_CONSISTENCY_TAB", unset = "w04_consistency_analyses")
}

w04_kappa_registry_tab <- function() {
  Sys.getenv("LEM_W04_KAPPA_REGISTRY_TAB", unset = "w04_kappa_registry")
}

w04_human_kappa_registry_tab <- function() {
  Sys.getenv("LEM_W04_HUMAN_KAPPA_REGISTRY_TAB", unset = "w04_human_kappa_registry")
}

ensure_w04_human_kappa_registry_tab <- function() {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- w04_human_kappa_registry_tab()
  cols <- w04_human_kappa_registry_columns()
  tabs <- sheet_names_cached(ss)
  if (!tab %in% tabs) {
    sheet_add_cached(ss,tab)
    googlesheets4::sheet_write(w04_empty_human_kappa_registry(),ss=ss,sheet=tab)
    return(invisible(TRUE))
  }
  x <- googlesheets4::read_sheet(ss,sheet=tab,col_types="c")
  if (!nrow(x) && !all(cols %in% names(x))) {
    googlesheets4::sheet_write(w04_empty_human_kappa_registry(),ss=ss,sheet=tab)
  } else if (nrow(x)) {
    w04_normalise_human_kappa_registry(x)
  }
  invisible(TRUE)
}

read_w04_human_kappa_registry <- function() {
  gs4_auth_from_env()
  ensure_w04_human_kappa_registry_tab()
  ss <- sheet_id_from_env()
  x <- googlesheets4::read_sheet(ss,sheet=w04_human_kappa_registry_tab(),col_types="c")
  w04_normalise_human_kappa_registry(x)
}

append_w04_human_kappa_registry <- function(row) {
  gs4_auth_from_env()
  ensure_w04_human_kappa_registry_tab()
  ss <- sheet_id_from_env()
  tab <- w04_human_kappa_registry_tab()
  row <- w04_normalise_human_kappa_registry(row)
  if (nrow(row) != 1L) stop("Exactly one human consistency row must be appended",call.=FALSE)
  existing <- read_w04_human_kappa_registry()
  id <- as.character(row$consistency_id[[1L]])
  if (id %in% existing$consistency_id) {
    return(existing[existing$consistency_id==id,,drop=FALSE][1,,drop=FALSE])
  }
  googlesheets4::sheet_append(ss,data=row,sheet=tab)
  verify <- read_w04_human_kappa_registry()
  if (sum(verify$consistency_id==id) != 1L) {
    stop("Human consistency registry append verification failed",call.=FALSE)
  }
  verify[verify$consistency_id==id,,drop=FALSE]
}

delete_w04_human_kappa_registry_row <- function(consistency_id) {
  gs4_auth_from_env()
  ensure_w04_human_kappa_registry_tab()
  ss <- sheet_id_from_env()
  tab <- w04_human_kappa_registry_tab()
  id <- as.character(consistency_id)
  if (!nzchar(id)) stop("Missing human consistency ID",call.=FALSE)
  x <- read_w04_human_kappa_registry()
  hits <- which(x$consistency_id==id)
  if (length(hits) != 1L) stop("Human consistency row was not found uniquely",call.=FALSE)
  keep <- x[-hits,,drop=FALSE]
  if (!nrow(keep)) keep <- w04_empty_human_kappa_registry()
  googlesheets4::sheet_write(keep,ss=ss,sheet=tab)
  verify <- read_w04_human_kappa_registry()
  if (id %in% verify$consistency_id) stop("Human consistency row deletion verification failed",call.=FALSE)
  invisible(verify)
}

read_github_w04_kappa_registry <- function() {
  repo <- Sys.getenv(
    "LEM_W04_KAPPA_REGISTRY_REPO",
    unset = "thesalmonandthetomato/LivingEvidenceMap"
  )
  ref <- Sys.getenv(
    "LEM_W04_KAPPA_REGISTRY_REF",
    unset = "workflow01-final-architecture"
  )
  path <- Sys.getenv(
    "LEM_W04_KAPPA_REGISTRY_PATH",
    unset = "docs/workflow04/kappa_registry.csv"
  )
  endpoint <- sprintf(
    "https://api.github.com/repos/%s/contents/%s",
    repo,
    paste(vapply(strsplit(path,"/",fixed=TRUE)[[1L]],URLencode,character(1),reserved=TRUE),collapse="/")
  )
  req <- httr2::request(endpoint) |>
    httr2::req_url_query(ref=ref) |>
    httr2::req_headers(
      Accept="application/vnd.github+json",
      `X-GitHub-Api-Version`="2022-11-28",
      `User-Agent`="LivingEvidenceMap-Adjudication"
    )
  token <- Sys.getenv("LEM_GITHUB_DISPATCH_TOKEN",unset="")
  if(nzchar(token)) {
    req <- httr2::req_headers(req,Authorization=paste("Bearer",token))
  }
  x <- tryCatch({
    resp <- httr2::req_perform(req)
    if(httr2::resp_status(resp)!=200L) stop("GitHub API HTTP ",httr2::resp_status(resp))
    meta <- jsonlite::fromJSON(httr2::resp_body_string(resp),simplifyVector=FALSE)
    encoding <- if(is.null(meta$encoding)) "" else as.character(meta$encoding)
    content <- if(is.null(meta$content)) "" else as.character(meta$content)
    if(!identical(encoding,"base64") || !nzchar(content)) {
      stop("GitHub registry response is missing base64 content")
    }
    txt <- rawToChar(jsonlite::base64_dec(gsub("[[:space:]]+","",content)))
    utils::read.csv(
      text=txt,
      stringsAsFactors=FALSE,
      check.names=FALSE,
      colClasses="character",
      na.strings=NULL
    )
  },error=function(e) {
    stop("Could not read authoritative GitHub W04 kappa registry: ",conditionMessage(e),call.=FALSE)
  })
  w04_normalise_kappa_registry(x)
}

read_sheet_w04_kappa_registry <- function() {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- w04_kappa_registry_tab()
  if (!tab %in% sheet_names_cached(ss)) return(w04_empty_kappa_registry())
  x <- googlesheets4::read_sheet(ss,sheet=tab,col_types="c")
  w04_normalise_kappa_registry(x)
}

write_sheet_w04_kappa_registry <- function(x) {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- w04_kappa_registry_tab()
  x <- w04_normalise_kappa_registry(x)
  if (!tab %in% sheet_names_cached(ss)) sheet_add_cached(ss,tab)
  googlesheets4::sheet_write(x,ss=ss,sheet=tab)
  verify <- googlesheets4::read_sheet(ss,sheet=tab,col_types="c")
  verify <- w04_normalise_kappa_registry(verify)
  if (!identical(
    unname(w04_kappa_registry_row_signatures(verify)),
    unname(w04_kappa_registry_row_signatures(x))
  ) || !identical(
    names(w04_kappa_registry_row_signatures(verify)),
    names(w04_kappa_registry_row_signatures(x))
  )) {
    stop("W04 kappa Google mirror write verification failed",call.=FALSE)
  }
  invisible(x)
}

sync_w04_kappa_registry_from_github <- function() {
  github <- read_github_w04_kappa_registry()
  google <- read_sheet_w04_kappa_registry()
  relation <- w04_kappa_registry_relation(github,google)
  if (relation %in% c("google_empty","google_subset")) {
    write_sheet_w04_kappa_registry(github)
  }
  github
}

ensure_w04_consistency_analysis_tab <- function() {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- w04_consistency_analysis_tab()
  cols <- c(
    "analysis_id","project_id","project_name","batch_id","queue_sha256",
    "review_mode","rater_ids_json","metric","eligible_n","complete_n",
    "missing_n","agreement_n","conflict_n","raw_agreement","kappa",
    "pairwise_json","patterns_json","conflict_case_ids_json",
    "agreement_case_ids_json","created_by","created_at_utc"
  )
  tabs <- sheet_names_cached(ss)
  if (!tab %in% tabs) {
    sheet_add_cached(ss,tab)
    empty <- as.data.frame(
      setNames(replicate(length(cols),character(),simplify=FALSE),cols),
      stringsAsFactors=FALSE
    )
    googlesheets4::sheet_write(empty,ss=ss,sheet=tab)
  }
  invisible(TRUE)
}

read_w04_consistency_analyses <- function() {
  gs4_auth_from_env()
  ensure_w04_consistency_analysis_tab()
  ss <- sheet_id_from_env()
  x <- googlesheets4::read_sheet(ss,sheet=w04_consistency_analysis_tab(),col_types="c")
  if (!nrow(x)) return(list())
  lapply(seq_len(nrow(x)),function(i)as.list(x[i,,drop=FALSE]))
}

w04_conflict_set_tab <- function() {
  Sys.getenv("LEM_W04_CONFLICT_SET_TAB", unset = "w04_conflict_sets")
}

ensure_w04_conflict_set_tab <- function() {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- w04_conflict_set_tab()
  cols <- c(
    "conflict_set_id","analysis_id","project_id","project_name",
    "parent_batch_id","parent_queue_sha256","conflict_queue_sha256",
    "comparison_type","rater_ids_json","conflict_case_ids_json",
    "created_by","created_at_utc"
  )
  tabs <- sheet_names_cached(ss)
  if (!tab %in% tabs) {
    sheet_add_cached(ss,tab)
    empty <- as.data.frame(
      setNames(replicate(length(cols),character(),simplify=FALSE),cols),
      stringsAsFactors=FALSE
    )
    googlesheets4::sheet_write(empty,ss=ss,sheet=tab)
  }
  invisible(TRUE)
}

read_w04_conflict_sets <- function() {
  gs4_auth_from_env()
  ensure_w04_conflict_set_tab()
  ss <- sheet_id_from_env()
  x <- googlesheets4::read_sheet(ss,sheet=w04_conflict_set_tab(),col_types="c")
  if (!nrow(x)) return(list())
  lapply(seq_len(nrow(x)),function(i)as.list(x[i,,drop=FALSE]))
}

append_w04_conflict_set <- function(analysis_row,created_by="") {
  analysis_id <- as.character(analysis_row$analysis_id %||% "")
  parent_batch_id <- as.character(analysis_row$batch_id %||% "")
  parent_queue_sha256 <- as.character(analysis_row$queue_sha256 %||% "")
  if (!nzchar(analysis_id) || !nzchar(parent_batch_id) || !nzchar(parent_queue_sha256)) {
    stop("Saved analysis is missing provenance required for conflict-set creation",call.=FALSE)
  }

  rater_ids <- tryCatch(
    as.character(jsonlite::fromJSON(as.character(analysis_row$rater_ids_json %||% "[]"))),
    error=function(e) character()
  )
  conflict_case_ids <- tryCatch(
    as.character(jsonlite::fromJSON(as.character(analysis_row$conflict_case_ids_json %||% "[]"))),
    error=function(e) character()
  )
  rater_ids <- unique(rater_ids[nzchar(rater_ids)])
  conflict_case_ids <- unique(conflict_case_ids[nzchar(conflict_case_ids)])
  if (length(rater_ids) < 2L) stop("Conflict set requires at least two raters",call.=FALSE)
  if (!length(conflict_case_ids)) stop("This analysis has no conflicts to resolve",call.=FALSE)

  has_model <- "model" %in% rater_ids
  human_n <- sum(rater_ids != "model")
  comparison_type <- if (has_model && human_n > 1L) {
    "human_human_model"
  } else if (has_model) {
    "human_model"
  } else {
    "human_human"
  }

  payload <- jsonlite::toJSON(
    list(
      analysis_id=analysis_id,
      parent_batch_id=parent_batch_id,
      parent_queue_sha256=parent_queue_sha256,
      rater_ids=rater_ids,
      conflict_case_ids=sort(conflict_case_ids)
    ),
    auto_unbox=TRUE,null="null",na="null"
  )
  conflict_queue_sha256 <- digest::digest(payload,algo="sha256",serialize=FALSE)
  conflict_set_id <- paste0(
    "w04-conflict-",
    substr(digest::digest(
      paste(analysis_id,conflict_queue_sha256,sep="|"),
      algo="sha256",serialize=FALSE
    ),1L,20L)
  )

  gs4_auth_from_env()
  ensure_w04_conflict_set_tab()
  ss <- sheet_id_from_env()
  tab <- w04_conflict_set_tab()
  existing <- googlesheets4::read_sheet(ss,sheet=tab,col_types="c")
  if (nrow(existing) && conflict_set_id %in% as.character(existing$conflict_set_id)) {
    hit <- existing[as.character(existing$conflict_set_id)==conflict_set_id,,drop=FALSE]
    return(as.list(hit[1,,drop=FALSE]))
  }

  now <- format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
  row <- data.frame(
    conflict_set_id=conflict_set_id,
    analysis_id=analysis_id,
    project_id=as.character(analysis_row$project_id %||% Sys.getenv("LEM_PROJECT_ID",unset="living-evidence-map")),
    project_name=as.character(analysis_row$project_name %||% Sys.getenv("LEM_PROJECT_NAME",unset="Living Evidence Map")),
    parent_batch_id=parent_batch_id,
    parent_queue_sha256=parent_queue_sha256,
    conflict_queue_sha256=conflict_queue_sha256,
    comparison_type=comparison_type,
    rater_ids_json=as.character(jsonlite::toJSON(rater_ids,auto_unbox=FALSE)),
    conflict_case_ids_json=as.character(jsonlite::toJSON(conflict_case_ids,auto_unbox=FALSE)),
    created_by=as.character(created_by),
    created_at_utc=now,
    stringsAsFactors=FALSE
  )
  googlesheets4::sheet_append(ss,data=row,sheet=tab)
  verify <- googlesheets4::read_sheet(ss,sheet=tab,col_types="c")
  if(sum(as.character(verify$conflict_set_id)==conflict_set_id)!=1L) {
    stop("W04 conflict-set write verification failed",call.=FALSE)
  }
  as.list(row[1,,drop=FALSE])
}

append_w04_consistency_analysis <- function(analysis,batch_id,queue_sha256,review_mode="",created_by="",analysis_id_override="") {
  gs4_auth_from_env()
  ensure_w04_consistency_analysis_tab()
  ss <- sheet_id_from_env()
  tab <- w04_consistency_analysis_tab()
  now <- format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
  payload <- paste(
    batch_id,queue_sha256,
    paste(analysis$rater_ids %||% character(),collapse="|"),
    analysis$complete %||% 0L,
    analysis$kappa %||% NA_real_,
    now,created_by,sep="|"
  )
  analysis_id <- as.character(analysis_id_override %||% "")
  if (!nzchar(analysis_id)) {
    analysis_id <- paste0(
      "w04-analysis-",
      substr(digest::digest(payload,algo="sha256",serialize=FALSE),1L,20L)
    )
  }
  existing <- googlesheets4::read_sheet(ss,sheet=tab,col_types="c")
  if (nrow(existing) && analysis_id %in% as.character(existing$analysis_id)) {
    hit <- existing[as.character(existing$analysis_id)==analysis_id,,drop=FALSE]
    return(as.list(hit[1,,drop=FALSE]))
  }
  row <- data.frame(
    analysis_id=analysis_id,
    project_id=Sys.getenv("LEM_PROJECT_ID",unset="living-evidence-map"),
    project_name=Sys.getenv("LEM_PROJECT_NAME",unset="Living Evidence Map"),
    batch_id=as.character(batch_id),
    queue_sha256=as.character(queue_sha256),
    review_mode=as.character(review_mode),
    rater_ids_json=as.character(jsonlite::toJSON(analysis$rater_ids %||% character(),auto_unbox=FALSE)),
    metric=as.character(analysis$metric %||% ""),
    eligible_n=as.character(analysis$eligible %||% 0L),
    complete_n=as.character(analysis$complete %||% 0L),
    missing_n=as.character(analysis$missing %||% 0L),
    agreement_n=as.character(analysis$agreement_cases %||% 0L),
    conflict_n=as.character(analysis$conflict_cases %||% 0L),
    raw_agreement=as.character(analysis$raw_agreement %||% NA_real_),
    kappa=as.character(analysis$kappa %||% NA_real_),
    pairwise_json=as.character(jsonlite::toJSON(analysis$pairwise %||% list(),auto_unbox=TRUE,null="null",na="null")),
    patterns_json=as.character(jsonlite::toJSON(analysis$patterns %||% list(),auto_unbox=TRUE,null="null",na="null")),
    conflict_case_ids_json=as.character(jsonlite::toJSON(analysis$conflict_case_ids %||% character(),auto_unbox=FALSE)),
    agreement_case_ids_json=as.character(jsonlite::toJSON(analysis$agreement_case_ids %||% character(),auto_unbox=FALSE)),
    created_by=as.character(created_by),
    created_at_utc=now,
    stringsAsFactors=FALSE
  )
  googlesheets4::sheet_append(ss,data=row,sheet=tab)
  verify <- googlesheets4::read_sheet(ss,sheet=tab,col_types="c")
  if(sum(as.character(verify$analysis_id)==analysis_id)!=1L) {
    stop("W04 consistency analysis write verification failed",call.=FALSE)
  }
  as.list(row[1,,drop=FALSE])
}

delete_w04_consistency_analysis_row <- function(analysis_id) {
  gs4_auth_from_env()
  ensure_w04_consistency_analysis_tab()
  ss <- sheet_id_from_env()
  tab <- w04_consistency_analysis_tab()
  id <- as.character(analysis_id)
  if (!nzchar(id)) stop("Missing W04 consistency analysis ID",call.=FALSE)
  x <- googlesheets4::read_sheet(ss,sheet=tab,col_types="c")
  if (!nrow(x)) return(invisible(TRUE))
  hits <- which(as.character(x$analysis_id)==id)
  if (!length(hits)) return(invisible(TRUE))
  if (length(hits)>1L) stop("W04 consistency analysis ID is not unique",call.=FALSE)
  keep <- x[-hits,,drop=FALSE]
  if (!nrow(keep)) {
    cols <- c(
      "analysis_id","project_id","project_name","batch_id","queue_sha256",
      "review_mode","rater_ids_json","metric","eligible_n","complete_n",
      "missing_n","agreement_n","conflict_n","raw_agreement","kappa",
      "pairwise_json","patterns_json","conflict_case_ids_json",
      "agreement_case_ids_json","created_by","created_at_utc"
    )
    keep <- as.data.frame(setNames(replicate(length(cols),character(),simplify=FALSE),cols),stringsAsFactors=FALSE)
  }
  googlesheets4::sheet_write(keep,ss=ss,sheet=tab)
  verify <- googlesheets4::read_sheet(ss,sheet=tab,col_types="c")
  if (nrow(verify) && id %in% as.character(verify$analysis_id)) {
    stop("W04 consistency analysis deletion verification failed",call.=FALSE)
  }
  invisible(TRUE)
}

read_sheet_w04_queue_from_tab <- function(tab) {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tabs <- sheet_names_cached(ss)
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
  tabs <- sheet_names_cached(ss)
  if(!tab %in% tabs) return(list())
  x <- googlesheets4::read_sheet(ss,sheet=tab,col_types="c")
  if(!nrow(x)) return(list())
  lapply(seq_len(nrow(x)),function(i)as.list(x[i,,drop=FALSE]))
}

active_w04_decisions_from_tab <- function(
  tab,
  identity_scope = c("case", "case_user")
) {
  identity_scope <- match.arg(identity_scope)
  active_decision_events(
    read_sheet_w04_decision_log_from_tab(tab),
    case_fields = c("case_id", "review_case_id"),
    identity_scope = identity_scope
  )
}

read_sheet_w04_decision_log <- function() read_sheet_w04_decision_log_from_tab(w04_decision_tab())
active_sheet_w04_decisions <- function() {
  active_w04_decisions_from_tab(w04_decision_tab(), identity_scope = "case_user")
}
active_sheet_w04_resolution_decisions <- function() {
  active_w04_decisions_from_tab(w04_resolution_decision_tab(), identity_scope = "case")
}
active_sheet_w04_conflict_decisions <- function() {
  active_w04_decisions_from_tab(w04_conflict_decision_tab(), identity_scope = "case")
}

append_w04_decision_to_tab <- function(decision, prior_decision=NULL, tab, prefix="w04-dec-") {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tabs <- sheet_names_cached(ss)

  required_cols <- c(
    "decision_id","review_case_id","record_id","decision","rationale",
    "reviewer","resolved_at_utc","queue_sha256","supersedes_decision_id"
  )
  if(!tab %in% tabs) {
    sheet_add_cached(ss, tab)
    empty <- as.data.frame(setNames(replicate(length(required_cols),character(),simplify=FALSE),required_cols))
    googlesheets4::sheet_write(empty,ss=ss,sheet=tab)
  }

  decision_id <- paste0(prefix,digest::digest(
    paste(
      decision$review_case_id,
      decision$reviewer %||% "",
      decision$resolved_at_utc,
      decision$decision,
      sep="|"
    ),
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
  normalise_saved_decision_event(
    as.list(hits[1,,drop=FALSE]),
    prior_decision = prior_decision,
    case_fields = c("case_id", "review_case_id")
  )
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
  tabs <- sheet_names_cached(ss)
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
  tabs <- sheet_names_cached(ss)
  if(!tab %in% tabs) return(list())
  x <- googlesheets4::read_sheet(ss,sheet=tab,col_types="c")
  if(!nrow(x)) return(list())
  lapply(seq_len(nrow(x)),function(i)as.list(x[i,,drop=FALSE]))
}

active_sheet_w08_decisions <- function() {
  active_decision_events(
    read_sheet_w08_decision_log(),
    case_fields = c("case_id", "record_id")
  )
}

append_sheet_w08_decision <- function(decision, prior_decision=NULL) {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- w08_decision_tab()
  tabs <- sheet_names_cached(ss)

  current_active <- active_sheet_w08_decisions()
  record_id <- as.character(decision$record_id %||% "")
  user_id <- as.character(decision$reviewer %||% "")
  current_record <- Filter(
    function(x) identical(as.character(x$record_id %||% x$case_id %||% ""), record_id),
    current_active
  )
  if (length(current_record)) {
    current <- current_record[[1L]]
    current_user <- as.character(current$reviewer %||% current$user_id %||% "")
    if (
      nzchar(current_user) &&
      !identical(current_user, user_id)
    ) {
      stop("This annotation record has already been resolved by another reviewer", call. = FALSE)
    }
  }

  cols <- c(
    "decision_id","record_id","queue_sha256","record_case_sha256",
    "issue_decisions_json","reviewer","resolved_at_utc","supersedes_decision_id"
  )

  if(!tab %in% tabs) {
    sheet_add_cached(ss, tab)
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
  normalise_saved_decision_event(
    as.list(hit[1,,drop=FALSE]),
    prior_decision = prior_decision,
    case_fields = c("case_id", "record_id")
  )
}


pipeline_status_tab <- function() {
  Sys.getenv("LEM_PIPELINE_STATUS_TAB", unset = "pipeline_run_status")
}

read_latest_pipeline_status <- function() {
  val <- function(x) if (is.null(x) || !length(x)) "" else as.character(x[[1L]])

  # Production KPI values must come from the canonical current-run status on
  # workflow01-final-architecture. Do not fall back to pipeline_run_status:
  # that Sheet is an operational event log and historical schema migrations
  # can leave older rows positionally incompatible with current KPI columns.
  repo_url <- paste0(
    "https://raw.githubusercontent.com/thesalmonandthetomato/",
    "LivingEvidenceMap/workflow01-final-architecture/",
    "docs/current_run/current_run_status.json"
  )
  # raw.githubusercontent.com may briefly serve a cached response for an
  # unchanged URL after the status file is updated. Add a cache-busting query
  # so the Shiny progress display follows the canonical branch state promptly.
  repo_url_fresh <- paste0(
    repo_url,
    "?v=",
    as.integer(as.numeric(Sys.time()))
  )
  current <- tryCatch(
    jsonlite::fromJSON(repo_url_fresh, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(current)) return(NULL)
  if (!identical(as.character(current$schema), "living-evidence-map-current-run-status-v1")) {
    return(NULL)
  }

  delta_value <- function(cur, prev) {
    a <- suppressWarnings(as.numeric(val(cur)))
    b <- suppressWarnings(as.numeric(val(prev)))
    if (is.na(a) || is.na(b)) "" else as.character(a - b)
  }
  prev <- current$baseline$previous_counts %||% list()

  list(
    update_id = val(current$update_id),
    event_at_utc = val(current$last_updated_at_utc),
    stage = val(current$progress$current_stage),
    workflow_run_id = val(current$workflow_runs[[val(current$progress$current_stage)]]),
    last_search_date = val(current$search$search_date),
    canonical_existing = val(current$baseline$canonical_records),
    search_results_total = val(current$counts$search_results),
    search_results_update = val(current$search$search_results),
    deduplicated_records = val(current$counts$deduplicated_records),
    deduplicated_update = delta_value(current$counts$deduplicated_records, prev$deduplicated_records),
    enriched_records = val(current$counts$enriched_records),
    enriched_update = delta_value(current$counts$enriched_records, prev$enriched_records),
    retracted_records = val(current$counts$retraction_exclusions),
    retracted_update = delta_value(current$counts$retraction_exclusions, prev$retraction_exclusions),
    screened_include = val(current$counts$screened_include),
    screened_exclude = val(current$counts$screened_exclude),
    screened_include_update = delta_value(current$counts$screened_include, prev$screened_include),
    screened_exclude_update = delta_value(current$counts$screened_exclude, prev$screened_exclude),
    species_records = val(current$counts$species_records),
    species_update = delta_value(current$counts$species_records, prev$species_records),
    geography_with = val(current$counts$geography$with),
    geography_without = val(current$counts$geography$without),
    geography_with_update = delta_value(current$counts$geography$with, (prev$geography %||% list())$with),
    geography_without_update = delta_value(current$counts$geography$without, (prev$geography %||% list())$without),
    topic_with = val(current$counts$topics$with),
    topic_without = val(current$counts$topics$without),
    topic_with_update = delta_value(current$counts$topics$with, (prev$topics %||% list())$with),
    topic_without_update = delta_value(current$counts$topics$without, (prev$topics %||% list())$without),
    completed_through = val(current$progress$completed_through),
    active_workflow = val(current$progress$active_position),
    status_label = val(current$progress$status_label)
  )
}

user_registry_tab <- function() {
  Sys.getenv("LEM_GOOGLE_USERS_TAB", unset = "users")
}

ensure_sheet_user_registry <- function() {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- user_registry_tab()
  tabs <- sheet_names_cached(ss)
  cols <- ADJUDICATION_SCHEMA$users

  if (!tab %in% tabs) {
    sheet_add_cached(ss, tab)
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
  tabs <- sheet_names_cached(ss)

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


assignment_registry_tab <- function() {
  Sys.getenv("LEM_GOOGLE_ASSIGNMENTS_TAB", unset = "assignments")
}

assignment_audit_tab <- function() {
  Sys.getenv("LEM_GOOGLE_ASSIGNMENT_AUDIT_TAB", unset = "assignment_audit")
}

ensure_sheet_assignment_registry <- function() {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- assignment_registry_tab()
  tabs <- sheet_names_cached(ss)
  cols <- ADJUDICATION_SCHEMA$assignments

  if (!tab %in% tabs) {
    sheet_add_cached(ss, tab)
    empty <- as.data.frame(
      setNames(replicate(length(cols), character(), simplify = FALSE), cols),
      stringsAsFactors = FALSE
    )
    googlesheets4::sheet_write(empty, ss = ss, sheet = tab)
  }
  invisible(TRUE)
}

ensure_sheet_assignment_audit <- function() {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- assignment_audit_tab()
  tabs <- sheet_names_cached(ss)
  cols <- c(
    "event_id","event_at_utc","actor_user_id","action",
    "assignment_id","workflow","task_type","batch_id","case_id",
    "user_id","blind_group","status"
  )

  if (!tab %in% tabs) {
    sheet_add_cached(ss, tab)
    empty <- as.data.frame(
      setNames(replicate(length(cols), character(), simplify = FALSE), cols),
      stringsAsFactors = FALSE
    )
    googlesheets4::sheet_write(empty, ss = ss, sheet = tab)
  }
  invisible(TRUE)
}

sheet_assignment_rows <- function(x) {
  if (is.null(x) || !nrow(x)) return(list())
  missing <- setdiff(ADJUDICATION_SCHEMA$assignments, names(x))
  if (length(missing)) {
    stop("Assignment registry tab missing field(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  out <- lapply(seq_len(nrow(x)), function(i) {
    normalise_assignment_row(as.list(x[i, ADJUDICATION_SCHEMA$assignments, drop = FALSE]))
  })
  validate_assignment_registry(out)
  out
}

read_sheet_assignments <- function(create_if_missing = TRUE) {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- assignment_registry_tab()
  tabs <- sheet_names_cached(ss)

  if (!tab %in% tabs) {
    if (!isTRUE(create_if_missing)) return(list())
    ensure_sheet_assignment_registry()
  }

  x <- googlesheets4::read_sheet(ss, sheet = tab, col_types = "c")
  sheet_assignment_rows(x)
}

assignment_row_signature <- function(x) {
  a <- normalise_assignment_row(x)
  paste(vapply(ADJUDICATION_SCHEMA$assignments, function(nm) a[[nm]], character(1)), collapse = "|")
}

assignment_registry_signature <- function(assignments) {
  assignments <- lapply(assignments %||% list(), normalise_assignment_row)
  if (!length(assignments)) return(digest::digest("", algo = "sha256", serialize = FALSE))
  rows <- vapply(assignments, assignment_row_signature, character(1))
  digest::digest(paste(sort(rows), collapse = "\n"), algo = "sha256", serialize = FALSE)
}

assignment_changes <- function(before, after) {
  before <- lapply(before %||% list(), normalise_assignment_row)
  after <- lapply(after %||% list(), normalise_assignment_row)

  before_by_id <- setNames(before, vapply(before, function(x) x$assignment_id, character(1)))
  after_by_id <- setNames(after, vapply(after, function(x) x$assignment_id, character(1)))
  ids <- union(names(before_by_id), names(after_by_id))

  out <- list()
  for (id in ids) {
    b <- before_by_id[[id]]
    a <- after_by_id[[id]]
    if (is.null(b) && !is.null(a)) {
      out[[length(out) + 1L]] <- list(action = "created", assignment = a)
    } else if (!is.null(b) && is.null(a)) {
      out[[length(out) + 1L]] <- list(action = "removed", assignment = b)
    } else if (!identical(assignment_row_signature(b), assignment_row_signature(a))) {
      action <- if (!identical(b$status, a$status) && identical(a$status, "cancelled")) {
        "cancelled"
      } else {
        "updated"
      }
      out[[length(out) + 1L]] <- list(action = action, assignment = a)
    }
  }
  out
}

append_sheet_assignment_audit <- function(changes, actor_user_id = "") {
  if (!length(changes)) return(invisible(TRUE))
  gs4_auth_from_env()
  ensure_sheet_assignment_audit()
  ss <- sheet_id_from_env()
  tab <- assignment_audit_tab()
  now <- format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")

  rows <- lapply(seq_along(changes), function(i) {
    ch <- changes[[i]]
    a <- normalise_assignment_row(ch$assignment)
    event_id <- paste0(
      "asg-event-",
      substr(digest::digest(
        paste(now, actor_user_id, ch$action, a$assignment_id, i, sep = "|"),
        algo = "sha256", serialize = FALSE
      ), 1L, 24L)
    )
    data.frame(
      event_id = event_id,
      event_at_utc = now,
      actor_user_id = as.character(actor_user_id),
      action = as.character(ch$action),
      assignment_id = a$assignment_id,
      workflow = a$workflow,
      task_type = a$task_type,
      batch_id = a$batch_id,
      case_id = a$case_id,
      user_id = a$user_id,
      blind_group = a$blind_group,
      status = a$status,
      stringsAsFactors = FALSE
    )
  })
  audit <- do.call(rbind, rows)
  googlesheets4::sheet_append(ss, data = audit, sheet = tab)
  invisible(TRUE)
}

write_sheet_assignments <- function(
  assignments,
  actor_user_id = "",
  expected_current_signature = NULL
) {
  assignments <- lapply(assignments %||% list(), normalise_assignment_row)
  validate_assignment_registry(assignments)

  gs4_auth_from_env()
  ensure_sheet_assignment_registry()
  ss <- sheet_id_from_env()
  tab <- assignment_registry_tab()

  before <- read_sheet_assignments(create_if_missing = FALSE)
  before_sig <- assignment_registry_signature(before)
  if (
    !is.null(expected_current_signature) &&
    nzchar(as.character(expected_current_signature)) &&
    !identical(before_sig, as.character(expected_current_signature))
  ) {
    stop("Assignment registry has changed since this dashboard loaded; refresh and try again", call. = FALSE)
  }
  changes <- assignment_changes(before, assignments)
  if (!length(changes)) return(invisible(assignments))

  # Re-read immediately before the write. This is a small optimistic
  # concurrency guard so an administrator cannot silently overwrite a
  # different assignment update that landed after the initial read.
  latest <- read_sheet_assignments(create_if_missing = FALSE)
  if (!identical(before_sig, assignment_registry_signature(latest))) {
    stop("Assignment registry changed during this operation; refresh and try again", call. = FALSE)
  }

  df <- if (length(assignments)) {
    rows <- lapply(assignments, function(a) {
      as.data.frame(as.list(a[ADJUDICATION_SCHEMA$assignments]), stringsAsFactors = FALSE)
    })
    do.call(rbind, rows)
  } else {
    as.data.frame(
      setNames(replicate(length(ADJUDICATION_SCHEMA$assignments), character(), simplify = FALSE), ADJUDICATION_SCHEMA$assignments),
      stringsAsFactors = FALSE
    )
  }

  googlesheets4::sheet_write(df, ss = ss, sheet = tab)

  verify <- read_sheet_assignments(create_if_missing = FALSE)
  if (!identical(assignment_registry_signature(verify), assignment_registry_signature(assignments))) {
    stop("Assignment registry write verification failed", call. = FALSE)
  }

  append_sheet_assignment_audit(changes, actor_user_id = actor_user_id)
  invisible(verify)
}


write_test_queue_tab <- function(tab, rows, expected_batch_id = "") {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tabs <- sheet_names_cached(ss)

  if (tab %in% tabs) {
    existing <- googlesheets4::read_sheet(ss, sheet = tab, col_types = "c")
    existing_batch <- if ("batch_id" %in% names(existing) && nrow(existing)) {
      unique(as.character(existing$batch_id))
    } else character()

    if (
      nzchar(as.character(expected_batch_id)) &&
      length(existing_batch) == 1L &&
      identical(existing_batch[[1L]], as.character(expected_batch_id))
    ) {
      return(invisible("existing_test_queue"))
    }

    stop(
      "Test queue was not created because sheet tab already exists and is not the expected synthetic test queue: ",
      tab,
      call. = FALSE
    )
  }

  sheet_add_cached(ss, tab)
  googlesheets4::sheet_write(rows, ss = ss, sheet = tab)
  invisible("created")
}

test_queue_json <- function(x) {
  jsonlite::toJSON(
    x,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  )
}

create_test_w02_queue <- function(
  tab = Sys.getenv("LEM_W02_QUEUE_TAB", unset = "queue_w02_active")
) {
  cases <- list(
    list(
      review_case_id = "test-w02-001",
      record_id = "test-w02-record-001",
      provider = "Scopus",
      field = "abstract",
      reason = "provider_field_conflict",
      canonical = list(
        title = "Atlantic salmon aquaculture and environmental monitoring",
        abstract = "Canonical abstract retained in the evidence map.",
        doi = "10.0000/test.w02.001"
      ),
      provider_response = list(
        title = "Atlantic salmon aquaculture and environmental monitoring",
        abstract = "Provider abstract supplied for human verification.",
        returned_doi = "10.0000/test.w02.001",
        eid = "2-s2.0-TEST001",
        author_keywords = c("Atlantic salmon", "aquaculture")
      )
    ),
    list(
      review_case_id = "test-w02-002",
      record_id = "test-w02-record-002",
      provider = "Scopus",
      field = "doi",
      reason = "returned_doi_mismatch",
      canonical = list(
        title = "Rainbow trout farming and water quality",
        abstract = "Canonical metadata for a synthetic test record.",
        doi = "10.0000/test.w02.002"
      ),
      provider_response = list(
        title = "Rainbow trout farming and water quality",
        abstract = "Synthetic provider response used only for assignment testing.",
        returned_doi = "10.0000/test.w02.WRONG",
        eid = "2-s2.0-TEST002",
        author_keywords = c("rainbow trout", "water quality")
      )
    )
  )

  json <- vapply(cases, test_queue_json, character(1))
  payload <- paste0(paste(json, collapse = "\n"), "\n")
  sha <- digest::digest(payload, algo = "sha256", serialize = FALSE)
  batch_id <- "w02-test-assignment-smoke"

  rows <- data.frame(
    batch_id = rep(batch_id, length(cases)),
    queue_sha256 = rep(sha, length(cases)),
    case_index = as.character(seq_along(cases)),
    review_case_id = vapply(cases, function(x) x$review_case_id, character(1)),
    case_json = json,
    stringsAsFactors = FALSE
  )
  write_test_queue_tab(tab, rows, expected_batch_id = batch_id)
  invisible(list(batch_id = batch_id, queue_sha256 = sha, cases = cases))
}

create_test_w04_queue <- function(
  tab = Sys.getenv("LEM_W04_TEST_QUEUE_TAB", unset = "queue_w04_test_active")
) {
  cases <- list(
    list(
      review_case_id="test-w04-001",record_id="test-w04-record-001",random_order=1L,
      screening=list(model_decision="retain"),
      bibliographic=list(
        title="Sea-cage production of Atlantic salmon and benthic impacts in a Norwegian fjord",
        authors="Larsen A; Moen B",year="2024",journal="Aquaculture Environment Interactions",
        volume="16",pages="101-119",doi="10.0000/test.w04.001",
        abstract="We assessed benthic organic enrichment beneath commercial Atlantic salmon farms using sediment chemistry and infaunal indicators. Sampling was conducted around sea cages throughout a full production cycle in western Norway.",
        keywords="Atlantic salmon; aquaculture; sea cages; benthic impact; Norway"
      )
    ),
    list(
      review_case_id="test-w04-002",record_id="test-w04-record-002",random_order=2L,
      screening=list(model_decision="exclude"),
      bibliographic=list(
        title="Juvenile salmon migration through a regulated river catchment",
        authors="Evans C; Morgan D",year="2021",journal="Freshwater Biology",
        volume="66",pages="881-895",doi="10.0000/test.w04.002",
        abstract="Telemetry was used to examine migration timing of wild juvenile salmon through a regulated river. The study did not investigate aquaculture, farming, cages, pens or other commercial production systems.",
        keywords="wild salmon; migration; river; telemetry"
      )
    ),
    list(
      review_case_id="test-w04-003",record_id="test-w04-record-003",random_order=3L,
      screening=list(model_decision="retain"),
      bibliographic=list(
        title="Antibiotic use and antimicrobial resistance around intensive rainbow trout farms",
        authors="Petrov I; Silva M",year="2023",journal="Aquaculture",
        volume="574",pages="739614",doi="10.0000/test.w04.003",
        abstract="Water and sediment were sampled upstream and downstream of commercial rainbow trout farming sites to quantify antibiotic residues and antimicrobial resistance genes associated with intensive freshwater aquaculture.",
        keywords="Rainbow trout; fish farm; antimicrobial resistance; aquaculture"
      )
    ),
    list(
      review_case_id="test-w04-004",record_id="test-w04-record-004",random_order=4L,
      screening=list(model_decision="exclude"),
      bibliographic=list(
        title="Performance of hatchery-reared Atlantic salmon following river release",
        authors="Nielsen J; Berg K",year="2020",journal="Fisheries Research",
        volume="229",pages="105617",doi="10.0000/test.w04.004",
        abstract="Survival and return rates were estimated for Atlantic salmon produced in a conservation hatchery and released as juveniles. No grow-out farming or commercial aquaculture production was studied.",
        keywords="Atlantic salmon; hatchery; stocking; fisheries"
      )
    ),
    list(
      review_case_id="test-w04-005",record_id="test-w04-record-005",random_order=5L,
      screening=list(model_decision="retain"),
      bibliographic=list(
        title="Welfare outcomes after stocking-density changes in farmed Chinook salmon",
        authors="Chen R; Walker P",year="2025",journal="Aquaculture Reports",
        volume="38",pages="102201",doi="10.0000/test.w04.005",
        abstract="Farmed Chinook salmon held in marine pens were exposed to three stocking densities. Fin damage, growth, mortality and behavioural indicators were measured over twelve weeks.",
        keywords="Chinook salmon; mariculture; stocking density; welfare"
      )
    ),
    list(
      review_case_id="test-w04-006",record_id="test-w04-record-006",random_order=6L,
      screening=list(model_decision="exclude"),
      bibliographic=list(
        title="Recreational angler preferences for salmon fishing regulations",
        authors="Jones H; Patel S",year="2019",journal="Marine Policy",
        volume="108",pages="103626",doi="10.0000/test.w04.006",
        abstract="A stated-preference survey examined how recreational anglers value catch limits, season length and access rules for salmon fisheries. Aquaculture and farm production were outside the scope of the study.",
        keywords="salmon fishery; angling; recreation; regulation"
      )
    ),
    list(
      review_case_id="test-w04-007",record_id="test-w04-record-007",random_order=7L,
      screening=list(model_decision="retain"),
      bibliographic=list(
        title="Escaped farmed salmon and genetic introgression near coastal aquaculture facilities",
        authors="Olsen T; Fraser D",year="2022",journal="Conservation Genetics",
        volume="23",pages="455-471",doi="10.0000/test.w04.007",
        abstract="Genetic markers were used to estimate introgression in wild Atlantic salmon populations located near commercial net-pen farms. The analysis linked observed admixture to documented escape events from aquaculture facilities.",
        keywords="farmed salmon; escapees; genetics; net pens; aquaculture"
      )
    ),
    list(
      review_case_id="test-w04-008",record_id="test-w04-record-008",random_order=8L,
      screening=list(model_decision="uncertain"),
      bibliographic=list(
        title="Feed ingredients and nutrient retention in salmonid production systems",
        authors="Garcia L; Ahmed N",year="2026",journal="Animal Feed Science and Technology",
        volume="321",pages="116023",doi="10.0000/test.w04.008",
        abstract="Experimental diets containing insect meal were evaluated in salmonids under controlled production conditions. The abstract refers to commercial farming applications but does not clearly state whether the study animals were reared in a farm-scale aquaculture setting.",
        keywords="salmonid; feed; farming; insect meal; nutrient retention"
      )
    )
  )

  json <- vapply(cases,test_queue_json,character(1))
  payload <- paste0(paste(json,collapse="\n"),"\n")
  sha <- digest::digest(payload,algo="sha256",serialize=FALSE)
  batch_id <- "w04-test-manual-screening"
  rows <- data.frame(
    batch_id=rep(batch_id,length(cases)),
    queue_sha256=rep(sha,length(cases)),
    case_index=as.character(seq_along(cases)),
    review_case_id=vapply(cases,function(x)x$review_case_id,character(1)),
    case_json=json,
    highlight_include_json=rep(jsonlite::toJSON(c(
      "farm","farmed","farming","aquaculture","mariculture","cage","cages","pen","pens"
    ),auto_unbox=FALSE),length(cases)),
    highlight_exclude_json=rep(jsonlite::toJSON(c(
      "hatchery","hatcheries","recreational","angler","angling","fishery","fisheries"
    ),auto_unbox=FALSE),length(cases)),
    review_mode=rep("manual_screening_test",length(cases)),
    source_run_id=rep("",length(cases)),
    stringsAsFactors=FALSE
  )

  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tabs <- sheet_names_cached(ss)
  if (tab %in% tabs) {
    existing <- googlesheets4::read_sheet(ss,sheet=tab,col_types="c")
    batches <- if(nrow(existing) && "batch_id" %in% names(existing)) unique(as.character(existing$batch_id)) else character()
    if(length(batches)==1L && identical(batches[[1L]],batch_id)) {
      googlesheets4::sheet_write(rows,ss=ss,sheet=tab)
      return(invisible(list(batch_id=batch_id,queue_sha256=sha,cases=cases,tab=tab)))
    }
    stop("W04 test queue tab exists but is not the expected synthetic test queue",call.=FALSE)
  }
  sheet_add_cached(ss,tab)
  googlesheets4::sheet_write(rows,ss=ss,sheet=tab)
  invisible(list(batch_id=batch_id,queue_sha256=sha,cases=cases,tab=tab))
}

create_test_w08_queue <- function(
  tab = Sys.getenv("LEM_W08_QUEUE_TAB", unset = "queue_w08_active")
) {
  issue1 <- list(
    issue_type = "zero_topic_eligibility_uncertain",
    allowed_human_outcomes = c("include_uncoded", "exclude_record"),
    automated_value = list()
  )
  issue1$issue_state_sha256 <- digest::digest(
    test_queue_json(issue1), algo = "sha256", serialize = FALSE
  )

  issue2 <- list(
    issue_type = "species_none",
    allowed_human_outcomes = c("assign_named_species", "assign_unspecified_species", "exclude_record"),
    automated_value = list()
  )
  issue2$issue_state_sha256 <- digest::digest(
    test_queue_json(issue2), algo = "sha256", serialize = FALSE
  )

  cases <- list(
    list(
      record_id = "test-w08-record-001",
      title = "Synthetic salmon farming topic annotation record",
      abstract = "A synthetic record for testing Workflow 08 reviewer assignment and completion.",
      issues = list(issue1)
    ),
    list(
      record_id = "test-w08-record-002",
      title = "Synthetic farmed salmon species annotation record",
      abstract = "A second synthetic record for testing single-reviewer Workflow 08 assignment.",
      issues = list(issue2)
    )
  )

  json <- vapply(cases, test_queue_json, character(1))
  payload <- paste0(paste(json, collapse = "\n"), "\n")
  sha <- digest::digest(payload, algo = "sha256", serialize = FALSE)
  batch_id <- "w08-test-assignment-smoke"

  rows <- data.frame(
    batch_id = rep(batch_id, length(cases)),
    queue_sha256 = rep(sha, length(cases)),
    case_index = as.character(seq_along(cases)),
    record_id = vapply(cases, function(x) x$record_id, character(1)),
    case_json = json,
    source_run_id = rep("", length(cases)),
    species_options_json = c(
      jsonlite::toJSON(
        c(
          "Atlantic salmon",
          "Rainbow trout",
          "Chinook salmon",
          "Coho salmon",
          "Sockeye salmon",
          "Chum salmon",
          "Pink salmon",
          "Masu salmon",
          "Unspecified species"
        ),
        auto_unbox = FALSE
      ),
      rep("", max(0L, length(cases) - 1L))
    ),
    topic_options_json = rep("[]", length(cases)),
    stringsAsFactors = FALSE
  )
  write_test_queue_tab(tab, rows, expected_batch_id = batch_id)
  invisible(list(batch_id = batch_id, queue_sha256 = sha, cases = cases))
}

start_fresh_test_w08_queue <- function(
  tab = Sys.getenv("LEM_W08_QUEUE_TAB", unset = "queue_w08_active")
) {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tabs <- sheet_names_cached(ss)
  if (!tab %in% tabs) {
    return(create_test_w08_queue(tab))
  }

  existing <- googlesheets4::read_sheet(ss, sheet = tab, col_types = "c")
  existing_batch <- if ("batch_id" %in% names(existing) && nrow(existing)) {
    unique(as.character(existing$batch_id))
  } else character()

  if (
    length(existing_batch) != 1L ||
    !grepl("^w08-test-assignment-smoke", existing_batch[[1L]])
  ) {
    stop("Fresh test batch refused because the active W08 queue is not a synthetic test queue", call. = FALSE)
  }

  stamp <- format(Sys.time(), tz = "UTC", format = "%Y%m%d%H%M%S")
  batch_id <- paste0("w08-test-assignment-smoke-", stamp)

  make_issue <- function(issue_type, outcomes, automated_value = list()) {
    z <- list(
      issue_type = issue_type,
      allowed_human_outcomes = outcomes,
      automated_value = automated_value
    )
    z$issue_state_sha256 <- digest::digest(
      test_queue_json(z), algo = "sha256", serialize = FALSE
    )
    z
  }

  cases <- list(
    list(
      record_id = paste0("test-w08-", stamp, "-001"),
      title = "Seasonal changes in water quality around commercial Atlantic salmon farms in northern Scotland",
      abstract = paste(
        "This synthetic study follows dissolved oxygen, nutrients and plankton communities around marine salmon cages over two production cycles.",
        "Samples were collected at farm sites and reference stations to test a deliberately realistic Workflow 08 topic-eligibility record with several aquaculture terms embedded in longer prose."
      ),
      issues = list(make_issue(
        "zero_topic_eligibility_uncertain",
        c("include_uncoded", "exclude_record")
      ))
    ),
    list(
      record_id = paste0("test-w08-", stamp, "-002"),
      title = "Growth and sea-lice susceptibility of farmed Atlantic salmon and rainbow trout under contrasting stocking densities",
      abstract = paste(
        "Atlantic salmon (Salmo salar) and rainbow trout were reared in replicated aquaculture pens under low and high stocking densities.",
        "The experiment measured growth, fin condition and parasite burden, and is designed to verify that common and scientific salmonid names are highlighted during species adjudication."
      ),
      issues = list(make_issue(
        "species_none",
        c("assign_named_species", "assign_unspecified_species", "exclude_record")
      ))
    ),
    list(
      record_id = paste0("test-w08-", stamp, "-003"),
      title = "Environmental monitoring of salmon aquaculture across Norway, Scotland and the Faroe Islands",
      abstract = paste(
        "The review discusses monitoring programmes in several North Atlantic regions, including Scotland and the Faroe Islands.",
        "For the focal empirical study, however, samples were collected from coastal farms in western Norway during the 2024 production season.",
        "This deliberately includes several country names so that only the model evidence phrase should receive the geography-evidence highlight."
      ),
      issues = list(make_issue(
        "geography_unresolved",
        c("accept_model", "override_country_set", "assign_none"),
        automated_value = list(
          luna_iso3c = c("NOR"),
          luna_country_names = c("Norway"),
          luna_evidence = c("samples were collected from coastal farms in western Norway")
        )
      ))
    ),
    list(
      record_id = paste0("test-w08-", stamp, "-004"),
      title = "Interactions between feed conversion, fish welfare and benthic deposition in intensive salmon farming",
      abstract = paste(
        "A multi-site study examined production growth, animal-health indicators and sediment enrichment beneath salmon cages.",
        "The record intentionally spans several plausible coding branches so that controlled replacement of an extreme-disagreement topic set can be tested using a realistic title and abstract."
      ),
      issues = list(make_issue(
        "topic_extreme_disagreement",
        c("accept_retained_topics", "replace_topic_set", "exclude_record", "no_code"),
        automated_value = list(
          retained_path_ids = c("production_growth","environment_water")
        )
      ))
    )
  )

  json <- vapply(cases, test_queue_json, character(1))
  payload <- paste0(paste(json, collapse = "\n"), "\n")
  sha <- digest::digest(payload, algo = "sha256", serialize = FALSE)
  species <- c(
    "Atlantic salmon","Rainbow trout","Chinook salmon","Coho salmon",
    "Sockeye salmon","Chum salmon","Pink salmon","Masu salmon","Unspecified species"
  )

  rows <- data.frame(
    batch_id = rep(batch_id, length(cases)),
    queue_sha256 = rep(sha, length(cases)),
    case_index = as.character(seq_along(cases)),
    record_id = vapply(cases, function(x) x$record_id, character(1)),
    case_json = json,
    source_run_id = rep("", length(cases)),
    species_options_json = c(
      jsonlite::toJSON(species, auto_unbox = FALSE),
      rep("", max(0L, length(cases) - 1L))
    ),
    topic_options_json = c(
      jsonlite::toJSON(
        list(
          list(path_id="production_growth", hierarchy_path="Production > Growth"),
          list(path_id="animal_health", hierarchy_path="Animal health"),
          list(path_id="environment_water", hierarchy_path="Environment > Water")
        ),
        auto_unbox = TRUE
      ),
      rep("", max(0L, length(cases) - 1L))
    ),
    stringsAsFactors = FALSE
  )

  googlesheets4::sheet_write(rows, ss = ss, sheet = tab)
  invisible(list(batch_id = batch_id, queue_sha256 = sha, cases = cases))
}

test_queue_tab_exists <- function(tab) {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab %in% sheet_names_cached(ss)
}


backend_reset_operational_tabs <- function() {
  unique(c(
    "queue_w01_active",
    "queue_w01_legacy_730",
    sheet_decision_tab(),
    Sys.getenv("LEM_W02_QUEUE_TAB", unset = "queue_w02_active"),
    w02_decision_tab(),
    Sys.getenv("LEM_W02_RESUME_REQUEST_TAB", unset = "w02_resume_requests"),
    Sys.getenv("LEM_W04_QUEUE_TAB", unset = "queue_w04_validation_active"),
    w04_decision_tab(),
    w04_validation_finalize_request_tab(),
    Sys.getenv("LEM_W04_RESOLUTION_QUEUE_TAB", unset = "queue_w04_resolution_active"),
    w04_resolution_decision_tab(),
    w04_resolution_abstract_edit_tab(),
    Sys.getenv("LEM_W04_CONFLICT_QUEUE_TAB", unset = "queue_w04_conflict_active"),
    w04_conflict_decision_tab(),
    Sys.getenv("LEM_W04_TEST_QUEUE_TAB", unset = "queue_w04_test_active"),
    w04_consistency_analysis_tab(),
    w04_human_kappa_registry_tab(),
    w04_conflict_set_tab(),
    Sys.getenv("LEM_W08_QUEUE_TAB", unset = "queue_w08_active"),
    w08_decision_tab(),
    assignment_registry_tab(),
    assignment_audit_tab(),
    batch_status_tab(),
    pipeline_status_tab()
  ))
}

backend_reset_queue_specs <- function() {
  list(
    list(stage = "01", tab = "queue_w01_active"),
    list(stage = "01", tab = "queue_w01_legacy_730"),
    list(stage = "02", tab = Sys.getenv("LEM_W02_QUEUE_TAB", unset = "queue_w02_active")),
    list(stage = "04", tab = Sys.getenv("LEM_W04_QUEUE_TAB", unset = "queue_w04_validation_active")),
    list(stage = "04", tab = Sys.getenv("LEM_W04_RESOLUTION_QUEUE_TAB", unset = "queue_w04_resolution_active")),
    list(stage = "04", tab = Sys.getenv("LEM_W04_CONFLICT_QUEUE_TAB", unset = "queue_w04_conflict_active")),
    list(stage = "04", tab = Sys.getenv("LEM_W04_TEST_QUEUE_TAB", unset = "queue_w04_test_active")),
    list(stage = "08", tab = Sys.getenv("LEM_W08_QUEUE_TAB", unset = "queue_w08_active"))
  )
}

backend_reset_is_synthetic_batch <- function(batch_id) {
  z <- tolower(trimws(as.character(batch_id %||% "")))
  nzchar(z) && (
    grepl("^test[-_]", z) ||
    grepl("^w0[1248]-test", z) ||
    grepl("synthetic", z, fixed = TRUE)
  )
}

backend_reset_blockers <- function() {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tabs <- sheet_names_cached(ss)
  blockers <- character()

  for (spec in backend_reset_queue_specs()) {
    tab <- as.character(spec$tab)
    if (!nzchar(tab) || !tab %in% tabs) next
    x <- googlesheets4::read_sheet(ss, sheet = tab, col_types = "c")
    if (!nrow(x)) next
    if (!all(c("batch_id", "queue_sha256") %in% names(x))) {
      blockers <- c(blockers, paste0(tab, ": queue schema is not recognised"))
      next
    }
    batches <- unique(trimws(as.character(x$batch_id)))
    hashes <- unique(tolower(trimws(as.character(x$queue_sha256))))
    batches <- batches[nzchar(batches)]
    hashes <- hashes[nzchar(hashes)]
    if (length(batches) != 1L || length(hashes) != 1L) {
      blockers <- c(blockers, paste0(tab, ": ambiguous batch/SHA state"))
      next
    }
    if (backend_reset_is_synthetic_batch(batches[[1L]])) next
    status <- latest_batch_status(spec$stage, batches[[1L]], hashes[[1L]])
    if (!identical(status, "consumed")) {
      blockers <- c(
        blockers,
        sprintf("%s: production batch %s is %s", tab, batches[[1L]], if(nzchar(status)) status else "not marked consumed")
      )
    }
  }
  unique(blockers)
}

archive_backend_queue_to_zenodo <- function(created_by = "") {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  token <- Sys.getenv("ZENODO_ACCESS_TOKEN", unset = "")
  if (!nzchar(token)) stop("ZENODO_ACCESS_TOKEN is not configured; backend reset refused", call. = FALSE)

  tabs <- sheet_names_cached(ss)
  archive_tabs <- intersect(backend_reset_operational_tabs(), tabs)
  td <- tempfile("lem-shiny-backend-archive-")
  dir.create(td, recursive = TRUE)
  data_dir <- file.path(td, "tabs")
  dir.create(data_dir)

  manifest_tabs <- list()
  for (tab in archive_tabs) {
    x <- googlesheets4::read_sheet(ss, sheet = tab, col_types = "c")
    safe_name <- gsub("[^A-Za-z0-9._-]+", "_", tab)
    p <- file.path(data_dir, paste0(safe_name, ".csv"))
    utils::write.csv(x, p, row.names = FALSE, na = "")
    manifest_tabs[[tab]] <- list(
      rows = nrow(x),
      columns = ncol(x),
      csv = basename(p),
      sha256 = digest::digest(file = p, algo = "sha256", serialize = FALSE)
    )
  }

  archived_at <- format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  manifest <- list(
    schema = "living-evidence-map-shiny-backend-archive-v1",
    archived_at_utc = archived_at,
    created_by = as.character(created_by),
    sheet_id_sha256 = digest::digest(as.character(ss), algo = "sha256", serialize = FALSE),
    excluded_tabs = intersect(c(user_registry_tab()), tabs),
    tabs = manifest_tabs
  )
  manifest_path <- file.path(td, "manifest.json")
  writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE, null = "null"), manifest_path, useBytes = TRUE)

  archive_path <- file.path(td, paste0("living-evidence-map-shiny-backend-", format(Sys.time(), tz="UTC", format="%Y%m%dT%H%M%SZ"), ".tar.gz"))
  oldwd <- getwd()
  on.exit(setwd(oldwd), add = TRUE)
  setwd(td)
  utils::tar(basename(archive_path), files = c("manifest.json", "tabs"), compression = "gzip", tar = "internal")
  setwd(oldwd)
  if (!file.exists(archive_path) || file.info(archive_path)$size <= 0) stop("Backend archive bundle was not created", call. = FALSE)

  api <- "https://zenodo.org/api/deposit/depositions"
  auth <- function(req) req |> httr2::req_headers(Authorization = paste("Bearer", token))
  perform <- function(req, expected, label, timeout = 120) {
    resp <- req |> httr2::req_timeout(timeout) |> httr2::req_error(is_error = function(resp) FALSE) |> httr2::req_perform()
    status <- httr2::resp_status(resp)
    if (!status %in% expected) {
      body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
      stop(sprintf("Zenodo %s HTTP %d: %s", label, status, body), call. = FALSE)
    }
    resp
  }

  created <- perform(
    httr2::request(api) |>
      httr2::req_method("POST") |>
      auth() |>
      httr2::req_headers("Content-Type" = "application/json") |>
      httr2::req_body_raw(charToRaw("{}"), type = "application/json"),
    201L, "draft creation"
  ) |> httr2::resp_body_json(simplifyVector = FALSE)

  dep_id <- as.character(created$id)
  bucket <- as.character(created$links$bucket)
  metadata <- list(metadata = list(
    title = paste0("Living Evidence Map Shiny adjudication backend archive | ", substr(archived_at, 1L, 10L)),
    upload_type = "dataset",
    publication_date = format(Sys.Date(), "%Y-%m-%d"),
    description = "<p>Restricted operational archive of the Living Evidence Map Shiny adjudication backend immediately before an administrator reset. User registry data are excluded.</p>",
    creators = list(list(name = "Haddaway, Neal")),
    access_right = "restricted",
    access_conditions = "Operational adjudication provenance archive. Access is restricted.",
    keywords = list("Living Evidence Map", "Shiny", "adjudication", "backend archive")
  ))

  perform(
    httr2::request(paste0(api, "/", dep_id)) |>
      httr2::req_method("PUT") |>
      auth() |>
      httr2::req_headers("Content-Type" = "application/json") |>
      httr2::req_body_json(metadata, auto_unbox = TRUE),
    200L, "metadata update"
  )

  uploaded <- perform(
    httr2::request(paste0(bucket, "/", URLencode(basename(archive_path), reserved = TRUE))) |>
      httr2::req_method("PUT") |>
      auth() |>
      httr2::req_headers(Expect = "") |>
      httr2::req_body_file(archive_path),
    c(200L, 201L), "archive upload", timeout = 600
  ) |> httr2::resp_body_json(simplifyVector = FALSE)

  published <- perform(
    httr2::request(paste0(api, "/", dep_id, "/actions/publish")) |>
      httr2::req_method("POST") |>
      auth(),
    c(200L, 201L, 202L), "publish"
  ) |> httr2::resp_body_json(simplifyVector = FALSE)

  record_id <- as.character(published$record_id %||% published$id %||% dep_id)
  list(
    record_id = record_id,
    doi = as.character(published$doi %||% ""),
    archive_sha256 = digest::digest(file = archive_path, algo = "sha256", serialize = FALSE),
    archived_at_utc = archived_at,
    tabs = archive_tabs
  )
}

reset_backend_queue_state <- function(created_by = "") {
  if (!identical(storage_backend(), "google_sheets")) {
    stop("Backend reset is only available with the Google Sheets backend", call. = FALSE)
  }
  blockers <- backend_reset_blockers()
  if (length(blockers)) {
    stop(
      paste(c("Backend reset refused because live production queue state remains:", blockers), collapse = "\n"),
      call. = FALSE
    )
  }

  receipt <- archive_backend_queue_to_zenodo(created_by = created_by)

  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tabs <- sheet_names_cached(ss)
  targets <- intersect(backend_reset_operational_tabs(), tabs)
  for (tab in targets) {
    googlesheets4::sheet_delete(ss, sheet = tab)
    invalidate_sheet_names_cache(ss)
  }

  list(
    archived = receipt,
    deleted_tabs = targets,
    preserved_tabs = intersect(c(user_registry_tab()), sheet_names_cached(ss))
  )
}
