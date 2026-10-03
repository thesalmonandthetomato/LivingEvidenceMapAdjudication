`%||%` <- function(x,y) if (is.null(x) || length(x)==0L) y else x

read_w01_cases <- function(path) {
  if (!file.exists(path)) stop("W01 queue not found: ", path, call.=FALSE)
  lines <- readLines(path, warn=FALSE, encoding="UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  cases <- lapply(lines, jsonlite::fromJSON, simplifyVector=FALSE)
  if (!length(cases)) stop("W01 queue is empty", call.=FALSE)
  ids <- vapply(cases, function(x) as.character(x$review_case_id %||% ""), character(1))
  if (any(!nzchar(ids)) || anyDuplicated(ids)) stop("Invalid/duplicate W01 review_case_id", call.=FALSE)
  for (z in cases) {
    if (!identical(z$schema, "living-evidence-map-workflow01-duplicate-adjudication-case-v1"))
      stop("Unsupported W01 adjudication schema", call.=FALSE)
    if (is.null(z$record_i) || is.null(z$record_j))
      stop("W01 case missing record_i/record_j", call.=FALSE)
  }
  cases
}

fmt_value <- function(x) {
  if (is.null(x) || length(x)==0L || is.na(x[[1L]])) return("")
  if (is.logical(x)) return(ifelse(isTRUE(x), "Yes", "No"))
  if (is.numeric(x)) return(format(round(x[[1L]], 3), trim=TRUE))
  as.character(x[[1L]])
}

export_w01_decisions <- function(decision_path, output_path) {
  ds <- read_local_decisions(decision_path)
  if (!length(ds)) stop("No decisions to export", call.=FALSE)
  ids <- vapply(ds, function(x) as.character(x$review_case_id), character(1))
  if (anyDuplicated(ids)) stop("Decision store contains duplicate active case IDs", call.=FALSE)
  valid <- c("duplicate","not_duplicate","uncertain")
  for (d in ds) {
    stopifnot(d$decision %in% valid)
    for (f in c("rationale","reviewer","resolved_at_utc","queue_sha256"))
      if (is.null(d[[f]]) || !nzchar(trimws(as.character(d[[f]]))))
        stop("Decision missing required field: ", f, call.=FALSE)
  }
  dir.create(dirname(output_path), recursive=TRUE, showWarnings=FALSE)
  con <- file(output_path, "wt", encoding="UTF-8")
  on.exit(close(con), add=TRUE)
  for (d in ds) writeLines(jsonlite::toJSON(d, auto_unbox=TRUE, null="null", na="null"), con, useBytes=TRUE)
  invisible(output_path)
}
