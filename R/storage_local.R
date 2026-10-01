read_local_decisions <- function(path) {
  if (!file.exists(path)) return(list())
  lines <- readLines(path, warn=FALSE, encoding="UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) return(list())
  lapply(lines, jsonlite::fromJSON, simplifyVector=FALSE)
}

write_local_decision <- function(path, decision) {
  current <- read_local_decisions(path)
  ids <- if (length(current)) vapply(current, function(x) as.character(x$review_case_id), character(1)) else character()
  pos <- match(as.character(decision$review_case_id), ids)
  if (is.na(pos)) current <- c(current, list(decision)) else current[[pos]] <- decision

  dir.create(dirname(path), recursive=TRUE, showWarnings=FALSE)
  tmp <- tempfile(pattern="w01-decisions-", tmpdir=dirname(path), fileext=".jsonl")
  con <- file(tmp, "wt", encoding="UTF-8")
  for (d in current) writeLines(jsonlite::toJSON(d, auto_unbox=TRUE, null="null", na="null"), con, useBytes=TRUE)
  close(con)
  if (!file.rename(tmp, path)) stop("Could not atomically replace local decision store", call.=FALSE)
  invisible(TRUE)
}
