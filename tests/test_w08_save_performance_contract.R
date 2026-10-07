app <- paste(readLines("app.R", warn=FALSE, encoding="UTF-8"), collapse="\n")
storage <- paste(readLines("R/storage_sheets.R", warn=FALSE, encoding="UTF-8"), collapse="\n")

start <- regexpr("append_sheet_w08_decision <- function", storage, fixed=TRUE)[[1L]]
stopifnot(start > 0L)
rest <- substring(storage, start)
end <- regexpr("\n\npipeline_status_tab <- function", rest, fixed=TRUE)[[1L]]
stopifnot(end > 0L)
block <- substring(rest, 1L, end - 1L)

stopifnot(
  grepl("current_active <- active_sheet_w08_decisions()", block, fixed=TRUE),
  grepl("already been resolved by another reviewer", block, fixed=TRUE),
  grepl("googlesheets4::sheet_append(ss,data=row,sheet=tab)", block, fixed=TRUE),
  grepl("as.list(row[1,,drop=FALSE])", block, fixed=TRUE),
  !grepl("verify <- googlesheets4::read_sheet", block, fixed=TRUE),
  !grepl("hit <- verify", block, fixed=TRUE)
)

w08_start <- regexpr("save_w08_record <- function", app, fixed=TRUE)[[1L]]
stopifnot(w08_start > 0L)
w08_rest <- substring(app, w08_start)
w08_end <- regexpr("\n\n  dispatch_completed_w08 <- function", w08_rest, fixed=TRUE)[[1L]]
stopifnot(w08_end > 0L)
w08_block <- substring(w08_rest, 1L, w08_end - 1L)

stopifnot(
  !grepl('ensure_direct_assignment(\n        "08"', w08_block, fixed=TRUE),
  grepl('assignment_mode_active(assignment_registry_rv(), "08"', w08_block, fixed=TRUE),
  grepl('!session_can("manage_assignments")', w08_block, fixed=TRUE)
)

cat("PASS: W08 save path avoids redundant remote reads and per-record admin assignment writes while retaining reviewer assignment enforcement\n")
