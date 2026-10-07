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

cat("PASS: W08 save path keeps pre-write authority check and avoids redundant post-write full-sheet read\n")
