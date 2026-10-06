app <- paste(readLines("app.R", warn=FALSE, encoding="UTF-8"), collapse="\n")
storage <- paste(readLines("R/storage_sheets.R", warn=FALSE, encoding="UTF-8"), collapse="\n")

stopifnot(grepl('w04_resolution_edit_abstract', app, fixed=TRUE))
stopifnot(grepl('w04_resolution_save_abstract', app, fixed=TRUE))
stopifnot(grepl('w04_resolution_abstract_text', app, fixed=TRUE))
stopifnot(grepl('append_sheet_w04_resolution_abstract_edit', app, fixed=TRUE))
stopifnot(grepl('active_sheet_w04_resolution_abstract_edits', app, fixed=TRUE))
stopifnot(grepl('w04_resolution_abstract_edits', storage, fixed=TRUE))
stopifnot(grepl('supersedes_edit_id', storage, fixed=TRUE))
stopifnot(grepl('Saved abstracts are carried into the canonical record when W04 is sent to GitHub.', app, fixed=TRUE))

cat("PASS: W04 model-resolution abstract editing contract\n")
