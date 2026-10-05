source("app.R", local = FALSE)

td <- tempfile("w08-kpi-")
dir.create(td, recursive = TRUE)
registry <- file.path(td, "zenodo_registry.csv")
pointer_dir <- file.path(td, "zenodo")
dir.create(pointer_dir)

write.csv(
  data.frame(
    source_run_id = c("111", "222"),
    status = c("superseded", "authoritative"),
    stringsAsFactors = FALSE
  ),
  registry,
  row.names = FALSE
)

jsonlite::write_json(
  list(
    source_github_run_id = "222",
    canonical_records = 21493L,
    doi = "10.5281/zenodo.test"
  ),
  file.path(pointer_dir, "run-222.json"),
  auto_unbox = TRUE,
  pretty = TRUE
)

x <- read_authoritative_w08_metrics(registry, pointer_dir)
stopifnot(
  identical(x$canonical_records, 21493L),
  identical(x$source_run_id, "222"),
  identical(x$doi, "10.5281/zenodo.test")
)

stopifnot(!w08_status_is_final(list(completed_through = "8")))
stopifnot(w08_status_is_final(list(completed_through = "9")))
stopifnot(w08_status_is_final(list(completed_through = "11")))

bad <- read.csv(registry, stringsAsFactors = FALSE)
bad$status <- "superseded"
write.csv(bad, registry, row.names = FALSE)
failed <- FALSE
tryCatch(
  read_authoritative_w08_metrics(registry, pointer_dir),
  error = function(e) failed <<- TRUE
)
stopifnot(failed)

app_src <- paste(readLines("app.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
stopifnot(grepl("fmt_pipeline_n(canonical_value)", app_src, fixed = TRUE))
stopifnot(grepl('if (isTRUE(canonical_current)) "current" else "pre-update"', app_src, fixed = TRUE))

cat("PASS: authoritative W08 canonical KPI contract\n")
