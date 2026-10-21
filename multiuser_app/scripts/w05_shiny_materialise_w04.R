#!/usr/bin/env Rscript
# Shiny-only offline bridge. Requires a verified W04 publication receipt and
# its corresponding checksum-verified canonical and included-ID files.
# Does not publish, dispatch workflows, or modify production state.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
args <- commandArgs(trailingOnly=TRUE)
get <- function(flag) {
  i <- match(flag,args)
  if(is.na(i) || i==length(args))stop(paste("Missing",flag),call.=FALSE)
  args[[i+1L]]
}
canonical <- get("--canonical")
included <- get("--included-ids")
receipt_path <- get("--receipt")
review <- get("--review-id")
output <- get("--output")
manifest_path <- get("--manifest")
for(p in c(canonical,included,receipt_path))
  if(!file.exists(p))stop("W04 source file is absent",call.=FALSE)
receipt <- fromJSON(receipt_path,simplifyVector=FALSE)
scalar <- function(x)if(is.null(x)||length(x)!=1L||is.na(x)) "" else as.character(x)
if(!identical(scalar(receipt$review_id),review) ||
   !identical(scalar(receipt$publication_status),"published") ||
   !identical(scalar(receipt$visibility),"restricted") ||
   !grepl("^[0-9]+$",scalar(receipt$publication_run_id)))
  stop("No verified restricted W04 publication for this review",call.=FALSE)
expected_sha <- scalar(receipt$included_ids_sha256)
actual_sha <- digest(file=included,algo="sha256",serialize=FALSE)
if(!grepl("^[0-9a-f]{64}$",expected_sha) ||
   !identical(actual_sha,expected_sha))
  stop("W04 included-record file SHA256 mismatch",call.=FALSE)
ids <- readLines(included,warn=FALSE,encoding="UTF-8")
ids <- ids[nzchar(trimws(ids))]
expected_n <- suppressWarnings(as.integer(scalar(receipt$retained)))
if(is.na(expected_n)||expected_n<1L||length(ids)!=expected_n ||
   anyDuplicated(ids) || any(!nzchar(ids)))
  stop("W04 retained-record count or IDs invalid",call.=FALSE)
# Only write the target after every identity and checksum is verified.
records <- list()
seen <- character()
input <- file(canonical,"rt",encoding="UTF-8")
on.exit(try(close(input),silent=TRUE),add=TRUE)
repeat {
  lines <- readLines(input,n=500L,warn=FALSE)
  if(!length(lines))break
  for(line in lines) {
    if(!nzchar(trimws(line)))next
    x <- fromJSON(line,simplifyVector=FALSE)
    id <- scalar((x$identity %||% list())$record_id)
    if(id %in% ids) {
      if(id %in% seen)stop("Duplicate canonical record",call.=FALSE)
      seen <- c(seen,id)
      records[[id]] <- line
    }
  }
}
if(length(seen)!=expected_n || !setequal(seen,ids))
  stop("Published W04 included records do not match canonical material",call.=FALSE)
dir.create(dirname(output),recursive=TRUE,showWarnings=FALSE)
# Retain canonical file ordering for reproducibility.
writeLines(unname(unlist(records,use.names=FALSE)),output,useBytes=TRUE)
input_sha <- digest(file=output,algo="sha256",serialize=FALSE)
provenance <- list(schema="lem-w05-shiny-w04-materialisation-v1",
  review_id=review,publication_run_id=scalar(receipt$publication_run_id),
  included_ids_sha256=actual_sha,input_sha256=input_sha,
  record_count=expected_n)
dir.create(dirname(manifest_path),recursive=TRUE,showWarnings=FALSE)
write_json(provenance,manifest_path,auto_unbox=TRUE,pretty=TRUE)
cat(sprintf("PASS W05 Shiny materialisation: %d published W04-included records; SHA256 %s\n",expected_n,input_sha))
