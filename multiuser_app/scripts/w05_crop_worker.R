#!/usr/bin/env Rscript
# Offline deterministic W05 worker, deliberately not connected to workflow dispatch.
# Run from multiuser_app/: Rscript scripts/w05_crop_worker.R --input path.jsonl
# --dictionary path.csv --output-dir output/ [--subset-count 100 | --full-corpus]
source("R/w01_contract.R")
source("R/w05_crop_coding.R")
args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default="") {
  i <- match(flag,args)
  if(is.na(i))return(default)
  if(i==length(args))stop("Missing value after ",flag,call.=FALSE)
  args[[i+1L]]
}
input <- arg("--input")
dictionary_path <- arg("--dictionary")
output <- arg("--output-dir")
count <- arg("--subset-count")
full <- "--full-corpus"%in%args
if(!nzchar(input)||!file.exists(input)||!nzchar(dictionary_path)||
   !file.exists(dictionary_path)||!nzchar(output))
  stop("--input, --dictionary and --output-dir are required",call.=FALSE)
if(identical(full,nzchar(count)))stop("Select exactly one of --subset-count or --full-corpus",call.=FALSE)
if(nzchar(count) && (!grepl("^[0-9]+$",count)||as.integer(count)<1L))
  stop("Subset count must be positive",call.=FALSE)
dict <- w05_crop_read(dictionary_path)
sha <- digest::digest(file=dictionary_path,algo="sha256",serialize=FALSE)
lines <- readLines(input,warn=FALSE,encoding="UTF-8")
lines <- lines[nzchar(trimws(lines))]
if(!length(lines))stop("No W05 input records",call.=FALSE)
parse_record <- function(line) {
  x <- jsonlite::fromJSON(line,simplifyVector=FALSE)
  scalar <- function(y) {
    if(is.null(y)||!length(y))return("")
    as.character(y[[1L]])
  }
  data.frame(record_id=scalar(x$record_id %||% (x$identity %||% list())$record_id),
    title=scalar(x$title %||% (x$canonical %||% list())$title),
    abstract=scalar(x$abstract %||% (x$canonical %||% list())$abstract),
    stringsAsFactors=FALSE)
}
rows <- do.call(rbind,lapply(lines,parse_record))
if(any(!nzchar(rows$record_id))||anyDuplicated(rows$record_id))
  stop("Record IDs must be present and unique",call.=FALSE)
original_count <- nrow(rows)
if(!full)rows <- head(rows,as.integer(count))
result <- w05_crop_code_records(rows,dict)
stopifnot(length(unique(result$assignments$record_id))==nrow(rows),
          !anyDuplicated(result$assignments[,c("record_id","code")]),
          sum(result$frequencies$records)==nrow(result$assignments))
dir.create(output,recursive=TRUE,showWarnings=FALSE)
utils::write.csv(result$assignments,file.path(output,"w05_crop_assignments.csv"),row.names=FALSE,na="")
utils::write.csv(result$frequencies,file.path(output,"w05_crop_frequencies.csv"),row.names=FALSE,na="")
manifest <- list(schema="lem-w05-crop-test-v1",mode=if(full)"full_corpus" else "subset_test",
  dictionary_sha256=sha,input_sha256=digest::digest(file=input,algo="sha256",serialize=FALSE),
  input_records=original_count,processed_records=nrow(rows),
  unmatched_records=sum(result$assignments$code=="unspecified_horticultural_crop"),
  outputs=c("w05_crop_assignments.csv","w05_crop_frequencies.csv"),
  created_at_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"))
writeLines(jsonlite::toJSON(manifest,pretty=TRUE,auto_unbox=TRUE),
           file.path(output,"w05_crop_manifest.json"))
cat(sprintf("PASS W05 %s: %d records, %d codes, %d unspecified\n",
  manifest$mode,nrow(rows),nrow(dict),manifest$unmatched_records))
