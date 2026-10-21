# Run from multiuser_app with Rscript tests/test_w05_crop_coding.R.
`%||%` <- function(x,y)if(is.null(x))y else x
source("R/w05_crop_coding.R")
d <- w05_crop_read("config/w05_almeria_crop_dictionary.csv")
stopifnot(nrow(d)==48L,identical(tail(d$code,1L),"unspecified_horticultural_crop"),
          identical(tail(d$matching_terms,1L),""))
examples <- data.frame(record_id=c("a","b","c","d"),
  title=c("Tomato and pepper cultivation","Cultivo del pepino",
          "Unrelated text about farm economics","Chilli pepper trial"),
  abstract=c("Solanum lycopersicum","","","jalapeño"),
  stringsAsFactors=FALSE)
x <- w05_crop_code_records(examples,d)
lookup <- split(x$assignments$code[x$assignments$entity=="crop"],x$assignments$record_id[x$assignments$entity=="crop"])
stopifnot(all(c("tomato","pepper")%in%lookup$a),
          identical(lookup$b,"cucumber"),
          identical(lookup$c,"unspecified_horticultural_crop"),
          "chilli_pepper"%in%lookup$d,
          !("unspecified_horticultural_crop"%in%lookup$a),
          sum(x$frequencies$records[x$frequencies$code=="unspecified_horticultural_crop"])==1L)
bad <- d
bad$matching_terms[bad$code=="unspecified_horticultural_crop"] <- "greenhouse"
stopifnot(inherits(try(w05_crop_validate(bad),silent=TRUE),"try-error"))
cat("PASS W05 crop CSV schema, multilingual matches, multi-crop and automatic unmatched fallback\n")

# Two entity types, independent matching and multiple codes on one record.
multi <- rbind(d,data.frame(entity=c("practice","practice"),
  code=c("irrigation","integrated_pest_management"),
  matching_terms=c("irrigation;riego","integrated pest management;IPM"),
  stringsAsFactors=FALSE))
multi_result <- w05_crop_code_records(data.frame(
  record_id=c("multi1","multi2"),
  title=c("Tomato irrigation and IPM","Irrigation system performance"),
  abstract=c("",""),stringsAsFactors=FALSE),multi)
one <- multi_result$assignments[multi_result$assignments$record_id=="multi1",]
two <- multi_result$assignments[multi_result$assignments$record_id=="multi2",]
stopifnot(all(c("tomato")%in%one$code),
          all(c("irrigation","integrated_pest_management")%in%one$code),
          "unspecified_horticultural_crop"%in%two$code,
          !any(two$entity=="practice" & two$code=="unspecified_horticultural_crop"),
          all(c("entity","code","records")%in%names(multi_result$frequencies)))
cat("PASS multi-entity assignments and crop-only fallback\n")

# A term may be relevant in distinct entity types but cannot map to two codes
# inside the same entity.
shared <- rbind(d,data.frame(entity="intervention",code="tomato_training",
  matching_terms="tomato",stringsAsFactors=FALSE))
stopifnot(nrow(w05_crop_validate(shared))==49L)
conflicted <- rbind(d,data.frame(entity="crop",code="duplicate_tomato",
  matching_terms="tomato",stringsAsFactors=FALSE))
stopifnot(inherits(try(w05_crop_validate(conflicted),silent=TRUE),"try-error"))
cat("PASS matching-term conflicts are scoped by entity\n")

# W05 Shiny dispatcher is never permitted to launch without W04 publication,
# and the only allowed endpoint is this repository's isolated Shiny workflow.
source("R/w05_shiny_subset_dispatch.R")
request <- list(review_id="almeria-map",zenodo_draft_id="123456",
  input_filename="w05-input.jsonl",input_sha256=paste(rep("a",64L),collapse=""),
  dictionary_filename="w05-crops.csv",dictionary_sha256=paste(rep("b",64L),collapse=""),
  subset_count="100")
calls <- 0L
w05_shiny_gh_request <- function(url,method="GET",payload=NULL) {
  calls <<- calls+1L
  stopifnot(identical(method,"POST"),
    identical(url,"https://api.github.com/repos/thesalmonandthetomato/LivingEvidenceMap/actions/workflows/w05_shiny_subset.yml/dispatches"),
    identical(payload$ref,"main"),identical(payload$inputs$subset_count,"100"))
  invisible(TRUE)
}
stopifnot(inherits(try(w05_shiny_subset_dispatch(request),silent=TRUE),"try-error"),calls==0L)
stopifnot(inherits(try(w05_shiny_subset_dispatch(request,w04_published=TRUE),silent=TRUE),"try-error"),calls==0L)
w05_shiny_subset_dispatch(request,w04_published=TRUE,schema_approved=TRUE)
stopifnot(calls==1L)
cat("PASS W05 Shiny dispatcher respects verified W04 gate and isolated workflow target\n")

# Verified results reader uses only the Shiny W05 workflow and confirms
# frequencies against record-level assignments. No GitHub/Zenodo calls.
source("R/w05_shiny_subset_results.R")
fake_api <- function(url,method="GET",payload=NULL) {
  if(grepl("/artifacts$",url))
    return(list(artifacts=list(list(name="w05-shiny-subset-12345",expired=FALSE,
      archive_download_url=paste0("https://api.github.com/repos/",
        "thesalmonandthetomato/LivingEvidenceMap/actions/artifacts/999/zip")))))
  list(path=".github/workflows/w05_shiny_subset.yml",
       status="completed",conclusion="success",
       html_url="https://github.com/test/actions/runs/12345")
}
stopifnot(identical(w05_shiny_subset_run_status("12345",fake_api)$conclusion,"success"))
fake_bad_api <- function(url,method="GET",payload=NULL) {
  list(path=".github/workflows/workflow_05_production.yml",
       status="completed",conclusion="success")
}
stopifnot(inherits(try(w05_shiny_subset_run_status("12345",fake_bad_api),silent=TRUE),"try-error"))
cat("PASS W05 results reader restricts origin to Shiny-only workflow\n")


# Approval is written only for the archived matching dictionary and verified again.
# Network dependencies are mocked; no Zenodo credentials or real publications.
source("R/w05_crop_archive.R")
sha <- paste(rep("c",64L),collapse="")
fname <- paste0("w05-crops-",substr(sha,1L,20L),".csv")
meta <- list(review_id="almeria-map")
receipt <- list(deposit_id="123456",filename=fname,sha256=sha)
saved <- list(list(filename=fname))
w05_crop_draft <- function(meta) list(id=123456L,files=saved,
  links=list(bucket="https://zenodo.org/api/files/test"))
w04_prompt_zenodo_request <- function(url,method="GET",file=NULL,expected=NULL) {
  stopifnot(identical(method,"PUT"),grepl(paste0("w05-approved-",sha),url))
  saved <<- append(saved,list(list(filename=paste0("w05-approved-",sha,".json"))))
  invisible(TRUE)
}
w04_prompt_review_metadata <- function(force=FALSE) meta
stopifnot(isTRUE(w05_crop_approve_version(meta,receipt)))
stopifnot(length(saved)==2L)
stopifnot(isTRUE(w05_crop_approve_version(meta,receipt)),length(saved)==2L)
wrong <- receipt
wrong$sha256 <- paste(rep("d",64L),collapse="")
stopifnot(inherits(try(w05_crop_approve_version(meta,wrong),silent=TRUE),"try-error"))
cat("PASS review-scoped W05 approval marker creation, verification and idempotence\\n")

# W04 bridge must reject any corpus/review/publication mismatch.
good_sha <- paste(rep("a",64L),collapse="")
receipt <- list(review_id="almeria-map",visibility="restricted",
  publication_status="published",publication_run_id="12345",
  included_ids_sha256=good_sha,retained=25L)
provenance <- list(review_id="almeria-map",
  included_ids_sha256=good_sha,input_sha256=paste(rep("b",64L),collapse=""),
  record_count=25L)
stopifnot(isTRUE(w05_shiny_verify_w04_bridge(receipt,provenance,"almeria-map")))
bad <- provenance;bad$record_count <- 24L
stopifnot(inherits(try(w05_shiny_verify_w04_bridge(receipt,bad,"almeria-map"),silent=TRUE),"try-error"))
bad <- provenance;bad$included_ids_sha256 <- paste(rep("c",64L),collapse="")
stopifnot(inherits(try(w05_shiny_verify_w04_bridge(receipt,bad,"almeria-map"),silent=TRUE),"try-error"))
bad_receipt <- receipt;bad_receipt$publication_status <- "draft"
stopifnot(inherits(try(w05_shiny_verify_w04_bridge(bad_receipt,provenance,"almeria-map"),silent=TRUE),"try-error"))
cat("PASS isolated W04-to-W05 corpus provenance gate\\n")


# End-to-end offline bridge fixture: correct included IDs are materialised;
# incorrect canonical checksum must fail before creating an output.
tmp <- tempfile("w05-bridge-");dir.create(tmp)
canon <- file.path(tmp,"canonical.jsonl")
idsfile <- file.path(tmp,"included.txt")
recfile <- file.path(tmp,"receipt.json")
out <- file.path(tmp,"w05-input.jsonl")
manifest <- file.path(tmp,"manifest.json")
writeLines(c('{"identity":{"record_id":"r1"},"canonical":{"title":"Tomato"}}',
             '{"identity":{"record_id":"r2"},"canonical":{"title":"Pepper"}}'),
           canon,useBytes=TRUE)
writeLines("r2",idsfile,useBytes=TRUE)
bridge_receipt <- list(review_id="almeria-map",
  publication_status="published",visibility="restricted",publication_run_id="1234",
  upstream_lean_canonical_sha256=digest::digest(file=canon,algo="sha256",serialize=FALSE),
  included_ids_sha256=digest::digest(file=idsfile,algo="sha256",serialize=FALSE),
  retained=1L)
jsonlite::write_json(bridge_receipt,recfile,auto_unbox=TRUE)
run_bridge <- function() {
  suppressWarnings(system2(file.path(R.home("bin"),"Rscript"),
    c("scripts/w05_shiny_materialise_w04.R","--canonical",canon,
      "--included-ids",idsfile,"--receipt",recfile,
      "--review-id","almeria-map","--output",out,"--manifest",manifest),
    stdout=TRUE,stderr=TRUE))
}
ok <- run_bridge()
stopifnot(is.null(attr(ok,"status")) || identical(attr(ok,"status"),0L),
          length(readLines(out))==1L,
          grepl('"r2"',readLines(out),fixed=TRUE))
unlink(c(out,manifest))
bridge_receipt$upstream_lean_canonical_sha256 <- paste(rep("0",64L),collapse="")
jsonlite::write_json(bridge_receipt,recfile,auto_unbox=TRUE)
bad <- run_bridge()
stopifnot(!is.null(attr(bad,"status")),!file.exists(out))
unlink(tmp,recursive=TRUE)
cat("PASS offline published-W04-to-W05 materialisation and fail-closed checksum checks\\n")


# Archive bridge must reject unverifiable input without any Zenodo writes.
bridge_input <- tempfile(fileext=".jsonl")
bridge_prov <- tempfile(fileext=".json")
writeLines('{"identity":{"record_id":"r1"}}',bridge_input)
real_sha <- digest::digest(file=bridge_input,algo="sha256",serialize=FALSE)
bridge_meta <- list(review_id="almeria-map")
bridge_receipt <- list(review_id="almeria-map",visibility="restricted",
  publication_status="published",publication_run_id="123",
  included_ids_sha256=paste(rep("a",64),collapse=""),retained=1L)
bridge_data <- list(review_id="almeria-map",record_count=1L,
  included_ids_sha256=bridge_receipt$included_ids_sha256,
  input_sha256=paste(rep("b",64),collapse=""))
jsonlite::write_json(bridge_data,bridge_prov,auto_unbox=TRUE)
archive_calls <- 0L
w05_crop_draft <- function(meta) {archive_calls <<- archive_calls+1L;stop("Must not reach Zenodo")}
stopifnot(inherits(try(w05_shiny_archive_input(bridge_meta,bridge_input,
  bridge_prov,bridge_receipt),silent=TRUE),"try-error"),archive_calls==0L)
unlink(c(bridge_input,bridge_prov))
cat("PASS invalid W05 input rejected before Zenodo archive activity\\n")


# Handover composition: authenticated schema marker is required before
# materialisation; no real Zenodo upload or GitHub dispatch.
schema_sha <- paste(rep("e",64L),collapse="")
schema_receipt <- list(deposit_id="123456",sha256=schema_sha)
meta <- list(review_id="almeria-map",values=list(w05_crop_zenodo_draft_id="123456"))
w05_crop_draft <- function(meta)list(files=list(),id=123456L)
w05_crop_draft_id <- function(meta)as.character(meta$values$w05_crop_zenodo_draft_id)
missing_source <- tempfile()
stopifnot(inherits(try(w05_shiny_prepare_handover(meta,missing_source,
 missing_source,list(),schema_receipt),silent=TRUE),"try-error"))
# Create complete fixture to prove the approval check fails closed.
dir.create(fixture_dir <- tempfile("w05-handover-"))
cpath <- file.path(fixture_dir,"canonical.jsonl")
ipath <- file.path(fixture_dir,"included.txt")
writeLines('{"identity":{"record_id":"a"}}',cpath)
writeLines("a",ipath)
minimal <- list(review_id="almeria-map",publication_status="published",
 visibility="restricted",publication_run_id="101",retained=1L,
 included_ids_sha256=digest::digest(file=ipath,algo="sha256",serialize=FALSE),
 upstream_lean_canonical_sha256=digest::digest(file=cpath,algo="sha256",serialize=FALSE))
stopifnot(inherits(try(w05_shiny_prepare_handover(meta,cpath,ipath,
 minimal,schema_receipt),silent=TRUE),"try-error"))
unlink(fixture_dir,recursive=TRUE)
cat("PASS W05 handover requires restored sources and approved dictionary before execution\\n")

# The existing global W04 registry has no review_id: never infer that its
# entries belong to the selected Shiny review.
legacy <- paste('"publication_github_run_id","visibility","retained","unresolved"',
                '"100","restricted",5,0',sep="\\n")
state <- w05_shiny_w04_publication_status("almeria-map",legacy)
stopifnot(identical(state$status,"awaiting_review_bound_receipt"))
cat("PASS Shiny does not adopt an unbound global W04 publication\\n")


# Exact Shiny publication run binds an established, unmodified W04 pointer
# to the review. Reject other reviews and the automatic-handoff workflow.
p <- list(workflow="04",state="relevance_screening",status="published",
 visibility="restricted",publication_github_run_id="919",
 retained=12L,zenodo_record_id="5151",
 included_record_ids_sha256=paste(rep("a",64L),collapse=""),
 upstream_lean_canonical_sha256=paste(rep("b",64L),collapse=""))
run <- list(id=919L,conclusion="success",
 path=".github/workflows/w04_shiny_publish_only.yml",
 display_title="W04 Shiny only publication almeria-map bundle.json")
bound <- w05_shiny_bind_w04_receipt(p,"almeria-map",run)
stopifnot(identical(bound$review_id,"almeria-map"),
 identical(bound$included_ids_sha256,p$included_record_ids_sha256),
 identical(bound$publication_status,"published"))
bad <- run;bad$path <- ".github/workflows/w04_shiny_reconciled_publish.yml"
stopifnot(inherits(try(w05_shiny_bind_w04_receipt(p,"almeria-map",bad),
 silent=TRUE),"try-error"))
stopifnot(inherits(try(w05_shiny_bind_w04_receipt(p,"other-map",run),
 silent=TRUE),"try-error"))
cat("PASS existing W04 receipt bound to exact Shiny publication review and run\n")


# Review-bound W04 source restoration, with archive transport stubbed.
# The selected W04 included IDs must match its receipt SHA.
had_archive <- exists("w04_subset_verified_archive",mode="function")
orig_archive <- if(had_archive) w04_subset_verified_archive else NULL
orig_included <- w05_shiny_w04_included_ids
canon_lines <- c('{"identity":{"record_id":"r1"},"canonical":{"title":"Tomato"}}',
                 '{"identity":{"record_id":"r2"},"canonical":{"title":"Pepper"}}')
w04_subset_verified_archive <- function(pointer,kind) {
  stopifnot(identical(kind,"lean"))
  canon_lines
}
w05_shiny_w04_included_ids <- function(pointer)c("r2")
tmp_canon <- tempfile();writeLines(canon_lines,tmp_canon,useBytes=TRUE)
tmp_ids <- tempfile();writeLines("r2",tmp_ids,useBytes=TRUE)
lean_sha <- digest::digest(file=tmp_canon,algo="sha256",serialize=FALSE)
included_sha <- digest::digest(file=tmp_ids,algo="sha256",serialize=FALSE)
unlink(c(tmp_canon,tmp_ids))
src <- list(lean=list(lean_canonical_sha256=lean_sha))
ptr <- list(workflow="04",state="relevance_screening",status="published",
 visibility="restricted",publication_github_run_id="123",retained=1L,
 included_record_ids_sha256=included_sha,upstream_lean_canonical_sha256=lean_sha)
run <- list(id=123L,path=".github/workflows/w04_shiny_publish_only.yml",
 conclusion="success",display_title="W04 Shiny only publication almeria-map test.csv")
restored <- w05_shiny_restore_published_sources(src,ptr,"almeria-map",run)
stopifnot(identical(readLines(restored$included_ids),"r2"),
          length(readLines(restored$canonical))==2L)
unlink(restored$directory,recursive=TRUE)
if(had_archive)w04_subset_verified_archive <- orig_archive else rm(w04_subset_verified_archive)
w05_shiny_w04_included_ids <- orig_included
cat("PASS W05 restoration binds W04 included IDs and canonical SHA to review\\n")


# Auto discovery considers only completed successful exact-review W04-only runs.
candidate <- list(id=100L,path=".github/workflows/w04_shiny_publish_only.yml",
  status="completed",conclusion="success",
  display_title="W04 Shiny only publication almeria-map bundle.json")
unrelated <- candidate;unrelated$id <- 200L
unrelated$display_title <- "W04 Shiny only publication other-map bundle.json"
failed <- candidate;failed$id <- 300L;failed$conclusion <- "failure"
got <- w05_shiny_discover_w04_publication("almeria-map",
  list(unrelated,failed,candidate))
stopifnot(identical(got$id,100L),
 is.null(w05_shiny_discover_w04_publication("other-review",list(candidate))))
cat("PASS review-bound W04 publication discovery excludes other reviews and failed runs\n")


# Overview projection is based entirely on W04 receipt fields, never project constants.
ptr <- list(workflow="04",state="relevance_screening",status="published",
  visibility="restricted",publication_github_run_id="123",records=12L,
  retained=8L,excluded=4L,unresolved=0L,
  included_record_ids_sha256=paste(rep("a",64L),collapse=""),
  upstream_lean_canonical_sha256=paste(rep("b",64L),collapse=""))
run <- list(id=123L,path=".github/workflows/w04_shiny_publish_only.yml",
  status="completed",conclusion="success",
  display_title="W04 Shiny only publication another-review model-bundle.json")
base <- list(completed_through=4L,search_results_total=99L,deduplicated_records=20L)
mapped <- w05_shiny_w04_pipeline_metrics(base,ptr,"another-review",run,
  ptr$upstream_lean_canonical_sha256)
stopifnot(identical(mapped$screened_include,8L),
          identical(mapped$screened_exclude,4L),
          identical(mapped$completed_through,5L),
          identical(mapped$search_results_total,99L))
ptr$retained <- 7L
stopifnot(inherits(try(w05_shiny_w04_pipeline_metrics(base,ptr,"another-review",
  run,ptr$upstream_lean_canonical_sha256),silent=TRUE),"try-error"))
cat("PASS W04 overview is data-driven and refuses inconsistent publication counts\\n")


# Reload actual archive implementation after earlier network stubs.
source("R/w05_crop_archive.R")
# Review metadata is a named character vector; the W05 draft key is
# normally absent until the first approved schema creates the draft.
stopifnot(identical(w05_crop_draft_id(list(values=c(review_id="almeria-map"))),""),
          identical(w05_crop_draft_id(list(values=c(review_id="almeria-map",
            w05_crop_zenodo_draft_id="12345"))),"12345"))
cat("PASS missing W05 draft metadata key is treated as an uninitialised draft\\n")
