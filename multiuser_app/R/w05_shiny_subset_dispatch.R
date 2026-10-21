# Isolated W05 Shiny runner. Does not touch LivingEvidenceMap production workflows.
w05_shiny_repo <- function() "thesalmonandthetomato/LivingEvidenceMapAdjudication"
w05_shiny_subset_validate_request <- function(request) {
  required <- c("review_id","zenodo_draft_id","input_filename","input_sha256",
                "dictionary_filename","dictionary_sha256","subset_count")
  if(!is.list(request)||!all(required%in%names(request)))
    stop("W05 Shiny subset request is incomplete",call.=FALSE)
  v <- lapply(request[required],function(x) {
    if(length(x)!=1L||is.na(x))stop("Invalid W05 request field",call.=FALSE)
    as.character(x)
  })
  if(!grepl("^[A-Za-z0-9][A-Za-z0-9._-]{2,127}$",v$review_id) ||
     !grepl("^[0-9]+$",v$zenodo_draft_id) ||
     !all(vapply(v[c("input_filename","dictionary_filename")],
        function(x)grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$",x),logical(1))) ||
     !all(vapply(v[c("input_sha256","dictionary_sha256")],
        function(x)grepl("^[0-9a-f]{64}$",x),logical(1))) ||
     !grepl("^[0-9]+$",v$subset_count) ||
     is.na(suppressWarnings(as.integer(v$subset_count))) ||
     as.integer(v$subset_count)<1L)
    stop("Invalid W05 Shiny subset provenance or size",call.=FALSE)
  v
}
w05_shiny_gh_request <- function(url,method="GET",payload=NULL) {
  token <- github_dispatch_token()
  req <- httr2::request(url) |>
    httr2::req_headers(Authorization=paste("Bearer",token),
      Accept="application/vnd.github+json",
      `X-GitHub-Api-Version`="2022-11-28",
      `User-Agent`="LivingEvidenceMap-Adjudication")
  if(identical(method,"POST"))
    req <- httr2::req_method(req,"POST") |> httr2::req_body_json(payload,auto_unbox=TRUE)
  resp <- httr2::req_perform(httr2::req_timeout(req,30))
  status <- httr2::resp_status(resp)
  if(identical(method,"POST") && status!=204L)
    stop(sprintf("W05 Shiny Actions dispatch failed: HTTP %d",status),call.=FALSE)
  if(identical(method,"POST"))return(invisible(TRUE))
  if(status!=200L)stop(sprintf("W05 Shiny Actions status failed: HTTP %d",status),call.=FALSE)
  httr2::resp_body_json(resp,simplifyVector=FALSE)
}
w05_shiny_subset_dispatch <- function(request, w04_published=FALSE, schema_approved=FALSE) {
  if(!isTRUE(w04_published))
    stop("W05 Shiny subset is paused until verified W04 publication",call.=FALSE)
  if(!isTRUE(schema_approved))
    stop("W05 Shiny subset is paused until its archived coding schema is explicitly approved",call.=FALSE)
  v <- w05_shiny_subset_validate_request(request)
  url <- sprintf("https://api.github.com/repos/%s/actions/workflows/w05_shiny_subset.yml/dispatches",
                  w05_shiny_repo())
  w05_shiny_gh_request(url,"POST",list(ref="main",inputs=v))
  invisible(TRUE)
}

# Pure, fail-closed provenance gate; deliberately does not download, publish or
# dispatch any workflow. The archive materialisation step must supply these
# independently verified values from the authoritative W04 publication receipt.
w05_shiny_verify_w04_bridge <- function(receipt, input_provenance, expected_review) {
  one <- function(x) length(x)==1L && !is.na(x) && nzchar(as.character(x))
  hash <- function(x) one(x) && grepl("^[0-9a-f]{64}$",as.character(x))
  if(!is.list(receipt)||!is.list(input_provenance) || !one(expected_review))
    stop("Missing W04-to-W05 provenance",call.=FALSE)
  if(!identical(as.character(receipt$review_id),as.character(expected_review)) ||
     !identical(as.character(input_provenance$review_id),as.character(expected_review)))
    stop("W04/W05 review identities differ",call.=FALSE)
  if(!identical(as.character(receipt$visibility),"restricted") ||
     !identical(as.character(receipt$publication_status),"published") ||
     !grepl("^[0-9]+$",as.character(receipt$publication_run_id)))
    stop("A verified restricted W04 publication is required",call.=FALSE)
  if(!hash(receipt$included_ids_sha256) || !hash(input_provenance$included_ids_sha256) ||
     !identical(as.character(receipt$included_ids_sha256),
                as.character(input_provenance$included_ids_sha256)) ||
     !hash(input_provenance$input_sha256))
    stop("W05 input does not match published W04 included-record provenance",call.=FALSE)
  if(!one(receipt$retained) || !one(input_provenance$record_count) ||
     is.na(suppressWarnings(as.integer(receipt$retained))) ||
     is.na(suppressWarnings(as.integer(input_provenance$record_count))) ||
     as.integer(receipt$retained)<1L ||
     !identical(as.integer(receipt$retained),
                as.integer(input_provenance$record_count)))
    stop("W05 input count differs from the published W04 retained count",call.=FALSE)
  invisible(TRUE)
}

# Single Shiny-only handover operation. The caller supplies files restored from
# authoritative W04 publication, never raw unverified screening selections.
# Does not dispatch W05; the user must separately request experimental execution.
w05_shiny_prepare_handover <- function(meta, canonical, included_ids,
                                        publication_receipt, dictionary_receipt) {
  if(!is.list(publication_receipt) || !is.list(dictionary_receipt))
    stop("W04 publication and W05 dictionary receipts are required",call.=FALSE)
  if(!identical(as.character(dictionary_receipt$deposit_id),
                w05_crop_draft_id(meta)) ||
     !grepl("^[0-9a-f]{64}$",as.character(dictionary_receipt$sha256)))
    stop("Dictionary receipt does not identify this review's W05 draft",call.=FALSE)
  if(!file.exists(canonical) || !file.exists(included_ids))
    stop("W04 canonical and included-record files must be restored first",call.=FALSE)
  draft <- w05_crop_draft(meta)
  marker <- paste0("w05-approved-",as.character(dictionary_receipt$sha256),".json")
  files <- draft$files %||% list()
  if(sum(vapply(files,function(x)identical(as.character(x$filename),marker),logical(1)))!=1L)
    stop("W05 schema approval not verified in restricted draft",call.=FALSE)
  tmp <- tempfile("w05-shiny-handover-")
  dir.create(tmp,recursive=TRUE)
  on.exit(unlink(tmp,recursive=TRUE),add=TRUE)
  receipt_path <- file.path(tmp,"publication.json")
  output <- file.path(tmp,"w05-input.jsonl")
  manifest <- file.path(tmp,"provenance.json")
  jsonlite::write_json(publication_receipt,receipt_path,auto_unbox=TRUE)
  script <- "scripts/w05_shiny_materialise_w04.R"
  if(!file.exists(script))
    stop("W05 Shiny materialisation script missing",call.=FALSE)
  arguments <- c(shQuote(script),"--canonical",shQuote(normalizePath(canonical)),
    "--included-ids",shQuote(normalizePath(included_ids)),
    "--receipt",shQuote(receipt_path),"--review-id",shQuote(meta$review_id),
    "--output",shQuote(output),"--manifest",shQuote(manifest))
  status <- suppressWarnings(system2(file.path(R.home("bin"),"Rscript"),
    arguments,stdout=TRUE,stderr=TRUE))
  if(!is.null(attr(status,"status")) && attr(status,"status")!=0L)
    stop("W05 Shiny materialisation failed: ",paste(status,collapse="; "),call.=FALSE)
  provenance <- jsonlite::fromJSON(manifest,simplifyVector=FALSE)
  w05_shiny_verify_w04_bridge(publication_receipt,provenance,meta$review_id)
  w05_shiny_archive_input(meta,output,manifest,publication_receipt)
}

# Read-only review-scoped lookup. Absence of a registered W04 publication is
# a normal "awaiting publication" state, never a reason to run production W05.
w05_shiny_w04_publication_status <- function(review_id, registry_text) {
  if(length(review_id)!=1L || !grepl("^[A-Za-z0-9._-]+$",review_id))
    stop("Invalid W05 review identity",call.=FALSE)
  rows <- utils::read.csv(text=registry_text,stringsAsFactors=FALSE,
                          check.names=FALSE)
  required <- c("review_id","publication_github_run_id","visibility",
                "unresolved","retained","included_record_ids_sha256",
                "upstream_lean_canonical_sha256")
  if(!all(required %in% names(rows)))
    return(list(status="awaiting_review_bound_receipt"))
  matches <- rows[rows$review_id==review_id & rows$visibility=="restricted" &
                    rows$unresolved==0L,,drop=FALSE]
  if(nrow(matches)!=1L)
    return(list(status="awaiting_review_bound_receipt"))
  x <- matches[1L,,drop=FALSE]
  if(!grepl("^[0-9]+$",as.character(x$publication_github_run_id)) ||
     !grepl("^[0-9a-f]{64}$",as.character(x$included_record_ids_sha256)) ||
     !grepl("^[0-9a-f]{64}$",as.character(x$upstream_lean_canonical_sha256)))
    stop("Invalid W04 publication registry provenance",call.=FALSE)
  list(status="candidate_receipt_only",review_id=review_id,
       publication_run_id=as.character(x$publication_github_run_id))
}

# Translate the established production W04 receipt into the Shiny bridge
# contract, only after independently binding the run to the current review.
w05_shiny_bind_w04_receipt <- function(pointer, review_id, run) {
  scalar <- function(x)if(is.null(x)||length(x)!=1L||is.na(x)) "" else as.character(x)
  id <- scalar(pointer$publication_github_run_id)
  title <- scalar(run$display_title)
  if(!identical(scalar(pointer$workflow),"04") ||
     !identical(scalar(pointer$state),"relevance_screening") ||
     !identical(scalar(pointer$status),"published") ||
     !identical(scalar(pointer$visibility),"restricted") ||
     !grepl("^[0-9]+$",id) ||
     !identical(id,scalar(run$id)) ||
     !identical(scalar(run$path),".github/workflows/w04_shiny_publish_only.yml") ||
     !identical(scalar(run$conclusion),"success") ||
     !startsWith(title,paste0("W04 Shiny only publication ",review_id," ")))
    stop("W04 receipt has no verified Shiny publication binding to this review",call.=FALSE)
  sha <- scalar(pointer$included_record_ids_sha256)
  lean <- scalar(pointer$upstream_lean_canonical_sha256)
  if(!grepl("^[a-f0-9]{64}$",sha) || !grepl("^[a-f0-9]{64}$",lean) ||
     is.na(suppressWarnings(as.integer(pointer$retained))) ||
     as.integer(pointer$retained)<1L)
    stop("Invalid published W04 receipt provenance",call.=FALSE)
  list(review_id=review_id,publication_status="published",
       publication_run_id=id,visibility="restricted",
       included_ids_sha256=sha,upstream_lean_canonical_sha256=lean,
       retained=as.integer(pointer$retained),
       zenodo_record_id=scalar(pointer$zenodo_record_id))
}

# Fetch the immutable receipt belonging to a specific completed Shiny W04
# publication, rather than selecting a global/latest W04 registry entry.
w05_shiny_fetch_w04_publication <- function(review_id,run_id) {
  if(length(run_id)!=1L || !grepl("^[0-9]+$",as.character(run_id)))
    stop("A specific W04 Shiny publication run ID is required",call.=FALSE)
  repo <- github_dispatch_repo()
  endpoint <- sprintf("https://api.github.com/repos/%s/actions/runs/%s",repo,run_id)
  run <- github_api_get_json(endpoint)
  path <- sprintf("docs/workflow04/zenodo/run-%s.json",run_id)
  pointer <- jsonlite::fromJSON(read_github_text_file(path),simplifyVector=FALSE)
  w05_shiny_bind_w04_receipt(pointer,review_id,run)
}

# Restore canonical and included identifiers only from checksum-verified
# upstream publication archives. This is invoked after binding the exact W04
# Shiny publication receipt; no latest-run fallback is permitted.
w05_shiny_restore_published_sources <- function(source, publication_pointer,
                                                 review_id, publication_run) {
  receipt <- w05_shiny_bind_w04_receipt(publication_pointer,review_id,
                                       publication_run)
  if(!identical(tolower(as.character(source$lean$lean_canonical_sha256)),
                receipt$upstream_lean_canonical_sha256))
    stop("W04 publication does not match this review's W02 canonical source",call.=FALSE)
  canonical_lines <- w04_subset_verified_archive(source$lean,"lean")
  ids_archive <- w04_subset_verified_archive(publication_pointer,"w04")
  ids <- ids_archive[nzchar(trimws(ids_archive))]
  if(length(ids)!=receipt$retained || anyDuplicated(ids))
    stop("W04 publication included-record count mismatch",call.=FALSE)
  root <- tempfile("w05-w04-")
  dir.create(root)
  canonical <- file.path(root,"canonical.jsonl")
  included <- file.path(root,"included.txt")
  writeLines(canonical_lines,canonical,useBytes=TRUE)
  writeLines(ids,included,useBytes=TRUE)
  if(!identical(digest::digest(file=canonical,algo="sha256",serialize=FALSE),
                receipt$upstream_lean_canonical_sha256) ||
     !identical(digest::digest(file=included,algo="sha256",serialize=FALSE),
                receipt$included_ids_sha256)) {
    unlink(root,recursive=TRUE)
    stop("Restored W04 publication source checksum mismatch",call.=FALSE)
  }
  list(directory=root,canonical=canonical,included_ids=included,receipt=receipt)
}
