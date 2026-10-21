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
