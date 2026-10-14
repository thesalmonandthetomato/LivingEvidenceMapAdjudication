suppressPackageStartupMessages(library(httr2))

if (!exists("%||%", mode="function")) {
  `%||%` <- function(x,y) if (is.null(x) || length(x)==0L) y else x
}

github_dispatch_token <- function() {
  token <- Sys.getenv("LEM_GITHUB_DISPATCH_TOKEN", unset = "")
  if (!nzchar(token)) stop("LEM_GITHUB_DISPATCH_TOKEN is not configured", call. = FALSE)
  token
}

github_dispatch_repo <- function() {
  value <- Sys.getenv("LEM_GITHUB_DISPATCH_REPO", unset = "")
  if (!identical(value, "thesalmonandthetomato/LivingEvidenceMap"))
    stop("Development Shiny app requires LEM_GITHUB_DISPATCH_REPO=thesalmonandthetomato/LivingEvidenceMap", call.=FALSE)
  value
}

github_scoping_ref <- function() {
  value <- Sys.getenv("LEM_GITHUB_SCOPING_REF", unset = "")
  if (!identical(value, "main"))
    stop("Development Shiny app requires LEM_GITHUB_SCOPING_REF=main", call.=FALSE)
  value
}

assert_development_github_target <- function(repo=github_dispatch_repo(), ref=github_scoping_ref()) {
  if (!identical(repo,"thesalmonandthetomato/LivingEvidenceMap") || !identical(ref,"main"))
    stop("Refusing GitHub operation outside development pipeline main",call.=FALSE)
  invisible(TRUE)
}

github_api_get_json <- function(endpoint) {
  assert_development_github_target()
  req <- httr2::request(endpoint) |>
    httr2::req_headers(
      Authorization = paste("Bearer", github_dispatch_token()),
      Accept = "application/vnd.github+json",
      `X-GitHub-Api-Version` = "2022-11-28",
      `User-Agent` = "LivingEvidenceMap-Adjudication"
    )
  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  if (status >= 400L) stop(sprintf("GitHub API GET failed with HTTP %d", status), call.=FALSE)
  httr2::resp_body_json(resp, simplifyVector=FALSE)
}

read_github_text_file <- function(path, ref = github_scoping_ref()) {
  assert_development_github_target(ref=ref)
  endpoint <- sprintf(
    "https://api.github.com/repos/%s/contents/%s?ref=%s",
    github_dispatch_repo(),
    path,
    utils::URLencode(ref, reserved=TRUE)
  )
  x <- github_api_get_json(endpoint)
  encoding <- as.character(x$encoding %||% "")
  content <- gsub("\\s+", "", as.character(x$content %||% ""))
  if (!identical(encoding, "base64") || !nzchar(content)) {
    stop(sprintf("GitHub file %s did not return base64 content", path), call.=FALSE)
  }
  rawToChar(jsonlite::base64_dec(content))
}

read_w00_scoping_source_catalogue <- function(ref = github_scoping_ref()) {
  cfg <- jsonlite::fromJSON(
    read_github_text_file("config/workflow00_ebsco_sources.json", ref=ref),
    simplifyVector=FALSE
  )
  ebsco <- cfg$sources %||% list()
  ebsco_slugs <- names(ebsco)
  ebsco_labels <- if (length(ebsco_slugs)) {
    vapply(ebsco, function(x) as.character(x$display_name %||% ""), character(1))
  } else character()

  core_labels <- c(
    lens="The Lens, via The Lens",
    scopus="Scopus, via Scopus",
    openalex="OpenAlex, via OpenAlex",
    agricola="AGRICOLA, via Europe PMC",
    pubmed="PubMed/MEDLINE, via Europe PMC",
    ethos="EThOS, via Europe PMC",
    cba="Chinese Biological Abstracts, via Europe PMC",
    epmc_preprints="Europe PMC preprints, via Europe PMC",
    wos="Web of Science Core Collection, via Web of Science"
  )
  ebsco_labels <- paste0(ebsco_labels, ", via EBSCO")
  labels <- c(core_labels, stats::setNames(ebsco_labels, ebsco_slugs))
  out <- data.frame(
    source_slug=names(labels),
    source=unname(labels),
    stringsAsFactors=FALSE
  )
  out[order(tolower(out$source)),,drop=FALSE]
}

# Launch the main W00 orchestrator from an explicit Shiny user action.
# The strategy and exact source set are passed together; no repository
# user_input file is edited and no global review environment is changed.
# Read-only discovery for the current single-review development deployment.
# Finds recent main-branch W00 orchestrator runs after page reload.
w00_formal_recent_run <- function(request_id, run_id = "") {
  assert_development_github_target()
  if (!is.character(request_id) || length(request_id)!=1L ||
      !grepl("^[A-Za-z0-9._-]{8,80}$",request_id))
    stop("Valid W00 request ID is required for monitoring",call.=FALSE)
  if(nzchar(run_id)) {
    if(!grepl("^[0-9]+$",run_id)) stop("Invalid stored W00 run ID",call.=FALSE)
    endpoint <- sprintf("https://api.github.com/repos/%s/actions/runs/%s",
      github_dispatch_repo(),run_id)
    candidate <- github_api_get_json(endpoint)
    if(!identical(as.character(candidate$display_title %||% ""),
      paste("W00 Shiny",request_id)))
      stop("Stored W00 run does not match this Shiny request",call.=FALSE)
    return(candidate)
  }
  url <- sprintf(
    "https://api.github.com/repos/%s/actions/workflows/workflow_00_search_orchestrator.yml/runs?branch=main&event=workflow_dispatch&per_page=100",
    github_dispatch_repo())
  runs <- github_api_get_json(url)$workflow_runs %||% list()
  matches <- Filter(function(x) identical(as.character(x$display_title %||% ""),
    paste("W00 Shiny",request_id)),runs)
  if (!length(matches)) return(NULL)
  matches[[1L]]
}

# Recover a W00 run using the review-specific metadata, not the latest global run.
w00_formal_review_run <- function() {
  if(!identical(storage_backend(),"google_sheets")) return(NULL)
  gs4_auth_from_env()
  rows <- as.data.frame(googlesheets4::read_sheet(
    sheet_id_from_env(),sheet="review_metadata",col_types="cc"))
  if(!all(c("key","value")%in%names(rows))) stop("Invalid review metadata",call.=FALSE)
  vals <- stats::setNames(as.character(rows$value),as.character(rows$key))
  run_id <- vals[["latest_w00_run_id"]] %||% ""
  if(is.na(run_id) || !nzchar(trimws(run_id))) return(NULL)
  if(!grepl("^[0-9]+$",run_id)) return(NULL)
  endpoint <- sprintf("https://api.github.com/repos/%s/actions/runs/%s",
    github_dispatch_repo(),run_id)
  run <- github_api_get_json(endpoint)
  title <- as.character(run$display_title %||% "")
  prefix <- "W00 Shiny "
  if(!startsWith(title,prefix)) return(NULL)
  request_id <- substring(title,nchar(prefix)+1L)
  if(!grepl("^[A-Za-z0-9._-]{8,80}$",request_id)) return(NULL)
  list(request_id=request_id,run_id=run_id)
}

w00_formal_run_jobs <- function(run_id) {
  assert_development_github_target()
  if (!grepl("^[0-9]+$",as.character(run_id))) stop("Invalid W00 run ID",call.=FALSE)
  url <- sprintf("https://api.github.com/repos/%s/actions/runs/%s/jobs?per_page=100",
    github_dispatch_repo(),as.character(run_id))
  github_api_get_json(url)$jobs %||% list()
}

dispatch_w00_formal <- function(search_string, sources, request_id) {
  assert_development_github_target()
  if (!is.character(search_string) || length(search_string)!=1L ||
      !nzchar(trimws(search_string))) stop("Search string is empty",call.=FALSE)
  sources <- unique(as.character(sources))
  if (!length(sources) || any(!grepl("^[a-z][a-z0-9_]*$",sources)))
    stop("Select valid source IDs before dispatch",call.=FALSE)
  if (!grepl("^[A-Za-z0-9._-]{8,80}$",request_id))
    stop("Invalid formal search request ID",call.=FALSE)
  strategy <- list(
    search_version=paste0("shiny-",request_id),
    advanced_boolean=search_string
  )
  strategy_b64 <- jsonlite::base64_enc(charToRaw(enc2utf8(
    jsonlite::toJSON(strategy,auto_unbox=TRUE))))
  groups <- c("core_multidisciplinary","health_medicine","environment_agriculture",
              "social_sciences","education","regional","media_communication",
              "humanities","sport_tourism","grey_preprints")
  inputs <- list(
    shiny_request_id=request_id,execution_pathway="shiny",review_id="almeria-map",review_sheet_id=sheet_id_from_env(),first_run="true",dispatch_w01="false",run_type="full",
    additional_sources=paste(sources,collapse=","),
    strategy_json_b64=strategy_b64
  )
  for (g in groups) inputs[[paste0("group_",g)]] <- "false"
  endpoint <- sprintf("https://api.github.com/repos/%s/actions/workflows/workflow_00_search_orchestrator.yml/dispatches",
    github_dispatch_repo())
  req <- httr2::request(endpoint) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Authorization=paste("Bearer",github_dispatch_token()),
      Accept="application/vnd.github+json",
      `X-GitHub-Api-Version`="2022-11-28",
      `User-Agent`="LivingEvidenceMap-Adjudication"
    ) |>
    httr2::req_body_json(list(ref=github_scoping_ref(),inputs=inputs))
  resp <- httr2::req_perform(req)
  if (!identical(httr2::resp_status(resp),204L))
    stop(sprintf("Formal W00 dispatch returned HTTP %d",httr2::resp_status(resp)),call.=FALSE)
  invisible(TRUE)
}


# Explicit read-only W01 input preflight for the active review. Production W01
# remains disabled in the pipeline until review-isolated publication is ready.
# A production launch must be independently configured even if GitHub is public.
# No unauthenticated GitHub or implicit legacy Sheet fallback is permitted.
w01_launch_configuration <- function() {
  review_id <- trimws(Sys.getenv("LEM_SEARCH_REVIEW_ID",unset=""))
  sheet_id <- trimws(tryCatch(sheet_id_from_env(),error=function(e)""))
  reason <- ""
  if(!identical(Sys.getenv("LEM_GITHUB_DISPATCH_REPO",unset=""),
                "thesalmonandthetomato/LivingEvidenceMap") ||
     !identical(Sys.getenv("LEM_GITHUB_SCOPING_REF",unset=""),"main") ||
     !nzchar(Sys.getenv("LEM_GITHUB_DISPATCH_TOKEN",unset="")))
    reason <- "Authenticated GitHub W01 dispatch is not configured."
  else if(nzchar(review_id) && !grepl("^[A-Za-z0-9._-]{3,128}$",review_id))
    reason <- "Configured LEM_SEARCH_REVIEW_ID is invalid."
  else if(!grepl("^[A-Za-z0-9_-]{15,150}$",sheet_id))
    reason <- "Configure the active review's Google Sheet ID."
  list(ready=!nzchar(reason),reason=reason,review_id=review_id,sheet_id=sheet_id)
}

dispatch_w01_input_preflight <- function(review_id, review_sheet_id, request_id, run_llm=FALSE) {
  assert_development_github_target()
  review_id <- trimws(as.character(review_id))
  review_sheet_id <- trimws(as.character(review_sheet_id))
  request_id <- trimws(as.character(request_id))
  if(length(request_id)!=1L || !grepl("^[A-Za-z0-9._-]{8,80}$",request_id))
    stop("Invalid W01 Shiny request ID",call.=FALSE)
  if(length(review_id)!=1L || !grepl("^[A-Za-z0-9._-]{3,128}$",review_id))
    stop("Invalid W01 review ID",call.=FALSE)
  if(length(review_sheet_id)!=1L || !grepl("^[A-Za-z0-9_-]{15,150}$",review_sheet_id))
    stop("Invalid W01 Google Sheet ID",call.=FALSE)
  endpoint <- sprintf(
    "https://api.github.com/repos/%s/actions/workflows/workflow_01_production.yml/dispatches",
    github_dispatch_repo()
  )
  payload <- list(ref=github_scoping_ref(),inputs=list(
    execution_pathway="shiny",review_id=review_id,
    review_sheet_id=review_sheet_id,shiny_request_id=request_id,first_run="true",
    run_review_llm=if(isTRUE(run_llm)) "true" else "false"
  ))
  req <- httr2::request(endpoint) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Authorization=paste("Bearer",github_dispatch_token()),
      Accept="application/vnd.github+json",
      `X-GitHub-Api-Version`="2022-11-28",
      `User-Agent`="LivingEvidenceMap-Adjudication"
    ) |>
    httr2::req_body_json(payload)
  resp <- httr2::req_perform(req)
  if(!identical(httr2::resp_status(resp),204L))
    stop(sprintf("W01 review dispatch returned HTTP %d",httr2::resp_status(resp)),call.=FALSE)
  invisible(TRUE)
}


# Correlate one Shiny launch with its actual GitHub Actions run. Never choose
# the latest W01 run, which may belong to another review or launch pathway.
w01_find_shiny_run <- function(review_id, request_id) {
  assert_development_github_target()
  if(!grepl("^[A-Za-z0-9._-]{3,128}$",as.character(review_id)) ||
     !grepl("^[A-Za-z0-9._-]{8,80}$",as.character(request_id)))
    stop("Invalid W01 launch identity",call.=FALSE)
  endpoint <- sprintf(
    "https://api.github.com/repos/%s/actions/workflows/workflow_01_production.yml/runs?branch=main&event=workflow_dispatch&per_page=100",
    github_dispatch_repo()
  )
  candidates <- github_api_get_json(endpoint)$workflow_runs %||% list()
  wanted <- paste("W01",review_id,request_id)
  hits <- Filter(function(x) identical(as.character(x$display_title %||% ""),wanted),candidates)
  if(!length(hits)) return(NULL)
  if(length(hits)!=1L) stop("Ambiguous W01 launch request ID",call.=FALSE)
  hits[[1L]]
}

# Read-only progress observation for an explicit W01 run; never dispatches jobs.
w01_github_run_progress <- function(run_id) {
  assert_development_github_target()
  run_id <- as.character(run_id)
  if(length(run_id)!=1L || !grepl("^[0-9]+$",run_id))
    stop("Invalid W01 GitHub run ID",call.=FALSE)
  base <- sprintf("https://api.github.com/repos/%s/actions/runs/%s",github_dispatch_repo(),run_id)
  run <- github_api_get_json(base)
  if(!grepl("(^|/)\\.github/workflows/workflow_01_production\\.yml(@|$)",as.character(run$path %||% "")))
    stop("GitHub run is not W01 production",call.=FALSE)
  if(!identical(as.character(run$head_branch %||% ""),"main"))
    stop("W01 run is not on development main",call.=FALSE)
  jobs <- github_api_get_json(paste0(base,"/jobs?per_page=100"))$jobs %||% list()
  active <- Filter(function(j) !identical(as.character(j$conclusion %||% ""),"skipped"), jobs)
  steps <- unlist(lapply(active,function(j) j$steps %||% list()),recursive=FALSE)
  completed <- sum(vapply(steps,function(x) identical(as.character(x$status %||% ""),"completed"),logical(1)))
  total <- length(steps)
  list(run_id=run_id,status=as.character(run$status %||% ""),
       conclusion=as.character(run$conclusion %||% ""),
       completed_steps=completed,total_steps=total,
       progress=if(total) min(99L,as.integer(floor(100*completed/total))) else 0L)
}



# Use published, review-bound Zenodo receipts, never infer counts from
# GitHub job completion or from the legacy current-run dashboard.
w01_published_review_results <- function(run_id, review_id, review_sheet_id) {
  assert_development_github_target()
  run_id <- as.character(run_id)
  if(length(run_id)!=1L || !grepl("^[0-9]+$",run_id))
    stop("Invalid W01 results run ID",call.=FALSE)
  if(!nzchar(as.character(review_id)) || !nzchar(as.character(review_sheet_id)))
    stop("W01 results require both review identifiers",call.=FALSE)
  if(!grepl("^[A-Za-z0-9._-]{3,128}$",as.character(review_id)))
    stop("Invalid W01 review ID for published results",call.=FALSE)
  path <- sprintf("docs/deduplication/reviews/%s/zenodo/run-%s.json",review_id,run_id)
  receipt <- tryCatch(
    jsonlite::fromJSON(read_github_text_file(path),simplifyVector=FALSE),
    error=function(e) NULL
  )
  if(is.null(receipt)) return(NULL)
  if(!identical(as.character(receipt$status %||% ""),"published") ||
     !identical(as.character(receipt$visibility %||% ""),"restricted") ||
     !identical(as.character(receipt$github_run_id %||% ""),run_id) ||
     !identical(as.character(receipt$review_id %||% ""),as.character(review_id)) ||
     !identical(as.character(receipt$google_sheet_id %||% ""),as.character(review_sheet_id)))
    stop("W01 published receipt does not match the active review",call.=FALSE)
  manifestations <- suppressWarnings(as.integer(receipt$source_manifestations %||% NA_integer_))
  unique_records <- suppressWarnings(as.integer(receipt$canonical_records %||% NA_integer_))
  uncertain <- suppressWarnings(as.integer(receipt$manual_review_pairs %||% NA_integer_))
  if(length(manifestations)!=1L || length(unique_records)!=1L ||
     length(uncertain)!=1L || anyNA(c(manifestations,unique_records,uncertain)) ||
     unique_records<0L || manifestations<unique_records || uncertain<0L)
    stop("Published W01 receipt lacks valid deduplication counts",call.=FALSE)
  # manual_review_pairs counts the cases originally requiring an LLM
  # decision, not the human cases still unresolved after final publication.
  list(duplicates_removed=manifestations-unique_records,
       unique_records=unique_records,uncertain_cases=0L,
       original_manual_review_pairs=uncertain,
       state=as.character(receipt$state %||% ""))
}

# Review-specific pause and resumption are separate GitHub runs. Follow only
# the immutable, review-bound checkpoint and resolution pointers, never the
# latest global W01 run or the legacy salmon current-run status.
w01_pending_review_checkpoint <- function(run_id,review_id,review_sheet_id) {
  if(!grepl("^[0-9]+$",as.character(run_id)) ||
     !grepl("^[A-Za-z0-9._-]{3,128}$",as.character(review_id)))
    stop("Invalid review checkpoint identity",call.=FALSE)
  path <- sprintf("docs/deduplication/reviews/%s/checkpoints/run-%s.json",review_id,run_id)
  x <- tryCatch(jsonlite::fromJSON(read_github_text_file(path),simplifyVector=FALSE),
                error=function(e)NULL)
  if(is.null(x)) return(NULL)
  if(!identical(as.character(x$status %||% ""),"published") ||
     !identical(as.character(x$state %||% ""),"pre_adjudication") ||
     !identical(as.character(x$visibility %||% ""),"restricted") ||
     !identical(as.character(x$github_run_id %||% ""),as.character(run_id)) ||
     !identical(as.character(x$review_id %||% ""),as.character(review_id)) ||
     !identical(as.character(x$google_sheet_id %||% ""),as.character(review_sheet_id)))
    stop("W01 checkpoint belongs to another review or is invalid",call.=FALSE)
  n <- suppressWarnings(as.integer(x$pending_human_cases %||% NA_integer_))
  if(length(n)!=1L || is.na(n) || n<1L)
    stop("W01 checkpoint has an invalid number of human cases",call.=FALSE)
  list(pending_human_cases=n,run_id=as.character(run_id))
}

w01_resolved_review_run_id <- function(checkpoint_run_id,review_id) {
  if(!grepl("^[0-9]+$",as.character(checkpoint_run_id)) ||
     !grepl("^[A-Za-z0-9._-]{3,128}$",as.character(review_id)))
    stop("Invalid review resume identity",call.=FALSE)
  path <- sprintf("docs/deduplication/reviews/%s/checkpoints/resolutions/run-%s.json",
                  review_id,checkpoint_run_id)
  x <- tryCatch(jsonlite::fromJSON(read_github_text_file(path),simplifyVector=FALSE),
                error=function(e)NULL)
  if(is.null(x)) return(NULL)
  if(!identical(as.character(x$status %||% ""),"resolved") ||
     !identical(as.character(x$checkpoint_run_id %||% ""),as.character(checkpoint_run_id)) ||
     !grepl("^[0-9]+$",as.character(x$resolved_by_run_id %||% "")))
    stop("Invalid W01 review resolution marker",call.=FALSE)
  as.character(x$resolved_by_run_id)
}

# One authenticated lookup at login; no recurring GitHub or Sheets polling.
# Any published W01 checkpoint/final record locks duplicate first-run launches.
w01_has_published_review_run <- function(review_id,sheet_id) {
  assert_development_github_target()
  if(!grepl("^[A-Za-z0-9._-]{3,128}$",review_id) ||
     !grepl("^[A-Za-z0-9_-]{15,150}$",sheet_id))
    stop("Invalid review identity for W01 launch lock",call.=FALSE)
  for(folder in c("checkpoints","zenodo")) {
    dir <- sprintf("docs/deduplication/reviews/%s/%s",review_id,folder)
    endpoint <- sprintf("https://api.github.com/repos/%s/contents/%s?ref=main",
                        github_dispatch_repo(),dir)
    entries <- tryCatch(github_api_get_json(endpoint),error=function(e)e)
    if(inherits(entries,"error")) {
      if(grepl("HTTP 404",conditionMessage(entries),fixed=TRUE)) next
      stop(conditionMessage(entries),call.=FALSE)
    }
    if(!length(entries)) next
    for(item in entries) {
      name <- as.character(item$name %||% "")
      if(!grepl("^run-[0-9]+\\.json$",name)) next
      receipt <- jsonlite::fromJSON(read_github_text_file(paste0(dir,"/",name)),
                                    simplifyVector=FALSE)
      if(identical(as.character(receipt$status %||% ""),"published") &&
         identical(as.character(receipt$visibility %||% ""),"restricted") &&
         identical(as.character(receipt$review_id %||% ""),review_id) &&
         identical(as.character(receipt$google_sheet_id %||% ""),sheet_id))
        return(TRUE)
    }
  }
  FALSE
}

# Discover final RIS eligibility from review-bound, restricted Zenodo receipts.
# Called once at authenticated session start, never as a Sheets poll.
w01_latest_final_review_receipt <- function(review_id,sheet_id) {
  assert_development_github_target()
  if(!grepl("^[A-Za-z0-9._-]{3,128}$",review_id) ||
     !grepl("^[A-Za-z0-9_-]{15,150}$",sheet_id))
    stop("Invalid W01 review identifier",call.=FALSE)
  directory <- sprintf("docs/deduplication/reviews/%s/zenodo",review_id)
  endpoint <- sprintf("https://api.github.com/repos/%s/contents/%s?ref=main",
                      github_dispatch_repo(),directory)
  entries <- tryCatch(github_api_get_json(endpoint),error=function(e)NULL)
  if(is.null(entries)||!length(entries)) return(NULL)
  filenames <- vapply(entries,function(x)as.character(x$name %||% ""),character(1))
  files <- filenames[grepl("^run-[0-9]+\\.json$",filenames)]
  if(!length(files)) return(NULL)
  ids <- as.numeric(sub("^run-([0-9]+)\\.json$","\\1",files))
  files <- files[order(ids,decreasing=TRUE)]
  for(fn in files) {
    x <- tryCatch(jsonlite::fromJSON(read_github_text_file(paste0(directory,"/",fn)),
                                      simplifyVector=FALSE),error=function(e)NULL)
    if(!is.null(x) && identical(as.character(x$status %||% ""),"published") &&
       identical(as.character(x$visibility %||% ""),"restricted") &&
       (as.character(x$state %||% "") %in% c("final","delta")) &&
       identical(as.character(x$review_id %||% ""),review_id) &&
       identical(as.character(x$google_sheet_id %||% ""),sheet_id))
      return(list(run_id=as.character(x$github_run_id),review_id=review_id))
  }
  NULL
}

# RIS is derived only from a final, review-bound W01 canonical receipt.
dispatch_w01_ris_export <- function(review_id,source_run_id) {
  assert_development_github_target()
  if(!grepl("^[A-Za-z0-9._-]{3,128}$",review_id) ||
     !grepl("^[0-9]+$",as.character(source_run_id)))
    stop("Invalid W01 RIS export identity",call.=FALSE)
  endpoint <- sprintf("https://api.github.com/repos/%s/actions/workflows/workflow_01_export_ris.yml/dispatches",github_dispatch_repo())
  req <- httr2::request(endpoint) |>
    httr2::req_method("POST") |>
    httr2::req_headers(Authorization=paste("Bearer",github_dispatch_token()),
      Accept="application/vnd.github+json",`X-GitHub-Api-Version`="2022-11-28",
      `User-Agent`="LivingEvidenceMap-Adjudication") |>
    httr2::req_body_json(list(ref=github_scoping_ref(),
      inputs=list(review_id=review_id,source_run_id=as.character(source_run_id))))
  response <- httr2::req_perform(req)
  if(!identical(httr2::resp_status(response),204L))
    stop("RIS export dispatch failed: HTTP ",httr2::resp_status(response),call.=FALSE)
  invisible(TRUE)
}

w01_ris_export_artifact <- function(review_id,source_run_id) {
  assert_development_github_target()
  if(!grepl("^[A-Za-z0-9._-]{3,128}$",review_id) ||
     !grepl("^[0-9]+$",as.character(source_run_id)))
    stop("Invalid RIS lookup identity",call.=FALSE)
  endpoint <- sprintf("https://api.github.com/repos/%s/actions/artifacts?per_page=100",github_dispatch_repo())
  xs <- github_api_get_json(endpoint)$artifacts %||% list()
  wanted <- paste0("workflow01-unique-ris-",review_id,"-",source_run_id)
  found <- Filter(function(x) identical(as.character(x$name %||% ""),wanted) &&
                        !isTRUE(x$expired),xs)
  if(!length(found)) return(NULL)
  found[[1L]]
}

w01_ris_download_artifact <- function(artifact,destination) {
  if(is.null(artifact$archive_download_url) ||
     !grepl("^https://api\\.github\\.com/repos/thesalmonandthetomato/LivingEvidenceMap/actions/artifacts/[0-9]+/zip$",
            as.character(artifact$archive_download_url)))
    stop("Unexpected GitHub RIS artifact download URL",call.=FALSE)
  zip_path <- tempfile(fileext=".zip")
  on.exit(unlink(zip_path),add=TRUE)
  req <- httr2::request(as.character(artifact$archive_download_url)) |>
    httr2::req_headers(Authorization=paste("Bearer",github_dispatch_token()),
      Accept="application/vnd.github+json",
      `X-GitHub-Api-Version`="2022-11-28",
      `User-Agent`="LivingEvidenceMap-Adjudication")
  httr2::req_perform(req,path=zip_path)
  members <- utils::unzip(zip_path,list=TRUE)$Name
  if(!identical(members,"unique_records.ris"))
    stop("Export artifact must contain exactly one RIS file",call.=FALSE)
  dest_dir <- tempfile("ris_extract_")
  dir.create(dest_dir)
  on.exit(unlink(dest_dir,recursive=TRUE),add=TRUE)
  utils::unzip(zip_path,files=members,exdir=dest_dir)
  if(!file.copy(file.path(dest_dir,"unique_records.ris"),destination,overwrite=TRUE))
    stop("Unable to stage RIS download",call.=FALSE)
}

dispatch_w00_scoping <- function(request_id, search_string = NULL, search_string_path = "user_input/scoping_search_string.txt") {
  request_id <- trimws(as.character(request_id))
  if (!grepl("^[A-Za-z0-9._-]{8,80}$", request_id)) {
    stop("Invalid scoping request ID", call.=FALSE)
  }
  search_string <- if (is.null(search_string)) "" else trimws(as.character(search_string))
  search_string_b64 <- if (nzchar(search_string)) {
    jsonlite::base64_enc(charToRaw(enc2utf8(search_string)))
  } else ""

  endpoint <- sprintf(
    "https://api.github.com/repos/%s/actions/workflows/workflow_00_search_scoping.yml/dispatches",
    github_dispatch_repo()
  )
  req <- httr2::request(endpoint) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Authorization = paste("Bearer", github_dispatch_token()),
      Accept = "application/vnd.github+json",
      `X-GitHub-Api-Version` = "2022-11-28",
      `User-Agent` = "LivingEvidenceMap-Adjudication"
    ) |>
    httr2::req_body_json(list(
      ref = github_scoping_ref(),
      inputs = list(
        search_string_path = as.character(search_string_path),
        search_string_b64 = search_string_b64,
        request_id = request_id
      )
    ))
  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  if (identical(status, 204L)) return(invisible(TRUE))
  if (!identical(status, 404L)) {
    stop(sprintf("W00 scoping dispatch failed with HTTP %d", status), call.=FALSE)
  }

  # Branch-only workflows can return 404 from the workflow-dispatch endpoint
  # when the workflow is not yet present on the repository default branch.
  # Fall back to a tiny branch commit watched only by the scoping workflow.
  marker_path <- sprintf(
    "docs/shiny_scoping_requests/%s.json",
    request_id
  )
  payload <- jsonlite::toJSON(list(
    request_id=request_id,
    search_string_path=as.character(search_string_path),
    search_string_b64=search_string_b64,
    requested_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"),
    requested_by="LivingEvidenceMapAdjudication"
  ),auto_unbox=TRUE,pretty=TRUE)

  endpoint <- sprintf(
    "https://api.github.com/repos/%s/contents/%s",
    github_dispatch_repo(), marker_path
  )
  marker_req <- httr2::request(endpoint) |>
    httr2::req_method("PUT") |>
    httr2::req_headers(
      Authorization = paste("Bearer", github_dispatch_token()),
      Accept = "application/vnd.github+json",
      `X-GitHub-Api-Version` = "2022-11-28",
      `User-Agent` = "LivingEvidenceMap-Adjudication"
    ) |>
    httr2::req_body_json(list(
      message=request_id,
      content=jsonlite::base64_enc(charToRaw(payload)),
      branch=github_scoping_ref()
    ))
  marker_resp <- httr2::req_perform(marker_req)
  marker_status <- httr2::resp_status(marker_resp)
  if (!marker_status %in% c(200L,201L)) {
    stop(sprintf("W00 scoping fallback request failed with HTTP %d",marker_status),call.=FALSE)
  }
  invisible(TRUE)
}

find_w00_scoping_run <- function(request_id) {
  endpoint <- sprintf(
    "https://api.github.com/repos/%s/actions/workflows/workflow_00_search_scoping.yml/runs?branch=%s&per_page=30",
    github_dispatch_repo(),
    utils::URLencode(github_scoping_ref(), reserved=TRUE)
  )
  x <- github_api_get_json(endpoint)
  runs <- x$workflow_runs %||% list()
  wanted <- paste("W00 scope", as.character(request_id))
  hits <- Filter(function(r) {
    title_match <- identical(as.character(r$display_title %||% ""), wanted)
    head_message <- as.character((r$head_commit %||% list())$message %||% "")
    push_fallback_match <- identical(head_message, as.character(request_id))
    title_match || push_fallback_match
  }, runs)
  if (!length(hits)) return(NULL)
  hits[[1L]]
}

w00_scoping_run_jobs <- function(run_id) {
  endpoint <- sprintf(
    "https://api.github.com/repos/%s/actions/runs/%s/jobs?per_page=100",
    github_dispatch_repo(),
    as.character(run_id)
  )
  x <- github_api_get_json(endpoint)
  x$jobs %||% list()
}

w00_scoping_run_artifacts <- function(run_id) {
  endpoint <- sprintf(
    "https://api.github.com/repos/%s/actions/runs/%s/artifacts?per_page=100",
    github_dispatch_repo(),
    as.character(run_id)
  )
  x <- github_api_get_json(endpoint)
  x$artifacts %||% list()
}

download_github_artifact_zip <- function(artifact_id, destination) {
  endpoint <- sprintf(
    "https://api.github.com/repos/%s/actions/artifacts/%s/zip",
    github_dispatch_repo(),
    as.character(artifact_id)
  )
  req <- httr2::request(endpoint) |>
    httr2::req_headers(
      Authorization = paste("Bearer", github_dispatch_token()),
      Accept = "application/vnd.github+json",
      `X-GitHub-Api-Version` = "2022-11-28",
      `User-Agent` = "LivingEvidenceMap-Adjudication"
    )
  resp <- httr2::req_perform(req, path=destination)
  if (!file.exists(destination) || file.info(destination)$size <= 0L) {
    stop("Downloaded GitHub artifact is empty", call.=FALSE)
  }
  destination
}

read_w00_formal_record_artifact <- function(artifact) {
  name <- as.character(artifact$name %||% "")
  if(!grepl("^workflow00-search-record-[a-z0-9_]+-[0-9]+$",name))
    return(NULL)
  td <- tempfile("w00-formal-record-")
  dir.create(td,recursive=TRUE)
  on.exit(unlink(td,recursive=TRUE,force=TRUE),add=TRUE)
  download_github_artifact_zip(as.character(artifact$id),file.path(td,"record.zip"))
  utils::unzip(file.path(td,"record.zip"),exdir=td)
  paths <- list.files(td,pattern="\\.json$",recursive=TRUE,full.names=TRUE)
  expected_source <- sub("^workflow00-search-record-(.+)-[0-9]+$","\\1",name)
  # The archive may contain supplementary JSON provenance. Locate the actual
  # search record by schema fields and the source encoded in the artifact name.
  candidates <- lapply(paths,function(p) tryCatch(
    jsonlite::fromJSON(p,simplifyVector=FALSE),error=function(e) NULL))
  valid <- vapply(candidates,function(x) is.list(x) &&
    identical(as.character(x$source %||% ""),expected_source) &&
    !is.null(x$reported_search_results) &&
    !is.null(x$successfully_downloaded_results) &&
    !is.null(x$search_string),logical(1))
  if(sum(valid)!=1L) stop(sprintf(
    "Expected one matching W00 search record for %s; found %d among %d JSON files",
    expected_source,sum(valid),length(paths)),call.=FALSE)
  record <- candidates[[which(valid)]]
  source <- as.character(record$source %||% "")
  reported <- suppressWarnings(as.integer(record$reported_search_results))
  downloaded <- suppressWarnings(as.integer(record$successfully_downloaded_results))
  if(!grepl("^[a-z][a-z0-9_]*$",source) || is.na(reported) || is.na(downloaded))
    stop("Invalid W00 record counts",call.=FALSE)
  list(source=source,reported=reported,downloaded=downloaded,
    search_string=as.character(record$search_string %||% ""),
    search_date=substr(as.character(record$recorded_at_utc %||% ""),1L,10L))
}

read_w00_scoping_count_artifact <- function(artifact) {
  artifact_id <- as.character(artifact$id %||% "")
  artifact_name <- as.character(artifact$name %||% "")
  if (!grepl("^workflow00-scope-count-", artifact_name)) return(NULL)
  td <- tempfile("w00-scope-artifact-")
  dir.create(td, recursive=TRUE)
  zip_path <- file.path(td, "artifact.zip")
  on.exit(unlink(td, recursive=TRUE, force=TRUE), add=TRUE)
  download_github_artifact_zip(artifact_id, zip_path)
  utils::unzip(zip_path, exdir=td)
  csvs <- list.files(td, pattern="\\.csv$", recursive=TRUE, full.names=TRUE)
  if (length(csvs) != 1L) stop("Expected exactly one CSV in scoping count artifact", call.=FALSE)
  x <- utils::read.csv(csvs[[1L]], stringsAsFactors=FALSE, na.strings=c("", "NA"))
  if (nrow(x) != 1L) stop("Scoping count artifact must contain exactly one row", call.=FALSE)
  as.list(x[1L,,drop=FALSE])
}

read_w00_scoping_final_artifact <- function(artifact) {
  artifact_id <- as.character(artifact$id %||% "")
  artifact_name <- as.character(artifact$name %||% "")
  if (!grepl("^workflow00-search-scoping-", artifact_name)) return(NULL)
  td <- tempfile("w00-scope-final-")
  dir.create(td, recursive=TRUE)
  zip_path <- file.path(td, "artifact.zip")
  on.exit(unlink(td, recursive=TRUE, force=TRUE), add=TRUE)
  download_github_artifact_zip(artifact_id, zip_path)
  utils::unzip(zip_path, exdir=td)
  csvs <- list.files(td, pattern="search_scope_counts\\.csv$", recursive=TRUE, full.names=TRUE)
  if (length(csvs) != 1L) stop("Final scoping artifact is missing search_scope_counts.csv", call.=FALSE)
  utils::read.csv(csvs[[1L]], stringsAsFactors=FALSE, na.strings=c("", "NA"))
}



# Check a review-bound W01 export without conflating accepted dispatch and success.
# A handoff is definitive; failure is attributed only to one unambiguous matching run.
w01_export_delivery_status <- function(review_id, sheet_id, checkpoint_run_id, requested_at_utc = "") {
  assert_development_github_target()
  stopifnot(grepl("^[A-Za-z0-9._-]{3,128}$",review_id),
            grepl("^[A-Za-z0-9_-]{15,150}$",sheet_id),
            grepl("^[0-9]+$",checkpoint_run_id))
  path <- sprintf("docs/deduplication/handoffs/w01/run-%s.json",checkpoint_run_id)
  handoff <- tryCatch(jsonlite::fromJSON(read_github_text_file(path),simplifyVector=FALSE),
                      error=function(e) {
                        if(grepl("HTTP 404",conditionMessage(e),fixed=TRUE)) return(NULL)
                        stop(e)
                      })
  if(!is.null(handoff)) {
    if(!identical(as.character(handoff$review_id %||% ""),review_id) ||
       !identical(as.character(handoff$google_sheet_id %||% ""),sheet_id) ||
       !identical(as.character(handoff$checkpoint_run_id %||% ""),checkpoint_run_id))
      return(list(state="failed",message="W01 handoff identity mismatch. Contact the administrator; do not rerun deduplication."))
    return(list(state="exported",message="W01 decisions exported to GitHub and handoff published. Resume/final publication must still be verified."))
  }
  # GitHub workflow_dispatch returns HTTP 204, not a run ID. Attribute a run
  # only when exactly one export was created within the request's time window.
  # Otherwise explicitly report that delivery cannot be attributed yet.
  if(nzchar(requested_at_utc)) {
    started <- as.POSIXct(requested_at_utc,format="%Y-%m-%dT%H:%M:%SZ",tz="UTC")
    if(!is.na(started)) {
      endpoint <- sprintf(
        "https://api.github.com/repos/%s/actions/workflows/shiny-adjudication-dry-run-export-w01.yml/runs?branch=main&event=workflow_dispatch&per_page=100",
        github_dispatch_repo())
      runs <- github_api_get_json(endpoint)$workflow_runs %||% list()
      candidates <- Filter(function(x) {
        at <- as.POSIXct(as.character(x$created_at %||% ""),
                         format="%Y-%m-%dT%H:%M:%SZ",tz="UTC")
        !is.na(at) && as.numeric(difftime(at,started,units="secs")) >= -15 &&
          as.numeric(difftime(at,started,units="secs")) <= 180
      },runs)
      if(length(candidates)==1L) {
        run <- candidates[[1L]]
        url <- as.character(run$html_url %||% "")
        conclusion <- as.character(run$conclusion %||% "")
        if(identical(as.character(run$status %||% ""),"completed") &&
           !identical(conclusion,"success"))
          return(list(state="failed",message=paste0(
            "W01 GitHub export failed (",conclusion,"). Review the export run: ",url)))
        if(identical(as.character(run$status %||% ""),"completed") &&
           identical(conclusion,"success"))
          return(list(state="pending",message=paste0(
            "W01 export run completed, but the expected review-specific handoff is missing: ",url)))
        return(list(state="pending",message=paste0("W01 export running in GitHub: ",url)))
      }
    }
  }
  list(state="pending",message="W01 export request accepted; no verified review-specific handoff yet. GitHub run attribution is unavailable or ambiguous.")
}

dispatch_w01_export <- function(batch_id, queue_sha256, review_id="", google_sheet_id="") {
  batch_id <- as.character(batch_id)
  queue_sha256 <- tolower(as.character(queue_sha256))
  if (!nzchar(batch_id)) stop("Invalid W01 batch ID", call.=FALSE)
  if (!grepl("^[0-9a-f]{64}$", queue_sha256)) stop("Invalid W01 queue SHA-256", call.=FALSE)
  review_id <- trimws(as.character(review_id))
  google_sheet_id <- trimws(as.character(google_sheet_id))
  if(xor(nzchar(review_id),nzchar(google_sheet_id)))
    stop("Review-specific W01 export requires both review_id and Google Sheet ID",call.=FALSE)
  if(nzchar(review_id) && (!grepl("^[A-Za-z0-9._-]{3,128}$",review_id) ||
     !grepl("^[A-Za-z0-9_-]{15,150}$",google_sheet_id)))
    stop("Invalid review-specific W01 export identity",call.=FALSE)
  checkpoint_run_id <- sub("^w01-run-","",batch_id)
  if(nzchar(review_id) && (!grepl("^w01-run-[0-9]+$",batch_id) ||
      !grepl("^[0-9]+$",checkpoint_run_id)))
    stop("Review-specific W01 export must identify the originating W01 run",call.=FALSE)

  endpoint <- sprintf(
    "https://api.github.com/repos/%s/actions/workflows/shiny-adjudication-dry-run-export-w01.yml/dispatches",
    github_dispatch_repo()
  )

  req <- httr2::request(endpoint) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Authorization = paste("Bearer", github_dispatch_token()),
      Accept = "application/vnd.github+json",
      `X-GitHub-Api-Version` = "2022-11-28",
      `User-Agent` = "LivingEvidenceMap-Adjudication"
    ) |>
    httr2::req_body_json(list(
      ref = github_scoping_ref(),
      inputs = if(nzchar(review_id)) list(review_id=review_id,
        google_sheet_id=google_sheet_id,checkpoint_run_id=checkpoint_run_id) else list()
    ))

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  if (identical(status, 204L)) return(invisible(TRUE))
  if (!identical(status, 404L)) {
    stop(sprintf("W01 export workflow dispatch failed with HTTP %d", status), call.=FALSE)
  }
  # The legacy push-marker fallback cannot carry trusted workflow-dispatch
  # review identity. Refuse it rather than export another review's Google Sheet.
  if(nzchar(review_id))
    stop("Authenticated W01 review export dispatch endpoint returned 404; refusing unscoped fallback",call.=FALSE)

  stamp <- format(Sys.time(), tz="UTC", format="%Y%m%dT%H%M%SZ")
  marker_path <- sprintf(
    "docs/deduplication/w01_export_requests/%s-%s.json",
    batch_id, stamp
  )
  payload <- jsonlite::toJSON(list(
    batch_id=batch_id,
    queue_sha256=queue_sha256,
    requested_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"),
    requested_by="LivingEvidenceMapAdjudication"
  ),auto_unbox=TRUE,pretty=TRUE)

  endpoint <- sprintf(
    "https://api.github.com/repos/%s/contents/%s",
    github_dispatch_repo(), marker_path
  )
  marker_req <- httr2::request(endpoint) |>
    httr2::req_method("PUT") |>
    httr2::req_headers(
      Authorization = paste("Bearer", github_dispatch_token()),
      Accept = "application/vnd.github+json",
      `X-GitHub-Api-Version` = "2022-11-28",
      `User-Agent` = "LivingEvidenceMap-Adjudication"
    ) |>
    httr2::req_body_json(list(
      message=sprintf("Request W01 export for %s",batch_id),
      content=jsonlite::base64_enc(charToRaw(payload)),
      branch=github_scoping_ref()
    ))

  marker_resp <- httr2::req_perform(marker_req)
  marker_status <- httr2::resp_status(marker_resp)
  if (!marker_status %in% c(200L,201L)) {
    stop(sprintf("W01 export request fallback failed with HTTP %d",marker_status),call.=FALSE)
  }
  invisible(TRUE)
}

# Launch the existing W02 production workflow using the active review's
# authoritative final W01 pointer. Never use another review's pointer.
dispatch_w02_from_shiny <- function(review_id, sheet_id) {
  assert_development_github_target()
  review_id <- trimws(as.character(review_id))
  sheet_id <- trimws(as.character(sheet_id))
  if(!grepl("^[A-Za-z0-9._-]{3,128}$",review_id) ||
     !grepl("^[A-Za-z0-9_-]{15,150}$",sheet_id))
    stop("Invalid W02 review identity",call.=FALSE)
  directory <- paste0("docs/deduplication/reviews/",review_id,"/zenodo")
  endpoint <- sprintf("https://api.github.com/repos/%s/contents/%s?ref=main",
                      github_dispatch_repo(),directory)
  entries <- github_api_get_json(endpoint)
  names <- vapply(entries,function(x) as.character(x$name %||% ""),character(1))
  names <- names[grepl("^run-[0-9]+\\.json$",names)]
  if(!length(names)) stop("No final W01 pointer found for this review",call.=FALSE)
  ids <- as.numeric(sub("^run-([0-9]+)\\.json$","\\1",names))
  ordered <- names[order(ids,decreasing=TRUE)]
  pointer <- NULL
  pointer_path <- ""
  for(n in ordered) {
    candidate <- paste0(directory,"/",n)
    x <- jsonlite::fromJSON(read_github_text_file(candidate),simplifyVector=FALSE)
    if(identical(as.character(x$workflow %||% ""),"01") &&
       identical(as.character(x$state %||% ""),"final") &&
       identical(as.character(x$status %||% ""),"published")) {
      pointer <- x
      pointer_path <- candidate
      break
    }
  }
  if(is.null(pointer) ||
     !identical(as.character(pointer$review_id %||% ""),review_id) ||
     !identical(as.character(pointer$google_sheet_id %||% ""),sheet_id))
    stop("Final W01 pointer is missing or does not match the active review",call.=FALSE)
  endpoint <- sprintf(
    "https://api.github.com/repos/%s/actions/workflows/workflow_02_batched_production.yml/dispatches",
    github_dispatch_repo())
  req <- httr2::request(endpoint) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Authorization=paste("Bearer",github_dispatch_token()),
      Accept="application/vnd.github+json",
      `X-GitHub-Api-Version`="2022-11-28",
      `User-Agent`="LivingEvidenceMap-Adjudication") |>
    httr2::req_body_json(list(ref=github_scoping_ref(),
      inputs=list(first_run="true",workflow01_pointer_path=pointer_path,
                  previous_workflow02_pointer_path="",google_sheet_id=sheet_id)))
  resp <- httr2::req_perform(req)
  if(!identical(httr2::resp_status(resp),204L))
    stop(sprintf("W02 launch failed with HTTP %d",httr2::resp_status(resp)),call.=FALSE)
  invisible(pointer_path)
}

# Track a W02 launch without choosing the latest global run.
w02_find_shiny_run <- function(launched_at_utc) {
  assert_development_github_target()
  launched <- as.POSIXct(launched_at_utc,format="%Y-%m-%dT%H:%M:%SZ",tz="UTC")
  if(length(launched)!=1L || is.na(launched)) stop("Invalid W02 launch time",call.=FALSE)
  endpoint <- sprintf(
    "https://api.github.com/repos/%s/actions/workflows/workflow_02_batched_production.yml/runs?branch=main&event=workflow_dispatch&per_page=100",
    github_dispatch_repo())
  runs <- github_api_get_json(endpoint)$workflow_runs %||% list()
  matches <- Filter(function(x) {
    when <- as.POSIXct(as.character(x$created_at %||% ""),format="%Y-%m-%dT%H:%M:%SZ",tz="UTC")
    length(when)==1L && !is.na(when) &&
      as.numeric(difftime(when,launched,units="secs")) >= -10 &&
      as.numeric(difftime(when,launched,units="secs")) <= 300
  },runs)
  if(length(matches)>1L) stop("Multiple W02 launches occurred in the matching window; run identity ambiguous",call.=FALSE)
  if(!length(matches)) return(NULL)
  matches[[1L]]
}

w02_github_run_progress <- function(run_id) {
  assert_development_github_target()
  id <- as.character(run_id)
  if(length(id)!=1L || !grepl("^[0-9]+$",id)) stop("Invalid W02 run ID",call.=FALSE)
  base <- sprintf("https://api.github.com/repos/%s/actions/runs/%s",github_dispatch_repo(),id)
  run <- github_api_get_json(base)
  if(!grepl("(^|/)\\.github/workflows/workflow_02_batched_production\\.yml(@|$)",
             as.character(run$path %||% "")))
    stop("Run is not W02 production",call.=FALSE)
  if(!identical(as.character(run$head_branch %||% ""),"main"))
    stop("W02 run is not on main",call.=FALSE)
  jobs <- github_api_get_json(paste0(base,"/jobs?per_page=100"))$jobs %||% list()
  active <- Filter(function(j) !identical(as.character(j$conclusion %||% ""),"skipped"),jobs)
  steps <- unlist(lapply(active,function(j)j$steps %||% list()),recursive=FALSE)
  total <- length(steps)
  completed <- sum(vapply(steps,function(x)identical(as.character(x$status %||% ""),"completed"),logical(1)))
  pct <- if(total) min(99L,as.integer(floor(100*completed/total))) else 0L
  if(identical(as.character(run$status %||% ""),"completed") &&
     identical(as.character(run$conclusion %||% ""),"success")) pct<-100L
  list(run_id=id,status=as.character(run$status %||% ""),
       conclusion=as.character(run$conclusion %||% ""),
       progress=pct,completed_steps=completed,total_steps=total)
}

# Explicit opt-in only: create a separate missing-abstract queue for one W02 run.
dispatch_w02_missing_abstract_queue <- function(source_run_id, google_sheet_id) {
  assert_development_github_target()
  if(!grepl("^[0-9]+$",as.character(source_run_id)) ||
     !grepl("^[A-Za-z0-9_-]{15,150}$",as.character(google_sheet_id)))
    stop("Invalid W02 missing-abstract queue identity",call.=FALSE)
  endpoint <- sprintf(
    "https://api.github.com/repos/%s/actions/workflows/workflow_02_optional_missing_abstracts.yml/dispatches",
    github_dispatch_repo())
  resp <- httr2::request(endpoint) |>
    httr2::req_method("POST") |>
    httr2::req_headers(Authorization=paste("Bearer",github_dispatch_token()),
      Accept="application/vnd.github+json",
      `X-GitHub-Api-Version`="2022-11-28",
      `User-Agent`="LivingEvidenceMap-Adjudication") |>
    httr2::req_body_json(list(ref=github_scoping_ref(),inputs=list(
      source_run_id=as.character(source_run_id),
      google_sheet_id=as.character(google_sheet_id)))) |>
    httr2::req_perform()
  if(!identical(httr2::resp_status(resp),204L))
    stop(sprintf("W02 optional abstract queue dispatch returned HTTP %d",httr2::resp_status(resp)),call.=FALSE)
  invisible(TRUE)
}

dispatch_w02_resume <- function(source_run_id, publish = TRUE, google_sheet_id = "") {
  source_run_id <- as.character(source_run_id)
  if (!grepl("^[0-9]+$", source_run_id)) stop("Invalid W02 source run ID", call. = FALSE)
  if(nzchar(google_sheet_id) && !grepl("^[A-Za-z0-9_-]{15,150}$",google_sheet_id))
    stop("Invalid W02 review Google Sheet ID",call.=FALSE)

  endpoint <- sprintf(
    "https://api.github.com/repos/%s/actions/workflows/workflow_02_resume_after_human_review.yml/dispatches",
    github_dispatch_repo()
  )

  req <- httr2::request(endpoint) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Authorization = paste("Bearer", github_dispatch_token()),
      Accept = "application/vnd.github+json",
      `X-GitHub-Api-Version` = "2022-11-28",
      `User-Agent` = "LivingEvidenceMap-Adjudication"
    ) |>
    httr2::req_body_json(list(
      ref = github_scoping_ref(),
      inputs = list(
        source_run_id = source_run_id,
        google_sheet_id = google_sheet_id,
        publish = if (isTRUE(publish)) "true" else "false"
      )
    ))

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  if (identical(status, 204L)) return(invisible(TRUE))

  stop(sprintf("GitHub W02 resume dispatch failed with HTTP %d", status), call. = FALSE)
}


w04_validation_dispatch_payload <- function(batch_id, queue_sha256) {
  batch_id <- as.character(batch_id)
  queue_sha256 <- tolower(as.character(queue_sha256))
  if (!nzchar(batch_id)) stop("Invalid W04 batch ID", call. = FALSE)
  if (!grepl("^[0-9a-f]{64}$", queue_sha256)) stop("Invalid W04 queue SHA-256", call. = FALSE)

  list(
    ref = github_scoping_ref(),
    inputs = list(
      batch_id = batch_id,
      queue_sha256 = queue_sha256
    )
  )
}

dispatch_w04_validation_finalize <- function(batch_id, queue_sha256) {
  payload <- w04_validation_dispatch_payload(batch_id, queue_sha256)

  endpoint <- sprintf(
    "https://api.github.com/repos/%s/actions/workflows/workflow_04_finalize_human_validation.yml/dispatches",
    github_dispatch_repo()
  )

  req <- httr2::request(endpoint) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Authorization = paste("Bearer", github_dispatch_token()),
      Accept = "application/vnd.github+json",
      `X-GitHub-Api-Version` = "2022-11-28",
      `User-Agent` = "LivingEvidenceMap-Adjudication"
    ) |>
    httr2::req_body_json(payload)

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  if (!identical(status, 204L)) {
    stop(sprintf("W04 finalisation dispatch failed with HTTP %d", status), call. = FALSE)
  }

  invisible(TRUE)
}


dispatch_w08_resume <- function(source_run_id, batch_id, queue_sha256) {
  source_run_id <- as.character(source_run_id)
  batch_id <- as.character(batch_id)
  queue_sha256 <- tolower(as.character(queue_sha256))
  if (!grepl("^[0-9]+$", source_run_id)) stop("Invalid W08 source run ID", call. = FALSE)
  if (!nzchar(batch_id)) stop("Invalid W08 batch ID", call. = FALSE)
  if (!grepl("^[0-9a-f]{64}$", queue_sha256)) stop("Invalid W08 queue SHA-256", call. = FALSE)

  endpoint <- sprintf(
    "https://api.github.com/repos/%s/actions/workflows/workflow_08_resume_after_shiny.yml/dispatches",
    github_dispatch_repo()
  )

  req <- httr2::request(endpoint) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Authorization = paste("Bearer", github_dispatch_token()),
      Accept = "application/vnd.github+json",
      `X-GitHub-Api-Version` = "2022-11-28",
      `User-Agent` = "LivingEvidenceMap-Adjudication"
    ) |>
    httr2::req_body_json(list(
      ref = github_scoping_ref(),
      inputs = list(
        source_run_id = source_run_id,
        batch_id = batch_id,
        queue_sha256 = queue_sha256
      )
    ))

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  if (!identical(status, 204L)) {
    stop(sprintf("W08 resume dispatch failed with HTTP %d", status), call. = FALSE)
  }

  invisible(TRUE)
}


dispatch_w04_resolution_resume <- function(source_run_id, batch_id, queue_sha256) {
  source_run_id <- as.character(source_run_id)
  batch_id <- as.character(batch_id)
  queue_sha256 <- tolower(as.character(queue_sha256))
  if (!grepl("^[0-9]+$", source_run_id)) stop("Invalid W04 source run ID", call. = FALSE)
  if (!nzchar(batch_id)) stop("Invalid W04 batch ID", call. = FALSE)
  if (!grepl("^[0-9a-f]{64}$", queue_sha256)) stop("Invalid W04 queue SHA-256", call. = FALSE)

  endpoint <- sprintf(
    "https://api.github.com/repos/%s/actions/workflows/workflow_04_resume_after_shiny.yml/dispatches",
    github_dispatch_repo()
  )

  req <- httr2::request(endpoint) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Authorization = paste("Bearer", github_dispatch_token()),
      Accept = "application/vnd.github+json",
      `X-GitHub-Api-Version` = "2022-11-28",
      `User-Agent` = "LivingEvidenceMap-Adjudication"
    ) |>
    httr2::req_body_json(list(
      ref = github_scoping_ref(),
      inputs = list(
        source_run_id = source_run_id,
        batch_id = batch_id,
        queue_sha256 = queue_sha256
      )
    ))

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  if (!identical(status, 204L)) {
    stop(sprintf("W04 resolution resume dispatch failed with HTTP %d", status), call. = FALSE)
  }
  invisible(TRUE)
}
