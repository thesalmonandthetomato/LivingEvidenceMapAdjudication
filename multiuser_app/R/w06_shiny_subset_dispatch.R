# W06 Shiny-only model trials. Production W06 stays untouched.
w06_shiny_published_w05 <- function(meta) {
  id <- w05_crop_draft_id(meta)
  if(!nzchar(id))stop("Final W05 Zenodo record is not registered",call.=FALSE)
  dep <- w04_prompt_zenodo_request(w04_prompt_deposit_endpoint(id))
  if(!isTRUE(dep$submitted) ||
     !identical(as.character(dep$metadata$title),
       paste("Living Evidence Map W05 crop coding |",meta$review_id)))
    stop("W06 requires published W05 results for the active review",call.=FALSE)
  names <- vapply(dep$files %||% list(),
    function(f)as.character(f$filename %||% ""),character(1))
  if(!all(c("w05-final-manifest.json","w05-final-assignments.csv",
            "w05-final-frequencies.csv") %in% names))
    stop("W05 finalised result artefacts not found",call.=FALSE)
  id
}
w06_shiny_subset_dispatch <- function(meta,prompt_receipt,subset_count) {
  w05_id <- w06_shiny_published_w05(meta)
  if(is.null(prompt_receipt) ||
     !grepl("^[0-9]+$",as.character(prompt_receipt$draft_id)) ||
     !grepl("^[a-f0-9]{64}$",as.character(prompt_receipt$sha256)))
    stop("An approved W06 prompt is required",call.=FALSE)
  # Check the approval marker in the archived prompt deposition, not merely
  # the Shiny session state.
  dep <- w06_shiny_prompt_deposit(meta)
  if(is.null(dep)||!identical(as.character(dep$id),as.character(prompt_receipt$draft_id)))
    stop("Archived W06 prompt draft identity mismatch",call.=FALSE)
  expected <- paste0("w06-approved-",prompt_receipt$sha256,".json")
  approved <- Filter(function(f)identical(as.character(f$filename),expected),dep$files %||% list())
  if(length(approved)!=1L)stop("The selected W06 prompt version is not approved",call.=FALSE)
  n <- suppressWarnings(as.integer(subset_count))
  if(is.na(n)||n<1L||as.character(n)!=as.character(subset_count))
    stop("W06 subset count must be a positive integer",call.=FALSE)
  payload <- list(ref="main",inputs=list(
    review_id=meta$review_id,w05_deposit_id=w05_id,
    prompt_deposit_id=as.character(prompt_receipt$draft_id),
    prompt_sha256=as.character(prompt_receipt$sha256),
    subset_count=as.character(n)))
  url <- sprintf("https://api.github.com/repos/%s/actions/workflows/w06_shiny_subset.yml/dispatches",
    w05_shiny_repo())
  w05_shiny_gh_request(url,"POST",payload)
  invisible(TRUE)
}
w06_shiny_subset_results <- function(run_id,download=NULL) {
  id <- as.character(run_id)
  if(!grepl("^[0-9]+$",id))stop("Invalid W06 trial run ID",call.=FALSE)
  prefix <- paste0("https://api.github.com/repos/",w05_shiny_repo())
  run <- w05_shiny_gh_request(paste0(prefix,"/actions/runs/",id))
  if(!identical(as.character(run$path %||% "" ),".github/workflows/w06_shiny_subset.yml") ||
     !identical(as.character(run$conclusion %||% ""),"success"))
    stop("W06 run has not successfully completed in the isolated subset pathway",call.=FALSE)
  response <- w05_shiny_gh_request(paste0(prefix,"/actions/runs/",id,"/artifacts"))
  expected <- paste0("w06-shiny-subset-",id)
  found <- Filter(function(x)identical(as.character(x$name %||% ""),expected) &&
      !isTRUE(x$expired),response$artifacts %||% list())
  if(length(found)!=1L)stop("Expected one unexpired W06 results artefact",call.=FALSE)
  url <- as.character(found[[1L]]$archive_download_url %||% "")
  if(!startsWith(url,paste0(prefix,"/actions/artifacts/")))
    stop("Unexpected W06 results archive source",call.=FALSE)
  if(is.null(download))download <- function(url,dest){
    req <- httr2::request(url) |> httr2::req_headers(
      Authorization=paste("Bearer",github_dispatch_token()),
      Accept="application/vnd.github+json",
      `User-Agent`="LivingEvidenceMap-Adjudication")
    httr2::req_perform(httr2::req_timeout(req,90),path=dest)
  }
  zip <- tempfile(fileext=".zip")
  target <- tempfile("w06-shiny-")
  dir.create(target)
  on.exit(unlink(c(zip,target),recursive=TRUE),add=TRUE)
  download(url,zip)
  entries <- utils::unzip(zip,list=TRUE)$Name
  if(!setequal(entries,c("w06_subset_results.jsonl","w06_subset_manifest.json")))
    stop("Unexpected W06 results archive contents",call.=FALSE)
  utils::unzip(zip,exdir=target)
  manifest <- jsonlite::fromJSON(file.path(target,"w06_subset_manifest.json"),simplifyVector=FALSE)
  if(!identical(as.character(manifest$schema),"lem-w06-shiny-subset-v1") ||
     !identical(as.character(manifest$mode),"subset_test"))
    stop("Unexpected W06 subset manifest schema",call.=FALSE)
  lines <- readLines(file.path(target,"w06_subset_results.jsonl"),warn=FALSE,encoding="UTF-8")
  rows <- lapply(lines[nzchar(trimws(lines))],function(line)jsonlite::fromJSON(line,simplifyVector=FALSE))
  ids <- vapply(rows,function(x)as.character(x$record_id),character(1))
  if(anyDuplicated(ids)||length(ids)!=as.integer(manifest$subset_records))
    stop("W06 subset result IDs do not match manifest count",call.=FALSE)
  if(any(vapply(rows,function(x)!identical(x$prompt_sha256,manifest$prompt_sha256),logical(1))))
    stop("W06 results include mismatched prompt versions",call.=FALSE)
  scalar <- function(x)if(is.null(x)||!length(x))"" else as.character(x[[1L]])
  table <- do.call(rbind,lapply(rows,function(x)data.frame(
    record_id=scalar(x$record_id),title=scalar(x$title),abstract=scalar(x$abstract),
    execution_status=scalar(x$status),
    geography_status=scalar((x$coding %||% list())$geography_status),
    locations=jsonlite::toJSON((x$coding %||% list())$locations %||% list(),auto_unbox=TRUE),
    explanation=scalar((x$coding %||% list())$explanation),
    error=scalar(x$error),stringsAsFactors=FALSE)))
  list(manifest=manifest,results=table,run_url=as.character(run$html_url))
}
