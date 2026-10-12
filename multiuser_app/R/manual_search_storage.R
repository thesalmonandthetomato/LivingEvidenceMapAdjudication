manual_search_review_id <- function() {
  if(!identical(storage_backend(),"google_sheets"))
    return(gsub("[^A-Za-z0-9._-]","-",Sys.getenv("LEM_SEARCH_REVIEW_ID",
      unset="local-review")))
  gs4_auth_from_env()
  meta <- as.data.frame(googlesheets4::read_sheet(sheet_id_from_env(),
    sheet="review_metadata",col_types="cc"))
  i <- match("review_id",as.character(meta$key))
  if(is.na(i)) stop("Review metadata is missing review_id",call.=FALSE)
  id <- as.character(meta$value[[i]])
  if(is.na(id) || !grepl("^[A-Za-z0-9][A-Za-z0-9._-]{3,127}$",id))
    stop("Invalid review-specific metadata ID",call.=FALSE)
  id
}

# Review-specific manual W00 searches: restricted Zenodo files and Sheet provenance.
# This module never dispatches W01 or writes RIS research records into GitHub.
manual_search_columns <- c("review_key","submission_id","database","platform","search_date",
  "search_string","results_reported","records_downloaded","file_count",
  "files_json","zenodo_record_id","zenodo_receipt_json","created_at_utc")
manual_search_log_path <- function() Sys.getenv("LEM_MANUAL_SEARCH_LOG",
  unset="local_state/manual_search_registry.jsonl")
manual_search_registry <- function(review_key) {
  if(identical(storage_backend(),"google_sheets")) {
    gs4_auth_from_env()
    ss <- sheet_id_from_env()
    if(!"manual_search_registry"%in%sheet_names_cached(ss)) return(data.frame())
    rows <- googlesheets4::read_sheet(ss,sheet="manual_search_registry",col_types="c")
  } else {
    path <- manual_search_log_path()
    if(!file.exists(path)) return(data.frame())
    lines <- readLines(path,warn=FALSE)
    if(!length(lines)) return(data.frame())
    rows <- do.call(rbind,lapply(lines,function(x)
      as.data.frame(jsonlite::fromJSON(x),stringsAsFactors=FALSE)))
  }
  if(!all(manual_search_columns%in%names(rows)))
    stop("Manual search registry has incompatible columns",call.=FALSE)
  rows[!is.na(rows$review_key)&rows$review_key==review_key,,drop=FALSE]
}
manual_search_append <- function(row) {
  if(!identical(sort(names(row)),sort(manual_search_columns)))
    stop("Invalid manual search registry record",call.=FALSE)
  old <- manual_search_registry(row$review_key)
  if(nrow(old)&&row$submission_id %in% old$submission_id)
    stop("This submission is already registered",call.=FALSE)
  if(identical(storage_backend(),"google_sheets")) {
    gs4_auth_from_env()
    ss <- sheet_id_from_env()
    if(!"manual_search_registry"%in%sheet_names_cached(ss)) {
      empty <- as.data.frame(setNames(replicate(length(manual_search_columns),
        character(),simplify=FALSE),manual_search_columns),stringsAsFactors=FALSE)
      googlesheets4::sheet_write(empty,ss=ss,sheet="manual_search_registry")
      invalidate_sheet_names_cache(ss)
    }
    googlesheets4::sheet_append(ss,
      data=as.data.frame(row,stringsAsFactors=FALSE),sheet="manual_search_registry")
  } else {
    path <- manual_search_log_path()
    dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
    cat(as.character(jsonlite::toJSON(row,auto_unbox=TRUE,na="null")),"\n",
      file=path,append=TRUE,sep="")
  }
  updated <- manual_search_registry(row$review_key)
  if(!row$submission_id %in% updated$submission_id)
    stop("Zenodo was published but manual registry verification failed; reconcile before retrying",call.=FALSE)
  invisible(TRUE)
}
manual_search_zenodo_publish <- function(review_id,submission_id,prepared,files) {
  token <- Sys.getenv("ZENODO_ACCESS_TOKEN",unset="")
  if(!nzchar(token))
    stop("ZENODO_ACCESS_TOKEN must be configured privately in the Shiny deployment",call.=FALSE)
  if(!grepl("^[A-Za-z0-9][A-Za-z0-9._-]{3,127}$",review_id))
    stop("Invalid review ID",call.=FALSE)
  if(!grepl("^manual-[A-Za-z0-9-]+$",submission_id))
    stop("Invalid manual submission ID",call.=FALSE)
  if(nrow(files)!=length(prepared$files))
    stop("Uploaded files differ from the validated submission",call.=FALSE)
  for(i in seq_len(nrow(files))) {
    actual <- digest::digest(file=files$datapath[[i]],algo="sha256")
    if(!identical(actual,prepared$files[[i]]$sha256))
      stop("RIS file changed after validation",call.=FALSE)
  }
  config <- jsonlite::fromJSON(read_github_text_file("user_input/zenodo_config.json"),
    simplifyVector=FALSE)
  if(is.null(config$creators)||!length(config$creators))
    stop("Zenodo creators are not configured",call.=FALSE)
  perform <- function(req,expected,label) {
    resp <- httr2::req_perform(httr2::req_error(httr2::req_timeout(req,600),
      is_error=function(resp)FALSE))
    if(!httr2::resp_status(resp)%in%expected)
      stop(sprintf("Zenodo %s failed (HTTP %s): %s",label,httr2::resp_status(resp),
        substr(httr2::resp_body_string(resp),1,300)),call.=FALSE)
    resp
  }
  auth <- function(url) httr2::req_headers(httr2::request(url),
    Authorization=paste("Bearer",token))
  api <- "https://zenodo.org/api/deposit/depositions"
  created <- httr2::resp_body_json(perform(
    httr2::req_body_raw(httr2::req_method(auth(api),"POST"),
      charToRaw("{}"),type="application/json"),201L,"draft creation"),simplifyVector=FALSE)
  id <- as.character(created$id)
  bucket <- as.character(created$links$bucket)
  if(!nzchar(id)||!nzchar(bucket)) stop("Zenodo draft response lacked an upload location",call.=FALSE)
  # A failed operation may leave a private draft; surface its ID for recovery.
  tryCatch({
    metadata <- list(metadata=list(
      title=sprintf("Living Evidence Map manual searches | review %s | %s",review_id,submission_id),
      upload_type="dataset",publication_date=format(Sys.Date(),"%Y-%m-%d"),
      description=paste0("<p>Restricted manually exported bibliographic search records.",
        " Original RIS files and search provenance for review ",review_id,
        ", database ",prepared$database,".</p>"),
      creators=config$creators,access_right="restricted",
      access_conditions="Bibliographic exports are restricted because source licensing may limit redistribution.",
      keywords=c("Living Evidence Map","manual search",paste0("review:",review_id),
        paste0("submission:",submission_id))))
    perform(httr2::req_body_json(httr2::req_method(auth(paste0(api,"/",id)),"PUT"),
      metadata,auto_unbox=TRUE),200L,"metadata update")
    # Distinct stable file names preserve split exports without collisions.
    staged <- tempfile("lem-manual-")
    dir.create(staged)
    on.exit(unlink(staged,recursive=TRUE,force=TRUE),add=TRUE)
    files_meta <- prepared$files
    for(i in seq_len(nrow(files))) {
      safe <- gsub("[^A-Za-z0-9._-]","_",basename(files$name[[i]]))
      archive_name <- sprintf("%03d_%s",i,safe)
      dest <- file.path(staged,archive_name)
      if(!file.copy(files$datapath[[i]],dest))
        stop("Could not stage RIS export",call.=FALSE)
      files_meta[[i]]$archive_name <- archive_name
    }
    manifest <- list(schema="lem-manual-search-v1",review_id=review_id,
      submission_id=submission_id,database=prepared$database,platform=prepared$platform,
      search_date=prepared$search_date,search_string=prepared$search_string,
      results_reported=prepared$reported_hits,records_downloaded=prepared$imported_records,
      files=files_meta)
    manifest_path <- file.path(staged,"manifest.json")
    writeLines(jsonlite::toJSON(manifest,auto_unbox=TRUE,pretty=TRUE,null="null"),
      manifest_path,useBytes=TRUE)
    for(path in c(file.path(staged,vapply(files_meta,`[[`,character(1),"archive_name")),manifest_path)) {
      url <- paste0(bucket,"/",utils::URLencode(basename(path),reserved=TRUE))
      perform(httr2::req_body_file(httr2::req_method(auth(url),"PUT"),path),
        c(200L,201L),paste("file",basename(path)))
    }
    published <- httr2::resp_body_json(perform(
      httr2::req_method(auth(paste0(api,"/",id,"/actions/publish")),"POST"),
      c(200L,201L,202L),"publication"),simplifyVector=FALSE)
    list(status="published",zenodo_record_id=as.character(
      published$record_id %||% published$id),deposition_id=id,
      submission_id=submission_id,review_id=review_id,
      record_url=as.character(published$links$html %||%
        paste0("https://zenodo.org/records/",id)),files=files_meta)
  },error=function(e)stop(sprintf("Zenodo draft %s: %s",id,conditionMessage(e)),call.=FALSE))
}
manual_search_successful_runs <- function() {
  if(!identical(storage_backend(),"google_sheets")) return(character())
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  metadata <- as.data.frame(googlesheets4::read_sheet(ss,
    sheet="review_metadata",col_types="cc"))
  events <- as.data.frame(googlesheets4::read_sheet(ss,
    sheet="workflow_events",col_types="cccccccc"))
  if(!all(c("key","value") %in% names(metadata)) ||
     !all(c("review_id","workflow","event_type","github_run_id") %in% names(events)))
    stop("Review search registry schema is incomplete",call.=FALSE)
  review_id <- as.character(metadata$value[match("review_id",metadata$key)])
  if(length(review_id)!=1L || is.na(review_id) || !nzchar(review_id))
    stop("Current review ID is unavailable",call.=FALSE)
  matched <- events$review_id==review_id & events$workflow=="W00" &
    events$event_type=="zenodo_published"
  matched[is.na(matched)] <- FALSE
  ids <- unique(as.character(events$github_run_id[matched]))
  ids[!is.na(ids)&grepl("^[0-9]+$",ids)]
}

manual_search_api_history <- function(run_id) {
  if(!grepl("^[0-9]+$",run_id)) stop("Invalid W00 run ID",call.=FALSE)
  artifacts <- w00_scoping_run_artifacts(run_id)
  records <- Filter(function(a)grepl(
    paste0("^workflow00-search-record-[a-z0-9_]+-",run_id,"$"),
    as.character(a$name %||% "")),artifacts)
  if(!length(records))stop("W00 search-record artifacts are not available",call.=FALSE)
  lapply(records,function(a) {
    x <- read_w00_formal_record_artifact(a)
    x
  })
}
