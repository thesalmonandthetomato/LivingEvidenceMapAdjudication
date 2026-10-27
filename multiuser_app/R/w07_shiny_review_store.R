# W07 experimental review decisions: append-only Google Sheets event ledger.
# One read on trial open; one append per save; no Sheets calls on navigation.
# This store is isolated from W04 screening and W08 production annotation.
w07_review_tab <- function() "w07_coding_decision_events"
w07_review_fields <- c("event_id","review_id","run_id","record_id","user_id",
  "codes","relevance","reason","timestamp","ontology_sha","prompt_sha",
  "case_sha256","queue_sha256","version","supersedes_event_id")

w07_review_legacy_fields <- w07_review_fields[seq_len(11L)]
w07_review_sheet <- function(meta,create=FALSE) {
  gs4_auth_from_env()
  ss <- meta$sheet_id
  tab <- w07_review_tab()
  if(!tab %in% sheet_names_cached(ss)) {
    if(!create)return(FALSE)
    blank <- as.data.frame(setNames(replicate(length(w07_review_fields),
      character(),simplify=FALSE),w07_review_fields),stringsAsFactors=FALSE)
    googlesheets4::sheet_write(blank,ss=ss,sheet=tab)
    invalidate_sheet_names_cache(ss)
  }
  TRUE
}
w07_review_read_rows <- function(meta) {
  if(!w07_review_sheet(meta,create=FALSE))return(NULL)
  rows <- as.data.frame(googlesheets4::read_sheet(meta$sheet_id,
    sheet=w07_review_tab(),col_types="c"),stringsAsFactors=FALSE)
  names_now <- names(rows)
  if(identical(names_now,w07_review_legacy_fields)) {
    for(field in setdiff(w07_review_fields,names_now))
      rows[[field]] <- rep("",nrow(rows))
    attr(rows,"legacy_header") <- TRUE
  } else if(!identical(names_now,w07_review_fields)) {
    stop("Unrecognised W07 decision ledger schema; no data were modified",call.=FALSE)
  }
  rows
}
w07_review_events <- function(meta,run_id) {
  id <- as.character(run_id)
  if(length(id)!=1L||!grepl("^[0-9]+$",id))
    stop("Invalid W07 run ID",call.=FALSE)
  rows <- w07_review_read_rows(meta)
  if(is.null(rows))return(list())
  rows <- rows[!is.na(rows$review_id)&rows$review_id==meta$review_id&
    !is.na(rows$run_id)&rows$run_id==id,,drop=FALSE]
  out <- lapply(seq_len(nrow(rows)),function(i)as.list(rows[i,,drop=FALSE]))
  if(length(out))out <- out[order(vapply(out,function(e)
    as.character(e$timestamp),character(1)),seq_along(out))]
  out
}
# Safe additive migration: extend only the header; no decision row is rewritten.
# The caller must pass its single batch read to avoid a second read on save.
w07_review_ensure_schema <- function(meta,legacy_header=FALSE) {
  w07_review_sheet(meta,create=TRUE)
  if(isTRUE(legacy_header)) {
    googlesheets4::range_write(meta$sheet_id,
      data=as.data.frame(as.list(stats::setNames(w07_review_fields,w07_review_fields)),
        stringsAsFactors=FALSE)[0,,drop=FALSE],
      sheet=w07_review_tab(),range="A1:O1",col_names=TRUE)
  }
  invisible(TRUE)
}
w07_review_save <- function(meta,run_id,record_id,user_id,codes,relevance,reason,
                            ontology_sha="",prompt_sha="",case_sha256="",
                            queue_sha256="",prior_decision=NULL,allowed_codes=NULL) {
  id <- as.character(run_id)
  if(length(id)!=1L||!grepl("^[0-9]+$",id))
    stop("Invalid W07 run ID",call.=FALSE)
  if(!nzchar(record_id)||!nzchar(user_id)||!nzchar(trimws(reason)))
    stop("Record, reviewer and justification are required",call.=FALSE)
  if(length(codes)!=1L||length(reason)!=1L||nchar(codes)>4000L||nchar(reason)>10000L)
    stop("Invalid W07 review content",call.=FALSE)
  if(!relevance %in% c("CODED_IN_SCOPE","IN_SCOPE_ONTOLOGY_GAP",
                        "OUT_OF_SCOPE","INSUFFICIENT_INFORMATION"))
    stop("Invalid W07 relevance decision",call.=FALSE)
  for(sha in c(ontology_sha,prompt_sha))
    if(nzchar(sha)&&!grepl("^[a-f0-9]{64}$",sha))
      stop("Invalid W07 configuration version",call.=FALSE)
  if(!grepl("^[a-f0-9]{64}$",case_sha256) ||
     !grepl("^[a-f0-9]{64}$",queue_sha256))
    stop("W07 review requires verified immutable case and queue checksums",call.=FALSE)
  ids <- trimws(unlist(strsplit(codes,",",fixed=TRUE),use.names=FALSE))
  ids <- unique(ids[nzchar(ids)])
  if(is.null(allowed_codes)||any(!ids %in% allowed_codes))
    stop("W07 review codes must belong to the selected ontology",call.=FALSE)
  if(relevance=="OUT_OF_SCOPE" && length(ids))
    stop("An out-of-scope record cannot retain topic codes",call.=FALSE)
  if(relevance=="CODED_IN_SCOPE" && !length(ids))
    stop("In-scope coded status requires at least one topic code",call.=FALSE)
  if(!is.null(prior_decision) && (!identical(as.character(prior_decision$record_id),record_id) ||
       !identical(as.character(prior_decision$run_id),id) ||
       !identical(as.character(prior_decision$user_id),user_id) ||
       !identical(as.character(prior_decision$case_sha256),case_sha256) ||
       !identical(as.character(prior_decision$queue_sha256),queue_sha256)))
    stop("Prior W07 decision belongs to another record or queue",call.=FALSE)
  version <- if(is.null(prior_decision))1L else as.integer(prior_decision$version)+1L
  if(is.na(version)||version<1L)stop("Invalid W07 decision revision",call.=FALSE)
  supersedes <- if(is.null(prior_decision))"" else as.character(prior_decision$event_id)
  w07_review_sheet(meta,create=TRUE)
  stamp <- format(Sys.time(),"%Y-%m-%dT%H:%M:%OS6Z",tz="UTC")
  event_id <- digest::digest(paste(meta$review_id,id,record_id,user_id,stamp,
    stats::runif(1),sep="|"),algo="sha256")
  row <- data.frame(event_id=event_id,review_id=meta$review_id,run_id=id,
    record_id=record_id,user_id=user_id,codes=codes,relevance=relevance,
    reason=reason,timestamp=stamp,ontology_sha=ontology_sha,
    prompt_sha=prompt_sha,case_sha256=case_sha256,queue_sha256=queue_sha256,
    version=as.character(version),supersedes_event_id=supersedes,stringsAsFactors=FALSE)
  # A successful append is acknowledged without an expensive verification read.
  # On an ambiguous network failure, stop rather than automatically retry the write.
  googlesheets4::sheet_append(meta$sheet_id,sheet=w07_review_tab(),data=row)
  invisible(as.list(row[1L,,drop=FALSE]))
}
w07_review_latest <- function(events) {
  if(!length(events))return(list())
  keys <- vapply(events,function(e)paste(e$record_id,e$user_id,sep="|"),character(1))
  events[!duplicated(keys,fromLast=TRUE)]
}
