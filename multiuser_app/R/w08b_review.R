# Separate W08b citation-chasing ambiguity queue; never reads or writes W01 decisions.
read_w08b_review_queue <- function() {
  gs4_auth_from_env()
  ss<-sheet_id_from_env()
  tab<-"queue_w08b_dedup_active"
  if(!tab %in% sheet_names_cached(ss,refresh=TRUE))return(NULL)
  x<-googlesheets4::read_sheet(ss,sheet=tab,col_types="c")
  need<-c("batch_id","queue_sha256","case_index","review_case_id","case_json")
  if(!nrow(x)||!all(need%in%names(x)))return(NULL)
  x<-x[order(as.integer(x$case_index)),need,drop=FALSE]
  if(length(unique(x$batch_id))!=1L||length(unique(x$queue_sha256))!=1L||
    anyDuplicated(x$review_case_id))stop("Invalid W08b queue identity",call.=FALSE)
  sha<-digest::digest(paste0(paste(x$case_json,collapse="\n"),"\n"),algo="sha256",serialize=FALSE)
  if(!identical(sha,x$queue_sha256[[1]]))stop("W08b review queue checksum mismatch",call.=FALSE)
  cs<-lapply(x$case_json,jsonlite::fromJSON,simplifyVector=FALSE)
  if(!identical(vapply(cs,function(z)as.character(z$review_case_id),character(1)),x$review_case_id))
    stop("W08b review-case identities mismatch",call.=FALSE)
  list(batch_id=x$batch_id[[1]],sha=sha,cases=cs)
}
read_w08b_review_decisions <- function(sha) {
  gs4_auth_from_env()
  ss<-sheet_id_from_env();tab<-"decisions_w08b_dedup"
  if(!tab%in%sheet_names_cached(ss,refresh=TRUE))return(list())
  x<-googlesheets4::read_sheet(ss,sheet=tab,col_types="c")
  if(!nrow(x))return(list())
  x<-x[x$queue_sha256==sha,,drop=FALSE]
  lapply(seq_len(nrow(x)),function(i)as.list(x[i,,drop=FALSE]))
}
append_w08b_review_decision <- function(queue,case,decision,target,rationale,reviewer) {
  if(!decision%in%c("same_publication","different_publications","unable_to_determine"))
    stop("Unsupported W08b review decision",call.=FALSE)
  valid<-vapply(case$historical%||%list(),function(z)as.character(z$w01_record_id),character(1))
  if(decision=="same_publication" && (!nzchar(target)||!target%in%valid))
    stop("Select the specific historical W01 publication",call.=FALSE)
  if(decision!="same_publication")target<-""
  prior<-read_w08b_review_decisions(queue$sha)
  if(any(vapply(prior,function(z)identical(as.character(z$review_case_id),as.character(case$review_case_id)),logical(1))))
    stop("This W08b case already has a saved decision; refresh the queue",call.=FALSE)
  now<-format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC")
  row<-data.frame(
    decision_id=digest::digest(paste(queue$sha,case$review_case_id,reviewer,now,sep="|"),algo="sha256",serialize=FALSE),
    review_case_id=as.character(case$review_case_id),decision=decision,target_w01_id=target,
    rationale=as.character(rationale),reviewer=as.character(reviewer),decided_at_utc=now,queue_sha256=queue$sha,
    stringsAsFactors=FALSE)
  googlesheets4::sheet_append(sheet_id_from_env(),data=row,sheet="decisions_w08b_dedup")
  invisible(TRUE)
}
