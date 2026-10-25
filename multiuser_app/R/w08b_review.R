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

# W01 canonical builder uses deterministic modal field selection: most frequent
# normalised value, then longest original value, then stable source key.
w08b_modal_field <- function(records,field) {
  vals<-vapply(records,function(z)trimws(as.character(z[[field]] %||% ""))[1L],character(1))
  valid<-which(nzchar(vals) & !is.na(vals))
  if(!length(valid))return("")
  normal<-tolower(gsub("[[:space:]]+"," ",vals[valid]))
  counts<-table(normal)
  winners<-valid[normal %in% names(counts)[counts==max(counts)]]
  keys<-vapply(records[winners],function(z)as.character(z$w01_record_id %||% z$source_id %||% ""),character(1))
  vals[winners[order(-nchar(vals[winners],type="chars"),keys)[1L]]]
}
# The historical W01 identity is authoritative. Resolve multiple historical
# candidates by strong identifier agreement, then exact abstract/title,
# with stable record ID tie-breaking, without asking the reviewer to select a master.
w08b_historical_target <- function(case) {
  hs<-case$historical %||% list()
  cs<-case$candidates %||% list()
  if(!length(hs)||!length(cs))stop("Missing matched historical/citation records",call.=FALSE)
  scalar<-function(x)tolower(trimws(as.character(x %||% "")[1L]))
  normal<-function(x)gsub("[^[:alnum:]]+","",scalar(x))
  nd<-function(x)sub("^https?://(dx[.])?doi[.]org/","",scalar(x))
  score<-vapply(hs,function(h) {
    score<-0L
    for(c in cs) {
      if(nzchar(nd(h$doi))&&nzchar(nd(c$doi))&&identical(nd(h$doi),nd(c$doi)))score<-score+100L
      if(nzchar(normal(h$abstract))&&nchar(normal(h$abstract))>=120L &&
         identical(normal(h$abstract),normal(c$abstract)))score<-score+20L
      if(nzchar(normal(h$title))&&identical(normal(h$title),normal(c$title)))score<-score+10L
    }
    score
  },integer(1))
  ids<-vapply(hs,function(h)as.character(h$w01_record_id),character(1))
  ids[order(-score,ids)[1L]]
}

append_w08b_review_decision <- function(queue,case,decision,target,rationale,reviewer) {
  if(!decision%in%c("same_publication","different_publications","unable_to_determine"))
    stop("Unsupported W08b review decision",call.=FALSE)
  valid<-vapply(case$historical%||%list(),function(z)as.character(z$w01_record_id),character(1))
  if(decision=="same_publication")target<-w08b_historical_target(case)
  else target<-""
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
