# Construct a complete W04 human screening queue from an immutable subset.
# No model decisions enter this pathway; no production queue is overwritten.
w04_subset_screening_text <- function(x) {
  if(is.null(x)) return("")
  if(is.atomic(x)) return(paste(as.character(x[!is.na(x)]),collapse="; "))
  if(is.list(x)) return(paste(Filter(nzchar,vapply(x,w04_subset_screening_text,character(1))),collapse="; "))
  ""
}
w04_subset_screening_first <- function(...) {
  for(x in list(...)) {
    value <- trimws(w04_subset_screening_text(x))
    if(nzchar(value)) return(value)
  }
  ""
}
w04_subset_screening_build <- function(subset,canonical_lines,checkpoint_sha256) {
  w04_subset_verify(subset)
  if(!identical(tolower(as.character(checkpoint_sha256)),
                tolower(as.character(subset$source_checkpoint_sha256))))
    stop("Canonical checkpoint checksum differs from registered subset",call.=FALSE)
  canonical <- lapply(canonical_lines[nzchar(trimws(canonical_lines))],
                      jsonlite::fromJSON,simplifyVector=FALSE)
  ids <- vapply(canonical,function(r)
    as.character((r$identity %||% list())$record_id %||% ""),character(1))
  if(any(!nzchar(ids))||anyDuplicated(ids))
    stop("Canonical corpus has invalid record IDs",call.=FALSE)
  positions <- match(as.character(subset$record_ids),ids)
  if(anyNA(positions)) stop("Subset members are absent from the canonical corpus",call.=FALSE)
  cases <- lapply(seq_along(positions),function(i) {
    r <- canonical[[positions[[i]]]]
    c <- r$canonical %||% list()
    rid <- subset$record_ids[[i]]
    list(
      review_case_id=paste0(subset$subset_id,"|",rid),
      record_id=rid,subset_id=subset$subset_id,
      random_order=as.integer(i),
      bibliographic=list(
        title=w04_subset_screening_first(c$title,r$title),
        abstract=w04_subset_screening_first(c$abstract,r$abstract),
        keywords=w04_subset_screening_first(c$keywords,r$keywords),
        journal=w04_subset_screening_first(c$source_title,c$journal,r$source_title,r$journal),
        authors=w04_subset_screening_first(c$authors,r$authors),
        year=w04_subset_screening_first(c$year,r$year),
        volume=w04_subset_screening_first(c$volume,r$volume),
        pages=w04_subset_screening_first(c$pages,r$pages),
        doi=w04_subset_screening_first(c$doi,r$doi)
      )
    )
  })
  json <- vapply(cases,function(x)as.character(jsonlite::toJSON(x,auto_unbox=TRUE,
                   null="null",na="null",digits=NA)),character(1))
  digest_input <- paste0(paste(json,collapse="\n"),"\n")
  sha <- digest::digest(digest_input,algo="sha256",serialize=FALSE)
  batch_id <- paste0("w04-human-",subset$subset_id)
  list(batch_id=batch_id,queue_sha256=sha,subset_id=subset$subset_id,
       source_checkpoint_sha256=subset$source_checkpoint_sha256,
       cases=cases,case_json=json)
}
w04_subset_screening_rows <- function(queue) {
  cases <- queue$cases
  if(!length(cases)||length(queue$case_json)!=length(cases))
    stop("Empty or invalid subset queue",call.=FALSE)
  sha <- digest::digest(paste0(paste(queue$case_json,collapse="\n"),"\n"),
                        algo="sha256",serialize=FALSE)
  if(!identical(sha,queue$queue_sha256))
    stop("Queue serialisation checksum mismatch",call.=FALSE)
  data.frame(batch_id=rep(queue$batch_id,length(cases)),
    queue_sha256=rep(queue$queue_sha256,length(cases)),
    case_index=as.character(seq_along(cases)),
    review_case_id=vapply(cases,function(c)c$review_case_id,character(1)),
    case_json=queue$case_json,
    review_mode=rep("registered_human_subset",length(cases)),
    subset_id=rep(queue$subset_id,length(cases)),stringsAsFactors=FALSE)
}
