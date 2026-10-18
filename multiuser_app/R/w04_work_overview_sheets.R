# W04 lightweight, append-only materialised overview. Original registries remain authoritative.
w04_summary_tab <- function() "w04_work_overview"
w04_summary_fields <- c("event_id","review_id","record_type","entity_id","event_at_utc","details_json")
w04_summary_read <- function(review_id,create_if_missing=FALSE) {
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- w04_summary_tab()
  if(!tab %in% sheet_names_cached(ss)) {
    if(!create_if_missing)return(list())
    googlesheets4::sheet_write(as.data.frame(setNames(replicate(length(w04_summary_fields),character(),simplify=FALSE),w04_summary_fields)),ss=ss,sheet=tab)
    invalidate_sheet_names_cache(ss)
    return(list())
  }
  rows <- as.data.frame(googlesheets4::read_sheet(ss,sheet=tab,col_types="c"))
  if(!all(w04_summary_fields %in% names(rows)))stop("W04 overview tab schema mismatch",call.=FALSE)
  rows <- rows[!is.na(rows$review_id)&as.character(rows$review_id)==review_id,,drop=FALSE]
  lapply(seq_len(nrow(rows)),function(i)as.list(rows[i,,drop=FALSE]))
}
w04_summary_append <- function(review_id,type,id,details,event_at_utc="") {
  stopifnot(nzchar(review_id),nzchar(type),nzchar(id),is.list(details))
  gs4_auth_from_env()
  ss <- sheet_id_from_env()
  tab <- w04_summary_tab()
  existing <- w04_summary_read(review_id,create_if_missing=TRUE)
  at <- if(nzchar(event_at_utc))event_at_utc else format(Sys.time(),"%Y-%m-%dT%H:%M:%OS6Z",tz="UTC")
  payload <- as.character(jsonlite::toJSON(details,auto_unbox=TRUE,null="null",na="null"))
  eid <- digest::digest(paste(review_id,type,id,at,payload,sep="\n"),algo="sha256",serialize=FALSE)
  if(any(vapply(existing,function(x)identical(as.character(x$event_id),eid),logical(1))))return(invisible(FALSE))
  row <- data.frame(event_id=eid,review_id=review_id,record_type=type,entity_id=id,
    event_at_utc=at,details_json=payload,stringsAsFactors=FALSE)
  googlesheets4::sheet_append(ss,sheet=tab,data=row)
  invisible(TRUE)
}
# One sheet read; all joins occur in memory. No network calls from here.
w04_summary_tables <- function(events) {
  types <- c(subsets="subset",prompts="prompt",runs="model_run",comparisons="comparison",
    conflicts="conflict_set",screening="human_screening",adjudications="adjudication")
  result <- lapply(types,function(type) {
    xs <- Filter(function(x)identical(as.character(x$record_type),type),events)
    if(!length(xs))return(data.frame(Date=character(),Item=character(),Details=character()))
    # Latest event per entity is the current summary, historical events stay in Sheets.
    dates <- vapply(xs,function(x)as.character(x$event_at_utc),character(1))
    xs <- xs[order(dates,decreasing=TRUE)]
    ids <- vapply(xs,function(x)as.character(x$entity_id),character(1))
    xs <- xs[!duplicated(ids)]
    data.frame(Date=vapply(xs,function(x)w04_overview_date(x$event_at_utc),character(1)),
      Item=vapply(xs,function(x) {
        v <- tryCatch(jsonlite::fromJSON(as.character(x$details_json)),error=function(e)list())
        as.character(v$label %||% x$entity_id)
      },character(1)),
      Details=vapply(xs,function(x) {
        v <- tryCatch(jsonlite::fromJSON(as.character(x$details_json)),error=function(e)list())
        as.character(v$summary %||% "")
      },character(1)),stringsAsFactors=FALSE)
  })
  result
}
