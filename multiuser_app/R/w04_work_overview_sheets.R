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
# One sheet read; all overview relationships are reconstructed in memory.
w04_summary_latest <- function(events,type) {
  x <- Filter(function(z)identical(w04_overview_chr(z$record_type),type),events)
  if(!length(x))return(list())
  times <- vapply(x,function(z)w04_overview_chr(z$event_at_utc),character(1))
  x <- x[order(times,decreasing=TRUE)]
  ids <- vapply(x,function(z)w04_overview_chr(z$entity_id),character(1))
  x[!duplicated(ids)]
}
w04_summary_details <- function(event) {
  tryCatch(jsonlite::fromJSON(w04_overview_chr(event$details_json,"{}"),
     simplifyVector=FALSE),error=function(e)list())
}
w04_summary_tables <- function(events) {
  subsets <- w04_summary_latest(events,"subset")
  prompts <- w04_summary_latest(events,"prompt")
  runs <- w04_summary_latest(events,"model_run")
  comparisons <- w04_summary_latest(events,"comparison")
  conflicts <- w04_summary_latest(events,"conflict_set")
  screening <- w04_summary_latest(events,"human_screening")
  adjudication <- w04_summary_latest(events,"adjudication")
  get <- function(z,key,default="")w04_overview_chr(w04_summary_details(z)[[key]],default)
  id <- function(z)w04_overview_chr(z$entity_id)
  display <- function(z,default="")get(z,"label",default)
  date <- function(z)w04_overview_date(z$event_at_utc)
  short <- function(x,prefix) {
    if(!nzchar(x))return("—")
    paste0(prefix," ",substr(x,max(1L,nchar(x)-5L),nchar(x)))
  }
  subset_label <- function(sid) {
    hits <- Filter(function(z)identical(id(z),sid),subsets)
    if(length(hits))display(hits[[1L]],short(sid,"Subset")) else short(sid,"Subset")
  }
  prompt_label <- function(pid) {
    hits <- Filter(function(z)identical(id(z),pid),prompts)
    if(length(hits))display(hits[[1L]],short(pid,"Prompt")) else short(pid,"Prompt")
  }
  bind <- function(rows,columns) {
    if(length(rows))return(do.call(rbind,rows))
    as.data.frame(setNames(replicate(length(columns),character(),simplify=FALSE),
      columns),stringsAsFactors=FALSE)
  }
  subset_rows <- lapply(subsets,function(z) {
    sid <- id(z)
    rs <- Filter(function(r)identical(get(r,"subset_id"),sid),runs)
    screens <- Filter(function(r)identical(get(r,"subset_id"),sid),screening)
    names <- vapply(screens,function(r)get(r,"reviewer"),character(1))
    counts <- vapply(screens,function(r)get(r,"completed","0"),character(1))
    data.frame(Subset=display(z,short(sid,"Subset")),Created=date(z),
      N=get(z,"records","—"),
      Reviewers=if(length(names))paste(names,collapse=", ") else "—",
      Done=if(length(counts))paste(counts,collapse=" / ") else "—",
      Models=length(rs),stringsAsFactors=FALSE)
  })
  prompt_rows <- lapply(prompts,function(z) {
    pid <- id(z)
    n <- sum(vapply(runs,function(r)identical(get(r,"prompt_version_id"),pid),logical(1)))
    data.frame(Prompt=display(z,short(pid,"Prompt")),Saved=date(z),
      Author=get(z,"author","—"),Models=n,stringsAsFactors=FALSE)
  })
  run_rows <- lapply(runs,function(z) {
    data.frame(Run=id(z),Published=date(z),
      Subset=subset_label(get(z,"subset_id")),
      Prompt=prompt_label(get(z,"prompt_version_id")),
      N=get(z,"total","—"),Uncertain=get(z,"uncertain","—"),stringsAsFactors=FALSE)
  })
  comparison_rows <- lapply(comparisons,function(z) {
    cid <- id(z)
    sets <- Filter(function(x)identical(get(x,"analysis_id"),cid),conflicts)
    resolved <- sum(vapply(sets,function(s) {
      hits <- Filter(function(x)identical(get(x,"conflict_set_id"),id(s)),adjudication)
      if(!length(hits))return(0)
      suppressWarnings(as.numeric(get(hits[[1L]],"completed","0"))) %||% 0
    },numeric(1)),na.rm=TRUE)
    data.frame(Type=get(z,"comparison_type",display(z,"Comparison")),
      Created=date(z),Source=get(z,"source_run","—"),
      Raters=get(z,"raters","—"),
      Agreement=get(z,"agreement","—"),Conflicts=get(z,"conflicts","0"),
      Resolved=resolved,stringsAsFactors=FALSE)
  })
  conflict_rows <- lapply(conflicts,function(z) {
    data.frame(Set=short(id(z),"Set"),Created=date(z),
      Comparison=get(z,"analysis_id","—"),
      Conflicts=get(z,"conflicts","—"),stringsAsFactors=FALSE)
  })
  list(subsets=bind(subset_rows,c("Subset","Created","N","Reviewers","Done","Models")),
    prompts=bind(prompt_rows,c("Prompt","Saved","Author","Models")),
    runs=bind(run_rows,c("Run","Published","Subset","Prompt","N","Uncertain")),
    comparisons=bind(comparison_rows,c("Type","Created","Source","Raters","Agreement","Conflicts","Resolved")),
    conflicts=bind(conflict_rows,c("Set","Created","Comparison","Conflicts")))
}

# Backfill is deliberately an explicit administrator operation, never part of login.
# Existing event IDs are preserved and repeated imports do not duplicate rows.
w04_summary_backfill <- function(review_id,subsets,prompts,runs,analyses,conflict_sets,
                                 assignments=list(),manual_decisions=list(),
                                 conflict_decisions=list(),users=list()) {
  to_event <- function(type,id,at,details) {
    json <- as.character(jsonlite::toJSON(details,auto_unbox=TRUE,null="null",na="null"))
    event_id <- digest::digest(paste(review_id,type,id,at,json,sep="\n"),
      algo="sha256",serialize=FALSE)
    data.frame(event_id=event_id,review_id=review_id,record_type=type,
      entity_id=id,event_at_utc=at,details_json=json,stringsAsFactors=FALSE)
  }
  rows <- list()
  add <- function(type,id,at,details) {
    if(!nzchar(id))return(invisible(NULL))
    rows[[length(rows)+1L]] <<- to_event(type,id,at,details)
    invisible(NULL)
  }
  for(x in subsets) {
    id <- w04_overview_chr(x$subset_id)
    label <- w04_overview_chr(x$label,paste0("Subset ",substr(id,max(1L,nchar(id)-5L),nchar(id))))
    add("subset",id,w04_overview_chr(x$created_at_utc),
      list(label=label,records=w04_overview_chr(x$actual_size,"0"),
        parent_subset_id=w04_overview_chr(x$parent_subset_id)))
  }
  for(x in prompts) {
    id <- w04_overview_chr(x$version_id)
    add("prompt",id,w04_overview_chr(x$created_at_utc),
      list(label=w04_overview_chr(x$notes,w04_overview_chr(x$original_filename,id)),
        author=w04_overview_chr(x$author),status=w04_overview_chr(x$status)))
  }
  for(x in runs) {
    id <- w04_overview_chr(x$run_id)
    add("model_run",id,w04_overview_chr(x$published_at_utc),
      list(label=paste("Model run",id),subset_id=w04_overview_chr(x$subset_id),
        prompt_version_id=w04_overview_chr(x$prompt_version_id),
        total=w04_overview_chr(x$total,"0"),uncertain=w04_overview_chr(x$uncertain,"0"),
        retain=w04_overview_chr(x$retain,"0"),exclude=w04_overview_chr(x$exclude,"0")))
  }
  for(x in analyses) {
    id <- w04_overview_chr(x$analysis_id)
    bid <- w04_overview_chr(x$batch_id)
    model <- startsWith(bid,"w04-model-run-")
    raters <- tryCatch(as.character(jsonlite::fromJSON(w04_overview_chr(x$rater_ids_json,"[]"))),
      error=function(e)character())
    label <- if(model)"Human–model" else "Human–human"
    add("comparison",id,w04_overview_chr(x$created_at_utc),
      list(label=label,comparison_type=label,
        source_run=if(model)sub("^w04-model-run-","",bid) else bid,
        raters=paste(raters,collapse=" vs "),
        conflicts=w04_overview_chr(x$conflict_n,"0"),
        agreement=w04_overview_chr(x$raw_agreement,"—"),
        kappa=w04_overview_chr(x$kappa,"—")))
  }
  for(x in conflict_sets) {
    id <- w04_overview_chr(x$conflict_set_id)
    ids <- tryCatch(jsonlite::fromJSON(w04_overview_chr(x$conflict_case_ids_json,"[]")),
      error=function(e)character())
    add("conflict_set",id,w04_overview_chr(x$created_at_utc),
      list(label=paste("Conflict set",substr(id,max(1L,nchar(id)-5L),nchar(id))),
        analysis_id=w04_overview_chr(x$analysis_id),
        comparison_type=w04_overview_chr(x$comparison_type),
        conflicts=as.character(length(ids))))
  }
  if(length(assignments) && length(subsets)) {
    for(s in subsets) {
      sid <- w04_overview_chr(s$subset_id)
      batch_id <- paste0("w04-human-",sid)
      assigned <- Filter(function(a)
        identical(w04_overview_chr(a$workflow),"04") &&
        identical(w04_overview_chr(a$task_type),"manual_screening") &&
        identical(w04_overview_chr(a$batch_id),batch_id) &&
        !identical(w04_overview_chr(a$status,"assigned"),"cancelled"),assignments)
      uids <- unique(vapply(assigned,function(a)w04_overview_chr(a$user_id),character(1)))
      uids <- uids[nzchar(uids)]
      for(uid in uids) {
        user <- Filter(function(u)identical(w04_overview_chr(u$user_id),uid),users)
        name <- if(length(user))w04_overview_chr(user[[1L]]$display_name,uid) else uid
        completed <- unique(vapply(Filter(function(d)
          identical(w04_overview_chr(d$reviewer),uid) &&
          startsWith(w04_overview_chr(d$review_case_id),paste0(sid,"|")) &&
          w04_overview_chr(d$decision) %in% c("retain","exclude","include"),
          manual_decisions),function(d)w04_overview_chr(d$review_case_id),character(1)))
        assigned_n <- sum(vapply(assigned,function(a)
          identical(w04_overview_chr(a$user_id),uid),logical(1)))
        add("human_screening",paste(sid,uid,sep="|"),
          format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"),
          list(label=name,subset_id=sid,reviewer=name,
            completed=as.character(length(completed)),assigned=as.character(assigned_n)))
      }
    }
  }
  if(length(conflict_sets) && length(conflict_decisions)) {
    for(s in conflict_sets) {
      sha <- w04_overview_chr(s$conflict_queue_sha256)
      cases <- unique(vapply(Filter(function(d)
        identical(w04_overview_chr(d$queue_sha256),sha),conflict_decisions),
        function(d)w04_overview_chr(d$review_case_id),character(1)))
      add("adjudication",w04_overview_chr(s$conflict_set_id),
        format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"),
        list(label="Adjudication progress",conflict_set_id=w04_overview_chr(s$conflict_set_id),
          completed=as.character(sum(nzchar(cases)))))
    }
  }
  if(!length(rows))return(0L)
  incoming <- do.call(rbind,rows)
  incoming <- incoming[!duplicated(incoming$event_id),,drop=FALSE]
  existing <- w04_summary_read(review_id,create_if_missing=TRUE)
  already <- vapply(existing,function(x)w04_overview_chr(x$event_id),character(1))
  incoming <- incoming[!incoming$event_id %in% already,,drop=FALSE]
  if(!nrow(incoming))return(0L)
  googlesheets4::sheet_append(sheet_id_from_env(),sheet=w04_summary_tab(),data=incoming)
  nrow(incoming)
}

# Successful authoritative W04 saves can call this safely. A failed summary
# append is logged, never concealed as an authoritative save failure.
w04_summary_record_safe <- function(type,id,at,label,summary,review_id=NULL) {
  tryCatch({
    if(is.null(review_id) || !nzchar(as.character(review_id)))
      review_id <- w04_prompt_review_metadata()$review_id
    w04_summary_append(as.character(review_id),type,as.character(id),
      list(label=as.character(label),summary=as.character(summary)),
      as.character(at %||% ""))
    TRUE
  },error=function(e) {
    warning(sprintf("W04 summary update not confirmed for %s %s: %s",
      type,id,conditionMessage(e)),call.=FALSE)
    FALSE
  })
}
