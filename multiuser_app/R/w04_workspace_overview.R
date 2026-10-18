# Read-only overview joins. No network access, modelling or persistence here.
w04_overview_chr <- function(x, default="") {
  z <- as.character(x %||% "")
  if(!length(z) || is.na(z[[1L]]) || !nzchar(z[[1L]])) default else z[[1L]]
}
w04_overview_date <- function(x) {
  z <- w04_overview_chr(x)
  if(!nzchar(z)) return("Not recorded")
  stamp <- suppressWarnings(as.POSIXct(z,format="%Y-%m-%dT%H:%M:%OS",tz="UTC"))
  if(is.na(stamp)) return(z)
  format(stamp,format="%d %b %y %H:%M",tz="Europe/London")
}
w04_overview_build <- function(subsets, prompts, runs, analyses, conflict_sets,
                               assignments, manual_decisions, conflict_decisions, users) {
  find_user <- function(uid) {
    matches <- Filter(function(u)
      identical(w04_overview_chr(u$user_id),uid),users)
    if(length(matches)) w04_overview_chr(matches[[1L]]$display_name,uid) else uid
  }
  prompt_by_id <- setNames(prompts,vapply(prompts,function(p)w04_overview_chr(p$version_id),character(1)))
  subset_by_id <- setNames(subsets,vapply(subsets,function(p)w04_overview_chr(p$subset_id),character(1)))
  run_count <- function(sid)sum(vapply(runs,function(r)identical(w04_overview_chr(r$subset_id),sid),logical(1)))
  subset_name <- function(sid) {
    z <- subset_by_id[[sid]]
    if(is.null(z))return(if(nzchar(sid))paste0("Unknown subset: ",sid) else "Not recorded")
    w04_overview_chr(z$label,paste0("Subset ",substr(sid,nchar(sid)-5L,nchar(sid))))
  }
  prompt_name <- function(pid) {
    z <- prompt_by_id[[pid]]
    if(is.null(z))return(if(nzchar(pid))paste0("Archived prompt unavailable: ",pid) else "Not recorded")
    w04_overview_chr(z$notes,w04_overview_chr(z$original_filename,paste0("Prompt ",substr(pid,nchar(pid)-5L,nchar(pid)))))
  }
  subset_rows <- lapply(subsets,function(s) {
    sid <- w04_overview_chr(s$subset_id)
    batch <- paste0("w04-human-",sid)
    assigned <- Filter(function(a)
      identical(w04_overview_chr(a$workflow),"04") &&
      identical(w04_overview_chr(a$task_type),"manual_screening") &&
      identical(w04_overview_chr(a$batch_id),batch) &&
      !identical(w04_overview_chr(a$status,"assigned"),"cancelled"),assignments)
    assigned_ids <- unique(vapply(assigned,function(a)w04_overview_chr(a$user_id),character(1)))
    assigned_ids <- assigned_ids[nzchar(assigned_ids)]
    completed <- Filter(function(d)
      startsWith(w04_overview_chr(d$review_case_id),paste0(sid,"|")) &&
      w04_overview_chr(d$decision) %in% c("retain","exclude","include"),manual_decisions)
    done <- vapply(assigned_ids,function(uid)sum(vapply(completed,function(d)
      identical(w04_overview_chr(d$reviewer),uid),logical(1))),integer(1))
    data.frame(Subset=subset_name(sid),Created=w04_overview_date(s$created_at_utc),
      N=w04_overview_chr(s$actual_size,"0"),
      Reviewers=if(length(assigned_ids))paste(vapply(assigned_ids,find_user,character(1)),collapse=", ") else "Unassigned",
      Done=if(length(done))paste(done,collapse=" / ") else "—",
      Models=run_count(sid),stringsAsFactors=FALSE)
  })
  prompt_rows <- lapply(prompts,function(p) {
    pid <- w04_overview_chr(p$version_id)
    data.frame(Prompt=prompt_name(pid),Saved=w04_overview_date(p$created_at_utc),
      Author=find_user(w04_overview_chr(p$author)),Models=sum(vapply(runs,function(r)
        identical(w04_overview_chr(r$prompt_version_id),pid),logical(1))),
      stringsAsFactors=FALSE)
  })
  run_rows <- lapply(runs,function(r) {
    data.frame(Run=w04_overview_chr(r$run_id),Published=w04_overview_date(r$published_at_utc),
      Subset=subset_name(w04_overview_chr(r$subset_id)),
      Prompt=prompt_name(w04_overview_chr(r$prompt_version_id)),
      N=w04_overview_chr(r$total,"0"),Uncertain=w04_overview_chr(r$uncertain,"0"),
      stringsAsFactors=FALSE)
  })
  comparison_rows <- lapply(analyses,function(a) {
    bid <- w04_overview_chr(a$batch_id)
    set <- Filter(function(s)identical(w04_overview_chr(s$analysis_id),
      w04_overview_chr(a$analysis_id)),conflict_sets)
    raters <- tryCatch(as.character(jsonlite::fromJSON(w04_overview_chr(a$rater_ids_json,"[]"))),
      error=function(e)character())
    kind <- if(any(raters=="model") || startsWith(bid,"w04-model-run-"))"Human–model" else "Human–human"
    rid <- if(startsWith(bid,"w04-model-run-"))sub("^w04-model-run-","",bid) else bid
    resolved <- if(length(set)) {
      sha <- w04_overview_chr(set[[1L]]$conflict_queue_sha256)
      unique(vapply(Filter(function(d)identical(w04_overview_chr(d$queue_sha256),sha),
        conflict_decisions),function(d)w04_overview_chr(d$review_case_id),character(1)))
    } else character()
    data.frame(Type=kind,Created=w04_overview_date(a$created_at_utc),
      Source=rid,Raters=if(length(raters))paste(vapply(raters,function(u)
        if(identical(u,"model"))"Model" else find_user(u),character(1)),collapse=" vs ") else "Not recorded",
      Agreement=w04_overview_chr(a$raw_agreement,"Not recorded"),
      Conflicts=w04_overview_chr(a$conflict_n,"0"),Resolved=length(resolved[nzchar(resolved)]),
      stringsAsFactors=FALSE)
  })
  bind <- function(rows,fields) {
    if(length(rows))do.call(rbind,rows) else
      as.data.frame(setNames(replicate(length(fields),character(),simplify=FALSE),fields),
        stringsAsFactors=FALSE)
  }
  newest <- function(rows,dates,fields) {
    if(length(rows)) rows <- rows[order(dates,decreasing=TRUE,na.last=TRUE)]
    bind(rows,fields)
  }
  list(subsets=newest(subset_rows,vapply(subsets,function(x)w04_overview_chr(x$created_at_utc),character(1)),
       c("Subset","Created","N","Reviewers","Done","Models")),
       prompts=newest(prompt_rows,vapply(prompts,function(x)w04_overview_chr(x$created_at_utc),character(1)),
       c("Prompt","Saved","Author","Models")),
       runs=newest(run_rows,vapply(runs,function(x)w04_overview_chr(x$published_at_utc),character(1)),
       c("Run","Published","Subset","Prompt","N","Uncertain")),
       comparisons=newest(comparison_rows,vapply(analyses,function(x)w04_overview_chr(x$created_at_utc),character(1)),
       c("Type","Created","Source","Raters","Agreement","Conflicts","Resolved")))
}
