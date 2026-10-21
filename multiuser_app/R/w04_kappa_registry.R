w04_kappa_registry_columns <- function() {
  c(
    "registry_row_id","date","source","batch_id","queue_sha256","workflow04_run_id",
    "records_reviewed","records_compared",
    "model_retain_human_retain","model_retain_human_exclude",
    "model_exclude_human_retain","model_exclude_human_exclude",
    "human_uncertain","model_uncertain","raw_agreement","human_model_kappa",
    "humans_model_fleiss_kappa","created_at_utc","source_github_run_id"
  )
}

w04_empty_kappa_registry <- function() {
  cols <- w04_kappa_registry_columns()
  as.data.frame(
    setNames(replicate(length(cols),character(),simplify=FALSE),cols),
    stringsAsFactors=FALSE
  )
}

w04_normalise_kappa_registry <- function(x) {
  if (is.null(x) || !nrow(x)) return(w04_empty_kappa_registry())
  cols <- w04_kappa_registry_columns()
  missing <- setdiff(cols,names(x))
  if (length(missing)) stop(
    "W04 kappa registry missing field(s): ",paste(missing,collapse=", "),
    call.=FALSE
  )
  x <- x[,cols,drop=FALSE]
  for (nm in cols) {
    x[[nm]] <- as.character(x[[nm]])
    x[[nm]][is.na(x[[nm]])] <- ""
  }
  if (any(!nzchar(x$registry_row_id)) || anyDuplicated(x$registry_row_id)) {
    stop("W04 kappa registry row identity invariant failed",call.=FALSE)
  }
  if (any(!nzchar(x$batch_id)) || anyDuplicated(x$batch_id)) {
    stop("W04 kappa registry batch identity invariant failed",call.=FALSE)
  }
  rownames(x) <- NULL
  x
}

w04_kappa_registry_row_signatures <- function(x) {
  x <- w04_normalise_kappa_registry(x)
  if (!nrow(x)) return(setNames(character(),character()))
  out <- vapply(seq_len(nrow(x)),function(i) {
    digest::digest(
      paste(unlist(x[i,,drop=FALSE],use.names=FALSE),collapse="\t"),
      algo="sha256",serialize=FALSE
    )
  },character(1))
  setNames(out,x$registry_row_id)
}

w04_kappa_registry_relation <- function(github, google) {
  github <- w04_normalise_kappa_registry(github)
  google <- w04_normalise_kappa_registry(google)
  gh <- w04_kappa_registry_row_signatures(github)
  gs <- w04_kappa_registry_row_signatures(google)

  extra_google <- setdiff(names(gs),names(gh))
  if (length(extra_google)) {
    stop(
      "Google W04 kappa registry contains row(s) absent from GitHub: ",
      paste(extra_google,collapse=", "),
      call.=FALSE
    )
  }
  shared <- intersect(names(gs),names(gh))
  mismatched <- shared[gs[shared] != gh[shared]]
  if (length(mismatched)) {
    stop(
      "Google/GitHub W04 kappa registry disagreement for row(s): ",
      paste(mismatched,collapse=", "),
      call.=FALSE
    )
  }
  if (!length(gs) && length(gh)) return("google_empty")
  if (length(gs) < length(gh)) return("google_subset")
  "identical"
}

w04_kappa_from_counts <- function(rr,re,er,ee) {
  vals <- suppressWarnings(as.numeric(c(rr,re,er,ee)))
  if (any(is.na(vals)) || any(vals < 0)) {
    return(list(n=0L,agreement=NA_real_,kappa=NA_real_))
  }
  rr<-vals[[1L]];re<-vals[[2L]];er<-vals[[3L]];ee<-vals[[4L]]
  n <- rr+re+er+ee
  if (n <= 0) return(list(n=0L,agreement=NA_real_,kappa=NA_real_))
  po <- (rr+ee)/n
  model_retain <- (rr+re)/n
  human_retain <- (rr+er)/n
  pe <- model_retain*human_retain + (1-model_retain)*(1-human_retain)
  k <- if (isTRUE(all.equal(pe,1))) NA_real_ else (po-pe)/(1-pe)
  list(n=as.integer(n),agreement=as.numeric(po),kappa=as.numeric(k))
}

w04_kappa_registry_summary <- function(registry) {
  x <- w04_normalise_kappa_registry(registry)
  if (!nrow(x)) {
    return(list(
      manually_screened=0L,kappa=NA_real_,kappa_n=0L,
      raw_agreement=NA_real_,batches=0L
    ))
  }
  int <- function(nm) {
    z<-suppressWarnings(as.integer(x[[nm]]))
    z[is.na(z)]<-0L
    z
  }
  rr<-sum(int("model_retain_human_retain"))
  re<-sum(int("model_retain_human_exclude"))
  er<-sum(int("model_exclude_human_retain"))
  ee<-sum(int("model_exclude_human_exclude"))
  s<-w04_kappa_from_counts(rr,re,er,ee)
  reviewed<-sum(int("records_reviewed"))
  list(
    manually_screened=as.integer(reviewed),
    kappa=s$kappa,
    kappa_n=s$n,
    raw_agreement=s$agreement,
    batches=as.integer(nrow(x)),
    rr=as.integer(rr),re=as.integer(re),er=as.integer(er),ee=as.integer(ee)
  )
}


# Latest saved human-model comparison per canonical W04 subset. Historical
# comparisons and repeated model runs of the same subset are not cumulative.
w04_latest_subset_model_kappa <- function(analyses,batches) {
  if(!length(analyses) || !length(batches))
    return(list(kappa=NA_real_,kappa_n=0L,subsets=0L))
  run_subsets <- setNames(vapply(batches,function(b)
    as.character(b$subset_id),character(1)),
    vapply(batches,function(b)as.character(b$run_id),character(1)))
  candidates <- Filter(function(x) {
    run <- sub("^w04-model-run-","",as.character(x$batch_id %||% ""))
    raters <- tryCatch(as.character(jsonlite::fromJSON(
      as.character(x$rater_ids_json %||% "[]"))),error=function(e)character())
    identical(as.character(x$review_mode %||% ""),"subset_model_validation") &&
      grepl("^w04-model-run-[0-9]+$",as.character(x$batch_id %||% "")) &&
      run %in% names(run_subsets) && length(raters)==2L &&
      sum(raters=="model")==1L
  },analyses)
  if(!length(candidates))
    return(list(kappa=NA_real_,kappa_n=0L,subsets=0L))
  subset <- vapply(candidates,function(x)
    unname(run_subsets[[sub("^w04-model-run-","",
      as.character(x$batch_id))]]),character(1))
  selected <- lapply(unique(subset),function(id) {
    group <- candidates[subset==id]
    date <- vapply(group,function(x)
      as.character(x$created_at_utc %||% ""),character(1))
    aid <- vapply(group,function(x)
      as.character(x$analysis_id %||% ""),character(1))
    group[[order(date,aid,decreasing=TRUE)[[1L]]]]
  })
  patterns <- unlist(lapply(selected,function(x) {
    value <- tryCatch(jsonlite::fromJSON(
      as.character(x$patterns_json %||% "[]"),simplifyVector=FALSE),
      error=function(e)stop("Saved W04 comparison pattern JSON is invalid",call.=FALSE))
    if(!is.list(value))stop("Saved W04 comparison patterns are invalid",call.=FALSE)
    value
  }),recursive=FALSE)
  if(!length(patterns))
    return(list(kappa=NA_real_,kappa_n=0L,subsets=length(selected)))
  stats <- w04_human_pattern_stats(patterns,2L)
  expected <- sum(vapply(selected,function(x) {
    n <- suppressWarnings(as.integer(x$complete_n))
    if(length(n)!=1L||is.na(n)||n<0L)
      stop("Latest W04 comparison has invalid record count",call.=FALSE)
    n
  },integer(1)))
  if(!identical(as.integer(stats$n),as.integer(expected)))
    stop("Latest W04 comparison patterns do not match saved comparable counts",call.=FALSE)
  list(kappa=stats$kappa,kappa_n=stats$n,subsets=length(selected))
}
