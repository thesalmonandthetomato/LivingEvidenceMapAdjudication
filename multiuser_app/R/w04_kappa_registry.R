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
