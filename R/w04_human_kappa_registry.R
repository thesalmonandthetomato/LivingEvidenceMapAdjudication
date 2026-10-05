w04_human_kappa_registry_columns <- function() {
  c(
    "consistency_id","date","batch_id","queue_sha256","assignment_scope_id",
    "scope_type","reviewer_key","rater_ids_json","rater_labels_json","n_raters",
    "records_n","record_ids_json","record_ids_sha256","metric",
    "agreement_n","conflict_n","raw_agreement","kappa",
    "patterns_json","pairwise_json","analysis_id","created_by","created_at_utc"
  )
}

w04_empty_human_kappa_registry <- function() {
  cols <- w04_human_kappa_registry_columns()
  as.data.frame(
    setNames(replicate(length(cols),character(),simplify=FALSE),cols),
    stringsAsFactors=FALSE
  )
}

w04_normalise_human_kappa_registry <- function(x) {
  if (is.null(x) || !nrow(x)) return(w04_empty_human_kappa_registry())
  cols <- w04_human_kappa_registry_columns()
  missing <- setdiff(cols,names(x))
  if (length(missing)) stop(
    "W04 human consistency registry missing field(s): ",
    paste(missing,collapse=", "),
    call.=FALSE
  )
  x <- x[,cols,drop=FALSE]
  for (nm in cols) {
    x[[nm]] <- as.character(x[[nm]])
    x[[nm]][is.na(x[[nm]])] <- ""
  }
  if (any(!nzchar(x$consistency_id)) || anyDuplicated(x$consistency_id)) {
    stop("W04 human consistency registry identity invariant failed",call.=FALSE)
  }
  if (any(!x$scope_type %in% c("partial","full"))) {
    stop("W04 human consistency registry contains an invalid scope_type",call.=FALSE)
  }
  rownames(x) <- NULL
  x
}

w04_human_consistency_raters <- function(rater_ids) {
  ids <- sort(unique(as.character(rater_ids)))
  ids <- ids[nzchar(ids) & ids != "model"]
  ids
}

w04_human_reviewer_key <- function(rater_ids) {
  ids <- w04_human_consistency_raters(rater_ids)
  if (length(ids) < 2L) return("")
  paste(ids,collapse="|")
}

w04_human_registry_json_chars <- function(x) {
  if (is.null(x) || !length(x)) return(character())
  tryCatch(as.character(jsonlite::fromJSON(as.character(x))),error=function(e)character())
}

w04_human_registry_record_ids <- function(row) {
  sort(unique(w04_human_registry_json_chars(row$record_ids_json %||% "[]")))
}

w04_human_registry_rater_ids <- function(row) {
  sort(unique(w04_human_registry_json_chars(row$rater_ids_json %||% "[]")))
}

w04_human_consistency_assignment_scope <- function(assignments,batch_id,rater_ids) {
  ids <- w04_human_consistency_raters(rater_ids)
  if (length(ids) < 2L) {
    return(list(
      rater_ids=ids,reviewer_key="",case_ids=character(),
      assignment_scope_id="",assignment_rows=list()
    ))
  }
  xs <- active_assignments_for_batch(
    assignments %||% list(),"04",as.character(batch_id),"manual_screening"
  )
  xs <- Filter(function(x) {
    a <- normalise_assignment_row(x)
    startsWith(a$blind_group,"w04-reviewer-consistency")
  },xs)
  case_ids <- sort(unique(vapply(xs,function(x)normalise_assignment_row(x)$case_id,character(1))))
  case_ids <- case_ids[nzchar(case_ids)]
  joint <- case_ids[vapply(case_ids,function(cid) {
    users <- unique(vapply(
      Filter(function(x)identical(normalise_assignment_row(x)$case_id,cid),xs),
      function(x)normalise_assignment_row(x)$user_id,
      character(1)
    ))
    all(ids %in% users)
  },logical(1))]
  key <- w04_human_reviewer_key(ids)
  scope_id <- if(length(joint)) paste0(
    "w04-human-scope-",
    substr(digest::digest(
      paste(as.character(batch_id),key,paste(joint,collapse="|"),sep="||"),
      algo="sha256",serialize=FALSE
    ),1L,20L)
  ) else ""
  list(
    rater_ids=ids,
    reviewer_key=key,
    case_ids=joint,
    assignment_scope_id=scope_id,
    assignment_rows=xs
  )
}

w04_human_complete_case_ids <- function(outcomes,rater_ids,case_ids=NULL) {
  ids <- w04_human_consistency_raters(rater_ids)
  xs <- outcomes %||% list()
  if (!is.null(case_ids)) {
    allowed <- unique(as.character(case_ids))
    xs <- Filter(function(x)as.character(x$case_id %||% "") %in% allowed,xs)
  }
  out <- vapply(xs,function(x) {
    all(vapply(ids,function(rid)nzchar(w04_consistency_rater_decision(x,rid)),logical(1)))
  },logical(1))
  sort(unique(vapply(xs[out],function(x)as.character(x$case_id %||% ""),character(1))))
}

w04_human_subset_outcomes <- function(outcomes,case_ids) {
  allowed <- unique(as.character(case_ids))
  Filter(function(x)as.character(x$case_id %||% "") %in% allowed,outcomes %||% list())
}

w04_human_consistency_scope <- function(
  outcomes,assignments,batch_id,rater_ids,registry,scope_type=c("partial","full")
) {
  scope_type <- match.arg(scope_type)
  scope <- w04_human_consistency_assignment_scope(assignments,batch_id,rater_ids)
  ids <- scope$rater_ids
  if (length(ids) < 2L) {
    return(list(
      valid=FALSE,reason="Select at least two human reviewers.",
      scope_type=scope_type,rater_ids=ids,analysis=w04_consistency_analysis(list(),ids)
    ))
  }
  if (!length(scope$case_ids)) {
    return(list(
      valid=FALSE,reason="The selected reviewers do not share an active reviewer-consistency assignment.",
      scope_type=scope_type,rater_ids=ids,reviewer_key=scope$reviewer_key,
      assignment_scope_id=scope$assignment_scope_id,assigned_case_ids=character(),
      target_case_ids=character(),analysis=w04_consistency_analysis(list(),ids)
    ))
  }

  complete_ids <- w04_human_complete_case_ids(outcomes,ids,scope$case_ids)
  reg <- w04_normalise_human_kappa_registry(registry)
  prior <- reg[
    reg$batch_id==as.character(batch_id) &
      reg$reviewer_key==scope$reviewer_key,
    ,drop=FALSE
  ]
  prior_ids <- if(nrow(prior)) sort(unique(unlist(lapply(seq_len(nrow(prior)),function(i) {
    w04_human_registry_record_ids(as.list(prior[i,,drop=FALSE]))
  }),use.names=FALSE))) else character()

  target_ids <- if(identical(scope_type,"partial")) {
    setdiff(complete_ids,prior_ids)
  } else {
    scope$case_ids
  }
  target_outcomes <- w04_human_subset_outcomes(outcomes,target_ids)
  analysis <- w04_consistency_analysis(target_outcomes,ids)
  full_ready <- length(scope$case_ids)>0L && setequal(complete_ids,scope$case_ids)
  save_ready <- if(identical(scope_type,"partial")) {
    length(target_ids)>0L && analysis$complete>0L
  } else {
    full_ready && analysis$complete==length(scope$case_ids)
  }
  reason <- if(save_ready) "" else if(identical(scope_type,"partial") && !length(target_ids)) {
    "There are no new jointly completed records since the last saved consistency result."
  } else if(identical(scope_type,"full") && !full_ready) {
    sprintf(
      "Full assignment is not complete: %d of %d jointly assigned records have decisions from all selected reviewers.",
      length(complete_ids),length(scope$case_ids)
    )
  } else {
    "No complete records are available for this consistency check."
  }

  list(
    valid=TRUE,
    save_ready=save_ready,
    reason=reason,
    scope_type=scope_type,
    rater_ids=ids,
    reviewer_key=scope$reviewer_key,
    assignment_scope_id=scope$assignment_scope_id,
    assigned_case_ids=scope$case_ids,
    complete_case_ids=complete_ids,
    previously_saved_case_ids=prior_ids,
    target_case_ids=target_ids,
    assigned_n=length(scope$case_ids),
    complete_n=length(complete_ids),
    previously_saved_n=length(intersect(prior_ids,scope$case_ids)),
    analysis=analysis
  )
}

w04_human_pattern_stats <- function(patterns,n_raters) {
  n_raters <- suppressWarnings(as.integer(n_raters))
  if(is.na(n_raters) || n_raters < 2L) {
    return(list(n=0L,agreement_n=0L,raw_agreement=NA_real_,metric="",kappa=NA_real_))
  }
  patterns <- patterns %||% list()
  lev <- c("retain","exclude","uncertain")
  if(!length(patterns)) {
    return(list(
      n=0L,agreement_n=0L,raw_agreement=NA_real_,
      metric=if(n_raters==2L)"Cohen's kappa" else "Fleiss' kappa",
      kappa=NA_real_
    ))
  }
  parsed <- lapply(patterns,function(z) {
    vals <- strsplit(as.character(z$pattern %||% "")," \\| ")[[1L]]
    vals <- vapply(vals,w04_normalise_screening_decision,character(1))
    count <- suppressWarnings(as.integer(z$n %||% 0L))
    if(is.na(count)) count <- 0L
    list(vals=vals,n=count)
  })
  good <- vapply(parsed,function(z)length(z$vals)==n_raters && all(nzchar(z$vals)) && z$n>0L,logical(1))
  parsed <- parsed[good]
  n <- sum(vapply(parsed,function(z)z$n,integer(1)))
  if(!n) {
    return(list(
      n=0L,agreement_n=0L,raw_agreement=NA_real_,
      metric=if(n_raters==2L)"Cohen's kappa" else "Fleiss' kappa",
      kappa=NA_real_
    ))
  }
  agreement_n <- sum(vapply(parsed,function(z)if(length(unique(z$vals))==1L)z$n else 0L,integer(1)))
  raw <- agreement_n/n

  if(n_raters==2L) {
    cells <- matrix(0,nrow=3L,ncol=3L,dimnames=list(lev,lev))
    for(z in parsed) {
      cells[z$vals[[1L]],z$vals[[2L]]] <- cells[z$vals[[1L]],z$vals[[2L]]] + z$n
    }
    pa <- rowSums(cells)/n
    pb <- colSums(cells)/n
    pe <- sum(pa*pb)
    po <- sum(diag(cells))/n
    k <- if(isTRUE(all.equal(pe,1)))NA_real_ else (po-pe)/(1-pe)
    return(list(
      n=as.integer(n),agreement_n=as.integer(agreement_n),raw_agreement=raw,
      metric="Cohen's kappa",kappa=as.numeric(k),cells=cells
    ))
  }

  category_ratings <- setNames(numeric(length(lev)),lev)
  agreement_sum <- 0
  for(z in parsed) {
    counts <- table(factor(z$vals,levels=lev))
    category_ratings <- category_ratings + as.numeric(counts)*z$n
    p_i <- (sum(as.numeric(counts)^2)-n_raters)/(n_raters*(n_raters-1))
    agreement_sum <- agreement_sum + p_i*z$n
  }
  p_bar <- agreement_sum/n
  p_j <- category_ratings/(n*n_raters)
  pe <- sum(p_j^2)
  k <- if(isTRUE(all.equal(pe,1)))NA_real_ else (p_bar-pe)/(1-pe)
  list(
    n=as.integer(n),agreement_n=as.integer(agreement_n),raw_agreement=raw,
    metric="Fleiss' kappa",kappa=as.numeric(k),
    fleiss_pairwise_agreement=as.numeric(p_bar),
    category_ratings=category_ratings
  )
}

w04_human_registry_row <- function(
  analysis,record_ids,batch_id,queue_sha256,assignment_scope_id,scope_type,
  rater_labels=character(),analysis_id="",created_by="",created_at_utc=NULL
) {
  ids <- w04_human_consistency_raters(analysis$rater_ids %||% character())
  if(length(ids)<2L) stop("Human consistency result requires at least two human reviewers",call.=FALSE)
  if("model"%in%as.character(analysis$rater_ids %||% character())) {
    stop("Model comparisons cannot be stored in the human consistency registry",call.=FALSE)
  }
  scope_type <- match.arg(as.character(scope_type),c("partial","full"))
  records <- sort(unique(as.character(record_ids)))
  records <- records[nzchar(records)]
  if(!length(records)) stop("Cannot save an empty human consistency set",call.=FALSE)
  if(as.integer(analysis$complete %||% 0L)!=length(records)) {
    stop("Saved human consistency set must contain only complete cases",call.=FALSE)
  }
  reviewer_key <- w04_human_reviewer_key(ids)
  record_hash <- digest::digest(paste(records,collapse="\n"),algo="sha256",serialize=FALSE)
  consistency_id <- paste0(
    "w04-human-kappa-",
    substr(digest::digest(
      paste(as.character(batch_id),reviewer_key,scope_type,record_hash,sep="|"),
      algo="sha256",serialize=FALSE
    ),1L,24L)
  )
  created <- if(is.null(created_at_utc)) {
    format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
  } else as.character(created_at_utc)
  labels <- as.character(rater_labels)
  if(length(labels)!=length(ids)) labels <- ids
  names(labels) <- NULL
  data.frame(
    consistency_id=consistency_id,
    date=substr(created,1L,10L),
    batch_id=as.character(batch_id),
    queue_sha256=tolower(as.character(queue_sha256)),
    assignment_scope_id=as.character(assignment_scope_id),
    scope_type=scope_type,
    reviewer_key=reviewer_key,
    rater_ids_json=as.character(jsonlite::toJSON(ids,auto_unbox=FALSE)),
    rater_labels_json=as.character(jsonlite::toJSON(labels,auto_unbox=FALSE)),
    n_raters=as.character(length(ids)),
    records_n=as.character(length(records)),
    record_ids_json=as.character(jsonlite::toJSON(records,auto_unbox=FALSE)),
    record_ids_sha256=record_hash,
    metric=as.character(analysis$metric %||% ""),
    agreement_n=as.character(analysis$agreement_cases %||% 0L),
    conflict_n=as.character(analysis$conflict_cases %||% 0L),
    raw_agreement=as.character(analysis$raw_agreement %||% NA_real_),
    kappa=as.character(analysis$kappa %||% NA_real_),
    patterns_json=as.character(jsonlite::toJSON(analysis$patterns %||% list(),auto_unbox=TRUE,null="null",na="null")),
    pairwise_json=as.character(jsonlite::toJSON(analysis$pairwise %||% list(),auto_unbox=TRUE,null="null",na="null")),
    analysis_id=as.character(analysis_id),
    created_by=as.character(created_by),
    created_at_utc=created,
    stringsAsFactors=FALSE
  )
}

w04_human_registry_patterns <- function(row) {
  tryCatch(
    jsonlite::fromJSON(as.character(row$patterns_json %||% "[]"),simplifyVector=FALSE),
    error=function(e)list()
  )
}

w04_human_registry_contributors <- function(registry,reviewer_key) {
  x <- w04_normalise_human_kappa_registry(registry)
  x <- x[x$reviewer_key==as.character(reviewer_key),,drop=FALSE]
  if(!nrow(x)) return(list(rows=x,superseded_ids=character(),error=""))
  memberships <- lapply(seq_len(nrow(x)),function(i)w04_human_registry_record_ids(as.list(x[i,,drop=FALSE])))
  superseded <- rep(FALSE,nrow(x))
  full_idx <- which(x$scope_type=="full")
  if(length(full_idx)) {
    for(i in seq_len(nrow(x))) {
      for(j in full_idx) {
        if(i==j) next
        if(length(memberships[[i]]) && all(memberships[[i]] %in% memberships[[j]])) {
          if(length(memberships[[j]])>length(memberships[[i]]) || x$scope_type[[i]]!="full") {
            superseded[[i]] <- TRUE
          }
        }
      }
    }
  }
  keep <- which(!superseded)
  if(length(keep)>1L) {
    pairs <- combn(keep,2L,simplify=FALSE)
    bad <- Filter(function(p)length(intersect(memberships[[p[[1L]]]],memberships[[p[[2L]]]]))>0L,pairs)
    if(length(bad)) {
      return(list(
        rows=x[keep,,drop=FALSE],
        superseded_ids=x$consistency_id[superseded],
        error="Saved human-consistency sets overlap without a containing Full result; cumulative kappa is not calculated."
      ))
    }
  }
  list(rows=x[keep,,drop=FALSE],superseded_ids=x$consistency_id[superseded],error="")
}

w04_human_registry_cumulative <- function(registry,reviewer_key) {
  z <- w04_human_registry_contributors(registry,reviewer_key)
  if(nzchar(z$error)) {
    return(list(valid=FALSE,error=z$error,reviewer_key=reviewer_key,n=0L,kappa=NA_real_))
  }
  rows <- z$rows
  if(!nrow(rows)) return(list(valid=TRUE,error="",reviewer_key=reviewer_key,n=0L,kappa=NA_real_))
  n_raters <- unique(suppressWarnings(as.integer(rows$n_raters)))
  n_raters <- n_raters[!is.na(n_raters)]
  if(length(n_raters)!=1L) {
    return(list(valid=FALSE,error="Reviewer count changed within one reviewer set.",reviewer_key=reviewer_key,n=0L,kappa=NA_real_))
  }
  patterns <- list()
  for(i in seq_len(nrow(rows))) {
    p <- w04_human_registry_patterns(as.list(rows[i,,drop=FALSE]))
    patterns <- c(patterns,p)
  }
  if(length(patterns)) {
    keys <- vapply(patterns,function(p)as.character(p$pattern %||% ""),character(1))
    vals <- vapply(patterns,function(p)suppressWarnings(as.integer(p$n %||% 0L)),integer(1))
    vals[is.na(vals)] <- 0L
    agg <- tapply(vals,keys,sum)
    patterns <- lapply(names(agg),function(k)list(pattern=k,n=as.integer(agg[[k]])))
  }
  stat <- w04_human_pattern_stats(patterns,n_raters[[1L]])
  labels <- w04_human_registry_json_chars(rows$rater_labels_json[[nrow(rows)]])
  list(
    valid=TRUE,error="",reviewer_key=reviewer_key,
    rater_ids=w04_human_registry_json_chars(rows$rater_ids_json[[nrow(rows)]]),
    rater_labels=labels,n_raters=n_raters[[1L]],
    n=stat$n,agreement_n=stat$agreement_n,raw_agreement=stat$raw_agreement,
    metric=stat$metric,kappa=stat$kappa,
    contributing_ids=as.character(rows$consistency_id),
    superseded_ids=z$superseded_ids
  )
}

w04_human_registry_cumulative_all <- function(registry) {
  x <- w04_normalise_human_kappa_registry(registry)
  keys <- unique(x$reviewer_key[nzchar(x$reviewer_key)])
  lapply(keys,function(k)w04_human_registry_cumulative(x,k))
}
