w04_blind_case_outcomes <- function(
  cases,
  assignments,
  decisions,
  batch_id,
  task_type = "manual_screening"
) {
  cases <- cases %||% list()
  decisions <- decisions %||% list()
  active_assignments <- active_assignments_for_batch(
    assignments %||% list(),
    "04",
    batch_id,
    task_type
  )

  case_ids <- vapply(
    cases,
    function(x) as.character(x$review_case_id %||% x$case_id %||% x$record_id %||% ""),
    character(1)
  )

  lapply(seq_along(cases), function(i) {
    case_id <- case_ids[[i]]
    case_assignments <- Filter(
      function(x) identical(normalise_assignment_row(x)$case_id, case_id),
      active_assignments
    )
    assigned_users <- unique(vapply(
      case_assignments,
      function(x) normalise_assignment_row(x)$user_id,
      character(1)
    ))
    assigned_users <- assigned_users[nzchar(assigned_users)]

    case_decisions <- Filter(
      function(x) {
        identical(decision_case_id(x), case_id) &&
          decision_user_id(x) %in% assigned_users
      },
      decisions
    )

    by_user <- lapply(assigned_users, function(uid) {
      hits <- Filter(
        function(x) identical(decision_user_id(x), uid),
        case_decisions
      )
      event <- if (length(hits)) hits[[1L]] else NULL
      list(
        user_id = uid,
        complete = !is.null(event),
        decision = if (is.null(event)) "" else as.character(event$decision %||% ""),
        event = event
      )
    })
    names(by_user) <- assigned_users

    completed_users <- assigned_users[vapply(
      by_user,
      function(x) isTRUE(x$complete),
      logical(1)
    )]
    all_complete <- length(assigned_users) > 0L &&
      length(completed_users) == length(assigned_users)

    completed_decisions <- if (length(completed_users)) {
      vapply(
        by_user[completed_users],
        function(x) as.character(x$decision %||% ""),
        character(1)
      )
    } else character()

    substantive <- completed_decisions %in% c("retain", "exclude")
    exact_agreement <- all_complete &&
      length(assigned_users) >= 2L &&
      length(completed_decisions) > 0L &&
      all(substantive) &&
      length(unique(completed_decisions)) == 1L

    single_human <- all_complete &&
      length(assigned_users) == 1L &&
      length(completed_decisions) == 1L &&
      completed_decisions[[1L]] %in% c("retain","exclude")

    status <- if (!length(assigned_users)) {
      "unassigned"
    } else if (!all_complete) {
      "pending"
    } else if (single_human) {
      "single_human"
    } else if (exact_agreement) {
      "agreement"
    } else {
      "conflict"
    }

    list(
      case_id = case_id,
      record_id = as.character(cases[[i]]$record_id %||% ""),
      status = status,
      assigned_user_ids = assigned_users,
      completed_user_ids = completed_users,
      reviewer_decisions = by_user,
      final_decision = if (exact_agreement || single_human) unique(completed_decisions)[[1L]] else "",
      case = cases[[i]]
    )
  })
}

w04_blind_agreements <- function(outcomes) {
  Filter(function(x) identical(x$status, "agreement"), outcomes %||% list())
}

w04_blind_conflicts <- function(outcomes) {
  Filter(function(x) identical(x$status, "conflict"), outcomes %||% list())
}

w04_blind_pending <- function(outcomes) {
  Filter(function(x) identical(x$status, "pending"), outcomes %||% list())
}


w04_normalise_screening_decision <- function(x) {
  x <- tolower(trimws(as.character(x %||% "")))
  if (x %in% c("retain","include","included")) return("retain")
  if (x %in% c("exclude","excluded")) return("exclude")
  if (x %in% c("uncertain","unsure")) return("uncertain")
  ""
}

w04_cohen_kappa <- function(a, b) {
  a <- vapply(a, w04_normalise_screening_decision, character(1))
  b <- vapply(b, w04_normalise_screening_decision, character(1))
  keep <- nzchar(a) & nzchar(b)
  a <- a[keep]
  b <- b[keep]
  n <- length(a)
  if (!n) return(list(n=0L, agreement=NA_real_, kappa=NA_real_))
  lev <- c("retain","exclude","uncertain")
  po <- mean(a == b)
  pa <- table(factor(a, levels=lev)) / n
  pb <- table(factor(b, levels=lev)) / n
  pe <- sum(pa * pb)
  k <- if (isTRUE(all.equal(pe,1))) NA_real_ else (po - pe) / (1 - pe)
  list(n=as.integer(n), agreement=po, kappa=as.numeric(k))
}

w04_fleiss_kappa <- function(decision_matrix) {
  if (is.null(dim(decision_matrix)) || nrow(decision_matrix) == 0L || ncol(decision_matrix) < 3L) {
    return(list(n=0L, raters=if(is.null(dim(decision_matrix)))0L else ncol(decision_matrix), agreement=NA_real_, kappa=NA_real_))
  }
  lev <- c("retain","exclude","uncertain")
  m <- apply(decision_matrix, c(1,2), w04_normalise_screening_decision)
  complete <- apply(m,1,function(x)all(nzchar(x)))
  m <- m[complete,,drop=FALSE]
  if (!nrow(m)) return(list(n=0L,raters=ncol(decision_matrix),agreement=NA_real_,kappa=NA_real_))
  n_raters <- ncol(m)
  counts <- t(apply(m,1,function(x)table(factor(x,levels=lev))))
  p_i <- (rowSums(counts^2) - n_raters) / (n_raters * (n_raters - 1))
  p_bar <- mean(p_i)
  p_j <- colSums(counts) / (nrow(m) * n_raters)
  p_e <- sum(p_j^2)
  k <- if (isTRUE(all.equal(p_e,1))) NA_real_ else (p_bar - p_e) / (1 - p_e)
  list(n=as.integer(nrow(m)),raters=as.integer(n_raters),agreement=as.numeric(p_bar),kappa=as.numeric(k))
}

w04_model_decision_from_case <- function(case) {
  screening <- case$screening %||% list()
  candidates <- list(
    case$model_decision,
    case$screening_decision,
    screening$model_decision,
    screening$consensus_decision,
    screening$final_decision
  )
  for (x in candidates) {
    d <- w04_normalise_screening_decision(x)
    if (nzchar(d)) return(d)
  }
  ""
}

w04_blind_agreement_stats <- function(outcomes) {
  outcomes <- outcomes %||% list()
  complete <- Filter(function(x)x$status %in% c("agreement","conflict"), outcomes)
  n_complete <- length(complete)
  n_agree <- length(Filter(function(x)identical(x$status,"agreement"), complete))
  n_conflict <- length(Filter(function(x)identical(x$status,"conflict"), complete))

  reviewer_ids <- sort(unique(unlist(lapply(complete,function(x)x$completed_user_ids),use.names=FALSE)))
  pairwise <- list()
  if (length(reviewer_ids) >= 2L) {
    pairs <- combn(reviewer_ids,2,simplify=FALSE)
    pairwise <- lapply(pairs,function(pair) {
      a <- character(); b <- character()
      for (x in complete) {
        ra <- x$reviewer_decisions[[pair[[1L]]]]
        rb <- x$reviewer_decisions[[pair[[2L]]]]
        if (is.null(ra) || is.null(rb) || !isTRUE(ra$complete) || !isTRUE(rb$complete)) next
        a <- c(a,as.character(ra$decision %||% ""))
        b <- c(b,as.character(rb$decision %||% ""))
      }
      s <- w04_cohen_kappa(a,b)
      c(list(reviewer_a=pair[[1L]],reviewer_b=pair[[2L]]),s)
    })
  }

  fleiss <- list(n=0L,raters=length(reviewer_ids),agreement=NA_real_,kappa=NA_real_)
  if (length(reviewer_ids) >= 3L && n_complete) {
    mat <- matrix("",nrow=n_complete,ncol=length(reviewer_ids),dimnames=list(NULL,reviewer_ids))
    for (i in seq_along(complete)) {
      for (uid in reviewer_ids) {
        r <- complete[[i]]$reviewer_decisions[[uid]]
        if (!is.null(r) && isTRUE(r$complete)) mat[i,uid] <- as.character(r$decision %||% "")
      }
    }
    fleiss <- w04_fleiss_kappa(mat)
  }

  human_model <- lapply(reviewer_ids,function(uid) {
    h <- character(); m <- character()
    for (x in complete) {
      r <- x$reviewer_decisions[[uid]]
      md <- w04_model_decision_from_case(x$case)
      if (is.null(r) || !isTRUE(r$complete) || !nzchar(md)) next
      h <- c(h,as.character(r$decision %||% ""))
      m <- c(m,md)
    }
    s <- w04_cohen_kappa(h,m)
    c(list(reviewer=uid),s)
  })
  names(human_model) <- reviewer_ids

  consensus_h <- character(); consensus_m <- character()
  for (x in complete) {
    if (!identical(x$status,"agreement")) next
    md <- w04_model_decision_from_case(x$case)
    if (!nzchar(md)) next
    consensus_h <- c(consensus_h,x$final_decision)
    consensus_m <- c(consensus_m,md)
  }
  consensus_model <- w04_cohen_kappa(consensus_h,consensus_m)

  list(
    complete=as.integer(n_complete),
    agreement_cases=as.integer(n_agree),
    conflict_cases=as.integer(n_conflict),
    raw_agreement=if(n_complete)n_agree/n_complete else NA_real_,
    reviewer_ids=reviewer_ids,
    pairwise=pairwise,
    fleiss=fleiss,
    human_model=human_model,
    consensus_model=consensus_model
  )
}
