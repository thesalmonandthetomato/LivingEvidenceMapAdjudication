suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(jsonlite)
  library(digest)
  library(countrycode)
})

source("R/w01_contract.R", local = TRUE)
source("R/adjudication_schema.R", local = TRUE)
source("R/users.R", local = TRUE)
source("R/assignments.R", local = TRUE)
source("R/decision_events.R", local = TRUE)
source("R/w04_blind_resolution.R", local = TRUE)
source("R/storage_local.R", local = TRUE)
source("R/storage_sheets.R", local = TRUE)
source("R/storage_backend.R", local = TRUE)
source("R/auth.R", local = TRUE)
source("R/github_dispatch.R", local = TRUE)

queue_path <- Sys.getenv("LEM_W01_QUEUE", unset = "fixtures/w01_real_sample_2.jsonl")
decision_path <- Sys.getenv("LEM_W01_DECISIONS", unset = "local_state/w01_decisions.jsonl")
assignment_path <- Sys.getenv("LEM_ASSIGNMENTS", unset = "fixtures/assignments_w01_local.jsonl")
reviewer <- Sys.getenv("LEM_REVIEWER", unset = "prototype-reviewer")

dir.create(dirname(decision_path), recursive = TRUE, showWarnings = FALSE)

theme <- bs_theme(
  version = 5,
  bg = "#f7f8fa",
  fg = "#17212b",
  primary = "#1f5d50",
  base_font = font_google("Source Sans 3"),
  heading_font = font_google("Source Sans 3")
)

normalise_display_text <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}


w08_country_lookup <- local({
  x <- countrycode::codelist
  keep <- !is.na(x$iso3c) & nzchar(as.character(x$iso3c)) &
    !is.na(x$country.name.en) & nzchar(as.character(x$country.name.en))
  z <- unique(data.frame(
    iso3c = toupper(as.character(x$iso3c[keep])),
    country_name = as.character(x$country.name.en[keep]),
    stringsAsFactors = FALSE
  ))
  z <- z[order(z$country_name, z$iso3c), , drop = FALSE]
  rownames(z) <- NULL
  z
})

w08_country_choices <- function() {
  stats::setNames(
    w08_country_lookup$iso3c,
    paste0(w08_country_lookup$iso3c, " — ", w08_country_lookup$country_name)
  )
}

w08_country_names_for_iso3 <- function(iso3) {
  iso3 <- toupper(as.character(iso3 %||% character()))
  idx <- match(iso3, w08_country_lookup$iso3c)
  if (anyNA(idx)) return(NULL)
  unname(w08_country_lookup$country_name[idx])
}

screening_green_terms <- c(
  "salmon",
  "salmonid",
  "salmonids",
  "salmonidae",
  "Salmo",
  "Oncorhynchus",
  "rainbow trout",
  "farm",
  "farms",
  "farmed",
  "farming",
  "farmer",
  "farmers",
  "cage",
  "cages",
  "caged",
  "caging",
  "pen",
  "pens",
  "penned",
  "aquaculture",
  "aquacultures",
  "aquacultural",
  "aquacultured",
  "aquaculturing",
  "aquaculturist",
  "aquaculturists",
  "commercial",
  "commercials",
  "commercially",
  "commerciality",
  "commercialisation",
  "commercialization",
  "commercialise",
  "commercialize",
  "commercialised",
  "commercialized",
  "commercialising",
  "commercializing"
)

screening_red_terms <- c("hatcheries")

highlight_screening_text <- function(text, include_terms = character(), exclude_terms = character()) {
  include_terms <- unique(c(as.character(include_terms), screening_green_terms))
  exclude_terms <- unique(c(as.character(exclude_terms), screening_red_terms))
  text <- normalise_display_text(text)
  if (!nzchar(text)) return("")
  terms <- c(
    setNames(as.character(include_terms), rep("screen-include", length(include_terms))),
    setNames(as.character(exclude_terms), rep("screen-exclude", length(exclude_terms)))
  )
  terms <- terms[nzchar(trimws(terms))]
  if (!length(terms)) return(text)

  hay <- tolower(text)
  candidates <- list()
  k <- 0L
  for (i in seq_along(terms)) {
    term <- trimws(terms[[i]])
    needle <- tolower(term)
    if (!nzchar(needle)) next
    start_at <- 1L
    repeat {
      tail <- substr(hay, start_at, nchar(hay))
      pos <- regexpr(needle, tail, fixed = TRUE)[[1L]]
      if (pos < 0L) break
      s <- start_at + pos - 1L
      e <- s + nchar(term) - 1L
      k <- k + 1L
      candidates[[k]] <- list(start=s,end=e,class=names(terms)[[i]],length=nchar(term))
      start_at <- s + 1L
      if (start_at > nchar(hay)) break
    }
  }
  if (!length(candidates)) return(text)

  ord <- order(
    vapply(candidates, function(x) x$start, integer(1)),
    -vapply(candidates, function(x) x$length, integer(1))
  )
  candidates <- candidates[ord]
  chosen <- list()
  last_end <- 0L
  for (x in candidates) {
    if (x$start > last_end) {
      chosen[[length(chosen)+1L]] <- x
      last_end <- x$end
    }
  }

  out <- list()
  cursor <- 1L
  for (x in chosen) {
    if (x$start > cursor) out[[length(out)+1L]] <- substr(text,cursor,x$start-1L)
    out[[length(out)+1L]] <- tags$span(class=x$class,substr(text,x$start,x$end))
    cursor <- x$end + 1L
  }
  if (cursor <= nchar(text)) out[[length(out)+1L]] <- substr(text,cursor,nchar(text))
  do.call(tagList,out)
}


highlight_named_terms <- function(text, terms) {
  text <- normalise_display_text(text)
  if (!nzchar(text)) return("")
  term_classes <- names(terms)
  terms <- as.character(terms)
  keep <- nzchar(trimws(terms)) & nzchar(term_classes %||% "")
  terms <- terms[keep]
  term_classes <- term_classes[keep]
  if (!length(terms)) return(text)

  hay <- tolower(text)
  candidates <- list()
  k <- 0L
  for (i in seq_along(terms)) {
    term <- normalise_display_text(terms[[i]])
    needle <- tolower(term)
    if (!nzchar(needle)) next
    start_at <- 1L
    repeat {
      tail <- substr(hay,start_at,nchar(hay))
      pos <- regexpr(needle,tail,fixed=TRUE)[[1L]]
      if (pos < 0L) break
      s <- start_at + pos - 1L
      e <- s + nchar(term) - 1L
      k <- k + 1L
      candidates[[k]] <- list(
        start=s,end=e,class=term_classes[[i]],length=nchar(term)
      )
      start_at <- s + 1L
      if (start_at > nchar(hay)) break
    }
  }
  if (!length(candidates)) return(text)

  ord <- order(
    vapply(candidates,function(x)x$start,integer(1)),
    -vapply(candidates,function(x)x$length,integer(1))
  )
  candidates <- candidates[ord]
  chosen <- list()
  last_end <- 0L
  for (x in candidates) {
    if (x$start > last_end) {
      chosen[[length(chosen)+1L]] <- x
      last_end <- x$end
    }
  }

  out <- list()
  cursor <- 1L
  for (x in chosen) {
    if (x$start > cursor) out[[length(out)+1L]] <- substr(text,cursor,x$start-1L)
    out[[length(out)+1L]] <- tags$span(class=x$class,substr(text,x$start,x$end))
    cursor <- x$end + 1L
  }
  if (cursor <= nchar(text)) out[[length(out)+1L]] <- substr(text,cursor,nchar(text))
  do.call(tagList,out)
}

w08_record_highlight_terms <- function(issues, species_options = character()) {
  issues <- issues %||% list()
  terms <- character()

  has_species <- any(vapply(
    issues,
    function(x) identical(as.character(x$issue_type %||% ""), "species_none"),
    logical(1)
  ))
  if (has_species) {
    species_terms <- unique(c(
      as.character(species_options %||% character()),
      "salmon", "salmonid", "salmonids", "salmonidae",
      "Salmo", "Oncorhynchus", "trout"
    ))
    species_terms <- species_terms[
      nzchar(trimws(species_terms)) &
        !tolower(trimws(species_terms)) %in% c("unspecified species")
    ]
    terms <- c(
      terms,
      stats::setNames(
        species_terms,
        rep("screen-include", length(species_terms))
      )
    )
  }

  geo_evidence <- unlist(lapply(issues, function(issue) {
    typ <- as.character(issue$issue_type %||% "")
    if (!typ %in% c("geography_unresolved", "geography_evidence_unvalidated")) {
      return(character())
    }
    av <- issue$automated_value %||% list()
    vals <- as.character(unlist(
      av$luna_evidence %||% av$model_evidence %||% character(),
      use.names = FALSE
    ))
    vals <- vapply(vals, normalise_display_text, character(1))
    vals <- trimws(vals)
    vals <- sub('^["“”]+', "", vals)
    vals <- sub('["“”]+$', "", vals)
    vals[nzchar(vals)]
  }), use.names = FALSE)
  geo_evidence <- unique(geo_evidence[nzchar(geo_evidence)])
  if (length(geo_evidence)) {
    terms <- c(
      terms,
      stats::setNames(
        geo_evidence,
        rep("w08-geo-evidence", length(geo_evidence))
      )
    )
  }

  terms
}

token_lcs_matches <- function(a, b, char_level = FALSE) {
  a <- as.character(a %||% "")
  b <- as.character(b %||% "")

  if (char_level) {
    ta <- strsplit(a, "", fixed = TRUE)[[1L]]
    tb <- strsplit(b, "", fixed = TRUE)[[1L]]
    sep <- ""
  } else {
    ta <- if (nzchar(a)) strsplit(a, "\\s+")[[1L]] else character()
    tb <- if (nzchar(b)) strsplit(b, "\\s+")[[1L]] else character()
    sep <- " "
  }

  na <- length(ta); nb <- length(tb)
  ma <- rep(FALSE, na); mb <- rep(FALSE, nb)

  if (na && nb) {
    dp <- matrix(0L, nrow = na + 1L, ncol = nb + 1L)
    for (i in seq_len(na)) {
      for (j in seq_len(nb)) {
        if (identical(tolower(ta[[i]]), tolower(tb[[j]]))) {
          dp[i + 1L, j + 1L] <- dp[i, j] + 1L
        } else {
          dp[i + 1L, j + 1L] <- max(dp[i, j + 1L], dp[i + 1L, j])
        }
      }
    }

    i <- na; j <- nb
    while (i > 0L && j > 0L) {
      if (identical(tolower(ta[[i]]), tolower(tb[[j]]))) {
        ma[[i]] <- TRUE; mb[[j]] <- TRUE
        i <- i - 1L; j <- j - 1L
      } else if (dp[i, j + 1L] >= dp[i + 1L, j]) {
        i <- i - 1L
      } else {
        j <- j - 1L
      }
    }
  }

  render <- function(tokens, matched) {
    if (!length(tokens)) return(tags$span(class = "diff-missing", ""))

    if (char_level) {
      runs <- list()
      start <- 1L
      for (k in seq_along(tokens)) {
        is_last <- k == length(tokens)
        changes <- !is_last && !identical(matched[[k]], matched[[k + 1L]])
        if (is_last || changes) {
          cls <- if (matched[[start]]) "diff-same" else "diff-different"
          runs[[length(runs) + 1L]] <- tags$span(
            class = paste(cls, "diff-char-run"),
            paste0(tokens[start:k], collapse = "")
          )
          start <- k + 1L
        }
      }
      return(do.call(tagList, runs))
    }

    parts <- lapply(seq_along(tokens), function(k) {
      cls <- if (matched[[k]]) "diff-same" else "diff-different"
      tagList(
        tags$span(class = cls, tokens[[k]]),
        if (k < length(tokens)) " " else NULL
      )
    })
    do.call(tagList, parts)
  }

  list(a = render(ta, ma), b = render(tb, mb))
}

field_pair <- function(a, b, char_level = FALSE) {
  a <- as.character(a %||% "")
  b <- as.character(b %||% "")
  if (!nzchar(a) && !nzchar(b)) {
    return(list(a = "", b = ""))
  }
  if (identical(tolower(a), tolower(b))) {
    return(list(
      a = tags$span(class = "diff-same", a),
      b = tags$span(class = "diff-same", b)
    ))
  }
  token_lcs_matches(a, b, char_level = char_level)
}

record_card <- function(rec, label, fields, side = c("a","b")) {
  side <- match.arg(side)
  card(
    class = "h-100 record-card",
    card_header(div(class = "d-flex justify-content-between align-items-center",
                    tags$strong(label),
                    fields$source[[side]])),
    div(
      class = "compact-record-body",
      div(class = "record-title", fields$title[[side]]),
      tags$dl(
        class = "record-meta",
        tags$dt("Authors"), tags$dd(fields$authors[[side]]),
        tags$dt("Year"), tags$dd(fields$year[[side]]),
        tags$dt("Journal"), tags$dd(fields$journal[[side]]),
        tags$dt("DOI"), tags$dd(fields$doi[[side]]),
        tags$dt("Source ID"), tags$dd(fields$source_record_id[[side]])
      ),
      tags$hr(class = "record-divider"),
      tags$h6(class = "abstract-heading", "Abstract"),
      div(class = "abstract-text", fields$abstract[[side]])
    )
  )
}

ui <- page_fillable(
  theme = theme,
  tags$head(
    tags$script(HTML("
      (function() {
        const storagePrefix = 'lem-accordion:';
        function restoreDetails(root) {
          (root || document).querySelectorAll('details[data-accordion-key]').forEach(function(el) {
            const key = storagePrefix + el.getAttribute('data-accordion-key');
            const saved = sessionStorage.getItem(key);
            if (saved === 'open') el.open = true;
            if (saved === 'closed') el.open = false;
            if (!el.dataset.lemBound) {
              el.addEventListener('toggle', function() {
                sessionStorage.setItem(key, el.open ? 'open' : 'closed');
              });
              el.dataset.lemBound = '1';
            }
          });
        }
        document.addEventListener('DOMContentLoaded', function() {
          restoreDetails(document);
          const observer = new MutationObserver(function() { restoreDetails(document); });
          observer.observe(document.body, { childList: true, subtree: true });

          const overlay = document.createElement('div');
          overlay.id = 'lem-busy-overlay';
          overlay.innerHTML = '<div class=\"lem-busy-box\"><div class=\"lem-busy-spinner\"></div><span>Working…</span></div>';
          document.body.appendChild(overlay);

          let busyTimer = null;
          $(document).on('shiny:busy', function() {
            clearTimeout(busyTimer);
            busyTimer = setTimeout(function() {
              overlay.classList.add('is-visible');
            }, 180);
          });
          $(document).on('shiny:idle', function() {
            clearTimeout(busyTimer);
            overlay.classList.remove('is-visible');
          });
        });
      })();
    ")),
    tags$style(HTML("
    body { background:#f7f8fa; }
    .app-shell { max-width:1500px; margin:0 auto; padding:20px; width:100%; }
    .login-shell { max-width:520px; margin:8vh auto 0 auto; padding:20px; width:100%; }
    .record-card { border:1px solid #dde3e8; box-shadow:0 2px 10px rgba(22,33,43,.05); }
    .record-card .card-body { padding:0; }
    .compact-record-body { padding:.65rem .9rem .8rem .9rem; }
    .w08-issue-card { min-height:320px; overflow:visible !important; position:relative; z-index:1; }
    .w08-issue-card:focus-within { z-index:100; }
    .w08-issue-card:has(.selectize-control.dropdown-active) { z-index:1000; }
    .w08-species-issue { min-height:430px; }
    .w08-issue-card .card-body,
    .w08-issue-card .selectize-control,
    .w08-issue-card .selectize-input { overflow:visible !important; }
    .w08-issue-card .selectize-dropdown { z-index:3000; max-height:240px; overflow-y:auto; }
    .w08-review-layout {
      display:grid;
      grid-template-columns:minmax(0,1.35fr) minmax(360px,.85fr);
      gap:1rem;
      align-items:start;
    }
    .w08-record-card,
    .w08-record-card > .card-body,
    .w08-review-layout,
    .w08-review-decisions,
    .w08-review-decisions .card,
    .w08-review-decisions .card-body {
      overflow:visible !important;
    }
    .w08-review-evidence,
    .w08-review-decisions { min-width:0; }
    .w08-review-decisions { position:relative; z-index:10; }
    .w08-review-decisions .w08-issue-card,
    .w08-review-decisions .w08-species-issue {
      min-height:0;
    }
    .w08-review-decisions .selectize-control { position:relative; z-index:30; }
    .w08-review-decisions .selectize-control.dropdown-active,
    .w08-review-decisions .selectize-control:focus-within {
      z-index:6000 !important;
    }
    .w08-review-decisions .selectize-dropdown {
      z-index:6100 !important;
      max-height:320px;
      overflow-y:auto !important;
    }
    .w08-review-decisions .w08-issue-card:last-child { margin-bottom:0 !important; }
    @media (max-width: 980px) {
      .w08-review-layout { grid-template-columns:1fr; }
    }
    #lem-busy-overlay {
      display:none;
      position:fixed;
      inset:0;
      z-index:10000;
      background:rgba(247,248,250,.72);
      align-items:center;
      justify-content:center;
      pointer-events:all;
    }
    #lem-busy-overlay.is-visible { display:flex; }
    .lem-busy-box {
      display:flex;
      align-items:center;
      gap:.7rem;
      background:#fff;
      border:1px solid #dde3e8;
      border-radius:10px;
      padding:.8rem 1rem;
      box-shadow:0 4px 18px rgba(22,33,43,.12);
      font-weight:600;
      color:#2f3943;
    }
    .lem-busy-spinner {
      width:22px;
      height:22px;
      border:3px solid #d7dfdc;
      border-top-color:#1f5d50;
      border-radius:50%;
      animation:lem-spin .8s linear infinite;
    }
    @keyframes lem-spin { to { transform:rotate(360deg); } }
    .record-title { font-size:1.05rem; font-weight:700; line-height:1.25; margin-bottom:.45rem; }
    .record-meta { display:grid; grid-template-columns:78px 1fr; gap:.08rem .55rem; margin:0; line-height:1.28; }
    .record-meta dt { color:#66727d; font-weight:600; }
    .record-meta dd { margin:0; overflow-wrap:anywhere; }
    .record-divider { margin:.55rem 0 .45rem 0; }
    .abstract-heading { margin:0 0 .35rem 0; }
    .abstract-text { line-height:1.32; white-space:normal; }
    .w04-text { font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Arial,sans-serif; font-variant-ligatures:none; font-feature-settings:'liga' 0; letter-spacing:normal; word-spacing:normal; }
    .w04-citation-grid { display:grid; grid-template-columns:minmax(260px,2.4fr) minmax(70px,.45fr) minmax(180px,1.4fr) minmax(70px,.45fr) minmax(90px,.6fr); gap:.4rem .8rem; margin:.2rem 0 .35rem 0; align-items:start; }
    .w04-citation-item { min-width:0; }
    .w04-citation-label { display:block; color:#66727d; font-size:.78rem; font-weight:600; margin-bottom:.05rem; }
    .w04-citation-value { display:block; overflow-wrap:anywhere; }
    .w04-doi { font-size:.9rem; margin:.15rem 0 .45rem 0; color:#4c5965; overflow-wrap:anywhere; }
    .w04-keywords { margin-top:.55rem; padding-top:.45rem; border-top:1px solid #e6eaed; font-size:.9rem; }
    .screen-include { background:#d9f2df; color:#145c2e; border-radius:3px; padding:0 .05rem; }
    .screen-exclude { background:#fde0e0; color:#8b1e1e; border-radius:3px; padding:0 .05rem; }
    .w08-geo-evidence { background:#dcecf7; color:#174f70; border-radius:3px; padding:0 .05rem; }
    .source-badge { border-radius:999px; padding:.2rem .55rem; font-size:.78rem; }
    .diff-same { background:#d9f2df; color:#145c2e; border-radius:3px; padding:0 .08rem; }
    .diff-different { background:#fde0e0; color:#8b1e1e; border-radius:3px; padding:0 .08rem; }
    .diff-char-run { padding:0; border-radius:2px; }
    .source-badge.diff-same { background:#d9f2df; color:#145c2e; }
    .source-badge.diff-different { background:#fde0e0; color:#8b1e1e; }
    .evidence-grid { display:grid; grid-template-columns:repeat(auto-fit,minmax(180px,1fr)); gap:.6rem; }
    .evidence-item { background:#fff; border:1px solid #e1e5e9; border-radius:8px; padding:.65rem .75rem; }
    .decision-panel { margin-bottom:.85rem; }
    .decision-panel .card-body { padding:.75rem 1rem; }
    .decision-row .btn { min-width:125px; }
    .decision-row .btn.decision-selected { outline:3px solid #17212b; outline-offset:2px; font-weight:700; }
    .saved-note { font-weight:600; color:#1f5d50; min-height:1.2rem; }
    .nav-row .btn { min-width:95px; }
    .task-shell { max-width:1050px; margin:4vh auto 0 auto; padding:20px; width:100%; }
    .task-card { border:1px solid #dde3e8; box-shadow:0 2px 10px rgba(22,33,43,.05); }
    .task-card .card-header strong { font-size:1.2rem; font-weight:700; line-height:1.2; }
    .task-kpis { display:grid; grid-template-columns:repeat(3,minmax(90px,1fr)); gap:.65rem; margin:.8rem 0; }
    .task-kpi { background:#f7f8fa; border:1px solid #e1e5e9; border-radius:8px; padding:.55rem .65rem; }
    .task-kpi strong { display:block; font-size:1.15rem; }
    .task-badge { background:#eef3f1; border-radius:999px; padding:.2rem .55rem; font-size:.78rem; }
    .assignment-summary { margin-bottom:1rem; border:1px solid #dde3e8; box-shadow:0 2px 10px rgba(22,33,43,.04); }
    .assignment-kpis { display:grid; grid-template-columns:repeat(5,minmax(100px,1fr)); gap:.5rem; margin-bottom:.75rem; }
    .assignment-kpi { background:#f7f8fa; border:1px solid #e1e5e9; border-radius:8px; padding:.55rem .65rem; }
    .assignment-kpi span { display:block; color:#6a747d; font-size:.76rem; }
    .assignment-kpi strong { display:block; font-size:1.08rem; margin-top:.08rem; }
    .assignment-table-wrap { overflow-x:auto; }
    .assignment-table { width:100%; border-collapse:collapse; font-size:.86rem; }
    .assignment-table th,.assignment-table td { padding:.42rem .5rem; border-top:1px solid #e7eaed; text-align:left; vertical-align:middle; white-space:nowrap; }
    .assignment-table th { color:#66727d; font-weight:600; }
    .assignment-progress-bar { width:110px; height:7px; border-radius:999px; background:#e5e9ec; overflow:hidden; display:inline-block; vertical-align:middle; margin-right:.4rem; }
    .assignment-progress-fill { height:100%; background:#1f5d50; }
    .assignment-disclosure > summary, .assignment-workflow > summary { cursor:pointer; list-style:none; }
    .assignment-disclosure > summary::-webkit-details-marker, .assignment-workflow > summary::-webkit-details-marker { display:none; }
    .assignment-disclosure > summary::before, .assignment-workflow > summary::before { content:'▸'; display:inline-block; width:1.1rem; color:#66727d; }
    .assignment-disclosure[open] > summary::before, .assignment-workflow[open] > summary::before { content:'▾'; }
    .assignment-workflow { border-top:1px solid #e7eaed; padding:.65rem 0 .15rem 0; }
    .assignment-workflow .shiny-input-container { width:100% !important; max-width:none !important; }
    .assignment-workflow .shiny-options-group { width:100%; max-width:none; }
    .assignment-workflow .form-check-label { max-width:none; }
    .assignment-mode-note { color:#66727d; font-size:.8rem; }
    @media (max-width:620px) { .assignment-kpis { grid-template-columns:repeat(2,minmax(100px,1fr)); } }
    .pipeline-summary { background:#fff; border:1px solid #dde3e8; border-radius:12px; padding:.85rem 1rem; margin-bottom:1rem; box-shadow:0 2px 10px rgba(22,33,43,.04); }
    .pipeline-summary-top { display:flex; flex-wrap:wrap; align-items:flex-end; justify-content:space-between; gap:.65rem 1rem; margin-bottom:.65rem; }
    .pipeline-kpis { display:grid; grid-template-columns:repeat(5,minmax(145px,1fr)); gap:.5rem; margin-top:.15rem; }
    .pipeline-kpi { background:#f7f8fa; border:1px solid #e1e5e9; border-radius:8px; padding:.62rem .72rem .58rem .72rem; min-width:0; }
    .pipeline-kpi-label { display:block; color:#6a747d; font-size:.8rem; line-height:1.2; margin-bottom:.2rem; overflow-wrap:anywhere; }
    .pipeline-kpi-value { display:block; font-size:1.12rem; line-height:1.2; font-weight:700; white-space:normal; overflow-wrap:anywhere; }
    .pipeline-kpi-sub { display:block; color:#7c858d; font-size:.74rem; line-height:1.2; margin-top:.12rem; overflow-wrap:anywhere; }
    .pipeline-kpi.pre-update {
      background:#fbfcfc;
      border-color:#eceff1;
    }
    .pipeline-kpi.pre-update .pipeline-kpi-label,
    .pipeline-kpi.pre-update .pipeline-kpi-value,
    .pipeline-kpi.pre-update .pipeline-kpi-sub {
      color:#9aa2a9;
    }
    .workflow-line { display:grid; grid-template-columns:repeat(11,1fr); gap:.28rem; margin-top:.75rem; }
    .workflow-segment { height:7px; border-radius:999px; background:#e5e9ec; }
    .workflow-segment.done { background:#1f5d50; }
    .workflow-segment.active { background:#8fb7ac; box-shadow:0 0 0 1px #1f5d50 inset; }
    .workflow-labels { display:grid; grid-template-columns:repeat(11,1fr); gap:.28rem; margin-top:.22rem; color:#7b858d; font-size:.69rem; text-align:center; }
    @media (max-width: 1000px) { .pipeline-kpis { grid-template-columns:repeat(3,minmax(135px,1fr)); } }
    @media (max-width: 620px) { .pipeline-kpis { grid-template-columns:repeat(2,minmax(120px,1fr)); } }
    @media (max-width: 390px) { .pipeline-kpis { grid-template-columns:1fr; } }
  "))
  ),
  uiOutput("root_ui")
)

server <- function(input, output, session) {
  authenticated <- reactiveVal(FALSE)
  current_user <- reactiveVal(NULL)
  user_registry_rv <- reactiveVal(list())
  assignment_registry_rv <- reactiveVal(list())
  assignment_manage_status <- reactiveVal("")
  test_queue_status <- reactiveVal("")
  w08_fresh_test_status <- reactiveVal("")
  w01_all_cases_rv <- reactiveVal(list())
  app_view <- reactiveVal("tasks")
  failed_attempts <- reactiveVal(0L)
  lock_until <- reactiveVal(as.POSIXct(NA))
  idx <- reactiveVal(1L)
  complete <- reactiveVal(FALSE)
  status <- reactiveVal("")
  cases_rv <- reactiveVal(NULL)
  queue_sha_rv <- reactiveVal("")
  batch_id_rv <- reactiveVal("")
  batch_status_rv <- reactiveVal("")
  decisions <- reactiveVal(list())

  w02_all_cases_rv <- reactiveVal(list())
  w02_cases_rv <- reactiveVal(NULL)
  w02_queue_sha_rv <- reactiveVal("")
  w02_batch_id_rv <- reactiveVal("")
  w02_idx <- reactiveVal(1L)
  w02_decisions <- reactiveVal(list())
  w02_batch_status_rv <- reactiveVal("")
  w02_status <- reactiveVal("")

  w04_all_cases_rv <- reactiveVal(list())
  w04_cases_rv <- reactiveVal(NULL)
  w04_queue_sha_rv <- reactiveVal("")
  w04_batch_id_rv <- reactiveVal("")
  w04_idx <- reactiveVal(1L)
  w04_decisions <- reactiveVal(list())
  w04_batch_status_rv <- reactiveVal("")
  w04_status <- reactiveVal("")
  w04_include_terms <- reactiveVal(character())
  w04_exclude_terms <- reactiveVal(character())
  w04_consistency_analyses_rv <- reactiveVal(list())
  w04_conflict_sets_rv <- reactiveVal(list())
  w04_consistency_history_loaded <- reactiveVal(FALSE)
  w04_consistency_status <- reactiveVal("")

  w04_resolution_cases_rv <- reactiveVal(NULL)
  w04_resolution_queue_sha_rv <- reactiveVal("")
  w04_resolution_batch_id_rv <- reactiveVal("")
  w04_resolution_source_run_id_rv <- reactiveVal("")
  w04_resolution_idx <- reactiveVal(1L)
  w04_resolution_decisions <- reactiveVal(list())
  w04_resolution_batch_status_rv <- reactiveVal("")
  w04_resolution_status <- reactiveVal("")
  w04_resolution_include_terms <- reactiveVal(character())
  w04_resolution_exclude_terms <- reactiveVal(character())

  w04_conflict_cases_rv <- reactiveVal(NULL)
  w04_conflict_queue_sha_rv <- reactiveVal("")
  w04_conflict_batch_id_rv <- reactiveVal("")
  w04_conflict_idx <- reactiveVal(1L)
  w04_conflict_decisions <- reactiveVal(list())
  w04_conflict_batch_status_rv <- reactiveVal("")
  w04_conflict_status <- reactiveVal("")

  w08_all_cases_rv <- reactiveVal(list())
  w08_cases_rv <- reactiveVal(NULL)
  w08_queue_sha_rv <- reactiveVal("")
  w08_batch_id_rv <- reactiveVal("")
  w08_source_run_id_rv <- reactiveVal("")
  w08_case_sha_rv <- reactiveVal(character())
  w08_species_options <- reactiveVal(character())
  w08_topic_options <- reactiveVal(list())
  w08_idx <- reactiveVal(1L)
  w08_decisions <- reactiveVal(list())
  w08_batch_status_rv <- reactiveVal("")
  w08_status <- reactiveVal("")
  pipeline_status_rv <- reactiveVal(NULL)
  manual_screening_rv <- reactiveVal(NULL)

  observe({
    req(authenticated())
    invalidateLater(30000, session)
    refreshed <- tryCatch(
      if(identical(storage_backend(),"google_sheets")) read_latest_pipeline_status() else NULL,
      error=function(e) NULL
    )
    if(!is.null(refreshed)) pipeline_status_rv(refreshed)
  })

  decision_ids <- function(ds = decisions()) {
    if (!length(ds)) return(character())
    keep <- vapply(ds,function(x)as.character(x$decision %||% "") %in% c("duplicate","not_duplicate"),logical(1))
    if(!any(keep)) return(character())
    unique(vapply(ds[keep], function(x) as.character(x$review_case_id %||% ""), character(1)))
  }

  filter_batch_decisions <- function(ds, sha) {
    if (!length(ds)) return(list())
    keep <- vapply(ds, function(x) identical(as.character(x$queue_sha256 %||% ""), sha), logical(1))
    ds[keep]
  }

  unresolved_indices <- function() {
    cs <- cases_rv()
    if (is.null(cs)) return(integer())
    ids <- vapply(cs, function(x) as.character(x$review_case_id), character(1))
    which(!ids %in% decision_ids())
  }

  w02_filter_batch_decisions <- function(ds, sha) {
    if (!length(ds)) return(list())
    keep <- vapply(ds, function(x) identical(as.character(x$queue_sha256 %||% ""), sha), logical(1))
    ds[keep]
  }

  w02_resolved_ids <- function(ds = w02_decisions()) {
    if (!length(ds)) return(character())
    keep <- vapply(ds, function(x) !identical(as.character(x$decision %||% ""), "uncertain"), logical(1))
    if (!any(keep)) return(character())
    unique(vapply(ds[keep], function(x) as.character(x$review_case_id %||% ""), character(1)))
  }

  w02_unresolved_indices <- function() {
    cs <- w02_cases_rv()
    if (is.null(cs)) return(integer())
    ids <- vapply(cs, function(x) as.character(x$review_case_id), character(1))
    which(!ids %in% w02_resolved_ids())
  }

  load_w02_batch <- function() {
    if (!identical(storage_backend(), "google_sheets")) return(NULL)
    read_sheet_w02_queue()
  }

  w04_filter_batch_decisions <- function(ds, sha) {
    if (!length(ds)) return(list())
    keep <- vapply(ds, function(x) identical(as.character(x$queue_sha256 %||% ""), sha), logical(1))
    ds[keep]
  }

  w04_decision_ids <- function(ds = w04_decisions(), user_id = session_reviewer_id()) {
    if (!length(ds)) return(character())
    mine <- Filter(
      function(x) identical(decision_user_id(x), as.character(user_id)),
      ds
    )
    if (!length(mine)) return(character())
    unique(vapply(mine,function(x)as.character(x$review_case_id %||% ""),character(1)))
  }

  w04_unresolved_indices <- function() {
    cs <- w04_cases_rv()
    if (is.null(cs)) return(integer())
    ids <- vapply(cs,function(x)as.character(x$review_case_id),character(1))
    which(!ids %in% w04_decision_ids())
  }

  load_w04_batch <- function() {
    if (!identical(storage_backend(), "google_sheets")) return(NULL)

    # A real pipeline W04 queue always takes precedence over synthetic test data.
    production <- read_sheet_w04_queue()
    if (!is.null(production)) return(production)

    # The isolated W04 test queue is reloaded on fresh reviewer sessions only
    # when its batch has active manual-screening assignments. This allows
    # assigned reviewers to see their synthetic records without ever allowing
    # test data to override a real pipeline queue.
    test_tab <- Sys.getenv("LEM_W04_TEST_QUEUE_TAB", unset="queue_w04_test_active")
    synthetic <- read_sheet_w04_queue_from_tab(test_tab)
    if (is.null(synthetic)) return(NULL)

    active_test_assignments <- active_assignments_for_batch(
      assignment_registry_rv(),
      "04",
      synthetic$batch_id,
      "manual_screening"
    )
    if (!length(active_test_assignments)) return(NULL)
    synthetic
  }

  w04_resolution_decision_ids <- function(ds = w04_resolution_decisions()) {
    if (!length(ds)) return(character())
    unique(vapply(ds,function(x)as.character(x$review_case_id %||% ""),character(1)))
  }
  w04_resolution_unresolved_indices <- function() {
    cs <- w04_resolution_cases_rv()
    if (is.null(cs)) return(integer())
    ids <- vapply(cs,function(x)as.character(x$review_case_id),character(1))
    which(!ids %in% w04_resolution_decision_ids())
  }
  load_w04_resolution_batch <- function() {
    if (!identical(storage_backend(),"google_sheets")) return(NULL)
    read_sheet_w04_resolution_queue()
  }

  observe({
    req(authenticated())
    batch_id <- as.character(w04_batch_id_rv() %||% "")
    all_cases <- w04_all_cases_rv() %||% list()
    user <- current_user()
    if (!nzchar(batch_id) || !length(all_cases) || is.null(user)) {
      w04_cases_rv(list())
      return()
    }
    visible <- cases_for_assignment_user(
      all_cases,
      assignment_registry_rv(),
      "04",
      batch_id,
      user,
      task_type="manual_screening",
      active_events=w04_decisions()
    )
    w04_cases_rv(visible)
    unresolved <- if(length(visible)) {
      ids <- vapply(visible,function(x)as.character(x$review_case_id %||% ""),character(1))
      which(!ids %in% w04_decision_ids())
    } else integer()
    current <- suppressWarnings(as.integer(w04_idx()))
    if (is.na(current) || current < 1L || current > max(1L,length(visible))) {
      w04_idx(if(length(unresolved)) unresolved[[1L]] else 1L)
    }
  })

  w04_blind_outcomes <- reactive({
    if (!nzchar(as.character(w04_batch_id_rv())) || !length(w04_all_cases_rv())) return(list())
    w04_blind_case_outcomes(
      cases = w04_all_cases_rv(),
      assignments = assignment_registry_rv(),
      decisions = w04_decisions(),
      batch_id = w04_batch_id_rv(),
      task_type = "manual_screening"
    )
  })

  w04_blind_review_complete <- reactive({
    if (!nzchar(as.character(w04_batch_id_rv()))) return(FALSE)
    workflow_all_assignments_complete(
      "04", w04_batch_id_rv(), "manual_screening", w04_decisions()
    )
  })

  w04_blind_conflict_cases <- reactive({
    if (!isTRUE(w04_blind_review_complete())) return(list())
    conflicts <- w04_blind_conflicts(w04_blind_outcomes())
    lapply(conflicts, function(x) {
      z <- x$case
      z$blind_review <- list(
        status = x$status,
        assigned_user_ids = x$assigned_user_ids,
        completed_user_ids = x$completed_user_ids,
        reviewer_decisions = x$reviewer_decisions
      )
      z$conflict_source <- "blind_manual_screening"
      z
    })
  })

  w04_blind_agreement_count <- reactive({
    if (!isTRUE(w04_blind_review_complete())) return(0L)
    length(w04_blind_agreements(w04_blind_outcomes()))
  })

  w04_active_consistency_conflict_set <- reactive({
    xs <- w04_conflict_sets_rv() %||% list()
    if (!length(xs)) return(NULL)
    xs <- Filter(
      function(x) identical(
        as.character(x$parent_batch_id %||% ""),
        as.character(w04_batch_id_rv())
      ),
      xs
    )
    if (!length(xs)) return(NULL)
    xs[[length(xs)]]
  })

  w04_consistency_conflict_cases <- reactive({
    z <- w04_active_consistency_conflict_set()
    if (is.null(z)) return(list())
    rater_ids <- tryCatch(
      as.character(jsonlite::fromJSON(as.character(z$rater_ids_json %||% "[]"))),
      error=function(e) character()
    )
    case_ids <- tryCatch(
      as.character(jsonlite::fromJSON(as.character(z$conflict_case_ids_json %||% "[]"))),
      error=function(e) character()
    )
    w04_conflict_cases_for_raters(
      outcomes=w04_blind_outcomes(),
      rater_ids=rater_ids,
      conflict_case_ids=case_ids,
      analysis_id=as.character(z$analysis_id %||% ""),
      conflict_set_id=as.character(z$conflict_set_id %||% "")
    )
  })

  w04_all_conflict_cases <- reactive({
    generated <- w04_consistency_conflict_cases()
    if (length(generated)) return(generated)
    w04_conflict_cases_rv() %||% list()
  })

  w04_active_conflict_cases <- reactive({
    cs <- w04_all_conflict_cases()
    if (!length(cs)) return(list())
    cases_for_assignment_user(
      cs,
      assignment_registry_rv(),
      "04",
      w04_active_conflict_batch_id(),
      current_user(),
      task_type = "conflict_resolution",
      active_events = w04_conflict_decisions()
    )
  })

  w04_active_conflict_queue_sha <- reactive({
    generated <- w04_active_consistency_conflict_set()
    if (!is.null(generated)) {
      return(as.character(generated$conflict_queue_sha256 %||% ""))
    }
    w04_conflict_queue_sha_rv()
  })

  w04_active_conflict_batch_id <- reactive({
    generated <- w04_active_consistency_conflict_set()
    if (!is.null(generated)) {
      return(as.character(generated$conflict_set_id %||% ""))
    }
    w04_conflict_batch_id_rv()
  })

  w04_active_conflict_batch_status <- reactive({
    if (!is.null(w04_active_consistency_conflict_set())) return("generated")
    w04_conflict_batch_status_rv()
  })

  w04_conflict_decision_ids <- function(ds = w04_conflict_decisions()) {
    if (!length(ds)) return(character())
    unique(vapply(ds,function(x)as.character(x$review_case_id %||% ""),character(1)))
  }
  w04_conflict_unresolved_indices <- function() {
    cs <- w04_active_conflict_cases()
    if (!length(cs)) return(integer())
    ids <- vapply(cs,function(x)as.character(x$review_case_id),character(1))
    which(!ids %in% w04_conflict_decision_ids())
  }
  load_w04_conflict_batch <- function() {
    if (!identical(storage_backend(),"google_sheets")) return(NULL)
    read_sheet_w04_conflict_queue()
  }

  w08_filter_batch_decisions <- function(ds, sha) {
    if (!length(ds)) return(list())
    keep <- vapply(ds,function(x)identical(as.character(x$queue_sha256 %||% ""),sha),logical(1))
    ds[keep]
  }

  w08_decision_ids <- function(ds = w08_decisions()) {
    if(!length(ds)) return(character())
    unique(vapply(ds,function(x)as.character(x$record_id %||% ""),character(1)))
  }

  w08_unresolved_indices <- function() {
    cs <- w08_cases_rv()
    if(is.null(cs)) return(integer())
    ids <- vapply(cs,function(x)as.character(x$record_id %||% ""),character(1))
    which(!ids %in% w08_decision_ids())
  }

  load_w08_batch <- function() {
    if(!identical(storage_backend(),"google_sheets")) return(NULL)
    read_sheet_w08_queue()
  }

  mark_review_complete <- function(stage,batch_id,queue_sha,status_rv) {
    if (identical(storage_backend(), "local")) {
      status_rv("review_complete")
      return(invisible(TRUE))
    }

    current <- tryCatch(
      append_batch_status(
        stage=stage,
        batch_id=batch_id,
        queue_sha256=queue_sha,
        status="review_complete",
        message="All active Shiny decisions are complete and persistently saved."
      ),
      error=function(e)e
    )
    if(inherits(current,"error")) stop(conditionMessage(current),call.=FALSE)
    status_rv("review_complete")
    invisible(TRUE)
  }

  load_batch <- function() {
    if (identical(storage_backend(), "google_sheets")) {
      return(read_sheet_w01_queue())
    }
    cs <- read_w01_cases(queue_path)
    list(
      batch_id = basename(queue_path),
      queue_sha256 = digest(file = queue_path, algo = "sha256", serialize = FALSE),
      cases = cs
    )
  }

  fmt_pipeline_n <- function(x) {
    z <- suppressWarnings(as.numeric(as.character(x %||% "")))
    if(is.na(z)) return("—")
    format(round(z),big.mark=",",scientific=FALSE,trim=TRUE)
  }

  fmt_pipeline_date <- function(x) {
    z <- as.character(x %||% "")
    if(!nzchar(z)) return("—")
    d <- suppressWarnings(as.Date(z))
    if(is.na(d)) return(z)
    format(d,"%d %b %Y")
  }

  read_manual_screening_metrics <- function() {
    url <- Sys.getenv(
      "LEM_W04_AGREEMENT_URL",
      unset = "https://raw.githubusercontent.com/thesalmonandthetomato/LivingEvidenceMap/workflow01-final-architecture/docs/workflow04/workflow04_agreement_summary.json"
    )
    x <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    list(
      manually_screened = as.integer(x$historical_comparator_records),
      kappa = as.numeric(x$consensus_vs_historical$substantive_binary$cohen_kappa),
      kappa_n = as.integer(x$consensus_vs_historical$substantive_binary$n)
    )
  }

  pipeline_summary_ui <- function() {
    p <- pipeline_status_rv()
    if(is.null(p)) {
      return(div(
        class="pipeline-summary",
        div(class="text-secondary small","Current-run metrics are not available yet.")
      ))
    }

    completed <- suppressWarnings(as.integer(as.character(p$completed_through %||% "0")))
    active <- suppressWarnings(as.integer(as.character(p$active_workflow %||% "")))
    if(is.na(completed)) completed <- 0L

    status_label <- tolower(trimws(as.character(p$status_label %||% "")))
    update_finalised <- is.na(active) &&
      grepl("(^|\\b)(complete|completed|final|finalised|finalized)(\\b|$)", status_label)

    workflow_labels <- c("W00","W01","W02","W03","W04","W05","W06","W07","W08","W09","W10")
    segs <- lapply(seq_along(workflow_labels),function(i){
      cls <- "workflow-segment"
      if(i <= completed) cls <- paste(cls,"done")
      else if(!is.na(active) && i==active) cls <- paste(cls,"active")
      div(class=cls,title=workflow_labels[[i]])
    })

    kpi <- function(label,value,sub=NULL,class_extra=NULL) {
      div(
        class=paste(c("pipeline-kpi", class_extra), collapse=" "),
        tags$span(class="pipeline-kpi-label",label),
        tags$span(class="pipeline-kpi-value",value),
        if(!is.null(sub)) tags$span(class="pipeline-kpi-sub",sub)
      )
    }

    div(
      class="pipeline-summary",
      div(
        class="pipeline-summary-top",
        div(
          tags$strong("Current update"),
          tags$span(
            class="text-secondary small ms-2",
            as.character(p$status_label %||% "")
          )
        ),
        div(
          class="text-secondary small",
          paste0("Run ",as.character(p$update_id %||% ""))
        )
      ),
      div(
        class="text-secondary small mb-2",
        paste0("Last search: ",fmt_pipeline_date(p$last_search_date))
      ),
      div(
        class="pipeline-kpis",
        kpi("Search results",fmt_pipeline_n(p$search_results_total),"W00"),
        kpi("After dedup.",fmt_pipeline_n(p$deduplicated_records),"W01"),
        kpi("Enriched",fmt_pipeline_n(p$enriched_records),"W02"),
        kpi("Retracted",fmt_pipeline_n(p$retracted_records),"W03"),
        {
          m <- manual_screening_rv()
          kpi(
            "Manually screened",
            if(is.null(m)) "—" else fmt_pipeline_n(m$manually_screened),
            if(is.null(m) || is.na(m$kappa)) NULL else paste0("κ ",sprintf("%.3f",m$kappa))
          )
        },
        kpi(
          "Screened",
          paste0(fmt_pipeline_n(p$screened_include)," / ",fmt_pipeline_n(p$screened_exclude)),
          "include / exclude"
        ),
        kpi(
          "Species",
          fmt_pipeline_n(p$screened_include),
          "records processed"
        ),
        kpi(
          "Geography",
          paste0(fmt_pipeline_n(p$geography_with)," / ",fmt_pipeline_n(p$geography_without)),
          "with / without"
        ),
        kpi(
          "Topics",
          paste0(fmt_pipeline_n(p$topic_with)," / ",fmt_pipeline_n(p$topic_without)),
          "with / without"
        ),
        kpi(
          "Canonical database",
          fmt_pipeline_n(p$canonical_existing),
          if (isTRUE(update_finalised)) "current" else "pre-update",
          if (isTRUE(update_finalised)) NULL else "pre-update"
        )
      ),
      div(class="workflow-line",segs),
      div(
        class="workflow-labels",
        lapply(workflow_labels,tags$span)
      )
    )
  }

  output$root_ui <- renderUI({
    if (!authenticated()) {
      return(div(
        class = "login-shell",
        card(
          card_header(tags$strong("LivingEvidenceMap adjudication")),
          tags$p("Sign in with your adjudication account."),
          textInput("login_email", "Email"),
          passwordInput("access_key", "Access key"),
          if (!individual_auth_configured()) {
            tags$div(
              class = "mt-2 text-danger small",
              "Individual authentication is not configured for this deployment. Check LEM_INITIAL_USERS_JSON and LEM_USER_ACCESS_KEY_HASHES_JSON in Connect Cloud."
            )
          },
          actionButton("login", "Continue", class = "btn-primary"),
          tags$div(class = "mt-2 text-danger", textOutput("login_status"))
        )
      ))
    }

    if (identical(app_view(), "tasks")) {
      w01_total <- length(cases_rv() %||% list())
      w01_remaining <- if (w01_total) length(unresolved_indices()) else 0L
      w01_completed <- max(0L, w01_total - w01_remaining)

      w02_user_total <- length(w02_cases_rv() %||% list())
      w02_user_remaining <- if (w02_user_total) length(w02_unresolved_indices()) else 0L
      if (session_can("manage_assignments")) {
        w02_total <- length(w02_all_cases_rv() %||% list())
        w02_all_ids <- if (w02_total) vapply(
          w02_all_cases_rv(),
          function(x) as.character(x$review_case_id %||% ""),
          character(1)
        ) else character()
        w02_completed <- sum(w02_all_ids %in% w02_resolved_ids())
        w02_remaining <- max(0L, w02_total - w02_completed)
      } else {
        w02_total <- w02_user_total
        w02_remaining <- w02_user_remaining
        w02_completed <- max(0L, w02_total - w02_remaining)
      }

      w04_total <- length(w04_cases_rv() %||% list())
      w04_remaining <- if (w04_total) length(w04_unresolved_indices()) else 0L
      w04_completed <- max(0L, w04_total - w04_remaining)

      w04_resolution_total <- length(w04_resolution_cases_rv() %||% list())
      w04_resolution_remaining <- if (w04_resolution_total) length(w04_resolution_unresolved_indices()) else 0L
      w04_resolution_completed <- max(0L, w04_resolution_total - w04_resolution_remaining)

      w04_conflict_user_total <- length(w04_active_conflict_cases())
      w04_conflict_user_remaining <- if (w04_conflict_user_total) length(w04_conflict_unresolved_indices()) else 0L
      if (session_can("manage_assignments")) {
        w04_conflict_total <- length(w04_all_conflict_cases())
        w04_conflict_ids <- if (w04_conflict_total) vapply(
          w04_all_conflict_cases(),
          function(x) as.character(x$review_case_id %||% ""),
          character(1)
        ) else character()
        w04_conflict_completed <- sum(w04_conflict_ids %in% w04_conflict_decision_ids())
        w04_conflict_remaining <- max(0L,w04_conflict_total-w04_conflict_completed)
      } else {
        w04_conflict_total <- w04_conflict_user_total
        w04_conflict_remaining <- w04_conflict_user_remaining
        w04_conflict_completed <- max(0L,w04_conflict_total-w04_conflict_remaining)
      }

      w08_user_total <- length(w08_cases_rv() %||% list())
      w08_user_remaining <- if (w08_user_total) length(w08_unresolved_indices()) else 0L
      if (session_can("manage_assignments")) {
        annotation_total <- length(w08_all_cases_rv() %||% list())
        w08_all_ids <- if (annotation_total) vapply(
          w08_all_cases_rv(),
          function(x) as.character(x$record_id %||% ""),
          character(1)
        ) else character()
        annotation_completed <- sum(w08_all_ids %in% w08_decision_ids())
        annotation_remaining <- max(0L, annotation_total - annotation_completed)
      } else {
        annotation_total <- w08_user_total
        annotation_remaining <- w08_user_remaining
        annotation_completed <- max(0L, annotation_total - annotation_remaining)
      }

      stage_card <- function(title, workflow, description, total, completed, remaining, button_id = NULL, button_label = NULL, batch = "", lifecycle_status = "", can_open = TRUE, idle_text = "No records awaiting review") {
        card(
          class = "task-card h-100",
          card_header(
            div(
              class = "d-flex justify-content-between align-items-center",
              tags$strong(title),
              tags$span(class = "task-badge", workflow)
            )
          ),
          div(
            class = "p-3 d-flex flex-column h-100",
            tags$p(class = "mb-2", description),
            div(
              class = "task-kpis",
              div(class = "task-kpi", tags$span(class = "text-secondary small", "Total"), tags$strong(total)),
              div(class = "task-kpi", tags$span(class = "text-secondary small", "Completed"), tags$strong(completed)),
              div(class = "task-kpi", tags$span(class = "text-secondary small", "Remaining"), tags$strong(remaining))
            ),
            if (nzchar(batch)) tags$div(class = "text-secondary small mb-1", batch),
            if (identical(lifecycle_status,"review_complete")) {
              tags$div(class="small mb-2",tags$span(class="task-badge","Awaiting workflow completion"))
            },
            if (!is.null(button_id) && remaining > 0L && isTRUE(can_open)) {
              actionButton(button_id, button_label, class = "btn-primary mt-auto")
            } else {
              tags$div(class = "text-secondary small mt-auto", idle_text)
            }
          )
        )
      }

      return(div(
        class = "task-shell",
        div(
          class = "d-flex justify-content-between align-items-end mb-3",
          div(
            tags$h2("Human verification", class = "mb-1"),
            tags$div("Records remaining at each verification stage.", class = "text-secondary")
          ),
          uiOutput("session_identity")
        ),
        pipeline_summary_ui(),
        uiOutput("assignment_progress"),
        div(
          class = "row g-3",
          div(
            class = "col-12 col-lg-6",
            stage_card(
              "Deduplication",
              "Workflow 01",
              "Potential duplicate bibliographic records.",
              w01_total, w01_completed, w01_remaining,
              if (w01_remaining > 0L) "open_w01" else NULL,
              "Continue deduplication",
              batch_id_rv(),
              batch_status_rv()
            )
          ),
          div(
            class = "col-12 col-lg-6",
            stage_card(
              "Enrichment",
              "Workflow 02",
              "Bibliographic enrichment conflicts requiring human resolution.",
              w02_total, w02_completed, w02_remaining,
              if (w02_remaining > 0L) "open_w02" else NULL,
              "Continue enrichment",
              w02_batch_id_rv(),
              w02_batch_status_rv(),
              can_open = w02_user_remaining > 0L,
              idle_text = if (!nzchar(w02_batch_id_rv())) {
                "No active queue"
              } else if (
                w02_remaining > 0L && session_can("manage_assignments") && w02_user_remaining == 0L
              ) {
                "Active cases are awaiting assignment"
              } else {
                "No records awaiting review"
              }
            )
          ),
          div(
            class = "col-12 col-lg-6",
            stage_card(
              "Manual screening",
              "Workflow 04",
              "Blind manual title and abstract screening for validation and ongoing human contribution.",
              w04_total, w04_completed, w04_remaining,
              if (w04_remaining > 0L) "open_w04" else NULL,
              "Continue manual screening",
              w04_batch_id_rv(),
              w04_batch_status_rv()
            )
          ),
          div(
            class = "col-12 col-lg-6",
            stage_card(
              "Model uncertainty resolution",
              "Workflow 04",
              "Records unresolved after model consensus passes requiring a final human include/exclude decision.",
              w04_resolution_total, w04_resolution_completed, w04_resolution_remaining,
              if (w04_resolution_remaining > 0L) "open_w04_resolution" else NULL,
              "Resolve model uncertainty",
              w04_resolution_batch_id_rv(),
              w04_resolution_batch_status_rv()
            )
          ),
          div(
            class = "col-12 col-lg-6",
            stage_card(
              "Reviewer conflict resolution",
              "Workflow 04",
              "Human–machine or human–human screening conflicts awaiting adjudication.",
              w04_conflict_total, w04_conflict_completed, w04_conflict_remaining,
              if (w04_conflict_remaining > 0L) "open_w04_conflict" else NULL,
              "Resolve reviewer conflicts",
              w04_active_conflict_batch_id(),
              w04_active_conflict_batch_status(),
              can_open = w04_conflict_user_remaining > 0L,
              idle_text = if (w04_conflict_total == 0L) {
                "No reviewer conflicts"
              } else if (w04_conflict_user_remaining == 0L && session_can("manage_assignments")) {
                "Conflicts exist and are awaiting assignment"
              } else if (w04_conflict_user_remaining == 0L) {
                "No conflicts assigned to you"
              } else {
                "No reviewer conflicts"
              }
            )
          ),
          div(
            class = "col-12 col-lg-6",
            stage_card(
              "Annotation",
              "Workflow 08",
              "Records requiring species, geography or topic verification.",
              annotation_total, annotation_completed, annotation_remaining,
              if (annotation_remaining > 0L) "open_w08" else NULL,
              "Continue annotation",
              w08_batch_id_rv(),
              w08_batch_status_rv(),
              can_open = w08_user_remaining > 0L,
              idle_text = if (!nzchar(w08_batch_id_rv())) {
                "No active queue"
              } else if (
                annotation_remaining > 0L && session_can("manage_assignments") && w08_user_remaining == 0L
              ) {
                "Active cases are awaiting assignment"
              } else {
                "No records awaiting review"
              }
            )
          )
        )
      ))
    }

    if (identical(app_view(), "w02")) {
      return(div(
        class = "app-shell",
        div(
          class = "d-flex justify-content-between align-items-center mb-3",
          div(
            tags$h2("LivingEvidenceMap adjudication", class="mb-0"),
            tags$div(
              sprintf("Workflow 02 · metadata conflict review · %s", w02_batch_id_rv()),
              class="text-secondary"
            )
          ),
          div(
            class = "d-flex align-items-center gap-3 flex-wrap justify-content-end",
            uiOutput("session_identity"),
            actionButton("back_to_tasks_w02", "Back to tasks", class = "btn-outline-secondary btn-sm"),
            uiOutput("w02_progress_text")
          )
        ),
        uiOutput("w02_progress_bar"),
        uiOutput("w02_decision_panel"),
        uiOutput("w02_case_view")
      ))
    }

    if (identical(app_view(), "w04")) {
      return(div(
        class = "app-shell",
        div(
          class = "d-flex justify-content-between align-items-center mb-3",
          div(
            tags$h2("LivingEvidenceMap manual screening", class="mb-0"),
            tags$div(sprintf("Workflow 04 · blind human screening · %s", w04_batch_id_rv()), class="text-secondary")
          ),
          div(
            class = "d-flex align-items-center gap-3 flex-wrap justify-content-end",
            uiOutput("session_identity"),
            actionButton("back_to_tasks_w04", "Back to tasks", class = "btn-outline-secondary btn-sm"),
            uiOutput("w04_progress_text")
          )
        ),
        uiOutput("w04_progress_bar"),
        card(
          class = "decision-panel",
          div(
            class = "d-flex flex-wrap justify-content-between align-items-center gap-2",
            tags$div(class="saved-note", textOutput("w04_save_status")),
            div(
              class = "d-flex flex-wrap gap-2",
              uiOutput("w04_decision_buttons"),
              div(
                class = "nav-row d-flex gap-2",
                actionButton("w04_previous", "← Previous"),
                actionButton("w04_next", "Next →")
              )
            )
          )
        ),
        uiOutput("w04_case_view")
      ))
    }

    if (identical(app_view(), "w04_resolution")) {
      return(div(
        class="app-shell",
        div(class="d-flex justify-content-between align-items-center mb-3",
          div(
            tags$h2("LivingEvidenceMap model uncertainty resolution",class="mb-0"),
            tags$div(sprintf("Workflow 04 · final human decision · %s",w04_resolution_batch_id_rv()),class="text-secondary")
          ),
          div(class="d-flex align-items-center gap-3 flex-wrap justify-content-end",
            uiOutput("session_identity"),
            actionButton("back_to_tasks_w04_resolution","Back to tasks",class="btn-outline-secondary btn-sm"),
            uiOutput("w04_resolution_progress_text")
          )
        ),
        uiOutput("w04_resolution_progress_bar"),
        card(class="decision-panel",
          div(class="d-flex flex-wrap justify-content-between align-items-center gap-2",
            tags$div(class="saved-note",textOutput("w04_resolution_save_status")),
            div(class="d-flex flex-wrap gap-2",
              uiOutput("w04_resolution_decision_buttons"),
              div(class="nav-row d-flex gap-2",
                actionButton("w04_resolution_previous","← Previous"),
                actionButton("w04_resolution_next","Next →")
              )
            )
          )
        ),
        uiOutput("w04_resolution_case_view")
      ))
    }

    if (identical(app_view(), "w04_conflict")) {
      return(div(
        class="app-shell",
        div(class="d-flex justify-content-between align-items-center mb-3",
          div(
            tags$h2("LivingEvidenceMap reviewer conflict resolution",class="mb-0"),
            tags$div(sprintf("Workflow 04 · human–machine / human–human adjudication · %s",w04_active_conflict_batch_id()),class="text-secondary")
          ),
          div(class="d-flex align-items-center gap-3 flex-wrap justify-content-end",
            uiOutput("session_identity"),
            actionButton("back_to_tasks_w04_conflict","Back to tasks",class="btn-outline-secondary btn-sm")
          )
        ),
        uiOutput("w04_conflict_progress_bar"),
        card(
          class="decision-panel",
          div(
            class="d-flex flex-wrap justify-content-between align-items-center gap-2",
            tags$div(class="saved-note",textOutput("w04_conflict_save_status")),
            div(
              class="d-flex flex-wrap gap-2",
              uiOutput("w04_conflict_decision_buttons"),
              div(
                class="nav-row d-flex gap-2",
                actionButton("w04_conflict_previous","← Previous"),
                actionButton("w04_conflict_next","Next →")
              )
            )
          )
        ),
        uiOutput("w04_conflict_case_view")
      ))
    }

    if (identical(app_view(), "w08")) {
      return(div(
        class = "app-shell",
        div(
          class = "d-flex justify-content-between align-items-center mb-3",
          div(
            tags$h2("LivingEvidenceMap annotation", class="mb-0"),
            tags$div(
              sprintf("Workflow 08 · combined annotation verification · %s",w08_batch_id_rv()),
              class="text-secondary"
            )
          ),
          div(
            class="d-flex align-items-center gap-3 flex-wrap justify-content-end",
            uiOutput("session_identity"),
            actionButton("back_to_tasks_w08","Back to tasks",class="btn-outline-secondary btn-sm"),
            uiOutput("w08_progress_text")
          )
        ),
        uiOutput("w08_progress_bar"),
        card(
          class="decision-panel",
          div(
            class="d-flex flex-wrap justify-content-between align-items-center gap-2",
            tags$div(class="saved-note",textOutput("w08_save_status")),
            div(
              class="d-flex gap-2",
              actionButton("w08_save","Save record",class="btn-primary"),
              actionButton("w08_previous","← Previous"),
              actionButton("w08_next","Next →")
            )
          )
        ),
        uiOutput("w08_case_view")
      ))
    }

    if (complete()) {
      return(div(
        class = "login-shell",
        card(
          card_header(tags$strong("Adjudication complete")),
          tags$h3("Finished"),
          tags$p(sprintf("All %d cases in this batch have been adjudicated.", length(cases_rv()))),
          tags$p("Your decisions have been saved."),
          div(
            class = "d-flex gap-2",
            actionButton("back_to_tasks_complete", "Back to tasks", class = "btn-primary"),
            actionButton("review_last", "Review last case", class = "btn-outline-secondary")
          )
        )
      ))
    }

    div(
      class = "app-shell",
      div(class = "d-flex justify-content-between align-items-center mb-3",
          div(
            tags$h2("LivingEvidenceMap adjudication", class="mb-0"),
            tags$div(textOutput("batch_label"), class="text-secondary")
          ),
          div(
            class = "d-flex align-items-center gap-3 flex-wrap justify-content-end",
            uiOutput("session_identity"),
            actionButton("back_to_tasks", "Back to tasks", class = "btn-outline-secondary btn-sm"),
            div(textOutput("progress_text"))
          )
      ),
      uiOutput("progress_bar"),
      card(
        class = "decision-panel",
        div(
          class = "d-flex flex-wrap justify-content-between align-items-center gap-2",
          tags$div(class="saved-note", textOutput("save_status")),
          div(
            class = "d-flex flex-wrap gap-2",
            uiOutput("w01_decision_buttons"),
            div(
              class = "nav-row d-flex gap-2",
              actionButton("previous", "← Previous"),
              actionButton("next", "Next →")
            )
          )
        )
      ),
      uiOutput("case_view")
    )
  })

  login_status <- reactiveVal("")
  output$login_status <- renderText(login_status())

  output$session_identity <- renderUI({
    req(authenticated(), current_user())
    u <- current_user()
    role_label <- if (identical(as.character(u$role), "administrator")) "Administrator" else "Reviewer"
    div(
      class = "d-flex align-items-center gap-2 flex-wrap justify-content-end",
      tags$span(
        class = "text-secondary small",
        paste0(as.character(u$display_name), " · ", role_label)
      ),
      actionButton("logout", "Log out", class = "btn-outline-secondary btn-sm")
    )
  })

  session_reviewer_id <- function() {
    u <- current_user()
    if (is.null(u)) return(reviewer)
    as.character(u$user_id)
  }

  session_can <- function(permission) {
    user_can(current_user(), permission)
  }

  w01_active_assignment_events <- function() {
    decisions() %||% list()
  }

  w01_all_assignments_complete <- function() {
    assignments <- assignments_for_batch(
      assignment_registry_rv(),
      "01",
      batch_id_rv(),
      task_type = "deduplication"
    )
    if (!length(assignments)) return(TRUE)
    progress <- assignment_progress(
      assignments,
      w01_active_assignment_events(),
      user_registry_rv()
    )
    identical(progress$remaining, 0L)
  }


  w02_active_assignment_events <- function() {
    w02_decisions() %||% list()
  }

  w04_active_assignment_events <- function() {
    w04_decisions() %||% list()
  }

  w08_active_assignment_events <- function() {
    w08_decisions() %||% list()
  }

  workflow_all_assignments_complete <- function(workflow, batch_id, task_type, events) {
    assignments <- active_assignments_for_batch(
      assignment_registry_rv(), workflow, batch_id, task_type
    )
    if (!length(assignments)) return(TRUE)
    progress <- assignment_progress(
      assignments,
      events,
      user_registry_rv(),
      workflow = workflow,
      batch_id = batch_id,
      task_type = task_type
    )
    identical(progress$remaining, 0L)
  }

  output$assignment_progress <- renderUI({
    req(authenticated())
    if (!session_can("manage_assignments")) return(NULL)

    all_assignments <- assignment_registry_rv()
    has_w01_batch <- nzchar(as.character(batch_id_rv())) && length(w01_all_cases_rv()) > 0L
    has_w02_batch <- nzchar(as.character(w02_batch_id_rv())) && length(w02_all_cases_rv()) > 0L
    has_w04_batch <- nzchar(as.character(w04_batch_id_rv())) && length(w04_all_cases_rv()) > 0L
    has_w04_conflict_batch <- nzchar(as.character(w04_active_conflict_batch_id())) && length(w04_all_conflict_cases()) > 0L
    has_w08_batch <- nzchar(as.character(w08_batch_id_rv())) && length(w08_all_cases_rv()) > 0L
    task_labels <- c(
      deduplication = "Deduplication",
      enrichment = "Enrichment",
      manual_screening = "Manual screening",
      model_uncertainty = "Model uncertainty",
      conflict_resolution = "Conflict resolution",
      annotation = "Annotation"
    )
    workflow_labels_admin <- c(
      "01" = "W01",
      "02" = "W02",
      "04" = "W04",
      "08" = "W08"
    )
    mode_labels <- c(
      shared_work_pool = "Shared work pool",
      independent_blind_review = "Independent blind review",
      single_reviewer = "Single-reviewer assignment"
    )

    assignment_group_key <- function(x) {
      a <- normalise_assignment_row(x)
      paste(a$workflow, a$task_type, a$batch_id, sep = "|")
    }
    grouped <- split(all_assignments, vapply(all_assignments, assignment_group_key, character(1)))

    # The registry is historical and append-only, but the administration UI
    # should show only the currently active W01/W02/W04/W08 batch. Older batches
    # remain in Sheets/audit history rather than appearing as duplicate panels.
    active_batch_for <- function(workflow, task_type) {
      if (identical(workflow, "01") && identical(task_type, "deduplication")) {
        return(if (has_w01_batch) as.character(batch_id_rv()) else "")
      }
      if (identical(workflow, "02") && identical(task_type, "enrichment")) {
        return(if (has_w02_batch) as.character(w02_batch_id_rv()) else "")
      }
      if (identical(workflow, "04") && identical(task_type, "manual_screening")) {
        return(if (has_w04_batch) as.character(w04_batch_id_rv()) else "")
      }
      if (identical(workflow, "04") && identical(task_type, "conflict_resolution")) {
        return(if (has_w04_conflict_batch) as.character(w04_active_conflict_batch_id()) else "")
      }
      if (identical(workflow, "08") && identical(task_type, "annotation")) {
        return(if (has_w08_batch) as.character(w08_batch_id_rv()) else "")
      }
      NA_character_
    }

    grouped <- Filter(function(a) {
      if (!length(a)) return(FALSE)
      z <- normalise_assignment_row(a[[1L]])
      active_batch <- active_batch_for(z$workflow, z$task_type)
      if (is.na(active_batch)) return(TRUE)
      nzchar(active_batch) && identical(z$batch_id, active_batch)
    }, grouped)

    events_for_group <- function(a) {
      z <- normalise_assignment_row(a[[1L]])
      if (
        identical(z$workflow, "01") &&
        identical(z$task_type, "deduplication") &&
        identical(z$batch_id, batch_id_rv())
      ) return(w01_active_assignment_events())
      if (
        identical(z$workflow, "02") &&
        identical(z$task_type, "enrichment") &&
        identical(z$batch_id, w02_batch_id_rv())
      ) return(w02_active_assignment_events())
      if (
        identical(z$workflow, "04") &&
        identical(z$task_type, "manual_screening") &&
        identical(z$batch_id, w04_batch_id_rv())
      ) return(w04_active_assignment_events())
      if (
        identical(z$workflow, "04") &&
        identical(z$task_type, "conflict_resolution") &&
        identical(z$batch_id, w04_active_conflict_batch_id())
      ) return(w04_conflict_decisions())
      if (
        identical(z$workflow, "08") &&
        identical(z$task_type, "annotation") &&
        identical(z$batch_id, w08_batch_id_rv())
      ) return(w08_active_assignment_events())
      list()
    }

    group_progress <- lapply(grouped, function(a) {
      z <- normalise_assignment_row(a[[1L]])
      p <- assignment_progress(
        a,
        events_for_group(a),
        user_registry_rv(),
        workflow = z$workflow,
        batch_id = z$batch_id,
        task_type = z$task_type
      )
      list(assignments = a, meta = z, progress = p)
    })

    add_empty_group <- function(workflow, task_type, batch_id, mode) {
      key <- paste(workflow, task_type, batch_id, sep = "|")
      has_group <- any(vapply(
        group_progress,
        function(g) {
          identical(g$meta$workflow, workflow) &&
            identical(g$meta$task_type, task_type) &&
            identical(g$meta$batch_id, batch_id)
        },
        logical(1)
      ))
      if (!has_group) {
        group_progress[[key]] <<- list(
          assignments = list(),
          meta = list(workflow = workflow, task_type = task_type, batch_id = batch_id),
          progress = list(
            assigned = 0L, completed = 0L, resolved_elsewhere = 0L,
            remaining = 0L, cases = 0L, by_user = list(), mode = mode
          )
        )
      }
    }

    if (has_w01_batch) add_empty_group("01","deduplication",batch_id_rv(),ASSIGNMENT_MODES[["shared_work_pool"]])
    add_empty_group(
      "02","enrichment",
      if (has_w02_batch) w02_batch_id_rv() else "no-active-queue",
      ASSIGNMENT_MODES[["shared_work_pool"]]
    )
    add_empty_group(
      "04","manual_screening",
      if (has_w04_batch) w04_batch_id_rv() else "no-active-queue",
      ASSIGNMENT_MODES[["independent_blind_review"]]
    )
    if (has_w04_conflict_batch) {
      add_empty_group(
        "04","conflict_resolution",
        w04_active_conflict_batch_id(),
        ASSIGNMENT_MODES[["single_reviewer"]]
      )
    }
    add_empty_group(
      "08","annotation",
      if (has_w08_batch) w08_batch_id_rv() else "no-active-queue",
      ASSIGNMENT_MODES[["shared_work_pool"]]
    )

    if (length(group_progress) > 1L) {
      workflow_order <- vapply(
        group_progress,
        function(g) suppressWarnings(as.integer(g$meta$workflow %||% "999")),
        integer(1)
      )
      task_rank <- c(
        deduplication=1L,
        enrichment=1L,
        manual_screening=1L,
        model_uncertainty=2L,
        conflict_resolution=3L,
        annotation=1L
      )
      task_order <- vapply(
        group_progress,
        function(g) as.integer(task_rank[[as.character(g$meta$task_type %||% "")]] %||% 99L),
        integer(1)
      )
      group_progress <- group_progress[order(workflow_order, task_order, names(group_progress))]
    }

    total_assigned <- sum(vapply(group_progress, function(x) x$progress$assigned, integer(1)))
    total_completed <- sum(vapply(group_progress, function(x) x$progress$completed, integer(1)))
    total_released <- sum(vapply(group_progress, function(x) x$progress$resolved_elsewhere, integer(1)))
    total_remaining <- sum(vapply(group_progress, function(x) x$progress$remaining, integer(1)))

    assignment_manager_ui <- function(z) {
      cfg <- if (
        identical(z$workflow, "01") &&
        identical(z$task_type, "deduplication") &&
        identical(z$batch_id, batch_id_rv())
      ) {
        list(prefix="w01", workflow="01", task_type="deduplication", batch_id=batch_id_rv(),
             events=w01_active_assignment_events(), label="W01")
      } else if (
        identical(z$workflow, "02") &&
        identical(z$task_type, "enrichment") &&
        identical(z$batch_id, w02_batch_id_rv())
      ) {
        list(prefix="w02", workflow="02", task_type="enrichment", batch_id=w02_batch_id_rv(),
             events=w02_active_assignment_events(), label="W02")
      } else if (
        identical(z$workflow, "04") &&
        identical(z$task_type, "manual_screening") &&
        identical(z$batch_id, w04_batch_id_rv())
      ) {
        list(prefix="w04", workflow="04", task_type="manual_screening", batch_id=w04_batch_id_rv(),
             events=w04_active_assignment_events(), label="W04")
      } else if (
        identical(z$workflow, "04") &&
        identical(z$task_type, "conflict_resolution") &&
        identical(z$batch_id, w04_active_conflict_batch_id())
      ) {
        list(prefix="w04conflict", workflow="04", task_type="conflict_resolution", batch_id=w04_active_conflict_batch_id(),
             events=w04_conflict_decisions(), label="W04 conflict")
      } else if (
        identical(z$workflow, "08") &&
        identical(z$task_type, "annotation") &&
        identical(z$batch_id, w08_batch_id_rv())
      ) {
        list(prefix="w08", workflow="08", task_type="annotation", batch_id=w08_batch_id_rv(),
             events=w08_active_assignment_events(), label="W08")
      } else NULL

      if (is.null(cfg)) {
        if (
          identical(z$workflow, "02") &&
          identical(z$task_type, "enrichment") &&
          identical(z$batch_id, "no-active-queue")
        ) {
          return(tags$div(
            class = "mt-2",
            tags$div(class = "text-secondary small mb-2", "No active W02 queue is loaded."),
            tagList(
              actionButton(
                "create_test_w02_queue_inline",
                "Create W02 test queue",
                class = "btn-outline-secondary btn-sm"
              ),
              tags$div(
                class = "saved-note mt-2",
                textOutput("test_queue_status_w02", inline = TRUE)
              )
            )
          ))
        }
        if (
          identical(z$workflow, "04") &&
          identical(z$task_type, "manual_screening") &&
          identical(z$batch_id, "no-active-queue")
        ) {
          return(tags$div(
            class = "mt-2",
            tags$div(
              class = "text-secondary small mb-2",
              "No active W04 manual-screening queue is loaded. Create an isolated synthetic W04 batch to test reviewer-consistency and validation-set assignment modes."
            ),
            tagList(
              actionButton(
                "create_test_w04_queue_inline",
                "Create W04 test queue",
                class = "btn-outline-secondary btn-sm"
              ),
              tags$div(
                class = "saved-note mt-2",
                textOutput("test_queue_status_w04", inline = TRUE)
              )
            )
          ))
        }
        if (
          identical(z$workflow, "08") &&
          identical(z$task_type, "annotation") &&
          identical(z$batch_id, "no-active-queue")
        ) {
          return(tags$div(
            class = "mt-2",
            tags$div(class = "text-secondary small mb-2", "No active W08 queue is loaded."),
            tagList(
              actionButton(
                "create_test_w08_queue_inline",
                "Create W08 test queue",
                class = "btn-outline-secondary btn-sm"
              ),
              tags$div(
                class = "saved-note mt-2",
                textOutput("test_queue_status_w08", inline = TRUE)
              )
            )
          ))
        }
        return(NULL)
      }

      eligible_users <- Filter(
        function(u) isTRUE(normalise_user_row(u)$active) && user_can(u, "adjudicate_assigned"),
        user_registry_rv()
      )
      eligible_ids <- vapply(eligible_users, function(u) normalise_user_row(u)$user_id, character(1))
      eligible_labels <- vapply(eligible_users, function(u) normalise_user_row(u)$display_name, character(1))
      eligible_choices <- stats::setNames(eligible_ids, eligible_labels)

      cancellable <- cancellable_assignments(
        assignment_registry_rv(),
        cfg$events,
        cfg$workflow,
        cfg$batch_id,
        cfg$task_type
      )
      remove_ids <- unique(vapply(
        cancellable,
        function(x) normalise_assignment_row(x)$user_id,
        character(1)
      ))
      remove_labels <- vapply(remove_ids, function(uid) {
        u <- find_user_by_id(user_registry_rv(), uid, require_active = FALSE)
        reviewer_label <- if (is.null(u)) uid else u$display_name
        n <- sum(vapply(
          cancellable,
          function(x) identical(normalise_assignment_row(x)$user_id, uid),
          logical(1)
        ))
        paste0(reviewer_label, " (", n, " unfinished)")
      }, character(1))
      remove_choices <- stats::setNames(remove_ids, remove_labels)

      mode <- assignment_mode_for(cfg$workflow, cfg$task_type)
      guidance <- if (
        identical(cfg$workflow,"04") && identical(cfg$task_type,"manual_screening")
      ) {
        "Choose whether this batch is for reviewer-consistency testing or for building a human validation set. Consistency mode gives the same random records to every selected reviewer; validation-set mode splits random records between reviewers."
      } else if (identical(mode, ASSIGNMENT_MODES[["shared_work_pool"]])) {
        "Choose whether to split different unresolved records between reviewers, or give the same records to all selected reviewers. In the shared option, the first substantive decision resolves the record for everyone."
      } else if (identical(mode, ASSIGNMENT_MODES[["single_reviewer"]])) {
        "Add unresolved, currently unassigned cases. Each case is assigned to one reviewer only."
      } else {
        "Assign independent blinded reviews."
      }

      tags$details(
        class = "assignment-workflow mt-2",
        `data-accordion-key` = paste0("manage-", cfg$prefix, "-", cfg$task_type),
        tags$summary(tags$strong("Manage assignments")),
        div(
          class = "pt-2",
          if (
            identical(cfg$workflow,"04") && identical(cfg$task_type,"manual_screening")
          ) {
            radioButtons(
              "w04_review_mode",
              "Manual screening purpose",
              choices = c(
                "Reviewer consistency · same random records to every selected reviewer" = "reviewer_consistency",
                "Build validation set · split random records across selected reviewers" = "validation_set"
              ),
              selected = "reviewer_consistency"
            )
          },
          tags$p(class = "text-secondary small mb-2", guidance),
          selectInput(
            paste0(cfg$prefix, "_assignment_users"),
            "Reviewers",
            choices = eligible_choices,
            multiple = TRUE
          ),
          if (identical(mode, ASSIGNMENT_MODES[["shared_work_pool"]])) {
            radioButtons(
              paste0(cfg$prefix, "_assignment_strategy"),
              "Allocation pattern",
              choices = c(
                "Split different records between reviewers" = "split",
                "Same records to all reviewers · first decision wins" = "shared"
              ),
              selected = "split"
            )
          },
          radioButtons(
            paste0(cfg$prefix, "_assignment_type"),
            "Assign by",
            choices = c(
              "Number of cases" = "number",
              "Percentage" = "percentage",
              "All available" = "all"
            ),
            selected = "number",
            inline = TRUE
          ),
          numericInput(
            paste0(cfg$prefix, "_assignment_amount"),
            "Amount (ignored when All available is selected)",
            value = 1,
            min = 1,
            step = 1
          ),
          uiOutput(paste0(cfg$prefix, "_assignment_preview")),
          div(
            class = "d-flex align-items-center gap-2 mt-2",
            actionButton(
              paste0(cfg$prefix, "_apply_assignments"),
              "Apply assignments",
              class = "btn-primary btn-sm"
            ),
            tags$span(
              class = "saved-note",
              textOutput(paste0(cfg$prefix, "_assignment_status"), inline = TRUE)
            )
          ),
          tags$hr(),
          tags$strong("Remove unfinished assignments"),
          tags$p(
            class = "text-secondary small mb-2",
            paste0(
              "Choose a reviewer to remove all of their unfinished ",
              cfg$label,
              " assignments. Completed or already resolved cases are protected."
            )
          ),
          selectInput(
            paste0(cfg$prefix, "_remove_assignment_user"),
            "Reviewer",
            choices = remove_choices,
            selectize = FALSE
          ),
          actionButton(
            paste0(cfg$prefix, "_remove_assignments"),
            "Remove reviewer's unfinished assignments",
            class = "btn-outline-danger btn-sm"
          ),
        )
      )
    }

    workflow_sections <- lapply(group_progress, function(g) {
      z <- g$meta
      p <- g$progress
      mode <- assignment_mode_for(z$workflow, z$task_type)
      workflow_label <- workflow_labels_admin[[z$workflow]] %||% paste0("W", z$workflow)
      task_label <- task_labels[[z$task_type]] %||% z$task_type
      mode_label <- mode_labels[[mode]] %||% mode
      if (identical(z$workflow,"04") && identical(z$task_type,"manual_screening")) {
        active_w04 <- active_assignments_for_batch(
          assignment_registry_rv(),"04",z$batch_id,"manual_screening"
        )
        groups <- unique(vapply(
          active_w04,
          function(x) normalise_assignment_row(x)$blind_group,
          character(1)
        ))
        mode_label <- if ("w04-validation-set" %in% groups) {
          "Build validation set"
        } else if ("w04-reviewer-consistency" %in% groups) {
          "Reviewer consistency"
        } else {
          "Choose screening purpose"
        }
      }

      group_cases <- if (
        identical(z$workflow,"01") && identical(z$task_type,"deduplication") &&
        identical(z$batch_id,batch_id_rv())
      ) w01_all_cases_rv() else if (
        identical(z$workflow,"02") && identical(z$task_type,"enrichment") &&
        identical(z$batch_id,w02_batch_id_rv())
      ) w02_all_cases_rv() else if (
        identical(z$workflow,"04") && identical(z$task_type,"manual_screening") &&
        identical(z$batch_id,w04_batch_id_rv())
      ) w04_all_cases_rv() else if (
        identical(z$workflow,"04") && identical(z$task_type,"conflict_resolution") &&
        identical(z$batch_id,w04_active_conflict_batch_id())
      ) w04_all_conflict_cases() else if (
        identical(z$workflow,"08") && identical(z$task_type,"annotation") &&
        identical(z$batch_id,w08_batch_id_rv())
      ) w08_all_cases_rv() else list()

      all_case_ids <- if (length(group_cases)) {
        unique(vapply(
          group_cases,
          function(x) as.character(x$review_case_id %||% x$case_id %||% x$record_id %||% ""),
          character(1)
        ))
      } else character()
      active_group_assignments <- active_assignments(g$assignments)
      assigned_case_ids <- if (length(active_group_assignments)) unique(vapply(
        active_group_assignments,
        function(x) normalise_assignment_row(x)$case_id,
        character(1)
      )) else character()
      unassigned <- if (length(all_case_ids)) {
        sum(nzchar(all_case_ids) & !all_case_ids %in% assigned_case_ids)
      } else 0L

      rows <- lapply(p$by_user, function(x) {
        pct <- round(100 * x$progress)
        role_label <- if (identical(x$role, "administrator")) "Administrator" else "Reviewer"
        tags$tr(
          tags$td(x$display_name),
          tags$td(role_label),
          tags$td(x$assigned),
          tags$td(x$completed),
          tags$td(x$resolved_elsewhere),
          tags$td(x$remaining),
          tags$td(
            tags$span(
              class = "assignment-progress-bar",
              tags$span(class = "assignment-progress-fill", style = sprintf("width:%s%%", pct))
            ),
            tags$span(sprintf("%s%%", pct))
          ),
          tags$td(if (nzchar(x$last_activity)) x$last_activity else "No activity")
        )
      })

      mode_note <- if (identical(mode, ASSIGNMENT_MODES[["shared_work_pool"]])) {
        "For shared cases, the first valid decision resolves the case. Other reviewers assigned to that case no longer need to review it."
      } else if (identical(mode, ASSIGNMENT_MODES[["independent_blind_review"]])) {
        "Every required reviewer completes the case independently. Manual screeners never see one another's decisions; comparison occurs only in administrator reporting and assigned conflict adjudication after independent screening is complete."
      } else {
        "A single completed review resolves the assigned case."
      }

      tags$details(
        class = "assignment-workflow",
        `data-accordion-key` = paste0("workflow-", z$workflow, "-", z$task_type),
        tags$summary(
          div(
            class = "d-inline-flex flex-wrap align-items-center gap-2",
            tags$strong(paste0(workflow_label, " · ", task_label)),
            tags$span(class = "task-badge", mode_label),
            tags$span(
              class = "text-secondary small",
              if (identical(z$batch_id, "no-active-queue")) {
                "No active queue"
              } else {
                sprintf("%d cases · %d remaining assignments", p$cases, p$remaining)
              }
            )
          )
        ),
        div(
          class = "pt-2",
          if (
            identical(z$workflow, "08") &&
            identical(z$task_type, "annotation") &&
            grepl("^w08-test-assignment-smoke", z$batch_id) &&
            identical(as.integer(p$remaining), 0L)
          ) {
            div(
              class = "p-2 mb-2 border rounded bg-light",
              div(
                class = "d-flex flex-wrap align-items-center justify-content-between gap-2",
                div(
                  tags$strong("No outstanding W08 test assignments"),
                  tags$div(
                    class = "text-secondary small",
                    "Start a fresh four-record smoke-test batch for species, geography and topic adjudication."
                  )
                ),
                div(
                  class = "d-flex align-items-center gap-2",
                  actionButton(
                    "w08_start_fresh_test_batch",
                    "Start fresh W08 test batch",
                    class = "btn-primary btn-sm"
                  ),
                  tags$span(
                    class = "saved-note",
                    textOutput("w08_fresh_test_status", inline = TRUE)
                  )
                )
              )
            )
          },
          div(
            class = "assignment-kpis",
            div(class = "assignment-kpi", tags$span("Cases"), tags$strong(p$cases)),
            div(class = "assignment-kpi", tags$span("Assignments"), tags$strong(p$assigned)),
            div(class = "assignment-kpi", tags$span("Completed"), tags$strong(p$completed)),
            div(class = "assignment-kpi", tags$span("Closed"), tags$strong(p$resolved_elsewhere)),
            div(class = "assignment-kpi", tags$span("Outstanding"), tags$strong(p$remaining))
          ),
          if (unassigned > 0L) {
            tags$div(class = "small mb-2", paste0("Unassigned cases: ", unassigned))
          },
          div(
            class = "assignment-table-wrap",
            tags$table(
              class = "assignment-table",
              tags$thead(tags$tr(
                tags$th("Reviewer"),
                tags$th("Role"),
                tags$th("Assigned"),
                tags$th("Completed"),
                tags$th("Closed"),
                tags$th("Outstanding"),
                tags$th("Resolved"),
                tags$th("Last activity")
              )),
              tags$tbody(rows)
            )
          ),
          tags$div(class = "assignment-mode-note mt-2", mode_note),
          assignment_manager_ui(z)
        )
      )
    })

    fmt_agreement_pct <- function(x) {
      if (is.na(x)) "—" else sprintf("%.1f%%",100*x)
    }
    fmt_kappa <- function(x) {
      if (is.na(x)) "—" else sprintf("%.3f",x)
    }
    reviewer_name <- function(uid) {
      if (identical(uid,"model")) return("Model")
      u <- find_user_by_id(user_registry_rv(),uid,require_active=FALSE)
      if (is.null(u)) uid else u$display_name
    }

    w04_outcomes_now <- w04_blind_outcomes()
    available_raters <- w04_available_consistency_raters(w04_outcomes_now)
    rater_choices <- stats::setNames(
      available_raters,
      vapply(available_raters, reviewer_name, character(1))
    )
    existing_selected <- isolate(as.character(input$w04_consistency_raters %||% character()))
    selected_raters <- intersect(existing_selected, available_raters)
    if (length(selected_raters) < 2L) {
      human_defaults <- setdiff(available_raters,"model")
      selected_raters <- if (length(human_defaults) >= 2L) {
        human_defaults
      } else {
        available_raters
      }
    }
    w04_selected_analysis <- w04_consistency_analysis(
      w04_outcomes_now,
      selected_raters
    )

    pairwise_rows <- lapply(w04_selected_analysis$pairwise,function(x) {
      directional <- x$directional %||% list()
      tags$tr(
        tags$td(paste(reviewer_name(x$rater_a),"vs",reviewer_name(x$rater_b))),
        tags$td(x$n),
        tags$td(x$include_include),
        tags$td(x$exclude_exclude),
        tags$td(x$uncertain_uncertain),
        tags$td(
          paste0(
            reviewer_name(x$rater_a)," Include / ",reviewer_name(x$rater_b)," Exclude: ",
            directional$retain_exclude %||% 0L,
            "; reverse: ",
            directional$exclude_retain %||% 0L
          )
        ),
        tags$td(fmt_agreement_pct(x$agreement)),
        tags$td(fmt_kappa(x$kappa))
      )
    })

    pattern_rows <- lapply(w04_selected_analysis$patterns,function(x) {
      tags$tr(tags$td(x$pattern),tags$td(x$n))
    })

    w04_results_panel <- if (has_w04_batch) {
      tags$details(
        class="assignment-workflow mb-2",
        `data-accordion-key`="w04-consistency-checking",
        tags$summary(
          div(
            class="d-inline-flex flex-wrap align-items-center gap-2",
            tags$strong("Consistency checking"),
            tags$span(class="task-badge","Administrator only"),
            tags$span(
              class="text-secondary small",
              if (length(selected_raters) >= 2L) {
                sprintf(
                  "%d complete · %s agreement · %s = %s",
                  w04_selected_analysis$complete,
                  fmt_agreement_pct(w04_selected_analysis$raw_agreement),
                  w04_selected_analysis$metric,
                  fmt_kappa(w04_selected_analysis$kappa)
                )
              } else {
                "Select at least two raters"
              }
            )
          )
        ),
        div(
          class="pt-2",
          tags$p(
            class="text-secondary small",
            "Select any combination of completed human raters and the model. Statistics use complete cases for the selected raters only; no raw decisions are altered."
          ),
          checkboxGroupInput(
            "w04_consistency_raters",
            "Raters to compare",
            choices=rater_choices,
            selected=selected_raters,
            inline=TRUE
          ),
          if (length(selected_raters) < 2L) {
            tags$div(class="text-secondary small","At least two available raters are required.")
          } else {
            tagList(
              div(
                class="assignment-kpis",
                div(class="assignment-kpi",tags$span("Eligible records"),tags$strong(w04_selected_analysis$eligible)),
                div(class="assignment-kpi",tags$span("Complete cases"),tags$strong(w04_selected_analysis$complete)),
                div(class="assignment-kpi",tags$span("Missing"),tags$strong(w04_selected_analysis$missing)),
                div(class="assignment-kpi",tags$span("Agreements"),tags$strong(w04_selected_analysis$agreement_cases)),
                div(class="assignment-kpi",tags$span("Conflicts"),tags$strong(w04_selected_analysis$conflict_cases)),
                div(class="assignment-kpi",tags$span("Agreement"),tags$strong(fmt_agreement_pct(w04_selected_analysis$raw_agreement))),
                div(class="assignment-kpi",tags$span(w04_selected_analysis$metric),tags$strong(fmt_kappa(w04_selected_analysis$kappa)))
              ),
              if(length(pairwise_rows)) {
                tagList(
                  tags$strong("Pairwise detail"),
                  div(
                    class="assignment-table-wrap mb-2",
                    tags$table(
                      class="assignment-table",
                      tags$thead(tags$tr(
                        tags$th("Comparison"),
                        tags$th("N"),
                        tags$th("Include / include"),
                        tags$th("Exclude / exclude"),
                        tags$th("Unsure / unsure"),
                        tags$th("Directional include/exclude disagreement"),
                        tags$th("Agreement"),
                        tags$th("κ")
                      )),
                      tags$tbody(pairwise_rows)
                    )
                  )
                )
              },
              if(length(pattern_rows) && length(selected_raters) > 2L) {
                tagList(
                  tags$strong("Multi-rater decision patterns"),
                  tags$div(
                    class="text-secondary small mb-1",
                    "Rater order follows the selection shown above."
                  ),
                  div(
                    class="assignment-table-wrap mb-2",
                    tags$table(
                      class="assignment-table",
                      tags$thead(tags$tr(tags$th("Decision pattern"),tags$th("N"))),
                      tags$tbody(pattern_rows)
                    )
                  )
                )
              },
              div(
                class="d-flex align-items-center gap-2 mt-2",
                actionButton(
                  "w04_save_consistency_analysis",
                  "Save analysis",
                  class="btn-primary btn-sm"
                ),
                tags$span(
                  class="saved-note",
                  textOutput("w04_consistency_status",inline=TRUE)
                )
              ),
              uiOutput("w04_consistency_history"),
              tags$div(
                class="text-secondary small mt-2",
                sprintf(
                  "Current batch: %s · queue SHA: %s",
                  w04_batch_id_rv(),
                  substr(w04_queue_sha_rv(),1L,16L)
                )
              )
            )
          }
        )
      )
    } else NULL

    card(
      class = "assignment-summary",
      tags$details(
        class = "assignment-disclosure",
        `data-accordion-key` = "administration-assignments",
        tags$summary(
          div(
            class = "d-inline-flex flex-wrap align-items-center gap-2 p-3",
            tags$strong("Administration & assignments"),
            tags$span(class = "task-badge", paste0(length(group_progress), " workflow section", if (length(group_progress) == 1L) "" else "s")),
            tags$span(
              class = "text-secondary small",
              sprintf("%d assignments · %d resolved · %d remaining", total_assigned, total_completed + total_released, total_remaining)
            )
          )
        ),
        div(
          class = "px-3 pb-3",
          div(
            class = "assignment-kpis",
            div(class = "assignment-kpi", tags$span("Assignments"), tags$strong(total_assigned)),
            div(class = "assignment-kpi", tags$span("Completed"), tags$strong(total_completed)),
            div(class = "assignment-kpi", tags$span("Closed"), tags$strong(total_released)),
            div(class = "assignment-kpi", tags$span("Outstanding"), tags$strong(total_remaining)),
            div(class = "assignment-kpi", tags$span("Conflicts"), tags$strong(length(w04_all_conflict_cases())))
          ),
          if (!has_w02_batch || !has_w08_batch) {
            tags$details(
              class = "assignment-workflow mb-2",
              `data-accordion-key` = "test-queue-setup",
              tags$summary(tags$strong("Test queue setup")),
              div(
                class = "pt-2",
                tags$p(
                  class = "text-secondary small mb-2",
                  "Create small synthetic W02/W08 queues in this isolated Google Sheet for assignment smoke testing. Existing queue tabs are never overwritten."
                ),
                div(
                  class = "d-flex flex-wrap gap-2",
                  if (!has_w02_batch) actionButton(
                    "create_test_w02_queue",
                    "Create W02 test queue",
                    class = "btn-outline-secondary btn-sm"
                  ),
                  if (!has_w08_batch) actionButton(
                    "create_test_w08_queue",
                    "Create W08 test queue",
                    class = "btn-outline-secondary btn-sm"
                  )
                ),
                tags$div(
                  class = "saved-note mt-2",
                  textOutput("test_queue_status", inline = TRUE)
                )
              )
            )
          },
          w04_results_panel,
          workflow_sections
        )
      )
    )
  })

  w01_assignment_plan <- reactive({
    req(authenticated())
    if (!session_can("manage_assignments")) {
      return(list(error = "Administrator permission is required."))
    }
    selected <- as.character(input$w01_assignment_users %||% character())
    strategy <- as.character(input$w01_assignment_strategy %||% "split")
    type <- as.character(input$w01_assignment_type %||% "number")
    amount <- input$w01_assignment_amount %||% NA_real_

    tryCatch(
      plan_shared_pool_assignment(
        cases = w01_all_cases_rv(),
        assignments = assignment_registry_rv(),
        active_events = w01_active_assignment_events(),
        workflow = "01",
        batch_id = batch_id_rv(),
        task_type = "deduplication",
        user_ids = selected,
        allocation_type = type,
        amount = amount,
        allocation_strategy = strategy
      ),
      error = function(e) list(error = conditionMessage(e))
    )
  })

  output$w01_assignment_preview <- renderUI({
    req(authenticated())
    plan <- w01_assignment_plan()
    if (!is.null(plan$error)) {
      return(tags$div(class = "text-secondary small", plan$error))
    }

    selected <- as.character(input$w01_assignment_users %||% character())
    registry <- user_registry_rv()
    current_assignments <- active_assignments_for_batch(
      assignment_registry_rv(),
      "01",
      batch_id_rv(),
      "deduplication"
    )
    reviewer_lines <- lapply(selected, function(uid) {
      u <- find_user_by_id(registry, uid, require_active = FALSE)
      label <- if (is.null(u)) uid else u$display_name
      n_new <- as.integer(plan$by_user[[uid]] %||% 0L)
      current_for_user <- Filter(
        function(x) identical(normalise_assignment_row(x)$user_id, uid),
        current_assignments
      )
      current_unresolved <- sum(vapply(
        current_for_user,
        function(x) is.null(case_authoritative_event(w01_active_assignment_events(), normalise_assignment_row(x)$case_id)),
        logical(1)
      ))
      tags$li(sprintf(
        "%s: %d current + %d new = %d active case%s",
        label,
        current_unresolved,
        n_new,
        current_unresolved + n_new,
        if ((current_unresolved + n_new) == 1L) "" else "s"
      ))
    })

    div(
      class = "p-2 border rounded bg-light",
      tags$strong("Preview"),
      tags$div(
        class = "small",
        sprintf(
          "%d unresolved case%s available; %d case%s selected, creating %d assignment%s.",
          plan$available,
          if (plan$available == 1L) "" else "s",
          plan$selected_cases %||% length(plan$case_ids %||% character()),
          if ((plan$selected_cases %||% length(plan$case_ids %||% character())) == 1L) "" else "s",
          plan$allocated,
          if (plan$allocated == 1L) "" else "s"
        )
      ),
      if (length(reviewer_lines)) tags$ul(class = "small mb-0 mt-1", reviewer_lines)
    )
  })

  output$w01_assignment_status <- renderText(assignment_manage_status())

  observeEvent(input$w01_apply_assignments, {
    req(authenticated())
    if (!session_can("manage_assignments")) {
      assignment_manage_status("You do not have permission to manage assignments.")
      return()
    }
    plan <- w01_assignment_plan()
    if (!is.null(plan$error)) {
      assignment_manage_status(plan$error)
      return()
    }
    if (!length(plan$new_assignments)) {
      assignment_manage_status("No eligible cases to assign.")
      return()
    }

    updated <- c(assignment_registry_rv(), plan$new_assignments)
    persisted <- tryCatch(
      save_assignment_registry(
        updated,
        assignment_path,
        actor_user_id = session_reviewer_id(),
        expected_current_signature = assignment_registry_signature(assignment_registry_rv())
      ),
      error = function(e) {
        assignment_manage_status(paste("Assignment save failed:", conditionMessage(e)))
        NULL
      }
    )
    if (is.null(persisted)) return()

    assignment_registry_rv(persisted)
    assignment_manage_status(sprintf(
      "Added %d new case%s.",
      plan$allocated,
      if (plan$allocated == 1L) "" else "s"
    ))
  })

  observeEvent(input$w01_remove_assignments, {
    req(authenticated())
    if (!session_can("manage_assignments")) {
      assignment_manage_status("You do not have permission to manage assignments.")
      return()
    }
    selected_user <- as.character(input$w01_remove_assignment_user %||% "")
    result <- tryCatch(
      cancel_user_assignments(
        assignment_registry_rv(),
        selected_user,
        w01_active_assignment_events(),
        "01",
        batch_id_rv(),
        "deduplication"
      ),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      assignment_manage_status(conditionMessage(result))
      return()
    }

    persisted <- tryCatch(
      save_assignment_registry(
        result$assignments,
        assignment_path,
        actor_user_id = session_reviewer_id(),
        expected_current_signature = assignment_registry_signature(assignment_registry_rv())
      ),
      error = function(e) {
        assignment_manage_status(paste("Assignment removal failed:", conditionMessage(e)))
        NULL
      }
    )
    if (is.null(persisted)) return()

    assignment_registry_rv(persisted)
    assignment_manage_status(sprintf(
      "Removed %d unfinished assignment%s.",
      result$cancelled,
      if (result$cancelled == 1L) "" else "s"
    ))
  })


  workflow_assignment_plan <- function(prefix, cases, events, workflow, batch_id, task_type) {
    selected <- as.character(input[[paste0(prefix, "_assignment_users")]] %||% character())
    strategy <- as.character(input[[paste0(prefix, "_assignment_strategy")]] %||% "split")
    type <- as.character(input[[paste0(prefix, "_assignment_type")]] %||% "number")
    amount <- input[[paste0(prefix, "_assignment_amount")]] %||% NA_real_
    tryCatch(
      plan_workflow_assignment(
        cases = cases,
        assignments = assignment_registry_rv(),
        active_events = events,
        workflow = workflow,
        batch_id = batch_id,
        task_type = task_type,
        user_ids = selected,
        allocation_type = type,
        amount = amount,
        allocation_strategy = strategy
      ),
      error = function(e) list(error = conditionMessage(e))
    )
  }

  workflow_assignment_preview <- function(plan, prefix, workflow, batch_id, task_type, events) {
    if (!is.null(plan$error)) {
      return(tags$div(class = "text-secondary small", plan$error))
    }
    selected <- as.character(input[[paste0(prefix, "_assignment_users")]] %||% character())
    registry <- user_registry_rv()
    current_assignments <- active_assignments_for_batch(
      assignment_registry_rv(), workflow, batch_id, task_type
    )
    reviewer_lines <- lapply(selected, function(uid) {
      u <- find_user_by_id(registry, uid, require_active = FALSE)
      label <- if (is.null(u)) uid else u$display_name
      n_new <- as.integer(plan$by_user[[uid]] %||% 0L)
      current_for_user <- Filter(
        function(x) identical(normalise_assignment_row(x)$user_id, uid),
        current_assignments
      )
      current_unresolved <- sum(vapply(
        current_for_user,
        function(x) is.null(case_authoritative_event(events, normalise_assignment_row(x)$case_id)),
        logical(1)
      ))
      tags$li(sprintf(
        "%s: %d current + %d new = %d active case%s",
        label,
        current_unresolved,
        n_new,
        current_unresolved + n_new,
        if ((current_unresolved + n_new) == 1L) "" else "s"
      ))
    })
    div(
      class = "p-2 border rounded bg-light",
      tags$strong("Preview"),
      tags$div(
        class = "small",
        sprintf(
          "%d unresolved case%s available; %d case%s selected, creating %d assignment%s.",
          plan$available,
          if (plan$available == 1L) "" else "s",
          plan$selected_cases %||% length(plan$case_ids %||% character()),
          if ((plan$selected_cases %||% length(plan$case_ids %||% character())) == 1L) "" else "s",
          plan$allocated,
          if (plan$allocated == 1L) "" else "s"
        )
      ),
      if (length(reviewer_lines)) tags$ul(class = "small mb-0 mt-1", reviewer_lines)
    )
  }

  apply_workflow_assignments <- function(plan) {
    if (!session_can("manage_assignments")) {
      assignment_manage_status("You do not have permission to manage assignments.")
      return(invisible(FALSE))
    }
    if (!is.null(plan$error)) {
      assignment_manage_status(plan$error)
      return(invisible(FALSE))
    }
    if (!length(plan$new_assignments)) {
      assignment_manage_status("No eligible cases to assign.")
      return(invisible(FALSE))
    }
    updated <- c(assignment_registry_rv(), plan$new_assignments)
    persisted <- tryCatch(
      save_assignment_registry(
        updated,
        assignment_path,
        actor_user_id = session_reviewer_id(),
        expected_current_signature = assignment_registry_signature(assignment_registry_rv())
      ),
      error = function(e) {
        assignment_manage_status(paste("Assignment save failed:", conditionMessage(e)))
        NULL
      }
    )
    if (is.null(persisted)) return(invisible(FALSE))
    assignment_registry_rv(persisted)
    assignment_manage_status(sprintf(
      "Added %d new case%s.",
      plan$allocated,
      if (plan$allocated == 1L) "" else "s"
    ))
    invisible(TRUE)
  }

  remove_workflow_user_assignments <- function(user_id, events, workflow, batch_id, task_type) {
    if (!session_can("manage_assignments")) {
      assignment_manage_status("You do not have permission to manage assignments.")
      return(invisible(FALSE))
    }
    result <- tryCatch(
      cancel_user_assignments(
        assignment_registry_rv(),
        as.character(user_id %||% ""),
        events,
        workflow,
        batch_id,
        task_type
      ),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      assignment_manage_status(conditionMessage(result))
      return(invisible(FALSE))
    }
    persisted <- tryCatch(
      save_assignment_registry(
        result$assignments,
        assignment_path,
        actor_user_id = session_reviewer_id(),
        expected_current_signature = assignment_registry_signature(assignment_registry_rv())
      ),
      error = function(e) {
        assignment_manage_status(paste("Assignment removal failed:", conditionMessage(e)))
        NULL
      }
    )
    if (is.null(persisted)) return(invisible(FALSE))
    assignment_registry_rv(persisted)
    assignment_manage_status(sprintf(
      "Removed %d unfinished assignment%s.",
      result$cancelled,
      if (result$cancelled == 1L) "" else "s"
    ))
    invisible(TRUE)
  }

  ensure_direct_assignment <- function(
    workflow,
    batch_id,
    task_type,
    case_obj,
    case_id,
    events
  ) {
    uid <- session_reviewer_id()
    if (!nzchar(uid)) return(invisible(FALSE))

    if (user_has_active_assignment(
      assignment_registry_rv(),
      workflow,
      batch_id,
      task_type,
      case_id,
      uid
    )) {
      return(invisible(TRUE))
    }

    plan <- tryCatch(
      plan_shared_pool_assignment(
        cases = list(case_obj),
        assignments = assignment_registry_rv(),
        active_events = events,
        workflow = workflow,
        batch_id = batch_id,
        task_type = task_type,
        user_ids = uid,
        allocation_type = "all",
        allocation_strategy = "shared"
      ),
      error = function(e) e
    )
    if (inherits(plan, "error") || !length(plan$new_assignments)) {
      return(invisible(FALSE))
    }

    persisted <- tryCatch(
      save_assignment_registry(
        c(assignment_registry_rv(), plan$new_assignments),
        assignment_path,
        actor_user_id = uid,
        expected_current_signature = assignment_registry_signature(assignment_registry_rv())
      ),
      error = function(e) NULL
    )
    if (is.null(persisted)) return(invisible(FALSE))

    assignment_registry_rv(persisted)
    invisible(TRUE)
  }

  w02_assignment_plan <- reactive({
    req(authenticated())
    workflow_assignment_plan(
      "w02", w02_all_cases_rv(), w02_active_assignment_events(),
      "02", w02_batch_id_rv(), "enrichment"
    )
  })
  output$w02_assignment_preview <- renderUI({
    req(authenticated())
    workflow_assignment_preview(
      w02_assignment_plan(), "w02", "02", w02_batch_id_rv(),
      "enrichment", w02_active_assignment_events()
    )
  })
  output$w02_assignment_status <- renderText(assignment_manage_status())
  observeEvent(input$w02_apply_assignments, {
    apply_workflow_assignments(w02_assignment_plan())
  })
  observeEvent(input$w02_remove_assignments, {
    remove_workflow_user_assignments(
      input$w02_remove_assignment_user,
      w02_active_assignment_events(),
      "02",
      w02_batch_id_rv(),
      "enrichment"
    )
  })

  output$w04_consistency_status <- renderText(w04_consistency_status())

  observe({
    req(authenticated())
    if (!session_can("manage_assignments")) return()
    if (isTRUE(w04_consistency_history_loaded())) return()
    if (!identical(storage_backend(),"google_sheets")) {
      w04_consistency_history_loaded(TRUE)
      return()
    }
    loaded <- tryCatch(
      list(
        analyses=read_w04_consistency_analyses(),
        conflict_sets=read_w04_conflict_sets()
      ),
      error=function(e)e
    )
    if (inherits(loaded,"error")) {
      w04_consistency_status(paste("Could not load saved W04 analysis history:",conditionMessage(loaded)))
      w04_consistency_history_loaded(TRUE)
      return()
    }
    w04_consistency_analyses_rv(loaded$analyses)
    w04_conflict_sets_rv(loaded$conflict_sets)

    current_sets <- Filter(
      function(x) identical(
        as.character(x$parent_batch_id %||% ""),
        as.character(w04_batch_id_rv())
      ),
      loaded$conflict_sets
    )
    if (length(current_sets)) {
      active_set <- current_sets[[length(current_sets)]]
      all_conf <- tryCatch(active_sheet_w04_conflict_decisions(),error=function(e)list())
      set_decisions <- w04_filter_batch_decisions(
        all_conf,
        as.character(active_set$conflict_queue_sha256 %||% "")
      )
      w04_conflict_decisions(set_decisions)
      w04_conflict_idx(1L)
    }
    w04_consistency_history_loaded(TRUE)
  })

  output$w04_consistency_history <- renderUI({
    req(authenticated())
    if (!session_can("manage_assignments")) return(NULL)
    xs <- w04_consistency_analyses_rv() %||% list()
    if (length(xs)) {
      xs <- Filter(
        function(x) identical(as.character(x$batch_id %||% ""),as.character(w04_batch_id_rv())),
        xs
      )
    }
    if (!length(xs)) {
      return(tags$div(class="text-secondary small mt-2","No saved analyses for this batch yet."))
    }
    xs <- rev(xs)
    xs <- xs[seq_len(min(length(xs),5L))]
    rows <- lapply(xs,function(x) {
      raters <- tryCatch(
        as.character(jsonlite::fromJSON(as.character(x$rater_ids_json %||% "[]"))),
        error=function(e) character()
      )
      labels <- if(length(raters)) vapply(raters,function(uid) {
        if(identical(uid,"model")) return("Model")
        u <- find_user_by_id(user_registry_rv(),uid,require_active=FALSE)
        if(is.null(u)) uid else u$display_name
      },character(1)) else character()
      raw <- suppressWarnings(as.numeric(as.character(x$raw_agreement %||% NA_character_)))
      kap <- suppressWarnings(as.numeric(as.character(x$kappa %||% NA_character_)))
      tags$tr(
        tags$td(as.character(x$analysis_id %||% "")),
        tags$td(paste(labels,collapse=", ")),
        tags$td(as.character(x$complete_n %||% "0")),
        tags$td(if(is.na(raw))"—" else sprintf("%.1f%%",100*raw)),
        tags$td(as.character(x$metric %||% "")),
        tags$td(if(is.na(kap))"—" else sprintf("%.3f",kap)),
        tags$td(as.character(x$created_at_utc %||% ""))
      )
    })
    tagList(
      tags$strong("Saved analyses"),
      div(
        class="assignment-table-wrap mt-1",
        tags$table(
          class="assignment-table",
          tags$thead(tags$tr(
            tags$th("Analysis ID"),tags$th("Raters"),tags$th("N"),
            tags$th("Agreement"),tags$th("Metric"),tags$th("κ"),tags$th("Saved")
          )),
          tags$tbody(rows)
        )
      ),
      {
        all_saved <- Filter(
          function(x) identical(
            as.character(x$batch_id %||% ""),
            as.character(w04_batch_id_rv())
          ),
          w04_consistency_analyses_rv() %||% list()
        )
        conflict_ready <- Filter(
          function(x) {
            n <- suppressWarnings(as.integer(as.character(x$conflict_n %||% "0")))
            !is.na(n) && n > 0L
          },
          all_saved
        )
        if (length(conflict_ready)) {
          ids <- vapply(conflict_ready,function(x)as.character(x$analysis_id %||% ""),character(1))
          labels <- vapply(conflict_ready,function(x) {
            sprintf(
              "%s · %s conflict%s",
              as.character(x$analysis_id %||% ""),
              as.character(x$conflict_n %||% "0"),
              if (identical(as.character(x$conflict_n %||% "0"),"1")) "" else "s"
            )
          },character(1))
          tagList(
            tags$hr(),
            tags$strong("Conflict resolution"),
            tags$p(
              class="text-secondary small mb-2",
              "Create a conflict set from one saved analysis. The exact raters and conflicting records are preserved as provenance."
            ),
            selectInput(
              "w04_conflict_analysis_id",
              "Saved analysis",
              choices=stats::setNames(ids,labels),
              selected=ids[[length(ids)]]
            ),
            actionButton(
              "w04_create_conflict_set",
              "Create conflict set",
              class="btn-outline-primary btn-sm"
            ),
            {
              active_set <- w04_active_consistency_conflict_set()
              if (!is.null(active_set)) {
                tags$div(
                  class="text-secondary small mt-2",
                  sprintf(
                    "Current conflict set: %s · source analysis: %s",
                    as.character(active_set$conflict_set_id %||% ""),
                    as.character(active_set$analysis_id %||% "")
                  )
                )
              }
            }
          )
        } else {
          tags$div(
            class="text-secondary small mt-2",
            "No saved analysis currently contains conflicts."
          )
        }
      }
    )
  })

  observeEvent(input$w04_save_consistency_analysis,{
    req(authenticated())
    if (!session_can("manage_assignments")) {
      w04_consistency_status("Administrator permission is required.")
      return()
    }
    rater_ids <- unique(as.character(input$w04_consistency_raters %||% character()))
    if (length(rater_ids) < 2L) {
      w04_consistency_status("Select at least two raters before saving.")
      return()
    }
    analysis <- w04_consistency_analysis(w04_blind_outcomes(),rater_ids)
    if (analysis$complete < 1L) {
      w04_consistency_status("There are no complete cases for the selected raters.")
      return()
    }
    if (!identical(storage_backend(),"google_sheets")) {
      w04_consistency_status("Saving consistency analyses is available with the Google Sheets backend.")
      return()
    }

    batch_assignments <- active_assignments_for_batch(
      assignment_registry_rv(),"04",w04_batch_id_rv(),"manual_screening"
    )
    groups <- unique(vapply(
      batch_assignments,
      function(x) normalise_assignment_row(x)$blind_group,
      character(1)
    ))
    review_mode <- if ("w04-reviewer-consistency" %in% groups) {
      "reviewer_consistency"
    } else if ("w04-validation-set" %in% groups) {
      "validation_set"
    } else {
      ""
    }

    saved <- tryCatch(
      append_w04_consistency_analysis(
        analysis=analysis,
        batch_id=w04_batch_id_rv(),
        queue_sha256=w04_queue_sha_rv(),
        review_mode=review_mode,
        created_by=session_reviewer_id()
      ),
      error=function(e)e
    )
    if (inherits(saved,"error")) {
      w04_consistency_status(paste("Save failed:",conditionMessage(saved)))
      return()
    }
    w04_consistency_analyses_rv(c(w04_consistency_analyses_rv(),list(saved)))
    w04_consistency_status(paste("Saved",as.character(saved$analysis_id %||% "analysis")))
  })

  observeEvent(input$w04_create_conflict_set,{
    req(authenticated())
    if (!session_can("manage_assignments")) {
      w04_consistency_status("Administrator permission is required.")
      return()
    }
    analysis_id <- as.character(input$w04_conflict_analysis_id %||% "")
    if (!nzchar(analysis_id)) {
      w04_consistency_status("Choose a saved analysis first.")
      return()
    }
    hits <- Filter(
      function(x) identical(as.character(x$analysis_id %||% ""),analysis_id),
      w04_consistency_analyses_rv() %||% list()
    )
    if (!length(hits)) {
      w04_consistency_status("The selected saved analysis could not be found.")
      return()
    }
    saved_set <- tryCatch(
      append_w04_conflict_set(hits[[length(hits)]],created_by=session_reviewer_id()),
      error=function(e)e
    )
    if (inherits(saved_set,"error")) {
      w04_consistency_status(paste("Conflict-set creation failed:",conditionMessage(saved_set)))
      return()
    }
    existing_ids <- vapply(
      w04_conflict_sets_rv() %||% list(),
      function(x)as.character(x$conflict_set_id %||% ""),
      character(1)
    )
    if (!as.character(saved_set$conflict_set_id %||% "") %in% existing_ids) {
      w04_conflict_sets_rv(c(w04_conflict_sets_rv(),list(saved_set)))
    }

    all_conf <- tryCatch(active_sheet_w04_conflict_decisions(),error=function(e)list())
    w04_conflict_decisions(w04_filter_batch_decisions(
      all_conf,
      as.character(saved_set$conflict_queue_sha256 %||% "")
    ))
    w04_conflict_idx(1L)
    w04_consistency_status(sprintf(
      "Created conflict set %s from %s. Assign its cases in the W04 conflict-resolution section below.",
      as.character(saved_set$conflict_set_id %||% ""),
      analysis_id
    ))
  })

  w04_assignment_plan <- reactive({
    req(authenticated())
    selected <- as.character(input$w04_assignment_users %||% character())
    type <- as.character(input$w04_assignment_type %||% "number")
    amount <- input$w04_assignment_amount %||% NA_real_
    review_mode <- as.character(input$w04_review_mode %||% "reviewer_consistency")
    tryCatch(
      plan_w04_manual_assignment(
        cases = w04_all_cases_rv(),
        assignments = assignment_registry_rv(),
        active_events = w04_active_assignment_events(),
        batch_id = w04_batch_id_rv(),
        user_ids = selected,
        review_mode = review_mode,
        allocation_type = type,
        amount = amount
      ),
      error=function(e) list(error=conditionMessage(e))
    )
  })
  output$w04_assignment_preview <- renderUI({
    req(authenticated())
    workflow_assignment_preview(
      w04_assignment_plan(), "w04", "04", w04_batch_id_rv(),
      "manual_screening", w04_active_assignment_events()
    )
  })
  output$w04_assignment_status <- renderText(assignment_manage_status())
  observeEvent(input$w04_apply_assignments, {
    apply_workflow_assignments(w04_assignment_plan())
  })
  observeEvent(input$w04_remove_assignments, {
    remove_workflow_user_assignments(
      input$w04_remove_assignment_user,
      w04_active_assignment_events(),
      "04",
      w04_batch_id_rv(),
      "manual_screening"
    )
  })

  w04conflict_assignment_plan <- reactive({
    req(authenticated())
    workflow_assignment_plan(
      "w04conflict", w04_all_conflict_cases(), w04_conflict_decisions(),
      "04", w04_active_conflict_batch_id(), "conflict_resolution"
    )
  })
  output$w04conflict_assignment_preview <- renderUI({
    req(authenticated())
    workflow_assignment_preview(
      w04conflict_assignment_plan(), "w04conflict", "04", w04_active_conflict_batch_id(),
      "conflict_resolution", w04_conflict_decisions()
    )
  })
  output$w04conflict_assignment_status <- renderText(assignment_manage_status())
  observeEvent(input$w04conflict_apply_assignments, {
    apply_workflow_assignments(w04conflict_assignment_plan())
  })
  observeEvent(input$w04conflict_remove_assignments, {
    remove_workflow_user_assignments(
      input$w04conflict_remove_assignment_user,
      w04_conflict_decisions(),
      "04",
      w04_active_conflict_batch_id(),
      "conflict_resolution"
    )
  })

  w08_assignment_plan <- reactive({
    req(authenticated())
    workflow_assignment_plan(
      "w08", w08_all_cases_rv(), w08_active_assignment_events(),
      "08", w08_batch_id_rv(), "annotation"
    )
  })
  output$w08_assignment_preview <- renderUI({
    req(authenticated())
    workflow_assignment_preview(
      w08_assignment_plan(), "w08", "08", w08_batch_id_rv(),
      "annotation", w08_active_assignment_events()
    )
  })
  output$w08_assignment_status <- renderText(assignment_manage_status())
  observeEvent(input$w08_apply_assignments, {
    apply_workflow_assignments(w08_assignment_plan())
  })
  observeEvent(input$w08_remove_assignments, {
    remove_workflow_user_assignments(
      input$w08_remove_assignment_user,
      w08_active_assignment_events(),
      "08",
      w08_batch_id_rv(),
      "annotation"
    )
  })

  output$w08_fresh_test_status <- renderText(w08_fresh_test_status())

  observeEvent(input$w08_start_fresh_test_batch, {
    req(authenticated())
    if (!session_can("manage_assignments")) {
      w08_fresh_test_status("Administrator permission is required.")
      return()
    }

    made <- tryCatch(start_fresh_test_w08_queue(), error = function(e) e)
    if (inherits(made, "error")) {
      w08_fresh_test_status(paste("Fresh W08 test batch failed:", conditionMessage(made)))
      return()
    }

    loaded <- tryCatch(load_w08_batch(), error = function(e) e)
    if (inherits(loaded, "error") || is.null(loaded)) {
      w08_fresh_test_status("Fresh W08 test batch was written but could not be loaded.")
      return()
    }

    all_decisions <- tryCatch(active_sheet_w08_decisions(), error = function(e) list())
    batch_decisions <- w08_filter_batch_decisions(all_decisions, loaded$queue_sha256)

    # Fresh synthetic W08 batches are for immediate smoke testing. Assign all
    # new cases to the administrator who created the batch so the review screen
    # can be opened without a second manual assignment step.
    test_plan <- tryCatch(
      plan_workflow_assignment(
        cases = loaded$cases,
        assignments = assignment_registry_rv(),
        active_events = batch_decisions,
        workflow = "08",
        batch_id = loaded$batch_id,
        task_type = "annotation",
        user_ids = session_reviewer_id(),
        allocation_type = "all",
        allocation_strategy = "split"
      ),
      error = function(e) e
    )
    if (inherits(test_plan, "error") || !length(test_plan$new_assignments)) {
      msg <- if (inherits(test_plan, "error")) {
        paste("Fresh W08 test batch was created, but automatic assignment failed:", conditionMessage(test_plan))
      } else {
        "Fresh W08 test batch was created, but no test assignments could be created."
      }
      w08_fresh_test_status(msg)
      return()
    }

    persisted_assignments <- tryCatch(
      save_assignment_registry(
        c(assignment_registry_rv(), test_plan$new_assignments),
        assignment_path,
        actor_user_id = session_reviewer_id(),
        expected_current_signature = assignment_registry_signature(assignment_registry_rv())
      ),
      error = function(e) e
    )
    if (inherits(persisted_assignments, "error")) {
      w08_fresh_test_status(paste(
        "Fresh W08 test batch was created, but automatic assignment could not be saved:",
        conditionMessage(persisted_assignments)
      ))
      return()
    }
    assignment_registry_rv(persisted_assignments)

    if (length(loaded$cases) != 4L) {
      w08_fresh_test_status(sprintf(
        "Fresh W08 test batch was created, but %d records were loaded instead of 4.",
        length(loaded$cases)
      ))
      return()
    }

    visible_test_cases <- cases_for_assignment_user(
      loaded$cases,
      assignment_registry_rv(),
      "08",
      loaded$batch_id,
      current_user(),
      task_type = "annotation",
      active_events = batch_decisions
    )
    if (length(visible_test_cases) != 4L) {
      w08_fresh_test_status(sprintf(
        "Fresh W08 test batch was created, but only %d of 4 records are reviewable by the current administrator.",
        length(visible_test_cases)
      ))
      return()
    }

    w08_all_cases_rv(loaded$cases)
    w08_cases_rv(visible_test_cases)
    w08_queue_sha_rv(loaded$queue_sha256)
    w08_batch_id_rv(loaded$batch_id)
    w08_source_run_id_rv(loaded$source_run_id %||% "")
    w08_batch_status_rv(loaded$batch_status %||% "")
    w08_case_sha_rv(loaded$case_sha256 %||% character())
    w08_species_options(loaded$species_options %||% character())
    w08_topic_options(loaded$topic_options %||% list())
    w08_decisions(batch_decisions)
    w08_idx(1L)
    w08_fresh_test_status("Fresh W08 test batch created; all 4 records are assigned to you and ready for review.")
    app_view("w08")
  })

  output$test_queue_status <- renderText(test_queue_status())
  output$test_queue_status_w02 <- renderText(test_queue_status())
  output$test_queue_status_w04 <- renderText(test_queue_status())
  output$test_queue_status_w08 <- renderText(test_queue_status())

  create_and_load_w02_test_queue <- function() {
    req(authenticated())
    if (!session_can("manage_assignments")) {
      test_queue_status("Administrator permission is required.")
      return(invisible(FALSE))
    }

    made <- tryCatch({
      create_test_w02_queue()
      TRUE
    }, error = function(e) {
      msg <- paste("W02 test queue could not be created:", conditionMessage(e))
      test_queue_status(msg)
      showNotification(msg, type = "error", duration = NULL)
      FALSE
    })
    if (!isTRUE(made)) return(invisible(FALSE))

    loaded <- tryCatch(load_w02_batch(), error = function(e) e)
    if (inherits(loaded, "error") || is.null(loaded)) {
      msg <- if (inherits(loaded, "error")) {
        paste("W02 queue could not be loaded:", conditionMessage(loaded))
      } else {
        "W02 queue was created but could not be loaded."
      }
      test_queue_status(msg)
      showNotification(msg, type = "error", duration = NULL)
      return(invisible(FALSE))
    }

    all_decisions <- tryCatch(active_sheet_w02_decisions(), error = function(e) list())
    batch_decisions <- w02_filter_batch_decisions(all_decisions, loaded$queue_sha256)
    w02_all_cases_rv(loaded$cases)
    visible <- cases_for_assignment_user(
      loaded$cases,
      assignment_registry_rv(),
      "02",
      loaded$batch_id,
      current_user(),
      task_type = "enrichment",
      active_events = batch_decisions
    )
    w02_cases_rv(visible)
    w02_queue_sha_rv(loaded$queue_sha256)
    w02_batch_id_rv(loaded$batch_id)
    w02_batch_status_rv(loaded$batch_status %||% "")
    w02_decisions(batch_decisions)
    w02_idx(1L)
    test_queue_status("W02 test queue created and loaded.")
    showNotification("W02 test queue created and loaded.", type = "message")
    invisible(TRUE)
  }

  create_and_load_w04_test_queue <- function() {
    req(authenticated())
    if (!session_can("manage_assignments")) {
      test_queue_status("Administrator permission is required.")
      return(invisible(FALSE))
    }

    made <- tryCatch(
      create_test_w04_queue(),
      error=function(e)e
    )
    if (inherits(made,"error")) {
      msg <- paste("W04 test queue could not be created:",conditionMessage(made))
      test_queue_status(msg)
      showNotification(msg,type="error",duration=NULL)
      return(invisible(FALSE))
    }

    tab <- as.character(made$tab %||% Sys.getenv(
      "LEM_W04_TEST_QUEUE_TAB",
      unset="queue_w04_test_active"
    ))
    loaded <- tryCatch(
      read_sheet_w04_queue_from_tab(tab),
      error=function(e)e
    )
    if (inherits(loaded,"error") || is.null(loaded)) {
      msg <- if (inherits(loaded,"error")) {
        paste("W04 test queue could not be loaded:",conditionMessage(loaded))
      } else {
        "W04 test queue was created but could not be loaded."
      }
      test_queue_status(msg)
      showNotification(msg,type="error",duration=NULL)
      return(invisible(FALSE))
    }

    all_decisions <- tryCatch(active_sheet_w04_decisions(),error=function(e)list())
    batch_decisions <- w04_filter_batch_decisions(all_decisions,loaded$queue_sha256)
    w04_all_cases_rv(loaded$cases)
    w04_cases_rv(list())
    w04_queue_sha_rv(loaded$queue_sha256)
    w04_batch_id_rv(loaded$batch_id)
    w04_batch_status_rv(loaded$batch_status %||% "")
    w04_include_terms(loaded$highlight_include %||% character())
    w04_exclude_terms(loaded$highlight_exclude %||% character())
    w04_decisions(batch_decisions)
    w04_idx(1L)
    w04_consistency_history_loaded(FALSE)
    test_queue_status(
      "W04 test queue created and loaded. Open Manage assignments to choose Reviewer consistency or Build validation set."
    )
    showNotification("W04 test queue created and loaded.",type="message")
    invisible(TRUE)
  }

  create_and_load_w08_test_queue <- function() {
    req(authenticated())
    if (!session_can("manage_assignments")) {
      test_queue_status("Administrator permission is required.")
      return(invisible(FALSE))
    }

    made <- tryCatch({
      create_test_w08_queue()
      TRUE
    }, error = function(e) {
      msg <- paste("W08 test queue could not be created:", conditionMessage(e))
      test_queue_status(msg)
      showNotification(msg, type = "error", duration = NULL)
      FALSE
    })
    if (!isTRUE(made)) return(invisible(FALSE))

    loaded <- tryCatch(load_w08_batch(), error = function(e) e)
    if (inherits(loaded, "error") || is.null(loaded)) {
      msg <- if (inherits(loaded, "error")) {
        paste("W08 queue could not be loaded:", conditionMessage(loaded))
      } else {
        "W08 queue was created but could not be loaded."
      }
      test_queue_status(msg)
      showNotification(msg, type = "error", duration = NULL)
      return(invisible(FALSE))
    }

    all_decisions <- tryCatch(active_sheet_w08_decisions(), error = function(e) list())
    batch_decisions <- w08_filter_batch_decisions(all_decisions, loaded$queue_sha256)
    w08_all_cases_rv(loaded$cases)
    visible <- cases_for_assignment_user(
      loaded$cases,
      assignment_registry_rv(),
      "08",
      loaded$batch_id,
      current_user(),
      task_type = "annotation",
      active_events = batch_decisions
    )
    w08_cases_rv(visible)
    w08_queue_sha_rv(loaded$queue_sha256)
    w08_batch_id_rv(loaded$batch_id)
    w08_source_run_id_rv(loaded$source_run_id %||% "")
    w08_batch_status_rv(loaded$batch_status %||% "")
    w08_case_sha_rv(loaded$case_sha256 %||% character())
    species_opts <- loaded$species_options %||% character()
    if (
      !length(species_opts) &&
      identical(as.character(loaded$batch_id %||% ""), "w08-test-assignment-smoke")
    ) {
      species_opts <- c(
        "Atlantic salmon",
        "Rainbow trout",
        "Chinook salmon",
        "Coho salmon",
        "Sockeye salmon",
        "Chum salmon",
        "Pink salmon",
        "Masu salmon",
        "Unspecified species"
      )
    }
    w08_species_options(species_opts)
    w08_topic_options(loaded$topic_options %||% list())
    w08_decisions(batch_decisions)
    w08_idx(1L)
    test_queue_status("W08 test queue created and loaded.")
    showNotification("W08 test queue created and loaded.", type = "message")
    invisible(TRUE)
  }

  observeEvent(input$create_test_w02_queue, {
    create_and_load_w02_test_queue()
  })
  observeEvent(input$create_test_w02_queue_inline, {
    create_and_load_w02_test_queue()
  })

  observeEvent(input$create_test_w04_queue_inline, {
    create_and_load_w04_test_queue()
  })

  observeEvent(input$create_test_w08_queue, {
    create_and_load_w08_test_queue()
  })
  observeEvent(input$create_test_w08_queue_inline, {
    create_and_load_w08_test_queue()
  })

  # Legacy observer bodies replaced by the shared helpers below.
  observeEvent(input$logout, {
    authenticated(FALSE)
    current_user(NULL)
    user_registry_rv(list())
    assignment_registry_rv(list())
    assignment_manage_status("")
    test_queue_status("")
    w01_all_cases_rv(list())
    w02_all_cases_rv(list())
    w04_all_cases_rv(list())
    w08_all_cases_rv(list())
    app_view("tasks")
    complete(FALSE)
    failed_attempts(0L)
    lock_until(as.POSIXct(NA))
    login_status("")
  })

  observeEvent(input$login, {
    now <- Sys.time()
    until <- lock_until()
    if (!is.na(until) && now < until) {
      login_status("Too many failed attempts. Try again shortly.")
      return()
    }
    login_user <- tryCatch({
      if (!individual_auth_configured()) {
        stop(
          "Individual authentication is not configured. Check LEM_INITIAL_USERS_JSON and LEM_USER_ACCESS_KEY_HASHES_JSON in Connect Cloud.",
          call. = FALSE
        )
      }
      registry <- read_user_registry()
      authenticate_registered_user(registry, input$login_email, input$access_key)
    }, error = function(e) {
      login_status(paste("Login configuration error:", conditionMessage(e)))
      NULL
    })

    if (!is.null(login_user)) {
      loaded <- tryCatch({
        user_registry_rv(registry)
        loaded_assignments <- read_assignment_registry(assignment_path)
        if (identical(storage_backend(), "local")) {
          loaded_assignments <- resolve_fixture_assignment_users(loaded_assignments, registry)
        }
        assignment_registry_rv(loaded_assignments)
        pipeline_status_rv(if(identical(storage_backend(),"google_sheets")) read_latest_pipeline_status() else NULL)
        manual_screening_rv(tryCatch(read_manual_screening_metrics(),error=function(e)NULL))
        batch <- load_batch()
        current_decisions <- list()
        if (!is.null(batch)) {
          all_decisions <- read_active_decisions(decision_path)
          current_decisions <- filter_batch_decisions(all_decisions, batch$queue_sha256)
        }
        w02_batch <- load_w02_batch()

        # One-off lifecycle repair for the completed historical W02 batch.
        # Finalizer run 37016080508 successfully applied and published all 13
        # decisions from source run 36989928117 before consumed-status
        # acknowledgement was added to the W02 finalizer.
        if (
          user_can(login_user,"control_workflows") &&
          !is.null(w02_batch) &&
          identical(as.character(w02_batch$batch_id), "w02-run-36989928117") &&
          identical(as.character(w02_batch$batch_status %||% ""), "review_complete")
        ) {
          repair_decisions <- active_sheet_w02_decisions()
          repair_decisions <- w02_filter_batch_decisions(repair_decisions, w02_batch$queue_sha256)
          repair_ids <- w02_resolved_ids(repair_decisions)
          repair_case_ids <- vapply(w02_batch$cases, function(x) as.character(x$review_case_id), character(1))
          if (
            length(w02_batch$cases) == 13L &&
            setequal(repair_ids, repair_case_ids)
          ) {
            append_batch_status(
              stage = "02",
              batch_id = w02_batch$batch_id,
              queue_sha256 = w02_batch$queue_sha256,
              status = "consumed",
              workflow_run_id = "37016080508",
              source_run_id = "36989928117",
              message = "Historical lifecycle repair: W02 decisions were successfully applied and published by finalizer run 37016080508."
            )
            w02_batch <- load_w02_batch()
          }
        }

        if (!is.null(w02_batch)) {
          w02_all_decisions <- active_sheet_w02_decisions()
          w02_batch_decisions <- w02_filter_batch_decisions(w02_all_decisions, w02_batch$queue_sha256)
          w02_all_cases_rv(w02_batch$cases)
          w02_visible_cases <- cases_for_assignment_user(
            w02_batch$cases,
            assignment_registry_rv(),
            "02",
            w02_batch$batch_id,
            login_user,
            task_type = "enrichment",
            active_events = w02_batch_decisions
          )
          w02_cases_rv(w02_visible_cases)
          w02_queue_sha_rv(w02_batch$queue_sha256)
          w02_batch_id_rv(w02_batch$batch_id)
          w02_batch_status_rv(w02_batch$batch_status %||% "")
          w02_decisions(w02_batch_decisions)
          w02_unresolved <- w02_unresolved_indices()
          w02_idx(if (length(w02_unresolved)) w02_unresolved[[1L]] else max(1L, length(w02_visible_cases)))
          if(
            user_can(login_user,"control_workflows") &&
            !identical(w02_batch_status_rv(),"review_complete") &&
            workflow_all_assignments_complete("02", w02_batch_id_rv(), "enrichment", w02_batch_decisions) &&
            length(w02_all_cases_rv()) > 0L &&
            all(vapply(
              w02_all_cases_rv(),
              function(x) as.character(x$review_case_id %||% "") %in% w02_resolved_ids(w02_batch_decisions),
              logical(1)
            ))
          ) {
            mark_review_complete("02",w02_batch_id_rv(),w02_queue_sha_rv(),w02_batch_status_rv)
          }
        } else {
          w02_all_cases_rv(list())
          w02_cases_rv(NULL)
        }

        w04_batch <- load_w04_batch()
        if (!is.null(w04_batch) && identical(as.character(w04_batch$review_mode %||% ""), "resolution")) {
          w04_batch <- NULL
        }
        if (!is.null(w04_batch)) {
          w04_all_decisions <- active_sheet_w04_decisions()
          w04_batch_decisions <- w04_filter_batch_decisions(w04_all_decisions, w04_batch$queue_sha256)
          w04_all_cases_rv(w04_batch$cases)
          w04_visible_cases <- cases_for_assignment_user(
            w04_batch$cases,
            assignment_registry_rv(),
            "04",
            w04_batch$batch_id,
            login_user,
            task_type = "manual_screening",
            active_events = w04_batch_decisions
          )
          w04_cases_rv(w04_visible_cases)
          w04_queue_sha_rv(w04_batch$queue_sha256)
          w04_batch_id_rv(w04_batch$batch_id)
          w04_batch_status_rv(w04_batch$batch_status %||% "")
          w04_include_terms(w04_batch$highlight_include %||% character())
          w04_exclude_terms(w04_batch$highlight_exclude %||% character())
          w04_decisions(w04_batch_decisions)
          w04_unresolved <- w04_unresolved_indices()
          w04_idx(if (length(w04_unresolved)) w04_unresolved[[1L]] else max(1L,length(w04_visible_cases)))
          if(
            workflow_all_assignments_complete(
              "04", w04_batch_id_rv(), "manual_screening", w04_batch_decisions
            ) &&
            !identical(w04_batch_status_rv(),"review_complete") &&
            user_can(login_user,"control_workflows")
          ) {
            mark_review_complete("04",w04_batch_id_rv(),w04_queue_sha_rv(),w04_batch_status_rv)
          }
        }

        w04_resolution_batch <- load_w04_resolution_batch()
        if (!is.null(w04_resolution_batch)) {
          all_res <- active_sheet_w04_resolution_decisions()
          w04_resolution_cases_rv(w04_resolution_batch$cases)
          w04_resolution_queue_sha_rv(w04_resolution_batch$queue_sha256)
          w04_resolution_batch_id_rv(w04_resolution_batch$batch_id)
          w04_resolution_source_run_id_rv(w04_resolution_batch$source_run_id %||% "")
          w04_resolution_batch_status_rv(w04_resolution_batch$batch_status %||% "")
          w04_resolution_include_terms(w04_resolution_batch$highlight_include %||% character())
          w04_resolution_exclude_terms(w04_resolution_batch$highlight_exclude %||% character())
          w04_resolution_decisions(w04_filter_batch_decisions(all_res,w04_resolution_batch$queue_sha256))
          rr <- w04_resolution_unresolved_indices()
          w04_resolution_idx(if(length(rr)) rr[[1L]] else max(1L,length(w04_resolution_batch$cases)))
        }

        w04_conflict_batch <- load_w04_conflict_batch()
        if (!is.null(w04_conflict_batch)) {
          all_conf <- active_sheet_w04_conflict_decisions()
          w04_conflict_cases_rv(w04_conflict_batch$cases)
          w04_conflict_queue_sha_rv(w04_conflict_batch$queue_sha256)
          w04_conflict_batch_id_rv(w04_conflict_batch$batch_id)
          w04_conflict_batch_status_rv(w04_conflict_batch$batch_status %||% "")
          w04_conflict_decisions(w04_filter_batch_decisions(all_conf,w04_conflict_batch$queue_sha256))
          cr <- w04_conflict_unresolved_indices()
          w04_conflict_idx(if(length(cr)) cr[[1L]] else max(1L,length(w04_conflict_batch$cases)))
        }

        if (length(w04_blind_conflict_cases())) {
          all_blind_conf <- active_sheet_w04_conflict_decisions()
          blind_conf_decisions <- w04_filter_batch_decisions(all_blind_conf,w04_queue_sha_rv())
          w04_conflict_decisions(blind_conf_decisions)
          cr <- w04_conflict_unresolved_indices()
          w04_conflict_idx(if(length(cr)) cr[[1L]] else max(1L,length(w04_blind_conflict_cases())))
        }

        w08_batch <- load_w08_batch()
        if (!is.null(w08_batch)) {
          w08_all_decisions <- active_sheet_w08_decisions()
          w08_batch_decisions <- w08_filter_batch_decisions(w08_all_decisions,w08_batch$queue_sha256)
          w08_all_cases_rv(w08_batch$cases)
          w08_visible_cases <- cases_for_assignment_user(
            w08_batch$cases,
            assignment_registry_rv(),
            "08",
            w08_batch$batch_id,
            login_user,
            task_type = "annotation",
            active_events = w08_batch_decisions
          )
          w08_cases_rv(w08_visible_cases)
          w08_queue_sha_rv(w08_batch$queue_sha256)
          w08_batch_id_rv(w08_batch$batch_id)
          w08_source_run_id_rv(w08_batch$source_run_id %||% "")
          w08_batch_status_rv(w08_batch$batch_status %||% "")
          w08_case_sha_rv(w08_batch$case_sha256 %||% character())
          species_opts <- w08_batch$species_options %||% character()
          if (
            !length(species_opts) &&
            identical(as.character(w08_batch$batch_id %||% ""), "w08-test-assignment-smoke")
          ) {
            species_opts <- c(
              "Atlantic salmon",
              "Rainbow trout",
              "Chinook salmon",
              "Coho salmon",
              "Sockeye salmon",
              "Chum salmon",
              "Pink salmon",
              "Masu salmon",
              "Unspecified species"
            )
          }
          w08_species_options(species_opts)
          w08_topic_options(w08_batch$topic_options %||% list())
          w08_decisions(w08_batch_decisions)
          w08_unresolved <- w08_unresolved_indices()
          w08_idx(if(length(w08_unresolved)) w08_unresolved[[1L]] else max(1L,length(w08_visible_cases)))
          if(
            user_can(login_user,"control_workflows") &&
            !identical(w08_batch_status_rv(),"review_complete") &&
            workflow_all_assignments_complete("08", w08_batch_id_rv(), "annotation", w08_batch_decisions) &&
            length(w08_all_cases_rv()) > 0L &&
            all(vapply(
              w08_all_cases_rv(),
              function(x) as.character(x$record_id %||% "") %in% w08_decision_ids(w08_batch_decisions),
              logical(1)
            ))
          ) {
            mark_review_complete("08",w08_batch_id_rv(),w08_queue_sha_rv(),w08_batch_status_rv)
          }
        } else {
          w08_all_cases_rv(list())
          w08_cases_rv(NULL)
        }

        if (!is.null(batch)) {
          w01_all_cases_rv(batch$cases)
          visible_cases <- cases_for_assignment_user(
            batch$cases,
            assignment_registry_rv(),
            "01",
            batch$batch_id,
            login_user,
            task_type = "deduplication",
            active_events = current_decisions
          )
          cases_rv(visible_cases)
          queue_sha_rv(batch$queue_sha256)
          batch_id_rv(batch$batch_id)
          decisions(current_decisions)

          ids <- vapply(visible_cases, function(x) as.character(x$review_case_id), character(1))
          done_ids <- if (length(current_decisions)) {
            unique(vapply(current_decisions, function(x) as.character(x$review_case_id), character(1)))
          } else character()
          unresolved <- which(!ids %in% done_ids)

          if (length(unresolved)) {
            idx(unresolved[[1L]])
            complete(FALSE)
          } else {
            idx(max(1L,length(visible_cases)))
            complete(TRUE)
            if(!identical(batch_status_rv(),"review_complete") &&
               user_can(login_user,"control_workflows") &&
               w01_all_assignments_complete()) {
              mark_review_complete("01",batch_id_rv(),queue_sha_rv(),batch_status_rv)
            }
          }
        } else {
          cases_rv(NULL)
          w01_all_cases_rv(list())
          queue_sha_rv("")
          batch_id_rv("")
          batch_status_rv("")
          decisions(list())
          idx(1L)
          complete(FALSE)
        }
        TRUE
      }, error = function(e) {
        login_status(paste("Batch could not be loaded:", conditionMessage(e)))
        FALSE
      })

      if (isTRUE(loaded)) {
        current_user(login_user)
        authenticated(TRUE)
        app_view("tasks")
        failed_attempts(0L)
        login_status("")
      }
    } else {
      n <- failed_attempts() + 1L
      failed_attempts(n)
      if (n >= 5L) {
        lock_until(now + 60)
        failed_attempts(0L)
        login_status("Too many failed attempts. Try again in one minute.")
      } else {
        login_status("Invalid email or access key.")
      }
    }
  })

  current_case <- reactive({ req(authenticated(), cases_rv()); cases_rv()[[idx()]] })

  w01_current_saved_choice <- reactive({
    z <- current_case()
    ds <- decisions()
    if (!length(ds)) return("")
    hit <- Filter(
      function(x) identical(
        as.character(x$review_case_id %||% x$case_id %||% ""),
        as.character(z$review_case_id)
      ),
      ds
    )
    if (!length(hit)) return("")
    as.character(hit[[1L]]$decision %||% "")
  })

  output$w01_decision_buttons <- renderUI({
    choice <- w01_current_saved_choice()
    div(
      class = "decision-row d-flex flex-wrap gap-2",
      actionButton(
        "duplicate", "Same record",
        class = paste("btn-success", if (identical(choice, "duplicate")) "decision-selected" else ""),
        `aria-pressed` = if (identical(choice, "duplicate")) "true" else "false"
      ),
      actionButton(
        "not_duplicate", "Different records",
        class = paste("btn-outline-danger", if (identical(choice, "not_duplicate")) "decision-selected" else ""),
        `aria-pressed` = if (identical(choice, "not_duplicate")) "true" else "false"
      ),
      actionButton(
        "uncertain", "Unsure",
        class = paste("btn-outline-secondary", if (identical(choice, "uncertain")) "decision-selected" else ""),
        `aria-pressed` = if (identical(choice, "uncertain")) "true" else "false"
      )
    )
  })

  output$batch_label <- renderText({
    req(authenticated())
    sprintf("Workflow 01 · duplicate review · %s", batch_id_rv())
  })

  output$progress_text <- renderText({
    req(authenticated(), cases_rv())
    total <- length(cases_rv())
    remaining <- length(unresolved_indices())
    sprintf("Case %d of %d · %d remaining", idx(), total, remaining)
  })
  output$progress_bar <- renderUI({
    req(authenticated())
    total <- length(cases_rv())
    remaining <- length(unresolved_indices())
    pct <- round(100 * (total - remaining) / total)
    div(class="progress mb-3",
        div(class="progress-bar", role="progressbar",
            style=sprintf("width:%s%%",pct),
            sprintf("%s%%",pct)))
  })

  output$case_view <- renderUI({
    req(authenticated())
    z <- current_case()
    ev <- z$deterministic_evidence %||% list()
    evidence <- Filter(function(x) !is.null(x$value) && !identical(x$value,""),
      list(
        list(label="Title similarity", value=fmt_value(ev$title_similarity)),
        list(label="Exact title", value=fmt_value(ev$exact_title)),
        list(label="Exact abstract", value=fmt_value(ev$exact_abstract)),
        list(label="Identifier conflict", value=fmt_value(ev$identifier_conflict)),
        list(label="Classifier", value=ev$classifier_decision %||% ""),
        list(label="Classifier rule", value=ev$classifier_rule %||% "")
      ))
    fields <- list(
      source = field_pair(z$record_i$source, z$record_j$source),
      title = field_pair(z$record_i$title, z$record_j$title),
      authors = field_pair(z$record_i$authors, z$record_j$authors),
      year = field_pair(z$record_i$year, z$record_j$year),
      journal = field_pair(z$record_i$journal, z$record_j$journal),
      doi = field_pair(z$record_i$doi, z$record_j$doi, char_level = TRUE),
      source_record_id = field_pair(z$record_i$source_record_id, z$record_j$source_record_id, char_level = TRUE),
      abstract = field_pair(
        normalise_display_text(z$record_i$abstract),
        normalise_display_text(z$record_j$abstract)
      )
    )
    fields$source$a <- tags$span(class = "source-badge", fields$source$a)
    fields$source$b <- tags$span(class = "source-badge", fields$source$b)

    tagList(
      layout_columns(
        col_widths = c(6,6),
        record_card(z$record_i, "Record A", fields, "a"),
        record_card(z$record_j, "Record B", fields, "b")
      ),
      card(
        class="mt-3",
        card_header("Matching evidence"),
        div(class="evidence-grid",
            lapply(evidence, function(x)
              div(class="evidence-item",
                  tags$div(class="text-secondary small", x$label),
                  tags$strong(x$value))))
      )
    )
  })

  w02_current_case <- reactive({
    req(authenticated(), w02_cases_rv())
    w02_cases_rv()[[w02_idx()]]
  })

  w02_current_saved_choice <- reactive({
    z <- w02_current_case()
    ds <- w02_decisions()
    if (!length(ds)) return("")
    hit <- Filter(
      function(x) identical(
        as.character(x$review_case_id %||% ""),
        as.character(z$review_case_id)
      ),
      ds
    )
    if (!length(hit)) return("")
    as.character(hit[[1L]]$decision %||% "")
  })

  output$w02_progress_text <- renderUI({
    req(authenticated(), w02_cases_rv())
    total <- length(w02_cases_rv())
    remaining <- length(w02_unresolved_indices())
    tags$span(sprintf("Case %d of %d · %d remaining", w02_idx(), total, remaining))
  })

  output$w02_progress_bar <- renderUI({
    req(authenticated(), w02_cases_rv())
    total <- length(w02_cases_rv())
    remaining <- length(w02_unresolved_indices())
    pct <- if (total) round(100 * (total - remaining) / total) else 0
    div(class="progress mb-3",
        div(class="progress-bar", role="progressbar",
            style=sprintf("width:%s%%",pct),
            sprintf("%s%%",pct)))
  })

  output$w02_decision_panel <- renderUI({
    z <- w02_current_case()
    choice <- w02_current_saved_choice()
    reason <- as.character(z$reason %||% z$conflict$reason %||% "")
    buttons <- if (identical(reason, "returned_doi_mismatch")) {
      tagList(
        actionButton(
          "w02_accept_field", "Accept provider field",
          class=paste("btn-success", if (identical(choice,"accept_provider_field")) "decision-selected" else ""),
          `aria-pressed`=if (identical(choice,"accept_provider_field")) "true" else "false"
        ),
        actionButton(
          "w02_reject_match", "Reject provider match",
          class=paste("btn-outline-danger", if (identical(choice,"reject_provider_match")) "decision-selected" else ""),
          `aria-pressed`=if (identical(choice,"reject_provider_match")) "true" else "false"
        ),
        actionButton(
          "w02_uncertain", "Unsure",
          class=paste("btn-outline-secondary", if (identical(choice,"uncertain")) "decision-selected" else ""),
          `aria-pressed`=if (identical(choice,"uncertain")) "true" else "false"
        )
      )
    } else {
      tagList(
        actionButton(
          "w02_accept_field", "Accept provider field",
          class=paste("btn-success", if (identical(choice,"accept_provider_field")) "decision-selected" else ""),
          `aria-pressed`=if (identical(choice,"accept_provider_field")) "true" else "false"
        ),
        actionButton(
          "w02_reject_field", "Reject provider field",
          class=paste("btn-outline-danger", if (identical(choice,"reject_provider_field")) "decision-selected" else ""),
          `aria-pressed`=if (identical(choice,"reject_provider_field")) "true" else "false"
        ),
        actionButton(
          "w02_uncertain", "Unsure",
          class=paste("btn-outline-secondary", if (identical(choice,"uncertain")) "decision-selected" else ""),
          `aria-pressed`=if (identical(choice,"uncertain")) "true" else "false"
        )
      )
    }

    card(
      class = "decision-panel",
      div(
        class = "d-flex flex-wrap justify-content-between align-items-center gap-2",
        tags$div(class="saved-note", textOutput("w02_save_status")),
        div(
          class="d-flex flex-wrap gap-2",
          div(class="decision-row d-flex flex-wrap gap-2", buttons),
          div(
            class="nav-row d-flex gap-2",
            actionButton("w02_previous", "← Previous"),
            actionButton("w02_next", "Next →")
          )
        )
      )
    )
  })

  output$w02_case_view <- renderUI({
    z <- w02_current_case()
    can <- z$canonical %||% list()
    pr <- z$provider_response %||% list()
    provider <- as.character(z$provider %||% z$conflict$provider %||% "")
    field <- as.character(z$field %||% z$conflict$field %||% "")
    reason <- as.character(z$reason %||% z$conflict$reason %||% "")
    returned_doi <- as.character(z$returned_doi %||% z$conflict$returned_doi %||% pr$returned_doi %||% "")
    provider_title <- as.character(pr$title %||% "")
    provider_abstract <- normalise_display_text(pr$abstract %||% "")
    can_title <- as.character(can$title %||% "")
    can_abstract <- normalise_display_text(can$abstract %||% "")
    can_doi <- as.character(can$doi %||% z$doi %||% "")

    title_pair <- field_pair(can_title, provider_title)
    doi_pair <- field_pair(can_doi, returned_doi, char_level=TRUE)
    abstract_pair <- field_pair(can_abstract, provider_abstract)

    provider_keywords <- pr$author_keywords %||% character()
    if (is.list(provider_keywords)) provider_keywords <- unlist(provider_keywords, use.names=FALSE)
    provider_keywords <- paste(as.character(provider_keywords), collapse="; ")

    tagList(
      card(
        class="mb-3",
        div(
          class="p-2 d-flex flex-wrap gap-4",
          div(tags$span(class="text-secondary small","Provider"), tags$strong(class="d-block",provider)),
          div(tags$span(class="text-secondary small","Field"), tags$strong(class="d-block",field)),
          div(tags$span(class="text-secondary small","Reason"), tags$strong(class="d-block",reason)),
          div(tags$span(class="text-secondary small","Record ID"), tags$strong(class="d-block",z$record_id %||% ""))
        )
      ),
      layout_columns(
        col_widths=c(6,6),
        card(
          class="record-card",
          card_header(tags$strong("Canonical record")),
          div(
            class="compact-record-body",
            div(class="record-title",title_pair$a),
            tags$dl(
              class="record-meta",
              tags$dt("DOI"),tags$dd(doi_pair$a)
            ),
            tags$hr(class="record-divider"),
            tags$h6(class="abstract-heading","Abstract"),
            div(class="abstract-text",abstract_pair$a)
          )
        ),
        card(
          class="record-card",
          card_header(tags$strong(paste("Provider candidate ·",provider))),
          div(
            class="compact-record-body",
            div(class="record-title",title_pair$b),
            tags$dl(
              class="record-meta",
              tags$dt("Returned DOI"),tags$dd(doi_pair$b),
              tags$dt("EID"),tags$dd(pr$eid %||% z$conflict$eid %||% ""),
              tags$dt("Keywords"),tags$dd(provider_keywords)
            ),
            tags$hr(class="record-divider"),
            tags$h6(class="abstract-heading","Abstract"),
            div(class="abstract-text",abstract_pair$b)
          )
        )
      )
    )
  })

  output$w02_save_status <- renderText(w02_status())

  save_w02_choice <- function(choice) {
    if(!session_can("adjudicate_assigned")) {
      w02_status("You do not have permission to adjudicate records.")
      return(FALSE)
    }
    z <- w02_current_case()

    if (
      session_can("manage_assignments") &&
      !user_has_active_assignment(
        assignment_registry_rv(), "02", w02_batch_id_rv(), "enrichment",
        as.character(z$review_case_id), session_reviewer_id()
      )
    ) {
      ensure_direct_assignment(
        "02", w02_batch_id_rv(), "enrichment",
        z, as.character(z$review_case_id), w02_active_assignment_events()
      )
    }

    if (
      assignment_mode_active(assignment_registry_rv(), "02", w02_batch_id_rv(), "enrichment") &&
      !session_can("manage_assignments") &&
      !user_has_active_assignment(
        assignment_registry_rv(), "02", w02_batch_id_rv(), "enrichment",
        as.character(z$review_case_id), session_reviewer_id()
      )
    ) {
      w02_status("This assignment is no longer active. Return to tasks to refresh your queue.")
      return(FALSE)
    }
    current <- w02_decisions()
    prior <- NULL
    if (length(current)) {
      hits <- Filter(function(x) identical(as.character(x$review_case_id %||% ""), as.character(z$review_case_id)), current)
      if (length(hits)) prior <- hits[[1L]]
    }
    decision <- list(
      review_case_id=as.character(z$review_case_id),
      record_id=as.character(z$record_id %||% ""),
      provider=as.character(z$provider %||% z$conflict$provider %||% ""),
      field=as.character(z$field %||% z$conflict$field %||% ""),
      reason=as.character(z$reason %||% z$conflict$reason %||% ""),
      decision=choice,
      note="",
      reviewer=session_reviewer_id(),
      resolved_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"),
      queue_sha256=w02_queue_sha_rv()
    )
    saved <- tryCatch(
      append_sheet_w02_decision(decision, prior_decision=prior),
      error=function(e){w02_status(paste("Save failed:",conditionMessage(e)));NULL}
    )
    if (is.null(saved)) return(FALSE)
    remaining <- Filter(
      function(x) !identical(as.character(x$review_case_id %||% ""), as.character(z$review_case_id)),
      current
    )
    w02_decisions(c(remaining,list(saved)))
    w02_status(sprintf("Saved %s at %s",choice,format(Sys.time(),"%H:%M:%S")))
    TRUE
  }

  dispatch_completed_w02 <- function() {
    if(!session_can("control_workflows")) {
      w02_status("Review complete. Awaiting an administrator to resume Workflow 02.")
      return(FALSE)
    }
    unresolved <- w02_unresolved_indices()
    if (length(unresolved)) return(FALSE)

    active <- w02_decisions()
    if (!length(active)) return(FALSE)
    if (any(vapply(active, function(x) identical(as.character(x$decision %||% ""), "uncertain"), logical(1)))) {
      return(FALSE)
    }

    batch_id <- as.character(w02_batch_id_rv())
    source_run_id <- sub("^w02-run-", "", batch_id)
    if (!grepl("^[0-9]+$", source_run_id)) {
      w02_status("Resume failed: active W02 batch does not contain a valid source run ID.")
      return(FALSE)
    }

    queue_sha <- as.character(w02_queue_sha_rv())
    already <- tryCatch(
      w02_resume_request_exists(queue_sha, source_run_id),
      error = function(e) {
        w02_status(paste("Resume status check failed:", conditionMessage(e)))
        NA
      }
    )
    if (is.na(already)) return(FALSE)
    if (isTRUE(already)) {
      w02_status("Workflow 02 resume has already been requested for this batch.")
      return(TRUE)
    }

    tryCatch({
      append_w02_resume_request(queue_sha, source_run_id, "dispatching")
      dispatch_w02_resume(source_run_id, publish = TRUE)
      append_w02_resume_request(queue_sha, source_run_id, "dispatched")
      w02_status("All cases complete. Workflow 02 resumed automatically.")
      TRUE
    }, error = function(e) {
      try(
        append_w02_resume_request(queue_sha, source_run_id, "failed", conditionMessage(e)),
        silent = TRUE
      )
      w02_status(paste("All cases are complete, but automatic resume failed:", conditionMessage(e)))
      FALSE
    })
  }

  advance_w02 <- function() {
    unresolved <- w02_unresolved_indices()
    if (!length(unresolved)) {
      if(session_can("control_workflows")) {
        mark_review_complete("02",w02_batch_id_rv(),w02_queue_sha_rv(),w02_batch_status_rv)
        dispatch_completed_w02()
      } else {
        w02_status("Review complete. Awaiting an administrator to resume Workflow 02.")
      }
      app_view("tasks")
      return(invisible(TRUE))
    }
    later <- unresolved[unresolved > w02_idx()]
    w02_idx(if (length(later)) later[[1L]] else unresolved[[1L]])
    invisible(TRUE)
  }

  w04_current_case <- reactive({
    req(authenticated(), w04_cases_rv())
    w04_cases_rv()[[w04_idx()]]
  })

  w04_current_saved_choice <- reactive({
    z <- w04_current_case()
    ds <- w04_decisions()
    if (!length(ds)) return("")
    hit <- Filter(
      function(x) {
        identical(as.character(x$review_case_id %||% ""), as.character(z$review_case_id)) &&
          identical(decision_user_id(x), session_reviewer_id())
      },
      ds
    )
    if (!length(hit)) return("")
    as.character(hit[[1L]]$decision %||% "")
  })

  output$w04_decision_buttons <- renderUI({
    choice <- w04_current_saved_choice()
    div(
      class = "decision-row d-flex flex-wrap gap-2",
      actionButton(
        "w04_retain", "Include",
        class = paste("btn-success", if (identical(choice, "retain")) "decision-selected" else ""),
        `aria-pressed` = if (identical(choice, "retain")) "true" else "false"
      ),
      actionButton(
        "w04_exclude", "Exclude",
        class = paste("btn-outline-danger", if (identical(choice, "exclude")) "decision-selected" else ""),
        `aria-pressed` = if (identical(choice, "exclude")) "true" else "false"
      ),
      actionButton(
        "w04_uncertain", "Unsure",
        class = paste("btn-outline-secondary", if (identical(choice, "uncertain")) "decision-selected" else ""),
        `aria-pressed` = if (identical(choice, "uncertain")) "true" else "false"
      )
    )
  })

  output$w04_progress_text <- renderUI({
    req(authenticated(),w04_cases_rv())
    total <- length(w04_cases_rv())
    remaining <- length(w04_unresolved_indices())
    tags$span(sprintf("Record %d of %d · %d remaining",w04_idx(),total,remaining))
  })

  output$w04_progress_bar <- renderUI({
    req(authenticated(),w04_cases_rv())
    total <- length(w04_cases_rv())
    remaining <- length(w04_unresolved_indices())
    pct <- if(total) round(100*(total-remaining)/total) else 0
    div(class="progress mb-3",
        div(class="progress-bar",role="progressbar",
            style=sprintf("width:%s%%",pct),
            sprintf("%s%%",pct)))
  })

  output$w04_case_view <- renderUI({
    z <- w04_current_case()
    b <- z$bibliographic %||% list()

    card(
      class="record-card",
      card_header(
        div(
          class="d-flex justify-content-between align-items-center",
          tags$strong("Title and abstract screening"),
          tags$span(class="task-badge",sprintf("Random order %s",z$random_order %||% w04_idx()))
        )
      ),
      div(
        class="compact-record-body w04-text",
        div(
          class="record-title",
          highlight_screening_text(b$title %||% "",w04_include_terms(),w04_exclude_terms())
        ),
        div(
          class="w04-citation-grid",
          div(class="w04-citation-item",span(class="w04-citation-label","Authors"),span(class="w04-citation-value",b$authors %||% "")),
          div(class="w04-citation-item",span(class="w04-citation-label","Year"),span(class="w04-citation-value",b$year %||% "")),
          div(class="w04-citation-item",span(class="w04-citation-label","Journal"),span(class="w04-citation-value",b$journal %||% "")),
          div(class="w04-citation-item",span(class="w04-citation-label","Volume"),span(class="w04-citation-value",b$volume %||% "")),
          div(class="w04-citation-item",span(class="w04-citation-label","Pages"),span(class="w04-citation-value",b$pages %||% ""))
        ),
        div(class="w04-doi",tags$strong("DOI: "),b$doi %||% ""),
        tags$h6(class="abstract-heading","Abstract"),
        div(
          class="abstract-text",
          highlight_screening_text(b$abstract %||% "",w04_include_terms(),w04_exclude_terms())
        ),

        div(
          class="w04-keywords",
          tags$strong("Keywords: "),
          highlight_screening_text(b$keywords %||% "",w04_include_terms(),w04_exclude_terms())
        )
      )
    )
  })

  output$w04_save_status <- renderText(w04_status())

  save_w04_choice <- function(choice) {
    if(!session_can("adjudicate_assigned")) {
      w04_status("You do not have permission to adjudicate records.")
      return(FALSE)
    }
    z <- w04_current_case()
    current <- w04_decisions()
    prior <- NULL
    if(length(current)) {
      hits <- Filter(
        function(x) {
          identical(as.character(x$review_case_id %||% ""),as.character(z$review_case_id)) &&
            identical(decision_user_id(x), session_reviewer_id())
        },
        current
      )
      if(length(hits)) prior <- hits[[1L]]
    }

    decision <- list(
      review_case_id=as.character(z$review_case_id),
      record_id=as.character(z$record_id),
      decision=choice,
      rationale="Manual validation screening in Shiny",
      reviewer=session_reviewer_id(),
      resolved_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"),
      queue_sha256=w04_queue_sha_rv()
    )

    saved <- tryCatch(
      append_sheet_w04_decision(decision,prior_decision=prior),
      error=function(e){w04_status(paste("Save failed:",conditionMessage(e)));NULL}
    )
    if(is.null(saved)) return(FALSE)

    remaining <- Filter(
      function(x) !(
        identical(as.character(x$review_case_id %||% ""),as.character(z$review_case_id)) &&
        identical(decision_user_id(x), session_reviewer_id())
      ),
      current
    )
    w04_decisions(c(remaining,list(saved)))
    w04_status(sprintf("Saved %s at %s",choice,format(Sys.time(),"%H:%M:%S")))
    TRUE
  }

  advance_w04 <- function() {
    unresolved <- w04_unresolved_indices()
    if(!length(unresolved)) {
      all_complete <- workflow_all_assignments_complete(
        "04", w04_batch_id_rv(), "manual_screening", w04_active_assignment_events()
      )
      if (!all_complete) {
        w04_status("Your blinded review is complete. Waiting for the other assigned reviewer(s).")
      } else if (session_can("manage_assignments")) {
        w04_status(
          "All assigned manual screening is complete. Use Consistency checking to compare selected raters, save the analysis, and create any conflict set that should be adjudicated."
        )
      } else {
        w04_status("Your assigned manual screening is complete.")
      }
      app_view("tasks")
      return(invisible(TRUE))
    }
    later <- unresolved[unresolved>w04_idx()]
    w04_idx(if(length(later)) later[[1L]] else unresolved[[1L]])
    invisible(TRUE)
  }


  w04_resolution_current_case <- reactive({
    req(authenticated(),w04_resolution_cases_rv())
    w04_resolution_cases_rv()[[w04_resolution_idx()]]
  })

  w04_resolution_current_saved_choice <- reactive({
    z <- w04_resolution_current_case()
    ds <- w04_resolution_decisions()
    if (!length(ds)) return("")
    hit <- Filter(
      function(x) identical(as.character(x$review_case_id %||% ""), as.character(z$review_case_id)),
      ds
    )
    if (!length(hit)) return("")
    as.character(hit[[1L]]$decision %||% "")
  })

  output$w04_resolution_decision_buttons <- renderUI({
    choice <- w04_resolution_current_saved_choice()
    div(
      class = "decision-row d-flex flex-wrap gap-2",
      actionButton(
        "w04_resolution_retain", "Include",
        class = paste("btn-success", if (identical(choice, "retain")) "decision-selected" else ""),
        `aria-pressed` = if (identical(choice, "retain")) "true" else "false"
      ),
      actionButton(
        "w04_resolution_exclude", "Exclude",
        class = paste("btn-outline-danger", if (identical(choice, "exclude")) "decision-selected" else ""),
        `aria-pressed` = if (identical(choice, "exclude")) "true" else "false"
      )
    )
  })

  output$w04_resolution_progress_text <- renderUI({
    req(authenticated(),w04_resolution_cases_rv())
    total<-length(w04_resolution_cases_rv()); remaining<-length(w04_resolution_unresolved_indices())
    tags$span(sprintf("Record %d of %d · %d remaining",w04_resolution_idx(),total,remaining))
  })

  output$w04_resolution_progress_bar <- renderUI({
    req(authenticated(),w04_resolution_cases_rv())
    total<-length(w04_resolution_cases_rv()); remaining<-length(w04_resolution_unresolved_indices())
    pct<-if(total)round(100*(total-remaining)/total)else 0
    div(class="progress mb-3",div(class="progress-bar",role="progressbar",style=sprintf("width:%s%%",pct),sprintf("%s%%",pct)))
  })

  output$w04_resolution_case_view <- renderUI({
    z<-w04_resolution_current_case(); b<-z$bibliographic %||% list()
    votes<-as.character((z$screening %||% list())$votes %||% character())
    card(class="record-card",
      card_header(div(class="d-flex justify-content-between align-items-center",
        tags$strong("Resolve model uncertainty"),
        tags$span(class="task-badge",sprintf("Record %d",w04_resolution_idx()))
      )),
      div(class="compact-record-body w04-text",
        div(class="record-title",highlight_screening_text(b$title %||% "",w04_resolution_include_terms(),w04_resolution_exclude_terms())),
        div(class="w04-citation-grid",
          div(class="w04-citation-item",span(class="w04-citation-label","Authors"),span(class="w04-citation-value",b$authors %||% "")),
          div(class="w04-citation-item",span(class="w04-citation-label","Year"),span(class="w04-citation-value",b$year %||% "")),
          div(class="w04-citation-item",span(class="w04-citation-label","Journal"),span(class="w04-citation-value",b$journal %||% "")),
          div(class="w04-citation-item",span(class="w04-citation-label","Volume"),span(class="w04-citation-value",b$volume %||% "")),
          div(class="w04-citation-item",span(class="w04-citation-label","Pages"),span(class="w04-citation-value",b$pages %||% ""))
        ),
        div(class="w04-doi",tags$strong("DOI: "),b$doi %||% ""),
        tags$h6(class="abstract-heading","Abstract"),
        div(class="abstract-text",highlight_screening_text(b$abstract %||% "",w04_resolution_include_terms(),w04_resolution_exclude_terms())),
        div(class="mt-3 p-2 border rounded",
          tags$strong("Model decisions: "),
          if(length(votes)) tagList(lapply(seq_along(votes),function(i)tags$span(class="task-badge me-1",sprintf("Pass %d: %s",i,votes[[i]])))) else tags$span(class="text-secondary","No model vote provenance available")
        ),
        div(class="w04-keywords",tags$strong("Keywords: "),highlight_screening_text(b$keywords %||% "",w04_resolution_include_terms(),w04_resolution_exclude_terms()))
      )
    )
  })
  output$w04_resolution_save_status <- renderText(w04_resolution_status())

  save_w04_resolution_choice <- function(choice) {
    if(!session_can("adjudicate_assigned")) {
      w04_resolution_status("You do not have permission to adjudicate records.")
      return(FALSE)
    }
    z<-w04_resolution_current_case(); current<-w04_resolution_decisions(); prior<-NULL
    if(length(current)){hits<-Filter(function(x)identical(as.character(x$review_case_id %||% ""),as.character(z$review_case_id)),current);if(length(hits))prior<-hits[[1L]]}
    decision<-list(review_case_id=as.character(z$review_case_id),record_id=as.character(z$record_id),decision=choice,
      rationale="Manual resolution of Workflow 04 model uncertainty in Shiny",reviewer=session_reviewer_id(),
      resolved_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"),queue_sha256=w04_resolution_queue_sha_rv())
    saved<-tryCatch(append_sheet_w04_resolution_decision(decision,prior_decision=prior),error=function(e){w04_resolution_status(paste("Save failed:",conditionMessage(e)));NULL})
    if(is.null(saved))return(FALSE)
    remaining<-Filter(function(x)!identical(as.character(x$review_case_id %||% ""),as.character(z$review_case_id)),current)
    w04_resolution_decisions(c(remaining,list(saved)))
    w04_resolution_status(sprintf("Saved %s at %s",choice,format(Sys.time(),"%H:%M:%S")));TRUE
  }

  advance_w04_resolution <- function() {
    unresolved<-w04_resolution_unresolved_indices()
    if(!length(unresolved)){
      if(session_can("control_workflows")) {
        mark_review_complete("04",w04_resolution_batch_id_rv(),w04_resolution_queue_sha_rv(),w04_resolution_batch_status_rv)
        dispatched<-tryCatch({dispatch_w04_resolution_resume(w04_resolution_source_run_id_rv(),w04_resolution_batch_id_rv(),w04_resolution_queue_sha_rv());TRUE},
          error=function(e){w04_resolution_status(paste("Review complete, but W04 resume dispatch failed:",conditionMessage(e)));FALSE})
        if(dispatched)w04_resolution_status("Review complete. Workflow 04 finalisation dispatched.")
      } else {
        w04_resolution_status("Review complete. Awaiting an administrator to resume Workflow 04.")
      }
      app_view("tasks");return(invisible(TRUE))
    }
    later<-unresolved[unresolved>w04_resolution_idx()]
    w04_resolution_idx(if(length(later))later[[1L]]else unresolved[[1L]]);invisible(TRUE)
  }

  w04_conflict_current_case <- reactive({
    req(authenticated())
    cs <- w04_active_conflict_cases()
    req(length(cs) > 0L)
    cs[[w04_conflict_idx()]]
  })

  w04_conflict_current_saved_choice <- reactive({
    z <- w04_conflict_current_case()
    ds <- w04_conflict_decisions()
    if (!length(ds)) return("")
    hits <- Filter(
      function(x) identical(
        as.character(x$review_case_id %||% ""),
        as.character(z$review_case_id %||% "")
      ),
      ds
    )
    if (!length(hits)) return("")
    as.character(hits[[1L]]$decision %||% "")
  })

  output$w04_conflict_decision_buttons <- renderUI({
    choice <- w04_conflict_current_saved_choice()
    div(
      class="decision-row d-flex flex-wrap gap-2",
      actionButton(
        "w04_conflict_retain","Include",
        class=paste("btn-success",if(identical(choice,"retain"))"decision-selected" else ""),
        `aria-pressed`=if(identical(choice,"retain"))"true" else "false"
      ),
      actionButton(
        "w04_conflict_exclude","Exclude",
        class=paste("btn-outline-danger",if(identical(choice,"exclude"))"decision-selected" else ""),
        `aria-pressed`=if(identical(choice,"exclude"))"true" else "false"
      )
    )
  })

  output$w04_conflict_progress_bar <- renderUI({
    cs <- w04_active_conflict_cases()
    req(length(cs) > 0L)
    remaining <- length(w04_conflict_unresolved_indices())
    total <- length(cs)
    pct <- round(100 * (total - remaining) / total)
    tagList(
      tags$div(
        class="text-secondary small mb-2",
        sprintf("Conflict %d of %d · %d remaining",w04_conflict_idx(),total,remaining)
      ),
      div(
        class="progress mb-3",
        div(
          class="progress-bar",
          role="progressbar",
          style=sprintf("width:%s%%",pct),
          sprintf("%s%%",pct)
        )
      )
    )
  })

  output$w04_conflict_case_view <- renderUI({
    z <- w04_conflict_current_case()
    b <- z$bibliographic %||% list()
    blind <- z$blind_review %||% list()
    reviewer_decisions <- blind$reviewer_decisions %||% list()
    rater_label <- function(uid) {
      if (identical(uid,"model")) return("Model")
      u <- find_user_by_id(user_registry_rv(),uid,require_active=FALSE)
      if (is.null(u)) uid else as.character(u$display_name)
    }
    reviewer_badges <- if (length(reviewer_decisions)) {
      ids <- names(reviewer_decisions)
      if (is.null(ids) || any(!nzchar(ids))) {
        ids <- paste0("rater-",seq_along(reviewer_decisions))
      }
      lapply(seq_along(reviewer_decisions), function(i) {
        d <- as.character(reviewer_decisions[[i]]$decision %||% "")
        label <- c(retain="Include",exclude="Exclude",uncertain="Unsure")[[d]] %||% d
        tags$span(
          class="task-badge me-1",
          sprintf("%s: %s",rater_label(ids[[i]]),label)
        )
      })
    } else NULL
    comparison_ids <- names(reviewer_decisions)
    comparison_label <- if (length(comparison_ids)) {
      paste(vapply(comparison_ids,rater_label,character(1)),collapse=" vs ")
    } else {
      "selected raters"
    }

    card(
      class="record-card",
      card_header(
        div(
          class="d-flex justify-content-between align-items-center",
          tags$strong(paste("Resolve conflict ·",comparison_label)),
          tags$span(class="task-badge",as.character(z$record_id %||% z$review_case_id %||% ""))
        )
      ),
      div(
        class="compact-record-body w04-text",
        div(class="record-title",highlight_screening_text(b$title %||% "",w04_include_terms(),w04_exclude_terms())),
        div(
          class="w04-citation-grid",
          div(class="w04-citation-item",span(class="w04-citation-label","Authors"),span(class="w04-citation-value",b$authors %||% "")),
          div(class="w04-citation-item",span(class="w04-citation-label","Year"),span(class="w04-citation-value",b$year %||% "")),
          div(class="w04-citation-item",span(class="w04-citation-label","Journal"),span(class="w04-citation-value",b$journal %||% ""))
        ),
        tags$h6(class="abstract-heading","Abstract"),
        div(class="abstract-text",highlight_screening_text(b$abstract %||% "",w04_include_terms(),w04_exclude_terms())),
        if (length(reviewer_badges)) {
          div(
            class="mt-3 p-2 border rounded",
            tags$strong("Decisions in this comparison: "),
            tagList(reviewer_badges)
          )
        },
        div(class="w04-keywords",tags$strong("Keywords: "),highlight_screening_text(b$keywords %||% "",w04_include_terms(),w04_exclude_terms()))
      )
    )
  })

  output$w04_conflict_save_status <- renderText(w04_conflict_status())

  save_w04_conflict_choice <- function(choice) {
    if(!session_can("adjudicate_assigned")) {
      w04_conflict_status("You do not have permission to adjudicate conflicts.")
      return(FALSE)
    }
    z <- w04_conflict_current_case()
    current <- w04_conflict_decisions()
    prior <- NULL
    if(length(current)) {
      hits <- Filter(
        function(x) identical(
          as.character(x$review_case_id %||% ""),
          as.character(z$review_case_id %||% "")
        ),
        current
      )
      if(length(hits)) prior <- hits[[1L]]
    }
    decision <- list(
      review_case_id=as.character(z$review_case_id),
      record_id=as.character(z$record_id %||% ""),
      decision=choice,
      rationale="Final adjudication of independent Workflow 04 reviewer conflict",
      reviewer=session_reviewer_id(),
      resolved_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"),
      queue_sha256=w04_active_conflict_queue_sha()
    )
    saved <- tryCatch(
      append_sheet_w04_conflict_decision(decision,prior_decision=prior),
      error=function(e){w04_conflict_status(paste("Save failed:",conditionMessage(e)));NULL}
    )
    if(is.null(saved)) return(FALSE)
    remaining <- Filter(
      function(x)!identical(
        as.character(x$review_case_id %||% ""),
        as.character(z$review_case_id %||% "")
      ),
      current
    )
    w04_conflict_decisions(c(remaining,list(saved)))
    w04_conflict_status(sprintf("Saved %s at %s",choice,format(Sys.time(),"%H:%M:%S")))
    TRUE
  }

  advance_w04_conflict <- function() {
    unresolved <- w04_conflict_unresolved_indices()
    if(!length(unresolved)) {
      set <- w04_active_consistency_conflict_set()
      if (!is.null(set)) {
        w04_conflict_status(sprintf(
          "All conflicts in %s have a resolved decision. The resolution is definitive for this case unless superseded by a later adjudication.",
          as.character(set$conflict_set_id %||% "this conflict set")
        ))
      } else {
        w04_conflict_status("All conflicts in this assigned conflict queue have been resolved.")
      }
      app_view("tasks")
      return(invisible(TRUE))
    }
    later <- unresolved[unresolved>w04_conflict_idx()]
    w04_conflict_idx(if(length(later))later[[1L]] else unresolved[[1L]])
    invisible(TRUE)
  }

  w08_current_case <- reactive({
    req(authenticated(),w08_cases_rv())
    w08_cases_rv()[[w08_idx()]]
  })

  output$w08_progress_text <- renderUI({
    req(authenticated(),w08_cases_rv())
    total <- length(w08_cases_rv())
    remaining <- length(w08_unresolved_indices())
    tags$span(sprintf("Record %d of %d · %d remaining",w08_idx(),total,remaining))
  })

  output$w08_progress_bar <- renderUI({
    req(authenticated(),w08_cases_rv())
    total <- length(w08_cases_rv())
    remaining <- length(w08_unresolved_indices())
    pct <- if(total) round(100*(total-remaining)/total) else 0
    div(class="progress mb-3",
        div(class="progress-bar",role="progressbar",
            style=sprintf("width:%s%%",pct),
            sprintf("%s%%",pct)))
  })

  w08_saved_record_decision <- function(record_id) {
    ds <- w08_decisions() %||% list()
    if (!length(ds)) return(NULL)
    hits <- Filter(
      function(x) identical(as.character(x$record_id %||% ""), as.character(record_id)),
      ds
    )
    if (!length(hits)) return(NULL)
    hits[[1L]]
  }

  w08_saved_issue_decision <- function(record_id, issue) {
    saved <- w08_saved_record_decision(record_id)
    if (is.null(saved)) return(NULL)
    raw <- as.character(saved$issue_decisions_json %||% "")
    if (!nzchar(raw)) return(NULL)
    items <- tryCatch(
      jsonlite::fromJSON(raw, simplifyVector = FALSE),
      error = function(e) list()
    )
    if (!length(items)) return(NULL)

    typ <- as.character(issue$issue_type %||% "")
    state_sha <- as.character(issue$issue_state_sha256 %||% "")
    hits <- Filter(function(x) {
      same_type <- identical(as.character(x$issue_type %||% ""), typ)
      saved_sha <- as.character(x$issue_state_sha256 %||% "")
      same_state <- !nzchar(state_sha) || !nzchar(saved_sha) || identical(saved_sha, state_sha)
      same_type && same_state
    }, items)
    if (!length(hits)) return(NULL)
    hits[[1L]]
  }

  w08_issue_panel <- function(issue,j) {
    typ <- as.character(issue$issue_type %||% "")
    av <- issue$automated_value %||% list()
    decision_id <- paste0("w08_decision_",j)
    current_record <- w08_current_case()
    saved_issue <- w08_saved_issue_decision(current_record$record_id, issue)
    saved_choice <- as.character(saved_issue$decision %||% "")
    saved_value <- saved_issue$final_value %||% list()

    label_map <- c(
      species_none="Species verification",
      geography_model_failure="Geography model failure",
      geography_unresolved="Geography verification",
      geography_evidence_unvalidated="Geography evidence verification",
      topic_extreme_disagreement="Topic verification",
      zero_topic_eligibility_uncertain="Topic eligibility verification"
    )
    allowed <- as.character(issue$allowed_human_outcomes %||% character())
    labels <- c(
      assign_named_species="Assign named species",
      assign_unspecified_species="Assign unspecified species",
      exclude_record="Exclude record",
      assign_country_set="Assign country set",
      assign_none="Assign no country",
      accept_model="Accept model geography",
      override_country_set="Override country set",
      accept_retained_topics="Accept retained topics",
      replace_topic_set="Replace topic set",
      no_code="Retain with no topic code",
      include_uncoded="Retain uncoded"
    )
    choices <- setNames(allowed,unname(labels[allowed]))

    detail <- switch(
      typ,
      geography_model_failure = tagList(
        tags$p(class="mb-1",tags$strong("Deterministic countries: "),as.character(av$deterministic_primary_countries %||% "")),
        tags$p(class="mb-1",tags$strong("Deterministic ISO3: "),as.character(av$deterministic_primary_iso3c %||% "")),
        tags$p(class="mb-2",tags$strong("Model error: "),as.character(av$llm_error %||% ""))
      ),
      geography_unresolved = tagList(
        tags$p(class="mb-1",tags$strong("Model countries: "),as.character(av$luna_country_names %||% "")),
        tags$p(class="mb-1",tags$strong("Evidence: "),as.character(av$luna_evidence %||% "")),
        tags$p(class="mb-2",tags$strong("Reason: "),as.character(av$geography_reason %||% ""))
      ),
      geography_evidence_unvalidated = tagList(
        tags$p(class="mb-1",tags$strong("Model countries: "),as.character(av$luna_country_names %||% "")),
        tags$p(class="mb-1",tags$strong("Evidence: "),as.character(av$luna_evidence %||% "")),
        tags$p(class="mb-2",tags$strong("Reason: "),as.character(av$geography_reason %||% ""))
      ),
      topic_extreme_disagreement = {
        ps <- av$pathways %||% list()
        items <- lapply(ps,function(p)tags$li(
          paste0(as.character(p$hierarchy_path %||% p$path_id %||% ""),
                 if(nzchar(as.character(p$stars %||% ""))) paste0(" · ",p$stars) else "")
        ))
        tagList(
          tags$p(class="mb-1",sprintf("Mean pairwise Jaccard: %s",as.character(av$mean_pairwise_jaccard %||% ""))),
          tags$ul(class="mb-2",items)
        )
      },
      zero_topic_eligibility_uncertain = tags$p(class="mb-2","No retained topic was assigned; verify whether the record should remain included."),
      tags$p(class="mb-2","Workflow 05 returned no eligible species assignment.")
    )

    extras <- switch(
      typ,
      species_none = {
        species_choices <- w08_species_options()
        tagList(
          if (length(species_choices)) {
            selectizeInput(
              paste0("w08_species_",j),
              "Named species",
              choices = species_choices,
              selected = as.character(unlist(saved_value$farmed_species %||% character(), use.names = FALSE)),
              multiple = TRUE,
              options = list(create = FALSE, persist = FALSE)
            )
          } else {
            tags$div(
              class = "text-danger small",
              "Accepted species list is unavailable for this queue. Named-species assignment is disabled."
            )
          }
        )
      },
      geography_model_failure = tagList(
        selectizeInput(
          paste0("w08_geo_",j),
          "Countries",
          choices = w08_country_choices(),
          selected = toupper(as.character(unlist(saved_value$iso3c %||% character(), use.names = FALSE))),
          multiple = TRUE,
          options = list(create = FALSE, persist = FALSE)
        )
      ),
      geography_unresolved = tagList(
        selectizeInput(
          paste0("w08_geo_",j),
          "Countries",
          choices = w08_country_choices(),
          selected = toupper(as.character(unlist(saved_value$iso3c %||% character(), use.names = FALSE))),
          multiple = TRUE,
          options = list(create = FALSE, persist = FALSE)
        )
      ),
      geography_evidence_unvalidated = tagList(
        selectizeInput(
          paste0("w08_geo_",j),
          "Override countries",
          choices = w08_country_choices(),
          selected = toupper(as.character(unlist(saved_value$iso3c %||% character(), use.names = FALSE))),
          multiple = TRUE,
          options = list(create = FALSE, persist = FALSE)
        )
      ),
      topic_extreme_disagreement = {
        opts <- w08_topic_options()
        topic_choices <- if(length(opts)) {
          setNames(
            vapply(opts,function(x)as.character(x$path_id %||% ""),character(1)),
            vapply(opts,function(x)as.character(x$hierarchy_path %||% x$path_id %||% ""),character(1))
          )
        } else character()
        tagList(
          if (length(topic_choices)) {
            selectizeInput(
              paste0("w08_topics_",j),
              "Replacement topic set",
              choices = topic_choices,
              selected = as.character(unlist(saved_value$path_ids %||% character(), use.names = FALSE)),
              multiple = TRUE,
              options = list(create = FALSE, persist = FALSE)
            )
          } else {
            tags$div(
              class = "text-danger small",
              "Accepted topic ontology is unavailable for this queue. Topic replacement is disabled."
            )
          }
        )
      },
      NULL
    )

    card(
      class=paste(
        "mb-3 w08-issue-card",
        if (identical(typ, "species_none")) "w08-species-issue" else ""
      ),
      card_header(tags$strong(unname(label_map[[typ]] %||% typ))),
      div(
        class="p-3",
        detail,
        selectInput(
          decision_id,
          "Decision",
          choices = c("Choose…"="",choices),
          selected = if (saved_choice %in% allowed) saved_choice else ""
        ),
        if (!is.null(saved_issue)) {
          tags$div(
            class = "saved-note small mb-2",
            "Saved decision loaded. Change the selections and click Save record to update it."
          )
        },
        extras
      )
    )
  }

  output$w08_case_view <- renderUI({
    z <- w08_current_case()
    issues <- z$issues %||% list()
    highlight_terms <- w08_record_highlight_terms(
      issues,
      species_options = w08_species_options()
    )
    card(
      class="record-card w08-record-card",
      card_header(
        div(
          class="d-flex justify-content-between align-items-center",
          tags$strong(sprintf("Annotation review · %d issue%s",length(issues),if(length(issues)==1L)"" else "s")),
          tags$span(class="task-badge",as.character(z$record_id %||% ""))
        )
      ),
      div(
        class="compact-record-body w04-text w08-review-layout",
        div(
          class="w08-review-evidence",
          div(
            class="record-title",
            highlight_named_terms(z$title %||% "",highlight_terms)
          ),
          tags$h6(class="abstract-heading","Abstract"),
          div(
            class="abstract-text",
            highlight_named_terms(z$abstract %||% "",highlight_terms)
          )
        ),
        div(
          class="w08-review-decisions",
          tagList(lapply(seq_along(issues),function(j)w08_issue_panel(issues[[j]],j)))
        )
      )
    )
  })

  output$w08_save_status <- renderText(w08_status())

  split_semicolon <- function(x) {
    z <- trimws(strsplit(as.character(x %||% ""),";",fixed=TRUE)[[1L]])
    z[nzchar(z)]
  }

  save_w08_record <- function() {
    if(!session_can("adjudicate_assigned")) {
      w08_status("You do not have permission to adjudicate records.")
      return(FALSE)
    }
    z <- w08_current_case()
    rid <- as.character(z$record_id)

    if (
      session_can("manage_assignments") &&
      !user_has_active_assignment(
        assignment_registry_rv(), "08", w08_batch_id_rv(), "annotation",
        rid, session_reviewer_id()
      )
    ) {
      ensure_direct_assignment(
        "08", w08_batch_id_rv(), "annotation",
        z, rid, w08_active_assignment_events()
      )
    }

    if (
      assignment_mode_active(assignment_registry_rv(), "08", w08_batch_id_rv(), "annotation") &&
      !session_can("manage_assignments") &&
      !user_has_active_assignment(
        assignment_registry_rv(), "08", w08_batch_id_rv(), "annotation",
        rid, session_reviewer_id()
      )
    ) {
      w08_status("This assignment is no longer active. Return to tasks to refresh your queue.")
      return(FALSE)
    }
    issues <- z$issues %||% list()
    now <- format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
    issue_decisions <- vector("list",length(issues))

    for(j in seq_along(issues)) {
      issue <- issues[[j]]
      typ <- as.character(issue$issue_type %||% "")
      choice <- as.character(input[[paste0("w08_decision_",j)]] %||% "")
      allowed <- as.character(issue$allowed_human_outcomes %||% character())
      if(!nzchar(choice) || !choice %in% allowed) {
        w08_status(sprintf("Choose a decision for %s.",typ))
        return(FALSE)
      }

      final_value <- NULL
      if(typ=="species_none") {
        if(choice=="assign_named_species") {
          allowed_species <- unique(as.character(w08_species_options() %||% character()))
          allowed_species <- allowed_species[nzchar(allowed_species)]
          if(!length(allowed_species)) {
            w08_status("Accepted species list is unavailable; named-species assignment is disabled.")
            return(FALSE)
          }
          vals <- as.character(input[[paste0("w08_species_",j)]] %||% character())
          vals <- vals[nzchar(vals)]
          if(!length(vals)) {w08_status("Select at least one named species.");return(FALSE)}
          if(any(!vals %in% allowed_species)) {
            w08_status("Named species must be selected from the accepted project species list.")
            return(FALSE)
          }
          final_value <- list(included=TRUE,farmed_species=vals)
        } else if(choice=="assign_unspecified_species") {
          final_value <- list(included=TRUE,farmed_species=c("Unspecified species"))
        } else if(choice=="exclude_record") {
          final_value <- list(included=FALSE)
        }
      } else if(typ %in% c("geography_model_failure","geography_unresolved","geography_evidence_unvalidated")) {
        if(choice %in% c("assign_country_set","override_country_set")) {
          iso <- unique(toupper(as.character(input[[paste0("w08_geo_",j)]] %||% character())))
          iso <- iso[nzchar(iso)]
          country <- w08_country_names_for_iso3(iso)
          if(!length(iso) || is.null(country)) {
            w08_status("Select at least one country from the accepted ISO country list.")
            return(FALSE)
          }
          final_value <- list(geography_status="RESOLVED",iso3c=iso,country_names=country)
        } else if(choice=="assign_none") {
          final_value <- list(geography_status="NONE",iso3c=character(),country_names=character())
        } else if(choice=="accept_model") {
          # Dynamic W08 finalisation preserves the existing W06 value for accept_model.
          final_value <- NULL
        }
      } else if(typ=="topic_extreme_disagreement") {
        retained <- av <- issue$automated_value$pathways %||% list()
        retained_ids <- vapply(Filter(function(p)isTRUE(p$retained_for_analysis),retained),function(p)as.character(p$path_id),character(1))
        if(choice=="accept_retained_topics") {
          final_value <- list(included=TRUE,path_ids=retained_ids)
        } else if(choice=="replace_topic_set") {
          opts <- w08_topic_options()
          allowed_topic_ids <- if(length(opts)) {
            unique(vapply(opts,function(x)as.character(x$path_id %||% ""),character(1)))
          } else character()
          allowed_topic_ids <- allowed_topic_ids[nzchar(allowed_topic_ids)]
          vals <- unique(as.character(input[[paste0("w08_topics_",j)]] %||% character()))
          vals <- vals[nzchar(vals)]
          if(!length(allowed_topic_ids)) {
            w08_status("Accepted topic ontology is unavailable; topic replacement is disabled.")
            return(FALSE)
          }
          if(!length(vals)) {
            w08_status("Select at least one replacement topic.")
            return(FALSE)
          }
          if(any(!vals %in% allowed_topic_ids)) {
            w08_status("Replacement topics must be selected from the accepted project ontology.")
            return(FALSE)
          }
          final_value <- list(included=TRUE,path_ids=vals)
        } else if(choice=="exclude_record") {
          final_value <- list(included=FALSE)
        } else if(choice=="no_code") {
          final_value <- list(included=TRUE,path_ids=character())
        }
      } else if(typ=="zero_topic_eligibility_uncertain") {
        if(choice=="include_uncoded") final_value <- list(included=TRUE,path_ids=character())
        if(choice=="exclude_record") final_value <- list(included=FALSE)
      }

      item <- list(
        review_key=paste(rid,typ,sep="::"),
        record_id=rid,
        issue_type=typ,
        issue_state_sha256=as.character(issue$issue_state_sha256 %||% ""),
        decision=choice,
        final_value=final_value,
        rationale="Adjudicated in combined Workflow 08 Shiny review",
        reviewer=session_reviewer_id(),
        resolved_at_utc=now,
        queue_sha256=w08_queue_sha_rv()
      )
      issue_decisions[[j]] <- item
    }

    current <- w08_decisions()
    prior <- NULL
    if(length(current)) {
      hits <- Filter(function(x)identical(as.character(x$record_id %||% ""),rid),current)
      if(length(hits)) prior <- hits[[1L]]
    }
    case_sha <- as.character(w08_case_sha_rv()[[rid]] %||% "")
    decision <- list(
      record_id=rid,
      queue_sha256=w08_queue_sha_rv(),
      record_case_sha256=case_sha,
      issue_decisions_json=jsonlite::toJSON(issue_decisions,auto_unbox=TRUE,null="null",na="null",digits=NA),
      reviewer=session_reviewer_id(),
      resolved_at_utc=now
    )
    saved <- tryCatch(
      append_sheet_w08_decision(decision,prior_decision=prior),
      error=function(e){w08_status(paste("Save failed:",conditionMessage(e)));NULL}
    )
    if(is.null(saved)) return(FALSE)

    remaining <- Filter(function(x)!identical(as.character(x$record_id %||% ""),rid),current)
    w08_decisions(c(remaining,list(saved)))
    w08_status(sprintf("Saved record at %s",format(Sys.time(),"%H:%M:%S")))
    TRUE
  }

  advance_w08 <- function() {
    unresolved <- w08_unresolved_indices()
    if(!length(unresolved)) {
      if(session_can("control_workflows")) {
        mark_review_complete("08",w08_batch_id_rv(),w08_queue_sha_rv(),w08_batch_status_rv)
        dispatched <- tryCatch({
          dispatch_w08_resume(w08_source_run_id_rv(),w08_batch_id_rv(),w08_queue_sha_rv())
          TRUE
        }, error=function(e){
          w08_status(paste("Review complete, but W08 resume dispatch failed:",conditionMessage(e)))
          FALSE
        })
        if(dispatched) w08_status("Review complete. Workflow 08 resume dispatched.")
      } else {
        w08_status("Review complete. Awaiting an administrator to resume Workflow 08.")
      }
      app_view("tasks")
      return(invisible(TRUE))
    }
    later <- unresolved[unresolved>w08_idx()]
    w08_idx(if(length(later)) later[[1L]] else unresolved[[1L]])
    invisible(TRUE)
  }

  save_choice <- function(choice) {
    req(authenticated())
    if(!session_can("adjudicate_assigned")) {
      status("You do not have permission to adjudicate records.")
      return(FALSE)
    }
    z <- current_case()

    if (
      session_can("manage_assignments") &&
      !user_has_active_assignment(
        assignment_registry_rv(), "01", batch_id_rv(), "deduplication",
        as.character(z$review_case_id), session_reviewer_id()
      )
    ) {
      ensure_direct_assignment(
        "01", batch_id_rv(), "deduplication",
        z, as.character(z$review_case_id), w01_active_assignment_events()
      )
    }

    current <- decisions()
    prior <- NULL
    if (length(current)) {
      hits <- Filter(function(x) identical(as.character(x$review_case_id %||% ""), as.character(z$review_case_id)), current)
      if (length(hits)) prior <- hits[[1L]]
    }

    decision <- list(
      review_case_id = z$review_case_id,
      decision = choice,
      rationale = "Adjudicated in Shiny",
      reviewer = session_reviewer_id(),
      resolved_at_utc = format(Sys.time(), tz="UTC", format="%Y-%m-%dT%H:%M:%SZ"),
      queue_sha256 = queue_sha_rv()
    )

    saved <- tryCatch(
      save_active_decision(decision, decision_path, prior_decision = prior),
      error = function(e) {
        status(paste("Save failed:", conditionMessage(e)))
        NULL
      }
    )
    if (is.null(saved)) return(FALSE)

    remaining <- Filter(
      function(x) !identical(as.character(x$review_case_id %||% ""), as.character(z$review_case_id)),
      current
    )
    decisions(c(remaining, list(saved)))
    event_version <- suppressWarnings(as.integer(saved$version %||% NA_integer_))
    status(sprintf(
      "Saved %s%s at %s",
      choice,
      if (is.na(event_version)) "" else paste0(" · event v", event_version),
      format(Sys.time(), "%H:%M:%S")
    ))
    TRUE
  }

  advance_after_save <- function() {
    unresolved <- unresolved_indices()
    if (!length(unresolved)) {
      assignments_active <- assignment_mode_active(
        assignment_registry_rv(),
        "01",
        batch_id_rv(),
        task_type = "deduplication"
      )
      if (assignments_active && !w01_all_assignments_complete()) {
        status("Your assigned review is complete. Waiting for other assigned reviewers.")
      } else if(session_can("control_workflows")) {
        mark_review_complete("01",batch_id_rv(),queue_sha_rv(),batch_status_rv)
      } else {
        status("Review complete. Awaiting an administrator to continue Workflow 01.")
      }
      complete(TRUE)
      return(invisible(TRUE))
    }

    later <- unresolved[unresolved > idx()]
    idx(if (length(later)) later[[1L]] else unresolved[[1L]])
    invisible(TRUE)
  }

  observeEvent(input$open_w01, {
    complete(FALSE)
    unresolved <- unresolved_indices()
    if (length(unresolved)) idx(unresolved[[1L]])
    app_view("w01")
  })

  observeEvent(input$open_w02, {
    unresolved <- w02_unresolved_indices()
    if (length(unresolved)) w02_idx(unresolved[[1L]])
    app_view("w02")
  })

  observeEvent(input$back_to_tasks_w02, app_view("tasks"))

  observeEvent(input$open_w04, {
    unresolved <- w04_unresolved_indices()
    if(length(unresolved)) w04_idx(unresolved[[1L]])
    app_view("w04")
  })

  observeEvent(input$back_to_tasks_w04, app_view("tasks"))
  observeEvent(input$open_w04_resolution, {
    unresolved<-w04_resolution_unresolved_indices()
    if(length(unresolved))w04_resolution_idx(unresolved[[1L]])
    app_view("w04_resolution")
  })
  observeEvent(input$back_to_tasks_w04_resolution, app_view("tasks"))
  observeEvent(input$w04_resolution_retain, {if(save_w04_resolution_choice("retain"))advance_w04_resolution()})
  observeEvent(input$w04_resolution_exclude, {if(save_w04_resolution_choice("exclude"))advance_w04_resolution()})
  observeEvent(input$w04_resolution_previous, if(w04_resolution_idx()>1L)w04_resolution_idx(w04_resolution_idx()-1L))
  observeEvent(input$w04_resolution_next, if(w04_resolution_idx()<length(w04_resolution_cases_rv()))w04_resolution_idx(w04_resolution_idx()+1L))
  observeEvent(input$open_w04_conflict, {
    unresolved <- w04_conflict_unresolved_indices()
    if(length(unresolved)) w04_conflict_idx(unresolved[[1L]])
    app_view("w04_conflict")
  })
  observeEvent(input$back_to_tasks_w04_conflict, app_view("tasks"))
  observeEvent(input$w04_conflict_retain, {
    if(save_w04_conflict_choice("retain")) advance_w04_conflict()
  })
  observeEvent(input$w04_conflict_exclude, {
    if(save_w04_conflict_choice("exclude")) advance_w04_conflict()
  })
  observeEvent(input$w04_conflict_previous, {
    if(w04_conflict_idx()>1L) w04_conflict_idx(w04_conflict_idx()-1L)
  })
  observeEvent(input$w04_conflict_next, {
    if(w04_conflict_idx()<length(w04_active_conflict_cases())) w04_conflict_idx(w04_conflict_idx()+1L)
  })
  observeEvent(input$open_w08, {
    unresolved <- w08_unresolved_indices()
    if(length(unresolved)) w08_idx(unresolved[[1L]])
    app_view("w08")
  })
  observeEvent(input$back_to_tasks_w08, app_view("tasks"))
  observeEvent(input$w08_save, {
    if(save_w08_record()) advance_w08()
  })
  observeEvent(input$w08_previous, if(w08_idx()>1L) w08_idx(w08_idx()-1L))
  observeEvent(input$w08_next, if(w08_idx()<length(w08_cases_rv())) w08_idx(w08_idx()+1L))
  observeEvent(input$w04_retain, {
    if(save_w04_choice("retain")) advance_w04()
  })
  observeEvent(input$w04_exclude, {
    if(save_w04_choice("exclude")) advance_w04()
  })
  observeEvent(input$w04_uncertain, {
    if(save_w04_choice("uncertain")) advance_w04()
  })
  observeEvent(input$w04_previous, if(w04_idx()>1L) w04_idx(w04_idx()-1L))
  observeEvent(input$w04_next, if(w04_idx()<length(w04_cases_rv())) w04_idx(w04_idx()+1L))

  observeEvent(input$w02_accept_field, {
    if (save_w02_choice("accept_provider_field")) advance_w02()
  })
  observeEvent(input$w02_reject_field, {
    if (save_w02_choice("reject_provider_field")) advance_w02()
  })
  observeEvent(input$w02_reject_match, {
    if (save_w02_choice("reject_provider_match")) advance_w02()
  })
  observeEvent(input$w02_uncertain, {
    if (save_w02_choice("uncertain")) advance_w02()
  })
  observeEvent(input$w02_previous, if (w02_idx()>1L) w02_idx(w02_idx()-1L))
  observeEvent(input$w02_next, if (w02_idx()<length(w02_cases_rv())) w02_idx(w02_idx()+1L))

  observeEvent(input$back_to_tasks, {
    complete(FALSE)
    app_view("tasks")
  })

  observeEvent(input$back_to_tasks_complete, {
    complete(FALSE)
    app_view("tasks")
  })

  observeEvent(input$duplicate, {
    if (save_choice("duplicate")) advance_after_save()
  })
  observeEvent(input$not_duplicate, {
    if (save_choice("not_duplicate")) advance_after_save()
  })
  observeEvent(input$uncertain, {
    if (save_choice("uncertain")) advance_after_save()
  })
  observeEvent(input$previous, if (idx()>1L) idx(idx()-1L))
  observeEvent(input[["next"]], if (idx()<length(cases_rv())) idx(idx()+1L))
  observeEvent(input$review_last, {
    complete(FALSE)
    idx(length(cases_rv()))
  })

  output$save_status <- renderText(status())
}

shinyApp(ui, server)
