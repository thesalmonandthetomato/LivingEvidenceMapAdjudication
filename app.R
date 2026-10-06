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
source("R/w04_validation_lifecycle.R", local = TRUE)
source("R/w04_kappa_registry.R", local = TRUE)
source("R/w04_human_kappa_registry.R", local = TRUE)
source("R/storage_local.R", local = TRUE)
source("R/storage_sheets.R", local = TRUE)
source("R/storage_backend.R", local = TRUE)
source("R/auth.R", local = TRUE)
source("R/github_dispatch.R", local = TRUE)

queue_path <- Sys.getenv("LEM_W01_QUEUE", unset = "fixtures/w01_real_sample_2.jsonl")
decision_path <- Sys.getenv("LEM_W01_DECISIONS", unset = "local_state/w01_decisions.jsonl")
w01_repair_path <- Sys.getenv("LEM_W01_REPAIRS", unset = "local_state/w01_repairs.jsonl")
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

google_scholar_title_url <- function(title) {
  title <- normalise_display_text(title)
  if (!nzchar(title)) return("")
  query <- gsub("[[:punct:]]+", "", title)
  query <- gsub("[[:space:]]+", " ", trimws(query))
  if (!nzchar(query)) return("")
  paste0(
    "https://scholar.google.co.uk/scholar?start=0&q=",
    gsub(" ", "+", query, fixed=TRUE)
  )
}

google_scholar_button <- function(title) {
  url <- google_scholar_title_url(title)
  if (!nzchar(url)) return(NULL)
  tags$a(
    href=url,
    target="_blank",
    rel="noopener noreferrer",
    title="Search this title on Google Scholar",
    `aria-label`="Search this title on Google Scholar",
    class="btn btn-sm btn-outline-secondary d-inline-flex align-items-center justify-content-center ms-2 flex-shrink-0",
    style="width:30px;height:30px;padding:3px;",
    tags$img(
      src="https://scholar.google.com/favicon.ico",
      alt="Google Scholar",
      style="width:18px;height:18px;display:block;"
    )
  )
}

display_sentence_case_if_all_caps <- function(x) {
  x <- normalise_display_text(x)
  if (!nzchar(x)) return(x)
  letters <- gsub("[^[:alpha:]]", "", x)
  if (!nzchar(letters) || !identical(letters, toupper(letters))) return(x)

  y <- tolower(x)
  chars <- strsplit(y, "", fixed = TRUE)[[1L]]
  capitalise_next <- TRUE
  for (i in seq_along(chars)) {
    ch <- chars[[i]]
    if (capitalise_next && grepl("[[:alpha:]]", ch)) {
      chars[[i]] <- toupper(ch)
      capitalise_next <- FALSE
    }
    if (ch %in% c(".", "!", "?")) {
      capitalise_next <- TRUE
    }
  }
  paste0(chars, collapse = "")
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
  "culture",
  "cultured",
  "culturing",
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
      before <- if (s > 1L) substr(hay, s - 1L, s - 1L) else ""
      after <- if (e < nchar(hay)) substr(hay, e + 1L, e + 1L) else ""
      starts_word <- grepl("^[[:alnum:]]", needle)
      ends_word <- grepl("[[:alnum:]]$", needle)
      left_ok <- !starts_word || !nzchar(before) || !grepl("[[:alnum:]]", before)
      right_ok <- !ends_word || !nzchar(after) || !grepl("[[:alnum:]]", after)
      if (left_ok && right_ok) {
        k <- k + 1L
        candidates[[k]] <- list(start=s,end=e,class=names(terms)[[i]],length=nchar(term))
      }
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

normalise_doi_value <- function(x) {
  z <- gsub("[[:space:]]+", "", as.character(x %||% ""))
  z <- sub("^https?://(dx\\.)?doi\\.org/", "", z, ignore.case=TRUE)
  z <- sub("^doi:", "", z, ignore.case=TRUE)
  trimws(z)
}

normalise_source_id_value <- function(x) {
  gsub("[[:space:]]+", "", as.character(x %||% ""))
}

doi_link <- function(x, label = NULL) {
  doi <- normalise_doi_value(x)
  if (!nzchar(doi)) return("")
  shown <- if (is.null(label)) doi else label
  tags$a(
    href = paste0("https://doi.org/", doi),
    target = "_blank",
    rel = "noopener noreferrer",
    shown
  )
}

record_card <- function(rec, label, fields, side = c("a","b"), abstract_editing = FALSE) {
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
        tags$dt("DOI"), tags$dd(doi_link(rec$doi, fields$doi[[side]])),
        tags$dt("Source ID"), tags$dd(fields$source_record_id[[side]])
      ),
      tags$hr(class = "record-divider"),
      tags$h6(class = "abstract-heading", "Abstract"),
      if (isTRUE(abstract_editing)) {
        tagList(
          textAreaInput(
            paste0("w01_abstract_", side),
            NULL,
            value = as.character(rec$display_abstract %||% rec$abstract %||% ""),
            rows = 6,
            width = "100%"
          ),
          div(
            class = "d-flex align-items-center gap-2 flex-wrap",
            actionButton(
              paste0("save_w01_abstract_", side),
              "Save abstract",
              class = "btn-primary btn-sm"
            ),
            actionButton(
              paste0("cancel_w01_abstract_", side),
              "Cancel",
              class = "btn-outline-secondary btn-sm"
            )
          )
        )
      } else {
        tagList(
          div(class = "abstract-text", fields$abstract[[side]]),
          div(
            class = "d-flex align-items-center gap-2 flex-wrap mt-2",
            actionButton(
              paste0("edit_w01_abstract_", side),
              "Edit abstract",
              class = "btn-outline-secondary btn-sm"
            ),
            actionButton(
              paste0("delete_w01_abstract_", side),
              "Delete abstract",
              class = "btn-outline-danger btn-sm"
            ),
            if (isTRUE(rec$abstract_repair_saved)) {
              tags$span(class = "text-success small", "Corrected abstract saved.")
            }
          )
        )
      }
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
          let busyEligibleUntil = 0;

          // Only show the blocking overlay for an explicit user action that
          // remains busy long enough to warrant feedback. Background reactive
          // refreshes must not interrupt screening or administration work.
          document.addEventListener('click', function(ev) {
            if (ev.target.closest('button, .action-button, .btn')) {
              busyEligibleUntil = Date.now() + 10000;
            }
          }, true);

          $(document).on('shiny:busy', function() {
            clearTimeout(busyTimer);
            if (Date.now() > busyEligibleUntil) return;
            busyTimer = setTimeout(function() {
              if (Date.now() <= busyEligibleUntil) {
                overlay.classList.add('is-visible');
              }
            }, 900);
          });
          $(document).on('shiny:idle', function() {
            clearTimeout(busyTimer);
            busyEligibleUntil = 0;
            overlay.classList.remove('is-visible');
          });

          document.addEventListener('click', function(ev) {
            const drill = ev.target.closest('.lem-drill-number');
            if (drill) {
              ev.preventDefault();
              Shiny.setInputValue('record_table_open', {
                workflow: drill.dataset.workflow || '',
                task_type: drill.dataset.taskType || '',
                batch_id: drill.dataset.batchId || '',
                metric: drill.dataset.metric || 'cases',
                user_id: drill.dataset.userId || '',
                label: drill.dataset.label || ''
              }, {priority:'event'});
              return;
            }
            const toggle = ev.target.closest('.lem-detail-toggle');
            if (toggle) {
              ev.preventDefault();
              Shiny.setInputValue('record_table_toggle', {
                case_id: toggle.dataset.caseId || '',
                detail: toggle.dataset.detail || ''
              }, {priority:'event'});
            }
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
    .lem-drill-number {
      appearance:none; border:0; background:transparent; padding:0; margin:0;
      font:inherit; font-weight:inherit; color:inherit; line-height:inherit;
      cursor:pointer;
    }
    .lem-drill-number:hover, .lem-drill-number:focus-visible { text-decoration:underline; }
    .lem-record-table-wrap { overflow-x:auto; border:1px solid #dde3e8; border-radius:8px; background:#fff; }
    .lem-record-table { width:100%; border-collapse:collapse; font-size:.92rem; }
    .lem-record-table th, .lem-record-table td { padding:.55rem .65rem; border-bottom:1px solid #e8ecef; vertical-align:top; text-align:left; }
    .lem-record-table th { background:#f7f8fa; white-space:nowrap; }
    .lem-record-table tr:last-child td { border-bottom:0; }
    .lem-record-cell { min-width:360px; max-width:620px; }
    .lem-detail-toggle {
      appearance:none; border:0; background:transparent; padding:0; margin:0;
      color:inherit; cursor:pointer; text-align:left; font:inherit;
    }
    .lem-detail-toggle:hover, .lem-detail-toggle:focus-visible { text-decoration:underline; }
    .lem-detail-row td { background:#fbfcfd; padding:.8rem 1rem 1rem 1rem; }
    .lem-detail-text { max-width:1050px; line-height:1.45; white-space:normal; }
    .lem-note-entry + .lem-note-entry { margin-top:.75rem; padding-top:.75rem; border-top:1px solid #e6eaed; }
    .lem-table-toolbar { display:flex; gap:.75rem; align-items:end; flex-wrap:wrap; margin-bottom:.8rem; }
    .lem-table-toolbar .form-group { margin-bottom:0; }
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

    /* Workflow colour system: muted accents shared by admin and task cards. */
    .wf-w01 { --wf-accent:#5d7896; --wf-tint:#edf3f8; --wf-soft:#f6f9fb; }
    .wf-w02 { --wf-accent:#4f817b; --wf-tint:#eaf3f1; --wf-soft:#f5f9f8; }
    .wf-w04 { --wf-accent:#a47a43; --wf-tint:#f6efe5; --wf-soft:#fbf8f3; }
    .wf-w08 { --wf-accent:#7b678d; --wf-tint:#f1edf5; --wf-soft:#f8f6fa; }

    .workflow-card {
      border-left:4px solid var(--wf-accent);
    }
    .workflow-card > .card-header {
      background:var(--wf-tint);
      border-bottom-color:color-mix(in srgb,var(--wf-accent) 22%, #e1e5e9);
    }
    .workflow-card .task-badge {
      background:var(--wf-tint);
      color:color-mix(in srgb,var(--wf-accent) 82%, #17212b);
    }
    .task-kpis { display:grid; grid-template-columns:repeat(3,minmax(90px,1fr)); gap:.65rem; margin:.8rem 0; }
    .task-kpi { background:#f7f8fa; border:1px solid #e1e5e9; border-radius:8px; padding:.55rem .65rem; }
    .task-kpi strong { display:block; font-size:1.15rem; }
    .task-badge { background:#eef3f1; border-radius:999px; padding:.2rem .55rem; font-size:.78rem; }
    .workflow-card .task-badge.workflow-complete-badge { background:#d9f2df !important; color:#145c2e !important; border:1px solid #9fd2ad; font-weight:600; }
    .decision-badge-include { background:#dff3e8 !important; color:#1f6b46 !important; border:1px solid #a8d9be; }
    .decision-badge-exclude { background:#f8e0e0 !important; color:#9d2f2f !important; border:1px solid #e4adad; }
    .decision-badge-neutral { background:#eef1f4 !important; color:#5f6973 !important; border:1px solid #d7dde2; }
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
    .workflow-section {
      border-left:4px solid var(--wf-accent);
      background:var(--wf-soft);
      border-radius:8px;
      margin:.45rem 0;
      padding:.55rem .75rem .45rem .75rem;
    }
    .workflow-section > summary {
      background:var(--wf-tint);
      margin:-.55rem -.75rem .3rem -.75rem;
      padding:.55rem .75rem;
      border-radius:8px 8px 0 0;
    }
    .assignment-submenu {
      background:color-mix(in srgb,var(--wf-tint) 55%, white);
      border:1px solid color-mix(in srgb,var(--wf-accent) 18%, #e7eaed);
      border-left:3px solid color-mix(in srgb,var(--wf-accent) 60%, white);
      border-radius:7px;
      margin:.55rem 0;
      padding:.45rem .65rem;
    }
    .assignment-submenu > summary {
      font-weight:600;
    }
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
    .pipeline-kpi-value-row { display:flex; align-items:baseline; gap:.42rem; flex-wrap:nowrap; white-space:nowrap; }
    .pipeline-kpi-value { display:block; font-size:1.12rem; line-height:1.2; font-weight:700; white-space:nowrap; }
    .pipeline-kpi-value.compact { font-size:1rem; }
    .pipeline-kpi-inline-note { display:block; color:#7c858d; font-size:.7rem; line-height:1.2; font-weight:600; white-space:nowrap; }
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


read_authoritative_w08_metrics <- function(
  registry_url = Sys.getenv(
    "LEM_W08_REGISTRY_URL",
    unset = "https://raw.githubusercontent.com/thesalmonandthetomato/LivingEvidenceMap/workflow01-final-architecture/docs/workflow08/zenodo_registry.csv"
  ),
  pointer_base = Sys.getenv(
    "LEM_W08_POINTER_BASE",
    unset = "https://raw.githubusercontent.com/thesalmonandthetomato/LivingEvidenceMap/workflow01-final-architecture/docs/workflow08/zenodo"
  )
) {
  reg <- utils::read.csv(registry_url, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c("source_run_id", "status")
  if (!all(required %in% names(reg))) stop("W08 registry contract mismatch", call. = FALSE)

  hit <- reg[tolower(trimws(as.character(reg$status))) == "authoritative", , drop = FALSE]
  if (nrow(hit) != 1L) stop("Expected exactly one authoritative W08 registry row", call. = FALSE)

  run_id <- trimws(as.character(hit$source_run_id[[1L]]))
  if (!grepl("^[0-9]+$", run_id)) stop("Invalid authoritative W08 source run ID", call. = FALSE)

  pointer_name <- paste0("run-", run_id, ".json")
  pointer <- if (grepl("^https?://", pointer_base)) {
    paste0(sub("/$", "", pointer_base), "/", pointer_name)
  } else {
    file.path(pointer_base, pointer_name)
  }

  x <- jsonlite::fromJSON(pointer, simplifyVector = FALSE)
  pointer_run <- as.character(x$source_github_run_id %||% "")
  n <- suppressWarnings(as.integer(x$canonical_records %||% NA_integer_))
  if (!identical(pointer_run, run_id)) stop("Authoritative W08 pointer/run mismatch", call. = FALSE)
  if (is.na(n) || n < 1L) stop("Authoritative W08 canonical count is invalid", call. = FALSE)

  list(
    canonical_records = n,
    source_run_id = run_id,
    doi = as.character(x$doi %||% "")
  )
}

w08_status_is_final <- function(p) {
  done <- suppressWarnings(as.integer(as.character((p %||% list())$completed_through %||% "")))
  !is.na(done) && done >= 9L
}

server <- function(input, output, session) {
  authenticated <- reactiveVal(FALSE)
  current_user <- reactiveVal(NULL)
  user_registry_rv <- reactiveVal(list())
  assignment_registry_rv <- reactiveVal(list())
  assignment_manage_status <- reactiveVal("")
  test_queue_status <- reactiveVal("")
  w08_fresh_test_status <- reactiveVal("")
  w01_all_cases_rv <- reactiveVal(list())
  w01_repairs_rv <- reactiveVal(list())
  w01_abstract_edit_rv <- reactiveVal(NULL)
  w01_abstract_delete_rv <- reactiveVal(NULL)
  w01_export_requested_rv <- reactiveVal(FALSE)
  w01_export_status_rv <- reactiveVal("")
  app_view <- reactiveVal("tasks")
  record_table_context <- reactiveVal(NULL)
  record_table_page <- reactiveVal(1L)
  record_table_abstract_open <- reactiveVal(character())
  record_table_notes_open <- reactiveVal(character())
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
  w02_resume_requested_rv <- reactiveVal(FALSE)

  w04_all_cases_rv <- reactiveVal(list())
  w04_cases_rv <- reactiveVal(NULL)
  w04_queue_sha_rv <- reactiveVal("")
  w04_batch_id_rv <- reactiveVal("")
  w04_idx <- reactiveVal(1L)
  w04_decisions <- reactiveVal(list())
  w04_screening_notes_rv <- reactiveVal(list())
  w04_note_status <- reactiveVal("")
  w04_batch_status_rv <- reactiveVal("")
  w04_status <- reactiveVal("")
  w04_include_terms <- reactiveVal(character())
  w04_exclude_terms <- reactiveVal(character())
  w04_consistency_analyses_rv <- reactiveVal(list())
  w04_conflict_sets_rv <- reactiveVal(list())
  w04_kappa_registry_rv <- reactiveVal(w04_empty_kappa_registry())
  w04_human_kappa_registry_rv <- reactiveVal(w04_empty_human_kappa_registry())
  w04_pending_human_kappa_delete <- reactiveVal("")
  w04_consistency_history_loaded <- reactiveVal(FALSE)
  w04_consistency_status <- reactiveVal("")
  w04_refresh_status <- reactiveVal("")
  w04_validation_finalize_requested_rv <- reactiveVal(FALSE)

  w04_resolution_all_cases_rv <- reactiveVal(list())
  w04_resolution_cases_rv <- reactiveVal(NULL)
  w04_resolution_queue_sha_rv <- reactiveVal("")
  w04_resolution_batch_id_rv <- reactiveVal("")
  w04_resolution_source_run_id_rv <- reactiveVal("")
  w04_resolution_idx <- reactiveVal(1L)
  w04_resolution_decisions <- reactiveVal(list())
  w04_resolution_abstract_edits_rv <- reactiveVal(list())
  w04_resolution_abstract_edit_rv <- reactiveVal(FALSE)
  w04_resolution_batch_status_rv <- reactiveVal("")
  w04_resolution_status <- reactiveVal("")
  w04_resolution_resume_requested_rv <- reactiveVal(FALSE)
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
  w08_resume_requested_rv <- reactiveVal(FALSE)
  pipeline_status_rv <- reactiveVal(NULL)
  manual_screening_rv <- reactiveVal(NULL)
  authoritative_w08_rv <- reactiveVal(NULL)
  backend_reset_status <- reactiveVal("")

  search_scope_string_rv <- reactiveVal("")
  search_scope_status_rv <- reactiveVal("")
  search_scope_request_id_rv <- reactiveVal("")
  search_scope_run_id_rv <- reactiveVal("")
  search_scope_rows_rv <- reactiveVal(NULL)
  search_scope_seen_artifacts_rv <- reactiveVal(character())
  search_scope_job_sources_rv <- reactiveVal(character())
  search_scope_progress_rv <- reactiveVal(list(completed=0L,total=0L,pct=0L))

  observe({
    req(authenticated())
    invalidateLater(30000, session)
    if (!identical(app_view(), "tasks")) return()
    refreshed <- tryCatch(
      if(identical(storage_backend(),"google_sheets")) read_latest_pipeline_status() else NULL,
      error=function(e) NULL
    )
    if(!is.null(refreshed)) {
      pipeline_status_rv(refreshed)
      if (w08_status_is_final(refreshed)) {
        authoritative_w08_rv(tryCatch(
          read_authoritative_w08_metrics(),
          error = function(e) NULL
        ))
      } else {
        authoritative_w08_rv(NULL)
      }
    }
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

  observe({
    req(authenticated())
    invalidateLater(300000, session)
    if (!identical(app_view(), "tasks")) return()
    refreshed <- tryCatch(read_manual_screening_metrics(),error=function(e)e)
    if (inherits(refreshed,"error")) {
      if (session_can("manage_assignments")) {
        w04_consistency_status(paste("Kappa registry integrity error:",conditionMessage(refreshed)))
      }
    } else {
      manual_screening_rv(refreshed)
    }
  })

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
    batch_id <- as.character(w04_resolution_batch_id_rv() %||% "")
    all_cases <- w04_resolution_all_cases_rv() %||% list()
    user <- current_user()
    if (!nzchar(batch_id) || !length(all_cases) || is.null(user)) {
      w04_resolution_cases_rv(list())
      return()
    }
    visible <- cases_for_assignment_user(
      all_cases,
      assignment_registry_rv(),
      "04",
      batch_id,
      user,
      task_type="model_uncertainty",
      active_events=w04_resolution_decisions()
    )
    w04_resolution_cases_rv(visible)
    unresolved <- if(length(visible)) {
      ids <- vapply(visible,function(x)as.character(x$review_case_id %||% ""),character(1))
      which(!ids %in% w04_resolution_decision_ids())
    } else integer()
    current <- suppressWarnings(as.integer(w04_resolution_idx()))
    if (is.na(current) || current < 1L || current > max(1L,length(visible))) {
      w04_resolution_idx(if(length(unresolved)) unresolved[[1L]] else 1L)
    }
  })

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

  w04_decision_signature <- function(xs) {
    if (!length(xs)) return("")
    keys <- vapply(xs,function(x) {
      paste(
        as.character(x$decision_id %||% ""),
        as.character(x$review_case_id %||% ""),
        decision_user_id(x),
        as.character(x$decision %||% ""),
        as.character(x$resolved_at_utc %||% ""),
        sep="|"
      )
    },character(1))
    digest::digest(sort(keys),algo="sha256",serialize=FALSE)
  }

  refresh_w04_admin_state <- function(show_status=FALSE) {
    if (!identical(storage_backend(),"google_sheets")) {
      if (isTRUE(show_status)) w04_refresh_status("Refresh is available with the Google Sheets backend.")
      return(invisible(FALSE))
    }
    if (!nzchar(as.character(w04_batch_id_rv() %||% ""))) {
      if (isTRUE(show_status)) w04_refresh_status("No active W04 batch.")
      return(invisible(FALSE))
    }

    latest_decisions <- tryCatch(active_sheet_w04_decisions(),error=function(e)e)
    latest_assignments <- tryCatch(read_sheet_assignments(create_if_missing=FALSE),error=function(e)e)

    errs <- character()
    if (inherits(latest_decisions,"error")) errs <- c(errs,conditionMessage(latest_decisions))
    if (inherits(latest_assignments,"error")) errs <- c(errs,conditionMessage(latest_assignments))
    if (length(errs)) {
      if (isTRUE(show_status)) w04_refresh_status(paste("Refresh failed:",paste(unique(errs),collapse="; ")))
      return(invisible(FALSE))
    }

    latest_decisions <- w04_filter_batch_decisions(latest_decisions,w04_queue_sha_rv())
    if (!identical(
      w04_decision_signature(latest_decisions),
      w04_decision_signature(w04_decisions())
    )) {
      w04_decisions(latest_decisions)
    }

    if (!identical(
      assignment_registry_signature(latest_assignments),
      assignment_registry_signature(assignment_registry_rv())
    )) {
      assignment_registry_rv(latest_assignments)
    }

    if (isTRUE(show_status)) {
      w04_refresh_status(sprintf(
        "Refreshed at %s · %d W04 decisions loaded",
        format(Sys.time(),"%H:%M:%S"),
        length(latest_decisions)
      ))
    }
    invisible(TRUE)
  }

  observeEvent(input$w04_refresh_status_button,{
    req(authenticated())
    if (!session_can("manage_assignments")) {
      w04_refresh_status("Administrator permission is required.")
      return()
    }
    refresh_w04_admin_state(show_status=TRUE)
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

  w04_selected_human_consistency_scope <- reactive({
    rater_ids <- unique(as.character(input$w04_consistency_raters %||% character()))
    if (length(rater_ids) < 2L || "model" %in% rater_ids) return(NULL)
    scope_type <- as.character(input$w04_consistency_scope %||% "partial")
    if (!scope_type %in% c("partial","full")) scope_type <- "partial"
    w04_human_consistency_scope(
      outcomes=w04_blind_outcomes(),
      assignments=assignment_registry_rv(),
      batch_id=w04_batch_id_rv(),
      rater_ids=rater_ids,
      registry=w04_human_kappa_registry_rv(),
      scope_type=scope_type
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

  fmt_pipeline_delta <- function(x) {
    z <- suppressWarnings(as.numeric(as.character(x %||% "")))
    if(is.na(z)) return(NULL)
    paste0(if(z >= 0) "+" else "−", fmt_pipeline_n(abs(z)))
  }

  fmt_pipeline_pair_delta <- function(a,b) {
    x <- fmt_pipeline_delta(a)
    y <- fmt_pipeline_delta(b)
    if(is.null(x) || is.null(y)) return(NULL)
    paste0(x," / ",y)
  }

  read_manual_screening_metrics <- function() {
    registry <- if (identical(storage_backend(),"google_sheets")) {
      sync_w04_kappa_registry_from_github()
    } else {
      read_github_w04_kappa_registry()
    }
    w04_kappa_registry_rv(registry)
    w04_kappa_registry_summary(registry)
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
    w08_finalised <- w08_status_is_final(p)
    w08_authoritative <- authoritative_w08_rv()
    canonical_current <- w08_finalised &&
      !is.null(w08_authoritative) &&
      !is.na(suppressWarnings(as.integer(w08_authoritative$canonical_records %||% NA_integer_)))
    canonical_value <- if (isTRUE(canonical_current)) {
      w08_authoritative$canonical_records
    } else {
      p$canonical_existing
    }

    workflow_labels <- c("0. Search","1. Dedup","2. Repair","3. Retract","4. Screen","5. Code","6. Country","7. Topics","8. Review","9. Report","10. Dashboard")
    segs <- lapply(seq_along(workflow_labels),function(i){
      cls <- "workflow-segment"
      if(i <= completed) cls <- paste(cls,"done")
      else if(!is.na(active) && i==active) cls <- paste(cls,"active")
      div(class=cls,title=workflow_labels[[i]])
    })

    stage_complete <- function(position) {
      !is.na(completed) && completed >= as.integer(position)
    }

    kpi <- function(label,value,sub=NULL,stage_position=NULL,class_extra=NULL,inline_note=NULL,compact_value=FALSE) {
      stale_class <- if(!is.null(stage_position) && !stage_complete(stage_position)) "pre-update" else NULL
      div(
        class=paste(c("pipeline-kpi", stale_class, class_extra), collapse=" "),
        tags$span(class="pipeline-kpi-label",label),
        div(
          class="pipeline-kpi-value-row",
          tags$span(class=paste(c("pipeline-kpi-value",if(isTRUE(compact_value)) "compact" else NULL),collapse=" "),value),
          if(!is.null(inline_note)) tags$span(class="pipeline-kpi-inline-note",inline_note)
        ),
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
        kpi(
          "Database searching",
          fmt_pipeline_n(p$search_results_total),
          "records",
          stage_position=1L,
          inline_note=if(stage_complete(1L)) fmt_pipeline_delta(p$search_results_update) else NULL
        ),
        kpi(
          "After deduplication",
          fmt_pipeline_n(p$deduplicated_records),
          "records",
          stage_position=2L,
          inline_note=if(stage_complete(2L)) fmt_pipeline_delta(p$deduplicated_update) else NULL
        ),
        kpi(
          "Enriched",
          fmt_pipeline_n(p$enriched_records),
          "records",
          stage_position=3L,
          inline_note=if(stage_complete(3L)) fmt_pipeline_delta(p$enriched_update) else NULL
        ),
        kpi(
          "Retracted",
          fmt_pipeline_n(p$retracted_records),
          "records",
          stage_position=4L,
          inline_note=if(stage_complete(4L)) fmt_pipeline_delta(p$retracted_update) else NULL
        ),
        {
          m <- manual_screening_rv()
          kpi(
            "Manually screened",
            if(is.null(m)) "—" else fmt_pipeline_n(m$manually_screened),
            if(is.null(m) || is.na(m$kappa)) NULL else paste0("κ ",sprintf("%.3f",m$kappa)),
            stage_position=5L
          )
        },
        kpi(
          "Screened",
          paste0(fmt_pipeline_n(p$screened_include)," / ",fmt_pipeline_n(p$screened_exclude)),
          "include / exclude",
          stage_position=5L,
          inline_note=if(stage_complete(5L)) fmt_pipeline_pair_delta(p$screened_include_update,p$screened_exclude_update) else NULL,
          compact_value=TRUE
        ),
        kpi(
          "Species",
          fmt_pipeline_n(p$species_records),
          "records processed",
          stage_position=6L,
          inline_note=if(stage_complete(6L)) fmt_pipeline_delta(p$species_update) else NULL
        ),
        kpi(
          "Geography",
          paste0(fmt_pipeline_n(p$geography_with)," / ",fmt_pipeline_n(p$geography_without)),
          "with / without",
          stage_position=7L,
          inline_note=if(stage_complete(7L)) fmt_pipeline_pair_delta(p$geography_with_update,p$geography_without_update) else NULL,
          compact_value=TRUE
        ),
        kpi(
          "Topics",
          paste0(fmt_pipeline_n(p$topic_with)," / ",fmt_pipeline_n(p$topic_without)),
          "with / without",
          stage_position=8L,
          inline_note=if(stage_complete(8L)) fmt_pipeline_pair_delta(p$topic_with_update,p$topic_without_update) else NULL,
          compact_value=TRUE
        ),
        kpi(
          "Canonical database",
          fmt_pipeline_n(canonical_value),
          if (isTRUE(canonical_current)) "current" else "pre-update",
          stage_position=9L,
          class_extra=if (isTRUE(canonical_current)) NULL else "pre-update"
        )
      ),
      div(class="workflow-line",segs),
      div(
        class="workflow-labels",
        lapply(workflow_labels,tags$span)
      )
    )
  }


  record_table_user_label <- function(user_id) {
    uid <- as.character(user_id %||% "")
    if (!nzchar(uid)) return("")
    u <- find_user_by_id(user_registry_rv(),uid,require_active=FALSE)
    if (is.null(u)) uid else as.character(u$display_name %||% uid)
  }

  record_table_decision_label <- function(x) {
    d <- tolower(trimws(as.character(x$decision %||% "")))
    if (nzchar(d)) {
      return(c(
        duplicate="Duplicate",
        not_duplicate="Not duplicate",
        retain="Include",
        exclude="Exclude",
        uncertain="Unsure",
        accept_provider_field="Accept provider field",
        reject_provider_field="Reject provider field",
        reject_provider_match="Reject provider match"
      )[[d]] %||% gsub("_"," ",d,fixed=TRUE))
    }
    if (nzchar(trimws(as.character(x$issue_decisions_json %||% "")))) return("Annotation saved")
    ""
  }

  record_table_case_id <- function(z) {
    as.character(z$review_case_id %||% z$case_id %||% z$record_id %||% "")
  }

  record_table_source_list <- function() {
    out <- list()
    add <- function(workflow,task_type,batch_id,cases,events) {
      bid <- as.character(batch_id %||% "")
      if (!nzchar(bid) || !length(cases %||% list())) return()
      key <- paste(workflow,task_type,bid,sep="|")
      out[[key]] <<- list(
        workflow=as.character(workflow),
        task_type=as.character(task_type),
        batch_id=bid,
        cases=cases %||% list(),
        events=events %||% list(),
        assignments=active_assignments_for_batch(
          assignment_registry_rv(),workflow,bid,task_type
        ),
        all_assignments=assignments_for_batch(
          assignment_registry_rv(),workflow,bid,task_type
        )
      )
    }
    add("01","deduplication",batch_id_rv(),w01_all_cases_rv(),decisions())
    add("02","enrichment",w02_batch_id_rv(),w02_all_cases_rv(),w02_decisions())
    add("04","manual_screening",w04_batch_id_rv(),w04_all_cases_rv(),w04_decisions())
    add("04","conflict_resolution",w04_active_conflict_batch_id(),w04_all_conflict_cases(),w04_conflict_decisions())
    add("04","model_uncertainty",w04_resolution_batch_id_rv(),w04_resolution_all_cases_rv(),w04_resolution_decisions())
    add("08","annotation",w08_batch_id_rv(),w08_all_cases_rv(),w08_decisions())
    out
  }

  record_table_source <- function(workflow,task_type,batch_id="") {
    xs <- record_table_source_list()
    if (identical(as.character(workflow),"all")) return(xs)
    hits <- Filter(function(x) {
      identical(x$workflow,as.character(workflow)) &&
        identical(x$task_type,as.character(task_type)) &&
        (!nzchar(as.character(batch_id %||% "")) || identical(x$batch_id,as.character(batch_id)))
    },xs)
    hits
  }

  record_table_case_fields <- function(z,workflow,task_type) {
    work_id <- as.character(z$work_id %||% z$record_id %||% z$review_case_id %||% z$case_id %||% "")
    title <- ""
    year <- ""
    doi <- ""
    abstract <- ""

    if (identical(workflow,"01")) {
      a <- z$record_i %||% list()
      b <- z$record_j %||% list()
      one <- function(rec,label) {
        rid <- as.character(rec$work_id %||% rec$record_id %||% rec$source_record_id %||% "")
        yr <- as.character(rec$year %||% "")
        ttl <- display_sentence_case_if_all_caps(rec$title %||% "")
        sprintf("%s: %s%s%s",label,rid,if(nzchar(yr)) paste0(" (",yr,")") else "",if(nzchar(ttl)) paste0(" ",ttl) else "")
      }
      title <- paste(one(a,"A"),one(b,"B"),sep=" / ")
      year <- ""
      dois <- unique(Filter(nzchar,c(normalise_doi_value(a$doi %||% ""),normalise_doi_value(b$doi %||% ""))))
      doi <- paste(dois,collapse="; ")
      abstract <- paste(
        if(nzchar(normalise_display_text(a$abstract %||% ""))) paste0("Record A: ",normalise_display_text(a$abstract)) else "",
        if(nzchar(normalise_display_text(b$abstract %||% ""))) paste0("Record B: ",normalise_display_text(b$abstract)) else "",
        sep=if(nzchar(normalise_display_text(a$abstract %||% "")) && nzchar(normalise_display_text(b$abstract %||% ""))) "\n\n" else ""
      )
    } else if (identical(workflow,"02")) {
      b <- z$canonical %||% list()
      work_id <- as.character(b$work_id %||% b$record_id %||% z$record_id %||% work_id)
      title <- display_sentence_case_if_all_caps(b$title %||% "")
      year <- as.character(b$year %||% "")
      doi <- normalise_doi_value(b$doi %||% z$doi %||% "")
      abstract <- display_sentence_case_if_all_caps(b$abstract %||% "")
    } else if (identical(workflow,"04")) {
      b <- z$bibliographic %||% list()
      work_id <- as.character(z$work_id %||% z$record_id %||% b$work_id %||% work_id)
      title <- display_sentence_case_if_all_caps(b$title %||% z$title %||% "")
      year <- as.character(b$year %||% z$year %||% "")
      doi <- normalise_doi_value(b$doi %||% z$doi %||% "")
      abstract <- display_sentence_case_if_all_caps(b$abstract %||% z$abstract %||% "")
    } else if (identical(workflow,"08")) {
      work_id <- as.character(z$work_id %||% z$record_id %||% work_id)
      title <- display_sentence_case_if_all_caps(z$title %||% "")
      year <- as.character(z$year %||% "")
      doi <- normalise_doi_value(z$doi %||% "")
      abstract <- display_sentence_case_if_all_caps(z$abstract %||% "")
    }

    list(work_id=work_id,title=title,year=year,doi=doi,abstract=abstract)
  }

  record_table_effective_assignments <- function(src) {
    xs <- src$assignments %||% list()
    events <- src$events %||% list()
    mode <- assignment_mode_for(src$workflow,src$task_type)

    resolving_events <- Filter(decision_resolves_case,events)
    if (length(resolving_events)) {
      existing <- src$all_assignments %||% list()
      existing_keys <- if(length(existing)) vapply(existing,function(raw) {
        a <- normalise_assignment_row(raw)
        paste(a$case_id,a$user_id,sep="|")
      },character(1)) else character()
      implicit <- list()
      for (e in resolving_events) {
        cid <- decision_case_id(e)
        uid <- decision_user_id(e)
        key <- paste(cid,uid,sep="|")
        if (!nzchar(cid) || !nzchar(uid) || key %in% existing_keys) next
        implicit[[length(implicit)+1L]] <- list(
          assignment_id=paste0("implicit-table-",substr(digest::digest(
            paste(src$workflow,src$task_type,src$batch_id,cid,uid,sep="|"),
            algo="sha256",serialize=FALSE
          ),1L,24L)),
          workflow=src$workflow,
          task_type=src$task_type,
          batch_id=src$batch_id,
          case_id=cid,
          user_id=uid,
          blind_group="implicit-table",
          status="assigned"
        )
        existing_keys <- c(existing_keys,key)
      }
      xs <- c(xs,implicit)
    }
    if (!length(xs)) return(list())

    lapply(xs,function(raw) {
      a <- normalise_assignment_row(raw)
      if (identical(mode,ASSIGNMENT_MODES[["independent_blind_review"]])) {
        hit <- Filter(function(e) {
          identical(decision_case_id(e),a$case_id) &&
            identical(decision_user_id(e),a$user_id) &&
            decision_resolves_case(e)
        },events)
        a$effective_status <- if(length(hit)) "complete" else "assigned"
      } else {
        resolved <- case_authoritative_event(events,a$case_id)
        a$effective_status <- if(is.null(resolved)) {
          "assigned"
        } else if(identical(decision_user_id(resolved),a$user_id)) {
          "complete"
        } else {
          "resolved_elsewhere"
        }
      }
      a
    })
  }

  record_table_notes_for <- function(case_id,workflow,task_type) {
    if (!identical(workflow,"04") || identical(task_type,"model_uncertainty")) return(list())
    Filter(function(n) {
      identical(as.character(n$review_case_id %||% ""),as.character(case_id)) &&
        nzchar(trimws(as.character(n$note %||% "")))
    },w04_screening_notes_rv() %||% list())
  }

  record_table_rows_for_source <- function(src,metric="cases",user_id="") {
    cases <- src$cases %||% list()
    events <- src$events %||% list()
    eff <- record_table_effective_assignments(src)
    metric <- as.character(metric %||% "cases")
    uid <- as.character(user_id %||% "")

    relevant_assignments <- if (nzchar(uid)) {
      Filter(function(a) identical(a$user_id,uid),eff)
    } else eff

    status_case_ids <- function(status) {
      unique(vapply(
        Filter(function(a) identical(a$effective_status,status),relevant_assignments),
        function(a)a$case_id,
        character(1)
      ))
    }
    assigned_ids <- unique(vapply(relevant_assignments,function(a)a$case_id,character(1)))
    all_ids <- vapply(cases,record_table_case_id,character(1))
    keep_ids <- switch(
      metric,
      cases=all_ids,
      assignments=assigned_ids,
      completed=status_case_ids("complete"),
      closed=status_case_ids("resolved_elsewhere"),
      outstanding=status_case_ids("assigned"),
      unassigned=setdiff(all_ids,unique(vapply(eff,function(a)a$case_id,character(1)))),
      all_ids
    )
    keep_ids <- unique(keep_ids[nzchar(keep_ids)])
    selected <- Filter(function(z) record_table_case_id(z) %in% keep_ids,cases)

    rows <- lapply(selected,function(z) {
      cid <- record_table_case_id(z)
      fields <- record_table_case_fields(z,src$workflow,src$task_type)
      ca <- Filter(function(a) identical(a$case_id,cid),eff)
      ce <- Filter(function(e) identical(decision_case_id(e),cid),events)
      if (nzchar(uid)) {
        ca_view <- Filter(function(a) identical(a$user_id,uid),ca)
        ce_view <- Filter(function(e) identical(decision_user_id(e),uid),ce)
      } else {
        ca_view <- ca
        ce_view <- ce
      }
      assigned_users <- unique(vapply(ca_view,function(a)a$user_id,character(1)))
      assigned_names <- vapply(assigned_users,record_table_user_label,character(1))
      decision_text <- if(length(ce_view)) {
        paste(vapply(ce_view,function(e) {
          who <- record_table_user_label(decision_user_id(e))
          lab <- record_table_decision_label(e)
          if(nzchar(who)) paste0(who,": ",lab) else lab
        },character(1)),collapse="; ")
      } else ""
      statuses <- unique(vapply(ca_view,function(a)a$effective_status,character(1)))
      status <- if(!length(ca_view)) {
        "Unassigned"
      } else if(length(statuses)==1L) {
        c(complete="Completed",assigned="Outstanding",resolved_elsewhere="Closed")[[statuses[[1L]]]] %||% statuses[[1L]]
      } else {
        paste(
          sum(vapply(ca_view,function(a)identical(a$effective_status,"complete"),logical(1))),"completed ·",
          sum(vapply(ca_view,function(a)identical(a$effective_status,"assigned"),logical(1))),"outstanding ·",
          sum(vapply(ca_view,function(a)identical(a$effective_status,"resolved_elsewhere"),logical(1))),"closed"
        )
      }
      last_activity <- ""
      if(length(ce)) {
        times <- vapply(ce,function(e)as.character(e$event_at_utc %||% e$resolved_at_utc %||% ""),character(1))
        times <- times[nzchar(times)]
        if(length(times)) last_activity <- max(times)
      }
      notes <- record_table_notes_for(cid,src$workflow,src$task_type)
      list(
        key=paste(src$workflow,src$task_type,cid,sep="|"),
        case_id=cid,
        workflow=src$workflow,
        task_type=src$task_type,
        work_id=fields$work_id,
        year=fields$year,
        title=fields$title,
        doi=fields$doi,
        abstract=fields$abstract,
        status=status,
        assigned=paste(assigned_names[nzchar(assigned_names)],collapse=", "),
        decisions=decision_text,
        notes=notes,
        last_activity=last_activity
      )
    })
    list(
      rows=rows,
      matched_assignments=length(Filter(function(a) {
        a$case_id %in% keep_ids &&
          (!nzchar(uid) || identical(a$user_id,uid)) &&
          (
            metric %in% c("cases","assignments","unassigned") ||
            identical(
              a$effective_status,
              c(completed="complete",closed="resolved_elsewhere",outstanding="assigned")[[metric]] %||% ""
            )
          )
      },eff))
    )
  }

  record_table_data <- reactive({
    ctx <- record_table_context()
    if (is.null(ctx)) return(list(rows=list(),matched_assignments=0L))
    sources <- record_table_source(ctx$workflow,ctx$task_type,ctx$batch_id)
    parts <- lapply(sources,function(src) {
      record_table_rows_for_source(src,ctx$metric,ctx$user_id)
    })
    list(
      rows=unlist(lapply(parts,`[[`,"rows"),recursive=FALSE),
      matched_assignments=sum(vapply(parts,function(x)as.integer(x$matched_assignments),integer(1)))
    )
  })

  record_table_filtered_rows <- reactive({
    rows <- record_table_data()$rows %||% list()
    q <- tolower(trimws(as.character(input$record_table_search %||% "")))
    if (!nzchar(q) || !length(rows)) return(rows)
    Filter(function(r) {
      hay <- tolower(paste(
        r$work_id,r$year,r$title,r$doi,r$status,r$assigned,r$decisions,
        paste(vapply(r$notes %||% list(),function(n)as.character(n$note %||% ""),character(1)),collapse=" "),
        sep=" "
      ))
      grepl(q,hay,fixed=TRUE)
    },rows)
  })

  record_table_link <- function(value,workflow,task_type,batch_id,metric,user_id="",label="") {
    tags$button(
      type="button",
      class="lem-drill-number",
      `data-workflow`=as.character(workflow),
      `data-task-type`=as.character(task_type),
      `data-batch-id`=as.character(batch_id),
      `data-metric`=as.character(metric),
      `data-user-id`=as.character(user_id),
      `data-label`=as.character(label),
      title="View records",
      as.character(value)
    )
  }

  record_table_preview <- function(text,n=5L) {
    x <- strsplit(normalise_display_text(text),"[[:space:]]+",perl=TRUE)[[1L]]
    x <- x[nzchar(x)]
    if(!length(x)) return("")
    paste(head(x,n),collapse=" ")
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

    if (identical(app_view(), "record_table")) {
      return(div(
        class="app-shell",
        div(
          class="d-flex justify-content-between align-items-center mb-3 gap-3 flex-wrap",
          div(
            tags$h2("Record table",class="mb-0"),
            tags$div(class="text-secondary",uiOutput("record_table_context_label"))
          ),
          div(
            class="d-flex align-items-center gap-2",
            uiOutput("session_identity"),
            actionButton("record_table_back","Back to main page",class="btn-outline-secondary btn-sm")
          )
        ),
        div(
          class="lem-table-toolbar",
          textInput("record_table_search","Search",value="",placeholder="Search citation, ID, reviewer, decision or note"),
          selectInput("record_table_page_size","Rows per page",choices=c("25"=25,"50"=50,"100"=100),selected=50,width="150px")
        ),
        uiOutput("record_table_body"),
        uiOutput("record_table_pager")
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

      w04_resolution_user_total <- length(w04_resolution_cases_rv() %||% list())
      w04_resolution_user_remaining <- if (w04_resolution_user_total) length(w04_resolution_unresolved_indices()) else 0L
      if (session_can("manage_assignments")) {
        w04_resolution_total <- length(w04_resolution_all_cases_rv() %||% list())
        w04_resolution_all_ids <- if (w04_resolution_total) vapply(
          w04_resolution_all_cases_rv(),
          function(x) as.character(x$review_case_id %||% ""),
          character(1)
        ) else character()
        w04_resolution_completed <- sum(w04_resolution_all_ids %in% w04_resolution_decision_ids())
        w04_resolution_remaining <- max(0L, w04_resolution_total - w04_resolution_completed)
      } else {
        w04_resolution_total <- w04_resolution_user_total
        w04_resolution_remaining <- w04_resolution_user_remaining
        w04_resolution_completed <- max(0L, w04_resolution_total - w04_resolution_remaining)
      }

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
        wf_class <- switch(
          workflow,
          "Workflow 01"="wf-w01",
          "Workflow 02"="wf-w02",
          "Workflow 04"="wf-w04",
          "Workflow 08"="wf-w08",
          ""
        )
        card(
          class = paste("task-card h-100 workflow-card", wf_class),
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
              tags$div(class="small mb-2",tags$span(class="task-badge","Human review complete"))
            } else if (identical(lifecycle_status,"consumed")) {
              tags$div(
                class="small mb-2",
                tags$span(class="task-badge workflow-complete-badge","Workflow completed")
              )
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
            tags$h2("Living Evidence Map", class = "mb-1"),
            tags$div("Project management for computer-driven/computer-assisted living evidence maps", class = "text-secondary")
          ),
          uiOutput("session_identity")
        ),
        pipeline_summary_ui(),
        uiOutput("configure_review"),
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
                "No records awaiting review"
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
                "No records awaiting review"
              } else if (w04_conflict_user_remaining == 0L && session_can("manage_assignments")) {
                "Conflicts exist and are awaiting assignment"
              } else if (w04_conflict_user_remaining == 0L) {
                "No conflicts assigned to you"
              } else {
                "No records awaiting review"
              }
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
              w04_resolution_batch_status_rv(),
              can_open = w04_resolution_user_remaining > 0L,
              idle_text = if (!nzchar(w04_resolution_batch_id_rv())) {
                "No records awaiting review"
              } else if (
                w04_resolution_remaining > 0L && session_can("manage_assignments") && w04_resolution_user_remaining == 0L
              ) {
                "Active cases are awaiting assignment"
              } else if (w04_resolution_user_remaining == 0L) {
                "No records assigned to you"
              } else {
                "No records awaiting review"
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
                "No records awaiting review"
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


  observeEvent(input$record_table_open,{
    req(authenticated())
    if (!session_can("manage_assignments")) {
      showNotification("Administrator permission is required to open record tables.",type="error")
      return()
    }
    z <- input$record_table_open
    if (is.null(z)) return()
    workflow <- as.character(z$workflow %||% "")
    task_type <- as.character(z$task_type %||% "")
    batch_id <- as.character(z$batch_id %||% "")
    metric <- as.character(z$metric %||% "cases")
    allowed_metrics <- c("cases","assignments","completed","closed","outstanding","unassigned")
    if (!metric %in% allowed_metrics) return()
    sources <- record_table_source(workflow,task_type,batch_id)
    if (!length(sources)) {
      showNotification("No active records are available for this table view.",type="warning")
      return()
    }
    record_table_context(list(
      workflow=workflow,
      task_type=task_type,
      batch_id=batch_id,
      metric=metric,
      user_id=as.character(z$user_id %||% ""),
      label=as.character(z$label %||% "")
    ))
    record_table_page(1L)
    record_table_abstract_open(character())
    record_table_notes_open(character())
    app_view("record_table")
  })

  observeEvent(input$record_table_back,{
    record_table_context(NULL)
    record_table_page(1L)
    record_table_abstract_open(character())
    record_table_notes_open(character())
    app_view("tasks")
  })

  observeEvent(input$record_table_toggle,{
    z <- input$record_table_toggle
    key <- as.character(z$case_id %||% "")
    detail <- as.character(z$detail %||% "")
    if (!nzchar(key)) return()
    if (identical(detail,"abstract")) {
      cur <- record_table_abstract_open()
      record_table_abstract_open(if(key %in% cur) setdiff(cur,key) else c(cur,key))
    } else if (identical(detail,"notes")) {
      cur <- record_table_notes_open()
      record_table_notes_open(if(key %in% cur) setdiff(cur,key) else c(cur,key))
    }
  })

  observeEvent(input$record_table_search,{
    record_table_page(1L)
  },ignoreInit=TRUE)

  observeEvent(input$record_table_page_size,{
    record_table_page(1L)
  },ignoreInit=TRUE)

  observeEvent(input$record_table_prev,{
    record_table_page(max(1L,record_table_page()-1L))
  })

  observeEvent(input$record_table_next,{
    size <- suppressWarnings(as.integer(input$record_table_page_size %||% 50L))
    if (is.na(size) || size < 1L) size <- 50L
    total <- length(record_table_filtered_rows())
    pages <- max(1L,ceiling(total/size))
    record_table_page(min(pages,record_table_page()+1L))
  })

  output$record_table_context_label <- renderUI({
    ctx <- record_table_context()
    if (is.null(ctx)) return(NULL)
    metric_label <- c(
      cases="Cases",
      assignments="Assignments",
      completed="Completed",
      closed="Closed",
      outstanding="Outstanding",
      unassigned="Unassigned cases"
    )[[ctx$metric]] %||% ctx$metric
    who <- if(nzchar(ctx$user_id)) paste0(" · ",record_table_user_label(ctx$user_id)) else ""
    label <- if(nzchar(ctx$label)) ctx$label else paste0(
      if(identical(ctx$workflow,"all")) "All workflows" else paste0("W",ctx$workflow),
      if(nzchar(ctx$task_type) && !identical(ctx$task_type,"all")) paste0(" · ",gsub("_"," ",ctx$task_type,fixed=TRUE)) else "",
      " · ",metric_label,who
    )
    rows <- length(record_table_filtered_rows())
    matched <- record_table_data()$matched_assignments
    suffix <- if(ctx$metric %in% c("assignments","completed","closed","outstanding") && matched != rows) {
      sprintf(" · %d assignments across %d records",matched,rows)
    } else {
      sprintf(" · %d record%s",rows,if(rows==1L)"" else "s")
    }
    tags$span(label,suffix)
  })

  output$record_table_body <- renderUI({
    rows <- record_table_filtered_rows()
    if (!length(rows)) {
      return(tags$div(class="p-3 border rounded bg-white text-secondary","No records match this view."))
    }
    size <- suppressWarnings(as.integer(input$record_table_page_size %||% 50L))
    if (is.na(size) || size < 1L) size <- 50L
    pages <- max(1L,ceiling(length(rows)/size))
    page <- min(max(1L,record_table_page()),pages)
    if (!identical(page,record_table_page())) record_table_page(page)
    idx <- seq.int((page-1L)*size+1L,min(page*size,length(rows)))
    shown <- rows[idx]

    render_record <- function(r) {
      citation <- tagList(
        tags$span(class="fw-semibold",as.character(r$work_id %||% "")),
        if(nzchar(r$year)) tags$span(paste0(" (",r$year,") ")) else " ",
        tags$span(as.character(r$title %||% "")),
        if(nzchar(r$doi)) tagList(
          tags$span(". "),
          tags$a(
            href=paste0("https://doi.org/",normalise_doi_value(r$doi)),
            target="_blank",rel="noopener noreferrer",
            normalise_doi_value(r$doi)
          )
        ) else NULL
      )
      abstract_open <- r$key %in% record_table_abstract_open()
      notes_open <- r$key %in% record_table_notes_open()
      abstract_preview <- record_table_preview(r$abstract,5L)
      note_count <- length(r$notes %||% list())

      main <- tags$tr(
        tags$td(class="lem-record-cell",citation),
        tags$td(
          if(nzchar(abstract_preview)) tags$button(
            type="button",class="lem-detail-toggle",
            `data-case-id`=r$key,`data-detail`="abstract",
            paste0(abstract_preview,"… ",if(abstract_open)"▴" else "▾")
          ) else tags$span(class="text-secondary","No abstract")
        ),
        tags$td(r$status),
        tags$td(if(nzchar(r$assigned)) r$assigned else tags$span(class="text-secondary","—")),
        tags$td(if(nzchar(r$decisions)) r$decisions else tags$span(class="text-secondary","—")),
        tags$td(
          if(note_count) tags$button(
            type="button",class="lem-detail-toggle",
            `data-case-id`=r$key,`data-detail`="notes",
            sprintf("%d note%s %s",note_count,if(note_count==1L)"" else "s",if(notes_open)"▴" else "▾")
          ) else tags$span(class="text-secondary","—")
        ),
        tags$td(if(nzchar(r$last_activity)) r$last_activity else tags$span(class="text-secondary","—"))
      )

      detail <- if(abstract_open || notes_open) {
        tags$tr(
          class="lem-detail-row",
          tags$td(
            colspan="7",
            if(abstract_open) div(
              class="lem-detail-text",
              tags$div(class="fw-semibold mb-1","Abstract"),
              tags$div(style="white-space:pre-wrap;",as.character(r$abstract %||% ""))
            ),
            if(notes_open && note_count) div(
              class=paste("lem-detail-text",if(abstract_open)"mt-3 pt-3 border-top" else ""),
              tags$div(class="fw-semibold mb-1","Notes"),
              tagList(lapply(r$notes,function(n) {
                div(
                  class="lem-note-entry",
                  tags$div(class="fw-semibold",record_table_user_label(as.character(n$reviewer %||% ""))),
                  tags$div(as.character(n$note %||% ""))
                )
              }))
            )
          )
        )
      } else NULL
      tagList(main,detail)
    }

    div(
      class="lem-record-table-wrap",
      tags$table(
        class="lem-record-table",
        tags$thead(tags$tr(
          tags$th("Record"),
          tags$th("Abstract"),
          tags$th("Status"),
          tags$th("Assigned reviewer(s)"),
          tags$th("Decision(s)"),
          tags$th("Notes"),
          tags$th("Last activity")
        )),
        tags$tbody(tagList(lapply(shown,render_record)))
      )
    )
  })

  output$record_table_pager <- renderUI({
    rows <- record_table_filtered_rows()
    size <- suppressWarnings(as.integer(input$record_table_page_size %||% 50L))
    if (is.na(size) || size < 1L) size <- 50L
    pages <- max(1L,ceiling(length(rows)/size))
    page <- min(max(1L,record_table_page()),pages)
    div(
      class="d-flex align-items-center justify-content-between gap-2 mt-2 flex-wrap",
      tags$span(class="text-secondary small",sprintf("Page %d of %d",page,pages)),
      div(
        class="d-flex gap-2",
        actionButton("record_table_prev","← Previous",class="btn-outline-secondary btn-sm",disabled=if(page<=1L)NA else NULL),
        actionButton("record_table_next","Next →",class="btn-outline-secondary btn-sm",disabled=if(page>=pages)NA else NULL)
      )
    )
  })

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

  search_scope_pretty_source <- function(slug) {
    slug <- as.character(slug %||% "")
    core <- c(
      lens="Lens",
      scopus="Scopus",
      openalex="OpenAlex",
      agricola="AGRICOLA",
      pubmed="PubMed/MEDLINE",
      ethos="EThOS",
      cba="Chinese Biological Abstracts",
      epmc_preprints="Europe PMC preprints",
      wos="Web of Science Core Collection",
      cab_abstracts="CAB Abstracts",
      proquest_dissertations="ProQuest Dissertations & Theses Global"
    )
    if (slug %in% names(core)) return(unname(core[[slug]]))
    x <- sub("^ebsco_", "", slug)
    tools::toTitleCase(gsub("_", " ", x, fixed=TRUE))
  }

  observe({
    req(authenticated())
    if (!session_can("control_workflows")) return()
    if (nzchar(search_scope_string_rv())) return()
    x <- tryCatch(
      read_github_text_file("user_input/scoping_search_string.txt"),
      error=function(e) structure("", error=conditionMessage(e))
    )
    if (nzchar(as.character(x))) {
      search_scope_string_rv(trimws(as.character(x)))
    } else {
      search_scope_status_rv(paste(
        "Could not load scoping search string:",
        attr(x, "error") %||% "unknown GitHub read error"
      ))
    }
  })

  output$configure_review <- renderUI({
    req(authenticated())
    if (!session_can("control_workflows")) return(NULL)

    running <- nzchar(as.character(search_scope_request_id_rv() %||% ""))
    card(
      class="assignment-summary",
      tags$details(
        class="assignment-disclosure",
        `data-accordion-key`="configure-review",
        tags$summary(
          div(
            class="d-inline-flex flex-wrap align-items-center gap-2 p-3",
            tags$strong("Configure review"),
            tags$span(class="task-badge","Search scoping")
          )
        ),
        div(
          class="px-3 pb-3",
          tags$h6("Search string"),
          tags$pre(
            class="border rounded bg-light p-3 small",
            style="white-space:pre-wrap;overflow-wrap:anywhere;",
            if (nzchar(search_scope_string_rv())) search_scope_string_rv() else "Loading search string…"
          ),
          div(
            class="d-flex align-items-center gap-2 flex-wrap mb-2",
            actionButton(
              "run_search_scope",
              if (running) "Scoping search running…" else "Run scoping search",
              class="btn-primary btn-sm",
              disabled=if (running) NA else NULL
            ),
            tags$span(class="saved-note", search_scope_status_rv())
          ),
          uiOutput("search_scope_progress"),
          uiOutput("search_scope_table")
        )
      )
    )
  })

  output$search_scope_progress <- renderUI({
    p <- search_scope_progress_rv() %||% list(completed=0L,total=0L,pct=0L)
    total <- as.integer(p$total %||% 0L)
    completed <- as.integer(p$completed %||% 0L)
    pct <- as.integer(p$pct %||% 0L)
    if (total < 1L && !nzchar(search_scope_run_id_rv())) return(NULL)
    label <- if (total > 0L) {
      sprintf("Scoping search progress: %d of %d databases complete",completed,total)
    } else {
      "Preparing scoping search…"
    }
    div(
      class="mt-3 mb-3",
      tags$div(class="d-flex justify-content-between small mb-1",
               tags$span(label),tags$span(sprintf("%d%%",pct))),
      div(
        class="progress",
        div(
          class="progress-bar",
          role="progressbar",
          style=sprintf("width:%d%%",pct),
          `aria-valuenow`=pct,
          `aria-valuemin`=0,
          `aria-valuemax`=100
        )
      )
    )
  })

  output$search_scope_table <- renderUI({
    rows <- search_scope_rows_rv()
    job_sources <- search_scope_job_sources_rv() %||% character()
    if (is.null(rows) && !length(job_sources)) return(NULL)

    if (is.null(rows)) {
      rows <- data.frame(
        source_slug=job_sources,
        source=vapply(job_sources,search_scope_pretty_source,character(1)),
        hits=NA_integer_,
        status="Queued",
        stringsAsFactors=FALSE
      )
    }

    if (!"source_slug" %in% names(rows)) rows$source_slug <- ""
    if (!"source" %in% names(rows)) rows$source <- vapply(rows$source_slug,search_scope_pretty_source,character(1))
    if (!"hits" %in% names(rows)) rows$hits <- NA_integer_
    if (!"status" %in% names(rows)) rows$status <- ""

    tags$div(
      class="table-responsive mt-2",
      tags$table(
        class="table table-sm align-middle mb-0",
        tags$thead(tags$tr(
          tags$th("Database"),
          tags$th(class="text-end","Hits"),
          tags$th("Status")
        )),
        tags$tbody(tagList(lapply(seq_len(nrow(rows)),function(i) {
          hit <- suppressWarnings(as.integer(rows$hits[[i]]))
          stat <- as.character(rows$status[[i]] %||% "")
          shown_status <- if (identical(stat,"counted live") || identical(stat,"validated W00 manual-search reported count")) {
            "Complete"
          } else if (startsWith(stat,"API count failed:")) {
            paste("Failed", sub("^API count failed:\\s*", "", stat))
          } else stat
          tags$tr(
            tags$td(as.character(rows$source[[i]])),
            tags$td(class="text-end",if(is.na(hit)) "—" else format(hit,big.mark=",",scientific=FALSE)),
            tags$td(shown_status)
          )
        })))
      )
    )
  })

  observeEvent(input$run_search_scope, {
    req(authenticated())
    if (!session_can("control_workflows")) {
      search_scope_status_rv("Administrator permission is required.")
      return()
    }
    if (nzchar(search_scope_request_id_rv())) return()

    stamp <- format(Sys.time(),tz="UTC",format="%Y%m%dT%H%M%SZ")
    request_id <- paste0(
      "shiny-",stamp,"-",
      substr(digest::digest(
        paste(stamp,session_reviewer_id(),search_scope_string_rv(),sep="|"),
        algo="sha256",serialize=FALSE
      ),1L,10L)
    )
    search_scope_rows_rv(NULL)
    search_scope_seen_artifacts_rv(character())
    search_scope_job_sources_rv(character())
    search_scope_progress_rv(list(completed=0L,total=0L,pct=0L))
    search_scope_run_id_rv("")
    search_scope_status_rv("Dispatching count-only scoping search…")

    ok <- tryCatch({
      dispatch_w00_scoping(request_id)
      TRUE
    },error=function(e) {
      search_scope_status_rv(paste("Scoping dispatch failed:",conditionMessage(e)))
      FALSE
    })
    if (ok) {
      search_scope_request_id_rv(request_id)
      search_scope_status_rv("Scoping search queued in GitHub Actions.")
    }
  })

  observe({
    req(authenticated())
    request_id <- as.character(search_scope_request_id_rv() %||% "")
    if (!nzchar(request_id)) return()
    invalidateLater(3000, session)

    run <- tryCatch(find_w00_scoping_run(request_id),error=function(e)e)
    if (inherits(run,"error")) {
      search_scope_status_rv(paste("Could not read scoping run:",conditionMessage(run)))
      return()
    }
    if (is.null(run)) {
      search_scope_status_rv("Waiting for GitHub Actions to create the scoping run…")
      return()
    }

    run_id <- as.character(run$id %||% "")
    search_scope_run_id_rv(run_id)
    jobs <- tryCatch(w00_scoping_run_jobs(run_id),error=function(e)e)
    if (inherits(jobs,"error")) {
      search_scope_status_rv(paste("Could not read scoping job progress:",conditionMessage(jobs)))
      return()
    }

    count_jobs <- Filter(function(j) startsWith(as.character(j$name %||% ""),"count / "), jobs)
    sources <- vapply(count_jobs,function(j) sub("^count / ","",as.character(j$name %||% "")),character(1))
    if (length(sources)) search_scope_job_sources_rv(sources)

    completed <- sum(vapply(count_jobs,function(j) identical(as.character(j$status %||% ""),"completed"),logical(1)))
    total <- length(count_jobs)
    pct <- if (total > 0L) round(100*completed/total) else if (identical(as.character(run$status %||% ""),"queued")) 0L else 1L
    search_scope_progress_rv(list(completed=completed,total=total,pct=pct))

    current <- search_scope_rows_rv()
    if (is.null(current) && length(sources)) {
      current <- data.frame(
        source_slug=sources,
        source=vapply(sources,search_scope_pretty_source,character(1)),
        hits=NA_integer_,
        status=vapply(count_jobs,function(j) {
          st <- as.character(j$status %||% "")
          if (identical(st,"in_progress")) "Running" else if (identical(st,"completed")) {
            if (identical(as.character(j$conclusion %||% ""),"success")) "Complete" else "Failed"
          } else "Queued"
        },character(1)),
        stringsAsFactors=FALSE
      )
    } else if (!is.null(current) && length(sources)) {
      for (k in seq_along(sources)) {
        slug <- sources[[k]]
        hit <- which(as.character(current$source_slug)==slug)
        if (!length(hit)) {
          current <- rbind(current,data.frame(
            source_slug=slug,source=search_scope_pretty_source(slug),hits=NA_integer_,
            status="Queued",stringsAsFactors=FALSE
          ))
          hit <- nrow(current)
        }
        if (is.na(suppressWarnings(as.integer(current$hits[[hit[[1L]]]])))) {
          st <- as.character(count_jobs[[k]]$status %||% "")
          current$status[[hit[[1L]]]] <- if (identical(st,"in_progress")) "Running" else if (identical(st,"completed")) {
            if (identical(as.character(count_jobs[[k]]$conclusion %||% ""),"success")) "Complete" else "Failed"
          } else "Queued"
        }
      }
    }

    artifacts <- tryCatch(w00_scoping_run_artifacts(run_id),error=function(e) list())
    seen <- search_scope_seen_artifacts_rv() %||% character()
    count_artifacts <- Filter(function(a) startsWith(as.character(a$name %||% ""),"workflow00-scope-count-"), artifacts)
    for (a in count_artifacts) {
      aid <- as.character(a$id %||% "")
      if (!nzchar(aid) || aid %in% seen) next
      row <- tryCatch(read_w00_scoping_count_artifact(a),error=function(e) NULL)
      if (is.null(row)) next
      slug <- as.character(row$source_slug %||% "")
      if (!nzchar(slug)) next
      if (is.null(current)) current <- data.frame(source_slug=character(),source=character(),hits=integer(),status=character(),stringsAsFactors=FALSE)
      current <- current[as.character(current$source_slug)!=slug,,drop=FALSE]
      current <- rbind(current,data.frame(
        source_slug=slug,
        source=as.character(row$source %||% search_scope_pretty_source(slug)),
        hits=suppressWarnings(as.integer(row$hits %||% NA_integer_)),
        status=as.character(row$status %||% ""),
        stringsAsFactors=FALSE
      ))
      seen <- c(seen,aid)
    }
    search_scope_seen_artifacts_rv(unique(seen))

    if (!is.null(current) && length(sources)) {
      current$.ord <- match(current$source_slug,sources)
      current <- current[order(current$.ord,current$source),setdiff(names(current),".ord"),drop=FALSE]
    }
    if (!is.null(current)) search_scope_rows_rv(current)

    run_status <- as.character(run$status %||% "")
    run_conclusion <- as.character(run$conclusion %||% "")
    if (identical(run_status,"completed")) {
      final_art <- Filter(function(a) startsWith(as.character(a$name %||% ""),"workflow00-search-scoping-"), artifacts)
      if (identical(run_conclusion,"success") && length(final_art)) {
        final_rows <- tryCatch(read_w00_scoping_final_artifact(final_art[[1L]]),error=function(e) NULL)
        if (!is.null(final_rows)) search_scope_rows_rv(final_rows)
        search_scope_progress_rv(list(completed=max(total,completed),total=max(total,completed),pct=100L))
        search_scope_status_rv("Scoping search complete.")
      } else {
        search_scope_status_rv(paste0(
          "Scoping search finished",
          if(nzchar(run_conclusion)) paste0(" with status: ",run_conclusion) else "."
        ))
      }
      search_scope_request_id_rv("")
    } else {
      search_scope_status_rv(if(total > 0L) sprintf("Scoping search running: %d of %d databases complete.",completed,total) else "Preparing database count jobs…")
    }
  })

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

  w01_resolution_ready <- reactive({
    cs <- w01_all_cases_rv()
    if (!length(cs) || !nzchar(batch_id_rv()) || !nzchar(queue_sha_rv())) return(FALSE)
    ids <- vapply(cs,function(x)as.character(x$review_case_id %||% ""),character(1))
    ds <- decisions() %||% list()
    final <- Filter(function(x)as.character(x$decision %||% "") %in% c("duplicate","not_duplicate"),ds)
    dids <- unique(vapply(final,function(x)as.character(x$review_case_id %||% ""),character(1)))
    setequal(ids,dids) && w01_all_assignments_complete()
  })


  w02_active_assignment_events <- function() {
    w02_decisions() %||% list()
  }

  w04_active_assignment_events <- function() {
    w04_decisions() %||% list()
  }

  w08_active_assignment_events <- function() {
    w08_decisions() %||% list()
  }

  w04_validation_state <- reactive({
    w04_validation_lifecycle(
      cases = w04_all_cases_rv(),
      assignments = assignment_registry_rv(),
      decisions = w04_active_assignment_events(),
      batch_id = w04_batch_id_rv(),
      queue_sha256 = w04_queue_sha_rv()
    )
  })

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

  w02_handoff_ready <- reactive({
    cs <- w02_all_cases_rv() %||% list()
    if (!length(cs) || !nzchar(as.character(w02_batch_id_rv() %||% ""))) return(FALSE)
    ids <- vapply(cs,function(x)as.character(x$review_case_id %||% ""),character(1))
    resolved <- w02_resolved_ids(w02_decisions())
    length(ids) > 0L && all(nzchar(ids)) && all(ids %in% resolved)
  })

  w04_resolution_handoff_ready <- reactive({
    cs <- w04_resolution_all_cases_rv() %||% list()
    if (!length(cs) || !nzchar(as.character(w04_resolution_batch_id_rv() %||% ""))) return(FALSE)
    ids <- vapply(cs,function(x)as.character(x$review_case_id %||% ""),character(1))
    resolved <- w04_resolution_decision_ids(w04_resolution_decisions())
    length(ids) > 0L && all(nzchar(ids)) && all(ids %in% resolved)
  })

  w08_handoff_ready <- reactive({
    cs <- w08_all_cases_rv() %||% list()
    if (!length(cs) || !nzchar(as.character(w08_batch_id_rv() %||% ""))) return(FALSE)
    ids <- vapply(cs,function(x)as.character(x$record_id %||% ""),character(1))
    resolved <- w08_decision_ids(w08_decisions())
    length(ids) > 0L && all(nzchar(ids)) && all(ids %in% resolved)
  })

  output$assignment_progress <- renderUI({
    req(authenticated())
    if (!session_can("manage_assignments")) return(NULL)

    all_assignments <- assignment_registry_rv()
    has_w01_batch <- nzchar(as.character(batch_id_rv())) && length(w01_all_cases_rv()) > 0L
    has_w02_batch <- nzchar(as.character(w02_batch_id_rv())) && length(w02_all_cases_rv()) > 0L
    has_w04_batch <- nzchar(as.character(w04_batch_id_rv())) && length(w04_all_cases_rv()) > 0L
    has_w04_resolution_batch <- nzchar(as.character(w04_resolution_batch_id_rv())) && length(w04_resolution_all_cases_rv()) > 0L
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
      if (identical(workflow, "04") && identical(task_type, "model_uncertainty")) {
        return(if (has_w04_resolution_batch) as.character(w04_resolution_batch_id_rv()) else "")
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
        identical(z$task_type, "model_uncertainty") &&
        identical(z$batch_id, w04_resolution_batch_id_rv())
      ) return(w04_resolution_decisions())
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

    add_empty_group(
      "01","deduplication",
      if (has_w01_batch) batch_id_rv() else "no-active-queue",
      ASSIGNMENT_MODES[["shared_work_pool"]]
    )
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
    add_empty_group(
      "04","conflict_resolution",
      if (has_w04_conflict_batch) w04_active_conflict_batch_id() else "no-active-queue",
      ASSIGNMENT_MODES[["single_reviewer"]]
    )
    add_empty_group(
      "04","model_uncertainty",
      if (has_w04_resolution_batch) w04_resolution_batch_id_rv() else "no-active-queue",
      ASSIGNMENT_MODES[["single_reviewer"]]
    )
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
        conflict_resolution=2L,
        model_uncertainty=3L,
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
        identical(z$task_type, "model_uncertainty") &&
        identical(z$batch_id, w04_resolution_batch_id_rv())
      ) {
        list(prefix="w04resolution", workflow="04", task_type="model_uncertainty", batch_id=w04_resolution_batch_id_rv(),
             events=w04_resolution_decisions(), label="W04 model uncertainty")
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
          identical(z$workflow, "01") &&
          identical(z$task_type, "deduplication") &&
          identical(z$batch_id, "no-active-queue")
        ) {
          return(tags$div(
            class = "mt-2",
            tags$div(class = "text-secondary small mb-2", "No active W01 deduplication queue is loaded.")
          ))
        }
        if (
          identical(z$workflow, "02") &&
          identical(z$task_type, "enrichment") &&
          identical(z$batch_id, "no-active-queue")
        ) {
          return(tags$div(
            class = "mt-2",
            tags$div(class = "text-secondary small mb-2", "No active W02 queue is loaded.")
          ))
        }
        if (
          identical(z$workflow, "04") &&
          identical(z$task_type, "manual_screening") &&
          identical(z$batch_id, "no-active-queue")
        ) {
          return(tags$div(
            class = "mt-2",
            tags$div(class = "text-secondary small mb-2", "No active W04 manual-screening queue is loaded.")
          ))
        }
        if (
          identical(z$workflow, "04") &&
          identical(z$task_type, "model_uncertainty") &&
          identical(z$batch_id, "no-active-queue")
        ) {
          return(tags$div(
            class = "mt-2",
            tags$div(class = "text-secondary small mb-2", "No active W04 model-uncertainty queue is loaded.")
          ))
        }
        if (
          identical(z$workflow, "04") &&
          identical(z$task_type, "conflict_resolution") &&
          identical(z$batch_id, "no-active-queue")
        ) {
          return(tags$div(
            class = "mt-2",
            tags$div(class = "text-secondary small mb-2", "No active W04 conflict-resolution queue is loaded.")
          ))
        }
        if (
          identical(z$workflow, "08") &&
          identical(z$task_type, "annotation") &&
          identical(z$batch_id, "no-active-queue")
        ) {
          return(tags$div(
            class = "mt-2",
            tags$div(class = "text-secondary small mb-2", "No active W08 queue is loaded.")
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
        class = paste(
          "assignment-workflow assignment-submenu mt-2",
          paste0("wf-w",cfg$workflow)
        ),
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
        identical(z$workflow,"04") && identical(z$task_type,"model_uncertainty") &&
        identical(z$batch_id,w04_resolution_batch_id_rv())
      ) w04_resolution_all_cases_rv() else if (
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
          tags$td(record_table_link(x$assigned,z$workflow,z$task_type,z$batch_id,"assignments",x$user_id,paste(workflow_label,task_label,x$display_name,"Assigned",sep=" · "))),
          tags$td(record_table_link(x$completed,z$workflow,z$task_type,z$batch_id,"completed",x$user_id,paste(workflow_label,task_label,x$display_name,"Completed",sep=" · "))),
          tags$td(record_table_link(x$resolved_elsewhere,z$workflow,z$task_type,z$batch_id,"closed",x$user_id,paste(workflow_label,task_label,x$display_name,"Closed",sep=" · "))),
          tags$td(record_table_link(x$remaining,z$workflow,z$task_type,z$batch_id,"outstanding",x$user_id,paste(workflow_label,task_label,x$display_name,"Outstanding",sep=" · "))),
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
        class = paste(
          "assignment-workflow workflow-section",
          paste0("wf-w",z$workflow)
        ),
        `data-accordion-key` = paste0("workflow-", z$workflow, "-", z$task_type),
        tags$summary(
          div(
            class = "d-inline-flex flex-wrap align-items-center gap-2",
            tags$strong(paste0(workflow_label, " · ", task_label)),
            tags$span(class = "task-badge", mode_label),
            tags$span(
              class = "text-secondary small",
              if (identical(z$batch_id, "no-active-queue")) {
                "No records awaiting review"
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
            div(class = "assignment-kpi", tags$span("Cases"), tags$strong(record_table_link(p$cases,z$workflow,z$task_type,z$batch_id,"cases",label=paste(workflow_label,task_label,"Cases",sep=" · ")))),
            div(class = "assignment-kpi", tags$span("Assignments"), tags$strong(record_table_link(p$assigned,z$workflow,z$task_type,z$batch_id,"assignments",label=paste(workflow_label,task_label,"Assignments",sep=" · ")))),
            div(class = "assignment-kpi", tags$span("Completed"), tags$strong(record_table_link(p$completed,z$workflow,z$task_type,z$batch_id,"completed",label=paste(workflow_label,task_label,"Completed",sep=" · ")))),
            div(class = "assignment-kpi", tags$span("Closed"), tags$strong(record_table_link(p$resolved_elsewhere,z$workflow,z$task_type,z$batch_id,"closed",label=paste(workflow_label,task_label,"Closed",sep=" · ")))),
            div(class = "assignment-kpi", tags$span("Outstanding"), tags$strong(record_table_link(p$remaining,z$workflow,z$task_type,z$batch_id,"outstanding",label=paste(workflow_label,task_label,"Outstanding",sep=" · "))))
          ),
          if (unassigned > 0L) {
            tags$div(
              class = "small mb-2",
              "Unassigned cases: ",
              record_table_link(unassigned,z$workflow,z$task_type,z$batch_id,"unassigned",label=paste(workflow_label,task_label,"Unassigned cases",sep=" · "))
            )
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
          if (
            identical(z$workflow, "04") &&
            identical(z$task_type, "manual_screening") &&
            isTRUE(w04_validation_state()$ready) &&
            session_can("control_workflows")
          ) {
            tags$div(
              class = "d-flex flex-wrap align-items-center gap-2 mt-3 p-2 border rounded bg-light",
              tags$div(
                tags$strong("Validation set ready"),
                tags$div(
                  class = "text-secondary small",
                  "Every queue case has exactly one assignment and one valid decision with the expected queue SHA."
                )
              ),
              if (isTRUE(w04_validation_finalize_requested_rv())) {
                tags$div(
                  class="text-success small fw-semibold",
                  "Sent to GitHub. Workflow 04 validation finalisation has been requested."
                )
              } else {
                actionButton(
                  "w04_finalize_validation",
                  "Send validation set to GitHub",
                  class = "btn-primary btn-sm"
                )
              }
            )
          },
          if (
            identical(z$workflow, "01") &&
            identical(z$task_type, "deduplication") &&
            isTRUE(w01_resolution_ready()) &&
            session_can("control_workflows")
          ) {
            tags$div(
              class = "d-flex flex-wrap align-items-center gap-2 mt-3 p-2 border rounded bg-light",
              tags$div(
                tags$strong(if (identical(batch_status_rv(),"review_complete")) "W01 ready to send" else "W01 review complete"),
                tags$div(
                  class = "text-secondary small",
                  if (identical(batch_status_rv(),"review_complete")) {
                    "All deduplication cases are complete. Send the reviewed batch back to GitHub for integrity checks and resume."
                  } else {
                    "All deduplication cases have final decisions. Marking W01 as resolved will send the reviewed batch back to GitHub for integrity checks and resume."
                  }
                )
              ),
              if (isTRUE(w01_export_requested_rv())) {
                tags$div(
                  class = "text-success small fw-semibold",
                  w01_export_status_rv()
                )
              } else {
                actionButton(
                  "w01_mark_resolved",
                  if (identical(batch_status_rv(),"review_complete")) "Send W01 to GitHub" else "Mark W01 as resolved",
                  class = "btn-primary btn-sm"
                )
              }
            )
          },
          if (
            identical(z$workflow, "02") &&
            identical(z$task_type, "enrichment") &&
            isTRUE(w02_handoff_ready()) &&
            session_can("control_workflows")
          ) {
            tags$div(
              class="d-flex flex-wrap align-items-center gap-2 mt-3 p-2 border rounded bg-light",
              tags$div(
                tags$strong("W02 review complete"),
                tags$div(class="text-secondary small","All enrichment cases have final decisions. Send the reviewed queue to GitHub to resume Workflow 02.")
              ),
              if (isTRUE(w02_resume_requested_rv())) {
                tags$div(class="text-success small fw-semibold","Sent to GitHub. Workflow 02 resume has been requested.")
              } else {
                actionButton("w02_send_github","Send W02 to GitHub",class="btn-primary btn-sm")
              }
            )
          },
          if (
            identical(z$workflow, "04") &&
            identical(z$task_type, "model_uncertainty") &&
            isTRUE(w04_resolution_handoff_ready()) &&
            session_can("control_workflows")
          ) {
            tags$div(
              class="d-flex flex-wrap align-items-center gap-2 mt-3 p-2 border rounded bg-light",
              tags$div(
                tags$strong("Model uncertainty review complete"),
                tags$div(class="text-secondary small","All model-uncertainty cases have final decisions. Send the reviewed queue to GitHub to finalise Workflow 04.")
              ),
              if (isTRUE(w04_resolution_resume_requested_rv())) {
                tags$div(
                  class="text-success small fw-semibold",
                  "Sent to GitHub. Workflow 04 finalisation has been requested."
                )
              } else {
                actionButton("w04_resolution_send_github","Send W04 to GitHub",class="btn-primary btn-sm")
              }
            )
          },
          if (
            identical(z$workflow, "08") &&
            identical(z$task_type, "annotation") &&
            isTRUE(w08_handoff_ready()) &&
            session_can("control_workflows")
          ) {
            tags$div(
              class="d-flex flex-wrap align-items-center gap-2 mt-3 p-2 border rounded bg-light",
              tags$div(
                tags$strong("W08 review complete"),
                tags$div(class="text-secondary small","All annotation records have final decisions. Send the reviewed queue to GitHub to resume Workflow 08.")
              ),
              if (isTRUE(w08_resume_requested_rv())) {
                tags$div(class="text-success small fw-semibold","Sent to GitHub. Workflow 08 resume has been requested.")
              } else {
                actionButton("w08_send_github","Send W08 to GitHub",class="btn-primary btn-sm")
              }
            )
          },
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
    input_selection <- input$w04_consistency_raters
    if (is.null(input_selection)) {
      human_defaults <- setdiff(available_raters,"model")
      selected_raters <- if (length(human_defaults) >= 2L) {
        human_defaults
      } else {
        available_raters
      }
    } else {
      selected_raters <- intersect(
        as.character(input_selection %||% character()),
        available_raters
      )
    }
    human_scope_now <- if (
      length(selected_raters) >= 2L &&
      !"model" %in% selected_raters
    ) w04_selected_human_consistency_scope() else NULL
    w04_selected_analysis <- if (!is.null(human_scope_now)) {
      human_scope_now$analysis
    } else {
      w04_consistency_analysis(w04_outcomes_now,selected_raters)
    }

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

    w04_results_panel <- if (
      session_can("manage_assignments") &&
      (has_w04_batch || nrow(w04_kappa_registry_rv()) > 0L || nrow(w04_human_kappa_registry_rv()) > 0L)
    ) {
      tags$details(
        class="assignment-workflow assignment-submenu wf-w04 mb-2",
        `data-accordion-key`="w04-consistency-checking",
        tags$summary(
          div(
            class="d-inline-flex flex-wrap align-items-center gap-2",
            tags$strong("Consistency checking"),
            tags$span(class="task-badge","Administrator only"),
            tags$span(
              class="text-secondary small",
              if (!has_w04_batch) {
                "Historical validation agreement"
              } else if (length(selected_raters) < 2L) {
                "Select at least two raters"
              } else if (w04_selected_analysis$complete < 1L) {
                "No complete cases for selected raters"
              } else {
                sprintf(
                  "%d complete · %s agreement · %s = %s",
                  w04_selected_analysis$complete,
                  fmt_agreement_pct(w04_selected_analysis$raw_agreement),
                  w04_selected_analysis$metric,
                  fmt_kappa(w04_selected_analysis$kappa)
                )
              }
            )
          )
        ),
        div(
          class="pt-2",
          uiOutput("w04_kappa_history"),
          tags$hr(),
          uiOutput("w04_human_kappa_history"),
          if (!has_w04_batch) {
            tags$div(
              class="text-secondary small",
              "No active W04 manual-screening batch. Historical kappa data remain available above."
            )
          } else tagList(
          tags$hr(),
          tags$p(
            class="text-secondary small mb-1",
            "Select any combination of human raters and the model. Statistics use complete cases for the selected raters only; no raw decisions are altered."
          ),
          tags$p(
            class="text-secondary small fw-semibold mb-2",
            "Agreement statistics update automatically when the selected raters change."
          ),
          div(
            class="d-flex flex-wrap align-items-center gap-2 mb-2",
            actionButton(
              "w04_refresh_status_button",
              "Refresh W04 status",
              class="btn-outline-secondary btn-sm"
            ),
            tags$span(
              class="saved-note",
              textOutput("w04_refresh_status",inline=TRUE)
            )
          ),
          checkboxGroupInput(
            "w04_consistency_raters",
            "Raters to compare",
            choices=rater_choices,
            selected=selected_raters,
            inline=TRUE
          ),
          if (
            length(selected_raters) >= 2L &&
            !"model" %in% selected_raters
          ) {
            tagList(
              radioButtons(
                "w04_consistency_scope",
                "Consistency set",
                choices=c(
                  "Since last saved check (Partial)"="partial",
                  "Entire assignment (Full)"="full"
                ),
                selected=as.character(input$w04_consistency_scope %||% "partial"),
                inline=TRUE
              ),
              if (!is.null(human_scope_now)) {
                tags$div(
                  class="text-secondary small mb-2",
                  if (identical(human_scope_now$scope_type,"partial")) {
                    sprintf(
                      "%d jointly assigned · %d complete · %d previously saved · %d new records in this Partial check.",
                      human_scope_now$assigned_n %||% 0L,
                      human_scope_now$complete_n %||% 0L,
                      human_scope_now$previously_saved_n %||% 0L,
                      length(human_scope_now$target_case_ids %||% character())
                    )
                  } else {
                    sprintf(
                      "%d jointly assigned · %d complete. A Full result can be saved only when all jointly assigned records are complete.",
                      human_scope_now$assigned_n %||% 0L,
                      human_scope_now$complete_n %||% 0L
                    )
                  }
                )
              }
            )
          },
          if (length(selected_raters) < 2L) {
            tags$div(
              class="p-2 border rounded bg-light text-secondary small",
              "Select at least two raters. Previous statistics are intentionally hidden until a valid comparison is selected."
            )
          } else if (w04_selected_analysis$complete < 1L) {
            tags$div(
              class="p-2 border rounded bg-light text-secondary small",
              sprintf(
                "No records have complete decisions for all selected raters. Eligible records: %d; missing for this comparison: %d. Agreement and kappa are not calculated.",
                w04_selected_analysis$eligible,
                w04_selected_analysis$missing
              )
            )
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
                  if (
                    length(selected_raters) >= 2L &&
                    !"model" %in% selected_raters
                  ) "Save consistency result" else "Save analysis",
                  class="btn-primary btn-sm",
                  disabled=if (!is.null(human_scope_now)) !isTRUE(human_scope_now$save_ready) else FALSE
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
      )
    } else NULL

    workflow_sections_display <- list()
    if (length(workflow_sections)) {
      for (i in seq_along(workflow_sections)) {
        workflow_sections_display[[length(workflow_sections_display)+1L]] <- workflow_sections[[i]]
        meta <- group_progress[[i]]$meta
        if (
          identical(meta$workflow,"04") &&
          identical(meta$task_type,"manual_screening") &&
          !is.null(w04_results_panel)
        ) {
          workflow_sections_display[[length(workflow_sections_display)+1L]] <- w04_results_panel
        }
      }
    }

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
            div(class = "assignment-kpi", tags$span("Assignments"), tags$strong(record_table_link(total_assigned,"all","all","","assignments",label="All workflows · Assignments"))),
            div(class = "assignment-kpi", tags$span("Completed"), tags$strong(record_table_link(total_completed,"all","all","","completed",label="All workflows · Completed"))),
            div(class = "assignment-kpi", tags$span("Closed"), tags$strong(record_table_link(total_released,"all","all","","closed",label="All workflows · Closed"))),
            div(class = "assignment-kpi", tags$span("Outstanding"), tags$strong(record_table_link(total_remaining,"all","all","","outstanding",label="All workflows · Outstanding"))),
            div(class = "assignment-kpi", tags$span("Conflicts"), tags$strong(record_table_link(length(w04_all_conflict_cases()),"04","conflict_resolution",w04_active_conflict_batch_id(),"cases",label="W04 · Reviewer conflict resolution · Cases")))
          ),
          workflow_sections_display,
          tags$hr(class = "my-3"),
          tags$details(
            class = "assignment-workflow border rounded",
            `data-accordion-key` = "backend-maintenance",
            tags$summary(
              div(
                class = "d-inline-flex flex-wrap align-items-center gap-2",
                tags$strong("Backend maintenance"),
                tags$span(class = "task-badge", "Administrator only")
              )
            ),
            div(
              class = "pt-2 px-3 pb-3",
              tags$p(
                class = "text-secondary small mb-2",
                "Archive the transient adjudication backend to restricted Zenodo and reset all operational queue, decision, assignment and status tabs for the next update. Production queues that are not marked consumed block the reset."
              ),
              actionButton(
                "reset_backend_queue",
                "Reset backend queue",
                class = "btn-outline-danger btn-sm"
              ),
              tags$div(
                class = "saved-note mt-2",
                textOutput("backend_reset_status", inline = TRUE)
              )
            )
          )
        )
      )
    )
  })

  output$backend_reset_status <- renderText(backend_reset_status())

  observeEvent(input$reset_backend_queue, {
    req(authenticated())
    if (!session_can("control_workflows")) {
      backend_reset_status("Administrator permission is required.")
      return()
    }
    showModal(modalDialog(
      title = "Reset backend queue",
      tags$p(
        "This will first archive the operational Google Sheets backend to a restricted Zenodo record, then delete the transient operational tabs."
      ),
      tags$p(
        class = "text-danger",
        tags$strong("Any non-test production queue that is not marked consumed will block the reset.")
      ),
      textInput(
        "backend_reset_confirmation",
        'Type "RESET" to confirm',
        value = ""
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_backend_reset", "Archive and reset", class = "btn-danger")
      ),
      easyClose = FALSE
    ))
  })

  observeEvent(input$confirm_backend_reset, {
    req(authenticated())
    if (!session_can("control_workflows")) {
      backend_reset_status("Administrator permission is required.")
      removeModal()
      return()
    }
    if (!identical(trimws(as.character(input$backend_reset_confirmation %||% "")), "RESET")) {
      backend_reset_status('Reset cancelled: type "RESET" exactly to confirm.')
      return()
    }

    p <- pipeline_status_rv()
    active <- suppressWarnings(as.integer(as.character((p %||% list())$active_workflow %||% "")))
    completed <- suppressWarnings(as.integer(as.character((p %||% list())$completed_through %||% "")))
    status_label <- tolower(trimws(as.character((p %||% list())$status_label %||% "")))
    update_complete <- !is.null(p) &&
      is.na(active) &&
      !is.na(completed) && completed >= 11L &&
      grepl("complete|final", status_label)

    if (!isTRUE(update_complete)) {
      backend_reset_status("Reset refused: the current update is not recorded as complete.")
      return()
    }

    backend_reset_status("Archiving backend to Zenodo before reset…")
    result <- tryCatch(
      reset_backend_queue_state(created_by = session_reviewer_id()),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      backend_reset_status(paste("Reset refused:", conditionMessage(result)))
      return()
    }

    removeModal()
    assignment_registry_rv(list())
    w01_all_cases_rv(list()); cases_rv(NULL); decisions(list()); batch_id_rv(""); queue_sha_rv(""); batch_status_rv("")
    w02_all_cases_rv(list()); w02_cases_rv(NULL); w02_decisions(list()); w02_batch_id_rv(""); w02_queue_sha_rv(""); w02_batch_status_rv(""); w02_resume_requested_rv(FALSE)
    w04_all_cases_rv(list()); w04_cases_rv(NULL); w04_decisions(list()); w04_batch_id_rv(""); w04_queue_sha_rv(""); w04_batch_status_rv(""); w04_validation_finalize_requested_rv(FALSE)
    w04_resolution_cases_rv(NULL); w04_resolution_decisions(list()); w04_resolution_batch_id_rv(""); w04_resolution_queue_sha_rv(""); w04_resolution_resume_requested_rv(FALSE)
    w04_conflict_cases_rv(NULL); w04_conflict_decisions(list()); w04_conflict_batch_id_rv(""); w04_conflict_queue_sha_rv("")
    w04_consistency_analyses_rv(list()); w04_conflict_sets_rv(list()); w04_consistency_history_loaded(FALSE)
    w08_all_cases_rv(list()); w08_cases_rv(NULL); w08_decisions(list()); w08_batch_id_rv(""); w08_queue_sha_rv(""); w08_batch_status_rv(""); w08_resume_requested_rv(FALSE)

    doi <- as.character(result$archived$doi %||% "")
    record_id <- as.character(result$archived$record_id %||% "")
    archive_label <- if (nzchar(doi)) doi else paste0("Zenodo record ", record_id)
    backend_reset_status(
      sprintf(
        "Backend reset complete. Archived %d operational tab(s) as %s; deleted %d transient tab(s).",
        length(result$archived$tabs %||% character()),
        archive_label,
        length(result$deleted_tabs %||% character())
      )
    )
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

  w01_assignment_plan <- reactive({
    req(authenticated())
    workflow_assignment_plan(
      "w01", w01_all_cases_rv(), w01_active_assignment_events(),
      "01", batch_id_rv(), "deduplication"
    )
  })
  output$w01_assignment_preview <- renderUI({
    req(authenticated())
    workflow_assignment_preview(
      w01_assignment_plan(), "w01", "01", batch_id_rv(),
      "deduplication", w01_active_assignment_events()
    )
  })
  output$w01_assignment_status <- renderText(assignment_manage_status())
  observeEvent(input$w01_apply_assignments, {
    apply_workflow_assignments(w01_assignment_plan())
  })
  observeEvent(input$w01_remove_assignments, {
    remove_workflow_user_assignments(
      input$w01_remove_assignment_user,
      w01_active_assignment_events(),
      "01",
      batch_id_rv(),
      "deduplication"
    )
  })

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
        human_kappa=read_w04_human_kappa_registry(),
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
    w04_human_kappa_registry_rv(loaded$human_kappa)
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

  output$w04_refresh_status <- renderText(w04_refresh_status())

  output$w04_kappa_history <- renderUI({
    req(authenticated())
    if (!session_can("manage_assignments")) return(NULL)
    x <- w04_kappa_registry_rv()
    x <- tryCatch(w04_normalise_kappa_registry(x),error=function(e)w04_empty_kappa_registry())
    if (!nrow(x)) {
      return(tags$div(class="text-secondary small mb-2","No W04 kappa history yet."))
    }
    summary <- w04_kappa_registry_summary(x)
    fmt_date <- function(z) {
      d <- suppressWarnings(as.Date(as.character(z)))
      if (is.na(d)) as.character(z) else format(d,"%d-%m-%Y")
    }
    fmt_num <- function(z) {
      v <- suppressWarnings(as.numeric(as.character(z)))
      if (is.na(v)) "—" else sprintf("%.3f",v)
    }
    rows <- list()
    ord <- order(as.character(x$date),as.character(x$created_at_utc),decreasing=FALSE)
    for (i in ord) {
      rows[[length(rows)+1L]] <- tags$tr(
        tags$td(fmt_date(x$date[[i]])),
        tags$td(format(suppressWarnings(as.integer(x$records_reviewed[[i]])),big.mark=",")),
        tags$td(fmt_num(x$human_model_kappa[[i]])),
        tags$td(fmt_num(x$humans_model_fleiss_kappa[[i]]))
      )
    }
    rows[[length(rows)+1L]] <- tags$tr(
      tags$td(tags$em("Cumulative")),
      tags$td(tags$em(format(summary$manually_screened,big.mark=","))),
      tags$td(tags$em(if(is.na(summary$kappa))"—" else sprintf("%.3f",summary$kappa))),
      tags$td(tags$em("—"))
    )
    tagList(
      tags$strong("Model validation history"),
      tags$p(
        class="text-secondary small mb-1",
        "The cumulative human–model kappa is recalculated from the stored contingency counts; archived individual decisions are not loaded."
      ),
      div(
        class="assignment-table-wrap mb-2",
        tags$table(
          class="assignment-table",
          tags$thead(tags$tr(
            tags$th("Date"),tags$th("Records"),tags$th("Human–model κ"),
            tags$th("Humans–model Fleiss κ")
          )),
          tags$tbody(rows)
        )
      )
    )
  })

  output$w04_human_kappa_history <- renderUI({
    req(authenticated())
    if (!session_can("manage_assignments")) return(NULL)
    x <- tryCatch(
      w04_normalise_human_kappa_registry(w04_human_kappa_registry_rv()),
      error=function(e)w04_empty_human_kappa_registry()
    )
    fmt_date <- function(z) {
      d <- suppressWarnings(as.Date(as.character(z)))
      if (is.na(d)) as.character(z) else format(d,"%d-%m-%Y")
    }
    fmt_num <- function(z) {
      v <- suppressWarnings(as.numeric(as.character(z)))
      if (is.na(v)) "—" else sprintf("%.3f",v)
    }
    fmt_pct <- function(z) {
      v <- suppressWarnings(as.numeric(as.character(z)))
      if (is.na(v)) "—" else sprintf("%.1f%%",100*v)
    }
    if (!nrow(x)) {
      return(tagList(
        tags$strong("Human consistency history"),
        tags$div(class="text-secondary small mb-2","No saved human–human consistency results yet.")
      ))
    }
    ord <- order(as.character(x$date),as.character(x$created_at_utc),decreasing=FALSE)
    rows <- lapply(ord,function(i) {
      labels <- w04_human_registry_json_chars(x$rater_labels_json[[i]])
      if(!length(labels)) labels <- w04_human_registry_json_chars(x$rater_ids_json[[i]])
      id <- as.character(x$consistency_id[[i]])
      tags$tr(
        tags$td(fmt_date(x$date[[i]])),
        tags$td(paste(labels,collapse=", ")),
        tags$td(if(identical(x$scope_type[[i]],"full"))"Full" else "Partial"),
        tags$td(format(suppressWarnings(as.integer(x$records_n[[i]])),big.mark=",")),
        tags$td(fmt_pct(x$raw_agreement[[i]])),
        tags$td(as.character(x$metric[[i]])),
        tags$td(fmt_num(x$kappa[[i]])),
        tags$td(
          tags$button(
            type="button",
            class="btn btn-link btn-sm p-0 text-secondary",
            title="Delete saved consistency result",
            `aria-label`="Delete saved consistency result",
            onclick=sprintf(
              "Shiny.setInputValue('w04_delete_human_kappa','%s',{priority:'event'})",
              id
            ),
            HTML("&#128465;")
          )
        )
      )
    })
    cumulatives <- w04_human_registry_cumulative_all(x)
    cumulative_rows <- lapply(cumulatives,function(z) {
      labels <- z$rater_labels %||% z$rater_ids %||% character()
      tags$tr(
        tags$td(tags$em("Cumulative")),
        tags$td(tags$em(paste(labels,collapse=", "))),
        tags$td(tags$em("—")),
        tags$td(tags$em(if(isTRUE(z$valid))format(z$n,big.mark=",") else "—")),
        tags$td(tags$em(if(isTRUE(z$valid))fmt_pct(z$raw_agreement) else "—")),
        tags$td(tags$em(if(isTRUE(z$valid))z$metric else "—")),
        tags$td(tags$em(if(isTRUE(z$valid))fmt_num(z$kappa) else "—")),
        tags$td("")
      )
    })
    errors <- unique(vapply(
      Filter(function(z)!isTRUE(z$valid),cumulatives),
      function(z)as.character(z$error %||% ""),
      character(1)
    ))
    tagList(
      tags$strong("Human consistency history"),
      tags$p(
        class="text-secondary small mb-1",
        "Partial rows are non-overlapping saved tranches. A Full row supersedes contained Partial rows in the cumulative calculation, while the earlier rows remain visible."
      ),
      div(
        class="assignment-table-wrap mb-2",
        tags$table(
          class="assignment-table",
          tags$thead(tags$tr(
            tags$th("Date"),tags$th("Reviewers"),tags$th("Scope"),tags$th("Records"),
            tags$th("Agreement"),tags$th("Metric"),tags$th("κ"),tags$th("")
          )),
          tags$tbody(c(rows,cumulative_rows))
        )
      ),
      if(length(errors)) tags$div(
        class="text-danger small mb-2",
        paste(errors,collapse=" ")
      )
    )
  })

  output$w04_consistency_history <- renderUI({
    req(authenticated())
    if (!session_can("manage_assignments")) return(NULL)
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
    if (!length(conflict_ready)) {
      return(tags$div(
        class="text-secondary small mt-2",
        "No saved consistency result currently contains conflicts."
      ))
    }
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
        "Create a conflict set from one saved consistency result. The exact raters and conflicting records are preserved as provenance."
      ),
      selectInput(
        "w04_conflict_analysis_id",
        "Saved consistency result",
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
    review_mode <- if (any(startsWith(groups,"w04-reviewer-consistency"))) {
      "reviewer_consistency"
    } else if ("w04-validation-set" %in% groups) {
      "validation_set"
    } else {
      ""
    }

    human_only <- !"model" %in% rater_ids
    if (human_only) {
      scope <- w04_selected_human_consistency_scope()
      if (is.null(scope) || !isTRUE(scope$valid)) {
        w04_consistency_status(if(is.null(scope))"Human consistency scope is unavailable." else scope$reason)
        return()
      }
      if (!isTRUE(scope$save_ready)) {
        w04_consistency_status(scope$reason %||% "This consistency set is not ready to save.")
        return()
      }
      analysis <- scope$analysis
      if (analysis$complete < 1L) {
        w04_consistency_status("There are no complete cases for the selected reviewers.")
        return()
      }
      labels <- vapply(scope$rater_ids,function(uid) {
        u <- find_user_by_id(user_registry_rv(),uid,require_active=FALSE)
        if(is.null(u)) uid else u$display_name
      },character(1))

      row <- tryCatch(
        w04_human_registry_row(
          analysis=analysis,
          record_ids=scope$target_case_ids,
          batch_id=w04_batch_id_rv(),
          queue_sha256=w04_queue_sha_rv(),
          assignment_scope_id=scope$assignment_scope_id,
          scope_type=scope$scope_type,
          rater_labels=labels,
          created_by=session_reviewer_id()
        ),
        error=function(e)e
      )
      if (inherits(row,"error")) {
        w04_consistency_status(paste("Save failed:",conditionMessage(row)))
        return()
      }
      analysis_id <- as.character(row$consistency_id[[1L]])
      row$analysis_id <- analysis_id

      saved_analysis <- tryCatch(
        append_w04_consistency_analysis(
          analysis=analysis,
          batch_id=w04_batch_id_rv(),
          queue_sha256=w04_queue_sha_rv(),
          review_mode=review_mode,
          created_by=session_reviewer_id(),
          analysis_id_override=analysis_id
        ),
        error=function(e)e
      )
      if (inherits(saved_analysis,"error")) {
        w04_consistency_status(paste("Save failed:",conditionMessage(saved_analysis)))
        return()
      }

      saved_row <- tryCatch(
        append_w04_human_kappa_registry(row),
        error=function(e)e
      )
      if (inherits(saved_row,"error")) {
        w04_consistency_status(paste("Save failed:",conditionMessage(saved_row)))
        return()
      }

      existing_human <- w04_human_kappa_registry_rv()
      existing_ids <- as.character(existing_human$consistency_id %||% character())
      if (!analysis_id %in% existing_ids) {
        w04_human_kappa_registry_rv(rbind(existing_human,saved_row))
      }
      existing_analyses <- w04_consistency_analyses_rv() %||% list()
      analysis_ids <- vapply(
        existing_analyses,
        function(x)as.character(x$analysis_id %||% ""),
        character(1)
      )
      if (!analysis_id %in% analysis_ids) {
        w04_consistency_analyses_rv(c(existing_analyses,list(saved_analysis)))
      }
      w04_consistency_status(sprintf(
        "Saved %s human consistency result for %d records.",
        if(identical(scope$scope_type,"full"))"Full" else "Partial",
        length(scope$target_case_ids)
      ))
      return()
    }

    analysis <- w04_consistency_analysis(w04_blind_outcomes(),rater_ids)
    if (analysis$complete < 1L) {
      w04_consistency_status("There are no complete cases for the selected raters.")
      return()
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

  observeEvent(input$w04_delete_human_kappa,{
    req(authenticated())
    if (!session_can("manage_assignments")) return()
    id <- as.character(input$w04_delete_human_kappa %||% "")
    if (!grepl("^w04-human-kappa-[0-9a-f]{24}$",id)) return()
    x <- w04_human_kappa_registry_rv()
    hit <- x[x$consistency_id==id,,drop=FALSE]
    if (nrow(hit)!=1L) {
      w04_consistency_status("Saved human consistency result could not be found.")
      return()
    }
    conflict_refs <- vapply(
      w04_conflict_sets_rv() %||% list(),
      function(z)as.character(z$analysis_id %||% ""),
      character(1)
    )
    if (id %in% conflict_refs) {
      w04_consistency_status("This saved result has already been used to create a conflict set and cannot be deleted.")
      return()
    }
    labels <- w04_human_registry_json_chars(hit$rater_labels_json[[1L]])
    w04_pending_human_kappa_delete(id)
    showModal(modalDialog(
      title="Delete saved consistency result?",
      tags$p(sprintf(
        "%s · %s · %s records · %s.",
        format(as.Date(hit$date[[1L]]),"%d-%m-%Y"),
        paste(labels,collapse=", "),
        as.character(hit$records_n[[1L]]),
        if(identical(hit$scope_type[[1L]],"full"))"Full" else "Partial"
      )),
      tags$p(
        class="text-secondary small",
        "This removes the saved milestone from the human consistency registry and from cumulative reporting. Screening decisions are not changed."
      ),
      footer=tagList(
        modalButton("Cancel"),
        actionButton(
          "w04_confirm_delete_human_kappa",
          "Delete result",
          class="btn-danger"
        )
      ),
      easyClose=TRUE
    ))
  })

  observeEvent(input$w04_confirm_delete_human_kappa,{
    req(authenticated())
    if (!session_can("manage_assignments")) return()
    id <- as.character(w04_pending_human_kappa_delete() %||% "")
    if (!grepl("^w04-human-kappa-[0-9a-f]{24}$",id)) return()
    conflict_refs <- vapply(
      w04_conflict_sets_rv() %||% list(),
      function(z)as.character(z$analysis_id %||% ""),
      character(1)
    )
    if (id %in% conflict_refs) {
      removeModal()
      w04_consistency_status("Deletion refused because this result is linked to a conflict set.")
      return()
    }
    deleted <- tryCatch({
      delete_w04_consistency_analysis_row(id)
      delete_w04_human_kappa_registry_row(id)
    },error=function(e)e)
    if (inherits(deleted,"error")) {
      removeModal()
      w04_consistency_status(paste("Delete failed:",conditionMessage(deleted)))
      return()
    }
    w04_human_kappa_registry_rv(deleted)
    w04_consistency_analyses_rv(Filter(
      function(x)!identical(as.character(x$analysis_id %||% ""),id),
      w04_consistency_analyses_rv() %||% list()
    ))
    w04_pending_human_kappa_delete("")
    removeModal()
    w04_consistency_status("Saved human consistency result deleted; cumulative statistics recalculated.")
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

  w04resolution_assignment_plan <- reactive({
    req(authenticated())
    workflow_assignment_plan(
      "w04resolution", w04_resolution_all_cases_rv(), w04_resolution_decisions(),
      "04", w04_resolution_batch_id_rv(), "model_uncertainty"
    )
  })
  output$w04resolution_assignment_preview <- renderUI({
    req(authenticated())
    workflow_assignment_preview(
      w04resolution_assignment_plan(), "w04resolution", "04", w04_resolution_batch_id_rv(),
      "model_uncertainty", w04_resolution_decisions()
    )
  })
  output$w04resolution_assignment_status <- renderText(assignment_manage_status())
  observeEvent(input$w04resolution_apply_assignments, {
    apply_workflow_assignments(w04resolution_assignment_plan())
  })
  observeEvent(input$w04resolution_remove_assignments, {
    remove_workflow_user_assignments(
      input$w04resolution_remove_assignment_user,
      w04_resolution_decisions(),
      "04",
      w04_resolution_batch_id_rv(),
      "model_uncertainty"
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
    w01_repairs_rv(list())
    w02_all_cases_rv(list())
    w04_all_cases_rv(list())
    w04_screening_notes_rv(list())
    w04_note_status("")
    w04_consistency_analyses_rv(list())
    w04_conflict_sets_rv(list())
    w04_kappa_registry_rv(w04_empty_kappa_registry())
    w04_human_kappa_registry_rv(w04_empty_human_kappa_registry())
    w04_pending_human_kappa_delete("")
    w04_consistency_history_loaded(FALSE)
    w04_refresh_status("")
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
        manual_screening_rv(read_manual_screening_metrics())
        batch <- load_batch()
        current_decisions <- list()
        if (!is.null(batch)) {
          all_decisions <- read_active_decisions(decision_path)
          current_decisions <- filter_batch_decisions(all_decisions, batch$queue_sha256)
          w01_repairs_rv(read_active_w01_repairs(w01_repair_path, batch$queue_sha256))
        } else {
          w01_repairs_rv(list())
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
          w02_resume_requested_rv(
            tryCatch(
              w02_resume_request_exists(
                w02_batch$queue_sha256,
                sub("^w02-run-", "", as.character(w02_batch$batch_id %||% ""))
              ),
              error=function(e) FALSE
            )
          )
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
          w02_resume_requested_rv(FALSE)
        }

        w04_screening_notes_rv(
          if (identical(storage_backend(),"google_sheets")) {
            tryCatch(active_sheet_w04_screening_notes(), error=function(e) list())
          } else list()
        )

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
          w04_validation_finalize_requested_rv(
            tryCatch(
              w04_validation_finalize_request_exists(
                w04_batch$queue_sha256,
                w04_batch$batch_id
              ),
              error=function(e) FALSE
            )
          )
          w04_unresolved <- w04_unresolved_indices()
          w04_idx(if (length(w04_unresolved)) w04_unresolved[[1L]] else max(1L,length(w04_visible_cases)))
          validation_state <- w04_validation_state()
          if(
            isTRUE(validation_state$ready) &&
            !identical(w04_batch_status_rv(),"review_complete") &&
            user_can(login_user,"control_workflows")
          ) {
            mark_review_complete("04",w04_batch_id_rv(),w04_queue_sha_rv(),w04_batch_status_rv)
          }
        }

        # Generated W04 conflict sets are reviewer work, not admin-only
        # reporting metadata. Load them for every authenticated user so an
        # assigned reviewer can reconstruct and open their conflict queue.
        if (identical(storage_backend(),"google_sheets")) {
          w04_conflict_sets_rv(read_w04_conflict_sets())
        } else {
          w04_conflict_sets_rv(list())
        }

        w04_resolution_batch <- load_w04_resolution_batch()
        if (!is.null(w04_resolution_batch)) {
          all_res <- active_sheet_w04_resolution_decisions()
          w04_resolution_all_cases_rv(w04_resolution_batch$cases)
          w04_resolution_queue_sha_rv(w04_resolution_batch$queue_sha256)
          w04_resolution_batch_id_rv(w04_resolution_batch$batch_id)
          w04_resolution_source_run_id_rv(w04_resolution_batch$source_run_id %||% "")
          w04_resolution_batch_status_rv(w04_resolution_batch$batch_status %||% "")
          w04_resolution_include_terms(w04_resolution_batch$highlight_include %||% character())
          w04_resolution_exclude_terms(w04_resolution_batch$highlight_exclude %||% character())
          w04_resolution_decisions(w04_filter_batch_decisions(all_res,w04_resolution_batch$queue_sha256))
          w04_resolution_resume_requested_rv(
            tryCatch(
              w04_resolution_resume_request_exists(
                w04_resolution_batch$queue_sha256,
                w04_resolution_batch$source_run_id %||% "",
                w04_resolution_batch$batch_id
              ),
              error=function(e) FALSE
            )
          )
          w04_resolution_abstract_edits_rv(
            tryCatch(
              active_sheet_w04_resolution_abstract_edits(w04_resolution_batch$queue_sha256),
              error=function(e) list()
            )
          )
          w04_resolution_abstract_edit_rv(FALSE)
          w04_resolution_visible <- cases_for_assignment_user(
            w04_resolution_batch$cases,
            assignment_registry_rv(),
            "04",
            w04_resolution_batch$batch_id,
            login_user,
            task_type = "model_uncertainty",
            active_events = w04_resolution_decisions()
          )
          w04_resolution_cases_rv(w04_resolution_visible)
          rr <- w04_resolution_unresolved_indices()
          w04_resolution_idx(if(length(rr)) rr[[1L]] else max(1L,length(w04_resolution_visible)))
        } else {
          w04_resolution_all_cases_rv(list())
          w04_resolution_cases_rv(NULL)
          w04_resolution_resume_requested_rv(FALSE)
          w04_resolution_abstract_edits_rv(list())
          w04_resolution_abstract_edit_rv(FALSE)
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
          w08_resume_requested_rv(
            tryCatch(
              w08_resume_request_exists(
                w08_batch$queue_sha256,
                w08_batch$source_run_id %||% "",
                w08_batch$batch_id
              ),
              error=function(e) FALSE
            )
          )
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
          w08_resume_requested_rv(FALSE)
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
          requested <- if (identical(storage_backend(),"google_sheets")) {
            w01_export_request_exists(batch$queue_sha256,batch$batch_id)
          } else FALSE
          # Compatibility for the live W01 batch that was dispatched immediately
          # before persistent export-request logging was introduced.
          legacy_dispatched_batch <- identical(
            as.character(batch$batch_id %||% ""),
            "w01-run-37347666369"
          ) && identical(
            tolower(as.character(batch$queue_sha256 %||% "")),
            "6a7bcd415f8fd9fc4b3c4eaf36776c898702cd590303c12f39d68bdab90c6778"
          ) && identical(as.character(batch$batch_status %||% ""), "review_complete")
          requested <- isTRUE(requested) || isTRUE(legacy_dispatched_batch)
          w01_export_requested_rv(isTRUE(requested))
          w01_export_status_rv(if(isTRUE(requested)) "Sent to GitHub. The reviewed batch is queued for integrity checks and resume." else "")

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
            if(user_can(login_user,"control_workflows") && w01_all_assignments_complete()) {
              status("All W01 assignments are complete. An administrator can mark W01 as resolved.")
            }
          }
        } else {
          cases_rv(NULL)
          w01_all_cases_rv(list())
          queue_sha_rv("")
          batch_id_rv("")
          batch_status_rv("")
          decisions(list())
          w01_export_requested_rv(FALSE)
          w01_export_status_rv("")
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
    repair_for <- function(rec) {
      hits <- Filter(function(x) {
        identical(as.character(x$source %||% ""), as.character(rec$source %||% "")) &&
          identical(as.character(x$source_record_id %||% ""), as.character(rec$source_record_id %||% "")) &&
          as.character(x$action %||% "") %in% c("replace_abstract","strip_abstract")
      }, w01_repairs_rv() %||% list())
      if (length(hits)) hits[[1L]] else NULL
    }
    effective_abstract <- function(rec, repair) {
      if (is.null(repair)) return(normalise_display_text(rec$abstract))
      action <- as.character(repair$action %||% "")
      if (identical(action, "strip_abstract")) return("")
      normalise_display_text(repair$value)
    }
    repair_i <- repair_for(z$record_i)
    repair_j <- repair_for(z$record_j)
    abstract_i <- effective_abstract(z$record_i, repair_i)
    abstract_j <- effective_abstract(z$record_j, repair_j)
    record_i_view <- z$record_i
    record_j_view <- z$record_j
    record_i_view$display_abstract <- abstract_i
    record_j_view$display_abstract <- abstract_j
    record_i_view$abstract_repair_saved <- !is.null(repair_i)
    record_j_view$abstract_repair_saved <- !is.null(repair_j)
    fields <- list(
      source = field_pair(z$record_i$source, z$record_j$source),
      title = field_pair(
        display_sentence_case_if_all_caps(z$record_i$title),
        display_sentence_case_if_all_caps(z$record_j$title)
      ),
      authors = field_pair(z$record_i$authors, z$record_j$authors),
      year = field_pair(z$record_i$year, z$record_j$year),
      journal = field_pair(z$record_i$journal, z$record_j$journal),
      doi = field_pair(normalise_doi_value(z$record_i$doi), normalise_doi_value(z$record_j$doi), char_level = TRUE),
      source_record_id = field_pair(
        normalise_source_id_value(z$record_i$source_record_id),
        normalise_source_id_value(z$record_j$source_record_id),
        char_level = TRUE
      ),
      abstract = field_pair(
        display_sentence_case_if_all_caps(abstract_i),
        display_sentence_case_if_all_caps(abstract_j)
      )
    )
    fields$source$a <- tags$span(class = "source-badge", fields$source$a)
    fields$source$b <- tags$span(class = "source-badge", fields$source$b)

    edit_state <- w01_abstract_edit_rv()
    editing_a <- !is.null(edit_state) &&
      identical(as.character(edit_state$review_case_id), as.character(z$review_case_id)) &&
      identical(as.character(edit_state$side), "a")
    editing_b <- !is.null(edit_state) &&
      identical(as.character(edit_state$review_case_id), as.character(z$review_case_id)) &&
      identical(as.character(edit_state$side), "b")

    tagList(
      layout_columns(
        col_widths = c(6,6),
        record_card(record_i_view, "Record A", fields, "a", abstract_editing = editing_a),
        record_card(record_j_view, "Record B", fields, "b", abstract_editing = editing_b)
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
    provider_title <- display_sentence_case_if_all_caps(pr$title %||% "")
    provider_abstract <- display_sentence_case_if_all_caps(pr$abstract %||% "")
    can_title <- display_sentence_case_if_all_caps(can$title %||% "")
    can_abstract <- display_sentence_case_if_all_caps(can$abstract %||% "")
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
              tags$dt("DOI"),tags$dd(doi_link(can_doi, doi_pair$a))
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
              tags$dt("Returned DOI"),tags$dd(doi_link(returned_doi, doi_pair$b)),
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

  observeEvent(input$w01_mark_resolved, {
    req(authenticated())
    if(!session_can("control_workflows")) {
      status("You do not have permission to resolve Workflow 01.")
      return()
    }
    if(!isTRUE(w01_resolution_ready())) {
      status("Workflow 01 cannot be resolved because one or more cases or assignments are incomplete.")
      return()
    }
    if (isTRUE(w01_export_requested_rv()) ||
        (identical(storage_backend(),"google_sheets") &&
         w01_export_request_exists(queue_sha_rv(),batch_id_rv()))) {
      w01_export_requested_rv(TRUE)
      w01_export_status_rv("Sent to GitHub.")
      status("W01 export has already been requested.")
      return()
    }
    dispatched <- tryCatch({
      if (!identical(batch_status_rv(),"review_complete")) {
        mark_review_complete("01",batch_id_rv(),queue_sha_rv(),batch_status_rv)
      }
      if (identical(storage_backend(),"google_sheets")) {
        append_w01_export_request(queue_sha_rv(),batch_id_rv(),"dispatching")
      }
      dispatch_w01_export(batch_id_rv(),queue_sha_rv())
      if (identical(storage_backend(),"google_sheets")) {
        append_w01_export_request(queue_sha_rv(),batch_id_rv(),"dispatched")
      }
      TRUE
    },error=function(e){
      if (identical(storage_backend(),"google_sheets")) {
        try(append_w01_export_request(queue_sha_rv(),batch_id_rv(),"failed",conditionMessage(e)),silent=TRUE)
      }
      status(paste("W01 resolution failed:",conditionMessage(e)))
      FALSE
    })
    if(dispatched) {
      w01_export_requested_rv(TRUE)
      w01_export_status_rv("Sent to GitHub. The reviewed batch is queued for integrity checks and resume.")
      status("W01 sent to GitHub.")
    }
  })
  observeEvent(input$w02_send_github, {
    req(authenticated())
    dispatch_completed_w02()
  })

  observeEvent(input$w04_resolution_send_github, {
    req(authenticated())
    dispatch_completed_w04_resolution()
  })

  observeEvent(input$w08_send_github, {
    req(authenticated())
    dispatch_completed_w08()
  })


  dispatch_completed_w02 <- function() {
    if(!session_can("control_workflows")) {
      w02_status("Review complete. Awaiting an administrator to resume Workflow 02.")
      return(FALSE)
    }
    if (!isTRUE(w02_handoff_ready())) {
      w02_status("Workflow 02 is not ready to send to GitHub because one or more cases remain unresolved.")
      return(FALSE)
    }

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
      w02_resume_requested_rv(TRUE)
      w02_status("Workflow 02 resume has already been requested for this batch.")
      return(TRUE)
    }

    tryCatch({
      if (!identical(w02_batch_status_rv(),"review_complete")) {
        mark_review_complete("02",w02_batch_id_rv(),w02_queue_sha_rv(),w02_batch_status_rv)
      }
      append_w02_resume_request(queue_sha, source_run_id, "dispatching")
      dispatch_w02_resume(source_run_id, publish = TRUE)
      append_w02_resume_request(queue_sha, source_run_id, "dispatched")
      w02_resume_requested_rv(TRUE)
      w02_status("Sent to GitHub. Workflow 02 resume requested.")
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
      if (isTRUE(w02_handoff_ready()) && session_can("control_workflows")) {
        w02_status("Review complete. Use Send W02 to GitHub in Administration & assignments.")
      } else if (isTRUE(w02_handoff_ready())) {
        w02_status("Review complete. Awaiting an administrator to send Workflow 02 to GitHub.")
      } else {
        w02_status("Your assigned enrichment review is complete. Other assigned or unassigned cases remain.")
      }
      app_view("tasks")
      return(invisible(TRUE))
    }
    later <- unresolved[unresolved > w02_idx()]
    w02_idx(if (length(later)) later[[1L]] else unresolved[[1L]])
    invisible(TRUE)
  }

  w04_screening_note_for <- function(review_case_id, user_id, queue_sha256 = "") {
    notes <- w04_screening_notes_rv() %||% list()
    if (!length(notes)) return(NULL)
    hits <- Filter(function(x) {
      same_case <- identical(
        as.character(x$review_case_id %||% ""),
        as.character(review_case_id %||% "")
      )
      same_user <- identical(
        as.character(x$reviewer %||% ""),
        as.character(user_id %||% "")
      )
      note_sha <- tolower(as.character(x$queue_sha256 %||% ""))
      want_sha <- tolower(as.character(queue_sha256 %||% ""))
      same_sha <- !nzchar(want_sha) || identical(note_sha,want_sha)
      same_case && same_user && same_sha
    }, notes)
    if (!length(hits)) NULL else hits[[1L]]
  }

  w04_current_note_text <- reactive({
    z <- w04_current_case()
    note <- w04_screening_note_for(
      z$review_case_id,
      session_reviewer_id(),
      w04_queue_sha_rv()
    )
    as.character(note$note %||% "")
  })

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
          class="d-flex align-items-start justify-content-between gap-2",
          div(
            class="record-title flex-grow-1",
            highlight_screening_text(display_sentence_case_if_all_caps(b$title %||% ""),w04_include_terms(),w04_exclude_terms())
          ),
          google_scholar_button(b$title %||% "")
        ),
        div(
          class="w04-citation-grid",
          div(class="w04-citation-item",span(class="w04-citation-label","Authors"),span(class="w04-citation-value",b$authors %||% "")),
          div(class="w04-citation-item",span(class="w04-citation-label","Year"),span(class="w04-citation-value",b$year %||% "")),
          div(class="w04-citation-item",span(class="w04-citation-label","Journal"),span(class="w04-citation-value",b$journal %||% "")),
          div(class="w04-citation-item",span(class="w04-citation-label","Volume"),span(class="w04-citation-value",b$volume %||% "")),
          div(class="w04-citation-item",span(class="w04-citation-label","Pages"),span(class="w04-citation-value",b$pages %||% ""))
        ),
        div(class="w04-doi",tags$strong("DOI: "),doi_link(b$doi %||% "")),
        tags$h6(class="abstract-heading","Abstract"),
        div(
          class="abstract-text",
          highlight_screening_text(display_sentence_case_if_all_caps(b$abstract %||% ""),w04_include_terms(),w04_exclude_terms())
        ),

        div(
          class="w04-keywords",
          tags$strong("Keywords: "),
          highlight_screening_text(b$keywords %||% "",w04_include_terms(),w04_exclude_terms())
        ),
        div(
          class="mt-3 pt-3 border-top",
          textAreaInput(
            "w04_note",
            "Notes",
            value=w04_current_note_text(),
            rows=3,
            width="100%",
            placeholder="Optional note for this record"
          ),
          div(
            class="d-flex align-items-center gap-2 flex-wrap",
            actionButton("w04_save_note","Save note",class="btn-outline-secondary btn-sm"),
            tags$span(class="saved-note",textOutput("w04_note_status",inline=TRUE))
          )
        )
      )
    )
  })

  output$w04_save_status <- renderText(w04_status())
  output$w04_note_status <- renderText(w04_note_status())

  dispatch_completed_w04_validation <- function() {
    if (!session_can("control_workflows")) {
      w04_status("Only an administrator can finalise a W04 validation batch.")
      return(FALSE)
    }

    state <- w04_validation_state()
    if (!isTRUE(state$ready)) {
      w04_status(paste0(
        "W04 validation set is not ready to send to GitHub (",
        as.character(state$reason %||% "incomplete"),
        ")."
      ))
      return(FALSE)
    }

    already <- tryCatch(
      w04_validation_finalize_request_exists(w04_queue_sha_rv(), w04_batch_id_rv()),
      error=function(e){w04_status(paste("Validation finalise status check failed:",conditionMessage(e)));NA}
    )
    if (is.na(already)) return(FALSE)
    if (isTRUE(already)) {
      w04_validation_finalize_requested_rv(TRUE)
      w04_status("Validation set has already been sent to GitHub for finalisation.")
      return(TRUE)
    }

    mark_review_complete(
      "04",
      w04_batch_id_rv(),
      w04_queue_sha_rv(),
      w04_batch_status_rv
    )
    dispatched <- tryCatch({
      append_w04_validation_finalize_request(
        w04_queue_sha_rv(), w04_batch_id_rv(), "dispatching"
      )
      dispatch_w04_validation_finalize(
        w04_batch_id_rv(),
        w04_queue_sha_rv()
      )
      append_w04_validation_finalize_request(
        w04_queue_sha_rv(), w04_batch_id_rv(), "dispatched"
      )
      TRUE
    }, error = function(e) {
      try(
        append_w04_validation_finalize_request(
          w04_queue_sha_rv(), w04_batch_id_rv(), "failed", conditionMessage(e)
        ),
        silent=TRUE
      )
      w04_status(paste(
        "Validation is complete, but W04 finalisation dispatch failed:",
        conditionMessage(e)
      ))
      FALSE
    })
    if (dispatched) {
      w04_validation_finalize_requested_rv(TRUE)
      w04_status("Validation set sent to GitHub for Workflow 04 finalisation.")
    }
    dispatched
  }

  observeEvent(input$w04_finalize_validation, {
    dispatch_completed_w04_validation()
  })

  observeEvent(input$w04_save_note, {
    req(authenticated())
    if (!session_can("adjudicate_assigned")) {
      w04_note_status("You do not have permission to save notes.")
      return()
    }
    if (!identical(storage_backend(),"google_sheets")) {
      w04_note_status("Notes are available in the production Google Sheets backend.")
      return()
    }

    z <- w04_current_case()
    note_text <- trimws(as.character(input$w04_note %||% ""))
    if (!nzchar(note_text)) {
      w04_note_status("Enter a note before saving.")
      return()
    }

    prior <- w04_screening_note_for(
      z$review_case_id,
      session_reviewer_id(),
      w04_queue_sha_rv()
    )
    item <- list(
      review_case_id=as.character(z$review_case_id),
      record_id=as.character(z$record_id %||% ""),
      note=note_text,
      reviewer=session_reviewer_id(),
      saved_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"),
      queue_sha256=w04_queue_sha_rv()
    )
    saved <- tryCatch(
      append_sheet_w04_screening_note(item,prior_note=prior),
      error=function(e){w04_note_status(paste("Note save failed:",conditionMessage(e)));NULL}
    )
    if (is.null(saved)) return()

    notes <- w04_screening_notes_rv() %||% list()
    notes <- Filter(function(x) !(
      identical(as.character(x$review_case_id %||% ""),as.character(z$review_case_id)) &&
      identical(as.character(x$reviewer %||% ""),session_reviewer_id()) &&
      identical(
        tolower(as.character(x$queue_sha256 %||% "")),
        tolower(as.character(w04_queue_sha_rv() %||% ""))
      )
    ),notes)
    w04_screening_notes_rv(c(notes,list(saved)))
    w04_note_status(sprintf("Note saved at %s",format(Sys.time(),"%H:%M:%S")))
  })

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
      validation_state <- w04_validation_state()
      if (!all_complete) {
        w04_status("Your blinded review is complete. Waiting for the other assigned reviewer(s).")
      } else if (identical(validation_state$mode, "validation_set")) {
        if (isTRUE(validation_state$ready) && session_can("control_workflows")) {
          w04_status("Validation set complete. Use Send validation set to GitHub in Administration & assignments.")
        } else if (isTRUE(validation_state$ready)) {
          w04_status("Validation set complete. Awaiting an administrator to finalise Workflow 04.")
        } else if (session_can("manage_assignments")) {
          w04_status("Assigned validation screening is complete, but the batch is not yet fully covered for finalisation.")
        } else {
          w04_status("Your assigned validation screening is complete.")
        }
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

  w04_resolution_current_abstract_edit <- reactive({
    z <- w04_resolution_current_case()
    edits <- w04_resolution_abstract_edits_rv() %||% list()
    hits <- Filter(
      function(x) identical(as.character(x$review_case_id %||% ""), as.character(z$review_case_id %||% "")),
      edits
    )
    if (!length(hits)) return(NULL)
    hits[[1L]]
  })

  w04_resolution_effective_abstract <- reactive({
    edit <- w04_resolution_current_abstract_edit()
    if (!is.null(edit) && nzchar(trimws(as.character(edit$abstract %||% "")))) {
      return(as.character(edit$abstract))
    }
    z <- w04_resolution_current_case()
    as.character((z$bibliographic %||% list())$abstract %||% "")
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
        div(
          class="d-flex align-items-start justify-content-between gap-2",
          div(class="record-title flex-grow-1",highlight_screening_text(display_sentence_case_if_all_caps(b$title %||% ""),w04_resolution_include_terms(),w04_resolution_exclude_terms())),
          google_scholar_button(b$title %||% "")
        ),
        div(class="w04-citation-grid",
          div(class="w04-citation-item",span(class="w04-citation-label","Authors"),span(class="w04-citation-value",b$authors %||% "")),
          div(class="w04-citation-item",span(class="w04-citation-label","Year"),span(class="w04-citation-value",b$year %||% "")),
          div(class="w04-citation-item",span(class="w04-citation-label","Journal"),span(class="w04-citation-value",b$journal %||% "")),
          div(class="w04-citation-item",span(class="w04-citation-label","Volume"),span(class="w04-citation-value",b$volume %||% "")),
          div(class="w04-citation-item",span(class="w04-citation-label","Pages"),span(class="w04-citation-value",b$pages %||% ""))
        ),
        div(class="w04-doi",tags$strong("DOI: "),doi_link(b$doi %||% "")),
        div(
          class="d-flex justify-content-between align-items-center mt-2",
          tags$h6(class="abstract-heading mb-0","Abstract"),
          div(
            class="d-flex align-items-center gap-2",
            if (isTRUE(w04_resolution_abstract_edit_rv())) {
              actionButton("w04_resolution_save_abstract","Save abstract",class="btn-sm btn-primary")
            },
            actionButton(
              "w04_resolution_edit_abstract",
              if (isTRUE(w04_resolution_abstract_edit_rv())) "Cancel edit" else "Edit abstract",
              class="btn-sm btn-outline-secondary"
            )
          )
        ),
        if (isTRUE(w04_resolution_abstract_edit_rv())) {
          tagList(
            textAreaInput(
              "w04_resolution_abstract_text",
              label=NULL,
              value=w04_resolution_effective_abstract(),
              rows=10,
              width="100%",
              placeholder="Paste or correct the abstract here."
            ),
            tags$div(
              class="text-secondary small mb-2",
              "Saved abstracts are carried into the canonical record when W04 is sent to GitHub."
            )
          )
        } else {
          tagList(
            div(class="abstract-text",highlight_screening_text(display_sentence_case_if_all_caps(w04_resolution_effective_abstract()),w04_resolution_include_terms(),w04_resolution_exclude_terms())),
            if (!is.null(w04_resolution_current_abstract_edit())) {
              tags$div(class="text-secondary small mt-1","Manually edited abstract saved for this W04 resolution.")
            }
          )
        },
        div(class="mt-3 p-2 border rounded",
          tags$strong("Model decisions: "),
          if(length(votes)) tagList(lapply(seq_along(votes),function(i)tags$span(class="task-badge me-1",sprintf("Pass %d: %s",i,votes[[i]])))) else tags$span(class="text-secondary","No model vote provenance available")
        ),
        div(class="w04-keywords",tags$strong("Keywords: "),highlight_screening_text(b$keywords %||% "",w04_resolution_include_terms(),w04_resolution_exclude_terms()))
      )
    )
  })
  output$w04_resolution_save_status <- renderText(w04_resolution_status())

  observeEvent(input$w04_resolution_edit_abstract, {
    w04_resolution_abstract_edit_rv(!isTRUE(w04_resolution_abstract_edit_rv()))
  })

  observeEvent(input$w04_resolution_save_abstract, {
    if(!session_can("adjudicate_assigned")) {
      w04_resolution_status("You do not have permission to edit this record.")
      return()
    }
    z <- w04_resolution_current_case()
    value <- trimws(as.character(input$w04_resolution_abstract_text %||% ""))
    if (!nzchar(value)) {
      w04_resolution_status("Abstract cannot be empty.")
      return()
    }
    current <- w04_resolution_abstract_edits_rv() %||% list()
    hits <- Filter(
      function(x) identical(as.character(x$review_case_id %||% ""),as.character(z$review_case_id %||% "")),
      current
    )
    prior <- if(length(hits)) hits[[1L]] else NULL
    edit <- list(
      review_case_id=as.character(z$review_case_id),
      record_id=as.character(z$record_id),
      abstract=value,
      reviewer=session_reviewer_id(),
      saved_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"),
      queue_sha256=w04_resolution_queue_sha_rv()
    )
    saved <- tryCatch(
      append_sheet_w04_resolution_abstract_edit(edit, prior_edit=prior),
      error=function(e){w04_resolution_status(paste("Abstract save failed:",conditionMessage(e)));NULL}
    )
    if(is.null(saved)) return()
    remaining <- Filter(
      function(x)!identical(as.character(x$review_case_id %||% ""),as.character(z$review_case_id %||% "")),
      current
    )
    w04_resolution_abstract_edits_rv(c(remaining,list(saved)))
    w04_resolution_abstract_edit_rv(FALSE)
    w04_resolution_status(sprintf("Saved abstract at %s. Choose Include or Exclude to resolve the record.",format(Sys.time(),"%H:%M:%S")))
  })

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

  dispatch_completed_w04_resolution <- function() {
    if (!session_can("control_workflows")) {
      w04_resolution_status("Review complete. Awaiting an administrator to send Workflow 04 to GitHub.")
      return(FALSE)
    }
    if (!isTRUE(w04_resolution_handoff_ready())) {
      w04_resolution_status("Workflow 04 model-uncertainty review is not ready to send to GitHub.")
      return(FALSE)
    }

    source_run_id <- as.character(w04_resolution_source_run_id_rv())
    batch_id <- as.character(w04_resolution_batch_id_rv())
    queue_sha <- as.character(w04_resolution_queue_sha_rv())
    already <- tryCatch(
      w04_resolution_resume_request_exists(queue_sha,source_run_id,batch_id),
      error=function(e){w04_resolution_status(paste("Resume status check failed:",conditionMessage(e)));NA}
    )
    if (is.na(already)) return(FALSE)
    if (isTRUE(already)) {
      w04_resolution_resume_requested_rv(TRUE)
      w04_resolution_status("Workflow 04 has already been sent to GitHub for this model-uncertainty batch.")
      return(TRUE)
    }

    tryCatch({
      if (!identical(w04_resolution_batch_status_rv(),"review_complete")) {
        mark_review_complete("04",batch_id,queue_sha,w04_resolution_batch_status_rv)
      }
      append_w04_resolution_resume_request(queue_sha,source_run_id,batch_id,"dispatching")
      dispatch_w04_resolution_resume(source_run_id,batch_id,queue_sha)
      append_w04_resolution_resume_request(queue_sha,source_run_id,batch_id,"dispatched")
      w04_resolution_resume_requested_rv(TRUE)
      w04_resolution_status("Sent to GitHub. Workflow 04 finalisation requested.")
      TRUE
    },error=function(e){
      try(append_w04_resolution_resume_request(queue_sha,source_run_id,batch_id,"failed",conditionMessage(e)),silent=TRUE)
      w04_resolution_status(paste("Workflow 04 send failed:",conditionMessage(e)))
      FALSE
    })
  }

  advance_w04_resolution <- function() {
    unresolved<-w04_resolution_unresolved_indices()
    if(!length(unresolved)){
      if (isTRUE(w04_resolution_handoff_ready()) && session_can("control_workflows")) {
        w04_resolution_status("Review complete. Use Send W04 to GitHub in Administration & assignments.")
      } else if (isTRUE(w04_resolution_handoff_ready())) {
        w04_resolution_status("Review complete. Awaiting an administrator to send Workflow 04 to GitHub.")
      } else {
        w04_resolution_status("Your assigned model-uncertainty records are complete. Other assigned or unassigned records remain.")
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
    case_decisions <- reviewer_decisions
    model_decision <- w04_model_decision_from_case(z)
    if (nzchar(model_decision) && !"model" %in% names(case_decisions)) {
      case_decisions$model <- list(
        decision=model_decision,
        complete=TRUE,
        source="model_context"
      )
    }
    reviewer_badges <- if (length(case_decisions)) {
      ids <- names(case_decisions)
      if (is.null(ids) || any(!nzchar(ids))) {
        ids <- paste0("rater-",seq_along(case_decisions))
      }
      lapply(seq_along(case_decisions), function(i) {
        d <- as.character(case_decisions[[i]]$decision %||% "")
        label <- c(retain="Include",exclude="Exclude",uncertain="Unsure")[[d]] %||% d
        badge_class <- if (identical(d,"retain")) {
          "decision-badge-include"
        } else if (identical(d,"exclude")) {
          "decision-badge-exclude"
        } else {
          "decision-badge-neutral"
        }
        tags$span(
          class=paste("task-badge me-1",badge_class),
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

    human_ids <- setdiff(as.character(comparison_ids %||% character()),"model")
    parent_sha <- {
      set <- w04_active_consistency_conflict_set()
      if (!is.null(set)) {
        as.character(set$parent_queue_sha256 %||% "")
      } else {
        as.character(w04_queue_sha_rv() %||% "")
      }
    }
    case_notes <- if (length(human_ids) >= 2L) {
      Filter(function(x) {
        identical(
          as.character(x$review_case_id %||% ""),
          as.character(z$review_case_id %||% "")
        ) &&
        as.character(x$reviewer %||% "") %in% human_ids &&
        (
          !nzchar(parent_sha) ||
          identical(
            tolower(as.character(x$queue_sha256 %||% "")),
            tolower(parent_sha)
          )
        ) &&
        nzchar(trimws(as.character(x$note %||% "")))
      }, w04_screening_notes_rv() %||% list())
    } else list()

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
        div(
          class="d-flex align-items-start justify-content-between gap-2",
          div(class="record-title flex-grow-1",highlight_screening_text(display_sentence_case_if_all_caps(b$title %||% ""),w04_include_terms(),w04_exclude_terms())),
          google_scholar_button(b$title %||% "")
        ),
        div(
          class="w04-citation-grid",
          div(class="w04-citation-item",span(class="w04-citation-label","Authors"),span(class="w04-citation-value",b$authors %||% "")),
          div(class="w04-citation-item",span(class="w04-citation-label","Year"),span(class="w04-citation-value",b$year %||% "")),
          div(class="w04-citation-item",span(class="w04-citation-label","Journal"),span(class="w04-citation-value",b$journal %||% ""))
        ),
        tags$h6(class="abstract-heading","Abstract"),
        div(class="abstract-text",highlight_screening_text(display_sentence_case_if_all_caps(b$abstract %||% ""),w04_include_terms(),w04_exclude_terms())),
        if (length(reviewer_badges)) {
          div(
            class="mt-3 p-2 border rounded",
            tags$strong("Decisions for this case: "),
            tagList(reviewer_badges)
          )
        },
        if (length(case_notes)) {
          div(
            class="mt-3 p-2 border rounded",
            tags$strong("Reviewer notes"),
            tagList(lapply(case_notes,function(n) {
              div(
                class="mt-2",
                tags$div(class="fw-semibold",rater_label(as.character(n$reviewer %||% ""))),
                tags$div(class="text-body",as.character(n$note %||% ""))
              )
            }))
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
      ontology_pathology="Topic ontology verification",
      topic_coding_review="Topic coding review",
      zero_topic_eligibility_uncertain="Zero-topic eligibility review"
    )
    allowed <- as.character(issue$allowed_human_outcomes %||% character())
    if (typ %in% c("geography_unresolved","geography_evidence_unvalidated")) {
      allowed <- c("accept_model","override_country_set","assign_none","exclude_record")
    } else if (identical(typ,"geography_model_failure")) {
      allowed <- c("assign_country_set","assign_none","exclude_record")
    } else if (typ %in% c("ontology_pathology","topic_coding_review")) {
      allowed <- c("accept_retained_topics","replace_topic_set","no_code","exclude_record")
    } else if (identical(typ,"zero_topic_eligibility_uncertain")) {
      allowed <- c("assign_topic_set","include_uncoded","exclude_record")
    }
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
      assign_topic_set="Assign topic set",
      include_uncoded="Retain uncoded"
    )
    choice_labels <- unname(labels[allowed])
    names(choice_labels) <- allowed
    choices <- setNames(allowed,unname(choice_labels[allowed]))

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
      ontology_pathology = {
        ps <- av$pathways %||% list()
        items <- lapply(ps,function(p)tags$li(
          paste0(as.character(p$hierarchy_path %||% p$path_id %||% ""),
                 if(nzchar(as.character(p$stars %||% ""))) paste0(" · ",p$stars) else "")
        ))
        tagList(
          tags$p(class="mb-1","Residual ontology conflict after deterministic topic pruning."),
          tags$ul(class="mb-2",items)
        )
      },
      topic_coding_review = {
        ps <- av$pathways %||% list()
        items <- lapply(ps,function(p)tags$li(
          paste0(as.character(p$hierarchy_path %||% p$path_id %||% ""),
                 if(nzchar(as.character(p$stars %||% ""))) paste0(" · ",p$stars) else "")
        ))
        tagList(tags$ul(class="mb-2",items))
      },
      zero_topic_eligibility_uncertain = tags$p(class="mb-2","No retained topic was assigned; verify eligibility and assign topics if appropriate."),
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
      geography_unresolved = {
        model_selected <- toupper(as.character(unlist(
          saved_value$iso3c %||% av$luna_iso3c %||% av$iso3c %||% character(),
          use.names = FALSE
        )))
        tagList(
          selectizeInput(
            paste0("w08_geo_",j),
            "Override countries",
            choices = w08_country_choices(),
            selected = model_selected[nzchar(model_selected)],
            multiple = TRUE,
            options = list(create = FALSE, persist = FALSE)
          )
        )
      },
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
      ontology_pathology = {
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
      topic_coding_review = {
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
      zero_topic_eligibility_uncertain = {
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
              "Topic set",
              choices = topic_choices,
              selected = as.character(unlist(saved_value$path_ids %||% character(), use.names = FALSE)),
              multiple = TRUE,
              options = list(create = FALSE, persist = FALSE)
            )
          } else {
            tags$div(
              class = "text-danger small",
              "Accepted topic ontology is unavailable for this queue. Topic assignment is disabled."
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
            highlight_named_terms(display_sentence_case_if_all_caps(z$title %||% ""),highlight_terms)
          ),
          tags$h6(class="abstract-heading","Abstract"),
          div(
            class="abstract-text",
            highlight_named_terms(display_sentence_case_if_all_caps(z$abstract %||% ""),highlight_terms)
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
      if (typ %in% c("geography_unresolved","geography_evidence_unvalidated")) {
        allowed <- c("accept_model","override_country_set","assign_none","exclude_record")
      } else if (identical(typ,"geography_model_failure")) {
        allowed <- c("assign_country_set","assign_none","exclude_record")
      } else if (typ %in% c("ontology_pathology","topic_coding_review")) {
        allowed <- c("accept_retained_topics","replace_topic_set","no_code","exclude_record")
      } else if (identical(typ,"zero_topic_eligibility_uncertain")) {
        allowed <- c("assign_topic_set","include_uncoded","exclude_record")
      }
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
        if(choice=="override_country_set" || (identical(typ,"geography_model_failure") && choice=="assign_country_set")) {
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
          final_value <- NULL
        } else if(choice=="exclude_record") {
          final_value <- list(included=FALSE)
        }
      } else if(typ %in% c("topic_extreme_disagreement","ontology_pathology","topic_coding_review")) {
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
        if(choice=="assign_topic_set") {
          opts <- w08_topic_options()
          allowed_topic_ids <- if(length(opts)) {
            unique(vapply(opts,function(x)as.character(x$path_id %||% ""),character(1)))
          } else character()
          allowed_topic_ids <- allowed_topic_ids[nzchar(allowed_topic_ids)]
          vals <- unique(as.character(input[[paste0("w08_topics_",j)]] %||% character()))
          vals <- vals[nzchar(vals)]
          if(!length(allowed_topic_ids)) {
            w08_status("Accepted topic ontology is unavailable; topic assignment is disabled.")
            return(FALSE)
          }
          if(!length(vals)) {
            w08_status("Select at least one topic.")
            return(FALSE)
          }
          if(any(!vals %in% allowed_topic_ids)) {
            w08_status("Assigned topics must be selected from the accepted project ontology.")
            return(FALSE)
          }
          final_value <- list(included=TRUE,path_ids=vals)
        } else if(choice=="include_uncoded") {
          final_value <- list(included=TRUE,path_ids=character())
        } else if(choice=="exclude_record") {
          final_value <- list(included=FALSE)
        }
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

  dispatch_completed_w08 <- function() {
    if (!session_can("control_workflows")) {
      w08_status("Review complete. Awaiting an administrator to send Workflow 08 to GitHub.")
      return(FALSE)
    }
    if (!isTRUE(w08_handoff_ready())) {
      w08_status("Workflow 08 is not ready to send to GitHub because one or more records remain unresolved.")
      return(FALSE)
    }

    source_run_id <- as.character(w08_source_run_id_rv())
    batch_id <- as.character(w08_batch_id_rv())
    queue_sha <- as.character(w08_queue_sha_rv())
    already <- tryCatch(
      w08_resume_request_exists(queue_sha,source_run_id,batch_id),
      error=function(e){w08_status(paste("Resume status check failed:",conditionMessage(e)));NA}
    )
    if (is.na(already)) return(FALSE)
    if (isTRUE(already)) {
      w08_resume_requested_rv(TRUE)
      w08_status("Workflow 08 has already been sent to GitHub for this batch.")
      return(TRUE)
    }

    tryCatch({
      if (!identical(w08_batch_status_rv(),"review_complete")) {
        mark_review_complete("08",batch_id,queue_sha,w08_batch_status_rv)
      }
      append_w08_resume_request(queue_sha,source_run_id,batch_id,"dispatching")
      dispatch_w08_resume(source_run_id,batch_id,queue_sha)
      append_w08_resume_request(queue_sha,source_run_id,batch_id,"dispatched")
      w08_resume_requested_rv(TRUE)
      w08_status("Sent to GitHub. Workflow 08 resume requested.")
      TRUE
    },error=function(e){
      try(append_w08_resume_request(queue_sha,source_run_id,batch_id,"failed",conditionMessage(e)),silent=TRUE)
      w08_status(paste("Workflow 08 send failed:",conditionMessage(e)))
      FALSE
    })
  }

  advance_w08 <- function() {
    unresolved <- w08_unresolved_indices()
    if(!length(unresolved)) {
      if (isTRUE(w08_handoff_ready()) && session_can("control_workflows")) {
        w08_status("Review complete. Use Send W08 to GitHub in Administration & assignments.")
      } else if (isTRUE(w08_handoff_ready())) {
        w08_status("Review complete. Awaiting an administrator to send Workflow 08 to GitHub.")
      } else {
        w08_status("Your assigned annotation review is complete. Other assigned or unassigned records remain.")
      }
      app_view("tasks")
      return(invisible(TRUE))
    }
    later <- unresolved[unresolved>w08_idx()]
    w08_idx(if(length(later)) later[[1L]] else unresolved[[1L]])
    invisible(TRUE)
  }

  save_w01_abstract_repair <- function(side = c("a","b"), explicit_action = NULL, explicit_value = NULL) {
    side <- match.arg(side)
    req(authenticated())
    if(!session_can("adjudicate_assigned")) {
      status("You do not have permission to edit W01 record metadata.")
      return(invisible(FALSE))
    }
    z <- current_case()
    rec <- if (identical(side,"a")) z$record_i else z$record_j
    if (is.null(explicit_action)) {
      value <- trimws(as.character(if (identical(side,"a")) input$w01_abstract_a else input$w01_abstract_b))
      action <- if (nzchar(value)) "replace_abstract" else "strip_abstract"
    } else {
      action <- as.character(explicit_action)
      value <- as.character(explicit_value %||% "")
    }
    original <- trimws(as.character(rec$abstract %||% ""))
    active <- w01_repairs_rv() %||% list()
    hits <- Filter(function(x) {
      identical(as.character(x$source %||% ""), as.character(rec$source %||% "")) &&
        identical(as.character(x$source_record_id %||% ""), as.character(rec$source_record_id %||% ""))
    }, active)
    prior <- if (length(hits)) hits[[1L]] else NULL
    if (identical(value, original) && is.null(prior)) {
      status("No abstract change to save.")
      return(invisible(FALSE))
    }
    repair <- list(
      review_case_id=as.character(z$review_case_id),
      source=as.character(rec$source),
      source_record_id=as.character(rec$source_record_id),
      action=action,
      value=value,
      reason=if (identical(action,"strip_abstract")) "human_abstract_removal_during_deduplication" else "human_abstract_correction_during_deduplication",
      reviewer=session_reviewer_id(),
      saved_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"),
      queue_sha256=queue_sha_rv()
    )
    saved <- tryCatch(
      save_active_w01_repair(repair, w01_repair_path, prior_repair=prior),
      error=function(e) {
        status(paste("Abstract correction save failed:", conditionMessage(e)))
        NULL
      }
    )
    if (is.null(saved)) return(invisible(FALSE))
    remaining <- Filter(function(x) !(
      identical(as.character(x$source %||% ""), as.character(rec$source %||% "")) &&
      identical(as.character(x$source_record_id %||% ""), as.character(rec$source_record_id %||% ""))
    ), active)
    w01_repairs_rv(c(remaining,list(saved)))
    w01_abstract_edit_rv(NULL)
    status(sprintf(
      "%s abstract for Record %s.",
      if (identical(action,"strip_abstract")) "Removed" else "Saved corrected",
      toupper(side)
    ))
    invisible(TRUE)
  }

  observeEvent(input$edit_w01_abstract_a, {
    z <- current_case()
    w01_abstract_edit_rv(list(review_case_id=as.character(z$review_case_id),side="a"))
  })
  observeEvent(input$edit_w01_abstract_b, {
    z <- current_case()
    w01_abstract_edit_rv(list(review_case_id=as.character(z$review_case_id),side="b"))
  })
  observeEvent(input$cancel_w01_abstract_a, {
    w01_abstract_edit_rv(NULL)
  })
  observeEvent(input$cancel_w01_abstract_b, {
    w01_abstract_edit_rv(NULL)
  })

  request_w01_abstract_delete <- function(side = c("a","b")) {
    side <- match.arg(side)
    z <- current_case()
    rec <- if (identical(side,"a")) z$record_i else z$record_j
    w01_abstract_delete_rv(list(
      review_case_id=as.character(z$review_case_id),
      side=side,
      source=as.character(rec$source %||% ""),
      source_record_id=as.character(rec$source_record_id %||% "")
    ))
    showModal(modalDialog(
      title = paste0("Delete abstract from Record ", toupper(side), "?"),
      "This will remove the abstract from this source record and save the change as an audited data-quality repair.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_w01_delete_abstract", "Delete abstract", class="btn-danger")
      ),
      easyClose = TRUE
    ))
  }

  observeEvent(input$delete_w01_abstract_a, {
    request_w01_abstract_delete("a")
  })
  observeEvent(input$delete_w01_abstract_b, {
    request_w01_abstract_delete("b")
  })

  observeEvent(input$confirm_w01_delete_abstract, {
    pending <- w01_abstract_delete_rv()
    req(pending)
    z <- current_case()
    if (!identical(as.character(pending$review_case_id), as.character(z$review_case_id))) {
      removeModal()
      w01_abstract_delete_rv(NULL)
      status("Delete cancelled because the active W01 case changed.")
      return()
    }
    side <- as.character(pending$side)
    ok <- save_w01_abstract_repair(
      side,
      explicit_action="strip_abstract",
      explicit_value=""
    )
    removeModal()
    w01_abstract_delete_rv(NULL)
    if (isTRUE(ok)) w01_abstract_edit_rv(NULL)
  })

    observeEvent(input$save_w01_abstract_a, {
    save_w01_abstract_repair("a")
  })
  observeEvent(input$save_w01_abstract_b, {
    save_w01_abstract_repair("b")
  })

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
        status("All W01 assignments are complete. Use Mark W01 as resolved in Administration & assignments.")
      } else {
        status("Review complete. Awaiting an administrator to mark Workflow 01 as resolved.")
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
    w04_resolution_abstract_edit_rv(FALSE)
    app_view("w04_resolution")
  })
  observeEvent(input$back_to_tasks_w04_resolution, app_view("tasks"))
  observeEvent(input$w04_resolution_retain, {if(save_w04_resolution_choice("retain"))advance_w04_resolution()})
  observeEvent(input$w04_resolution_exclude, {if(save_w04_resolution_choice("exclude"))advance_w04_resolution()})
  observeEvent(input$w04_resolution_previous, {
    if(w04_resolution_idx()>1L) {
      w04_resolution_idx(w04_resolution_idx()-1L)
      w04_resolution_abstract_edit_rv(FALSE)
    }
  })
  observeEvent(input$w04_resolution_next, {
    if(w04_resolution_idx()<length(w04_resolution_cases_rv())) {
      w04_resolution_idx(w04_resolution_idx()+1L)
      w04_resolution_abstract_edit_rv(FALSE)
    }
  })
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

  observeEvent(app_view(), {
    req(authenticated())
    if (!identical(app_view(), "tasks")) return()

    refreshed <- tryCatch(
      if(identical(storage_backend(),"google_sheets")) read_latest_pipeline_status() else NULL,
      error=function(e) NULL
    )
    if(!is.null(refreshed)) {
      pipeline_status_rv(refreshed)
      if (w08_status_is_final(refreshed)) {
        authoritative_w08_rv(tryCatch(
          read_authoritative_w08_metrics(),
          error=function(e) NULL
        ))
      } else {
        authoritative_w08_rv(NULL)
      }
    }

    screening <- tryCatch(read_manual_screening_metrics(), error=function(e)e)
    if (!inherits(screening,"error")) {
      manual_screening_rv(screening)
    }
  }, ignoreInit=TRUE)

  observeEvent(input$back_to_tasks_complete, {
    complete(FALSE)
    app_view("tasks")
  })

  observeEvent(input$duplicate, {
    w01_abstract_edit_rv(NULL)
    w01_abstract_delete_rv(NULL)
    if (save_choice("duplicate")) advance_after_save()
  })
  observeEvent(input$not_duplicate, {
    w01_abstract_edit_rv(NULL)
    w01_abstract_delete_rv(NULL)
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
