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
source("R/w04_prompt_versions.R", local = TRUE)
source("R/w04_prompt_zenodo.R", local = TRUE)
source("R/w04_shiny_model_launch.R", local = TRUE)
source("R/w04_model_batches.R", local = TRUE)
source("R/w05_crop_coding.R", local = TRUE)
source("R/w05_crop_archive.R", local = TRUE)
source("R/w06_shiny_prompt.R", local = TRUE)
source("R/w06_shiny_subset_dispatch.R", local = TRUE)
source("R/w07_shiny_prepare_dispatch.R", local = TRUE)
source("R/w07_shiny_configuration.R", local = TRUE)
source("R/w07_shiny_subset_dispatch.R", local = TRUE)
source("R/w07_shiny_results.R", local = TRUE)
source("R/w05_shiny_subset_dispatch.R", local = TRUE)
source("R/w05_shiny_subset_results.R", local = TRUE)
source("R/w04_workspace_overview.R", local = TRUE)
source("R/w04_work_overview_sheets.R", local = TRUE)
source("R/w04_model_conflicts.R", local = TRUE)
source("R/w04_prompt_refinement.R", local = TRUE)
source("R/w04_highlight_sheets.R", local = TRUE)
source("R/w04_subset_registry.R", local = TRUE)
source("R/w04_subset_storage.R", local = TRUE)
source("R/w04_subset_eligible.R", local = TRUE)
source("R/w04_subset_review_source.R", local = TRUE)
source("R/w04_subset_screening_adapter.R", local = TRUE)
source("R/w04_subset_queue_builder.R", local = TRUE)
source("R/w04_subset_queue_storage.R", local = TRUE)
source("R/storage_local.R", local = TRUE)
source("R/storage_sheets.R", local = TRUE)
source("R/w08b_review.R", local = TRUE)
source("R/storage_backend.R", local = TRUE)
source("R/auth.R", local = TRUE)
source("R/github_dispatch.R", local = TRUE)
source("R/search_boolean_validation.R", local = TRUE)
source("R/search_draft_storage.R", local = TRUE)
source("R/search_strategy_versions.R", local = TRUE)
source("R/search_source_selection.R", local = TRUE)
source("R/manual_ris.R", local = TRUE)
source("R/manual_search_storage.R", local = TRUE)

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

highlight_plural_variants <- function(term) {
  term <- trimws(as.character(term))
  if (!nzchar(term)) return(character())
  # Only regular singular/plural forms. Never use substring matching.
  if (grepl("[^aeiou]ies$", term, ignore.case=TRUE))
    return(unique(c(term, sub("ies$", "y", term, ignore.case=TRUE))))
  if (grepl("[^aeiou]y$", term, ignore.case=TRUE))
    return(unique(c(term, paste0(sub("y$", "", term, ignore.case=TRUE), "ies"))))
  if (grepl("(ch|sh|x|z|ss)es$", term, ignore.case=TRUE))
    return(unique(c(term, sub("es$", "", term, ignore.case=TRUE))))
  if (grepl("(ch|sh|x|z|ss)$", term, ignore.case=TRUE))
    return(unique(c(term, paste0(term, "es"))))
  if (grepl("s$", term, ignore.case=TRUE)) return(unique(c(term, sub("s$", "", term, ignore.case=TRUE))))
  unique(c(term, paste0(term, "s")))
}

highlight_screening_text <- function(text, include_terms = character(), exclude_terms = character()) {
  # Use only terms supplied by the active review's Google Sheets configuration.
  # Accept comma-separated lists as well as newline-separated entries.
  # Existing saved Google Sheets payloads remain unchanged: splitting happens
  # only for the display matcher, not for persistence or checksum validation.
  split_highlight_entries <- function(values) {
    # Split literal commas and literal line breaks, never the letters r or n.
    entries <- unlist(strsplit(as.character(values), ",", fixed=TRUE),use.names=FALSE)
    entries <- unlist(strsplit(gsub(intToUtf8(13L), intToUtf8(10L),
      entries, fixed=TRUE),intToUtf8(10L),fixed=TRUE),use.names=FALSE)
    entries <- trimws(entries)
    unique(entries[nzchar(entries)])
  }
  include_terms <- unique(unlist(lapply(split_highlight_entries(include_terms),
    highlight_plural_variants), use.names=FALSE))
  exclude_terms <- unique(unlist(lapply(split_highlight_entries(exclude_terms),
    highlight_plural_variants), use.names=FALSE))
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
    # Locate every occurrence against the same full string. This avoids
    # offsets being recalculated on progressively shortened substrings.
    positions <- gregexpr(needle, hay, fixed = TRUE)[[1L]]
    if (length(positions) == 1L && positions[[1L]] < 0L) next
    for (s in positions) {
      e <- s + nchar(needle, type="chars") - 1L
      before <- if (s > 1L) substr(hay, s - 1L, s - 1L) else ""
      after <- if (e < nchar(hay)) substr(hay, e + 1L, e + 1L) else ""
      starts_word <- grepl("^[[:alnum:]]", needle)
      ends_word <- grepl("[[:alnum:]]$", needle)
      left_ok <- !starts_word || !nzchar(before) || !grepl("[[:alnum:]]", before)
      right_ok <- !ends_word || !nzchar(after) || !grepl("[[:alnum:]]", after)
      if (left_ok && right_ok) {
        k <- k + 1L
        candidates[[k]] <- list(start=as.integer(s),end=as.integer(e),
          class=names(terms)[[i]],length=nchar(needle,type="chars"))
      }
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
    out[[length(out)+1L]] <- tags$span(
      class=x$class,
      style=if (identical(x$class,"screen-include"))
        "background-color:#d9f2df!important;color:#145c2e!important;"
      else "background-color:#fde0e0!important;color:#8b1e1e!important;",
      substr(text,x$start,x$end)
    )
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

record_card <- function(rec, label, fields, side = c("a","b"), abstract_editing = FALSE, abstract_prefix = "w01") {
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
            paste0(abstract_prefix, "_abstract_", side),
            NULL,
            value = as.character(rec$display_abstract %||% rec$abstract %||% ""),
            rows = 6,
            width = "100%"
          ),
          div(
            class = "d-flex align-items-center gap-2 flex-wrap",
            actionButton(
              paste0("save_", abstract_prefix, "_abstract_", side),
              "Save abstract",
              class = "btn-primary btn-sm"
            ),
            actionButton(
              paste0("cancel_", abstract_prefix, "_abstract_", side),
              "Cancel",
              class = "btn-outline-secondary btn-sm"
            )
          )
        )
      } else {
        tagList(
          div(class = "abstract-text",
            if (isTRUE(rec$abstract_deleted)) tags$span(class="text-muted fst-italic","Abstract deleted")
            else fields$abstract[[side]]),
          div(
            class = "d-flex align-items-center gap-2 flex-wrap mt-2",
            actionButton(
              paste0("edit_", abstract_prefix, "_abstract_", side),
              "Edit abstract",
              class = "btn-outline-secondary btn-sm"
            ),
            actionButton(
              paste0("delete_", abstract_prefix, "_abstract_", side),
              "Delete abstract",
              class = "btn-outline-danger btn-sm"
            ),
            if (isTRUE(rec$abstract_repair_saved) && !isTRUE(rec$abstract_deleted)) {
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
            // Restore only on newly mounted elements. Reapplying stale
            // sessionStorage state to existing details races the toggle event
            // and can collapse a disclosure immediately after opening it.
            if (!el.dataset.lemBound) {
              el.dataset.lemBound = '1';
              const saved = sessionStorage.getItem(key);
              if (saved === 'open') el.open = true;
              if (saved === 'closed') el.open = false;
              el.addEventListener('toggle', function() {
                sessionStorage.setItem(key, el.open ? 'open' : 'closed');
              });
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
            const edit = ev.target.closest('.lem-record-edit');
            if (edit) {
              ev.preventDefault();
              Shiny.setInputValue('record_table_edit', {
                workflow: edit.dataset.workflow || '',
                task_type: edit.dataset.taskType || '',
                batch_id: edit.dataset.batchId || '',
                case_id: edit.dataset.caseId || ''
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
    .assignment-kpis.four { grid-template-columns:repeat(4,minmax(100px,1fr)); }
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
    .pipeline-kpi-value-row { display:block; white-space:nowrap; }
    .pipeline-kpi-value { display:block; font-size:1.12rem; line-height:1.2; font-weight:700; white-space:nowrap; }
    .pipeline-kpi-value.compact { font-size:1rem; }
    .pipeline-kpi-inline-note { display:block; color:#7c858d; font-size:.7rem; line-height:1.2; font-weight:600; white-space:nowrap; margin-top:.08rem; }
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
  registry_path = Sys.getenv(
    "LEM_W08_REGISTRY_PATH",
    unset = "docs/workflow08/zenodo_registry.csv"
  ),
  pointer_base = Sys.getenv(
    "LEM_W08_POINTER_BASE_PATH",
    unset = "docs/workflow08/zenodo"
  ),
  ref = github_scoping_ref()
) {
  if (file.exists(registry_path)) {
    reg <- utils::read.csv(
      registry_path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else {
    registry_text <- read_github_text_file(registry_path, ref = ref)
    reg <- utils::read.csv(
      text = registry_text,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  required <- c("source_run_id", "status")
  if (!all(required %in% names(reg))) stop("W08 registry contract mismatch", call. = FALSE)

  hit <- reg[tolower(trimws(as.character(reg$status))) == "authoritative", , drop = FALSE]
  if (nrow(hit) != 1L) stop("Expected exactly one authoritative W08 registry row", call. = FALSE)

  run_id <- trimws(as.character(hit$source_run_id[[1L]]))
  if (!grepl("^[0-9]+$", run_id)) stop("Invalid authoritative W08 source run ID", call. = FALSE)

  pointer_name <- paste0("run-", run_id, ".json")
  pointer_path <- file.path(pointer_base, pointer_name)
  if (file.exists(pointer_path)) {
    x <- jsonlite::fromJSON(pointer_path, simplifyVector = FALSE)
  } else {
    pointer_text <- read_github_text_file(pointer_path, ref = ref)
    x <- jsonlite::fromJSON(pointer_text, simplifyVector = FALSE)
  }
  pointer_run <- as.character(x$source_github_run_id %||% "")
  n <- suppressWarnings(as.integer(x$canonical_records %||% NA_integer_))
  if (!identical(pointer_run, run_id)) stop("Authoritative W08 pointer/run mismatch", call. = FALSE)
  if (is.na(n) || n < 1L) stop("Authoritative W08 canonical count is invalid", call. = FALSE)

  excluded_n <- suppressWarnings(as.integer(x$excluded_records %||% NA_integer_))
  source_n <- suppressWarnings(as.integer(x$source_canonical_population %||% NA_integer_))
  if (is.na(excluded_n) || is.na(source_n) || excluded_n < 0L ||
      source_n != n + excluded_n)
    stop("Authoritative W08 final included/excluded population mismatch", call. = FALSE)
  list(
    canonical_records = n,
    excluded_records = excluded_n,
    source_canonical_population = source_n,
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
  w04_queued_init_rv <- reactiveVal(NULL)
  w04_queue_loaded_rv <- reactiveVal(FALSE)
  w04_queue_load_status_rv <- reactiveVal("")
  w04_load_requested_rv <- reactiveVal(0L)
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
  w02_abstract_queue_rv <- reactiveVal(NULL)
  w02_abstract_edits_rv <- reactiveVal(data.frame())
  w02_abstract_idx_rv <- reactiveVal(1L)
  w02_abstract_status_rv <- reactiveVal("")
  w02_optional_pause_rv <- reactiveVal(NULL)
  w02_optional_resume_requested_rv <- reactiveVal(FALSE)
  w02_launch_requested_rv <- reactiveVal(FALSE)
  w02_launch_status_rv <- reactiveVal("")
  w02_launch_record_rv <- reactiveVal(NULL)
  w02_launch_run_id_rv <- reactiveVal("")
  w02_launch_snapshot_rv <- reactiveVal(NULL)
  w02_launch_error_rv <- reactiveVal("")

  w02_queue_sha_rv <- reactiveVal("")
  w02_batch_id_rv <- reactiveVal("")
  w02_idx <- reactiveVal(1L)
  w02_decisions <- reactiveVal(list())
  w02_batch_status_rv <- reactiveVal("")
  w02_status <- reactiveVal("")
  w02_resume_requested_rv <- reactiveVal(FALSE)

  w04_subset_status_rv <- reactiveVal("")
  w04_subset_corpus_rv <- reactiveVal(NULL)
  w05_crop_dictionary_rv <- reactiveVal(NULL)
  w05_crop_dictionary_path_rv <- reactiveVal("")
  w05_crop_status_rv <- reactiveVal("W05 is paused. Upload a validated CSV to archive its version in a restricted Zenodo draft.")
  w05_crop_frequencies_rv <- reactiveVal(NULL)
  w05_unspecified_evidence_rv <- reactiveVal(NULL)
  w05_dictionary_receipt_rv <- reactiveVal(NULL)
  w06_results_rv <- reactiveVal(NULL)
  w06_trial_status_rv <- reactiveVal("No W06 model subset has been run.")
  w06_prompt_preview_rv <- reactiveVal("")
  w06_prompt_receipt_rv <- reactiveVal(NULL)
  w06_prompt_approved_rv <- reactiveVal(FALSE)
  w06_prompt_status_rv <- reactiveVal("W06 paused: upload and approve a review-specific prompt.")
  w05_verified_input_rv <- reactiveVal(NULL)
  w05_dictionary_approved_sha_rv <- reactiveVal("")
  w05_shiny_subset_launch_status_rv <- reactiveVal("Waiting for verified W04 publication and a validated W05 input bundle.")
  w05_shiny_results_status_rv <- reactiveVal("No completed W05 Shiny subset results loaded.")
  w04_manual_queue_choices_rv <- reactiveVal(list())
  w04_manual_queue_status_rv <- reactiveVal("")
  w04_subsets_rv <- reactiveVal(list())
  w04_overview_rv <- reactiveVal(NULL)
  w04_overview_status_rv <- reactiveVal("Select Refresh overview to load saved W04 information.")
  w04_overview_refreshed_rv <- reactiveVal("")
  w04_consistency_registry_rv <- reactiveVal(list())
  w04_consistency_subset_snapshot_rv <- reactiveVal(NULL)
  w04_consistency_model_runs_rv <- reactiveVal(list())
  w04_consistency_model_status_rv <- reactiveVal("")
  w04_consistency_subset_status_rv <- reactiveVal("")
  w04_prompt_status_rv <- reactiveVal("")
  w04_prompt_versions_rv <- reactiveVal(list())
  w04_model_subsets_rv <- reactiveVal(list())
  w04_model_versions_rv <- reactiveVal(list())
  w04_finalise_selection_batches_rv <- reactiveVal(list())
  w04_model_status_rv <- reactiveVal("")
  w04_model_launch_busy_rv <- reactiveVal(FALSE)
  w04_model_batches_rv <- reactiveVal(list())
  w04_model_results_rv <- reactiveVal(NULL)
  w04_model_selected_batch_rv <- reactiveVal(NULL)
  w04_model_return_status_rv <- reactiveVal("")
  w04_refinement_request_rv <- reactiveVal(NULL)
  w04_refinement_response_rv <- reactiveVal(NULL)
  w04_refinement_status_rv <- reactiveVal("")
  w04_refinement_poll_active_rv <- reactiveVal(FALSE)
  w04_refinement_run_url_rv <- reactiveVal("")
  w04_refinement_versions_rv <- reactiveVal(list())
  w04_reviewer_model_batches_rv <- reactiveVal(list())
  w04_reviewer_model_status_rv <- reactiveVal("")
  w04_all_cases_rv <- reactiveVal(list())
  w04_cases_rv <- reactiveVal(NULL)
  w04_queue_sha_rv <- reactiveVal("")
  w04_batch_id_rv <- reactiveVal("")
  w04_idx <- reactiveVal(1L)
  w04_decisions <- reactiveVal(list())
  w04_screening_notes_rv <- reactiveVal(list())
  w04_manual_abstract_edits_rv <- reactiveVal(list())
  w04_manual_abstract_edit_rv <- reactiveVal(FALSE)
  w04_manual_abstract_status_rv <- reactiveVal("")
  w04_manual_abstract_status_case_rv <- reactiveVal("")
  w04_note_status <- reactiveVal("")
  w04_batch_status_rv <- reactiveVal("")
  w04_status <- reactiveVal("")
  w04_highlight_status_rv <- reactiveVal("")
  w04_highlight_version_rv <- reactiveVal(NULL)
  w04_include_terms <- reactiveVal(character())
  w04_exclude_terms <- reactiveVal(character())
  w04_consistency_analyses_rv <- reactiveVal(list())
  w04_conflict_sets_rv <- reactiveVal(list())
  w04_model_conflict_active_rv <- reactiveVal("")
  w04_model_conflict_cases_rv <- reactiveVal(list())
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
  w04_resolution_origin_rv <- reactiveVal("production")
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
  w04_admin_direct_conflict_set_rv <- reactiveVal("")

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
  review_published_metrics_rv <- reactiveVal(NULL)
  review_reporting_error_rv <- reactiveVal("")
  manual_screening_rv <- reactiveVal(NULL)
  manual_screening_kpi_error_rv <- reactiveVal("")
  authoritative_w08_rv <- reactiveVal(NULL)
  backend_reset_status <- reactiveVal("")

  search_scope_string_rv <- reactiveVal("")
  search_formal_draft_rv <- reactiveVal("")
  search_manual_preview_rv <- reactiveVal(NULL)
  search_manual_pending_rv <- reactiveVal(NULL)
  search_manual_status_rv <- reactiveVal("")
  search_manual_success_rv <- reactiveVal(NULL)
  search_manual_file_reset_rv <- reactiveVal(0L)
  search_manual_busy_rv <- reactiveVal(FALSE)
  search_manual_registry_refresh_rv <- reactiveVal(0L)
  search_history_export_error_rv <- reactiveVal("")
  search_formal_validation_rv <- reactiveVal(NULL)
  search_formal_expected_version_rv <- reactiveVal(NULL)
  search_formal_finalisation_message_rv <- reactiveVal("")
  search_formal_launched_rv <- reactiveVal(FALSE)
  search_formal_selected_sources_rv <- reactiveVal(character())
  search_formal_monitor_rv <- reactiveVal(NULL)
  search_formal_counts_rv <- reactiveVal(list())
  search_formal_seen_records_rv <- reactiveVal(character())
  search_formal_request_id_rv <- reactiveVal("")
  search_formal_run_id_rv <- reactiveVal("")
  search_formal_monitor_restored_rv <- reactiveVal(FALSE)
  search_formal_loaded_key_rv <- reactiveVal("")
  search_formal_draft_message_rv <- reactiveVal("")
  search_source_groups_rv <- reactiveVal(NULL)
  search_source_error_rv <- reactiveVal("")
  search_scope_status_rv <- reactiveVal("")
  search_scope_request_id_rv <- reactiveVal("")
  search_scope_run_id_rv <- reactiveVal("")
  search_scope_rows_rv <- reactiveVal(NULL)
  search_scope_seen_artifacts_rv <- reactiveVal(character())
  search_scope_job_sources_rv <- reactiveVal(character())
  search_scope_catalogue_rv <- reactiveVal(NULL)
  search_scope_progress_rv <- reactiveVal(list(completed=0L,total=0L,pct=0L,label=""))
  search_scope_versions_rv <- reactiveVal(character())
  search_scope_results_rv <- reactiveVal(list())
  search_scope_history_loaded_rv <- reactiveVal(FALSE)
  search_scope_active_version_rv <- reactiveVal("original")
  search_scope_running_version_rv <- reactiveVal("")

  # Pipeline status is loaded once during authentication. Task navigation
  # must not trigger repeated Google Sheets reads under the shared account.

  # Review-scoped publication receipts change only on workflow publication.
  # Poll less often than interactive review queues to avoid exhausting the
  # shared service-account read quota during idle task-page sessions.
  observeEvent(authenticated(), {
    req(authenticated(),identical(storage_backend(),"google_sheets"))
    # Publication receipts are loaded once per login, not polled while idle.
    metrics <- tryCatch({
      ss <- sheet_id_from_env()
      metadata <- read_review_metadata_cached(ss)
      review <- as.character(metadata$value[match("review_id",metadata$key)])
      if(length(review)!=1L||is.na(review)||!grepl("^[A-Za-z0-9._-]{3,128}$",review))
        stop("Missing valid review_id in the active review_metadata tab")
      base <- read_review_published_w01_w02(review,ss)
      # Only use an exact-review successful W04-only workflow and its
      # independently published pointer, bound to this review's W03 source.
      tryCatch({
        runs <- github_api_get_json(sprintf(
          "https://api.github.com/repos/%s/actions/runs?per_page=100",
          github_dispatch_repo()))$workflow_runs %||% list()
        run <- w05_shiny_discover_w04_publication(review,runs)
        if(is.null(run)) {
          base
        } else {
          pointer <- jsonlite::fromJSON(read_github_text_file(sprintf(
            "docs/workflow04/zenodo/run-%s.json",run$id)),simplifyVector=FALSE)
          source <- w04_subset_review_source_pointers()
          w05_shiny_w04_pipeline_metrics(base,pointer,review,run,
            source$lean$lean_canonical_sha256)
        }
      },error=function(e) {
        message("W04 overview publication overlay not verified: ",conditionMessage(e))
        base
      })
    },error=function(e)e)
    if(inherits(metrics,"error")) {
      review_reporting_error_rv(conditionMessage(metrics))
    } else {
      review_reporting_error_rv("")
      review_published_metrics_rv(metrics)
    }
  })

  # Restore saved manual-screening and model-comparison statistics at
  # login. No separate Compare coding tab visit is required for the KPI.
  observeEvent(authenticated(), {
    req(authenticated())
    summary <- tryCatch(read_manual_screening_metrics(),error=function(e)e)
    if(inherits(summary,"error")) {
      manual_screening_kpi_error_rv(conditionMessage(summary))
      message("LEM W04 manual screening KPI read failed: ",conditionMessage(summary))
    } else {
      manual_screening_kpi_error_rv("")
      manual_screening_rv(summary)
    }
  })

  # Restore the read-only published W04 model-run registry when the session
  # authenticates. The summary must not depend on a separate admin button.
  observeEvent(authenticated(), {
    req(authenticated(),identical(storage_backend(),"google_sheets"))
    result <- tryCatch({
      meta <- w04_prompt_review_metadata()
      w04_model_batch_registry_read(meta$review_id)
    },error=function(e)e)
    if(inherits(result,"error")) {
      w04_model_return_status_rv(paste("W04 screening KPI unavailable:",conditionMessage(result)))
    } else {
      w04_model_batches_rv(result)
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

  # Manual-screening metrics are loaded into local reactive state at login.
  # They are updated after local writes or explicit user refresh, not polled.

  load_w02_batch <- function() {
    if (!identical(storage_backend(), "google_sheets")) return(NULL)
    read_sheet_w02_queue()
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

  load_w04_batch <- function(user = current_user()) {
    if(!identical(storage_backend(),"google_sheets")) return(NULL)
    # Manual screening is sourced exclusively from registered subset batches.
    # Legacy model/validation and synthetic queues remain untouched on disk,
    # but are never automatically presented as manual-screening cases.
    assignments <- assignment_registry_rv() %||% list()
    if(is.null(user)) return(NULL)
    meta <- w04_prompt_review_metadata()
    registered <- w04_subset_registry_read(meta$review_id)
    if(!length(registered)) return(NULL)
    assigned <- Filter(function(s) {
      batch <- paste0("w04-human-",s$subset_id)
      length(assignments_for_user(assignments,"04",batch,
        as.character(normalise_user_row(user)$user_id),"manual_screening"))>0L
    },registered)
    # Multiple batches must be explicitly selected, not silently conflated.
    if(length(assigned)!=1L) return(NULL)
    w04_subset_queue_read(assigned[[1L]]$subset_id,meta$review_id)
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
    if (inherits(latest_decisions,"error")) {
      if (isTRUE(show_status)) w04_refresh_status(paste("Refresh failed:",conditionMessage(latest_decisions)))
      return(invisible(FALSE))
    }
    # Do not make a second Sheets request when the first has already failed.
    latest_assignments <- tryCatch(read_sheet_assignments(create_if_missing=FALSE),error=function(e)e)
    if (inherits(latest_assignments,"error")) {
      if (isTRUE(show_status)) w04_refresh_status(paste("Refresh failed:",conditionMessage(latest_assignments)))
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
    model_id <- as.character(w04_model_conflict_active_rv() %||% "")
    if(nzchar(model_id)) {
      matches <- Filter(function(x)
        identical(as.character(x$conflict_set_id %||% ""),model_id) &&
        identical(as.character(x$comparison_type %||% ""),"human_model"),xs)
      if(length(matches)==1L) return(matches[[1L]])
    }
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
    if(identical(as.character(z$comparison_type %||% ""),"human_model"))
      return(w04_model_conflict_cases_rv())
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
    # Direct access is restricted to an administrator explicitly opening this exact set.
    if(session_can("manage_assignments") &&
       identical(as.character(w04_admin_direct_conflict_set_rv() %||% ""),
                 as.character(w04_active_conflict_batch_id()))) return(cs)
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

  # The W01 queue may be published while users remain logged in. Load a
  # changed immutable batch without resetting active decisions on the same one.
  w01_queue_refresh_signal <- reactiveVal(0L)
  observeEvent(w01_queue_refresh_signal(), {
    req(authenticated(),current_user())
    if (!identical(storage_backend(),"google_sheets")) return()
    incoming <- tryCatch(read_sheet_w01_queue(),error=function(e)e)
    if(inherits(incoming,"error")) {
      if(!nzchar(batch_id_rv()))
        status(paste("Unable to refresh W01 review queue:",conditionMessage(incoming)))
      return()
    }
    if(is.null(incoming)) return()
    next_id <- as.character(incoming$batch_id %||% "")
    next_sha <- as.character(incoming$queue_sha256 %||% "")
    if(!nzchar(next_id) || !nzchar(next_sha)) return()
    if(identical(next_id,isolate(batch_id_rv())) &&
       identical(next_sha,isolate(queue_sha_rv()))) return()
    loaded <- tryCatch({
      latest_assignments <- read_assignment_registry(assignment_path)
      all_decisions <- read_active_decisions(decision_path)
      current_decisions <- filter_batch_decisions(all_decisions,next_sha)
      reviewer_cases <- cases_for_assignment_user(
        incoming$cases,latest_assignments,"01",next_id,current_user(),
        task_type="deduplication",active_events=current_decisions)
      repair_events <- read_active_w01_repairs(w01_repair_path,next_sha)
      export_requested <- w01_export_request_exists(next_sha,next_id)
      list(assignments=latest_assignments,decisions=current_decisions,
           cases=reviewer_cases,repairs=repair_events,
           export_requested=isTRUE(export_requested))
    },error=function(e)e)
    if(inherits(loaded,"error")) {
      status(paste("Unable to load newly published W01 queue:",conditionMessage(loaded)))
      return()
    }
    assignment_registry_rv(loaded$assignments)
    w01_all_cases_rv(incoming$cases)
    w01_repairs_rv(loaded$repairs)
    cases_rv(loaded$cases)
    decisions(loaded$decisions)
    queue_sha_rv(next_sha)
    batch_id_rv(next_id)
    batch_status_rv(incoming$batch_status %||% "")
    w01_export_requested_rv(loaded$export_requested)
    w01_export_status_rv(if(loaded$export_requested)
      "W01 export requested; checking GitHub handoff status." else "")
    ids <- if(length(loaded$cases)) vapply(loaded$cases,function(z)
      as.character(z$review_case_id),character(1)) else character()
    finished <- if(length(loaded$decisions)) unique(vapply(loaded$decisions,
      function(z)as.character(z$review_case_id),character(1))) else character()
    remaining <- which(!ids %in% finished)
    idx(if(length(remaining)) remaining[[1L]] else max(1L,length(ids)))
    complete(!length(remaining))
    status(sprintf("W01 review queue %s loaded automatically (%d cases).",
                   next_id,length(incoming$cases)))
  })

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
    if(length(z)!=1L || is.na(z)) return("—")
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
    if(length(z)!=1L || is.na(z)) return(NULL)
    paste0(if(z >= 0) "+" else "−", fmt_pipeline_n(abs(z)))
  }

  fmt_pipeline_pair_delta <- function(a,b) {
    x <- fmt_pipeline_delta(a)
    y <- fmt_pipeline_delta(b)
    if(is.null(x) || is.null(y)) return(NULL)
    paste0(x," / ",y)
  }

  read_manual_screening_metrics <- function() {
    if(!identical(storage_backend(),"google_sheets")) {
      registry <- read_github_w04_kappa_registry()
      w04_kappa_registry_rv(registry)
      return(w04_kappa_registry_summary(registry))
    }
    meta <- w04_prompt_review_metadata()
    source <- w04_subset_review_source_pointers()
    if(!identical(as.character(meta$review_id),as.character(source$review_id)))
      stop("W04 manual screening KPI review mismatch",call.=FALSE)
    subsets <- Filter(function(x)
      identical(tolower(as.character(x$source_checkpoint_sha256)),
        tolower(as.character(source$lean$lean_canonical_sha256))),
      w04_subset_registry_read(meta$review_id))
    events <- active_sheet_w04_decisions()
    manually_screened_ids <- character()
    for(subset in subsets) {
      queue <- w04_subset_queue_read(as.character(subset$subset_id),meta$review_id)
      if(is.null(queue)) next
      queue_ids <- vapply(queue$cases,function(x)as.character(x$record_id),character(1))
      case_ids <- vapply(queue$cases,function(x)
        as.character(x$review_case_id %||% ""),character(1))
      if(anyDuplicated(queue_ids) || anyDuplicated(case_ids))
        stop("W04 KPI queue contains duplicate canonical or case identities",call.=FALSE)
      decisions <- w04_filter_batch_decisions(events,queue$queue_sha256)
      valid <- Filter(function(x) {
        case_id <- as.character(x$review_case_id %||% x$case_id %||% "")
        n <- match(case_id,case_ids)
        !is.na(n) && identical(as.character(x$record_id %||% ""),queue_ids[[n]]) &&
          as.character(x$decision %||% "") %in% c("retain","exclude")
      },decisions)
      manually_screened_ids <- union(manually_screened_ids,
        vapply(valid,function(x)as.character(x$record_id),character(1)))
    }
    manual <- length(manually_screened_ids)
    latest <- w04_latest_subset_model_kappa(
      read_w04_consistency_analyses(),w04_model_batch_registry_read(meta$review_id))
    list(manually_screened=as.integer(manual),kappa=latest$kappa,
         kappa_n=latest$kappa_n,batches=latest$subsets)
  }

  pipeline_summary_ui <- function() {
    review_mode <- identical(storage_backend(),"google_sheets")
    if(review_mode) {
      counts <- search_formal_counts_rv()
      m <- search_formal_monitor_rv()
      verified <- !is.null(m) &&
        identical(as.character(m$conclusion %||% ""),"success") &&
        identical(as.character(m$status %||% ""),"completed")
      source_jobs <- if(is.null(m)) list() else Filter(function(j)
        grepl("^search-sources \\([^)]+\\) / search$",
          as.character(j$name %||% "")),m$jobs %||% list())
      sources <- vapply(source_jobs,function(j)
        sub("^search-sources \\(([^)]+)\\) / search$","\\1",
          as.character(j$name %||% "")),character(1))
      api_complete <- verified && length(sources)>0L &&
        all(vapply(sources,function(source)!is.null(counts[[source]]),logical(1)))
      manual <- search_manual_history_data()
      manual_values <- if(inherits(manual,"error")) NA_real_ else if(nrow(manual))
        suppressWarnings(as.numeric(manual$records_downloaded)) else numeric()
      manual_complete <- all(is.finite(manual_values))
      total <- if(api_complete && manual_complete) {
        sum(vapply(sources,function(source)as.numeric(counts[[source]]$downloaded),
          numeric(1)))+sum(manual_values)
      } else NA_real_
      p <- list(status_label=if(verified) "W00 completed" else "W00 pending",
        update_id=if(!is.null(m)) as.character(m$id %||% "") else "",
        last_search_date="",completed_through=if(verified) 1L else 0L,
        active_workflow="",search_results_total=total)
      published <- review_published_metrics_rv()
      if(!is.null(published)) {
        p <- utils::modifyList(p,published)
        if(!is.finite(total) && nzchar(as.character(published$search_results_ingested %||% ""))) {
          p$search_results_total <- published$search_results_ingested
          p$search_total_is_ingested <- TRUE
        }
        p$update_id <- if(nzchar(as.character(published$run_id %||% "")))
          as.character(published$run_id) else p$update_id
      }
    } else {
      p <- pipeline_status_rv()
    }
    if(review_mode && nzchar(review_reporting_error_rv())) {
      return(div(class="pipeline-summary",
        div(class="text-danger small",
          paste("Review-specific pipeline reporting unavailable:",
                review_reporting_error_rv()))))
    }
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
    w08_finalised <- !review_mode && w08_status_is_final(p)
    w08_authoritative <- authoritative_w08_rv()
    canonical_current <- w08_finalised &&
      !is.null(w08_authoritative) &&
      !is.na(suppressWarnings(as.integer(w08_authoritative$canonical_records %||% NA_integer_)))
    canonical_value <- if (isTRUE(canonical_current)) {
      w08_authoritative$canonical_records
    } else {
      p$canonical_existing
    }
    # W08 finalisation retrospectively reconciles eligibility KPIs. The raw
    # W04 and W05 processing counts stay in pipeline provenance; the current
    # summary shows final disposition, not provisional upstream retention.
    final_include <- if (isTRUE(canonical_current)) {
      suppressWarnings(as.integer(canonical_value))
    } else NA_integer_
    final_retracted <- suppressWarnings(as.integer(p$retracted_records %||% NA_integer_))
    final_total_excluded <- suppressWarnings(as.integer(w08_authoritative$excluded_records %||% NA_integer_))
    final_exclude <- if (!is.na(final_include) && !is.na(final_retracted) &&
                         !is.na(final_total_excluded) && final_total_excluded >= final_retracted &&
                         identical(as.integer(w08_authoritative$source_canonical_population),
                                   as.integer(final_include + final_total_excluded))) {
      final_total_excluded - final_retracted
    } else NA_integer_
    reconciled_eligibility <- isTRUE(canonical_current) && !is.na(final_exclude)

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
          tags$span(class=paste(c("pipeline-kpi-value",if(isTRUE(compact_value)) "compact" else NULL),collapse=" "),value)
        ),
        if(!is.null(inline_note)) tags$span(class="pipeline-kpi-inline-note",inline_note),
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
          "search results",
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
            if(nzchar(manual_screening_kpi_error_rv())) "Unavailable" else if(is.null(m)) "—" else fmt_pipeline_n(m$manually_screened),
            if(nzchar(manual_screening_kpi_error_rv())) "Saved κ could not be loaded" else if(is.null(m) || is.na(m$kappa)) "κ not recorded in registry" else paste0("κ ",sprintf("%.3f",m$kappa))
          )
        },
        kpi(
          "Screened",
          paste0(fmt_pipeline_n(if(reconciled_eligibility) final_include else p$screened_include),
                 " / ",fmt_pipeline_n(if(reconciled_eligibility) final_exclude else p$screened_exclude)),
          if(reconciled_eligibility) "final include / exclude after W08" else "include / exclude",
          stage_position=5L,
          inline_note=if(reconciled_eligibility) NULL else if(stage_complete(5L)) fmt_pipeline_pair_delta(p$screened_include_update,p$screened_exclude_update) else NULL,
          compact_value=TRUE
        ),
        kpi(
          "Deterministic",
          fmt_pipeline_n(if(reconciled_eligibility) final_include else p$species_records),
          if(reconciled_eligibility) "eligible records after W08" else "records processed",
          stage_position=6L,
          inline_note=if(reconciled_eligibility) NULL else if(stage_complete(6L)) fmt_pipeline_delta(p$species_update) else NULL
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
          if (isTRUE(canonical_current)) "current" else if(review_mode) NULL else "pre-update",
          stage_position=9L,
          class_extra=if (isTRUE(canonical_current) || review_mode) NULL else "pre-update"
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
    uid <- if (length(uid)) uid[[1L]] else ""
    if (is.na(uid) || !nzchar(uid)) return("")
    u <- tryCatch(
      find_user_by_id(user_registry_rv(),uid,require_active=FALSE),
      error=function(e) NULL
    )
    if (is.null(u)) return(uid)
    label <- as.character(u$display_name %||% uid)
    label <- if (length(label)) label[[1L]] else uid
    if (is.na(label) || !nzchar(label)) uid else label
  }

  record_table_decision_label <- function(x) {
    d <- as.character(x$decision %||% "")
    d <- if (length(d)) d[[1L]] else ""
    if (is.na(d)) d <- ""
    d <- tolower(trimws(d))
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
    issue_json <- as.character(x$issue_decisions_json %||% "")
    issue_json <- if (length(issue_json)) issue_json[[1L]] else ""
    if (!is.na(issue_json) && nzchar(trimws(issue_json))) return("Annotation saved")
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
    if(!is.null(w08b_review_queue_rv())) add("01","citation_deduplication",
      w08b_review_queue_rv()$batch_id,w08b_review_queue_rv()$cases,w08b_review_decisions_rv())
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

  record_table_completed_case_ids <- function(src) {
    cases <- src$cases %||% list()
    case_ids <- unique(vapply(cases,record_table_case_id,character(1)))
    case_ids <- case_ids[nzchar(case_ids)]
    if (!length(case_ids)) return(character())

    events <- Filter(decision_resolves_case,src$events %||% list())
    mode <- assignment_mode_for(src$workflow,src$task_type)

    if (!identical(mode,ASSIGNMENT_MODES[["independent_blind_review"]])) {
      resolved <- unique(vapply(events,decision_case_id,character(1)))
      return(case_ids[case_ids %in% resolved])
    }

    formal <- active_assignments(src$assignments %||% list())
    complete <- vapply(case_ids,function(cid) {
      ca <- Filter(function(a) identical(normalise_assignment_row(a)$case_id,cid),formal)
      ce <- Filter(function(e) identical(decision_case_id(e),cid),events)
      if (!length(ca)) return(length(ce) > 0L)
      required_users <- unique(vapply(ca,function(a)normalise_assignment_row(a)$user_id,character(1)))
      completed_users <- unique(vapply(ce,decision_user_id,character(1)))
      length(required_users) > 0L && all(required_users %in% completed_users)
    },logical(1))
    case_ids[complete]
  }

  record_table_outstanding_case_ids <- function(src) {
    all_ids <- unique(vapply(src$cases %||% list(),record_table_case_id,character(1)))
    all_ids <- all_ids[nzchar(all_ids)]
    setdiff(all_ids,record_table_completed_case_ids(src))
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
      resolved_cases=record_table_completed_case_ids(src),
      outstanding_cases=record_table_outstanding_case_ids(src),
      unassigned=setdiff(
        setdiff(all_ids,record_table_completed_case_ids(src)),
        unique(vapply(eff,function(a)a$case_id,character(1)))
      ),
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
        c(complete="Completed",assigned="Outstanding",resolved_elsewhere="Resolved elsewhere")[[statuses[[1L]]]] %||% statuses[[1L]]
      } else {
        paste(
          sum(vapply(ca_view,function(a)identical(a$effective_status,"complete"),logical(1))),"completed ·",
          sum(vapply(ca_view,function(a)identical(a$effective_status,"assigned"),logical(1))),"outstanding ·",
          sum(vapply(ca_view,function(a)identical(a$effective_status,"resolved_elsewhere"),logical(1))),"resolved elsewhere"
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
            metric %in% c("cases","assignments","unassigned","resolved_cases","outstanding_cases") ||
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

  record_table_batch_dispatched <- function(workflow,task_type,batch_id) {
    workflow <- as.character(workflow %||% "")
    task_type <- as.character(task_type %||% "")
    batch_id <- as.character(batch_id %||% "")

    if (identical(workflow,"01") && identical(task_type,"deduplication") && identical(batch_id,batch_id_rv())) {
      return(isTRUE(w01_export_requested_rv()))
    }
    if (identical(workflow,"02") && identical(task_type,"enrichment") && identical(batch_id,w02_batch_id_rv())) {
      return(isTRUE(w02_resume_requested_rv()))
    }
    if (identical(workflow,"04") && identical(task_type,"manual_screening") && identical(batch_id,w04_batch_id_rv())) {
      return(isTRUE(w04_validation_finalize_requested_rv()))
    }
    if (identical(workflow,"04") && identical(task_type,"model_uncertainty") && identical(batch_id,w04_resolution_batch_id_rv())) {
      return(isTRUE(w04_resolution_resume_requested_rv()))
    }
    if (identical(workflow,"08") && identical(task_type,"annotation") && identical(batch_id,w08_batch_id_rv())) {
      return(isTRUE(w08_resume_requested_rv()))
    }
    FALSE
  }

  record_table_preview <- function(text,n=5L) {
    x <- strsplit(normalise_display_text(text),"[[:space:]]+",perl=TRUE)[[1L]]
    x <- x[nzchar(x)]
    if(!length(x)) return("")
    paste(head(x,n),collapse=" ")
  }


  # Dedicated W08b citation-chasing review. Independent of production W01.
  w08b_review_queue_rv <- reactiveVal(NULL)
  w08b_review_decisions_rv <- reactiveVal(list())
  w08b_review_visible_rv <- reactiveVal(FALSE)
  w08b_review_idx <- reactiveVal(1L)
  w08b_pair_idx <- reactiveVal(1L)
  w08b_review_status_rv <- reactiveVal("")
  w08b_export_requested_rv <- reactiveVal(FALSE)
  w08b_export_status_rv <- reactiveVal("")
  w08b_abstract_repairs_rv <- reactiveVal(list())
  w08b_abstract_edit_rv <- reactiveVal(NULL)
  refresh_w08b_review <- function() {
    q <- read_w08b_review_queue()
    w08b_review_queue_rv(q)
    w08b_review_decisions_rv(if(is.null(q))list() else read_w08b_review_decisions(q$sha))
    w08b_abstract_repairs_rv(if(is.null(q))list() else read_w08b_abstract_repairs(q$sha))
    invisible(q)
  }
  observeEvent(authenticated(), {
    if(isTRUE(authenticated()))tryCatch(refresh_w08b_review(),
      error=function(e)w08b_review_status_rv(conditionMessage(e)))
  },ignoreInit=FALSE)
  observeEvent(input$w08b_open_review, {
    req(authenticated())
    tryCatch({
      refresh_w08b_review()
      w08b_review_visible_rv(TRUE)
      assigned <- w08b_review_assigned()
      reviewed <- vapply(w08b_review_decisions_rv(),function(z)as.character(z$review_case_id),character(1))
      remaining <- which(!vapply(assigned,function(z)as.character(z$review_case_id) %in% reviewed,logical(1)))
      w08b_review_idx(if(length(remaining))remaining[[1L]] else 1L)
      w08b_pair_idx(1L)
      app_view("w08b")
    },error=function(e)showNotification(conditionMessage(e),type="error"))
  })
  w08b_pending_cases <- reactive({
    q<-w08b_review_queue_rv()
    if(is.null(q))return(list())
    done<-vapply(w08b_review_decisions_rv(),function(z)as.character(z$review_case_id),character(1))
    Filter(function(z)!as.character(z$review_case_id)%in%done,q$cases)
  })
  w08b_review_assigned <- reactive({
    q <- w08b_review_queue_rv()
    if(is.null(q))return(list())
    uid <- as.character(current_user()$user_id %||% "")
    assignments <- active_assignments_for_batch(
      assignment_registry_rv(),"01",q$batch_id,"citation_deduplication")
    ids <- vapply(Filter(function(z)
      identical(normalise_assignment_row(z)$user_id,uid),assignments),
      function(z)normalise_assignment_row(z)$case_id,character(1))
    if(!length(assignments) && session_can("manage_assignments"))return(q$cases)
    Filter(function(z)as.character(z$review_case_id) %in% ids,q$cases)
  })
  w08b_case_saved <- reactive({
    z <- w08b_current_case()
    any(vapply(w08b_review_decisions_rv(),function(event)
      identical(as.character(event$review_case_id),as.character(z$review_case_id)),
      logical(1)))
  })
  w08b_current_case <- reactive({
    p <- w08b_review_assigned()
    req(length(p)>0L)
    i <- min(max(1L,as.integer(w08b_review_idx())),length(p))
    p[[i]]
  })
  output$w08b_page_label <- renderText({
    q <- w08b_review_queue_rv()
    paste("Workflow 01 · duplicate review · citation chasing ·",q$batch_id %||% "")
  })
  output$w08b_page_progress <- renderText({
    assigned <- w08b_review_assigned()
    n <- length(assigned)
    i <- if(n) max(1L,min(as.integer(w08b_review_idx()),n)) else 0L
    remaining <- sum(!vapply(assigned,function(z)as.character(z$review_case_id) %in%
      vapply(w08b_review_decisions_rv(),function(d)as.character(d$review_case_id),character(1)),logical(1)))
    sprintf("Case %d of %d · %d assigned unreviewed",i,n,remaining)
  })
  output$w08b_page_progress_bar <- renderUI({
    q <- w08b_review_queue_rv();req(!is.null(q))
    total <- length(q$cases)
    done <- total-length(w08b_pending_cases())
    div(class="progress mb-3",div(class="progress-bar",role="progressbar",
      style=sprintf("width:%.1f%%",if(total>0)100*done/total else 100),
      sprintf("%d of %d resolved",done,total)))
  })
  w08b_review_pairs <- reactive({
    z <- w08b_current_case()
    candidates <- z$candidates %||% list()
    historical <- z$historical %||% list()
    if(!length(candidates)||!length(historical))return(list())
    pairs <- list()
    for(i in seq_along(candidates))for(j in seq_along(historical))
      pairs[[length(pairs)+1L]] <- list(candidate=candidates[[i]],historical=historical[[j]])
    pairs
  })
  w08b_pair_record <- function(side) {
    pairs <- w08b_review_pairs()
    req(length(pairs)>0L)
    j <- max(1L,min(as.integer(w08b_pair_idx()),length(pairs)))
    if(identical(side,"a")) list(kind="candidate",record=pairs[[j]]$candidate)
    else list(kind="historical",record=pairs[[j]]$historical)
  }
  w08b_repair_for <- function(kind,record) {
    id <- if(identical(kind,"candidate")) as.character(record$source_id %||% "")
          else as.character(record$w01_record_id %||% "")
    rows <- Filter(function(x) identical(as.character(x$source_kind),kind) &&
      identical(as.character(x$source_record_id),id),w08b_abstract_repairs_rv())
    if(length(rows)) tail(rows,1)[[1]] else NULL
  }
  w08b_save_abstract <- function(side, action=NULL) {
    req(authenticated())
    if(!session_can("adjudicate_assigned")) {
      showNotification("You do not have permission to edit abstracts.",type="error")
      return(invisible(FALSE))
    }
    z <- w08b_current_case()
    pair <- w08b_pair_record(side)
    input_id <- paste0("w08b_abstract_",side)
    value <- if(is.null(action)) trimws(as.character(input[[input_id]] %||% ""))
             else ""
    operation <- if(!is.null(action)) action else if(nzchar(value))"replace_abstract" else "strip_abstract"
    tryCatch({
      saved_repair <- append_w08b_abstract_repair(w08b_review_queue_rv(),z,pair$kind,pair$record,
        operation,value,session_reviewer_id())
      w08b_abstract_repairs_rv(c(w08b_abstract_repairs_rv(),list(saved_repair)))
      w08b_abstract_edit_rv(NULL)
      w08b_review_status_rv(paste("Record",toupper(side),": abstract",if(operation=="strip_abstract")"deleted" else "saved","with audit trail."))
      showNotification(paste("Record",toupper(side),if(operation=="strip_abstract")"abstract deleted" else "abstract saved"),type="message",duration=5)
      invisible(TRUE)
    },error=function(e) {
      showNotification(paste("Abstract save failed:",conditionMessage(e)),type="error")
      invisible(FALSE)
    })
  }
  for(side in c("a","b")) local({
    which_side <- side
    observeEvent(input[[paste0("edit_w08b_abstract_",which_side)]],{
      w08b_abstract_edit_rv(list(case=as.character(w08b_current_case()$review_case_id),
        pair=as.integer(w08b_pair_idx()),side=which_side))
    })
    observeEvent(input[[paste0("cancel_w08b_abstract_",which_side)]],w08b_abstract_edit_rv(NULL))
    observeEvent(input[[paste0("save_w08b_abstract_",which_side)]],w08b_save_abstract(which_side))
    observeEvent(input[[paste0("delete_w08b_abstract_",which_side)]],{
      identity <- list(case=as.character(w08b_current_case()$review_case_id),
        pair=as.integer(w08b_pair_idx()),side=which_side)
      showModal(modalDialog(title=paste("Delete abstract from Record",toupper(which_side),"?"),
        "The original record remains unchanged. An audited removal will be saved.",
        footer=tagList(modalButton("Cancel"),
          actionButton("confirm_w08b_abstract_delete","Delete abstract",class="btn-danger")),
        easyClose=TRUE))
      w08b_abstract_delete_rv(identity)
    })
  })
  w08b_abstract_delete_rv <- reactiveVal(NULL)
  observeEvent(input$confirm_w08b_abstract_delete,{
    pending <- w08b_abstract_delete_rv()
    req(pending)
    if(identical(pending$case,as.character(w08b_current_case()$review_case_id)) &&
       identical(pending$pair,as.integer(w08b_pair_idx())))
      w08b_save_abstract(pending$side,"strip_abstract")
    else showNotification("Delete cancelled: comparison changed.",type="warning")
    w08b_abstract_delete_rv(NULL)
    removeModal()
  })
  output$w08b_page_case <- renderUI({
    if(!length(w08b_review_assigned()))return(card(tags$p("No assigned citation-deduplication cases remain.")))
    z <- w08b_current_case()
    pairs <- w08b_review_pairs()
    req(length(pairs)>0L)
    pair_idx <- min(max(1L,as.integer(w08b_pair_idx())),length(pairs))
    a <- pairs[[pair_idx]]$candidate
    b <- pairs[[pair_idx]]$historical
    authors <- function(rec){
      x <- rec$authors %||% list()
      if(!length(x))return("")
      paste(vapply(x,function(v){
        if(is.list(v))as.character(v$display_name %||% v$name %||% "") else as.character(v)
      },character(1)),collapse="; ")
    }
    rec <- function(x,src)list(source=src,
      source_record_id=as.character(x$source_id %||% x$w01_record_id %||% ""),
      title=as.character(x$title %||% ""),abstract=as.character(x$abstract %||% ""),
      authors=authors(x),year=as.character(x$year %||% ""),
      journal=as.character(x$journal %||% ""),doi=as.character(x$doi %||% ""))
    left<-rec(a,as.character(a$provider %||% "Citation chasing"))
    right<-rec(b,"Historical W01")
    for (kind in c("candidate","historical")) {
      original <- if(kind=="candidate") a else b
      correction <- w08b_repair_for(kind,original)
      replacement <- if(is.null(correction)) as.character(original$abstract %||% "")
        else if(identical(as.character(correction$action),"strip_abstract")) ""
        else as.character(correction$value %||% "")
      if(kind=="candidate") {
        left$abstract_deleted <- !is.null(correction) && identical(as.character(correction$action),"strip_abstract")
        left$display_abstract <- replacement
        left$abstract_repair_saved <- !is.null(correction)
      } else {
        right$abstract_deleted <- !is.null(correction) && identical(as.character(correction$action),"strip_abstract")
        right$display_abstract <- replacement
        right$abstract_repair_saved <- !is.null(correction)
      }
    }
    edit_state <- w08b_abstract_edit_rv()
    editing <- function(side) !is.null(edit_state) &&
      identical(as.character(edit_state$case),as.character(z$review_case_id)) &&
      identical(as.integer(edit_state$pair),pair_idx) &&
      identical(as.character(edit_state$side),side)
    comparison_evidence <- lapply(z$match_methods %||% character(),
      function(method)list(label="Match",value=method))
    tagList(
      tags$p(class="text-secondary",paste("Case",z$review_case_id,
        "· Pair",pair_idx,"of",length(pairs),
        "· Evidence:",paste(z$match_methods %||% character(),collapse=", "))),
      dedup_pair_cards(left,right,comparison_evidence,allow_abstract_edit=TRUE,
        editing_a=editing("a"),editing_b=editing("b"),abstract_prefix="w08b"),
      tags$p(class="text-secondary small","Canonical identity is selected deterministically, not by reviewers."),
      textAreaInput("w08b_rationale","Adjudication rationale",rows=2),
      tags$p(class="text-secondary small",w08b_review_status_rv()))
  })
  output$w08b_decision_controls <- renderUI({
    z <- w08b_current_case()
    events <- Filter(function(d)identical(as.character(d$review_case_id),as.character(z$review_case_id)),w08b_review_decisions_rv())
    choice <- if(length(events))as.character(tail(events,1)[[1]]$decision) else ""
    saved <- length(events)>0L
    button <- function(id,label,value,style){
      actionButton(id,label,class=paste(style,if(identical(choice,value))"decision-selected" else ""),
        `aria-pressed`=if(identical(choice,value))"true" else "false",
        disabled=if(saved)"disabled" else NULL)
    }
    div(class="decision-row d-flex flex-wrap gap-2",
      button("w08b_duplicate","Same record","same_publication","btn-success"),
      button("w08b_distinct","Different records","different_publications","btn-primary"),
      button("w08b_uncertain","Uncertain","unable_to_determine","btn-outline-secondary"),
      actionButton("w08b_previous","← Previous"),actionButton("w08b_next","Next →"),
      if(saved)tags$span(class="saved-note align-self-center","Saved decision · read-only"))
  })
  w08b_navigate <- function(delta){
    cases<-w08b_review_assigned()
    if(!length(cases))return(invisible(FALSE))
    i<-min(max(1L,as.integer(w08b_review_idx())),length(cases))
    pairs<-w08b_review_pairs()
    j<-min(max(1L,as.integer(w08b_pair_idx())),length(pairs))
    if(delta<0L){
      if(j>1L)dedup_navigate(-1L,j,w08b_pair_idx,length(pairs))
      else if(i>1L){
        previous<-cases[[i-1L]]
        w08b_review_idx(i-1L)
        w08b_pair_idx(max(1L,length(previous$candidates %||% list())*length(previous$historical %||% list())))
      }
    }else{
      if(j<length(pairs))dedup_navigate(1L,j,w08b_pair_idx,length(pairs))
      else if(i<length(cases)){
        w08b_review_idx(i+1L)
        w08b_pair_idx(1L)
      }
    }
    invisible(TRUE)
  }
  observeEvent(input$w08b_previous,w08b_navigate(-1L))
  observeEvent(input$w08b_next,w08b_navigate(1L))
  w08b_save_page_decision <- function(value){
    req(authenticated())
    tryCatch({
      q<-w08b_review_queue_rv();z<-w08b_current_case()
      if(isTRUE(w08b_case_saved()))stop("Already adjudicated: use Previous/Next to inspect this case; decisions are read-only.")
      saved_event <- append_w08b_review_decision(q,z,value,
        as.character(input$w08b_target %||% ""),
        as.character(input$w08b_rationale %||% ""),
        as.character(current_user()$user_id %||% ""))
      old_index <- as.integer(w08b_review_idx())
      # The queue and repair registry are immutable during a decision save.
      # Keep the accepted event locally rather than re-fetching three Sheets.
      w08b_review_decisions_rv(c(w08b_review_decisions_rv(),list(saved_event)))
      assigned <- w08b_review_assigned()
      reviewed <- vapply(w08b_review_decisions_rv(),function(d)as.character(d$review_case_id),character(1))
      remaining <- which(!vapply(assigned,function(c)as.character(c$review_case_id) %in% reviewed,logical(1)))
      following <- remaining[remaining > old_index]
      destination <- if(length(following))following[[1L]] else if(length(remaining))remaining[[1L]] else old_index
      w08b_review_idx(max(1L,min(destination,length(assigned))))
      w08b_pair_idx(1L)
      w08b_review_status_rv("Decision saved.")
    },error=function(e)showNotification(conditionMessage(e),type="error"))
  }
  observeEvent(input$w08b_send_github,{
    req(authenticated())
    if(!session_can("control_workflows")) {
      showNotification("Administrator access required.",type="error")
      return()
    }
    q <- w08b_review_queue_rv()
    if(is.null(q) || !length(q$cases) || length(w08b_pending_cases())>0L) {
      showNotification("Citation-chasing review is not complete.",type="error")
      return()
    }
    tryCatch({
      token <- github_dispatch_token()
      # A workflow_dispatch workflow must exist on GitHub's default branch.
      # W08b is deliberately restricted to workflow01-final-architecture, so
      # use its existing push-trigger marker convention instead.
      branch <- "workflow01-final-architecture"
      url <- "https://api.github.com/repos/thesalmonandthetomato/LivingEvidenceMap/contents/docs/workflow08b/.run_export_decisions"
      headers <- c(Authorization=paste("Bearer",token),Accept="application/vnd.github+json",
                   `X-GitHub-Api-Version`="2022-11-28")
      get <- httr2::request(paste0(url,"?ref=",branch)) |>
        httr2::req_headers(Authorization=paste("Bearer",token),Accept="application/vnd.github+json",
          `X-GitHub-Api-Version`="2022-11-28") |>
        httr2::req_error(is_error=function(resp) httr2::resp_status(resp)!=404L && httr2::resp_status(resp)>=400L) |>
        httr2::req_perform()
      existing_sha <- if(httr2::resp_status(get)==200L)
        as.character(httr2::resp_body_json(get)$sha) else NULL
      marker <- jsonlite::toJSON(list(batch_id=as.character(q$batch_id),
        queue_sha256=as.character(q$sha),
        requested_at=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"),
        reviewer=session_reviewer_id()),auto_unbox=TRUE)
      payload <- list(message=paste("Request W08b completed decision export",q$batch_id),
        branch=branch,content=jsonlite::base64_enc(charToRaw(enc2utf8(marker))))
      if(!is.null(existing_sha))payload$sha <- existing_sha
      response <- httr2::request(url) |>
        httr2::req_method("PUT") |>
        httr2::req_headers(Authorization=paste("Bearer",token),Accept="application/vnd.github+json",
          `X-GitHub-Api-Version`="2022-11-28") |>
        httr2::req_body_json(payload) |>
        httr2::req_perform()
      if(!httr2::resp_status(response)%in%c(200L,201L))
        stop(paste("GitHub marker write HTTP",httr2::resp_status(response)))
      w08b_export_requested_rv(TRUE)
      w08b_export_status_rv("GitHub export requested. Check the W08b export workflow result.")
      showNotification("W08b export requested on GitHub.",type="message")
    },error=function(e)showNotification(paste("W08b export failed:",conditionMessage(e)),type="error"))
  })
  observeEvent(input$w08b_duplicate,w08b_save_page_decision("same_publication"))
  observeEvent(input$w08b_distinct,w08b_save_page_decision("different_publications"))
  observeEvent(input$w08b_uncertain,w08b_save_page_decision("unable_to_determine"))
  observeEvent(input$back_to_tasks_w08b,app_view("tasks"))

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

    # Authentication is independent of the slower workflow registry reads.
    # Do not show empty queues as if they were confirmed empty during loading.
    if (isTRUE(login_initialising())) {
      return(div(class="app-shell",
        div(class="d-flex justify-content-between align-items-center mb-3",
          tags$h2("LivingEvidenceMap adjudication"),
          div(uiOutput("session_identity"),
            actionButton("logout","Log out",class="btn-outline-secondary btn-sm"))),
        card(class="assignment-summary",
          tags$p(class="mb-2",tags$strong("Signed in. Loading review data…")),
          tags$p(class="saved-note mb-0",
            "Retrieving assignments and workflow queues. No decisions are being changed by this loading screen."),
          tags$div(class="spinner-border spinner-border-sm mt-3",role="status",
            tags$span(class="visually-hidden","Loading")))))
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
      # Completed historical W01 cases remain archived but are not active tasks.
      if (w01_remaining == 0L && length(w08b_pending_cases()) > 0L) {
        w01_total <- 0L
        w01_completed <- 0L
      }

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

      # Initial W04 buttons are gated by this user's active assignments,
      # using the assignment registry already read for ordinary dashboard login.
      # Unknown assignments never imply a task is available.
      w04_user_has_assignment <- function(task_type) {
        uid <- as.character(current_user()$user_id %||% "")
        if(!nzchar(uid))return(FALSE)
        assignments <- assignment_registry_rv() %||% list()
        any(vapply(assignments,function(x) {
          z <- normalise_assignment_row(x)
          identical(z$workflow,"04") &&
            identical(z$task_type,task_type) &&
            identical(z$user_id,uid) &&
            identical(z$status,"assigned")
        },logical(1)))
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
            if (identical(title,"Deduplication") && length(w08b_pending_cases())>0L)
              tags$div(class="text-secondary small mb-2",
                paste(length(w08b_pending_cases()),"citation-chasing ambiguities awaiting review")),
            if (nzchar(batch)) tags$div(class = "text-secondary small mb-1", batch),
            if (identical(lifecycle_status,"review_complete")) {
              tags$div(class="small mb-2",tags$span(class="task-badge","Human review complete"))
            } else if (identical(lifecycle_status,"consumed")) {
              tags$div(
                class="small mb-2",
                tags$span(class="task-badge workflow-complete-badge","Workflow completed")
              )
            },
            if (!is.null(button_id) && isTRUE(can_open) && (isTRUE(remaining > 0L) || button_id %in% c("w04_open_manual_on_demand","w04_open_conflicts_on_demand","w04_open_uncertainties_on_demand"))) {
              if (identical(button_id,"open_w04")) {
                actionButton(button_id,button_label,class="btn-primary mt-auto",
                  onclick="this.textContent='Opening manual screening…'; this.setAttribute('aria-busy','true');")
              } else if(identical(button_id,"open_w04_resolution")) {
                actionButton(button_id,button_label,class="btn-primary mt-auto",
                  onclick="this.textContent='Opening model uncertainties…'; this.setAttribute('aria-busy','true'); this.disabled=true;")
              } else {
                actionButton(button_id, button_label, class = "btn-primary mt-auto")
              }
            } else {
              if (!(identical(title,"Deduplication") && length(w08b_pending_cases())>0L))
                tags$div(class = "text-secondary small mt-auto", idle_text)
            },
            if (identical(title,"Deduplication") && length(w08b_pending_cases())>0L)
              actionButton("w08b_open_review","Review citation duplicates",class="btn-primary mt-2")
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
        if(session_can("manage_assignments"))
          card(class="assignment-summary",
            tags$details(class="assignment-disclosure",
              `data-accordion-key`="configure-admin-root",
              ontoggle="if(this.open && window.Shiny) Shiny.setInputValue('open_configure_review', Date.now(), {priority:'event'});",
              tags$summary(tags$strong("Configure and administer review")),
              div(class="p-3",
                uiOutput("configure_review"),
                tags$div(class="mt-3",
                  textOutput("administration_load_status"))))),
        div(
          class = "row g-3",
          div(
            class = "col-12 col-lg-6",
            stage_card(
              "Deduplication",
              "Workflow 01",
              "Potential duplicate bibliographic records, including citation-chasing ambiguities.",
              w01_total + if(is.null(w08b_review_queue_rv())) 0L else length(w08b_review_queue_rv()$cases),
              w01_completed + length(w08b_review_decisions_rv()),
              w01_remaining + length(w08b_pending_cases()),
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
            ),

          ),
          if(!is.null(w02_optional_pause_rv())) uiOutput("w02_missing_abstract_editor"),
          div(
            class = "col-12 col-lg-6",
            stage_card(
              "Manual screening",
              "Workflow 04",
              "Blind manual title and abstract screening for validation and ongoing human contribution.",
              if(isTRUE(w04_queue_loaded_rv())) w04_total else "—",
              if(isTRUE(w04_queue_loaded_rv())) w04_completed else "—",
              if(isTRUE(w04_queue_loaded_rv())) w04_remaining else "—",
              if(!isTRUE(w04_queue_loaded_rv()) && w04_user_has_assignment("manual_screening")) "w04_open_manual_on_demand" else if(w04_remaining > 0L) "open_w04" else NULL,
              if(!isTRUE(w04_queue_loaded_rv())) "Open manual screening" else "Continue manual screening",
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
              if(isTRUE(w04_queue_loaded_rv())) w04_conflict_total else "—",
              if(isTRUE(w04_queue_loaded_rv())) w04_conflict_completed else "—",
              if(isTRUE(w04_queue_loaded_rv())) w04_conflict_remaining else "—",
              if(!isTRUE(w04_queue_loaded_rv()) && w04_user_has_assignment("conflict_resolution")) "w04_open_conflicts_on_demand" else if(w04_conflict_remaining > 0L) "open_w04_conflict" else NULL,
              "Resolve reviewer conflicts",
              w04_active_conflict_batch_id(),
              w04_active_conflict_batch_status(),
              can_open = !isTRUE(w04_queue_loaded_rv()) || w04_conflict_user_remaining > 0L,
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
              if(isTRUE(w04_queue_loaded_rv())) w04_resolution_total else "—",
              if(isTRUE(w04_queue_loaded_rv())) w04_resolution_completed else "—",
              if(isTRUE(w04_queue_loaded_rv())) w04_resolution_remaining else "—",
              if(!isTRUE(w04_queue_loaded_rv()) && w04_user_has_assignment("model_uncertainty")) "w04_open_uncertainties_on_demand" else if(w04_resolution_remaining > 0L) "open_w04_resolution" else NULL,
              "Resolve model uncertainty",
              w04_resolution_batch_id_rv(),
              w04_resolution_batch_status_rv(),
              can_open = !isTRUE(w04_queue_loaded_rv()) || w04_resolution_user_remaining > 0L,
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

    if (identical(app_view(),"w08b")) {
      return(div(class="app-shell",
        div(class="d-flex justify-content-between align-items-center mb-3",
          div(tags$h2("LivingEvidenceMap adjudication",class="mb-0"),
            tags$div(textOutput("w08b_page_label"),class="text-secondary")),
          div(class="d-flex align-items-center gap-3",
            uiOutput("session_identity"),
            actionButton("back_to_tasks_w08b","Back to tasks",class="btn-outline-secondary btn-sm"),
            textOutput("w08b_page_progress"))),
        uiOutput("w08b_page_progress_bar"),
        card(class="decision-panel",
          div(class="d-flex flex-wrap justify-content-between align-items-center gap-2",
            tags$div(class="saved-note","Citation-chasing duplicate review"),
            uiOutput("w08b_decision_controls"))),
        uiOutput("w08b_page_case")))
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
        uiOutput("w02_case_view"),
        if(session_can("control_workflows")) {
          div(class="mt-2",
            actionButton("w02_prepare_abstract_queue","Prepare optional missing-abstract queue",
              class="btn-outline-secondary btn-sm"),
            tags$span(class="text-secondary small ms-2",
              "Only if you wish to review abstracts left empty after enrichment."))
        },
        uiOutput("w02_missing_abstract_editor")
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
        div(class="d-flex align-items-center gap-2 flex-wrap mb-2",
          actionButton("w04_manual_find_queues","Find assigned subsets",class="btn-outline-secondary btn-sm",
            onclick="window.lemBusyStart('w04_manual_find_queues','Checking assigned subset queues…')"),
          uiOutput("w04_manual_queue_selector"),
          actionButton("w04_manual_load_queue","Load selected subset",class="btn-outline-secondary btn-sm",
            onclick="window.lemBusyStart('w04_manual_load_queue','Loading assigned subset decisions…')")),
        tags$div(id="w04_manual_find_queues_busy",class="saved-note",
          style="display:none;",role="status","Checking assigned subset queues…"),
        tags$div(id="w04_manual_load_queue_busy",class="saved-note",
          style="display:none;",role="status","Loading assigned subset decisions…"),
        textOutput("w04_manual_queue_status"),
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
        if(identical(as.character((w04_active_consistency_conflict_set() %||% list())$comparison_type %||% ""),"human_model"))
          div(class="mb-3",
            actionButton("w04_load_conflict_vote_reasons","Load archived model explanations",class="btn-outline-secondary btn-sm"),
            textOutput("w04_load_conflict_vote_reasons_status")),
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
  login_initialising <- reactiveVal(FALSE)
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
    allowed_metrics <- c("cases","assignments","completed","closed","outstanding","resolved_cases","outstanding_cases","unassigned")
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

  observeEvent(input$record_table_edit,{
    req(authenticated())
    if (!session_can("manage_assignments")) {
      showNotification("Administrator permission is required to edit completed records.",type="error")
      return()
    }
    z <- input$record_table_edit
    if (is.null(z)) return()
    workflow <- as.character(z$workflow %||% "")
    task_type <- as.character(z$task_type %||% "")
    batch_id <- as.character(z$batch_id %||% "")
    case_id <- as.character(z$case_id %||% "")
    if (!nzchar(case_id)) return()
    if (record_table_batch_dispatched(workflow,task_type,batch_id)) {
      showNotification(
        "This batch has already been sent to GitHub. Completed responses can no longer be edited in Shiny.",
        type="warning"
      )
      record_table_context(NULL)
      app_view("tasks")
      return()
    }

    open_case <- function(cases,set_cases,set_index,view,status_value="") {
      if (nzchar(status_value) && identical(status_value,"consumed")) {
        showNotification("This batch has already been consumed by GitHub and is read-only.",type="warning")
        return(FALSE)
      }
      ids <- vapply(cases,record_table_case_id,character(1))
      hit <- match(case_id,ids)
      if (is.na(hit)) {
        showNotification("The selected record is no longer available in the active batch.",type="warning")
        return(FALSE)
      }
      set_cases(cases)
      set_index(hit)
      record_table_context(NULL)
      app_view(view)
      TRUE
    }

    if (identical(workflow,"01") && identical(task_type,"deduplication") && identical(batch_id,batch_id_rv())) {
      complete(FALSE)
      open_case(w01_all_cases_rv(),cases_rv,idx,"w01",batch_status_rv())
    } else if (identical(workflow,"02") && identical(task_type,"enrichment") && identical(batch_id,w02_batch_id_rv())) {
      open_case(w02_all_cases_rv(),w02_cases_rv,w02_idx,"w02",w02_batch_status_rv())
    } else if (identical(workflow,"04") && identical(task_type,"manual_screening") && identical(batch_id,w04_batch_id_rv())) {
      open_case(w04_all_cases_rv(),w04_cases_rv,w04_idx,"w04",w04_batch_status_rv())
    } else if (identical(workflow,"04") && identical(task_type,"model_uncertainty") && identical(batch_id,w04_resolution_batch_id_rv())) {
      open_case(w04_resolution_all_cases_rv(),w04_resolution_cases_rv,w04_resolution_idx,"w04_resolution",w04_resolution_batch_status_rv())
    } else if (identical(workflow,"04") && identical(task_type,"conflict_resolution") && identical(batch_id,w04_active_conflict_batch_id())) {
      cases <- w04_all_conflict_cases()
      ids <- vapply(cases,record_table_case_id,character(1))
      hit <- match(case_id,ids)
      if (is.na(hit)) {
        showNotification("The selected record is no longer available in the active conflict batch.",type="warning")
      } else {
        w04_conflict_idx(hit)
        record_table_context(NULL)
        app_view("w04_conflict")
      }
    } else if (identical(workflow,"08") && identical(task_type,"annotation") && identical(batch_id,w08_batch_id_rv())) {
      open_case(w08_all_cases_rv(),w08_cases_rv,w08_idx,"w08",w08_batch_status_rv())
    } else {
      showNotification("This completed record is no longer part of an active editable batch.",type="warning")
    }
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
      closed="Resolved elsewhere",
      outstanding="Outstanding",
      resolved_cases="Completed",
      outstanding_cases="Outstanding",
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
    ctx <- record_table_context()
    allow_edit <- !is.null(ctx) &&
      identical(as.character(ctx$metric %||% ""),"resolved_cases") &&
      session_can("manage_assignments") &&
      !identical(as.character(ctx$workflow %||% ""),"all") &&
      !record_table_batch_dispatched(ctx$workflow,ctx$task_type,ctx$batch_id)

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
        tags$td(if(nzchar(r$last_activity)) r$last_activity else tags$span(class="text-secondary","—")),
        if (allow_edit) tags$td(
          tags$button(
            type="button",
            class="btn btn-outline-primary btn-sm lem-record-edit",
            `data-workflow`=r$workflow,
            `data-task-type`=r$task_type,
            `data-batch-id`=ctx$batch_id,
            `data-case-id`=r$case_id,
            "Edit"
          )
        ) else NULL
      )

      detail <- if(abstract_open || notes_open) {
        tags$tr(
          class="lem-detail-row",
          tags$td(
            colspan=if(allow_edit) "8" else "7",
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
          tags$th("Last activity"),
          if (allow_edit) tags$th("Edit") else NULL
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
    catalogue <- search_scope_catalogue_rv()
    if (!is.null(catalogue) && nrow(catalogue)) {
      hit <- match(slug, as.character(catalogue$source_slug))
      if (!is.na(hit)) return(as.character(catalogue$source[[hit]]))
    }
    x <- sub("^ebsco_", "", slug)
    tools::toTitleCase(gsub("_", " ", x, fixed=TRUE))
  }

  search_scope_display_status <- function(x) {
    x <- tolower(trimws(as.character(x %||% "")))
    if (!nzchar(x)) return("Queued")
    if (grepl("fail|error", x)) return("Failed")
    if (x %in% c("queued","waiting","pending")) return("Queued")
    if (x %in% c("in_progress","running")) return("Running")
    "Complete"
  }

  search_scope_counts_resolved <- function(rows = search_scope_rows_rv()) {
    if (is.null(rows) || !nrow(rows) || !"status" %in% names(rows)) return(FALSE)
    states <- vapply(rows$status,search_scope_display_status,character(1))
    !any(states %in% c("Queued","Running"))
  }

  search_scope_change_summary <- function(original, revised) {
    original <- as.character(original %||% "")
    revised <- as.character(revised %||% "")
    if (identical(original, revised)) return("No change from original.")
    a <- strsplit(original, "", fixed=TRUE)[[1L]]
    b <- strsplit(revised, "", fixed=TRUE)[[1L]]
    max_prefix <- min(length(a), length(b))
    prefix <- 0L
    if (max_prefix > 0L) {
      while (prefix < max_prefix && identical(a[[prefix + 1L]], b[[prefix + 1L]])) prefix <- prefix + 1L
    }
    max_suffix <- min(length(a) - prefix, length(b) - prefix)
    suffix <- 0L
    if (max_suffix > 0L) {
      while (
        suffix < max_suffix &&
        identical(a[[length(a) - suffix]], b[[length(b) - suffix]])
      ) suffix <- suffix + 1L
    }
    old_end <- length(a) - suffix
    new_end <- length(b) - suffix
    old_mid <- if (old_end >= prefix + 1L) paste0(a[(prefix + 1L):old_end], collapse="") else ""
    new_mid <- if (new_end >= prefix + 1L) paste0(b[(prefix + 1L):new_end], collapse="") else ""
    old_mid <- trimws(old_mid)
    new_mid <- trimws(new_mid)
    if (!nzchar(old_mid)) return(sprintf("Added relative to original: %s", new_mid))
    if (!nzchar(new_mid)) return(sprintf("Removed relative to original: %s", old_mid))
    sprintf("Changed relative to original: %s  ->  %s", old_mid, new_mid)
  }

  search_scope_result_rows <- function(version) {
    results <- search_scope_results_rv() %||% list()
    rows <- results[[version]]
    if (
      identical(version, search_scope_active_version_rv()) &&
      !is.null(search_scope_rows_rv())
    ) rows <- search_scope_rows_rv()
    rows
  }

  search_scope_total_for_version <- function(version) {
    rows <- search_scope_result_rows(version)
    if (is.null(rows) || !nrow(rows) || !"hits" %in% names(rows)) return(NA_integer_)
    hits <- suppressWarnings(as.integer(rows$hits))
    if (all(is.na(hits))) return(NA_integer_)
    sum(hits, na.rm=TRUE)
  }

  # Scoping begins empty. No GitHub query is read into this field.
  # A separate request still reads the W00 source catalogue when needed.
  observeEvent(input$search_scope_input_text, {
    req(authenticated())
    if (!session_can("control_workflows")) return()
    if (nzchar(search_scope_request_id_rv())) return()
    query <- as.character(input$search_scope_input_text %||% "")
    if (!identical(query,search_scope_string_rv())) search_scope_string_rv(query)
  },ignoreInit=TRUE)

  observeEvent(input$w04_subset_load_corpus,{
    on.exit(session$sendCustomMessage("lem_busy_finished",list(id="w04_subset_load_corpus")),add=TRUE)
    req(authenticated())
    if(!session_can("control_workflows")) return()
    result <- withProgress(message="Verifying eligible W04 corpus",detail="Restoring restricted W02 and W03 archives",value=0.1,{
      tryCatch({
        pointers <- w04_subset_review_source_pointers()
        incProgress(0.2,detail="Checking review-specific W02/W03 provenance")
        eligible <- w04_subset_load_eligible(pointers$lean,pointers$w03)
        incProgress(0.6,detail="Validating canonical record eligibility")
        c(eligible,list(review_id=pointers$review_id))
      },error=function(e)e)
    })
    if(inherits(result,"error")) {
      w04_subset_corpus_rv(NULL)
      w04_subset_status_rv(paste("Eligible corpus could not be verified:",conditionMessage(result)))
      return()
    }
    w04_subset_corpus_rv(result)
    registered <- tryCatch(w04_subset_registry_read(result$review_id),error=function(e)e)
    if(inherits(registered,"error")) {
      w04_subset_corpus_rv(NULL)
      w04_subset_status_rv(paste("Subset history could not be checked:",conditionMessage(registered)))
      return()
    }
    w04_subsets_rv(registered)
    w04_subset_status_rv(sprintf("Verified %d eligible canonical records (lean SHA %s).",
      length(result$record_ids),substr(result$checkpoint_sha256,1,16)))
  })
  output$w04_subset_create_control <- renderUI({
    if(is.null(w04_subset_corpus_rv()) ||
       !identical(as.character(current_user()$role %||% ""),"administrator"))
      return(tags$p(class="saved-note","Creation requires a verified eligible corpus."))
    actionButton("w04_subset_create","Create and save subset",class="btn-primary btn-sm")
  })
  output$w04_subset_parent_select <- renderUI({
    corpus <- w04_subset_corpus_rv()
    if(is.null(corpus)) return(NULL)
    subsets <- Filter(function(x)identical(x$source_checkpoint_sha256,
      corpus$checkpoint_sha256),w04_subsets_rv())
    choices <- c("Full eligible corpus"="full")
    if(length(subsets)) choices <- c(choices,"Unsampled remainder"="remainder")
    if(length(subsets)) {
      ids <- vapply(subsets,function(x)x$subset_id,character(1))
      choices <- c(choices,stats::setNames(ids,paste0("Child of ",ids)))
    }
    selectInput("w04_subset_source","Sample from",choices=choices)
  })
  observeEvent(input$w04_subset_create,{
    req(authenticated())
    if(!session_can("control_workflows")||
       !identical(as.character(current_user()$role),"administrator")) return()
    corpus <- w04_subset_corpus_rv()
    if(is.null(corpus)) {
      w04_subset_status_rv("Verify the eligible canonical corpus before creating subsets.")
      return()
    }
    result <- withProgress(message="Saving W04 subset",detail="Checking existing subset assignments",value=0.1,{
      tryCatch({
      previous <- w04_subset_registry_read(corpus$review_id)
      matching <- Filter(function(x) identical(x$source_checkpoint_sha256,
        corpus$checkpoint_sha256),previous)
      origin <- as.character(input$w04_subset_source %||% "full")
      if(!length(matching) && !identical(origin,"full"))
        stop("The first subset must be sampled from the full eligible corpus",call.=FALSE)
      parent <- NULL
      pool <- corpus$record_ids
      if(identical(origin,"remainder")) {
        pool <- w04_subset_pool_remainder(pool,matching)
      } else if(!identical(origin,"full")) {
        hits <- Filter(function(x)identical(x$subset_id,origin),matching)
        if(length(hits)!=1L) stop("Selected parent subset is unavailable",call.=FALSE)
        parent <- hits[[1L]]
      }
      subset <- w04_subset_create(corpus$review_id,pool,
        size=as.integer(input$w04_subset_size),seed=sample.int(.Machine$integer.max,1L),
        parent=parent,source_checkpoint_sha256=corpus$checkpoint_sha256,
        created_by=session_reviewer_id(),existing=previous)
      w04_subset_registry_append(subset)
      new_label <- w04_subset_label_text(input$w04_subset_new_label %||% "")
      if(nzchar(new_label)) w04_subset_label_save(corpus$review_id,subset$subset_id,new_label,session_reviewer_id())
      list(id=subset$subset_id,all=w04_subset_registry_read(corpus$review_id),
           n=subset$actual_size)
      },error=function(e)e)
    })
    if(inherits(result,"error")) {
      w04_subset_status_rv(paste("Subset not confirmed saved:",conditionMessage(result)))
      return()
    }
    w04_subsets_rv(result$all)
    w04_subset_status_rv(sprintf("Verified subset %s (%d canonical records).",
      result$id,result$n))
  })
  observeEvent(input$w04_subset_prepare_queue,{
    on.exit(session$sendCustomMessage("lem_busy_finished",list(id="w04_subset_prepare_queue")),add=TRUE)
    req(authenticated())
    if(!session_can("manage_assignments")) return()
    result <- withProgress(message="Preparing W04 manual screening",detail="Verifying canonical source and subset membership",value=0.1,{
      tryCatch({
        meta <- w04_prompt_review_metadata()
        selected <- as.character(input$w04_subset_queue_selected %||% "")
        registry <- w04_subset_registry_read(meta$review_id)
        matches <- Filter(function(x)identical(x$subset_id,selected),registry)
        if(length(matches)!=1L) stop("Select a valid registered subset",call.=FALSE)
        existing <- w04_subset_queue_read(selected,meta$review_id)
        if(!is.null(existing)) {
          list(queue=existing,created=FALSE)
        } else {
          pointers <- w04_subset_review_source_pointers()
          if(!identical(pointers$review_id,meta$review_id) ||
             !identical(tolower(as.character(pointers$lean$lean_canonical_sha256)),matches[[1L]]$source_checkpoint_sha256))
            stop("Archived source differs from saved subset provenance",call.=FALSE)
          incProgress(0.25,detail="Restoring checksum-verified canonical metadata")
          source <- w04_subset_verified_archive(pointers$lean,"lean")
          queue <- w04_subset_screening_build(matches[[1L]],source,matches[[1L]]$source_checkpoint_sha256)
          incProgress(0.45,detail="Saving and verifying isolated subset queue")
          w04_subset_queue_store(queue,meta$review_id)
          list(queue=w04_subset_queue_read(selected,meta$review_id),created=TRUE)
        }
      },error=function(e)e)
    })
    if(inherits(result,"error")) {
      msg <- paste("Manual subset queue unavailable:",conditionMessage(result))
      w04_subset_status_rv(msg)
      showNotification(msg,type="error",duration=NULL)
      return()
    }
    q <- result$queue
    existing_decisions <- tryCatch(
      w04_filter_batch_decisions(active_sheet_w04_decisions(),q$queue_sha256),
      error=function(e)e)
    if(inherits(existing_decisions,"error")) {
      msg <- paste("Queue saved but could not activate assignment controls:",
        conditionMessage(existing_decisions))
      w04_subset_status_rv(msg)
      showNotification(msg,type="error",duration=NULL)
      return()
    }
    w04_reload_highlights()
    w04_all_cases_rv(q$cases)
    w04_queue_sha_rv(q$queue_sha256)
    w04_batch_id_rv(q$batch_id)
    w04_batch_status_rv(q$batch_status %||% "active")
    w04_decisions(existing_decisions)
    w04_manual_abstract_edits_rv(tryCatch(
      active_sheet_w04_manual_abstract_edits(q$queue_sha256,session_reviewer_id()),
      error=function(e){w04_manual_abstract_status_rv(paste("Abstract edits unavailable:",conditionMessage(e)));list()}))
    w04_manual_abstract_edit_rv(FALSE)
    # Highlight configuration belongs to the review, not the loaded subset.
    # Preserve already loaded values; the login/configuration reader refreshes them.
    if(!is.null(w04_highlight_version_rv())) {
      w04_include_terms(w04_highlight_version_rv()$include)
      w04_exclude_terms(w04_highlight_version_rv()$exclude)
    }
    w04_idx(1L)
    msg <- sprintf("%s screening queue %s with %d records. It is now active under W04 Manual screening → Manage assignments.",
      if(isTRUE(result$created))"Created" else "Loaded",q$batch_id,length(q$cases))
    w04_subset_status_rv(msg)
    showNotification(msg,type="message",duration=12)
  })
  output$w04_subset_verification_state <- renderUI({
    corpus <- w04_subset_corpus_rv()
    if(is.null(corpus)) return(tags$p(class="saved-note","Not yet verified in this session. Complete Step 1 before preparing a queue."))
    tags$p(class="saved-note",sprintf("Verified: %d eligible records. You can continue to Steps 2 and 3.",length(corpus$record_ids)))
  })
  output$w04_subset_prepare_control <- renderUI({
    ready <- length(w04_subsets_rv())>0L
    if(!ready) return(tags$p(class="saved-note","Load or create a registered subset to prepare a screening queue."))
    actionButton("w04_subset_prepare_queue","3. Prepare manual screening queue",
      class="btn-outline-primary btn-sm",
      onclick="window.lemBusyStart('w04_subset_prepare_queue','Preparing and verifying subset screening queue…')")
  })
  output$w04_subset_queue_selector <- renderUI({
    subsets <- w04_subsets_rv()
    if(!length(subsets)) return(tags$p(class="saved-note","Register a subset before preparing screening."))
    ids <- vapply(subsets,function(x)as.character(x$subset_id),character(1))
    selectInput("w04_subset_queue_selected","Subset for manual screening",
      choices=stats::setNames(ids,vapply(subsets,w04_subset_display,character(1))))
  })
  observeEvent(input$w04_manual_find_queues,{
    on.exit(session$sendCustomMessage("lem_busy_finished",list(id="w04_manual_find_queues")),add=TRUE)
    req(authenticated())
    result <- tryCatch({
      meta <- w04_prompt_review_metadata()
      subsets <- w04_subset_registry_read(meta$review_id)
      registered <- Filter(function(x) {
        batch_id <- paste0("w04-human-",x$subset_id)
        assigned <- active_assignments_for_batch(assignment_registry_rv(),"04",
          batch_id,"manual_screening")
        if(session_can("manage_assignments")) return(length(assigned)>0L)
        mine <- assignments_for_user(assignment_registry_rv(),"04",batch_id,
          session_reviewer_id(),"manual_screening")
        length(mine)>0L
      },subsets)
      registered
    },error=function(e)e)
    if(inherits(result,"error")) {
      w04_manual_queue_status_rv(paste("Unable to find assigned subset queues:",conditionMessage(result)))
      return()
    }
    w04_manual_queue_choices_rv(result)
    w04_manual_queue_status_rv(sprintf("%d assigned subset queues available.",length(result)))
  })
  output$w04_manual_queue_selector <- renderUI({
    xs <- w04_manual_queue_choices_rv()
    if(!length(xs)) return(NULL)
    ids <- vapply(xs,function(x)x$subset_id,character(1))
    selectInput("w04_manual_queue_selected","Assigned subset",choices=stats::setNames(ids,vapply(xs,w04_subset_display,character(1))))
  })
  output$w04_manual_queue_status <- renderText(w04_manual_queue_status_rv())
  observeEvent(input$w04_manual_load_queue,{
    on.exit(session$sendCustomMessage("lem_busy_finished",list(id="w04_manual_load_queue")),add=TRUE)
    req(authenticated())
    result <- tryCatch({
      subset_id <- as.character(input$w04_manual_queue_selected %||% "")
      choices <- w04_manual_queue_choices_rv()
      if(!nzchar(subset_id)||!any(vapply(choices,function(x)identical(x$subset_id,subset_id),logical(1))))
        stop("Select an assigned registered subset",call.=FALSE)
      meta <- w04_prompt_review_metadata()
      queue <- w04_subset_queue_read(subset_id,meta$review_id)
      if(is.null(queue)) stop("Registered subset queue not prepared",call.=FALSE)
      assigned <- active_assignments_for_batch(assignment_registry_rv(),"04",
        queue$batch_id,"manual_screening")
      if(!length(assigned)) stop("No active assignments for this subset",call.=FALSE)
      if(!session_can("manage_assignments")) {
        mine <- assignments_for_user(assignment_registry_rv(),"04",
          queue$batch_id,session_reviewer_id(),"manual_screening")
        if(!length(mine)) stop("No assigned records for this user",call.=FALSE)
      }
      all <- active_sheet_w04_decisions()
      list(queue=queue,decisions=w04_filter_batch_decisions(all,queue$queue_sha256))
    },error=function(e)e)
    if(inherits(result,"error")) {
      w04_manual_queue_status_rv(paste("Could not load subset screening:",conditionMessage(result)))
      return()
    }
    q <- result$queue
    w04_reload_highlights()
    w04_all_cases_rv(q$cases)
    w04_queue_sha_rv(q$queue_sha256)
    w04_batch_id_rv(q$batch_id)
    w04_batch_status_rv(q$batch_status %||% "active")
    # Highlight configuration belongs to the review, not the loaded subset.
    # Preserve already loaded values; the login/configuration reader refreshes them.
    if(!is.null(w04_highlight_version_rv())) {
      w04_include_terms(w04_highlight_version_rv()$include)
      w04_exclude_terms(w04_highlight_version_rv()$exclude)
    }
    w04_decisions(result$decisions)
    w04_manual_abstract_edits_rv(tryCatch(
      active_sheet_w04_manual_abstract_edits(q$queue_sha256,session_reviewer_id()),
      error=function(e){w04_manual_abstract_status_rv(paste("Abstract edits unavailable:",conditionMessage(e)));list()}))
    w04_manual_abstract_edit_rv(FALSE)
    w04_idx(1L)
    w04_manual_queue_status_rv(sprintf("Loaded assigned subset %s (%d records).",q$subset_id,length(q$cases)))
  })

  output$w04_highlight_status <- renderText(w04_highlight_status_rv())
  w04_apply_highlight_config <- function(config) {
    w04_highlight_version_rv(config)
    w04_include_terms(config$include)
    w04_exclude_terms(config$exclude)
    w04_highlight_status_rv(sprintf("Highlight terms loaded: include [%s]; exclude [%s].",
      paste(config$include,collapse=", "),paste(config$exclude,collapse=", ")))
  }
  w04_reload_highlights <- function() {
    config <- tryCatch(w04_highlight_read(w04_prompt_review_metadata()),error=function(e)e)
    if(inherits(config,"error")) {
      w04_highlight_status_rv(paste("Highlight configuration unavailable:",conditionMessage(config)))
      return(invisible(FALSE))
    }
    w04_apply_highlight_config(config)
    invisible(TRUE)
  }
  observeEvent(input$w04_highlight_load,{
    on.exit(session$sendCustomMessage("lem_busy_finished",list(id="w04_highlight_load")),add=TRUE)
    req(authenticated())
    if(!identical(as.character(current_user()$role %||% ""),"administrator")) return()
    z <- tryCatch(w04_highlight_read(w04_prompt_review_metadata()),error=function(e)e)
    if(inherits(z,"error")) {
      w04_highlight_status_rv(paste("Could not load highlight terms:",conditionMessage(z)))
      return()
    }
    w04_apply_highlight_config(z)
    updateTextAreaInput(session,"w04_highlight_include",value=paste(z$include,collapse="\n"))
    updateTextAreaInput(session,"w04_highlight_exclude",value=paste(z$exclude,collapse="\n"))
    w04_highlight_status_rv("Loaded current highlighted terms from review Google Sheet.")
  })
  observeEvent(input$w04_highlight_save,{
    on.exit(session$sendCustomMessage("lem_busy_finished",list(id="w04_highlight_save")),add=TRUE)
    req(authenticated())
    if(!identical(as.character(current_user()$role %||% ""),"administrator")) return()
    z <- tryCatch({
      meta <- w04_prompt_review_metadata()
      baseline <- w04_highlight_version_rv()
      if(is.null(baseline)) baseline <- w04_highlight_read(meta)
      config <- w04_highlight_config(meta,
        as.character(input$w04_highlight_include %||% ""),
        as.character(input$w04_highlight_exclude %||% ""),session_reviewer_id())
      w04_highlight_save(meta,config,baseline$sha256)
    },error=function(e)e)
    if(inherits(z,"error")) {
      w04_highlight_status_rv(paste("Highlight terms were not saved:",conditionMessage(z)))
      return()
    }
    w04_apply_highlight_config(z)
    w04_highlight_status_rv("Saved current highlighted terms to review Google Sheet.")
  })
  observeEvent(authenticated(),{
    if(!isTRUE(authenticated())||!identical(storage_backend(),"google_sheets")) return()
    w04_reload_highlights()
  },ignoreInit=TRUE)
  output$w04_subset_registry_status <- renderText(w04_subset_status_rv())
  output$w04_subset_registry_history <- renderUI({
    subsets <- w04_subsets_rv()
    if(!length(subsets)) return(tags$p(class="saved-note","No registered subsets loaded."))
    tags$ul(lapply(subsets,function(s)tags$li(
      tags$strong(w04_subset_display(s,records=FALSE)),
      paste(" · ",s$actual_size,"records · seed ",s$random_seed,
        " · checkpoint ",substr(s$source_checkpoint_sha256,1,12),
        if(nzchar(s$parent_subset_id)) paste(" · child of",s$parent_subset_id) else "")
    )))
  })
  output$w04_subset_label_editor <- renderUI({
    req(authenticated())
    if(!identical(as.character(current_user()$role %||% ""),"administrator")) return(NULL)
    xs <- w04_subsets_rv()
    if(!length(xs)) return(NULL)
    ids <- vapply(xs,function(x)as.character(x$subset_id),character(1))
    selected <- as.character(input$w04_subset_label_selected %||% "")
    if(!selected %in% ids) selected <- ids[[1L]]
    current <- Filter(function(x)identical(x$subset_id,selected),xs)[[1L]]
    tagList(
      tags$hr(),
      tags$h5("Edit subset label"),
      selectInput("w04_subset_label_selected","Registered subset",
        choices=stats::setNames(ids,vapply(xs,w04_subset_display,character(1))),
        selected=selected),
      textInput("w04_subset_label_value","Label (optional, maximum 100 characters)",
        value=as.character(current$label %||% "")),
      actionButton("w04_subset_label_save","Save label",class="btn-outline-primary btn-sm")
    )
  })
  observeEvent(input$w04_subset_label_save,{
    req(authenticated())
    if(!identical(as.character(current_user()$role %||% ""),"administrator")) return()
    result <- tryCatch({
      meta <- w04_prompt_review_metadata()
      w04_subset_label_save(meta$review_id,
        as.character(input$w04_subset_label_selected %||% ""),
        as.character(input$w04_subset_label_value %||% ""),
        session_reviewer_id())
    },error=function(e)e)
    if(inherits(result,"error")) {
      w04_subset_status_rv(paste("Subset label was not saved:",conditionMessage(result)))
      return()
    }
    w04_subsets_rv(result)
    w04_model_subsets_rv(result)
    w04_subset_status_rv("Subset label saved and verified.")
  })
  observeEvent(input$w04_subset_registry_refresh,{
    on.exit(session$sendCustomMessage("lem_busy_finished",list(id="w04_subset_registry_refresh")),add=TRUE)
    req(authenticated())
    if(!session_can("control_workflows")) return()
    result <- tryCatch({
      meta <- w04_prompt_review_metadata()
      w04_subset_registry_read(meta$review_id)
    },error=function(e)e)
    if(inherits(result,"error")) {
      w04_subset_status_rv(paste("Cannot load W04 subset registry:",conditionMessage(result)))
      return()
    }
    w04_subsets_rv(result)
    w04_subset_status_rv(sprintf("%d registered W04 subsets loaded.",length(result)))
  })

  # One event-bound refresh, no polling, no recalculation of saved kappa.
  output$w04_overview_status <- renderText({
    paste(w04_overview_status_rv(),
      if(nzchar(w04_overview_refreshed_rv()))paste("· Last refreshed",w04_overview_refreshed_rv()) else "")
  })
  observeEvent(list(input$configure_workflow_tab,input$w04_workspace_tab),{
    req(identical(input$configure_workflow_tab,"W04"),
        identical(input$w04_workspace_tab,"Model results"))
    req(authenticated(),session_can("manage_assignments"))
    if(is.null(w04_overview_rv())) {
      meta <- tryCatch(w04_prompt_review_metadata(),error=function(e)e)
      if(inherits(meta,"error")) {
        w04_overview_status_rv(paste("Overview unavailable:",conditionMessage(meta)))
      } else {
        loaded <- tryCatch(w04_summary_tables(w04_summary_read(review_id=meta$review_id,create_if_missing=FALSE)),error=function(e)e)
        if(inherits(loaded,"error")) w04_overview_status_rv(paste("Overview unavailable:",conditionMessage(loaded)))
        else {w04_overview_rv(loaded);w04_overview_status_rv("Loaded from W04 summary sheet.")}
      }
    }
  })
  observeEvent(list(input$w04_overview_refresh,input$w04_model_results_refresh),{
    req(authenticated())
    if(!session_can("manage_assignments"))return()
    w04_overview_status_rv("Reading W04 summary sheet…")
    loaded <- tryCatch({
      w04_summary_tables(w04_summary_read(review_id=w04_prompt_review_metadata()$review_id,create_if_missing=FALSE))
    },error=function(e)e)
    if(inherits(loaded,"error")) {
      w04_overview_status_rv(paste("Overview refresh failed:",conditionMessage(loaded)))
      return()
    }
    w04_overview_rv(loaded)
    w04_overview_refreshed_rv(format(Sys.time(),"%d %b %Y %H:%M %Z",tz="Europe/London"))
    w04_overview_status_rv(sprintf("Loaded %d subsets, %d prompts, %d model runs and %d saved comparisons from one summary tab.",
      nrow(loaded$subsets),nrow(loaded$prompts),nrow(loaded$runs),nrow(loaded$comparisons)))
  })
  output$w04_overview_tables <- renderUI({
    x <- w04_overview_rv()
    if(is.null(x))return(tags$p(class="saved-note","No summary loaded yet. Refresh to read the W04 summary sheet."))
    tags$div(class="w04-compact-overview",
      tags$style(HTML(".w04-compact-overview {font-size:0.8rem;line-height:1.2} .w04-compact-overview h5{font-size:0.93rem;margin:0.65rem 0 0.25rem} .w04-compact-overview table{font-size:0.8rem} .w04-compact-overview th,.w04-compact-overview td{padding:0.2rem 0.35rem}")),
      tags$h5("Human-screened subsets"),tableOutput("w04_overview_subsets"),
      tags$h5("Prompt versions"),tableOutput("w04_overview_prompts"),
      tags$h5("Model runs"),tableOutput("w04_overview_runs"),
      tags$h5("Saved comparisons"),tableOutput("w04_overview_comparisons"),
      tags$p(class="saved-note","Open saved disagreements in the Conflicts tab.")
    )
  })
  output$w04_overview_subsets <- renderTable({req(w04_overview_rv());w04_overview_rv()$subsets},striped=TRUE,spacing="xs")
  output$w04_overview_prompts <- renderTable({req(w04_overview_rv());w04_overview_rv()$prompts},striped=TRUE,spacing="xs")
  output$w04_overview_runs <- renderTable({req(w04_overview_rv());w04_overview_rv()$runs},striped=TRUE,spacing="xs")
  output$w04_overview_comparisons <- renderTable({req(w04_overview_rv());w04_overview_rv()$comparisons},striped=TRUE,spacing="xs")
  output$w04_overview_conflicts <- renderUI({
    req(authenticated(),session_can("manage_assignments"),w04_overview_rv())
    overview <- w04_overview_rv()
    rows <- overview$conflicts
    ids <- overview$conflict_choices
    if(!nrow(rows))return(tags$p(class="saved-note","No conflict sets saved."))
    tags$table(class="table table-striped table-sm",
      tags$thead(tags$tr(lapply(names(rows),tags$th))),
      tags$tbody(lapply(seq_len(nrow(rows)),function(i) {
        cells <- lapply(seq_len(ncol(rows)),function(j) {
          value <- as.character(rows[i,j])
          if(j==1L && !is.null(ids) && nrow(ids)>=i && nzchar(as.character(ids$id[[i]])))
            tags$td(actionLink(paste0("w04_conflict_set_open_",i),value,
              title="Open conflict set for adjudication"))
          else tags$td(value)
        })
        tags$tr(cells)
      }))
    )
  })
  w04_conflict_row_observers <- list()
  observeEvent(w04_overview_rv(),{
    lapply(w04_conflict_row_observers,function(x)x$destroy())
    w04_conflict_row_observers <<- list()
    if(!isTRUE(authenticated()) || !session_can("manage_assignments"))return()
    overview <- w04_overview_rv()
    if(is.null(overview) || is.null(overview$conflict_choices))return()
    ids <- as.character(overview$conflict_choices$id)
    w04_conflict_row_observers <<- lapply(seq_along(ids),function(i) {
      local({
        index <- i
        target <- ids[[i]]
        observeEvent(input[[paste0("w04_conflict_set_open_",index)]],{
          w04_open_saved_conflict_set(target)
        },ignoreInit=TRUE)
      })
    })
  })
  w04_overview_conflict_action_status_rv <- reactiveVal("")
  output$w04_overview_conflict_action_status <- renderText(w04_overview_conflict_action_status_rv())
  w04_open_saved_conflict_set <- function(id) {
    req(authenticated(),session_can("manage_assignments"))
    w04_overview_conflict_action_status_rv("Opening conflict set…")
    result <- tryCatch({
      sets <- read_w04_conflict_sets()
      matches <- Filter(function(z)
        identical(as.character(z$conflict_set_id %||% ""),id),sets)
      if(length(matches)!=1L)stop("Conflict set not uniquely present in the authoritative registry.")
      selected <- matches[[1L]]
      if(!identical(as.character(selected$comparison_type %||% ""),"human_model"))
        stop("Direct opening of human-only sets is not yet supported; use the existing assignment-based review.")
      loaded <- activate_w04_model_conflict(selected)
      if(loaded<1L)stop("No conflict cases could be reconstructed.")
      w04_conflict_sets_rv(sets)
      w04_admin_direct_conflict_set_rv(id)
      unresolved <- w04_conflict_unresolved_indices()
      w04_conflict_idx(if(length(unresolved))unresolved[[1L]] else 1L)
      app_view("w04_conflict")
      loaded
    },error=function(e)e)
    if(inherits(result,"error")) {
      w04_overview_conflict_action_status_rv(paste("Could not open conflict set:",conditionMessage(result)))
      showNotification(conditionMessage(result),type="error",duration=10)
    } else {
      w04_overview_conflict_action_status_rv(sprintf("Opened %d conflict cases for adjudication.",result))
    }
  }
  output$w04_overview_conflict_action <- renderUI({
    req(authenticated(),session_can("manage_assignments"))
    x <- w04_overview_rv()
    if(is.null(x) || is.null(x$comparison_choices) || !nrow(x$comparison_choices))return(NULL)
    rows <- x$comparison_choices
    rows <- rows[!is.na(rows$conflicts) & rows$conflicts>0L & nzchar(rows$id),,drop=FALSE]
    if(!nrow(rows))return(NULL)
    div(class="d-flex flex-wrap align-items-end gap-2 mb-2",
      div(style="min-width:340px;max-width:600px;flex:1",
        selectInput("w04_overview_conflict_analysis","Comparison to examine",
          choices=stats::setNames(rows$id,rows$label),selected=tail(rows$id,1L))),
      actionButton("w04_overview_open_conflicts","Examine conflicts",class="btn-outline-primary btn-sm"))
  })
  observeEvent(input$w04_overview_open_conflicts,{
    req(authenticated(),session_can("manage_assignments"))
    id <- as.character(input$w04_overview_conflict_analysis %||% "")
    overview <- isolate(w04_overview_rv())
    valid <- !is.null(overview) && !is.null(overview$comparison_choices) &&
      id %in% as.character(overview$comparison_choices$id)
    if(!nzchar(id) || !isTRUE(valid))return()
    w04_overview_conflict_action_status_rv("Retrieving the saved comparison…")
    result <- tryCatch({
      analyses <- read_w04_consistency_analyses()
      matches <- Filter(function(z)identical(as.character(z$analysis_id %||% ""),id),analyses)
      if(length(matches)!=1L)stop("Comparison not found uniquely in the authoritative history.")
      n <- suppressWarnings(as.integer(as.character(matches[[1L]]$conflict_n %||% "0")))
      if(is.na(n) || n<1L)stop("The saved comparison has no disagreements.")
      saved <- append_w04_conflict_set(matches[[1L]],created_by=session_reviewer_id())
      all_sets <- w04_conflict_sets_rv() %||% list()
      sid <- as.character(saved$conflict_set_id %||% "")
      if(!any(vapply(all_sets,function(z)identical(as.character(z$conflict_set_id %||% ""),sid),logical(1))))
        w04_conflict_sets_rv(c(all_sets,list(saved)))
      if(identical(as.character(saved$comparison_type %||% ""),"human_model")) {
        loaded <- tryCatch(activate_w04_model_conflict(saved),error=function(e)e)
        if(inherits(loaded,"error"))
          stop(paste("Conflict set exists, but its cases could not be activated:",conditionMessage(loaded)))
      }
      fresh <- tryCatch(w04_summary_tables(w04_summary_read(
        review_id=w04_prompt_review_metadata()$review_id,create_if_missing=FALSE)),error=function(e)e)
      if(!inherits(fresh,"error"))w04_overview_rv(fresh)
      sid
    },error=function(e)e)
    w04_overview_conflict_action_status_rv(if(inherits(result,"error"))
      paste("Could not open conflicts:",conditionMessage(result))
      else paste("Conflict set",result,"is available. Open W04 conflict resolution to examine its cases; assign them first if needed."))
  })

  output$w04_prompt_archive_status <- renderText(w04_prompt_status_rv())
  output$w04_prompt_zenodo_link <- renderUI({
    req(authenticated())
    if(!session_can("control_workflows")) return(NULL)
    meta <- tryCatch(w04_prompt_review_metadata(),error=function(e)NULL)
    if(is.null(meta)) return(NULL)
    id <- w04_prompt_draft_id(meta)
    if(!nzchar(id)) return(tags$span(class="saved-note","No W04 Zenodo draft created yet."))
    tags$a(href=paste0("https://zenodo.org/uploads/",id),
      target="_blank",rel="noopener noreferrer",
      paste("Open restricted W04 Zenodo draft",id))
  })
  # Saved W04 prompt selectors use the archive's save timestamp, newest first.
  # Missing dates are kept last; version_id breaks ties deterministically.
  w04_prompts_by_save_date <- function(versions) {
    if(!length(versions))return(versions)
    dates <- vapply(versions,function(v)as.character(v$created_at_utc %||% ""),character(1))
    ids <- vapply(versions,function(v)as.character(v$version_id %||% ""),character(1))
    versions[order(!nzchar(dates),-xtfrm(dates),-xtfrm(ids))]
  }
  output$w04_prompt_archive_history <- renderUI({
    versions <- w04_prompts_by_save_date(w04_prompt_versions_rv())
    if(!length(versions)) return(tags$p(class="saved-note","No archived versions loaded."))
    labels <- w04_prompt_display_labels(versions)
    tags$ul(lapply(seq_along(versions),function(i) tags$li(
      tags$strong(labels[[i]]),
      paste(" · ",versions[[i]]$status," · ",versions[[i]]$author,
        " · SHA-256 ",substr(versions[[i]]$sha256,1,16))
    )))
  })
  observeEvent(input$w04_prompt_upload,{
    req(authenticated())
    if(!session_can("control_workflows")) return()
    uploaded <- input$w04_prompt_upload
    req(uploaded)
    result <- tryCatch(w04_prompt_from_upload(uploaded$datapath,uploaded$name),
      error=function(e)e)
    if(inherits(result,"error")) {
      w04_prompt_status_rv(paste("Prompt import failed:",conditionMessage(result)))
      return()
    }
    updateTextAreaInput(session,"w04_prompt_text",value=result)
    w04_prompt_status_rv("Prompt extracted into editor. Not saved to Zenodo.")
  })
  observeEvent(input$w04_prompt_refresh,{
    on.exit(session$sendCustomMessage("lem_busy_finished",list(id="w04_prompt_refresh")),add=TRUE)
    req(authenticated())
    if(!session_can("control_workflows")) return()
    result <- tryCatch({
      meta <- w04_prompt_review_metadata()
      w04_prompt_archive_versions(meta)
    },error=function(e)e)
    if(inherits(result,"error")) {
      w04_prompt_status_rv(paste("Could not load W04 archive:",conditionMessage(result)))
      return()
    }
    w04_prompt_versions_rv(result)
    if(length(result)) {
      timestamps <- vapply(result,function(v)as.character(v$created_at_utc %||% ""),character(1))
      latest <- result[[order(timestamps,decreasing=TRUE)[[1L]]]]
      updateTextAreaInput(session,"w04_prompt_text",value=latest$content)
      w04_prompt_status_rv(paste("Loaded latest archived prompt into editor:",latest$version_id,
        sprintf("(%d archived versions available).",length(result))))
    } else {
      w04_prompt_status_rv("No archived W04 prompt versions found.")
    }
  })
  observeEvent(input$w04_prompt_save,{
    req(authenticated())
    if(!session_can("control_workflows")||
       !identical(as.character(current_user()$role),"administrator")) {
      w04_prompt_status_rv("Administrator permission is required.")
      session$sendCustomMessage("w04_prompt_feedback",list(done=TRUE))
      return()
    }
    result <- tryCatch({
      meta <- w04_prompt_review_metadata()
      existing <- w04_prompt_archive_versions(meta)
      prompt <- as.character(input$w04_prompt_text %||% "")
      filename <- if(!is.null(input$w04_prompt_upload))
        as.character(input$w04_prompt_upload$name) else ""
      version <- w04_prompt_version(meta$review_id,prompt,session_reviewer_id(),
        filename=filename,notes=as.character(input$w04_prompt_change_notes %||% ""),
        status="draft",parent_version=if(length(existing))
          as.character(existing[[length(existing)]]$version_id) else "")
      w04_prompt_archive_save(meta,version)
      refreshed <- w04_prompt_archive_versions(w04_prompt_review_metadata())
      list(id=version$version_id,versions=refreshed)
    },error=function(e)e)
    if(inherits(result,"error")) {
      w04_prompt_status_rv(paste("Prompt was not confirmed saved:",conditionMessage(result)))
      session$sendCustomMessage("w04_prompt_feedback",list(done=TRUE))
      return()
    }
    w04_prompt_versions_rv(result$versions)
    w04_prompt_status_rv(paste("Verified restricted Zenodo draft version:",result$id))
    session$sendCustomMessage("w04_prompt_feedback",list(done=TRUE))
  })

  observeEvent(input$w04_model_batches_refresh,{
    req(authenticated())
    if(!session_can("control_workflows")) return()
    result <- tryCatch({
      meta <- w04_prompt_review_metadata()
      w04_model_batch_registry_read(meta$review_id)
    },error=function(e)e)
    if(inherits(result,"error")) {
      w04_model_return_status_rv(paste("Cannot load model batches:",conditionMessage(result)))
      return()
    }
    w04_model_batches_rv(result)
    w04_finalise_selection_batches_rv(result)
    # Publication metadata is synchronised in bulk when completed model runs
    # are loaded; never poll the model registry from the read-only overview.
    synced <- tryCatch({
      meta <- w04_prompt_review_metadata()
      w04_summary_backfill(meta$review_id,list(),list(),result,list(),list())
    },error=function(e)e)
    w04_model_return_status_rv(if(inherits(synced,"error"))
      sprintf("%d completed model run(s) loaded, but summary sync failed: %s",
        length(result),conditionMessage(synced))
      else sprintf("%d completed model run(s) available; %d new summary row(s).",
        length(result),synced))
  })
  output$w04_model_registry_diagnostic <- renderUI({
    xs <- w04_model_batches_rv()
    if(!length(xs))return(tags$p(class="saved-note",
      "No model batches loaded. Click Load completed model batches; any read error appears below."))
    tags$p(class="saved-note",sprintf(
      "%d saved model runs loaded. Run IDs: %s.",
      length(xs),paste(vapply(xs,function(x)as.character(x$run_id),character(1)),
        collapse=", ")))
  })
  output$w04_model_batch_selector <- renderUI({
    req(authenticated())
    xs <- w04_model_batches_rv()
    if(!length(xs)) return(tags$p(class="saved-note","Load completed batches after a model run has finished."))
    ids <- vapply(xs,function(x)as.character(x$run_id),character(1))
    subsets <- w04_subsets_rv()
    labels <- vapply(xs,w04_model_run_display,character(1),
      subsets=subsets,prompts=w04_model_versions_rv())
    selectInput("w04_model_batch_selected","Model screening run",
      choices=stats::setNames(ids,labels))
  })
  observeEvent(input$w04_model_batch_open,{
    req(authenticated())
    if(!session_can("control_workflows")) return()
    result <- tryCatch({
      id <- as.character(input$w04_model_batch_selected %||% "")
      xs <- Filter(function(x)identical(as.character(x$run_id),id),w04_model_batches_rv())
      if(length(xs)!=1L)stop("Choose a registered model run",call.=FALSE)
      batch <- xs[[1L]]
      rows <- w04_model_batch_results_read(batch)
      queue <- NULL
      decisions <- list()
      if(as.integer(batch$uncertain)>0L) {
        queue <- read_sheet_w04_resolution_queue(as.character(batch$resolution_tab))
        if(is.null(queue) ||
           !identical(as.character(queue$source_run_id),id) ||
           !identical(as.character(queue$queue_sha256),as.character(batch$resolution_queue_sha256)))
          stop("Model run uncertainty queue is missing or does not match its batch",call.=FALSE)
        if(length(queue$cases)!=as.integer(batch$uncertain))
          stop("Model uncertainty queue count differs from batch registry",call.=FALSE)
        decisions <- w04_filter_batch_decisions(active_sheet_w04_resolution_decisions(),queue$queue_sha256)
      }
      list(batch=batch,rows=rows,queue=queue,decisions=decisions)
    },error=function(e)e)
    if(inherits(result,"error")) {
      w04_model_return_status_rv(paste("Model batch could not be opened:",conditionMessage(result)))
      return()
    }
    w04_model_selected_batch_rv(result$batch)
    w04_model_results_rv(result$rows)
    if(!is.null(result$queue)) {
      q <- result$queue
      w04_resolution_all_cases_rv(q$cases)
      w04_resolution_queue_sha_rv(q$queue_sha256)
      w04_resolution_batch_id_rv(q$batch_id)
      w04_resolution_source_run_id_rv(q$source_run_id)
      w04_resolution_origin_rv("shiny_model")
      w04_resolution_batch_status_rv(q$batch_status %||% "")
      w04_resolution_decisions(result$decisions)
      w04_resolution_resume_requested_rv(FALSE)
      w04_resolution_idx(1L)
      w04_model_return_status_rv(sprintf(
        "Opened run %s: %d model uncertainties in W04 Model uncertainty resolution. Assign these cases to a reviewer below.",
        result$batch$run_id,length(q$cases)))
    } else {
      w04_resolution_all_cases_rv(list())
      w04_resolution_queue_sha_rv("")
      w04_resolution_batch_id_rv("")
      w04_resolution_source_run_id_rv("")
      w04_resolution_origin_rv("")
      w04_resolution_batch_status_rv("")
      w04_resolution_decisions(list())
      w04_resolution_resume_requested_rv(FALSE)
      w04_resolution_idx(1L)
      w04_model_return_status_rv(sprintf(
        "Opened run %s: no model uncertainties requiring adjudication.",result$batch$run_id))
    }
  })
  observeEvent(input$w04_refinement_load_prompts,{
    req(authenticated(),session_can("control_workflows"))
    batch <- w04_model_selected_batch_rv()
    if(is.null(batch)) {
      w04_refinement_status_rv("Open a completed model run before loading prompt versions.")
      return()
    }
    found <- tryCatch(w04_prompt_archive_versions(w04_prompt_review_metadata()),error=function(e)e)
    if(inherits(found,"error")) {
      w04_refinement_status_rv(paste("Cannot load archived prompts:",conditionMessage(found)))
      return()
    }
    w04_refinement_versions_rv(found)
    w04_refinement_status_rv(sprintf("%d archived prompt version(s) available. Choose the one to improve.",length(found)))
  })
  output$w04_refinement_prompt_selector <- renderUI({
    batch <- w04_model_selected_batch_rv()
    versions <- w04_prompts_by_save_date(w04_refinement_versions_rv())
    if(is.null(batch)||!length(versions))return(NULL)
    ids <- vapply(versions,function(v)as.character(v$version_id),character(1))
    labels <- w04_prompt_display_labels(versions)
    labels <- paste0(labels,ifelse(ids==as.character(batch$prompt_version_id),
                                   " (screening-run original)",""))
    choices <- stats::setNames(ids,labels)
    shiny::selectInput("w04_refinement_prompt_selected","Archived prompt to revise",
      choices=choices,selected=as.character(batch$prompt_version_id))
  })
  output$w04_refinement_status <- renderText(w04_refinement_status_rv())
  output$w04_refinement_run_link <- renderUI({
    url <- w04_refinement_run_url_rv()
    if(!nzchar(url))return(NULL)
    tags$a("View refinement GitHub Actions run",href=url,target="_blank",rel="noopener noreferrer")
  })
  output$w04_refinement_review <- renderUI({
    ans <- w04_refinement_response_rv()
    if(is.null(ans)) return(NULL)
    shiny::tagList(
      shiny::tags$strong("Proposed revised prompt"),
      shiny::tags$p(class="saved-note","New or modified lines are shown in red. The complete proposed text is editable below."),
      w04_refinement_diff_ui(as.character(ans$original_prompt),as.character(ans$proposed_prompt)),
      shiny::tags$p(class="saved-note",paste("Suggested changes:",as.character(ans$explanation %||% ""))),
      shiny::textAreaInput("w04_refinement_edited","Review or edit complete revised prompt",
        value=as.character(ans$proposed_prompt),rows=15,width="100%"),
      shiny::actionButton("w04_refinement_accept","Accept suggested prompt and save new draft",class="btn-primary btn-sm"),
      shiny::actionButton("w04_refinement_save_edit","Save my edited prompt as new draft",class="btn-outline-primary btn-sm"),
      shiny::actionButton("w04_refinement_reject","Reject suggestion",class="btn-outline-danger btn-sm")
    )
  })
  observeEvent(input$w04_refinement_request,{
    req(authenticated())
    if(!session_can("control_workflows") ||
       !identical(as.character(current_user()$role),"administrator"))return()
    result <- tryCatch({
      meta <- w04_prompt_review_metadata()
      batch <- w04_model_selected_batch_rv()
      if(is.null(batch))stop("Open the completed model run first",call.=FALSE)
      prior <- w04_refinement_find_existing(meta,batch)
      if(!w04_refinement_can_start_new(meta,prior))
        stop(paste("A previous refinement is still active or its result cannot be verified.",
          "Retrieve its result or inspect the GitHub Actions run before requesting another."),call.=FALSE)
      if(!identical(as.character(w04_resolution_source_run_id_rv()),as.character(batch$run_id)) ||
         !identical(w04_resolution_origin_rv(),"shiny_model"))
        stop("Open this model run's uncertainty queue before requesting a revision",call.=FALSE)
      versions <- w04_prompt_archive_versions(meta)
      chosen <- as.character(input$w04_refinement_prompt_selected %||% "")
      if(!nzchar(chosen))stop("Load and select an archived prompt to revise first",call.=FALSE)
      prompt <- Filter(function(x)identical(as.character(x$version_id),chosen),versions)
      if(length(prompt)!=1L)stop("Selected archived prompt is unavailable",call.=FALSE)
      cases <- w04_resolution_all_cases_rv()
      decisions <- w04_filter_batch_decisions(active_sheet_w04_resolution_decisions(),
        w04_resolution_queue_sha_rv())
      request <- w04_refinement_build_request(meta,batch,prompt[[1L]]$content,
        cases,decisions,as.character(input$w04_refinement_note %||% ""),
        selected_version_id=chosen)
      info <- w04_refinement_archive_request(meta,request)
      # Never automatically launch W04 screening. This dispatch runs only one
      # prompt-suggestion call; the accepted draft is a separate user decision.
      w04_refinement_dispatch(info)
      info
    },error=function(e)e)
    if(inherits(result,"error")) {
      w04_refinement_status_rv(paste("Prompt revision request not dispatched:",conditionMessage(result)))
      return()
    }
    w04_refinement_request_rv(result)
    w04_refinement_response_rv(NULL)
    w04_refinement_run_url_rv("")
    w04_refinement_poll_active_rv(TRUE)
    w04_refinement_status_rv(paste("One refinement request dispatched to GitHub for run",
      result$run_id,". When it completes, click Retrieve suggested prompt."))
  })
  observe({
    req(authenticated(),session_can("control_workflows"))
    if(!isTRUE(w04_refinement_poll_active_rv()))return()
    shiny::invalidateLater(20000,session)
    info <- w04_refinement_request_rv()
    if(is.null(info)) {
      w04_refinement_poll_active_rv(FALSE)
      return()
    }
    outcome <- tryCatch({
      status <- w04_refinement_run_status(info)
      proposed <- if(identical(status$state,"completed"))
        w04_refinement_load_response(w04_prompt_review_metadata(),info) else NULL
      list(status=status,proposed=proposed)
    },error=function(e)e)
    if(inherits(outcome,"error")) {
      w04_refinement_status_rv(paste("Refinement status check failed; will retry:",conditionMessage(outcome)))
      return()
    }
    status <- outcome$status
    w04_refinement_run_url_rv(status$url)
    if(identical(status$state,"failed")) {
      w04_refinement_poll_active_rv(FALSE)
      w04_refinement_status_rv(paste("Refinement workflow failed:",status$conclusion,
        ". The original prompt is unchanged. View the GitHub Actions run for details."))
    } else if(!is.null(outcome$proposed)) {
      w04_refinement_response_rv(outcome$proposed)
      w04_refinement_poll_active_rv(FALSE)
      w04_refinement_status_rv("Refinement completed. The revised prompt has been loaded for review below.")
      showNotification("W04 refinement completed: proposed prompt ready for review.",type="message",duration=10)
    } else {
      w04_refinement_status_rv(if(identical(status$state,"completed"))
        "GitHub finished; awaiting the archived proposal. Checking again…" else
        if(identical(status$state,"running")) "Refinement running. Checking automatically every 20 seconds…" else
          "Refinement queued. Checking automatically every 20 seconds…")
    }
  })
  observeEvent(input$w04_refinement_fetch,{
    req(authenticated())
    if(!session_can("control_workflows"))return()
    result <- tryCatch({
      info <- w04_refinement_request_rv()
      meta <- w04_prompt_review_metadata()
      if(is.null(info)) {
        batch <- w04_model_selected_batch_rv()
        if(is.null(batch))stop("Open the completed model run first",call.=FALSE)
        info <- w04_refinement_find_existing(meta,batch)
        if(is.null(info))stop("No refinement request exists for this model run",call.=FALSE)
        w04_refinement_request_rv(info)
      }
      w04_refinement_load_response(meta,info)
    },error=function(e)e)
    if(inherits(result,"error")) {
      w04_refinement_status_rv(paste("Cannot retrieve proposed prompt:",conditionMessage(result)))
    } else if(is.null(result)) {
      w04_refinement_status_rv("The GitHub refinement workflow has not published its response yet.")
    } else {
      w04_refinement_response_rv(result)
      w04_refinement_poll_active_rv(FALSE)
      w04_refinement_status_rv("Proposal retrieved. Review highlighted changes before accepting, editing or rejecting.")
    }
  })
  save_w04_refined_prompt <- function(accept=TRUE) {
    if(!session_can("control_workflows") ||
       !identical(as.character(current_user()$role),"administrator"))return()
    ans <- w04_refinement_response_rv()
    if(is.null(ans)) {
      w04_refinement_status_rv("Load a proposed prompt before saving.")
      return()
    }
    text <- if(accept)as.character(ans$proposed_prompt) else as.character(input$w04_refinement_edited %||% "")
    outcome <- tryCatch({
      meta <- w04_prompt_review_metadata()
      if(!identical(as.character(ans$review_id),meta$review_id))
        stop("Review identity changed",call.=FALSE)
      v <- w04_prompt_version(meta$review_id,text,session_reviewer_id(),
        notes=paste0("Refinement of model run ",ans$source_run_id,
          "; source prompt ",ans$prompt_version_id,
          "; request ",substr(ans$request_sha256,1L,16L),
          if(accept)"; accepted suggestion" else "; manually edited suggestion"),
        status="draft",parent_version=as.character(ans$prompt_version_id))
      w04_prompt_archive_save(meta,v)
      v
    },error=function(e)e)
    if(inherits(outcome,"error")) {
      w04_refinement_status_rv(paste("Revised prompt was not saved:",conditionMessage(outcome)))
      return()
    }
    w04_refinement_response_rv(NULL)
    w04_refinement_status_rv(paste("Saved immutable new W04 prompt draft",outcome$version_id,
      ". Select this new version explicitly for the NEXT model run. The original prompt is unchanged."))
  }
  observeEvent(input$w04_refinement_accept,save_w04_refined_prompt(TRUE))
  observeEvent(input$w04_refinement_save_edit,save_w04_refined_prompt(FALSE))
  observeEvent(input$w04_refinement_reject,{
    w04_refinement_response_rv(NULL)
    w04_refinement_poll_active_rv(FALSE)
    w04_refinement_status_rv("Suggestion rejected. The original archived prompt remains unchanged.")
  })

  output$w04_model_batch_summary <- renderUI({
    b <- w04_model_selected_batch_rv()
    if(is.null(b)) return(NULL)
    tags$p(class="saved-note",sprintf(
      "Run %s · %s records · %s retain · %s exclude · %s uncertain · prompt %s",
      b$run_id,b$total,b$retain,b$exclude,b$uncertain,
      substr(as.character(b$prompt_sha256),1L,12L)))
  })
  observeEvent(input$w04_reviewer_batches_refresh,{
    req(authenticated())
    if(!session_can("adjudicate_assigned")) {
      w04_reviewer_model_status_rv("Your account does not have permission to review model uncertainties.")
      return()
    }
    w04_reviewer_model_status_rv("Checking completed model runs and reviewer assignments...")
    result <- tryCatch({
      meta <- w04_prompt_review_metadata()
      xs <- w04_model_batch_registry_read(meta$review_id)
      with_uncertainties <- Filter(function(b)
        nzchar(as.character(b$resolution_tab %||% "")) &&
        !is.na(suppressWarnings(as.integer(b$uncertain))) &&
        as.integer(b$uncertain)>0L,xs)
      available <- if(session_can("manage_assignments")) with_uncertainties else Filter(function(b) {
        batch_id <- paste0("w04-resolution-",b$run_id,"-",
          substr(as.character(b$resolution_queue_sha256),1L,12L))
        length(assignments_for_user(assignment_registry_rv(),"04",batch_id,
          session_reviewer_id(),"model_uncertainty"))>0L
      },with_uncertainties)
      list(all=xs,with_uncertainties=with_uncertainties,available=available)
    },error=function(e)e)
    if(inherits(result,"error")) {
      w04_reviewer_model_batches_rv(list())
      w04_reviewer_model_status_rv(paste("Could not load model uncertainty batches:",conditionMessage(result)))
      return()
    }
    w04_reviewer_model_batches_rv(result$available)
    message <- if(!length(result$all)) {
      "No completed W04 model runs have been published to this review spreadsheet. A model run must finish and publish before uncertainty batches appear."
    } else if(!length(result$with_uncertainties)) {
      sprintf("%d completed model run(s) found, but none has uncertain records to adjudicate.",length(result$all))
    } else if(!length(result$available)) {
      sprintf("%d model run(s) with uncertainties exist, but none is assigned to your account. Ask an administrator to assign an uncertainty batch.",length(result$with_uncertainties))
    } else {
      sprintf("%d model uncertainty batch(es) available to load.",length(result$available))
    }
    w04_reviewer_model_status_rv(message)
  })
  output$w04_reviewer_model_batch_selector <- renderUI({
    xs <- w04_reviewer_model_batches_rv()
    if(!length(xs)) return(NULL)
    ids <- vapply(xs,function(b)as.character(b$run_id),character(1))
    labels <- vapply(xs,function(b)sprintf("%s · run %s · %s uncertain",
      b$subset_id,b$run_id,b$uncertain),character(1))
    selectInput("w04_reviewer_model_batch_selected","W04 model run",choices=stats::setNames(ids,labels))
  })
  observeEvent(input$w04_reviewer_model_batch_open,{
    req(authenticated())
    if(!session_can("adjudicate_assigned")) return()
    result <- tryCatch({
      run_id <- as.character(input$w04_reviewer_model_batch_selected %||% "")
      matches <- Filter(function(b)identical(as.character(b$run_id),run_id),w04_reviewer_model_batches_rv())
      if(length(matches)!=1L)stop("Select an available model run",call.=FALSE)
      b <- matches[[1L]]
      if(!nzchar(as.character(b$resolution_tab %||% "")))
        stop("This model batch has no uncertain cases",call.=FALSE)
      q <- read_sheet_w04_resolution_queue(as.character(b$resolution_tab))
      if(is.null(q)||!identical(as.character(q$source_run_id),run_id) ||
         !identical(as.character(q$queue_sha256),as.character(b$resolution_queue_sha256)))
        stop("W04 model uncertainty queue identity mismatch",call.=FALSE)
      if(!session_can("manage_assignments") &&
         !length(assignments_for_user(assignment_registry_rv(),"04",q$batch_id,
           session_reviewer_id(),"model_uncertainty")))
        stop("This batch is not assigned to the current reviewer",call.=FALSE)
      ds <- w04_filter_batch_decisions(active_sheet_w04_resolution_decisions(),q$queue_sha256)
      list(queue=q,decisions=ds)
    },error=function(e)e)
    if(inherits(result,"error")) {
      w04_reviewer_model_status_rv(paste("Cannot load W04 model batch:",conditionMessage(result)))
      return()
    }
    q <- result$queue
    w04_resolution_all_cases_rv(q$cases)
    w04_resolution_queue_sha_rv(q$queue_sha256)
    w04_resolution_batch_id_rv(q$batch_id)
    w04_resolution_source_run_id_rv(q$source_run_id)
    w04_resolution_origin_rv("shiny_model")
    w04_resolution_batch_status_rv(q$batch_status %||% "")
    w04_resolution_decisions(result$decisions)
    w04_resolution_resume_requested_rv(FALSE)
    w04_resolution_idx(1L)
    w04_reviewer_model_status_rv(sprintf("Loaded model run %s: %d uncertainties.",q$source_run_id,length(q$cases)))
  })
  output$w04_finalise_check_status <- renderText(w04_finalise_check_status())
  output$w04_model_status <- renderText(w04_model_status_rv())
  output$w04_model_subset_selector <- renderUI({
    xs <- w04_model_subsets_rv()
    if(!length(xs)) return(tags$span(class="saved-note","No registered subsets loaded."))
    ids <- vapply(xs,function(x)as.character(x$subset_id),character(1))
    labels <- vapply(xs,w04_subset_display,character(1))
    selectInput("w04_model_subset_selected","Registered human-screened subset",
      choices=stats::setNames(ids,labels))
  })
  output$w04_reconstructed_subset_selector <- renderUI({
    xs <- w04_model_subsets_rv()
    if(!length(xs)) return(tags$span(class="saved-note","No registered subsets loaded."))
    ids <- vapply(xs,function(x)as.character(x$subset_id),character(1))
    labels <- vapply(xs,w04_subset_display,character(1))
    selectInput("w04_reconstructed_subsets","Additional registered subsets",
      choices=stats::setNames(ids,labels),multiple=TRUE)
  })
  # Use existing saved human-subset decisions and assignment-aware consensus.
  # A missing queue or an unreadable review fails closed, never inventing coverage.
  w04_registered_human_coded_ids <- function(meta,corpus,subsets) {
    relevant <- Filter(function(s)
      identical(tolower(as.character(s$source_checkpoint_sha256)),
                tolower(as.character(corpus$checkpoint_sha256))),subsets)
    if(!length(relevant)) return(character())
    events <- active_sheet_w04_decisions()
    outcomes <- list()
    for(s in relevant) {
      queue <- w04_subset_queue_read(as.character(s$subset_id),meta$review_id)
      if(is.null(queue)) next
      decisions <- w04_filter_batch_decisions(events,queue$queue_sha256)
      found <- w04_blind_case_outcomes(queue$cases,assignment_registry_rv(),
        decisions,batch_id=paste0("w04-human-",s$subset_id),
        task_type="manual_screening")
      outcomes <- c(outcomes,found)
    }
    w04_human_coded_ids(outcomes,corpus$record_ids)
  }
  # Finalise is read-only until a fully reconciled standard W04 publication
  # route has been verified. Never infer missing coverage from subset membership.
  w04_finalise_check_status <- reactiveVal("Run the coverage check to inspect saved W04 screening.")
  w04_finalise_export_rv <- reactiveVal(NULL)
  output$w04_finalise_export_control <- renderUI({
    if(!isTRUE(session_can("control_workflows")) ||
       !identical(as.character(current_user()$role %||% ""),"administrator") ||
       is.null(w04_finalise_export_rv()))return(NULL)
    downloadButton("w04_finalise_export","Download verified W04 reconciliation JSONL",
                   class="btn-outline-secondary btn-sm")
  })
  output$w04_finalise_export <- downloadHandler(
    filename=function() "workflow04_reconciled_verified.jsonl",
    content=function(file) {
      req(authenticated())
      if(!session_can("control_workflows") ||
         !identical(as.character(current_user()$role),"administrator"))
        stop("Administrator authentication required",call.=FALSE)
      snapshot <- w04_finalise_export_rv()
      if(is.null(snapshot))stop("Repeat the finalisation verification",call.=FALSE)
      # Verify current review and canonical checkpoint before downloading.
      meta <- w04_prompt_review_metadata()
      source <- w04_subset_review_source_pointers()
      if(!identical(as.character(meta$review_id),snapshot$review_id) ||
         !identical(tolower(as.character(source$lean$lean_canonical_sha256)),
                    snapshot$checkpoint_sha256))
        stop("Verified W04 checkpoint changed; repeat coverage check",call.=FALSE)
      rows <- snapshot$rows
      writeLines(vapply(rows,function(z)jsonlite::toJSON(z,auto_unbox=TRUE,
                       null="null",na="null",digits=NA),character(1)),
                 file,useBytes=TRUE)
    })
  w04_finalise_dispatch_busy_rv <- reactiveVal(FALSE)
  w04_finalise_submission_rv <- reactiveVal(NULL)
  output$w04_finalise_submission_status <- renderUI({
    submission <- w04_finalise_submission_rv()
    if(is.null(submission))return(tags$p(class="saved-note",
      "No finalisation has been dispatched in this session."))
    state <- as.character(submission$status %||% "queued")
    url <- as.character(submission$url %||% "")
    div(class=if(state=="completed" && identical(submission$conclusion,"success"))
        "alert alert-success" else if(state=="completed")"alert alert-danger" else
        "alert alert-info",
      tags$strong(if(state=="completed" && identical(submission$conclusion,"success"))
        "W04 publication workflow succeeded." else if(state=="completed")
        "W04 publication workflow failed." else if(nzchar(url))
        "W04 publication workflow is running." else
        "GitHub accepted the request. Waiting for workflow registration."),
      tags$p(paste("Bundle:",submission$bundle)),
      if(nzchar(url))tags$a(href=url,target="_blank",rel="noopener noreferrer",
        "Open W04 publication workflow") else NULL,
      tags$p(class="small",
        if(state=="completed" && identical(submission$conclusion,"success"))
          "W04 workflow succeeded. Confirm the restricted Zenodo receipt separately; W05 remains paused pending approved coding inputs."
        else "This is not confirmation of published Zenodo state or a completed W05 run.")
    )
  })
  observe({
    submission <- w04_finalise_submission_rv()
    if(is.null(submission) || identical(submission$status,"completed"))return()
    shiny::invalidateLater(12000,session)
    result <- tryCatch({
      endpoint <- sprintf("https://api.github.com/repos/%s/actions/workflows/w04_shiny_publish_only.yml/runs?per_page=30",
                          github_dispatch_repo())
      x <- github_api_get_json(endpoint)
      matched <- Filter(function(z)
        grepl(as.character(submission$bundle),as.character(z$display_title %||% ""),
              fixed=TRUE),x$workflow_runs %||% list())
      if(!length(matched))return(NULL)
      matched[[1L]]
    },error=function(e)NULL)
    if(is.null(result))return()
    current <- w04_finalise_submission_rv()
    if(is.null(current)||!identical(as.character(current$bundle),as.character(submission$bundle)))return()
    updated <- list(bundle=submission$bundle,
      status=as.character(result$status %||% "queued"),
      conclusion=as.character(result$conclusion %||% ""),
      url=as.character(result$html_url %||% ""))
    if(!identical(current,updated))w04_finalise_submission_rv(updated)
  })
  output$w04_finalise_publish_reason <- renderText({
    if(!identical(Sys.getenv("LEM_W04_SHINY_PUBLISH_ENABLED",unset="true"),"true"))
      return("Publication is disabled by the Shiny deployment setting LEM_W04_SHINY_PUBLISH_ENABLED. An administrator must enable it.")
    snapshot <- w04_finalise_export_rv()
    if(is.null(snapshot))
      return("Publication is not ready: no verified export bundle is held in this session. Refresh finalisation data, then verify the final screening runs.")
    rows <- snapshot$rows %||% list()
    if(!length(rows))
      return("Publication is not ready: the verified export contains no records.")
    if(isTRUE(w04_finalise_dispatch_busy_rv()))
      return("W04 publication submission is being processed.")
    "Ready to request publication. Click Finalise W04, then confirm in the dialogue."
  })
  output$w04_finalise_publish_control <- renderUI({
    if(!session_can("control_workflows") ||
       !identical(as.character(current_user()$role %||% ""),"administrator"))
      return(NULL)
    ready <- identical(Sys.getenv("LEM_W04_SHINY_PUBLISH_ENABLED",unset="true"),"true") &&
      !is.null(w04_finalise_export_rv()) &&
      !isTRUE(w04_finalise_dispatch_busy_rv())
    actionButton("w04_finalise_publish",
      "Finalise W04 (pause before W05)",
      class="btn-primary",disabled=if(!ready)"disabled" else NULL)
  })
  observeEvent(input$w04_finalise_publish,{
    req(authenticated())
    if(!session_can("control_workflows") ||
       !identical(as.character(current_user()$role %||% ""),"administrator") ||
       isTRUE(w04_finalise_dispatch_busy_rv()) ||
       !identical(Sys.getenv("LEM_W04_SHINY_PUBLISH_ENABLED",unset="true"),"true") ||
       is.null(w04_finalise_export_rv()))return()
    showModal(modalDialog(
      title="Confirm W04 finalisation",
      "Publish the verified W04 screening results to restricted Zenodo? W05 will remain paused until its coding schema is approved. W04 publication cannot be undone from the application.",
      footer=tagList(modalButton("Cancel"),
        actionButton("w04_finalise_confirm_publish","Confirm W04 publication only",class="btn-danger")),
      easyClose=TRUE))
  })
  observeEvent(input$w04_finalise_confirm_publish,{
    removeModal()
    req(authenticated())
    if(!session_can("control_workflows") ||
       !identical(as.character(current_user()$role %||% ""),"administrator") ||
       isTRUE(w04_finalise_dispatch_busy_rv()) ||
       !identical(Sys.getenv("LEM_W04_SHINY_PUBLISH_ENABLED",unset="true"),"true"))return()
    w04_finalise_dispatch_busy_rv(TRUE)
    on.exit(w04_finalise_dispatch_busy_rv(FALSE),add=TRUE)
    showNotification("Submitting verified W04 finalisation to GitHub…",type="message",duration=8)
    result <- tryCatch({
      snapshot <- w04_finalise_export_rv()
      if(is.null(snapshot))stop("Verified reconciliation is no longer current",call.=FALSE)
      meta <- w04_prompt_review_metadata()
      source <- w04_subset_review_source_pointers()
      if(!identical(as.character(meta$review_id),snapshot$review_id) ||
         !identical(tolower(as.character(source$lean$lean_canonical_sha256)),
                    snapshot$checkpoint_sha256))
        stop("Review or canonical checkpoint changed; refresh Finalise",call.=FALSE)
      bundle <- w04_finalise_bundle_build(meta,source,snapshot)
      archived <- w04_shiny_model_bundle_archive(meta,bundle)
      w04_finalise_publication_dispatch(meta,source,archived)
      archived
    },error=function(e)e)
    if(inherits(result,"error")) {
      message <- paste("W04 submission failed:",conditionMessage(result))
      w04_finalise_check_status(message)
      showNotification(message,type="error",duration=NULL)
    } else {
      w04_finalise_submission_rv(list(bundle=as.character(result$filename),
        status="queued",conclusion="",url=""))
      w04_finalise_export_rv(NULL)
      w04_finalise_check_status("GitHub accepted the submission; monitoring publication workflow.")
      showNotification("GitHub accepted the finalisation request. Its status appears below.",type="message",duration=12)
    }
  })
  w04_finalise_prepare_status_rv <- reactiveVal("")
  w04_finalise_last_refresh_rv <- reactiveVal(-1L)
  output$w04_finalise_prepare_status <- renderText(w04_finalise_prepare_status_rv())
  observeEvent(list(input$w04_workspace_tab,input$w04_finalise_prepare),{
    if(!identical(input$w04_workspace_tab,"Finalise")) return()
    req(authenticated())
    if(!session_can("control_workflows") ||
       !identical(as.character(current_user()$role),"administrator"))return()
    refresh <- as.integer(input$w04_finalise_prepare %||% 0L)
    if(length(w04_finalise_selection_batches_rv()) &&
       identical(w04_finalise_last_refresh_rv(),refresh)) return()
    w04_finalise_export_rv(NULL)
    w04_finalise_selection_batches_rv(list())
    result <- tryCatch({
      meta <- w04_prompt_review_metadata()
      source <- w04_subset_review_source_pointers()
      if(!identical(as.character(meta$review_id),as.character(source$review_id)))
        stop("Review-bound source mismatch",call.=FALSE)
      eligible <- w04_subset_load_eligible(source$lean,source$w03)
      list(corpus=c(eligible,list(review_id=meta$review_id)),
           batches=w04_model_batch_registry_read(meta$review_id))
    },error=function(e)e)
    if(inherits(result,"error")) {
      w04_finalise_prepare_status_rv(paste("Cannot load finalisation:",conditionMessage(result)))
      return()
    }
    w04_subset_corpus_rv(result$corpus)
    w04_finalise_selection_batches_rv(result$batches)
    w04_finalise_last_refresh_rv(refresh)
    w04_finalise_prepare_status_rv(sprintf(
      "%d eligible records; %d saved model runs. Select one run per subset. Coverage checks then run automatically.",
      length(result$corpus$record_ids),length(result$batches)))
  })
  w04_finalise_selection_signature <- reactive({
    candidates <- w04_finalise_common_prompt_runs()
    if(!length(candidates))return(NULL)
    selected <- as.character(input$w04_finalise_prompt_sha %||% "")
    if(!selected %in% names(candidates)) {
      scores <- vapply(candidates,function(xs)
        max(vapply(xs,function(x)as.character(x$published_at_utc %||% ""),character(1))),
        character(1))
      selected <- names(candidates)[[order(scores,decreasing=TRUE)[[1L]]]]
    }
    paste(selected,paste(vapply(candidates[[selected]],
      function(x)as.character(x$run_id),character(1)),collapse="|"),sep="|")
  })
  observeEvent(w04_finalise_selection_signature(),{
    w04_finalise_export_rv(NULL)
    req(w04_finalise_selection_signature())
    req(authenticated())
    if(!session_can("control_workflows") ||
       !identical(as.character(current_user()$role),"administrator"))return()
    result <- tryCatch({
      corpus <- w04_subset_corpus_rv()
      if(is.null(corpus))stop("Verify the eligible canonical corpus in Subsets & tasks",call.=FALSE)
      meta <- w04_prompt_review_metadata()
      if(!identical(as.character(meta$review_id),as.character(corpus$review_id)))
        stop("W04 review identity mismatch",call.=FALSE)
      source <- w04_subset_review_source_pointers()
      if(!identical(tolower(as.character(corpus$checkpoint_sha256)),
                    tolower(as.character(source$lean$lean_canonical_sha256))))
        stop("Canonical checkpoint identity mismatch",call.=FALSE)
      subsets <- w04_subset_registry_read(meta$review_id)
      human <- list()
      relevant <- Filter(function(s)
        identical(tolower(as.character(s$source_checkpoint_sha256)),
                  tolower(as.character(corpus$checkpoint_sha256))),subsets)
      if(length(relevant)) {
        events <- active_sheet_w04_decisions()
        for(s in relevant) {
          q <- w04_subset_queue_read(as.character(s$subset_id),meta$review_id)
          if(is.null(q))next
          ds <- w04_filter_batch_decisions(events,q$queue_sha256)
          queue_cases <- q$cases %||% list()
          for(case in queue_cases) {
            case_id <- as.character(case$review_case_id %||% case$case_id %||% "")
            record_id <- as.character(case$record_id %||% "")
            matches <- Filter(function(d)
              identical(as.character(d$review_case_id %||% d$case_id %||% ""),case_id) &&
              identical(as.character(d$record_id %||% ""),record_id),ds)
            by_reviewer <- list()
            for(d in matches) {
              reviewer <- as.character(d$reviewer %||% d$user_id %||% "")
              decision <- as.character(d$decision %||% "")
              if(!nzchar(reviewer) || !decision %in% c("retain","exclude"))next
              if(is.null(by_reviewer[[reviewer]]))by_reviewer[[reviewer]] <- decision
            }
            decisions <- unique(unlist(by_reviewer,use.names=FALSE))
            if(!length(decisions))next
            human[[length(human)+1L]] <- list(record_id=record_id,
              status=if(length(decisions)==1L)"single_human" else "conflict",
              final_decision=if(length(decisions)==1L)decisions[[1L]] else "")
          }
        }
      }
      batches <- w04_selected_batches()
      result_cache <- new.env(parent=emptyenv())
      cached_results <- function(batch) {
        key <- paste0("run_",as.character(batch$run_id))
        if(!exists(key,envir=result_cache,inherits=FALSE))
          assign(key,w04_model_batch_results_read(batch),envir=result_cache)
        get(key,envir=result_cache,inherits=FALSE)
      }
      w04_model_verify_batch_checkpoints(batches,meta$review_id,
        corpus$checkpoint_sha256,subsets,read_results=cached_results)
      resolved <- w04_model_verified_resolution_events(batches)
      coverage <- w04_screening_coverage_reconcile(corpus$record_ids,batches,human,resolved,
        read_results=cached_results)
      export <- if(isTRUE(coverage$ready))
        w04_reconciled_export_rows(coverage,batches,human,read_results=cached_results) else NULL
      list(coverage=coverage,export=export,review_id=meta$review_id,
           selected_model_runs=lapply(batches,function(b)list(
             run_id=as.character(b$run_id),subset_id=as.character(b$subset_id),
             prompt_version_id=as.character(b$prompt_version_id),
             prompt_sha256=as.character(b$prompt_sha256))),
           checkpoint_sha256=tolower(as.character(corpus$checkpoint_sha256)))
    },error=function(e)e)
    if(inherits(result,"error")) {
      w04_finalise_check_status(paste("Coverage check failed:",conditionMessage(result)))
    } else {
      coverage <- result$coverage
      if(isTRUE(coverage$ready))
        w04_finalise_export_rv(list(rows=result$export,
          review_id=result$review_id,selected_model_runs=result$selected_model_runs,
          checkpoint_sha256=result$checkpoint_sha256))
      w04_finalise_check_status(sprintf(
        "Verified: %d eligible, %d resolved; %d missing, %d uncertain, %d conflicts. %s",
        coverage$eligible,coverage$resolved,length(coverage$missing),
        length(coverage$uncertain),length(coverage$conflicts),
        if(coverage$ready)"Ready for explicit publication confirmation. No publication has occurred."
        else "Finalisation blocked until coverage and conflicts are resolved."))
    }
  },ignoreNULL=FALSE)
  observeEvent(input$w04_reconstructed_verify,{
    req(authenticated())
    if(!session_can("control_workflows") ||
       !identical(as.character(current_user()$role),"administrator")) return()
    result <- tryCatch({
      corpus <- w04_subset_corpus_rv()
      if(is.null(corpus)) stop("First verify the eligible corpus in Subsets & tasks",call.=FALSE)
      meta <- w04_prompt_review_metadata()
      if(!identical(as.character(corpus$review_id),as.character(meta$review_id)))
        stop("Loaded corpus belongs to a different review",call.=FALSE)
      subset_ids <- as.character(input$w04_reconstructed_subsets %||% character())
      subsets <- Filter(function(x) as.character(x$subset_id) %in% subset_ids,
        w04_model_subsets_rv())
      if(length(subsets)!=length(unique(subset_ids)))
        stop("Selected subset is unavailable",call.=FALSE)
      if(any(vapply(subsets,function(x)
        !identical(tolower(as.character(x$source_checkpoint_sha256)),
                   tolower(as.character(corpus$checkpoint_sha256))),logical(1))))
        stop("A selected subset refers to a different canonical checkpoint",call.=FALSE)
      runs <- w04_selected_batches()
      coded <- union(w04_model_coded_ids(runs,corpus$record_ids),
        w04_registered_human_coded_ids(meta,corpus,w04_model_subsets_rv()))
      w04_subset_reconstruct_selection(corpus$record_ids,coded,subsets,
        isTRUE(input$w04_reconstructed_include_remaining))
    },error=function(e)e)
    w04_model_status_rv(if(inherits(result,"error"))
      paste("Reconstructed screening verification failed:",conditionMessage(result))
      else sprintf("Verified %d unique canonical records (%d remaining; %d from subsets; %d overlaps removed). SHA-256: %s. Reconstructed selection can be launched using the screening selection control.",
        result$record_count,result$remaining_count,result$selected_subset_count,
        result$duplicate_count,result$membership_sha256))
  })
  output$w04_model_prompt_selector <- renderUI({
    versions <- w04_prompts_by_save_date(w04_model_versions_rv())
    if(!length(versions)) return(tags$span(class="saved-note","No archived W04 prompts loaded."))
    ids <- vapply(versions,function(x)as.character(x$version_id),character(1))
    labels <- w04_prompt_display_labels(versions)
    selectInput("w04_model_prompt_selected","Immutable archived prompt",
      choices=stats::setNames(ids,labels))
  })
  w04_finalise_common_prompt_runs <- reactive({
    batches <- w04_finalise_selection_batches_rv()
    if(!length(batches))return(list())
    subset_ids <- unique(vapply(batches,function(x)as.character(x$subset_id),character(1)))
    hashes <- unique(vapply(batches,function(x)tolower(as.character(x$prompt_sha256 %||% "")),character(1)))
    hashes <- hashes[grepl("^[0-9a-f]{64}$",hashes)]
    candidates <- list()
    for(hash in hashes) {
      same <- Filter(function(x)identical(tolower(as.character(x$prompt_sha256)),hash),batches)
      present <- unique(vapply(same,function(x)as.character(x$subset_id),character(1)))
      if(!setequal(present,subset_ids))next
      chosen <- lapply(subset_ids,function(id) {
        runs <- Filter(function(x)identical(as.character(x$subset_id),id),same)
        dates <- vapply(runs,function(x)as.character(x$published_at_utc %||% ""),character(1))
        ids <- suppressWarnings(as.numeric(vapply(runs,function(x)as.character(x$run_id),character(1))))
        runs[[order(dates,ids,decreasing=TRUE)[[1L]]]]
      })
      candidates[[hash]] <- chosen
    }
    candidates
  })
  output$w04_model_run_choices <- renderUI({
    batches <- w04_finalise_selection_batches_rv()
    if(!length(batches))return(tags$p(class="saved-note","No completed model runs are available."))
    candidates <- w04_finalise_common_prompt_runs()
    if(!length(candidates))return(div(class="alert alert-warning",
      tags$strong("No single prompt was used across all three subsets."),
      tags$p("Finalisation is blocked. The same saved prompt must have completed model runs for every subset. Existing results will not be mixed across different prompts.")))
    hashes <- names(candidates)
    scores <- vapply(candidates,function(xs)
      max(vapply(xs,function(x)as.character(x$published_at_utc %||% ""),character(1))),
      character(1))
    proposed <- hashes[[order(scores,decreasing=TRUE)[[1L]]]]
    selected <- as.character(input$w04_finalise_prompt_sha %||% "")
    if(!selected %in% hashes) selected <- proposed
    runs <- candidates[[selected]]
    labels <- vapply(hashes,function(h) {
      xs <- candidates[[h]]
      paste0("Saved prompt ",substr(as.character(xs[[1L]]$prompt_version_id),1L,22L),
        " (",length(xs)," subsets, latest ",substr(scores[[h]],1L,10L),")")
    },character(1))
    div(class="mb-3",
      tags$h5("One final screening prompt"),
      tags$p(class="saved-note","The app automatically matches the same saved prompt across all three subsets and selects the most recent completed run for each subset. It will never combine different prompts."),
      selectInput("w04_finalise_prompt_sha","Prompt used for the final screening",
        choices=stats::setNames(hashes,labels),selected=selected),
      tags$p(class="saved-note",
        paste0("Automatically matched: ",paste(vapply(runs,function(x)
          paste0(x$total," records (run ",x$run_id,")"),character(1)),collapse="; "))),
      tags$details(tags$summary("View technical provenance"),
        tags$p(paste("Prompt checksum:",selected)),
        tags$ul(lapply(runs,function(x)tags$li(paste0("Run ",x$run_id,
          "; ",x$total," records; ",x$retain," include, ",x$exclude,
          " exclude, ",x$uncertain," uncertain")))))
    )
  })
  w04_selected_batches <- function() {
    candidates <- w04_finalise_common_prompt_runs()
    if(!length(candidates))stop("No prompt has completed runs covering all subsets",call.=FALSE)
    hashes <- names(candidates)
    chosen <- as.character(input$w04_finalise_prompt_sha %||% "")
    if(!chosen %in% hashes) {
      scores <- vapply(candidates,function(xs)
        max(vapply(xs,function(x)as.character(x$published_at_utc %||% ""),character(1))),
        character(1))
      chosen <- hashes[[order(scores,decreasing=TRUE)[[1L]]]]
    }
    selected <- candidates[[chosen]]
    if(length(unique(vapply(selected,function(x)
      tolower(as.character(x$prompt_sha256)),character(1))))!=1L)
      stop("Selected model runs do not share the identical final prompt",call.=FALSE)
    selected
  }
  output$w04_model_launch_control <- renderUI({
    ready <- !is.null(w04_subset_corpus_rv()) &&
      length(w04_model_versions_rv())>0L &&
      !isTRUE(w04_model_launch_busy_rv())
    if(!ready)
      return(tags$p(class="saved-note","Prepare screening to enable model execution. No screening will run during preparation."))
    actionButton("w04_model_launch","2. Run selected W04 model screening",
      class="btn-primary btn-sm",
      onclick="window.lemBusyStart('w04_model_launch','Validating and archiving W04 inputs, then dispatching GitHub…')")
  })
  observeEvent(input$w04_model_refresh,{
    on.exit(session$sendCustomMessage("lem_busy_finished",list(id="w04_model_refresh")),add=TRUE)
    req(authenticated())
    if(!session_can("control_workflows")) return()
    result <- tryCatch({
      meta <- w04_prompt_review_metadata()
      source <- w04_subset_review_source_pointers()
      eligible <- w04_subset_load_eligible(source$lean,source$w03)
      if(!identical(as.character(source$review_id),as.character(meta$review_id)))
        stop("Review identity mismatch",call.=FALSE)
      list(subsets=w04_subset_registry_read(meta$review_id),
           batches=w04_model_batch_registry_read(meta$review_id),
           versions=w04_prompt_archive_versions(meta),
           corpus=c(eligible,list(review_id=meta$review_id)))
    },error=function(e)e)
    if(inherits(result,"error")) {
      w04_model_status_rv(paste("Could not load model input options:",conditionMessage(result)))
      return()
    }
    w04_subset_corpus_rv(result$corpus)
    w04_model_subsets_rv(result$subsets)
    w04_finalise_selection_batches_rv(result$batches)
    w04_model_versions_rv(result$versions)
    w04_model_status_rv(sprintf("Prepared %d eligible canonical records, %d registered subsets and %d archived prompts. Choose the screening selection and press Run; all IDs and decisions will be checked again before launch.",
      length(result$corpus$record_ids),length(result$subsets),length(result$versions)))
  })
  observeEvent(input$w04_model_launch,{
    on.exit(session$sendCustomMessage("lem_busy_finished",list(id="w04_model_launch")),add=TRUE)
    req(authenticated())
    if(!session_can("control_workflows") ||
       !identical(as.character(current_user()$role),"administrator")) {
      w04_model_status_rv("Only an administrator may launch W04 model screening.")
      return()
    }
    if(isTRUE(w04_model_launch_busy_rv())) return()
    w04_model_launch_busy_rv(TRUE)
    on.exit(w04_model_launch_busy_rv(FALSE),add=TRUE)
    result <- tryCatch({
      meta <- w04_prompt_review_metadata()
      launch_reconstructed <- identical(as.character(input$w04_model_launch_mode %||% "registered"),"reconstructed")
      subset_id <- as.character(input$w04_model_subset_selected %||% "")
      version_id <- as.character(input$w04_model_prompt_selected %||% "")
      subsets <- w04_model_subsets_rv()
      versions <- w04_model_versions_rv()
      selected <- Filter(function(x)identical(as.character(x$subset_id),subset_id),subsets)
      prompt <- Filter(function(x)identical(as.character(x$version_id),version_id),versions)
      if(length(prompt)!=1L || (!launch_reconstructed && length(selected)!=1L))
        stop("Choose a valid model selection and immutable prompt version",call.=FALSE)
      source <- w04_subset_review_source_pointers()
      if(launch_reconstructed) {
        corpus <- w04_subset_corpus_rv()
        if(is.null(corpus) || !identical(as.character(corpus$review_id),as.character(meta$review_id)) ||
           !identical(tolower(as.character(corpus$checkpoint_sha256)),
             tolower(as.character(source$lean$lean_canonical_sha256))))
          stop("Verify the current eligible canonical corpus before launching",call.=FALSE)
        subset_ids <- as.character(input$w04_reconstructed_subsets %||% character())
        chosen <- Filter(function(x)as.character(x$subset_id) %in% subset_ids,subsets)
        if(length(chosen)!=length(unique(subset_ids)))stop("Registered subset selection is unavailable",call.=FALSE)
        if(any(vapply(chosen,function(x)
          !identical(tolower(as.character(x$source_checkpoint_sha256)),
                     tolower(as.character(corpus$checkpoint_sha256))),logical(1))))
          stop("Registered subset uses a different canonical checkpoint",call.=FALSE)
        batches <- w04_selected_batches()
        coded <- union(w04_model_coded_ids(batches,corpus$record_ids),
          w04_registered_human_coded_ids(meta,corpus,subsets))
        selection <- w04_subset_reconstruct_selection(corpus$record_ids,coded,chosen,
          isTRUE(input$w04_reconstructed_include_remaining))
        bundle <- w04_shiny_model_selection_bundle_build(meta,selection,prompt[[1L]],source)
        subset <- list(subset_id=bundle$subset_id,source_checkpoint_sha256=bundle$source_checkpoint_sha256)
      } else {
        subset <- selected[[1L]]
        queue <- w04_subset_queue_read(subset_id,meta$review_id)
        if(is.null(queue)) stop("Registered subset screening queue is missing",call.=FALSE)
        ds <- w04_filter_batch_decisions(active_sheet_w04_decisions(),queue$queue_sha256)
        edits <- active_sheet_w04_manual_abstract_edits(queue$queue_sha256)
        bundle <- w04_shiny_model_bundle_build(meta,subset,queue,prompt[[1L]],ds,edits)
      }
      if(!identical(tolower(as.character(source$lean$lean_canonical_sha256)),
                    tolower(as.character(bundle$source_checkpoint_sha256))))
        stop("Registered subset does not match the review-bound W03 source",call.=FALSE)
      archived <- w04_shiny_model_bundle_archive(meta,bundle)
      w04_shiny_model_dispatch(meta,source,subset,archived)
      archived
    },error=function(e)e)
    if(inherits(result,"error")) {
      w04_model_status_rv(paste("W04 model launch was not confirmed:",conditionMessage(result)))
      return()
    }
    w04_model_status_rv(paste("Dispatched W04 model screening to GitHub. Archived input:",
      result$filename,"· SHA-256",result$sha256,
      "· No full-review finalisation or automatic W05 hand-off."))
  })

  # Delay construction of the large configuration tree until an administrator
  # explicitly opens it. Do not delay user identity, logout, or the task page.
  configure_review_requested <- reactiveVal(FALSE)
  observeEvent(input$open_configure_review,{
    req(authenticated())
    if(identical(as.character(current_user()$role %||% ""),"administrator"))
      configure_review_requested(TRUE)
  })
  observeEvent(authenticated(),{
    if(!isTRUE(authenticated())) configure_review_requested(FALSE)
  })

  # W05 is configured independently and never dispatches the existing salmon worker.
  observeEvent(configure_review_requested(),{
    if(!isTRUE(configure_review_requested()) || !authenticated())return()
    if(is.null(w05_crop_dictionary_rv())) {
      example <- tryCatch(w05_crop_read("config/w05_almeria_crop_dictionary.csv"),
                          error=function(e)e)
      if(!inherits(example,"error")) {
        w05_crop_dictionary_rv(example)
        w05_crop_dictionary_path_rv("config/w05_almeria_crop_dictionary.csv")
      } else w05_crop_status_rv(paste("Cannot load draft vocabulary:",conditionMessage(example)))
    }
  })
  output$w05_dictionary_approval_status <- renderText({
    receipt <- w05_dictionary_receipt_rv()
    if(is.null(receipt))return("Displayed schema is ready for approval. No CSV upload is needed unless you want to replace it.")
    if(identical(w05_dictionary_approved_sha_rv(),as.character(receipt$sha256)))
      return(paste("Approved dictionary version:",receipt$sha256,
                   "| Restricted Zenodo draft:",receipt$deposit_id))
    paste("Awaiting administrator approval of archived dictionary version:",receipt$sha256)
  })
  output$w05_dictionary_approve_control <- renderUI({
    if(!session_can("control_workflows") ||
       !identical(as.character(current_user()$role %||% ""),"administrator"))return(NULL)
    receipt <- w05_dictionary_receipt_rv()
    if(!is.null(receipt) && identical(w05_dictionary_approved_sha_rv(),as.character(receipt$sha256)))return(NULL)
    actionButton("w05_dictionary_approve","Approve displayed W05 coding schema",class="btn-primary")
  })
  observeEvent(input$w05_dictionary_approve,{
    req(authenticated())
    if(!session_can("control_workflows") ||
       !identical(as.character(current_user()$role %||% ""),"administrator"))return()
    saved <- tryCatch({
      path <- w05_crop_dictionary_path_rv()
      if(length(path)!=1L || !nzchar(path) || !file.exists(path))
        stop("No displayed CSV dictionary source is available",call.=FALSE)
      current <- w05_crop_dictionary_rv()
      if(is.null(current))stop("No displayed dictionary to approve",call.=FALSE)
      disk <- w05_crop_read(path)
      if(!identical(current,disk))
        stop("Displayed dictionary differs from its source CSV; refresh the tab",call.=FALSE)
      meta <- w04_prompt_review_metadata()
      receipt <- w05_crop_archive_upload(meta,path)
      w05_crop_approve_version(w04_prompt_review_metadata(force=TRUE),receipt)
      receipt
    },error=function(e)e)
    if(inherits(saved,"error")) {
      w05_crop_status_rv(paste("W05 approval could not be verified:",conditionMessage(saved)))
      return()
    }
    w05_verified_input_rv(NULL)
    w05_dictionary_receipt_rv(saved)
    w05_dictionary_approved_sha_rv(as.character(saved$sha256))
    w05_crop_status_rv("Displayed W05 schema approved and version archived in restricted Zenodo. You can now verify and prepare the published W04 records.")
  })
  observeEvent(input$w05_shiny_prepare_input,{
    req(authenticated())
    if(!session_can("control_workflows") ||
       !identical(as.character(current_user()$role %||% ""),"administrator"))return()
    w05_verified_input_rv(NULL)
    w05_shiny_subset_launch_status_rv("Verifying review-bound W04 publication and restoring its included records.")
    stage <- "begin preparation"
    result <- tryCatch({
      stage <- "load review metadata and approved dictionary"
      meta <- w04_prompt_review_metadata()
      dictionary <- w05_dictionary_receipt_rv()
      if(is.null(dictionary) ||
         !identical(w05_dictionary_approved_sha_rv(),as.character(dictionary$sha256)))
        stop("Approve the archived W05 dictionary first",call.=FALSE)
      run_id <- trimws(as.character(input$w05_w04_publication_run %||% ""))
      if(!nzchar(run_id)) {
        endpoint <- sprintf(
          "https://api.github.com/repos/%s/actions/runs?per_page=100",
          github_dispatch_repo())
        runs <- github_api_get_json(endpoint)$workflow_runs %||% list()
        found <- w05_shiny_discover_w04_publication(meta$review_id,runs)
        if(is.null(found))
          stop("No completed W04-only publication found for this review in the recent Actions history. Finalise W04 first, or enter its specific successful run ID.",call.=FALSE)
        run_id <- as.character(found$id)
      }
      if(!grepl("^[0-9]+$",run_id))
        stop("Invalid W04 Shiny publication run ID",call.=FALSE)
      stage <- "resolve review-specific W03 and canonical pointers"
      source <- w04_subset_review_source_pointers()
      stage <- "read the published W04 pointer from GitHub"
      pointer_path <- sprintf("docs/workflow04/zenodo/run-%s.json",run_id)
      pointer <- jsonlite::fromJSON(read_github_text_file(pointer_path),
                                    simplifyVector=FALSE)
      stage <- "verify W04 GitHub publication run"
      run <- github_api_get_json(sprintf(
        "https://api.github.com/repos/%s/actions/runs/%s",
        github_dispatch_repo(),run_id))
      stage <- "download and verify W02 canonical and W04 included-ID archives"
      restored <- w05_shiny_restore_published_sources(
        source,pointer,meta$review_id,run)
      on.exit(unlink(restored$directory,recursive=TRUE),add=TRUE)
      stage <- "materialise included records and archive W05 inputs"
      receipt <- w05_shiny_prepare_handover(meta,restored$canonical,
        restored$included_ids,restored$receipt,dictionary)
      if(!identical(as.character(receipt$deposit_id),
                    as.character(dictionary$deposit_id)))
        stop("W05 input archive and approved dictionary drafts differ",call.=FALSE)
      list(input=receipt,dictionary=dictionary,review_id=meta$review_id,
           publication_run_id=run_id)
    },error=function(e)e)
    if(inherits(result,"error")) {
      w05_shiny_subset_launch_status_rv(
        paste("W05 input preparation failed during",stage,":",conditionMessage(result)))
      return()
    }
    w05_verified_input_rv(result)
    w05_shiny_subset_launch_status_rv(sprintf(
      "Verified %d W04-included records in restricted W05 draft %s. Ready for an experimental subset.",
      result$input$record_count,result$input$deposit_id))
  })
  observeEvent(input$w05_shiny_subset_launch,{
    req(authenticated())
    if(!session_can("control_workflows") ||
       !identical(as.character(current_user()$role %||% ""),"administrator"))return()
    result <- tryCatch({
      prepared <- w05_verified_input_rv()
      dictionary <- w05_dictionary_receipt_rv()
      meta <- w04_prompt_review_metadata()
      if(is.null(prepared) || is.null(dictionary) ||
         !identical(as.character(prepared$review_id),as.character(meta$review_id)) ||
         !identical(prepared$dictionary,dictionary) ||
         !identical(w05_dictionary_approved_sha_rv(),as.character(dictionary$sha256)))
        stop("Verified W04 input and approved coding schema are required",call.=FALSE)
      n <- suppressWarnings(as.integer(input$w05_shiny_subset_size))
      if(is.na(n)||n<1L||n>as.integer(prepared$input$record_count))
        stop("Subset size exceeds the verified included-record corpus",call.=FALSE)
      request <- list(review_id=meta$review_id,
        zenodo_draft_id=prepared$input$deposit_id,
        input_filename=prepared$input$filename,
        input_sha256=prepared$input$sha256,
        dictionary_filename=dictionary$filename,
        dictionary_sha256=dictionary$sha256,
        subset_count=as.character(n))
      w05_shiny_subset_dispatch(request,w04_published=TRUE,
                                schema_approved=TRUE)
      n
    },error=function(e)e)
    if(inherits(result,"error")) {
      w05_shiny_subset_launch_status_rv(
        paste("W05 subset dispatch not confirmed:",conditionMessage(result)))
      return()
    }
    w05_shiny_subset_launch_status_rv(sprintf(
      "GitHub accepted an isolated W05 subset request for %d records. Check its completed run before interpreting results.",result))
  })
  w07_config_receipts_rv <- reactiveVal(list())
  w07_config_status_rv <- reactiveVal("Loading saved W07 configuration when W07 is opened.")
  w07_approval_status_rv <- reactiveVal(list())
  w07_preferred_versions_rv <- reactiveVal(list())
  w07_version_details_rv <- reactiveVal(list())
  w07_versions_rv <- reactiveVal(data.frame(type=character(),sha256=character(),
                                             approved=logical()))
  refresh_w07_versions <- function(meta=w04_prompt_review_metadata()) {
    versions <- w07_shiny_config_versions(meta)
    details <- list()
    if(nrow(versions)) {
      for(i in seq_len(nrow(versions))) {
        type <- versions$type[[i]]
        sha <- versions$sha256[[i]]
        receipt <- tryCatch(w07_shiny_config_select(meta,type,sha),error=function(e)e)
        if(!inherits(receipt,"error"))details[[paste(type,sha,sep=":")]] <- receipt
      }
    }
    w07_version_details_rv(details)
    w07_versions_rv(versions)
    versions
  }
  output$w07_saved_versions <- renderUI({
    versions <- w07_versions_rv()
    if(!nrow(versions))return(tags$p("No saved W07 versions found in this review."))
    tags$div(lapply(c("ontology","prompt"),function(type) {
      choices <- versions[versions$type==type,,drop=FALSE]
      if(!nrow(choices))return(tags$p(paste("No saved",type,"versions.")))
      tags$div(class="mb-3",
        tags$strong(paste("Saved",type,"versions")),
        selectInput(paste0("w07_saved_",type),NULL,
          choices=setNames(choices$sha256,
            vapply(seq_len(nrow(choices)),function(i) {
              sha <- choices$sha256[[i]]
              receipt <- w07_version_details_rv()[[paste(type,sha,sep=":")]]
              summary <- if(is.null(receipt)) "Archived version" else
                if(identical(type,"ontology")) paste0(receipt$count," categories") else
                  substr(gsub("[[:space:]]+"," ",receipt$content),1L,75L)
              paste0(substr(sha,1L,12L)," | ",summary)
            },character(1))),
          selected={
            preferred <- w07_preferred_versions_rv()[[type]]
            if(!is.null(preferred) && preferred %in% choices$sha256) preferred
            else if(any(choices$approved))choices$sha256[which(choices$approved)[1L]]
            else choices$sha256[1L]
          }))
    }))
  })
  observeEvent(input$w07_select_saved,{
    req(authenticated())
    type <- as.character(input$w07_config_type %||% "ontology")
    if(!type %in% c("ontology","prompt"))return()
    sha <- input[[paste0("w07_saved_",type)]]
    if(is.null(sha)||!nzchar(sha))return()
    result <- tryCatch(w07_shiny_config_select(w04_prompt_review_metadata(),type,sha),
                       error=function(e)e)
    if(inherits(result,"error")) {
      w07_config_status_rv(paste("Could not load selected version:",conditionMessage(result)))
      return()
    }
    receipts <- w07_config_receipts_rv()
    receipts[[type]] <- result
    w07_config_receipts_rv(receipts)
    versions <- w07_versions_rv()
    is_approved <- any(versions$type==type & versions$sha256==sha & versions$approved)
    approvals <- w07_approval_status_rv()
    approvals[[type]] <- if(is_approved)sha else NULL
    w07_approval_status_rv(approvals)
    w07_config_status_rv(paste("Selected saved",type,"version",substr(sha,1L,12L),
      if(is_approved)"(approved)." else "(approval required)."))
  })

  observeEvent(input$configure_workflow_tab,{
    req(authenticated())
    if(!identical(input$configure_workflow_tab,"W07"))return()
    meta <- tryCatch(w04_prompt_review_metadata(),error=function(e)e)
    result <- if(inherits(meta,"error"))meta else
      tryCatch(w07_shiny_config_restore(meta),error=function(e)e)
    if(inherits(result,"error")) {
      w07_config_status_rv(paste("Could not restore saved W07 configuration:",
                                 conditionMessage(result)))
      return()
    }
    versions <- tryCatch(refresh_w07_versions(meta),error=function(e)e)
    if(inherits(versions,"error")) {
      w07_config_status_rv(paste("Could not list saved W07 versions:",conditionMessage(versions)))
      return()
    }
    w07_config_receipts_rv(result$receipts)
    w07_approval_status_rv(result$approved)
    statuses <- vapply(c("ontology","prompt"),function(type) {
      r <- result$receipts[[type]]
      if(is.null(r))return(paste(type,"not uploaded"))
      if(identical(result$approved[[type]],r$sha256))
        return(paste(type,"saved and approved"))
      paste(type,"saved; approval required")
    },character(1))
    w07_config_status_rv(paste("Restored from restricted Zenodo:",
                               paste(statuses,collapse="; ")))
  })
  observeEvent(input$w07_ontology_upload,{
    req(authenticated())
    if(!session_can("control_workflows"))return()
    uploaded <- input$w07_ontology_upload
    result <- tryCatch({
      if(!identical(tolower(tools::file_ext(uploaded$name)),"csv"))
        stop("W07 ontology must be a CSV",call.=FALSE)
      w07_shiny_config_archive(w04_prompt_review_metadata(),
        uploaded$datapath,"ontology")
    },error=function(e)e)
    if(inherits(result,"error")) {
      w07_config_status_rv(paste("Ontology upload failed:",conditionMessage(result)))
    } else {
      receipts <- w07_config_receipts_rv();receipts$ontology <- result
      w07_config_receipts_rv(receipts)
      preferred <- w07_preferred_versions_rv()
      preferred$ontology <- result$sha256
      w07_preferred_versions_rv(preferred)
      tryCatch(refresh_w07_versions(),error=function(e)
        w07_config_status_rv(paste("Saved W07 versions could not be refreshed:",conditionMessage(e))))
      approved <- w07_approval_status_rv()
      if(!identical(approved$ontology,result$sha256)) {
        approved$ontology <- NULL
        w07_approval_status_rv(approved)
      }
      w07_config_status_rv(paste("W07 ontology archived:",result$count,
        "codes. Review and approve this version."))
    }
  })
  observeEvent(input$w07_prompt_upload,{
    req(authenticated())
    if(!session_can("control_workflows"))return()
    uploaded <- input$w07_prompt_upload
    result <- tryCatch({
      if(!tolower(tools::file_ext(uploaded$name)) %in% c("txt","md","docx","pdf"))
        stop("W07 model prompt must be TXT, Markdown, DOCX or PDF",call.=FALSE)
      w07_shiny_config_archive(w04_prompt_review_metadata(),
        uploaded$datapath,"prompt",uploaded$name)
    },error=function(e)e)
    if(inherits(result,"error")) {
      w07_config_status_rv(paste("Prompt upload failed:",conditionMessage(result)))
    } else {
      receipts <- w07_config_receipts_rv();receipts$prompt <- result
      w07_config_receipts_rv(receipts)
      preferred <- w07_preferred_versions_rv()
      preferred$prompt <- result$sha256
      w07_preferred_versions_rv(preferred)
      tryCatch(refresh_w07_versions(),error=function(e)
        w07_config_status_rv(paste("Saved W07 versions could not be refreshed:",conditionMessage(e))))
      approved <- w07_approval_status_rv()
      if(!identical(approved$prompt,result$sha256)) {
        approved$prompt <- NULL
        w07_approval_status_rv(approved)
      }
      w07_config_status_rv("W07 model prompt archived. Review and approve this version.")
    }
  })
  observeEvent(input$w07_config_approve,{
    req(authenticated())
    type <- as.character(input$w07_config_type %||% "")
    if(!type %in% c("ontology","prompt"))return()
    if(!session_can("control_workflows")) {
      w07_config_status_rv("W07 approval unavailable: insufficient permissions.")
      return()
    }
    sha <- as.character(input[[paste0("w07_saved_",type)]] %||% "")
    if(length(sha)!=1L || !grepl("^[a-f0-9]{64}$",sha)) {
      w07_config_status_rv(paste("Select a saved",type,"version to approve."))
      return()
    }
    w07_config_status_rv(paste("Verifying and approving selected W07",type,
                               "version",substr(sha,1L,12L),"..."))
    result <- tryCatch({
      receipt <- w07_shiny_config_select(w04_prompt_review_metadata(),type,sha)
      w07_shiny_config_approve(w04_prompt_review_metadata(),receipt)
      receipt
    },error=function(e)e)
    if(inherits(result,"error")) {
      w07_config_status_rv(paste("W07 approval failed for",type,
                                substr(sha,1L,12L),":",conditionMessage(result)))
    } else {
      receipts <- w07_config_receipts_rv()
      receipts[[type]] <- result
      w07_config_receipts_rv(receipts)
      preferred <- w07_preferred_versions_rv()
      preferred[[type]] <- sha
      w07_preferred_versions_rv(preferred)
      approvals <- w07_approval_status_rv()
      approvals[[type]] <- sha
      w07_approval_status_rv(approvals)
      try(refresh_w07_versions(),silent=TRUE)
      w07_config_status_rv(paste("W07",type,"approved and verified in Zenodo. SHA-256:",sha))
      shiny::showNotification(paste("W07",type,substr(sha,1L,12L),
                                    "approval confirmed"),type="message",duration=8)
    }
  })
  output$w07_approval_summary <- renderText({
    receipts <- w07_config_receipts_rv()
    approved <- w07_approval_status_rv()
    paste(vapply(c("ontology","prompt"),function(type) {
      r <- receipts[[type]]
      if(is.null(r))return(paste(type,": not uploaded"))
      if(identical(approved[[type]],r$sha256))
        return(paste(type,": approved (",substr(r$sha256,1L,12L),")",sep=""))
      paste(type,": approval required (",substr(r$sha256,1L,12L),")",sep="")
    },character(1)),collapse=" | ")
  })
  output$w07_approval_target <- renderText({
    type <- as.character(input$w07_config_type %||% "ontology")
    sha <- input[[paste0("w07_saved_",type)]]
    if(is.null(sha)||!nzchar(sha))return(paste("No saved",type,"version selected."))
    paste("Approval target:",type,substr(sha,1L,12L))
  })
  output$w07_config_status <- renderText(w07_config_status_rv())
  output$w07_config_preview <- renderText({
    type <- as.character(input$w07_config_type %||% "ontology")
    r <- w07_config_receipts_rv()[[type]]
    if(is.null(r))return("No uploaded version for this selection.")
    paste0("SHA-256: ",r$sha256,"\n\n",r$content)
  })
  output$w07_ontology_preview <- renderText({
    sha <- input$w07_saved_ontology
    r <- w07_version_details_rv()[[paste("ontology",sha,sep=":")]]
    if(is.null(r))return("No saved ontology selected.")
    paste0("Version: ",r$sha256,"\n\n",r$content)
  })
  output$w07_prompt_preview <- renderText({
    sha <- input$w07_saved_prompt
    r <- w07_version_details_rv()[[paste("prompt",sha,sep=":")]]
    if(is.null(r))return("No saved prompt selected.")
    paste0("Version: ",r$sha256,"\n\n",r$content)
  })
  w07_trial_status_rv <- reactiveVal("No W07 model trial has been requested.")
  observeEvent(input$w07_trial_launch,{
    req(authenticated())
    if(!session_can("control_workflows"))return()
    result <- tryCatch({
      meta <- w04_prompt_review_metadata()
      ontology <- w07_shiny_config_select(meta,"ontology",input$w07_saved_ontology)
      prompt <- w07_shiny_config_select(meta,"prompt",input$w07_saved_prompt)
      w07_shiny_subset_dispatch(meta,ontology,prompt,input$w07_trial_size)
    },error=function(e)e)
    w07_trial_status_rv(if(inherits(result,"error"))
      paste("W07 trial was not dispatched:",conditionMessage(result)) else
      "W07 experimental trial dispatched. Review results in the isolated W07 GitHub Actions workflow; no final classification has been published.")
  })
  output$w07_trial_status <- renderText(w07_trial_status_rv())
  w07_results_rv <- reactiveVal(NULL)
  w07_results_status_rv <- reactiveVal("No W07 trial results loaded.")
  observeEvent(input$w07_results_load,{
    req(authenticated())
    run_id <- trimws(as.character(input$w07_results_run %||% ""))
    result <- tryCatch(w07_shiny_load_results(run_id),error=function(e)e)
    if(inherits(result,"error")) {
      w07_results_rv(NULL)
      w07_results_status_rv(paste("W07 results could not be loaded:",conditionMessage(result)))
    } else {
      w07_results_rv(result)
      w07_results_status_rv(paste("Loaded",nrow(result$table),
        "W07 records. These are experimental results, not final classifications."))
    }
  })
  output$w07_results_status <- renderText(w07_results_status_rv())
  output$w07_results_table <- renderTable({
    result <- w07_results_rv()
    if(is.null(result))return(NULL)
    result$table
  },striped=TRUE,bordered=FALSE,spacing="s")
  output$w07_results_csv <- downloadHandler(
    filename=function()paste0("w07-experimental-",Sys.Date(),".csv"),
    content=function(file) {
      result <- w07_results_rv()
      req(!is.null(result))
      utils::write.csv(result$table,file,row.names=FALSE,na="")
    }
  )

  w07_prepare_status_rv <- reactiveVal("W07 preparation has not been requested.")
  observeEvent(input$w07_shiny_prepare,{
    req(authenticated())
    if(!session_can("control_workflows") ||
       !identical(as.character(current_user()$role %||% ""),"administrator"))return()
    result <- tryCatch(w07_shiny_prepare_dispatch(w04_prompt_review_metadata()),
                       error=function(e)e)
    w07_prepare_status_rv(if(inherits(result,"error"))
      paste("W07 preparation not dispatched:",conditionMessage(result)) else
      "W07 read-only W05 handover dispatched. Find the completed run in W07 Shiny prepare Actions; no LLM or Zenodo publication occurs.")
  })
  output$w07_shiny_prepare_status <- renderText(w07_prepare_status_rv())
  observeEvent(input$w06_shiny_prompt_upload,{
    req(authenticated())
    if(!session_can("control_workflows") ||
       !identical(as.character(current_user()$role %||% ""),"administrator"))return()
    w06_prompt_approved_rv(FALSE)
    w06_prompt_receipt_rv(NULL)
    w06_prompt_preview_rv("")
    uploaded <- input$w06_shiny_prompt_upload
    result <- tryCatch({
      if(is.null(uploaded))stop("Upload a W06 prompt",call.=FALSE)
      content <- w06_shiny_import_prompt(uploaded$datapath,uploaded$name)
      if(!nzchar(trimws(content)))stop("Extracted W06 prompt is empty",call.=FALSE)
      w06_prompt_preview_rv(content)
      prompt_path <- tempfile(fileext=".txt")
      on.exit(unlink(prompt_path),add=TRUE)
      writeLines(content,prompt_path,useBytes=TRUE)
      meta <- w04_prompt_review_metadata()
      w06_shiny_prompt_save(meta,prompt_path)
    },error=function(e)e)
    if(inherits(result,"error")) {
      w06_prompt_status_rv(paste("W06 prompt not archived:",conditionMessage(result)))
      return()
    }
    w06_prompt_receipt_rv(result)
    w06_prompt_status_rv(sprintf("Archived W06 prompt %s in restricted Zenodo draft %s. Awaiting explicit approval.",
      substr(result$sha256,1L,16L),result$draft_id))
  })
  observeEvent(input$w06_shiny_prompt_approve,{
    req(authenticated())
    if(!session_can("control_workflows") ||
       !identical(as.character(current_user()$role %||% ""),"administrator"))return()
    result <- tryCatch({
      receipt <- w06_prompt_receipt_rv()
      if(is.null(receipt))stop("Upload and archive the W06 prompt first",call.=FALSE)
      w06_shiny_prompt_approve(w04_prompt_review_metadata(),receipt)
      receipt
    },error=function(e)e)
    if(inherits(result,"error")) {
      w06_prompt_status_rv(paste("W06 prompt approval failed:",conditionMessage(result)))
      return()
    }
    w06_prompt_approved_rv(TRUE)
    w06_prompt_status_rv(sprintf("W06 prompt %s approved and archived. W06 model execution remains paused.",
      substr(result$sha256,1L,16L)))
  })
  observeEvent(input$w06_shiny_subset_launch,{
    req(authenticated())
    if(!session_can("control_workflows") ||
       !identical(as.character(current_user()$role %||% ""),"administrator"))return()
    result <- tryCatch({
      if(!isTRUE(w06_prompt_approved_rv()))
        stop("Approve the archived W06 prompt before running a subset",call.=FALSE)
      meta <- w04_prompt_review_metadata()
      receipt <- w06_prompt_receipt_rv()
      n <- suppressWarnings(as.integer(input$w06_shiny_subset_size))
      if(is.na(n)||n<1L)stop("Specify a positive subset size",call.=FALSE)
      w06_shiny_subset_dispatch(meta,receipt,as.character(n))
      n
    },error=function(e)e)
    w06_trial_status_rv(if(inherits(result,"error"))
      paste("W06 subset dispatch not confirmed:",conditionMessage(result)) else
      sprintf("Dispatched isolated W06 model trial on %d records. Retrieve results using its completed GitHub Actions run ID.",result))
  })
  observeEvent(input$w06_shiny_load_results,{
    req(authenticated())
    if(!session_can("control_workflows") ||
       !identical(as.character(current_user()$role %||% ""),"administrator"))return()
    run <- trimws(as.character(input$w06_shiny_run_id %||% ""))
    result <- tryCatch(w06_shiny_subset_results(run),error=function(e)e)
    if(inherits(result,"error")) {
      w06_trial_status_rv(paste("W06 results retrieval failed:",conditionMessage(result)))
      return()
    }
    w06_results_rv(result$results)
    w06_trial_status_rv(sprintf("Verified W06 subset: %s records, %s review needed. Prompt SHA %s. Results are experimental.",
      result$manifest$subset_records,result$manifest$needs_review,
      substr(result$manifest$prompt_sha256,1L,16L)))
  })
  output$w06_shiny_trial_status <- renderText(w06_trial_status_rv())
  output$w06_shiny_trial_table <- renderTable({
    x <- w06_results_rv()
    if(is.null(x))return(NULL)
    x
  },striped=TRUE,spacing="xs",width="100%")
  output$w06_shiny_trial_download <- downloadHandler(
    filename=function()"w06_geography_subset_review.csv",
    content=function(file){
      req(authenticated())
      x <- w06_results_rv()
      req(!is.null(x))
      utils::write.csv(x,file,row.names=FALSE,na="")
    },contentType="text/csv")
  output$w06_shiny_prompt_approval_control <- renderUI({
    receipt <- w06_prompt_receipt_rv()
    if(is.null(receipt) || !nzchar(as.character(receipt$sha256 %||% "")) ||
       !nzchar(as.character(receipt$draft_id %||% "")) ||
       isTRUE(w06_prompt_approved_rv())) return(NULL)
    actionButton("w06_shiny_prompt_approve","Approve archived W06 prompt",
                 class="btn-outline-primary")
  })
  output$w06_shiny_prompt_status <- renderText(w06_prompt_status_rv())
  output$w06_shiny_prompt_preview <- renderText({
    content <- w06_prompt_preview_rv()
    if(!nzchar(content))return("No W06 prompt text extracted this session.")
    content
  })
  observeEvent(input$w05_shiny_finalise,{
    req(authenticated())
    if(!session_can("control_workflows") ||
       !identical(as.character(current_user()$role %||% ""),"administrator"))return()
    result <- tryCatch({
      prepared <- w05_verified_input_rv()
      dictionary <- w05_dictionary_receipt_rv()
      meta <- w04_prompt_review_metadata()
      if(is.null(prepared)||is.null(dictionary)||
         !identical(as.character(prepared$review_id),as.character(meta$review_id))||
         !identical(prepared$dictionary,dictionary)||
         !identical(w05_dictionary_approved_sha_rv(),as.character(dictionary$sha256)))
        stop("Verified W04 corpus and explicitly approved dictionary required",call.=FALSE)
      if(as.integer(prepared$input$record_count)<1L)
        stop("No verified W04 included records",call.=FALSE)
      request <- list(review_id=meta$review_id,
        zenodo_draft_id=prepared$input$deposit_id,
        input_filename=prepared$input$filename,
        input_sha256=prepared$input$sha256,
        dictionary_filename=dictionary$filename,
        dictionary_sha256=dictionary$sha256)
      w05_shiny_finalise_dispatch(request,w04_published=TRUE,schema_approved=TRUE)
      as.integer(prepared$input$record_count)
    },error=function(e)e)
    w05_shiny_subset_launch_status_rv(if(inherits(result,"error"))
      paste("Full-corpus W05 dispatch failed:",conditionMessage(result)) else
      sprintf("Full-corpus W05 coding dispatched for %d records. W06 remains paused until final W05 verification and prompt approval.",result))
  })
  output$w05_shiny_finalise_control <- renderUI({
    if(!session_can("control_workflows") ||
       !identical(as.character(current_user()$role %||% ""),"administrator"))return(NULL)
    prepared <- w05_verified_input_rv()
    dictionary <- w05_dictionary_receipt_rv()
    ready <- !is.null(prepared) && !is.null(dictionary) &&
      identical(prepared$dictionary,dictionary) &&
      identical(w05_dictionary_approved_sha_rv(),as.character(dictionary$sha256))
    actionButton("w05_shiny_finalise","Finalise W05: code complete corpus",
      class="btn-primary",disabled=if(ready)NULL else "disabled")
  })
  output$w05_shiny_subset_launch_control <- renderUI({
    if(!session_can("control_workflows") ||
       !identical(as.character(current_user()$role %||% ""),"administrator"))return(NULL)
    prepared <- w05_verified_input_rv()
    dictionary <- w05_dictionary_receipt_rv()
    ready <- !is.null(prepared) && !is.null(dictionary) &&
      identical(prepared$dictionary,dictionary) &&
      identical(w05_dictionary_approved_sha_rv(),as.character(dictionary$sha256))
    actionButton("w05_shiny_subset_launch","Run W05 Shiny subset",
      class="btn-outline-primary",disabled=if(ready)NULL else "disabled")
  })
  output$w05_shiny_subset_launch_status <- renderText(
    w05_shiny_subset_launch_status_rv())
  output$w05_shiny_results_status <- renderText(w05_shiny_results_status_rv())
  observeEvent(input$w05_shiny_load_results,{
    req(authenticated())
    if(!session_can("control_workflows") ||
       !identical(as.character(current_user()$role %||% ""),"administrator"))return()
    run <- trimws(as.character(input$w05_shiny_completed_run %||% ""))
    if(!grepl("^[0-9]+$",run)) {
      w05_shiny_results_status_rv("Enter a numeric W05 Shiny GitHub Actions run ID.")
      return()
    }
    result <- tryCatch(w05_shiny_subset_results(run),error=function(e)e)
    if(inherits(result,"error")) {
      w05_shiny_results_status_rv(paste("Cannot verify W05 subset results:",conditionMessage(result)))
      return()
    }
    w05_crop_frequencies_rv(result$frequencies)
    w05_unspecified_evidence_rv(result$evidence)
    w05_shiny_results_status_rv(sprintf("Verified W05 Shiny subset run %s: %s records coded. Results are experimental, not finalised.",
      run,result$manifest$processed_records))
  })
  output$w05_crop_table <- renderTable({
    dict <- w05_crop_dictionary_rv()
    if(is.null(dict))return(NULL)
    dict
  },striped=TRUE,bordered=FALSE,spacing="xs",width="100%")
  output$w05_crop_status <- renderText(w05_crop_status_rv())
  output$w05_unspecified_evidence <- renderTable({
    evidence <- w05_unspecified_evidence_rv()
    if(is.null(evidence))return(NULL)
    evidence
  },striped=TRUE,spacing="xs",width="100%")
  output$w05_unspecified_download <- downloadHandler(
    filename=function()"w05_unspecified_crop_evidence.csv",
    content=function(file) {
      req(authenticated())
      evidence <- w05_unspecified_evidence_rv()
      req(!is.null(evidence))
      utils::write.csv(evidence,file,row.names=FALSE,na="")
    },contentType="text/csv")
  output$w05_crop_frequencies <- renderTable({
    frequencies <- w05_crop_frequencies_rv()
    if(is.null(frequencies))return(data.frame(
       Status="No W05 coding has been executed",check.names=FALSE))
    frequencies
  },striped=TRUE,spacing="xs")
  output$w05_crop_download <- downloadHandler(
    filename=function()"w05_crop_dictionary.csv",
    content=function(file) {
      req(authenticated())
      src <- w05_crop_dictionary_path_rv()
      if(!nzchar(src) || !file.exists(src))stop("No dictionary available",call.=FALSE)
      file.copy(src,file,overwrite=TRUE)
    },contentType="text/csv")
  observeEvent(input$w05_crop_upload,{
    req(authenticated())
    if(!session_can("control_workflows") ||
       !identical(as.character(current_user()$role %||% ""),"administrator"))return()
    info <- input$w05_crop_upload
    if(is.null(info))return()
    result <- tryCatch({
      dict <- w05_crop_read(info$datapath)
      meta <- w04_prompt_review_metadata()
      receipt <- w05_crop_archive_upload(meta,info$datapath)
      list(dictionary=dict,receipt=receipt)
    },error=function(e)e)
    if(inherits(result,"error")) {
      w05_crop_status_rv(paste("W05 upload not confirmed:",conditionMessage(result)))
      showNotification("W05 dictionary upload could not be confirmed. Check status before retrying.",
                       type="error",duration=10)
      return()
    }
    dest <- tempfile("w05-verified-",fileext=".csv")
    if(!file.copy(info$datapath,dest,overwrite=TRUE))
      stop("Cannot preserve verified W05 CSV",call.=FALSE)
    # Restore an already-approved immutable version across Shiny sessions.
    approval <- tryCatch({
      meta <- w04_prompt_review_metadata()
      draft <- w05_crop_draft(meta)
      marker <- paste0("w05-approved-",as.character(result$receipt$sha256),".json")
      files <- draft$files %||% list()
      sum(vapply(files,function(x)identical(as.character(x$filename),marker),
                 logical(1)))==1L
    },error=function(e)FALSE)
    w05_dictionary_approved_sha_rv(if(isTRUE(approval))
      as.character(result$receipt$sha256) else "")
    w05_verified_input_rv(NULL)
    w05_dictionary_receipt_rv(result$receipt)
    w05_crop_dictionary_rv(result$dictionary)
    w05_crop_dictionary_path_rv(dest)
    w05_crop_frequencies_rv(NULL)
    w05_crop_status_rv(sprintf("Archived %d codes in restricted Zenodo draft %s; immutable file %s. W05 execution remains paused.",
      result$receipt$codes,result$receipt$deposit_id,result$receipt$filename))
  })
  # Keep the administrator entry point independent of the expensive configuration
  # renderer. Once mounted, the entry point remains available during reactivity.
  output$configure_review <- renderUI({
    req(authenticated())
    if (!identical(as.character(current_user()$role %||% ""), "administrator")) return(NULL)
    uiOutput("configure_review_contents")
  })

  # These frequently changing scoping values update their own output bindings.
  # They must not invalidate the full Configure review tree and remount the
  # W04 prompt, highlighting and subset input controls.
  output$configure_scope_status <- renderText({
    req(authenticated(), configure_review_requested())
    search_scope_status_rv()
  })
  output$configure_scope_versions <- renderUI({
    req(authenticated(), configure_review_requested())
    versions <- search_scope_versions_rv()
    if (length(versions) <= 1L) return(NULL)
    tags$div(class="saved-note mb-2",
      paste("Temporary versions in this session:",paste(names(versions),collapse=", ")))
  })

  # Keep W01 launch status and formal-search availability independent of
  # unrelated W04 input controls in the large configuration tree.
  output$configure_w01_launch_state <- renderUI({
    req(authenticated(), configure_review_requested())
    if (!identical(as.character(current_user()$role %||% ""), "administrator")) return(NULL)
    tagList(
              if (isTRUE(w01_persisted_lock_rv()) || nzchar(batch_id_rv()))
                tags$p(class="small text-muted mt-2",
                  if (nzchar(batch_id_rv()))
                    "W01 has run for this review. Complete human adjudication and finalisation before a subsequent update."
                  else
                    "W01 launch unavailable: the prior run is already published or launch eligibility has not yet been verified.")
              else if (!is.null(w01_launch_request_rv()))
                tags$p(class="small text-muted mt-2","W01 has been launched; awaiting workflow completion.")
              else if (session_can("run_search_scoping") && identical(as.character(current_user()$role),"administrator"))
                actionButton("w01_begin_deduplication","Begin deduplication",class="btn-primary btn-sm",
                  disabled=if (isTRUE(w01_launch_configuration()$ready)) NULL else "disabled",
                  title=w01_launch_configuration()$reason),
              if (!isTRUE(w01_launch_configuration()$ready))
                tags$p(class="small text-muted mt-2",w01_launch_configuration()$reason),
    )
  })
  output$configure_formal_start_control <- renderUI({
    req(authenticated(), configure_review_requested())
    if (!identical(as.character(current_user()$role %||% ""), "administrator")) return(NULL)
    tagList(
      if (!search_formal_launched_rv())
        actionButton("search_formal_start", "Start search", class="btn-primary btn-sm mt-2"),
      textOutput("search_formal_finalisation_message")
    )
  })

  # Scoping changes only replace the scoping panel, never W04 input controls.
  output$configure_scoping_panel <- renderUI({
    req(authenticated(), configure_review_requested())
    if (!identical(as.character(current_user()$role %||% ""), "administrator")) return(NULL)
    request_active <- nzchar(as.character(search_scope_request_id_rv() %||% ""))
    counts_resolved <- search_scope_counts_resolved()
    original_rows <- (search_scope_results_rv() %||% list())[["original"]]
    original_complete <- search_scope_counts_resolved(original_rows)
          tags$details(
            class="assignment-disclosure",
            `data-accordion-key`="configure-review-scoping",
            tags$summary(tags$strong("Scoping")),
            div(
              class="px-3 pb-3",
          if (!request_active && identical(as.character(current_user()$role),"administrator") &&
              !isTRUE(original_complete)) textAreaInput(
            "search_scope_input_text","Scoping search string",
            value=isolate(search_scope_string_rv()),rows=8,width="100%"
          ) else tagList(
            tags$h6("Scoping search string"),
            tags$pre(class="border rounded bg-light p-3 small mt-2",
              style="white-space:pre-wrap;overflow-wrap:anywhere;",
              search_scope_string_rv()),
            if (!request_active && isTRUE(original_complete)) actionButton(
              "edit_search_scope","Create revised scoping version",
              class="btn-outline-secondary btn-sm"
            )
          ),
          tags$p(class="saved-note",style="font-size:0.85em;",
            tags$strong("Wildcards and plural forms: "),
            "Use an asterisk (*) at the end of a word stem to search for multiple word endings, including regular plurals (e.g. vegetable* retrieves vegetable and vegetables). Truncation can also retrieve related terms (e.g. farm* may retrieve farm, farms, farmer and farming). Irregular plurals, such as woman and women, should be entered separately. Wildcard behaviour varies between databases, so check that your terms capture the intended variants."),
          if (!request_active && !isTRUE(original_complete))
            textOutput("search_scope_validation_message"),
          uiOutput("configure_scope_versions"),
          div(
            class="d-flex align-items-center gap-2 flex-wrap mb-2",
            if (request_active && !counts_resolved) {
              tags$span(class="saved-note fw-semibold","Scoping search running…")
            } else if (request_active && counts_resolved) {
              tags$span(class="saved-note fw-semibold","Scoping search complete.")
            } else {
              uiOutput("search_scope_run_control")
            },
            tags$span(class="saved-note", textOutput("configure_scope_status", inline=TRUE))
          ),
          uiOutput("search_scope_progress"),
          uiOutput("search_scope_table"),
          div(
            class="d-flex align-items-center flex-wrap gap-2 mt-3",
            uiOutput("search_scope_report_download"),
            if (isTRUE(counts_resolved)) actionButton(
              "search_scope_finalise_to_searching", "Finalise search string",
              class="btn-outline-primary btn-sm"
            )
          )
            )
          )
  })

  output$configure_review_contents <- renderUI({
    req(authenticated(), configure_review_requested())
    if (!identical(as.character(current_user()$role %||% ""), "administrator")) return(NULL)
    # Administrator access controls this menu; individual operations retain their own permission checks.

    card(
      class="assignment-summary",
      div(
          class="px-3 pb-3",
          tabsetPanel(id="configure_workflow_tab",type="tabs",selected="W04",
            tabPanel("Searching",div(class="px-3 pb-3",
          uiOutput("configure_scoping_panel"),
          tags$details(
            class="assignment-disclosure",
            `data-accordion-key`="configure-review-api-searching",
            tags$summary(tags$strong("Bibliographic database API searches")),
            div(
              class="px-3 pb-3",
              textAreaInput("search_formal_text", "Formal search string",
                value=isolate(search_formal_draft_rv()), rows=8, width="100%"),
              tags$p(class="saved-note",style="font-size:0.85em;",
                tags$strong("Wildcards and plural forms: "),
                "Use an asterisk (*) at the end of a word stem to search for multiple word endings, including regular plurals (e.g. vegetable* retrieves vegetable and vegetables). Truncation can also retrieve related terms (e.g. farm* may retrieve farm, farms, farmer and farming). Irregular plurals, such as woman and women, should be entered separately. Wildcard behaviour varies between databases, so check that your terms capture the intended variants."),
              fileInput("search_formal_upload", "Upload search string (.txt)",
                accept=c(".txt","text/plain")),
              actionButton("search_formal_save", "Save draft", class="btn-outline-secondary btn-sm"),
              textOutput("search_formal_draft_message"),
              textOutput("search_formal_validation_message"),
              uiOutput("search_source_group_controls"),
              textOutput("search_source_selection_preview"),
              uiOutput("configure_formal_start_control"),
              uiOutput("search_formal_progress"),
              tags$p(class="saved-note",
                "Start search requires successful scoping counts for the same search string and selected databases, then saves the strategy version and launches W00 on development main.")
            )
          ),
          tags$details(
            class="assignment-disclosure",
            `data-accordion-key`="configure-review-manual",
            tags$summary(tags$strong("Manual database searches (RIS)")),
            div(class="px-3 pb-3",
              textInput("search_manual_database","Database name"),
              textInput("search_manual_platform","Search platform (optional)"),
              dateInput("search_manual_date","Date searched",value=Sys.Date(),format="dd-mm-yyyy"),
              textAreaInput("search_manual_query","Database-specific search string",rows=5),
              numericInput("search_manual_reported","Hits reported (optional)",value=NA,min=0,step=1),
              uiOutput("search_manual_files_control"),
              actionButton("search_manual_check","Check RIS files",class="btn-outline-secondary btn-sm"),
              uiOutput("search_manual_message"),
              tableOutput("search_manual_file_preview"),
              if(identical(as.character(current_user()$role %||% ""),"administrator"))
                actionButton("search_manual_submit","Publish RIS submission",
                  class="btn-primary btn-sm",
                  disabled=if(!nzchar(Sys.getenv("ZENODO_ACCESS_TOKEN",unset=""))) "disabled" else NULL),
              tags$p(class="saved-note",
                if(nzchar(Sys.getenv("ZENODO_ACCESS_TOKEN",unset="")))
                  "Each submission is published to a separate restricted Zenodo record, with its original RIS files and search manifest."
                else "Manual publication requires ZENODO_ACCESS_TOKEN in the private Shiny deployment environment."),
              tags$hr(),
              tags$h6("Manual upload history"),
              uiOutput("search_manual_history")
            )
          ),
          tags$details(
            class="assignment-disclosure",
            `data-accordion-key`="configure-review-summary-report",
            tags$summary(tags$strong("Summary and report")),
            div(class="px-3 pb-3",
              tags$h6("Combined search totals"),
              uiOutput("search_combined_totals"),
              downloadButton("download_search_history","Download search history (CSV)",
                class="btn-outline-secondary btn-sm"),
              textOutput("search_history_export_error")
            )
          ),
          tags$details(
            class="assignment-disclosure",
            `data-accordion-key`="configure-review-scheduling",
            tags$summary(tags$strong("Scheduling")),
            div(class="px-3 pb-3 saved-note","Scheduled searching currently disabled")
          ),
            )),
            tabPanel("W01",div(class="px-3 pb-3",
              uiOutput("configure_w01_launch_state"),
              uiOutput("w01_launch_progress"),
              uiOutput("w01_launch_results"),
              uiOutput("w01_ris_controls"),
              textOutput("w01_ris_status")
            ),uiOutput("w01_admin_controls")),
            tabPanel("W02",div(class="px-3 pb-3",
              tags$p(class="saved-note","Launch automated bibliographic enrichment after W01. Human adjudication and optional missing-abstract completion are handled in the Enrichment task."),
              uiOutput("w02_launch_controls"),
              uiOutput("w02_launch_progress"),
              uiOutput("w02_launch_results"),
              uiOutput("w02_optional_pause_controls")
            ),uiOutput("w02_admin_controls")),
            tabPanel("W04",div(class="px-3 pb-3",
              tabsetPanel(id="w04_workspace_tab",type="tabs",selected="Model results",
                tabPanel("Subsets & tasks",
                  div(class="px-3 pb-3",
                  tags$p(class="saved-note","Verify the eligible corpus only when creating a new subset. Existing registered subsets can be loaded, prepared and assigned without repeating verification."),
                  tags$h5("New subsets · Verify eligible corpus"),
                  tags$p(class="saved-note","Check the review-specific W02/W03 source records. This does not run screening or modify records."),
                  actionButton("w04_subset_load_corpus","1. Verify eligible corpus",class="btn-outline-primary btn-sm",
                    onclick="window.lemBusyStart('w04_subset_load_corpus','Verifying W02/W03 corpus provenance…')"),
                  tags$div(id="w04_subset_load_corpus_busy",role="status",`aria-live`="polite",
                    style="display:none;",class="saved-note","Verifying W02/W03 corpus provenance…"),
                  uiOutput("w04_subset_verification_state"),
                  tags$hr(),
                  tags$h5("Select an existing subset or create a new one"),
                  actionButton("w04_subset_registry_refresh","Load registered subsets",class="btn-outline-secondary btn-sm",
                    onclick="window.lemBusyStart('w04_subset_registry_refresh','Loading review-specific subset registry…')"),
                  tags$div(id="w04_subset_registry_refresh_busy",role="status",`aria-live`="polite",
                    style="display:none;",class="saved-note","Loading review-specific subset registry…"),
                  uiOutput("w04_subset_parent_select"),
                  numericInput("w04_subset_size","Number of records",value=100,min=1,step=1),
                  textInput("w04_subset_new_label","Optional subset label",placeholder="e.g. Check 1, MG"),
                  uiOutput("w04_subset_create_control"),
                  uiOutput("w04_subset_registry_history"),
                  uiOutput("w04_subset_label_editor"),
                  tags$hr(),
                  tags$h5("Prepare human screening from a registered subset"),
                  tags$p(class="saved-note","Choose a registered subset to assign to human reviewers. This does not launch an LLM run. Model execution will use the same subset registry separately."),
                  uiOutput("w04_subset_queue_selector"),
                  uiOutput("w04_subset_prepare_control"),
                  tags$div(id="w04_subset_prepare_queue_busy",role="status",`aria-live`="polite",
                    style="display:none;",class="saved-note","Preparing and verifying subset screening queue…"),
                  textOutput("w04_subset_registry_status"),
                  tags$hr(),
                  tags$h5("Assignments and screening progress"),
                  uiOutput("w04_admin_controls")
                )),
                tabPanel("Compare coding",
                  tags$p(class="saved-note",
                    "Select one human-screened subset. Its saved decisions and associated model runs load automatically."),
                  div(class="px-3 pb-3",
                  tags$p(class="saved-note",
                    "Select a human-screened subset once. Associated model runs and reviewer choices load automatically; the latest completed model run is selected by default."),
                  uiOutput("w04_comparison_body"))),
                tabPanel("Run model",
                  tags$details(class="assignment-disclosure",
                    tags$summary(tags$strong("Prompt management")),
                    div(class="px-3 pb-3",
                  tags$p(class="saved-note","Drafts are archived in a separate, restricted review-specific Zenodo deposit. Saving never launches model screening."),
                  fileInput("w04_prompt_upload","Import prompt (TXT, Markdown, DOCX, PDF)",
                    accept=c(".txt",".md",".docx",".pdf")),
                  textAreaInput("w04_prompt_text","Prompt text",rows=12,width="100%"),
                  textInput("w04_prompt_change_notes","Change notes"),
                  actionButton("w04_prompt_save","Save immutable draft",class="btn-primary btn-sm",
                    onclick="this.disabled=true;this.textContent='Saving…';var s=document.getElementById('w04_prompt_progress');if(s){s.style.display='block';s.textContent='Saving and verifying prompt in Zenodo…';}"),
                  tags$div(id="w04_prompt_progress",role="status",`aria-live`="polite",
                    style="display:none;margin-top:8px;",
                    tags$span(class="spinner-border spinner-border-sm me-2",role="status"),
                    "Saving and verifying prompt in Zenodo…"),
                  tags$script(HTML("if(window.Shiny&&!window.w04PromptFeedbackInstalled){window.w04PromptFeedbackInstalled=true;Shiny.addCustomMessageHandler('w04_prompt_feedback',function(x){var b=document.getElementById('w04_prompt_save');if(b){b.disabled=false;b.textContent='Save immutable draft';}var s=document.getElementById('w04_prompt_progress');if(s){s.style.display='none';} });}")),
                  tags$script(HTML("if(!window.lemBusyStart){window.lemBusyStart=function(id,msg){var b=document.getElementById(id);if(!b)return; b.dataset.originalLabel=b.textContent;b.disabled=true;b.textContent='Working…';var s=document.getElementById(id+'_busy');if(s){s.style.display='block';s.textContent=msg;}};}if(window.Shiny&&!window.lemBusyHandlerInstalled){window.lemBusyHandlerInstalled=true;Shiny.addCustomMessageHandler('lem_busy_finished',function(x){var b=document.getElementById(x.id);if(b){b.disabled=false;b.textContent=b.dataset.originalLabel||b.textContent;}var s=document.getElementById(x.id+'_busy');if(s)s.style.display='none';});}")),
                  actionButton("w04_prompt_refresh","Load latest prompt",class="btn-outline-secondary btn-sm",
                    onclick="window.lemBusyStart('w04_prompt_refresh','Retrieving latest archived prompt from Zenodo…')"),
                  tags$div(id="w04_prompt_refresh_busy",role="status",`aria-live`="polite",
                    style="display:none;",class="saved-note","Retrieving latest archived prompt from Zenodo…"),
                  textOutput("w04_prompt_archive_status"),
                  uiOutput("w04_prompt_archive_history"),
                  uiOutput("w04_prompt_zenodo_link")
                )),
                  tags$details(class="assignment-disclosure",open="open",
                    tags$summary(tags$strong("Screen remaining records")),
                    div(class="px-3 pb-3",
                  tags$p(class="saved-note",
                    "Step 1: Prepare screening. The app verifies the eligible corpus and loads registered subsets and archived prompts together. Step 2: Choose the records and prompt, then run screening. You do not need to visit Subsets & tasks first."),
                  actionButton("w04_model_refresh","1. Prepare screening",
                    class="btn-outline-secondary btn-sm",
                    onclick="window.lemBusyStart('w04_model_refresh','Loading review-bound model input options…')"),
                  tags$div(id="w04_model_refresh_busy",class="saved-note",
                    style="display:none;","Loading registered subsets and archived prompts…"),
                  selectInput("w04_model_launch_mode","Screening selection",
                    choices=c("Reconstructed selection (remaining records and/or subsets)"="reconstructed",
                              "Existing human-screened validation subset"="registered")),
                  conditionalPanel("input.w04_model_launch_mode == 'registered'",
                    uiOutput("w04_model_subset_selector")),
                  uiOutput("w04_model_prompt_selector"),
                  conditionalPanel("input.w04_model_launch_mode == 'reconstructed'",
                    tags$div(class="mt-3",
                      tags$p(class="saved-note","Remaining records exclude completed model classifications. Add previously registered subsets only if you intentionally want to rescreen them."),
                      checkboxInput("w04_reconstructed_include_remaining",
                        "Include remaining records",value=TRUE),
                      uiOutput("w04_reconstructed_subset_selector"))),
                  uiOutput("w04_model_launch_control"),
                  tags$div(id="w04_model_launch_busy",class="saved-note",
                    style="display:none;","Archiving selected subset and launching model screening…"),
                  textOutput("w04_model_status"),

                  tags$details(class="assignment-disclosure",
                    `data-accordion-key`="configure-review-w04-prompt-refinement",
                    tags$summary(tags$strong("Improve screening prompt from adjudicated model uncertainties")),
                    div(class="px-3 pb-3",
                      tags$p(class="saved-note",
                        "Use the selected completed run and adjudicated model uncertainties to propose a revised archived prompt. No screening is launched and no archived prompt is overwritten."),
                      actionButton("w04_refinement_load_prompts","Load archived prompts",class="btn-outline-secondary btn-sm"),
                      uiOutput("w04_refinement_prompt_selector"),
                      textAreaInput("w04_refinement_note","Optional note for the suggested changes",
                        rows=3,width="100%"),
                      actionButton("w04_refinement_request","Request prompt improvement (one paid API call)",
                        class="btn-outline-primary btn-sm"),
                      actionButton("w04_refinement_fetch","Retrieve suggested prompt",class="btn-outline-secondary btn-sm"),
                      textOutput("w04_refinement_status"),
                      uiOutput("w04_refinement_run_link"),
                      uiOutput("w04_refinement_review")
                    )
                  ),
                  tags$a("View W04 subset model runs",
                    href="https://github.com/thesalmonandthetomato/LivingEvidenceMap/actions/workflows/w04_shiny_subset_launcher.yml",
                    target="_blank",rel="noopener noreferrer")
                )),
                  tags$details(class="assignment-disclosure",
                    tags$summary(tags$strong("Highlighted screening terms")),
                    div(class="px-3 pb-3",
                  tags$p(class="saved-note","One term or phrase per line. Include terms appear green and exclude terms red in W04 titles, abstracts and keywords. Changes replace the current configuration in the review-specific Google Sheet; the prompt Zenodo draft remains unchanged."),
                  textAreaInput("w04_highlight_include","Terms for include (green)",rows=6,width="100%"),
                  textAreaInput("w04_highlight_exclude","Terms for exclude (red)",rows=6,width="100%"),
                  actionButton("w04_highlight_load","Load highlighted terms",
                    class="btn-outline-secondary btn-sm",
                    onclick="window.lemBusyStart('w04_highlight_load','Loading highlighted terms from Google Sheets…')"),
                  actionButton("w04_highlight_save","Save highlighted terms",
                    class="btn-primary btn-sm",
                    onclick="window.lemBusyStart('w04_highlight_save','Saving highlighted terms to Google Sheets…')"),
                  tags$div(id="w04_highlight_load_busy",role="status",style="display:none;","Loading highlighted terms…"),
                  tags$div(id="w04_highlight_save_busy",role="status",style="display:none;","Saving highlighted terms…"),
                  textOutput("w04_highlight_status")
                ))),
                tabPanel("Model results",
                  div(class="px-3 pb-3",
                  tags$p(class="saved-note","Saved W04 metadata from the summary sheet."),
                  actionButton("w04_model_results_refresh","Refresh overview",class="btn-outline-secondary btn-sm"),
                  textOutput("w04_overview_status"),
                  uiOutput("w04_overview_tables"),
                  tags$hr(),
                  tags$h5("Inspect completed model runs"),
                  tags$p(class="saved-note","Inspect completed runs here; launching models remains in Run model."),
                  actionButton("w04_model_batches_refresh","Load completed model batches",class="btn-outline-secondary btn-sm"),
                  uiOutput("w04_model_registry_diagnostic"),
                  uiOutput("w04_model_batch_selector"),
                  actionButton("w04_model_batch_open","Open selected model batch",class="btn-outline-primary btn-sm"),
                  textOutput("w04_model_return_status"),
                  uiOutput("w04_model_batch_summary"))),
                tabPanel("Uncertain cases",
                  div(class="px-3 pb-3",
                    tags$p(class="saved-note","Select a completed model run with uncertain cases. Only runs assigned to your account are available; administrators can access all runs. Screening decisions are saved through the existing W04 adjudication workflow."),
                    actionButton("w04_reviewer_batches_refresh","Load available uncertainty batches",class="btn-outline-secondary btn-sm"),
                    uiOutput("w04_reviewer_model_batch_selector"),
                    actionButton("w04_reviewer_model_batch_open","Load selected batch",class="btn-outline-primary btn-sm"),
                    textOutput("w04_reviewer_model_status"),
                    uiOutput("w04_uncertain_active_batch"),
                    actionButton("w04_uncertain_open","Examine assigned uncertain cases",class="btn-primary btn-sm"))),
                tabPanel("Conflicts",
                  div(class="px-3 pb-3",
                    tags$p(class="saved-note","Open saved human–human or human–model conflict sets directly for adjudication."),
                    actionButton("w04_overview_refresh","Refresh conflict history",class="btn-outline-secondary btn-sm"),
                    uiOutput("w04_overview_conflict_action"),
                    textOutput("w04_overview_conflict_action_status"),
                    uiOutput("w04_overview_conflicts"))),
                tabPanel("Finalise",
                  div(class="px-3 pb-3",
                    tags$p(class="saved-note","Choose the approved final prompt once. The app matches its completed screening runs across all subsets, reconciles saved human decisions and checks all 3,083 eligible records before enabling publication."),
                    actionButton("w04_finalise_prepare","Refresh finalisation data",class="btn-outline-secondary btn-sm"),
                    textOutput("w04_finalise_prepare_status"),
                    uiOutput("w04_model_run_choices"),
                    textOutput("w04_finalise_check_status"),
                    uiOutput("w04_finalise_publish_control"),
                    textOutput("w04_finalise_publish_reason"),
                    uiOutput("w04_finalise_submission_status")))
              )
            )),
            tabPanel("W05",div(class="px-3 pb-3",
              tags$h5("W05 entity coding"),
              tags$p(class="saved-note","The displayed coding schema can be approved directly. Upload a CSV only to replace it. Unspecified crop is assigned when no named crop matches."),
              fileInput("w05_crop_upload","Upload entity coding dictionary CSV",accept=c(".csv","text/csv")),
              downloadButton("w05_crop_download","Download CSV",class="btn-outline-secondary btn-sm"),
              textOutput("w05_crop_status"),
              textOutput("w05_dictionary_approval_status"),
              uiOutput("w05_dictionary_approve_control"),
              tags$div(style="max-height:420px;overflow:auto;border:1px solid #dce3e8;border-radius:6px;margin-top:10px;",
                tableOutput("w05_crop_table")),
              tags$hr(),
              tags$h6("Deterministic coding results"),
              tags$p(class="saved-note","Frequency counts are the number of distinct records assigned to each code. One record can have multiple identified crops; unspecified is assigned only when no crop is identified."),
              tableOutput("w05_crop_frequencies"),
              tags$h6("Shiny subset testing"),
              tags$p(class="saved-note","This is the separate W05 Shiny execution pathway, not the existing production GitHub pipeline. Experimental subset runs use the uploaded dictionary. The full-corpus run will later start afresh."),
              textInput("w05_w04_publication_run","W04 publication run ID (optional; automatically detect latest for this review)",value=""),
              actionButton("w05_shiny_prepare_input","Verify and prepare W04-included records",class="btn-outline-secondary"),
              numericInput("w05_shiny_subset_size","Records to test",value=100,min=1,max=100000,step=1),
              uiOutput("w05_shiny_subset_launch_control"),
              uiOutput("w05_shiny_finalise_control"),
              textOutput("w05_shiny_subset_launch_status"),
              tags$h6("Retrieve completed subset results"),
              tags$p(class="saved-note","Enter a completed W05 Shiny subset GitHub Actions run ID. Results are read only, retrieved on demand, and checked against their assignments."),
              textInput("w05_shiny_completed_run","W05 Shiny run ID",value=""),
              actionButton("w05_shiny_load_results","Load code frequencies",class="btn-outline-secondary"),
              textOutput("w05_shiny_results_status"),
              downloadButton("w05_unspecified_download","Download unspecified-crop titles and abstracts"),
              tableOutput("w05_unspecified_evidence"),
              tags$p(class="saved-note","Full-corpus execution and final Zenodo publication remain disabled until W04 has a verified publication receipt and the Shiny execution pathway is approved."))),
            tabPanel("W06",div(class="px-3 pb-3",
              tags$h5("W06 geography prompt configuration"),
              tags$p(class="saved-note","W06 is topic-agnostic. Any geography limits, location definitions and extraction rules belong in the uploaded prompt. No model runs automatically."),
              fileInput("w06_shiny_prompt_upload","Upload W06 model prompt (TXT, Markdown, DOCX or PDF)",
                accept=c(".txt",".md",".docx",".pdf")),
              textOutput("w06_shiny_prompt_status"),
              uiOutput("w06_shiny_prompt_approval_control"),
              tags$h6("Prompt preview"),
              tags$pre(style="white-space:pre-wrap;max-height:350px;overflow:auto;",
                textOutput("w06_shiny_prompt_preview")),
              tags$hr(),
              tags$h6("Geography model subset testing"),
              tags$p(class="saved-note","After prompt approval, test any chosen number of published W05 records. Geographic restrictions belong only in the prompt. Trial outputs are not final W06 decisions."),
              numericInput("w06_shiny_subset_size","Records to test",value=20,min=1,max=100000,step=1),
              actionButton("w06_shiny_subset_launch","Run W06 model subset",class="btn-outline-primary"),
              textInput("w06_shiny_run_id","Completed W06 Shiny Actions run ID",value=""),
              actionButton("w06_shiny_load_results","Load W06 trial results",class="btn-outline-secondary"),
              textOutput("w06_shiny_trial_status"),
              downloadButton("w06_shiny_trial_download","Download W06 trial records"),
              tags$div(style="max-height:420px;overflow:auto;",
                tableOutput("w06_shiny_trial_table")),
              tags$p(class="saved-note","Full-corpus W06 finalisation is not enabled. Review trial outputs and uncertainties before approving a final prompt."))),
            tabPanel("W07",div(class="px-3 pb-3",
              tags$h5("W07 topic coding"),
              tags$p(class="saved-note","W06 is bypassed for this review. Prepare an isolated, read-only W05 to W07 handover. This operation does not classify records or publish results."),
              actionButton("w07_shiny_prepare","Prepare W07 input from published W05",
                           class="btn-outline-primary"),
              textOutput("w07_shiny_prepare_status"),
              tags$hr(),
              tags$h6("W07 ontology and prompt"),
              tags$p(class="saved-note","Choose archived versions below. The preview updates automatically. No separate approval is required."),
              uiOutput("w07_saved_versions"),
              tags$h6("Selected ontology"),
              tags$pre(style="white-space:pre-wrap;max-height:240px;overflow:auto;",
                textOutput("w07_ontology_preview")),
              tags$h6("Selected prompt"),
              tags$pre(style="white-space:pre-wrap;max-height:240px;overflow:auto;",
                textOutput("w07_prompt_preview")),
              textOutput("w07_config_status"),
              tags$details(
                tags$summary("Add a new ontology or prompt version"),
                fileInput("w07_ontology_upload","Upload ontology (CSV)",accept=".csv"),
                fileInput("w07_prompt_upload","Upload prompt (TXT, Markdown, DOCX or PDF)",
                  accept=c(".txt",".md",".docx",".pdf"))),
              tags$hr(),
              tags$h6("Experimental W07 topic subset"),
              tags$p(class="saved-note","Both the ontology and prompt must be archived and approved. Run only a small explicit model trial; no full-corpus finalisation occurs."),
              numericInput("w07_trial_size","Records to test",value=10,min=1,max=30,step=1),
              actionButton("w07_trial_launch","Run W07 topic subset",
                class="btn-outline-primary"),
              textOutput("w07_trial_status"),
              tags$hr(),
              tags$h6("Inspect completed W07 trial results"),
              textInput("w07_results_run","Successful W07 Actions run ID",value=""),
              actionButton("w07_results_load","Load W07 results",
                class="btn-outline-secondary"),
              textOutput("w07_results_status"),
              downloadButton("w07_results_csv","Download readable W07 CSV"),
              tags$div(style="max-height:520px;overflow:auto;",
                tableOutput("w07_results_table")),
              tags$p(class="saved-note","Results are experimental GitHub artefacts; human-review and finalisation are not yet enabled."))),
            tabPanel("W08",uiOutput("w08_admin_controls")),
            tabPanel("System",uiOutput("system_admin_controls"))
          )
        )
    )
  }) |> shiny::bindEvent(authenticated(), configure_review_requested(), ignoreNULL=FALSE)

  observeEvent(input$configure_workflow_tab, {
    req(identical(input$configure_workflow_tab,"Searching"))
    req(authenticated(),configure_review_requested())
    if (!session_can("run_search_scoping")) return()
    if (!is.null(search_source_groups_rv())) return()
    cfg <- tryCatch(
      search_source_groups_parse(read_github_text_file("config/workflow00_source_groups.json")),
      error=function(e)e)
    if (inherits(cfg,"error")) {
      search_source_error_rv(paste("Unable to load W00 source groups:",conditionMessage(cfg)))
    } else {
      search_source_groups_rv(cfg)
      search_source_error_rv("")
      # Database labels must be available before scoping has ever run.
      catalogue <- tryCatch(read_w00_scoping_source_catalogue(),error=function(e)e)
      if(inherits(catalogue,"error") || is.null(catalogue) || !nrow(catalogue)) {
        search_source_error_rv("Unable to load database display names. Retry loading the app.")
      } else {
        search_scope_catalogue_rv(catalogue)
      }
    }
  })

  output$search_manual_files_control <- renderUI({
    search_manual_file_reset_rv()
    fileInput("search_manual_files","RIS exports (multiple files for one search)",
      multiple=TRUE,accept=c(".ris"))
  })

  observeEvent(input$search_manual_check,{
    req(authenticated())
    if(!session_can("run_search_scoping")) return()
    result <- tryCatch(manual_ris_prepare(input$search_manual_files,
      as.character(input$search_manual_database %||% ""),
      as.character(input$search_manual_platform %||% ""),
      as.character(input$search_manual_query %||% ""),
      as.character(input$search_manual_date %||% ""),
      suppressWarnings(as.numeric(input$search_manual_reported %||% NA_real_))),
      error=function(e)e)
    if(inherits(result,"error")) {
      search_manual_preview_rv(NULL)
      search_manual_status_rv(conditionMessage(result))
    } else {
      search_manual_preview_rv(result)
      search_manual_status_rv(sprintf("%d RIS file(s), %d records validated. Upload not yet enabled.",
        length(result$files),result$imported_records))
    }
  })
  output$search_manual_message <- renderUI({
    success <- search_manual_success_rv()
    if(!is.null(success)) return(
      div(class="alert alert-success mt-3",role="status",
        tags$strong("RIS upload successful"),
        tags$p(sprintf("%s: %s records across %s RIS files published to restricted Zenodo.",
          success$database,format(success$records,big.mark=","),
          success$file_count),class="mb-1"),
        tags$p(tags$a(href=success$url,target="_blank",rel="noopener noreferrer",
          paste("View Zenodo record",success$record_id)),class="mb-0")
      ))
    message <- search_manual_status_rv()
    if(nzchar(message)) tags$p(class="saved-note",message) else NULL
  })
  output$search_manual_file_preview <- renderTable({
    x <- search_manual_preview_rv()
    if(is.null(x)) return(NULL)
    data.frame(File=vapply(x$files,`[[`,character(1),"name"),
      Records=vapply(x$files,`[[`,integer(1),"records"),
      check.names=FALSE)
  },striped=TRUE)

  observeEvent(input$search_manual_submit,{
    req(authenticated(),current_user())
    search_manual_success_rv(NULL)
    if(!identical(as.character(current_user()$role),"administrator")) return()
    if(isTRUE(search_manual_busy_rv())) return()
    result <- tryCatch(manual_ris_prepare(input$search_manual_files,
      as.character(input$search_manual_database %||% ""),
      as.character(input$search_manual_platform %||% ""),
      as.character(input$search_manual_query %||% ""),
      as.character(input$search_manual_date %||% ""),
      suppressWarnings(as.numeric(input$search_manual_reported %||% NA_real_))),
      error=function(e)e)
    if(inherits(result,"error")) {
      search_manual_status_rv(conditionMessage(result))
      return()
    }
    id <- paste0("manual-",format(Sys.time(),"%Y%m%dT%H%M%SZ",tz="UTC"),"-",
      substr(digest::digest(paste(Sys.time(),result$database,result$search_string),
        algo="sha256",serialize=FALSE),1L,10L))
    showModal(modalDialog(title="Confirm restricted RIS publication",
      tags$p(sprintf("Publish %d RIS file(s), containing %d records, for %s?",
        length(result$files),result$imported_records,result$database)),
      tags$p("This creates a new restricted Zenodo record; it cannot be undone automatically."),
      easyClose=FALSE,
      footer=tagList(modalButton("Cancel"),
        actionButton("search_manual_confirm_publish","Confirm publication",
          class="btn-primary"))))
    search_manual_pending_rv(list(prepared=result,files=input$search_manual_files,submission_id=id))
  })
  observeEvent(input$search_manual_confirm_publish,{
    req(authenticated(),current_user())
    if(!identical(as.character(current_user()$role),"administrator") ||
      isTRUE(search_manual_busy_rv())) return()
    submission <- search_manual_pending_rv()
    if(is.null(submission$prepared)) return()
    search_manual_busy_rv(TRUE)
    removeModal()
    on.exit(search_manual_busy_rv(FALSE),add=TRUE)
    result <- tryCatch({
      review <- search_formal_review_id()
      key <- search_strategy_review_key(review)
      if(nrow(manual_search_registry(key)) &&
         submission$submission_id %in% manual_search_registry(key)$submission_id)
        stop("This manual search submission is already registered",call.=FALSE)
      receipt <- manual_search_zenodo_publish(manual_search_review_id(),submission$submission_id,
        submission$prepared,submission$files)
      p <- submission$prepared
      row <- list(review_key=key,submission_id=submission$submission_id,
        database=p$database,platform=p$platform,search_date=p$search_date,
        search_string=p$search_string,
        results_reported=if(is.null(p$reported_hits)) "" else as.character(p$reported_hits),
        records_downloaded=as.character(p$imported_records),
        file_count=as.character(length(p$files)),
        files_json=as.character(jsonlite::toJSON(receipt$files,auto_unbox=TRUE)),
        zenodo_record_id=receipt$zenodo_record_id,
        zenodo_receipt_json=as.character(jsonlite::toJSON(receipt,auto_unbox=TRUE)),
        created_at_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"))
      manual_search_append(row)
      receipt
    },error=function(e)e)
    if(inherits(result,"error")) {
      search_manual_success_rv(NULL)
      search_manual_status_rv(paste("Publication or registration failed:",
        conditionMessage(result),
        "If a Zenodo draft or publication ID is shown, reconcile it before retrying."))
    } else {
      search_manual_status_rv("")
      search_manual_success_rv(list(
        database=submission$prepared$database,
        records=submission$prepared$imported_records,
        file_count=length(submission$prepared$files),
        record_id=result$zenodo_record_id,url=result$record_url))
      updateTextInput(session,"search_manual_database",value="")
      updateTextInput(session,"search_manual_platform",value="")
      updateDateInput(session,"search_manual_date",value=Sys.Date())
      updateTextAreaInput(session,"search_manual_query",value="")
      updateNumericInput(session,"search_manual_reported",value=NA_real_)
      search_manual_file_reset_rv(isolate(search_manual_file_reset_rv())+1L)
      search_manual_preview_rv(NULL)
      search_manual_pending_rv(NULL)
      search_manual_registry_refresh_rv(isolate(search_manual_registry_refresh_rv())+1L)
      showNotification("RIS upload successful. Fields cleared for the next search.",
        type="message",duration=10)
    }
  })
  search_manual_history_data <- reactive({
    req(authenticated())
    search_manual_registry_refresh_rv()
    key <- search_strategy_review_key(search_formal_review_id())
    tryCatch(manual_search_registry(key),error=function(e)e)
  })
  output$search_manual_history <- renderUI({
    manual <- search_manual_history_data()
    if(inherits(manual,"error")) return(tags$p(class="text-danger",
      paste("Unable to read manual upload registry:",conditionMessage(manual))))
    if(!nrow(manual)) return(tags$p(class="saved-note",
      "No manual RIS uploads recorded for this review."))
    values <- suppressWarnings(as.numeric(manual$records_downloaded))
    valid <- !is.na(values) & is.finite(values) & values>=0
    if(!all(valid)) return(tags$p(class="text-danger",
      "Manual upload registry contains invalid record counts; total unavailable."))
    table <- tags$table(class="table table-sm",
      tags$thead(tags$tr(tags$th("Database"),tags$th("Search date"),
        tags$th("RIS files"),tags$th("Records"),tags$th("Archive"))),
      tags$tbody(lapply(seq_len(nrow(manual)),function(i) {
        x <- manual[i,,drop=FALSE]
        record <- as.character(x$zenodo_record_id)
        tags$tr(tags$td(as.character(x$database)),
          tags$td(as.character(x$search_date)),
          tags$td(as.character(x$file_count)),
          tags$td(format(values[[i]],big.mark=",",scientific=FALSE)),
          tags$td(if(nzchar(record))
            tags$a(href=paste0("https://zenodo.org/records/",record),
              target="_blank",rel="noopener noreferrer",record)
            else "—"))
      })),
      tags$tfoot(tags$tr(class="fw-semibold",tags$td("Total"),
        tags$td(sprintf("%d submissions",nrow(manual))),
        tags$td(format(sum(suppressWarnings(as.numeric(manual$file_count)),
          na.rm=TRUE),big.mark=",",scientific=FALSE)),
        tags$td(format(sum(values),big.mark=",",scientific=FALSE)),
        tags$td(""))))
    div(class="table-responsive",table)
  })
  output$search_combined_totals <- renderUI({
    manual <- search_manual_history_data()
    if(inherits(manual,"error")) return(tags$p(class="text-danger",
      "Combined totals unavailable: manual registry could not be read."))
    manual_values <- if(nrow(manual)) suppressWarnings(as.numeric(manual$records_downloaded))
      else numeric()
    if(anyNA(manual_values) || any(!is.finite(manual_values)))
      return(tags$p(class="text-danger","Combined totals unavailable: invalid manual record counts."))
    counts <- search_formal_counts_rv()
    api_values <- vapply(counts,function(x)as.numeric(x$downloaded),numeric(1))
    api_total <- sum(api_values)
    manual_total <- sum(manual_values)
    m <- search_formal_monitor_rv()
    finished <- !is.null(m) && identical(as.character(m$status %||% ""),"completed") &&
      identical(as.character(m$conclusion %||% ""),"success")
    jobs <- if(!is.null(m)) m$jobs %||% list() else list()
    source_jobs <- Filter(function(j) grepl(
      "^search-sources \\([^)]+\\) / search$",
      as.character(j$name %||% "")),jobs)
    complete_api <- finished && length(source_jobs)>0 &&
      length(api_values)>=length(source_jobs)
    tags$div(
      tags$table(class="table table-sm",
        tags$thead(tags$tr(tags$th("Search source"),tags$th("Records downloaded"))),
        tags$tbody(
          tags$tr(tags$td("API databases"),
            tags$td(if(length(api_values))format(api_total,big.mark=",",scientific=FALSE) else "—")),
          tags$tr(tags$td("Manual RIS uploads"),
            tags$td(format(manual_total,big.mark=",",scientific=FALSE)))),
        tags$tfoot(tags$tr(class="fw-semibold",tags$td("Combined total"),
          tags$td(if(complete_api)format(api_total+manual_total,
            big.mark=",",scientific=FALSE) else "Pending complete API counts")))),
      tags$p(class="saved-note",
        if(complete_api)
          "Downloaded records before deduplication. Counts may include duplicates between searches."
        else "API counts are incomplete or not yet restored. The combined total will appear when all successful W00 database counts are available.")
    )
  })

  output$search_history_export_error <- renderText(search_history_export_error_rv())
  output$download_search_history <- downloadHandler(
    filename=function()sprintf("search-history-%s.csv",format(Sys.Date(),"%Y-%m-%d")),
    content=function(file) {
      search_history_export_error_rv("")
      tryCatch({
      req(authenticated())
      review <- search_formal_review_id()
      key <- search_strategy_review_key(review)
      manual <- manual_search_registry(key)
      run_ids <- manual_search_successful_runs()
      rows <- list()
      omissions <- character()
      add_row <- function(search_id,type,database,platform,date,query,reported,downloaded,
        count,zenodo,github) {
        rows[[length(rows)+1L]] <<- data.frame(search_id=search_id,
          search_type=type,database=database,platform=platform,
          search_date=date,search_string=query,
          results_reported=as.character(reported),
          records_downloaded=as.character(downloaded),
          file_count=as.character(count),zenodo_record_id=zenodo,
          github_run_id=github,stringsAsFactors=FALSE)
      }
      for(run in run_ids) {
        records <- tryCatch(manual_search_api_history(run),error=function(e)e)
        if(inherits(records,"error")) {
          omissions <- c(omissions,paste0("W00 run ",run,": ",conditionMessage(records)))
          next
        }
        for(record in records) {
          name <- search_scope_pretty_source(record$source)
          platform <- if(grepl(", via ",name,fixed=TRUE))
            sub("^.*?, via ","",name) else ""
          database <- sub(", via .*","",name)
          add_row(paste0("W00:",run,":",record$source),"API",
            database,platform,record$search_date,record$search_string,
            record$reported,record$downloaded,"", "",run)
        }
      }
      if(nrow(manual)) for(i in seq_len(nrow(manual))) {
        x <- manual[i,,drop=FALSE]
        add_row(as.character(x$submission_id),"manual",
          as.character(x$database),as.character(x$platform),
          as.character(x$search_date),as.character(x$search_string),
          as.character(x$results_reported),as.character(x$records_downloaded),
          as.character(x$file_count),as.character(x$zenodo_record_id),"")
      }
      if(length(omissions)) stop(paste(
        "Complete search history cannot be exported because published API search records are unavailable:",
        paste(omissions,collapse="; ")),call.=FALSE)
      if(!length(rows))stop(paste(
        "No completed API or manual search records are available.",
        if(length(omissions)) paste("Unavailable:",paste(omissions,collapse="; ")) else ""),
        call.=FALSE)
      out <- do.call(rbind,rows)
      out <- out[order(out$search_date,out$database),,drop=FALSE]
      utils::write.csv(out,file,row.names=FALSE,na="")
      },error=function(e) {
        message <- paste("Search history export failed:",conditionMessage(e))
        search_history_export_error_rv(message)
        showNotification(message,type="error",duration=NULL)
        stop(message,call.=FALSE)
      })
    }
  )

  output$search_source_group_controls <- renderUI({
    groups <- search_source_groups_rv()
    if (isTRUE(search_formal_launched_rv())) return(NULL)
    if (is.null(groups)) return(tags$p(class="saved-note",search_source_error_rv()))
    tagList(
      checkboxInput("search_all_databases","All databases",FALSE),
      tags$h6("Database groups"),
      lapply(names(groups),function(id) {
        group <- groups[[id]]
        ids <- unlist(group$sources,use.names=FALSE)
        tags$details(
          class="assignment-disclosure border rounded mb-2",
          tags$summary(class="p-2 fw-semibold",as.character(group$display_name)),
          div(class="px-3 pb-2",
            checkboxInput(paste0("search_group_all_",id),"Select all in this group",FALSE),
            checkboxGroupInput(paste0("search_group_sources_",id),NULL,
              choices=stats::setNames(ids,vapply(ids,search_scope_pretty_source,character(1))),selected=character())
          )
        )
      })
    )
  })

  observe({
    groups <- search_source_groups_rv()
    req(authenticated(),groups)
    for(id in names(groups)) {
      local({
        group_id <- id
        source_ids <- unlist(groups[[group_id]]$sources,use.names=FALSE)
        observeEvent(input[[paste0("search_group_all_",group_id)]], {
          chosen <- isTRUE(input[[paste0("search_group_all_",group_id)]])
          updateCheckboxGroupInput(session,paste0("search_group_sources_",group_id),
            selected=if(chosen) source_ids else character())
        },ignoreInit=TRUE)
      })
    }
  })

  observeEvent(input$search_all_databases, {
    groups <- search_source_groups_rv()
    req(authenticated(),groups)
    selected <- isTRUE(input$search_all_databases)
    for (id in names(groups)) {
      updateCheckboxInput(session,paste0("search_group_all_",id),value=selected)
      updateCheckboxGroupInput(session,paste0("search_group_sources_",id),
        selected=if(selected) unlist(groups[[id]]$sources,use.names=FALSE) else character())
    }
  },ignoreInit=TRUE)

  search_source_selected <- reactive({
    groups <- search_source_groups_rv()
    if (is.null(groups)) return(character())
    unique(unlist(lapply(names(groups),function(id)
      as.character(input[[paste0("search_group_sources_",id)]] %||% character())),
      use.names=FALSE))
  })

  output$search_source_selection_preview <- renderText({
    groups <- search_source_groups_rv()
    if (is.null(groups)) return("")
    if (isTRUE(search_formal_launched_rv())) {
      submitted <- search_formal_selected_sources_rv()
      if (length(submitted))
        return(paste0("Databases submitted for W00 (",length(submitted),"): ",
          paste(vapply(submitted,search_scope_pretty_source,character(1)),collapse=", ")))
      return("W00 search submitted. Database selection is no longer editable.")
    }
    resolved <- search_source_selected()
    if(!length(resolved)) return("No databases selected.")
    paste0(length(resolved)," unique W00 database(s): ",paste(vapply(resolved,search_scope_pretty_source,character(1)),collapse=", "))
  })

  observeEvent(input$search_scope_finalise_to_searching, {
    req(authenticated())
    if (!session_can("run_search_scoping")) return()
    query <- as.character(search_scope_string_rv() %||% "")
    if (!nzchar(trimws(query))) {
      showNotification("There is no scoping query to transfer.", type="error")
      return()
    }
    search_formal_draft_rv(query)
    search_formal_validation_rv(NULL)
    updateTextAreaInput(session, "search_formal_text", value=query)
    showNotification("Scoping query copied to Searching as an editable draft.", type="message")
  })


  # Restore completed scoping evidence from the review storage backend.
  observe({
    req(authenticated())
    if (!session_can("run_search_scoping") || isTRUE(search_scope_history_loaded_rv())) return()
    review <- tryCatch(search_formal_review_id(),error=function(e)e)
    if (inherits(review,"error")) return()
    history <- tryCatch(search_scoping_history(search_strategy_review_key(review)),error=function(e)e)
    if (inherits(history,"error")) {
      search_scope_status_rv(paste("Unable to restore saved scoping:",conditionMessage(history)))
      return()
    }
    if (nrow(history)) {
      latest <- history[nrow(history),,drop=FALSE]
      rows <- tryCatch(jsonlite::fromJSON(as.character(latest$counts_json[[1L]]),
        simplifyDataFrame=TRUE),error=function(e)NULL)
      if (is.data.frame(rows) && nrow(rows)) {
        query <- as.character(latest$query[[1L]])
        search_scope_versions_rv(c(original=query))
        search_scope_results_rv(list(original=rows))
        search_scope_rows_rv(rows)
        search_scope_string_rv(query)
        search_scope_active_version_rv("original")
        search_scope_status_rv("Previous completed scoping results restored from review storage.")
      }
    }
    search_scope_history_loaded_rv(TRUE)
  })

  # Temporary single-review identity. Uses the existing per-deployment Sheet ID
  # instead of requiring a second environment variable. No registry is created.
  search_formal_review_id <- function() {
    configured <- trimws(Sys.getenv("LEM_SEARCH_REVIEW_ID",unset=""))
    if (nzchar(configured)) return(configured)
    if (identical(storage_backend(),"google_sheets"))
      return(paste0("single-review-sheet:",sheet_id_from_env()))
    "single-review-local"
  }

  search_formal_key <- function() {
    u <- current_user()
    if (is.null(u)) return("")
    review_id <- tryCatch(search_formal_review_id(),error=function(e) "")
    if (!nzchar(review_id)) return("")
    search_draft_key(review_id,as.character(u$user_id))
  }

  observe({
    req(authenticated(),current_user())
    key <- search_formal_key()
    if (!nzchar(key)) {
      search_formal_draft_message_rv("Draft storage is not configured for this deployment.")
      return()
    }
    if (identical(search_formal_loaded_key_rv(),key)) return()
    record <- tryCatch(search_draft_read(key),error=function(e)e)
    if (inherits(record,"error")) {
      search_formal_draft_message_rv(paste("Unable to restore draft:",conditionMessage(record)))
      return()
    }
    search_formal_loaded_key_rv(key)
    if (is.null(record)) return()
    query <- as.character(record$search_string[[1L]])
    search_formal_draft_rv(query)
    search_formal_validation_rv(NULL)
    updateTextAreaInput(session,"search_formal_text",value=query)
    search_formal_draft_message_rv("Saved draft restored.")
  })

  observeEvent(input$search_formal_save, {
    req(authenticated(),current_user())
    if (!session_can("run_search_scoping")) return()
    key <- search_formal_key()
    if (!nzchar(key)) {
      search_formal_draft_message_rv("Draft not saved: review storage is not configured.")
      return()
    }
    value <- as.character(input$search_formal_text %||% "")
    result <- tryCatch(search_draft_save(key,value),error=function(e)e)
    search_formal_draft_message_rv(if(inherits(result,"error"))
      paste("Draft not confirmed:",conditionMessage(result)) else "Draft saved and verified.")
  })

  observeEvent(input$search_formal_text, {
    req(authenticated())
    if (!session_can("run_search_scoping")) return()
    value <- as.character(input$search_formal_text %||% "")
    if (!identical(value, search_formal_draft_rv())) {
      search_formal_draft_rv(value)
      search_formal_validation_rv(NULL)
      search_formal_expected_version_rv(NULL)
    }
  }, ignoreInit=TRUE)

  observeEvent(input$search_formal_upload, {
    req(authenticated())
    if (!session_can("run_search_scoping")) return()
    upload <- input$search_formal_upload
    if (is.null(upload)) return()
    if (!grepl("\\.txt$", upload$name, ignore.case=TRUE)) {
      showNotification("Upload a .txt search-string file.", type="error")
      return()
    }
    bytes <- file.info(upload$datapath)$size
    if (is.na(bytes) || bytes > 1024L * 1024L || bytes == 0L) {
      showNotification("Search-string file must be between 1 byte and 1 MB.", type="error")
      return()
    }
    lines <- tryCatch(readLines(upload$datapath, warn=FALSE, encoding="UTF-8"),
                      error=function(e) e)
    if (inherits(lines,"error")) {
      showNotification("Unable to read the uploaded text file.", type="error")
      return()
    }
    query <- sub("^\\ufeff", "", paste(lines, collapse="\\n"))
    if (!nzchar(trimws(query))) {
      showNotification("Uploaded search string is empty.", type="error")
      return()
    }
    search_formal_draft_rv(query)
    search_formal_validation_rv(NULL)
    updateTextAreaInput(session, "search_formal_text", value=query)
  })

  search_formal_live_validation <- reactive({
    search_boolean_validate(as.character(input$search_formal_text %||% ""))
  })

  # Require successful count-only scoping of this exact query for selected databases.
  search_formal_scoping_check <- function(query, sources) {
    versions <- search_scope_versions_rv()
    results <- search_scope_results_rv() %||% list()
    matched <- names(versions)[vapply(versions,function(x)
      identical(trimws(as.character(x)),trimws(as.character(query))),logical(1))]
    if (!length(matched))
      return("Run scoping for this search string before starting the full search.")
    for (version in rev(matched)) {
      rows <- results[[version]]
      if (is.null(rows) || !nrow(rows) ||
          !all(c("source_slug","status","hits") %in% names(rows))) next
      positions <- match(sources,as.character(rows$source_slug))
      if (anyNA(positions)) next
      selected <- rows[positions,,drop=FALSE]
      states <- vapply(selected$status,search_scope_display_status,character(1))
      hits <- suppressWarnings(as.numeric(as.character(selected$hits)))
      if (all(states=="Complete") && all(is.finite(hits) & hits>=0))
        return(NULL)
    }
    "Complete successful scoping for every selected database with this search string before starting W00."
  }

  # Production launch is explicitly gated until the review-isolated W01 route,
  # human queue and final Zenodo receipt have been connected.
  observeEvent(input$w01_begin_deduplication, {
    req(authenticated(),current_user())
    cfg <- w01_launch_configuration()
    if (!identical(as.character(current_user()$role),"administrator") ||
        !session_can("run_search_scoping")) {
      w01_launch_error_rv("An administrator with review configuration permission is required.")
      return()
    }
    if (!isTRUE(cfg$ready)) {
      w01_launch_error_rv(cfg$reason)
      showNotification(cfg$reason,type="error",duration=10)
      return()
    }
    if (!is.null(w01_launch_request_rv())) {
      showNotification("A W01 launch is already being monitored in this session.",type="message",duration=8)
      return()
    }
    if(isTRUE(w01_persisted_lock_rv())) {
      showNotification("W01 has been run already, or its prior state cannot yet be verified. A new W00 update is required before another W01 launch.",type="warning",duration=10)
      return()
    }
    if(nzchar(batch_id_rv())) {
      showNotification("W01 already has a review batch for this project. Finish the batch before running an update.",type="warning",duration=10)
      return()
    }
    w01_launch_error_rv("")
    sheet_id <- trimws(tryCatch(sheet_id_from_env(),error=function(e)""))
    if (!grepl("^[A-Za-z0-9_-]{15,150}$",sheet_id)) {
      w01_launch_error_rv("Configure a valid review Google Sheet ID before starting W01.")
      return()
    }
    showModal(modalDialog(
      title="Start deduplication",
      tags$p("Run W01 for this review using the configured Google Sheet and authenticated GitHub API?"),
      tags$p("Any unresolved duplicate decisions will be sent for adjudication."),
      easyClose=TRUE,
      footer=tagList(modalButton("Cancel"),
        actionButton("w01_confirm_deduplication","Confirm and begin",class="btn-primary"))
    ))
  })

  observeEvent(input$w01_confirm_deduplication, {
    req(authenticated(),current_user())
    removeModal()
    if (!isTRUE(w01_launch_configuration()$ready) ||
        !identical(as.character(current_user()$role),"administrator") ||
        !session_can("run_search_scoping") || !is.null(w01_launch_request_rv()) ||
        nzchar(batch_id_rv()) || isTRUE(w01_persisted_lock_rv())) return()
    sheet_id <- trimws(tryCatch(sheet_id_from_env(),error=function(e)""))
    configured_review_id <- trimws(Sys.getenv("LEM_SEARCH_REVIEW_ID",unset=""))
    identity <- tryCatch({
      if(!grepl("^[A-Za-z0-9_-]{15,150}$",sheet_id))
        stop("Review Google Sheet ID is missing or invalid")
      metadata <- read_review_metadata_cached(sheet_id)
      if(!all(c("key","value") %in% names(metadata))) stop("Invalid review_metadata")
      review_id <- as.character(metadata$value[match("review_id",metadata$key)])
      if(length(review_id)!=1L || is.na(review_id) ||
         !grepl("^[A-Za-z0-9._-]{3,128}$",review_id))
        stop("Google Sheet review_metadata has no valid review_id")
      if(nzchar(configured_review_id) && !identical(configured_review_id,review_id))
        stop("Configured LEM_SEARCH_REVIEW_ID does not match the Google Sheet")
      review_id
    },error=function(e) {
      w01_launch_error_rv(paste("W01 review identity verification failed:",conditionMessage(e)))
      showNotification(conditionMessage(e),type="error",duration=12)
      NULL
    })
    if(is.null(identity)) return()
    review_id <- identity
    request_id <- paste0("shiny-",format(Sys.time(),"%Y%m%d%H%M%S",tz="UTC"),"-",
                         paste(sample(c(letters,0:9),12L,replace=TRUE),collapse=""))
    sent <- tryCatch({
      dispatch_w01_input_preflight(review_id,sheet_id,request_id,run_llm=TRUE)
      TRUE
    },error=function(e) {
      w01_launch_error_rv(conditionMessage(e))
      FALSE
    })
    if (!sent) return()
    w01_launch_result_rv(NULL)
    w01_launch_pending_rv(NULL)
    w01_launch_run_id_rv("")
    w01_launch_snapshot_rv(NULL)
    w01_launch_error_rv("")
    w01_launch_request_rv(list(review_id=review_id,review_sheet_id=sheet_id,
                               request_id=request_id))
  })

  # W01 production remains fail-closed. These outputs become live when
  # review-isolated launch, monitoring and result publication are integrated.
  w01_persisted_lock_rv <- reactiveVal(TRUE)
  observeEvent(list(authenticated(), configure_review_requested()), {
    if(!isTRUE(authenticated()) || !isTRUE(configure_review_requested()) ||
       !identical(storage_backend(),"google_sheets")) return()
    locked <- tryCatch({
      sheet <- sheet_id_from_env()
      meta <- read_review_metadata_cached(sheet)
      review <- as.character(meta$value[match("review_id",meta$key)])
      if(length(review)!=1L || is.na(review) || !nzchar(review)) stop("Missing review identity")
      w01_has_published_review_run(review,sheet)
    },error=function(e) {
      w01_launch_error_rv(paste("Cannot verify W01 launch eligibility:",conditionMessage(e)))
      TRUE
    })
    w01_persisted_lock_rv(isTRUE(locked))
  })
  w01_launch_request_rv <- reactiveVal(NULL)
  w01_launch_result_rv <- reactiveVal(NULL)
  w01_launch_pending_rv <- reactiveVal(NULL)
  w01_launch_run_id_rv <- reactiveVal("")
  w01_launch_error_rv <- reactiveVal("")
  w01_launch_snapshot_rv <- reactiveVal(NULL)

  # Locate the exact run associated with this review's launch request.
  # Never display progress from the latest global W01 run.
  observe({
    req(authenticated())
    request <- w01_launch_request_rv()
    if(is.null(request) || nzchar(w01_launch_run_id_rv())) return()
    invalidateLater(5000,session)
    run <- tryCatch(
      w01_find_shiny_run(request$review_id,request$request_id),
      error=function(e)e
    )
    if(inherits(run,"error")) {
      w01_launch_error_rv(conditionMessage(run))
      return()
    }
    if(is.null(run)) return()
    w01_launch_run_id_rv(as.character(run$id))
    w01_launch_error_rv("")
  })

  # A run is only monitored after its exact GitHub run ID has been verified.
  observe({
    req(authenticated())
    run_id <- w01_launch_run_id_rv()
    if(!nzchar(run_id)) return()
    invalidateLater(5000,session)
    snapshot <- tryCatch(w01_github_run_progress(run_id),error=function(e)e)
    if(inherits(snapshot,"error")) {
      w01_launch_error_rv(conditionMessage(snapshot))
    } else {
      w01_launch_error_rv("")
      w01_launch_snapshot_rv(snapshot)
      request <- w01_launch_request_rv()
      if(!is.null(request) && identical(snapshot$status,"completed") &&
         identical(snapshot$conclusion,"success") &&
         is.null(w01_launch_result_rv())) {
        checked <- tryCatch({
          result <- w01_published_review_results(
            snapshot$run_id,request$review_id,request$review_sheet_id)
          if(!is.null(result)) {
            list(result=c(result,list(run_id=snapshot$run_id)),pending=NULL)
          } else {
            pending <- w01_pending_review_checkpoint(
              snapshot$run_id,request$review_id,request$review_sheet_id)
            if(is.null(pending)) {
              list(result=NULL,pending=NULL)
            } else {
              resolved_run <- w01_resolved_review_run_id(
                snapshot$run_id,request$review_id)
              if(is.null(resolved_run)) {
                list(result=NULL,pending=pending)
              } else {
                final <- w01_published_review_results(
                  resolved_run,request$review_id,request$review_sheet_id)
                list(result=if(is.null(final)) NULL else c(final,list(run_id=resolved_run)),
                     pending=if(is.null(final)) pending else NULL)
              }
            }
          }
        },error=function(e)e)
        if(inherits(checked,"error")) {
          w01_launch_error_rv(conditionMessage(checked))
        } else {
          if(!is.null(checked$pending) && is.null(w01_launch_pending_rv())) {
            w01_queue_refresh_signal(isolate(w01_queue_refresh_signal())+1L)
          }
          w01_launch_pending_rv(checked$pending)
          if(!is.null(checked$result)) w01_launch_result_rv(checked$result)
        }
      }
    }
  })

  output$w01_launch_progress <- renderUI({
    m <- w01_launch_snapshot_rv()
    if(is.null(m)) return(NULL)
    if(identical(m$status,"completed")) return(NULL)
    pct <- as.integer(m$progress %||% 0L)
    tags$div(
      class="mt-2",
      tags$div(class="progress",role="progressbar",
        `aria-valuenow`=pct,`aria-valuemin`="0",`aria-valuemax`="100",
        tags$div(class="progress-bar progress-bar-striped progress-bar-animated",
          style=paste0("width:",pct,"%")))
    )
  })

  # Export is intentionally unavailable until a review-specific final W01
  # receipt exists. The RIS is created on demand, not polled from Google Sheets.
  w01_final_receipt_rv <- reactiveVal(NULL)
  observeEvent(list(authenticated(), configure_review_requested()), {
    if(!isTRUE(authenticated()) || !isTRUE(configure_review_requested()) ||
       !identical(storage_backend(),"google_sheets")) return()
    receipt <- tryCatch({
      sheet <- sheet_id_from_env()
      metadata <- read_review_metadata_cached(sheet)
      review <- as.character(metadata$value[match("review_id",metadata$key)])
      if(length(review)!=1L || is.na(review) ||
         !grepl("^[A-Za-z0-9._-]{3,128}$",review)) return(NULL)
      w01_latest_final_review_receipt(review,sheet)
    },error=function(e) NULL)
    w01_final_receipt_rv(receipt)
  })
  w01_ris_export_rv <- reactiveVal(NULL)
  w01_ris_status_rv <- reactiveVal("")
  w01_ris_artifact_rv <- reactiveVal(NULL)
  w01_ris_check_active_rv <- reactiveVal(FALSE)

  w01_final_export_identity <- reactive({
    request <- w01_launch_request_rv()
    result <- w01_launch_result_rv()
    if(!is.null(request) && !is.null(result) &&
       grepl("^[0-9]+$",as.character(result$run_id %||% "")))
      return(list(review_id=request$review_id,run_id=as.character(result$run_id)))
    w01_final_receipt_rv()
  })

  output$w01_ris_controls <- renderUI({
    id <- w01_final_export_identity()
    if(is.null(id)) return(NULL)
    tagList(
      actionButton("w01_prepare_ris","Prepare unique records RIS",class="btn-outline-primary btn-sm"),
      if(!is.null(w01_ris_artifact_rv())) downloadButton("w01_download_ris","Download RIS",class="btn-primary btn-sm")
    )
  })
  output$w01_ris_status <- renderText(w01_ris_status_rv())

  observeEvent(input$w01_prepare_ris,{
    req(authenticated(),current_user())
    id <- w01_final_export_identity()
    if(is.null(id)) return()
    w01_ris_artifact_rv(NULL)
    ok <- tryCatch({
      dispatch_w01_ris_export(id$review_id,id$run_id)
      TRUE
    },error=function(e) {
      w01_ris_status_rv(paste("RIS preparation failed:",conditionMessage(e)))
      FALSE
    })
    if(ok) {
      w01_ris_export_rv(id)
      w01_ris_check_active_rv(TRUE)
      w01_ris_status_rv("Preparing deduplicated RIS from the final restricted W01 record…")
    }
  })

  observe({
    req(authenticated())
    id <- w01_ris_export_rv()
    if(is.null(id) || !isTRUE(w01_ris_check_active_rv())) return()
    invalidateLater(10000,session)
    x <- tryCatch(w01_ris_export_artifact(id$review_id,id$run_id),error=function(e)e)
    if(inherits(x,"error")) {
      w01_ris_status_rv(paste("RIS artifact lookup failed:",conditionMessage(x)))
      w01_ris_check_active_rv(FALSE)
    } else if(!is.null(x)) {
      w01_ris_artifact_rv(x)
      w01_ris_check_active_rv(FALSE)
      w01_ris_status_rv("RIS ready: one record per unique canonical work.")
    }
  })

  output$w01_download_ris <- downloadHandler(
    filename=function() {
      id <- w01_final_export_identity()
      if(is.null(id)) return("unique_records.ris")
      paste0(id$review_id,"-unique-records.ris")
    },
    content=function(file) {
      req(authenticated(),w01_ris_artifact_rv())
      w01_ris_download_artifact(w01_ris_artifact_rv(),file)
    },
    contentType="application/x-research-info-systems"
  )

  output$w01_launch_results <- renderUI({
    msg <- w01_launch_error_rv()
    if(nzchar(msg)) return(tags$p(class="text-danger",paste("Deduplication status unavailable:",msg)))
    m <- w01_launch_snapshot_rv()
    if(is.null(m) || !identical(m$status,"completed")) return(NULL)
    if(!identical(m$conclusion,"success"))
      return(tags$p(class="text-danger","Deduplication failed. The existing review data have not been replaced."))
    result <- w01_launch_result_rv()
    if(is.null(result)) {
      pending <- w01_launch_pending_rv()
      if(!is.null(pending))
        return(tags$p(sprintf("Awaiting human review: %d uncertain duplicate cases. W01 will resume when decisions are returned.",pending$pending_human_cases)))
      return(tags$p("W01 completed its processing stage. Awaiting verified review-specific publication."))
    } # Never imply W01 finalisation succeeded before its restricted publication is verified.
    tags$div(
      tags$p(tags$strong("Deduplication completed")),
      tags$p(sprintf("%d duplicates removed",result$duplicates_removed)),
      tags$p(sprintf("%d unique records remain",result$unique_records)),
      tags$p(sprintf("%d uncertain cases require review",result$uncertain_cases))
    )
  })

  observeEvent(input$search_formal_start, {
    req(authenticated(),current_user())
    if (!identical(as.character(current_user()$role),"administrator") ||
        !session_can("run_search_scoping") || search_formal_launched_rv()) return()
    query <- as.character(input$search_formal_text %||% "")
    validation <- search_formal_live_validation()
    if (!isTRUE(validation$valid)) {
      search_formal_finalisation_message_rv(paste("Cannot start:",validation$message))
      return()
    }
    sources <- search_source_selected()
    if (!length(sources)) {
      search_formal_finalisation_message_rv("Cannot start: select at least one database.")
      return()
    }
    scope_issue <- search_formal_scoping_check(query,sources)
    if (!is.null(scope_issue)) {
      search_formal_finalisation_message_rv(scope_issue)
      return()
    }
    showModal(modalDialog(
      title="Confirm full database search",
      tags$p("Start a new full W00 search using the currently entered search string and these databases?"),
      tags$p(paste(vapply(sources,search_scope_pretty_source,character(1)),collapse=", ")),
      tags$p("This will call live database APIs. W01 will not be dispatched automatically."),
      easyClose=TRUE,
      footer=tagList(modalButton("Cancel"),
        actionButton("search_formal_confirm_start","Confirm and start search",class="btn-primary"))
    ))
  })

  observeEvent(input$search_formal_confirm_start, {
    req(authenticated(),current_user())
    removeModal()
    if (!identical(as.character(current_user()$role),"administrator") ||
        !session_can("run_search_scoping") || search_formal_launched_rv()) return()
    query <- as.character(input$search_formal_text %||% "")
    if (!isTRUE(search_boolean_validate(query)$valid)) {
      search_formal_finalisation_message_rv("Search string changed or is invalid. Review it and start again.")
      return()
    }
    sources <- search_source_selected()
    if (!length(sources)) {
      search_formal_finalisation_message_rv("Select at least one database.")
      return()
    }
    scope_issue <- search_formal_scoping_check(query,sources)
    if (!is.null(scope_issue)) {
      search_formal_finalisation_message_rv(scope_issue)
      return()
    }
    review <- tryCatch(search_formal_review_id(),error=function(e)e)
    if (inherits(review,"error")) {
      search_formal_finalisation_message_rv(paste("Cannot resolve review storage:",conditionMessage(review)))
      return()
    }
    existing <- tryCatch(search_strategy_versions(search_strategy_review_key(review)),error=function(e)e)
    if (inherits(existing,"error")) {
      search_formal_finalisation_message_rv(paste("Cannot read strategy history:",conditionMessage(existing)))
      return()
    }
    query_hash <- digest::digest(query,algo="sha256",serialize=FALSE)
    last_hash <- if(nrow(existing)) as.character(tail(existing$strategy_sha256,1L)) else ""
    # Reuse an already-finalised identical search; never append it twice.
    if (!identical(query_hash,last_hash)) {
      saved <- tryCatch({
        search_review_require_sole_admin(user_registry_rv(),as.character(current_user()$user_id))
        search_strategy_finalise(search_strategy_review_key(review),query,
          as.character(current_user()$user_id),expected_version=nrow(existing))
      },error=function(e)e)
      if (inherits(saved,"error")) {
        search_formal_finalisation_message_rv(paste("Cannot save strategy:",conditionMessage(saved)))
        return()
      }
    } else {
      authorised <- tryCatch(
        search_review_require_sole_admin(user_registry_rv(),as.character(current_user()$user_id)),
        error=function(e)e)
      if (inherits(authorised,"error")) {
        search_formal_finalisation_message_rv(paste("Not authorised:",conditionMessage(authorised)))
        return()
      }
    }
    request_id <- paste0("formal-",format(Sys.time(),tz="UTC",format="%Y%m%dT%H%M%SZ"),"-",
      substr(digest::digest(paste(review,query_hash,paste(sort(sources),collapse=","),sep="|"),
        algo="sha256",serialize=FALSE),1L,10L))
    outcome <- tryCatch(dispatch_w00_formal(query,sources,request_id),error=function(e)e)
    if (inherits(outcome,"error")) {
      search_formal_finalisation_message_rv(paste("W00 launch not confirmed:",conditionMessage(outcome),
        "Check GitHub Actions before trying again."))
    } else {
      search_formal_selected_sources_rv(sources)
      search_formal_request_id_rv(request_id)
      search_formal_run_id_rv("")
      search_formal_monitor_rv(NULL)
      search_formal_counts_rv(list())
      search_formal_seen_records_rv(character())
      persisted <- tryCatch({
        w00_monitor_record(search_strategy_review_key(review),request_id)
        TRUE
      },error=function(e)e)
      if(inherits(persisted,"error"))
        search_formal_finalisation_message_rv(paste("W00 dispatched, but monitoring could not be saved:",
          conditionMessage(persisted)))
      search_formal_launched_rv(TRUE)
      if(!inherits(persisted,"error")) search_formal_finalisation_message_rv(paste(
        "W00 full search dispatch accepted for",length(sources),"databases.",
        "Check LivingEvidenceMap Actions → Workflow 00 - Search orchestrator (main).",
        "No W01 dispatch requested."))
    }
  })

  # Restore the review's latest formal W00 request after Shiny reload.
  observe({
    req(authenticated())
    if(isTRUE(search_formal_monitor_restored_rv())) return()
    review <- tryCatch(search_formal_review_id(),error=function(e)e)
    if(inherits(review,"error")) return()
    saved <- tryCatch(w00_monitor_latest(search_strategy_review_key(review)),error=function(e)e)
    if(inherits(saved,"error")) {
      search_formal_finalisation_message_rv(paste("Unable to restore W00 monitoring:",conditionMessage(saved)))
      return()
    }
    if(is.null(saved)) {
      saved <- tryCatch(w00_formal_review_run(),error=function(e)e)
      if(inherits(saved,"error")) {
        search_formal_finalisation_message_rv(paste("Unable to restore W00 from review metadata:",
          conditionMessage(saved)))
        return()
      }
    }
    if(!is.null(saved) && !nzchar(search_formal_request_id_rv())) {
      search_formal_request_id_rv(saved$request_id)
      search_formal_run_id_rv(saved$run_id)
      search_formal_launched_rv(TRUE)
      search_formal_finalisation_message_rv(paste("Restored W00 GitHub run",saved$run_id,
        "from review-specific monitoring history."))
    }
    search_formal_monitor_restored_rv(TRUE)
  })

  # Read-only progress: recover latest single-review W00 dispatch after
  # reloading Shiny. Never launch or resume any workflow during polling.
  observe({
    req(authenticated())
    invalidateLater(10000,session)
    request_id <- as.character(search_formal_request_id_rv() %||% "")
    if (!nzchar(request_id)) return()
    run <- tryCatch(w00_formal_recent_run(request_id,
      as.character(search_formal_run_id_rv() %||% "")),error=function(e)e)
    if (inherits(run,"error")) {
      search_formal_monitor_rv(list(error=conditionMessage(run)))
      return()
    }
    if (is.null(run)) {
      search_formal_monitor_rv(list(waiting=TRUE))
      return()
    }
    id <- as.character(run$id %||% "")
    bound <- as.character(search_formal_run_id_rv() %||% "")
    if(nzchar(bound) && !identical(bound,id)) {
      search_formal_monitor_rv(list(error="Stored GitHub run ID differs from the matched W00 request."))
      return()
    }
    if(!nzchar(bound)) {
      search_formal_run_id_rv(id)
      saved <- tryCatch({
        review <- search_formal_review_id()
        w00_monitor_record(search_strategy_review_key(review),request_id,id)
        TRUE
      },error=function(e)e)
      if(inherits(saved,"error"))
        search_formal_finalisation_message_rv(paste("W00 progress available, but run ID could not be saved:",
          conditionMessage(saved)))
    }
    jobs <- tryCatch(w00_formal_run_jobs(id),error=function(e)e)
    if (inherits(jobs,"error")) {
      search_formal_monitor_rv(list(id=id,url=as.character(run$html_url %||% ""),
        error=conditionMessage(jobs)))
      return()
    }
    artifacts <- tryCatch(w00_scoping_run_artifacts(id),error=function(e)list())
    record_artifacts <- Filter(function(a) grepl(
      paste0("^workflow00-search-record-[a-z0-9_]+-",id,"$"),
      as.character(a$name %||% "")),artifacts)
    seen <- search_formal_seen_records_rv() %||% character()
    counts <- search_formal_counts_rv() %||% list()
    for(a in record_artifacts) {
      artifact_id <- as.character(a$id %||% "")
      if(!nzchar(artifact_id) || artifact_id %in% seen) next
      record <- tryCatch(read_w00_formal_record_artifact(a),error=function(e)NULL)
      if(is.null(record)) next
      counts[[record$source]] <- record
      seen <- c(seen,artifact_id)
    }
    search_formal_counts_rv(counts)
    search_formal_seen_records_rv(unique(seen))
    search_formal_monitor_rv(list(id=id,url=as.character(run$html_url %||% ""),
      status=as.character(run$status %||% ""),
      conclusion=as.character(run$conclusion %||% ""),
      jobs=jobs))
  })

  output$search_formal_progress <- renderUI({
    m <- search_formal_monitor_rv()
    if (is.null(m)) return(NULL)
    if (isTRUE(m$waiting)) return(tags$p(class="saved-note","Waiting for the matching W00 GitHub Actions run to appear…"))
    if (nzchar(as.character(m$error %||% ""))) return(
      tags$p(class="saved-note",paste("W00 progress temporarily unavailable:",m$error)))
    jobs <- m$jobs %||% list()
    # The orchestrator creates reusable source-child jobs after planning.
    source_jobs <- Filter(function(j) {
      n <- as.character(j$name %||% "")
      grepl("^(search|source-search|source /|search /|search-source|harvest)",n,ignore.case=TRUE) ||
        grepl(" / search$",n,ignore.case=TRUE)
    },jobs)
    # The single search-sources job is an orchestrator, not a database.
    source_jobs <- Filter(function(j) !identical(as.character(j$name %||% ""),"search-sources"),source_jobs)
    n_total <- length(source_jobs)
    n_done <- sum(vapply(source_jobs,function(j)
      identical(as.character(j$status %||% ""),"completed"),logical(1)))
    run_finished <- identical(as.character(m$status %||% ""),"completed")
    successful <- run_finished && identical(as.character(m$conclusion %||% ""),"success")
    pct <- if(successful) 100L else if(n_total) min(95L,round(10+85*n_done/n_total)) else if(run_finished) 0L else 5L
    failed_jobs <- Filter(function(j) identical(as.character(j$conclusion %||% ""),"failure"),jobs)
    failed_stage <- if(length(failed_jobs)) as.character(failed_jobs[[1L]]$name %||% "") else ""
    overview <- if(run_finished && !successful) paste0("W00 failed",if(nzchar(failed_stage)) paste0(" at ",failed_stage) else "",". No database retrieval completion can be reported.") else if(run_finished) "W00 completed successfully" else if(n_total) {
      sprintf("Database jobs: %d of %d finished",n_done,n_total)
    } else "W00 preparing search and database jobs…"
    div(class="mt-3",
      tags$h6("W00 search progress"),
      tags$p(class="small",overview),
      tags$p(class="small",sprintf("Overall progress: %d%% (%s)",pct,if(run_finished && !successful) "failed before completion" else if(successful) "complete" else "approximate")),
      tags$p(tags$a(href=m$url,target="_blank",rel="noopener noreferrer",
        paste("GitHub Actions run",m$id))),
      div(class="progress mb-3",div(class="progress-bar",
        role="progressbar",style=sprintf("width:%d%%",pct),
        `aria-valuenow`=pct,`aria-valuemin`=0,`aria-valuemax`=100)),
      if(n_total) tags$div(class="table-responsive",
        tags$table(class="table table-sm",
          tags$thead(tags$tr(tags$th("Database"),tags$th("Status"),tags$th("Records retrieved"))),
          tags$tbody(lapply(source_jobs,function(j) {
            st <- as.character(j$status %||% "queued")
            outcome <- as.character(j$conclusion %||% "")
            raw_name <- as.character(j$name %||% "")
            source_id <- sub("^search-sources \\(([^)]+)\\) / search$","\\1",raw_name)
            label <- if(!identical(source_id,raw_name)) search_scope_pretty_source(source_id) else raw_name
            status_label <- if(identical(st,"completed")) {
              if(identical(outcome,"success")) "Complete"
              else if(identical(outcome,"skipped")) "Skipped"
              else if(identical(outcome,"cancelled")) "Cancelled"
              else "Failed"
            } else if(identical(st,"in_progress")) "Searching…"
              else if(identical(st,"queued") || identical(st,"waiting")) "Queued"
              else tools::toTitleCase(gsub("_"," ",st,fixed=TRUE))
            tags$tr(tags$td(label),
              tags$td(status_label),
              tags$td({
                count <- search_formal_counts_rv()[[source_id]]
                if(is.null(count)) "—" else format(as.integer(count$downloaded),big.mark=",",scientific=FALSE)
              }))
          })),
          tags$tfoot(tags$tr(class="fw-semibold",
            tags$td("Total"),
            tags$td(sprintf("%d of %d databases with retrieval counts",
              sum(vapply(source_jobs,function(j) {
                source_id <- sub("^search-sources \\(([^)]+)\\) / search$","\\1",
                  as.character(j$name %||% ""))
                !is.null(search_formal_counts_rv()[[source_id]])
              },logical(1))),n_total)),
            tags$td({
              counts <- search_formal_counts_rv()
              sources <- vapply(source_jobs,function(j)
                sub("^search-sources \\(([^)]+)\\) / search$","\\1",
                  as.character(j$name %||% "")),character(1))
              values <- vapply(sources,function(source) {
                x <- counts[[source]]
                if(is.null(x)) return(NA_real_)
                as.numeric(x$downloaded)
              },numeric(1))
              if(all(is.na(values))) "—"
              else format(sum(values,na.rm=TRUE),big.mark=",",scientific=FALSE)
            })
          ))
        ))
      else if(run_finished) tags$p(class="saved-note",
        "No database jobs were started. Check the failed preparation step in GitHub Actions.")
      else tags$p(class="saved-note","Waiting for GitHub to create database jobs.")
    )
  })

  output$search_formal_finalisation_message <- renderText({
    search_formal_finalisation_message_rv()
  })

  output$search_formal_validation_message <- renderText({
    if (!nzchar(trimws(as.character(input$search_formal_text %||% "")))) return("Enter a search string.")
    search_formal_live_validation()$message
  })

  output$search_scope_progress <- renderUI({
    p <- search_scope_progress_rv() %||% list(completed=0L,total=0L,pct=0L,label="")
    total <- as.integer(p$total %||% 0L)
    completed <- as.integer(p$completed %||% 0L)
    pct <- as.integer(p$pct %||% 0L)
    if (pct < 1L && total < 1L && !nzchar(search_scope_run_id_rv())) return(NULL)
    label <- as.character(p$label %||% "")
    if (!nzchar(label)) {
      label <- if (total > 0L) {
        sprintf("Searching databases: %d of %d complete",completed,total)
      } else {
        "Preparing scoping search…"
      }
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

  observeEvent(input$edit_search_scope, {
    req(authenticated())
    if (!session_can("run_search_scoping")) return()
    if (nzchar(search_scope_request_id_rv())) return()
    versions <- search_scope_versions_rv()
    if (!length(versions)) return()
    next_version <- paste0("v", length(versions) + 1L)
    showModal(modalDialog(
      title=sprintf("Edit search string - create %s", next_version),
      textAreaInput(
        "search_scope_edit_text",
        "Search string",
        value=search_scope_string_rv(),
        rows=8,
        width="100%"
      ),
      tags$p(
        class="saved-note",
        "This creates a temporary scoping version without modifying any GitHub search string."
      ),
      easyClose=FALSE,
      footer=tagList(
        modalButton("Cancel"),
        actionButton("save_search_scope_edit","Save new version",class="btn-primary")
      )
    ))
  })

  observeEvent(input$save_search_scope_edit, {
    req(authenticated())
    if (!session_can("run_search_scoping")) return()
    proposed <- trimws(as.character(input$search_scope_edit_text %||% ""))
    if (!nzchar(proposed)) {
      showNotification("Search string cannot be empty.", type="error")
      return()
    }
    versions <- search_scope_versions_rv()
    if (!length(versions)) return()
    if (identical(proposed, as.character(tail(versions,1L)))) {
      showNotification("No change was made, so no new version was created.", type="message")
      return()
    }
    version <- paste0("v", length(versions) + 1L)
    versions[[version]] <- proposed
    search_scope_versions_rv(versions)
    search_scope_active_version_rv(version)
    search_scope_string_rv(proposed)
    search_scope_rows_rv(NULL)
    search_scope_seen_artifacts_rv(character())
    search_scope_job_sources_rv(character())
    search_scope_progress_rv(list(completed=0L,total=0L,pct=0L,label=""))
    search_scope_run_id_rv("")
    search_scope_request_id_rv("")
    search_scope_running_version_rv("")
    search_scope_status_rv(sprintf(
      "%s saved temporarily in this app session. Run scoping to compare it with the original.",
      version
    ))
    removeModal()
  })

  output$search_scope_report_download <- renderUI({
    results <- search_scope_results_rv() %||% list()
    active_rows <- search_scope_rows_rv()
    has_completed <- any(vapply(results, search_scope_counts_resolved, logical(1)))
    if (!has_completed && !search_scope_counts_resolved(active_rows)) return(NULL)
    downloadButton(
      "download_search_scope_report",
      "Download scoping report",
      class="btn-outline-secondary btn-sm"
    )
  })

  output$download_search_scope_report <- downloadHandler(
    filename=function() sprintf("search-scoping-report-%s.pdf",format(Sys.Date(),"%Y-%m-%d")),
    content=function(file) {
      versions <- search_scope_versions_rv()
      req(length(versions) > 0L)
      results <- search_scope_results_rv() %||% list()
      active_version <- search_scope_active_version_rv()
      if (!is.null(search_scope_rows_rv())) results[[active_version]] <- search_scope_rows_rv()

      catalogue <- search_scope_catalogue_rv()
      source_slugs <- if (!is.null(catalogue) && nrow(catalogue)) as.character(catalogue$source_slug) else {
        unique(unlist(lapply(results,function(x) if(is.null(x)) character() else as.character(x$source_slug)),use.names=FALSE))
      }
      source_labels <- if (!is.null(catalogue) && nrow(catalogue)) {
        stats::setNames(as.character(catalogue$source),as.character(catalogue$source_slug))
      } else {
        vals <- unlist(lapply(results,function(x) {
          if(is.null(x) || !nrow(x)) return(character())
          stats::setNames(as.character(x$source),as.character(x$source_slug))
        }),use.names=TRUE)
        vals[!duplicated(names(vals))]
      }

      version_hits <- lapply(names(versions),function(v) {
        rows <- results[[v]]
        out <- stats::setNames(rep(NA_integer_,length(source_slugs)),source_slugs)
        if (!is.null(rows) && nrow(rows) && "source_slug" %in% names(rows)) {
          idx <- match(source_slugs,as.character(rows$source_slug))
          ok <- !is.na(idx)
          out[ok] <- suppressWarnings(as.integer(rows$hits[idx[ok]]))
        }
        out
      })
      names(version_hits) <- names(versions)
      totals <- vapply(version_hits,function(x) if(all(is.na(x))) NA_integer_ else sum(x,na.rm=TRUE),integer(1))
      baseline_total <- totals[["original"]]

      grDevices::pdf(file,width=8.27,height=11.69,onefile=TRUE,useDingbats=FALSE)
      on.exit(grDevices::dev.off(),add=TRUE)

      graphics::plot.new()
      graphics::text(0.06,0.95,"Search scoping report",adj=c(0,1),family="sans",font=2,cex=1.45)
      graphics::text(
        0.06,0.91,
        sprintf("Scoping date: %s",format(Sys.Date(),"%d-%m-%Y")),
        adj=c(0,1),family="sans",cex=0.9
      )
      # Fit text to the actual graphics device width, rather than an
      # estimated character count (which can overflow in the PDF).
      fit_pdf_lines <- function(text, max_width=0.88, family="sans", cex=0.82) {
        words <- unlist(strsplit(as.character(text), "[[:space:]]+"))
        words <- words[nzchar(words)]
        lines <- character()
        line <- ""
        for (word in words) {
          proposal <- if(nzchar(line)) paste(line,word) else word
          if (graphics::strwidth(proposal,units="user",family=family,cex=cex) <= max_width) {
            line <- proposal
          } else {
            if(nzchar(line)) lines <- c(lines,line)
            line <- ""
            # Break exceptionally long unspaced tokens as well.
            for(ch in strsplit(word,"",fixed=TRUE)[[1L]]) {
              if(nzchar(line) &&
                 graphics::strwidth(paste0(line,ch),units="user",family=family,cex=cex)>max_width) {
                lines <- c(lines,line)
                line <- ""
              }
              line <- paste0(line,ch)
            }
          }
        }
        if(nzchar(line)) lines <- c(lines,line)
        lines
      }
      intro <- fit_pdf_lines(
        "This report records count-only scoping searches. No bibliographic records or raw API responses were retained.",
        family="sans",cex=0.82
      )
      intro_y <- 0.865
      for(line in intro) {
        graphics::text(0.06,intro_y,line,adj=c(0,1),family="sans",cex=0.82)
        intro_y <- intro_y-0.025
      }
      y <- intro_y-0.055
      original <- as.character(versions[["original"]])

      for (v in names(versions)) {
        heading <- if (identical(v,"original")) "Search string original" else sprintf("Search string %s",v)
        if (y < 0.18) {
          graphics::plot.new()
          y <- 0.94
        }
        graphics::text(0.06,y,heading,adj=c(0,1),family="sans",font=2,cex=1.02)
        y <- y-0.035
        q_lines <- fit_pdf_lines(versions[[v]],family="mono",cex=0.68)
        for (line in q_lines) {
          if (y < 0.10) {
            graphics::plot.new()
            graphics::text(0.06,0.95,paste(heading,"(continued)"),adj=c(0,1),family="sans",font=2,cex=0.95)
            y <- 0.90
          }
          graphics::text(0.06,y,line,adj=c(0,1),family="mono",cex=0.68)
          y <- y-0.022
        }
        if (!identical(v,"original")) {
          diff_lines <- fit_pdf_lines(search_scope_change_summary(original,versions[[v]]),family="sans",cex=0.72)
          y <- y-0.006
          for (line in diff_lines) {
            graphics::text(0.06,y,line,adj=c(0,1),family="sans",cex=0.72,col="red3")
            y <- y-0.022
          }
        }
        y <- y-0.035
      }

      graphics::plot.new()
      graphics::text(0.06,0.95,"Database counts",adj=c(0,1),family="sans",font=2,cex=1.15)

      version_names <- names(versions)
      n_versions <- length(version_names)
      db_x <- 0.06
      status_x <- 0.88
      result_left <- 0.57
      result_right <- 0.84
      result_x <- if (n_versions == 1L) 0.76 else seq(result_left,result_right,length.out=n_versions)
      graphics::text(db_x,0.91,"Database",adj=c(0,1),family="sans",font=2,cex=0.78)
      for (j in seq_along(version_names)) {
        graphics::text(result_x[[j]],0.91,version_names[[j]],adj=c(0.5,1),family="sans",font=2,cex=0.76)
      }
      graphics::text(status_x,0.91,"Status",adj=c(0,1),family="sans",font=2,cex=0.76)
      graphics::segments(0.06,0.885,0.96,0.885)
      y <- 0.865

      active_rows <- results[[active_version]]
      for (slug in source_slugs) {
        label <- as.character(source_labels[[slug]] %||% search_scope_pretty_source(slug))
        label_lines <- strwrap(label,width=55)
        if (y < 0.10) {
          graphics::plot.new()
          graphics::text(0.06,0.95,"Database counts (continued)",adj=c(0,1),family="sans",font=2,cex=1.0)
          y <- 0.90
        }
        graphics::text(db_x,y,label_lines[[1L]],adj=c(0,1),family="sans",cex=0.70)
        for (j in seq_along(version_names)) {
          val <- version_hits[[version_names[[j]]]][[slug]]
          txt <- if(is.na(val)) "-" else format(val,big.mark=",",scientific=FALSE)
          graphics::text(result_x[[j]],y,txt,adj=c(0.5,1),family="sans",cex=0.70)
        }
        st <- ""
        if (!is.null(active_rows) && nrow(active_rows) && "source_slug" %in% names(active_rows)) {
          hit <- which(as.character(active_rows$source_slug)==slug)
          if(length(hit)) st <- search_scope_display_status(active_rows$status[[hit[[1L]]]])
        }
        graphics::text(status_x,y,st,adj=c(0,1),family="sans",cex=0.68)
        if (length(label_lines)>1L) {
          for (extra in label_lines[-1L]) {
            y <- y-0.019
            graphics::text(db_x,y,extra,adj=c(0,1),family="sans",cex=0.70)
          }
        }
        y <- y-0.027
      }

      graphics::segments(0.06,y+0.009,0.96,y+0.009)
      total_y <- y-0.008
      graphics::text(db_x,total_y,"Total",adj=c(0,1),family="sans",font=2,cex=0.78)
      for (j in seq_along(version_names)) {
        v <- version_names[[j]]
        total <- totals[[v]]
        txt <- if(is.na(total)) "-" else format(total,big.mark=",",scientific=FALSE)
        graphics::text(result_x[[j]],total_y,txt,adj=c(0.5,1),family="sans",font=2,cex=0.76)
        if (!identical(v,"original") && !is.na(total) && !is.na(baseline_total)) {
          delta <- total-baseline_total
          delta_txt <- sprintf("(%s%s)",if(delta>=0) "+" else "",format(delta,big.mark=",",scientific=FALSE))
          graphics::text(result_x[[j]],total_y-0.022,delta_txt,adj=c(0.5,1),family="sans",cex=0.66,col="red3")
        }
      }
      graphics::text(
        0.06,0.045,
        "Counts are totals reported by each live-searchable database/API at the time of each scoping run.",
        adj=c(0,0),family="sans",cex=0.7
      )
    },
    contentType="application/pdf"
  )

  output$search_scope_table <- renderUI({
    versions <- search_scope_versions_rv()
    if (!length(versions)) return(NULL)
    results <- search_scope_results_rv() %||% list()
    active_version <- search_scope_active_version_rv()
    active_rows <- search_scope_rows_rv()
    if (!is.null(active_rows)) results[[active_version]] <- active_rows

    catalogue <- search_scope_catalogue_rv()
    job_sources <- search_scope_job_sources_rv() %||% character()
    source_slugs <- if (!is.null(catalogue) && nrow(catalogue)) {
      as.character(catalogue$source_slug)
    } else if (length(job_sources)) {
      as.character(job_sources)
    } else {
      unique(unlist(lapply(results,function(x) if(is.null(x)) character() else as.character(x$source_slug)),use.names=FALSE))
    }
    if (!length(source_slugs)) return(NULL)
    source_slugs <- source_slugs[order(tolower(vapply(source_slugs,search_scope_pretty_source,character(1))))]
    baseline_total <- search_scope_total_for_version("original")

    value_cell <- function(version, slug) {
      rows <- results[[version]]
      if (is.null(rows) || !nrow(rows) || !"source_slug" %in% names(rows)) return("—")
      hit <- which(as.character(rows$source_slug)==slug)
      if (!length(hit)) return("—")
      value <- suppressWarnings(as.integer(rows$hits[[hit[[1L]]]]))
      if (is.na(value)) "—" else format(value,big.mark=",",scientific=FALSE)
    }

    total_cell <- function(version) {
      total <- search_scope_total_for_version(version)
      if (is.na(total)) return("—")
      main <- tags$strong(format(total,big.mark=",",scientific=FALSE))
      if (identical(version,"original") || is.na(baseline_total)) return(main)
      delta <- total-baseline_total
      tagList(
        main,
        tags$div(
          class="small text-danger",
          sprintf("(%s%s)",if(delta>=0) "+" else "",format(delta,big.mark=",",scientific=FALSE))
        )
      )
    }

    tags$div(
      class="table-responsive mt-2",
      tags$table(
        class="table table-sm align-middle mb-0",
        tags$thead(tags$tr(
          tags$th("Database"),
          lapply(names(versions),function(v) tags$th(class="text-end",v)),
          tags$th("Status")
        )),
        tags$tbody(tagList(
          lapply(source_slugs,function(slug) {
            st <- ""
            rows <- results[[active_version]]
            if (!is.null(rows) && nrow(rows) && "source_slug" %in% names(rows)) {
              hit <- which(as.character(rows$source_slug)==slug)
              if(length(hit)) st <- search_scope_display_status(rows$status[[hit[[1L]]]])
            }
            tags$tr(
              tags$td(search_scope_pretty_source(slug)),
              lapply(names(versions),function(v) tags$td(class="text-end",value_cell(v,slug))),
              tags$td(st)
            )
          }),
          tags$tr(
            class="fw-semibold border-top",
            tags$td("Total"),
            lapply(names(versions),function(v) tags$td(class="text-end",total_cell(v))),
            tags$td("")
          )
        ))
      )
    )
  })

  output$search_scope_run_control <- renderUI({
    req(authenticated())
    if (!identical(as.character(current_user()$role),"administrator")) return(NULL)
    if (!isTRUE(search_boolean_validate(search_scope_string_rv())$valid)) return(NULL)
    actionButton("run_search_scope",
      sprintf("Run scoping search %s",search_scope_active_version_rv()),
      class="btn-primary btn-sm")
  })

  output$search_scope_validation_message <- renderText({
    if (nzchar(search_scope_request_id_rv())) return("")
    query <- as.character(search_scope_string_rv() %||% "")
    if (!nzchar(trimws(query))) return("Enter a scoping search string to enable Run scoping search.")
    result <- search_boolean_validate(query)
    if (isTRUE(result$valid)) "Basic Boolean syntax valid. Database-specific syntax is checked when scoping starts." else result$message
  })

  observeEvent(input$run_search_scope, {
    req(authenticated())
    if (!session_can("control_workflows")) {
      search_scope_status_rv("Search scoping permission is required.")
      return()
    }
    if (nzchar(search_scope_request_id_rv())) return()
    query <- as.character(input$search_scope_input_text %||% search_scope_string_rv())
    if (!isTRUE(search_boolean_validate(query)$valid)) {
      search_scope_status_rv(paste("Invalid scoping search:",search_boolean_validate(query)$message))
      return()
    }
    search_scope_string_rv(query)
    if (!length(search_scope_versions_rv()))
      search_scope_versions_rv(stats::setNames(query,"original"))

    stamp <- format(Sys.time(),tz="UTC",format="%Y%m%dT%H%M%SZ")
    request_id <- paste0(
      "shiny-",stamp,"-",
      substr(digest::digest(
        paste(stamp,session_reviewer_id(),search_scope_string_rv(),sep="|"),
        algo="sha256",serialize=FALSE
      ),1L,10L)
    )
    catalogue <- search_scope_catalogue_rv()
    if (is.null(catalogue) || !nrow(catalogue)) {
      catalogue <- tryCatch(read_w00_scoping_source_catalogue(), error=function(e) NULL)
      if (!is.null(catalogue) && nrow(catalogue)) search_scope_catalogue_rv(catalogue)
    }
    if (!is.null(catalogue) && nrow(catalogue)) {
      search_scope_job_sources_rv(as.character(catalogue$source_slug))
      search_scope_rows_rv(data.frame(
        source_slug=as.character(catalogue$source_slug),
        source=as.character(catalogue$source),
        hits=NA_integer_,
        status="Queued",
        stringsAsFactors=FALSE
      ))
      search_scope_progress_rv(list(completed=0L,total=nrow(catalogue),pct=2L,label="Preparing scoping search…"))
    } else {
      search_scope_rows_rv(NULL)
      search_scope_job_sources_rv(character())
      search_scope_progress_rv(list(completed=0L,total=0L,pct=2L,label="Preparing scoping search…"))
    }
    search_scope_seen_artifacts_rv(character())
    search_scope_run_id_rv("")
    running_version <- search_scope_active_version_rv()
    search_scope_running_version_rv(running_version)
    search_scope_status_rv(sprintf("Dispatching count-only scoping search %s…",running_version))

    ok <- tryCatch({
      dispatch_w00_scoping(request_id, search_string=search_scope_string_rv())
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
      search_scope_progress_rv(list(
        completed=0L,
        total=as.integer((search_scope_progress_rv() %||% list(total=0L))$total %||% 0L),
        pct=4L,
        label="Preparing scoping search…"
      ))
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
    if (length(sources) && is.null(search_scope_catalogue_rv())) search_scope_job_sources_rv(sources)

    completed <- sum(vapply(count_jobs,function(j) identical(as.character(j$status %||% ""),"completed"),logical(1)))
    catalogue <- search_scope_catalogue_rv()
    total <- if (!is.null(catalogue) && nrow(catalogue)) nrow(catalogue) else length(count_jobs)

    prepare_jobs <- Filter(function(j) identical(as.character(j$name %||% ""),"prepare"), jobs)
    merge_jobs <- Filter(function(j) identical(as.character(j$name %||% ""),"merge"), jobs)
    prepare_status <- if (length(prepare_jobs)) as.character(prepare_jobs[[1L]]$status %||% "") else ""
    merge_status <- if (length(merge_jobs)) as.character(merge_jobs[[1L]]$status %||% "") else ""

    if (!length(count_jobs)) {
      pct <- if (identical(prepare_status,"in_progress")) 7L else if (identical(prepare_status,"completed")) 10L else 5L
      progress_label <- "Preparing scoping search…"
    } else if (total > 0L && completed < total) {
      pct <- 10L + round(80L * completed / total)
      progress_label <- sprintf("Searching databases: %d of %d complete",completed,total)
    } else if (total > 0L && completed >= total) {
      pct <- if (identical(merge_status,"in_progress")) 96L else 92L
      progress_label <- "Database counts complete; awaiting verified GitHub report…"
    } else {
      pct <- 10L
      progress_label <- "Preparing scoping search…"
    }
    search_scope_progress_rv(list(completed=completed,total=total,pct=pct,label=progress_label))

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
        source=search_scope_pretty_source(slug),
        hits=suppressWarnings(as.integer(row$hits %||% NA_integer_)),
        status=search_scope_display_status(row$status),
        stringsAsFactors=FALSE
      ))
      seen <- c(seen,aid)
    }
    search_scope_seen_artifacts_rv(unique(seen))

    if (!is.null(current)) {
      catalogue <- search_scope_catalogue_rv()
      order_slugs <- if (!is.null(catalogue) && nrow(catalogue)) {
        as.character(catalogue$source_slug)
      } else sources
      if (nrow(current)) {
        current <- current[order(tolower(current$source)),,drop=FALSE]
        rownames(current) <- NULL
      }
    }
    if (!is.null(current)) search_scope_rows_rv(current)

    run_status <- as.character(run$status %||% "")
    run_conclusion <- as.character(run$conclusion %||% "")
    if (identical(run_status,"completed")) {
      final_art <- Filter(function(a) startsWith(as.character(a$name %||% ""),"workflow00-search-scoping-"), artifacts)
      if (length(final_art)) {
        final_rows <- tryCatch(read_w00_scoping_final_artifact(final_art[[1L]]),error=function(e) NULL)
        if (!is.null(final_rows)) {
          final_rows$status <- vapply(final_rows$status,search_scope_display_status,character(1))
          if ("source_slug" %in% names(final_rows)) {
            final_rows$source <- vapply(final_rows$source_slug,search_scope_pretty_source,character(1))
          }
          final_rows <- final_rows[order(tolower(final_rows$source)),,drop=FALSE]
          rownames(final_rows) <- NULL
          search_scope_rows_rv(final_rows)
          completed_version <- as.character(search_scope_running_version_rv() %||% search_scope_active_version_rv())
          if (!nzchar(completed_version)) completed_version <- search_scope_active_version_rv()
          results <- search_scope_results_rv() %||% list()
          results[[completed_version]] <- final_rows
          search_scope_results_rv(results)
          saved_scope <- tryCatch({
            review <- search_formal_review_id()
            versions <- search_scope_versions_rv()
            scoped_query <- as.character(versions[[completed_version]] %||% search_scope_string_rv())
            search_scoping_save(search_strategy_review_key(review),run_id,scoped_query,final_rows)
            TRUE
          },error=function(e)e)
          if (inherits(saved_scope,"error"))
            search_scope_status_rv(paste("Scoping completed, but persistence failed:",
              conditionMessage(saved_scope)))
          failed_n <- sum(final_rows$status=="Failed")
          search_scope_progress_rv(list(completed=nrow(final_rows),total=nrow(final_rows),pct=100L,label="Scoping search complete"))
          if (!inherits(saved_scope,"error")) search_scope_status_rv(if (failed_n) {
            sprintf("Scoping search complete with %d failed database%s.",failed_n,if(failed_n==1L)"" else "s")
          } else "Scoping search complete.")
        } else {
          search_scope_status_rv("Scoping search finished, but the final count artefact could not be read.")
        }
      } else {
        search_scope_status_rv(paste0(
          "Scoping search finished",
          if(nzchar(run_conclusion)) paste0(" with status: ",run_conclusion) else "."
        ))
      }
      search_scope_request_id_rv("")
      search_scope_running_version_rv("")
    } else {
      if (total > 0L && completed >= total && search_scope_counts_resolved(current)) {
        search_scope_status_rv("All database searches complete. Finalising GitHub report artefact…")
      } else {
        search_scope_status_rv(if(total > 0L) sprintf("Scoping search running: %d of %d databases complete.",completed,total) else "Preparing database count jobs…")
      }
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

  # Prior W04 subset metrics are stable across unrelated assignment updates.
  # Do not perform Google Sheets reads inside every summary re-render.
  w04_summary_history_cache <- new.env(parent=emptyenv())
  output$administration_load_status <- renderText({
    if(!isTRUE(input$open_administration_assignments > 0L))
      return("Open to load administrator controls; historical assignment details are loaded only when needed.")
    "Administration controls requested. Historical assignment data may take time to load."
  })
  # Independent comparison renderer: avoids rebuilding all assignment sections
  # or reading historical assignment progress when raters/model selections change.
  w04_consistency_panel <- reactive({
    req(authenticated(),session_can("manage_assignments"))
    has_w04_batch <- nzchar(as.character(w04_batch_id_rv())) &&
      length(w04_all_cases_rv()) > 0L
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

    snapshot <- w04_consistency_subset_snapshot_rv()
    w04_outcomes_now <- if(is.null(snapshot)) w04_blind_outcomes() else snapshot$outcomes
    available_raters <- w04_available_consistency_raters(w04_outcomes_now)
    rater_choices <- stats::setNames(
      available_raters,
      vapply(available_raters, reviewer_name, character(1))
    )
    input_selection <- input$w04_consistency_raters
    if (!is.null(snapshot) && nzchar(as.character(snapshot$model_run_id %||% "")) &&
        (!length(intersect(as.character(input_selection %||% character()),"model")))) {
      human_defaults <- setdiff(available_raters,"model")
      match_matt <- Filter(function(uid)grepl("matt|matthew",reviewer_name(uid),ignore.case=TRUE),human_defaults)
      selected_raters <- c(if(length(match_matt)) match_matt[[1L]] else if(length(human_defaults)) human_defaults[[1L]] else character(),"model")
    } else if (is.null(input_selection)) {
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
    ) if(is.null(snapshot)) w04_selected_human_consistency_scope() else NULL else NULL
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

    # Comparison controls must remain accessible even when W04 queues are
    # deferred, so an administrator can load any historical human subset
    # and compare it with a completed model run on demand.
    panel <- if (session_can("manage_assignments")) {
      tags$details(
        class="assignment-workflow assignment-submenu wf-w04 mb-2",
        `data-accordion-key`="w04-consistency-checking",
        ontoggle="if (this.open && window.Shiny) Shiny.setInputValue('w04_consistency_opened', true, {priority: 'event'});",
        tags$summary(
          div(
            class="d-inline-flex flex-wrap align-items-center gap-2",
            tags$strong("Consistency checking"),
            tags$span(class="task-badge","Administrator only"),
            tags$span(
              class="text-secondary small",
              if (!has_w04_batch && is.null(snapshot)) {
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
          tags$h6("Human-screened subset"),
          tags$p(class="text-secondary small","Choose a subset once. Associated model runs are discovered automatically."),
          uiOutput("w04_consistency_subset_selector"),
          textOutput("w04_consistency_subset_status"),
          if(!is.null(snapshot)) tagList(
            tags$hr(),
            tags$h6("Completed model run (latest selected automatically)"),
            if(length(w04_consistency_model_runs_rv())) uiOutput("w04_consistency_model_selector"),
            textOutput("w04_consistency_model_status")
          ),
          if(!is.null(snapshot) && nzchar(as.character(snapshot$model_run_id %||% "")))
            tags$h6("Compare and save"),
          tags$p(class="text-secondary small",
            "The statistics below are calculated only from the selected W04 subset and its loaded model run. Historic review-wide kappa figures are not part of this comparison."),
          if (!has_w04_batch && is.null(snapshot)) {
            tags$div(
              class="text-secondary small",
              "Load a W04 subset above to calculate subset-specific agreement and kappa."
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
          if(!is.null(snapshot) && nzchar(as.character(snapshot$model_run_id %||% "")) ||
             is.null(snapshot)) checkboxGroupInput(
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
              if(is.null(snapshot) || nzchar(as.character(snapshot$model_run_id %||% ""))) div(
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
              tags$div(
                class="text-secondary small mt-2",
                sprintf(
                  "Current batch: %s · queue SHA: %s",
                  w04_batch_id_rv(),
                  substr(w04_queue_sha_rv(),1L,16L)
                )
              )
            )
          },
          tags$hr(),
          tags$h5("Saved comparisons and conflict adjudication"),
          tags$p(class="saved-note",
            "Saved comparisons and their disagreements remain available even when the current rater selection is incomplete. The W04 decision count above is a separate manual-screening status."),
          uiOutput("w04_consistency_history"),
          uiOutput("w04_model_conflict_loader")
          )
        )
      )
    } else NULL

    panel
  })

  admin_assignment_panels <- reactive({
    req(authenticated())
    if (!session_can("manage_assignments")) return(NULL)
    req(
      as.character(input$configure_workflow_tab %||% "") %in% c("W01","W02","W08","System") ||
      (identical(input$configure_workflow_tab,"W04") &&
       identical(input$w04_workspace_tab,"Subsets & tasks"))
    )

    all_assignments <- assignment_registry_rv()
    has_w01_batch <- nzchar(as.character(batch_id_rv())) && length(w01_all_cases_rv()) > 0L && length(w01_active_assignment_events()) < length(w01_all_cases_rv())
    w08b_queue <- w08b_review_queue_rv()
    has_w08b_batch <- !is.null(w08b_queue) && length(w08b_queue$cases) > 0L
    has_w02_batch <- nzchar(as.character(w02_batch_id_rv())) && length(w02_all_cases_rv()) > 0L
    has_w04_batch <- nzchar(as.character(w04_batch_id_rv())) && length(w04_all_cases_rv()) > 0L
    has_w04_resolution_batch <- nzchar(as.character(w04_resolution_batch_id_rv())) && length(w04_resolution_all_cases_rv()) > 0L
    has_w04_conflict_batch <- nzchar(as.character(w04_active_conflict_batch_id())) && length(w04_all_conflict_cases()) > 0L
    has_w08_batch <- nzchar(as.character(w08_batch_id_rv())) && length(w08_all_cases_rv()) > 0L
    task_labels <- c(
      deduplication = "Deduplication",
      citation_deduplication = "Citation deduplication",
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
      if (identical(workflow, "01") && identical(task_type, "citation_deduplication")) {
        return(if (has_w08b_batch) w08b_queue$batch_id else "")
      }
      if (identical(workflow, "01") && identical(task_type, "deduplication")) {
        return(if (has_w01_batch) as.character(batch_id_rv()) else "")
      }
      if (identical(workflow, "02") && identical(task_type, "enrichment")) {
        return(if (has_w02_batch) as.character(w02_batch_id_rv()) else "")
      }
      if (identical(workflow, "04") && identical(task_type, "manual_screening")) {
        # Registered human subsets may have assignments even when the admin
        # has no personal queue loaded. Preserve every batch in the summary.
        return(NA_character_)
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

    # Only compute historical progress for the administrator's selected
    # workflow. W04 historical subsets can otherwise cause repeated Sheets
    # reads even when the user is administering W01, W02 or W08.
    active_scope <- switch(as.character(input$configure_workflow_tab %||% ""),
      W01="01",W02="02",W04="04",W08="08",System="")
    grouped <- Filter(function(a) {
      if(!length(a))return(FALSE)
      identical(normalise_assignment_row(a[[1L]])$workflow,active_scope)
    },grouped)

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
        identical(z$task_type, "manual_screening")
      ) {
        if(identical(z$batch_id,w04_batch_id_rv())) return(w04_active_assignment_events())
        cache_key <- as.character(z$batch_id)
        if(exists(cache_key,envir=w04_summary_history_cache,inherits=FALSE))
          return(get(cache_key,envir=w04_summary_history_cache,inherits=FALSE))
        subset_id <- sub("^w04-human-","",cache_key)
        loaded <- tryCatch({
          meta <- w04_prompt_review_metadata()
          queue <- w04_subset_queue_read(subset_id,meta$review_id)
          if(is.null(queue)) return(NULL)
          all <- active_sheet_w04_decisions()
          w04_filter_batch_decisions(all,queue$queue_sha256)
        },error=function(e)NULL)
        # Cache only successful reads. Missing queues or Sheets errors remain
        # eligible for retry rather than being recorded as empty results.
        if(is.null(loaded)) return(list())
        assign(cache_key,loaded,envir=w04_summary_history_cache)
        return(loaded)
      }
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

    if (has_w01_batch || !has_w08b_batch) add_empty_group(
      "01","deduplication",
      if (has_w01_batch) batch_id_rv() else "no-active-queue",
      ASSIGNMENT_MODES[["shared_work_pool"]]
    )
    if (has_w08b_batch) add_empty_group(
      "01","citation_deduplication",w08b_queue$batch_id,
      ASSIGNMENT_MODES[["shared_work_pool"]]
    )
    add_empty_group(
      "02","enrichment",
      if (has_w02_batch) w02_batch_id_rv() else "no-active-queue",
      ASSIGNMENT_MODES[["shared_work_pool"]]
    )
    # A newly prepared W04 subset needs its own assignment panel even
    # when other registered subsets already have reviewer assignments.
    # add_empty_group() checks the exact batch ID, so existing batches remain.
    if (has_w04_batch) {
      add_empty_group(
        "04","manual_screening",w04_batch_id_rv(),
        ASSIGNMENT_MODES[["independent_blind_review"]]
      )
    } else if (!any(vapply(group_progress, function(g)
      identical(g$meta$workflow,"04") &&
      identical(g$meta$task_type,"manual_screening"), logical(1)))) {
      add_empty_group(
        "04","manual_screening","no-active-queue",
        ASSIGNMENT_MODES[["independent_blind_review"]]
      )
    }
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
        citation_deduplication=2L,
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

    active_sources_for_summary <- record_table_source_list()
    total_assigned <- sum(vapply(
      active_sources_for_summary,
      function(src) length(active_assignments(src$assignments %||% list())),
      integer(1)
    ))
    total_completed_cases <- sum(vapply(
      active_sources_for_summary,
      function(src) length(record_table_completed_case_ids(src)),
      integer(1)
    ))
    total_outstanding_cases <- sum(vapply(
      active_sources_for_summary,
      function(src) length(record_table_outstanding_case_ids(src)),
      integer(1)
    ))

    assignment_manager_ui <- function(z) {
      cfg <- if (
        identical(z$workflow, "01") &&
        identical(z$task_type, "deduplication") &&
        identical(z$batch_id, batch_id_rv())
      ) {
        list(prefix="w01", workflow="01", task_type="deduplication", batch_id=batch_id_rv(),
             events=w01_active_assignment_events(), label="W01")
      } else if (
        identical(z$workflow, "01") &&
        identical(z$task_type, "citation_deduplication") &&
        has_w08b_batch &&
        identical(z$batch_id,w08b_queue$batch_id)
      ) {
        list(prefix="w08b",workflow="01",task_type="citation_deduplication",
          batch_id=w08b_queue$batch_id,events=w08b_review_decisions_rv(),label="W08b citation")
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
        if(identical(z$workflow,"04") &&
           identical(z$task_type,"manual_screening") &&
           startsWith(z$batch_id,"w04-human-")) {
          return(tags$p(class="saved-note",
            "Registered subset assignments are shown above. To manage this batch, prepare or load its subset from Configure review."))
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
            tagList(
              uiOutput("w04_assignment_subset_selector"),
              radioButtons(
              "w04_review_mode",
              "Manual screening purpose",
              choices = c(
                "Reviewer consistency · same random records to every selected reviewer" = "reviewer_consistency",
                "Build validation set · split random records across selected reviewers" = "validation_set"
              ),
              selected = "reviewer_consistency"
              )
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

    # Resolve W04 selection before building the assignment cards. The selector
    # within each card requires these values during lapply evaluation.
    manual_groups <- Filter(function(g)
      identical(g$meta$workflow,"04") &&
      identical(g$meta$task_type,"manual_screening") &&
      startsWith(as.character(g$meta$batch_id),"w04-human-"),group_progress)
    manual_ids <- vapply(manual_groups,function(g)as.character(g$meta$batch_id),character(1))
    chosen_manual <- as.character(input$w04_admin_manual_subset %||% "")
    if(!chosen_manual %in% manual_ids)
      chosen_manual <- if(length(manual_ids)) manual_ids[[1L]] else ""

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
        identical(z$workflow,"01") && identical(z$task_type,"citation_deduplication") &&
        has_w08b_batch && identical(z$batch_id,w08b_queue$batch_id)
      ) w08b_queue$cases else if (
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
      src_now <- record_table_source(z$workflow,z$task_type,z$batch_id)
      completed_case_ids <- if (length(src_now)) record_table_completed_case_ids(src_now[[1L]]) else character()
      # Registered W04 subset queues need not be loaded into the admin session.
      # Always show actual assigned case counts from the assignment registry.
      if(identical(z$workflow,"04") && identical(z$task_type,"manual_screening") &&
         !length(all_case_ids)) all_case_ids <- assigned_case_ids
      case_total <- length(all_case_ids[nzchar(all_case_ids)])
      case_completed <- sum(all_case_ids %in% completed_case_ids)
      case_outstanding <- max(0L,case_total-case_completed)
      formal_assignments <- length(active_group_assignments)
      unassigned <- if (length(all_case_ids)) {
        sum(nzchar(all_case_ids) & !all_case_ids %in% assigned_case_ids & !all_case_ids %in% completed_case_ids)
      } else 0L

      rows <- lapply(p$by_user, function(x) {
        pct <- round(100 * x$progress)
        role_label <- if (identical(x$role, "administrator")) "Administrator" else "Reviewer"
        tags$tr(
          tags$td(x$display_name),
          tags$td(role_label),
          tags$td(record_table_link(x$assigned,z$workflow,z$task_type,z$batch_id,"assignments",x$user_id,paste(workflow_label,task_label,x$display_name,"Assigned",sep=" · "))),
          tags$td(record_table_link(x$completed,z$workflow,z$task_type,z$batch_id,"completed",x$user_id,paste(workflow_label,task_label,x$display_name,"Completed",sep=" · "))),
          tags$td(record_table_link(x$resolved_elsewhere,z$workflow,z$task_type,z$batch_id,"closed",x$user_id,paste(workflow_label,task_label,x$display_name,"Resolved elsewhere",sep=" · "))),
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
          paste0("wf-w",z$workflow),
          if(identical(z$workflow,"01")) "w01-administration-subsection" else ""
        ),
        open = if(identical(z$workflow,"01")) "open" else NULL,
        `data-accordion-key` = paste0("workflow-", z$workflow, "-", z$task_type, "-", z$batch_id),
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
                sprintf("%d cases · %d outstanding", case_total, case_outstanding)
              }
            )
          )
        ),
        div(
          class = "pt-2",
          if(identical(z$workflow,"04") && identical(z$task_type,"manual_screening") &&
             startsWith(as.character(z$batch_id),"w04-human-")) {
            div(class="mb-3",
              tags$strong("Selected manual-screening subset"),
              tags$p(class="saved-note",paste0("Assignment batch: ",z$batch_id)),
              selectInput("w04_admin_manual_subset","Choose subset to manage",
                choices=stats::setNames(manual_ids,vapply(manual_ids,function(b) {
                  id <- sub("^w04-human-","",b)
                  matches <- Filter(function(x)identical(as.character(x$subset_id),id),w04_subsets_rv())
                  if(length(matches)) w04_subset_display(matches[[1L]],records=FALSE) else id
                },character(1))),selected=chosen_manual)
            )
          },
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
            class = "assignment-kpis four",
            div(class = "assignment-kpi", tags$span("Cases"), tags$strong(record_table_link(case_total,z$workflow,z$task_type,z$batch_id,"cases",label=paste(workflow_label,task_label,"Cases",sep=" · ")))),
            div(class = "assignment-kpi", tags$span("Assignments"), tags$strong(record_table_link(formal_assignments,z$workflow,z$task_type,z$batch_id,"assignments",label=paste(workflow_label,task_label,"Assignments",sep=" · ")))),
            div(
              class = "assignment-kpi",
              tags$span("Completed"),
              tags$strong(record_table_link(case_completed,z$workflow,z$task_type,z$batch_id,"resolved_cases",label=paste(workflow_label,task_label,"Completed",sep=" · ")))
            ),
            div(class = "assignment-kpi", tags$span("Outstanding"), tags$strong(record_table_link(case_outstanding,z$workflow,z$task_type,z$batch_id,"outstanding_cases",label=paste(workflow_label,task_label,"Outstanding",sep=" · "))))
          ),
          tags$div(
            class = "text-secondary small mb-2",
            if (record_table_batch_dispatched(z$workflow,z$task_type,z$batch_id)) {
              "Click on 'Completed' to view the responses. This batch has been sent to GitHub and is read-only."
            } else {
              "Click on 'Completed' to manually edit the responses."
            }
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
                tags$th("Resolved elsewhere"),
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
                  class = "text-secondary small fw-semibold",
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
          if (identical(z$workflow,"01") &&
              identical(z$task_type,"citation_deduplication") &&
              session_can("control_workflows") && !is.null(w08b_review_queue_rv()) &&
              length(w08b_review_queue_rv()$cases)>0L &&
              length(w08b_pending_cases())==0L) {
            tags$div(class="d-flex flex-wrap align-items-center gap-2 mt-3 p-2 border rounded bg-light",
              tags$div(tags$strong("Citation-chasing deduplication complete"),
                tags$div(class="text-secondary small",
                  "All citation-chasing cases are resolved. Send the decisions to GitHub for a verified restricted archive.")),
              if(isTRUE(w08b_export_requested_rv()))
                tags$span(class="saved-note",w08b_export_status_rv())
              else actionButton("w08b_send_github","Send citation-chasing decisions to GitHub",class="btn-primary btn-sm"))
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
            !identical(w04_resolution_origin_rv(),"shiny_model") &&
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

    # Show only one W04 manual-screening assignment section at a time.
    # All subset assignments remain intact in the registry and audit history.
    visible_group_indices <- which(!vapply(group_progress,function(g)
      identical(g$meta$workflow,"04") &&
      identical(g$meta$task_type,"manual_screening") &&
      startsWith(as.character(g$meta$batch_id),"w04-human-") &&
      !identical(as.character(g$meta$batch_id),chosen_manual),logical(1)))
    w04_indices <- visible_group_indices[vapply(group_progress[visible_group_indices],
      function(g) identical(g$meta$workflow,"04"),logical(1))]
    other_indices <- setdiff(visible_group_indices,w04_indices)
    w04_sections_display <- workflow_sections[w04_indices]
    other_sections_display <- workflow_sections[other_indices]
    # Consistency UI is hosted by a static W04 disclosure elsewhere.
    # Never rebuild its enclosing <details> as outcomes or inputs change.

    by_workflow <- function(workflow) {
      indices <- which(vapply(group_progress,function(g)
        identical(as.character(g$meta$workflow),workflow),logical(1)))
      if(!length(indices))
        return(tags$p(class="saved-note","No assignment batches are currently registered for this workflow."))
      if(identical(workflow,"01")) {
        return(tags$details(
          class="assignment-workflow workflow-section wf-w01",
          `data-accordion-key`="workflow-01-deduplication-all",
          tags$summary(div(class="d-inline-flex flex-wrap align-items-center gap-2",
            tags$strong("W01 · Deduplication"),
            tags$span(class="task-badge","Shared work pool"))),
          tags$div(class="pt-2", tags$style(
            ".w01-administration-subsection > summary { display:none; } .w01-administration-subsection {margin:0 0 1rem 0; border:0; box-shadow:none;}"),
            tagList(workflow_sections[indices]))))
      }
      tagList(workflow_sections[indices])
    }
    system_panel <- tagList(
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
    list(w01=by_workflow("01"),w02=by_workflow("02"),
      w08=by_workflow("08"),system=system_panel,
      w04=tagList(w04_sections_display,uiOutput("w04_model_conflict_task_loader")))

  })

  output$w01_admin_controls <- renderUI({
    req(authenticated(),configure_review_requested(),
      identical(input$configure_workflow_tab,"W01"))
    q <- w08b_review_queue_rv()
    existing <- admin_assignment_panels()$w01
    if(is.null(q) || !length(q$cases) || length(w08b_pending_cases())>0L ||
       !session_can("control_workflows")) return(existing)
    assignments <- active_assignments_for_batch(
      assignment_registry_rv(),"01",q$batch_id,"citation_deduplication")
    if(length(assignments)) return(existing)
    tagList(existing,div(class="mt-3 p-3 border rounded",
      tags$strong("Citation-chasing deduplication complete"),
      tags$p(class="text-secondary small","All citation-chasing decisions are saved."),
      if(isTRUE(w08b_export_requested_rv()))
        tags$span(class="saved-note",w08b_export_status_rv())
      else actionButton("w08b_send_github",
        "Send citation-chasing decisions to GitHub",class="btn-primary btn-sm")))
  })
  output$w02_admin_controls <- renderUI({
    req(authenticated(),configure_review_requested(),
      identical(input$configure_workflow_tab,"W02"))
    admin_assignment_panels()$w02
  })
  output$w08_admin_controls <- renderUI({
    req(authenticated(),configure_review_requested(),
      identical(input$configure_workflow_tab,"W08"))
    admin_assignment_panels()$w08
  })
  output$system_admin_controls <- renderUI({
    req(authenticated(),configure_review_requested(),
      identical(input$configure_workflow_tab,"System"))
    admin_assignment_panels()$system
  })
  output$w04_admin_controls <- renderUI({
    req(authenticated(),configure_review_requested(),
      identical(input$configure_workflow_tab,"W04"),
      identical(input$w04_workspace_tab,"Subsets & tasks"))
    admin_assignment_panels()$w04
  })
  output$w04_comparison_body <- renderUI({
    req(authenticated(),configure_review_requested(),session_can("manage_assignments"))
    panel <- w04_consistency_panel()
    if(is.null(panel))return(NULL)
    # Preserve the parent accordion and its user-controlled open state.
    panel$children[[2L]]
  })

  output$backend_reset_status <- renderText(backend_reset_status())
  output$backend_reset_modal_status <- renderUI({
    msg <- trimws(as.character(backend_reset_status() %||% ""))
    if (!nzchar(msg)) return(NULL)
    div(
      class = if (grepl("^Reset refused|^Reset cancelled|permission", msg, ignore.case = TRUE)) "alert alert-danger mt-3 mb-0" else "alert alert-info mt-3 mb-0",
      style = "white-space: pre-wrap;",
      msg
    )
  })

  observeEvent(input$reset_backend_queue, {
    req(authenticated())
    if (!session_can("control_workflows")) {
      backend_reset_status("Administrator permission is required.")
      return()
    }
    backend_reset_status("")
    showModal(modalDialog(
      title = "Reset backend queue",
      tags$p(
        "This will first archive the operational Google Sheets backend to a restricted Zenodo record, then delete the transient operational tabs."
      ),
      tags$p(
        class = "text-danger",
        tags$strong("Any non-test production queue that is not marked consumed will block the reset.")
      ),
      tags$p(
        class = "text-secondary small",
        'Normal reset: type "RESET". For the current controlled integrity repair only, type "RESET REPAIR"; this can ignore only the exact obsolete W08 batch w08-run-37553444492 and no other live production queue.'
      ),
      textInput(
        "backend_reset_confirmation",
        'Type "RESET" or "RESET REPAIR" to confirm',
        value = ""
      ),
      uiOutput("backend_reset_modal_status"),
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
    confirmation <- trimws(as.character(input$backend_reset_confirmation %||% ""))
    repair_reset <- identical(confirmation, "RESET REPAIR")
    if (!confirmation %in% c("RESET", "RESET REPAIR")) {
      backend_reset_status('Reset cancelled: type "RESET" or "RESET REPAIR" exactly to confirm.')
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

    if (!isTRUE(update_complete) && !repair_reset) {
      backend_reset_status('Reset refused: the current update is not recorded as complete. For the controlled integrity-repair reset, type "RESET REPAIR".')
      return()
    }

    repair_override <- if (repair_reset) {
      list(list(
        stage = "08",
        tab = Sys.getenv("LEM_W08_QUEUE_TAB", unset = "queue_w08_active"),
        batch_id = "w08-run-37553444492",
        queue_sha256 = "6d1d4ccb24c3e929eb4357b58f53e25a56438c749c32428ca812e51637766f85"
      ))
    } else list()

    backend_reset_status("Archiving backend to Zenodo before reset…")
    result <- tryCatch(
      reset_backend_queue_state(
        created_by = session_reviewer_id(),
        allowed_obsolete_batches = repair_override
      ),
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
    assignment_start <- proc.time()[["elapsed"]]
    on.exit(message(sprintf("[W01 timing] automatic_assignment_total=%.3fs",proc.time()[["elapsed"]]-assignment_start)),add=TRUE)
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

    persistence_start <- proc.time()[["elapsed"]]
    persisted <- tryCatch(
      save_assignment_registry(
        c(assignment_registry_rv(), plan$new_assignments),
        assignment_path,
        actor_user_id = uid,
        expected_current_signature = assignment_registry_signature(assignment_registry_rv())
      ),
      error = function(e) {
        message("[W01 timing] automatic_assignment_persistence_error=",conditionMessage(e))
        NULL
      }
    )
    message(sprintf("[W01 timing] automatic_assignment_persistence=%.3fs new_assignments=%d",
                    proc.time()[["elapsed"]]-persistence_start,length(plan$new_assignments)))
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

  w08b_assignment_cases <- reactive({
    q <- w08b_review_queue_rv()
    if(is.null(q))return(list())
    q$cases
  })
  w08b_assignment_plan <- reactive({
    req(authenticated())
    q <- w08b_review_queue_rv()
    if(is.null(q))return(list(error="W08b queue unavailable"))
    workflow_assignment_plan("w08b",w08b_assignment_cases(),
      w08b_review_decisions_rv(),"01",q$batch_id,"citation_deduplication")
  })
  output$w08b_assignment_preview <- renderUI({
    q <- w08b_review_queue_rv();if(is.null(q))return(NULL)
    workflow_assignment_preview(w08b_assignment_plan(),"w08b","01",
      q$batch_id,"citation_deduplication",w08b_review_decisions_rv())
  })
  output$w08b_assignment_status <- renderText(assignment_manage_status())
  observeEvent(input$w08b_apply_assignments,{
    apply_workflow_assignments(w08b_assignment_plan())
  })
  observeEvent(input$w08b_remove_assignments,{
    q <- w08b_review_queue_rv();req(!is.null(q))
    remove_workflow_user_assignments(input$w08b_remove_assignment_user,
      w08b_review_decisions_rv(),"01",q$batch_id,"citation_deduplication")
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

  output$w04_consistency_subset_selector <- renderUI({
    if(!session_can("manage_assignments")) return(NULL)
    xs <- w04_consistency_registry_rv()
    if(!length(xs)) return(tags$p(class="saved-note","Loading the registered human-screening subsets…"))
    ids <- vapply(xs,function(x)as.character(x$subset_id),character(1))
    labels <- vapply(xs,w04_subset_display,character(1))
    current <- isolate(as.character(input$w04_consistency_subset_selected %||% w04_comparison_loaded_subset()))
    if(!current %in% ids)current <- ids[[1L]]
    selectInput("w04_consistency_subset_selected","Human-screened subset",
      choices=stats::setNames(ids,labels),selected=current)
  })
  output$w04_consistency_subset_status <- renderText(w04_consistency_subset_status_rv())
  refresh_w04_consistency_registry <- function() {
    meta <- w04_prompt_review_metadata()
    registered <- w04_subset_registry_read(meta$review_id)
    w04_consistency_registry_rv(registered)
    registered
  }
  observeEvent(list(input$configure_workflow_tab,input$w04_workspace_tab),{
    req(identical(input$configure_workflow_tab,"W04"),
        identical(input$w04_workspace_tab,"Compare coding"))
    req(authenticated())
    if(!session_can("manage_assignments")) return()
    if(length(w04_consistency_registry_rv())) return()
    result <- tryCatch(refresh_w04_consistency_registry(),error=function(e)e)
    if(inherits(result,"error"))
      w04_consistency_subset_status_rv(paste("Cannot load registered subsets:",conditionMessage(result)))
  })
  observeEvent(input$w04_consistency_subset_refresh,{
    req(authenticated())
    if(!session_can("manage_assignments")) return()
    result <- tryCatch(refresh_w04_consistency_registry(),error=function(e)e)
    if(inherits(result,"error")) {
      w04_consistency_subset_status_rv(paste("Cannot refresh registered subsets:",conditionMessage(result)))
    } else {
      w04_consistency_subset_status_rv(sprintf("%d subset(s) registered in this review.",length(result)))
    }
  })
    w04_comparison_loaded_subset <- reactiveVal("")
  output$w04_consistency_model_selector <- renderUI({
    xs <- w04_consistency_model_runs_rv()
    if(!length(xs))return(NULL)
    ids <- vapply(xs,function(x)as.character(x$run_id),character(1))
    labels <- vapply(xs,function(x)sprintf("Run %s · %s records · %s uncertain",
      x$run_id,x$total,x$uncertain),character(1))
    selected <- isolate(as.character(input$w04_consistency_model_selected %||% ""))
    if(!selected %in% ids) selected <- as.character(w04_comparison_loaded_run())
    if(!selected %in% ids) {
      dates <- vapply(xs,function(x)as.character(x$published_at_utc %||% ""),character(1))
      selected <- ids[[order(dates,ids,decreasing=TRUE)[[1L]]]]
    }
    selectInput("w04_consistency_model_selected","Completed model run",
      choices=stats::setNames(ids,labels),selected=selected)
  })
  output$w04_consistency_model_status <- renderText(w04_consistency_model_status_rv())

  w04_comparison_loaded_run <- reactiveVal("")
  w04_comparison_load_subset <- function(id) {

    req(authenticated())
    if(!session_can("manage_assignments")) return()
    result <- tryCatch({
      registered <- w04_consistency_registry_rv()
      if(!length(registered)) registered <- refresh_w04_consistency_registry()
      matches <- Filter(function(x)identical(as.character(x$subset_id),id),registered)
      if(length(matches)!=1L)
        stop("Subset selection was not found in the current review registry. The list has been refreshed; select a subset and try again.",call.=FALSE)
      meta <- w04_prompt_review_metadata()
      q <- w04_subset_queue_read(id,meta$review_id)
      if(is.null(q)) stop("The selected subset has no saved manual-screening queue",call.=FALSE)
      decisions <- w04_filter_batch_decisions(active_sheet_w04_decisions(),q$queue_sha256)
      batch <- paste0("w04-human-",id)
      outcomes <- w04_blind_case_outcomes(q$cases,assignment_registry_rv(),decisions,
        batch_id=batch,task_type="manual_screening")
      list(subset_id=id,batch_id=batch,outcomes=outcomes)
    },error=function(e)e)
    if(inherits(result,"error")) {
      w04_consistency_subset_status_rv(paste("Subset could not be loaded:",conditionMessage(result)))
      return()
    }
    result$human_outcomes <- result$outcomes
    w04_consistency_subset_snapshot_rv(result)
    w04_consistency_model_runs_rv(list())
    w04_consistency_model_status_rv("")
    w04_consistency_subset_status_rv(sprintf("Loaded %s (%d cases) for read-only consistency analysis.",id,length(result$outcomes)))
    if(!is.null(w04_consistency_subset_snapshot_rv()) &&
       identical(w04_consistency_subset_snapshot_rv()$subset_id,id))
      w04_comparison_loaded_subset(id)
    w04_comparison_loaded_run("")
  }
  w04_comparison_find_runs <- function() {

    req(authenticated())
    if(!session_can("manage_assignments")) return()
    snap <- w04_consistency_subset_snapshot_rv()
    if(is.null(snap)) {
      w04_consistency_model_status_rv("Load a human-screening subset first.")
      return()
    }
    found <- tryCatch({
      meta <- w04_prompt_review_metadata()
      Filter(function(x)identical(as.character(x$subset_id),snap$subset_id),
        w04_model_batch_registry_read(meta$review_id))
    },error=function(e)e)
    if(inherits(found,"error")) {
      w04_consistency_model_runs_rv(list())
      w04_consistency_model_status_rv(paste("Model runs unavailable:",conditionMessage(found)))
      return()
    }
    w04_consistency_model_runs_rv(found)
    w04_consistency_model_status_rv(sprintf("%d completed model run(s) for selected subset.",length(found)))
  }
  w04_comparison_load_model <- function(id) {

    req(authenticated())
    if(!session_can("manage_assignments")) return()
    result <- tryCatch({
      snap <- w04_consistency_subset_snapshot_rv()
      if(is.null(snap)) stop("Load a human subset first",call.=FALSE)
      hits <- Filter(function(x)identical(as.character(x$run_id),id),w04_consistency_model_runs_rv())
      if(length(hits)!=1L || !identical(as.character(hits[[1L]]$subset_id),snap$subset_id))
        stop("Choose a model run for the selected subset",call.=FALSE)
      batch <- hits[[1L]]
      model <- w04_model_batch_results_read(batch)
      resolved <- list()
      if(as.integer(batch$uncertain)>0L) {
        queue <- read_sheet_w04_resolution_queue(as.character(batch$resolution_tab))
        if(is.null(queue) || !identical(as.character(queue$source_run_id),id) ||
           !identical(as.character(queue$queue_sha256),as.character(batch$resolution_queue_sha256)))
          stop("The run-specific uncertainty queue is unavailable or inconsistent",call.=FALSE)
        active <- w04_filter_batch_decisions(active_sheet_w04_resolution_decisions(),queue$queue_sha256)
        resolved <- Filter(function(x)as.character(x$decision %||% "") %in% c("retain","exclude"),active)
      }
      w04_model_consistency_outcomes(snap$human_outcomes %||% snap$outcomes,model,resolved)
    },error=function(e)e)
    if(inherits(result,"error")) {
      w04_consistency_model_status_rv(paste("Model κ input unavailable:",conditionMessage(result)))
      return()
    }
    snap <- w04_consistency_subset_snapshot_rv()
    snap$outcomes <- result
    snap$model_run_id <- as.character(id)
    snap$model_sha256 <- as.character(Filter(function(x)identical(as.character(x$run_id),snap$model_run_id),w04_consistency_model_runs_rv())[[1L]]$model_sha256)
    w04_consistency_subset_snapshot_rv(snap)
    human_ids <- w04_available_consistency_raters(result)
    human_ids <- setdiff(human_ids,"model")
    preferred <- Filter(function(uid) {
      user <- find_user_by_id(user_registry_rv(),uid,require_active=FALSE)
      !is.null(user) && grepl("matt|matthew",as.character(user$display_name %||% ""),ignore.case=TRUE)
    },human_ids)
    selected <- c(if(length(preferred)) preferred[[1L]] else if(length(human_ids)==1L) human_ids[[1L]] else character(),"model")
    if(length(selected)==2L)
      updateCheckboxGroupInput(session,"w04_consistency_raters",selected=selected)
    w04_consistency_model_status_rv(sprintf(
      "Loaded model run %s. %d human reviewer(s) available. %s",
      as.character(id),length(human_ids),
      if(length(selected)==2L) "Selected reviewer and Model for κ comparison below." else "Select the intended human reviewer and Model under Raters to compare."
    ))
    if(identical(as.character(w04_consistency_subset_snapshot_rv()$model_run_id %||% ""),id))
      w04_comparison_loaded_run(id)
  }
  observeEvent(input$w04_consistency_subset_selected,{
    req(authenticated(),session_can("manage_assignments"))
    id <- as.character(input$w04_consistency_subset_selected %||% "")
    if(!nzchar(id) || identical(id,w04_comparison_loaded_subset())) return()
    w04_comparison_load_subset(id)
    if(!identical(id,w04_comparison_loaded_subset())) return()
    w04_comparison_find_runs()
    runs <- w04_consistency_model_runs_rv()
    if(length(runs)){
      dates <- vapply(runs,function(x)as.character(x$published_at_utc %||% ""),character(1))
      ids <- vapply(runs,function(x)as.character(x$run_id %||% ""),character(1))
      latest <- ids[[order(dates,ids,decreasing=TRUE)[[1L]]]]
      updateSelectInput(session,"w04_consistency_model_selected",selected=latest)
      w04_comparison_load_model(latest)
    }
  })
  observeEvent(input$w04_consistency_model_selected,{
    req(authenticated(),session_can("manage_assignments"))
    id <- as.character(input$w04_consistency_model_selected %||% "")
    if(!nzchar(id) || identical(id,w04_comparison_loaded_run()))return()
    available <- vapply(w04_consistency_model_runs_rv(),function(x)
      as.character(x$run_id),character(1))
    if(!id %in% available)return()
    w04_comparison_load_model(id)
  })

  observeEvent(list(input$configure_workflow_tab,input$w04_workspace_tab), {
    req(identical(input$configure_workflow_tab,"W04"),
        identical(input$w04_workspace_tab,"Compare coding"))
    req(authenticated())
    if (!session_can("manage_assignments")) return()
    if (isTRUE(w04_consistency_history_loaded())) return()
    if (!identical(storage_backend(),"google_sheets")) {
      w04_consistency_history_loaded(TRUE)
      return()
    }
    kappa_result <- tryCatch(read_manual_screening_metrics(), error=function(e)e)
    if (inherits(kappa_result,"error")) {
      w04_consistency_status(paste("Could not load W04 kappa history:",conditionMessage(kappa_result)))
      return()
    }
    manual_screening_rv(kappa_result)
    # Reviewer conflict sets are already loaded from Sheets at login.
    # Reuse that snapshot rather than immediately rereading the same tab.
    conflict_snapshot <- isolate(w04_conflict_sets_rv())
    loaded <- tryCatch(
      list(
        analyses=read_w04_consistency_analyses(),
        human_kappa=read_w04_human_kappa_registry(),
        conflict_sets=conflict_snapshot
      ),
      error=function(e)e
    )
    if (inherits(loaded,"error")) {
      w04_consistency_status(paste("Could not load saved W04 analysis history:",conditionMessage(loaded)))
      # Keep the history eligible for a retry when the administrator reopens it.
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
      active_sha <- as.character(active_set$conflict_queue_sha256 %||% "")
      # Reuse the login snapshot when it represents this same conflict queue.
      # Otherwise load the correct queue's decisions from Sheets.
      if (nzchar(active_sha) &&
          !length(isolate(w04_blind_conflict_cases())) &&
          identical(active_sha, as.character(isolate(w04_conflict_queue_sha_rv()) %||% ""))) {
        set_decisions <- isolate(w04_conflict_decisions())
      } else {
        all_conf <- tryCatch(active_sheet_w04_conflict_decisions(),error=function(e)e)
        if (inherits(all_conf,"error")) {
          w04_consistency_status(paste("Could not load W04 conflict decisions:",conditionMessage(all_conf)))
          return()
        }
        set_decisions <- w04_filter_batch_decisions(all_conf, active_sha)
      }
      w04_conflict_decisions(set_decisions)
      w04_conflict_idx(1L)
    }
    w04_consistency_history_loaded(TRUE)
    # Repair comparisons that were saved authoritatively before their summary
    # could be appended. Never backfill other W04 metadata during login.
    sync <- tryCatch({
      review_id <- w04_prompt_review_metadata()$review_id
      repaired <- w04_summary_reconcile_comparisons(review_id,loaded$analyses)
      if(repaired>0L) {
        overview <- w04_summary_tables(w04_summary_read(review_id,create_if_missing=FALSE))
        w04_overview_rv(overview)
        w04_overview_status_rv(sprintf("Reconciled %d missing comparison(s).",repaired))
      }
      repaired
    },error=function(e)e)
    if(inherits(sync,"error"))
      w04_consistency_status(paste(
        "Saved comparisons loaded, but overview reconciliation failed:",
        conditionMessage(sync)))
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

  w04_consistency_choice_label <- function(z) {
    batch <- as.character(z$batch_id %||% "")
    source <- if(startsWith(batch,"w04-model-run-"))
      paste0("Model run ",sub("^w04-model-run-","",batch)) else
      if(startsWith(batch,"w04-human-"))
        paste0("Human subset ",sub("^w04-human-","",batch)) else batch
    raters <- tryCatch(as.character(jsonlite::fromJSON(
      as.character(z$rater_ids_json %||% "[]"))),error=function(e)character())
    rater_labels <- vapply(raters,function(id) {
      if(identical(id,"model") || startsWith(id,"model:"))return("Model")
      user <- find_user_by_id(user_registry_rv(),id,require_active=FALSE)
      if(is.null(user))id else as.character(normalise_user_row(user)$display_name %||% id)
    },character(1))
    saved <- as.character(z$created_at_utc %||% "")
    if(nzchar(saved))saved <- gsub("T"," ",sub("Z$"," UTC",saved),fixed=TRUE)
    n <- as.character(z$conflict_n %||% "0")
    paste0(source," · ",if(length(rater_labels))paste(rater_labels,collapse=" vs ") else "Reviewers unavailable",
      " · ",n,if(identical(n,"1"))" conflict" else " conflicts",
      if(nzchar(saved))paste0(" · Saved ",saved) else "")
  }
  output$w04_consistency_history <- renderUI({
    req(authenticated())
    if (!session_can("manage_assignments")) return(NULL)
    all_saved <- Filter(function(x) {
      id <- as.character(x$batch_id %||% "")
      identical(id,as.character(w04_batch_id_rv())) ||
        grepl("^w04-model-run-[0-9]+$",id)
    },w04_consistency_analyses_rv() %||% list())
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
    saved <- vapply(conflict_ready,function(x)as.character(x$created_at_utc %||% ""),character(1))
    conflict_ready <- conflict_ready[order(saved,decreasing=TRUE)]
    ids <- vapply(conflict_ready,function(x)as.character(x$analysis_id %||% ""),character(1))
    labels <- vapply(conflict_ready,w04_consistency_choice_label,character(1))
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
        selected=ids[[1L]]
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
    snapshot <- w04_consistency_subset_snapshot_rv()
    if(!is.null(snapshot)) {
      if(!nzchar(as.character(snapshot$model_run_id %||% ""))) {
        w04_consistency_status("Step 3: load a completed model run before saving this subset's comparison.")
        return()
      }
      rater_ids <- unique(as.character(input$w04_consistency_raters %||% character()))
      if(length(rater_ids)!=2L || !"model" %in% rater_ids) {
        w04_consistency_status("Select exactly one human reviewer and Model before saving.")
        return()
      }
      analysis <- w04_consistency_analysis(snapshot$outcomes,rater_ids)
      if(analysis$complete<1L) {
        w04_consistency_status("No records have complete human and resolved model decisions; nothing to save.")
        return()
      }
      run <- as.character(snapshot$model_run_id)
      model_sha <- as.character(snapshot$model_sha256 %||% "")
      if(!grepl("^[0-9]+$",run) || !grepl("^[0-9a-f]{64}$",model_sha)) {
        w04_consistency_status("Model run provenance is incomplete; reload the model before saving.")
        return()
      }
      # Bind the registry record to the immutable model run, not the active
      # production screening batch. Incomplete model cases remain excluded.
      saved <- tryCatch(append_w04_consistency_analysis(
        analysis=analysis,batch_id=paste0("w04-model-run-",run),
        queue_sha256=model_sha,review_mode="subset_model_validation",
        created_by=session_reviewer_id()),error=function(e)e)
      if(inherits(saved,"error")) {
        w04_consistency_status(paste("Save failed:",conditionMessage(saved)))
        return()
      }
      current <- w04_consistency_analyses_rv() %||% list()
      if(!any(vapply(current,function(x)identical(as.character(x$analysis_id %||% ""),
          as.character(saved$analysis_id %||% "")),logical(1))))
        w04_consistency_analyses_rv(c(current,list(saved)))
      # A verified authoritative save does not guarantee the summary append.
      # Refresh the existing one-sheet overview only when the summary write
      # succeeded; explicitly disclose a failed append so it can be repaired.
      if(isTRUE(saved$overview_synced)) {
        overview <- tryCatch(w04_summary_tables(w04_summary_read(
          review_id=w04_prompt_review_metadata()$review_id,
          create_if_missing=FALSE)),error=function(e)e)
        if(!inherits(overview,"error")) {
          w04_overview_rv(overview)
          w04_overview_status_rv("Overview includes the latest saved comparison.")
        }
      }
      w04_consistency_status(sprintf(
        "Saved comparison %s: subset %s, model run %s, %d comparable records, %d pending or missing, Cohen's κ %s. %s",
        as.character(saved$analysis_id),snapshot$subset_id,run,
        analysis$complete,analysis$missing,
        if(is.na(analysis$kappa))"not calculable" else sprintf("%.3f",analysis$kappa),
        if(isTRUE(saved$overview_synced))
          "Overview summary updated."
        else "WARNING: authoritative comparison saved, but overview summary update failed. Do not save again; summary repair is required."))
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

  w04_load_conflict_vote_reasons_status_rv <- reactiveVal("")
  output$w04_load_conflict_vote_reasons_status <- renderText(w04_load_conflict_vote_reasons_status_rv())
  observeEvent(input$w04_load_conflict_vote_reasons,{
    req(authenticated())
    z <- w04_active_consistency_conflict_set()
    if(is.null(z) || !identical(as.character(z$comparison_type %||% ""),"human_model"))return()
    w04_load_conflict_vote_reasons_status_rv("Retrieving archived model-pass explanations. This can take a while.")
    result <- tryCatch({
      run <- sub("^w04-model-run-","",as.character(z$parent_batch_id))
      meta <- w04_prompt_review_metadata()
      batches <- w04_model_batch_registry_read(meta$review_id)
      hit <- Filter(function(x)identical(as.character(x$run_id),run),batches)
      if(length(hit)!=1L)stop("The archived model run could not be identified",call.=FALSE)
      votes <- w04_model_vote_reasons_from_artifacts(run,as.character(hit[[1]]$prompt_sha256))
      current <- w04_model_conflict_cases_rv()
      if(!length(current))stop("Load the conflict cases before retrieving model explanations",call.=FALSE)
      changed <- lapply(current,function(item) {
        item$model_vote_details <- votes[[as.character(item$record_id %||% "")]] %||% list()
        item
      })
      w04_model_conflict_cases_rv(changed)
      sum(vapply(changed,function(item)length(item$model_vote_details %||% list())>0L,logical(1)))
    },error=function(e)e)
    w04_load_conflict_vote_reasons_status_rv(if(inherits(result,"error"))
      paste("Archived explanations unavailable:",conditionMessage(result))
      else sprintf("Archived model reasons loaded for %d conflicting records.",result))
  })
  activate_w04_model_conflict <- function(z) {
    cases <- w04_model_conflict_reconstruct(z,assignment_registry_rv())
    w04_model_conflict_cases_rv(cases)
    w04_model_conflict_active_rv(as.character(z$conflict_set_id))
    events <- active_sheet_w04_conflict_decisions()
    w04_conflict_decisions(w04_filter_batch_decisions(events,
      as.character(z$conflict_queue_sha256)))
    w04_conflict_idx(1L)
    length(cases)
  }
  w04_model_conflict_load_status_rv <- reactiveVal("")
  output$w04_model_conflict_load_status <- renderText(w04_model_conflict_load_status_rv())
  output$w04_model_conflict_loader <- renderUI({
    sets <- Filter(function(z)
      identical(as.character(z$comparison_type %||% ""),"human_model"),
      w04_conflict_sets_rv() %||% list())
    if(!length(sets))return(NULL)
    ids <- vapply(sets,function(z)as.character(z$conflict_set_id),character(1))
    labels <- vapply(sets,function(z)paste0(
      "Model run ",sub("^w04-model-run-","",as.character(z$parent_batch_id)),
      " | ",as.character(z$created_at_utc %||% "")),character(1))
    tagList(selectInput("w04_model_conflict_selected","Saved model disagreement set",
      choices=stats::setNames(ids,labels)),
      actionButton("w04_model_conflict_load","Load model conflicts for adjudication",
        class="btn-outline-primary btn-sm",
        onclick="this.setAttribute('aria-busy','true');"),
      tags$div(class="saved-note mt-2",
        "Loading a conflict set makes its records available to the existing W04 assignment controls. It does not assign records automatically."),
      tags$div(class="saved-note mt-2",
        textOutput("w04_model_conflict_load_status",inline=TRUE)))
  })
  output$w04_model_conflict_task_loader <- renderUI({
    req(authenticated())
    if(!session_can("adjudicate_assigned") && !session_can("manage_assignments"))
      return(NULL)
    sets <- Filter(function(z)
      identical(as.character(z$comparison_type %||% ""),"human_model"),
      w04_conflict_sets_rv() %||% list())
    if(!session_can("manage_assignments")) {
      sets <- Filter(function(z) {
        assignments <- active_assignments_for_batch(
          assignment_registry_rv(),"04",as.character(z$conflict_set_id),
          "conflict_resolution")
        any(vapply(assignments,function(a)
          identical(as.character(normalise_assignment_row(a)$user_id),
                    as.character(current_user()$user_id %||% "")),logical(1)))
      },sets)
    }
    if(!length(sets)) {
      if(session_can("manage_assignments"))
        return(tags$p(class="saved-note",
          "No saved human–model conflict set is available. In Consistency checking, select a saved comparison with disagreements and click Create conflict set."))
      return(NULL)
    }
    ids <- vapply(sets,function(z)as.character(z$conflict_set_id),character(1))
    labels <- vapply(sets,function(z)paste0("Run ",
      sub("^w04-model-run-","",as.character(z$parent_batch_id)),
      " | ",as.character(z$conflict_set_id)),character(1))
    card(class="assignment-summary",div(class="p-3",
      tags$strong("Saved human–model conflict sets"),
      tags$p(class="saved-note","Activate a saved set here to expose its cases in W04 · Conflict resolution above. Then open Manage assignments to allocate the cases. Archived model-vote explanations can be retrieved separately during adjudication."),
      selectInput("w04_task_model_conflict_selected","Model conflict set",
        choices=stats::setNames(ids,labels)),
      actionButton("w04_task_model_conflict_load","Load reviewer conflict cases",
        class="btn-outline-primary btn-sm"),
      textOutput("w04_task_model_conflict_status")))
  })
  w04_task_model_conflict_status_rv <- reactiveVal("")
  output$w04_task_model_conflict_status <- renderText(w04_task_model_conflict_status_rv())
  observeEvent(input$w04_task_model_conflict_load,{
    req(authenticated())
    id <- as.character(input$w04_task_model_conflict_selected %||% "")
    eligible <- Filter(function(z)identical(as.character(z$conflict_set_id),id) &&
      identical(as.character(z$comparison_type),"human_model"),
      w04_conflict_sets_rv() %||% list())
    if(length(eligible)!=1L) return()
    if(!session_can("manage_assignments")) {
      assignments <- active_assignments_for_batch(assignment_registry_rv(),
        "04",id,"conflict_resolution")
      owned <- any(vapply(assignments,function(a)
        identical(as.character(normalise_assignment_row(a)$user_id),
                  as.character(current_user()$user_id %||% "")),logical(1)))
      if(!owned)return()
    }
    loaded <- tryCatch(activate_w04_model_conflict(eligible[[1L]]),error=function(e)e)
    w04_task_model_conflict_status_rv(if(inherits(loaded,"error"))
      paste("Loading failed:",conditionMessage(loaded))
      else sprintf("Activated %d conflict records. In W04 · Conflict resolution above, expand Manage assignments, choose a reviewer and Apply assignments.",loaded))
  })
  observeEvent(input$w04_model_conflict_load,{
    req(authenticated())
    if(!session_can("manage_assignments")) {
      w04_model_conflict_load_status_rv("Administrator permission is required.")
      return()
    }
    w04_model_conflict_load_status_rv("Loading archived cases and individual model votes…")
    id <- as.character(input$w04_model_conflict_selected %||% "")
    sets <- Filter(function(z)identical(as.character(z$conflict_set_id),id) &&
      identical(as.character(z$comparison_type),"human_model"),
      w04_conflict_sets_rv() %||% list())
    if(length(sets)!=1L) {
      w04_model_conflict_load_status_rv("Select a saved model conflict set.")
      return()
    }
    loaded <- tryCatch(activate_w04_model_conflict(sets[[1L]]),error=function(e)e)
    status <- if(inherits(loaded,"error"))
      paste("Loading model conflicts failed:",conditionMessage(loaded))
      else sprintf("Loaded %d model conflicts from run %s. Use the W04 Reviewer conflict resolution assignment controls below to allocate the cases.",
        loaded,sub("^w04-model-run-","",as.character(sets[[1L]]$parent_batch_id)))
    w04_model_conflict_load_status_rv(status)
    w04_consistency_status(status)
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
    if(identical(as.character(saved_set$comparison_type %||% ""),"human_model")) {
      loaded <- tryCatch(activate_w04_model_conflict(saved_set),error=function(e)e)
      if(inherits(loaded,"error")) {
        w04_consistency_status(paste("Conflict set saved, but model cases could not be loaded:",conditionMessage(loaded)))
        return()
      }
    }
    w04_consistency_status(sprintf(
      "Created conflict set %s from %s. Assign its cases in the W04 conflict-resolution section below.",
      as.character(saved_set$conflict_set_id %||% ""),
      analysis_id
    ))
  })

  output$w04_assignment_subset_selector <- renderUI({
    req(authenticated())
    available <- w04_subsets_rv()
    ids <- if(length(available)) vapply(available,function(s)as.character(s$subset_id),character(1)) else character()
    selectInput("w04_assignment_subset_id","Registered human-screening subset",
      choices=c("Select a registered subset"="",stats::setNames(ids,vapply(available,w04_subset_display,character(1)))))
  })
  w04_assignment_plan <- reactive({
    req(authenticated())
    selected <- as.character(input$w04_assignment_users %||% character())
    type <- as.character(input$w04_assignment_type %||% "number")
    amount <- input$w04_assignment_amount %||% NA_real_
    review_mode <- as.character(input$w04_review_mode %||% "reviewer_consistency")
    tryCatch({
      subset_id <- as.character(input$w04_assignment_subset_id %||% "")
      if(!nzchar(subset_id)) stop("Select a registered human-screening subset",call.=FALSE)
      meta <- w04_prompt_review_metadata()
      registry <- w04_subset_registry_read(meta$review_id)
      chosen <- Filter(function(x)identical(x$subset_id,subset_id),registry)
      if(length(chosen)!=1L) stop("Selected subset is not in this review registry",call.=FALSE)
      prepared <- w04_subset_queue_read(subset_id,meta$review_id)
      if(is.null(prepared)) stop("Create the screening queue for this subset before assigning reviewers",call.=FALSE)
      cases <- w04_subset_queue_cases(chosen[[1L]],prepared$cases,
        chosen[[1L]]$source_checkpoint_sha256)
      subset_events <- w04_filter_batch_decisions(active_sheet_w04_decisions(),prepared$queue_sha256)
      plan <- plan_w04_manual_assignment(
        cases = cases,
        assignments = assignment_registry_rv(),
        active_events = subset_events,
        batch_id = prepared$batch_id,
        user_ids = selected,
        review_mode = review_mode,
        allocation_type = type,
        amount = amount
      )
      plan$subset_batch_id <- prepared$batch_id
      plan$subset_queue_sha256 <- prepared$queue_sha256
      plan$subset_active_events <- subset_events
      plan
      }, error=function(e) list(error=conditionMessage(e)))
  })
  output$w04_assignment_preview <- renderUI({
    req(authenticated())
    workflow_assignment_preview(
      w04_assignment_plan(), "w04", "04",
      as.character(w04_assignment_plan()$subset_batch_id %||% ""),
      "manual_screening", w04_assignment_plan()$subset_active_events %||% list()
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

  # Read-only login timing: no changes to backend retries, writes or caches.
  login_timed_read <- function(label, operation) {
    started <- proc.time()[["elapsed"]]
    on.exit(message(sprintf("LEM login read [%s]: %.2f seconds",
      label, proc.time()[["elapsed"]] - started)), add=TRUE)
    operation()
  }

  restore_w04_model_uncertainty_on_login <- function(user_id) {
    # Do not overwrite an active production queue or a batch opened manually.
    if(!isTRUE(authenticated()) || !identical(session_reviewer_id(),user_id) ||
       length(w04_resolution_all_cases_rv() %||% list())) return(invisible(FALSE))
    result <- tryCatch({
      meta <- w04_prompt_review_metadata()
      batches <- w04_model_batch_registry_read(meta$review_id)
      batches <- Filter(function(b) {
        n <- suppressWarnings(as.integer(b$uncertain %||% 0L))
        length(n)==1L && !is.na(n) && n>0L &&
          !is.na(b$resolution_tab) && nzchar(as.character(b$resolution_tab))
      },batches)
      if(!session_can("manage_assignments")) {
        batches <- Filter(function(b) {
          batch_id <- paste0("w04-resolution-",b$run_id,"-",
            substr(as.character(b$resolution_queue_sha256),1L,12L))
          length(assignments_for_user(assignment_registry_rv(),"04",batch_id,
            user_id,"model_uncertainty"))>0L
        },batches)
      }
      if(!length(batches)) return(NULL)
      ord <- order(vapply(batches,function(b)as.character(b$published_at_utc %||% ""),
        character(1)),decreasing=TRUE)
      b <- batches[[ord[[1L]]]]
      q <- read_sheet_w04_resolution_queue(as.character(b$resolution_tab))
      if(is.null(q) || !identical(as.character(q$source_run_id),as.character(b$run_id)) ||
         !identical(as.character(q$queue_sha256),as.character(b$resolution_queue_sha256)) ||
         length(q$cases)!=as.integer(b$uncertain))
        stop("Restored model queue identity/count mismatch",call.=FALSE)
      ds <- w04_filter_batch_decisions(active_sheet_w04_resolution_decisions(),q$queue_sha256)
      list(queue=q,decisions=ds,run_id=b$run_id)
    },error=function(e)e)
    if(inherits(result,"error")) {
      message("LEM W04 model queue auto-restore failed: ",conditionMessage(result))
      w04_model_return_status_rv(paste("Model uncertainty auto-restore unavailable:",conditionMessage(result)))
      return(invisible(FALSE))
    }
    if(is.null(result) || !isTRUE(authenticated()) ||
       !identical(session_reviewer_id(),user_id) ||
       length(w04_resolution_all_cases_rv() %||% list())) return(invisible(FALSE))
    q <- result$queue
    w04_resolution_all_cases_rv(q$cases)
    w04_resolution_queue_sha_rv(q$queue_sha256)
    w04_resolution_batch_id_rv(q$batch_id)
    w04_resolution_source_run_id_rv(q$source_run_id)
    w04_resolution_origin_rv("shiny_model")
    w04_resolution_batch_status_rv(q$batch_status %||% "")
    w04_resolution_decisions(result$decisions)
    w04_resolution_resume_requested_rv(FALSE)
    w04_resolution_idx(1L)
    w04_model_return_status_rv(sprintf("Restored model run %s: %d uncertainty cases.",
      result$run_id,length(q$cases)))
    invisible(TRUE)
  }

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
      registry <- login_timed_read("user registry", read_user_registry)
      authenticate_registered_user(registry, input$login_email, input$access_key)
    }, error = function(e) {
      login_status(paste("Login configuration error:", conditionMessage(e)))
      NULL
    })

    if (!is.null(login_user)) {
      # Authenticate and flush the identity/loading screen before performing
      # legacy workflow initialisation. The existing queue/decision logic is
      # deliberately retained verbatim until workflow-specific loading is split.
      user_registry_rv(registry)
      current_user(login_user)
      authenticated(TRUE)
      app_view("tasks")
      failed_attempts(0L)
      login_status("")
      login_initialising(TRUE)
      load_workflow_queues <- function() {
      shiny::isolate({
      loaded <- tryCatch({
        user_registry_rv(registry)
        loaded_assignments <- login_timed_read("assignments",function() read_assignment_registry(assignment_path))
        if (identical(storage_backend(), "local")) {
          loaded_assignments <- resolve_fixture_assignment_users(loaded_assignments, registry)
        }
        assignment_registry_rv(loaded_assignments)
        pipeline_status_rv(if(identical(storage_backend(),"google_sheets")) login_timed_read("pipeline status",read_latest_pipeline_status) else NULL)
        # Administrative W04 kappa history loads when Consistency checking opens.
        batch <- login_timed_read("W01 queue",load_batch)
        current_decisions <- list()
        if (!is.null(batch)) {
          all_decisions <- login_timed_read("W01 decisions", function() read_active_decisions(decision_path))
          current_decisions <- filter_batch_decisions(all_decisions, batch$queue_sha256)
          w01_repairs_rv(login_timed_read("W01 repairs", function() read_active_w01_repairs(w01_repair_path, batch$queue_sha256)))
        } else {
          w01_repairs_rv(list())
        }
        w02_batch <- login_timed_read("W02 queue",load_w02_batch)

        # Reuse one W02 decision snapshot for the historical repair check and
        # normal W02 queue initialisation, unless the repair changes its status.
        w02_login_decision_snapshot <- NULL
        read_w02_login_decisions <- function() {
          if (is.null(w02_login_decision_snapshot))
            w02_login_decision_snapshot <<- login_timed_read(
              "W02 decisions", active_sheet_w02_decisions)
          w02_login_decision_snapshot
        }

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
          repair_decisions <- read_w02_login_decisions()
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
          w02_all_decisions <- read_w02_login_decisions()
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

        # Defer W04 case queues, decision histories and conflict sets until requested.
        # The closure retains the authenticated reviewer and assignment snapshot.
        w04_queued_init_rv(function() {
          shiny::isolate({
        # First identify whether W04 has an active manual-screening queue.
        # Avoid reading screening notes for sessions without that queue.

        w04_batch <- login_timed_read("W04 queue",function() load_w04_batch(login_user))
        if (!is.null(w04_batch) && identical(as.character(w04_batch$review_mode %||% ""), "resolution")) {
          w04_batch <- NULL
        }
        if (!is.null(w04_batch)) {
          w04_screening_notes_rv(
            if (identical(storage_backend(),"google_sheets")) {
              tryCatch(login_timed_read("W04 screening notes",active_sheet_w04_screening_notes),
                error=function(e) list())
            } else list()
          )
          w04_all_decisions <- login_timed_read("W04 decisions",active_sheet_w04_decisions)
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
          # Never restore legacy queue-specific terms over review-level terms.
          if(!is.null(w04_highlight_version_rv())) {
            w04_include_terms(w04_highlight_version_rv()$include)
            w04_exclude_terms(w04_highlight_version_rv()$exclude)
          }
          w04_decisions(w04_batch_decisions)
          w04_manual_abstract_edits_rv(tryCatch(
            login_timed_read("W04 abstract edits",function() active_sheet_w04_manual_abstract_edits(w04_batch$queue_sha256,
              as.character(normalise_user_row(login_user)$user_id))),
            error=function(e){w04_manual_abstract_status_rv(paste("Abstract edits unavailable:",conditionMessage(e)));list()}))
          w04_manual_abstract_edit_rv(FALSE)
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
          w04_conflict_sets_rv(login_timed_read("W04 conflict sets",read_w04_conflict_sets))
        } else {
          w04_conflict_sets_rv(list())
        }

        w04_resolution_batch <- login_timed_read("W04 resolution queue",load_w04_resolution_batch)
        if (!is.null(w04_resolution_batch)) {
          all_res <- login_timed_read("W04 resolution decisions",active_sheet_w04_resolution_decisions)
          w04_resolution_origin_rv("production")
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
              login_timed_read("W04 resolution edits",function() active_sheet_w04_resolution_abstract_edits(w04_resolution_batch$queue_sha256)),
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

        # A single decision-log snapshot serves both ordinary conflict and
        # double-blind conflict views during this login initialisation.
        w04_conflict_decision_snapshot <- NULL
        login_conflict_decisions <- function() {
          if (is.null(w04_conflict_decision_snapshot))
            w04_conflict_decision_snapshot <<- login_timed_read(
              "W04 conflict decisions",active_sheet_w04_conflict_decisions)
          w04_conflict_decision_snapshot
        }
        w04_conflict_batch <- login_timed_read("W04 conflict queue",load_w04_conflict_batch)
        if (!is.null(w04_conflict_batch)) {
          all_conf <- login_conflict_decisions()
          w04_conflict_cases_rv(w04_conflict_batch$cases)
          w04_conflict_queue_sha_rv(w04_conflict_batch$queue_sha256)
          w04_conflict_batch_id_rv(w04_conflict_batch$batch_id)
          w04_conflict_batch_status_rv(w04_conflict_batch$batch_status %||% "")
          w04_conflict_decisions(w04_filter_batch_decisions(all_conf,w04_conflict_batch$queue_sha256))
          cr <- w04_conflict_unresolved_indices()
          w04_conflict_idx(if(length(cr)) cr[[1L]] else max(1L,length(w04_conflict_batch$cases)))
        }

        if (length(w04_blind_conflict_cases())) {
          all_blind_conf <- login_conflict_decisions()
          blind_conf_decisions <- w04_filter_batch_decisions(all_blind_conf,w04_queue_sha_rv())
          w04_conflict_decisions(blind_conf_decisions)
          cr <- w04_conflict_unresolved_indices()
          w04_conflict_idx(if(length(cr)) cr[[1L]] else max(1L,length(w04_blind_conflict_cases())))
        }

          })
        })

        w08_batch <- login_timed_read("W08 queue",load_w08_batch)
        if (!is.null(w08_batch)) {
          w08_all_decisions <- login_timed_read("W08 decisions",active_sheet_w08_decisions)
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
          w01_export_status_rv(if(isTRUE(requested)) "W01 export requested; checking GitHub handoff status." else "")

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
        detail <- conditionMessage(e)
        message("LEM review data initialisation failed: ", detail)
        login_status(paste("Review data could not be loaded:", detail))
        FALSE
      })

      if (!isTRUE(loaded)) {
        # Never expose a partially initialised workflow as ready for review.
        authenticated(FALSE)
        current_user(NULL)
        login_initialising(FALSE)
        showNotification("Review data loading failed. See the sign-in error for details; you can retry without logging out.",
          type="error",duration=12)
      } else {
        login_initialising(FALSE)

      }
      })
      }
      # All users enter the standard task dashboard, not the W04 overview.
      # The W04 summary remains event-bound and is only read on explicit refresh.
      session$onFlushed(load_workflow_queues,once=TRUE)
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

  output$w04_queue_load_status <- renderText(w04_queue_load_status_rv())
  w04_requested_task_rv <- reactiveVal("")
  observeEvent(input$w04_open_manual_on_demand,{
    w04_requested_task_rv("manual")
    shiny::isolate({
      if(isTRUE(w04_queue_loaded_rv())) app_view("w04")
      else shiny::isolate(w04_load_requested_rv(w04_load_requested_rv()+1L))
    })
  })
  observeEvent(input$w04_open_conflicts_on_demand,{
    w04_requested_task_rv("conflict")
    if(isTRUE(w04_queue_loaded_rv())) app_view("w04_conflict")
    else w04_load_requested_rv(w04_load_requested_rv()+1L)
  })
  observeEvent(input$w04_open_uncertainties_on_demand,{
    w04_requested_task_rv("resolution")
    if(isTRUE(w04_queue_loaded_rv())) app_view("w04_resolution")
    else w04_load_requested_rv(w04_load_requested_rv()+1L)
  })
  observeEvent(w04_load_requested_rv(),{
    req(w04_load_requested_rv()>0L)
    req(authenticated())
    if(isTRUE(w04_queue_loaded_rv()))return()
    fn <- shiny::isolate(w04_queued_init_rv())
    if(!is.function(fn)) {
      w04_queue_load_status_rv("W04 task initialisation is not ready. Please retry after dashboard loading completes.")
      return()
    }
    w04_queue_load_status_rv("Loading W04 queues and decisions…")
    result <- tryCatch({fn(); TRUE},error=function(e)e)
    if(inherits(result,"error")) {
      w04_queue_load_status_rv(paste("W04 tasks could not be loaded:",conditionMessage(result)))
      return()
    }
    w04_queue_loaded_rv(TRUE)
    w04_queued_init_rv(NULL)
    w04_queue_load_status_rv("")
    destination <- switch(w04_requested_task_rv(),manual="w04",conflict="w04_conflict",resolution="w04_resolution",NULL)
    if(!is.null(destination)) app_view(destination)
    user_id <- as.character(normalise_user_row(current_user())$user_id)
    later::later(function() {
      shiny::withReactiveDomain(session,{
        shiny::isolate(tryCatch(restore_w04_model_uncertainty_on_login(user_id),
          error=function(e)message("W04 uncertainty restoration: ",conditionMessage(e))))
      })
    },delay=0.5)
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

  # Shared W01/W08b pairwise renderer. Source-specific state and decision
  # persistence stay separate; comparison layout is defined only here.
  dedup_pair_cards <- function(left,right,evidence=list(),allow_abstract_edit=FALSE,
                               editing_a=FALSE,editing_b=FALSE,abstract_prefix="w01") {
    fields <- list(
      source=field_pair(left$source,right$source),
      title=field_pair(display_sentence_case_if_all_caps(left$title),
                       display_sentence_case_if_all_caps(right$title)),
      authors=field_pair(left$authors,right$authors),
      year=field_pair(left$year,right$year),
      journal=field_pair(left$journal,right$journal),
      doi=field_pair(normalise_doi_value(left$doi),normalise_doi_value(right$doi),char_level=TRUE),
      source_record_id=field_pair(normalise_source_id_value(left$source_record_id),
                                  normalise_source_id_value(right$source_record_id),char_level=TRUE),
      abstract=field_pair(display_sentence_case_if_all_caps(left$display_abstract %||% left$abstract),
                          display_sentence_case_if_all_caps(right$display_abstract %||% right$abstract))
    )
    fields$source$a <- tags$span(class="source-badge",fields$source$a)
    fields$source$b <- tags$span(class="source-badge",fields$source$b)
    tagList(
      layout_columns(col_widths=c(6,6),
        record_card(left,"Record A",fields,"a",abstract_editing=allow_abstract_edit && editing_a,abstract_prefix=abstract_prefix),
        record_card(right,"Record B",fields,"b",abstract_editing=allow_abstract_edit && editing_b,abstract_prefix=abstract_prefix)),
      card(class="mt-3",card_header("Matching evidence"),
        div(class="evidence-grid",lapply(evidence,function(z)
          div(class="evidence-item",tags$div(class="text-secondary small",z$label),
            tags$strong(z$value)))))
    )
  }
  dedup_navigate <- function(direction,index,set_index,n,first=1L) {
    if(n<1L)return(invisible(FALSE))
    destination <- max(first,min(n,index+direction))
    set_index(destination)
    invisible(TRUE)
  }

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
    edit_state <- w01_abstract_edit_rv()
    editing_a <- !is.null(edit_state) &&
      identical(as.character(edit_state$review_case_id), as.character(z$review_case_id)) &&
      identical(as.character(edit_state$side), "a")
    editing_b <- !is.null(edit_state) &&
      identical(as.character(edit_state$review_case_id), as.character(z$review_case_id)) &&
      identical(as.character(edit_state$side), "b")

    dedup_pair_cards(record_i_view,record_j_view,evidence,
      allow_abstract_edit=TRUE,editing_a=editing_a,editing_b=editing_b)
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

  observeEvent(authenticated(), {
    req(authenticated(),identical(storage_backend(),"google_sheets"))
    # Refresh optional-pause status once at login, not every 30 seconds.
    state <- tryCatch(w02_optional_pause_current(),error=function(e)NULL)
    if(is.null(state)) {
      w02_optional_pause_rv(NULL)
    } else {
      review <- as.character(state$review_id %||% "")
      configured <- trimws(Sys.getenv("LEM_SEARCH_REVIEW_ID",unset=""))
      if(!nzchar(configured) || identical(configured,review)) {
        w02_optional_pause_rv(state)
      }
    }
  })
  output$w02_optional_pause_controls <- renderUI({
    z <- w02_optional_pause_rv()
    if(is.null(z)||!session_can("control_workflows"))return(NULL)
    run <- as.character(z$source_run_id %||% "")
    if(!grepl("^[0-9]+$",run))return(NULL)
    q <- w02_abstract_queue_rv()
    edits <- w02_abstract_edits_rv()
    has_queue <- !is.null(q) && nrow(q)>0 && all(as.character(q$source_run_id)==run)
    pending <- if(has_queue) sum(!as.character(q$record_id) %in% as.character(edits$record_id)) else 0L
    div(class="mt-2 p-2 border rounded",
      tags$strong("W02 paused: optional abstract completion"),
      tags$p(class="text-secondary small",
        sprintf("%s of %s abstracts missing after enrichment. Source run %s.",
          as.character(z$missing_abstracts),as.character(z$canonical_records),run)),
      if(has_queue) {
        tags$p(class="text-secondary small",sprintf("%d manual decisions remain.",pending))
      } else {
        actionButton("w02_optional_prepare","Review missing abstracts",class="btn-outline-primary btn-sm")
      },
      if(!isTRUE(w02_optional_resume_requested_rv())) {
        actionButton("w02_optional_continue",
          if(has_queue) "Complete review and continue W02" else "Skip abstract review and continue W02",
          class="btn-primary btn-sm")
      } else tags$span(class="text-success","W02 resume requested"),
      textOutput("w02_optional_control_status")
    )
  })
  output$w02_optional_control_status <- renderText(w02_abstract_status_rv())
  observeEvent(input$w02_optional_prepare,{
    req(authenticated(),session_can("control_workflows"))
    z <- req(w02_optional_pause_rv())
    id <- as.character(z$source_run_id)
    response <- tryCatch(dispatch_w02_missing_abstract_queue(id,sheet_id_from_env()),
                         error=function(e)e)
    w02_abstract_status_rv(if(inherits(response,"error"))
      paste("Optional queue failed:",conditionMessage(response)) else
      "Abstract queue requested; records will load after publication.")
  })
  observeEvent(input$w02_optional_continue,{
    req(authenticated(),session_can("control_workflows"))
    z <- req(w02_optional_pause_rv())
    id <- as.character(z$source_run_id)
    q <- w02_abstract_queue_rv()
    if(!is.null(q) && nrow(q) && all(as.character(q$source_run_id)==id)) {
      ed <- w02_missing_abstract_edits(id,unique(as.character(q$queue_sha256)))
      if(!setequal(as.character(q$record_id),as.character(ed$record_id))) {
        w02_abstract_status_rv("Optional abstract queue incomplete; save or skip each record.")
        return()
      }
    }
    response <- tryCatch(dispatch_w02_resume(id,publish=TRUE,
                                             google_sheet_id=sheet_id_from_env()),error=function(e)e)
    if(inherits(response,"error")){
      w02_abstract_status_rv(paste("Resume failed:",conditionMessage(response)))
    }else{
      w02_optional_resume_requested_rv(TRUE)
      w02_abstract_status_rv("W02 resume dispatched.")
    }
  })

  # Restore review-bound W02 launch monitoring from Google Sheets on login.
  observeEvent(authenticated(), {
    req(authenticated(),session_can("control_workflows"))
    # Restore launch registry once per login; active run monitoring is separate.
    record <- tryCatch({
      ss <- sheet_id_from_env()
      m <- read_review_metadata_cached(ss)
      review <- as.character(m$value[match("review_id",m$key)])
      if(!length(review)||is.na(review)||!nzchar(review)) return(NULL)
      w02_launch_record(review,ss)
    },error=function(e) NULL)
    if(!is.null(record)) {
      w02_launch_record_rv(record)
      id <- as.character(record$run_id %||% "")
      if(grepl("^[0-9]+$",id)) w02_launch_run_id_rv(id)
    }
  })
  observe({
    req(authenticated(),session_can("control_workflows"))
    record <- w02_launch_record_rv()
    if(is.null(record)||nzchar(w02_launch_run_id_rv()))return()
    invalidateLater(5000,session)
    found <- tryCatch(w02_find_shiny_run(
      as.character(record$launched_at_utc),as.character(record$review_id),
      as.character(record$sheet_id)),error=function(e)e)
    if(inherits(found,"error")) {
      w02_launch_error_rv(conditionMessage(found))
    } else if(!is.null(found)) {
      id <- as.character(found$id)
      w02_launch_run_id_rv(id)
      w02_launch_error_rv("")
      try(append_w02_launch_record(as.character(record$review_id),
        as.character(record$sheet_id),as.character(record$launched_at_utc),id,"identified"),
        silent=TRUE)
    }
  })
  observe({
    req(authenticated(),session_can("control_workflows"))
    id <- w02_launch_run_id_rv()
    if(!nzchar(id))return()
    invalidateLater(5000,session)
    snap <- tryCatch(w02_github_run_progress(id),error=function(e)e)
    if(inherits(snap,"error")) {
      w02_launch_error_rv(conditionMessage(snap))
    } else {
      w02_launch_snapshot_rv(snap)
      w02_launch_error_rv("")
    }
  })
  output$w02_launch_progress <- renderUI({
    m <- w02_launch_snapshot_rv()
    if(is.null(m)||identical(m$status,"completed"))return(NULL)
    pct <- max(0L,min(99L,as.integer(m$progress %||% 0L)))
    tags$div(class="mt-2",
      tags$div(class="progress",role="progressbar",
        `aria-valuenow`=pct,`aria-valuemin`="0",`aria-valuemax`="100",
        tags$div(class="progress-bar progress-bar-striped progress-bar-animated",
          style=paste0("width:",pct,"%"))))
  })
  output$w02_launch_results <- renderUI({
    err <- w02_launch_error_rv()
    m <- w02_launch_snapshot_rv()
    id <- as.character(w02_launch_run_id_rv() %||% "")
    if(!nzchar(id)) return(
      if(nzchar(err)) tags$p(class="text-danger",paste("W02 monitoring:",err))
      else if(!is.null(w02_launch_record_rv()))
        tags$p(class="text-secondary small","W02 requested. Identifying its exact GitHub run…")
      else NULL)
    link <- tags$a(href=sprintf("https://github.com/thesalmonandthetomato/LivingEvidenceMap/actions/runs/%s",id),
                   target="_blank",rel="noopener noreferrer",paste("GitHub Actions run",id))
    if(is.null(m))return(tagList(link,if(nzchar(err))tags$p(class="text-danger",err)))
    msg <- if(identical(m$status,"completed")) {
      if(identical(m$conclusion,"success"))
        "Automated W02 run completed. Review the Shiny adjudication or optional abstract pause before treating W02 as published."
      else paste("W02 run",m$conclusion,". Inspect GitHub Actions before retrying.")
    } else sprintf("W02 running: %d of %d reported Actions steps completed.",
                  m$completed_steps,m$total_steps)
    tagList(tags$p(class=if(identical(m$conclusion,"failure"))"text-danger" else "text-secondary small",msg),link)
  })

  output$w02_launch_controls <- renderUI({
    if(!session_can("control_workflows")) {
      return(div(class="text-secondary small mt-2",
                 "W02 enrichment launch requires workflow administrator permissions."))
    }
    if(!is.null(w02_optional_pause_rv())) return(NULL)
    batch <- as.character(w02_batch_id_rv() %||% "")
    if(nzchar(batch)) {
      return(div(class="text-secondary small mt-2",
        sprintf("W02 launch unavailable: adjudication batch %s has status %s.",
                batch,as.character(w02_batch_status_rv() %||% "unknown"))))
    }
    if(isTRUE(w02_launch_requested_rv()) || !is.null(w02_launch_record_rv())) {
      return(div(class="text-secondary small mt-2",
        "W02 launch requested. Check GitHub Actions before attempting another launch."))
    }
    div(class="mt-2",
      actionButton("w02_begin_enrichment","Begin enrichment",class="btn-primary btn-sm"),
      textOutput("w02_launch_status"))
  })

  output$w02_launch_status <- renderText(w02_launch_status_rv())

  observeEvent(input$w02_begin_enrichment,{
    req(authenticated(),session_can("control_workflows"))
    if(isTRUE(w02_launch_requested_rv()) || nzchar(w02_batch_id_rv())) return()
    launched_at <- format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
    result <- tryCatch({
      sheet_id <- sheet_id_from_env()
      meta <- read_review_metadata_cached(sheet_id)
      if(!all(c("key","value") %in% names(meta))) stop("Invalid review_metadata")
      review_id <- as.character(meta$value[match("review_id",meta$key)])
      if(length(review_id)!=1L || is.na(review_id) ||
         !grepl("^[A-Za-z0-9._-]{3,128}$",review_id))
        stop("Missing valid review_id in active Google Sheet")
      configured <- trimws(Sys.getenv("LEM_SEARCH_REVIEW_ID",unset=""))
      if(nzchar(configured) && !identical(configured,review_id))
        stop("Environment review ID conflicts with active Google Sheet")
      pointer <- dispatch_w02_from_shiny(review_id,sheet_id)
      list(pointer=pointer,review_id=review_id,sheet_id=sheet_id)
    },error=function(e)e)
    if(inherits(result,"error")) {
      w02_launch_status_rv(paste("W02 launch failed:",conditionMessage(result)))
      showNotification(w02_launch_status_rv(),type="error",duration=12)
      return()
    }
    now <- launched_at
    record <- list(review_id=result$review_id,sheet_id=result$sheet_id,launched_at_utc=now,
                   run_id="",state="requested")
    w02_launch_requested_rv(TRUE)
    w02_launch_record_rv(record)
    saved <- tryCatch(append_w02_launch_record(result$review_id,result$sheet_id,now),
                      error=function(e)e)
    if(inherits(saved,"error")) {
      w02_launch_status_rv(paste("W02 dispatched, but persistent monitoring could not be saved:",
                                 conditionMessage(saved)))
    } else {
      w02_launch_status_rv(paste("W02 enrichment requested using",result$pointer))
    }
  })

  observeEvent(input$w02_prepare_abstract_queue,{
    req(authenticated(),session_can("control_workflows"))
    id <- sub("^w02-run-","",as.character(w02_batch_id_rv()))
    if(!grepl("^[0-9]+$",id)) {
      w02_abstract_status_rv("No paused W02 enrichment run available")
      return()
    }
    outcome <- tryCatch(dispatch_w02_missing_abstract_queue(id,sheet_id_from_env()),
                        error=function(e)e)
    if(inherits(outcome,"error")) {
      w02_abstract_status_rv(paste("Queue request failed:",conditionMessage(outcome)))
      showNotification(w02_abstract_status_rv(),type="error")
    } else {
      w02_abstract_status_rv("Optional abstract queue requested. It will appear when GitHub publishes it.")
    }
  })

  # The optional queue is independent of normal W02 conflicts and decisions.
  observeEvent(authenticated(), {
    req(authenticated(),identical(storage_backend(),"google_sheets"))
    # The queue can be refreshed through workflow actions; no idle poll.
    q <- tryCatch(w02_missing_abstract_queue(),error=function(e) {
      w02_abstract_status_rv(paste("Abstract queue unavailable:",conditionMessage(e)))
      NULL
    })
    if(is.null(q)) return()
    required <- c("source_run_id","queue_sha256","record_id","title","doi")
    if(nrow(q) && !all(required %in% names(q))) {
      w02_abstract_status_rv("Invalid W02 missing-abstract queue format")
      return()
    }
    if(!nrow(q)) return()
    runs <- unique(as.character(q$source_run_id))
    shas <- unique(as.character(q$queue_sha256))
    if(length(runs)!=1L || length(shas)!=1L) {
      w02_abstract_status_rv("Ambiguous abstract queue identity")
      return()
    }
    active_run <- sub("^w02-run-","",as.character(w02_batch_id_rv()))
    if(nzchar(w02_batch_id_rv()) && !identical(runs,active_run)) return()
    w02_abstract_queue_rv(q)
    edits <- tryCatch(w02_missing_abstract_edits(runs,shas),error=function(e) {
      w02_abstract_status_rv(conditionMessage(e))
      NULL
    })
    if(!is.null(edits)) w02_abstract_edits_rv(edits)
  })
  output$w02_missing_abstract_editor <- renderUI({
    q <- w02_abstract_queue_rv()
    if(is.null(q)||!nrow(q)) return(NULL)
    n <- nrow(q)
    i <- max(1L,min(as.integer(w02_abstract_idx_rv()),n))
    z <- q[i,,drop=FALSE]
    edited <- w02_abstract_edits_rv()
    done <- if(nrow(edited)) sum(q$record_id %in% edited$record_id) else 0L
    previous <- if(nrow(edited)) edited[edited$record_id==z$record_id[[1L]],,drop=FALSE] else data.frame()
    card(class="mt-3",
      card_header(tags$strong(sprintf("Optional missing-abstract review · %d of %d · %d completed",i,n,done))),
      div(class="p-2",
        tags$h5(as.character(z$title[[1L]])),
        tags$div("DOI: ",as.character(z$doi[[1L]])),
        tags$div(class="text-secondary small",sprintf("Record ID: %s",z$record_id[[1L]])),
        textAreaInput("w02_abstract_manual_text","Enter abstract",
          value=if(nrow(previous)) as.character(previous$abstract[[1L]]) else "",rows=8,width="100%"),
        textInput("w02_abstract_provenance","Abstract source / provenance",
          value=if(nrow(previous)) as.character(previous$provenance[[1L]]) else "",
          placeholder="Publication DOI, publisher page or other verified source"),
        div(class="d-flex gap-2",
          actionButton("w02_abstract_save","Save abstract",class="btn-primary btn-sm"),
          actionButton("w02_abstract_skip","Leave abstract empty",class="btn-outline-secondary btn-sm"),
          actionButton("w02_abstract_prev","Previous",class="btn-outline-secondary btn-sm"),
          actionButton("w02_abstract_next","Next",class="btn-outline-secondary btn-sm")),
        tags$div(class="text-secondary small",textOutput("w02_missing_abstract_status"))
      ))
  })
  output$w02_missing_abstract_status <- renderText(w02_abstract_status_rv())
  observeEvent(input$w02_abstract_prev,{
    w02_abstract_idx_rv(max(1L,w02_abstract_idx_rv()-1L))
  })
  observeEvent(input$w02_abstract_next,{
    q <- w02_abstract_queue_rv()
    if(!is.null(q)) w02_abstract_idx_rv(min(nrow(q),w02_abstract_idx_rv()+1L))
  })
  save_optional_abstract <- function(decision) {
    req(authenticated(),session_can("adjudicate_assigned"))
    q <- w02_abstract_queue_rv()
    req(!is.null(q),nrow(q)>0)
    z <- q[w02_abstract_idx_rv(),,drop=FALSE]
    result <- tryCatch(append_w02_missing_abstract_edit(
      as.character(z$source_run_id[[1L]]),as.character(z$queue_sha256[[1L]]),
      as.character(z$record_id[[1L]]),
      if(identical(decision,"add")) as.character(input$w02_abstract_manual_text %||% "") else "",
      decision,session_reviewer_id(),
      if(identical(decision,"add")) as.character(input$w02_abstract_provenance %||% "") else ""),
      error=function(e)e)
    if(inherits(result,"error")) {
      w02_abstract_status_rv(paste("Save failed:",conditionMessage(result)))
      return()
    }
    edits <- w02_missing_abstract_edits(as.character(z$source_run_id[[1L]]),
                                        as.character(z$queue_sha256[[1L]]))
    w02_abstract_edits_rv(edits)
    w02_abstract_status_rv(sprintf("Saved %s for %s",decision,z$record_id[[1L]]))
    w02_abstract_idx_rv(min(nrow(q),w02_abstract_idx_rv()+1L))
  }
  observeEvent(input$w02_abstract_save,save_optional_abstract("add"))
  observeEvent(input$w02_abstract_skip,save_optional_abstract("skip"))

  output$w02_save_status <- renderText(w02_status())

  save_w02_choice <- function(choice) {
    if(!session_can("adjudicate_assigned")) {
      w02_status("You do not have permission to adjudicate records.")
      return(FALSE)
    }
    z <- w02_current_case()

    assignment_check_start <- proc.time()[["elapsed"]]
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
      w01_export_status_rv("W01 export was requested; checking whether GitHub completed it.")
      status("W01 export request already exists.")
      return()
    }
    dispatched <- tryCatch({
      if (!identical(batch_status_rv(),"review_complete")) {
        mark_review_complete("01",batch_id_rv(),queue_sha_rv(),batch_status_rv)
      }
      if (identical(storage_backend(),"google_sheets")) {
        append_w01_export_request(queue_sha_rv(),batch_id_rv(),"dispatching")
      }
      sheet_id <- trimws(sheet_id_from_env())
      meta <- read_review_metadata_cached(sheet_id)
      if(!all(c("key","value") %in% names(meta))) stop("Invalid review_metadata")
      review_id <- as.character(meta$value[match("review_id",meta$key)])
      if(length(review_id)!=1L || is.na(review_id) ||
         !grepl("^[A-Za-z0-9._-]{3,128}$",review_id))
        stop("Missing or invalid review_id in Google Sheet review_metadata")
      configured <- trimws(Sys.getenv("LEM_SEARCH_REVIEW_ID",unset=""))
      if(nzchar(configured) && !identical(configured,review_id))
        stop("Configured LEM_SEARCH_REVIEW_ID conflicts with Google Sheet review_id")
      dispatch_w01_export(batch_id_rv(),queue_sha_rv(),
                          review_id=review_id,google_sheet_id=sheet_id)
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
      w01_export_status_rv("GitHub accepted the W01 export request. Awaiting verified handoff.")
      status("W01 export requested; outcome not yet verified.")
    } else {
      w01_export_requested_rv(FALSE)
      w01_export_status_rv("W01 delivery failed. See the status message and retry.")
    }
  })
  # A GitHub dispatch acknowledgement is not a successful export. Inspect the
  # review-specific handoff and exact matching export run, never another review.
  observe({
    req(authenticated(),isTRUE(w01_export_requested_rv()))
    batch <- as.character(batch_id_rv())
    if(!grepl("^w01-run-[0-9]+$",batch)) return()
    invalidateLater(15000,session)
    result <- tryCatch({
      sheet <- sheet_id_from_env()
      meta <- read_review_metadata_cached(sheet)
      review <- as.character(meta$value[match("review_id",meta$key)])
      if(length(review)!=1L || is.na(review) || !nzchar(review))
        stop("Cannot identify review in Google Sheets")
      request_time <- ""
      request_rows <- googlesheets4::read_sheet(sheet,sheet=w01_export_request_tab(),col_types="c")
      matching <- request_rows[
        as.character(request_rows$batch_id)==batch &
        as.character(request_rows$queue_sha256)==as.character(queue_sha_rv()) &
        as.character(request_rows$status) %in% c("dispatching","dispatched"),
        ,drop=FALSE]
      if(nrow(matching)) {
        timestamps <- as.character(matching$requested_at_utc)
        timestamps <- timestamps[grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T",timestamps)]
        if(length(timestamps)) request_time <- min(timestamps)
      }
      w01_export_delivery_status(review,sheet,sub("^w01-run-","",batch),
                                 requested_at_utc=request_time)
    },error=function(e) list(state="unknown",message=paste("W01 delivery status unavailable:",conditionMessage(e))))
    previous_status <- isolate(w01_export_status_rv())
    w01_export_status_rv(as.character(result$message))
    if(identical(result$state,"failed") &&
       !identical(previous_status,as.character(result$message))) {
      status(as.character(result$message))
      showNotification(as.character(result$message),type="error",duration=NULL)
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
    abstract_ready <- tryCatch({
      q <- w02_missing_abstract_queue()
      if(!nrow(q)) {
        TRUE
      } else {
        if(!all(c("source_run_id","queue_sha256","record_id") %in% names(q)))
          stop("Invalid optional abstract queue")
        if(any(is.na(q$source_run_id)) ||
           any(as.character(q$source_run_id)!=source_run_id))
          stop("Optional abstract queue belongs to a different W02 run")
        shas <- unique(as.character(q$queue_sha256))
        if(length(shas)!=1L) stop("Ambiguous optional abstract queue")
        ed <- w02_missing_abstract_edits(source_run_id,shas)
        complete <- setequal(as.character(q$record_id),as.character(ed$record_id))
        if(!complete) w02_status(sprintf("Optional abstract review incomplete: %d records remain.",
                            sum(!q$record_id %in% ed$record_id)))
        complete
      }
    },error=function(e){w02_status(paste("W02 abstract queue check failed:",conditionMessage(e)));FALSE})
    if(!isTRUE(abstract_ready)) return(FALSE)
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
      dispatch_w02_resume(source_run_id, publish = TRUE,
                          google_sheet_id=sheet_id_from_env())
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
    req(authenticated())
    cases <- w04_cases_rv()
    index <- w04_idx()
    req(is.list(cases), length(cases) > 0L,
        length(index) == 1L, !is.na(index),
        index >= 1L, index <= length(cases))
    cases[[index]]
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

  w04_manual_current_abstract_edit <- reactive({
    z <- w04_current_case()
    current <- w04_manual_abstract_edits_rv() %||% list()
    hits <- Filter(function(x) {
      identical(as.character(x$review_case_id %||% ""),as.character(z$review_case_id %||% "")) &&
        identical(as.character(x$reviewer %||% ""),session_reviewer_id())
    },current)
    if(length(hits)) hits[[1L]] else NULL
  })
  w04_manual_effective_abstract <- reactive({
    edit <- w04_manual_current_abstract_edit()
    if(!is.null(edit) && nzchar(trimws(as.character(edit$abstract %||% ""))))
      return(as.character(edit$abstract))
    z <- w04_current_case()
    original <- (z$bibliographic %||% list())[["abstract"]] %||% ""
    if(grepl("^pending_workflow[0-9]+[[:space:]]*;[[:space:]]*[0-9]+$",trimws(as.character(original)),ignore.case=TRUE))
      return("")
    as.character(original)
  })
  output$w04_manual_abstract_status <- renderText({
    status <- w04_manual_abstract_status_rv()
    if (!nzchar(status)) return("")
    case_id <- as.character(w04_current_case()$review_case_id %||% "")
    if (!identical(case_id,w04_manual_abstract_status_case_rv())) return("")
    status
  })
  observeEvent(w04_current_case()$review_case_id, {
    w04_manual_abstract_status_rv("")
    w04_manual_abstract_status_case_rv("")
  }, ignoreInit=TRUE)
  observeEvent(input$w04_manual_edit_abstract,{
    w04_manual_abstract_edit_rv(!isTRUE(w04_manual_abstract_edit_rv()))
  })
  observeEvent(input$w04_manual_save_abstract,{
    req(authenticated())
    if(!session_can("adjudicate_assigned")) {
      w04_manual_abstract_status_rv("You do not have permission to edit abstracts.")
      return()
    }
    z <- w04_current_case()
    w04_manual_abstract_status_case_rv(as.character(z$review_case_id %||% ""))
    value <- trimws(as.character(input$w04_manual_abstract_text %||% ""))
    if(!nzchar(value)) {
      w04_manual_abstract_status_rv("Enter a non-empty abstract before saving.")
      return()
    }
    prior <- w04_manual_current_abstract_edit()
    edit <- list(review_case_id=as.character(z$review_case_id),
      record_id=as.character(z$record_id),abstract=value,
      reviewer=session_reviewer_id(),
      saved_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"),
      queue_sha256=w04_queue_sha_rv())
    saved <- tryCatch(append_sheet_w04_manual_abstract_edit(edit,prior_edit=prior),
      error=function(e){w04_manual_abstract_status_rv(paste("Abstract save failed:",conditionMessage(e)));NULL})
    if(is.null(saved)) return()
    current <- w04_manual_abstract_edits_rv() %||% list()
    current <- Filter(function(x)!(
      identical(as.character(x$review_case_id %||% ""),as.character(z$review_case_id)) &&
      identical(as.character(x$reviewer %||% ""),session_reviewer_id())),current)
    w04_manual_abstract_edits_rv(c(list(saved),current))
    w04_manual_abstract_edit_rv(FALSE)
    w04_manual_abstract_status_rv("Abstract saved to review Google Sheet.")
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
        ),
        NULL
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
        div(class="d-flex justify-content-between align-items-center mt-2",
          tags$h6(class="abstract-heading mb-0","Abstract"),
          div(class="d-flex align-items-center gap-2",
            if(isTRUE(w04_manual_abstract_edit_rv()))
              actionButton("w04_manual_save_abstract","Save abstract",class="btn-sm btn-primary"),
            actionButton("w04_manual_edit_abstract",
              if(isTRUE(w04_manual_abstract_edit_rv())) "Cancel edit" else "Edit abstract",
              class="btn-sm btn-outline-secondary")
          )
        ),
        if(isTRUE(w04_manual_abstract_edit_rv())) {
          textAreaInput("w04_manual_abstract_text",label=NULL,
            value=w04_manual_effective_abstract(),rows=10,width="100%",
            placeholder="Paste the abstract from a verified source.")
        } else {
          div(class="abstract-text",
            if(nzchar(trimws(w04_manual_effective_abstract())))
              highlight_screening_text(display_sentence_case_if_all_caps(w04_manual_effective_abstract()),
                w04_include_terms(),w04_exclude_terms())
            else tags$span(class="text-secondary","No abstract available"))
        },
        tags$div(class="saved-note",textOutput("w04_manual_abstract_status",inline=TRUE)),

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

  # Retain the exact attempted event until its Google Sheets outcome is known.
  # A retry must retain the original timestamp/ID rather than append a new event.
  w04_pending_save_rv <- reactiveVal(NULL)
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

    pending <- isolate(w04_pending_save_rv())
    if (!is.null(pending) && !(
      identical(as.character(pending$decision$review_case_id),as.character(z$review_case_id)) &&
      identical(as.character(pending$decision$queue_sha256),as.character(w04_queue_sha_rv())) &&
      identical(as.character(pending$decision$decision),as.character(choice))
    )) {
      w04_status("Previous save is unverified. Retry the same decision before changing records or choices.")
      return(FALSE)
    }
    decision <- if (!is.null(pending)) pending$decision else list(
      review_case_id=as.character(z$review_case_id),
      record_id=as.character(z$record_id),
      decision=choice,
      rationale="Manual validation screening in Shiny",
      reviewer=session_reviewer_id(),
      resolved_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"),
      queue_sha256=w04_queue_sha_rv()
    )
    if (is.null(pending)) w04_pending_save_rv(list(decision=decision))

    saved <- tryCatch(
      append_sheet_w04_decision(decision,prior_decision=prior,
        recover_existing=!is.null(pending)),
      error=function(e){w04_status(paste("Save could not be verified. Retry the same decision:",conditionMessage(e)));NULL}
    )
    if(is.null(saved)) return(FALSE)
    w04_pending_save_rv(NULL)

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
    w04_manual_abstract_edit_rv(FALSE)
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

  w04_resolution_highlight_include <- reactive({
    configured <- w04_resolution_include_terms()
    if(length(configured)) configured else w04_include_terms()
  })
  w04_resolution_highlight_exclude <- reactive({
    configured <- w04_resolution_exclude_terms()
    if(length(configured)) configured else w04_exclude_terms()
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
          div(class="record-title flex-grow-1",highlight_screening_text(display_sentence_case_if_all_caps(b$title %||% ""),w04_resolution_highlight_include(),w04_resolution_highlight_exclude())),
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
            div(class="abstract-text",highlight_screening_text(display_sentence_case_if_all_caps(w04_resolution_effective_abstract()),w04_resolution_highlight_include(),w04_resolution_highlight_exclude())),
            if (!is.null(w04_resolution_current_abstract_edit())) {
              tags$div(class="text-secondary small mt-1","Manually edited abstract saved for this W04 resolution.")
            }
          )
        },
        div(class="mt-3 p-2 border rounded",
          tags$strong("Model decisions: "),
          if(length(votes)) tagList(lapply(seq_along(votes),function(i)tags$span(class="task-badge me-1",sprintf("Pass %d: %s",i,votes[[i]])))) else tags$span(class="text-secondary","No model vote provenance available")
        ),
        div(class="w04-keywords",tags$strong("Keywords: "),highlight_screening_text(b$keywords %||% "",w04_resolution_highlight_include(),w04_resolution_highlight_exclude()))
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
    if (identical(w04_resolution_origin_rv(),"shiny_model")) {
      w04_resolution_status("Experimental model uncertainty decisions remain in Shiny; no production W04 finalisation is permitted.")
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
    recorded_human_notes <- lapply(human_ids,function(uid) {
      event <- (case_decisions[[uid]] %||% list())$event %||% list()
      value <- trimws(as.character(event$note %||% event$rationale %||% ""))
      list(uid=uid,note=value)
    })
    recorded_human_notes <- Filter(function(z)nzchar(z$note),recorded_human_notes)
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
        if("model" %in% names(case_decisions)) {
          div(class="mt-3 p-2 border rounded",
            tags$strong("Individual model votes and explanations"),
            if(length(z$model_vote_details %||% list()))
              tagList(lapply(z$model_vote_details,function(v) {
                div(class="small mt-2",
                  tags$strong(sprintf("Pass %s: %s",
                    as.character(v$pass),as.character(v$decision))),
                  tags$div(as.character(v$reason %||% "No reason archived"))
                )
              }))
            else tags$div(if(nzchar(as.character(z$model_reason %||% "")))
              as.character(z$model_reason)
              else if(nzchar(as.character(z$model_vote_error %||% "")))
                paste("Archived vote reasons unavailable:",as.character(z$model_vote_error))
              else "The published consensus contains no individual reason for this record."),
            if(length(z$model_votes %||% list()))
              tags$div(class="small text-secondary",paste("Consensus votes:",
                paste(unlist(z$model_votes,use.names=FALSE),collapse=", ")))
          )
        },
        if (length(reviewer_badges)) {
          div(
            class="mt-3 p-2 border rounded",
            tags$strong("Decisions for this case: "),
            tagList(reviewer_badges)
          )
        },
        if (length(recorded_human_notes)) {
          div(class="mt-3 p-2 border rounded",
            tags$strong("Original human reviewer notes"),
            tagList(lapply(recorded_human_notes,function(n) div(
              tags$strong(rater_label(n$uid)),tags$div(n$note)))))
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
    set <- w04_active_consistency_conflict_set()
    if(!is.null(set)) {
      target_ids <- tryCatch(as.character(jsonlite::fromJSON(
        as.character(set$conflict_case_ids_json %||% "[]"))),error=function(e)character())
      complete <- sum(vapply(c(remaining,list(saved)),function(d)
        as.character(d$review_case_id %||% "") %in% target_ids,logical(1)))
      w04_summary_record_safe("adjudication",
        as.character(set$conflict_set_id),
        format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"),
        "Adjudication progress",
        paste(complete,"of",length(target_ids),"resolved"),
        metadata=list(conflict_set_id=as.character(set$conflict_set_id),
          completed=as.character(complete)))
    }
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
      assign_country_set="Enter geography",
      assign_none="Assign no country",
      accept_model="Accept model geography",
      override_country_set="Enter geography",
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
        conditionalPanel(
          condition = sprintf("input.w08_decision_%d == 'assign_country_set'", j),
          selectizeInput(
            paste0("w08_geo_",j),
            "Countries",
            choices = w08_country_choices(),
            selected = toupper(as.character(unlist(saved_value$iso3c %||% character(), use.names = FALSE))),
            multiple = TRUE,
            options = list(create = FALSE, persist = FALSE)
          )
        )
      ),
      geography_unresolved = {
        model_selected <- toupper(as.character(unlist(
          saved_value$iso3c %||% av$luna_iso3c %||% av$iso3c %||% character(),
          use.names = FALSE
        )))
        tagList(
          conditionalPanel(
            condition = sprintf("input.w08_decision_%d == 'override_country_set'", j),
            selectizeInput(
              paste0("w08_geo_",j),
              "Countries",
              choices = w08_country_choices(),
              selected = model_selected[nzchar(model_selected)],
              multiple = TRUE,
              options = list(create = FALSE, persist = FALSE)
            )
          )
        )
      },
      geography_evidence_unvalidated = tagList(
        conditionalPanel(
          condition = sprintf("input.w08_decision_%d == 'override_country_set'", j),
          selectizeInput(
            paste0("w08_geo_",j),
            "Countries",
            choices = w08_country_choices(),
            selected = toupper(as.character(unlist(saved_value$iso3c %||% character(), use.names = FALSE))),
            multiple = TRUE,
            options = list(create = FALSE, persist = FALSE)
          )
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
            class="d-flex align-items-start justify-content-between gap-2",
            div(
              class="record-title flex-grow-1",
              highlight_named_terms(display_sentence_case_if_all_caps(z$title %||% ""),highlight_terms)
            ),
            google_scholar_button(z$title %||% "")
          ),
          div(class="w04-citation-grid",
            div(class="w04-citation-item",span(class="w04-citation-label","Authors"),span(class="w04-citation-value",z$authors %||% "")),
            div(class="w04-citation-item",span(class="w04-citation-label","Year"),span(class="w04-citation-value",z$year %||% "")),
            div(class="w04-citation-item",span(class="w04-citation-label","Journal"),span(class="w04-citation-value",z$journal %||% "")),
            div(class="w04-citation-item",span(class="w04-citation-label","Volume"),span(class="w04-citation-value",z$volume %||% "")),
            div(class="w04-citation-item",span(class="w04-citation-label","Pages"),span(class="w04-citation-value",z$pages %||% ""))
          ),
          div(class="w04-doi",tags$strong("DOI: "),doi_link(z$doi %||% "")),
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

    # Administrators may adjudicate W08 records directly. Do not create a
    # per-record assignment as a side effect of Save: that rewrites the shared
    # assignment registry and is unnecessary because the W08 decision event
    # itself records reviewer/provenance. Explicit assignments remain enforced
    # for non-administrators below.
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
    save_start <- proc.time()[["elapsed"]]
    on.exit(message(sprintf("[W01 timing] save_choice_total=%.3fs",proc.time()[["elapsed"]]-save_start)),add=TRUE)
    req(authenticated())
    if(!session_can("adjudicate_assigned")) {
      status("You do not have permission to adjudicate records.")
      return(FALSE)
    }
    z <- current_case()
    assignment_check_start <- proc.time()[["elapsed"]]

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
    message(sprintf("[W01 timing] assignment_check_and_creation=%.3fs",
                    proc.time()[["elapsed"]]-assignment_check_start))

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

    persist_start <- proc.time()[["elapsed"]]
    saved <- withProgress(message="Saving W01 decision to Google Sheets…",value=0,{
      tryCatch(
        save_active_decision(decision, decision_path, prior_decision = prior),
        error = function(e) {
          status(paste("Save failed:", conditionMessage(e)))
          NULL
        }
      )
    })
    message(sprintf("[W01 timing] save_active_decision=%.3fs",
                    proc.time()[["elapsed"]]-persist_start))
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
    advance_start <- proc.time()[["elapsed"]]
    on.exit(message(sprintf("[W01 timing] advance_after_save=%.3fs",proc.time()[["elapsed"]]-advance_start)),add=TRUE)
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
  output$w04_uncertain_active_batch <- renderUI({
    req(authenticated())
    cases <- w04_resolution_cases_rv() %||% list()
    if(!length(cases)) return(tags$p(class="saved-note",
      "No assigned uncertainty cases are loaded. Select a run above and choose Load selected batch, or ask an administrator for an assignment."))
    tags$p(class="saved-note",sprintf("Active batch: %s · %d assigned cases · %d unresolved",
      as.character(w04_resolution_batch_id_rv()),length(cases),
      length(w04_resolution_unresolved_indices())))
  })
  observeEvent(list(input$open_w04_resolution,input$w04_uncertain_open), {
    started <- proc.time()[["elapsed"]]
    on.exit(message(sprintf("[W04 timing] open_model_uncertainties=%.3fs",
      proc.time()[["elapsed"]]-started)),add=TRUE)
    if(!length(w04_resolution_cases_rv() %||% list())) {
      w04_resolution_status("No uncertainty records are assigned to your account. Ask the administrator to assign this model batch.")
      return()
    }
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
  observeEvent(input$back_to_tasks_w04_conflict, {
    w04_admin_direct_conflict_set_rv("")
    app_view("tasks")
  })
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
  observeEvent(input$w04_previous, {
    w04_manual_abstract_edit_rv(FALSE)
    if(w04_idx()>1L) w04_idx(w04_idx()-1L)
  })
  observeEvent(input$w04_next, {
    w04_manual_abstract_edit_rv(FALSE)
    if(w04_idx()<length(w04_cases_rv())) w04_idx(w04_idx()+1L)
  })

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
    # Login initialisation already reads these metrics. Do not launch a
    # concurrent duplicate refresh when the initial Tasks view appears.
    if (isTRUE(shiny::isolate(login_initialising()))) return()

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
  observeEvent(input$previous, dedup_navigate(-1L,idx(),idx,length(cases_rv())))
  observeEvent(input[["next"]], dedup_navigate(1L,idx(),idx,length(cases_rv())))
  observeEvent(input$review_last, {
    complete(FALSE)
    idx(length(cases_rv()))
  })

  output$save_status <- renderText(status())
}

shinyApp(ui, server)
