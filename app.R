suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(jsonlite)
  library(digest)
})

source("R/w01_contract.R", local = TRUE)
source("R/storage_local.R", local = TRUE)
source("R/storage_sheets.R", local = TRUE)
source("R/storage_backend.R", local = TRUE)
source("R/auth.R", local = TRUE)
source("R/github_dispatch.R", local = TRUE)

queue_path <- Sys.getenv("LEM_W01_QUEUE", unset = "fixtures/w01_real_sample_2.jsonl")
decision_path <- Sys.getenv("LEM_W01_DECISIONS", unset = "local_state/w01_decisions.jsonl")
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
  x <- gsub("[\\r\\n\\t]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

highlight_screening_text <- function(text, include_terms = character(), exclude_terms = character()) {
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
    parts <- lapply(seq_along(tokens), function(k) {
      cls <- if (matched[[k]]) "diff-same" else "diff-different"
      tagList(tags$span(class = cls, tokens[[k]]),
              if (!char_level && k < length(tokens)) " " else NULL)
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
  tags$head(tags$style(HTML("
    body { background:#f7f8fa; }
    .app-shell { max-width:1500px; margin:0 auto; padding:20px; width:100%; }
    .login-shell { max-width:520px; margin:8vh auto 0 auto; padding:20px; width:100%; }
    .record-card { border:1px solid #dde3e8; box-shadow:0 2px 10px rgba(22,33,43,.05); }
    .record-card .card-body { padding:0; }
    .compact-record-body { padding:.65rem .9rem .8rem .9rem; }
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
    .source-badge { border-radius:999px; padding:.2rem .55rem; font-size:.78rem; }
    .diff-same { background:#d9f2df; color:#145c2e; border-radius:3px; padding:0 .08rem; }
    .diff-different { background:#fde0e0; color:#8b1e1e; border-radius:3px; padding:0 .08rem; }
    .source-badge.diff-same { background:#d9f2df; color:#145c2e; }
    .source-badge.diff-different { background:#fde0e0; color:#8b1e1e; }
    .evidence-grid { display:grid; grid-template-columns:repeat(auto-fit,minmax(180px,1fr)); gap:.6rem; }
    .evidence-item { background:#fff; border:1px solid #e1e5e9; border-radius:8px; padding:.65rem .75rem; }
    .decision-panel { margin-bottom:.85rem; }
    .decision-panel .card-body { padding:.75rem 1rem; }
    .decision-row .btn { min-width:125px; }
    .saved-note { font-weight:600; color:#1f5d50; min-height:1.2rem; }
    .nav-row .btn { min-width:95px; }
    .task-shell { max-width:1050px; margin:4vh auto 0 auto; padding:20px; width:100%; }
    .task-card { border:1px solid #dde3e8; box-shadow:0 2px 10px rgba(22,33,43,.05); }
    .task-kpis { display:grid; grid-template-columns:repeat(3,minmax(90px,1fr)); gap:.65rem; margin:.8rem 0; }
    .task-kpi { background:#f7f8fa; border:1px solid #e1e5e9; border-radius:8px; padding:.55rem .65rem; }
    .task-kpi strong { display:block; font-size:1.15rem; }
    .task-badge { background:#eef3f1; border-radius:999px; padding:.2rem .55rem; font-size:.78rem; }
  "))),
  uiOutput("root_ui")
)

server <- function(input, output, session) {
  authenticated <- reactiveVal(FALSE)
  app_view <- reactiveVal("tasks")
  failed_attempts <- reactiveVal(0L)
  lock_until <- reactiveVal(as.POSIXct(NA))
  idx <- reactiveVal(1L)
  complete <- reactiveVal(FALSE)
  status <- reactiveVal("")
  cases_rv <- reactiveVal(NULL)
  queue_sha_rv <- reactiveVal("")
  batch_id_rv <- reactiveVal("")
  decisions <- reactiveVal(list())

  w02_cases_rv <- reactiveVal(NULL)
  w02_queue_sha_rv <- reactiveVal("")
  w02_batch_id_rv <- reactiveVal("")
  w02_idx <- reactiveVal(1L)
  w02_decisions <- reactiveVal(list())
  w02_status <- reactiveVal("")

  w04_cases_rv <- reactiveVal(NULL)
  w04_queue_sha_rv <- reactiveVal("")
  w04_batch_id_rv <- reactiveVal("")
  w04_idx <- reactiveVal(1L)
  w04_decisions <- reactiveVal(list())
  w04_status <- reactiveVal("")
  w04_include_terms <- reactiveVal(character())
  w04_exclude_terms <- reactiveVal(character())

  decision_ids <- function(ds = decisions()) {
    if (!length(ds)) return(character())
    unique(vapply(ds, function(x) as.character(x$review_case_id %||% ""), character(1)))
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

  w04_decision_ids <- function(ds = w04_decisions()) {
    if (!length(ds)) return(character())
    unique(vapply(ds,function(x)as.character(x$review_case_id %||% ""),character(1)))
  }

  w04_unresolved_indices <- function() {
    cs <- w04_cases_rv()
    if (is.null(cs)) return(integer())
    ids <- vapply(cs,function(x)as.character(x$review_case_id),character(1))
    which(!ids %in% w04_decision_ids())
  }

  load_w04_batch <- function() {
    if (!identical(storage_backend(), "google_sheets")) return(NULL)
    read_sheet_w04_queue()
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

  output$root_ui <- renderUI({
    if (!authenticated()) {
      return(div(
        class = "login-shell",
        card(
          card_header(tags$strong("LivingEvidenceMap adjudication")),
          tags$p("Enter the adjudication access key to continue."),
          passwordInput("access_key", "Access key"),
          actionButton("login", "Continue", class = "btn-primary"),
          tags$div(class = "mt-2 text-danger", textOutput("login_status"))
        )
      ))
    }

    if (identical(app_view(), "tasks")) {
      total <- length(cases_rv() %||% list())
      remaining <- length(unresolved_indices())
      completed_n <- max(0L, total - remaining)

      return(div(
        class = "task-shell",
        div(
          class = "d-flex justify-content-between align-items-end mb-3",
          div(
            tags$h2("Outstanding adjudication tasks", class = "mb-1"),
            tags$div("Choose a workflow to continue.", class = "text-secondary")
          ),
          tags$span(class = "task-badge", "LivingEvidenceMap")
        ),
        card(
          class = "task-card mb-3",
          card_header(
            div(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Workflow 01 · duplicate review"),
              tags$span(class = "task-badge", batch_id_rv())
            )
          ),
          div(
            class = "p-3",
            tags$p(
              class = "mb-2",
              "Review potential duplicate bibliographic records and decide whether each pair represents the same record."
            ),
            div(
              class = "task-kpis",
              div(class = "task-kpi", tags$span(class = "text-secondary small", "Total"), tags$strong(total)),
              div(class = "task-kpi", tags$span(class = "text-secondary small", "Completed"), tags$strong(completed_n)),
              div(class = "task-kpi", tags$span(class = "text-secondary small", "Remaining"), tags$strong(remaining))
            ),
            actionButton(
              "open_w01",
              if (remaining > 0L) "Continue Workflow 01" else "Review Workflow 01",
              class = "btn-primary"
            )
          )
        ),
        if (!is.null(w02_cases_rv())) {
          w02_total <- length(w02_cases_rv())
          w02_remaining <- length(w02_unresolved_indices())
          w02_completed <- max(0L, w02_total - w02_remaining)
          card(
            class = "task-card mb-3",
            card_header(
              div(
                class = "d-flex justify-content-between align-items-center",
                tags$strong("Workflow 02 · metadata conflict review"),
                tags$span(class = "task-badge", w02_batch_id_rv())
              )
            ),
            div(
              class = "p-3",
              tags$p(
                class = "mb-2",
                "Review quarantined bibliographic enrichment conflicts before provider metadata can be accepted or rejected."
              ),
              div(
                class = "task-kpis",
                div(class = "task-kpi", tags$span(class = "text-secondary small", "Total"), tags$strong(w02_total)),
                div(class = "task-kpi", tags$span(class = "text-secondary small", "Completed"), tags$strong(w02_completed)),
                div(class = "task-kpi", tags$span(class = "text-secondary small", "Remaining"), tags$strong(w02_remaining))
              ),
              actionButton(
                "open_w02",
                if (w02_remaining > 0L) "Continue Workflow 02" else "Review Workflow 02",
                class = "btn-primary"
              )
            )
          )
        },
        if (!is.null(w04_cases_rv())) {
          w04_total <- length(w04_cases_rv())
          w04_remaining <- length(w04_unresolved_indices())
          w04_completed <- max(0L, w04_total - w04_remaining)
          card(
            class = "task-card",
            card_header(
              div(
                class = "d-flex justify-content-between align-items-center",
                tags$strong("Workflow 04 · validation screening"),
                tags$span(class = "task-badge", w04_batch_id_rv())
              )
            ),
            div(
              class = "p-3",
              tags$p(
                class = "mb-2",
                "Blindly screen a randomised sample of titles and abstracts to create independent human validation data for Workflow 04."
              ),
              div(
                class = "task-kpis",
                div(class = "task-kpi", tags$span(class = "text-secondary small", "Total"), tags$strong(w04_total)),
                div(class = "task-kpi", tags$span(class = "text-secondary small", "Completed"), tags$strong(w04_completed)),
                div(class = "task-kpi", tags$span(class = "text-secondary small", "Remaining"), tags$strong(w04_remaining))
              ),
              actionButton(
                "open_w04",
                if (w04_remaining > 0L) "Continue Workflow 04 validation" else "Review Workflow 04 validation",
                class = "btn-primary"
              )
            )
          )
        }
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
            class = "d-flex align-items-center gap-3",
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
            tags$h2("LivingEvidenceMap validation screening", class="mb-0"),
            tags$div(
              sprintf("Workflow 04 · blind human validation · %s", w04_batch_id_rv()),
              class="text-secondary"
            )
          ),
          div(
            class = "d-flex align-items-center gap-3",
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
              div(
                class = "decision-row d-flex flex-wrap gap-2",
                actionButton("w04_retain", "Include", class = "btn-success"),
                actionButton("w04_exclude", "Exclude", class = "btn-outline-danger"),
                actionButton("w04_uncertain", "Unsure", class = "btn-outline-secondary")
              ),
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
            class = "d-flex align-items-center gap-3",
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
            div(
              class = "decision-row d-flex flex-wrap gap-2",
              actionButton("duplicate", "Same record", class = "btn-success"),
              actionButton("not_duplicate", "Different records", class = "btn-outline-danger"),
              actionButton("uncertain", "Unsure", class = "btn-outline-secondary")
            ),
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

  observeEvent(input$login, {
    now <- Sys.time()
    until <- lock_until()
    if (!is.na(until) && now < until) {
      login_status("Too many failed attempts. Try again shortly.")
      return()
    }
    if (access_key_valid(input$access_key)) {
      loaded <- tryCatch({
        batch <- load_batch()
        all_decisions <- read_active_decisions(decision_path)
        current_decisions <- filter_batch_decisions(all_decisions, batch$queue_sha256)
        w02_batch <- load_w02_batch()
        if (!is.null(w02_batch)) {
          w02_all_decisions <- active_sheet_w02_decisions()
          w02_cases_rv(w02_batch$cases)
          w02_queue_sha_rv(w02_batch$queue_sha256)
          w02_batch_id_rv(w02_batch$batch_id)
          w02_decisions(w02_filter_batch_decisions(w02_all_decisions, w02_batch$queue_sha256))
          w02_unresolved <- w02_unresolved_indices()
          w02_idx(if (length(w02_unresolved)) w02_unresolved[[1L]] else max(1L, length(w02_batch$cases)))
        }

        w04_batch <- load_w04_batch()
        if (!is.null(w04_batch)) {
          w04_all_decisions <- active_sheet_w04_decisions()
          w04_cases_rv(w04_batch$cases)
          w04_queue_sha_rv(w04_batch$queue_sha256)
          w04_batch_id_rv(w04_batch$batch_id)
          w04_include_terms(w04_batch$highlight_include %||% character())
          w04_exclude_terms(w04_batch$highlight_exclude %||% character())
          w04_decisions(w04_filter_batch_decisions(w04_all_decisions, w04_batch$queue_sha256))
          w04_unresolved <- w04_unresolved_indices()
          w04_idx(if (length(w04_unresolved)) w04_unresolved[[1L]] else max(1L,length(w04_batch$cases)))
        }

        cases_rv(batch$cases)
        queue_sha_rv(batch$queue_sha256)
        batch_id_rv(batch$batch_id)
        decisions(current_decisions)

        ids <- vapply(batch$cases, function(x) as.character(x$review_case_id), character(1))
        done_ids <- if (length(current_decisions)) {
          unique(vapply(current_decisions, function(x) as.character(x$review_case_id), character(1)))
        } else character()
        unresolved <- which(!ids %in% done_ids)

        if (length(unresolved)) {
          idx(unresolved[[1L]])
          complete(FALSE)
        } else {
          idx(length(batch$cases))
          complete(TRUE)
        }
        TRUE
      }, error = function(e) {
        login_status(paste("Batch could not be loaded:", conditionMessage(e)))
        FALSE
      })

      if (isTRUE(loaded)) {
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
        login_status("Invalid access key.")
      }
    }
  })

  current_case <- reactive({ req(authenticated(), cases_rv()); cases_rv()[[idx()]] })

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
    reason <- as.character(z$reason %||% z$conflict$reason %||% "")
    buttons <- if (identical(reason, "returned_doi_mismatch")) {
      tagList(
        actionButton("w02_reject_match", "Reject provider match", class="btn-outline-danger"),
        actionButton("w02_uncertain", "Unsure", class="btn-outline-secondary")
      )
    } else {
      tagList(
        actionButton("w02_accept_field", "Accept provider field", class="btn-success"),
        actionButton("w02_reject_field", "Reject provider field", class="btn-outline-danger"),
        actionButton("w02_uncertain", "Unsure", class="btn-outline-secondary")
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
    z <- w02_current_case()
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
      reviewer=reviewer,
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
      dispatch_completed_w02()
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
    z <- w04_current_case()
    current <- w04_decisions()
    prior <- NULL
    if(length(current)) {
      hits <- Filter(function(x)identical(as.character(x$review_case_id %||% ""),as.character(z$review_case_id)),current)
      if(length(hits)) prior <- hits[[1L]]
    }

    decision <- list(
      review_case_id=as.character(z$review_case_id),
      record_id=as.character(z$record_id),
      decision=choice,
      rationale="Manual validation screening in Shiny",
      reviewer=reviewer,
      resolved_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"),
      queue_sha256=w04_queue_sha_rv()
    )

    saved <- tryCatch(
      append_sheet_w04_decision(decision,prior_decision=prior),
      error=function(e){w04_status(paste("Save failed:",conditionMessage(e)));NULL}
    )
    if(is.null(saved)) return(FALSE)

    remaining <- Filter(
      function(x)!identical(as.character(x$review_case_id %||% ""),as.character(z$review_case_id)),
      current
    )
    w04_decisions(c(remaining,list(saved)))
    w04_status(sprintf("Saved %s at %s",choice,format(Sys.time(),"%H:%M:%S")))
    TRUE
  }

  advance_w04 <- function() {
    unresolved <- w04_unresolved_indices()
    if(!length(unresolved)) {
      app_view("tasks")
      return(invisible(TRUE))
    }
    later <- unresolved[unresolved>w04_idx()]
    w04_idx(if(length(later)) later[[1L]] else unresolved[[1L]])
    invisible(TRUE)
  }

  save_choice <- function(choice) {
    req(authenticated())
    z <- current_case()
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
      reviewer = reviewer,
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
    status(sprintf("Saved %s at %s", choice, format(Sys.time(), "%H:%M:%S")))
    TRUE
  }

  advance_after_save <- function() {
    unresolved <- unresolved_indices()
    if (!length(unresolved)) {
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
