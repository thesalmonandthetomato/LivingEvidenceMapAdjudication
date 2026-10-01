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
  "))),
  uiOutput("root_ui")
)

server <- function(input, output, session) {
  authenticated <- reactiveVal(FALSE)
  failed_attempts <- reactiveVal(0L)
  lock_until <- reactiveVal(as.POSIXct(NA))
  idx <- reactiveVal(1L)
  complete <- reactiveVal(FALSE)
  status <- reactiveVal("")
  cases_rv <- reactiveVal(NULL)
  queue_sha_rv <- reactiveVal("")
  batch_id_rv <- reactiveVal("")
  decisions <- reactiveVal(list())

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

    if (complete()) {
      return(div(
        class = "login-shell",
        card(
          card_header(tags$strong("Adjudication complete")),
          tags$h3("Finished"),
          tags$p(sprintf("All %d cases in this batch have been adjudicated.", length(cases_rv()))),
          tags$p("Your decisions have been saved."),
          actionButton("review_last", "Review last case", class = "btn-outline-secondary")
        )
      ))
    }

    div(
      class = "app-shell",
      div(class = "d-flex justify-content-between align-items-center mb-3",
          div(tags$h2("LivingEvidenceMap adjudication", class="mb-0"),
              tags$div(textOutput("batch_label"), class="text-secondary")),
          div(textOutput("progress_text"))
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
