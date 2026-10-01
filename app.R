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

cases <- read_w01_cases(queue_path)
queue_sha <- digest(file = queue_path, algo = "sha256", serialize = FALSE)
dir.create(dirname(decision_path), recursive = TRUE, showWarnings = FALSE)

theme <- bs_theme(
  version = 5,
  bg = "#f7f8fa",
  fg = "#17212b",
  primary = "#1f5d50",
  base_font = font_google("Source Sans 3"),
  heading_font = font_google("Source Sans 3")
)

record_card <- function(rec, label) {
  card(
    class = "h-100 record-card",
    card_header(div(class = "d-flex justify-content-between align-items-center",
                    tags$strong(label),
                    tags$span(class = "source-badge", rec$source %||% ""))),
    div(class = "record-title", rec$title %||% ""),
    tags$dl(
      class = "record-meta",
      tags$dt("Authors"), tags$dd(rec$authors %||% ""),
      tags$dt("Year"), tags$dd(rec$year %||% ""),
      tags$dt("Journal"), tags$dd(rec$journal %||% ""),
      tags$dt("DOI"), tags$dd(rec$doi %||% ""),
      tags$dt("Source ID"), tags$dd(rec$source_record_id %||% "")
    ),
    tags$hr(),
    tags$h6("Abstract"),
    div(class = "abstract-text", rec$abstract %||% "No abstract available.")
  )
}

ui <- page_fillable(
  theme = theme,
  tags$head(tags$style(HTML("
    body { background:#f7f8fa; }
    .app-shell { max-width:1500px; margin:0 auto; padding:20px; width:100%; }
    .login-shell { max-width:520px; margin:8vh auto 0 auto; padding:20px; width:100%; }
    .record-card { border:1px solid #dde3e8; box-shadow:0 2px 10px rgba(22,33,43,.05); }
    .record-card .card-body { padding:.9rem 1rem; }
    .record-title { font-size:1.08rem; font-weight:700; line-height:1.3; margin-bottom:.65rem; }
    .record-meta { display:grid; grid-template-columns:80px 1fr; gap:.15rem .65rem; margin:0; }
    .record-meta dt { color:#66727d; font-weight:600; }
    .record-meta dd { margin:0; overflow-wrap:anywhere; }
    .abstract-text { line-height:1.42; white-space:pre-wrap; }
    .source-badge { background:#eef3f1; border-radius:999px; padding:.2rem .55rem; font-size:.78rem; }
    .evidence-grid { display:grid; grid-template-columns:repeat(auto-fit,minmax(180px,1fr)); gap:.6rem; }
    .evidence-item { background:#fff; border:1px solid #e1e5e9; border-radius:8px; padding:.65rem .75rem; }
    .decision-panel { margin-bottom:.85rem; }
    .decision-panel .card-body { padding:.75rem 1rem; }
    .decision-row .btn { min-width:125px; }
    .decision-panel .form-group { margin-bottom:0; }
    .decision-panel textarea { min-height:38px !important; height:38px !important; resize:vertical; }
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
  status <- reactiveVal("")
  decisions <- reactiveVal(list())

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

    div(
      class = "app-shell",
      div(class = "d-flex justify-content-between align-items-center mb-3",
          div(tags$h2("LivingEvidenceMap adjudication", class="mb-0"),
              tags$div("Workflow 01 · duplicate review", class="text-secondary")),
          div(textOutput("progress_text"))
      ),
      uiOutput("progress_bar"),
      card(
        class = "decision-panel",
        layout_columns(
          col_widths = c(5, 7),
          div(
            textAreaInput(
              "rationale", "Rationale", rows = 1,
              placeholder = "Brief reason for the decision"
            ),
            tags$div(class="saved-note", textOutput("save_status"))
          ),
          div(
            class = "d-flex flex-column justify-content-end h-100 gap-2",
            div(
              class = "decision-row d-flex flex-wrap justify-content-end gap-2",
              actionButton("duplicate", "Same record", class = "btn-success"),
              actionButton("not_duplicate", "Different records", class = "btn-outline-danger"),
              actionButton("uncertain", "Unsure", class = "btn-outline-secondary")
            ),
            div(
              class = "nav-row d-flex justify-content-end gap-2",
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
      authenticated(TRUE)
      failed_attempts(0L)
      login_status("")
      decisions(read_active_decisions(decision_path))
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

  current_case <- reactive({ req(authenticated()); cases[[idx()]] })

  output$progress_text <- renderText({ req(authenticated()); sprintf("Case %d of %d", idx(), length(cases)) })
  output$progress_bar <- renderUI({
    req(authenticated())
    pct <- round(100 * idx() / length(cases))
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
    tagList(
      layout_columns(
        col_widths = c(6,6),
        record_card(z$record_i, "Record A"),
        record_card(z$record_j, "Record B")
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
    rationale <- trimws(input$rationale %||% "")
    if (!nzchar(rationale)) {
      status("Add a short rationale before saving.")
      return(invisible(FALSE))
    }
    decision <- list(
      review_case_id = z$review_case_id,
      decision = choice,
      rationale = rationale,
      reviewer = reviewer,
      resolved_at_utc = format(Sys.time(), tz="UTC", format="%Y-%m-%dT%H:%M:%SZ"),
      queue_sha256 = queue_sha
    )
    save_active_decision(decision, decision_path)
    decisions(read_active_decisions(decision_path))
    status(sprintf("Saved %s at %s", choice, format(Sys.time(), "%H:%M:%S")))
    updateTextAreaInput(session, "rationale", value="")
    TRUE
  }

  observeEvent(input$duplicate, {
    if (save_choice("duplicate") && idx() < length(cases)) idx(idx()+1L)
  })
  observeEvent(input$not_duplicate, {
    if (save_choice("not_duplicate") && idx() < length(cases)) idx(idx()+1L)
  })
  observeEvent(input$uncertain, {
    if (save_choice("uncertain") && idx() < length(cases)) idx(idx()+1L)
  })
  observeEvent(input$previous, if (idx()>1L) idx(idx()-1L))
  observeEvent(input[["next"]], if (idx()<length(cases)) idx(idx()+1L))

  output$save_status <- renderText(status())
}

shinyApp(ui, server)
