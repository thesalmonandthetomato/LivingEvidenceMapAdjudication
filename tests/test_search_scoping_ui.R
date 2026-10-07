app <- paste(readLines("app.R", warn=FALSE, encoding="UTF-8"), collapse="\n")
gh <- paste(readLines("R/github_dispatch.R", warn=FALSE, encoding="UTF-8"), collapse="\n")

stopifnot(
  grepl('uiOutput("configure_review")', app, fixed=TRUE),
  grepl('tags$strong("Configure review")', app, fixed=TRUE),
  grepl('Run scoping search', app, fixed=TRUE),
  grepl('Preparing scoping search', app, fixed=TRUE),
  grepl('Searching databases:', app, fixed=TRUE),
  grepl('Finalising scoping report', app, fixed=TRUE),
  grepl('Database', app, fixed=TRUE),
  grepl('search_scope_counts.csv', gh, fixed=TRUE),
  grepl('workflow_00_search_scoping.yml', gh, fixed=TRUE),
  grepl('request_id', gh, fixed=TRUE),
  grepl('workflow00-scope-count-', gh, fixed=TRUE),
  grepl('search-scoping-report-%s.pdf', app, fixed=TRUE),
  grepl('contentType="application/pdf"', app, fixed=TRUE),
  grepl('The Lens, via The Lens', gh, fixed=TRUE)
)

cat("PASS: Configure review W00 scoping UI contract\n")
