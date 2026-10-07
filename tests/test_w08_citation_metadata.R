app <- paste(readLines("app.R", warn=FALSE, encoding="UTF-8"), collapse="\n")

start <- regexpr('output$w08_case_view <- renderUI({', app, fixed=TRUE)[[1L]]
stopifnot(start > 0L)
rest <- substring(app, start)
end <- regexpr('\n\n  output$w08_save_status <-', rest, fixed=TRUE)[[1L]]
stopifnot(end > 0L)
block <- substring(rest, 1L, end - 1L)

stopifnot(
  grepl('google_scholar_button(z$title %||% "")', block, fixed=TRUE),
  grepl('"Authors"', block, fixed=TRUE),
  grepl('z$authors %||% ""', block, fixed=TRUE),
  grepl('"Year"', block, fixed=TRUE),
  grepl('z$year %||% ""', block, fixed=TRUE),
  grepl('"Journal"', block, fixed=TRUE),
  grepl('z$journal %||% ""', block, fixed=TRUE),
  grepl('"Volume"', block, fixed=TRUE),
  grepl('z$volume %||% ""', block, fixed=TRUE),
  grepl('"Pages"', block, fixed=TRUE),
  grepl('z$pages %||% ""', block, fixed=TRUE),
  grepl('doi_link(z$doi %||% "")', block, fixed=TRUE)
)

cat("PASS: W08 review renders the W04-style citation metadata block\n")
