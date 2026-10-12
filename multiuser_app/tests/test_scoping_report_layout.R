# Regression guard for scoping PDF text wrapping and Shiny button alignment.
app <- paste(readLines("app.R",warn=FALSE),collapse="\n")
stopifnot(grepl('fit_pdf_lines <- function',app,fixed=TRUE))
stopifnot(grepl('graphics::strwidth(proposal,units="user"',app,fixed=TRUE))
stopifnot(grepl('q_lines <- fit_pdf_lines(versions[[v]],family="mono",cex=0.68)',app,fixed=TRUE))
stopifnot(grepl('class="d-flex align-items-center flex-wrap gap-2 mt-3"',app,fixed=TRUE))
stopifnot(!grepl('class="btn-outline-secondary btn-sm mt-3"',app,fixed=TRUE))
cat("PASS: report width-aware wrapping and aligned scoping actions\n")
