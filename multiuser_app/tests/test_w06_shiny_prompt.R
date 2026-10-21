# Offline W06 prompt configuration validation. No Zenodo or GitHub writes.
`%||%` <- function(x,y) if(is.null(x)||!length(x))y else x
source("R/w06_shiny_prompt.R")
tmp <- tempfile(fileext=".txt")
writeLines(c("Identify the reported study location.", "Do not use author affiliations."),tmp)
text <- w06_shiny_prompt_text(tmp)
stopifnot(grepl("author affiliations",text,fixed=TRUE))
writeLines("  ",tmp)
stopifnot(inherits(try(w06_shiny_prompt_text(tmp),silent=TRUE),"try-error"))
unlink(tmp)
meta <- list(values=c(w06_prompt_zenodo_draft_id="123456"))
stopifnot(identical(w06_shiny_prompt_draft_id(meta),"123456"))
stopifnot(identical(w06_shiny_prompt_draft_id(list(values=c(review_id="demo"))),""))
stopifnot(inherits(try(w06_shiny_prompt_draft_id(
  list(values=c(w06_prompt_zenodo_draft_id="bad-id"))),silent=TRUE),"try-error"))
# A DOCX must remain importable when xml2 is absent in the Shiny deployment.
xml_fixture <- paste0('<w:document><w:body><w:p><w:r><w:t>',
  'Research in Almer&amp;iacute;a &amp; elsewhere', 
  '</w:t></w:r></w:p><w:p><w:r><w:t>Study geography</w:t></w:r></w:p></w:body></w:document>')
plain <- w06_shiny_docx_decode(xml_fixture)
stopifnot(grepl("Study geography",plain,fixed=TRUE),
          grepl("Research in",plain,fixed=TRUE),
          !grepl("<w:",plain,fixed=TRUE))
cat("PASS dependency-free W06 DOCX fallback extraction\\n")
cat("PASS W06 topic-agnostic prompt validation, empty-file guard and review draft ID checks\n")
