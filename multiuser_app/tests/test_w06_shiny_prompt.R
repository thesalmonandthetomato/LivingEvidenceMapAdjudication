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
# Test a genuine DOCX (ZIP archive), not only isolated XML strings.
docx_dir <- tempfile("w06-docx-")
dir.create(file.path(docx_dir,"word"),recursive=TRUE)
writeLines(
  c('<?xml version="1.0" encoding="UTF-8"?>',
    '<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">',
    '<w:body><w:p><w:r><w:t>Study location</w:t></w:r></w:p>',
    '<w:p><w:r><w:t>Include reported areas</w:t></w:r></w:p></w:body></w:document>'),
  file.path(docx_dir,"word","document.xml"),useBytes=TRUE)
docx_file <- tempfile(fileext=".docx")
old_wd <- getwd()
setwd(docx_dir)
zip_result <- try(utils::zip(docx_file,files="word/document.xml",flags="-q"),silent=TRUE)
setwd(old_wd)
stopifnot(!inherits(zip_result,"try-error"),file.exists(docx_file))
# Directly test the no-xml2 branch even when the CI runner has xml2 installed.
xml_text <- readLines(unz(docx_file,"word/document.xml"),warn=FALSE)
stopifnot(grepl("Study location",w06_shiny_docx_decode(paste(xml_text,collapse="")),fixed=TRUE))
source("R/w04_prompt_versions.R")
imported <- w06_shiny_import_prompt(docx_file,"geography.DOCX")
stopifnot(grepl("Study location",imported,fixed=TRUE),
          grepl("Include reported areas",imported,fixed=TRUE))
unlink(docx_file)
unlink(docx_dir,recursive=TRUE)
for(ext in c("txt","md")) {
  testfile <- tempfile(fileext=paste0(".",ext))
  writeLines("Prompt in plain text",testfile)
  stopifnot(identical(w06_shiny_import_prompt(testfile,paste0("prompt.",ext)),
                      "Prompt in plain text"))
  unlink(testfile)
}
cat("PASS genuine DOCX import and TXT/Markdown compatibility\\n")
cat("PASS dependency-free W06 DOCX fallback extraction\\n")
cat("PASS W06 topic-agnostic prompt validation, empty-file guard and review draft ID checks\n")
