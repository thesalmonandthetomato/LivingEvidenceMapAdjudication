source("multiuser_app/R/w06_shiny_prompt.R")
source("multiuser_app/R/w07_shiny_configuration.R")
write_tmp <- function(lines,extension) {
  p <- tempfile(fileext=extension)
  writeLines(lines,p,useBytes=TRUE)
  p
}
csv <- write_tmp(c("path_id,level_1,level_2,level_3,definition,matching_terms",
  "ALM_001,Production,Crop performance,Yield,Crop yield,yield",
  "ALM_002,Social,Labour,Working conditions,Working conditions,wages"),".csv")
parsed <- w07_shiny_config_parse(csv,"ontology")
stopifnot(identical(parsed$count,2L),grepl("ALM_002",parsed$contents,fixed=TRUE))
bad <- write_tmp(c("path_id,level_1,level_2,level_3,definition",
  "ALM_001,Production,Crop performance,Yield,Crop yield",
  "ALM_001,Production,Crop performance,Quality,Quality"),".csv")
stopifnot(inherits(try(w07_shiny_config_parse(bad,"ontology"),silent=TRUE),"try-error"))
prompt <- write_tmp(c("Classify all substantive topics.","Allow multiple codes."),".txt")
stopifnot(identical(w07_shiny_config_parse(prompt,"prompt")$contents,
  "Classify all substantive topics.\nAllow multiple codes."))
stopifnot(inherits(try(w07_shiny_config_parse(prompt,"other"),silent=TRUE),"try-error"))
cat("PASS W07 Shiny ontology and prompt parse contracts\n")

# Upload names are separate from Shiny's extensionless temporary paths.
dir <- tempfile("w07-docx-test-");dir.create(dir)
writeLines('<?xml version="1.0" encoding="UTF-8"?><w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"><w:body><w:p><w:r><w:t>Code substantive topics only.</w:t></w:r></w:p></w:body></w:document>',
 file.path(dir,"document.xml"))
dir.create(file.path(dir,"word"))
file.copy(file.path(dir,"document.xml"),file.path(dir,"word","document.xml"))
docx <- tempfile(fileext=".docx")
oldwd <- getwd();setwd(dir)
tryCatch(utils::zip(docx,"word/document.xml",flags="-q"),finally=setwd(oldwd))
stopifnot(file.exists(docx))
extensionless <- tempfile("shiny-upload-")
file.copy(docx,extensionless)
doc <- w07_shiny_config_parse(extensionless,"prompt","topic-prompt.docx")
stopifnot(grepl("Code substantive topics only.",doc$contents,fixed=TRUE))
unlink(dir,recursive=TRUE)
cat("PASS W07 DOCX prompt import using extensionless Shiny upload path\\n")
