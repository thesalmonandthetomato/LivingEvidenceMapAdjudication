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
