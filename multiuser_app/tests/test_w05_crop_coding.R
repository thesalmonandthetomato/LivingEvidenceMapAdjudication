# Run from multiuser_app with Rscript tests/test_w05_crop_coding.R.
`%||%` <- function(x,y)if(is.null(x))y else x
source("R/w05_crop_coding.R")
d <- w05_crop_read("config/w05_almeria_crop_dictionary.csv")
stopifnot(nrow(d)==48L,identical(tail(d$code,1L),"unspecified_horticultural_crop"),
          identical(tail(d$matching_terms,1L),""))
examples <- data.frame(record_id=c("a","b","c","d"),
  title=c("Tomato and pepper cultivation","Cultivo del pepino",
          "Unrelated text about farm economics","Chilli pepper trial"),
  abstract=c("Solanum lycopersicum","","","jalapeño"),
  stringsAsFactors=FALSE)
x <- w05_crop_code_records(examples,d)
lookup <- split(x$assignments$code[x$assignments$entity=="crop"],x$assignments$record_id[x$assignments$entity=="crop"])
stopifnot(all(c("tomato","pepper")%in%lookup$a),
          identical(lookup$b,"cucumber"),
          identical(lookup$c,"unspecified_horticultural_crop"),
          "chilli_pepper"%in%lookup$d,
          !("unspecified_horticultural_crop"%in%lookup$a),
          sum(x$frequencies$records[x$frequencies$code=="unspecified_horticultural_crop"])==1L)
bad <- d
bad$matching_terms[bad$code=="unspecified_horticultural_crop"] <- "greenhouse"
stopifnot(inherits(try(w05_crop_validate(bad),silent=TRUE),"try-error"))
cat("PASS W05 crop CSV schema, multilingual matches, multi-crop and automatic unmatched fallback\n")

# Two entity types, independent matching and multiple codes on one record.
multi <- rbind(d,data.frame(entity=c("practice","practice"),
  code=c("irrigation","integrated_pest_management"),
  matching_terms=c("irrigation;riego","integrated pest management;IPM"),
  stringsAsFactors=FALSE))
multi_result <- w05_crop_code_records(data.frame(
  record_id=c("multi1","multi2"),
  title=c("Tomato irrigation and IPM","Irrigation system performance"),
  abstract=c("",""),stringsAsFactors=FALSE),multi)
one <- multi_result$assignments[multi_result$assignments$record_id=="multi1",]
two <- multi_result$assignments[multi_result$assignments$record_id=="multi2",]
stopifnot(all(c("tomato")%in%one$code),
          all(c("irrigation","integrated_pest_management")%in%one$code),
          "unspecified_horticultural_crop"%in%two$code,
          !any(two$entity=="practice" & two$code=="unspecified_horticultural_crop"),
          all(c("entity","code","records")%in%names(multi_result$frequencies)))
cat("PASS multi-entity assignments and crop-only fallback\n")
