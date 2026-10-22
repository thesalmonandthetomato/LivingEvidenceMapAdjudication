# Explicit Shiny-only W07 experimental subset dispatch.
w07_shiny_subset_dispatch <- function(meta,ontology_receipt,prompt_receipt,n) {
  if(is.null(ontology_receipt)||is.null(prompt_receipt) ||
     !identical(ontology_receipt$type,"ontology") ||
     !identical(prompt_receipt$type,"prompt"))
    stop("Both W07 ontology and prompt versions must be uploaded",call.=FALSE)
  count <- suppressWarnings(as.integer(n))
  if(length(count)!=1L || is.na(count)||count<1L||count>30L)
    stop("W07 trial sizes must be between 1 and 30 records",call.=FALSE)
  dep <- w07_shiny_config_deposit(meta)
  if(is.null(dep)||!identical(as.character(dep$id),ontology_receipt$draft_id)||
     !identical(as.character(dep$id),prompt_receipt$draft_id))
    stop("W07 trial configuration does not belong to this review",call.=FALSE)
  files <- vapply(dep$files %||% list(),
    function(f)as.character(f$filename %||% ""),character(1))
  for(r in list(ontology_receipt,prompt_receipt)) {
    version <- paste0("w07-",r$type,"-",r$sha256,".json")
    marker <- paste0("w07-approved-",r$type,"-",r$sha256,".json")
    if(!identical(r$filename,version) ||
       !grepl("^[a-f0-9]{64}$",r$sha256) ||
       sum(files==version)!=1L||sum(files==marker)!=1L)
      stop("W07 trial requires explicitly approved immutable versions",call.=FALSE)
  }
  w05 <- w05_crop_draft_id(meta)
  if(!nzchar(w05))stop("W05 source is not registered",call.=FALSE)
  source <- w04_prompt_zenodo_request(w04_prompt_deposit_endpoint(w05))
  if(!isTRUE(source$submitted) ||
     !identical(as.character(source$metadata$title),
       paste("Living Evidence Map W05 crop coding |",meta$review_id)))
    stop("W07 trial requires published review-matching W05 source",call.=FALSE)
  url <- sprintf(
    "https://api.github.com/repos/%s/actions/workflows/w07_shiny_subset.yml/dispatches",
    w05_shiny_repo())
  request <- list(ref="main",inputs=list(review_id=as.character(meta$review_id),
    w05_deposit_id=w05,config_deposit_id=as.character(dep$id),
    ontology_sha=ontology_receipt$sha256,prompt_sha=prompt_receipt$sha256,
    subset_count=as.character(count)))
  w05_shiny_gh_request(url,"POST",request)
  invisible(TRUE)
}
