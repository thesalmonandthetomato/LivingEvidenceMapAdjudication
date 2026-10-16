# Only resolve a W04 source through an existing review-bound W01 -> W02 -> W03
# chain. Never use the latest global W03 run without this binding.
w04_subset_review_source_pointers <- function() {
  meta <- w04_prompt_review_metadata()
  state <- read_review_published_w01_w02(meta$review_id,meta$sheet_id)
  if(is.null(state)||!identical(as.character(state$stage),"W03")||
     !grepl("^[0-9]+$",as.character(state$run_id)))
    stop("This review does not yet have a verified published W03 source",call.=FALSE)
  w03_path <- sprintf("docs/publication_status/zenodo/run-%s.json",state$run_id)
  w03 <- jsonlite::fromJSON(read_github_text_file(w03_path),
                             simplifyVector=FALSE)
  if(!identical(as.character(w03$publication_github_run_id %||% ""),
                as.character(state$run_id)))
    stop("Review-bound W03 run identity mismatch",call.=FALSE)
  lean_sha <- tolower(w04_subset_scalar(w03$upstream_lean_canonical_sha256))
  if(!grepl("^[0-9a-f]{64}$",lean_sha))
    stop("W03 pointer has no valid upstream lean SHA",call.=FALSE)
  dir <- "docs/compaction/zenodo"
  endpoint <- sprintf("https://api.github.com/repos/%s/contents/%s?ref=main",
                      github_dispatch_repo(),dir)
  entries <- github_api_get_json(endpoint)
  fn <- vapply(entries,function(x)as.character(x$name %||% ""),character(1))
  fn <- fn[grepl("^run-[0-9]+[.]json$",fn)]
  matches <- list()
  for(name in fn) {
    z <- jsonlite::fromJSON(read_github_text_file(paste0(dir,"/",name)),
                            simplifyVector=FALSE)
    if(identical(tolower(w04_subset_scalar(z$lean_canonical_sha256)),lean_sha))
      matches[[length(matches)+1L]] <- list(pointer=z,path=paste0(dir,"/",name))
  }
  if(!length(matches)) stop("Cannot find W03 upstream published lean pointer",call.=FALSE)
  # Identical SHA means the underlying canonical source bytes are identical.
  lean <- matches[[1L]]$pointer
  w04_subset_valid_pointer_pair(lean,w03)
  list(review_id=meta$review_id,lean=lean,w03=w03,
       lean_path=matches[[1L]]$path,w03_path=w03_path,
       source_run_id=as.character(state$run_id))
}
