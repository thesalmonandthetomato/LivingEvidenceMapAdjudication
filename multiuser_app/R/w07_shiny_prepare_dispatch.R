# Read-only Shiny W05 -> W07 preparation, no production workflow or LLM.
w07_shiny_prepare_dispatch <- function(meta) {
  id <- w05_crop_draft_id(meta)
  if(!grepl("^[0-9]+$",id))
    stop("Published W05 source not registered for this review",call.=FALSE)
  dep <- w04_prompt_zenodo_request(w04_prompt_deposit_endpoint(id))
  if(!isTRUE(dep$submitted) ||
     !identical(as.character(dep$metadata$title),
       paste("Living Evidence Map W05 crop coding |",meta$review_id)))
    stop("W07 requires this review's published W05 dataset",call.=FALSE)
  names <- vapply(dep$files %||% list(),
    function(x)as.character(x$filename %||% ""),character(1))
  if(!all(c("w05-final-manifest.json","w05-final-assignments.csv") %in% names))
    stop("Final W05 files are incomplete",call.=FALSE)
  payload <- list(ref="main",inputs=list(
    review_id=as.character(meta$review_id),w05_deposit_id=id))
  url <- sprintf("https://api.github.com/repos/%s/actions/workflows/w07_shiny_prepare.yml/dispatches",
    w05_shiny_repo())
  w05_shiny_gh_request(url,"POST",payload)
  invisible(TRUE)
}
