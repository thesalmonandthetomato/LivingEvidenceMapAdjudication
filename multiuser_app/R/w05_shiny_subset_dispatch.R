# Isolated W05 Shiny runner. Does not touch LivingEvidenceMap production workflows.
w05_shiny_repo <- function() "thesalmonandthetomato/LivingEvidenceMapAdjudication"
w05_shiny_subset_validate_request <- function(request) {
  required <- c("review_id","zenodo_draft_id","input_filename","input_sha256",
                "dictionary_filename","dictionary_sha256","subset_count")
  if(!is.list(request)||!all(required%in%names(request)))
    stop("W05 Shiny subset request is incomplete",call.=FALSE)
  v <- lapply(request[required],function(x) {
    if(length(x)!=1L||is.na(x))stop("Invalid W05 request field",call.=FALSE)
    as.character(x)
  })
  if(!grepl("^[A-Za-z0-9][A-Za-z0-9._-]{2,127}$",v$review_id) ||
     !grepl("^[0-9]+$",v$zenodo_draft_id) ||
     !all(vapply(v[c("input_filename","dictionary_filename")],
        function(x)grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$",x),logical(1))) ||
     !all(vapply(v[c("input_sha256","dictionary_sha256")],
        function(x)grepl("^[0-9a-f]{64}$",x),logical(1))) ||
     !grepl("^[0-9]+$",v$subset_count) ||
     is.na(suppressWarnings(as.integer(v$subset_count))) ||
     as.integer(v$subset_count)<1L)
    stop("Invalid W05 Shiny subset provenance or size",call.=FALSE)
  v
}
w05_shiny_gh_request <- function(url,method="GET",payload=NULL) {
  token <- github_dispatch_token()
  req <- httr2::request(url) |>
    httr2::req_headers(Authorization=paste("Bearer",token),
      Accept="application/vnd.github+json",
      `X-GitHub-Api-Version`="2022-11-28",
      `User-Agent`="LivingEvidenceMap-Adjudication")
  if(identical(method,"POST"))
    req <- httr2::req_method(req,"POST") |> httr2::req_body_json(payload,auto_unbox=TRUE)
  resp <- httr2::req_perform(httr2::req_timeout(req,30))
  status <- httr2::resp_status(resp)
  if(identical(method,"POST") && status!=204L)
    stop(sprintf("W05 Shiny Actions dispatch failed: HTTP %d",status),call.=FALSE)
  if(identical(method,"POST"))return(invisible(TRUE))
  if(status!=200L)stop(sprintf("W05 Shiny Actions status failed: HTTP %d",status),call.=FALSE)
  httr2::resp_body_json(resp,simplifyVector=FALSE)
}
w05_shiny_subset_dispatch <- function(request, w04_published=FALSE) {
  if(!isTRUE(w04_published))
    stop("W05 Shiny subset is paused until verified W04 publication",call.=FALSE)
  v <- w05_shiny_subset_validate_request(request)
  url <- sprintf("https://api.github.com/repos/%s/actions/workflows/w05_shiny_subset.yml/dispatches",
                  w05_shiny_repo())
  w05_shiny_gh_request(url,"POST",list(ref="main",inputs=v))
  invisible(TRUE)
}
