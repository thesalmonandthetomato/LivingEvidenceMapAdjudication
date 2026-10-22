# Read-only W07 Shiny trial results, never alters or finalises model outputs.
w07_shiny_load_results <- function(run_id,api=w05_shiny_gh_request,download=NULL) {
  id <- as.character(run_id)
  if(length(id)!=1L || !grepl("^[0-9]+$",id))stop("Invalid W07 run ID",call.=FALSE)
  base <- paste0("https://api.github.com/repos/",w05_shiny_repo(),"/actions/runs/",id)
  run <- api(base)
  if(!identical(as.character(run$path %||% ""),".github/workflows/w07_shiny_subset.yml") ||
     !identical(as.character(run$conclusion %||% ""),"success"))
    stop("This is not a successful isolated W07 subset run",call.=FALSE)
  arts <- api(paste0(base,"/artifacts"))$artifacts %||% list()
  name <- paste0("w07-shiny-subset-",id)
  matches <- Filter(function(x)identical(as.character(x$name %||% ""),name) &&
                    !isTRUE(x$expired),arts)
  if(length(matches)!=1L)stop("W07 results artefact is missing or expired",call.=FALSE)
  url <- as.character(matches[[1L]]$archive_download_url %||% "")
  if(!startsWith(url,paste0("https://api.github.com/repos/",w05_shiny_repo(),
                            "/actions/artifacts/")))
    stop("Unexpected W07 artefact URL",call.=FALSE)
  if(is.null(download))download <- function(url,dest) {
    req <- httr2::request(url) |> httr2::req_headers(
      Authorization=paste("Bearer",github_dispatch_token()),
      Accept="application/vnd.github+json")
    httr2::req_perform(httr2::req_timeout(req,90),path=dest)
  }
  zip <- tempfile(fileext=".zip")
  folder <- tempfile("w07-results-")
  dir.create(folder)
  on.exit(unlink(c(zip,folder),recursive=TRUE),add=TRUE)
  download(url,zip)
  entries <- utils::unzip(zip,list=TRUE)$Name
  needed <- c("w07_subset_results.jsonl","w07_subset_manifest.json")
  if(!setequal(entries,needed)||anyDuplicated(entries))
    stop("W07 results archive has unexpected contents",call.=FALSE)
  utils::unzip(zip,files=needed,exdir=folder)
  manifest <- jsonlite::fromJSON(file.path(folder,needed[[2L]]),simplifyVector=FALSE)
  if(!identical(manifest$mode,"subset_test")||
     !identical(manifest$schema,"lem-w07-shiny-subset-input-v1"))
    stop("Unexpected W07 subset manifest",call.=FALSE)
  lines <- readLines(file.path(folder,needed[[1L]]),warn=FALSE,encoding="UTF-8")
  if(length(lines)!=as.integer(manifest$subset_records))
    stop("W07 results count differs from manifest",call.=FALSE)
  data <- lapply(lines,jsonlite::fromJSON,simplifyVector=FALSE)
  ids <- vapply(data,function(x)as.character(x$record_id %||% ""),character(1))
  if(any(!nzchar(ids))||anyDuplicated(ids))
    stop("W07 results contain duplicate or missing IDs",call.=FALSE)
  val <- function(x)if(is.null(x)||!length(x))"" else as.character(x[[1L]])
  rows <- lapply(data,function(x) {
    assigns <- x$coding$assignments %||% list()
    paste_items <- function(f)paste(vapply(assigns,function(a)val(a[[f]]),
                                           character(1)),collapse="; ")
    data.frame(record_id=val(x$record_id),title=val(x$title),
      abstract=val(x$abstract),status=val(x$status),
      topic_codes=paste_items("path_id"),confidence=paste_items("confidence"),
      evidence=paste_items("evidence"),explanations=paste_items("explanation"),
      human_review=isTRUE(x$coding$human_review),
      ambiguity=val(x$coding$ambiguity),
      validation_issues=paste(as.character(unlist(x$validation_issues %||% list())),
                              collapse="; "),error=val(x$error),
      stringsAsFactors=FALSE)
  })
  list(manifest=manifest,table=do.call(rbind,rows))
}
