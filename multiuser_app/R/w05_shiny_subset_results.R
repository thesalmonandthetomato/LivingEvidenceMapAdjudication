# Read only isolated W05 Shiny Action outputs. Never dispatch anything here.
w05_shiny_subset_run_status <- function(run_id,api=w05_shiny_gh_request) {
  id <- as.character(run_id)
  if(length(id)!=1L || !grepl("^[0-9]+$",id))
    stop("Invalid W05 Shiny run identifier",call.=FALSE)
  base <- paste0("https://api.github.com/repos/",w05_shiny_repo(),"/actions/runs/",id)
  x <- api(base)
  if(!identical(as.character(x$path %||% "" ),".github/workflows/w05_shiny_subset.yml"))
    stop("Workflow run is not the isolated W05 Shiny subset pathway",call.=FALSE)
  list(id=id,status=as.character(x$status %||% "unknown"),
       conclusion=as.character(x$conclusion %||% ""),
       url=as.character(x$html_url %||% ""),
       completed=identical(as.character(x$status %||% ""),"completed"))
}
w05_shiny_subset_results <- function(run_id,api=w05_shiny_gh_request,
                                     download=NULL) {
  status <- w05_shiny_subset_run_status(run_id,api)
  if(!isTRUE(status$completed) || !identical(status$conclusion,"success"))
    stop("W05 Shiny subset run is not successfully complete",call.=FALSE)
  endpoint <- paste0("https://api.github.com/repos/",w05_shiny_repo(),
                     "/actions/runs/",status$id,"/artifacts")
  response <- api(endpoint)
  arts <- response$artifacts %||% list()
  expected <- paste0("w05-shiny-subset-",status$id)
  selected <- Filter(function(x)identical(as.character(x$name %||% ""),expected)&&
                     !isTRUE(x$expired),arts)
  if(length(selected)!=1L)stop("Expected exactly one unexpired W05 Shiny results artefact",call.=FALSE)
  archive_url <- as.character(selected[[1L]]$archive_download_url %||% "")
  if(!startsWith(archive_url,paste0("https://api.github.com/repos/",
                                   w05_shiny_repo(),"/actions/artifacts/")))
    stop("Unexpected W05 Shiny artefact URL",call.=FALSE)
  if(is.null(download)) {
    download <- function(url,dest) {
      token <- github_dispatch_token()
      req <- httr2::request(url) |>
        httr2::req_headers(Authorization=paste("Bearer",token),
          Accept="application/vnd.github+json",
          `User-Agent`="LivingEvidenceMap-Adjudication")
      httr2::req_perform(httr2::req_timeout(req,90),path=dest)
    }
  }
  zipfile <- tempfile(fileext=".zip")
  unpacked <- tempfile("w05-shiny-results-")
  dir.create(unpacked)
  on.exit(unlink(c(zipfile,unpacked),recursive=TRUE),add=TRUE)
  download(archive_url,zipfile)
  entries <- utils::unzip(zipfile,list=TRUE)$Name
  required <- c("w05_crop_manifest.json","w05_crop_frequencies.csv",
                "w05_crop_assignments.csv")
  if(!setequal(entries,required) ||
     any(grepl("(^/|(^|/)\\.\\.(/|$))",entries)))
    stop("W05 Shiny results archive has unexpected contents",call.=FALSE)
  utils::unzip(zipfile,files=required,exdir=unpacked)
  manifest <- jsonlite::fromJSON(file.path(unpacked,required[[1L]]),simplifyVector=FALSE)
  if(!identical(as.character(manifest$schema),"lem-w05-crop-test-v1")||
     !identical(as.character(manifest$mode),"subset_test"))
    stop("W05 Shiny subset manifest mismatch",call.=FALSE)
  freq <- utils::read.csv(file.path(unpacked,required[[2L]]),
                          stringsAsFactors=FALSE,check.names=FALSE)
  assignments <- utils::read.csv(file.path(unpacked,required[[3L]]),
                                 stringsAsFactors=FALSE,check.names=FALSE)
  if(!identical(names(freq),c("entity","code","records")) ||
     !identical(names(assignments),c("record_id","entity","code")) ||
     anyDuplicated(assignments) || any(is.na(freq$records))||
     any(freq$records<0) ||
     sum(freq$records)!=nrow(assignments) ||
     length(unique(assignments$record_id))!=as.integer(manifest$processed_records))
    stop("W05 Shiny results fail coverage or frequency checks",call.=FALSE)
  actual <- stats::aggregate(rep(1L,nrow(assignments)),
    by=list(entity=assignments$entity,code=assignments$code),FUN=sum)
  reported <- freq[freq$records>0,c("entity","code","records"),drop=FALSE]
  if(nrow(actual)!=nrow(reported)||
     !identical(sort(paste(actual$entity,actual$code,actual$x)),
                sort(paste(reported$entity,reported$code,reported$records))))
    stop("W05 Shiny frequencies do not match assignments",call.=FALSE)
  list(status=status,frequencies=freq,manifest=manifest)
}
