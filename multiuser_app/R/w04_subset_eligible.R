# Restore W04-eligible IDs from *verified* W02 lean and W03 publication
# archives. Caller must resolve the two pointer files from the active review,
# never from the globally latest GitHub run.
w04_subset_scalar <- function(x) {
  if(is.null(x)||!length(x)) return("")
  as.character(x[[1L]])
}
w04_subset_valid_pointer_pair <- function(lean,w03) {
  if(!identical(lean$status,"published") ||
     !identical(lean$state,"post_w02_lean_canonical_checkpoint") ||
     !identical(lean$visibility,"restricted") ||
     !identical(w03$status,"published") ||
     !identical(w03$workflow,"03") ||
     !identical(w03$state,"publication_status") ||
     !identical(w03$visibility,"restricted"))
    stop("W04 source pointers are not published restricted W02/W03 states",call.=FALSE)
  sha <- tolower(w04_subset_scalar(lean$lean_canonical_sha256))
  if(!grepl("^[0-9a-f]{64}$",sha) ||
     !identical(sha,tolower(w04_subset_scalar(w03$upstream_lean_canonical_sha256))))
    stop("W03 status was not computed for the supplied W02 lean checkpoint",call.=FALSE)
  TRUE
}
w04_subset_verified_archive <- function(pointer,kind) {
  stopifnot(kind%in%c("lean","w03"))
  files <- pointer$archive_files
  archives <- Filter(function(z) grepl("[.]tar[.]gz$",
    w04_subset_scalar(z$filename)),files)
  if(length(archives)!=1L) stop("Expected exactly one archived state file",call.=FALSE)
  z <- archives[[1L]]
  expected <- tolower(w04_subset_scalar(z$sha256))
  if(!grepl("^[0-9a-f]{64}$",expected)) stop("Missing archive SHA256",call.=FALSE)
  deposition <- w04_subset_scalar(pointer$zenodo_deposition_id)
  if(!grepl("^[0-9]+$",deposition)) stop("Invalid Zenodo deposition ID",call.=FALSE)
  dep <- w04_prompt_zenodo_request(w04_prompt_deposit_endpoint(deposition))
  if(!isTRUE(dep$submitted)) stop("Upstream state is not published",call.=FALSE)
  bucket <- w04_subset_scalar(dep$links$bucket)
  if(!startsWith(bucket,"https://zenodo.org/api/files/"))
    stop("Unexpected upstream Zenodo bucket",call.=FALSE)
  archive <- tempfile(fileext=".tar.gz")
  on.exit(unlink(archive),add=TRUE)
  req <- httr2::req_headers(
    httr2::request(paste0(sub("/$","",bucket),"/",
      utils::URLencode(w04_subset_scalar(z$filename),reserved=TRUE))),
    Authorization=paste("Bearer",Sys.getenv("ZENODO_ACCESS_TOKEN",unset="")))
  resp <- httr2::req_perform(httr2::req_timeout(req,1800),path=archive)
  if(httr2::resp_status(resp)!=200L ||
     !identical(tolower(digest::digest(file=archive,algo="sha256",
                          serialize=FALSE)),expected) ||
     as.numeric(file.info(archive)$size)!=as.numeric(z$bytes))
    stop("Upstream archive retrieval/checksum failed",call.=FALSE)
  tar_files <- utils::untar(archive,list=TRUE)
  wanted <- if(identical(kind,"lean"))"canonical_lean.jsonl" else "publication_status.jsonl"
  matches <- tar_files[basename(tar_files)==wanted]
  if(length(matches)!=1L ||
     grepl("(^/|(^|/)\\.\\.(/|$))",matches[[1L]]))
    stop("Unexpected upstream archive structure",call.=FALSE)
  out <- tempfile("w04-upstream-")
  dir.create(out)
  on.exit(unlink(out,recursive=TRUE,force=TRUE),add=TRUE)
  utils::untar(archive,files=matches,exdir=out)
  p <- file.path(out,matches[[1L]])
  expected_file <- tolower(if(identical(kind,"lean"))
    w04_subset_scalar(pointer$lean_canonical_sha256)
    else w04_subset_scalar(pointer$publication_status_sha256))
  if(!identical(tolower(digest::digest(file=p,algo="sha256",serialize=FALSE)),
                expected_file))
    stop("Upstream canonical/status checksum mismatch",call.=FALSE)
  readLines(p,warn=FALSE,encoding="UTF-8")
}
w04_subset_eligible_ids_from_lines <- function(canonical_lines,status_lines,
                                                expected_records) {
  parse_line <- function(x)jsonlite::fromJSON(x,simplifyVector=FALSE)
  canonical <- lapply(canonical_lines[nzchar(trimws(canonical_lines))],parse_line)
  statuses <- lapply(status_lines[nzchar(trimws(status_lines))],parse_line)
  ids <- vapply(canonical,function(x)w04_subset_scalar((x$identity %||% list())$record_id),
                character(1))
  sids <- vapply(statuses,function(x)w04_subset_scalar(x$record_id),character(1))
  if(length(ids)!=as.integer(expected_records)||!length(ids)||
     any(!nzchar(ids))||anyDuplicated(ids)||
     length(sids)!=length(ids)||any(!nzchar(sids))||anyDuplicated(sids)||
     !setequal(ids,sids))
    stop("W02 canonical and W03 status record coverage mismatch",call.=FALSE)
  status_map <- stats::setNames(statuses,sids)
  excluded <- vapply(ids,function(id) {
    x <- status_map[[id]]
    nested <- x$publication_status %||% list()
    isTRUE(nested$exclude_from_workflow04 %||%
           x$exclude_from_workflow04 %||% FALSE)
  },logical(1))
  sort(ids[!excluded])
}
w04_subset_load_eligible <- function(lean_pointer,w03_pointer) {
  w04_subset_valid_pointer_pair(lean_pointer,w03_pointer)
  canonical_lines <- w04_subset_verified_archive(lean_pointer,"lean")
  status_lines <- w04_subset_verified_archive(w03_pointer,"w03")
  list(record_ids=w04_subset_eligible_ids_from_lines(canonical_lines,status_lines,
                    lean_pointer$canonical_records),
       checkpoint_sha256=tolower(w04_subset_scalar(lean_pointer$lean_canonical_sha256)),
       w03_status_sha256=tolower(w04_subset_scalar(w03_pointer$publication_status_sha256)))
}
