# Review-specific W07 topic configuration, separate from W06 and production pathways.
w07_shiny_config_id <- function(meta) {
  id <- as.character(meta$values[["w07_config_zenodo_draft_id"]] %||% "")
  if(length(id)!=1L || is.na(id) ||
     (nzchar(id) && !grepl("^[0-9]+$",id)))
    stop("Invalid W07 configuration draft ID",call.=FALSE)
  id
}
w07_shiny_config_deposit <- function(meta) {
  id <- w07_shiny_config_id(meta)
  if(!nzchar(id))return(NULL)
  dep <- w04_prompt_zenodo_request(w04_prompt_deposit_endpoint(id))
  if(isTRUE(dep$submitted) ||
     !identical(as.character(dep$metadata$title),
       paste("Living Evidence Map W07 topic configuration |",meta$review_id)))
    stop("W07 topic configuration draft/review mismatch",call.=FALSE)
  dep
}
w07_shiny_config_create <- function(meta) {
  old <- w07_shiny_config_id(w04_prompt_review_metadata(force=TRUE))
  if(nzchar(old))return(old)
  creators <- jsonlite::fromJSON(read_github_text_file("user_input/zenodo_config.json"),
    simplifyVector=FALSE)$creators
  if(!length(creators))stop("Zenodo creators are required",call.=FALSE)
  created <- w04_prompt_zenodo_request(w04_prompt_deposit_endpoint(),
    "POST",body=list(),expected=201L)
  id <- as.character(created$id)
  if(!grepl("^[0-9]+$",id))stop("Zenodo did not identify the W07 draft",call.=FALSE)
  tryCatch({
    w04_prompt_zenodo_request(w04_prompt_deposit_endpoint(id),"PUT",
      body=list(metadata=list(
        title=paste("Living Evidence Map W07 topic configuration |",meta$review_id),
        upload_type="dataset",publication_date=format(Sys.Date(),"%Y-%m-%d"),
        description="Restricted W07 Shiny topic ontology and model prompt versions.",
        creators=creators,access_right="restricted",
        access_conditions="Private review-specific coding configuration.")),expected=200L)
    googlesheets4::sheet_append(meta$sheet_id,
      data=data.frame(key="w07_config_zenodo_draft_id",value=id),
      sheet="review_metadata")
    if(!identical(w07_shiny_config_id(w04_prompt_review_metadata(force=TRUE)),id))
      stop("W07 configuration draft registration did not persist",call.=FALSE)
  },error=function(e)stop(sprintf(
    "W07 Zenodo draft %s may exist; reconcile registration before retry: %s",
    id,conditionMessage(e)),call.=FALSE))
  id
}
w07_shiny_config_parse <- function(path,type) {
  if(!file.exists(path)||is.na(file.info(path)$size)||
     file.info(path)$size<1L||file.info(path)$size>2*1024*1024)
    stop("W07 configuration must be nonempty and smaller than 2 MB",call.=FALSE)
  if(identical(type,"ontology")) {
    ontology <- utils::read.csv(path,stringsAsFactors=FALSE,
      check.names=FALSE,fileEncoding="UTF-8")
    required <- c("path_id","level_1","level_2","level_3","definition")
    if(!all(required %in% names(ontology))||!nrow(ontology)||
       anyDuplicated(ontology$path_id)||
       any(is.na(ontology$path_id)|!nzchar(trimws(ontology$path_id))) ||
       any(is.na(ontology$definition)|!nzchar(trimws(ontology$definition))))
      stop("W07 ontology requires unique path_id and nonempty hierarchy/definitions",call.=FALSE)
    return(list(contents=paste(readLines(path,warn=FALSE,encoding="UTF-8"),
      collapse="\n"),count=nrow(ontology)))
  }
  if(!identical(type,"prompt"))stop("Unknown W07 configuration type",call.=FALSE)
  content <- paste(readLines(path,warn=FALSE,encoding="UTF-8"),collapse="\n")
  if(!nzchar(trimws(content)))stop("W07 prompt is empty",call.=FALSE)
  list(contents=content,count=NA_integer_)
}
w07_shiny_config_archive <- function(meta,path,type) {
  type <- match.arg(type,c("ontology","prompt"))
  parsed <- w07_shiny_config_parse(path,type)
  content <- parsed$contents
  sha <- digest::digest(content,algo="sha256",serialize=FALSE)
  filename <- paste0("w07-",type,"-",sha,".json")
  id <- w07_shiny_config_id(meta)
  if(!nzchar(id))id <- w07_shiny_config_create(meta)
  dep <- w07_shiny_config_deposit(w04_prompt_review_metadata(force=TRUE))
  matches <- Filter(function(x)identical(as.character(x$filename),filename),
    dep$files %||% list())
  if(length(matches)>1L)stop("Duplicate W07 configuration file",call.=FALSE)
  if(!length(matches)) {
    tmp <- tempfile(fileext=".json")
    on.exit(unlink(tmp),add=TRUE)
    jsonlite::write_json(list(schema="lem-w07-shiny-configuration-v1",
      review_id=meta$review_id,type=type,sha256=sha,content=content,
      categories=if(identical(type,"ontology"))parsed$count else NULL),
      tmp,auto_unbox=TRUE,pretty=TRUE,null="null")
    bucket <- as.character(dep$links$bucket %||% "")
    if(!startsWith(bucket,"https://zenodo.org/api/files/"))
      stop("Untrusted Zenodo bucket for W07 configuration",call.=FALSE)
    w04_prompt_zenodo_request(paste0(sub("/$","",bucket),"/",filename),
      "PUT",file=tmp,expected=c(200L,201L))
  }
  fresh <- w07_shiny_config_deposit(w04_prompt_review_metadata(force=TRUE))
  matching <- Filter(function(x)identical(as.character(x$filename),filename),
    fresh$files %||% list())
  if(length(matching)!=1L)stop("W07 configuration archive missing",call.=FALSE)
  url <- as.character(matching[[1L]]$links$download %||% "")
  if(!startsWith(url,"https://zenodo.org/"))
    stop("Untrusted W07 archive verification URL",call.=FALSE)
  token <- Sys.getenv("ZENODO_ACCESS_TOKEN",unset="")
  response <- httr2::req_perform(httr2::req_timeout(
    httr2::req_auth_bearer_token(httr2::request(url),token),120))
  stored <- jsonlite::fromJSON(httr2::resp_body_string(response),simplifyVector=FALSE)
  if(!identical(stored$review_id,meta$review_id) ||
     !identical(stored$type,type) || !identical(stored$sha256,sha) ||
     !identical(stored$content,content))
    stop("W07 archived configuration differs from uploaded content",call.=FALSE)
  list(draft_id=id,type=type,filename=filename,sha256=sha,
    content=content,count=parsed$count)
}
w07_shiny_config_approve <- function(meta,receipt) {
  dep <- w07_shiny_config_deposit(meta)
  if(is.null(dep)||!identical(as.character(dep$id),receipt$draft_id) ||
     !receipt$type %in% c("ontology","prompt") ||
     !grepl("^[0-9a-f]{64}$",receipt$sha256) ||
     !identical(receipt$filename,
       paste0("w07-",receipt$type,"-",receipt$sha256,".json")))
    stop("W07 approval receipt does not match review configuration",call.=FALSE)
  files <- vapply(dep$files %||% list(),
    function(x)as.character(x$filename %||% ""),character(1))
  if(sum(files==receipt$filename)!=1L)
    stop("W07 configuration version not found",call.=FALSE)
  name <- paste0("w07-approved-",receipt$type,"-",receipt$sha256,".json")
  if(sum(files==name)>1L)stop("Duplicate W07 approval marker",call.=FALSE)
  if(!name %in% files) {
    tmp <- tempfile(fileext=".json")
    on.exit(unlink(tmp),add=TRUE)
    jsonlite::write_json(list(schema="lem-w07-shiny-approval-v1",
      review_id=meta$review_id,type=receipt$type,
      sha256=receipt$sha256,filename=receipt$filename),
      tmp,auto_unbox=TRUE,pretty=TRUE)
    bucket <- as.character(dep$links$bucket)
    if(!startsWith(bucket,"https://zenodo.org/api/files/"))
      stop("Untrusted W07 approval bucket",call.=FALSE)
    w04_prompt_zenodo_request(paste0(sub("/$","",bucket),"/",name),
      "PUT",file=tmp,expected=c(200L,201L))
  }
  fresh <- w07_shiny_config_deposit(meta)
  present <- vapply(fresh$files %||% list(),
    function(x)as.character(x$filename %||% ""),character(1))
  if(sum(present==name)!=1L)stop("W07 approval was not persisted",call.=FALSE)
  invisible(TRUE)
}
