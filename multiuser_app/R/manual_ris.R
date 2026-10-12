# Generic RIS preparation for manually searched bibliographic sources.
# No project-specific database names and no implicit W01 dispatch.
manual_ris_count <- function(path) {
  lines <- readLines(path,warn=FALSE,encoding="UTF-8")
  tags <- grep("^TY  - ",lines)
  endings <- grep("^ER  -",lines)
  if(!length(tags) || length(tags)!=length(endings))
    stop("RIS file has unmatched TY/ER record boundaries",call.=FALSE)
  if(any(endings < tags)) stop("Invalid RIS record boundaries",call.=FALSE)
  as.integer(length(tags))
}
manual_ris_prepare <- function(files, database, platform, query, searched_on, reported) {
  if(is.null(files) || !nrow(files)) stop("Select at least one RIS file",call.=FALSE)
  fields <- c(database,query,searched_on)
  if(anyNA(fields) || any(!nzchar(trimws(fields))))
    stop("Database, search string and search date are required",call.=FALSE)
  if(!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",searched_on))
    stop("Search date must be YYYY-MM-DD",call.=FALSE)
  if(!is.na(reported) && (reported<0 || reported!=as.integer(reported)))
    stop("Reported hits must be a non-negative integer",call.=FALSE)
  ext <- tolower(tools::file_ext(files$name))
  if(any(ext!="ris")) stop("All uploaded files must have a .ris extension",call.=FALSE)
  n <- vapply(files$datapath,manual_ris_count,integer(1))
  list(database=trimws(database),platform=trimws(platform),
    search_string=query,search_date=searched_on,
    reported_hits=if(is.na(reported)) NULL else as.integer(reported),
    imported_records=sum(n),files=lapply(seq_len(nrow(files)),function(i)
      list(name=basename(files$name[[i]]),size=as.numeric(files$size[[i]]),
        records=n[[i]],sha256=digest::digest(file=files$datapath[[i]],algo="sha256"))))
}
