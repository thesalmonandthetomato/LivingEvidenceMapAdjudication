# W04 prompt provenance primitives. No production workflow is launched here.
w04_prompt_checksum <- function(text) {
  stopifnot(is.character(text),length(text)==1L,!is.na(text))
  digest::digest(enc2utf8(text),algo="sha256",serialize=FALSE)
}
w04_prompt_version <- function(review_id,text,author,filename="",notes="",status="draft",
                               parent_version="",created_at=Sys.time()) {
  if(!grepl("^[A-Za-z0-9][A-Za-z0-9._-]{2,127}$",review_id))
    stop("Invalid review ID",call.=FALSE)
  if(!is.character(text)||length(text)!=1L||is.na(text)||!nzchar(trimws(text)))
    stop("Prompt must contain text",call.=FALSE)
  if(!status %in% c("draft","approved","superseded"))
    stop("Invalid prompt status",call.=FALSE)
  sha <- w04_prompt_checksum(text)
  at <- format(created_at,tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
  nonce <- digest::digest(paste(review_id,sha,at,author,parent_version,sep=":"),algo="sha256",
                          serialize=FALSE)
  list(schema="lem-w04-prompt-v1",review_id=review_id,
       version_id=paste0("w04-",substr(nonce,1L,20L)),
       parent_version_id=parent_version,created_at_utc=at,
       author=as.character(author),original_filename=basename(as.character(filename)),
       notes=as.character(notes),status=status,sha256=sha,
       model_runs=list(),subset_ids=list(),content=text)
}
w04_prompt_from_upload <- function(path,filename) {
  ext <- tolower(tools::file_ext(filename))
  if(!ext %in% c("txt","md","docx","pdf"))
    stop("Supported prompt formats: TXT, Markdown, DOCX, PDF",call.=FALSE)
  if(!file.exists(path)||file.info(path)$size>10*1024*1024)
    stop("Prompt file is missing or exceeds 10 MB",call.=FALSE)
  if(ext %in% c("txt","md"))
    return(paste(readLines(path,warn=FALSE,encoding="UTF-8"),collapse="\n"))
  if(ext=="docx") {
    if(!requireNamespace("xml2",quietly=TRUE)) stop("DOCX upload requires xml2",call.=FALSE)
    xml <- xml2::read_xml(unz(path,"word/document.xml"))
    ns <- c(w="http://schemas.openxmlformats.org/wordprocessingml/2006/main")
    paras <- xml2::xml_find_all(xml,".//w:p",ns)
    return(paste(vapply(paras,function(p) paste(xml2::xml_text(xml2::xml_find_all(p,".//w:t",ns)),
      collapse=""),character(1)),collapse="\n"))
  }
  if(!requireNamespace("pdftools",quietly=TRUE)) stop("PDF upload requires pdftools",call.=FALSE)
  paste(pdftools::pdf_text(path),collapse="\n\n")
}
w04_prompt_manifest <- function(versions) {
  lapply(versions,function(v) {
    z <- v
    z$content <- NULL
    if(!identical(w04_prompt_checksum(v$content),v$sha256))
      stop("Prompt checksum mismatch",call.=FALSE)
    z
  })
}
