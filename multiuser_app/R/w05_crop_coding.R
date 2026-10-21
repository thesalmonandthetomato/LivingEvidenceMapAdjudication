# W05 crop dictionary and deterministic coding. No external writes in this module.
w05_crop_validate <- function(x) {
  expected <- c("entity","code","matching_terms")
  if(!is.data.frame(x)||!identical(names(x),expected)||!nrow(x))
    stop("W05 CSV requires exactly: entity,code,matching_terms",call.=FALSE)
  x[] <- lapply(x,as.character)
  x[is.na(x)] <- ""
  x[] <- lapply(x,trimws)
  if(any(x$entity!="crop")||any(!nzchar(x$code))||anyDuplicated(x$code))
    stop("W05 entity must be crop and codes must be unique",call.=FALSE)
  if(!"unspecified_horticultural_crop"%in%x$code)
    stop("W05 requires unspecified_horticultural_crop",call.=FALSE)
  if(nzchar(x$matching_terms[x$code=="unspecified_horticultural_crop"]))
    stop("Unspecified crop is automatic and must have no matching terms",call.=FALSE)
  if(any(!nzchar(x$matching_terms[x$code!="unspecified_horticultural_crop"])))
    stop("All identified crop codes require matching terms",call.=FALSE)
  term_sets <- lapply(x$matching_terms,function(t)
    unique(tolower(trimws(strsplit(t,";",fixed=TRUE)[[1L]]))))
  term_sets <- lapply(term_sets,function(t)t[nzchar(t)])
  owner <- unlist(Map(function(code,terms)rep(code,length(terms)),x$code,term_sets))
  tokens <- unlist(term_sets,use.names=FALSE)
  if(anyDuplicated(paste(owner,tokens,sep=":")))
    stop("Duplicate terms detected",call.=FALSE)
  conflicts <- unique(tokens[duplicated(tokens)])
  if(length(conflicts))
    stop(paste("Matching terms used by more than one code:",paste(head(conflicts,8),collapse=", ")),call.=FALSE)
  x
}
w05_crop_read <- function(path) {
  if(!file.exists(path) || is.na(file.info(path)$size) || file.info(path)$size>2e6)
    stop("W05 CSV is missing or exceeds 2 MB",call.=FALSE)
  x <- utils::read.csv(path,check.names=FALSE,stringsAsFactors=FALSE,
    fileEncoding="UTF-8",na.strings=NULL)
  w05_crop_validate(x)
}
w05_crop_normalise <- function(x) {
  x <- tolower(as.character(x %||% ""))
  y <- iconv(x,from="",to="ASCII//TRANSLIT",sub="")
  y[is.na(y)] <- x[is.na(y)]
  trimws(gsub("[[:space:]]+"," ",y,perl=TRUE))
}
w05_crop_match <- function(title,abstract,dictionary) {
  dict <- w05_crop_validate(dictionary)
  text <- w05_crop_normalise(paste(title,abstract))
  found <- character()
  for(i in which(dict$code!="unspecified_horticultural_crop")) {
    terms <- trimws(strsplit(dict$matching_terms[[i]],";",fixed=TRUE)[[1L]])
    terms <- unique(w05_crop_normalise(terms[nzchar(terms)]))
    hit <- any(vapply(terms,function(t) {
      # Unicode-aware word boundaries after accent folding. Literal escaped terms.
      p <- paste0("(?<![\\p{L}\\p{N}])",gsub("([][{}()+*^$|\\?.])","\\\\\\1",t,perl=TRUE),
                  "(?![\\p{L}\\p{N}])")
      isTRUE(grepl(p,text,perl=TRUE))
    },logical(1)))
    if(hit)found <- c(found,dict$code[[i]])
  }
  if(!length(found))"unspecified_horticultural_crop" else unique(found)
}
w05_crop_code_records <- function(records,dictionary) {
  stopifnot(is.data.frame(records),all(c("record_id","title","abstract")%in%names(records)))
  if(anyDuplicated(records$record_id)||any(!nzchar(records$record_id)))
    stop("W05 input record identifiers must be unique",call.=FALSE)
  matched <- lapply(seq_len(nrow(records)),function(i)
    w05_crop_match(records$title[[i]],records$abstract[[i]],dictionary))
  assignments <- data.frame(record_id=rep(as.character(records$record_id),lengths(matched)),
    code=unlist(matched,use.names=FALSE),stringsAsFactors=FALSE)
  counts <- table(factor(assignments$code,levels=dictionary$code))
  list(assignments=assignments,frequencies=data.frame(
    code=names(counts),records=as.integer(counts),stringsAsFactors=FALSE),
    records_processed=nrow(records))
}
