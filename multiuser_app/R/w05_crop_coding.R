# W05 crop dictionary and deterministic coding. No external writes in this module.
w05_crop_validate <- function(x) {
  expected <- c("entity","code","matching_terms")
  if(!is.data.frame(x)||!identical(names(x),expected)||!nrow(x))
    stop("W05 CSV requires exactly: entity,code,matching_terms",call.=FALSE)
  x[] <- lapply(x,as.character)
  x[is.na(x)] <- ""
  x[] <- lapply(x,trimws)
  if(any(!nzchar(x$entity))||any(!nzchar(x$code))||
     anyDuplicated(paste(x$entity,x$code,sep="::")))
    stop("W05 entity/code pairs must be present and unique",call.=FALSE)
  if(!any(x$entity=="crop" & x$code=="unspecified_horticultural_crop"))
    stop("W05 requires unspecified_horticultural_crop",call.=FALSE)
  if(any(nzchar(x$matching_terms[x$entity=="crop" & x$code=="unspecified_horticultural_crop"])))
    stop("Unspecified crop is automatic and must have no matching terms",call.=FALSE)
  if(any(!nzchar(x$matching_terms[!(x$entity=="crop" & x$code=="unspecified_horticultural_crop")])))
    stop("All identified crop codes require matching terms",call.=FALSE)
  term_sets <- lapply(x$matching_terms,function(t)
    unique(tolower(trimws(strsplit(t,";",fixed=TRUE)[[1L]]))))
  term_sets <- lapply(term_sets,function(t)t[nzchar(t)])
  owner <- unlist(Map(function(entity,code,terms)rep(paste(entity,code,sep="::"),length(terms)),x$entity,x$code,term_sets))
  tokens <- unlist(term_sets,use.names=FALSE)
  if(anyDuplicated(paste(owner,tokens,sep=":")))
    stop("Duplicate terms detected",call.=FALSE)
  scoped_terms <- paste(rep(x$entity,lengths(term_sets)),tokens,sep="::")
  conflicts <- unique(scoped_terms[duplicated(scoped_terms)])
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
w05_crop_compile <- function(dictionary) {
  dict <- w05_crop_validate(dictionary)
  indices <- which(!(dict$entity=="crop" & dict$code=="unspecified_horticultural_crop"))
  rules <- lapply(indices,function(i) {
    terms <- trimws(strsplit(dict$matching_terms[[i]],";",fixed=TRUE)[[1L]])
    terms <- unique(w05_crop_normalise(terms[nzchar(terms)]))
    paste0("(?<![\\p{L}\\p{N}])\\Q",terms,"\\E(?![\\p{L}\\p{N}])")
  })
  names(rules) <- paste(dict$entity[indices],dict$code[indices],sep="::")
  rules
}
w05_crop_match_compiled <- function(title,abstract,rules) {
  txt <- w05_crop_normalise(paste(title,abstract))
  found <- names(rules)[vapply(rules,function(patterns)
    any(vapply(patterns,function(p)isTRUE(grepl(p,txt,perl=TRUE)),logical(1))),
    logical(1))]
  # Crop has a required automatic fallback; other entities only emit actual matches.
  if(!any(startsWith(found,"crop::")))
    found <- c(found,"crop::unspecified_horticultural_crop")
  found
}
w05_crop_match <- function(title,abstract,dictionary) {
  w05_crop_match_compiled(title,abstract,w05_crop_compile(dictionary))
}
w05_crop_code_records <- function(records,dictionary) {
  stopifnot(is.data.frame(records),all(c("record_id","title","abstract")%in%names(records)))
  if(anyDuplicated(records$record_id)||any(!nzchar(records$record_id)))
    stop("W05 input record identifiers must be unique",call.=FALSE)
  dict <- w05_crop_validate(dictionary)
  rules <- w05_crop_compile(dict)
  matched <- lapply(seq_len(nrow(records)),function(i)
    w05_crop_match_compiled(records$title[[i]],records$abstract[[i]],rules))
  keys <- unlist(matched,use.names=FALSE)
  parts <- strsplit(keys,"::",fixed=TRUE)
  assignments <- data.frame(record_id=rep(as.character(records$record_id),lengths(matched)),
    entity=vapply(parts,`[[`,character(1),1L),
    code=vapply(parts,`[[`,character(1),2L),stringsAsFactors=FALSE)
  wanted <- paste(dict$entity,dict$code,sep="::")
  counts <- table(factor(keys,levels=wanted))
  frequencies <- data.frame(entity=dict$entity,code=dict$code,
    records=as.integer(counts),stringsAsFactors=FALSE)
  list(assignments=assignments,frequencies=frequencies,
       records_processed=nrow(records))
}
