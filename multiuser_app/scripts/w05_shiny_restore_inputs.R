# Isolated Shiny W05 subset input restoration. Reads an existing Zenodo draft;
# never creates/publishes a draft or dispatches another workflow.
suppressPackageStartupMessages({library(httr2);library(jsonlite);library(digest)})
scalar <- function(key) Sys.getenv(key,unset="")
review <- scalar("W05_REVIEW_ID")
id <- scalar("W05_DRAFT_ID")
input <- scalar("W05_INPUT_FILE")
dict <- scalar("W05_DICT_FILE")
ish <- tolower(scalar("W05_INPUT_SHA"))
dsh <- tolower(scalar("W05_DICT_SHA"))
token <- scalar("ZENODO_ACCESS_TOKEN")
if(!grepl("^[A-Za-z0-9][A-Za-z0-9._-]{2,127}$",review)||
   !grepl("^[0-9]+$",id)||
   !all(grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$",c(input,dict)))||
   !all(grepl("^[a-f0-9]{64}$",c(ish,dsh)))||!nzchar(token))
  stop("Invalid W05 Shiny Zenodo input parameters",call.=FALSE)
get <- function(url) {
  req <- request(url) |> req_headers(Authorization=paste("Bearer",token))
  resp <- req_perform(req_timeout(req,180))
  if(resp_status(resp)!=200L)stop("W05 Zenodo GET failed",call.=FALSE)
  resp
}
dep <- resp_body_json(get(paste0("https://zenodo.org/api/deposit/depositions/",id)),simplifyVector=FALSE)
if(isTRUE(dep$submitted))stop("W05 subset inputs must be stored in an unpublished restricted draft",call.=FALSE)
if(!identical(as.character(dep$metadata$title),
              paste("Living Evidence Map W05 crop coding |",review)))
  stop("Zenodo draft title/review identity mismatch",call.=FALSE)
files <- dep$files %||% list()
find_one <- function(name) {
  found <- Filter(function(f)identical(as.character(f$filename),name),files)
  if(length(found)!=1L)stop("W05 Zenodo draft input missing or duplicated",call.=FALSE)
  url <- as.character(found[[1L]]$links$download %||% "")
  if(!startsWith(url,"https://zenodo.org/"))stop("Untrusted Zenodo file link",call.=FALSE)
  url
}
`%||%` <- function(x,y)if(is.null(x)||length(x)==0L)y else x
dest <- file.path(Sys.getenv("RUNNER_TEMP"),"w05-shiny")
dir.create(dest,recursive=TRUE,showWarnings=FALSE)
for(item in list(list(name=input,sha=ish,target="input.jsonl"),
                 list(name=dict,sha=dsh,target="dictionary.csv"))) {
  bytes <- resp_body_raw(get(find_one(item$name)))
  if(!identical(tolower(digest(bytes,algo="sha256",serialize=FALSE)),item$sha))
    stop("W05 Zenodo file SHA256 mismatch",call.=FALSE)
  writeBin(bytes,file.path(dest,item$target))
}
cat("PASS restored verified W05 Shiny subset inputs\n")
