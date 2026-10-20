# Run from multiuser_app; no external services or model calls.
`%||%` <- function(x,y) if(is.null(x)||!length(x)) y else x
source("R/w04_subset_registry.R")
source("R/w04_shiny_model_launch.R")
w04_prompt_checksum <- function(x)digest::digest(x,algo="sha256",serialize=FALSE)
sha <- paste(rep("a",64),collapse="")
ids <- c("id-a","id-b")
selected <- w04_subset_reconstruct_selection(ids,include_remaining=TRUE)
meta <- list(review_id="review-123",sheet_id="12345678901234567890")
prompt <- list(review_id=meta$review_id,version_id="w04-12345678901234567890",
               content="Sample valid model prompt")
prompt$sha256 <- w04_prompt_checksum(prompt$content)
source <- list(review_id=meta$review_id,lean=list(lean_canonical_sha256=sha))
bundle <- w04_shiny_model_selection_bundle_build(meta,selected,prompt,source)
stopifnot(identical(bundle$selection_type,"reconstructed"),
          identical(unlist(bundle$record_ids),ids),
          identical(bundle$membership_sha256,selected$membership_sha256),
          identical(bundle$human_decision_count,0L),
          grepl("^w04-subset-[0-9a-f]{20}$",bundle$subset_id))
bad <- selected
bad$record_ids <- rev(ids)
stopifnot(identical(w04_shiny_model_selection_bundle_build(meta,bad,prompt,source)$membership_sha256,
                    selected$membership_sha256))
bad$record_ids <- "id-c"
stopifnot(inherits(tryCatch(w04_shiny_model_selection_bundle_build(meta,bad,prompt,source),error=identity),"error"))
cat("PASS reconstructed Shiny launch bundle contract\n")
