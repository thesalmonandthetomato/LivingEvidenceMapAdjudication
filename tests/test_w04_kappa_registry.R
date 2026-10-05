source("R/w04_kappa_registry.R")

baseline <- data.frame(
  registry_row_id="baseline",date="2026-09-26",source="historical_baseline",
  batch_id="historical",queue_sha256="",workflow04_run_id="1",
  records_reviewed="22198",records_compared="21950",
  model_retain_human_retain="15301",model_retain_human_exclude="344",
  model_exclude_human_retain="360",model_exclude_human_exclude="5945",
  human_uncertain="0",model_uncertain="248",
  raw_agreement="0.9679271070615034",
  human_model_kappa="0.9216127141125978",
  humans_model_fleiss_kappa="",created_at_utc="2026-09-26T07:49:35Z",
  source_github_run_id="1",stringsAsFactors=FALSE
)

s <- w04_kappa_registry_summary(baseline)
stopifnot(s$manually_screened==22198L,s$kappa_n==21950L)
stopifnot(abs(s$kappa-0.9216127141125978)<1e-12)

empty <- w04_empty_kappa_registry()
stopifnot(identical(w04_kappa_registry_relation(baseline,empty),"google_empty"))
stopifnot(identical(w04_kappa_registry_relation(baseline,baseline),"identical"))

new <- baseline
new2 <- baseline
new2$registry_row_id <- "batch2"
new2$batch_id <- "batch2"
new2$date <- "2026-10-05"
new2$records_reviewed <- "400"
new2$records_compared <- "400"
new2$model_retain_human_retain <- "180"
new2$model_retain_human_exclude <- "20"
new2$model_exclude_human_retain <- "10"
new2$model_exclude_human_exclude <- "190"
new2$human_uncertain <- "0"
new2$model_uncertain <- "0"
new <- rbind(baseline,new2)
stopifnot(identical(w04_kappa_registry_relation(new,baseline),"google_subset"))

bad <- baseline
bad$human_model_kappa <- "0.5"
err <- tryCatch({w04_kappa_registry_relation(baseline,bad);NULL},error=function(e)e)
stopifnot(inherits(err,"error"))
extra <- rbind(baseline,new2)
err2 <- tryCatch({w04_kappa_registry_relation(baseline,extra);NULL},error=function(e)e)
stopifnot(inherits(err2,"error"))

s2 <- w04_kappa_registry_summary(new)
stopifnot(s2$manually_screened==22598L,s2$kappa_n==22350L,s2$batches==2L)
cat("PASS: W04 kappa registry summary and mirror-integrity rules\n")
