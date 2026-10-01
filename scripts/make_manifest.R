#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(rsconnect)
})

rsconnect::writeManifest(appDir = ".")
cat("PASS: manifest.json written\n")
