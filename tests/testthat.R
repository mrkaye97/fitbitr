library(testthat)
library(fitbitr)

if (interactive()) {
  Sys.setenv(
    "FITBITR_CACHE_LOC" = system.file(".httr-oauth", package = "fitbitr"),
    "FITBITR_ENVIRONMENT" = "testing mode"
  )
}

test_check("fitbitr")
