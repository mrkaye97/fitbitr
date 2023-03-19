library(testthat)
library(fitbitr)

if (interactive()) {
  Sys.setenv(
    "FITBITR_ENVIRONMENT" = "testing mode"
  )
}

test_check("fitbitr")
