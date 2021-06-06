library(testthat)
library(fitbitr)

if (interactive()) Sys.setenv("FITBITR_CACHE_LOC" = system.file('.httr-oauth', package = 'fitbitr'))

test_check("fitbitr")
