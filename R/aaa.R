.fitbitr_token <- NULL
.onLoad <- function(libname, pkgname) {

  utils::assignInMyNamespace(
    ".fitbitr_token",
    NULL
  )
}

## package constants
base_url <- "https://api.fitbit.com"

## misc helpers for using pipes in the package
. <- NULL

refresh_oauth2.0 <- utils::getFromNamespace("refresh_oauth2.0", "httr")
