#' @noRd
.onLoad <- function(libname, pkgname) {
  .fitbitr_token <<- NULL

  invisible()
}


## package constants
base_url <- "https://api.fitbit.com"

## misc helpers for using pipes in the package
. <- NULL

refresh_oauth2.0 <- utils::getFromNamespace("refresh_oauth2.0", "httr")
