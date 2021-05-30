.onLoad <- function(libname, pkgname) {

  .fitbitr_token <<- NULL

  invisible()
}

base_url <- "https://api.fitbit.com"

## misc helpers for using pipes in the package
. <- NULL
