if (interactive()) {
  Sys.setenv(
    FITBITR_CACHE_LOC = paste(
      rprojroot::find_package_root_file(),
      ".httr-oauth",
      sep = "/"
    ),
    FITBITR_ENVIRONMENT = "testing mode"
  )

  rlang::inform(Sys.getenv("FITBITR_CACHE_LOC"))
  .fitbitr_token <- load_cached_token(
    Sys.getenv("FITBITR_CACHE_LOC")
  )
}
