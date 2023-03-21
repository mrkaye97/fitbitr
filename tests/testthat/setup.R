if (interactive()) {
  Sys.setenv(
    FITBITR_CACHE_LOC = paste(
      rprojroot::find_package_root_file(),
      ".httr-oauth",
      sep = "/"
    )
  )

  rlang::inform(Sys.getenv("FITBITR_CACHE_LOC"))
  token <- load_cached_token(
    Sys.getenv("FITBITR_CACHE_LOC")
  )
}
