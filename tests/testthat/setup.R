if (interactive()) {
  Sys.setenv(
    FITBITR_CACHE_LOC = paste(
      rprojroot::find_package_root_file(),
      ".httr-oauth",
      sep = "/"
    )
  )

  token <- load_cached_token(
    Sys.getenv("FITBITR_CACHE_LOC")
  )
}

start_date <- date <- lubridate::as_date("2020-05-21")
end_date <- start_date + lubridate::days(7)
