fitbitr_token <- generate_fitbitr_token(
  refresh_token = Sys.getenv("FITBIT_REFRESH_TOKEN")
)

token <- refresh_api_token(fitbitr_token)

start_date <- date <- lubridate::as_date("2020-05-21")
end_date <- start_date + lubridate::days(7)
