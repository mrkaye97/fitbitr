start_date <- date <- lubridate::as_date("2020-05-21")
end_date <- start_date + lubridate::days(7)

.fitbitr_token <<- generate_fitbitr_token()
