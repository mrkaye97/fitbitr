url_base <- "https://api.fitbit.com/1/"
url_api <- paste0(url_base, "user/-/")

url_activity <- paste0(url_api, "activities/")
url_body <- paste0(url_api, "body/")
url_sleep <- paste0(url_api, "sleep/")
url_heart <- paste0(url_api, 'activities/heart/')

## misc helpers for pipes
. <- NULL
