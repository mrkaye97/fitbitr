#' Heart Rate Intraday
#'
#' Returns heart rate data for the specified day
#'
#' @param date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param minutes a boolean for whether data should be returned in minutes (TRUE) or seconds (FALSE)
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @importFrom dplyr rename
#' @importFrom lubridate as_datetime
#' @importFrom rlang .data
#'
#' @details
#' See \url{https://dev.fitbit.com/reference/web-api/heart-rate/#get-heart-rate-time-series} for more details.
#'
#' @export
heart_rate_intraday <- function(date, minutes = TRUE, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  check_config_exists(token, user_id)

  date_conv <- paste0("/", as.Date(date))
  detail_level <- ifelse(minutes, "/1min", "/1sec")

  url <- paste0(url_heart, "date", date_conv, "/1d", detail_level, ".json")
  url <- gsub("user/-/", paste0("user/", user_id, "/"), url)

  r <- get(
    url = url,
    token = token
  )

  r %>%
    content() %>%
    pluck("activities-heart-intraday") %>%
    pluck("dataset") %>%
    bind_rows() %>%
    mutate(
      time = as_datetime(paste0(date, .data$time))
    ) %>%
    rename(
      heart_rate = .data$value
    )
}

#' Heart Rate Zones
#'
#' See \url{https://dev.fitbit.com/build/reference/web-api/activity/} for more details.
#' @param date The date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @export
heart_rate_zones <- function(date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  check_config_exists(token, user_id)
  date_conv <- paste0("/", as.Date(date))

  url <- paste0(url_activity, "date", date_conv, ".json")
  url <- gsub("user/-/", paste0("user/", user_id, "/"), url)

  r <- get(
    url = url,
    token = token
  )

  r %>%
    content() %>%
    pluck("summary") %>%
    pluck("heartRateZones") %>%
    bind_rows() %>%
    mutate(
      date = date
    ) %>%
    select(
      date,
      zone = .data$name,
      min_hr = .data$min,
      max_hr = .data$max,
      minutes_in_zone = .data$minutes,
      calories_out = .data$caloriesOut
    )
}
