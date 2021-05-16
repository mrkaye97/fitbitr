#' @title Heart Rate Time Series
#'
#' @description
#'  \code{get_heart_rate()} returns time series data in the specified range
#'   If you specify earlier dates in the request, the response will retrieve only data since the user's join date or the first log entry date for the requested collection.
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
get_heart_rate_intraday <- function(date, minutes = TRUE, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {

  check_config_exists(token, user_id)

  date_conv <- paste0('/', as.Date(date))
  detail_level <- ifelse(minutes, '/1min', '/1sec')

  url <- paste0(url_heart, 'date', date_conv, '/1d', detail_level, '.json')
  url <- gsub('user/-/', paste0("user/", user_id, "/"), url)

  r <- get(
    url = url,
    token = token
  )

  r %>%
    content() %>%
    pluck('activities-heart-intraday') %>%
    pluck('dataset') %>%
    bind_rows() %>%
    mutate(
      time = as_datetime(paste0(date, .data$time))
    ) %>%
    rename(
      heart_rate = .data$value
    )
}

#' @title Heart Rate Zones
#'
#' @details
#' See \url{https://dev.fitbit.com/build/reference/web-api/activity/} for more details.
#' @param date The date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @export
get_heart_rate_zones <- function(date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {

  date_conv <- paste0('/', as.Date(date))

  url <- paste0(url_activity, 'date', date_conv, '.json')
  url <- gsub('user/-/', paste0("user/", user_id, "/"), url)

  # We can not simplify this output because it is so complicated nested list
  r <- get(
    url = url,
    token = token
  )

  r %>%
    content() %>%
    pluck('summary') %>%
    pluck('heartRateZones') %>%
    bind_rows() %>%
    mutate(
      date = date
    ) %>%
    select(date, everything())
}

