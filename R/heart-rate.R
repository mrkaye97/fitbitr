#' @title Heart Rate Time Series
#'
#' @description
#'  \code{get_heart_rate()} returns time series data in the specified range
#'   If you specify earlier dates in the request, the response will retrieve only data since the user's join date or the first log entry date for the requested collection.
#'
#' @param date The end date of the period specified in the format "yyyy-MM-dd" or "today" as a string or Date object.
#' @param period The range for which data will be returned. Options are "1d", "7d", "30d", "1w", "1m".
#' @param base_date The range start date in the format "yyyy-MM-dd" or "today" as a string or Date object.
#' @param end_date The end date of the range in the format "yyyy-MM-dd" or "today" as a string or Date object.
#'
#' @details
#' See \url{https://dev.fitbit.com/reference/web-api/heart-rate/#get-heart-rate-time-series} for more details.
#'
#' @export
get_heart_rate <- function(start_date, end_date = NULL, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {

  check_config_exists(token, user_id)

  start_date_conv <- paste0('/', as.Date(start_date))
  if (!is.null(end_date)) end_date <- paste0('/', as.Date(end_date))

  url <- paste0(url_sleep, 'date', start_date_conv, end_date, '.json')
  url <- gsub('user/-/', paste0("user/", user_id, "/"), url)

  r <- get(
    url = url,
    token = token
  )

  r %>%
    content() %>%
    pluck('sleep') %>%
    map(
      function(x) list_modify(x, "minuteData" = NULL)
    ) %>%
    bind_rows() %>%
    arrange(dateOfSleep)
}

#' @title Get Heart Rate Intraday Time Series
#'
#' @description
#'   \code{get_heart_rate_intraday()} returns the intraday time series.
#'   If your application has the appropriate access, your calls to a time series endpoint for a specific day (by using start and end dates on the same day or a period of 1d),
#'   the response will include extended intraday values with a one-minute detail level for that day.
#'   Access to the Intraday Time Series for personal use (accessing your own data) is available through the "Personal" App Type.
#'
#' @param date The end date of the period specified in the format "yyyy-MM-dd" or "today" as a string or Date object.
#' @param detail_level Number of data points to include. Either "1sec" or "1min".
#' @param start_time The start of the period, in the format "HH:mm".
#' @param end_time The end of the period, in the format "HH:mm"
#'
#' @details
#' See \url{'https://dev.fitbit.com/reference/web-api/heart-rate/#get-heart-rate-intraday-time-series} for more details.
#
#' @export
get_heart_rate_intraday <- function(token, date = "", detail_level = "1min", start_time = NULL, end_time = NULL, simplify = TRUE) {
  date <- format_date(date)
  url <- if (!is.null(start_time) && !is.null(end_time)) {
    date2 <- as.Date(date) + 1
    paste0(url_heart, sprintf("date/%s/%s/%s/time/%s/%s.json", date, date2, detail_level, start_time, end_time))
  } else {
    paste0(url_heart, sprintf("date/%s/1d/%s.json", date, detail_level))
  }

  content <- get(url, token)
  if (simplify) {
    content$`activities-heart-intraday`$dataset
  } else {
    content
  }
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

