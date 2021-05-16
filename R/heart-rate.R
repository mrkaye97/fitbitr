#' @title Heart Rate Time Series
#'
#' @description
#'  \code{get_heart_rate()} returns time series data in the specified range
#'   If you specify earlier dates in the request, the response will retrieve only data since the user's join date or the first log entry date for the requested collection.
#'
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param token Fitbit access token
#' @param user_id Fitbit user id
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

