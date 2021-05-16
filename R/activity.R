#' @title Daily Activity Summary
#'
#' @details
#' See \url{https://dev.fitbit.com/build/reference/web-api/activity/} for more details.
#' @param date The date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @export
get_activity_summary <- function(date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {

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
    list_modify('heartRateZones' = NULL) %>%
    list_modify('distances' = NULL) %>%
    bind_rows() %>%
    mutate(
      date = date
    ) %>%
    select(date, everything())
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

#' @title Distances
#'
#' @details
#' See \url{https://dev.fitbit.com/build/reference/web-api/activity/} for more details.
#' @param date The date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @export
get_distances <- function(date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {

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
    pluck('distances') %>%
    bind_rows() %>%
    mutate(
      date = date
    ) %>%
    select(date, everything())
}

#' @title Activity Time Series
#'
#' @details
#' See \url{https://dev.fitbit.com/build/reference/web-api/activity/} for more details.
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param resource_path The resource path. See \url{https://dev.fitbit.com/build/reference/web-api/activity/} for options
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @importFrom purrr flatten_dfr
#' @export
get_activity_time_series <- function(start_date, end_date, resource_path, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {

  if (grepl('activities/', resource_path)) resource_path <- gsub('activities/', '', resource_path)

  start_date_conv <- paste0('/', as.Date(start_date))
  end_date_conv <- paste0('/', as.Date(end_date))

  url <- paste0(url_activity, resource_path, '/date', start_date_conv, end_date_conv, '.json')
  url <- gsub('user/-/', paste0("user/", user_id, "/"), url)

  # We can not simplify this output because it is so complicated nested list
  r <- get(
    url = url,
    token = token
  )

  r %>%
    content() %>%
    flatten_dfr()
}

