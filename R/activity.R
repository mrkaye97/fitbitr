#' @title Daily Activity Summary
#'
#' @details
#' See \url{https://dev.fitbit.com/build/reference/web-api/activity/} for more details.
#' @param date The date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @importFrom dplyr bind_rows mutate select everything
#' @importFrom httr content
#' @export
get_activity_summary <- function(date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {

  date_conv <- paste0('/', as.Date(date))

  url <- paste0(url_activity, 'date', date_conv, '.json')
  url <- gsub('user/-/', paste0("user/", user_id, "/"), url)

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
#' @noRd
get_activity_time_series <- function(start_date, end_date, resource_path, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {

  start_date_conv <- paste0('/', as.Date(start_date))
  end_date_conv <- paste0('/', as.Date(end_date))

  url <- paste0(url_activity, resource_path, '/date', start_date_conv, end_date_conv, '.json')
  url <- gsub('user/-/', paste0("user/", user_id, "/"), url)

  r <- get(
    url = url,
    token = token
  )

  r %>%
    content() %>%
    flatten_dfr()
}

#' Calories Time Series
#'
#' Resource path /activities/calories
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @export
get_calories <- function(start_date, end_date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  get_activity_time_series(
    start_date,
    end_date,
    resource_path = 'calories',
    token = token,
    user_id = user_id
  )
}

#' Calories BMR Time Series
#'
#' Resource path /activities/caloriesBMR
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @export
get_calories_bmr <- function(start_date, end_date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  get_activity_time_series(
    start_date,
    end_date,
    resource_path = 'caloriesBMR',
    token = token,
    user_id = user_id
  )
}

#' Steps Time Series
#'
#' Resource path /activities/steps
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @export
get_steps <- function(start_date, end_date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  get_activity_time_series(
    start_date,
    end_date,
    resource_path = 'steps',
    token = token,
    user_id = user_id
  )
}

#' Distance Time Series
#'
#' Resource path /activities/distance
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @export
get_distance <- function(start_date, end_date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  get_activity_time_series(
    start_date,
    end_date,
    resource_path = 'distance',
    token = token,
    user_id = user_id
  )
}

#' Floors Time Series
#'
#' Resource path /activities/floors
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @export
get_floors <- function(start_date, end_date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  get_activity_time_series(
    start_date,
    end_date,
    resource_path = 'floors',
    token = token,
    user_id = user_id
  )
}

#' Elevation Time Series
#'
#' Resource path /activities/elevation
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @export
get_elevation <- function(start_date, end_date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  get_activity_time_series(
    start_date,
    end_date,
    resource_path = 'elevation',
    token = token,
    user_id = user_id
  )
}

#' Minutes Sedentary Time Series
#'
#' Resource path /activities/minutesSedentary
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @export
get_minutes_sedentary <- function(start_date, end_date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  get_activity_time_series(
    start_date,
    end_date,
    resource_path = 'minutesSedentary',
    token = token,
    user_id = user_id
  )
}

#' Minutes Lightly Active Time Series
#'
#' Resource path /activities/minutesLightlyActive
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @export
get_minutes_lightly_active <- function(start_date, end_date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  get_activity_time_series(
    start_date,
    end_date,
    resource_path = 'minutesLightlyActive',
    token = token,
    user_id = user_id
  )
}

#' Minutes Fairly Active Time Series
#'
#' Resource path /activities/minutesFairlyActive
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @export
get_minutes_fairly_active <- function(start_date, end_date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  get_activity_time_series(
    start_date,
    end_date,
    resource_path = 'minutesFairlyActive',
    token = token,
    user_id = user_id
  )
}

#' Minutes Very Active Time Series
#'
#' Resource path /activities/minutesVeryActive
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @export
get_minutes_very_active <- function(start_date, end_date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  get_activity_time_series(
    start_date,
    end_date,
    resource_path = 'minutesVeryActive',
    token = token,
    user_id = user_id
  )
}

#' Activity Calories Time Series
#'
#' Resource path /activities/activityCalories
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @export
get_activity_calories <- function(start_date, end_date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  get_activity_time_series(
    start_date,
    end_date,
    resource_path = 'activityCalories',
    token = token,
    user_id = user_id
  )
}
