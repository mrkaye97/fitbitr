#' Activity Summary
#'
#' See \url{https://dev.fitbit.com/build/reference/web-api/activity/} for more details.
#' @param date The date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @importFrom dplyr bind_rows mutate select everything
#' @importFrom httr content
#' @export
activity_summary <- function(date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  check_config_exists(token, user_id)

  url <- sprintf(
    "%s/user/%s/activities/date/%s.json",
    base_url,
    user_id,
    date
  )

  r <- get(
    url = url,
    token = token
  )

  r %>%
    content() %>%
    pluck("summary") %>%
    list_modify("heartRateZones" = NULL) %>%
    list_modify("distances" = NULL) %>%
    bind_rows() %>%
    mutate(
      date = date
    ) %>%
    select(date, everything())
}

#' Activity Time Series
#'
#' See \url{https://dev.fitbit.com/build/reference/web-api/activity/} for more details.
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param resource_path The resource path. See \url{https://dev.fitbit.com/build/reference/web-api/activity/} for options
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @importFrom purrr flatten_dfr
#' @importFrom rlang :=
#' @noRd
activity_time_series <- function(start_date, end_date, resource_path, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  check_config_exists(token, user_id)

  url <- sprintf(
    "%s/user/%s/%s/date/%s.json",
    base_url,
    user_id,
    resource_path,
    date
  )

  r <- get(
    url = url,
    token = token
  )

  r %>%
    content() %>%
    flatten_dfr() %>%
    rename(
      date = .data$dateTime,
      !!resource_path := .data$value
    )
}

#' Calories Time Series
#'
#' Resource path /activities/calories
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @export
calories <- function(start_date, end_date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "calories",
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
calories_bmr <- function(start_date, end_date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "caloriesBMR",
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
steps <- function(start_date, end_date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "steps",
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
distance <- function(start_date, end_date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "distance",
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
floors <- function(start_date, end_date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "floors",
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
elevation <- function(start_date, end_date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "elevation",
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
minutes_sedentary <- function(start_date, end_date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "minutesSedentary",
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
minutes_lightly_active <- function(start_date, end_date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "minutesLightlyActive",
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
minutes_fairly_active <- function(start_date, end_date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "minutesFairlyActive",
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
minutes_very_active <- function(start_date, end_date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "minutesVeryActive",
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
activity_calories <- function(start_date, end_date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "activityCalories",
    token = token,
    user_id = user_id
  )
}

#' @noRd
get_bests_and_totals <- function(best, tracker, token, user_id) {

  check_config_exists(token, user_id)

  url <- sprintf(
    "%s/user/%s/activities.json",
    base_url,
    user_id
  )

  r <- get(
    url = url,
    token = token
  )

  r %>%
    content() %>%
    pluck(ifelse(best, 'best', 'lifetime')) %>%
    pluck(ifelse(tracker, 'tracker', 'total'))
}
#' Tracker Totals
#'
#' Retrieve tracker total distance, floors, steps calories, and active score
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @export
tracker_totals <- function(token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  get_bests_and_totals(
    best = FALSE,
    tracker = TRUE,
    token = token,
    user_id = user_id
  ) %>%
    bind_rows() %>%
    select(
      .data$distance,
      .data$floors,
      .data$steps,
      active_score = .data$activeScore,
      calories_out = .data$caloriesOut
    )
}

#' Lifetime Totals
#'
#' Retrieve lifetime total distance, floors, steps calories, and active score
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @export
lifetime_totals <- function(token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  get_bests_and_totals(
    best = FALSE,
    tracker = FALSE,
    token = token,
    user_id = user_id
  ) %>%
    bind_rows() %>%
    select(
      .data$distance,
      .data$floors,
      .data$steps,
      active_score = .data$activeScore,
      calories_out = .data$caloriesOut
    )

}

#' Tracker Bests
#'
#' Retrieve tracker best distance, floors, and steps
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @importFrom tidyr unnest_wider
#' @importFrom tibble enframe
#' @export
tracker_bests <- function(token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  get_bests_and_totals(
    best = TRUE,
    tracker = TRUE,
    token = token,
    user_id = user_id
  ) %>%
    enframe() %>%
    unnest_wider(.data$value) %>%
    rename(
      metric = .data$name
    )
}

#' Lifetime Bests
#'
#' Retrieve lifetime best distance, floors, and steps
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @export
lifetime_bests <- function(token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  get_bests_and_totals(
    best = TRUE,
    tracker = FALSE,
    token = token,
    user_id = user_id
  ) %>%
    enframe() %>%
    unnest_wider(.data$value) %>%
    rename(
      metric = .data$name
    )
}
