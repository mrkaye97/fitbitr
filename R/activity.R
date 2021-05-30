#' Activity Summary
#'
#' See \url{https://dev.fitbit.com/build/reference/web-api/activity/} for more details.
#' @param date The date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @importFrom dplyr bind_rows mutate select everything
#' @importFrom httr content
#' @importFrom janitor clean_names
#' @return A tibble with the `date` and a number of activity summary metrics for the day.
#' @export
activity_summary <- function(date) {
  check_token_exists()

  url <- sprintf(
    "%s/1/user/%s/activities/date/%s.json",
    base_url,
    .fitbitr_token$credentials$user_id,
    date
  )

  r <- get(
    url = url
  )

  r %>%
    content() %>%
    pluck("summary") %>%
    list_modify("heartRateZones" = NULL) %>%
    list_modify("distances" = NULL) %>%
    bind_rows() %>%
    mutate(
      date = as.Date(date)
    ) %>%
    select(date, everything()) %>%
    clean_names()
}

#' Activity Time Series
#'
#' See \url{https://dev.fitbit.com/build/reference/web-api/activity/} for more details.
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param resource_path The resource path. See \url{https://dev.fitbit.com/build/reference/web-api/activity/} for options
#' @importFrom purrr flatten_dfr
#' @importFrom rlang :=
#' @noRd
activity_time_series <- function(start_date, end_date, resource_path) {
  check_token_exists()

  url <- sprintf(
    "%s/1/user/%s/activities/%s/date/%s/%s.json",
    base_url,
    .fitbitr_token$credentials$user_id,
    resource_path,
    start_date,
    end_date
  )

  r <- get(
    url = url
  )

  r %>%
    content() %>%
    flatten_dfr() %>%
    rename(
      date = .data$dateTime,
      !!resource_path := .data$value
    ) %>%
    mutate(
      date = as.Date(.data$date)
    ) %>%
    clean_names()
}

#' Calories Time Series
#'
#' Resource path /activities/calories
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @return A tibble with two columns: `date` and `calories`
#' @export
calories <- function(start_date, end_date) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "calories"
  )
}

#' Calories BMR Time Series
#'
#' Resource path /activities/caloriesBMR
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @return A tibble with two columns: `date` and `calories_bmr`
#' @export
calories_bmr <- function(start_date, end_date) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "caloriesBMR"
  )
}

#' Steps Time Series
#'
#' Resource path /activities/steps
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @return A tibble with two columns: `date` and `steps`
#' @export
steps <- function(start_date, end_date) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "steps"
  )
}

#' Distance Time Series
#'
#' Resource path /activities/distance
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @return A tibble with two columns: `date` and `distance`
#' @export
distance <- function(start_date, end_date) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "distance"
  )
}

#' Floors Time Series
#'
#' Resource path /activities/floors
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @return A tibble with two columns: `date` and `floors`
#' @export
floors <- function(start_date, end_date) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "floors"
  )
}

#' Elevation Time Series
#'
#' Resource path /activities/elevation
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @return A tibble with two columns: `date` and `elevation`
#' @export
elevation <- function(start_date, end_date) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "elevation"
  )
}

#' Minutes Sedentary Time Series
#'
#' Resource path /activities/minutesSedentary
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @return A tibble with two columns: `date` and `minutes_sedentary`
#' @export
minutes_sedentary <- function(start_date, end_date) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "minutesSedentary"
  )
}

#' Minutes Lightly Active Time Series
#'
#' Resource path /activities/minutesLightlyActive
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @return A tibble with two columns: `date` and `minutes_lightly_active`
#' @export
minutes_lightly_active <- function(start_date, end_date) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "minutesLightlyActive"
  )
}

#' Minutes Fairly Active Time Series
#'
#' Resource path /activities/minutesFairlyActive
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @return A tibble with two columns: `date` and `minutes_fairly_active`
#' @export
minutes_fairly_active <- function(start_date, end_date) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "minutesFairlyActive"
  )
}

#' Minutes Very Active Time Series
#'
#' Resource path /activities/minutesVeryActive
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @return A tibble with two columns: `date` and `minutes_very_active`
#' @export
minutes_very_active <- function(start_date, end_date) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "minutesVeryActive"
  )
}

#' Activity Calories Time Series
#'
#' Resource path /activities/activityCalories
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @return A tibble with two columns: `date` and `activity_calories`
#' @export
activity_calories <- function(start_date, end_date) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "activityCalories"
  )
}

#' @noRd
get_bests_and_totals <- function(best, tracker) {
  check_token_exists()

  url <- sprintf(
    "%s/1/user/%s/activities.json",
    base_url,
    .fitbitr_token$credentials$user_id
  )

  r <- get(
    url = url
  )

  r %>%
    content() %>%
    pluck(ifelse(best, "best", "lifetime")) %>%
    pluck(ifelse(tracker, "tracker", "total"))
}
#' Tracker Totals
#'
#' Retrieve tracker total distance, floors, steps calories, and active score
#' @export
#' @return A tibble of all-time tracker totals (i.e. the total `distance`, `floors`, `steps`, `active_score`, and `calories_out` tracked by your tracker)
tracker_totals <- function() {
  get_bests_and_totals(
    best = FALSE,
    tracker = TRUE
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
#' @return A tibble of all-time totals across trackers (i.e. the total `distance`, `floors`, `steps`, `active_score`, and `calories_out` tracked across all of your trackers)
#' @export
lifetime_totals <- function() {
  get_bests_and_totals(
    best = FALSE,
    tracker = FALSE
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
#' @importFrom tidyr unnest_wider
#' @importFrom tibble enframe
#' @return A tibble the best `distance`, `floors`, and `steps` (by date) tracked on your tracker
#' @export
tracker_bests <- function() {
  get_bests_and_totals(
    best = TRUE,
    tracker = TRUE
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
#' @return A tibble the best `distance`, `floors`, and `steps` (by date) tracked on any of your trackers
#' @export
lifetime_bests <- function() {
  get_bests_and_totals(
    best = TRUE,
    tracker = FALSE
  ) %>%
    enframe() %>%
    unnest_wider(.data$value) %>%
    rename(
      metric = .data$name
    )
}
