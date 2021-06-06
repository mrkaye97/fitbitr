#' Activity Summary
#'
#' See \url{https://dev.fitbit.com/build/reference/web-api/activity/} for more details.
#' @param date The date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @importFrom dplyr bind_rows mutate select everything
#' @importFrom httr content
#' @importFrom janitor clean_names
#' @examples
#' \dontrun{
#' date <- lubridate::today()
#' activity_summary(date)
#' }
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
    url = url,
    .example_identifier = "activity summary"
  )

  r %>%
    content(as = "parsed", type = "application/json") %>%
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
activity_time_series <- function(start_date, end_date, resource_path, .example_identifier) {
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
    url = url,
    .example_identifier = .example_identifier
  )

  r %>%
    content(as = "parsed", type = "application/json") %>%
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
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#' calories(date)
#' }
#' @return A tibble with two columns: `date` and `calories`
#' @export
calories <- function(start_date, end_date) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "calories",
    .example_identifier = "activity time series"
  )
}

#' Calories BMR Time Series
#'
#' Resource path /activities/caloriesBMR
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#' calories_bmr(date)
#' }
#' @return A tibble with two columns: `date` and `calories_bmr`
#' @export
calories_bmr <- function(start_date, end_date) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "caloriesBMR",
    .example_identifier = "activity time series"
  )
}

#' Steps Time Series
#'
#' Resource path /activities/steps
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#' steps(date)
#' }
#' @return A tibble with two columns: `date` and `steps`
#' @export
steps <- function(start_date, end_date) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "steps",
    .example_identifier = "activity time series"
  )
}

#' Distance Time Series
#'
#' Resource path /activities/distance
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#' distance(date)
#' }
#' @return A tibble with two columns: `date` and `distance`
#' @export
distance <- function(start_date, end_date) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "distance",
    .example_identifier = "activity time series"
  )
}

#' Floors Time Series
#'
#' Resource path /activities/floors
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#' floors(date)
#' }
#' @return A tibble with two columns: `date` and `floors`
#' @export
floors <- function(start_date, end_date) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "floors",
    .example_identifier = "activity time series"
  )
}

#' Elevation Time Series
#'
#' Resource path /activities/elevation
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#' elevation(date)
#' }
#' @return A tibble with two columns: `date` and `elevation`
#' @export
elevation <- function(start_date, end_date) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "elevation",
    .example_identifier = "activity time series"
  )
}

#' Minutes Sedentary Time Series
#'
#' Resource path /activities/minutesSedentary
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#' minutes_sedentary(date)
#' }
#' @return A tibble with two columns: `date` and `minutes_sedentary`
#' @export
minutes_sedentary <- function(start_date, end_date) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "minutesSedentary",
    .example_identifier = "activity time series"
  )
}

#' Minutes Lightly Active Time Series
#'
#' Resource path /activities/minutesLightlyActive
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#' minutes_lightly_active(date)
#' }
#' @return A tibble with two columns: `date` and `minutes_lightly_active`
#' @export
minutes_lightly_active <- function(start_date, end_date) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "minutesLightlyActive",
    .example_identifier = "activity time series"
  )
}

#' Minutes Fairly Active Time Series
#'
#' Resource path /activities/minutesFairlyActive
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#' minutes_fairly_active(date)
#' }
#' @return A tibble with two columns: `date` and `minutes_fairly_active`
#' @export
minutes_fairly_active <- function(start_date, end_date) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "minutesFairlyActive",
    .example_identifier = "activity time series"
  )
}

#' Minutes Very Active Time Series
#'
#' Resource path /activities/minutesVeryActive
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#' minutes_very_active(date)
#' }
#' @return A tibble with two columns: `date` and `minutes_very_active`
#' @export
minutes_very_active <- function(start_date, end_date) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "minutesVeryActive",
    .example_identifier = "activity time series"
  )
}

#' Activity Calories Time Series
#'
#' Resource path /activities/activityCalories
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#' activity_calories(date)
#' }
#' @return A tibble with two columns: `date` and `activity_calories`
#' @export
activity_calories <- function(start_date, end_date) {
  activity_time_series(
    start_date,
    end_date,
    resource_path = "activityCalories",
    .example_identifier = "activity time series"
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
    url = url,
    .example_identifier = "bests and totals"
  )

  r %>%
    content(as = "parsed", type = "application/json") %>%
    pluck(
      ifelse(best, "best", "lifetime"), ## pluck best or lifetime
      ifelse(tracker, "tracker", "total") ## pluck tracker or total
    )
}
#' Tracker Totals
#'
#' Retrieve tracker total distance, floors, and steps
#' @examples
#' \dontrun{
#' tracker_totals()
#' }
#' @export
#' @return A tibble of all-time tracker totals (i.e. the total `distance`, `floors`, and `steps` tracked by your tracker)
tracker_totals <- function() {
  get_bests_and_totals(
    best = FALSE,
    tracker = TRUE
  ) %>%
    bind_rows() %>%
    select(
      .data$distance,
      .data$floors,
      .data$steps
    )
}

#' Lifetime Totals
#'
#' Retrieve lifetime total distance, floors, and steps
#' @examples
#' \dontrun{
#' lifetime_totals()
#' }
#' @return A tibble of all-time totals across trackers (i.e. the total `distance`, `floors`, and `steps` tracked across all of your trackers)
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
      .data$steps
    )
}

#' Tracker Bests
#'
#' Retrieve tracker best distance, floors, and steps
#' @examples
#' \dontrun{
#' tracker_bests()
#' }
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
#' @examples
#' \dontrun{
#' lifetime_bests()
#' }
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
