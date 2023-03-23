#' Activity Summary
#'
#' See \url{https://dev.fitbit.com/build/reference/web-api/activity/} for more details.
#'
#' @param date The date of records to be returned in "yyyy-mm-dd" or date(time) format
#'
#' @importFrom dplyr bind_rows mutate select everything
#' @importFrom httr content
#' @importFrom janitor clean_names
#'
#' @examples
#' \dontrun{
#' date <- lubridate::today()
#' get_activity_summary(date)
#' }
#' @return A tibble with the `date` and a number of activity summary metrics for the day.
#' @export
get_activity_summary <- function(date) {
  validate_date(date)

  url <- sprintf(
    "%s/1/user/%s/activities/date/%s.json",
    base_url,
    fetch_user_id(),
    date
  )

  r <- perform_get(url)

  r %>%
    content(as = "parsed", type = "application/json") %>%
    pluck("summary") %>%
    list_modify("heartRateZones" = NULL) %>%
    list_modify("distances" = NULL) %>%
    bind_rows() %>%
    mutate(
      date = as.Date(date),
      across(-"date", as.numeric)
    ) %>%
    select("date", everything()) %>%
    clean_names()
}

#' Activity Time Series
#'
#' See \url{https://dev.fitbit.com/build/reference/web-api/activity/} for more details.
#'
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param resource_path The resource path. See \url{https://dev.fitbit.com/build/reference/web-api/activity/} for options
#'
#' @importFrom purrr flatten_dfr
#' @importFrom rlang :=
#' @importFrom dplyr rename
#'
#' @noRd
get_activity_time_series <- function(start_date, end_date, resource_path) {
  validate_date(start_date)
  validate_date(end_date)

  url <- sprintf(
    "%s/1/user/%s/activities/%s/date/%s/%s.json",
    base_url,
    fetch_user_id(),
    resource_path,
    start_date,
    end_date
  )

  r <- perform_get(url)

  r %>%
    content(as = "parsed", type = "application/json") %>%
    flatten_dfr() %>%
    rename(
      date = "dateTime",
      !!resource_path := "value"
    ) %>%
    mutate(
      date = as.Date(.data$date),
      across(-"date", as.numeric)
    ) %>%
    clean_names()
}

#' Calories Time Series
#'
#' Resource path /activities/calories
#'

#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#'
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#' calories(date)
#' }
#'
#' @return A tibble with two columns: `date` and `calories`
#' @export
get_calories <- function(start_date, end_date) {
  get_activity_time_series(
    start_date,
    end_date,
    resource_path = "calories"
  )
}

#' Calories BMR Time Series
#'
#' Resource path /activities/caloriesBMR
#'
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#'
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#' get_calories_bmr(date)
#' }
#'
#' @return A tibble with two columns: `date` and `calories_bmr`
#' @export
get_calories_bmr <- function(start_date, end_date) {
  get_activity_time_series(
    start_date,
    end_date,
    resource_path = "caloriesBMR"
  )
}

#' Steps Time Series
#'
#' Resource path /activities/steps
#'
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#'
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#' get_steps(date)
#' }
#'
#' @return A tibble with two columns: `date` and `steps`
#' @export
get_steps <- function(start_date, end_date) {
  get_activity_time_series(
    start_date,
    end_date,
    resource_path = "steps"
  )
}

#' Distance Time Series
#'
#' Resource path /activities/distance
#'
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#'
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#' get_distance(date)
#' }
#'
#' @return A tibble with two columns: `date` and `distance`
#' @export
get_distance <- function(start_date, end_date) {
  get_activity_time_series(
    start_date,
    end_date,
    resource_path = "distance"
  )
}

#' Floors Time Series
#'
#' Resource path /activities/floors
#'
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#'
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#' get_floors(date)
#' }
#'
#' @return A tibble with two columns: `date` and `floors`
#' @export
get_floors <- function(start_date, end_date) {
  get_activity_time_series(
    start_date,
    end_date,
    resource_path = "floors"
  )
}

#' Elevation Time Series
#'
#' Resource path /activities/elevation
#'
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#'
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#' get_elevation(date)
#' }
#'
#' @return A tibble with two columns: `date` and `elevation`
#' @export
get_elevation <- function(start_date, end_date) {
  get_activity_time_series(
    start_date,
    end_date,
    resource_path = "elevation"
  )
}

#' Minutes Sedentary Time Series
#'
#' Resource path /activities/minutesSedentary
#'
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#'
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#' get_minutes_sedentary(date)
#' }
#'
#' @return A tibble with two columns: `date` and `minutes_sedentary`
#' @export
get_minutes_sedentary <- function(start_date, end_date) {
  get_activity_time_series(
    start_date,
    end_date,
    resource_path = "minutesSedentary"
  )
}

#' Minutes Lightly Active Time Series
#'
#' Resource path /activities/minutesLightlyActive
#'
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#'
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#' get_minutes_lightly_active(date)
#' }
#'
#' @return A tibble with two columns: `date` and `minutes_lightly_active`
#' @export
get_minutes_lightly_active <- function(start_date, end_date) {
  get_activity_time_series(
    start_date,
    end_date,
    resource_path = "minutesLightlyActive"
  )
}

#' Minutes Fairly Active Time Series
#'
#' Resource path /activities/minutesFairlyActive
#'
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#'
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#' get_minutes_fairly_active(date)
#' }
#'
#' @return A tibble with two columns: `date` and `minutes_fairly_active`
#' @export
get_minutes_fairly_active <- function(start_date, end_date) {
  get_activity_time_series(
    start_date,
    end_date,
    resource_path = "minutesFairlyActive"
  )
}

#' Minutes Very Active Time Series
#'
#' Resource path /activities/minutesVeryActive
#'
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#'
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#' get_minutes_very_active(date)
#' }
#'
#' @return A tibble with two columns: `date` and `minutes_very_active`
#' @export
get_minutes_very_active <- function(start_date, end_date) {
  get_activity_time_series(
    start_date,
    end_date,
    resource_path = "minutesVeryActive"
  )
}

#' Activity Calories Time Series
#'
#' Resource path /activities/activityCalories
#'
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#'
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#' get_activity_calories(date)
#' }
#'
#' @return A tibble with two columns: `date` and `activity_calories`
#' @export
get_activity_calories <- function(start_date, end_date) {
  get_activity_time_series(
    start_date,
    end_date,
    resource_path = "activityCalories"
  )
}

#' @noRd
get_bests_and_totals <- function(best, tracker) {
  url <- sprintf(
    "%s/1/user/%s/activities.json",
    base_url,
    fetch_user_id()
  )

  r <- perform_get(url)

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
#'
#'
#' @examples
#' \dontrun{
#' get_tracker_totals()
#' }
#'
#' @export
#' @return A tibble of all-time tracker totals (i.e. the total `distance`, `floors`, and `steps` tracked by your tracker)
get_tracker_totals <- function() {
  get_bests_and_totals(
    best = FALSE,
    tracker = TRUE
  ) %>%
    bind_rows() %>%
    select(
      "distance",
      "floors",
      "steps"
    )
}

#' Lifetime Totals
#'
#' Retrieve lifetime total distance, floors, and steps
#'
#'
#' @examples
#' \dontrun{
#' get_lifetime_totals()
#' }
#'
#' @return A tibble of all-time totals across trackers (i.e. the total `distance`, `floors`, and `steps` tracked across all of your trackers)
#' @export
get_lifetime_totals <- function() {
  get_bests_and_totals(
    best = FALSE,
    tracker = FALSE
  ) %>%
    bind_rows() %>%
    select(
      "distance",
      "floors",
      "steps"
    )
}

#' Tracker Bests
#'
#' Retrieve tracker best distance, floors, and steps
#'
#'
#' @examples
#' \dontrun{
#' get_tracker_bests()
#' }
#'
#' @importFrom tidyr unnest_wider
#' @importFrom tibble enframe
#'
#' @return A tibble the best `distance`, `floors`, and `steps` (by date) tracked on your tracker
#' @export
get_tracker_bests <- function() {
  get_bests_and_totals(
    best = TRUE,
    tracker = TRUE
  ) %>%
    enframe() %>%
    unnest_wider("value") %>%
    rename(
      metric = "name"
    )
}

#' Lifetime Bests
#'
#' Retrieve lifetime best distance, floors, and steps
#'
#'
#' @examples
#' \dontrun{
#' get_lifetime_bests()
#' }
#'
#' @return A tibble the best `distance`, `floors`, and `steps` (by date) tracked on any of your trackers
#' @export
get_lifetime_bests <- function() {
  get_bests_and_totals(
    best = TRUE,
    tracker = FALSE
  ) %>%
    enframe() %>%
    unnest_wider("value") %>%
    rename(
      metric = "name"
    )
}
