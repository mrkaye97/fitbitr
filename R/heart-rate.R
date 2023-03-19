#' Heart Rate Intraday
#'
#' Returns heart rate data for the specified day
#'
#' @param date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param minutes a boolean for whether data should be returned in minutes (TRUE) or seconds (FALSE)
#' @importFrom dplyr rename
#' @importFrom lubridate as_datetime
#' @importFrom rlang .data
#' @return A tibble of the `time` and your `heart_rate` at that time.
#' @details
#' See \url{https://dev.fitbit.com/reference/web-api/heart-rate/#get-heart-rate-time-series} for more details.
#'
#' @examples
#' \dontrun{
#' date <- lubridate::today()
#'
#' ## get minute by minute data
#' get_heart_rate_intraday(date, minutes = TRUE)
#'
#' ## get more granular data
#' ##  (not necessarily by second, but more granular than minutes)
#' get_heart_rate_intraday(date, minutes = FALSE)
#' }
#' @export
get_heart_rate_intraday <- function(date, minutes = TRUE) {
  check_token_exists()

  url <- sprintf(
    "%s/1/user/%s/activities/heart/date/%s/1d/%s.json",
    base_url,
    .fitbitr_token$credentials$user_id,
    date,
    ifelse(minutes, "1min", "1sec")
  )

  r <- get(
    url = url,
    .example_identifier = "hr intraday"
  )

  r %>%
    content(as = "parsed", type = "application/json") %>%
    pluck("activities-heart-intraday", "dataset") %>%
    bind_rows() %>%
    mutate(
      time = as_datetime(paste0(date, "time"))
    ) %>%
    rename(
      heart_rate = "value"
    ) %>%
    clean_names()
}

#' Heart Rate Zones
#'
#' See \url{https://dev.fitbit.com/build/reference/web-api/activity/} for more details.
#' @importFrom purrr map_chr
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#'
#' get_heart_rate_zones(start_date, end_date)
#' }
#' @return A tibble of the date, the heart rate zone (`zone`), the minimum heart rate in that zone (`min_hr`), the maximum heart rate in that zone (`max_hr`), the minutes in that zone (`minutes_in_zone`), and the calories burned in that zone (`calories_out`)
#' @export
get_heart_rate_zones <- function(start_date, end_date = start_date) {
  check_token_exists()

  url <- sprintf(
    "%s/1/user/%s/activities/heart/date/%s/%s.json",
    base_url,
    .fitbitr_token$credentials$user_id,
    start_date,
    end_date
  )

  r <- get(
    url = url,
    .example_identifier = "hr zones"
  )

  hr_data <- r %>%
    content(as = "parsed", type = "application/json") %>%
    pluck("activities-heart") %>%
    map(
      pluck, "value"
    ) %>%
    map(
      pluck, "heartRateZones"
    ) %>%
    bind_rows()

  dates <- r %>%
    content(as = "parsed", type = "application/json") %>%
    pluck("activities-heart") %>%
    map_chr(
      pluck, "dateTime"
    )

  hr_data %>%
    mutate(
      date = dates %>% map(rep, 4) %>% unlist() %>% as.Date()
    ) %>%
    select(
      "date",
      zone = "name",
      min_hr = "min",
      max_hr = "max",
      minutes_in_zone = "minutes",
      calories_out = "caloriesOut"
    )
}
