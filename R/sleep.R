#' Nightly Sleep Summary
#'
#' Returns a tibble of summary by night
#'

#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#'
#' @importFrom purrr pluck list_modify map flatten
#' @importFrom tibble enframe
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr arrange across
#'
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#'
#' get_sleep_summary(start_date, end_date)
#' }
#'
#' @return A tibble of a variety of sleep summary data by day
#' @export
get_sleep_summary <- function(start_date, end_date = start_date) {
  url <- sprintf(
    "%s/1.2/user/%s/sleep/date/%s/%s.json",
    base_url,
    extract_user_id(.fitbitr_token),
    start_date,
    end_date
  )

  r <- perform_get(url)

  r %>%
    content(as = "parsed", type = "application/json") %>%
    pluck("sleep") %>%
    map(
      function(x) list_modify(x, "levels" = NULL)
    ) %>%
    bind_rows() %>%
    arrange("dateOfSleep") %>%
    mutate(
      date = as.Date(.data$dateOfSleep)
    ) %>%
    clean_names() %>%
    select(
      "log_id",
      "date",
      "start_time",
      "end_time",
      "duration",
      "efficiency",
      "minutes_to_fall_asleep",
      "minutes_asleep",
      "minutes_awake",
      "minutes_after_wakeup",
      "time_in_bed"
    ) %>%
    mutate(
      across(c("start_time", "end_time"), as_datetime)
    )
}

#' Nightly Sleep Stage Summary Data
#'
#' Returns a tibble of nightly sleep stage data.
#' Minutes in each stage, count of times in each stage, and a thirty day average for the number of minutes in each stage.
#'

#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#'
#' @importFrom purrr pluck list_modify map_dfr
#' @importFrom tibble enframe
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr arrange
#'
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#'
#' get_sleep_stage_summary(start_date, end_date)
#' }
#'
#' @return A tibble of a variety of sleep stage summary data, by day
#' @export
get_sleep_stage_summary <- function(start_date, end_date = start_date) {
  url <- sprintf(
    "%s/1.2/user/%s/sleep/date/%s/%s.json",
    base_url,
    extract_user_id(.fitbitr_token),
    start_date,
    end_date
  )

  r <- perform_get(url) %>%
    content(as = "parsed", type = "application/json") %>%
    pluck("sleep")

  r %>%
    map_dfr(
      function(x) {
        values <- x %>%
          pluck("levels", "summary")

        n <- names(values)

        values %>%
          bind_rows() %>%
          mutate(
            stage = n,
            date = x$dateOfSleep
          )
      }
    ) %>%
    select(
      "date",
      "stage",
      "count",
      "minutes",
      thirty_day_avg_minutes = "thirtyDayAvgMinutes"
    )
}

#' Granular Sleep Stage Data
#'
#' Returns a tibble of nightly sleep stage data.
#' Very granular. Returns blocks of time spent in each phase.
#'

#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#'
#' @importFrom purrr pluck list_modify
#' @importFrom tibble enframe
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr arrange
#'
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#'
#' get_sleep_stage_granular(start_date, end_date)
#' }
#'
#' @return A tibble of granular sleep stage data. This method is more granular than \link[fitbitr]{get_sleep_stage_summary}, and returns blocks of time that you spent in each zone throughout the night.
#' @export
get_sleep_stage_granular <- function(start_date, end_date = start_date) {
  url <- sprintf(
    "%s/1.2/user/%s/sleep/date/%s/%s.json",
    base_url,
    extract_user_id(.fitbitr_token),
    start_date,
    end_date
  )

  r <- perform_get(url) %>%
    content(as = "parsed", type = "application/json") %>%
    pluck("sleep")


  r %>%
    map_dfr(
      pluck, "levels", "data"
    ) %>%
    rename(
      time = "dateTime"
    ) %>%
    mutate(
      time = as_datetime(.data$time)
    )
}
