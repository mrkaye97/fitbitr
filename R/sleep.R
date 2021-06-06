#' Nightly Sleep Summary
#'
#' Returns a tibble of summary by night
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @importFrom purrr pluck list_modify map flatten
#' @importFrom tibble enframe
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr arrange
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#'
#' sleep_summary(start_date, end_date)
#' }
#' @return A tibble of a variety of sleep summary data by day
#' @export
sleep_summary <- function(start_date, end_date = start_date) {
  check_token_exists()

  url <- sprintf(
    "%s/1.2/user/%s/sleep/date/%s/%s.json",
    base_url,
    .fitbitr_token$credentials$user_id,
    start_date,
    end_date
  )

  r <- get(
    url = url,
    .example_identifier = "sleep summary"
  )

  r %>%
    content(as = "parsed", type = "application/json") %>%
    pluck("sleep") %>%
    map(
      function(x) list_modify(x, "levels" = NULL)
    ) %>%
    bind_rows() %>%
    arrange(.data$dateOfSleep) %>%
    mutate(
      date = as.Date(.data$dateOfSleep)
    ) %>%
    clean_names() %>%
    select(
      .data$log_id,
      .data$date,
      .data$start_time,
      .data$end_time,
      .data$duration,
      .data$efficiency,
      .data$minutes_to_fall_asleep,
      .data$minutes_asleep,
      .data$minutes_awake,
      .data$minutes_after_wakeup,
      .data$time_in_bed
    )
}

#' Nightly Sleep Stage Summary Data
#'
#' Returns a tibble of nightly sleep stage data.
#' Minutes in each stage, count of times in each stage, and a thirty day average for the number of minutes in each stage.
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @importFrom purrr pluck list_modify
#' @importFrom tibble enframe
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr arrange
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#'
#' sleep_stage_summary(start_date, end_date)
#' }
#' @return A tibble of a variety of sleep stage summary data, by day
#' @export
sleep_stage_summary <- function(start_date, end_date = start_date) {
  check_token_exists()

  url <- sprintf(
    "%s/1.2/user/%s/sleep/date/%s/%s.json",
    base_url,
    .fitbitr_token$credentials$user_id,
    start_date,
    end_date
  )

  r <- get(
    url = url,
    .example_identifier = "sleep stage"
  ) %>%
    content(as = "parsed", type = "application/json") %>%
    pluck("sleep")


  sleep <- r %>%
    map(
      pluck, "levels"
    ) %>%
    map(
      pluck, "summary"
    ) %>%
    flatten() %>%
    enframe() %>%
    unnest_wider(.data$value)

  dates <- r %>%
    map_chr(
      pluck, "dateOfSleep"
    ) %>%
    map(
      rep, 4
    ) %>%
    unlist() %>%
    as.Date()

  sleep %>%
    mutate(
      date = dates
    ) %>%
    select(
      .data$date,
      stage = .data$name,
      .data$count,
      .data$minutes,
      thirty_day_avg_minutes = .data$thirtyDayAvgMinutes
    )
}

#' Granular Sleep Stage Data
#'
#' Returns a tibble of nightly sleep stage data.
#' Very granular. Returns blocks of time spent in each phase.
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @importFrom purrr pluck list_modify
#' @importFrom tibble enframe
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr arrange
#' @examples
#' \dontrun{
#' start_date <- lubridate::today() - lubridate::weeks(1)
#' end_date <- lubridate::today()
#'
#' sleep_stage_granular(start_date, end_date)
#' }
#' @return A tibble of granular sleep stage data. This method is more granular than \link[fitbitr]{sleep_stage_summary}, and returns blocks of time that you spent in each zone throughout the night.
#' @export
sleep_stage_granular <- function(start_date, end_date = start_date) {
  check_token_exists()

  url <- sprintf(
    "%s/1.2/user/%s/sleep/date/%s/%s.json",
    base_url,
    .fitbitr_token$credentials$user_id,
    start_date,
    end_date
  )

  r <- get(
    url = url,
    .example_identifier = "sleep stage"
  ) %>%
    content(as = "parsed", type = "application/json") %>%
    pluck("sleep")


  r %>%
    map(
      pluck, "levels"
    ) %>%
    map(
      pluck, "data"
    ) %>%
    bind_rows() %>%
    rename(
      time = .data$dateTime
    ) %>%
    mutate(
      time = as_datetime(.data$time)
    )
}
