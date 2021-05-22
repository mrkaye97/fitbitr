#' Nightly Sleep Summary
#'
#' Returns a tibble of summary by night
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @importFrom purrr pluck list_modify
#' @importFrom tibble enframe
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr arrange
#' @return a tibble of sleep summary data
#' @export
sleep_summary <- function(start_date, end_date = start_date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  check_config_exists(token, user_id)

  url <- sprintf(
    "%s/1.2/user/%s/sleep/date/%s/%s.json",
    base_url,
    user_id,
    start_date,
    end_date
  )

  r <- get(
    url = url,
    token = token
  )

  r %>%
    content() %>%
    pluck("sleep") %>%
    map(
      function(x) list_modify(x, "levels" = NULL)
    ) %>%
    bind_rows() %>%
    arrange(.data$dateOfSleep) %>%
    select(
      log_id = .data$logId,
      date = .data$dateOfSleep,
      start_time = .data$startTime,
      end_time = .data$endTime,
      .data$duration,
      .data$efficiency,
      minutes_to_fall_asleep = .data$minutesToFallAsleep,
      minutes_asleep = .data$minutesAsleep,
      minutes_awake = .data$minutesAwake,
      minutes_after_wakeup = .data$minutesAfterWakeup,
      time_in_bed = .data$timeInBed
    )
}

#' Nightly Sleep Stage Summary Data
#'
#' Returns a tibble of nightly sleep stage data.
#' Minutes in each stage, count of times in each stage, and a thirty day average for the number of minutes in each stage.
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @importFrom purrr pluck list_modify
#' @importFrom tibble enframe
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr arrange
#' @return a tibble of sleep summary data
#' @export
sleep_stage_summary <- function(start_date, end_date = start_date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  check_config_exists(token, user_id)

  url <- sprintf(
    "%s/1.2/user/%s/sleep/date/%s/%s.json",
    base_url,
    user_id,
    start_date,
    end_date
  )

  r <- get(
    url = url,
    token = token
  ) %>%
    content() %>%
    pluck("sleep")


  sleep <- r %>%
    map(
      pluck, 'levels'
    ) %>%
    map(
      pluck, 'summary'
    ) %>%
    flatten() %>%
    enframe() %>%
    unnest_wider(.data$value)

  dates <- r %>%
    map_chr(
      pluck, 'dateOfSleep'
    ) %>%
    map(
      rep, 4
    ) %>%
    unlist()

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
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @importFrom purrr pluck list_modify
#' @importFrom tibble enframe
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr arrange
#' @return a tibble of sleep summary data
#' @export
sleep_stage_granular <- function(start_date, end_date = start_date, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {
  check_config_exists(token, user_id)

  url <- sprintf(
    "%s/1.2/user/%s/sleep/date/%s/%s.json",
    base_url,
    user_id,
    start_date,
    end_date
  )

  r <- get(
    url = url,
    token = token
  ) %>%
    content() %>%
    pluck("sleep")


  r %>%
    map(
      pluck, 'levels'
    ) %>%
    map(
      pluck, 'data'
    ) %>%
    bind_rows() %>%
    rename(
      time = .data$dateTime
    )
}
