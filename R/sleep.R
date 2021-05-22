#' Nightly Sleep Summary
#'
#' Returns a single row tibble of summary data on the night of sleep
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
    "%s/user/%s/sleep/date/%s/%s.json",
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
      function(x) list_modify(x, "minuteData" = NULL)
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
      awake_count = .data$awakeCount,
      awakenings_count = .data$awakeningsCount,
      awake_duration = .data$awakeDuration,
      restless_count = .data$restlessCount,
      restless_duration = .data$restlessDuration,
      time_in_bed = .data$timeInBed
    )
}
