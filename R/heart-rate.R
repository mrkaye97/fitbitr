#' Heart Rate Zones
#'
#' See \url{https://dev.fitbit.com/build/reference/web-api/activity/} for more details.
#'
#' @importFrom purrr map_chr
#'
#' @param token A `fitbitr_token` object or an `httr::Token2.0` object a la \link[httr]{Token2.0}
#' @param start_date The start date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param end_date The end date of records to be returned in "yyyy-mm-dd" or date(time) format
#'
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
  url <- sprintf(
    "%s/1/user/%s/activities/heart/date/%s/%s.json",
    base_url,
    extract_user_id.fitbitr_token,
    start_date,
    end_date
  )

  r <- perform_get(url)

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
