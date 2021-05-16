# Constants
url_sleep <- paste0(url_api, "sleep/")

#' Nightly Sleep Summary
#'
#' Returns a single row tibble of summary data on the night of sleep
#' @param date The date of records to be returned in "yyyy-mm-dd" or date(time) format
#' @param token Fitbit access token
#' @param user_id Fitbit user id
#' @importFrom purrr pluck list_modify
#' @importFrom tibble enframe
#' @importFrom tidyr pivot_wider
#' @return a tibble of sleep summary data
#' @export
get_sleep_summary <- function(start_date, end_date = NULL, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {

  start_date <- paste0('/', as.Date(start_date))
  if (!is.null(end_date)) end_date <- paste0('/', as.Date(end_date))

  url <- paste0(url_sleep, 'date', start_date, end_date, '.json')
  url <- gsub('user/-/', paste0("user/", user_id, "/"), url)

  r <- get(
    url = url,
    token = token
  )

  r %>%
    content() %>%
    pluck('sleep') %>%
    map(
      function(x) list_modify(x, "minuteData" = NULL)
    ) %>%
    bind_rows() %>%
    arrange(dateOfSleep)
}

#' Get Sleep Time Series
#'
#' Returns time series data in the specified range for a given resource in the format requested.
#' Note: Even if you provide earlier dates in the request, the response retrieves only data since the user's join date or the first log entry date for the requested collection.
#'
#' @param token An OAuth 2.0 token
#' @param resource_path	The resource path; see the Resource Path Options below for a list of options.
#' @param date	The end date of the period specified in the format "yyyy-MM-dd" or "today" as a string or Date object.
#' @param period	The range for which data will be returned. Options are 1d, 7d, 30d, 1w, 1m, 3m, 6m, 1y, or max.
#' @param base_date	The range start date in the format "yyyy-MM-dd" or "today" as a string or Date object.
#' @param end_date	The end date of the range in the format "yyyy-MM-dd" or "today" as a string or Date object.
#' @details Resource Path Options in more detail.
#' \itemize{
#'   \item{startTime}
#'   \item{timeInBed}
#'   \item{minutesAsleep}
#'   \item{awakeningsCount}
#'   \item{minutesAwake}
#'   \item{minutesToFallAsleep}
#'   \item{minutesAfterWakeup}
#'   \item{efficiency}
#' }
#'
#' @export
get_sleep <- function(token, resource_path, date = "", period = "", base_date = "", end_date = "") {
  url <- if (date != "" && period != "") {
    paste0(url_sleep, sprintf("%s/date/%s/%s.json", resource_path, as.Date(date), period))
  } else if (base_date != "" & end_date != "") {
    paste0(url_sleep, sprintf("date/%s/%s.json", as.Date(base_date), as.Date(end_date)))
  }

  url <- gsub('user/-/', paste0("user/", user_id, "/"), url)

  x <- content(get(url, token))
}

