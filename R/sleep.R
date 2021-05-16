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
get_sleep_summary <- function(start_date, end_date = NULL, token = Sys.getenv("FITBIT_ACCESS_TOKEN"), user_id = Sys.getenv("FITBIT_USER_ID")) {

  check_config_exists(token, user_id)

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
    arrange(.data$dateOfSleep)
}
