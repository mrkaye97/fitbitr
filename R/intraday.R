stop_for_one_sided_interval <- function(start_time, end_time) {
  if (xor(is.null(start_time), is.null(end_time))) {
    abort("If you provide either `start_time` or `end_time`, both must be provided.")
  }
}

#' Get an intraday time series
#'
#' See the \href{https://dev.fitbit.com/build/reference/web-api/intraday/get-activity-intraday-by-date/}{API documentation} for
#' more detailed explanations of parameters and more usage information and examples.
#'
#' @param token A `fitbitr_token` object or an `httr::Token2.0` object a la \link[httr]{Token2.0}
#' @param resource The resource to get
#' @param date A date to get data for
#' @param detail_level The detail level. One of `"1min"`, `"5min"`, or `"15min"`
#' @param start_time The start time of the time window. Default: `NULL` gets the whole day
#' @param end_time The end time of the time window. Default: `NULL` gets the whole day
#'
#' @importFrom lubridate as_datetime
#' @importFrom rlang .data
#'
#' @return A tibble with two columns: `time` and `{{resource}}`
get_intraday_time_series <- function(
  token,
  resource = c("active-zone-minutes", "calories", "distance", "elevation", "floors", "heart", "steps"),
  date,
  detail_level,
  start_time,
  end_time
) {
  resource <- match.arg(resource)

  if (!is.null(start_time)) {
    url_suffix <- sprintf("/time/%s/%s.json", start_time, end_time)
  } else {
    url_suffix <- ".json"
  }

  url <- sprintf(
    "%s/1/user/%s/activities/%s/date/%s/1d/%s%s",
    base_url,
    extract_user_id(token),
    resource,
    date,
    detail_level,
    url_suffix
  )

  r <- perform_get(token, url)

  r %>%
    content(as = "parsed", type = "application/json") %>%
    pluck(
      sprintf("activities-%s-intraday", resource),
      "dataset"
    ) %>%
    bind_rows() %>%
    dplyr::transmute(
      time = as_datetime(paste(date, .data$time)),
      !!resource := .data$value
    ) %>%
    arrange("time")
}

#' Get intraday calories time series
#'
#' See the \href{https://dev.fitbit.com/build/reference/web-api/intraday/get-activity-intraday-by-date/}{API documentation} for
#' more detailed explanations of parameters and more usage information and examples.
#'
#' @family intraday
#'
#' @param token A `fitbitr_token` object or an `httr::Token2.0` object a la \link[httr]{Token2.0}
#' @param date A date to get data for
#' @param detail_level The detail level. One of `"1min"`, `"5min"`, or `"15min"`
#' @param start_time The start time of the time window. Default: `NULL` gets the whole day
#' @param end_time The end time of the time window. Default: `NULL` gets the whole day
#'
#' @examples
#' \dontrun{
#' date <- lubridate::today()
#'
#' ## get minute by minute data
#' get_calories_intraday(detail_level = "15min")
#'
#' ## get more granular data
#' get_calories_intraday(detail_level = "1min")
#' }
#'
#' @return A tibble with two columns: `time` and `calories`
#' @export
get_calories_intraday <- function(
  token,
  date = lubridate::today(),
  detail_level = c("1min", "5min", "15min"),
  start_time = NULL,
  end_time = NULL
) {
  detail_level <- match.arg(detail_level)
  stop_for_one_sided_interval(start_time, end_time)

  get_intraday_time_series(
    token = token,
    resource = "calories",
    date = date,
    detail_level = detail_level,
    start_time = start_time,
    end_time = end_time
  )
}

#' Get intraday distance time series
#'
#' See the \href{https://dev.fitbit.com/build/reference/web-api/intraday/get-activity-intraday-by-date/}{API documentation} for
#' more detailed explanations of parameters and more usage information and examples.
#'
#' @family intraday
#'
#' @param token A `fitbitr_token` object or an `httr::Token2.0` object a la \link[httr]{Token2.0}
#' @param date A date to get data for
#' @param detail_level The detail level. One of `"1min"`, `"5min"`, or `"15min"`
#' @param start_time The start time of the time window. Default: `NULL` gets the whole day
#' @param end_time The end time of the time window. Default: `NULL` gets the whole day
#'
#' @examples
#' \dontrun{
#' date <- lubridate::today()
#'
#' ## get minute by minute data
#' get_distance_intraday(detail_level = "15min")
#'
#' ## get more granular data
#' get_distance_intraday(detail_level = "1min")
#' }
#'
#' @return A tibble with two columns: `time` and `distance`
#' @export
get_distance_intraday <- function(
  token,
  date = lubridate::today(),
  detail_level = c("1min", "5min", "15min"),
  start_time = NULL,
  end_time = NULL
) {
  detail_level <- match.arg(detail_level)
  stop_for_one_sided_interval(start_time, end_time)

  get_intraday_time_series(
    token = token,
    resource = "distance",
    date = date,
    detail_level = detail_level,
    start_time = start_time,
    end_time = end_time
  )
}

#' Get intraday floors time series
#'
#' See the \href{https://dev.fitbit.com/build/reference/web-api/intraday/get-activity-intraday-by-date/}{API documentation} for
#' more detailed explanations of parameters and more usage information and examples.
#'
#' @family intraday
#'
#' @param token A `fitbitr_token` object or an `httr::Token2.0` object a la \link[httr]{Token2.0}
#' @param date A date to get data for
#' @param detail_level The detail level. One of `"1min"`, `"5min"`, or `"15min"`
#' @param start_time The start time of the time window. Default: `NULL` gets the whole day
#' @param end_time The end time of the time window. Default: `NULL` gets the whole day
#'
#' @examples
#' \dontrun{
#' date <- lubridate::today()
#'
#' ## get minute by minute data
#' get_floors_intraday(detail_level = "15min")
#'
#' ## get more granular data
#' get_floors_intraday(detail_level = "1min")
#' }
#'
#' @return A tibble with two columns: `time` and `floors`
#' @export
get_floors_intraday <- function(
  token,
  date = lubridate::today(),
  detail_level = c("1min", "5min", "15min"),
  start_time = NULL,
  end_time = NULL
) {
  detail_level <- match.arg(detail_level)
  stop_for_one_sided_interval(start_time, end_time)

  get_intraday_time_series(
    token = token,
    resource = "floors",
    date = date,
    detail_level = detail_level,
    start_time = start_time,
    end_time = end_time
  )
}

#' Get intraday steps time series
#'
#' See the \href{https://dev.fitbit.com/build/reference/web-api/intraday/get-activity-intraday-by-date/}{API documentation} for
#' more detailed explanations of parameters and more usage information and examples.
#'
#' @family intraday
#'
#' @param token A `fitbitr_token` object or an `httr::Token2.0` object a la \link[httr]{Token2.0}
#' @param date A date to get data for
#' @param detail_level The detail level. One of `"1min"`, `"5min"`, or `"15min"`
#' @param start_time The start time of the time window. Default: `NULL` gets the whole day
#' @param end_time The end time of the time window. Default: `NULL` gets the whole day
#'
#' @examples
#' \dontrun{
#' date <- lubridate::today()
#'
#' ## get minute by minute data
#' get_steps_intraday(detail_level = "15min")
#'
#' ## get more granular data
#' get_steps_intraday(detail_level = "1min")
#' }
#'
#' @return A tibble with two columns: `time` and `steps`
#' @export
get_steps_intraday <- function(
  token,
  date = lubridate::today(),
  detail_level = c("1min", "5min", "15min"),
  start_time = NULL,
  end_time = NULL
) {
  detail_level <- match.arg(detail_level)
  stop_for_one_sided_interval(start_time, end_time)

  get_intraday_time_series(
    token = token,
    resource = "steps",
    date = date,
    detail_level = detail_level,
    start_time = start_time,
    end_time = end_time
  )
}

#' Get intraday elevation time series
#'
#' See the \href{https://dev.fitbit.com/build/reference/web-api/intraday/get-activity-intraday-by-date/}{API documentation} for
#' more detailed explanations of parameters and more usage information and examples.
#'
#' @family intraday
#'
#' @param token A `fitbitr_token` object or an `httr::Token2.0` object a la \link[httr]{Token2.0}
#' @param date A date to get data for
#' @param detail_level The detail level. One of `"1min"`, `"5min"`, or `"15min"`
#' @param start_time The start time of the time window. Default: `NULL` gets the whole day
#' @param end_time The end time of the time window. Default: `NULL` gets the whole day
#'
#' @examples
#' \dontrun{
#' date <- lubridate::today()
#'
#' ## get minute by minute data
#' get_elevation_intraday(detail_level = "15min")
#'
#' ## get more granular data
#' get_elevation_intraday(detail_level = "1min")
#' }
#'
#' @return A tibble with two columns: `time` and `elevation`
#' @export
get_elevation_intraday <- function(
  token,
  date = lubridate::today(),
  detail_level = c("1min", "5min", "15min"),
  start_time = NULL,
  end_time = NULL
) {
  detail_level <- match.arg(detail_level)
  stop_for_one_sided_interval(start_time, end_time)

  get_intraday_time_series(
    token = token,
    resource = "elevation",
    date = date,
    detail_level = detail_level,
    start_time = start_time,
    end_time = end_time
  )
}

#' Get intraday heart time series
#'
#' See the \href{https://dev.fitbit.com/reference/web-api/heart-rate/#get-heart-rate-time-series}{API documentation} for
#' more detailed explanations of parameters and more usage information and examples.
#'
#' @family intraday
#'
#' @param token A `fitbitr_token` object or an `httr::Token2.0` object a la \link[httr]{Token2.0}
#' @param date A date to get data for
#' @param detail_level The detail level. One of `"1min"`, `"5min"`, or `"15min"`
#' @param start_time The start time of the time window. Default: `NULL` gets the whole day
#' @param end_time The end time of the time window. Default: `NULL` gets the whole day
#'
#' @examples
#' \dontrun{
#' date <- lubridate::today()
#'
#' ## get minute by minute data
#' get_heart_rate_intraday(detail_level = "15min")
#'
#' ## get more granular data
#' get_heart_rate_intraday(detail_level = "1min")
#' }
#' @export
get_heart_rate_intraday <- function(
  token,
  date = lubridate::today(),
  detail_level = c("1min", "5min", "15min"),
  start_time = NULL,
  end_time = NULL
) {
  detail_level <- match.arg(detail_level)
  stop_for_one_sided_interval(start_time, end_time)

  get_intraday_time_series(
    token = token,
    resource = "heart",
    date = date,
    detail_level = detail_level,
    start_time = start_time,
    end_time = end_time
  ) %>%
    rename(heart_rate = "heart")
}

#' Get intraday active zone minutes time series
#'
#' See the \href{https://dev.fitbit.com/build/reference/web-api/intraday/get-azm-intraday-by-date/}{API documentation} for
#' more detailed explanations of parameters and more usage information and examples.
#'
#' @family intraday
#'
#' @param token A `fitbitr_token` object or an `httr::Token2.0` object a la \link[httr]{Token2.0}
#' @param date A date to get data for
#' @param detail_level The detail level. One of `"1min"`, `"5min"`, or `"15min"`
#' @param start_time The start time of the time window. Default: `NULL` gets the whole day
#' @param end_time The end time of the time window. Default: `NULL` gets the whole day
#'
#' @examples
#' \dontrun{
#' date <- lubridate::today()
#'
#' ## get minute by minute data
#' get_active_zone_minutes_intraday(detail_level = "15min")
#'
#' ## get more granular data
#' get_active_zone_minutes_intraday(detail_level = "1min")
#' }
#' @export
get_active_zone_minutes_intraday <- function(
  token,
  date = lubridate::today(),
  detail_level = c("1min", "5min", "15min"),
  start_time = NULL,
  end_time = NULL
) {
  detail_level <- match.arg(detail_level)
  stop_for_one_sided_interval(start_time, end_time)

  get_intraday_time_series(
    token = token,
    resource = "active-zone-minutes",
    date = date,
    detail_level = detail_level,
    start_time = start_time,
    end_time = end_time
  ) %>%
    clean_names()
}
