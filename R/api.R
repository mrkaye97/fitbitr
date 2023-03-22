fetch_user_id <- function() {
  if (is.null(.fitbitr_token)) {
    abort("No token provided.")
  }

  if (class(.fitbitr_token) != "httr2_token") {
    abort("You must provide a token of class `httr2_token`")
  }

  user_id <- pluck("user_id", .default = NULL)
  if (is.null(user_id)) {
    abort("The token you provided had no associated `user_id`. Maybe it was empty? Please supply a valid token.")
  }

  user_id
}

#' @importFrom jsonlite toJSON
stop_for_status <- function(response) {
  status_code <- response$status_code
  if (status_code == 200) {
    response
  } else {
    response <- content(response)

    abort(
      c(
        sprintf("Fitbit API request failed with status code %s", status_code),
        "*" = "Error text below:",
        toJSON(response$errors, pretty = TRUE, auto_unbox = TRUE)
      )
    )
  }
}

#' Perform a GET request
#'
#' @rdname get
#'
#' @importFrom httr GET add_headers
#'
#' @param url The URL to make the request to
#' @param \dots Additional arguments (not currently used)
#'
#' @return The response
#' @export
perform_get <- function(url, ...) {
  if (is.null(.fitbitr_token)) {
    abort("No token found. Please run `generate_fitbitr_token()` to create one.")
  }

  if (class(.fitbitr_token) != "httr2_token") {
    abort("You must provide a token of class `httr2_token`")
  }

  response <- GET(
    url,
    add_headers(
      .headers = c(
        Authorization = paste0("Bearer ", .fitbitr_token$access_token)
      )
    )
  )

  if (check_token_expiry(response)) {
    abort("Token expired. Please generate a new one with `generate_fitbitr_token()")
  }

  if (check_rate_limit(response)) {
    abort("Fitbit API rate limit exceeded. For details, see https://dev.fitbit.com/build/reference/web-api/basics/#rate-limits.")
  }

  stop_for_status(response)
}

#' @noRd
#' @param r the API response
#' @return `TRUE` if the token is expired, `FALSE` otherwise
check_token_expiry <- function(r) {
  if (r$status_code == 401 && grepl("expired", content(r, as = "parsed", type = "application/json")$errors[[1]]$message)) {
    TRUE
  } else {
    FALSE
  }
}

#' @noRd
#' @param r the API response
#' @return `TRUE` if the token is expired, `FALSE` otherwise
check_rate_limit <- function(r) {
  if (r$status_code == 429 && grepl("Too Many Requests", content(r, as = "parsed", type = "application/json")$errors[[1]]$message)) {
    TRUE
  } else {
    FALSE
  }
}

