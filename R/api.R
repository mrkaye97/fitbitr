extract_user_id <- function(token) {
  if (missing(token) || is.null(token)) {
    abort("No token provided.")
  }

  if (!class(token)[1] %in% c("fitbitr_token", "Token2.0")) {
    abort("You must provide a token of class `fitbitr_token` or `httr::Token2.0.")
  }

  user_id <- pluck(token, "credentials", "user_id", .default = NULL)
  if (is.null(user_id)) {
    abort("The token you provided had no associated `user_id`. Maybe it was empty? Please supply a valid token.")
  }

  token$credentials$user_id
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
#' @param token A `fitbitr_token` object or an `httr::Token2.0` object a la \link[httr]{Token2.0}
#' @param url The URL to make the request to
#' @param \dots Additional arguments (not currently used)
#'
#' @return The response
#' @export
perform_get <- function(token, url, ...) {
  if (is.null(token) || !(class(token)[1] %in% c("fitbitr_token", "Token2.0"))) {
    abort("You must supply a valid token.")
  }

  response <- GET(
    url,
    add_headers(
      .headers = c(
        Authorization = paste0("Bearer ", token$credentials$access_token)
      )
    )
  )

  if (check_token_expiry(response)) {
    tryCatch(
      {
        inform("Token expired. Trying to refresh...\n\n ...\n")
        token <- refresh_api_token(token)
      },
      error = function(e) {
        ask_refresh("Refresh failed", e)
      }
    )

    return(perform_get(token, url))
  }

  if (check_rate_limit(response)) {
    abort("Fitbit API rate limit exceeded. For details, see https://dev.fitbit.com/build/reference/web-api/basics/#rate-limits.")
  }

  tryCatch(
    stop_for_status(response),
    error = function(e) {
      ask_refresh("Failed to query the API with your token", e)
    }
  )
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

#' @noRd
#'
#' @param token A `fitbitr_token` object or an `httr::Token2.0` object a la \link[httr]{Token2.0}
#' @param reason A string reason for why the request failed
#' @param error_message the error message
#'
#' @importFrom rlang inform abort
#' @importFrom utils askYesNo
#'
#' @return No return value. Called for side effects
ask_refresh <- function(token, reason, error_message) {
  inform(sprintf("%s. Error message: \n\n", reason))
  inform(error_message$message)
  inform("\n")

  do_refresh <- askYesNo(
    "Would you like to generate a new token?",
    default = FALSE,
    prompts = c("y", "n", "c")
  )

  if (do_refresh & !is.na(do_refresh)) {
    inform("Trying to generate a new token...")
    refresh_api_token(token)
  } else {
    abort("No token was found, and a new one was not generated.")
  }

  invisible()
}
