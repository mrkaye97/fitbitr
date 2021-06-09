#' @importFrom httr GET add_headers stop_for_status
#' @importFrom utils askYesNo
#' @param url the endpoint
#' @param .example_identifier An internal identifier to choose which example to run
#' @noRd
get <- function(url, .example_identifier) {
  if (Sys.getenv("FITBITR_ENVIRONMENT") == "testing mode") {
    r <- get_example_response(url, .example_identifier)
  } else {
    r <- GET(
      url,
      add_headers(
        .headers = c(
          Authorization = paste0("Bearer ", .fitbitr_token$credentials$access_token)
        )
      )
    )

    if (check_token_expiry(r)) {
      tryCatch(
        .fitbitr_token$refresh(),
        error = function(e) ask_refresh("Token expired. Trying to refresh...", e)
      )

      get(url)
    }

    if (check_rate_limit(r)) {
      abort("Fitbit API rate limit exceeded. For details, see https://dev.fitbit.com/build/reference/web-api/basics/#rate-limits.")
    }

    tryCatch(
      stop_for_status(r),
      error = function(e) {
        ask_refresh("Failed to query the API with your token", e)
      }
    )
  }
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
#' @param reason A string reason for why the request failed
#' @param error_message the error message
#' @importFrom rlang inform
#' @return No return value. Called for side effects
ask_refresh <- function(reason, error_message) {
  inform(sprintf("%s. Error message: \n\n", error_message))
  inform(error_message)
  inform("\n")

  do_refresh <- askYesNo(
    "Would you like to generate a new token?",
    default = FALSE,
    prompts = c("y", "n", "c")
  )

  if (do_refresh & !is.na(do_refresh)) {
    inform("Trying to generate a new token...")
    .fitbitr_token$init_credentials()
  }

  invisible()
}
