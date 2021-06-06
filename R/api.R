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
      message("Token expired. Trying to refresh...")
      tryCatch(
        .fitbitr_token$refresh(),
        error = function(e) ask_refresh("refresh", e)
      )

      get(url)
    }

    tryCatch(
      stop_for_status(r),
      error = function(e) {
        ask_refresh("successfully query the API with", e)
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
#' @param reason A string reason for why the request failed
#' @param error_message the error message
#' @return No return value. Called for side effects
ask_refresh <- function(reason, error_message) {
  message(sprintf("Could not %s your token. Error message:\n", reason))
  message(error_message)
  message("\n")

  do_refresh <- askYesNo(
    "Would you like to generate a new token?",
    default = FALSE,
    prompts = c("y", "n", "c")
  )

  if (do_refresh & !is.na(do_refresh)) {
    message("Trying to generate a new token...")
    .fitbitr_token$init_credentials()
  }

  invisible()
}
