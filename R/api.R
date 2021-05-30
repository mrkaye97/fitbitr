#' @importFrom httr GET add_headers stop_for_status
#' @param url the endpoint
#' @noRd
get <- function(url) {
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
      finally = function(e) {
        do_refresh <- askYesNo(
          "Could not refresh your token. Would you like to generate a new one?",
          default = FALSE,
          prompts = c('y', 'n', 'c')
        )

        if (do_refresh) {
          message("Trying to generate a new token...")
          .fitbitr_token$init_credentials()
        }
    })

    get(url)
  }

  stop_for_status(r)
}

#' @importFrom httr POST
#' @param url the endpoint
#' @param body the post body
#' @noRd
post <- function(url, body) {
  r <- POST(
    url,
    body = body,
    add_headers(
      .headers = c(
        Authorization = paste0("Bearer ", .fitbitr_token$credentials$access_token)
      )
    )
  )

  if (check_token_expiry(r)) {
    .fitbitr_token$refresh()
    post(url, body)
  }

  stop_for_status(r)
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
