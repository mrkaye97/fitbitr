#' @importFrom httr GET add_headers
#' @param url the endpoint
#' @param token your Fitbit API bearer token
#' @noRd
get <- function(url, token = Sys.getenv("FITBIT_ACCESS_TOKEN")) {
  r <- GET(
    url,
    add_headers(
      .headers = c(
        Authorization = paste0("Bearer ", token)
      )
    )
  )

  check_response(r)
}

#' @importFrom httr POST
#' @param url the endpoint
#' @param body the post body
#' @param token your Fitbit API bearer token
#' @noRd
post <- function(url, body, token = Sys.getenv("FITBIT_ACCESS_TOKEN")) {
  r <- POST(
    url,
    body = body,
    add_headers(
      .headers = c(
        Authorization = paste0("Bearer ", token)
      )
    )
  )

  check_response(r)
}

#' @noRd
#' @param r the API response
#' @return r (the response) on success, and otherwise errors out with an informative message
check_response <- function(r) {
  if (r$status_code == 200) {
    return(r)
  } else if (r$status_code == 401 && grepl("expired", content(r, as = 'parsed', type = 'application/json')$errors[[1]]$message)) {
    stop("Your Fitbit Access Token has expired. Refresh it with refresh_token()")
  } else {
    msg <- content(r)$errors[[1]]$message
    stop(msg)
  }
}

