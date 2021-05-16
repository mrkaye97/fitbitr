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
