#' @importFrom httr GET add_headers
get <- function(url, token = Sys.getenv("FITBIT_ACCESS_TOKEN")) {
  GET(
    url,
    add_headers(
      .headers =  c(
        Authorization = paste0("Bearer ", token)
      )
    )
  )
}

#' @importFrom httr POST
post <- function(url, body, token = Sys.getenv("FITBIT_ACCESS_TOKEN")) {
  POST(
    url,
    body = body,
    add_headers(
      .headers =  c(
        Authorization = paste0("Bearer ", token)
      )
    )
  )
}

#' @importFrom httr DELETE
delete <- function(url, token) {
  DELETE(
    url,
    add_headers(
      .headers =  c(
        Authorization = paste0("Bearer ", token)
      )
    )
  )
}
