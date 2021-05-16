#' @noRd
#' @param token the Fitbit Access Token
#' @param user_id the user id
#' @return TRUE (invisibly) on success
check_config_exists <- function(token, user_id) {
  if (token == '' & user_id == '') {
    stop("No token or user id provided. Did you forget to call fitbitr_setup()?")
  } else if (token == '') {
    stop("No token provided. Did you forget to call fitbitr_setup()?")
  } else if (user_id == '') {
    stop("No user id provided. Did you forget to call fitbitr_setup()?")
  } else {
    invisible(TRUE)
  }
}

#' @noRd
#' @param r the API response
#' @return r (the response) on success, and otherwise errors out with an informative message
check_response <- function(r) {
  if (r$status_code == 200) {
    return(r)
  } else if (r$status_code == 401 && grepl('expired', content(r)$errors[[1]]$message)) {
    stop('Your Fitbit Access Token has expired. Refresh it with refresh_token()')
  } else {
    msg <- content(r)$errors[[1]]$message
    stop(msg)
  }
}
