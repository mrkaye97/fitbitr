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
