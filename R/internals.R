#' @noRd
#' @return TRUE (invisibly) on success
check_token_exists <- function() {
  if (is.null(.fitbitr_token)) {
    stop("No Fitbit API token found. You can generate one with `generate_token()` or load one from a cache with `load_cached_token()`.")
  } else if (!.fitbitr_token$can_refresh()) {
    stop("Your Fitbit API token cannot be refreshed. Please generate a new one with `generate_token()`")
  } else {
    invisible(TRUE)
  }
}
