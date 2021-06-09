#' @noRd
#' @return TRUE (invisibly) on success
#' @importFrom rlang abort
check_token_exists <- function() {
  if (Sys.getenv("FITBITR_ENVIRONMENT") == "testing mode") {
    invisible(TRUE)
  } else if (is.null(.fitbitr_token)) {
    abort("No Fitbit API token found. You can generate one with `generate_token()` or load one from a cache with `load_cached_token()`.")
  } else if (!.fitbitr_token$can_refresh()) {
    abort("Your Fitbit API token cannot be refreshed. Please generate a new one with `generate_token()`")
  } else {
    invisible(TRUE)
  }
}
