#' Generate a Fitbit API token
#'
#' Simplify the setup process by persisting your Fitbit client_id and secret in the `.fitbitr-oauth` file.
#'
#' @importFrom httr oauth_app oauth2.0_token
#' @param client_id your Fitbit client ID
#' @param client_secret your Fitbit client secret
#' @param callback your Fitbit redirect URL
#' @param scope the scopes to enable
#' @param cache Do you want to cache your token? See \link[httr]{oauth2.0_token} for details
#' @param use_basic_auth A boolean for whether or not to use basic auth in \link[httr]{oauth2.0_token}. Defaults to `TRUE`
#' @param ... additional arguments to be passed to \link[httr]{oauth2.0_token}
#' @examples
#' \dontrun{
#' generate_token(
#'   client_id = <YOUR-CLIENT-ID>
#'   client_secret = <YOUR-CLIENT-SECRET>,
#'   cache = TRUE
#' )
#' }
#' @return No return value. This function generates a token and saves it (hidden) in the global environment to be used for the remainder of the R session. You can cache this token with `cache = TRUE` or explicitly setting a filepath to cache to. See \link[httr]{oauth2.0_token} for details.
#' @export
generate_token <- function(client_id,
                           client_secret,
                           callback = "http://localhost:1410/",
                           scope = c("sleep", "activity", "heartrate", "location", "nutrition", "profile", "settings", "social", "weight"),
                           cache = FALSE,
                           use_basic_auth = TRUE,
                           ...) {
  endpoint <- create_endpoint()
  myapp <- oauth_app("r-package-test", key = client_id, secret = client_secret, redirect_uri = callback)

  .fitbitr_token <<- oauth2.0_token(
    endpoint,
    myapp,
    scope = scope,
    use_basic_auth = use_basic_auth,
    cache = cache,
    ...
  )

  invisible()
}

#' Load a token from the cache
#'
#' @importFrom purrr keep
#' @param path the path to the file where the token is stored
#' @examples
#' \dontrun{
#' load_cached_token()
#' }
#' @return No return value. The token is stored in the global environment as a hidden variable named `.fitbitr_token`
#' @export
load_cached_token <- function(path = ".httr-oauth") {
  .fitbitr_token <<- path %>%
    readRDS() %>%
    keep(
      ~ grepl("fitbit", .x$endpoint$request)
    ) %>%
    pluck(1)

  invisible()
}

create_endpoint <- function() {
  request <- "https://api.fitbit.com/oauth2/token"
  authorize <- "https://www.fitbit.com/oauth2/authorize"
  access <- "https://api.fitbit.com/oauth2/token"
  httr::oauth_endpoint(request, authorize, access)
}
