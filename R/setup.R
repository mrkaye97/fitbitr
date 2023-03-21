#' Create an object of class `fitbitr_token`
#'
#' This type of object can be used in place of a genuine OAuth 2.0
#' token for authorizing requests to the API in non-interactive sessions.
#'
#' The class itself is quite minimal: It's just a list with two
#' elements: the access token (used for authorizing requests), and a refresh token.
#'
#' @param refresh_token A refresh token. Defaults to the value of the `FITBIT_REFRESH_TOKEN` env var.
#' @param access_token An access token. Default: `NULL`
#' @param user_id A Fitbit API client ID
#'
#' @return A `fitbitr_token` object. Contains a single element `credentials` with its own two elements: `access_token` and `refresh_token`
generate_fitbitr_token <- function(refresh_token = Sys.getenv("FITBIT_REFRESH_TOKEN"), access_token = NULL, user_id = Sys.getenv("FITBIT_CLIENT_ID")) {
  token <- list(
    credentials = list(
      user_id = user_id,
      access_token = access_token,
      refresh_token = refresh_token
    )
  )

  class(token) <- "fitbitr_token"

  token
}

#' Load a cached Fitbit API token from a cache
#'
#' @importFrom purrr keep
#'
#' @param path the path to the file where the token is stored. Default: `.httr-oauth`
#'
#' @examples
#' \dontrun{
#' load_cached_token()
#' }
#'
#' @return Returns an OAuth 2.0 token that can be used to authorize requests to the Fitbit API
#' @export
load_cached_token <- function(path = ".httr-oauth") {
  path %>%
    readRDS() %>%
    keep(
      ~ grepl("fitbit", .x$endpoint$request)
    ) %>%
    pluck(1)
}

#' Generate an Oauth token
#'
#' Performs the OAuth 2.0 dance to create a token to use with the Fitbit API.
#'
#' @importFrom httr  oauth2.0_token
#'
#' @param oauth_app_name The name of your OAuth app. Default: `fitbitr`
#' @param client_id Your Fitbit client ID
#' @param client_secret Your Fitbit client secret
#' @param callback Your Fitbit redirect URL
#' @param scope The scopes to enable
#' @param cache Do you want to cache your token? See \link[httr]{oauth2.0_token} for details
#' @param use_basic_auth A boolean for whether or not to use basic auth in \link[httr]{oauth2.0_token}. Defaults to `TRUE`
#' @param ... Additional arguments to be passed to \link[httr]{oauth2.0_token}
#'
#' @examples
#' \dontrun{
#' generate_oauth_token(
#'   client_id = <YOUR-CLIENT-ID>
#'   client_secret = <YOUR-CLIENT-SECRET>,
#'   cache = TRUE
#' )
#' }
#'
#' @return Returns an OAuth 2.0 token that can be used to authorize requests to the Fitbit API
#' @export
generate_oauth_token <- function(
  oauth_app_name = "fitbitr",
  client_id = Sys.getenv("FITBIT_CLIENT_ID"),
  client_secret = Sys.getenv("FITBIT_CLIENT_SECRET"),
  callback = Sys.getenv("FITBIT_CALLBACK", "http://localhost:1410/"),
  scope = c(
    "activity",
    "cardio_fitness",
    "electrocardiogram",
    "heartrate",
    "location",
    "nutrition",
    "oxygen_saturation",
    "profile",
    "respiratory_rate",
    "settings",
    "sleep",
    "social",
    "temperature",
    "weight"
  ),
  cache = FALSE,
  use_basic_auth = TRUE,
  ...
) {
  endpoint <- create_endpoint()
  myapp <- create_oauth_app(
    oauth_app_name = oauth_app_name,
    client_id = client_id,
    client_secret = client_secret,
    callback = callback
  )

  oauth2.0_token(
    endpoint,
    myapp,
    scope = scope,
    use_basic_auth = use_basic_auth,
    cache = cache,
    ...
  )
}

#' Refresh an existing Fitbit API token
#'
#' A token can be either of class `Token2.0` (from `httr`) or of class
#' `fitbitr_token` (from `fitbitr`).
#'
#' @param x A token to refresh
#' @param \dots Arguments passed to other methods
#'
#' @rdname refresh_api_token
#'
#' @export
refresh_api_token <- function(x, ...) {
  UseMethod("refresh_api_token", x)
}

#' Refresh api token
#'
#' @rdname refresh_api_token
#'
#' @param x A `fitbitr_token` to refresh
#' @param oauth_app_name The name of your OAuth app. Default: `fitbitr`
#' @param client_id Your Fitbit client ID
#' @param client_secret Your Fitbit client secret
#' @param callback Your Fitbit redirect URL
#' @param use_basic_auth A boolean for whether or not to use basic auth in \link[httr]{oauth2.0_token}. Defaults to `TRUE`
#'
#' @export
refresh_api_token.fitbitr_token <- function(
  x,
  oauth_app_name = "fitbitr",
  client_id = Sys.getenv("FITBIT_CLIENT_ID"),
  client_secret = Sys.getenv("FITBIT_CLIENT_SECRET"),
  callback = Sys.getenv("FITBIT_CALLBACK", "http://localhost:1410/"),
  use_basic_auth = TRUE,
  ...
) {
  endpoint <- create_endpoint()
  oauth_app <- create_oauth_app(
    oauth_app_name = oauth_app_name,
    client_id = client_id,
    client_secret = client_secret,
    callback = callback
  )

  token <- refresh_oauth2.0(
    endpoint = endpoint,
    app = oauth_app,
    credentials = list(
      refresh_token = x$credentials$refresh_token
    ),
    use_basic_auth = use_basic_auth
  )

  generate_fitbitr_token(
    access_token = token$access_token,
    refresh_token = token$refresh_token,
    user_id = client_id
  )
}

#' @importFrom httr Token2.0
#'
#' @rdname refresh_api_token
#'
#' @param x An `httr::Token2.0` token to refresh
#'
#' @export
refresh_api_token.Token2.0 <- function(x, ...) {
  x$refresh()
}

#' @rdname refresh_api_token
#' @export
refresh_fitbitr_token.default <- function(x, ...) {
  abort("`x` should be either an object of class `fitbitr_token` or `Token2.0`.")
}

#' Create an OAuth app to use for authorization
#'
#' @importFrom httr oauth_app
#'
#' @param oauth_app_name The name of your OAuth app. Default: `fitbitr`
#' @param client_id Your Fitbit client ID
#' @param client_secret Your Fitbit client secret
#' @param callback Your Fitbit redirect URL
create_oauth_app <- function(oauth_app_name, client_id, client_secret, callback) {
  oauth_app(
    appname = oauth_app_name,
    key = client_id,
    secret = client_secret,
    redirect_uri = callback
  )
}

create_endpoint <- function() {
  request <- "https://api.fitbit.com/oauth2/token"
  authorize <- "https://www.fitbit.com/oauth2/authorize"
  access <- "https://api.fitbit.com/oauth2/token"

  httr::oauth_endpoint(request, authorize, access)
}
