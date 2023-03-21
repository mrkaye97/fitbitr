#' Generate an Oauth token
#'
#' Performs the OAuth 2.0 dance to create a token to use with the Fitbit API.
#'
#' @importFrom httr2 oauth_flow_auth_code oauth_client
#'
#' @param app_name The name of your OAuth app. Default: `fitbitr`
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
generate_fitbitr_token <- function(
  app_name = "fitbitr",
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
  refresh_token = Sys.getenv("FITBIT_REFRESH_TOKEN"),
  cache = FALSE,
  use_basic_auth = TRUE,
  ...
) {

  callback_params <- httr2::url_parse(callback)
  client <- oauth_client(
    name = app_name,
    id = client_id,
    secret = client_secret,
    token_url = "https://api.fitbit.com/oauth2/token",
    auth = "header"
  )

  oauth_flow_auth_code(
    client = client,
    auth_url = "https://www.fitbit.com/oauth2/authorize",
    scope = paste(scope, collapse = " "),
    host_name = callback_params$hostname,
    port = as.integer(callback_params$port)
  )
}
