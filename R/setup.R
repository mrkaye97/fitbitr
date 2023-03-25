#' Generate an Oauth token
#'
#' Performs the OAuth 2.0 dance to create a token to use with the Fitbit API.
#'
#' Saves a token as `.fitbitr_token` which can then be used in the
#' background to authorize requests
#'
#' @importFrom httr oauth_app oauth2.0_token
#'
#' @param app_name The name of your OAuth app. Default: `fitbitr`
#' @param client_id Your Fitbit client ID
#' @param client_secret Your Fitbit client secret
#' @param callback Your Fitbit redirect URL
#' @param scope The scopes to enable
#' @param cache Do you want to cache your token? See \link[httr]{oauth2.0_token} for details
#' @param use_basic_auth A boolean for whether or not to use basic auth in \link[httr]{oauth2.0_token}. Defaults to `TRUE`
#' @param ... additional arguments to be passed to \link[httr]{oauth2.0_token}
#'
#' @examples
#' \dontrun{
#' generate_fitbitr_token(
#'   client_id = <YOUR-CLIENT-ID>
#'   client_secret = <YOUR-CLIENT-SECRET>,
#'   cache = TRUE
#' )
#' }
#'
#' @return Returns an OAuth 2.0 token (invisibly) that can be used to authorize requests to the Fitbit API. Also saves the token to `.fitbitr_token`.
#' @export
generate_fitbitr_token <- function(
  app_name = "fitbitr",
  client_id = Sys.getenv("FITBIT_CLIENT_ID"),
  client_secret = Sys.getenv("FITBIT_CLIENT_SECRET"),
  callback = Sys.getenv("FITBIT_CALLBACK", "https://localhost:1410/"),
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
  cache = TRUE,
  use_basic_auth = TRUE,
  ...
) {

  endpoint <- create_endpoint()
  myapp <- oauth_app(
    appname = app_name,
    key = client_id,
    secret = client_secret,
    redirect_uri = callback
  )

  .fitbitr_token <<- oauth2.0_token(
    endpoint,
    myapp,
    scope = scope,
    use_basic_auth = use_basic_auth,
    cache = cache,
    ...
  )

  invisible(.fitbitr_token)
}

create_endpoint <- function() {
  request <- "https://api.fitbit.com/oauth2/token"
  authorize <- "https://www.fitbit.com/oauth2/authorize"
  access <- "https://api.fitbit.com/oauth2/token"
  httr::oauth_endpoint(request, authorize, access)
}
