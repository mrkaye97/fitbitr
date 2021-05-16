#' @noRd
#' @param client_id the env var to set
#' @param value the value of the env var
#' @return TRUE invisibly on success
set_env_var <- function(client_id, value) {
  args = list(value)
  names(args) = client_id
  do.call(Sys.setenv, args)

  return(
    invisible(TRUE)
  )
}

#' Persist necessary credentials
#'
#' Simplify the setup process by persisting your Fitbit client_id and secret in the `.fitbitr-oauth` file.
#'
#' @importFrom httr oauth_app oauth2.0_token
#' @param client_id your Fitbit client ID
#' @param client_secret your Fitbit client secret
#' @param callback your Fitbit redirect URL
#' @param path the path to your `.fitbitr-oauth` file
#' @param scope the scopes to enable
#' @param overwrite do you want to overwrite your existing environment variables? not recommended, as this is destructive.
#' @return TRUE (invisibly) on success
#' @export
initial_setup <- function(
  client_id,
  client_secret,
  callback,
  path = '~/.fitbitr-oauth',
  scope = c("sleep", "activity", "heartrate", "location", "nutrition", "profile", "settings", "social", "weight"),
  overwrite = FALSE
) {

  FITBIT_VARS <- c("FITBIT_CLIENT_ID", "FITBIT_CLIENT_SECRET", "FITBIT_CALLBACK", "FITBIT_ACCESS_TOKEN", "FITBIT_REFRESH_TOKEN", "FITBIT_USER_ID")

  check_if_var_exists <- function(all_vars, x) {
    any(
      unlist(
        lapply(
          all_vars,
          function(v) grepl(v, x)
        )
      )
    )
  }

  if (!file.exists(path)) file.create(path)
  env <- readLines(path)

  creds_exist <- env %>%
    lapply(
      function(x) check_if_var_exists(FITBIT_VARS, x)
    ) %>%
    unlist() %>%
    any()

  if (creds_exist & (!overwrite)) stop(paste("Fitbit credentials already found in", path, ". If you wish to overwrite them, please rerun one_time_setup() with overwrite=TRUE."))

  endpoint <- create_endpoint()
  myapp <- oauth_app("r-package-test", key = client_id, secret = client_secret, redirect_uri = callback)

  token <- oauth2.0_token(
    endpoint,
    myapp,
    scope = scope,
    use_basic_auth = TRUE,
    use_oob = TRUE,
    oob_value = callback,
    query_authorize_extra = list(prompt = "none"),
    cache = FALSE
  )

  update_fitbit_config(
    FITBIT_CLIENT_ID = client_id,
    FITBIT_CLIENT_SECRET = client_secret,
    FITBIT_CALLBACK = callback,
    FITBIT_ACCESS_TOKEN = token$credentials$access_token,
    FITBIT_REFRESH_TOKEN = token$credentials$refresh_token,
    FITBIT_USER_ID = token$credentials$user_id,
    path = path
  )

  return(
    invisible(TRUE)
  )
}

#' @importFrom purrr discard map flatten
#' @noRd
#' @param ... named args to add to the env file
#' @param path the path to the env file (defaults to `~/.fitbitr-oauth`)
#' @return TRUE (invisibly) on success
update_fitbit_config <- function(..., path = '~/.fitbitr-oauth') {
  args <- list(...)

  env <- readLines(path) %>%
    purrr::discard(
      ~ .x == ''
    ) %>%
    map(
      strsplit, "="
    ) %>%
    flatten() %>%
    purrr::discard(
      ~ .x[1] %in% names(args)
    ) %>%
    map(
      ~ paste0(.x[1], '=', .x[2])
    ) %>%
    unlist() %>%
    c(
      args %>%
        paste0(names(.), ': ', .)
    )

  do.call(Sys.setenv, args)

  writeLines(
    env,
    path,
    sep = '\n'
  )

  message(
    paste('New environment variables have been added to', path, '.')
  )

  return(
    invisible(TRUE)
  )
}

#' Refresh your Fitbit Access Token
#'
#' @param refresh_token the Fitbit refresh token
#' @param client_id the Fitbit Client ID
#' @param client_secret the Fitbit Client Secret
#' @param expires_in How long do you want your token to remain valid? (default: 86400 sec = 1 day)
#' @return TRUE (invisibly) on success
#' @export
refresh_token <- function(
  refresh_token = Sys.getenv("FITBIT_REFRESH_TOKEN"),
  client_id = Sys.getenv("FITBIT_CLIENT_ID"),
  client_secret = Sys.getenv("FITBIT_CLIENT_SECRET"),
  expires_in = 86400
) {

  r <- POST(
    'https://api.fitbit.com/oauth2/token',
    add_headers(
      .headers =  c(
        Authorization = paste0("Basic ", RCurl::base64Encode(charToRaw(paste0(client_id, ":", client_secret)))))
    ),
    httr::content_type("application/x-www-form-urlencoded"),
    body = list(
      expires_in = expires_in,   ## expires in one day
      grant_type = 'refresh_token',
      refresh_token = refresh_token
    ),
    encode = 'form'
  )

  token <- content(r)

  update_fitbit_config(
    access_token = token$access_token,
    refresh_token = token$refresh_token
  )

  return(
    invisible(TRUE)
  )
}

create_endpoint <- function() {
  request <- "https://api.fitbit.com/oauth2/token"
  authorize <- "https://www.fitbit.com/oauth2/authorize"
  access <- "https://api.fitbit.com/oauth2/token"
  httr::oauth_endpoint(request, authorize, access)
}


#' Set necessary env vars
#'
#' Make calling the Fitbit API easier by setting env vars at setup time.
#' This function sets the config that's necessary for making API requests,
#' including the access token, refresh token, user id, client id, and client secret.
#' Then, functions making API calls will use `Sys.getenv()` to access the config.
#'
#' @param config_file the path to your config file (default: `~/.fitbitr-oauth`)
#' @param access_token the Fitbit access token (default: NULL)
#' @param refresh_token the Fitbit refresh token (default: NULL)
#' @param user_id the Fitbit user_id (default: NULL)
#' @param client_id the Fitbit Client Id (default: NULL)
#' @param client_secret the Fitbit Client Secret (default: NULL)
#' @return TRUE (invisibly) on success
#' @export
fitbitr_setup <- function(config_file = '~/.fitbitr-oauth', access_token = NULL, refresh_token = NULL, user_id = NULL, client_id = NULL, client_secret = NULL) {
  if (!is.null(access_token) & (!is.null(refresh_token)) & (!is.null(user_id)) & (!is.null(client_id)) & (!is.null(client_secret))) {
    Sys.setenv(
      FITBIT_ACCESS_TOKEN = access_token,
      FITBIT_REFRESH_TOKEN = refresh_token,
      FITBIT_USER_ID = user_id,
      FITBIT_CLIENT_ID = client_id,
      FITBIT_CLIENT_SECRET = client_secret
    )
  } else if (file.exists(config_file)) {
    config <- read.dcf(config_file)

    Sys.setenv(
      FITBIT_ACCESS_TOKEN = config[, 'FITBIT_ACCESS_TOKEN'],
      FITBIT_REFRESH_TOKEN = config[, 'FITBIT_REFRESH_TOKEN'],
      FITBIT_USER_ID = config[, 'FITBIT_USER_ID'],
      FITBIT_CLIENT_ID = config[, 'FITBIT_CLIENT_ID'],
      FITBIT_CLIENT_SECRET = config[, 'FITBIT_CLIENT_SECRET']
    )
  } else {
    stop("You must either provide an access token, refresh token, user id, client id, and client secret or a config file (in .dcf format) that contains them.")
  }

  return(
    invisible(
      TRUE
    )
  )
}
