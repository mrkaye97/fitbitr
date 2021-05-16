#' @noRd
#' @param key the env var to set
#' @param value the value of the env var
#' @return TRUE invisibly on success
set_env_var <- function(key, value) {
  args = list(value)
  names(args) = key
  do.call(Sys.setenv, args)

  return(
    invisible(TRUE)
  )
}

#' Persist key and secret in .Reviron
#' Simplify the setup process by persisting your Fitbit key and secret in the `.fitbitr-oauth` file.
#'
#' @param key your Fitbit client ID
#' @param secret your Fitbit client secret
#' @param callback your Fitbit redirect URL
#' @param path the path to your `.fitbitr-oauth` file
#' @param scope the scopes to enable
#' @param overwrite do you want to overwrite your existing environment variables? not recommended, as this is destructive.
#' @return TRUE (invisibly) on success
#' @export
initial_setup <- function(
  key,
  secret,
  callback,
  path = '~/.fitbitr-oauth',
  scope = c("sleep", "activity", "heartrate", "location", "nutrition", "profile", "settings", "social", "weight"),
  overwrite = FALSE
) {

  FITBIT_VARS <- c("FITBIT_KEY", "FITBIT_SECRET", "FITBIT_CALLBACK", "FITBIT_ACCESS_TOKEN", "FITBIT_REFRESH_TOKEN")

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

  env <- readLines(path)

  creds_exist <- env %>%
    lapply(
      function(x) check_if_var_exists(FITBIT_VARS, x)
    ) %>%
    unlist() %>%
    any()

  if (creds_exist & (!overwrite)) stop(paste("Fitbit credentials already found in", path, ". If you wish to overwrite them, please rerun one_time_setup() with overwrite=TRUE."))

  scope <- c("sleep", "activity", "heartrate", "location", "nutrition", "profile", "settings", "social", "weight")
  endpoint <- create_endpoint()
  myapp <- httr::oauth_app("r-package-test", key = key, secret = secret, redirect_uri = callback)

  token <- httr::oauth2.0_token(
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
    FITBIT_KEY = key,
    FITBIT_SECRET = secret,
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
        paste0(names(.), '=', .)
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

#' Get token
refresh_token <- function(
  key = Sys.getenv("FITBIT_KEY"),
  secret = Sys.getenv("FITBIT_SECRET"),
  refresh_token = Sys.getenv("FITBIT_REFRESH_TOKEN")
) {

  r <- POST(
    'https://api.fitbit.com/oauth2/token',
    add_headers(
      .headers =  c(
        Authorization = paste0("Basic ", RCurl::base64Encode(charToRaw(paste0(key, ":", secret)))))
    ),
    httr::content_type("application/x-www-form-urlencoded"),
    body = list(
      expires_in = 86400,   ## expires in one day
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


#' Set Access and Refresh Token as Env Vars
#'
fitbitr_setup <- function(access_token = NULL, refresh_token = NULL, user_id = NULL, config_file = '~/.fitbitr-oauth') {
  if (!is.null(access_token) & (!is.null(refresh_token)) & (!is.null(user_id))) {
    Sys.setenv(
      FITBIT_ACCESS_TOKEN = access_token,
      FITBIT_REFRESH_TOKEN = refresh_token,
      FITBIT_USER_ID = user_id
    )
  } else if (file.exists(config_file)) {
    config <- read.dcf(config_file)

    Sys.setenv(
      FITBIT_ACCESS_TOKEN = config[, 'FITBIT_ACCESS_TOKEN'],
      FITBIT_REFRESH_TOKEN = config[, 'FITBIT_REFRESH_TOKEN'],
      FITBIT_USER_ID = config[, 'FITBIT_USER_ID']
    )
  } else {
    stop("You must either provide an access token, refresh token, and user id or a config file (in .dcf format) that contains them.")
  }
}
