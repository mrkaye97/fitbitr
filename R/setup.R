#' Persist key and secret in .Reviron
#' Simplify the setup process by persisting your Fitbit key and secret in the `.Renviron` file.
#'
#' @param key your Fitbit client ID
#' @param secret your Fitbit client secret
#' @param callback your Fitbit redirect URL
#' @param path the path to your `.Renviron` file
#' @param scope the scopes to enable
#' @param overwrite do you want to overwrite your existing environment variables? not recommended, as this is destructive.
#' @return TRUE (invisibly) on success
#' @export
one_time_setup <- function(
  key,
  secret,
  callback,
  path = '~/.Renviron',
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

  Sys.setenv(
    FITBIT_KEY = key,
    FITBIT_SECRET = secret,
    FITBIT_CALLBACK = callback,
    FITBIT_ACCESS_TOKEN = token$credentials$access_token,
    FITBIT_REFRESH_TOKEN = token$credentials$refresh_token,
    FITBIT_USER_ID = token$credentials$user_id
  )

  update_fitbit_config(
    list(
      FITBIT_KEY = key,
      FITBIT_SECRET = secret,
      FITBIT_CALLBACK = callback,
      FITBIT_ACCESS_TOKEN = token$credentials$access_token,
      FITBIT_REFRESH_TOKEN = token$credentials$refresh_token,
      FITBIT_USER_ID = token$credentials$user_id
    ),
    path = path
  )

  return(
    invisible(TRUE)
  )
}

#' @importFrom purrr discard map flatten
#' @noRd
#' @param ... named args to add to the env file
#' @param path the path to the env file (defaults to `~/.Renviron`)
#' @return TRUE (invisibly) on success
update_fitbit_config <- function(..., path = '~/.Renviron') {
  args <- as.list(...)

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

  update_fitbit_config(
    access_token = access_token,
    refresh_token = refresh_token
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

