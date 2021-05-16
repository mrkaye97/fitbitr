test_that('fitbitr_setup() sets env variables from config file', {
  skip_on_cran()
  fitbitr_setup()

  conditions <- c(
    Sys.getenv("FITBIT_ACCESS_TOKEN") == '',
    Sys.getenv("FITBIT_CLIENT_ID") == '',
    Sys.getenv("FITBIT_CLIENT_SECRET") == '',
    Sys.getenv("FITBIT_REFRESH_TOKEN") == '',
    Sys.getenv("FITBIT_USER_ID") == ''
  )
  expect_false(
    any(conditions)
  )
})

test_that('fitbitr_setup() sets env variables from explicit parameters', {
  fitbitr_setup(
    access_token = Sys.getenv("FITBIT_ACCESS_TOKEN"),
    refresh_token = Sys.getenv("FITBIT_REFRESH_TOKEN"),
    client_id = Sys.getenv("FITBIT_CLIENT_ID"),
    client_secret = Sys.getenv("FITBIT_CLIENT_SECRET"),
    user_id = Sys.getenv("FITBIT_USER_ID")
  )

  conditions <- c(
    Sys.getenv("FITBIT_ACCESS_TOKEN") == '',
    Sys.getenv("FITBIT_CLIENT_ID") == '',
    Sys.getenv("FITBIT_CLIENT_SECRET") == '',
    Sys.getenv("FITBIT_REFRESH_TOKEN") == '',
    Sys.getenv("FITBIT_USER_ID") == ''
  )
  expect_false(
    any(conditions)
  )
})
