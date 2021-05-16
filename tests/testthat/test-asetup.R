test_that('fitbitr_setup() sets env variables from config file', {
  skip_on_cran()
  skip_on_ci()

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

test_that('fitbitr_teardown() tears down env successfully', {

  skip_on_cran()

  env <- c(
    access_token = Sys.getenv("FITBIT_ACCESS_TOKEN"),
    refresh_token = Sys.getenv("FITBIT_REFRESH_TOKEN"),
    client_id = Sys.getenv("FITBIT_CLIENT_ID"),
    client_secret = Sys.getenv("FITBIT_CLIENT_SECRET"),
    user_id = Sys.getenv("FITBIT_USER_ID")
  )

  fitbitr_teardown()

  conditions <- c(
    Sys.getenv("FITBIT_ACCESS_TOKEN") == '',
    Sys.getenv("FITBIT_CLIENT_ID") == '',
    Sys.getenv("FITBIT_CLIENT_SECRET") == '',
    Sys.getenv("FITBIT_REFRESH_TOKEN") == '',
    Sys.getenv("FITBIT_USER_ID") == ''
  )

  expect_true(
    all(conditions)
  )

  fitbitr_setup(
    access_token = env['access_token'],
    refresh_token = env['refresh_token'],
    client_id = env['client_id'],
    client_secret = env['client_secret'],
    user_id = env['user_id']
  )
})

test_that("fitbitr_setup() works with explicit params", {

  skip_on_cran()
  skip_on_ci()

  env <- c(
    access_token = Sys.getenv("FITBIT_ACCESS_TOKEN"),
    refresh_token = Sys.getenv("FITBIT_REFRESH_TOKEN"),
    client_id = Sys.getenv("FITBIT_CLIENT_ID"),
    client_secret = Sys.getenv("FITBIT_CLIENT_SECRET"),
    user_id = Sys.getenv("FITBIT_USER_ID")
  )

  fitbitr_teardown()
  fitbitr_setup(
    access_token = env['access_token'],
    refresh_token = env['refresh_token'],
    client_id = env['client_id'],
    client_secret = env['client_secret'],
    user_id = env['user_id']
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
