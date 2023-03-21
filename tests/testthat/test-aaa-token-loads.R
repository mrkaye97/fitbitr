test_that("Token loads", {
  skip_on_cran()
  skip_on_ci()

  cache_loc <- Sys.getenv("FITBITR_CACHE_LOC")

  token <- load_cached_token(cache_loc)

  expect_s3_class(
    token,
    "Token2.0"
  )
})

test_that("Testing mode warns", {
  expect_warning(
    get_steps(token, '2020-01-01', '2020-01-03'),
    "Heads up: You're in"
  )
})
