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
