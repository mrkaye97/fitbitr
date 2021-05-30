test_that('Token loads', {

  skip_on_cran()
  skip_on_ci()

  cache_loc <- Sys.getenv("FITBITR_CACHE_LOC")

  load_cached_token(cache_loc)
  tmp <- check_token_exists()

  expect_true(tmp)
})
