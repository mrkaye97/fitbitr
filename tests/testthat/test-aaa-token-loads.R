test_that('Token loads', {

  skip_on_cran()
  skip_on_ci()

  load_cached_token('../../.httr-oauth')
  tmp <- check_token_exists()

  expect_true(tmp)
})
