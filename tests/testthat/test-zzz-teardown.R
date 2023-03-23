test_that("Torn down", {
  skip_on_cran()
  skip_on_ci()

  unlink(".httr-oauth")
  expect_false(file.exists(".httr-oauth"))
})

