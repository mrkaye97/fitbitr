test_that("Torn down", {
  unlink(".httr-oauth")
  expect_false(file.exists(".httr-oauth"))
})

