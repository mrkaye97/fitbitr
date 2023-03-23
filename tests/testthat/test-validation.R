test_that("Date validation works", {
  skip_on_cran()
  skip_on_ci()

  expect_error(
    get_steps("foobar"),
    "Date validation failed"
  )

  expect_error(
    validate_date("foo"),
    "Date validation failed"
  )

  expect_true(validate_date("2021-02-01"))
  expect_true(validate_date("1/2/22"))
  expect_true(validate_date("1/2/2022"))
  expect_true(validate_date(lubridate::today()))
})
