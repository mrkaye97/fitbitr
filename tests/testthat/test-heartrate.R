test_that("Heart rate zones works", {
  skip_on_cran()
  skip_on_ci()

  tmp <- get_heart_rate_zones(date)

  expect_equal(nrow(tmp), 4)
  expect_equal(colnames(tmp), c("date", "zone", "min_hr", "max_hr", "minutes_in_zone", "calories_out"))
  checkmate::expect_date(tmp$date)
})

