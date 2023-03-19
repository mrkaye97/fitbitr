date <- start_date <- "2021-05-20"
end_date <- "2021-05-21"

test_that("Heart rate by minute works", {
  skip_on_cran()

  tmp <- get_heart_rate_intraday(
    date = date,
    minutes = TRUE
  )

  expect_equal(colnames(tmp), c("time", "heart_rate"))
  expect_equal(nrow(tmp), 5)
  checkmate::expect_posixct(tmp$time)
})

test_that("Heart rate by second works", {
  skip_on_cran()

  tmp <- get_heart_rate_intraday(
    date = date,
    minutes = FALSE
  )

  expect_equal(colnames(tmp), c("time", "heart_rate"))
  expect_equal(nrow(tmp), 5)
  checkmate::expect_posixct(tmp$time)
})

test_that("Heart rate zones works", {
  skip_on_cran()

  tmp <- get_heart_rate_zones(date)

  expect_equal(nrow(tmp), 4)
  expect_equal(colnames(tmp), c("date", "zone", "min_hr", "max_hr", "minutes_in_zone", "calories_out"))
  checkmate::expect_date(tmp$date)
})

