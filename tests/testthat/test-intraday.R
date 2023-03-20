date <- "2021-05-20"

test_that("Heart rate by minute works", {
  skip_on_cran()

  heart <- get_heart_rate_intraday(
    date = date
  )

  expect_equal(colnames(heart), c("time", "heart_rate"))
  expect_equal(nrow(heart), 5)
  checkmate::expect_posixct(heart$time)
})

test_that("Heart rate by second works", {
  skip_on_cran()

  tmp <- get_heart_rate_intraday(
    date = date,
    detail_level = "1min"
  )

  expect_equal(colnames(tmp), c("time", "heart_rate"))
  expect_equal(nrow(tmp), 5)
  checkmate::expect_posixct(tmp$time)
})

test_that("Intradays work", {
  skip_on_cran()
  skip_on_ci()

  load_cached_token(Sys.getenv("FITBITR_CACHE_LOC"))

  one_min_granularity <- get_heart_rate_intraday(
    date = date,
    detail_level = "1min"
  )
  fifteen_min_granularity <- get_heart_rate_intraday(
    date = date,
    detail_level = "15min"
  )

  expect_equal(colnames(one_min_granularity), c("time", "heart_rate"))
  expect_equal(nrow(one_min_granularity), 1348)
  checkmate::expect_posixct(one_min_granularity$time)

  expect_gt(
    nrow(one_min_granularity),
    nrow(fifteen_min_granularity)
  )

  mean_hr_granular <- mean(
    dplyr::filter(
      one_min_granularity,
      dplyr::between(
        time,
        fifteen_min_granularity$time[1],
        fifteen_min_granularity$time[2]
      )
    )$heart_rate
  )

  expect_lte(
    abs(mean_hr_granular - fifteen_min_granularity$heart_rate[1]),
    2
  )

  expect_gt(nrow(get_steps_intraday(date, detail_level = "1min")), 1000)
  expect_gt(nrow(get_floors_intraday(date, detail_level = "1min")), 1000)
  expect_gt(nrow(get_elevation_intraday(date, detail_level = "1min")), 1000)
  expect_gt(nrow(get_calories_intraday(date, detail_level = "1min")), 1000)
  expect_gt(nrow(get_distance_intraday(date, detail_level = "1min")), 1000)
})
