test_that("Heart rate by minute works", {
  skip_on_cran()
  skip_on_ci()

  heart <- get_heart_rate_intraday(
    date = date
  )

  expect_equal(colnames(heart), c("time", "heart_rate"))
  expect_gt(nrow(heart), 1000)
  checkmate::expect_posixct(heart$time)

  heart <- get_heart_rate_intraday(
    date = date,
    detail_level = "1min",
    start_time = "00:00:00",
    end_time = "00:02:00"
  )

  expect_equal(colnames(heart), c("time", "heart_rate"))
  expect_equal(nrow(heart), 3)
  checkmate::expect_posixct(heart$time)
})

test_that("Intradays work", {
  skip_on_cran()
  skip_on_ci()

  test_one_min_gran <- function(f, col) {
    x <- f(date, detail_level = "1min")

    expect_equal(colnames(x), c("time", col))
    expect_gt(nrow(x), 1000)
    checkmate::expect_posixct(x$time)
    expect_true(x$time[1] == x$time[2] - lubridate::minutes(1))
  }

  test_fifteen_min_gran <- function(f, col) {
    x <- f(date, detail_level = "15min")

    expect_equal(colnames(x), c("time", col))
    expect_gt(nrow(x), 10)
    checkmate::expect_posixct(x$time)
    expect_true(x$time[1] == x$time[2] - lubridate::minutes(15))
  }

  resources <- list(
    calories = get_calories_intraday,
    distance = get_distance_intraday,
    elevation = get_elevation_intraday,
    floors = get_floors_intraday,
    heart_rate = get_heart_rate_intraday,
    steps = get_steps_intraday
  )

  purrr::iwalk(
    resources,
    test_one_min_gran
  )

  purrr::iwalk(
    resources,
    test_fifteen_min_gran
  )
})
