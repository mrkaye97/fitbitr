test_that("Sleep works", {
  skip_on_cran()
  skip_on_ci()

  tmp <- get_sleep_summary(
    start_date = start_date,
    end_date = end_date
  )

  expect_equal(
    colnames(tmp),
    c("log_id", "date", "start_time", "end_time", "duration", "efficiency", "minutes_to_fall_asleep", "minutes_asleep", "minutes_awake", "minutes_after_wakeup", "time_in_bed")
  )

  checkmate::expect_date(tmp$date)
})

test_that("Sleep stage granular works", {
  skip_on_cran()
  skip_on_ci()

  sleep <- get_sleep_stage_granular(
    start_date,
    end_date
  )

  expect_equal(max(lubridate::as_date(sleep$time)), end_date)
  expect_true(dplyr::between(min(lubridate::as_date(sleep$time)), start_date - lubridate::days(1), start_date))
  expect_equal(
    colnames(sleep),
    c("time", "level", "seconds")
  )
  expect_setequal(
    unique(sleep$level),
    c("rem", "deep", "light", "wake")
  )
})

test_that("Sleep stage summary works", {
  skip_on_cran()
  skip_on_ci()

  sleep <- get_sleep_stage_summary(
    start_date,
    end_date
  )

  expect_identical(
    colnames(sleep),
    c("date", "stage", "count", "minutes", "thirty_day_avg_minutes")
  )
  expect_setequal(
    sleep$stage,
    c("deep", "light", "rem", "wake")
  )
})
