date <- start_date <- "2021-05-20"
end_date <- "2021-05-21"

test_that("Sleep works", {
  skip_on_cran()

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

  sleep <- get_sleep_stage_granular(
    start_date,
    end_date
  )

  expected <- jsonlite::fromJSON(sleep_stage_example)

  expect_identical(
    sleep$time,
    lubridate::as_datetime(expected$sleep$levels$data[[1]]$dateTime)
  )

  expect_identical(
    sleep$seconds,
    expected$sleep$levels$data[[1]]$seconds
  )

  expect_identical(
    sleep$level,
    expected$sleep$levels$data[[1]]$level
  )
})

test_that("Sleep stage summary works", {
  skip_on_cran()

  sleep <- get_sleep_stage_summary(
    start_date,
    end_date
  )

  expected <- jsonlite::fromJSON(sleep_stage_example)$sleep$levels$summary

  expect_identical(
    sleep$count,
    c(
      expected$deep$count,
      expected$light$count,
      expected$rem$count,
      expected$wake$count
    )
  )

  expect_identical(
    sleep$stage,
    c("deep", "light", "rem", "wake")
  )
})
