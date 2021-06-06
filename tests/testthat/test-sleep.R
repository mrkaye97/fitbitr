date <- start_date <- "2021-05-20"
end_date <- "2021-05-21"

test_that("Sleep works", {
  skip_on_cran()

  tmp <- sleep_summary(
    start_date = start_date,
    end_date = end_date
  )

  expect_equal(
    colnames(tmp),
    c("log_id", "date", "start_time", "end_time", "duration", "efficiency", "minutes_to_fall_asleep", "minutes_asleep", "minutes_awake", "minutes_after_wakeup", "time_in_bed")
  )

  checkmate::expect_date(tmp$date)
})
