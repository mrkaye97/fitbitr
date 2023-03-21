test_that("Activites summary downloads", {
  skip_on_cran()

  tmp <- get_activity_summary(token, date)

  expect_equal(nrow(tmp), 1)
  expect_equal(ncol(tmp), 14)
  expect_equal(colnames(tmp)[1], "date")
  checkmate::expect_date(tmp$date)
})

test_that("Activity time series download correctly", {
  skip_on_cran()

  run_many <- function(f, token, start_date) {
    purrr::walk(
      round(runif(3, 1, 100)),
      ~ {
        response <- f(token, start_date, start_date + lubridate::days(.x))

        expect_equal(nrow(response), .x + 1)
        expect_equal(ncol(response), 2)
        expect_equal(colnames(response)[1], "date")
        checkmate::expect_date(response$date)
      }
    )
  }

  run_many(get_activity_calories, token, lubridate::as_date(start_date))
  run_many(get_calories_bmr, token, lubridate::as_date(start_date))
  run_many(get_calories, token, lubridate::as_date(start_date))
  run_many(get_distance, token, lubridate::as_date(start_date))
  run_many(get_elevation, token, lubridate::as_date(start_date))
  run_many(get_floors, token, lubridate::as_date(start_date))
  run_many(get_minutes_sedentary, token, lubridate::as_date(start_date))
  run_many(get_minutes_lightly_active, token, lubridate::as_date(start_date))
  run_many(get_minutes_fairly_active, token, lubridate::as_date(start_date))
  run_many(get_minutes_very_active, token, lubridate::as_date(start_date))
  run_many(get_steps, token, lubridate::as_date(start_date))
})


test_that("Bests and totals", {
  tracker_best <- get_bests_and_totals(token, TRUE, TRUE)
  tracker_total <- get_bests_and_totals(token, FALSE, TRUE)
  lifetime_best <- get_bests_and_totals(token, TRUE, FALSE)
  lifetime_total <- get_bests_and_totals(token, FALSE, FALSE)

  purrr::walk(
    names(tracker_best),
    ~ expect_true(.x %in% names(tracker_total))
  )

  purrr::walk(
    names(lifetime_best),
    ~ expect_true(.x %in% names(lifetime_total))
  )

  expect_identical(
    names(tracker_best),
    names(lifetime_best)
  )

  expect_identical(
    names(tracker_total),
    names(lifetime_total)
  )
})
