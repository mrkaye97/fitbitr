start_date <- date <- "2021-05-21"
end_date <- "2021-05-22"

test_that("Activites summary downloads", {
  skip_on_cran()

  tmp <- get_activity_summary(date)

  expect_equal(nrow(tmp), 1)
  expect_equal(ncol(tmp), 12)
  expect_equal(colnames(tmp)[1], "date")
  checkmate::expect_date(tmp$date)
})

test_that("Activity calories downloads", {
  skip_on_cran()

  tmp <- get_activity_calories(start_date, end_date)

  expect_equal(nrow(tmp), 7)
  expect_equal(ncol(tmp), 2)
  expect_equal(colnames(tmp), c("date", "activity_calories"))
  checkmate::expect_date(tmp$date)
})

test_that("BMR calories downloads", {
  skip_on_cran()


  tmp <- get_calories_bmr(start_date, end_date)

  expect_equal(nrow(tmp), 7)
  expect_equal(ncol(tmp), 2)
  expect_equal(colnames(tmp), c("date", "calories_bmr"))
  checkmate::expect_date(tmp$date)
})

test_that("Total calories downloads", {
  skip_on_cran()


  tmp <- get_calories(start_date, end_date)

  expect_equal(nrow(tmp), 7)
  expect_equal(ncol(tmp), 2)
  expect_equal(colnames(tmp), c("date", "calories"))
  checkmate::expect_date(tmp$date)
})

test_that("Distance downloads", {
  skip_on_cran()


  tmp <- get_distance(start_date, end_date)

  expect_equal(nrow(tmp), 7)
  expect_equal(ncol(tmp), 2)
  expect_equal(colnames(tmp), c("date", "distance"))
  checkmate::expect_date(tmp$date)
})

test_that("Elevation downloads", {
  skip_on_cran()


  tmp <- get_elevation(start_date, end_date)

  expect_equal(nrow(tmp), 7)
  expect_equal(ncol(tmp), 2)
  expect_equal(colnames(tmp), c("date", "elevation"))
  checkmate::expect_date(tmp$date)
})

test_that("Floors downloads", {
  skip_on_cran()


  tmp <- get_floors(start_date, end_date)

  expect_equal(nrow(tmp), 7)
  expect_equal(ncol(tmp), 2)
  expect_equal(colnames(tmp), c("date", "floors"))
  checkmate::expect_date(tmp$date)
})

test_that("Minutes downloads", {
  skip_on_cran()


  sedentary <- get_minutes_sedentary(start_date, end_date)
  lightly <- get_minutes_lightly_active(start_date, end_date)
  fairly <- get_minutes_fairly_active(start_date, end_date)
  very <- get_minutes_very_active(start_date, end_date)

  all_data <- list(
    sedentary,
    lightly,
    fairly,
    very
  )

  all_data %>%
    purrr::map(
      ~ expect_equal(nrow(.x), 7)
    )

  all_data %>%
    purrr::map(
      ~ expect_equal(ncol(.x), 2)
    )

  all_data %>%
    purrr::map(
      ~ checkmate::expect_date(.x$date)
    )
})

test_that("Steps downloads", {
  skip_on_cran()

  steps <- get_steps(start_date, end_date)

  expect_equal(
    nrow(steps),
    7
  )

  expect_equal(
    min(steps$date),
    as.Date("2011-04-27")
  )

  expect_equal(
    max(steps$date),
    as.Date("2011-05-03")
  )

  expect_equal(
    max(steps$steps),
    15828
  )

  expect_identical(
    colnames(steps),
    c("date", "steps")
  )
})

test_that("Activity TS schemas match", {
  skip_on_cran()

  steps <- get_steps(start_date, end_date)

  purrr::iwalk(
    list(
      floors = get_floors,
      calories_bmr = get_calories_bmr,
      distance = get_distance,
      elevation = get_elevation,
      minutes_sedentary = get_minutes_sedentary,
      minutes_lightly_active = get_minutes_lightly_active,
      minutes_fairly_active = get_minutes_fairly_active,
      minutes_very_active = get_minutes_very_active,
      activity_calories = get_activity_calories
    ),
    ~ {
      series <- .x(start_date, end_date)

      expect_identical(
        steps$date,
        series$date
      )

      expect_identical(
        steps$steps,
        series[[2]]
      )

      expect_equal(
        ncol(steps),
        ncol(series)
      )
    }
  )
})
