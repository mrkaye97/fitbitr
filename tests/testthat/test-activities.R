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


  tmp <- calories_bmr(start_date, end_date)

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


  tmp <- distance(start_date, end_date)

  expect_equal(nrow(tmp), 7)
  expect_equal(ncol(tmp), 2)
  expect_equal(colnames(tmp), c("date", "distance"))
  checkmate::expect_date(tmp$date)
})

test_that("Elevation downloads", {
  skip_on_cran()


  tmp <- elevation(start_date, end_date)

  expect_equal(nrow(tmp), 7)
  expect_equal(ncol(tmp), 2)
  expect_equal(colnames(tmp), c("date", "elevation"))
  checkmate::expect_date(tmp$date)
})

test_that("Floors downloads", {
  skip_on_cran()


  tmp <- floors(start_date, end_date)

  expect_equal(nrow(tmp), 7)
  expect_equal(ncol(tmp), 2)
  expect_equal(colnames(tmp), c("date", "floors"))
  checkmate::expect_date(tmp$date)
})

test_that("Minutes downloads", {
  skip_on_cran()


  sedentary <- minutes_sedentary(start_date, end_date)
  lightly <- minutes_lightly_active(start_date, end_date)
  fairly <- minutes_fairly_active(start_date, end_date)
  very <- minutes_very_active(start_date, end_date)

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

