date <- start_date <- '2021-05-20'
end_date <- '2021-05-21'

test_that("Sleep works", {

  skip_on_ci()
  skip_on_cran()

  sleep_summary(
    start_date = start_date,
    end_date = end_date
  )
})
