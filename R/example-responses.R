response_class <- utils::getFromNamespace("response", "httr")

#' GET example response, given a URL
#'
#' @noRd
#' @param url the url to get an example response for
#' @param .example_identifier An internal identifier to choose which example to run
#' @importFrom lubridate today
get_example_response <- function(url, .example_identifier) {
  message("Heads up: You're in `fitbitr` testing mode. \n\nYou're seeing this message because you have the `FITBITR_ENVIRONMENT` env var set to 'testing mode'. \nIf you don't want to be in testing mode, please change the value of this environment variable.")

  response_class(
    url = 'test',
    status_code = 200,
    cookies = NULL,
    header = list(
      date = today(),
      `content-type` = "application/octet-stream",
      `content-length` = "1",
      `www-authenticate` = "local",
      `x-frame-options` = "SAMEORIGIN",
      via = "1.1 google",
      `cf-cache-status` = "DYNAMIC",
      `cf-request-id` = "xxxxxxxxxxxxxxxx",
      `expect-ct` = "max-age=604800, report-uri=\"testing",
      server = "local",
      `cf-ray` = "xxxxxxxx"
    ),
    content = switch(
      .example_identifier,
      "activity summary" = activity_summary_example_response,
      "activity time series" = activity_ts_example_response,
      "bests and totals" = bests_and_totals_example_response
    ) %>% charToRaw()
  )
}

#' POST example response, given a URL
#'
#' @noRd
#' @param url the url to get an example response for
#' @param body the post body
#' @param .example_identifier An internal identifier to choose which example to run
post_example_response <- function(url, body, .example_identifier) {
  message("Heads up: You're in `fitbitr` testing mode. \n\nYou're seeing this message because you have the `FITBITR_ENVIRONMENT` env var set to 'testing mode'. \nIf you don't want to be in testing mode, please change the value of this environment variable.")


  response_class(
    url = 'test',
    status_code = 200,
    cookies = NULL,
    header = list(
      date = today(),
      `content-type` = "application/octet-stream",
      `content-length` = "1",
      `www-authenticate` = "local",
      `x-frame-options` = "SAMEORIGIN",
      via = "1.1 google",
      `cf-cache-status` = "DYNAMIC",
      `cf-request-id` = "xxxxxxxxxxxxxxxx",
      `expect-ct` = "max-age=604800, report-uri=\"testing",
      server = "local",
      `cf-ray` = "xxxxxxxx"
    ),
    content = switch(
      .example_identifier,
      "activity summary" = activity_summary_example_response,
      "activity time series" = activity_ts_example_response
    )
  )
}


## Example responses
activity_summary_example_response <- '
{
  "activities": [
    {
      "activityId": 51007,
      "activityParentId": 90019,
      "calories": 230,
      "description": "7mph",
      "distance": 2.04,
      "duration": 1097053,
      "hasStartTime": true,
      "isFavorite": true,
      "logId": 1154701,
      "name": "Treadmill, 0% Incline",
      "startTime": "00:25",
      "steps": 3783
    }
  ],
  "goals": {
    "caloriesOut": 2826,
    "distance": 8.05,
    "floors": 150,
    "steps": 10000
  },
  "summary": {
    "activityCalories": 230,
    "caloriesBMR": 1913,
    "caloriesOut": 2143,
    "distances": [
      { "activity": "tracker", "distance": 1.32 },
      { "activity": "loggedActivities", "distance": 0 },
      { "activity": "total", "distance": 1.32 },
      { "activity": "veryActive", "distance": 0.51 },
      { "activity": "moderatelyActive", "distance": 0.51 },
      { "activity": "lightlyActive", "distance": 0.51 },
      { "activity": "sedentaryActive", "distance": 0.51 },
      { "activity": "Treadmill, 0% Incline", "distance": 3.28 }
    ],
    "elevation": 48.77,
    "fairlyActiveMinutes": 0,
    "floors": 16,
    "lightlyActiveMinutes": 0,
    "marginalCalories": 200,
    "sedentaryMinutes": 1166,
    "steps": 0,
    "veryActiveMinutes": 0
  }
}
'

activity_ts_example_response <- '
{
  "example": [
    { "dateTime": "2011-04-27", "value": 5490 },
    { "dateTime": "2011-04-28", "value": 2344 },
    { "dateTime": "2011-04-29", "value": 2779 },
    { "dateTime": "2011-04-30", "value": 9196 },
    { "dateTime": "2011-05-01", "value": 15828 },
    { "dateTime": "2011-05-02", "value": 1945 },
    { "dateTime": "2011-05-03", "value": 366 }
  ]
}
'

bests_and_totals_example_response <- '
{
  "best": {
    "total": {
      "distance": {
        "date": "2012-01-07",
        "value": 20.31597
      },
      "floors": {
        "date": "2012-01-29",
        "value": 14
      },
      "steps": {
        "date": "2012-01-07",
        "value": 26901
      }
    },
    "tracker": {
      "distance": {
        "date": "2012-01-07",
        "value": 20.31597
      },
      "floors": {
        "date": "2012-01-29",
        "value": 14
      },
      "steps": {
        "date": "2012-01-07",
        "value": 26901
      }
    }
  },
  "lifetime": {
    "total": {
      "distance": 2711.62,
      "floors": 2500,
      "steps": 203300
    },
    "tracker": {
      "distance": 2579.82,
      "floors": 2500,
      "steps": 106934
    }
  }
}
'
