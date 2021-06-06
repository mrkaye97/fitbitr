response_class <- utils::getFromNamespace("response", "httr")

#' GET example response, given a URL
#'
#' @noRd
#' @param url the url to get an example response for
#' @param .example_identifier An internal identifier to choose which example to run
#' @importFrom lubridate today
get_example_response <- function(url, .example_identifier) {
  warn(
    "Heads up: You're in `fitbitr` testing mode. \n\nYou're seeing this message because you have the `FITBITR_ENVIRONMENT` env var set to 'testing mode'. \nIf you don't want to be in testing mode, please change the value of this environment variable.",
    .frequency = "once",
    .frequency_id = "0b69edb2-1c01-4014-8954-1a65136647cf"
  )

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
      "bests and totals" = bests_and_totals_example_response,
      "hr zones" = hr_zones_example,
      "hr intraday" = hr_intraday_example,
      "sleep summary" = sleep_summary_example,
      "sleep stage" = sleep_stage_example
    ) %>% charToRaw()
  )
}

#' POST example response, given a URL
#'
#' @noRd
#' @importFrom rlang warn
#' @param url the url to get an example response for
#' @param body the post body
#' @param .example_identifier An internal identifier to choose which example to run
post_example_response <- function(url, body, .example_identifier) {
  warn(
    "Heads up: You're in `fitbitr` testing mode. \n\nYou're seeing this message because you have the `FITBITR_ENVIRONMENT` env var set to 'testing mode'. \nIf you don't want to be in testing mode, please change the value of this environment variable.",
    .frequency = "once",
    .frequency_id = "0b69edb2-1c01-4014-8954-1a65136647cf"
  )

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

hr_zones_example <- '
{
    "activities-heart": [
        {
            "dateTime": "2015-08-04",
            "value": {
                "customHeartRateZones": [],
                "heartRateZones": [
                    {
                        "caloriesOut": 740.15264,
                        "max": 94,
                        "min": 30,
                        "minutes": 593,
                        "name": "Out of Range"
                    },
                    {
                        "caloriesOut": 249.66204,
                        "max": 132,
                        "min": 94,
                        "minutes": 46,
                        "name": "Fat Burn"
                    },
                    {
                        "caloriesOut": 0,
                        "max": 160,
                        "min": 132,
                        "minutes": 0,
                        "name": "Cardio"
                    },
                    {
                        "caloriesOut": 0,
                        "max": 220,
                        "min": 160,
                        "minutes": 0,
                        "name": "Peak"
                    }
                ],
                "restingHeartRate": 68
            }
        }
    ]
}
'

hr_intraday_example <- '
{
    "activities-heart": [
        {
            "customHeartRateZones": [],
            "dateTime": "today",
            "heartRateZones": [
                {
                    "caloriesOut": 2.3246,
                    "max": 94,
                    "min": 30,
                    "minutes": 2,
                    "name": "Out of Range"
                },
                {
                    "caloriesOut": 0,
                    "max": 132,
                    "min": 94,
                    "minutes": 0,
                    "name": "Fat Burn"
                },
                {
                    "caloriesOut": 0,
                    "max": 160,
                    "min": 132,
                    "minutes": 0,
                    "name": "Cardio"
                },
                {
                    "caloriesOut": 0,
                    "max": 220,
                    "min": 160,
                    "minutes": 0,
                    "name": "Peak"
                }
            ],
            "value": "64.2"
        }
    ],
    "activities-heart-intraday": {
        "dataset": [
            {
                "time": "00:00:00",
                "value": 64
            },
            {
                "time": "00:00:10",
                "value": 63
            },
            {
                "time": "00:00:20",
                "value": 64
            },
            {
                "time": "00:00:30",
                "value": 65
            },
            {
                "time": "00:00:45",
                "value": 65
            }
        ],
        "datasetInterval": 1,
        "datasetType": "second"
    }
}
'

sleep_summary_example <- '
{
    "sleep": [
        {
            "dateOfSleep": "2017-04-02",
            "duration": 100,
            "efficiency": 50,
            "isMainSleep": true,
            "levels": {
                "summary": {
                    "deep": {
                        "count": 50,
                        "minutes": 50,
                        "thirtyDayAvgMinutes": 50
                    },
                    "light": {
                        "count": 50,
                        "minutes": 50,
                        "thirtyDayAvgMinutes": 50
                    },
                    "rem": {
                        "count": 50,
                        "minutes": 50,
                        "thirtyDayAvgMinutes": 50
                    },
                    "wake": {
                        "count": 50,
                        "minutes": 50,
                        "thirtyDayAvgMinutes": 50
                    }
                },
                "data": [
                    {
                        "datetime": "2017-04-01T23:58:30.000",
                        "level": "wake",
                        "seconds": 50
                    },
                    {
                        "datetime": "2017-04-02T00:16:30.000",
                        "level": "rem",
                        "seconds": 50
                    }
                ],
                "shortData": [
                    {
                        "datetime": "2017-04-02T05:58:30.000",
                        "level": "wake",
                        "seconds": 50
                    },
                    {
                        "datetime": "2017-04-02T06:58:30.000",
                        "level": "wake",
                        "seconds": 50
                    }
                ]
            },
            "logId": 50,
            "minutesAfterWakeup": 50,
            "minutesAsleep": 50,
            "minutesAwake": 50,
            "minutesToFallAsleep": 50, // this is generally 0 for autosleep created sleep logs
            "startTime": "2017-04-01T23:58:30.000",
            "endTime": "2017-04-01T08:18:30.000",
            "timeInBed": 2,
            "type": "stages"
        },
        {
            "dateOfSleep": "2017-04-02",
            "duration": 100,
            "efficiency": 50,
            "isMainSleep": false,
            "levels": {
                "data": [
                    {
                        "dateTime": "2017-04-02T12:06:00.000",
                        "level": "asleep",
                        "seconds": 50
                    },
                    {
                        "dateTime": "2017-04-02T12:13:00.000",
                        "level": "restless",
                        "seconds": 50
                    },
                    {
                        "dateTime": "2017-04-02T12:14:00.000",
                        "level": "awake",
                        "seconds": 50
                    }
                ],
                "summary": {
                    "asleep": {
                        "count": 0, // this field should not be used for "asleep" summary info
                        "minutes": 50
                    },
                    "awake": {
                        "count": 50,
                        "minutes": 50
                    },
                    "restless": {
                        "count": 50,
                        "minutes": 50
                    }
                }
            },
            "logId": 50,
            "minutesAfterWakeup": 50,
            "minutesAsleep": 50,
            "minutesAwake": 50,
            "minutesToFallAsleep": 50, // this is generally 0 for autosleep created sleep logs
            "startTime": "2017-04-02T12:06:00.000",
            "timeInBed": 2,
            "type": "classic"
        }
    ],
    "summary": {
        "totalMinutesAsleep": 50,
        "totalSleepRecords": 2,
        "totalTimeInBed": 2
    }
}
'

sleep_stage_example <- '
{
    "sleep": [
        {
            "dateOfSleep": "2017-04-02",
            "duration": 100,
            "efficiency": 50,
            "isMainSleep": <true|false>,
            "levels": {
                "summary": {
                    "deep": {
                        "count": 50,
                        "minutes": 50,
                        "thirtyDayAvgMinutes": 50
                    },
                    "light": {
                        "count": 50,
                        "minutes": 50,
                        "thirtyDayAvgMinutes": 50
                    },
                    "rem": {
                        "count": 50,
                        "minutes": 50,
                        "thirtyDayAvgMinutes": 50
                    },
                    "wake": {
                        "count": 50,
                        "minutes": 50,
                        "thirtyDayAvgMinutes": 50
                    }
                },
                "data": [
                    {
                        "datetime": "2017-04-01T23:58:30.000",
                        "level": "wake",
                        "seconds": 50
                    },
                    {
                        "datetime": "2017-04-02T00:16:30.000",
                        "level": "light",
                        "seconds": 50
                    }
                ],
                "shortData": [
                    {
                        "datetime": "2017-04-02T05:58:30.000",
                        "level": "wake",
                        "seconds": 50
                    }
                ]
            },
            "logId": 50,
            "minutesAfterWakeup": 50,
            "minutesAsleep": 50,
            "minutesAwake": 50,
            "minutesToFallAsleep": 50, // this is generally 0 for autosleep created sleep logs
            "startTime": "2017-04-01T23:58:30.000",
            "timeInBed": 2,
            "type": "stages"
        }
    ]
}
'
