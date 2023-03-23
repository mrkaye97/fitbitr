## R CMD check results

0 errors | 0 warnings | 0 notes

* Overhauls function naming to be more verb-centric a la tidy convention
* Removes some unused / not useful functions
* Adds significant test coverage hitting the actual API, since it turns out that Fitbit's example API responses in their docs don't actually match what the API gives back...
* Improves error messaging by trying to parse JSON messages in a custom `stop_for_status()` implementation
* Adds some new methods for intraday time series (i.e. your heart rate every 1 minute for a day)
