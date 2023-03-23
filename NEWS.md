# fitbitr 0.3.0

`v0.3.0` is a significant overhaul of `fitbitr`

### Breaking changes:

* Functions are now verb-prefixed, so `steps` has become `get_steps()`

### Other changes

* Significantly more validation exists for function arguments before sending requests to the API
* More information is returned to the user on API errors
* Adds new intraday time series
* Many more updates to internals
* Significant additional test coverage and overhauls how tests are run.

# fitbitr 0.2.0

* Added a `NEWS.md` file to track changes to the package.
* Fixed a couple of bugs with how token generation works
* Added some more opinionated types to the columns in the responses
* Improved error handling

# fitbitr 0.1.0

* Initial version of `fitbitr`
