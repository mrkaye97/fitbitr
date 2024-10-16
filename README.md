
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fitbitr - Explore your Fitbit data in R

<!-- badges: start -->

[![R-CMD-check](https://github.com/mrkaye97/fitbitr/workflows/R-CMD-check/badge.svg)](https://github.com/mrkaye97/fitbitr/actions)
[![CRAN
Version](http://www.r-pkg.org/badges/version/fitbitr)](https://CRAN.R-project.org/package=fitbitr)
[![](https://cranlogs.r-pkg.org/badges/fitbitr)](https://cran.r-project.org/package=fitbitr)
 [![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`fitbitr` makes it easy to interface with Fitbit data in R.

## Installation

You can install the CRAN version of `fitbitr` with:

``` r
install.packages("fitbitr")
```

Or you can install the latest development version from Github:

``` r
# install.packages("devtools")
devtools::install_github("mrkaye97/fitbitr")
```

## Setup

There are a few steps you’ll need to do before you can start pulling
your Fitbit data:

1.  Make an app [here](https://dev.fitbit.com/apps/new).

2.  Fill in the fields as you like (see image below for guidance).

    <img src="https://raw.githubusercontent.com/mrkaye97/fitbitr/master/inst/app_setup.png" width="100%" />

3.  You’ll be redirected to a page with your credentials. **Make sure
    you keep the Client ID, Client Secret, and Redirect URL. You’ll need
    them to finish the setup process**. You can always refer back to
    [the apps page](https://dev.fitbit.com/apps) to find them again.

4.  Generate a token:

    ``` r
    library(fitbitr)

    .fitbitr_token <- generate_fitbitr_token(
      oauth_app_name = <YOUR-APP-NAME>,
      client_id = <YOUR-CLIENT-ID>,
      client_secret = <YOUR-CLIENT-SECRET>,
      callback = <YOUR-REDIRECT-URL>
    )
    ```

    - If you want to edit the scopes that are enabled, you can do so
      with the `scopes = c('scopes', 'you', 'want', 'enabled')`
      argument. You can find information on the available scope options
      [here](https://dev.fitbit.com/build/reference/web-api/oauth2/#scope).
    - If you want to cache your token, you can do so by specifying
      either `cache = TRUE` or `cache = <some-file-path>`. See the docs
      on `httr::oauth2.0_token()` for details.

5.  And that’s it! You now have your Fitbit API credentials set up.
    `fitbitr` tracks them behind the scenes for you, so all that you
    need to do at the start of each R session is
    `generate_fitbitr_token()`. On a session restart,
    `generate_fitbitr_token()` will try to laod a token from your
    `.httr-oauth` if it exists.

## Using `fitbitr`

Once you have a token, using `fitbitr` is very straightforward:

    ```r
    > get_steps("2020-05-21", "2020-05-28")
    # A tibble: 8 × 2
      date       steps
      <date>     <dbl>
    1 2020-05-21  3734
    2 2020-05-22  5107
    3 2020-05-23  5640
    4 2020-05-24  6595
    5 2020-05-25  8466
    6 2020-05-26  5833
    7 2020-05-27  8616
    8 2020-05-28  3161
    ```

`fitbitr` tries to return a useful, tidy object back to you from the
API.

## Known Issues / Futute Work

Given the structure of the Fitbit API, it doesn’t appear to be currently
possible to use `fitbitr` *outside* of an interactive session. And
furthermore, it doesn’t help that Fitbit refresh tokens only last 8
hours, at which point you’d need another interactive session to generate
a new one.

For these reasons, it is not advised to try to use `fitbitr`
non-interactively, such as in a Shiny app server, a CI/CD process, a
background job, etc.
