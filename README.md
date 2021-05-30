
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fitbitr - Explore your Fitbit data in R

<!-- badges: start -->

[![R-CMD-check](https://github.com/mrkaye97/fitbitr/workflows/R-CMD-check/badge.svg)](https://github.com/mrkaye97/fitbitr/actions)
[![CRAN
Version](http://www.r-pkg.org/badges/version/fitbitr)](https://CRAN.R-project.org/package=fitbitr)
![](http://cranlogs.r-pkg.org/badges/grand-total/fitbitr) [![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`fitbitr` makes it easy to interface with Fitbit data in R.

## Installation

You can download `fitbitr` from Github:

``` r
# install.packages("devtools")
devtools::install_github("mrkaye97/fitbitr")
```

CRAN release coming soon!

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

    generate_token(
      client_id = <YOUR-CLIENT-ID>,
      client_secret = <YOUR-CLIENT-SECRET>
      callback = <YOUR-REDIRECT-URL>
    )
    ```

    -   If you want to edit the scopes that are enabled, you can do so
        with the `scopes = c('scopes', 'you', 'want', 'enabled')`
        argument. You can find information on the available scope
        options
        [here](https://dev.fitbit.com/build/reference/web-api/oauth2/#scope).
    -   If you want to cache your token, you can do so by specifying
        either `cache = TRUE` or `cache = <some-file-path>`. See the
        docs on `httr::oauth2.0_token()` for details.

5.  And that’s it! You now have your Fitbit API credentials set up.
    `fitbitr` tracks them behind the scenes for you, so all that you
    need to do at the start of each R session is either
    `generate_token()` or `load_cached_token()`, and you’ll be off and
    running.
