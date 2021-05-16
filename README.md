
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fitbitr - Explore your Fitbit data in R

<!-- badges: start -->

[![R-CMD-check](https://github.com/mrkaye97/fitbitr/workflows/R-CMD-check/badge.svg)](https://github.com/mrkaye97/fitbitr/actions)
[![CRAN
Version](http://www.r-pkg.org/badges/version/fitbitr)](http://cran.rstudio.com/web/packages/fitbitr)
![](http://cranlogs.r-pkg.org/badges/grand-total/fitbitr)
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
2.  Fill in the following fields as you like.
3.  You’ll be redirected to a page with your credentials. **Make sure
    you keep the Client ID, Client Secret, and Redirect URL. You’ll need
    them to finish the setup process**. You can always refer back to
    [the apps page](https://dev.fitbit.com/apps) to find them again.

<img src="https://raw.githubusercontent.com/mrkaye97/fitbitr/master/inst/app_setup.png" width="100%" />

3.  Run the following command:

``` r
library(fitbitr)

initial_setup(
  client_id = <YOUR-CLIENT-ID>,
  client_secret = <YOUR-CLIENT-SECRET>
  callback = <YOUR-REDIRECT-URL>
)
```

-   If you want to edit the scopes that are enabled, you can do so with
    the `scopes = c('scopes', 'you', 'want', 'enabled')` argument. You
    can find information on the available scope options
    [here](https://dev.fitbit.com/build/reference/web-api/oauth2/#scope).
-   If you want to use a file other than `~/.fitbitr-oauth` to cache
    your credentials, you can do so by specifying a file path with
    `path = <your-file_path>`.

4.  `initial_setup()` will send you to a page in your browser with a
    code in the URL. Copy the code into the prompt in your R session.
5.  And that’s it! You now have your Fitbit API credentials set up in
    the file you specified, and you can use `fitbitr_setup()` to set
    them as env vars to make calling individual functions easier.

## Usage

See the vignettes for details on how to use `fitbitr`
