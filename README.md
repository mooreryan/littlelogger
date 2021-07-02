# littlelogger

[![R-CMD-check](https://github.com/mooreryan/littlelogger/workflows/R-CMD-check/badge.svg)](https://github.com/mooreryan/littlelogger/actions) [![codecov](https://codecov.io/gh/mooreryan/littlelogger/branch/main/graph/badge.svg?token=RSGYTC86IK)](https://codecov.io/gh/mooreryan/littlelogger)


A little logging library for R scripts.

## Installation

You need [remotes](https://github.com/r-lib/remotes) to install `littlelogger`.

``` r
remotes::install_github("mooreryan/littlelogger")
```

## Usage

Make a logger, then log something!

``` r
library(littlelogger)

## The logger prints some useful info.  It's based on Ruby's logger.
logger <- make_logger()
logger$info("I'm %s and I'm %d years old.", "Elrond", 3000)
#=> I, [2021-07-01 18:43:33.937682 #52832] INFO -- I'm Elrond and I'm 3000 years old.
#       ^ Date     ^ Time          ^ PID   ^ Level ^ Message
```

For more examples, see the `?littlelogger::make_logger` help page or the unit tests in `tests/testthat/test-logger.R`.
