# littlelogger

[![R-CMD-check](https://github.com/mooreryan/littlelogger/workflows/R-CMD-check/badge.svg)](https://github.com/mooreryan/littlelogger/actions)

A little logging library for R scripts.

## Installation

You need [remotes](https://github.com/r-lib/remotes) to install `littlelogger`. Once it is installed, run the following in the R console.

```r
remotes::install_github("mooreryan/littlelogger")
```

## Usage

Make a logger with `new_logger`, then log something!

```r
library(littlelogger)

## The logger prints some useful info.  It's based on Ruby's logger.
logger <- new_logger()
logger$info("I'm %s and I'm %d years old.", "Elrond", 3000)
#=> I, [2021-07-01 18:43:33.937682 #52832] INFO -- I'm Elrond and I'm 3000 years old.
#       ^ Date     ^ Time          ^ PID   ^ Level ^ Message
```

- You can create many instances of the logger and they will not interfere with each other.
- You can log to files by specifying the `file` argument

For more examples, see the `?littlelogger::new_logger` help page or the unit tests in `tests/testthat/test-logger.R`.
