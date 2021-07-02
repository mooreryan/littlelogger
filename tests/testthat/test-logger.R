# See https://dune.readthedocs.io/en/stable/tests.html#test-output-sanitation
#
# returns "nothing" if no msg is present.
redact <- function(msg) {
  if (length(msg) == 0) {
    "nothing"
  } else {
    m <- sub(pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}",
             replacement = "DATE",
             x = msg,
             perl = TRUE)

    m <- sub(pattern = "[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{6}",
             replacement = "TIME",
             x = m)

    m <- sub(pattern = "#[0-9]+",
             replacement = "PID",
             x = m)

    m
  }
}

redact_capture <- function(x) {
  redact(capture.output(x, type = "message"))
}

test_that("bad log_levels give errors", {
  expect_error(make_logger(0), regexp = "log_level must be 1, 2, or 3")
  expect_error(make_logger(-1), regexp = "log_level must be 1, 2, or 3")
  expect_error(make_logger(4), regexp = "log_level must be 1, 2, or 3")
  expect_error(make_logger("asth"), regexp = "log_level must be 1, 2, or 3")
})

test_that("log_level = 3 prints all messages", {
  logger <- make_logger(log_level = 3)

  ## Checking levels
  expect_equal(
    redact_capture(logger$unknown("hi %s %d", "bob", 123)),
    "U, [DATE TIME PID] UNKNOWN -- hi bob 123"
  )
  expect_equal(
    redact_capture(logger$fatal("hi %s %d", "bob", 123)),
    "F, [DATE TIME PID] FATAL -- hi bob 123"
  )
  expect_equal(
    redact_capture(logger$error("hi %s %d", "bob", 123)),
    "E, [DATE TIME PID] ERROR -- hi bob 123"
  )
  expect_equal(
    redact_capture(logger$warn("hi %s %d", "bob", 123)),
    "W, [DATE TIME PID] WARN -- hi bob 123"
  )
  expect_equal(
    redact_capture(logger$info("hi %s %d", "bob", 123)),
    "I, [DATE TIME PID] INFO -- hi bob 123"
  )
  expect_equal(
    redact_capture(logger$debug("hi %s %d", "bob", 123)),
    "D, [DATE TIME PID] DEBUG -- hi bob 123"
  )
})

test_that("log_level = 2 prints some messages", {
  logger <- make_logger(log_level = 2)

  ## Checking levels
  expect_equal(
    redact_capture(logger$unknown("hi %s %d", "bob", 123)),
    "U, [DATE TIME PID] UNKNOWN -- hi bob 123"
  )
  expect_equal(
    redact_capture(logger$fatal("hi %s %d", "bob", 123)),
    "F, [DATE TIME PID] FATAL -- hi bob 123"
  )
  expect_equal(
    redact_capture(logger$error("hi %s %d", "bob", 123)),
    "E, [DATE TIME PID] ERROR -- hi bob 123"
  )
  expect_equal(
    redact_capture(logger$warn("hi %s %d", "bob", 123)),
    "W, [DATE TIME PID] WARN -- hi bob 123"
  )
  expect_equal(
    redact_capture(logger$info("hi %s %d", "bob", 123)),
    "I, [DATE TIME PID] INFO -- hi bob 123"
  )
  expect_equal(
    redact_capture(logger$debug("hi %s %d", "bob", 123)),
    "nothing"
  )
})

test_that("log_level = 1 prints important messages", {
  logger <- make_logger(log_level = 1)

  ## Checking levels
  expect_equal(
    redact_capture(logger$unknown("hi %s %d", "bob", 123)),
    "U, [DATE TIME PID] UNKNOWN -- hi bob 123"
  )
  expect_equal(
    redact_capture(logger$fatal("hi %s %d", "bob", 123)),
    "F, [DATE TIME PID] FATAL -- hi bob 123"
  )
  expect_equal(
    redact_capture(logger$error("hi %s %d", "bob", 123)),
    "E, [DATE TIME PID] ERROR -- hi bob 123"
  )
  expect_equal(
    redact_capture(logger$warn("hi %s %d", "bob", 123)),
    "nothing"
  )
  expect_equal(
    redact_capture(logger$info("hi %s %d", "bob", 123)) ,
    "nothing"
  )
  expect_equal(
    redact_capture(logger$debug("hi %s %d", "bob", 123)),
    "nothing"
  )
})
