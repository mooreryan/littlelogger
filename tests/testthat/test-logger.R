# See https://dune.readthedocs.io/en/stable/tests.html#test-output-sanitation
#
# returns "nothing" if no msg is present.
redact <- function(msg) {
  if (length(msg) == 0) {
    "nothing"
  } else {
    m <- sub(
      pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}",
      replacement = "DATE",
      x = msg,
      perl = TRUE
    )

    m <- sub(
      pattern = "[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{6}",
      replacement = "TIME",
      x = m
    )

    m <- sub(pattern = "#[0-9]+", replacement = "PID", x = m)

    m
  }
}

redact_capture <- function(x) {
  redact(capture.output(x, type = "message"))
}

test_that("bad log_levels give errors", {
  expect_error(make_logger("invalid"), regexp = "log_level must be one of")
  expect_error(make_logger(0), regexp = "log_level must be one of")
  expect_error(make_logger(1), regexp = "log_level must be one of")
  expect_error(make_logger(""), regexp = "log_level must be one of")
})

test_that("log_level = 'debug' prints debug messages and above", {
  logger <- make_logger(log_level = "debug")

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
    redact_capture(logger$warning("hi %s %d", "bob", 123)),
    "W, [DATE TIME PID] WARNING -- hi bob 123"
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

test_that("log_level = 'info' prints info messages and above", {
  logger <- make_logger(log_level = "info")

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
    redact_capture(logger$warning("hi %s %d", "bob", 123)),
    "W, [DATE TIME PID] WARNING -- hi bob 123"
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

test_that("log_level = 'error' prints error messages and above", {
  logger <- make_logger(log_level = "error")

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
    redact_capture(logger$warning("hi %s %d", "bob", 123)),
    "nothing"
  )
  expect_equal(
    redact_capture(logger$info("hi %s %d", "bob", 123)),
    "nothing"
  )
  expect_equal(
    redact_capture(logger$debug("hi %s %d", "bob", 123)),
    "nothing"
  )
})

test_that("log_level = 'warning' prints warning messages and above", {
  logger <- make_logger(log_level = "warning")

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
    redact_capture(logger$warning("hi %s %d", "bob", 123)),
    "W, [DATE TIME PID] WARNING -- hi bob 123"
  )
  expect_equal(
    redact_capture(logger$info("hi %s %d", "bob", 123)),
    "nothing"
  )
  expect_equal(
    redact_capture(logger$debug("hi %s %d", "bob", 123)),
    "nothing"
  )
})

test_that("log_level = 'fatal' prints only fatal messages and above", {
  logger <- make_logger(log_level = "fatal")

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
    "nothing"
  )
  expect_equal(
    redact_capture(logger$warning("hi %s %d", "bob", 123)),
    "nothing"
  )
  expect_equal(
    redact_capture(logger$info("hi %s %d", "bob", 123)),
    "nothing"
  )
  expect_equal(
    redact_capture(logger$debug("hi %s %d", "bob", 123)),
    "nothing"
  )
})

test_that("log_level = 'unknown' prints only unknown messages", {
  logger <- make_logger(log_level = "unknown")

  expect_equal(
    redact_capture(logger$unknown("hi %s %d", "bob", 123)),
    "U, [DATE TIME PID] UNKNOWN -- hi bob 123"
  )
  expect_equal(
    redact_capture(logger$fatal("hi %s %d", "bob", 123)),
    "nothing"
  )
  expect_equal(
    redact_capture(logger$error("hi %s %d", "bob", 123)),
    "nothing"
  )
  expect_equal(
    redact_capture(logger$warning("hi %s %d", "bob", 123)),
    "nothing"
  )
  expect_equal(
    redact_capture(logger$info("hi %s %d", "bob", 123)),
    "nothing"
  )
  expect_equal(
    redact_capture(logger$debug("hi %s %d", "bob", 123)),
    "nothing"
  )
})

describe("sending log messages to a file", {
  it("allows redirecting log messages to a named file", {
    log_file <- tempfile()
    logger <- make_logger(log_level = "debug", log_file = log_file)

    logger$info("Hello, %s!", "World")

    result <- readLines(log_file) |> redact()

    expect_equal(result, "I, [DATE TIME PID] INFO -- Hello, World!")
  })

  it("allows redirecting log messages when given a connection", {
    log_file_name <- tempfile()
    connection <- file(
      description = log_file_name,
      open = "at",
      blocking = TRUE
    )
    on.exit(close(connection))

    logger <- make_logger(log_level = "debug", log_file = connection)

    logger$info("Hello, %s!", "World")

    result <- readLines(log_file_name) |> redact()

    expect_equal(result, "I, [DATE TIME PID] INFO -- Hello, World!")
  })
})

describe("different loggers are independent", {
  it("doesn't interfere with any other logger", {
    default_logger <- make_logger()
    log_file_name <- tempfile()
    file_logger <- make_logger(log_file = log_file_name)

    file_logger$error("1")

    expect_equal(
      redact_capture(default_logger$error("hi")),
      "E, [DATE TIME PID] ERROR -- hi"
    )

    file_logger$info("2")

    result <- readLines(log_file_name) |> redact()

    expect_equal(
      result,
      c(
        "E, [DATE TIME PID] ERROR -- 1",
        "I, [DATE TIME PID] INFO -- 2"
      )
    )
  })
})
