#' Make a new logger.
#'
#' @description Make a new logger that logs to stderr or to the provided log
#' file with the given \code{log_level}.
#'
#' @details By default, log messages are sent to the same location that
#'   \code{message} is configured to send messages to, which is the
#'   \code{stderr()} connection unless you have configured your R session
#'   differently. You can control which messages get printed with the
#'   \code{log_level} option. More restrictive levels print only high priority
#'   messages. More permissive levels print less important messages.
#'
#'   The log levels in order from most restrictive to most verbose are:
#'   "unknown", "fatal", "error", "warning", "info", "debug", and "trace".
#'
#'   The return value is a structure with class of "littlelogger" of logging
#'   functions.  The logging functions each take the same arguments as
#'   \code{\link[base]{sprintf}}.
#'
#' @examples
#' # The logger prints some useful info.  It's based on Ruby's logger.
#' logger <- new_logger()
#' logger$info("I'm %s and I'm %d years old.", "Elrond", 3000)
#' #=> I, [2021-07-01 18:43:33.937682 #52832] INFO -- I'm Elrond and I'm 3000 years old.
#' #       ^ Date     ^ Time          ^ PID   ^ Level ^ Message
#'
#' # log_level = "error" only prints the most important messages.
#' logger <- new_logger(log_level = "error")
#' logger$unknown("Will print? %s", "yes")
#' logger$fatal("Will print? %s", "yes")
#' logger$error("Will print? %s", "yes")
#' logger$warning("Will print? %s", "no")
#' logger$info("Will print? %s", "no")
#' logger$debug("Will print? %s", "no")
#' logger$trace("Will print? %s", "no")
#'
#' # log_level = "info" prints most of the messages
#' logger <- new_logger(log_level = "info")
#' logger$unknown("Will print? %s", "yes")
#' logger$fatal("Will print? %s", "yes")
#' logger$error("Will print? %s", "yes")
#' logger$warning("Will print? %s", "yes")
#' logger$info("Will print? %s", "yes")
#' logger$debug("Will print? %s", "no")
#' logger$trace("Will print? %s", "no")
#'
#' # log_level = "trace" prints all messages
#' logger <- new_logger(log_level = "trace")
#' logger$unknown("Will print? %s", "yes")
#' logger$fatal("Will print? %s", "yes")
#' logger$error("Will print? %s", "yes")
#' logger$warning("Will print? %s", "yes")
#' logger$info("Will print? %s", "yes")
#' logger$debug("Will print? %s", "yes")
#' logger$trace("Will print? %s", "yes")
#'
#' @param log_level The logging level of this logger.  Valid values
#'   are "unknown", "fatal", "error", "warning", "info", and "debug".
#'   "unknown" prints only UNKNOWN messages. "fatal" prints UNKNOWN and FATAL
#'   messages. "error" prints UNKNOWN, FATAL, and ERROR messages. "warning"
#'   prints UNKNOWN, FATAL, ERROR, and WARNING messages. "info" prints UNKNOWN,
#'   FATAL, ERROR, WARNING, and INFO messages. "debug" prints UNKNOWN, FATAL,
#'   ERROR, WARNING, INFO, and DEBUG messages. "trace" prints all messages.
#'
#' @param log_file The filename or connection object to which the log messages
#'   should be directed. The default value of \code{NULL} prints messages to the
#'   same location of R's \code{message} function. If you provide a filename,
#'   then a new connection will be opened and closed for every logged message.
#'   If you provide an open connection, then that connection is used for
#'   writing. If you provide a connection that is closed, it will be opened and
#'   closed. (Currently, this is the behavior of `cat`, since that is what is
#'   used to write messages to the connection. Check out that documentation for
#'   more info.)
#'
#' @return A new Logger with specified log_level and the following functions:
#' \itemize{
#' \item unknown: Logs an UNKNOWN message when \code{log_level} is "unknown" or more verbose.
#' \item fatal: Logs a FATAL message when \code{log_level} is "fatal" or more verbose.
#' \item error: Logs an ERROR message when \code{log_level} is "error" or more verbose.
#' \item warning: Logs a WARNING message when \code{log_level} is "warning" or more verbose.
#' \item info: Logs an INFO message when \code{log_level} is "info" or more verbose.
#' \item debug: Logs a DEBUG message when \code{log_level} is "debug".
#' \item trace: Logs a TRACE message when \code{log_level} is "trace".
#' }
#'
#' @export
#'
new_logger <- function(log_level = "info", log_file = NULL) {
  valid_levels <- c(
    "unknown",
    "fatal",
    "error",
    "warning",
    "info",
    "debug",
    "trace"
  )

  if (!(log_level %in% valid_levels)) {
    stop(sprintf(
      "log_level must be one of: %s",
      paste(shQuote(valid_levels), collapse = ", ")
    ))
  }

  # Convert log level name to integer for comparison.
  # Lower numbers are more restrictive (fewer messages).
  # Higher numbers are more verbose (more messages).
  log_level_to_int <- function(level_name) {
    switch(
      level_name,
      "unknown" = 1,
      "UNKNOWN" = 1,
      "fatal" = 2,
      "FATAL" = 2,
      "error" = 3,
      "ERROR" = 3,
      "warning" = 4,
      "WARNING" = 4,
      "info" = 5,
      "INFO" = 5,
      "debug" = 6,
      "DEBUG" = 6,
      "trace" = 7,
      "TRACE" = 7,
      1 # else
    )
  }

  # Expects an all caps log_level name.  `msg` can be a format string as it
  # is passed to sprintf. `...` can be used to pass sprintf style
  # format string opts.
  make_log_msg <- function(log_level, msg, ...) {
    # Messages look like this:
    # E, [2019-06-20 18:21:28.150640 #18632] ERROR -- Hi error
    now <- strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%OS6")
    pid <- Sys.getpid()
    log_level_code <- substr(log_level, start = 1, stop = 1)
    msg_prefix <- sprintf(
      "%s, [%s #%d] %s -- ",
      log_level_code,
      now,
      pid,
      log_level
    )

    paste0(msg_prefix, sprintf(msg, ...))
  }

  # Convert the log_level string to an integer once and use it in the
  # comparisons.
  log_level_int <- log_level_to_int(log_level)

  log_msg_default <- function(log_level, msg, ...) {
    if (log_level_to_int(log_level) <= log_level_int) {
      msg <- make_log_msg(log_level, msg, ...)
      message(msg)
    }
  }

  log_msg_to_file <- function(log_level, msg, ...) {
    if (log_level_to_int(log_level) <= log_level_int) {
      msg <- make_log_msg(log_level, msg, ...)

      withCallingHandlers(
        message(msg),
        message = function(m) {
          cat(conditionMessage(m), file = log_file, append = TRUE)
          invokeRestart("muffleMessage")
        }
      )
    }
  }

  log_msg <- if (is.null(log_file)) {
    log_msg_default
  } else {
    log_msg_to_file
  }

  structure(
    list(
      unknown = function(msg, ...) {
        log_msg("UNKNOWN", msg, ...)
      },
      fatal = function(msg, ...) {
        log_msg("FATAL", msg, ...)
      },
      error = function(msg, ...) {
        log_msg("ERROR", msg, ...)
      },
      warning = function(msg, ...) {
        log_msg("WARNING", msg, ...)
      },
      info = function(msg, ...) {
        log_msg("INFO", msg, ...)
      },
      debug = function(msg, ...) {
        log_msg("DEBUG", msg, ...)
      },
      trace = function(msg, ...) {
        log_msg("TRACE", msg, ...)
      }
    ),
    class = "littlelogger"
  )
}
