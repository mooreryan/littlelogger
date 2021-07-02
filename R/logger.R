#' Make a new logger.
#'
#' @description Make a new logger that logs to stderr with the given
#'   \code{log_level}.
#'
#' @details Logging is to stderr.  You can control which messages get
#'   printed with the \code{log_level} option.  Lower numbers print
#'   only high priority messages.  Higher numbers print less important
#'   messages.
#'
#'   The return value is a list with logging and abort functions.  The
#'   logging functions each take the same arguments as
#'   \code{\link[base]{sprintf}}.  The \code{abort} function takes
#'   those arguments plus a \code{status} argument that defaults to 1
#'   and specifies the exit code of your script.
#'
#' @examples
#' ## The logger prints some useful info.  It's based on Ruby's logger.
#' logger <- make_logger()
#' logger$info("I'm %s and I'm %d years old.", "Elrond", 3000)
#' #=> I, [2021-07-01 18:43:33.937682 #52832] INFO -- I'm Elrond and I'm 3000 years old.
#' #       ^ Date     ^ Time          ^ PID   ^ Level ^ Message
#'
#' ## log_level = 1 only prints the most important messages.
#' logger <- make_logger(log_level = 1)
#' logger$unknown("Will print? %s", "yes")
#' logger$fatal("Will print? %s", "yes")
#' logger$error("Will print? %s", "yes")
#' logger$warn("Will print? %s", "no")
#' logger$info("Will print? %s", "no")
#' logger$debug("Will print? %s", "no")
#'
#' ## log_level = 2 prints most of the messages
#' logger <- make_logger(log_level = 2)
#' logger$unknown("Will print? %s", "yes")
#' logger$fatal("Will print? %s", "yes")
#' logger$error("Will print? %s", "yes")
#' logger$warn("Will print? %s", "yes")
#' logger$info("Will print? %s", "yes")
#' logger$debug("Will print? %s", "no")
#'
#' ## log_level = 3 prints all messages
#' logger <- make_logger(log_level = 3)
#' logger$unknown("Will print? %s", "yes")
#' logger$fatal("Will print? %s", "yes")
#' logger$error("Will print? %s", "yes")
#' logger$warn("Will print? %s", "yes")
#' logger$info("Will print? %s", "yes")
#' logger$debug("Will print? %s", "yes")
#'
#' ## You can abort a script with a nice message like this:
#' \dontrun{
#' logger$abort("1 + 1 should be 2.  Got %d", 3, status = 111)
#' }
#'
#' @param log_level The logging level of this logger.  Valid values
#'   are 1, 2, and 3.  1 prints only UNKNOWN, FATAL, and ERROR
#'   messages.  2 prints UNKNOWN, FATAL, ERROR, WARN, and INFO
#'   messages.  3 prints UNKNOWN, FATAL, ERROR, WARN, INFO, and DEBUG
#'   messages.
#'
#' @return A new Logger with specified log_level and the following functions:
#' \itemize{
#' \item abort: Aborts the script and logs a FATAL message.
#' \item unknown: Logs an UNKNOWN message when \code{log_level >= 1}.
#' \item fatal: Logs an FATAL message when \code{log_level >= 1}.
#' \item error: Logs an ERROR message when \code{log_level >= 1}.
#' \item warn: Logs an WARN message when \code{log_level >= 2}.
#' \item info: Logs an INFO message when \code{log_level >= 2}.
#' \item debug: Logs an DEBUG message when \code{log_level = 3}.
#' }
#'
#' @export
make_logger <- function(log_level = 3) {
  if (!(log_level == 1 || log_level == 2 || log_level == 3)) {
    stop("log_level must be 1, 2, or 3")
  }

  ## Lower log_level means less verbose.  So only messages less than
  ## log_level level will get printed.  This is a lookup function
  ## for log_level levels.
  log_level_to_int <- function(msg_type) {
    switch(
      msg_type,
      "UNKNOWN" = 1,
      "FATAL" = 1,
      "ERROR" = 1,
      "WARN" = 2,
      "INFO" = 2,
      "DEBUG" = 3,
      3 # default
    )
  }

  ## Expects a valid msg_type.  `msg` can be a format string as it
  ## is passed to sprintf.`...` can be used to pass sprintf style
  ## format string opts.
  make_log_msg <- function(msg_type, msg, ...) {
    ## Messages look like this:
    ## E, [2019-06-20 18:21:28.150640 #18632] ERROR -- Hi error
    now <- strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%OS6")
    pid <- Sys.getpid()
    msg_code <- substr(msg_type, start = 1, stop = 1)
    msg_prefix <- sprintf(
      "%s, [%s #%d] %s -- ",
      msg_code,
      now,
      pid,
      msg_type
    )

    paste0(
      msg_prefix,
      sprintf(msg, ...))
  }


  ## If msg_type is not one of the recognized options, it silently does
  ## nothing.  So don't use it out of context.
  log_msg <- function(msg_type, msg, ...) {
    if (log_level_to_int(msg_type) <= log_level) {
      msg <- make_log_msg(msg_type, msg, ...)
      message(msg)
    }
  }

  structure(
    list(
      abort = function(msg, ..., status = 1) {
        log_msg("FATAL", msg, ...)
        quit(save = "no", status = status, runLast = TRUE)
      },
      unknown = function(msg, ...) {
        log_msg("UNKNOWN", msg, ...)
      },
      fatal = function(msg, ...) {
        log_msg("FATAL", msg, ...)
      },
      error = function(msg, ...) {
        log_msg("ERROR", msg, ...)
      },
      warn = function(msg, ...) {
        log_msg("WARN", msg, ...)
      },
      info = function(msg, ...) {
        log_msg("INFO", msg, ...)
      },
      debug = function(msg, ...) {
        log_msg("DEBUG", msg, ...)
      }
    ),
    class = "logger"
  )
}
