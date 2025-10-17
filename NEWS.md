# littlelogger

## Version 1.0.0 -- 2025-10-17

### Added

- Add the `trace` log level
- Add support for logging to a file

### Changed

- The `make_logger` function is now `new_logger`
- The `warn` function is now `warning`
- The default log level is changed from `debug` to `info`
- The class name of the returned logger list is now `littlelogger` rather than `logger`
- Log levels are now specified using names rather than verbosity levels

### Removed

- The `abort` function has been removed from the returned logger structure

## Version 0.1.0 -- 2021-07-01

- Initial release
