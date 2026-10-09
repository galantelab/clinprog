# ====================================================================================================================
# Tests for logging.R
#
# Covers:
#   - log_message()
#   - log_warning()
#   - log_stop()
#   - write_log()
#   - .timestamp()
#
# These are lightweight tests (no skip_on_cran()) so they run both locally and on CRAN.
# ====================================================================================================================

test_that("log_message emits INFO messages and records them", {
  # Resets internal log
  .clinprog_env$log_messages <- character()
  .clinprog_env$verbose <- TRUE

  # Should emit a message
  expect_message(log_message("hello world"), regexp = "INFO")

  # Should have recorded it
  expect_gt(length(.clinprog_env$log_messages), 0)
  expect_true(any(grepl("hello world", .clinprog_env$log_messages, fixed = TRUE)))
})

test_that("log_message respects verbose = FALSE", {
  .clinprog_env$log_messages <- character()
  .clinprog_env$verbose <- FALSE

  # Should NOT emit a message
  expect_silent(log_message("silent hello"))

  # But it should still be recorded
  expect_true(any(grepl("silent hello", .clinprog_env$log_messages, fixed = TRUE)))

  # Restore verbose
  .clinprog_env$verbose <- TRUE
})

test_that("log_warning emits WARN and records the message", {
  .clinprog_env$log_messages <- character()
  .clinprog_env$verbose <- TRUE

  expect_warning(log_warning("careful!"), regexp = "WARN")
  expect_true(any(grepl("careful!", .clinprog_env$log_messages, fixed = TRUE)))
})

test_that("log_stop raises an error and records the message", {
  .clinprog_env$log_messages <- character()

  expect_error(log_stop("fatal error"), regexp = "ERROR")
  expect_true(any(grepl("fatal error", .clinprog_env$log_messages, fixed = TRUE)))
})

test_that("write_log writes the accumulated messages to disk", {
  .clinprog_env$log_messages <- c("msg 1", "msg 2")

  tmp <- tempfile(fileext = ".log")
  out <- write_log(tmp)

  expect_true(file.exists(tmp))
  lines <- readLines(tmp)
  expect_true(any(grepl("msg 1", lines, fixed = TRUE)))
  expect_true(any(grepl("msg 2", lines, fixed = TRUE)))
})

test_that("write_log warns when no messages are available", {
  .clinprog_env$log_messages <- character()

  expect_warning(write_log(tempfile()), regexp = "No clinprog log")
})

test_that(".timestamp returns a properly formatted string", {
  ts <- .timestamp()
  expect_type(ts, "character")
  expect_length(ts, 1)
  # Format: YYYY-MM-DD HH:MM:SS
  expect_match(ts, "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$")
})
