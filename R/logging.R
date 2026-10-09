# Internal environment for clinprog
.clinprog_env <- new.env(parent = emptyenv())
.clinprog_env$log_messages <- character()
.clinprog_env$verbose <- TRUE

# Internal helper to generate timestamps
.timestamp <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

# Internal helper for console colors
.console_color <- function(text, color) {
  paste0("\033[", color, "m", text, "\033[0m")
}

#' Log an informational message
#'
#' @description
#' Records a message in the in-memory log and, if enabled, in the log file.
#' Console output is controlled by the verbose setting.
#'
#' @param ... Character inputs to be concatenated into a message.
#'
#' @keywords internal
log_message <- function(...) {
  msg <- paste(...)
  full_msg <- paste0("[", .timestamp(), "] [INFO] ", msg)

  # Record the message regardless of verbosity
  .clinprog_env$log_messages <- c(.clinprog_env$log_messages, full_msg)

  # Write to the log file, if enabled
  if (exists("log_con", envir = .clinprog_env) &&
      !is.null(.clinprog_env$log_con) &&
      isOpen(.clinprog_env$log_con)) {
    writeLines(full_msg, .clinprog_env$log_con)
  }

  # Console output
  if (isTRUE(.clinprog_env$verbose)) {
    message(full_msg)
  }

  invisible(NULL)
}

#' Log a warning message
#'
#' @description
#' Records a warning in the in-memory log and, if enabled, in the log file.
#' Console output is controlled by the verbose setting.
#'
#' @param ... Character inputs to be concatenated into a message.
#'
#' @keywords internal
log_warning <- function(...) {
  msg <- paste(...)
  full_msg <- paste0("[", .timestamp(), "] [WARN] ", msg)

  # Record the warning regardless of verbosity
  .clinprog_env$log_messages <- c(.clinprog_env$log_messages, full_msg)

  # Write to the log file, if enabled
  if (exists("log_con", envir = .clinprog_env) &&
      !is.null(.clinprog_env$log_con) &&
      isOpen(.clinprog_env$log_con)) {
    writeLines(full_msg, .clinprog_env$log_con)
  }

  # Console output
  if (isTRUE(.clinprog_env$verbose)) {
    warning(.console_color(full_msg, "33"), call. = FALSE)
  }

  invisible(NULL)
}

#' Log an error and stop execution
#'
#' @description
#' Records an error in the in-memory log and, if enabled, in the log file.
#' Execution is then stopped.
#'
#' @param ... Character inputs to be concatenated into a message.
#'
#' @keywords internal
log_stop <- function(...) {
  msg <- paste(...)
  full_msg <- paste0("[", .timestamp(), "] [ERROR] ", msg)

  # Record the error
  .clinprog_env$log_messages <- c(.clinprog_env$log_messages, full_msg)

  # Write to the log file, if enabled
  if (exists("log_con", envir = .clinprog_env) &&
      !is.null(.clinprog_env$log_con) &&
      isOpen(.clinprog_env$log_con)) {
    writeLines(full_msg, .clinprog_env$log_con)
  }

  stop(.console_color(full_msg, "31"), call. = FALSE)
}

#' Save clinprog log to file
#'
#' @description
#' Writes accumulated clinprog log messages to a file.
#'
#' @param file Character. Output file name. Defaults to "clinprog.log".
#'
#' @return Invisibly returns the output file path, or NULL if no messages
#'   are available.
#'
#' @export
write_log <- function(file) {
  # Validate available messages
  if (length(.clinprog_env$log_messages) == 0) {
    warning("No clinprog log messages are available.", call. = FALSE)
    return(invisible(NULL))
  }

  # Handle file parameter
  if (missing(file) || is.null(file)) {
    message("No filename provided. Using 'clinprog.log'...")
    file <- "clinprog.log"
  } else if (!is.character(file) ||
             length(file) != 1 ||
             is.na(file)) {
    stop("'file' must be a single character string", call. = FALSE)
  }

  # Check file extension
  ext <- tolower(tools::file_ext(file))

  # Add default extension if missing
  if (ext == "") {
    warning("No file extension detected. Using '.log'...", call. = FALSE)
    file <- paste0(file, ".log")
  }

  writeLines(.clinprog_env$log_messages, con = file)

  invisible(file)
}
