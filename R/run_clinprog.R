#' Discover a molecular prognostic signature (module I)
#'
#' @description
#' Identifies molecular features associated with overall survival using feature filtering,
#' bootstrap resampling, and penalized (LASSO) Cox regression.
#'
#' @param data A data.frame with one row per sample and sample identifiers as row names.
#'   The first two columns must contain the overall-survival event indicator and follow-up time,
#'   respectively; subsequent columns contain numeric gene- or transcript-level expression values.
#'   The first two columns are interpreted as \code{OS} (0 = censored, 1 = event) and
#'   \code{OS.time}, regardless of their original column names.
#' @param output_prefix Character string used as the prefix for output files (default: \code{"clinprog"}).
#' @param norm_exp Logical. If \code{TRUE}, applies \code{log2(expression + 1)} to feature columns
#'   before analysis (default: \code{FALSE}).
#' @param bootstrap Positive integer specifying the requested number of valid bootstrap iterations
#'   (default: \code{1}).
#' @param group_size Non-negative integer specifying the number of features sampled per bootstrap iteration
#'   (default: \code{10}).
#' @param percentage_filter Numeric correlation-filter threshold in the interval \code{(0, 1]}
#'   (default: \code{0.3}).
#' @param variance_filter Numeric variance-filter threshold in the interval \code{(0, 1]}
#'   (default: \code{0.01}).
#' @param max_follow_up Positive numeric maximum follow-up time, or \code{NULL} to disable follow-up truncation
#'   (default: \code{NULL}). Its units must match \code{OS.time}.
#' @param feature_type Character string specifying the input feature level: \code{"gene"} or
#'   \code{"transcript"} (default: \code{"gene"}).
#' @param min_signature_size Positive integer or \code{NULL}. If supplied, searches for the shortest prefix
#'   of the ranked signature that achieves a univariate log-rank p-value below 0.05 on the same dataset
#'   (default: \code{NULL}, disabled). This is a parsimony heuristic, not independent validation.
#' @param min_signature_cutoff Character string or \code{NULL}. Score-splitting strategy used during the
#'   minimum-signature search: \code{"median"}, \code{"mean"}, \code{"signal"}, or \code{"roc"}
#'   (default: \code{NULL}, which uses \code{"median"} when the search is enabled).
#' @param skip_survival_checks Logical. If \code{TRUE}, relaxes/skips survival-related filtering steps
#'   (default: \code{FALSE}). Use with care because it can retain features that fail these checks.
#' @param save_plots Logical. If \code{TRUE}, saves signature histogram and lollipop plots (default: \code{FALSE}).
#' @param save_table Logical. If \code{TRUE}, writes the selected signature to a tabular file (default: \code{FALSE}).
#' @param save_json Logical. If \code{TRUE}, writes execution parameters and session metadata to JSON (default: \code{FALSE}).
#' @param save_rds Logical. If \code{TRUE}, saves the result object as an RDS file (default: \code{FALSE}).
#' @param report_format Logical or character. Use \code{FALSE} to disable report generation,
#'   \code{TRUE} or \code{"PDF"} for PDF, or \code{"HTML"} for HTML (default: \code{FALSE}).
#'   Report rendering requires the package's Quarto template and a working Quarto installation.
#' @param verbose Logical or 0/1. If \code{TRUE} or \code{1}, prints progress messages; if \code{FALSE}
#'   or \code{0}, suppresses them (default: \code{1}).
#' @param n_cores Positive integer number of CPU cores used for parallel bootstrap regression
#'   (default: \code{1}, serial execution).
#' @param seed Integer seed controlling stochastic operations in \code{run_regression()} (default: \code{123}).
#'   The function restores the caller's R random-number-generator state on exit.
#' @param save_log Logical. If \code{TRUE}, writes a log file (default: \code{FALSE}).
#'
#' @details
#' The function validates the input, optionally truncates follow-up, filters low-variance features and
#' features failing proportional-hazards checks, and derives a signature through a simplified regression
#' path or bootstrap LASSO Cox regression. Selected features are ranked by their coefficients.
#'
#' If \code{min_signature_size} is supplied, a top-prefix search is performed on the same dataset used
#' for signature discovery. Because this is not an independent cohort, its result should not be treated as
#' external validation.
#'
#' The \code{seed} argument applies to the regression module only. It does not control stochastic steps
#' in \code{run_survival()} when that function is called separately or as part of \code{run_complete()}.
#'
#' @return An object of class \code{"clinprog_signature"}, containing the signature data.frame,
#'   generated plots (if requested), analysis parameters, metadata, and the original call.
#'
#' @seealso \code{\link{run_survival}}, \code{\link{run_complete}}, \code{\link{plot_histogram}},
#'   \code{\link{plot_lollipop}}, \code{\link{write_signature}}, \code{\link{write_rds}},
#'   \code{\link{write_report}}, \code{\link{write_log}}.
#'
#' @examples
#' \dontrun{
#' data("toy_gene_expression", package = "clinprog")
#' fit <- run_regression(
#'   data = toy_gene_expression,
#'   feature_type = "gene",
#'   norm_exp = TRUE,
#'   bootstrap = 10,
#'   group_size = 5,
#'   n_cores = 1,
#'   seed = 123
#' )
#' print(fit)
#' summary(fit)
#' coef(fit)
#' plot(fit)
#' }
#'
#' @export
run_regression <- function(data,
                           output_prefix = "clinprog",
                           norm_exp = FALSE,
                           bootstrap = 1,
                           group_size = 10,
                           percentage_filter = 0.3,
                           variance_filter = 0.01,
                           max_follow_up = NULL,
                           feature_type = "gene",
                           min_signature_size = NULL,
                           min_signature_cutoff = NULL,
                           skip_survival_checks = FALSE,
                           save_plots = FALSE,
                           save_table = FALSE,
                           save_json = FALSE,
                           save_rds = FALSE,
                           report_format = FALSE,
                           verbose = 1,
                           n_cores = 1,
                           seed = 123,
                           save_log = FALSE)
{
  # Start time counter
  start_time <- Sys.time()

  # Checks LOG argument
  if (!is.logical(save_log) || length(save_log) != 1) {log_stop("Argument 'save_log' must be a boolean [TRUE or FALSE]")}

  # Checks verbosity and updates environment
  if (!verbose %in% c(0, 1, FALSE, TRUE) || length(verbose) != 1)
  {
    log_stop("Argument 'verbose' must be 0, 1, FALSE or TRUE")
  }
  .clinprog_env$verbose <- as.logical(verbose)

  # Checks output prefix argument
  if (!is.character(output_prefix) || length(output_prefix) != 1) {log_stop("Argument 'output_prefix' must be a string")}

  # Logging setup - creating
  start_msg <- paste0("[", .timestamp(), "] [INFO] clinprog regression log started")
  .clinprog_env$log_messages <- c(.clinprog_env$log_messages, start_msg)
  if (save_log)
  {
    logfile <- paste0(output_prefix, "_regression.log")
    .clinprog_env$log_con <- file(logfile, open = "wt")
    writeLines(start_msg, .clinprog_env$log_con)
  }

  # Logging setup - closing
  on.exit({
    finish_msg <- paste0("[", .timestamp(), "] [INFO] clinprog regression finished")
    .clinprog_env$log_messages <- c(.clinprog_env$log_messages, finish_msg)

    if (!is.null(.clinprog_env$log_con) && isOpen(.clinprog_env$log_con)) {
      writeLines(finish_msg, .clinprog_env$log_con)
      close(.clinprog_env$log_con)
      .clinprog_env$log_con <- NULL
    }
  }, add = TRUE)

  # Validate R data.frame
  if (!is.data.frame(data)) {log_stop("'data' must be a data.frame")}

  # Validates input parameters
  log_message("Validating parameters...")
  if (!feature_type %in% c("gene", "transcript")) {log_stop("Argument 'feature_type' must be either 'gene' or 'transcript'")}
  if (!is.numeric(bootstrap) || length(bootstrap) != 1 || !is.finite(bootstrap) ||
      bootstrap < 1 || bootstrap %% 1 != 0)
  {
    log_stop("Argument 'bootstrap' must be a positive integer")
  }
  if (!is.numeric(group_size) || length(group_size) != 1 || group_size < 0 || group_size %% 1 != 0)
  {
    log_stop("Argument 'group_size' must be an integer")
  }
  if (!is.numeric(percentage_filter) || length(percentage_filter) != 1 ||
      percentage_filter <= 0 || percentage_filter > 1)
  {
    log_stop("Argument 'percentage_filter' must be a positive numeric value in range (0, 1]")
  }
  if (!is.numeric(variance_filter) || length(variance_filter) != 1 ||
      variance_filter <= 0 || variance_filter > 1)
  {
    log_stop("Argument 'variance_filter' must be a positive numeric value in range (0, 1]")
  }
  if (!is.null(max_follow_up) && (!is.numeric(max_follow_up) || length(max_follow_up) != 1 || max_follow_up <= 0))
  {
    log_stop("Argument 'max_follow_up' must be a positive numeric value")
  }
  if (!is.logical(norm_exp) || length(norm_exp) != 1) {log_stop("Argument 'norm_exp' must be a boolean [TRUE or FALSE]")}
  if (!is.null(min_signature_size) && (!is.numeric(min_signature_size) || length(min_signature_size) != 1 ||
                                       min_signature_size < 1 || min_signature_size %% 1 != 0))
  {
    log_stop("Argument 'min_signature_size' must be NULL or a single positive integer")
  }
  if (!is.null(min_signature_cutoff)) {
    min_signature_cutoff <- tolower(min_signature_cutoff)
  }
  if (!is.null(min_signature_cutoff) && (!is.character(min_signature_cutoff) || length(min_signature_cutoff) != 1 ||
                                         !min_signature_cutoff %in% c("median", "mean", "signal", "roc")))
  {
    log_stop("Argument 'min_signature_cutoff' must be NULL or one of: 'median', 'mean', 'signal', 'roc'")
  }
  if (is.null(min_signature_size) && !is.null(min_signature_cutoff))
  {
    log_warning("'min_signature_cutoff' was provided but 'min_signature_size' is NULL. The minimum signature search ",
                "will not run. Set 'min_signature_size' to enable it.")
  }
  if (!is.logical(skip_survival_checks) || length(skip_survival_checks) != 1) {log_stop("Argument 'skip_survival_checks' must be a boolean [TRUE or FALSE]")}
  if (!is.logical(save_table) || length(save_table) != 1) {log_stop("Argument 'save_table' must be a boolean [TRUE or FALSE]")}
  if (!is.logical(save_json) || length(save_json) != 1) {log_stop("Argument 'save_json' must be a boolean [TRUE or FALSE]")}
  if (!is.logical(save_rds) || length(save_rds) != 1) {log_stop("Argument 'save_rds' must be a boolean [TRUE or FALSE]")}
  if (!is.logical(save_plots) || length(save_plots) != 1) {log_stop("Argument 'save_plots' must be a boolean [TRUE or FALSE]")}
  if (!(is.logical(report_format) || (is.character(report_format) && length(report_format) == 1 && toupper(report_format) %in% c("PDF", "HTML"))))
  {
    log_stop("'report_format' must be a boolean [TRUE or FALSE] or a character ['PDF' or 'HTML']")
  }
  if (!is.numeric(n_cores) || length(n_cores) != 1 || !is.finite(n_cores) || n_cores < 1 || n_cores %% 1 != 0)
  {
    log_stop("Argument 'n_cores' must be a positive integer")
  }
  if (!is.numeric(seed) || length(seed) != 1 || !is.finite(seed) || seed %% 1 != 0)
  {
    log_stop("Argument 'seed' must be an integer")
  }
  log_message("Done.")

  # Get n_cores and seed
  n_cores <- as.integer(n_cores)
  seed <- as.integer(seed)

  # Snapshot the current global RNG state (if any) into old_seed,
  # so it can be restored when run_regression() exits
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else {
    NULL
  }

  # Restore the global RNG state previously saved
  on.exit({
    if (!is.null(old_seed)) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  # Reads INput
  full_data <- data
  colnames(full_data) <- gsub("-", "__", colnames(full_data))
  log_message("Done.")

  # Standardizes the column names of the survival metrics
  log_message("Renaming first two (survival) columns to 'OS' and 'OS.time'...")
  colnames(full_data)[1:2] <- c("OS", "OS.time")
  log_message("Done.")

  # Log2(Expression + 1) transforms expression data if requested
  if (norm_exp)
  {
    log_message("Transforming expression data with formula log2(expression + 1)...")
    gene_cols <- 3:ncol(full_data)
    full_data[, gene_cols] <- log2(full_data[, gene_cols] + 1)
    log_message("Done.")
  }

  # Validates input file
  log_message("Validating input...")

  # General structure
  if (ncol(full_data) < 3) {log_stop("Input data must contain at least two [OS, OS.time] survival columns and one feature")}

  # OS survival column
  os_values <- full_data[,1]
  if (!is.numeric(os_values)) {log_stop("Column [OS] must be numeric and the first in the table")}
  if (anyNA(os_values)) {log_stop("Column [OS] must be the first in the table and contain no missing values")}
  unique_os <- sort(unique(stats::na.omit(os_values)))
  if (!setequal(unique_os, c(0, 1))) {log_stop("Column [OS] must be the first in the table and contain only binary values: 0 and 1")}

  # OS.time survival column
  time_values <- full_data[,2]
  if (!is.numeric(time_values)) {log_stop("Column [OS.time] must be numeric and the second in the table")}
  if (anyNA(time_values)) {log_stop("Column [OS.time] must be the second in the table and contain no missing values")}
  if (any(time_values < 0, na.rm = TRUE)) {log_stop("Column [OS.time] must be the second in the table and contain only positive values")}
  log_message("Done.")

  # Applies maximum follow-up censoring if requested
  log_message("Applying maximum max_follow_up value...")
  full_data <- clinprog_followup(data = full_data, max_follow_up = max_follow_up)
  log_message("Done.")

  # Defines coefficient cutoffs to minimize false positives
  bar <- if (feature_type == "gene") 0.0035 else 0.011

  # Variance filter
  log_message("Running variance filter...")
  full_data <- clinprog_varfun(full_data, variance_filter, skip_survival_checks)
  log_message("Done.")

  # Schoenfeld tests
  log_message("Performing Schoenfeld test...")
  full_data <- clinprog_ph_assumptions(full_data)
  log_message("Done.")

  # Validates processed data
  log_message("Validating processed data...")
  if (!is.data.frame(full_data)) {log_stop("Processed data is not a 'data.frame' anymore")}
  if (nrow(full_data) == 0) {log_stop("No samples remaining after filtering steps")}
  if ((ncol(full_data) - 2) < 1) {log_stop("No features remaining after filtering steps")}
  num_data <- full_data[, vapply(full_data, is.numeric, FUN.VALUE = logical(1)), drop = FALSE]
  if (!all(is.finite(as.matrix(num_data)))) {log_stop("Processed data contains invalid values [NA, NaN or Inf]")}
  log_message("Done.")

  # Runs simplified regression
  log_message("Checking if simplified regression with no bootstraps is required...")
  early_signature <- clinprog_feature_check(full_data, group_size, seed = seed)
  log_message("Done.")

  # Decides if user needs to use simplified result or go for the more robust Bootstrap approach
  if (!is.null(early_signature)) {
    log_message("Using simplified regression with no bootstraps...")
    signature <- early_signature
    log_message("Done.")
  } else {
    log_message("Running robust regression with bootstraps...")
    signature <- clinprog_bootstrapfun(
      full_data = full_data,
      bootstrap = bootstrap,
      group_size = group_size,
      cor_threshold = percentage_filter,
      coef_threshold = bar,
      n_cores = n_cores,
      seed = seed
    )
    log_message("Done.")
  }

  # Validation
  log_message("Validating generated signature...")
  if (!is.data.frame(signature)) {log_stop("signature must be a 'data.frame'")}
  if (nrow(signature) == 0) {log_stop("No features selected in the final signature")}
  if (!all(c("feature", "coefficient") %in% colnames(signature))) {log_stop("signature must contain 'feature' and 'coefficient' columns")}
  if (anyNA(signature$coefficient))
  {
    log_warning("NA values found in coefficients. Removing them...")
    signature <- signature[!is.na(signature$coefficient), ]
  }
  if (nrow(signature) == 0) {log_stop("All coefficients were NA after filtering")}
  signature <- signature[order(abs(signature$coefficient), decreasing = TRUE), ]
  log_message("Done.")

  # Search for the minimum signature with significant univariate survival
  if (!is.null(min_signature_size))
  {
    if (is.null(min_signature_cutoff))
    {
      log_warning("'min_signature_cutoff' not provided. Using 'median' (default).")
      cutoff_used <- "median"
    } else {
      cutoff_used <- min_signature_cutoff
    }
    min_sig_result <- clinprog_min_signature(signature = signature, data = full_data,
                                             min_size = min_signature_size, score_cutoff = cutoff_used)
    signature <- min_sig_result$signature
  }

  # Produces histogram and lollipop plots for generated signature
  plot_list <- NULL
  if (save_plots)
  {
    log_message("Making histogram and lollipop plots...")
    plot_list <- list(
      hist_plot = plot_histogram(signature, output_prefix),
      lolli_plot = plot_lollipop(signature, output_prefix))
    log_message("Done.")
  }

  # Writes signature table if requested
  if (save_table)
  {
    log_message("Writing signature table...")
    write_signature(signature, paste0(output_prefix, "_signature.txt"))
    log_message("Done.")
  }

  # Creates S3 object containing the signature and plots
  # Listed parameters are only those directly affecting the scientific results/model
  result <- new_clinprog_signature(
    signature = signature,
    plots = plot_list,
    params = list(
      norm_exp = norm_exp,
      bootstrap = bootstrap,
      group_size = group_size,
      percentage_filter = percentage_filter,
      variance_filter = variance_filter,
      max_follow_up = max_follow_up,
      feature_type = feature_type,
      min_signature_size = min_signature_size,
      min_signature_cutoff = min_signature_cutoff,
      skip_survival_checks = skip_survival_checks,
      n_cores = n_cores,
      seed = seed
    ),
    metadata = list(
      timestamp = Sys.time(),
      R_version = R.version.string,
      package_version = utils::packageVersion("clinprog")
    ),
    call = match.call()
  )

  # Writes execution metadata
  if (save_json)
  {
    log_message("Writing JSON metadata...")
    write_metadata(
      module = "run_regression",
      output_prefix = output_prefix,
      parameters = list(
        data = "data.frame",
        output_prefix = output_prefix,
        norm_exp = norm_exp,
        bootstrap = bootstrap,
        group_size = group_size,
        percentage_filter = percentage_filter,
        variance_filter = variance_filter,
        max_follow_up = max_follow_up,
        feature_type = feature_type,
        min_signature_size = min_signature_size,
        min_signature_cutoff = min_signature_cutoff,
        skip_survival_checks = skip_survival_checks,
        save_plots = save_plots,
        save_table = save_table,
        save_json = save_json,
        save_rds = save_rds,
        report_format = report_format,
        verbose = verbose,
        n_cores = n_cores,
        seed = seed,
        save_log = save_log
      ),
      command = "none"
    )
    log_message("Done.")
  }

  # Writes RDS object
  if (save_rds)
  {
    log_message("Writing RDS object...")
    write_rds(object = result, file = paste0(output_prefix, "_regression.rds"))
    log_message("Done.")
  }

  # Generates report_format if requested
  report_ext <- if (is.character(report_format) && toupper(report_format) == "HTML") ".html" else ".pdf"
  if (!isFALSE(report_format))
  {
    log_message("Writing clinprog report_format...")
    write_report(object = result, format = report_format, file = paste0(output_prefix, "_report_regression", report_ext))
    log_message("Done.")
  }

  # End time counter
  elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  # Prints elapsed time
  if (elapsed_time >= 3600) {
    log_message(sprintf("Time to run [module I]: %.2f hours", elapsed_time / 3600))
  } else if (elapsed_time >= 60) {
    log_message(sprintf("Time to run [module I]: %.2f minutes", elapsed_time / 60))
  } else {
    log_message(sprintf("Time to run [module I]: %.2f seconds", elapsed_time))
  }

  # Returns the OBJ with the signature
  return(result)
}

#' Evaluate a molecular signature with survival analysis (module II)
#'
#' @description
#' Calculates sample-level signature scores and evaluates their association with overall survival.
#' Optionally adjusts for binary clinical covariates using multivariate Cox regression.
#'
#' @param data A data.frame with one row per sample and sample identifiers as row names.
#'   The first two columns contain the overall-survival event indicator and follow-up time, respectively;
#'   subsequent columns contain numeric expression values for the features used by the signature.
#' @param signature Either an object returned by \code{run_regression()} or a data.frame containing at least
#'   \code{feature} and \code{coefficient} columns.
#' @param output_prefix Character string used as the prefix for output files (default: \code{"clinprog"}).
#' @param norm_exp Logical. If \code{TRUE}, applies \code{log2(expression + 1)} to expression columns
#'   before score calculation (default: \code{FALSE}). Use the same setting used when deriving a signature
#'   from raw expression data.
#' @param run_multivariate Logical. If \code{TRUE}, attempts multivariate Cox analysis with clinical covariates
#'   (default: \code{FALSE}).
#' @param clinical_data A data.frame of clinical covariates, or \code{NULL}. Required when
#'   \code{run_multivariate = TRUE}. Row names must match the sample identifiers in \code{data}; each
#'   clinical variable must have exactly two categories.
#' @param score_cutoff Character string used to divide samples into low- and high-score groups. Accepted values
#'   are \code{"median"}, \code{"mean"}, \code{"signal"}, and \code{"roc"} (default: \code{"median"}).
#' @param variance_filter Numeric variance-filter threshold in the interval \code{(0, 1]}
#'   (default: \code{0.01}).
#' @param max_follow_up Positive numeric maximum follow-up time, or \code{NULL} to disable follow-up truncation
#'   (default: \code{NULL}). Its units must match the time column in \code{data}.
#' @param p_value_cutoff Numeric univariate p-value threshold used to select clinical covariates for multivariate
#'   analysis, strictly between 0 and 1 (default: \code{0.2}).
#' @param bootstrap Positive integer number of bootstrap iterations used during the multivariate Cox procedure
#'   (default: \code{1}).
#' @param skip_survival_checks Logical. If \code{TRUE}, relaxes/skips survival-related filtering steps
#'   (default: \code{FALSE}). Use with care.
#' @param save_plots Logical. If \code{TRUE}, saves available survival and score plots (default: \code{FALSE}).
#' @param save_table Logical. If \code{TRUE}, writes score and survival-analysis tables (default: \code{FALSE}).
#' @param save_json Logical. If \code{TRUE}, writes execution parameters and session metadata to JSON (default: \code{FALSE}).
#' @param save_rds Logical. If \code{TRUE}, saves the result object as an RDS file (default: \code{FALSE}).
#' @param report_format Logical or character. Use \code{FALSE} to disable report generation,
#'   \code{TRUE} or \code{"PDF"} for PDF, or \code{"HTML"} for HTML (default: \code{FALSE}).
#'   Report rendering requires the package's Quarto template and a working Quarto installation.
#' @param verbose Logical or 0/1. If \code{TRUE} or \code{1}, prints progress messages; if \code{FALSE}
#'   or \code{0}, suppresses them (default: \code{1}).
#' @param save_log Logical. If \code{TRUE}, writes a log file (default: \code{FALSE}).
#'
#' @details
#' The function calculates a score from the supplied signature, defines low/high score groups according to
#' \code{score_cutoff}, and evaluates the score's association with survival. When requested, clinical
#' variables are screened using univariate Cox models before multivariate analysis. A ROC-based cutoff is
#' calculated when \code{score_cutoff = "roc"}.
#'
#' @return An object of class \code{"clinprog_survival"}, containing the normalized signature, numeric score
#'   cutoff, sample-level expression/survival data, survival results, optional clinical and ROC results,
#'   proportional-hazards results, plots (if requested), parameters, metadata, and the original call.
#'
#' @seealso \code{\link{run_regression}}, \code{\link{run_complete}}, \code{\link{plot_km}},
#'   \code{\link{plot_ph}}, \code{\link{plot_roc}}, \code{\link{plot_forest}},
#'   \code{\link{plot_swimmer}}, \code{\link{write_score}}, \code{\link{write_univariate}},
#'   \code{\link{write_multivariate}}, \code{\link{write_report}}.
#'
#' @examples
#' \dontrun{
#' data("toy_gene_expression", package = "clinprog")
#' data("toy_clinics", package = "clinprog")
#' fit <- run_regression(
#'   data = toy_gene_expression, feature_type = "gene", norm_exp = TRUE,
#'   bootstrap = 10, group_size = 5, n_cores = 1, seed = 123
#' )
#' surv <- run_survival(
#'   data = toy_gene_expression, signature = fit, norm_exp = TRUE,
#'   run_multivariate = TRUE, clinical_data = toy_clinics,
#'   score_cutoff = "median", bootstrap = 10
#' )
#' print(surv)
#' summary(surv)
#' plot(surv, type = "km")
#' }
#'
#' @export
run_survival <- function(data,
                         signature,
                         output_prefix = "clinprog",
                         norm_exp = FALSE,
                         run_multivariate = FALSE,
                         clinical_data = NULL,
                         score_cutoff = "median",
                         variance_filter = 0.01,
                         max_follow_up = NULL,
                         p_value_cutoff = 0.2,
                         bootstrap = 1,
                         skip_survival_checks = FALSE,
                         save_plots = FALSE,
                         save_table = FALSE,
                         save_json = FALSE,
                         save_rds = FALSE,
                         report_format = FALSE,
                         verbose = 1,
                         save_log = FALSE)
{
  # Start time counter
  start_time <- Sys.time()

  # Initializes specific multivariate objects
  multi_cox <- NULL
  multi_model <- NULL
  multi_cox_initial <- NULL

  # Checks LOG argument
  if (!is.logical(save_log) || length(save_log) != 1) {log_stop("Argument 'save_log' must be a boolean [TRUE or FALSE]")}

  # Checks verbosity and updates environment
  if (!verbose %in% c(0, 1, FALSE, TRUE) || length(verbose) != 1)
  {
    log_stop("Argument 'verbose' must be 0, 1, FALSE or TRUE")
  }
  .clinprog_env$verbose <- as.logical(verbose)

  # Checks output prefix argument
  if (!is.character(output_prefix) || length(output_prefix) != 1) {log_stop("Argument 'output_prefix' must be a string")}

  # Logging setup - creating
  start_msg <- paste0("[", .timestamp(), "] [INFO] clinprog survival log started")
  .clinprog_env$log_messages <- c(.clinprog_env$log_messages, start_msg)
  if (save_log)
  {
    logfile <- paste0(output_prefix, "_survival.log")
    .clinprog_env$log_con <- file(logfile, open = "wt")
    writeLines(start_msg, .clinprog_env$log_con)
  }

  # Logging setup - closing
  on.exit({
    finish_msg <- paste0("[", .timestamp(), "] [INFO] clinprog survival finished")
    .clinprog_env$log_messages <- c(.clinprog_env$log_messages, finish_msg)

    if (!is.null(.clinprog_env$log_con) && isOpen(.clinprog_env$log_con))
    {
      writeLines(finish_msg, .clinprog_env$log_con)
      close(.clinprog_env$log_con)
      .clinprog_env$log_con <- NULL
    }
  }, add = TRUE)

  # Handles 2 scenarios: (I) file PATH; (II) R data.frame
  log_message("Validating file parameters...")
  if (!is.data.frame(data)) {log_stop("'data' must be a data.frame")}

  # Validates file input parameters
  if (!is.logical(run_multivariate) || length(run_multivariate) != 1) {log_stop("Argument 'run_multivariate' must be a boolean [TRUE or FALSE]")}

  # Validates clinical input for run_multivariate analysis
  if (run_multivariate)
  {
    if (is.null(clinical_data)) {log_stop("'clinical_data' must be provided when 'run_multivariate = TRUE'")}
    if (!is.data.frame(clinical_data)) {log_stop("'clinical_data' must be a data.frame")}
  }
  log_message("Done.")

  # Loads molecular signature
  log_message("Loading signature...")

  # Handles 2 scenarios: (I) signature from module I (regression); (II) R data.frame
  if (inherits(signature, "clinprog_signature")) {
    raw_sig_df <- signature$signature
  } else if (is.data.frame(signature)) {
    raw_sig_df <- signature
  } else {
    log_stop("Invalid 'signature' input")
  }

  # Guarantees the signature is of class "clinprog_signature"
  raw_sig_df <- new_clinprog_signature(raw_sig_df)$signature
  log_message("Done.")

  # Adds extra column to associate the coefficient signal to its prognostic value
  raw_sig_df$prognostic <- ifelse(raw_sig_df$coefficient > 0, "worse", "better")

  # Writes updated signature table if requested
  if (save_table)
  {
    log_message("Writing updated signature table...")
    write_signature(raw_sig_df, paste0(output_prefix, "_signature_updated.tsv"))
    log_message("Done.")
  }

  # Gets only the first 2 relevant columns ('feature' and 'coefficient') for downstream analyses
  sig_df <- raw_sig_df[,c(1, 2)]

  # Adjusts feature names to match the format of those found in the survival/expression table
  raw_sig_df$feature <- gsub("-", "__", raw_sig_df$feature, fixed = TRUE)
  sig_df$feature <- gsub("-", "__", sig_df$feature, fixed = TRUE)

  # Validates other parameters
  log_message("Validating other parameters...")
  if (!is.numeric(variance_filter) || length(variance_filter) != 1 || variance_filter <= 0 || variance_filter > 1)
  {
    log_stop("Argument 'variance_filter' must be a positive numeric value in range (0, 1]")
  }
  if (!is.null(max_follow_up) && (!is.numeric(max_follow_up) || length(max_follow_up) != 1 || max_follow_up <= 0))
  {
    log_stop("Argument 'max_follow_up' must be a positive numeric value")
  }
  if (!is.numeric(p_value_cutoff) || length(p_value_cutoff) != 1 || p_value_cutoff <= 0 || p_value_cutoff >= 1)
  {
    log_stop("'p_value_cutoff' must be a numeric value between 0 and 1")
  }
  if (!is.numeric(bootstrap) || length(bootstrap) != 1 || !is.finite(bootstrap) ||
      bootstrap < 1 || bootstrap %% 1 != 0)
  {
    log_stop("Argument 'bootstrap' must be a positive integer")
  }
  score_cutoff <- tolower(score_cutoff)
  if (!is.character(score_cutoff) || length(score_cutoff) != 1 ||
      !score_cutoff %in% c("roc", "median", "mean", "signal"))
  {
    log_stop("Argument 'score_cutoff' must be one of: 'roc', 'median', 'mean', or 'signal'.")
  }
  if (!is.logical(norm_exp) || length(norm_exp) != 1) {log_stop("Argument 'norm_exp' must be a boolean [TRUE or FALSE]")}
  if (!is.logical(skip_survival_checks) || length(skip_survival_checks) != 1) {log_stop("Argument 'skip_survival_checks' must be a boolean [TRUE or FALSE]")}
  if (!is.logical(save_table) || length(save_table) != 1) {log_stop("Argument 'save_table' must be a boolean [TRUE or FALSE]")}
  if (!is.logical(save_json) || length(save_json) != 1) {log_stop("Argument 'save_json' must be a boolean [TRUE or FALSE]")}
  if (!is.logical(save_rds) || length(save_rds) != 1) {log_stop("Argument 'save_rds' must be a boolean [TRUE or FALSE]")}
  if (!is.logical(save_plots) || length(save_plots) != 1) {log_stop("Argument 'save_plots' must be a boolean [TRUE or FALSE]")}
  if (!(is.logical(report_format) || (is.character(report_format) && length(report_format) == 1 && toupper(report_format) %in% c("PDF", "HTML"))))
  {
    log_stop("'report_format' must be a boolean [TRUE or FALSE] or a character ['PDF' or 'HTML']")
  }
  log_message("Done.")

  # Loads INput with expression and survival data
  log_message("Reading input expression/survival file...")
  colnames(data) <- gsub("-", "__", colnames(data), fixed = TRUE)
  log_message("Done.")

  # Standardizes the column names of the survival metrics
  log_message("Renaming first two (survival) columns to 'OS' and 'OS.time'...")
  colnames(data)[1:2] <- c("OS", "OS.time")
  log_message("Done.")

  # Log2(Expression + 1) transforms expression data if requested
  if (norm_exp)
  {
    log_message("Transforming expression data with formula log2(expression + 1)...")
    gene_cols <- 3:ncol(data)
    data[, gene_cols] <- log2(data[, gene_cols] + 1)
    log_message("Done.")
  }

  # Validates input file
  log_message("Validating input...")

  # General structure
  if (ncol(data) < 3) {log_stop("Input data must contain at least two [OS, OS.time] survival columns and one feature")}

  # OS survival column
  os_values <- data[,1]
  if (!is.numeric(os_values)) {log_stop("Column [OS] must be numeric and the first in the table")}
  if (anyNA(os_values)) {log_stop("Column [OS] must be the first in the table and contain no missing values")}
  unique_os <- sort(unique(stats::na.omit(os_values)))
  if (!setequal(unique_os, c(0, 1))) {log_stop("Column [OS] must be the first in the table and contain only binary values: 0 and 1")}

  # OS.time survival column
  time_values <- data[,2]
  if (!is.numeric(time_values)) {log_stop("Column [OS.time] must be numeric and the second in the table")}
  if (anyNA(time_values)) {log_stop("Column [OS.time] must be the second in the table and contain no missing values")}
  if (any(time_values < 0, na.rm = TRUE)) {log_stop("Column [OS.time] must be the second in the table and contain only positive values")}
  log_message("Done.")

  # Applies maximum follow-up censoring if requested
  log_message("Applying maximum max_follow_up value...")
  data <- clinprog_followup(data = data, max_follow_up = max_follow_up)
  log_message("Done.")

  # Applies OS and OS.time filters if user set skip_survival_checks = FALSE (default)
  if (!skip_survival_checks)
  {
    perc <- sum(data$OS) / nrow(data)
    if (perc < 0.2 || perc > 0.8)
    {
      log_stop(paste0("Survival status proportion outside acceptable range [between 20-80%]. ",
                      "Consider other endpoints. If you want to continue anyway, set skip_survival_checks = T."))
    }
    fvar <- stats::var(data$OS.time / max(data$OS.time))
    if (fvar < variance_filter)
    {
      log_stop(paste0("Follow-up variance: ", fvar, " below threshold. Consider changing the 'variance_filter' parameter",
                      " or choosing other endpoints. If you want to continue anyway, set skip_survival_checks = T."))
    }
  }

  # Checks if all features in signature are present in the expression file
  log_message("Checking compatibility between expression/survival and signature files...")
  expr_features <- colnames(data)[-(1:2)]
  if (anyDuplicated(expr_features)) {log_stop("Duplicated features are not allowed in expression data")}
  if (anyDuplicated(sig_df$feature)) {log_stop("Duplicated features are not allowed in signature")}
  missing_features <- setdiff(sig_df$feature, expr_features)
  if (length(missing_features) > 0) {log_stop("Signature features missing in expression data: ", paste(missing_features, collapse = ", "))}
  log_message("Done.")

  # Reorders and subsets data according to signature features and order, essential to correct score calculation later
  data <- data[, c("OS", "OS.time", sig_df$feature), drop = FALSE]

  # Calculates clinprog score
  log_message("Calculating signature score...")

  # Reorders expression matrix according to signature features
  expr <- as.matrix(data[, sig_df$feature, drop = FALSE])

  # Explicitly aligns coefficients to expression columns
  match_idx <- match(colnames(expr), sig_df$feature)

  # Safety check
  if (anyNA(match_idx)) {log_stop("Expression matrix and signature coefficients are not aligned")}
  coef_vector <- sig_df$coefficient[match_idx]

  # Calculates signature score
  score <- expr %*% coef_vector

  # Ensures the calculated scores are numeric
  data$score <- as.numeric(score)
  log_message("Done.")

  # Categorizes the clinprog score into high/low groups
  log_message("Categorizing score...")

  # Based on user's choice, use either the ROC curve or the median or the mean or the signal
  if (score_cutoff == "roc")
  {
    # This value can be changed. By default, it uses the median max_follow_up time
    roc_obj <- survivalROC::survivalROC(Stime = data$OS.time, status = data$OS, marker = data$score,
                                        predict.time = stats::median(data$OS.time, na.rm = TRUE), method = "NNE",
                                        span = 0.25*nrow(data)^(-0.20))
    roc_cutoff <- clinprog_cutoff_roc(data, direction = ifelse(as.numeric(roc_obj$AUC) < 0.5, ">", "<"))
    score_cutoff_value <- roc_cutoff$cutoff
    log_message(paste0("ROC cutoff: ", round(score_cutoff_value, 4)))
  } else {
    if (score_cutoff == "median")
    {
      score_cutoff_value <- stats::median(data$score, na.rm = TRUE)
      log_message(paste0("Median cutoff: ", round(score_cutoff_value, 4)))
    } else {
      if (score_cutoff == "mean")
      {
        score_cutoff_value <- mean(data$score, na.rm = TRUE)
        log_message(paste0("Mean cutoff: ", round(score_cutoff_value, 4)))
      } else {
        if (score_cutoff == "signal")
        {
          score_cutoff_value <- 0
          log_message(paste0("Signal cutoff (positive vs. negative): ", round(score_cutoff_value, 4)))
        }
      }
    }
  }

  # Explicitly set the "high" group as REF to control for HR (CI 95%) values
  data$score_group <- ifelse(data$score > score_cutoff_value, "high", "low")
  data$score_group <- factor(data$score_group, levels = c("high", "low"))
  log_message("Done.")

  # Writes table with scores (both values and categories) if requested
  if (save_table)
  {
    log_message("Writing score table...")
    colnames(data)[colnames(data) == "score_group"] <- "Score"
    write_score(x = data, file = paste0(output_prefix, "_scoreCont.tsv"), include_clinical = FALSE)
    colnames(data)[colnames(data) == "Score"] <- "score_group"
    log_message("Done.")
  }

  # Performs univariate survival analysis
  log_message("Running univariate survival analysis...")

  # Runs log-rank test for score signatures
  res_logrank <- clinprog_uniCox_model(data)
  log_message("Done.")

  # Checks if model is valid
  if (is.null(res_logrank$table)) {log_stop("Cox model failed (convergence issue)")}

  # Writes table with univariate survival results if requested
  if (save_table)
  {
    log_message("Writing univariate survival results table...")
    res_logrank$table$feature[res_logrank$table$feature == "score"] <- "Score"
    write_univariate(res_logrank$table, paste0(output_prefix, "_uniCox.txt"))
    res_logrank$table$feature[res_logrank$table$feature == "Score"] <- "score"
    log_message("Done.")
  }

  # Performs multivariate survival analysis
  if (run_multivariate)
  {
    log_message("Running multivariate survival analysis...")

    # Checks if all samples are present in both datasets
    log_message("Checking compatibility between expression/survival and clinical files...")
    if (!identical(sort(rownames(clinical_data)), sort(rownames(data))))
    {
      miss_in_clin <- setdiff(rownames(data), rownames(clinical_data))
      miss_in_data <- setdiff(rownames(clinical_data), rownames(data))
      if (length(miss_in_clin) > 0) {log_stop("Samples missing in clinical data: ", paste(miss_in_clin, collapse = ", "))}
      if (length(miss_in_data) > 0) {log_stop("Samples missing in expression/survival data: ", paste(miss_in_data, collapse = ", "))}
    }
    log_message("Done.")

    # Reorders clinical data according to expression/survival data
    clinical_data <- clinical_data[rownames(data), , drop = FALSE]

    # Keeps only survival and score-related columns
    data_surv <- data[, c("OS", "OS.time", "score", "score_group"), drop = FALSE]

    # Merges survival/score data with clinical covariates
    clinical_data <- cbind(data_surv, clinical_data)

    # Validates if all clinical variables are binary categorical
    log_message("Checking if all variables in clinical file are binary...")
    for (i in seq_len(ncol(clinical_data))[-(1:4)])
    {
      if (nlevels(as.factor(clinical_data[, i])) != 2)
      {
        log_stop("Clinical variable '", colnames(clinical_data)[i], "' must contain exactly 2 categories.")
      }
    }
    log_message("Done.")

    # Defines clinical covariates
    log_message("Selecting covariables with multiple univariate Cox analyses...")
    uni_covariates <- colnames(clinical_data)[5:ncol(clinical_data)]

    # Runs multiple univariate Cox analyses
    univ_cox <- clinprog_uniCox_test(data = clinical_data, covariates = uni_covariates, p_value_cutoff = p_value_cutoff)

    # Selects covariates with p-value < user-defined cutoff (ex: 0.2)
    selected <- univ_cox[!is.na(univ_cox$Cox.pvalue) & univ_cox$Cox.pvalue < p_value_cutoff, , drop = FALSE]
    log_message("Done.")

    # Checks if any covariate passed univariate filtering
    if (nrow(selected) == 0)
    {
      log_warning("No covariables passed univariate filtering. Multivariate analysis could not be performed.")
    } else {
      # Transforms clinical covariate names
      selected_covariates <- vapply(selected$variable, function(var)
      {
        matched <- uni_covariates[vapply(uni_covariates, function(x) grepl(paste0("^", x), var), FUN.VALUE = logical(1))]
        if (length(matched) > 0) matched[[1]] else var
      }, FUN.VALUE = character(1))
      selected_covariates <- unique(selected_covariates)
      log_message("Covariables selected: ", paste(selected_covariates, collapse = ", "))

      # Runs initial multivariate Cox regression and builds initial model
      log_message("Building initial model...")
      multi_cox_initial <- clinprog_multiCox_test(data = clinical_data, univ_result = univ_cox, logrank_result = res_logrank$table,
                                                  covariates = selected_covariates, all_covariates = uni_covariates,
                                                  use_bootstrap = TRUE, bootstrap = bootstrap, bootstrap_freq = 0.25, multi_p_value_cutoff = 0.1)
      if (multi_cox_initial$bootstrap.used)
      {
        log_message(paste0("Bootstrap resampling performed with ", bootstrap, " iterations."))
      } else {
        if (bootstrap == 1)
        {
          log_message(paste0("Bootstrap not performed. Default value: ", bootstrap, " used."))
        } else {
          log_message("Bootstrap failed. Resuming analysis with no resampling...")
        }
      }
      log_message("Done.")

      # Tests proportional hazards assumptions
      log_message("Testing proportional hazards assumptions...")
      ph_test <- clinprog_schoenfeld(model_object = multi_cox_initial$model, covariates = multi_cox_initial$covariates, is_multi = TRUE)
      ph_covariates <- ph_test$variables
      log_message("Done.")

      # Checks if any covariate passed proportional hazards assumptions
      if (length(ph_covariates) == 0 || (length(ph_covariates) == 1 && ph_covariates == "score_group"))
      {
        log_warning("No covariables passed PH assumptions. Multivariate analysis could not be performed.")
      } else {
        # Runs final multivariate Cox regression and builds final model
        log_message("Building final model...")
        multi_cox <- clinprog_multiCox_test(data = clinical_data, univ_result = univ_cox, logrank_result = res_logrank$table,
                                            covariates = ph_covariates, all_covariates = uni_covariates, use_bootstrap = FALSE)
        multi_model <- multi_cox$model
        log_message("Done.")

        # Writes tables with multivariate survival results and score + clinical variables if requested
        if (save_table)
        {
          log_message("Writing clinical and multivariate survival results tables...")
          colnames(clinical_data)[colnames(clinical_data) == "score_group"] <- "Score"
          write_score(x = clinical_data, file = paste0(output_prefix, "_scoreCat.tsv"), include_clinical = TRUE)
          colnames(clinical_data)[colnames(clinical_data) == "Score"] <- "score_group"
          multi_cox$table$variable[multi_cox$table$variable == "score_group"] <- "Score"
          write_multivariate(multi_cox$table, paste0(output_prefix, "_multiCox.txt"))
          log_message("Done.")
        }
      }
    }
  }

  # Produces uni and multivariate plots
  plot_list <- NULL
  if (save_plots)
  {
    log_message("Making plots from univariate and multivariate (if applicable) survival analyses...")
    plot_list <- list(
      km_plot = plot_km(data = data, score_cutoff_value = score_cutoff_value, output_prefix = output_prefix,
                        p_value = paste0("p = ", res_logrank$table$log.rank.pvalue,
                                         "\nHR: ", res_logrank$table$hazard.ratio)),
      swim_plot = plot_swimmer(data = data, score_cutoff_value = score_cutoff_value, output_prefix = output_prefix,
                               facet_by = "score_group"),
      ph_plot = if (!is.null(multi_model)) plot_ph(x = ph_test, is_multi = TRUE,
                                                   output_prefix = output_prefix) else plot_ph(x = res_logrank$ph,
                                                                                       output_prefix = output_prefix),
      box_plot = plot_boxplot(data = data, signature = raw_sig_df, output_prefix = output_prefix, norm_exp = FALSE,
                              y_label = if (norm_exp) "log2(Expression + 1)" else "Expression"),
      scatter_plot = plot_scatter(data = data, signature = raw_sig_df, output_prefix = output_prefix, norm_exp = FALSE,
                                  y_label = if (norm_exp) "log2(Expression + 1)" else "Expression"),
      wordcloud_plot = plot_wordcloud(data = data, signature = raw_sig_df, output_prefix = output_prefix),
      score_bar_plot = plot_barplot_score(data = data, score_cutoff_value = score_cutoff_value, output_prefix = output_prefix),
      line_plot = plot_lineplot_score(data = data, score_cutoff_value = score_cutoff_value, output_prefix = output_prefix),
      roc_plot = if (score_cutoff == "roc") plot_roc(x = roc_cutoff, output_prefix = output_prefix) else NULL,
      forest_plot = if (!is.null(multi_model)) plot_forest(model_object = multi_model,
                                                           output_prefix = output_prefix) else NULL,
      clinics_box_plot = if (!is.null(multi_model)) plot_clinics(data = clinical_data, output_prefix = output_prefix) else NULL,
      bar_plot = if (!is.null(multi_cox_initial$model) &&
                     isTRUE(multi_cox_initial$bootstrap.used) &&
                     !is.null(multi_cox_initial$bootstrap.table)) plot_barplot(plot_df = multi_cox_initial$bootstrap.table,
                                                                               output_prefix = output_prefix, type = "absolute",
                                                                               bootstrap = bootstrap,
                                                                               frequency_threshold = 25) else NULL)
    log_message("Done.")
  }

  # Creates S3 object containing the updated signature, tables, plots, and survival information
  # Listed parameters are only those directly affecting the scientific results/model
  result <- new_clinprog_survival(
    signature = raw_sig_df,
    score_cutoff_value = score_cutoff_value,
    expression_data = data,
    clinical_data = clinical_data,
    survival = list(
      univariate = res_logrank,
      boot_table = if (!is.null(multi_cox) && multi_cox_initial$bootstrap.used) multi_cox_initial$bootstrap.table else NULL,
      multivariate = if (!is.null(multi_cox)) multi_cox else NULL
    ),
    roc_result = if (score_cutoff == "roc") roc_cutoff else NULL,
    ph_result = if (!is.null(multi_cox)) ph_test else res_logrank$ph,
    plots = plot_list,
    params = list(
      norm_exp = norm_exp,
      score_cutoff = score_cutoff,
      run_multivariate = run_multivariate,
      variance_filter = variance_filter,
      max_follow_up = max_follow_up,
      p_value_cutoff = p_value_cutoff,
      bootstrap = bootstrap,
      skip_survival_checks = skip_survival_checks
    ),
    metadata = list(
      timestamp = Sys.time(),
      R_version = R.version.string,
      package_version = utils::packageVersion("clinprog")
    ),
    call = match.call()
  )

  # Writes execution metadata
  if (save_json)
  {
    log_message("Writing JSON metadata...")
    write_metadata(
      module = "run_survival",
      output_prefix = output_prefix,
      parameters = list(
        data = "data.frame",
        clinical_data = "data.frame",
        signature = class(signature)[1],
        output_prefix = output_prefix,
        norm_exp = norm_exp,
        run_multivariate = run_multivariate,
        score_cutoff = score_cutoff,
        variance_filter = variance_filter,
        max_follow_up = max_follow_up,
        p_value_cutoff = p_value_cutoff,
        bootstrap = bootstrap,
        skip_survival_checks = skip_survival_checks,
        save_plots = save_plots,
        save_table = save_table,
        save_json = save_json,
        save_rds = save_rds,
        report_format = report_format,
        verbose = verbose,
        save_log = save_log
      ),
      command = "none"
    )
    log_message("Done.")
  }

  # Writes RDS object
  if (save_rds)
  {
    log_message("Writing RDS object...")
    write_rds(object = result, file = paste0(output_prefix, "_survival.rds"))
    log_message("Done.")
  }

  # Generates report_format if requested
  report_ext <- if (is.character(report_format) && toupper(report_format) == "HTML") ".html" else ".pdf"
  if (!isFALSE(report_format))
  {
    log_message("Writing clinprog report_format...")
    write_report(object = result, format = report_format, file = paste0(output_prefix, "_report_survival", report_ext))
    log_message("Done.")
  }

  # End time counter
  elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  # Prints elapsed time
  if (elapsed_time >= 3600) {
    log_message(sprintf("Time to run [module II]: %.2f hours", elapsed_time / 3600))
  } else if (elapsed_time >= 60) {
    log_message(sprintf("Time to run [module II]: %.2f minutes", elapsed_time / 60))
  } else {
    log_message(sprintf("Time to run [module II]: %.2f seconds", elapsed_time))
  }

  # Returns the OBJ with the signature and survival results
  return(result)
}

#' Run the complete clinprog workflow
#'
#' @description
#' Runs signature discovery with \code{run_regression()} and passes the resulting signature to
#' \code{run_survival()} for prognostic evaluation.
#'
#' @param data A data.frame with one row per sample and sample identifiers as row names. The first two columns
#'   contain the overall-survival event indicator and follow-up time, respectively; remaining columns contain
#'   numeric gene- or transcript-level expression values.
#' @param output_prefix Character string used as the prefix for output files (default: \code{"clinprog"}).
#' @param norm_exp Logical. If \code{TRUE}, applies \code{log2(expression + 1)} to expression columns
#'   in both modules (default: \code{FALSE}).
#' @param bootstrap Positive integer. Number of bootstrap iterations for regression and, if requested,
#'   multivariate survival analysis (default: \code{1}).
#' @param group_size Non-negative integer specifying the number of features sampled per regression iteration
#'   (default: \code{10}).
#' @param percentage_filter Numeric correlation-filter threshold in \code{(0, 1]} for regression
#'   (default: \code{0.3}).
#' @param variance_filter Numeric variance-filter threshold in \code{(0, 1]} for the two modules
#'   (default: \code{0.01}).
#' @param max_follow_up Positive numeric maximum follow-up time or \code{NULL} to disable follow-up truncation
#'   (default: \code{NULL}). Units must match the time column in \code{data}.
#' @param feature_type Character string specifying \code{"gene"} or \code{"transcript"} for signature discovery
#'   (default: \code{"gene"}).
#' @param min_signature_size Positive integer or \code{NULL}. If supplied, searches for a shorter signature
#'   with a univariate log-rank p-value below 0.05 on the same dataset (default: \code{NULL}).
#' @param min_signature_cutoff Character string or \code{NULL}. Score cutoff strategy for the minimum-signature
#'   search: \code{"median"}, \code{"mean"}, \code{"signal"}, or \code{"roc"} (default: \code{NULL}).
#' @param run_multivariate Logical. If \code{TRUE}, attempts multivariate survival analysis (default: \code{FALSE}).
#' @param clinical_data A data.frame or \code{NULL}. Required for multivariate analysis. Row names must match
#'   the sample identifiers in \code{data}, and each clinical variable must contain exactly two categories.
#' @param score_cutoff Character string used to divide samples into low/high score groups:
#'   \code{"median"}, \code{"mean"}, \code{"signal"}, or \code{"roc"} (default: \code{"median"}).
#' @param p_value_cutoff Numeric univariate p-value threshold for clinical-covariate screening, strictly between
#'   0 and 1 (default: \code{0.2}).
#' @param skip_survival_checks Logical. If \code{TRUE}, relaxes/skips survival-related filtering steps
#'   in the two modules (default: \code{FALSE}). Use with care.
#' @param save_plots Logical. If \code{TRUE}, saves available plots from both modules (default: \code{FALSE}).
#' @param save_table Logical. If \code{TRUE}, writes available signature, score, and survival tables (default: \code{FALSE}).
#' @param save_json Logical. If \code{TRUE}, writes execution parameters and session metadata to JSON (default: \code{FALSE}).
#' @param save_rds Logical. If \code{TRUE}, saves the complete result object as an RDS file (default: \code{FALSE}).
#' @param report_format Logical or character. Use \code{FALSE} to disable report generation,
#'   \code{TRUE} or \code{"PDF"} for PDF, or \code{"HTML"} for HTML (default: \code{FALSE}).
#' @param verbose Logical or 0/1. If \code{TRUE} or \code{1}, prints progress messages; if \code{FALSE}
#'   or \code{0}, suppresses them (default: \code{1}).
#' @param n_cores Positive integer number of CPU cores used for bootstrap regression (default: \code{1}).
#' @param seed Integer seed used by \code{run_regression()} for stochastic regression operations (default: \code{123}).
#'   This argument does not control stochastic steps in \code{run_survival()}.
#' @param save_log Logical. If \code{TRUE}, writes log files for both modules (default: \code{FALSE}).
#'
#' @details
#' The signature created by \code{run_regression()} is automatically supplied to \code{run_survival()}.
#' The \code{seed} argument controls \code{run_regression()} only; it does not make the complete workflow
#' fully deterministic when stochastic procedures are used by the survival module.
#'
#' @return An object of class \code{"clinprog_complete"}, containing the regression and survival result
#'   objects, shared analysis parameters, metadata, and the original call.
#'
#' @seealso \code{\link{run_regression}}, \code{\link{run_survival}}, \code{\link{run_app}}.
#'
#' @examples
#' \dontrun{
#' data("toy_gene_expression", package = "clinprog")
#' data("toy_clinics", package = "clinprog")
#' result <- run_complete(
#'   data = toy_gene_expression,
#'   norm_exp = TRUE,
#'   feature_type = "gene",
#'   bootstrap = 10,
#'   group_size = 5,
#'   run_multivariate = FALSE,
#'   n_cores = 1,
#'   seed = 123
#' )
#' print(result)
#' summary(result)
#' }
#'
#' @export
run_complete <- function(data,
                         output_prefix = "clinprog",
                         norm_exp = FALSE,
                         bootstrap = 1,
                         group_size = 10,
                         percentage_filter = 0.3,
                         variance_filter = 0.01,
                         max_follow_up = NULL,
                         feature_type = "gene",
                         min_signature_size = NULL,
                         min_signature_cutoff = NULL,
                         run_multivariate = FALSE,
                         clinical_data = NULL,
                         score_cutoff = "median",
                         p_value_cutoff = 0.2,
                         skip_survival_checks = FALSE,
                         save_plots = FALSE,
                         save_table = FALSE,
                         save_json = FALSE,
                         save_rds = FALSE,
                         report_format = FALSE,
                         verbose = 1,
                         n_cores = 1,
                         seed = 123,
                         save_log = FALSE)
{
  # Reset LOG
  .clinprog_env$log_messages <- character()

  # Checks LOG argument
  if (!is.logical(save_log) || length(save_log) != 1) {log_stop("Argument 'save_log' must be a boolean [TRUE or FALSE]")}

  # Checks verbosity and updates environment
  if (!verbose %in% c(0, 1, FALSE, TRUE) || length(verbose) != 1)
  {
    log_stop("Argument 'verbose' must be 0, 1, FALSE or TRUE")
  }
  .clinprog_env$verbose <- as.logical(verbose)

  # Start time counter
  start_time <- Sys.time()

  log_message("clinprog complete log started")

  # Runs module 1 (regression)
  log_message("=============== clinprog: module I - regression ===============")
  regression_result <- run_regression(
    data = data,
    output_prefix = output_prefix,
    norm_exp = norm_exp,
    bootstrap = bootstrap,
    group_size = group_size,
    percentage_filter = percentage_filter,
    variance_filter = variance_filter,
    max_follow_up = max_follow_up,
    feature_type = feature_type,
    min_signature_size = min_signature_size,
    min_signature_cutoff = min_signature_cutoff,
    skip_survival_checks = skip_survival_checks,
    save_plots = save_plots,
    save_table = save_table,
    save_json = save_json,
    save_rds = save_rds,
    report_format = report_format,
    verbose = verbose,
    n_cores = n_cores,
    seed = seed,
    save_log = save_log
  )
  log_message("============================================================")

  # Runs module 2 (survival)
  log_message("=============== clinprog: module II - survival ===============")
  survival_result <- run_survival(
    data = data,
    signature = regression_result,
    output_prefix = output_prefix,
    norm_exp = norm_exp,
    run_multivariate = run_multivariate,
    clinical_data = clinical_data,
    score_cutoff = score_cutoff,
    variance_filter = variance_filter,
    max_follow_up = max_follow_up,
    p_value_cutoff = p_value_cutoff,
    bootstrap = bootstrap,
    skip_survival_checks = skip_survival_checks,
    save_plots = save_plots,
    save_table = save_table,
    save_json = save_json,
    save_rds = save_rds,
    report_format = report_format,
    verbose = verbose,
    save_log = save_log
  )
  log_message("============================================================")

  # Creates final S3 object
  # Listed parameters are only those directly affecting the scientific results/model
  result <- new_clinprog_complete(
    regression = regression_result,
    survival = survival_result,
    params = list(
      norm_exp = norm_exp,
      bootstrap = bootstrap,
      group_size = group_size,
      percentage_filter = percentage_filter,
      variance_filter = variance_filter,
      max_follow_up = max_follow_up,
      feature_type = feature_type,
      min_signature_size = min_signature_size,
      min_signature_cutoff = min_signature_cutoff,
      run_multivariate = run_multivariate,
      score_cutoff = score_cutoff,
      p_value_cutoff = p_value_cutoff,
      skip_survival_checks = skip_survival_checks,
      n_cores = n_cores,
      seed = seed
    ),
    metadata = list(
      timestamp = Sys.time(),
      R_version = R.version.string,
      package_version = utils::packageVersion("clinprog")
    ),
    call = match.call()
  )

  # Writes execution metadata
  if (save_json)
  {
    log_message("Writing JSON metadata...")
    write_metadata(
      module = "run_complete",
      output_prefix = output_prefix,
      parameters = list(
        data = "data.frame",
        clinical_data = if (is.null(clinical_data)) {NULL} else {"data.frame"},
        signature = "data.frame",
        output_prefix = output_prefix,
        norm_exp = norm_exp,
        run_multivariate = run_multivariate,
        score_cutoff = score_cutoff,
        group_size = group_size,
        percentage_filter = percentage_filter,
        variance_filter = variance_filter,
        max_follow_up = max_follow_up,
        p_value_cutoff = p_value_cutoff,
        bootstrap = bootstrap,
        feature_type = feature_type,
        min_signature_size = min_signature_size,
        min_signature_cutoff = min_signature_cutoff,
        skip_survival_checks = skip_survival_checks,
        save_plots = save_plots,
        save_table = save_table,
        save_json = save_json,
        save_rds = save_rds,
        report_format = report_format,
        verbose = verbose,
        n_cores = n_cores,
        seed = seed,
        save_log = save_log
      ),
      command = "none"
    )
    log_message("Done.")
  }

  # Writes RDS object
  if (save_rds)
  {
    log_message("Writing RDS object...")
    write_rds(object = result, file = paste0(output_prefix, "_complete.rds"))
    log_message("Done.")
  }

  # Generates report_format if requested
  report_ext <- if (is.character(report_format) && toupper(report_format) == "HTML") ".html" else ".pdf"
  if (!isFALSE(report_format))
  {
    log_message("Writing clinprog report_format...")
    write_report(object = result, format = report_format, file = paste0(output_prefix, "_report_complete", report_ext))
    log_message("Done.")
  }

  # End time counter
  elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  # Prints elapsed time
  if (elapsed_time >= 3600) {
    log_message(sprintf("Time to run [complete workflow]: %.2f hours", elapsed_time / 3600))
  } else if (elapsed_time >= 60) {
    log_message(sprintf("Time to run [complete workflow]: %.2f minutes", elapsed_time / 60))
  } else {
    log_message(sprintf("Time to run [complete workflow]: %.2f seconds", elapsed_time))
  }
  log_message("clinprog complete finished")

  # Returns object
  return(result)
}
