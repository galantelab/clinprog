#' Run clinprog pipeline - module 1 (regression)
#'
#' @description
#' Main function for finding a molecular signature using bootstrap resampling and penalized (LASSO) Cox regression.
#'
#' @param data Character or data.frame. A data.frame or a path to a tab-separated file containing survival data
#'   (e.g., OS, OS.time) and expression values (genes or transcripts). Example: \code{"myPATH/myTPM.tsv"}.
#' @param output_prefix Character. Output prefix used for generated files (Default: \code{"clinprog"}).
#' @param norm_exp Logical. If \code{TRUE}, log2(exp+1) transforms expression data (Default: \code{FALSE}).
#' @param bootstrap Positive integer. Number of bootstrap iterations (minimum: \code{1}; default: \code{1}).
#' @param group_size Integer. Number of genes/transcripts sampled per iteration (Default: \code{10}).
#' @param percentage_filter Numeric. Correlation filter threshold (0–1; Default: \code{0.3}).
#' @param variance_filter Numeric. Variance filter threshold (0–1; Default: \code{0.01}).
#' @param max_follow_up Numeric or \code{NULL}. Maximum follow up time (Default: \code{NULL}).
#' @param feature_type Character. Feature type: either \code{"gene"} (default) or \code{"transcript"}.
#' @param min_signature_size Integer or \code{NULL} (disabled). If provided, after the bootstrap/Cox LASSO regression
#'   step, searches for the smallest signature (top-ranked features by absolute coefficient) that still yields a
#'   significant univariate log-rank test on the same dataset. The search starts at \code{min_signature_size} features
#'   and increases one feature at a time until the log-rank p-value drops below \code{0.05}.
#' @param min_signature_cutoff Character or \code{NULL} (default). Strategy used to split scores into low/high
#'   groups during the minimum signature search. One of \code{"median"}, \code{"mean"}, \code{"signal"}, or
#'   \code{"roc"}.
#' @param skip_survival_checks Logical. If \code{TRUE}, overrides filtering steps related to survival assumptions (Default: \code{FALSE}).
#' @param save_plots Logical. If \code{TRUE}, generates histogram and lollipop plots (Default: \code{FALSE}).
#'
#'   These can also be generated later using \code{plot_histogram()} and \code{plot_lollipop()}.
#' @param save_table Logical. If \code{TRUE}, writes signature table to disk (Default: \code{FALSE}).
#'
#'   This can also be done later using \code{write_signature()} for the signature file.
#' @param save_json Logical. If \code{TRUE}, saves to disk the metadata in a JSON file (Default: \code{FALSE}).
#'
#'   This can also be done later using \code{write_metadata()} for a JSON with metadata.
#' @param save_rds Logical. If \code{TRUE}, saves to disk the R object in a RDS file (Default: \code{FALSE}).
#'
#'   This can also be done later using \code{write_rds()}.
#' @param report_format Logical or character. Whether to generate a final analysis report_format. Accepted values are:
#' \itemize{
#'   \item \code{FALSE}: Do not generate a report_format (default).
#'   \item \code{TRUE}: Generate a PDF report_format.
#'   \item \code{"PDF"}: Generate a PDF report_format.
#'   \item \code{"HTML"}: Generate an HTML report_format.
#' }
#'
#'   This can also be done later using \code{write_report()}.
#' @param verbose Integer or Logical. If \code{1} (or \code{TRUE}), prints progress messages to the console. On the
#'   other hand, if \code{0} (or \code{FALSE}), runs silently (Default: \code{1}).
#' @param save_log Logical. If \code{TRUE}, writes a minimalist log file tracking the pipeline execution (Default: \code{FALSE}).
#'   This can also be done later using \code{write_log()}.
#' @param n_cores Integer. Number of CPU cores used for parallel bootstrap regression
#'   (Default: \code{1}, i.e. sequential execution).
#' @param seed Integer or \code{NULL}. Random seed controlling every stochastic step in the pipeline
#'   (missing-value imputation, bootstrap feature subsampling, and LASSO cross-validation folds), so
#'   that results are reproducible across runs for the same input and parameters (Default: \code{123}).
#'
#' @details
#' The clinprog pipeline applies a sequence of preprocessing and modeling steps:
#' \enumerate{
#'   \item Checking minimum requirements in input dataset and parameters
#'   \item Max max_follow_up definition to control for survival analysis
#'   \item Variance filtering to remove low-variance features
#'   \item Proportional hazards assumption testing (Schoenfeld residuals)
#'   \item Optional simplified regression (if dataset is small)
#'   \item Bootstrap resampling with LASSO Cox regression
#'   \item Features (pairs) correlation filtering for every iteration
#'   \item Custom coefficient filtering for genes/transcripts
#'   \item Aggregation and ranking of selected features
#'   \item Optional search for the most parsimonious signature with significant results (top-k prefix search)
#' }
#'
#' The final signature consists of features with stable and non-zero coefficients across bootstrap iterations.
#'
#' @return
#' An S3 object of class \code{"clinprog_signature"} containing:
#' \itemize{
#'   \item \code{signature}: data.frame with selected features and coefficients
#'   \item \code{save_plots}: list of generated plots (if requested)
#'   \item \code{params}: list of parameters used in the analysis
#'   \item \code{metadata}: list of metadata information such as timestamp, R version, and clinprog package version
#'   \item \code{call}: original function call
#' }
#'
#' @note
#' Runtime may increase substantially with larger values of \code{bootstrap} (e.g., >1000 iterations) in combination with \code{group_size}.
#'   Large dataset sizes (samples and/or features) may also significantly increase runtime.
#'
#'   For exploratory analyses, smaller values are recommended.
#'   Input data must be properly formatted and free of missing or infinite values after preprocessing steps to avoid errors.
#'
#' Set \code{norm_exp} parameter to \code{TRUE} if a log2(expression + 1) transformation on expression data is required.
#' Otherwise, keep it \code{FALSE}. Log2-transformed expression data are highly recommended for survival analyses.
#'
#' Change \code{percentage_filter} and \code{variance_filter} parameters to make the filters more or less flexible and,
#' as a result, obtaining bigger or smaller signatures.
#'
#' Change \code{max_follow_up} to adjust the maximum max_follow_up. Ideal when time-specific survival analyses are needed.
#' For instance, 1-year OS, or 3-year OS, or 5-year OS or even 10-year OS.
#'
#' Careful with the \code{feature_type} parameter to get proper results, depending on the input (genes or transcripts).
#'
#' Keeping \code{skip_survival_checks = FALSE} parameter is recommended, especially if other endpoints can be evaluated.
#'
#' Setting \code{min_signature_size} is useful when the user wants to obtain the smallest signature that still shows a
#' significant univariate association with survival, trading off statistical power for parsimony. This search uses the
#' same dataset used to derive the signature, so it should be interpreted as a heuristic rather than a formal validation.
#'
#' Set \code{seed} parameter to \code{NULL} to disable seeding and restore the previous (non-reproducible) behavior.
#' The seed is applied locally and the caller's R session RNG state is restored afterwards.
#'
#' @seealso
#' Project Github repository: \url{https://github.com/galantelab/clinprog} \cr
#'
#' Documentation and tutorials: \url{https://galantelab.github.io/clinprog/} \cr
#'
#' Other functions: \code{\link{plot_histogram}}, \code{\link{plot_lollipop}}, \code{\link{read_table}},
#' \code{\link{read_rds}}, \code{\link{write_rds}}, \code{\link{write_metadata}},
#' \code{\link{write_signature}}, \code{\link{write_report}}, and \code{\link{write_log}}.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' result <- run_regression(
#'   data = "expression_data.tsv",
#'   output_prefix = "my_analysis",
#'   norm_exp = TRUE,
#'   bootstrap = 100,
#'   group_size = 10,
#'   percentage_filter = 0.3,
#'   variance_filter = 0.01,
#'   max_follow_up = NULL,
#'   feature_type = "gene",
#'   min_signature_size = NULL,
#'   min_signature_cutoff = NULL,
#'   skip_survival_checks = TRUE,
#'   save_plots = TRUE,
#'   save_table = TRUE,
#'   save_json = TRUE,
#'   save_rds = TRUE,
#'   report_format = TRUE,
#'   verbose = 1,
#'   n_cores = 4,
#'   seed = 123,
#'   save_log = TRUE
#' )
#'
#' # Access signature through custom clinprog S3 methods
#' as.data.frame(result)
#' summary(result)
#' print(result)
#' coef(result)
#' plot(result)
#'
#' # Load previous result saved in RDS
#' read_rds(file = "my_analysis.rds")
#'
#' # Import expression and survival data manually
#' full_data <- read_table("expression_data.tsv", row.names = TRUE)
#'
#' # Generate plots later
#' plot_histogram(result$signature, "my_analysis_hist")
#' plot_lollipop(result$signature, "my_analysis_lolli")
#'
#' # Export signature manually
#' write_signature(result, "my_analysis_signature.txt")
#'
#' # Export metadata manually
#' write_metadata(module = "run_regression", output_prefix = output_prefix,
#'                parameters = list(...), command = "none")
#'
#' # Save result in RDS
#' write_rds(object = result, file = "my_analysis.rds")
#'
#' # Save LOG file
#' write_log(file = "my_analysis.log")
#'
#' # Generate final report_format
#' write_report(object = result, format = report_format, file = "my_analysis_report.pdf")
#'
#' # Search for the smallest signature with significant univariate survival
#' result_min <- run_regression(
#'   data = "expression_data.tsv",
#'   output_prefix = "my_analysis_min",
#'   bootstrap = 100,
#'   min_signature_size = 2,
#'   min_signature_cutoff = "roc",
#'   skip_survival_checks = TRUE,
#'   n_cores = 4,
#'   seed = 123
#' )
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

#' Run clinprog pipeline - module 2 (survival analysis)
#'
#' @description
#' Applies a molecular signature to survival data, generating prognostic scores and performing
#' survival analyses (univariate and, optionally, multivariate Cox regression).
#'
#' @param data Character or data.frame. A data.frame or a path to a tab-separated file containing survival data
#'   (e.g., OS, OS.time) and expression values (genes or transcripts). Example: \code{"myPATH/myTPM.tsv"}.
#' @param signature A molecular signature. Can be:
#'   \itemize{
#'     \item An object of class \code{"clinprog_signature"} from the \code{run_regression()} clinprog function (regression - module I)
#'     \item A data.frame with columns \code{feature} and \code{coefficient}
#'     \item A path to a .tsv file containing the signature
#'   }
#' @param output_prefix Character. Output prefix used for generated files (Default: \code{"clinprog"}).
#' @param norm_exp Logical. If \code{TRUE}, log2(exp+1) transforms expression data (Default: \code{FALSE}).
#' @param run_multivariate Logical. If \code{TRUE}, performs multivariate survival analysis including clinical variables
#'   present in \code{clinical_data} (Default: \code{FALSE}).
#' @param clinical_data Character or data.frame or \code{NULL}. A data.frame or a path to a tab-separated file containing
#'   binary-categorized clinical data (e.g., Age, Gender, ...) (required if \code{run_multivariate = TRUE}).
#'   Example: \code{"myPATH/myClinics.tsv"}.
#' @param score_cutoff Character. Uses the \code{"median"}, the \code{"mean"}, the \code{"signal"}, or a \code{"roc"}
#'   curve to define the score cutoff (Default: \code{"median"}).
#' @param variance_filter Numeric. Minimum normalized variance for follow-up time (0–1; Default: \code{0.01}).
#' @param max_follow_up Numeric or \code{NULL}. Maximum max_follow_up time (Default: \code{NULL}).
#' @param p_value_cutoff Numeric. P-value threshold used to select clinical covariates from univariate Cox regression
#'   for inclusion in multivariate analysis (Default: \code{0.2}).
#' @param bootstrap Positive integer. Number of bootstrap iterations for multivariate Cox regression (minimum: \code{1}; default: \code{1}).
#' @param skip_survival_checks Logical. If \code{TRUE}, overrides filtering steps related to survival assumptions (Default: \code{FALSE}).
#' @param save_plots Logical. If \code{TRUE}, generates a Kaplan-Meier curve, the proportional hazards assumption plot,
#'   plots related to the signature created and the derived score, and extra figures,
#'   depending on the chosen parameters (Default: \code{FALSE}).
#'
#'   These can also be generated later using \code{plot_km()}, \code{plot_ph()}, \code{plot_swimmer()},
#'   \code{plot_boxplot()}, \code{plot_scatter()}, \code{plot_wordcloud()},
#'   \code{plot_barplot_score()}, and \code{plot_lineplot_score()}.
#'   If \code{score_cutoff = "roc"}, a ROC curve can also be generated using \code{plot_roc()}.
#'   If \code{run_multivariate = TRUE}, a forest plot can also be generated using \code{plot_forest()} and
#'   a boxplot of clinical variables with \code{plot_clinics()}.
#'   If bootstrap is performed, an extra summary barplot can also be generated using \code{plot_barplot()}.
#' @param save_table Logical. If \code{TRUE}, writes output tables to disk (Default: \code{FALSE}).
#'
#'   These can also be done later using \code{write_signature()} for an updated signature file,
#'   \code{write_score()} for a table showing the calculated clinprog scores, \code{write_univariate()}
#'   for a file containing the univariate survival results, and \code{write_multivariate()}
#'   for a file containing the multivariate survival results.
#' @param save_json Logical. If \code{TRUE}, saves to disk the metadata in a JSON file (Default: \code{FALSE}).
#'
#'   This can also be done later using \code{write_metadata()} for a JSON with metadata.
#' @param save_rds Logical. If \code{TRUE}, saves to disk the R object in a RDS file (Default: \code{FALSE}).
#'
#'   This can also be done later using \code{write_rds()} for a RDS object.
#' @param report_format Logical or character. Whether to generate a final analysis report_format. Accepted values are:
#' \itemize{
#'   \item \code{FALSE}: Do not generate a report_format (default).
#'   \item \code{TRUE}: Generate a PDF report_format.
#'   \item \code{"PDF"}: Generate a PDF report_format.
#'   \item \code{"HTML"}: Generate an HTML report_format.
#' }
#'
#'   This can also be done later using \code{write_report()}.
#' @param verbose Integer or Logical. If \code{1} (or \code{TRUE}), prints progress messages to the console. On the
#'   other hand, if \code{0} (or \code{FALSE}), runs silently (Default: \code{1}).
#' @param save_log Logical. If \code{TRUE}, writes a minimalist log file tracking the pipeline execution (Default: \code{FALSE}).
#'   This can also be done later using \code{write_log()}.
#'
#' @details
#' The clinprog pipeline applies a sequence of analyses steps:
#' \enumerate{
#'   \item Checking minimum requirements in input datasets and parameters
#'   \item Max max_follow_up definition to control for survival analysis
#'   \item Updating the molecular signature
#'   \item Creating the signature scores for each patient/sample
#'   \item Proportional hazards assumption testing (Schoenfeld residuals)
#'   \item Categorizing the scores into low/high groups
#'   \item Performing the univariate Cox regression for the score
#'   \item Running multiple univariate Cox regression for other clinical variables
#'   \item Performing the multivariate Cox regression with bootstrap resampling if the requirements are met
#' }
#'
#' The final complete results consist of multiple tables and figures to show the association between the clinprog scores,
#' created based on the molecular signature, and survival parameters, optionally correcting for clinical variables.
#'
#' @return
#' An S3 object of class \code{"clinprog_survival"} containing:
#' \itemize{
#'   \item \code{signature}: data.frame with selected features, coefficients, and extra information
#'   \item \code{score_cutoff}: cutoff used to create the low/high score groups
#'   \item \code{expression_data}: data.frame with scores and survival information for all patients/samples
#'   \item \code{clinical_data}: data.frame with score groups and clinical variables for all patients/samples (if requested)
#'   \item \code{survival}: survival analysis outputs
#'   \item \code{roc_result}: ROC analysis outputs (if requested)
#'   \item \code{ph_result}: proportional hazards analysis outputs
#'   \item \code{plots}: list of generated plots (if requested)
#'   \item \code{params}: list of parameters used in the analysis
#'   \item \code{metadata}: list of metadata information such as timestamp, R version, and clinprog package version
#'   \item \code{call}: original function call
#' }
#'
#' @note
#' Runtime may increase substantially with larger values of \code{bootstrap} (e.g., >1000 iterations) and
#' large dataset sizes (samples and/or features and clinical variables).
#'
#'   For exploratory analyses, smaller values are recommended.
#'   Input data must be properly formatted and free of missing or infinite values after preprocessing steps to avoid errors.
#'
#' Users are allowed and encouraged to further investigate the clinprog \code{signature} provided. One possibility is to
#' test smaller signatures (e.g., establishing a coefficient cutoff) and even to find a minimum signature related to
#' patient survival.
#'
#' Set \code{norm_exp} parameter to \code{TRUE} if a log2(expression + 1) transformation on expression data is required.
#' Otherwise, keep it \code{FALSE}. Log2-transformed expression data are highly recommended for survival analyses.
#'
#' Change \code{score_cutoff} to adjust the cutoff applied to the score for the survival analyses. Options include
#' simpler ones, such as \code{"median"} and \code{"mean"}. Also, \code{"signal"} can be used to split patients into
#' positive and negative scores to facilitate interpretation. For a more complex and robust metric, choose \code{"roc"}
#' to use a cutoff based on the ROC curve, which adopts the Nearest Neighbour Estimate (NNE) method and the
#' J's Youden statistics (sensitivity + (specificity -1) strategy)
#'
#' Change \code{max_follow_up} to adjust the maximum max_follow_up. Ideal when time-specific survival analyses are needed.
#' For instance, 1-year OS, or 3-year OS, or 5-year OS or even 10-year OS.
#'
#' Keeping \code{skip_survival_checks = FALSE} parameter is recommended, especially if other endpoints can be evaluated.
#' In case an error occurs, it is recommended to experiment changing \code{variance_filter} and \code{p_value_cutoff}
#' parameters first to make the follow up variance and variable selection filters more flexible.
#'
#' @seealso
#' Project Github repository: \url{https://github.com/galantelab/clinprog} \cr
#'
#' Documentation and tutorials: \url{https://galantelab.github.io/clinprog/} \cr
#'
#' Other functions: \code{\link{plot_km}}, \code{\link{plot_ph}}, \code{\link{plot_roc}},
#' \code{\link{plot_swimmer}}, \code{\link{plot_forest}}, \code{\link{plot_barplot}},
#' \code{\link{plot_clinics}}, \code{\link{plot_boxplot}}, \code{\link{plot_scatter}},
#' \code{\link{plot_wordcloud}}, \code{\link{plot_barplot_score}}, \code{\link{plot_lineplot_score}},
#' \code{\link{read_table}}, \code{\link{read_rds}}, \code{\link{write_rds}},
#' \code{\link{write_signature}}, \code{\link{write_score}}, \code{write_metadata()},
#' \code{\link{write_univariate}}, \code{\link{write_multivariate}},
#' \code{\link{write_report}}, and \code{\link{write_log}}
#'
#' @examples
#' \dontrun{
#' # Example usage
#' result <- run_survival(
#'   data = "expression_data.tsv",
#'   signature = "signature_data.tsv",
#'   output_prefix = "my_analysis",
#'   norm_exp = TRUE,
#'   run_multivariate = TRUE,
#'   clinical_data = "clinical_data.tsv",
#'   score_cutoff = "roc",
#'   variance_filter = 0.01,
#'   max_follow_up = NULL,
#'   p_value_cutoff = 0.2,
#'   bootstrap = 100,
#'   skip_survival_checks = TRUE,
#'   save_plots = TRUE,
#'   save_table = TRUE,
#'   save_json = TRUE,
#'   save_rds = TRUE,
#'   report_format = TRUE,
#'   verbose = 1,
#'   save_log = TRUE
#' )
#'
#' # Access signature through custom clinprog S3 methods
#' as.data.frame(result)
#' summary(result)
#' print(result)
#' coef(result)
#' plot(result)
#'
#' # Load previous result saved in RDS
#' read_rds(file = "my_analysis.rds")
#'
#' # Import signature data manually
#' raw_sig_df <- read_table("signature_data.tsv", row.names = FALSE)
#'
#' # Import expression and survival data manually
#' data <- read_table("expression_data.tsv", row.names = TRUE)
#'
#' # Import clinical data manually
#' clin <- read_table("clinical_data.tsv", row.names = TRUE)
#'
#' # Generate plots later (assuming multivariate analysis was performed)
#' plot_roc(x = result$roc_result, output_prefix = "my_analysis_roc")
#' plot_km(data = result$expression_data, cutoff = result$score_cutoff,
#'         pval = result$survival$univariate$table$log.rank.pvalue, output_prefix = "my_analysis_km")
#' plot_ph(x = result$ph_result, is_multi = TRUE, output_prefix = "my_analysis_phplot")
#' plot_forest(model_object = result$survival$multivariate$model, output_prefix = "my_analysis_forest")
#' plot_clinics(data = result$clinical_data, output_prefix = "my_analysis_clinics-boxplot")
#' plot_swimmer(data = result$expression_data, cutoff = result$score_cutoff,
#'              output_prefix = "my_analysis_swimmer", facet_by = "score_group")
#' plot_barplot(plot_df = result$survival$boot_table, output_prefix = "my_analysis_bar",
#'              type = "absolute", bootstrap = 100)
#' plot_scatter(data = result$expression_data, signature = result$signature,
#'              output_prefix = "my_analysis_scatterplot")
#' plot_boxplot(data = result$expression_data, signature = result$signature,
#'              output_prefix = "my_analysis_boxplot")
#' plot_wordcloud(data = result$expression_data, signature = result$signature,
#'                output_prefix = "my_analysis_wordcloud")
#' plot_barplot_score(data = result$clinical_data, score_cutoff = result$score_cutoff,
#'                    output_prefix = "my_analysis_score-bar")
#' plot_lineplot_score(data = result$clinical_data, score_cutoff = result$score_cutoff,
#'                     output_prefix = "my_analysis_lineplot")
#'
#' # Export signature manually
#' write_signature(result, "my_analysis_signature_updated.tsv")
#'
#' # Export scores manually
#' write_score(result, "my_analysis_scoreCont.tsv") # for univariate mode only
#' write_score(result, "my_analysis_scoreCat.tsv", include_clinical = TRUE) # if run_multivariate mode is on
#'
#' # Export survival results manually
#' write_univariate(result, "my_analysis_uniCox.txt") # for univariate mode only
#' write_multivariate(result, "my_analysis_multiCox.txt") # if run_multivariate mode is on
#'
#' # Export metadata manually
#' write_metadata(module = "run_survival", output_prefix = output_prefix,
#'                parameters = list(...), command = "none")
#'
#' # Save result in RDS
#' write_rds(object = result, file = "my_analysis.rds")
#'
#' # Save LOG file
#' write_log(file = "my_analysis.log")
#'
#' # Generate final report_format
#' write_report(object = result, format = report_format, file = "my_analysis_report.pdf")
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
  clin <- NULL

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

    # Reads clinical data
    log_message("Reading input clinical file...")
    if (is.character(clinical_data)) {clin <- read_table(clinical_data, sep = "\t")} else {clin <- clinical_data}
    log_message("Done.")

    # Checks if all samples are present in both datasets
    log_message("Checking compatibility between expression/survival and clinical files...")
    if (!identical(sort(rownames(clin)), sort(rownames(data))))
    {
      miss_in_clin <- setdiff(rownames(data), rownames(clin))
      miss_in_data <- setdiff(rownames(clin), rownames(data))
      if (length(miss_in_clin) > 0) {log_stop("Samples missing in clinical data: ", paste(miss_in_clin, collapse = ", "))}
      if (length(miss_in_data) > 0) {log_stop("Samples missing in expression/survival data: ", paste(miss_in_data, collapse = ", "))}
    }
    log_message("Done.")

    # Reorders clinical data according to expression/survival data
    clin <- clin[rownames(data), , drop = FALSE]

    # Keeps only survival and score-related columns
    data_surv <- data[, c("OS", "OS.time", "score", "score_group"), drop = FALSE]

    # Merges survival/score data with clinical covariates
    clin <- cbind(data_surv, clin)

    # Validates if all clinical variables are binary categorical
    log_message("Checking if all variables in clinical file are binary...")
    for (i in seq_len(ncol(clin))[-(1:4)])
    {
      if (nlevels(as.factor(clin[, i])) != 2)
      {
        log_stop("Clinical variable '", colnames(clin)[i], "' must contain exactly 2 categories.")
      }
    }
    log_message("Done.")

    # Defines clinical covariates
    log_message("Selecting covariables with multiple univariate Cox analyses...")
    uni_covariates <- colnames(clin)[5:ncol(clin)]

    # Runs multiple univariate Cox analyses
    univ_cox <- clinprog_uniCox_test(data = clin, covariates = uni_covariates, p_value_cutoff = p_value_cutoff)

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
      multi_cox_initial <- clinprog_multiCox_test(data = clin, univ_result = univ_cox, logrank_result = res_logrank$table,
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
        multi_cox <- clinprog_multiCox_test(data = clin, univ_result = univ_cox, logrank_result = res_logrank$table,
                                            covariates = ph_covariates, all_covariates = uni_covariates, use_bootstrap = FALSE)
        multi_model <- multi_cox$model
        log_message("Done.")

        # Writes tables with multivariate survival results and score + clinical variables if requested
        if (save_table)
        {
          log_message("Writing clinical and multivariate survival results tables...")
          colnames(clin)[colnames(clin) == "score_group"] <- "Score"
          write_score(x = clin, file = paste0(output_prefix, "_scoreCat.tsv"), include_clinical = TRUE)
          colnames(clin)[colnames(clin) == "Score"] <- "score_group"
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
      clinics_box_plot = if (!is.null(multi_model)) plot_clinics(data = clin, output_prefix = output_prefix) else NULL,
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
    clinical_data = clin,
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

#' Run clinprog complete pipeline
#'
#' @description Runs the complete clinprog workflow by sequentially executing:
#' \enumerate{
#'   \item \code{run_regression()} (module 1)
#'   \item \code{run_survival()} (module 2)
#' }
#'
#' @param data Character or data.frame. A data.frame or a path to a tab-separated expression/survival file
#' @param output_prefix Character. Output prefix used for generated files (Default: \code{"clinprog"}).
#' @param norm_exp Logical. If \code{TRUE}, log2(exp+1) transforms expression data in both \code{run_regression()}
#'   and \code{run_survival()} modules (Default: \code{FALSE}).
#' @param bootstrap Positive integer. Number of bootstrap iterations used in both \code{run_regression()} and
#'   \code{run_survival()} modules (minimum: \code{1}; default: \code{1}).
#' @param group_size Integer. Number of features per iteration in \code{run_regression()} module (Default: \code{10}).
#' @param percentage_filter Numeric. Correlation filter threshold used in \code{run_regression()} (Default: \code{0.3}).
#' @param variance_filter Numeric. Variance filter threshold used in both \code{run_regression()} and
#'   \code{run_survival()} modules (Default: \code{0.01}).
#' @param max_follow_up Numeric or \code{NULL}. Maximum max_follow_up time used in both \code{run_regression()} and
#'   \code{run_survival()} modules (Default: \code{NULL}).
#' @param feature_type Character. Feature feature_type: either \code{"gene"} (default) or \code{"transcript"} used in \code{run_regression()}.
#' @param min_signature_size Integer or \code{NULL} (disabled). Searches for the smallest signature that still yields
#'   a significant univariate log-rank test on the same dataset.
#' @param min_signature_cutoff Character or \code{NULL} (default). Strategy used to split scores into low/high
#'   groups during the minimum signature search.
#' @param run_multivariate Logical. If \code{TRUE}, performs run_multivariate survival analysis in
#'   \code{run_survival()} (Default: \code{FALSE}).
#' @param clinical_data Character or data.frame or \code{NULL}. A data.frame or a path to a tab-separated clinical file used
#'   for run_multivariate analysis.
#' @param score_cutoff Character. Uses the \code{"median"}, the \code{"mean"}, the \code{"signal"}, or a
#'   \code{"roc"} curve to define the score cutoff used in \code{run_survival()} (Default: \code{"median"}).
#' @param p_value_cutoff Numeric. Univariate covariate filtering cutoff used for run_multivariate analysis in
#'   \code{run_survival()} (Default: \code{0.2}).
#' @param skip_survival_checks Logical. If \code{TRUE}, overrides filtering steps related to survival assumptions in both
#'   modules (Default: \code{FALSE}).
#' @param save_plots Logical. If \code{TRUE}, generates plots from both modules (Default: \code{FALSE}).
#' @param save_table Logical. If \code{TRUE}, writes output tables from both modules (Default: \code{FALSE}).
#' @param save_json Logical. If \code{TRUE}, saves to disk the metadata in a JSON file (Default: \code{FALSE}).
#' @param save_rds Logical. If \code{TRUE}, saves to disk the R object in a RDS file (Default: \code{FALSE}).
#' @param report_format Logical or character. Report generation mode. Use \code{FALSE} (default) to disable report_format generation,
#'   \code{TRUE} or \code{"PDF"} to generate a PDF report_format, or \code{"HTML"} to generate an HTML report_format.
#' @param verbose Integer or Logical. Prints progress messages to the console if \code{1} (or \code{TRUE}) (default).
#'   On the other hand, if \code{0} (or \code{FALSE}), runs silently (Default: \code{1}).
#' @param save_log Logical. If \code{TRUE}, writes log files for both modules (Default: \code{FALSE}).
#' @param n_cores Integer. Number of CPU cores used for parallel bootstrap regression
#'   (Default: \code{1}, i.e. sequential execution).
#' @param seed Integer or \code{NULL}. Random seed controlling every stochastic step in both \code{run_regression()}
#'   and \code{run_survival()}, so that results are reproducible across runs for the same input and parameters
#'   Set to \code{NULL} to disable seeding (Default: \code{123}).
#'
#' @details
#' The molecular signature generated during the regression step is automatically passed to the survival analysis step.
#'
#' @return An S3 object of class \code{"clinprog_complete"} containing:
#' \itemize{
#'   \item \code{regression}: result from \code{run_regression()}
#'   \item \code{survival}: result from \code{run_survival()}
#'   \item \code{params}: list of parameters used in the analysis
#'   \item \code{metadata}: metadata information
#'   \item \code{call}: original function call
#' }
#'
#' @seealso
#' Project Github repository: \url{https://github.com/galantelab/clinprog} \cr
#'
#' Documentation and tutorials: \url{https://galantelab.github.io/clinprog/} \cr
#'
#' Main functions: \code{\link{run_regression}} (module I) and \code{\link{run_survival}} (module II)
#'
#' @examples
#' \dontrun{
#' # Example usage
#' result <- run_complete(
#'   data = "expression_data.tsv",
#'   output_prefix = "my_analysis",
#'   norm_exp = FALSE,
#'   bootstrap = 100,
#'   group_size = 10,
#'   percentage_filter = 0.3,
#'   variance_filter = 0.01,
#'   max_follow_up = NULL,
#'   feature_type = "gene",
#'   min_signature_size = NULL,
#'   min_signature_cutoff = NULL,
#'   run_multivariate = TRUE,
#'   clinical_data = "clinical_data.tsv",
#'   score_cutoff = "roc",
#'   p_value_cutoff = 0.2,
#'   skip_survival_checks = TRUE,
#'   save_plots = TRUE,
#'   save_table = TRUE,
#'   save_json = TRUE,
#'   save_rds = TRUE,
#'   report_format = TRUE,
#'   verbose = 1,
#'   n_cores = 1,
#'   seed = 123,
#'   save_log = TRUE
#' )
#'
#' # Explore complete analysis results
#' print(result)
#' summary(result)
#' coef(result)
#' plot(result)
#'
#' # Access module-specific results
#' result$regression
#' result$survival
#'
#' # Use module-specific S3 methods
#' summary(result$regression)
#' summary(result$survival)
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
