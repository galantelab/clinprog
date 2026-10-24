#!/usr/bin/env Rscript

library(argparse)
library(clinprog)

parser <- ArgumentParser(
  prog = "clinprog",
  description = "Bootstrap-Based Multivariate LASSO Cox Regression for Survival Signature Discovery"
)

subparsers <- parser$add_subparsers(
  dest = "sub_name",
  metavar = "<subcommand>",
  help = "choose only one option"
)

parser$add_argument(
  "-V", "--version",
  action = "version",
  version = "clinprog 0.3.0"
)

########## create the parser for the "REGRESSION" command ##########

parser_reg <- subparsers$add_parser(
  "regression",
  help = "generate a molecular signature through penalized LASSO Cox regression"
)

parser_reg$add_argument(
  "input",
  type = "character",
  metavar = "<input>",
  help = "tab separated values (tsv) file containing expression and survival parameters"
)

parser_reg$add_argument(
  "--output-prefix",
  type = "character",
  dest = "output_prefix",
  default = "clinprog",
  metavar = "STR",
  help = "output prefix used for generated files (default: %(default)s)"
)

parser_reg$add_argument(
  "--norm-exp",
  action = "store_true",
  dest = "norm_exp",
  default = FALSE,
  help = "normalize expression to log2(exp+1)"
)

parser_reg$add_argument(
  "--bootstrap",
  type = "integer",
  dest = "bootstrap",
  default = 1,
  metavar = "INT",
  help = "number of bootstrap iterations (default: %(default)s)"
)

parser_reg$add_argument(
  "--group-size",
  type = "integer",
  dest = "group_size",
  default = 10,
  metavar = "INT",
  help = "number of genes/transcripts sampled per iteration (default: %(default)s)"
)

parser_reg$add_argument(
  "--percentage-filter",
  type = "double",
  dest = "percentage_filter",
  default = 0.3,
  metavar = "DOUBLE",
  help = "correlation filter threshold (default: %(default)s)"
)

parser_reg$add_argument(
  "--variance-filter",
  type = "double",
  dest = "variance_filter",
  default = 0.01,
  metavar = "DOUBLE",
  help = "minimum normalized variance for follow-up time (default: %(default)s)"
)

parser_reg$add_argument(
  "--max-follow-up",
  type = "integer",
  dest = "max_follow_up",
  default = NULL,
  metavar = "INT",
  help = "max follow up time (default: %(default)s)"
)

parser_reg$add_argument(
  "--feature-type",
  type = "character",
  dest = "feature_type",
  choices = c("gene", "transcript"),
  default = "gene",
  help = "type of transcriptome data: gene or transcript (default: %(default)s)"
)

parser_reg$add_argument(
  "--min-signature-size",
  type = "integer",
  dest = "min_signature_size",
  default = NULL,
  metavar = "INT",
  help = "search for the smallest signature that still yields a significant univariate log-rank test on the same dataset"
)

parser_reg$add_argument(
  "--min-signature-cutoff",
  type = "integer",
  dest = "min_signature_cutoff",
  choices = c("median", "mean", "signal", "roc"),
  default = NULL,
  help = "strategy used to split scores into low/high groups during the minimum signature search"
)

parser_reg$add_argument(
  "--skip-survival-checks",
  action = "store_true",
  dest = "skip_survival_checks",
  default = FALSE,
  help = "override filtering steps related to survival assumptions"
)

parser_reg$add_argument(
  "--report-format",
  type = "character",
  dest = "report_format",
  choices = c("pdf", "html"),
  default = "pdf",
  help = "report format: pdf or html (default: %(default)s)"
)

parser_reg$add_argument(
  "--verbose",
  action = "store_true",
  dest = "verbose",
  default = FALSE,
  help = "print progress messages to the console"
)

parser_reg$add_argument(
  "--n_cores",
  type = "integer",
  dest = "n_cores",
  default = 1,
  metavar = "INT",
  help = "number of CPU cores used for parallel bootstrap regression (default: %(default)s)"
)

parser_reg$add_argument(
  "--seed",
  type = "integer",
  dest = "seed",
  default = 123,
  metavar = "INT",
  help = "random seed used for reproducible bootstrap regression (default: %(default)s)"
)

#####################################################################################################################

########## create the parser for the "SURVIVAL" command ##########

parser_sur <- subparsers$add_parser(
  "survival",
  help = "apply a molecular signature in survival analysis for prognosis"
)

parser_sur$add_argument(
  "input",
  type = "character",
  metavar = "<input>",
  help = "tab separated values (tsv) file containing expression and survival parameters"
)

parser_sur$add_argument(
  "signature",
  type = "character",
  metavar = "<signature>",
  help = "tab separated values (tsv) file containing a set of features and corresponding regression coefficients"
)

parser_sur$add_argument(
  "--output-prefix",
  type = "character",
  dest = "output_prefix",
  metavar = "STR",
  default = "clinprog",
  help = "output prefix used for generated files (default: %(default)s)"
)

parser_sur$add_argument(
  "--norm-exp",
  action = "store_true",
  dest = "norm_exp",
  default = FALSE,
  help = "normalize expression to log2(exp+1)"
)

parser_sur$add_argument(
  "--run-multivariate",
  action = "store_true",
  dest = "run_multivariate",
  default = FALSE,
  help = "if clinical variables should be included. This option is tied with --clinical-data option"
)

parser_sur$add_argument(
  "--clinical-file",
  type = "character",
  dest = "clinical_file",
  default = NULL,
  metavar = "FILE",
  help = "tab separated values (tsv) file containing binary categorical variables only. Required if --run-multivariate option is chosen"
)

parser_sur$add_argument(
  "--score-cutoff",
  type = "character",
  dest = "score_cutoff",
  choices = c("median", "mean", "signal", "roc"),
  default = "median",
  help = "curve to define the score cutoff: median, mean, signal, roc (default: %(default)s)"
)

parser_sur$add_argument(
  "--variance-filter",
  type = "double",
  dest = "variance_filter",
  default = 0.01,
  metavar = "DOUBLE",
  help = "minimum normalized variance for follow-up time (default: %(default)s)"
)

parser_sur$add_argument(
  "--max-follow-up",
  type = "integer",
  dest = "max_follow_up",
  default = NULL,
  metavar = "INT",
  help = "max follow up time"
)

parser_sur$add_argument(
  "--p-value-cutoff",
  type = "double",
  dest = "p_value_cutoff",
  default = 0.2,
  metavar = "DOUBLE",
  help = "p-value threshold used to select clinical covariates from univariate Cox regression (default: %(default)s)"
)

parser_sur$add_argument(
  "--bootstrap",
  type = "integer",
  dest = "bootstrap",
  default = 1,
  metavar = "INT",
  help = "number of bootstrap iterations (default: %(default)s)"
)

parser_sur$add_argument(
  "--skip-survival-checks",
  action = "store_true",
  dest = "skip_survival_checks",
  default = FALSE,
  help = "override filtering steps related to survival assumptions"
)

parser_sur$add_argument(
  "--report-format",
  type = "character",
  dest = "report_format",
  choices = c("pdf", "html"),
  default = "pdf",
  help = "report format: pdf or html (default: %(default)s)"
)

parser_sur$add_argument(
  "--verbose",
  action = "store_true",
  dest = "verbose",
  default = FALSE,
  help = "print progress messages to the console"
)

#####################################################################################################################

########## create the parser for the "COMPLETE" command ##########

parser_all <- subparsers$add_parser(
  "complete",
  help = "generate and apply a molecular signature (integrated analysis)"
)

parser_all$add_argument(
  "input",
  type = "character",
  metavar = "<input>",
  help = "tab separated values (tsv) file containing expression and survival parameters"
)

parser_all$add_argument(
  "--output-prefix",
  type = "character",
  dest = "output_prefix",
  default = "clinprog",
  metavar = "STR",
  help = "output prefix used for generated files (default: %(default)s)"
)

parser_all$add_argument(
  "--norm-exp",
  action = "store_true",
  dest = "norm_exp",
  default = FALSE,
  help = "normalize expression to log2(exp+1)"
)

parser_all$add_argument(
  "--bootstrap",
  type = "integer",
  dest = "bootstrap",
  default = 1,
  metavar = "INT",
  help = "number of bootstrap iterations (default: %(default)s)"
)

parser_all$add_argument(
  "--group-size",
  type = "integer",
  dest = "group_size",
  default = 10,
  metavar = "INT",
  help = "number of genes/transcripts sampled per iteration (default: %(default)s)"
)

parser_all$add_argument(
  "--percentage-filter",
  type = "double",
  dest = "percentage_filter",
  default = 0.3,
  metavar = "DOUBLE",
  help = "correlation filter threshold (default: %(default)s)"
)

parser_all$add_argument(
  "--variance-filter",
  type = "double",
  dest = "variance_filter",
  default = 0.01,
  metavar = "DOUBLE",
  help = "minimum normalized variance for follow-up time (default: %(default)s)"
)

parser_all$add_argument(
  "--max-follow-up",
  type = "integer",
  dest = "max_follow_up",
  default = NULL,
  metavar = "INT",
  help = "max follow up time (default: %(default)s)"
)

parser_all$add_argument(
  "--feature-type",
  type = "character",
  dest = "feature_type",
  choices = c("gene", "transcript"),
  default = "gene",
  help = "type of transcriptome data: gene or transcript (default: %(default)s)"
)

parser_all$add_argument(
  "--min-signature-size",
  type = "integer",
  dest = "min_signature_size",
  default = NULL,
  metavar = "INT",
  help = "search for the smallest signature that still yields a significant univariate log-rank test on the same dataset"
)

parser_all$add_argument(
  "--min-signature-cutoff",
  type = "integer",
  dest = "min_signature_cutoff",
  choices = c("median", "mean", "signal", "roc"),
  default = NULL,
  help = "strategy used to split scores into low/high groups during the minimum signature search"
)

parser_all$add_argument(
  "--run-multivariate",
  action = "store_true",
  dest = "run_multivariate",
  default = FALSE,
  help = "if clinical variables should be included. This option is tied with --clinical-data option"
)

parser_all$add_argument(
  "--clinical-file",
  type = "character",
  dest = "clinical_file",
  default = NULL,
  metavar = "FILE",
  help = "tab separated values (tsv) file containing binary categorical variables only. Required if --run-multivariate option is chosen"
)

parser_all$add_argument(
  "--score-cutoff",
  type = "character",
  dest = "score_cutoff",
  choices = c("median", "mean", "signal", "roc"),
  default = "median",
  help = "curve to define the score cutoff: median, mean, signal, roc (default: %(default)s)"
)

parser_all$add_argument(
  "--p-value-cutoff",
  type = "double",
  dest = "p_value_cutoff",
  default = 0.2,
  metavar = "DOUBLE",
  help = "p-value threshold used to select clinical covariates from univariate Cox allression (default: %(default)s)"
)

parser_all$add_argument(
  "--skip-survival-checks",
  action = "store_true",
  dest = "skip_survival_checks",
  default = FALSE,
  help = "override filtering steps related to survival assumptions"
)

parser_all$add_argument(
  "--report-format",
  type = "character",
  dest = "report_format",
  choices = c("pdf", "html"),
  default = "pdf",
  help = "report format: pdf or html (default: %(default)s)"
)

parser_all$add_argument(
  "--verbose",
  action = "store_true",
  dest = "verbose",
  default = FALSE,
  help = "print progress messages to the console"
)

parser_all$add_argument(
  "--n_cores",
  type = "integer",
  dest = "n_cores",
  default = 1,
  metavar = "INT",
  help = "number of CPU cores used for parallel bootstrap allression (default: %(default)s)"
)

parser_all$add_argument(
  "--seed",
  type = "integer",
  dest = "seed",
  default = 123,
  metavar = "INT",
  help = "random seed used for reproducible bootstrap allression (default: %(default)s)"
)

#####################################################################################################################


########## parse command-line options ###############################################################################

newargs <- commandArgs(trailingOnly = TRUE)
if (length(newargs) == 0) {
  parser$parse_args("--help")
}

args <- parser$parse_args()

if (is.null(args$sub_name)) {
  stop("A subcommand is required: regression, survival, or complete", call. = FALSE)
}

require_nonempty <- function(value, option) {
  if (is.null(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
    stop(sprintf("Required option %s was not provided", option), call. = FALSE)
  }
}

validate_cli_file <- function(path, option) {
  require_nonempty(path, option)
  if (!file.exists(path)) {
    stop(sprintf("File supplied to %s does not exist: %s", option, path), call. = FALSE)
  }
}
#####################################################################################################################

########## REGRESSION ##########

if (args$sub_name == "regression") {
  validate_cli_file(args$input, "input")
  data <- read_table(args$input, sep = "\t")

  run_regression(
    data = data,
    output_prefix = args$output_prefix,
    norm_exp = args$norm_exp,
    bootstrap = args$bootstrap,
    group_size = args$group_size,
    percentage_filter = args$percentage_filter,
    variance_filter = args$variance_filter,
    max_follow_up = args$max_follow_up,
    feature_type = args$feature_type,
    min_signature_size = args$min_signature_size,
    min_signature_cutoff = args$min_signature_cutoff,
    skip_survival_checks = args$skip_survival_checks,
    save_plots = TRUE,
    save_table = TRUE,
    save_json = TRUE,
    save_rds = TRUE,
    report_format = args$report_format,
    verbose = args$verbose,
    n_cores = args$n_cores,
    seed = args$seed,
    save_log = TRUE
  )
}
################################

########## SURVIVAL ##########
if (args$sub_name == "survival") {
  validate_cli_file(args$input, "input")
  require_nonempty(args$signature, "signature")

  if (args$run_multivariate) {
    validate_cli_file(args$clinical_file, "--clinical-file")
  } else if (!is.null(args$clinical_file) && nzchar(args$clinical_file)) {
    stop("--clinical-file can only be used together with --run-multivariate", call. = FALSE)
  }

  data <- read_table(args$input, sep = "\t")

  clinical_data <- if (args$run_multivariate) {
    read_table(args$clinical_file, sep = "\t")
  } else {
    NULL
  }

  signature <- read_table(args$signature, sep = "\t", row.names = FALSE)

  run_survival(
    data = data,
    signature = signature,
    output_prefix = args$output_prefix,
    norm_exp = args$norm_exp,
    run_multivariate = args$run_multivariate,
    clinical_data = clinical_data,
    score_cutoff = args$score_cutoff,
    variance_filter = args$variance_filter,
    max_follow_up = args$max_follow_up,
    p_value_cutoff = args$p_value_cutoff,
    bootstrap = args$bootstrap,
    skip_survival_checks = args$skip_survival_checks,
    save_plots = TRUE,
    save_table = TRUE,
    save_json = TRUE,
    save_rds = TRUE,
    report_format = args$report_format,
    verbose = args$verbose,
    save_log = TRUE
  )
}

##############################

########## COMPLETE ##########

if (args$sub_name == "complete") {
  validate_cli_file(args$input, "input")

  if (args$run_multivariate) {
    validate_cli_file(args$clinical_file, "--clinical-file")
  } else if (!is.null(args$clinical_file) && nzchar(args$clinical_file)) {
    stop("--clinical-file can only be used together with --run-multivariate", call. = FALSE)
  }

  data <- read_table(args$input, sep = "\t")

  clinical_data <- if (args$run_multivariate) {
    read_table(args$clinical_file, sep = "\t")
  } else {
    NULL
  }

  run_complete(
    data = data,
    output_prefix = args$output_prefix,
    norm_exp = args$norm_exp,
    bootstrap = args$bootstrap,
    group_size = args$group_size,
    percentage_filter = args$percentage_filter,
    variance_filter = args$variance_filter,
    max_follow_up = args$max_follow_up,
    feature_type = args$feature_type,
    min_signature_size = args$min_signature_size,
    min_signature_cutoff = args$min_signature_cutoff,
    run_multivariate = args$run_multivariate,
    clinical_data = clinical_data,
    score_cutoff = args$score_cutoff,
    p_value_cutoff = args$p_value_cutoff,
    skip_survival_checks = args$skip_survival_checks,
    save_plots = TRUE,
    save_table = TRUE,
    save_json = TRUE,
    save_rds = TRUE,
    report_format = args$report_format,
    verbose = args$verbose,
    n_cores = args$n_cores,
    seed = args$seed,
    save_log = TRUE
  )
}

##############################
