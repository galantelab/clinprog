#!/usr/bin/env Rscript

library(argparse)
library(clinprog)

parser <- ArgumentParser(
  prog = "clinprog",
  description = "Bootstrap-based LASSO Cox regression and survival analysis for molecular signature discovery"
)

subparsers <- parser$add_subparsers(
  dest = "sub_name",
  metavar = "<subcommand>",
  help = "select an analysis command"
)

parser$add_argument(
  "-V", "--version",
  action = "version",
  version = paste("clinprog", as.character(utils::packageVersion("clinprog")))
)

########## create the parser for the "REGRESSION" command ##########

parser_reg <- subparsers$add_parser(
  "regression",
  help = "generate a molecular signature through penalized LASSO Cox regression"
)

parser_reg$add_argument(
  "input",
  type = "character",
  metavar = "EXPRESSION_TSV",
  help = paste0(
    "TSV expression/survival table. The first column contains sample IDs; ",
    "the next two columns are binary overall-survival status (0/1) and ",
    "follow-up time; remaining columns contain gene or transcript expression."
  )
)

parser_reg$add_argument(
  "-o", "--output-prefix",
  type = "character",
  dest = "output_prefix",
  default = "clinprog",
  metavar = "STR",
  help = "output prefix used for generated files (default: %(default)s)"
)

parser_reg$add_argument(
  "-n", "--norm-exp",
  action = "store_true",
  dest = "norm_exp",
  default = FALSE,
  help = "apply the log2(expression + 1) transformation to expression values"
)

parser_reg$add_argument(
  "-b", "--bootstrap",
  type = "integer",
  dest = "bootstrap",
  default = 1,
  metavar = "INT",
  help = "number of bootstrap iterations; must be a positive integer (default: %(default)s)"
)

parser_reg$add_argument(
  "-g", "--group-size",
  type = "integer",
  dest = "group_size",
  default = 10,
  metavar = "INT",
  help = "number of genes/transcripts sampled per regression iteration (default: %(default)s)"
)

parser_reg$add_argument(
  "--percentage-filter",
  type = "double",
  dest = "percentage_filter",
  default = 0.3,
  metavar = "DOUBLE",
  help = "correlation-filter threshold between 0 and 1 (default: %(default)s)"
)

parser_reg$add_argument(
  "--variance-filter",
  type = "double",
  dest = "variance_filter",
  default = 0.01,
  metavar = "DOUBLE",
  help = "minimum normalized variance threshold for the follow-up-time variable (default: %(default)s)"
)

parser_reg$add_argument(
  "--max-follow-up",
  type = "double",
  dest = "max_follow_up",
  default = NULL,
  metavar = "NUMBER",
  help = "maximum follow-up time to consider, in the time unit used by the input (default: no truncation)"
)

parser_reg$add_argument(
  "-t", "--feature-type",
  type = "character",
  dest = "feature_type",
  choices = c("gene", "transcript"),
  default = "gene",
  help = "feature granularity of expression data: gene or transcript (default: %(default)s)"
)

parser_reg$add_argument(
  "-m", "--min-signature-size",
  type = "integer",
  dest = "min_signature_size",
  default = NULL,
  metavar = "INT",
  help = "start searching at this signature size and add features until the univariate log-rank p-value is below 0.05; unset disables the search"
)

parser_reg$add_argument(
  "--min-signature-cutoff",
  type = "character",
  dest = "min_signature_cutoff",
  choices = c("median", "mean", "signal", "roc"),
  default = NULL,
  help = "score-cutoff method for the minimum-signature search: median, mean, signal, or roc (default: median when search is enabled)"
)

parser_reg$add_argument(
  "-f", "--skip-survival-checks",
  action = "store_true",
  dest = "skip_survival_checks",
  default = FALSE,
  help = "bypass filtering checks related to survival assumptions; use cautiously"
)

parser_reg$add_argument(
  "-r", "--report-format",
  type = "character",
  dest = "report_format",
  choices = c("pdf", "html"),
  default = "pdf",
  help = "report format: pdf or html; the CLI generates a report by default (default: %(default)s)"
)

parser_reg$add_argument(
  "-v", "--verbose",
  action = "store_true",
  dest = "verbose",
  default = FALSE,
  help = "print progress messages to the console"
)

parser_reg$add_argument(
  "-j", "--n-cores",
  type = "integer",
  dest = "n_cores",
  default = 1,
  metavar = "INT",
  help = "number of parallel workers for bootstrap regression; 1 runs serially (default: %(default)s)"
)

parser_reg$add_argument(
  "-s", "--seed",
  type = "integer",
  dest = "seed",
  default = 123,
  metavar = "INT",
  help = "random seed for stochastic steps in the regression module (default: %(default)s)"
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
  metavar = "EXPRESSION_TSV",
  help = paste0(
    "TSV expression/survival table. The first column contains sample IDs; ",
    "the next two columns are binary overall-survival status (0/1) and ",
    "follow-up time; remaining columns contain gene or transcript expression."
  )
)

parser_sur$add_argument(
  "signature",
  type = "character",
  metavar = "SIGNATURE_TSV",
  help = "TSV signature table with columns named feature and coefficient"
)

parser_sur$add_argument(
  "-o", "--output-prefix",
  type = "character",
  dest = "output_prefix",
  metavar = "STR",
  default = "clinprog",
  help = "output prefix used for generated files (default: %(default)s)"
)

parser_sur$add_argument(
  "-n", "--norm-exp",
  action = "store_true",
  dest = "norm_exp",
  default = FALSE,
  help = "apply the log2(expression + 1) transformation to expression values"
)

parser_sur$add_argument(
  "--run-multivariate",
  action = "store_true",
  dest = "run_multivariate",
  default = FALSE,
  help = "include clinical covariates in multivariate Cox analysis; requires --clinical-file"
)

parser_sur$add_argument(
  "--clinical-file",
  type = "character",
  dest = "clinical_file",
  default = NULL,
  metavar = "FILE",
  help = "TSV clinical table: first column contains sample IDs and remaining columns contain binary categorical covariates; required with --run-multivariate"
)

parser_sur$add_argument(
  "-c", "--score-cutoff",
  type = "character",
  dest = "score_cutoff",
  choices = c("median", "mean", "signal", "roc"),
  default = "median",
  help = "method used to define the low/high score cutoff: median, mean, signal, or roc (default: %(default)s)"
)

parser_sur$add_argument(
  "--variance-filter",
  type = "double",
  dest = "variance_filter",
  default = 0.01,
  metavar = "DOUBLE",
  help = "minimum normalized variance threshold for the follow-up-time variable (default: %(default)s)"
)

parser_sur$add_argument(
  "--max-follow-up",
  type = "double",
  dest = "max_follow_up",
  default = NULL,
  metavar = "NUMBER",
  help = "maximum follow-up time to consider, in the time unit used by the input (default: no truncation)"
)

parser_sur$add_argument(
  "-p", "--p-value-cutoff",
  type = "double",
  dest = "p_value_cutoff",
  default = 0.2,
  metavar = "DOUBLE",
  help = "p-value threshold used to select clinical covariates from univariate Cox regression (default: %(default)s)"
)

parser_sur$add_argument(
  "-b", "--bootstrap",
  type = "integer",
  dest = "bootstrap",
  default = 1,
  metavar = "INT",
  help = "number of bootstrap iterations for multivariate Cox resampling; must be positive (default: %(default)s)"
)

parser_sur$add_argument(
  "-f", "--skip-survival-checks",
  action = "store_true",
  dest = "skip_survival_checks",
  default = FALSE,
  help = "bypass filtering checks related to survival assumptions; use cautiously"
)

parser_sur$add_argument(
  "-r", "--report-format",
  type = "character",
  dest = "report_format",
  choices = c("pdf", "html"),
  default = "pdf",
  help = "report format: pdf or html; the CLI generates a report by default (default: %(default)s)"
)

parser_sur$add_argument(
  "-v", "--verbose",
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
  metavar = "EXPRESSION_TSV",
  help = paste0(
    "TSV expression/survival table. The first column contains sample IDs; ",
    "the next two columns are binary overall-survival status (0/1) and ",
    "follow-up time; remaining columns contain gene or transcript expression."
  )
)

parser_all$add_argument(
  "-o", "--output-prefix",
  type = "character",
  dest = "output_prefix",
  default = "clinprog",
  metavar = "STR",
  help = "output prefix used for generated files (default: %(default)s)"
)

parser_all$add_argument(
  "-n", "--norm-exp",
  action = "store_true",
  dest = "norm_exp",
  default = FALSE,
  help = "apply the log2(expression + 1) transformation to expression values"
)

parser_all$add_argument(
  "-b", "--bootstrap",
  type = "integer",
  dest = "bootstrap",
  default = 1,
  metavar = "INT",
  help = "number of bootstrap iterations; must be a positive integer (default: %(default)s)"
)

parser_all$add_argument(
  "-g", "--group-size",
  type = "integer",
  dest = "group_size",
  default = 10,
  metavar = "INT",
  help = "number of genes/transcripts sampled per regression iteration (default: %(default)s)"
)

parser_all$add_argument(
  "--percentage-filter",
  type = "double",
  dest = "percentage_filter",
  default = 0.3,
  metavar = "DOUBLE",
  help = "correlation-filter threshold between 0 and 1 (default: %(default)s)"
)

parser_all$add_argument(
  "--variance-filter",
  type = "double",
  dest = "variance_filter",
  default = 0.01,
  metavar = "DOUBLE",
  help = "minimum normalized variance threshold for the follow-up-time variable (default: %(default)s)"
)

parser_all$add_argument(
  "--max-follow-up",
  type = "double",
  dest = "max_follow_up",
  default = NULL,
  metavar = "NUMBER",
  help = "maximum follow-up time to consider, in the time unit used by the input (default: no truncation)"
)

parser_all$add_argument(
  "-t", "--feature-type",
  type = "character",
  dest = "feature_type",
  choices = c("gene", "transcript"),
  default = "gene",
  help = "feature granularity of expression data: gene or transcript (default: %(default)s)"
)

parser_all$add_argument(
  "-m", "--min-signature-size",
  type = "integer",
  dest = "min_signature_size",
  default = NULL,
  metavar = "INT",
  help = "start searching at this signature size and add features until the univariate log-rank p-value is below 0.05; unset disables the search"
)

parser_all$add_argument(
  "--min-signature-cutoff",
  type = "character",
  dest = "min_signature_cutoff",
  choices = c("median", "mean", "signal", "roc"),
  default = NULL,
  help = "score-cutoff method for the minimum-signature search: median, mean, signal, or roc (default: median when search is enabled)"
)

parser_all$add_argument(
  "--run-multivariate",
  action = "store_true",
  dest = "run_multivariate",
  default = FALSE,
  help = "include clinical covariates in multivariate Cox analysis; requires --clinical-file"
)

parser_all$add_argument(
  "--clinical-file",
  type = "character",
  dest = "clinical_file",
  default = NULL,
  metavar = "FILE",
  help = "TSV clinical table: first column contains sample IDs and remaining columns contain binary categorical covariates; required with --run-multivariate"
)

parser_all$add_argument(
  "-c", "--score-cutoff",
  type = "character",
  dest = "score_cutoff",
  choices = c("median", "mean", "signal", "roc"),
  default = "median",
  help = "method used to define the low/high score cutoff: median, mean, signal, or roc (default: %(default)s)"
)

parser_all$add_argument(
  "-p", "--p-value-cutoff",
  type = "double",
  dest = "p_value_cutoff",
  default = 0.2,
  metavar = "DOUBLE",
  help = "p-value threshold used to select clinical covariates from univariate Cox regression (default: %(default)s)"
)

parser_all$add_argument(
  "-f", "--skip-survival-checks",
  action = "store_true",
  dest = "skip_survival_checks",
  default = FALSE,
  help = "bypass filtering checks related to survival assumptions; use cautiously"
)

parser_all$add_argument(
  "-r", "--report-format",
  type = "character",
  dest = "report_format",
  choices = c("pdf", "html"),
  default = "pdf",
  help = "report format: pdf or html; the CLI generates a report by default (default: %(default)s)"
)

parser_all$add_argument(
  "-v", "--verbose",
  action = "store_true",
  dest = "verbose",
  default = FALSE,
  help = "print progress messages to the console"
)

parser_all$add_argument(
  "-j", "--n-cores",
  type = "integer",
  dest = "n_cores",
  default = 1,
  metavar = "INT",
  help = "number of parallel workers for bootstrap regression; 1 runs serially (default: %(default)s)"
)

parser_all$add_argument(
  "-s", "--seed",
  type = "integer",
  dest = "seed",
  default = 123,
  metavar = "INT",
  help = "random seed for stochastic steps in the regression module (default: %(default)s)"
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
  validate_cli_file(args$signature, "signature")

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

  signature <- read_table(args$signature, sep = "\t", row_names = FALSE)

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
