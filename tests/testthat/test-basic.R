############################################# TEST MODULE I - REGRESSION #############################################

test_that("run_regression runs on toy data", {
  example_file <- system.file("extdata", "toy_expression.tsv", package = "clinprog")
  example_data <- read_table(example_file, sep = "\t")

  result <- run_regression(
    data = example_data,
    bootstrap = 4,
    group_size = 3,
    feature_type = "transcript",
    skip_survival_checks = TRUE,
    n_cores = 1,
    seed = 123
  )

  expect_s3_class(result, "clinprog_signature")
  expect_true(is.data.frame(result$signature))
  expect_true(nrow(result$signature) > 0)
  expect_true(all(c("feature", "coefficient") %in% colnames(result$signature)))

  expect_equal(result$params$n_cores, 1)
  expect_equal(result$params$seed, 123)
})


test_that("run_regression is reproducible with the same seed", {
  example_file <- system.file("extdata", "toy_expression.tsv", package = "clinprog")
  example_data <- read_table(example_file, sep = "\t")

  result1 <- run_regression(
    data = example_data,
    bootstrap = 4,
    group_size = 3,
    feature_type = "transcript",
    skip_survival_checks = TRUE,
    n_cores = 1,
    seed = 123
  )

  result2 <- run_regression(
    data = example_data,
    bootstrap = 4,
    group_size = 3,
    feature_type = "transcript",
    skip_survival_checks = TRUE,
    n_cores = 1,
    seed = 123
  )

  expect_identical(result1$signature, result2$signature)
})


test_that("run_regression gives identical results in serial and parallel modes", {
  example_file <- system.file("extdata", "toy_expression.tsv", package = "clinprog")
  example_data <- read_table(example_file, sep = "\t")

  result_serial <- run_regression(
    data = example_data,
    bootstrap = 4,
    group_size = 3,
    feature_type = "transcript",
    skip_survival_checks = TRUE,
    n_cores = 1,
    seed = 123
  )

  result_parallel <- run_regression(
    data = example_data,
    bootstrap = 4,
    group_size = 3,
    feature_type = "transcript",
    skip_survival_checks = TRUE,
    n_cores = 2,
    seed = 123
  )

  expect_identical(result_serial$signature, result_parallel$signature)
})


test_that("run_regression validates n_cores and seed", {
  example_file <- system.file("extdata", "toy_expression.tsv", package = "clinprog")
  example_data <- read_table(example_file, sep = "\t")

  expect_error(
    run_regression(
      data = example_data,
      bootstrap = 4,
      group_size = 3,
      feature_type = "transcript",
      skip_survival_checks = TRUE,
      n_cores = 0
    )
  )

  expect_error(
    run_regression(
      data = example_data,
      bootstrap = 4,
      group_size = 3,
      feature_type = "transcript",
      skip_survival_checks = TRUE,
      n_cores = 1.5
    )
  )

  expect_error(
    run_regression(
      data = example_data,
      bootstrap = 4,
      group_size = 3,
      feature_type = "transcript",
      skip_survival_checks = TRUE,
      seed = 1.5
    )
  )
})

test_that("run_regression restores the global RNG state", {
  example_file <- system.file("extdata", "toy_expression.tsv", package = "clinprog")
  example_data <- read_table(example_file, sep = "\t")

  set.seed(42)
  expected <- .Random.seed

  run_regression(
    data = example_data,
    bootstrap = 4,
    group_size = 3,
    feature_type = "transcript",
    skip_survival_checks = TRUE,
    seed = 123,
    n_cores = 1
  )

  expect_identical(.Random.seed, expected)
})

test_that("run_regression restores absence of the global RNG state", {
  example_file <- system.file("extdata", "toy_expression.tsv", package = "clinprog")
  example_data <- read_table(example_file, sep = "\t")

  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }

  run_regression(
    data = example_data,
    bootstrap = 4,
    group_size = 3,
    feature_type = "transcript",
    skip_survival_checks = TRUE,
    seed = 123,
    n_cores = 1
  )

  expect_false(
    exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  )
})

#######################################################################################################################

################################### TEST COMPLETE WORKFLOW - REGRESSION + SURVIVAL ###################################
test_that("run_complete runs on toy data", {
  # Temporary output directory
  tmp_dir <- tempdir()
  message("Using temporary directory: ", tmp_dir)
  output_prefix <- file.path(tmp_dir, "run_complete")
  # output_prefix <- "run_complete"

  # Loads toy datasets
  example_file <- system.file("extdata", "toy_expression.tsv", package = "clinprog")
  clinical_file <- system.file("extdata", "toy_clinics.tsv", package = "clinprog")
  example_data <- read_table(example_file, sep = "\t")
  clinical_data <- read_table(clinical_file, sep = "\t")

  # Runs clinprog modules I + II - regression + survival
  result <- run_complete(
    data = example_data,
    output_prefix = output_prefix,
    bootstrap = 10,
    group_size = 10,
    percentage_filter = 0.3,
    variance_filter = 0.01,
    max_follow_up = NULL,
    feature_type = "transcript",
    run_multivariate = TRUE,
    clinical_data = clinical_data,
    score_cutoff = "roc",
    p_value_cutoff = 0.2,
    skip_survival_checks = TRUE,
    save_plots = TRUE,
    save_table = TRUE,
    save_json = TRUE,
    save_rds = TRUE,
    report_format = FALSE,
    save_log = TRUE,
    n_cores = 1,
    seed = 123
  )

  # Main object
  expect_s3_class(result, "clinprog_complete")

  # Internal modules
  expect_s3_class(result$regression, "clinprog_signature")
  expect_s3_class(result$survival, "clinprog_survival")

  # Regression outputs
  expect_true(is.data.frame(result$regression$signature))
  expect_true(nrow(result$regression$signature) > 0)

  # Survival outputs
  expect_true(is.list(result$survival$survival))
  expect_true(inherits(result$survival$survival$univariate$model, "coxph"))

  # Parameters
  expect_true(is.list(result$params))
  expect_equal(result$params$bootstrap, 10)
  expect_equal(result$params$n_cores, 1)
  expect_equal(result$params$seed, 123)
  expect_equal(result$params$variance_filter, 0.01)
  expect_true(result$params$run_multivariate)
  expect_equal(result$params$score_cutoff, "roc")

  # Metadata and reproducibility
  expect_true(!is.null(result$call))
  expect_true(is.list(result$metadata))
  expect_true(!is.null(result$metadata$package_version))
})
######################################################################################################################

test_that("run_regression validates norm_exp", {
  expect_error(
    run_regression(
      data = toy_expression,
      norm_exp = "TRUE"
    ),
    "Argument 'norm_exp' must be a boolean"
  )

  expect_error(
    run_regression(
      data = toy_expression,
      norm_exp = 1
    ),
    "Argument 'norm_exp' must be a boolean"
  )
})

test_that("clinprog_min_signature validates arguments", {

  signature <- data.frame(
    feature = c("GENE-A", "GENE-B"),
    coefficient = c(1, 0.5)
  )

  data <- data.frame(
    OS = c(1, 1, 0, 0),
    OS.time = c(1, 2, 10, 12),
    GENE__A = c(1, 1, 0, 0),
    GENE__B = c(0, 1, 1, 0)
  )

  expect_error(
    clinprog_min_signature(
      signature = signature,
      data = data,
      min_size = 0,
      score_cutoff = "median"
    ),
    "min_size"
  )

  expect_error(
    clinprog_min_signature(
      signature = signature,
      data = data,
      min_size = 1.5,
      score_cutoff = "median"
    ),
    "min_size"
  )

  expect_error(
    clinprog_min_signature(
      signature = signature,
      data = data,
      min_size = 1,
      score_cutoff = "invalid"
    ),
    "score_cutoff"
  )
})

test_that("clinprog_min_signature returns full signature when min_size is too large", {

  signature <- data.frame(
    feature = c("GENE-A", "GENE-B"),
    coefficient = c(1, 0.5)
  )

  data <- data.frame(
    OS = c(1, 1, 0, 0),
    OS.time = c(1, 2, 10, 12),
    GENE__A = c(1, 1, 0, 0),
    GENE__B = c(0, 1, 1, 0)
  )

  result <- clinprog_min_signature(
    signature = signature,
    data = data,
    min_size = 3,
    score_cutoff = "median"
  )

  expect_equal(result$size, 2)
  expect_true(is.na(result$pvalue))
  expect_equal(nrow(result$signature), 2)
  expect_equal(nrow(result$tested), 0)
})

test_that("clinprog_min_signature ranks features by absolute coefficient", {

  signature <- data.frame(
    feature = c("GENE-A", "GENE-B", "GENE-C"),
    coefficient = c(0.1, -2, 0.5)
  )

  data <- data.frame(
    OS = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
           1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
    OS.time = c(
      3, 8, 12, 5, 20, 7, 15, 10, 25, 6,
      4, 11, 9, 18, 13, 22, 16, 14, 30, 19
    ),
    GENE__A = c(
      0, 1, 0, 1, 0, 1, 1, 0, 1, 0,
      1, 0, 1, 0, 1, 0, 0, 1, 0, 1
    ),
    GENE__B = c(
      1, 0, 1, 1, 0, 1, 0, 0, 1, 0,
      1, 1, 0, 1, 0, 0, 1, 0, 1, 0
    ),
    GENE__C = c(
      0, 0, 1, 0, 1, 0, 1, 1, 0, 1,
      0, 1, 0, 1, 0, 1, 0, 1, 1, 0
    )
  )

  result <- clinprog_min_signature(
    signature = signature,
    data = data,
    min_size = 1,
    score_cutoff = "median"
  )

  expect_equal(
    result$signature$feature[1],
    "GENE-B"
  )
})

test_that("run_regression validates minimum signature parameters", {

  expect_error(
    run_regression(
      data = toy_expression,
      min_signature_size = 0
    ),
    "min_signature_size"
  )

  expect_error(
    run_regression(
      data = toy_expression,
      min_signature_size = 1.5
    ),
    "min_signature_size"
  )

  expect_error(
    run_regression(
      data = toy_expression,
      min_signature_cutoff = "invalid"
    ),
    "min_signature_cutoff"
  )
})
