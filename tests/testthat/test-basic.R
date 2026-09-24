############################################# TEST MODULE I - REGRESSION #############################################

test_that("run_regression runs on toy data", {
  example_file <- system.file("extdata", "toy_expression.tsv", package = "clinprog")
  example_data <- read_table(example_file, sep = "\t")

  result <- run_regression(
    data = example_data,
    bootstrap = 4,
    groupsize = 3,
    type = "transcript",
    force = TRUE,
    ncores = 1,
    seed = 123
  )

  expect_s3_class(result, "clinprog_signature")
  expect_true(is.data.frame(result$signature))
  expect_true(nrow(result$signature) > 0)
  expect_true(all(c("feature", "coefficient") %in% colnames(result$signature)))

  expect_equal(result$params$ncores, 1)
  expect_equal(result$params$seed, 123)
})


test_that("run_regression is reproducible with the same seed", {
  example_file <- system.file("extdata", "toy_expression.tsv", package = "clinprog")
  example_data <- read_table(example_file, sep = "\t")

  result1 <- run_regression(
    data = example_data,
    bootstrap = 4,
    groupsize = 3,
    type = "transcript",
    force = TRUE,
    ncores = 1,
    seed = 123
  )

  result2 <- run_regression(
    data = example_data,
    bootstrap = 4,
    groupsize = 3,
    type = "transcript",
    force = TRUE,
    ncores = 1,
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
    groupsize = 3,
    type = "transcript",
    force = TRUE,
    ncores = 1,
    seed = 123
  )

  result_parallel <- run_regression(
    data = example_data,
    bootstrap = 4,
    groupsize = 3,
    type = "transcript",
    force = TRUE,
    ncores = 2,
    seed = 123
  )

  expect_identical(result_serial$signature, result_parallel$signature)
})


test_that("run_regression validates ncores and seed", {
  example_file <- system.file("extdata", "toy_expression.tsv", package = "clinprog")
  example_data <- read_table(example_file, sep = "\t")

  expect_error(
    run_regression(
      data = example_data,
      bootstrap = 4,
      groupsize = 3,
      type = "transcript",
      force = TRUE,
      ncores = 0
    )
  )

  expect_error(
    run_regression(
      data = example_data,
      bootstrap = 4,
      groupsize = 3,
      type = "transcript",
      force = TRUE,
      ncores = 1.5
    )
  )

  expect_error(
    run_regression(
      data = example_data,
      bootstrap = 4,
      groupsize = 3,
      type = "transcript",
      force = TRUE,
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
    groupsize = 3,
    type = "transcript",
    force = TRUE,
    seed = 123,
    ncores = 1
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
    groupsize = 3,
    type = "transcript",
    force = TRUE,
    seed = 123,
    ncores = 1
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
  outprefix <- file.path(tmp_dir, "run_complete")
  # outprefix <- "run_complete"

  # Loads toy datasets
  example_file <- system.file("extdata", "toy_expression.tsv", package = "clinprog")
  clinical_file <- system.file("extdata", "toy_clinics.tsv", package = "clinprog")
  example_data <- read_table(example_file, sep = "\t")
  clinical_data <- read_table(clinical_file, sep = "\t")

  # Runs clinprog modules I + II - regression + survival
  result <- run_complete(
    data = example_data,
    outprefix = outprefix,
    bootstrap = 10,
    groupsize = 10,
    percentagefilter = 0.3,
    variancefilter = 0.01,
    followup = NULL,
    type = "transcript",
    multivariate = TRUE,
    clindata = clinical_data,
    roc = TRUE,
    p.cutoff = 0.2,
    force = TRUE,
    plots = TRUE,
    table = TRUE,
    saveJSON = TRUE,
    saveRDS = TRUE,
    report = FALSE,
    log = TRUE,
    ncores = 1,
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
  expect_equal(result$params$ncores, 1)
  expect_equal(result$params$seed, 123)
  expect_equal(result$params$variancefilter, 0.01)
  expect_true(result$params$multivariate)
  expect_true(result$params$roc)

  # Metadata and reproducibility
  expect_true(!is.null(result$call))
  expect_true(is.list(result$metadata))
  expect_true(!is.null(result$metadata$package_version))
})
#######################################################################################################################
