############################################## TEST MODULE I - REGRESSION #############################################

#test_that("run_regression runs on toy data", {
  #example_file <- system.file("extdata", "toy_expression.tsv", package = "clinprog")
  #example_data <- read_table(example_file, sep = "\t")

  #result <- run_regression(
    #data = example_data,
    #bootstrap = 4,
    #groupsize = 3,
    #type = "transcript",
    #force = TRUE,
    #ncores = 1,
    #seed = 123
  #)

  #expect_s3_class(result, "clinprog_signature")
  #expect_true(is.data.frame(result$signature))
  #expect_true(nrow(result$signature) > 0)
  #expect_true(all(c("feature", "coefficient") %in% colnames(result$signature)))

  #expect_equal(result$params$ncores, 1)
  #expect_equal(result$params$seed, 123)
#})


#test_that("run_regression is reproducible with the same seed", {
  #example_file <- system.file("extdata", "toy_expression.tsv", package = "clinprog")
  #example_data <- read_table(example_file, sep = "\t")

  #result1 <- run_regression(
    #data = example_data,
    #bootstrap = 4,
    #groupsize = 3,
    #type = "transcript",
    #force = TRUE,
    #ncores = 1,
    #seed = 123
  #)

  #result2 <- run_regression(
    #data = example_data,
    #bootstrap = 4,
    #groupsize = 3,
    #type = "transcript",
    #force = TRUE,
    #ncores = 1,
    #seed = 123
  #)

  #expect_identical(result1$signature, result2$signature)
#})


#test_that("run_regression gives identical results in serial and parallel modes", {
  #example_file <- system.file("extdata", "toy_expression.tsv", package = "clinprog")
  #example_data <- read_table(example_file, sep = "\t")

  #result_serial <- run_regression(
    #data = example_data,
    #bootstrap = 4,
    #groupsize = 3,
    #type = "transcript",
    #force = TRUE,
    #ncores = 1,
    #seed = 123
  #)

  #result_parallel <- run_regression(
    #data = example_data,
    #bootstrap = 4,
    #groupsize = 3,
    #type = "transcript",
    #force = TRUE,
    #ncores = 2,
    #seed = 123
  #)

  #expect_identical(result_serial$signature, result_parallel$signature)
#})


#test_that("run_regression validates ncores and seed", {
  #example_file <- system.file("extdata", "toy_expression.tsv", package = "clinprog")
  #example_data <- read_table(example_file, sep = "\t")

  #expect_error(
    #run_regression(
      #data = example_data,
      #bootstrap = 4,
      #groupsize = 3,
      #type = "transcript",
      #force = TRUE,
      #ncores = 0
    #)
  #)

  #expect_error(
    #run_regression(
      #data = example_data,
      #bootstrap = 4,
      #groupsize = 3,
      #type = "transcript",
      #force = TRUE,
      #ncores = 1.5
    #)
  #)

  #expect_error(
    #run_regression(
      #data = example_data,
      #bootstrap = 4,
      #groupsize = 3,
      #type = "transcript",
      #force = TRUE,
      #seed = 1.5
    #)
  #)
#})

#test_that("run_regression restores the global RNG state", {
  #example_file <- system.file("extdata", "toy_expression.tsv", package = "clinprog")
  #example_data <- read_table(example_file, sep = "\t")

  #set.seed(42)
  #expected <- .Random.seed

  #run_regression(
    #data = example_data,
    #bootstrap = 4,
    #groupsize = 3,
    #type = "transcript",
    #force = TRUE,
    #seed = 123,
    #ncores = 1
  #)

  #expect_identical(.Random.seed, expected)
#})

#test_that("run_regression restores absence of the global RNG state", {
  #example_file <- system.file("extdata", "toy_expression.tsv", package = "clinprog")
  #example_data <- read_table(example_file, sep = "\t")

  #if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    #rm(".Random.seed", envir = .GlobalEnv)
  #}

  #run_regression(
    #data = example_data,
    #bootstrap = 4,
    #groupsize = 3,
    #type = "transcript",
    #force = TRUE,
    #seed = 123,
    #ncores = 1
  #)

  #expect_false(
    #exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  #)
#})

########################################################################################################################

#################################### TEST COMPLETE WORKFLOW - REGRESSION + SURVIVAL ###################################
#test_that("run_complete runs on toy data", {
  ## Temporary output directory
  #tmp_dir <- tempdir()
  #message("Using temporary directory: ", tmp_dir)
  #outprefix <- file.path(tmp_dir, "run_complete")
  ## outprefix <- "run_complete"

  ## Loads toy datasets
  #example_file <- system.file("extdata", "toy_expression.tsv", package = "clinprog")
  #clinical_file <- system.file("extdata", "toy_clinics.tsv", package = "clinprog")
  #example_data <- read_table(example_file, sep = "\t")
  #clinical_data <- read_table(clinical_file, sep = "\t")

  ## Runs clinprog modules I + II - regression + survival
  #result <- run_complete(
    #data = example_data,
    #outprefix = outprefix,
    #bootstrap = 10,
    #groupsize = 10,
    #percentagefilter = 0.3,
    #variancefilter = 0.01,
    #followup = NULL,
    #type = "transcript",
    #multivariate = TRUE,
    #clindata = clinical_data,
    #roc = TRUE,
    #p.cutoff = 0.2,
    #force = TRUE,
    #plots = TRUE,
    #table = TRUE,
    #saveJSON = TRUE,
    #saveRDS = TRUE,
    #report = FALSE,
    #log = TRUE,
    #ncores = 1,
    #seed = 123
  #)

  ## Main object
  #expect_s3_class(result, "clinprog_complete")

  ## Internal modules
  #expect_s3_class(result$regression, "clinprog_signature")
  #expect_s3_class(result$survival, "clinprog_survival")

  ## Regression outputs
  #expect_true(is.data.frame(result$regression$signature))
  #expect_true(nrow(result$regression$signature) > 0)

  ## Survival outputs
  #expect_true(is.list(result$survival$survival))
  #expect_true(inherits(result$survival$survival$univariate$model, "coxph"))

  ## Parameters
  #expect_true(is.list(result$params))
  #expect_equal(result$params$bootstrap, 10)
  #expect_equal(result$params$ncores, 1)
  #expect_equal(result$params$seed, 123)
  #expect_equal(result$params$variancefilter, 0.01)
  #expect_true(result$params$multivariate)
  #expect_true(result$params$roc)

  ## Metadata and reproducibility
  #expect_true(!is.null(result$call))
  #expect_true(is.list(result$metadata))
  #expect_true(!is.null(result$metadata$package_version))
#})
########################################################################################################################

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
      min_size = 0
    ),
    "min_size"
  )

  expect_error(
    clinprog_min_signature(
      signature = signature,
      data = data,
      min_size = 1.5
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
    min_size = 3
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

  expect_warning(
    result <- clinprog_min_signature(
      signature = signature,
      data = data,
      min_size = 1,
      score_cutoff = "median"
    ),
    "No minimum signature smaller than the complete signature"
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

expect_error(
  run_survival(
    data = toy_expression,
    signature = toy_signature,
    score_cutoff = "invalid"
  ),
  "score_cutoff"
)

test_that("plot_swimmer returns a ggplot object", {
  data <- data.frame(
    score_group = c("low", "high", "low", "high"),
    score = c(-1, 1, -0.5, 2),
    OS = c(1, 0, 1, 0),
    OS.time = c(10, 20, 30, 40)
  )

  result <- plot_swimmer(
    data = data,
    cutoff = 0
  )

  expect_s3_class(result, "ggplot")
})

test_that("plot_swimmer groups patients according to cutoff", {
  data <- data.frame(
    score_group = c("low", "high", "low", "high"),
    score = c(-1, 1, -0.5, 2),
    OS = c(1, 0, 1, 0),
    OS.time = c(10, 20, 30, 40)
  )

  result <- plot_swimmer(
    data = data,
    cutoff = 0
  )

  plot_data <- ggplot2::ggplot_build(result)$data[[1]]

  expect_equal(
    sort(unique(plot_data$fill)),
    sort(c("#D73027", "#1A9850"))
  )
})

test_that("plot_swimmer validates its input", {
  data <- data.frame(
    score_group = c("low", "high"),
    score = c(-1, 1),
    OS = c(1, 0),
    OS.time = c(10, 20)
  )

  expect_error(
    plot_swimmer(
      data = "not a data.frame",
      cutoff = 0
    ),
    "data"
  )

  expect_error(
    plot_swimmer(
      data = data[, c("score", "OS", "OS.time")],
      cutoff = 0
    ),
    "score_group"
  )

  expect_error(
    plot_swimmer(
      data = data,
      cutoff = "zero"
    ),
    "cutoff"
  )

  expect_error(
    plot_swimmer(
      data = data,
      cutoff = 0,
      sort_by = "invalid"
    )
  )
})

test_that("plot_swimmer accepts patient labels", {
  data <- data.frame(
    score_group = c("low", "high"),
    score = c(-1, 1),
    OS = c(1, 0),
    OS.time = c(10, 20),
    patient = c("patient_1", "patient_2")
  )

  result <- plot_swimmer(
    data = data,
    cutoff = 0,
    id_col = "patient",
    show_labels = TRUE
  )

  expect_s3_class(result, "ggplot")
})

test_that("plot_boxplot returns a patchwork object", {
  data <- data.frame(
    score_group = c("low", "high", "low", "high"),
    OS = c(1, 0, 1, 0),
    OS.time = c(10, 20, 30, 40),
    GeneA = c(1, 5, 2, 6),
    GeneB = c(2, 7, 3, 8)
  )

  result <- plot_boxplot(
    data = data
  )

  expect_s3_class(result, "patchwork")
})

test_that("plot_boxplot supports feature selection with a signature", {
  data <- data.frame(
    score_group = c("low", "high", "low", "high"),
    OS = c(1, 0, 1, 0),
    OS.time = c(10, 20, 30, 40),
    GeneA = c(1, 5, 2, 6),
    GeneB = c(2, 7, 3, 8),
    GeneC = c(3, 9, 4, 10)
  )

  signature <- data.frame(
    feature = c("GeneA", "GeneB", "GeneC"),
    coefficient = c(0.1, -2, 0.5)
  )

  result <- plot_boxplot(
    data = data,
    signature = signature,
    max_features = 2
  )

  expect_s3_class(result, "patchwork")
})

test_that("plot_boxplot supports expression normalization", {
  data <- data.frame(
    score_group = c("low", "high", "low", "high"),
    OS = c(1, 0, 1, 0),
    OS.time = c(10, 20, 30, 40),
    GeneA = c(1, 5, 2, 6)
  )

  result <- plot_boxplot(
    data = data,
    norm_exp = TRUE
  )

  expect_s3_class(result, "patchwork")
})

test_that("plot_boxplot supports faceting and sample sizes", {
  data <- data.frame(
    score_group = c(
      "low", "high", "low", "high",
      "low", "high", "low", "high"
    ),
    OS = c(
      1, 1, 1, 1,
      0, 0, 0, 0
    ),
    OS.time = c(
      10, 20, 30, 40,
      15, 25, 35, 45
    ),
    GeneA = c(
      1, 5, 2, 6,
      3, 7, 4, 8
    )
  )

  result <- plot_boxplot(
    data = data,
    facet_by = "OS",
    show_n = TRUE
  )

  expect_s3_class(result, "patchwork")
})

test_that("plot_boxplot validates its input", {
  data <- data.frame(
    score_group = c("low", "high"),
    OS = c(1, 0),
    OS.time = c(10, 20),
    GeneA = c(1, 5)
  )

  expect_error(
    plot_boxplot(
      data = "not a data.frame"
    ),
    "data"
  )

  expect_error(
    plot_boxplot(
      data = data,
      group_col = "missing"
    )
  )

  expect_error(
    plot_boxplot(
      data = data,
      facet_by = "missing"
    ),
    "facet_by"
  )

  expect_error(
    plot_boxplot(
      data = data,
      test = "invalid"
    )
  )

  expect_error(
    plot_boxplot(
      data = data,
      max_features = 0
    ),
    "max_features"
  )
})

test_that("plot_clinics returns a patchwork object", {
  data <- data.frame(
    score = c(-1, 1, -0.5, 2),
    OS = c(1, 0, 1, 0),
    OS.time = c(10, 20, 30, 40),
    sex = c("F", "M", "F", "M"),
    treatment = c("A", "B", "A", "B")
  )

  result <- plot_clinics(
    data = data,
    test = "none"
  )

  expect_s3_class(result, "patchwork")
})

test_that("plot_clinics skips non-categorical covariates", {
  data <- data.frame(
    score = c(-1, 1, -0.5, 2),
    OS = c(1, 0, 1, 0),
    OS.time = c(10, 20, 30, 40),
    sex = c("F", "M", "F", "M"),
    age = c(50, 60, 55, 65)
  )

  expect_warning(
    result <- plot_clinics(
      data = data,
      test = "none"
    ),
    "not categorical"
  )

  expect_s3_class(result, "patchwork")
})

test_that("plot_clinics skips covariates with more than two levels", {
  data <- data.frame(
    score = c(-1, 1, -0.5, 2, 0.2, 1.5),
    OS = c(1, 0, 1, 0, 1, 0),
    OS.time = c(10, 20, 30, 40, 50, 60),
    sex = c("F", "M", "F", "M", "F", "M"),
    stage = c("I", "II", "III", "I", "II", "III")
  )

  expect_warning(
    result <- plot_clinics(
      data = data,
      test = "none"
    ),
    "do not have exactly 2 levels"
  )

  expect_s3_class(result, "patchwork")
})

test_that("plot_clinics supports max_features", {
  data <- data.frame(
    score = c(-1, 1, -0.5, 2),
    OS = c(1, 0, 1, 0),
    OS.time = c(10, 20, 30, 40),
    sex = c("F", "M", "F", "M"),
    treatment = c("A", "B", "A", "B"),
    smoker = c("yes", "no", "yes", "no")
  )

  result <- plot_clinics(
    data = data,
    max_features = 2,
    test = "none"
  )

  expect_s3_class(result, "patchwork")
})

test_that("plot_clinics supports faceting", {
  data <- data.frame(
    score = c(
      -1, 1, -0.5, 2,
      -0.8, 1.5, -0.3, 2.2
    ),
    OS = c(
      1, 1, 1, 1,
      0, 0, 0, 0
    ),
    OS.time = c(
      10, 20, 30, 40,
      15, 25, 35, 45
    ),
    sex = c(
      "F", "M", "F", "M",
      "F", "M", "F", "M"
    )
  )

  result <- plot_clinics(
    data = data,
    facet_by = "OS",
    test = "none"
  )

  expect_s3_class(result, "patchwork")
})

test_that("plot_clinics supports plotting options", {
  data <- data.frame(
    score = c(-1, 1, -0.5, 2, 0, 1.5),
    OS = c(1, 0, 1, 0, 1, 0),
    OS.time = c(10, 20, 30, 40, 50, 60),
    sex = c("F", "M", "F", "M", "F", "M")
  )

  result <- plot_clinics(
    data = data,
    show_points = TRUE,
    show_n = TRUE,
    order_by_median = TRUE,
    test = "none"
  )

  expect_s3_class(result, "patchwork")
})

test_that("plot_clinics validates its input", {
  data <- data.frame(
    score = c(-1, 1, -0.5, 2),
    OS = c(1, 0, 1, 0),
    OS.time = c(10, 20, 30, 40),
    sex = c("F", "M", "F", "M")
  )

  expect_error(
    plot_clinics(
      data = "not a data.frame"
    )
  )

  expect_error(
    plot_clinics(
      data = data,
      score_col = "missing"
    )
  )

  expect_error(
    plot_clinics(
      data = data,
      facet_by = "missing"
    )
  )

  expect_error(
    plot_clinics(
      data = data,
      test = "invalid"
    )
  )

  expect_error(
    plot_clinics(
      data = data,
      max_features = 0
    )
  )

  expect_error(
    plot_clinics(
      data = data,
      id_col = "missing"
    )
  )
})

test_that("plot_clinics ignores individual without outprefix", {
  data <- data.frame(
    score = c(-1, 1, -0.5, 2),
    OS = c(1, 0, 1, 0),
    OS.time = c(10, 20, 30, 40),
    sex = c("F", "M", "F", "M")
  )

  expect_warning(
    result <- plot_clinics(
      data = data,
      individual = TRUE,
      test = "none"
    ),
    "individual"
  )

  expect_s3_class(result, "patchwork")
})

test_that("plot_clinics errors when no valid covariates remain", {
  data <- data.frame(
    score = c(-1, 1, -0.5, 2),
    OS = c(1, 0, 1, 0),
    OS.time = c(10, 20, 30, 40),
    age = c(50, 60, 55, 65)
  )

  expect_warning(
    expect_error(
      plot_clinics(
        data = data,
        test = "none"
      ),
      "categorical"
    ),
    "not categorical"
  )
})

test_that("plot_wordcloud returns a ggplot object", {
  data <- data.frame(
    OS = c(1, 0, 1, 0),
    OS.time = c(10, 20, 30, 40),
    TP53 = c(1, 2, 3, 4),
    EGFR = c(4, 3, 2, 1),
    BRCA1 = c(2, 2, 2, 2)
  )

  signature <- data.frame(
    feature = c("TP53", "EGFR", "BRCA1"),
    coefficient = c(1, -0.5, 0.25)
  )

  result <- plot_wordcloud(
    data = data,
    signature = signature
  )

  expect_s3_class(result, "ggplot")
})

test_that("plot_wordcloud skips missing signature features", {
  data <- data.frame(
    TP53 = c(1, 2, 3, 4),
    EGFR = c(4, 3, 2, 1)
  )

  signature <- data.frame(
    feature = c("TP53", "EGFR", "MISSING"),
    coefficient = c(1, -0.5, 0.25)
  )

  expect_warning(
    result <- plot_wordcloud(
      data = data,
      signature = signature
    ),
    "missing from data"
  )

  expect_s3_class(result, "ggplot")
})

test_that("plot_wordcloud errors when no signature features are present", {
  data <- data.frame(
    TP53 = c(1, 2, 3, 4),
    EGFR = c(4, 3, 2, 1)
  )

  signature <- data.frame(
    feature = c("BRCA1", "MYC"),
    coefficient = c(1, -0.5)
  )

  expect_error(
    plot_wordcloud(
      data = data,
      signature = signature
    ),
    "None of the signature features"
  )
})

test_that("plot_wordcloud errors when all weights are zero", {
  data <- data.frame(
    TP53 = c(0, 0, 0, 0),
    EGFR = c(0, 0, 0, 0)
  )

  signature <- data.frame(
    feature = c("TP53", "EGFR"),
    coefficient = c(1, -1)
  )

  expect_error(
    plot_wordcloud(
      data = data,
      signature = signature
    ),
    "zero or NA"
  )
})

test_that("plot_wordcloud handles missing expression values", {
  data <- data.frame(
    TP53 = c(1, 2, NA, 4),
    EGFR = c(4, NA, 2, 1)
  )

  signature <- data.frame(
    feature = c("TP53", "EGFR"),
    coefficient = c(1, -0.5)
  )

  result <- plot_wordcloud(
    data = data,
    signature = signature
  )

  expect_s3_class(result, "ggplot")
})

test_that("plot_wordcloud validates its arguments", {
  data <- data.frame(
    TP53 = c(1, 2, 3, 4),
    EGFR = c(4, 3, 2, 1)
  )

  signature <- data.frame(
    feature = c("TP53", "EGFR"),
    coefficient = c(1, -0.5)
  )

  expect_error(
    plot_wordcloud(
      data = "not a data.frame",
      signature = signature
    )
  )

  expect_error(
    plot_wordcloud(
      data = data,
      signature = "not a data.frame"
    )
  )

  expect_error(
    plot_wordcloud(
      data = data,
      signature = data.frame(coefficient = c(1, -0.5))
    )
  )

  expect_error(
    plot_wordcloud(
      data = data,
      signature = data.frame(feature = c("TP53", "EGFR"))
    )
  )

  expect_error(
    plot_wordcloud(
      data = data,
      signature = signature,
      min_alpha = -0.1
    )
  )

  expect_error(
    plot_wordcloud(
      data = data,
      signature = signature,
      max_alpha = 1.1
    )
  )

  expect_error(
    plot_wordcloud(
      data = data,
      signature = signature,
      min_alpha = 0.8,
      max_alpha = 0.2
    )
  )

  expect_error(
    plot_wordcloud(
      data = data,
      signature = signature,
      max_size = 0
    )
  )
})

test_that("plot_wordcloud accepts custom size and dimensions", {
  data <- data.frame(
    TP53 = c(1, 2, 3, 4),
    EGFR = c(4, 3, 2, 1),
    BRCA1 = c(2, 3, 2, 3)
  )

  signature <- data.frame(
    feature = c("TP53", "EGFR", "BRCA1"),
    coefficient = c(1, -0.5, 0.25)
  )

  result <- plot_wordcloud(
    data = data,
    signature = signature,
    max_size = 20,
    min_alpha = 0.3,
    max_alpha = 0.9,
    width = 7,
    height = 5
  )

  expect_s3_class(result, "ggplot")
})

test_that("plot_wordcloud handles different signature sizes", {
  data <- data.frame(
    TP53 = 1:4,
    EGFR = 2:5,
    BRCA1 = 3:6,
    MYC = 4:7,
    KRAS = 5:8
  )

  for (n in c(1, 2, 4)) {
    signature <- data.frame(
      feature = colnames(data)[seq_len(n)],
      coefficient = seq_len(n)
    )

    result <- plot_wordcloud(
      data = data,
      signature = signature
    )

    expect_s3_class(result, "ggplot")
  }
})

test_that("plot_wordcloud maps word size to absolute expression-coefficient weight", {
  data <- data.frame(
    TP53 = c(1, 1, 1, 1),
    EGFR = c(2, 2, 2, 2)
  )

  signature <- data.frame(
    feature = c("TP53", "EGFR"),
    coefficient = c(2, -3)
  )

  result <- plot_wordcloud(
    data = data,
    signature = signature
  )

  expect_equal(
    rlang::as_label(result$mapping$size),
    "AbsWeight"
  )
})

test_that("plot_barplot_score returns a ggplot object", {
  data <- data.frame(
    OS = c(0, 0, 0, 1, 1, 1),
    score = c(-2, -1, 0.5, 1, 2, 3)
  )

  result <- plot_barplot_score(data)

  expect_s3_class(result, "ggplot")
})


test_that("plot_barplot_score supports a custom score cutoff", {
  data <- data.frame(
    OS = c(0, 0, 0, 1, 1, 1),
    score = c(-2, -1, 0.5, 1, 2, 3)
  )

  result <- plot_barplot_score(
    data = data,
    score_cutoff = 1
  )

  expect_s3_class(result, "ggplot")
})


test_that("plot_barplot_score removes missing OS and score values", {
  data <- data.frame(
    OS = c(0, 0, 1, 1, NA, 0),
    score = c(-2, NA, 1, 2, 3, -1)
  )

  result <- plot_barplot_score(data)

  expect_s3_class(result, "ggplot")
})


test_that("plot_barplot_score validates required columns", {
  data <- data.frame(
    OS = c(0, 1),
    score = c(-1, 1)
  )

  expect_error(
    plot_barplot_score(
      data = data,
      os_col = "status"
    ),
    "not found"
  )

  expect_error(
    plot_barplot_score(
      data = data,
      score_col = "risk"
    ),
    "not found"
  )
})


test_that("plot_barplot_score validates score column", {
  data <- data.frame(
    OS = c(0, 1),
    score = c("low", "high")
  )

  expect_error(
    plot_barplot_score(data),
    "must be numeric"
  )
})


test_that("plot_barplot_score validates score cutoff", {
  data <- data.frame(
    OS = c(0, 0, 1, 1),
    score = c(-1, 0, 1, 2)
  )

  expect_error(
    plot_barplot_score(
      data = data,
      score_cutoff = c(0, 1)
    ),
    "score_cutoff"
  )
})


test_that("plot_barplot_score validates palette", {
  data <- data.frame(
    OS = c(0, 0, 1, 1),
    score = c(-1, 0, 1, 2)
  )

  expect_error(
    plot_barplot_score(
      data = data,
      palette = "black"
    ),
    "palette"
  )
})


test_that("plot_barplot_score validates plot dimensions", {
  data <- data.frame(
    OS = c(0, 0, 1, 1),
    score = c(-1, 0, 1, 2)
  )

  expect_error(
    plot_barplot_score(data, width = 0),
    "width"
  )

  expect_error(
    plot_barplot_score(data, height = 0),
    "height"
  )
})


test_that("plot_barplot_score validates axis labels", {
  data <- data.frame(
    OS = c(0, 0, 1, 1),
    score = c(-1, 0, 1, 2)
  )

  expect_error(
    plot_barplot_score(data, xlab = 1),
    "xlab"
  )

  expect_error(
    plot_barplot_score(data, ylab = 1),
    "ylab"
  )
})


test_that("plot_barplot_score accepts custom column names", {
  data <- data.frame(
    status = c(0, 0, 1, 1),
    risk_score = c(-1, 0, 1, 2)
  )

  result <- plot_barplot_score(
    data = data,
    os_col = "status",
    score_col = "risk_score"
  )

  expect_s3_class(result, "ggplot")
})


test_that("plot_barplot_score saves a PDF when outprefix is provided", {
  data <- data.frame(
    OS = c(0, 0, 1, 1),
    score = c(-1, 0, 1, 2)
  )

  outprefix <- file.path(tempdir(), "clinprog_barplot_score")

  result <- plot_barplot_score(
    data = data,
    outprefix = outprefix
  )

  expect_s3_class(result, "ggplot")
  expect_true(
    file.exists(paste0(outprefix, "_score_barplot.pdf"))
  )
})

test_that("plot_lineplot_score returns a ggplot object", {
  data <- data.frame(
    OS = c(0, 0, 0, 1, 1, 1),
    OS.time = c(2, 4, 6, 3, 5, 7),
    score = c(-2, -1, 0.5, 1, 2, 3)
  )

  result <- plot_lineplot_score(data)

  expect_s3_class(result, "ggplot")
})


test_that("plot_lineplot_score supports a custom score cutoff", {
  data <- data.frame(
    OS = c(0, 0, 0, 1, 1, 1),
    OS.time = c(2, 4, 6, 3, 5, 7),
    score = c(-2, -1, 0.5, 1, 2, 3)
  )

  result <- plot_lineplot_score(
    data = data,
    score_cutoff = 1
  )

  expect_s3_class(result, "ggplot")
})


test_that("plot_lineplot_score removes missing values", {
  data <- data.frame(
    OS = c(0, 0, 1, 1, NA, 0),
    OS.time = c(2, NA, 5, 7, 8, 10),
    score = c(-2, 0, 1, 2, 3, NA)
  )

  result <- plot_lineplot_score(data)

  expect_s3_class(result, "ggplot")
})


test_that("plot_lineplot_score validates required columns", {
  data <- data.frame(
    OS = c(0, 1),
    OS.time = c(2, 4),
    score = c(-1, 1)
  )

  expect_error(
    plot_lineplot_score(
      data = data,
      os_col = "status"
    ),
    "not found"
  )

  expect_error(
    plot_lineplot_score(
      data = data,
      time_col = "followup"
    ),
    "not found"
  )

  expect_error(
    plot_lineplot_score(
      data = data,
      score_col = "risk"
    ),
    "not found"
  )
})


test_that("plot_lineplot_score validates numeric columns", {
  data <- data.frame(
    OS = c(0, 1),
    OS.time = c(2, 4),
    score = c(-1, 1)
  )

  expect_error(
    plot_lineplot_score(
      data = transform(data, score = c("low", "high"))
    ),
    "must be numeric"
  )

  expect_error(
    plot_lineplot_score(
      data = transform(data, OS.time = c("short", "long"))
    ),
    "must be numeric"
  )
})


test_that("plot_lineplot_score validates score cutoff", {
  data <- data.frame(
    OS = c(0, 0, 1, 1),
    OS.time = c(2, 4, 3, 5),
    score = c(-1, 0, 1, 2)
  )

  expect_error(
    plot_lineplot_score(
      data = data,
      score_cutoff = c(0, 1)
    ),
    "score_cutoff"
  )
})


test_that("plot_lineplot_score validates palette", {
  data <- data.frame(
    OS = c(0, 0, 1, 1),
    OS.time = c(2, 4, 3, 5),
    score = c(-1, 0, 1, 2)
  )

  expect_error(
    plot_lineplot_score(
      data = data,
      palette = "black"
    ),
    "palette"
  )
})


test_that("plot_lineplot_score validates plot dimensions", {
  data <- data.frame(
    OS = c(0, 0, 1, 1),
    OS.time = c(2, 4, 3, 5),
    score = c(-1, 0, 1, 2)
  )

  expect_error(
    plot_lineplot_score(data, width = 0),
    "width"
  )

  expect_error(
    plot_lineplot_score(data, height = 0),
    "height"
  )
})


test_that("plot_lineplot_score validates axis labels", {
  data <- data.frame(
    OS = c(0, 0, 1, 1),
    OS.time = c(2, 4, 3, 5),
    score = c(-1, 0, 1, 2)
  )

  expect_error(
    plot_lineplot_score(data, xlab = 1),
    "xlab"
  )

  expect_error(
    plot_lineplot_score(data, ylab = 1),
    "ylab"
  )
})


test_that("plot_lineplot_score accepts custom column names", {
  data <- data.frame(
    status = c(0, 0, 1, 1),
    followup = c(2, 4, 3, 5),
    risk_score = c(-1, 0, 1, 2)
  )

  result <- plot_lineplot_score(
    data = data,
    os_col = "status",
    time_col = "followup",
    score_col = "risk_score"
  )

  expect_s3_class(result, "ggplot")
})


test_that("plot_lineplot_score saves a PDF when outprefix is provided", {
  data <- data.frame(
    OS = c(0, 0, 1, 1),
    OS.time = c(2, 4, 3, 5),
    score = c(-1, 0, 1, 2)
  )

  outprefix <- file.path(tempdir(), "clinprog_lineplot_score")

  result <- plot_lineplot_score(
    data = data,
    outprefix = outprefix
  )

  expect_s3_class(result, "ggplot")
  expect_true(
    file.exists(
      paste0(outprefix, "_lineplot_score_followup.pdf")
    )
  )
})

test_that("plot_scatter returns a patchwork object", {
  data <- data.frame(
    OS = c(0, 0, 0, 1, 1, 1),
    OS.time = c(2, 4, 6, 3, 5, 7),
    GeneA = c(1, 2, 3, 2, 4, 5),
    GeneB = c(5, 4, 3, 4, 2, 1)
  )

  result <- plot_scatter(data)

  expect_s3_class(result, "patchwork")
})


test_that("plot_scatter accepts a signature", {
  data <- data.frame(
    OS = c(0, 0, 0, 1, 1, 1),
    OS.time = c(2, 4, 6, 3, 5, 7),
    GeneA = c(1, 2, 3, 2, 4, 5),
    GeneB = c(5, 4, 3, 4, 2, 1),
    GeneC = c(2, 3, 4, 3, 5, 6)
  )

  signature <- data.frame(
    feature = c("GeneA", "GeneB", "GeneC"),
    coefficient = c(0.1, -0.8, 0.3)
  )

  result <- plot_scatter(
    data = data,
    signature = signature,
    max_features = 2
  )

  expect_s3_class(result, "patchwork")
})


test_that("plot_scatter supports max_features", {
  data <- data.frame(
    OS = c(0, 0, 0, 1, 1, 1),
    OS.time = c(2, 4, 6, 3, 5, 7),
    GeneA = c(1, 2, 3, 2, 4, 5),
    GeneB = c(5, 4, 3, 4, 2, 1),
    GeneC = c(2, 3, 4, 3, 5, 6)
  )

  result <- plot_scatter(
    data = data,
    max_features = 2
  )

  expect_s3_class(result, "patchwork")
})


test_that("plot_scatter supports expression normalization", {
  data <- data.frame(
    OS = c(0, 0, 0, 1, 1, 1),
    OS.time = c(2, 4, 6, 3, 5, 7),
    GeneA = c(0, 1, 3, 2, 4, 7)
  )

  result <- plot_scatter(
    data = data,
    norm_exp = TRUE
  )

  expect_s3_class(result, "patchwork")
})


test_that("plot_scatter supports disabling statistical annotation", {
  data <- data.frame(
    OS = c(0, 0, 0, 1, 1, 1),
    OS.time = c(2, 4, 6, 3, 5, 7),
    GeneA = c(1, 2, 3, 2, 4, 5)
  )

  result <- plot_scatter(
    data = data,
    add_stat = FALSE
  )

  expect_s3_class(result, "patchwork")
})


test_that("plot_scatter removes missing values", {
  data <- data.frame(
    OS = c(0, 0, 0, 1, 1, 1),
    OS.time = c(2, NA, 6, 3, 5, 7),
    GeneA = c(1, 2, NA, 2, 4, 5)
  )

  result <- plot_scatter(data, add_stat = FALSE)

  expect_s3_class(result, "patchwork")
})


test_that("plot_scatter validates required columns", {
  data <- data.frame(
    OS = c(0, 1, 0, 1),
    OS.time = c(2, 4, 6, 8),
    GeneA = c(1, 2, 3, 4)
  )

  expect_error(
    plot_scatter(data, os_col = "status"),
    "not found"
  )

  expect_error(
    plot_scatter(data, time_col = "followup"),
    "not found"
  )
})


test_that("plot_scatter validates follow-up time", {
  data <- data.frame(
    OS = c(0, 1, 0, 1),
    OS.time = c("short", "long", "short", "long"),
    GeneA = c(1, 2, 3, 4)
  )

  expect_error(
    plot_scatter(data),
    "must be numeric"
  )
})


test_that("plot_scatter validates signature", {
  data <- data.frame(
    OS = c(0, 1, 0, 1),
    OS.time = c(2, 4, 6, 8),
    GeneA = c(1, 2, 3, 4)
  )

  expect_error(
    plot_scatter(
      data = data,
      signature = data.frame(
        feature = "GeneA",
        weight = 1
      )
    ),
    "feature.*coefficient"
  )

  expect_error(
    plot_scatter(
      data = data,
      signature = data.frame(
        feature = "MissingGene",
        coefficient = 1
      )
    ),
    "present in 'data'"
  )
})


test_that("plot_scatter validates max_features", {
  data <- data.frame(
    OS = c(0, 1, 0, 1),
    OS.time = c(2, 4, 6, 8),
    GeneA = c(1, 2, 3, 4)
  )

  expect_error(
    plot_scatter(data, max_features = 0),
    "max_features"
  )

  expect_error(
    plot_scatter(data, max_features = 1.5),
    "max_features"
  )
})


test_that("plot_scatter validates logical arguments", {
  data <- data.frame(
    OS = c(0, 1, 0, 1),
    OS.time = c(2, 4, 6, 8),
    GeneA = c(1, 2, 3, 4)
  )

  expect_error(
    plot_scatter(data, add_stat = 1),
    "add_stat"
  )

  expect_error(
    plot_scatter(data, norm_exp = 1),
    "norm_exp"
  )

  expect_error(
    plot_scatter(data, individual = 1),
    "individual"
  )
})


test_that("plot_scatter validates palette", {
  data <- data.frame(
    OS = c(0, 1, 0, 1),
    OS.time = c(2, 4, 6, 8),
    GeneA = c(1, 2, 3, 4)
  )

  expect_error(
    plot_scatter(data, palette = "black"),
    "palette"
  )
})


test_that("plot_scatter validates plot dimensions and labels", {
  data <- data.frame(
    OS = c(0, 1, 0, 1),
    OS.time = c(2, 4, 6, 8),
    GeneA = c(1, 2, 3, 4)
  )

  expect_error(
    plot_scatter(data, width = 0),
    "width"
  )

  expect_error(
    plot_scatter(data, height = 0),
    "height"
  )

  expect_error(
    plot_scatter(data, xlab = 1),
    "xlab"
  )

  expect_error(
    plot_scatter(data, ylab = 1),
    "ylab"
  )
})


test_that("plot_scatter warns when individual output has no prefix", {
  data <- data.frame(
    OS = c(0, 0, 0, 1, 1, 1),
    OS.time = c(2, 4, 6, 3, 5, 7),
    GeneA = c(1, 2, 3, 2, 4, 5)
  )

  expect_warning(
    result <- plot_scatter(
      data = data,
      individual = TRUE
    ),
    "outprefix"
  )

  expect_s3_class(result, "patchwork")
})


test_that("plot_scatter saves combined and individual PDFs", {
  data <- data.frame(
    OS = c(0, 0, 0, 1, 1, 1),
    OS.time = c(2, 4, 6, 3, 5, 7),
    GeneA = c(1, 2, 3, 2, 4, 5),
    GeneB = c(5, 4, 3, 4, 2, 1)
  )

  outprefix <- file.path(tempdir(), "clinprog_scatter")

  result <- plot_scatter(
    data = data,
    outprefix = outprefix,
    individual = TRUE
  )

  expect_s3_class(result, "patchwork")

  expect_true(
    file.exists(
      paste0(outprefix, "_feature_scatter.pdf")
    )
  )

  expect_true(
    file.exists(
      paste0(outprefix, "_feature_scatter_GeneA.pdf")
    )
  )

  expect_true(
    file.exists(
      paste0(outprefix, "_feature_scatter_GeneB.pdf")
    )
  )
})
