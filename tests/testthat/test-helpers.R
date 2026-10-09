test_that("clinprog_followup preserves data when no censoring is needed", {
  data <- data.frame(
    OS = c(1, 0, 1, 0),
    OS.time = c(2, 4, 6, 8),
    GeneA = c(3, 2, 5, 1)
  )

  expect_message(
    result <- clinprog_followup(data, followup = NULL),
    "\\[INFO\\].*Returning original data.frame|Returning original data.frame"
  )
  expect_identical(result, data)

  expect_message(
    result <- clinprog_followup(data, followup = 10),
    "\\[INFO\\].*Returning original data.frame|Returning original data.frame"
  )
  expect_identical(result, data)
})

test_that("clinprog_followup applies administrative censoring", {
  data <- data.frame(
    OS = c(1, 1, 0, 1),
    OS.time = c(2, 4, 6, 8),
    GeneA = c(3, 2, 5, 1)
  )

  result <- clinprog_followup(data, followup = 5)

  expect_equal(result$OS, c(1, 1, 0, 0))
  expect_equal(result$OS.time, c(2, 4, 5, 5))
  expect_equal(result$GeneA, data$GeneA)
  # The input object should not be modified by reference.
  expect_equal(data$OS, c(1, 1, 0, 1))
  expect_equal(data$OS.time, c(2, 4, 6, 8))
})

test_that("clinprog_followup validates its inputs through the logging layer", {
  expect_error(
    clinprog_followup("not a data frame"),
    "\\[ERROR\\].*data.frame"
  )

  expect_error(
    clinprog_followup(data.frame(OS = c(0, 1))),
    "\\[ERROR\\].*OS.*OS.time"
  )

  data <- data.frame(OS = c(0, 1), OS.time = c(1, 2))
  expect_error(clinprog_followup(data, followup = 0), "\\[ERROR\\].*followup")
  expect_error(clinprog_followup(data, followup = "5"), "\\[ERROR\\].*followup")
  expect_error(clinprog_followup(data, followup = c(2, 3)), "\\[ERROR\\].*followup")
})

test_that("clinprog_varfun removes low-variance features and preserves survival columns", {
  data <- data.frame(
    status = rep(c(0, 1), each = 10),
    time = seq_len(20),
    constant = rep(5, 20),
    varying = seq_len(20)
  )

  result <- suppressMessages(clinprog_varfun(
    cmatrix = data,
    var_threshold = 0.01,
    force = TRUE
  ))

  expect_named(result, c("OS", "OS.time", "varying"))
  expect_equal(result$OS, data$status)
  expect_equal(result$OS.time, data$time)
  expect_equal(result$varying, data$varying)
})

test_that("clinprog_varfun rejects zero-only feature columns", {
  data <- data.frame(
    OS = rep(c(0, 1), each = 5),
    OS.time = seq_len(10),
    GeneA = rep(0, 10),
    GeneB = seq_len(10)
  )

  expect_error(
    clinprog_varfun(data, var_threshold = 0.01, force = TRUE),
    "\\[ERROR\\].*only zeros|only zeros"
  )
})

test_that("clinprog_corfun handles samples with fewer than two features", {
  data <- data.frame(
    OS = c(0, 1, 0, 1),
    OS.time = c(1, 2, 3, 4),
    GeneA = c(2, 4, 1, 3)
  )

  expect_equal(clinprog_corfun(data, cor_threshold = 0.5), 0)
})

test_that("clinprog_corfun identifies highly correlated features", {
  x <- seq_len(30)
  data <- data.frame(
    OS = rep(c(0, 1), 15),
    OS.time = seq_len(30),
    GeneA = x,
    GeneB = x
  )

  expect_message(
    result <- clinprog_corfun(data, cor_threshold = 1),
    "Iteration skipped due to high correlation"
  )
  expect_equal(result, 1)
})

test_that("clinprog_corfun does not reject strongly negatively correlated features", {
  x <- seq_len(30)
  data <- data.frame(
    OS = rep(c(0, 1), 15),
    OS.time = seq_len(30),
    GeneA = x,
    GeneB = rev(x)
  )

  expect_equal(clinprog_corfun(data, cor_threshold = 1), 0)
})

test_that("clinprog_subsample preserves survival columns and is reproducible", {
  data <- data.frame(
    OS = rep(c(0, 1), 10),
    OS.time = seq_len(20),
    GeneA = seq_len(20),
    GeneB = seq_len(20) * 2,
    GeneC = seq_len(20) * 3,
    GeneD = seq_len(20) * 4
  )

  first <- suppressMessages(clinprog_subsample(data, group_size = 2, seed = 42))
  second <- suppressMessages(clinprog_subsample(data, group_size = 2, seed = 42))

  expect_identical(first, second)
  expect_identical(names(first)[1:2], c("OS", "OS.time"))
  expect_equal(ncol(first), 4)
  expect_true(all(names(first)[-(1:2)] %in% names(data)[-(1:2)]))
})

test_that("clinprog_subsample validates feature availability and group size", {
  no_features <- data.frame(OS = c(0, 1), OS.time = c(1, 2))
  expect_error(
    clinprog_subsample(no_features, group_size = 1),
    "\\[ERROR\\].*No features available|No features available"
  )

  data <- data.frame(
    OS = c(0, 1),
    OS.time = c(1, 2),
    GeneA = c(3, 4)
  )
  expect_error(
    clinprog_subsample(data, group_size = 2),
    "\\[ERROR\\].*group_size"
  )
})

test_that("clinprog_feature_check continues when enough features remain", {
  data <- data.frame(
    OS = c(0, 1, 0, 1),
    OS.time = c(1, 2, 3, 4),
    GeneA = 1:4,
    GeneB = 4:1
  )

  expect_null(clinprog_feature_check(dataf = data, g = 2))
})

test_that("clinprog_feature_check rejects data without features", {
  data <- data.frame(
    OS = c(0, 1),
    OS.time = c(1, 2)
  )

  expect_error(
    clinprog_feature_check(dataf = data, g = 2),
    "\\[ERROR\\].*No features remaining"
  )
})

test_that("clinprog_bootstrap validates its inputs", {
  expect_error(
    clinprog_bootstrap(raw_data = "not a data frame",
                       bootstrap_data = list(strap = list()),
                       iteration = 1),
    "\\[ERROR\\].*raw_data"
  )

  raw_data <- data.frame(OS = c(0, 1), OS.time = c(1, 2))
  expect_error(
    clinprog_bootstrap(raw_data = raw_data, bootstrap_data = list(), iteration = 1),
    "\\[ERROR\\].*bootstrap_data"
  )

  bootstrap_data <- list(strap = list(list(id = c(1, 2))))
  expect_error(
    clinprog_bootstrap(raw_data, bootstrap_data, iteration = 0),
    "\\[ERROR\\].*iteration"
  )
  expect_error(
    clinprog_bootstrap(raw_data, bootstrap_data, iteration = 2),
    "\\[ERROR\\].*iteration"
  )
  expect_error(
    clinprog_bootstrap(raw_data, bootstrap_data, iteration = 1, min.prop = 0.5),
    "\\[ERROR\\].*min.prop"
  )
})

test_that("clinprog_bootstrap returns a valid resample when covariates are balanced binary variables", {
  raw_data <- data.frame(
    OS = c(0, 1, 0, 1, 0, 1),
    OS.time = 1:6,
    score = c(1, 2, 3, 4, 5, 6),
    score_group = factor(c("low", "high", "low", "high", "low", "high")),
    stage = c("I", "II", "I", "II", "I", "II")
  )
  bootstrap_data <- list(strap = list(list(id = 1:6)))

  result <- clinprog_bootstrap(raw_data, bootstrap_data, iteration = 1)

  expect_equal(result, raw_data)
})

test_that("clinprog_bootstrap records why a resample is rejected", {
  raw_data <- data.frame(
    OS = c(0, 1, 0, 1),
    OS.time = 1:4,
    stage = c("I", "I", "I", "II")
  )
  bootstrap_data <- list(strap = list(list(id = 1:3)))

  result <- clinprog_bootstrap(raw_data, bootstrap_data, iteration = 1)

  expect_equal(nrow(as.data.frame(result)), 0)
  expect_match(attr(result, "reason"), "stage")
})

test_that("clinprog_merge combines covariate selection frequencies", {
  tables <- list(
    data.frame(
      variable = c("score_grouphigh", "stageII"),
      prognosis = c("worse", "better")
    ),
    data.frame(
      variable = c("score_grouplow", "stageII"),
      prognosis = c("better", "better")
    ),
    data.frame(
      variable = "stageI",
      prognosis = "worse"
    )
  )

  result <- clinprog_merge(tables, covariates = "stage")

  expect_equal(
    result$frequency[match("score_group", result$covariate)],
    1L
  )

  expect_equal(
    result$frequency[match("stage", result$covariate)],
    1L
  )

  expect_equal(
    sum(result$covariate == "score_group"),
    2L
  )

  expect_equal(
    sum(result$covariate == "stage"),
    2L
  )
})

test_that("clinprog_merge validates its inputs and handles empty results", {
  expect_error(clinprog_merge(list(), covariates = "stage"),
               "\\[ERROR\\].*non-empty list")
  expect_error(clinprog_merge(list(data.frame(variable = "stage", prognosis = "worse")),
                              covariates = character()),
               "\\[ERROR\\].*covariates")
  expect_error(clinprog_merge(list(data.frame(variable = "stage")),
                              covariates = "stage"),
               "\\[ERROR\\].*prognosis")

  expect_warning(
    result <- clinprog_merge(
      list(data.frame(variable = character(), prognosis = character())),
      covariates = "stage"
    ),
    "No valid bootstrap iterations"
  )
  expect_null(result)
})
