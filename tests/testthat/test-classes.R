test_that("new_clinprog_signature creates and standardizes a signature object", {
  signature <- data.frame(
    feature = c("gene_a", "gene_b", "gene_c"),
    coefficient = c(0.5, -2, 1),
    stringsAsFactors = FALSE
  )

  result <- new_clinprog_signature(
    signature,
    params = list(method = "test"),
    metadata = list(source = "test"),
    call = quote(run_regression())
  )

  expect_s3_class(result, "clinprog_signature")
  expect_named(result, c("signature", "plots", "params", "metadata", "call"))
  expect_equal(result$signature$feature, c("gene_b", "gene_c", "gene_a"))
  expect_equal(result$signature$coefficient, c(-2, 1, 0.5))
  expect_null(result$plots)
  expect_equal(result$params, list(method = "test"))
  expect_equal(result$metadata, list(source = "test"))
  expect_equal(result$call, quote(run_regression()))
  expect_identical(rownames(result$signature), c("1", "2", "3"))
})

test_that("new_clinprog_signature removes missing coefficients", {
  signature <- data.frame(
    feature = c("gene_a", "gene_b", "gene_c"),
    coefficient = c(NA_real_, 0.5, -1),
    stringsAsFactors = FALSE
  )

  result <- new_clinprog_signature(signature)

  expect_equal(result$signature$feature, c("gene_c", "gene_b"))
  expect_equal(result$signature$coefficient, c(-1, 0.5))
})

test_that("new_clinprog_signature rejects invalid signatures", {
  expect_error(
    new_clinprog_signature(list(feature = "gene_a", coefficient = 1)),
    "'signature' must be a data.frame"
  )

  expect_error(
    new_clinprog_signature(data.frame(feature = "gene_a")),
    "'signature' must contain 'feature' and 'coefficient' columns"
  )

  expect_error(
    new_clinprog_signature(data.frame(feature = c("gene_a", "gene_b"),
                                      coefficient = c(NA_real_, NA_real_))),
    "No valid coefficients available after removing NA values"
  )

  expect_error(
    new_clinprog_signature(data.frame(feature = c("gene_a", "gene_b"),
                                      coefficient = c("low", "high"))),
    "'coefficient' column must be numeric"
  )
})

test_that("new_clinprog_survival creates a survival object", {
  signature <- data.frame(feature = "gene_a", coefficient = 1)
  expression_data <- data.frame(OS = c(0, 1), OS.time = c(10, 20), score = c(0.2, 0.8))
  survival_results <- list(univariate = list(table = data.frame()))
  ph_result <- list(test = "placeholder")

  result <- new_clinprog_survival(
    signature = signature,
    score_cutoff = 0.5,
    expression_data = expression_data,
    survival = survival_results,
    ph_result = ph_result,
    clinical_data = data.frame(age = c(50, 60)),
    roc_result = list(auc = 0.7),
    params = list(test = TRUE),
    metadata = list(source = "test"),
    call = quote(run_survival())
  )

  expect_s3_class(result, "clinprog_survival")
  expect_named(
    result,
    c("signature", "score_cutoff", "expression_data", "clinical_data",
      "survival", "roc_result", "ph_result", "plots", "params", "metadata", "call")
  )
  expect_equal(result$signature, signature)
  expect_equal(result$score_cutoff, 0.5)
  expect_equal(result$expression_data, expression_data)
  expect_equal(result$clinical_data, data.frame(age = c(50, 60)))
  expect_equal(result$survival, survival_results)
  expect_equal(result$roc_result, list(auc = 0.7))
  expect_equal(result$ph_result, ph_result)
  expect_equal(result$params, list(test = TRUE))
  expect_equal(result$metadata, list(source = "test"))
  expect_equal(result$call, quote(run_survival()))
})

test_that("new_clinprog_survival rejects invalid required inputs", {
  valid_signature <- data.frame(feature = "gene_a", coefficient = 1)
  valid_expression <- data.frame(OS = 0, OS.time = 10, score = 0.2)
  valid_survival <- list()
  valid_ph <- list(test = "placeholder")

  expect_error(
    new_clinprog_survival(
      signature = list(), score_cutoff = 0.5,
      expression_data = valid_expression, survival = valid_survival,
      ph_result = valid_ph
    ),
    "'signature' must be a data.frame"
  )

  expect_error(
    new_clinprog_survival(
      signature = valid_signature, score_cutoff = 0.5,
      expression_data = list(), survival = valid_survival,
      ph_result = valid_ph
    ),
    "'expression_data' must be a data.frame"
  )

  expect_error(
    new_clinprog_survival(
      signature = valid_signature, score_cutoff = 0.5,
      expression_data = valid_expression, survival = valid_survival,
      ph_result = valid_ph, clinical_data = list()
    ),
    "'clinical_data' must be a data.frame or NULL"
  )

  expect_error(
    new_clinprog_survival(
      signature = valid_signature, score_cutoff = "median",
      expression_data = valid_expression, survival = valid_survival,
      ph_result = valid_ph
    ),
    "'score_cutoff' must be a single numeric value"
  )

  expect_error(
    new_clinprog_survival(
      signature = valid_signature, score_cutoff = c(0.2, 0.5),
      expression_data = valid_expression, survival = valid_survival,
      ph_result = valid_ph
    ),
    "'score_cutoff' must be a single numeric value"
  )

  expect_error(
    new_clinprog_survival(
      signature = valid_signature, score_cutoff = 0.5,
      expression_data = valid_expression, survival = "invalid",
      ph_result = valid_ph
    ),
    "'survival' must be a list"
  )

  expect_error(
    new_clinprog_survival(
      signature = valid_signature, score_cutoff = 0.5,
      expression_data = valid_expression, survival = valid_survival,
      ph_result = NULL
    ),
    "'ph_result' cannot be NULL"
  )
})

test_that("new_clinprog_complete creates a complete object", {
  regression <- new_clinprog_signature(
    data.frame(feature = "gene_a", coefficient = 1)
  )
  survival <- new_clinprog_survival(
    signature = regression$signature,
    score_cutoff = 0.5,
    expression_data = data.frame(OS = 0, OS.time = 10, score = 0.2),
    survival = list(),
    ph_result = list(test = "placeholder")
  )

  result <- new_clinprog_complete(
    regression = regression,
    survival = survival,
    params = list(test = TRUE),
    metadata = list(source = "test"),
    call = quote(run_complete())
  )

  expect_s3_class(result, "clinprog_complete")
  expect_named(result, c("regression", "survival", "params", "metadata", "call"))
  expect_equal(result$regression, regression)
  expect_equal(result$survival, survival)
  expect_equal(result$params, list(test = TRUE))
  expect_equal(result$metadata, list(source = "test"))
  expect_equal(result$call, quote(run_complete()))
})

test_that("new_clinprog_complete rejects invalid inputs", {
  regression <- new_clinprog_signature(
    data.frame(feature = "gene_a", coefficient = 1)
  )
  survival <- new_clinprog_survival(
    signature = regression$signature,
    score_cutoff = 0.5,
    expression_data = data.frame(OS = 0, OS.time = 10, score = 0.2),
    survival = list(),
    ph_result = list(test = "placeholder")
  )

  expect_error(
    new_clinprog_complete(regression = list(), survival = survival),
    "'regression' must be a 'clinprog_signature' object"
  )

  expect_error(
    new_clinprog_complete(regression = regression, survival = list()),
    "'survival' must be a 'clinprog_survival' object"
  )

  expect_error(
    new_clinprog_complete(regression = regression, survival = survival, params = "invalid"),
    "'params' must be NULL or a list"
  )

  expect_error(
    new_clinprog_complete(regression = regression, survival = survival, metadata = "invalid"),
    "'metadata' must be NULL or a list"
  )
})
