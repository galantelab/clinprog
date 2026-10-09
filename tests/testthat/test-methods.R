# ====================================================================================================================
# Tests for methods.R
#
# Covers:
#   - print.clinprog_signature
#   - summary.clinprog_signature
#   - print.summary.clinprog_signature
#   - as.data.frame.clinprog_signature
#   - coef.clinprog_signature
#   - plot.clinprog_signature
#   - print.clinprog_survival
#   - summary.clinprog_survival
#   - coef.clinprog_survival
#   - plot.clinprog_survival
#   - print.clinprog_complete
#   - summary.clinprog_complete
#   - coef.clinprog_complete
#   - plot.clinprog_complete
#
# Lightweight tests (no skip_on_cran()).
# ====================================================================================================================

# Helper: build a minimal clinprog_signature
make_sig_obj <- function() {
  new_clinprog_signature(data.frame(
    feature = c("GENE_A", "GENE_B"),
    coefficient = c(0.5, -0.3),
    stringsAsFactors = FALSE
  ))
}

# Helper: build a minimal clinprog_survival
make_surv_obj <- function() {
  # Minimal coxph model via survival
  df <- data.frame(
    OS = c(0, 1, 0, 1, 1, 0, 1, 0),
    OS.time = c(100, 200, 300, 400, 500, 600, 700, 800),
    x = c(0, 1, 0, 1, 0, 1, 0, 1)
  )
  model <- survival::coxph(survival::Surv(OS.time, OS) ~ x, data = df)
  
  new_clinprog_survival(
    signature = data.frame(
      feature = c("GENE_A", "GENE_B"),
      coefficient = c(0.5, -0.3),
      stringsAsFactors = FALSE
    ),
    score_cutoff = 0,
    expression_data = df,
    survival = list(univariate = list(model = model, table = NULL)),
    ph_result = survival::cox.zph(model),
    clinical_data = NULL
  )
}

# --------------------------------------------------------
# clinprog_signature
# --------------------------------------------------------

test_that("print.clinprog_signature shows feature count and preview", {
  obj <- make_sig_obj()
  expect_output(print(obj), "clinprog signature object")
  expect_output(print(obj), "Number of features: 2")
})

test_that("summary.clinprog_signature returns a summary object", {
  obj <- make_sig_obj()
  s <- summary(obj)
  expect_s3_class(s, "summary.clinprog_signature")
  expect_identical(s$n_features, 2L)
})

test_that("print.summary.clinprog_signature displays correctly", {
  obj <- make_sig_obj()
  s <- summary(obj)
  expect_output(print(s), "clinprog signature summary")
})

test_that("as.data.frame.clinprog_signature returns the underlying signature", {
  obj <- make_sig_obj()
  df <- as.data.frame(obj)
  expect_s3_class(df, "data.frame")
  expect_identical(nrow(df), 2L)
  expect_true(all(c("feature", "coefficient") %in% colnames(df)))
})

test_that("coef.clinprog_signature returns a named numeric vector", {
  obj <- make_sig_obj()
  cf <- coef(obj)
  expect_type(cf, "double")
  expect_named(cf, c("GENE_A", "GENE_B"))
  expect_equal(unname(cf), c(0.5, -0.3))
})

# --------------------------------------------------------
# clinprog_survival
# --------------------------------------------------------

test_that("print.clinprog_survival displays core fields", {
  obj <- make_surv_obj()
  expect_output(print(obj), "clinprog survival object")
  expect_output(print(obj), "Signature features: 2")
})

test_that("summary.clinprog_survival returns a summary object", {
  obj <- make_surv_obj()
  s <- summary(obj)
  expect_s3_class(s, "summary.clinprog_survival")
  expect_identical(s$n_features, 2L)
})

test_that("print.summary.clinprog_survival displays correctly", {
  obj <- make_surv_obj()
  s <- summary(obj)
  expect_output(print(s), "clinprog survival summary")
})

test_that("as.data.frame.clinprog_survival returns the underlying signature", {
  obj <- make_surv_obj()
  df <- as.data.frame(obj)
  expect_s3_class(df, "data.frame")
  expect_identical(nrow(df), 2L)
})

test_that("coef.clinprog_survival returns a named numeric vector", {
  obj <- make_surv_obj()
  cf <- coef(obj)
  expect_type(cf, "double")
  expect_named(cf, c("GENE_A", "GENE_B"))
})

# --------------------------------------------------------
# clinprog_complete
# --------------------------------------------------------

test_that("print.clinprog_complete displays core fields", {
  sig <- make_sig_obj()
  # Minimal valid survival for the complete object
  surv <- make_surv_obj()
  
  complete <- new_clinprog_complete(
    regression = sig,
    survival = surv,
    metadata = list(
      timestamp = Sys.time(),
      R_version = R.version.string,
      package_version = "1.2.0"
    )
  )
  
  expect_output(print(complete), "clinprog Complete Analysis")
})

test_that("summary.clinprog_complete returns a summary object", {
  sig <- make_sig_obj()
  surv <- make_surv_obj()
  complete <- new_clinprog_complete(
    regression = sig,
    survival = surv,
    metadata = list(
      timestamp = Sys.time(),
      R_version = R.version.string,
      package_version = "1.2.0"
    )
  )
  
  s <- summary(complete)
  expect_s3_class(s, "summary.clinprog_complete")
})

test_that("coef.clinprog_complete extracts coefficients from survival", {
  sig <- make_sig_obj()
  surv <- make_surv_obj()
  complete <- new_clinprog_complete(
    regression = sig,
    survival = surv,
    metadata = list(
      timestamp = Sys.time(),
      R_version = R.version.string,
      package_version = "1.2.0"
    )
  )
  
  cf <- coef(complete)
  expect_type(cf, "double")
  expect_named(cf, c("GENE_A", "GENE_B"))
})
