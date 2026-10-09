# ====================================================================================================================
# Tests for io.R
#
# Covers:
#   - read_table()
#   - read_rds()
#   - write_rds()
#   - write_signature()
#   - write_score()
#   - write_univariate()
#   - write_multivariate()
#   - write_metadata()
#
# Lightweight tests (no skip_on_cran()).
# ====================================================================================================================

# Helper: build a tiny synthetic signature
make_sig <- function() {
  data.frame(
    feature = c("GENE_A", "GENE_B", "GENE_C"),
    coefficient = c(0.5, -0.3, 0.1),
    stringsAsFactors = FALSE
  )
}

# Helper: build a tiny synthetic score data.frame
make_score <- function() {
  data.frame(
    OS = c(0, 1, 0, 1),
    OS.time = c(100, 200, 300, 400),
    score = c(-1.2, 0.4, 0.9, -0.6),
    stringsAsFactors = FALSE
  )
}

# Helper: build a tiny synthetic univariate result
make_uni <- function() {
  data.frame(
    feature = "score",
    hazard.ratio = "1.5 (95% CI, 1.1 - 2.0)",
    log.rank.pvalue = 0.01,
    stringsAsFactors = FALSE
  )
}

# Helper: build a tiny synthetic multivariate result
make_multi <- function() {
  data.frame(
    variable = c("score_group", "Age"),
    condition = c("low", "Old"),
    univariate.hazard.ratio = c("1.5", "1.2"),
    univariate.Cox.pvalue = c(0.01, 0.05),
    univariate.prognosis = c("worse", "worse"),
    multivariate.hazard.ratio = c("1.4", "1.1"),
    multivariate.Cox.pvalue = c(0.02, 0.10),
    multivariate.prognosis = c("worse", "---"),
    stringsAsFactors = FALSE
  )
}

# --------------------------------------------------------
# read_table
# --------------------------------------------------------

test_that("read_table reads a TSV with row names", {
  tmp <- tempfile(fileext = ".tsv")
  df <- data.frame(a = 1:3, b = 4:6, row.names = c("s1", "s2", "s3"))
  write.table(df, tmp, sep = "\t", quote = FALSE)
  
  res <- read_table(tmp, row.names = TRUE)
  expect_s3_class(res, "data.frame")
  expect_identical(nrow(res), 3L)
  expect_identical(ncol(res), 2L)
  expect_identical(rownames(res), c("s1", "s2", "s3"))
})

test_that("read_table reads a TSV without row names", {
  tmp <- tempfile(fileext = ".tsv")
  df <- data.frame(a = 1:3, b = 4:6)
  write.table(df, tmp, sep = "\t", quote = FALSE, row.names = FALSE)
  
  res <- read_table(tmp, row.names = FALSE)
  expect_s3_class(res, "data.frame")
  expect_identical(nrow(res), 3L)
})

test_that("read_table validates its arguments", {
  expect_error(read_table(123), regexp = "single character string")
  
  # Create a real file so the validation order doesn't short-circuit
  tmp <- tempfile(fileext = ".tsv")
  writeLines("a\tb\n1\t2", tmp)
  expect_error(read_table(tmp, row.names = "yes"), regexp = "TRUE or FALSE")
  
  # Non-existent file (separate test since it short-circuits)
  expect_error(read_table("non_existent_file.tsv"), regexp = "does not exist")
})

# --------------------------------------------------------
# write_signature
# --------------------------------------------------------

test_that("write_signature writes a valid data.frame", {
  tmp <- tempfile(fileext = ".tsv")
  write_signature(make_sig(), tmp)
  expect_true(file.exists(tmp))
  
  # Round-trip
  res <- read.delim(tmp, sep = "\t")
  expect_identical(nrow(res), 3L)
  expect_true(all(c("feature", "coefficient") %in% colnames(res)))
})

test_that("write_signature validates input", {
  tmp <- tempfile(fileext = ".tsv")
  # Missing required columns
  expect_error(
    write_signature(data.frame(x = 1:3), tmp),
    regexp = "'feature' and 'coefficient'"
  )
  # Bad input type
  expect_error(
    write_signature(42, tmp),
    regexp = "must be of class"
  )
})

# --------------------------------------------------------
# write_score
# --------------------------------------------------------

test_that("write_score writes a valid data.frame", {
  tmp <- tempfile(fileext = ".tsv")
  write_score(make_score(), tmp)
  expect_true(file.exists(tmp))
  
  res <- read.delim(tmp, sep = "\t")
  expect_identical(nrow(res), 4L)
})

test_that("write_score validates input", {
  tmp <- tempfile(fileext = ".tsv")
  expect_error(
    write_score(data.frame(x = 1:3), tmp),
    regexp = "'OS', 'OS.time', and 'score'"
  )
})

# --------------------------------------------------------
# write_univariate
# --------------------------------------------------------

test_that("write_univariate writes a valid data.frame", {
  tmp <- tempfile(fileext = ".tsv")
  write_univariate(make_uni(), tmp)
  expect_true(file.exists(tmp))
  
  res <- read.delim(tmp, sep = "\t")
  expect_identical(nrow(res), 1L)
})

test_that("write_univariate validates input", {
  tmp <- tempfile(fileext = ".tsv")
  expect_error(
    write_univariate(data.frame(x = 1:3), tmp),
    regexp = "must contain at least"
  )
})

# --------------------------------------------------------
# write_multivariate
# --------------------------------------------------------

test_that("write_multivariate writes a valid data.frame", {
  tmp <- tempfile(fileext = ".tsv")
  write_multivariate(make_multi(), tmp)
  expect_true(file.exists(tmp))
  
  res <- read.delim(tmp, sep = "\t")
  expect_identical(nrow(res), 2L)
})

test_that("write_multivariate validates input", {
  tmp <- tempfile(fileext = ".tsv")
  expect_error(
    write_multivariate(data.frame(x = 1:3), tmp),
    regexp = "must contain valid multivariate"
  )
})

# --------------------------------------------------------
# write_rds / read_rds
# --------------------------------------------------------

test_that("write_rds and read_rds round-trip a signature object", {
  sig <- new_clinprog_signature(make_sig())
  tmp <- tempfile(fileext = ".rds")
  
  write_rds(sig, tmp)
  expect_true(file.exists(tmp))
  
  loaded <- read_rds(tmp)
  expect_s3_class(loaded, "clinprog_signature")
  expect_identical(nrow(loaded$signature), 3L)
})

test_that("write_rds validates input", {
  expect_error(
    write_rds(list(a = 1), tempfile(fileext = ".rds")),
    regexp = "must be a valid clinprog object"
  )
})

test_that("read_rds validates input", {
  expect_error(read_rds("non_existent.rds"), regexp = "does not exist")
  
  # Write a non-clinprog object
  tmp <- tempfile(fileext = ".rds")
  saveRDS(list(a = 1), tmp)
  expect_error(read_rds(tmp), regexp = "not a valid clinprog object")
})

# --------------------------------------------------------
# write_metadata
# --------------------------------------------------------

test_that("write_metadata writes a JSON file", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  params <- list(a = 1, b = "text", c = TRUE)
  write_metadata(
    module = "run_regression",
    outprefix = file.path(tmp_dir, "test_run"),
    parameters = params,
    command = "none"
  )
  
  out_file <- file.path(tmp_dir, "test_run_metadata_regression.json")
  expect_true(file.exists(out_file))
  
  # Parse it back
  json <- jsonlite::fromJSON(out_file)
  expect_identical(json$module, "run_regression")
  expect_equal(json$parameters$a, 1)
  expect_identical(json$parameters$b, "text")
})

test_that("write_metadata validates input", {
  expect_error(
    write_metadata("bad_module", "x", list()),
    regexp = "must be one of"
  )
  expect_error(
    write_metadata("run_regression", 123, list()),
    regexp = "must be a character string"
  )
  expect_error(
    write_metadata("run_regression", "x", "not a list"),
    regexp = "must be a named list"
  )
})
