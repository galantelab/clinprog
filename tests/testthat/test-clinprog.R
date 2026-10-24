# Tests using the simulated toy datasets (see data-raw/README.md).
# Quick structural checks run everywhere; the regression tests are slower
# (bootstrap = 30) and are skipped on CRAN.

load_toy <- function() {
  e <- new.env()
  data(list = c("toy_clinics", "toy_gene_expression", "toy_transcript_expression"),
       package = "clinprog", envir = e)
  as.list(e)
}

############################################ DATA INTEGRITY (fast) ############################################

test_that("toy datasets are consistent with each other", {
  toy <- load_toy()
  clin <- toy$toy_clinics
  gene <- toy$toy_gene_expression
  tx   <- toy$toy_transcript_expression

  # Same patients everywhere, identified by row names
  expect_setequal(rownames(gene), rownames(tx))
  expect_setequal(rownames(gene), rownames(clin))
  expect_false(anyDuplicated(rownames(gene)) > 0)

  # Survival columns come first and are identical in both expression tables
  expect_equal(colnames(gene)[1:2], c("OS", "OS.time"))
  expect_equal(colnames(tx)[1:2], c("OS", "OS.time"))
  expect_equal(gene[, c("OS", "OS.time")], tx[rownames(gene), c("OS", "OS.time")])
  expect_true(all(gene$OS %in% c(0, 1)))
  expect_true(all(gene$OS.time > 0))

  # Clinical variables
  expect_equal(colnames(clin), c("Age", "Gender", "Race", "Histotype", "Grade"))
  expect_setequal(clin$Age, c("Low", "High"))
  expect_setequal(clin$Grade, c("G2", "G3"))
  expect_setequal(clin$Histotype, c("Astrocytoma", "Oligodendroglioma"))
})

test_that("toy expression tables meet clinprog input rules", {
  toy <- load_toy()
  for (x in list(toy$toy_gene_expression, toy$toy_transcript_expression)) {
    expr <- x[, -(1:2)]
    expect_true(all(vapply(expr, is.numeric, logical(1))))
    expect_false(anyNA(expr))
    expect_true(all(expr >= 0))
    # clinprog rejects columns containing only zeros
    expect_false(any(colSums(expr) == 0))
  }
  # Event proportion that clinprog accepts without skipping survival checks
  events <- mean(toy$toy_gene_expression$OS)
  expect_true(events > 0.2 && events < 0.8)
})

test_that("shipped .rda objects match the raw TSV files", {
  toy <- load_toy()
  pairs <- c(toy_clinics = "toy_clinics.tsv",
             toy_gene_expression = "toy_gene_expression.tsv",
             toy_transcript_expression = "toy_transcript_expression.tsv")
  for (nm in names(pairs)) {
    raw <- read_table(system.file("extdata", pairs[[nm]], package = "clinprog"), sep = "\t")
    expect_equal(raw, toy[[nm]])
  }
})

test_that("planted features listed in the truth file exist in the data", {
  truth_file <- system.file("fixtures", "toy_truth.tsv", package = "clinprog")
  skip_if(truth_file == "", "toy_truth.tsv not installed")
  truth <- read.delim(truth_file, stringsAsFactors = FALSE)
  toy <- load_toy()

  genes <- truth$feature[truth$file == "gene"]
  txs   <- truth$feature[truth$file == "transcript"]
  expect_true(all(genes %in% colnames(toy$toy_gene_expression)))
  expect_true(all(txs %in% colnames(toy$toy_transcript_expression)))
})

############################################ REGRESSION (slow) ################################################

check_signature <- function(sig, risk_pattern, prot_pattern, junk) {
  expect_true(all(c("feature", "coefficient") %in% colnames(sig)))
  planted <- grepl(paste0("^(", risk_pattern, "|", prot_pattern, ")"), sig$feature)
  # The planted prognostic features are recovered ...
  expect_gt(sum(planted), 0)
  # ... with the right direction of effect
  expect_true(all(sig$coefficient[grepl(paste0("^", risk_pattern), sig$feature)] > 0))
  expect_true(all(sig$coefficient[grepl(paste0("^", prot_pattern), sig$feature)] < 0))
  # The near-zero-variance feature is removed by the variance filter
  expect_false(junk %in% sig$feature)
}

test_that("run_regression recovers the planted genes", {
  skip_on_cran()
  gene <- load_toy()$toy_gene_expression

  result <- run_regression(
    data = gene,
    bootstrap = 30,
    group_size = 10,
    feature_type = "gene",
    norm_exp = TRUE,
    skip_survival_checks = TRUE,
    n_cores = 1,
    seed = 123
  )

  expect_s3_class(result, "clinprog_signature")
  check_signature(result$signature, "TOYRISK", "TOYPROT", "TOYLOWVAR")
})

test_that("run_regression recovers the planted transcripts", {
  skip_on_cran()
  tx <- load_toy()$toy_transcript_expression

  result <- run_regression(
    data = tx,
    bootstrap = 30,
    group_size = 10,
    feature_type = "transcript",
    norm_exp = TRUE,
    skip_survival_checks = TRUE,
    n_cores = 1,
    seed = 123
  )

  expect_s3_class(result, "clinprog_signature")
  check_signature(result$signature, "MSTRG\\.9001", "MSTRG\\.9002", "MSTRG.9999.1")
})

test_that("run_regression is reproducible on the toy genes", {
  skip_on_cran()
  gene <- load_toy()$toy_gene_expression
  args <- list(data = gene, bootstrap = 10, group_size = 10, feature_type = "gene",
               norm_exp = TRUE, skip_survival_checks = TRUE, n_cores = 1, seed = 123)

  r1 <- do.call(run_regression, args)
  r2 <- do.call(run_regression, args)
  expect_equal(r1$signature, r2$signature)
})

############################################ FULL WORKFLOW (slow) #############################################

test_that("run_complete works on the toy genes with clinical data", {
  skip_on_cran()
  toy <- load_toy()
  output_prefix <- file.path(tempdir(), "toy_run_complete")

  result <- run_complete(
    data = toy$toy_gene_expression,
    output_prefix = output_prefix,
    bootstrap = 30,
    group_size = 10,
    feature_type = "gene",
    norm_exp = TRUE,
    run_multivariate = TRUE,
    clinical_data = toy$toy_clinics,
    score_cutoff = "median",
    skip_survival_checks = TRUE,
    n_cores = 1,
    seed = 123
  )

  expect_s3_class(result, "clinprog_complete")
  expect_s3_class(result$regression, "clinprog_signature")
  expect_s3_class(result$survival, "clinprog_survival")
  expect_gt(nrow(result$regression$signature), 0)
})
