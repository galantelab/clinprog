test_that("bootstrap retries after correlation rejection and serial/parallel signatures match", {
  # Keep fixture generation isolated from the caller's RNG state.
  withr::local_seed(20261022)

  # Select feature pairs exactly as clinprog_subsample() does. Pick a feature
  # count for which seed 1 and seed 2 produce different subsets.
  sample_features <- function(seed, features) {
    set.seed(seed)
    sample(features, size = 2L, replace = FALSE)
  }

  fixture <- NULL
  for (n_features in 3:10) {
    feature_names <- paste0("Gene", seq_len(n_features))
    rejected_features <- sample_features(1L, feature_names)
    next_features <- sample_features(2L, feature_names)

    if (!setequal(rejected_features, next_features)) {
      fixture <- list(
        n_features = n_features,
        feature_names = feature_names,
        rejected_features = rejected_features,
        next_features = next_features
      )
      break
    }
  }

  if (is.null(fixture)) {
    skip("Could not construct distinct feature subsets for seeds 1 and 2")
  }

  n <- 100L
  n_features <- fixture$n_features
  feature_names <- fixture$feature_names
  rejected_features <- fixture$rejected_features

  # Construct orthogonal feature vectors, then make the pair selected by seed
  # 1 perfectly correlated. All other pairs should pass the correlation filter.
  set.seed(20261022)
  basis <- qr.Q(qr(matrix(
    stats::rnorm(n * (n_features - 1L)),
    nrow = n,
    ncol = n_features - 1L
  ))) * sqrt(n)

  feature_matrix <- matrix(
    NA_real_,
    nrow = n,
    ncol = n_features,
    dimnames = list(NULL, feature_names)
  )

  independent_column <- 2L
  for (feature in feature_names) {
    if (feature %in% rejected_features) {
      feature_matrix[, feature] <- basis[, 1L]
    } else {
      feature_matrix[, feature] <- basis[, independent_column]
      independent_column <- independent_column + 1L
    }
  }

  # Generate event and censoring times from a proportional-hazards setup so
  # the accepted samples contain a useful survival signal.
  set.seed(1729)
  linear_predictor <- rowSums(feature_matrix) / sqrt(n_features)
  event_time <- stats::rexp(n, rate = exp(0.3 * linear_predictor))
  censor_time <- stats::rexp(n, rate = 0.15)
  os <- as.integer(event_time <= censor_time)
  os_time <- pmin(event_time, censor_time)

  full_data <- data.frame(
    OS = os,
    OS.time = os_time,
    as.data.frame(feature_matrix, check.names = FALSE),
    check.names = FALSE
  )

  # Verify the fixture really causes seed 1 to be rejected and seed 2 to pass.
  first_sample <- suppressMessages(
    clinprog_subsample(full_data, group_size = 2L, seed = 1L)
  )
  second_sample <- suppressMessages(
    clinprog_subsample(full_data, group_size = 2L, seed = 2L)
  )

  expect_setequal(names(first_sample)[-(1:2)], rejected_features)
  expect_setequal(names(second_sample)[-(1:2)], fixture$next_features)
  expect_equal(
    suppressMessages(clinprog_corfun(first_sample, cor_threshold = 1)),
    1
  )
  expect_equal(
    suppressMessages(clinprog_corfun(second_sample, cor_threshold = 1)),
    0
  )

  # The serial path must move from seed 1 (rejected) to seed 2 (accepted),
  # rather than repeatedly sampling the same rejected pair. The parallel path
  # processes seeds 1 and 2 in its first batch. Since the only deliberate
  # failure here is the correlation filter, both paths should use the same
  # first valid sample and produce identical signatures.
  serial_signature <- suppressMessages(
    clinprog_bootstrapfun(
      full_data = full_data,
      bootstrap = 1L,
      group_size = 2L,
      cor_threshold = 1,
      coef_threshold = 0,
      n_cores = 1L,
      seed = 123L
    )
  )

  parallel_signature <- suppressMessages(
    clinprog_bootstrapfun(
      full_data = full_data,
      bootstrap = 1L,
      group_size = 2L,
      cor_threshold = 1,
      coef_threshold = 0,
      n_cores = 2L,
      seed = 123L
    )
  )

  expect_true(is.data.frame(serial_signature))
  expect_true(nrow(serial_signature) > 0L)
  expect_identical(serial_signature, parallel_signature)

  # The rejected two-feature subset must not have been used as one successful
  # iteration. The accepted subset differs from it by at least one feature.
  expect_lt(
    sum(rejected_features %in% serial_signature$feature),
    length(rejected_features)
  )
})
