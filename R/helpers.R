utils::globalVariables(c("variable"))

###### MAIN (REGRESSION - MODULE I) ######

#' Applies maximum follow-up censoring to survival data
#'
#' @description Applies administrative censoring to survival data by limiting follow-up time to a user-defined
#' maximum value. Samples with survival times greater than the specified follow-up are censored.
#'
#' @param data Data.frame containing survival information.
#' @param max_follow_up Numeric. Maximum follow-up time. If \code{NULL} (default), no filtering is applied.
#'
#' @return A filtered data.frame with adjusted \code{OS} and \code{OS.time}.
#' @keywords internal
clinprog_followup <- function(data, max_follow_up = NULL) {
  # Validates input data
  if (!is.data.frame(data)) {log_stop("'data' must be a data.frame")}
  required_cols <- c("OS", "OS.time")
  if (!all(required_cols %in% colnames(data))) {log_stop("Input data must contain columns: 'OS' and 'OS.time'")}

  # Returns original data if max_follow_up is NULL
  if (is.null(max_follow_up))
  {
    log_message("Returning original data.frame (no max_follow_up max value passed)...")
    return(data)
  }

  # Validates max_follow_up
  if (!is.numeric(max_follow_up) || length(max_follow_up) != 1 || max_follow_up <= 0) {log_stop("'max_follow_up' must be a positive numeric value")}

  # Skips filtering if max_follow_up exceeds maximum observed time
  max_observed_time <- max(data$OS.time, na.rm = TRUE)
  if (max_follow_up >= max_observed_time)
  {
    log_message("Returning original data.frame (max_follow_up is greater than or equal to the max 'OS.time')...")
    return(data)
  }

  # Identifies samples exceeding max_follow_up threshold
  idx <- which(data$OS.time > max_follow_up)

  # Applies administrative censoring
  data$OS[idx] <- 0
  data$OS.time[idx] <- max_follow_up

  return(data)
}

#' Variance filtering for clinprog pipeline
#'
#' @description Internal helper function used by \code{run_regression()}.
#' Applies variance-based filtering to remove low-variance features after normalization.
#' Also performs basic checks on survival data.
#'
#' @param cmatrix A data.frame with survival data (first two columns) and expression values.
#' @param var_threshold Numeric. Variance threshold for filtering.
#' @param skip_survival_checks Logical. Whether to bypass survival-related checks.
#'
#' @return A filtered data.frame.
#' @keywords internal
clinprog_varfun <- function(cmatrix, var_threshold, skip_survival_checks) {

  # Handle missing values
  if (anyNA(cmatrix)) {
    log_message("Checking NAs and imputing with mice...")
    impu <- mice::mice(cmatrix, print = FALSE)
    cmatrix <- mice::complete(impu)
  }

  # Forces the first 2 columns to be named OS and OS.time
  colnames(cmatrix)[1:2] <- c("OS", "OS.time")

  # Handles cases where the user has only 1 feature
  if (ncol(cmatrix) == 3) {
    if (any(cmatrix[[3]] == 0)) {log_stop("Feature column contains zero values, cannot normalize")}
    divisor <- max(cmatrix[[3]])
    dividendo <- cmatrix[[3]]
    normalized <- divisor / dividendo
    variance <- stats::var(normalized)

    if (variance < var_threshold) {log_stop("All columns rejected by variance filter")}
    log_message("No columns rejected by variance filter")
  } else {
    maxes <- apply(cmatrix[, 3:ncol(cmatrix), drop = FALSE], 2, max)

    if (0 %in% maxes) {log_stop("Columns containing only zeros found in input data. Remove such columns and try again.")}

    divisor <- cmatrix[, 3:ncol(cmatrix), drop = FALSE]
    dividendo <- matrix(maxes, nrow = nrow(cmatrix), ncol = length(maxes), byrow = TRUE)
    normalized <- divisor / dividendo
    variances <- apply(normalized, 2, stats::var)
    filtered <- which(variances > var_threshold)

    if (length(filtered) == 0) {
      log_stop("All columns rejected by variance filter")
    } else if (length(filtered) == length(variances)) {
      log_message("No columns rejected by variance filter")
    } else {
      losers <- names(variances)[-filtered]
      cmatrix <- cmatrix[, c(1, 2, filtered + 2), drop = FALSE]
      log_message(length(losers), " columns removed by variance filter: ", paste(losers, collapse = ", "))
    }

    # Survival checks
    if (!skip_survival_checks) {
      OSstatus <- cmatrix[, 1]
      percentage <- sum(OSstatus) / length(OSstatus)

      if (percentage < 0.2 || percentage > 0.8) {log_stop("Survival status proportion is not adequate for analysis")}

      max_follow_up <- cmatrix[, 2]
      uplimit <- max(max_follow_up)
      normalized <- max_follow_up / uplimit
      fvar <- stats::var(normalized)

      if (fvar < var_threshold) {log_stop("Follow-up variance did not pass the threshold")}
    }
  }

  return(cmatrix)
}

#' Schoenfeld test filtering for clinprog pipeline
#'
#' @description Internal helper function used by \code{run_regression()}.
#' Filters features that violate proportional hazards assumption using Schoenfeld residuals.
#'
#' @param full_data A data.frame with survival data and features.
#'
#' @return A filtered data.frame.
#' @keywords internal
clinprog_ph_assumptions <- function(full_data) {

  filt <- character()
  attributes <- colnames(full_data)[-c(1, 2)]

  for (i in attributes) {
    formula_str <- paste("survival::Surv(OS.time, OS) ~", i)

    # Modification to run Cox model faster (avoid redundancy)
    tryCatch({
      phmodel <- survival::coxph(stats::as.formula(formula_str), data = full_data)

      if (!is.na(phmodel$coef)) {
        schoen <- survival::cox.zph(phmodel)
        pval <- schoen$table[1, 3]
        if (!is.na(pval) && pval > 0.05) {filt <- c(filt, i)}
      }
    },
    warning = function(w) {log_message("Warning in feature ", i, ": ", conditionMessage(w))},
    error = function(e) {log_message("Skipping feature ", i, " due to error: ", conditionMessage(e))})
  }

  losers <- setdiff(attributes, filt)

  # Controls user output to avoid pollution
  if (length(losers) > 50) {
    log_message(length(losers), " columns removed by Schoenfeld test (showing first 50): ",
            paste(utils::head(losers, 50), collapse = ", "))
  } else {
    log_message(length(losers), " columns removed by Schoenfeld test: ", paste(losers, collapse = ", "))
  }

  if (length(filt) == 0) {log_stop("No features passed the Schoenfeld test")}

  cols <- match(filt, colnames(full_data))
  cols <- cols[!is.na(cols)]
  return(full_data[, c(1, 2, cols), drop = FALSE])
}

#' Feature count validation for clinprog pipeline
#' Substitutes the old numberfilter1() and numberfilter2()
#'
#' @description Internal helper function used by \code{run_regression()}.
#' Checks if sufficient features remain for bootstrap regression.
#' If not, performs a single regression and returns a signature.
#'
#' @param dataf A data.frame with survival data and features.
#' @param g Integer. Group size.
#' @param seed Integer or NULL. Random seed used for direct regression
#'   when the number of features is smaller than the group size.
#'
#' @return NULL if pipeline should continue, or a data.frame signature.
#' @keywords internal
clinprog_feature_check <- function(dataf, g, seed = NULL) {

  n_features <- ncol(dataf) - 2

  # Extreme case: no features
  if (n_features <= 0) {log_stop("No features remaining after filtering steps")}

  # Low features case: direct regression
  if (n_features < g) {
    log_message("Number of features is smaller than group size. Performing single multivariate regression...")
    coefs <- clinprog_regression(dataf, seed = seed)

    if (is.null(coefs) || length(coefs) == 0 || all(coefs == 0)) {
      log_stop("No signature found: all coefficients are zero")
    }

    signature <- data.frame(
      feature = names(coefs),
      coefficient = unname(coefs),
      stringsAsFactors = FALSE
    )

    signature$feature <- gsub("__", "-", signature$feature, fixed = TRUE)

    return(signature)
  }

  # Normal expected case: move forward in the pipeline
  return(NULL)
}

#' Bootstrap regression for clinprog pipeline
#'
#' @description Internal helper function used by \code{run_regression()}.
#' Performs bootstrap resampling, applies correlation filtering and regression,
#' and aggregates coefficients across iterations to derive a final signature.
#'
#' @param full_data A data.frame with survival data and features.
#' @param bootstrap Integer. Number of bootstrap iterations.
#' @param group_size Integer. Number of features sampled per iteration.
#' @param cor_threshold Numeric. Correlation filter threshold.
#' @param coef_threshold Numeric. Minimum absolute coefficient value to retain features.
#' @param n_cores Integer. Number of CPU cores used for parallel bootstrap
#'   regression (Default: \code{1}).
#' @param seed Integer. Random seed used for reproducible bootstrap regression
#'   (Default: \code{123}).
#'
#' @return A data.frame with the following signature columns:
#' \describe{
#'   \item{feature}{Feature name (gene/transcript)}
#'   \item{coefficient}{Mean coefficient across bootstrap iterations}
#'   \item{sd}{Standard deviation of coefficients}
#' }
#' @keywords internal
clinprog_bootstrapfun <- function(full_data,
                                  bootstrap,
                                  group_size,
                                  cor_threshold,
                                  coef_threshold,
                                  n_cores = 1,
                                  seed = 123) {

  log_message("Starting bootstrap with ", bootstrap, " iterations...")

  features <- colnames(full_data)[-c(1, 2)]
  results <- stats::setNames(vector("list", length(features)), features)

  # Avoids infinite loop
  attempts <- 0
  max_attempts <- bootstrap * 10

  # Collects coefficients returned by one or more bootstrap attempts
  collect_coefficients <- function(results_list) {

    for (coefs in results_list) {

      if (is.null(coefs)) {
        next
      }

      if (is.null(names(coefs))) {
        log_stop("Regression result must be a named vector")
      }

      for (j in seq_along(coefs)) {

        fname <- names(coefs)[j]

        if (!fname %in% names(results)) {
          results[[fname]] <- numeric(0)
        }

        results[[fname]] <<- c(results[[fname]], coefs[[j]])
      }
    }
  }

  # Runs one bootstrap attempt.
  #
  # IMPORTANT:
  # seed_val controls feature subsampling.
  # seed controls the regression itself. This intentionally preserves
  # the current master behavior where every regression receives the
  # same user-provided seed.
  run_attempt <- function(seed_val, retry = FALSE) {

    # Subsampling
    if (ncol(full_data) == 3) {

      cmatrix <- full_data

    } else {

      cmatrix <- clinprog_subsample(
        full_data = full_data,
        group_size = group_size,
        seed = seed_val
      )

      # Correlation filter
      if (group_size > 1 &&
          clinprog_corfun(cmatrix, cor_threshold) == 1) {
        return(NULL)
      }
    }

    # Regression
    if (retry) {

      # Serial path preserves the existing clinprog_regcall() retry behavior.
      return(
        clinprog_regcall(
          cmatrix = cmatrix,
          group_size = group_size,
          full_data = full_data,
          seed = seed
        )
      )

    } else {

      # Parallel path mirrors the master implementation:
      # direct regression with timeout and no retry.
      return(
        tryCatch(
          R.utils::withTimeout(
            clinprog_regression(cmatrix, seed = seed),
            timeout = 60,
            onTimeout = "warning"
          ),
          warning = function(w) NULL,
          error = function(e) NULL
        )
      )
    }
  }

  if (n_cores == 1) {

    # Serial path
    i <- 1

    while (i <= bootstrap) {

      attempts <- attempts + 1

      if (attempts > max_attempts) {
        log_stop("Too many failed bootstrap attempts (correlation filter too strict?)")
      }

      # Avoids spam messages
      if (i %% 10 == 0) {
        log_message("Processing iteration: ", i)
      }

      coefs <- run_attempt(
        seed_val = i,
        retry = TRUE
      )

      if (!is.null(coefs)) {
        collect_coefficients(list(coefs))
        i <- i + 1
      }
    }

  } else {

    # Parallel path
    cl <- parallel::makeCluster(n_cores)

    on.exit(
      parallel::stopCluster(cl),
      add = TRUE
    )

    # Export the objects and helper functions required by workers.
    parallel::clusterExport(
      cl,
      varlist = c(
        "full_data",
        "group_size",
        "cor_threshold",
        "seed",
        "run_attempt"
      ),
      envir = environment()
    )

    # Export ALL internal package functions (including dependencies like .timestamp)
    # directly from the package namespace environment
    parallel::clusterExport(
      cl = cl,
      varlist = names(asNamespace("clinprog")),
      envir = asNamespace("clinprog")
    )

    parallel::clusterEvalQ(
      cl,
      {
        library(survival)
        library(penalized)
        library(R.utils)
        NULL
      }
    )

    valid_results <- list()
    seed_counter <- 1

    while (length(valid_results) < bootstrap) {

      attempts_needed <- bootstrap - length(valid_results)

      # Preserve the master implementation's batching behavior:
      # each batch contains at least n_cores attempts.
      batch_size <- max(attempts_needed, n_cores)

      if (attempts + batch_size > max_attempts) {
        batch_size <- max_attempts - attempts
      }

      if (batch_size <= 0) {
        log_stop("Too many failed bootstrap attempts (correlation filter too strict?)")
      }

      batch_seeds <- seed_counter:(seed_counter + batch_size - 1)

      seed_counter <- seed_counter + batch_size
      attempts <- attempts + batch_size

      batch <- parallel::parLapply(
        cl,
        batch_seeds,
        function(seed_val) {
          run_attempt(seed_val, retry = FALSE)
        }
      )

      valid_batch <- Filter(Negate(is.null), batch)

      valid_results <- c(valid_results, valid_batch)

      if (length(valid_results) >= bootstrap) {
        break
      }

      log_message(
        "Bootstrap progress: ",
        min(length(valid_results), bootstrap),
        "/",
        bootstrap,
        " valid iterations"
      )
    }

    # Only the requested number of successful iterations contribute.
    collect_coefficients(
      valid_results[seq_len(bootstrap)]
    )
  }

  # Aggregates results
  summary_list <- lapply(names(results), function(feature) {

    vals <- results[[feature]]

    if (length(vals) == 0) {
      return(NULL)
    }

    data.frame(
      feature = feature,
      coefficient = mean(vals, na.rm = TRUE),
      sd = if (sum(!is.na(vals)) > 1) {
        stats::sd(vals, na.rm = TRUE)
      } else {
        NA_real_
      },
      stringsAsFactors = FALSE
    )
  })

  summary_list <- Filter(Negate(is.null), summary_list)

  tt <- do.call(rbind, summary_list)

  # Checks validity of results
  if (is.null(tt) || nrow(tt) == 0) {
    log_stop("No coefficients estimated during bootstrap")
  }

  if (anyNA(tt$coefficient)) {
    log_warning("NA coefficients detected. Consider increasing bootstrap iterations.")
  }

  # Applies coefficient threshold
  tt <- tt[
    abs(tt$coefficient) >= coef_threshold,
    ,
    drop = FALSE
  ]

  if (nrow(tt) == 0 || all(tt$coefficient == 0)) {
    log_stop("No signature found: coefficients below threshold")
  }

  tt$feature <- gsub("__", "-", tt$feature, fixed = TRUE)

  log_message("Bootstrap completed successfully.")

  return(tt)
}

###### SUPPORT (REGRESSION - MODULE I) ######

#' Correlation filtering for clinprog pipeline
#'
#' @description Internal helper function used by \code{run_regression()}.
#' Evaluates pairwise Spearman correlation between features and determines
#' whether the current bootstrap sample should be discarded based on a threshold.
#'
#' @param cmatrix A data.frame with survival data and features.
#' @param cor_threshold Numeric. Maximum allowed proportion of highly correlated feature pairs.
#'
#' @return Integer. Returns 1 if the sample should be discarded, 0 otherwise.
#' @keywords internal
clinprog_corfun <- function(cmatrix, cor_threshold) {

  n_features <- ncol(cmatrix) - 2

  # No correlation possible with < 2 features
  if (n_features < 2) return(0)

  # Optimized (and faster) way of creating vectors (pre-defining types and length)
  feature_idx <- 3:ncol(cmatrix)
  n_pairs <- choose(length(feature_idx), 2)
  correlations <- numeric(n_pairs)
  pvals <- numeric(n_pairs)
  pair_names <- character(n_pairs)
  k <- 1

  # Performs correlations
  for (i in seq_len(length(feature_idx) - 1)) {
    for (j in (i + 1):length(feature_idx)) {
      t <- feature_idx[i]
      u <- feature_idx[j]

      # Brings more security to correlation
      test <- tryCatch(
        stats::cor.test(cmatrix[, t], cmatrix[, u], method = "spearman", exact = FALSE),
        error = function(e) return(NULL)
      )
      if (is.null(test)) next

      # No rounding here to make more precise decisions
      correlations[k] <- as.numeric(test$estimate)
      pvals[k] <- test$p.value
      pair_names[k] <- paste(colnames(cmatrix)[t], colnames(cmatrix)[u], sep = "_")

      k <- k + 1
    }
  }

  # Avoids division by zero
  if (length(pvals) == 0) return(0)

  high_corr <- (correlations > 0.80) & (pvals < 0.05)
  proportion <- sum(high_corr) / length(pvals)

  if (proportion >= cor_threshold) {
    log_message("Iteration skipped due to high correlation among features.")
    if (any(high_corr)) {log_message("High correlation pairs: ", paste(pair_names[high_corr], collapse = ", "))}
    return(1)
  }

  return(0)
}

#' Bootstrap subsampling for clinprog pipeline
#'
#' @description Internal helper function used by \code{run_regression()}.
#' Randomly samples a subset of features for each bootstrap iteration,
#' preserving survival columns and enabling stochastic feature selection.
#'
#' @param full_data A data.frame with survival data and features
#' @param group_size Integer. Number of features to sample in the current iteration.
#' @param seed Integer or NULL. Optional random seed for reproducibility. If NULL, no seed is set.
#'
#' @return A data.frame containing the survival data and the sampled features.
#' @keywords internal
clinprog_subsample <- function(full_data, group_size, seed = NULL) {

  # Checks validity of input
  if (ncol(full_data) <= 2) {log_stop("No features available for subsampling")}
  if (!is.null(seed)) {set.seed(seed)}
  feature_cols <- colnames(full_data)[3:ncol(full_data)]
  if (group_size > length(feature_cols)) {log_stop("'group_size' is larger than available features")}

  # Makes bootstrap resampling with no replacement
  shuffle <- sample(feature_cols, size = group_size, replace = FALSE)
  log_message("Picked ", length(shuffle), " features: ", paste(shuffle, collapse = ", "))
  surv_cols <- colnames(full_data)[1:2]
  cmatrix <- full_data[, c(surv_cols, shuffle), drop = FALSE]
  return(cmatrix)
}

#' Regression call with timeout and retry logic
#'
#' @description Internal helper function used by \code{run_regression()}.
#' Executes regression on a bootstrap sample with a timeout limit.
#' If execution exceeds the time limit, a new subsample is drawn and the regression is retried once.
#'
#' @param cmatrix A data.frame with survival data and features.
#' @param group_size Integer. Number of features used in subsampling.
#' @param full_data Original full dataset used for resampling if needed.
#' @param seed Integer or NULL. Random seed used for reproducible regression.
#'
#' @return Named numeric vector of regression coefficients, or NULL if failure.
#' @keywords internal
clinprog_regcall <- function(cmatrix, group_size, full_data, seed = NULL) {

  # Double tryCatch to avoid calling the function itself
  result <- tryCatch(

    R.utils::withTimeout(
      clinprog_regression(cmatrix, seed = seed),
      timeout = 60,
      onTimeout = "error"
    ),

    error = function(e) {

      log_message("Regression timeout/error: retrying with new subsample...")

      new_matrix <- clinprog_subsample(
        full_data,
        group_size
      )

      # Retries one more time
      tryCatch(

        R.utils::withTimeout(
          clinprog_regression(new_matrix, seed = seed),
          timeout = 60,
          onTimeout = "error"
        ),

        error = function(e2) {
          log_message("Retry failed: ", conditionMessage(e2))
          return(NULL)
        }
      )
    }
  )

  return(result)
}

#' Penalized (LASSO) Cox regression core for clinprog pipeline
#'
#' @description Internal helper function used by \code{run_regression()}.
#' Fits a penalized Cox regression model using L1 (LASSO) penalty and returns coefficients.
#'
#' @param cmatrix A data.frame with survival data and features.
#' @param seed Integer or NULL. Random seed used for reproducible regression.
#'   If NULL, the current random-number-generator state is used.
#'
#' @return Named numeric vector of regression coefficients.
#' @keywords internal
clinprog_regression <- function(cmatrix, seed = NULL) {

  # Sets random seed when provided.
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Ensures data has OS and OS.time data
  if (!all(c("OS", "OS.time") %in% colnames(cmatrix))) {
    log_stop("'cmatrix' must contain 'OS' and 'OS.time' names")
  }

  # Ensures survival formula exists correctly
  form <- survival::Surv(OS.time, OS) ~ .

  # Model exploration (initial cross-validation using fold = 10)
  fit1 <- tryCatch(
    penalized::profL1(
      form,
      data = cmatrix,
      fold = 10,
      plot = FALSE,
      trace = FALSE
    ),
    error = function(e) {
      log_stop("'profL1' failed: ", conditionMessage(e))
    }
  )

  # Selection of the best lambda
  opt1 <- tryCatch(
    penalized::optL1(
      form,
      data = cmatrix,
      fold = fit1$fold,
      trace = FALSE
    ),
    error = function(e) {
      log_stop("'optL1' failed: ", conditionMessage(e))
    }
  )

  # Final penalized model
  fit <- tryCatch(
    penalized::penalized(
      form,
      data = cmatrix,
      lambda1 = opt1$lambda,
      trace = FALSE
    ),
    error = function(e) {
      log_stop("'penalized' model failed: ", conditionMessage(e))
    }
  )

  coefs <- penalized::coefficients(fit, "all")

  if (is.null(coefs) || length(coefs) == 0) {
    log_stop("No coefficients returned by penalized regression model")
  }

  return(coefs)
}

###### MAIN (SURVIVAL - MODULE II) ######

#' Determine optimal ROC-based cutoff for clinprog scores
#'
#' @description Internal helper function used by \code{run_survival()} to determine the optimal cutoff for continuous
#' clinprog scores using a time-dependent ROC curve and the Youden index criterion.
#'
#' The function automatically determines the ROC direction based on the AUC value estimated
#' with \code{survivalROC::survivalROC()} unless explicitly provided by the user.
#'
#' @param data A data.frame containing at least the columns \code{score}, \code{OS}, and \code{OS.time}.
#' @param direction Character. ROC direction passed to \code{OptimalCutpoints::optimal.cutpoints()}.
#' If \code{NULL} (default), direction is automatically inferred from the AUC value.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{cutoff}: numeric optimal cutoff value
#'   \item \code{auc}: numeric AUC value from the time-dependent ROC curve
#'   \item \code{optimal_cutpoint}: complete output from \code{OptimalCutpoints::optimal.cutpoints()}
#'   \item \code{direction}: ROC direction used
#' }
#'
#' @keywords internal
clinprog_cutoff_roc <- function(data, direction = NULL)
{
  # Checks input feature_type
  if (!is.data.frame(data)) {log_stop("Argument 'data' must be a data.frame")}

  # Validates input
  required_cols <- c("score", "OS", "OS.time")
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {log_stop("Missing required columns: ", paste(missing_cols, collapse = ", "))}

  # Checks direction
  if (!is.null(direction) && (!is.character(direction) || length(direction) != 1 || !direction %in% c("<", ">")))
  {
    log_stop("Argument 'direction' must be NULL, '<', or '>'")
  }

  # Determines ROC direction automatically if not provided
  if (is.null(direction))
  {
    # Calculates ROC object
    auc_obj <- survivalROC::survivalROC(Stime = data$OS.time, status = data$OS, marker = data$score,
                                        predict.time = stats::median(data$OS.time, na.rm = TRUE),
                                        method = "NNE", span = 0.25*nrow(data)^(-0.20))

    # Extracts preliminary AUC used only for direction inference
    auc_preliminary <- as.numeric(auc_obj$AUC)

    # Gets direction
    direction <- ifelse(auc_preliminary < 0.5, ">", "<")
  }

  # Calculates optimal cutoff
  optimal_cutpoint <- OptimalCutpoints::optimal.cutpoints(X = "score", status = "OS", tag.healthy = 0,
                                                          methods = "Youden", data = data,
                                                          control = OptimalCutpoints::control.cutpoints(),
                                                          direction = direction)

  # Extracts final ROC AUC from optimal cutpoint analysis
  auc <- as.numeric(optimal_cutpoint$Youden$Global$measures.acc$AUC[["AUC"]])

  # Extracts cutoff
  cutoff <- as.numeric(optimal_cutpoint$Youden$Global$optimal.cutoff$cutoff)

  # Handles rare multiple-cutoff outputs
  if (length(cutoff) >= 2) {cutoff <- cutoff[1]}

  # Returns structured result
  return(list(cutoff = cutoff, auc = auc, optimal_cutpoint = optimal_cutpoint, direction = direction))
}

#' Run univariate Cox regression model for clinprog scores
#'
#' @description Internal helper used by \code{run_survival()} to perform univariate survival analysis using
#' categorized clinprog scores.
#'
#' The function:
#' \itemize{
#'   \item Fits a univariate Cox proportional hazards model
#'   \item Tests proportional hazards assumptions
#'   \item Computes hazard ratios and confidence intervals
#'   \item Calculates median survival statistics
#'   \item Organizes all results into a structured list
#' }
#'
#' Input data must already contain:
#' \itemize{
#'   \item \code{OS}
#'   \item \code{OS.time}
#'   \item \code{score_group}
#' }
#'
#' @param data A data.frame containing survival information and categorized (binary) clinprog scores.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{table}: formatted univariate Cox regression results table
#'   \item \code{model}: fitted \code{survival::coxph} object
#'   \item \code{fit}: fitted \code{survival::survfit} object
#'   \item \code{ph}: proportional hazards assumptions test results
#' }
#'
#' Returns \code{NULL} if model fitting fails.
#'
#' @keywords internal
clinprog_uniCox_model <- function(data)
{
  # Checks input feature_type
  if (!is.data.frame(data)) {log_stop("Argument 'data' must be a data.frame")}

  # Validates input
  required_cols <- c("OS", "OS.time", "score_group")
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {log_stop("Missing required columns: ", paste(missing_cols, collapse = ", "))}

  # Runs analysis
  tryCatch({
    # Fits univariate Cox model
    uni_model <- survival::coxph(survival::Surv(OS.time, OS) ~ score_group, data = data)

    # Tests proportional hazards assumptions
    ph_test <- clinprog_schoenfeld(model_object = uni_model, covariates = "score_group", is_multi = FALSE)

    # Computes hazard ratio statistics
    hr_model <- clinprog_hazard_ratio(x = data$score_group, surv_time = data$OS.time, surv_event = data$OS,
                                      alpha = 0.05, method_test = "logrank", na.rm = TRUE)

    # Extracts statistics
    hazard_ratio <- paste0(signif(hr_model$hazard.ratio, 2), " (95% CI, ", signif(hr_model$lower, 2),
                           " - ", signif(hr_model$upper, 2), ")")
    coef <- hr_model$coef
    logrank_pvalue <- hr_model$p.value

    # Counts samples per group
    low_high_samples <- paste0(nrow(data[data$score_group == "low", ]), "/", nrow(data[data$score_group == "high", ]))

    # Fits Kaplan-Meier estimator
    fit <- survival::survfit(survival::Surv(OS.time, OS) ~ score_group, data = data)

    # Extracts median survival statistics
    fit_summary <- summary(fit)$table
    median_survival_low <- paste0(fit_summary[2, 7], " (95% CI, ", fit_summary[2, 8], " - ", fit_summary[2, 9], ")")
    median_survival_high <- paste0(fit_summary[1, 7], " (95% CI, ", fit_summary[1, 8], " - ", fit_summary[1, 9], ")")

    # Assigns prognosis label
    if (!is.na(logrank_pvalue) && logrank_pvalue < 0.05 && coef < 0)
    {
      prognosis <- "better"
    } else if (!is.na(logrank_pvalue) && logrank_pvalue < 0.05 && coef > 0) {
      prognosis <- "worse"
    } else {prognosis <- "---"}

    # Builds results table
    result_table <- data.frame(feature = "score", control = "high", condition = "low", coefficient = signif(coef, 2),
                               hazard.ratio = hazard_ratio, log.rank.pvalue = signif(logrank_pvalue, 2),
                               low.high.samples = low_high_samples, median.survival.low = median_survival_low,
                               median.survival.high = median_survival_high, prognosis = prognosis,
                               stringsAsFactors = FALSE)

    # Returns structured result
    return(list(table = result_table, model = uni_model, fit = fit, ph = ph_test))
  }, error = function(e) {
    log_warning("Univariate Cox model failed: ", conditionMessage(e))
    return(NULL)
  })
}

#' Search for the minimum signature size with significant univariate survival
#'
#' @description Internal helper used by \code{run_regression()} to automatically search for the smallest signature
#' (subset of the top-ranked features by absolute coefficient) that still yields a significant univariate log-rank test.
#'
#' @details The function ranks features by decreasing absolute coefficient then tests progressively larger cuts,
#' starting from \code{min_size} up to the full signature size.
#'
#' For each cut, the following steps are performed:
#' \enumerate{
#'   \item A clinprog score is computed for the provided survival dataset using only the features in the current cut.
#'   \item Patients are split into low/high groups using the strategy defined by \code{score_cutoff}.
#'   \item A univariate Cox model with log-rank test is fitted via \code{clinprog_uniCox_model()}.
#' }
#'
#' The search stops at the first cut whose log-rank p-value is below \code{0.05}. If no cut reaches significance,
#' the full signature is returned with a warning.
#'
#' @param signature A data.frame with columns \code{feature} and \code{coefficient}, as produced by the bootstrap step
#'   of \code{run_regression()}.
#' @param data A data.frame containing at least the columns \code{OS}, \code{OS.time}, and all features listed in
#'   \code{signature$feature}.
#' @param min_size Integer. Minimum signature size to test (must be >= 1 and <= \code{nrow(signature)}).
#' @param score_cutoff Character. Strategy used to split scores into low/high groups.
#'   One of \code{"median"}, \code{"mean"}, \code{"signal"}, or \code{"roc"}.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{signature}: the selected (possibly truncated) signature data.frame
#'   \item \code{size}: the effective size of the selected signature
#'   \item \code{pvalue}: the log-rank p-value for the selected signature
#'   \item \code{tested}: a data.frame with all tested sizes and their p-values
#' }
#'
#' @keywords internal
clinprog_min_signature <- function(signature, data, min_size, score_cutoff) {

  # Validates signature
  if (!is.data.frame(signature)) {log_stop("'signature' must be a data.frame")}
  if (!all(c("feature", "coefficient") %in% colnames(signature)))
  {
    log_stop("'signature' must contain 'feature' and 'coefficient' columns")
  }
  if (nrow(signature) == 0) {log_stop("'signature' must contain at least one feature")}

  # Normalizes feature names from the hyphenated format (public convention)
  # to the double-underscore format used internally by 'data' columns
  signature$feature <- gsub("-", "__", signature$feature, fixed = TRUE)

  # Validates survival data
  if (!is.data.frame(data)) {log_stop("'data' must be a data.frame")}
  if (!all(c("OS", "OS.time") %in% colnames(data))) {log_stop("'data' must contain 'OS' and 'OS.time' columns")}

  # Validates min_size
  if (!is.numeric(min_size) || length(min_size) != 1 || min_size < 1 || min_size %% 1 != 0)
  {
    log_stop("'min_size' must be a single positive integer")
  }

  # Validates score_cutoff
  if (!is.character(score_cutoff) || length(score_cutoff) != 1 ||
      !score_cutoff %in% c("median", "mean", "signal", "roc"))
  {
    log_stop("'score_cutoff' must be one of: 'median', 'mean', 'signal', 'roc'")
  }

  # Ranks features by decreasing absolute coefficient (same as new_clinprog_signature)
  signature <- signature[order(abs(signature$coefficient), decreasing = TRUE), , drop = FALSE]
  rownames(signature) <- NULL
  n_total <- nrow(signature)

  # Handles min_size larger than the full signature
  if (min_size > n_total)
  {
    log_warning("'min_size' (", min_size, ") exceeds the number of features in the ",
                "full signature (", n_total, "). Using the full signature.")
    return(list(signature = signature, size = n_total, pvalue = NA_real_,
                tested = data.frame(size = integer(0), pvalue = numeric(0))))
  }

  # Checks that all features are present in the data
  missing_features <- setdiff(signature$feature, colnames(data))
  if (length(missing_features) > 0)
  {
    log_stop("Signature features missing in the survival data: ", paste(missing_features, collapse = ", "))
  }

  # Header log
  log_message("Searching for the minimum signature with significant univariate survival (min_size = ",
              min_size, ", cutoff = ", score_cutoff, ", p_value_cutoff = 0.05)...")

  # Iterates over signature sizes
  tested <- data.frame(size = integer(0), pvalue = numeric(0))
  selected_size <- NULL
  selected_pvalue <- NA_real_

  for (k in min_size:n_total)
  {
    sub_sig <- signature[seq_len(k), , drop = FALSE]

    # Computes clinprog score for this cut
    expr <- as.matrix(data[, sub_sig$feature, drop = FALSE])
    match_idx <- match(colnames(expr), sub_sig$feature)
    coef_vector <- sub_sig$coefficient[match_idx]
    score <- as.numeric(expr %*% coef_vector)

    # Builds a temporary data.frame for the Cox model
    temp_df <- data.frame(OS = data$OS, OS.time = data$OS.time, score = score, stringsAsFactors = FALSE)

    # Categorizes into low/high according to the selected strategy
    if (score_cutoff == "median") {
      cut_val <- stats::median(temp_df$score, na.rm = TRUE)
    } else if (score_cutoff == "mean") {
      cut_val <- mean(temp_df$score, na.rm = TRUE)
    } else if (score_cutoff == "signal") {
      cut_val <- 0
    } else {  # "roc"
      # ROC-based cutoff, mirroring run_survival() behavior
      cut_res <- tryCatch(
        {
          roc_obj <- survivalROC::survivalROC(Stime = temp_df$OS.time, status = temp_df$OS, marker = temp_df$score,
                                              predict.time = stats::median(temp_df$OS.time, na.rm = TRUE),
                                              method = "NNE", span = 0.25 * nrow(temp_df)^(-0.20))
          clinprog_cutoff_roc(temp_df, direction = ifelse(as.numeric(roc_obj$AUC) < 0.5, ">", "<"))$cutoff
        },
        error = function(e) {
          log_warning("Size ", k, ": ROC-based cutoff failed (", conditionMessage(e), "). Falling back to median.")
          stats::median(temp_df$score, na.rm = TRUE)
        }
      )
      cut_val <- cut_res
    }

    temp_df$score_group <- ifelse(temp_df$score > cut_val, "high", "low")
    temp_df$score_group <- factor(temp_df$score_group, levels = c("high", "low"))

    # Skips if the cut cannot produce two groups
    if (length(unique(stats::na.omit(temp_df$score_group))) < 2)
    {
      log_warning("Size ", k, ": could not partition samples into low/high groups. Skipping.")
      tested <- rbind(tested, data.frame(size = k, pvalue = NA_real_))
      next
    }

    # Fits univariate Cox model
    pval_k <- tryCatch({
      res_uni <- clinprog_uniCox_model(temp_df)
      if (is.null(res_uni) || is.null(res_uni$table)) {
        NA_real_
      } else {as.numeric(res_uni$table$log.rank.pvalue)}
    }, error = function(e) {
      log_warning("Size ", k, ": Cox model failed (", conditionMessage(e), "). Skipping.")
      NA_real_
    })

    tested <- rbind(tested, data.frame(size = k, pvalue = pval_k))

    # Checks significance
    if (!is.na(pval_k) && pval_k < 0.05) {
      selected_size <- k
      selected_pvalue <- pval_k
      log_message("Size ", k, ": log-rank p = ", signif(pval_k, 4), "  <-- selected")
      break
    } else if (!is.na(pval_k)) {log_message("Size ", k, ": log-rank p = ", signif(pval_k, 4))}
  }

  # If nothing passed, falls back to the full signature
  if (is.null(selected_size))
  {
    log_warning("No signature size from ", min_size, " to ", n_total, " reached log-rank p < 0.05. ",
                "Returning the full signature (", n_total, " features) and proceeding with the analysis.")
    selected_size <- n_total
    selected_pvalue <- tested$pvalue[tested$size == n_total][1]
  }

  # Prepares the selected signature
  final_signature <- signature[seq_len(selected_size), , drop = FALSE]
  rownames(final_signature) <- NULL

  # Reverts feature names back to the hyphenated format (public convention)
  final_signature$feature <- gsub("__", "-", final_signature$feature, fixed = TRUE)

  # Final log with the selected features
  log_message("Selected minimum signature size: ", selected_size, " (features: ",
              paste(final_signature$feature, collapse = ", "), ")")

  list(signature = final_signature, size = selected_size, pvalue = selected_pvalue, tested = tested)
}

#' Run multiple univariate Cox regressions for clinical covariates
#'
#' @description Fits independent univariate Cox proportional hazards models for each provided clinical covariate.
#'
#' @param data Data.frame containing survival and binary clinical information.
#' Must contain columns \code{OS} and \code{OS.time}.
#' @param covariates Character. Vector containing clinical covariate names.
#' @param p_value_cutoff Numeric. P-value threshold used to define prognosis groups (default: \code{0.2}).
#'
#' @return A data.frame containing univariate Cox regression statistics for all tested covariates.
#'
#' @keywords internal
clinprog_uniCox_test <- function(data, covariates, p_value_cutoff = 0.2)
{
  # Validates input data
  required_cols <- c("OS", "OS.time")
  if (!all(required_cols %in% colnames(data))) {log_stop("Input data must contain columns: 'OS' and 'OS.time'")}

  # Validates covariates
  if (!is.character(covariates) || length(covariates) < 1) {log_stop("'covariates' must be a character vector")}
  missing_covariates <- covariates[!covariates %in% colnames(data)]
  if (length(missing_covariates) > 0) {log_stop(paste("Covariates not found in data:",
                                                  paste(missing_covariates, collapse = ", ")))}

  # Validates p-value cutoff
  if (!is.numeric(p_value_cutoff) || length(p_value_cutoff) != 1 || p_value_cutoff <= 0 || p_value_cutoff >= 1)
  {
    log_stop("'p_value_cutoff' must be a numeric value between 0 and 1")
  }

  # Runs multiple univariate Cox models for each variable
  results <- lapply(covariates, function(variable)
  {
    formula <- stats::as.formula(paste("survival::Surv(OS.time, OS) ~", variable))
    model <- tryCatch(survival::coxph(formula, data = data), error = function(e) NULL)

    # Skips failed models
    if (is.null(model)) {return(NULL)}

    # Extracts statistics
    coef_df <- as.data.frame(stats::coef(summary(model)))
    ci_df <- as.data.frame(summary(model)$conf.int)
    res <- cbind(
      variable = rownames(coef_df),
      coefficient = coef_df$coef,
      hazard.ratio.raw = ci_df$`exp(coef)`,
      Cox.pvalue = coef_df$`Pr(>|z|)`,
      lower.ci = ci_df$`lower .95`,
      upper.ci = ci_df$`upper .95`
    )

    # Organizes results as a data.frame
    res <- as.data.frame(res, stringsAsFactors = FALSE)

    # Formats statistics
    res$coefficient <- as.numeric(res$coefficient)
    res$hazard.ratio.raw <- as.numeric(res$hazard.ratio.raw)
    res$Cox.pvalue <- as.numeric(res$Cox.pvalue)
    res$lower.ci <- as.numeric(res$lower.ci)
    res$upper.ci <- as.numeric(res$upper.ci)

    # Creates formatted HR column
    res$hazard.ratio <- paste0(round(res$hazard.ratio.raw, 4), " (95% CI, ",
                               round(res$lower.ci, 4), " - ", round(res$upper.ci, 4), ")")

    # Formats p-values
    res$Cox.pvalue <- round(res$Cox.pvalue, 4)

    # Defines prognosis
    res$prognosis <- "---"
    res$prognosis[!is.na(res$Cox.pvalue) & res$Cox.pvalue < p_value_cutoff & res$coefficient < 0] <- "better"
    res$prognosis[!is.na(res$Cox.pvalue) & res$Cox.pvalue < p_value_cutoff & res$coefficient > 0] <- "worse"

    # Returns final columns
    res[, c("variable", "hazard.ratio", "Cox.pvalue", "prognosis")]
  })

  # Combines all results into a list
  results <- Filter(Negate(is.null), results)

  # Checks if any final result is valid
  if (length(results) == 0)
  {
    log_warning("No valid univariate Cox models could be fitted")
    return(NULL)
  }

  # Transforms the list into a new consolidated data.frame
  univ_res <- do.call(rbind, results)
  rownames(univ_res) <- NULL

  return(univ_res)
}

#' Run multivariate Cox regression with optional bootstrap resampling
#'
#' @description Performs multivariate Cox regression using selected clinical covariates and clinprog score groups.
#' Optionally applies bootstrap resampling to identify stable covariates prior to fitting the final model.
#'
#' @param data Data.frame containing survival, clinprog score and clinical information.
#' Must contain columns \code{OS}, \code{OS.time}, and \code{score_group}.
#' @param univ_result Data.frame generated by \code{clinprog_uniCox_test()}.
#' @param logrank_result Data.frame generated by \code{clinprog_uniCox_model()}.
#' @param covariates Character. Vector containing covariates selected for multivariate analysis.
#' @param all_covariates Character. Vector containing all available clinical covariates.
#' @param use_bootstrap Logical. Whether bootstrap resampling should be used before final model fitting (default: \code{TRUE}).
#' @param bootstrap Numeric. Number of bootstrap iterations (default: \code{1}).
#' @param bootstrap_freq Numeric. Minimum frequency to retain variables after bootstrap resampling (default: \code{0.25}).
#' @param multi_p_value_cutoff Numeric. P-value used to define prognosis groups in multivariate models (default: \code{0.1}).
#'
#' @return A list containing:
#' \describe{
#'   \item{table}{Formatted multivariate Cox regression results.}
#'   \item{model}{Final fitted Cox proportional hazards model.}
#'   \item{covariates}{Covariates retained in the final model.}
#'   \item{bootstrap.table}{Bootstrap frequency table (if applicable).}
#'   \item{bootstrap.used}{Logical indicating whether bootstrap was used.}
#' }
#'
#' @keywords internal
clinprog_multiCox_test <- function(data, univ_result, logrank_result, covariates, all_covariates,
                                   use_bootstrap = TRUE, bootstrap = 1, bootstrap_freq = 0.25, multi_p_value_cutoff = 0.1)
{
  # Validates input data
  required_cols <- c("OS", "OS.time", "score_group")
  if (!is.data.frame(data)) {log_stop("'data' must be a data.frame")}
  if (!all(required_cols %in% colnames(data))) {log_stop("Input data must contain columns: 'OS', 'OS.time', and 'score_group'")}

  # Validates univariate results for clinical variables
  if (!is.data.frame(univ_result)) {log_stop("'univ_result' must be a data.frame")}
  required_univ_cols <- c("variable", "hazard.ratio", "Cox.pvalue", "prognosis")
  if (!all(required_univ_cols %in% colnames(univ_result))) {log_stop("'univ_result' is missing required columns")}

  # Validates log-rank results for score
  if (!is.data.frame(logrank_result)) {log_stop("'logrank_result' must be a data.frame")}
  required_logrank_cols <- c("feature", "coefficient", "hazard.ratio", "log.rank.pvalue", "prognosis")
  if (!all(required_logrank_cols %in% colnames(logrank_result))) {log_stop("'logrank_result' is missing required columns")}

  # Validates covariates
  if (!is.character(covariates) || length(covariates) < 1) {log_stop("'covariates' must be a character vector")}
  if (!is.character(all_covariates) || length(all_covariates) < 1) {log_stop("'all_covariates' must be a character vector")}

  # Validates bootstrap parameters
  if (!is.logical(use_bootstrap) || length(use_bootstrap) != 1) {log_stop("'use_bootstrap' must be either TRUE or FALSE")}
  if (!is.numeric(bootstrap) || length(bootstrap) != 1 || bootstrap < 1) {log_stop("'bootstrap' must be a positive numeric value")}
  if (!is.numeric(bootstrap_freq) || length(bootstrap_freq) != 1 || bootstrap_freq <= 0 || bootstrap_freq > 1)
  {
    log_stop("'bootstrap_freq' must be a numeric value between 0 and 1")
  }
  if (!is.numeric(multi_p_value_cutoff) || length(multi_p_value_cutoff) != 1 || multi_p_value_cutoff <= 0 || multi_p_value_cutoff >= 1)
  {
    log_stop("'multi_p_value_cutoff' must be a numeric value between 0 and 1")
  }

  # Harmonizes score naming between univariate and multivariate analyses
  logrank_result$feature[logrank_result$feature == "score"] <- "score_group"

  # Ensures "score_group" is included
  covariates <- unique(c("score_group", covariates))

  # Internal helper to extract Cox statistics
  extract_cox_results <- function(model)
  {
    coef_df <- as.data.frame(stats::coef(summary(model)))
    ci_df <- as.data.frame(summary(model)$conf.int)

    res <- cbind(variable = rownames(coef_df), coefficient = coef_df$coef, hazard.ratio.raw = ci_df$`exp(coef)`,
                 Cox.pvalue = coef_df$`Pr(>|z|)`, lower.ci = ci_df$`lower .95`, upper.ci = ci_df$`upper .95`)
    res <- as.data.frame(res, stringsAsFactors = FALSE)

    # Converts numeric columns
    numeric_cols <- c("coefficient", "hazard.ratio.raw", "Cox.pvalue", "lower.ci", "upper.ci")
    res[numeric_cols] <- lapply(res[numeric_cols], as.numeric)

    # Creates formatted hazard ratio column
    res$hazard.ratio <- paste0(round(res$hazard.ratio.raw, 4), " (95% CI, ",
                               round(res$lower.ci, 4), " - ", round(res$upper.ci, 4), ")")

    # Formats p-values
    res$Cox.pvalue <- round(res$Cox.pvalue, 4)

    # Defines prognosis
    res$prognosis <- NA
    res$prognosis[!is.na(res$Cox.pvalue) & res$Cox.pvalue < multi_p_value_cutoff & res$coefficient < 0] <- "better"
    res$prognosis[!is.na(res$Cox.pvalue) & res$Cox.pvalue < multi_p_value_cutoff & res$coefficient > 0] <- "worse"
    res[, c("variable", "hazard.ratio", "Cox.pvalue", "prognosis")]
  }

  # Builds multivariate formula
  multi_formula <- stats::as.formula(paste("survival::Surv(OS.time, OS) ~", paste(covariates, collapse = " + ")))

  # Initializes basic bootstrap objects
  bootstrap_table <- NULL
  bootstrap_used <- FALSE

  # Optional bootstrap resampling
  if (use_bootstrap)
  {
    # Runs bootstrap resampling
    log_message(paste0("Running bootstrap resampling with ", bootstrap, " iterations..."))
    resampling_results <- clinprog_resampling(data = data, covariates = covariates, all_covariates = all_covariates,
                                              bootstrap = bootstrap, bootstrap_freq = bootstrap_freq,
                                              boot_p_value_cutoff = multi_p_value_cutoff)

    # Extracts main results from iterations
    covariates <- resampling_results$covariates
    bootstrap_table <- resampling_results$table
    bootstrap_used <- resampling_results$bootstrap.used

    # Rebuilds formula after variable selection
    multi_formula <- stats::as.formula(paste("survival::Surv(OS.time, OS) ~", paste(covariates, collapse = " + ")))
  }

  # Fits final multivariate Cox model
  model <- survival::coxph(formula = multi_formula, data = data)

  # Extracts multivariate statistics
  multi_res <- extract_cox_results(model)

  # Merges univariate (score) and multivariate (clinical_data) results
  final <- merge(univ_result, multi_res, by = "variable", all = TRUE)

  # Replaces score_group univariate values
  score_idx <- grepl("score_group", final$variable, fixed = TRUE)
  final[score_idx, "hazard.ratio.x"] <- as.character(logrank_result$hazard.ratio)
  final[score_idx, "Cox.pvalue.x"] <- logrank_result$log.rank.pvalue
  final[score_idx, "prognosis.x"] <- as.character(logrank_result$prognosis)

  # Replaces missing values
  final[is.na(final)] <- "---"

  # Renames columns
  colnames(final) <- c("variable", "univariate.hazard.ratio", "univariate.Cox.pvalue", "univariate.prognosis",
                       "multivariate.hazard.ratio", "multivariate.Cox.pvalue", "multivariate.prognosis")

  # Extracts reference conditions
  tmp_variables <- c()
  tmp_conditions <- c()

  reference_covariates <- c("score_group", all_covariates)

  for (var in final$variable)
  {
    for (ref_var in reference_covariates)
    {
      if (grepl(ref_var, var))
      {
        tmp_variables <- append(tmp_variables, ref_var)
        ref_group <- substr(x = var, start = nchar(ref_var) + 1, stop = nchar(var))
        tmp_conditions <- append(tmp_conditions, ref_group)
      }
    }
  }

  final$variable <- tmp_variables
  final$condition <- tmp_conditions

  final <- final[, c("variable", "condition", "univariate.hazard.ratio", "univariate.Cox.pvalue", "univariate.prognosis",
                     "multivariate.hazard.ratio", "multivariate.Cox.pvalue", "multivariate.prognosis")]

  rownames(final) <- NULL

  return(list(table = final, model = model, covariates = covariates,
              bootstrap.table = bootstrap_table, bootstrap.used = bootstrap_used))
}

###### SUPPORT (SURVIVAL - MODULE II) ######

#' Test proportional hazards assumptions for Cox models
#'
#' @description Internal helper used by \code{run_survival()} and \code{clinprog_uniCox_model()} to evaluate
#' proportional hazards assumptions using Schoenfeld residuals from \code{survival::cox.zph()}.
#'
#' The function identifies which covariates satisfy the proportional hazards assumptions and
#' returns the approved variables for downstream analyses, as well as the global test result.
#'
#' @param model_object A fitted Cox proportional hazards model generated by \code{survival::coxph()}.
#' @param covariates Character vector containing covariate names included in the model.
#' @param is_multi Logical. Whether the model corresponds to a multivariate analysis (default: FALSE).
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{variables}: covariates passing PH assumptions
#'   \item \code{dropped}: covariates failing PH assumptions
#'   \item \code{global_p}: global proportional hazards p-value
#'   \item \code{table}: full \code{cox.zph()} results table
#'   \item \code{object}: original \code{cox.zph()} object
#' }
#'
#' @keywords internal
clinprog_schoenfeld <- function(model_object, covariates, is_multi = FALSE)
{
  # Validates model
  if (is.null(model_object))
  {
    if (is_multi)
    {
      log_warning("Multivariate Cox model could not be fitted due to convergence issues.")
    } else {log_warning("Univariate Cox model could not be fitted due to convergence issues.")}
    return(NULL)
  }

  # Validates model object
  if (!inherits(model_object, "coxph")) {log_stop("'model_object' must be a valid survival::coxph model")}

  # Validates covariates
  if (!is.character(covariates) || length(covariates) < 1) {log_stop("'covariates' must be a non-empty character vector")}

  # Validates multivariate flag
  if (!is.logical(is_multi) || length(is_multi) != 1) {log_stop("'is_multi' must be either TRUE or FALSE")}

  # Runs Schoenfeld residuals test
  test_ph <- survival::cox.zph(fit = model_object, global = TRUE)

  # Adjusts covariate names for multivariate models
  if (is_multi)
  {
    tmp_covariates <- character()
    covariates <- unique(c("score_group", covariates))
    new_covariates <- colnames(test_ph$y)

    for (var in new_covariates)
    {
      matched <- covariates[vapply(covariates, function(x) {grepl(paste0("^", x), var)}, FUN.VALUE = logical(1))]
      if (length(matched) > 0) {tmp_covariates <- c(tmp_covariates, matched[[1]])}
    }
  } else {tmp_covariates <- covariates}

  # Validates recovered covariate names
  expected_n <- nrow(test_ph$table) - 1
  if (length(tmp_covariates) != expected_n) {log_stop("Mismatch between recovered covariate names and Schoenfeld test output")}

  # Updates labels
  rownames(test_ph$table) <- c(tmp_covariates, "GLOBAL")
  colnames(test_ph$y) <- tmp_covariates

  # Extracts PH statistics
  tmp_df <- as.data.frame(test_ph$table)
  global_p <- tmp_df[nrow(tmp_df), "p"]
  pvalues <- tmp_df[1:(nrow(tmp_df) - 1), "p", drop = FALSE]
  variables <- rownames(pvalues[pvalues$p > 0.05, , drop = FALSE])
  dropped <- rownames(pvalues[pvalues$p <= 0.05, , drop = FALSE])

  # Evaluates global PH assumptions
  if (global_p <= 0.05)
  {
    log_warning(paste0("Global proportional hazards assumptions not met (Global p = ", round(global_p, 4), ")."))
  } else {log_message(paste0("Global proportional hazards assumptions met (Global p = ", round(global_p, 4), ")."))}

  # Reports dropped covariates
  if (length(dropped) > 0)
  {
    log_warning(paste0("The following covariates failed proportional hazards assumptions and will be removed: ",
                   paste(dropped, collapse = ", ")))
  }

  # Returns structured output
  return(list(variables = variables, dropped = dropped, global_p = global_p, table = tmp_df, object = test_ph))
}

#' Extract hazard ratio statistics from a univariate Cox model
#'
#' @description Internal helper used by \code{clinprog_uniCox_model()} to fit a univariate Cox proportional
#' hazards model and extract hazard ratio statistics, confidence intervals, coefficients, and p-values.
#'
#' @param x Predictor variable.
#' @param surv_time Numeric vector containing follow-up times.
#' @param surv_event Numeric or logical vector containing survival events.
#' @param alpha Numeric. Significance level used for confidence intervals (default: \code{0.05}).
#' @param method_test Character. Statistical test used to compute p-values.
#' One of \code{"logrank"}, \code{"likelihood.ratio"}, or \code{"wald"}.
#' @param na.rm Logical. Whether to remove incomplete observations (default: \code{FALSE}).
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{hazard.ratio}: Hazard ratio
#'   \item \code{coef}: Cox regression coefficient
#'   \item \code{se}: Standard error
#'   \item \code{lower}: Lower confidence interval
#'   \item \code{upper}: Upper confidence interval
#'   \item \code{p.value}: Model p-value
#'   \item \code{n}: Number of observations
#'   \item \code{model}: Fitted Cox model
#' }
#'
#' Returns \code{NULL} if the Cox model cannot be fitted.
#'
#' @keywords internal
clinprog_hazard_ratio <- function(x, surv_time, surv_event, alpha = 0.05,
                                  method_test = c("logrank", "likelihood.ratio", "wald"), na.rm = FALSE)
{
  # Validates x
  if (length(x) != length(surv_time) || length(x) != length(surv_event))
  {
    log_stop("Arguments 'x', 'surv_time', and 'surv_event' must have the same length")
  }

  # Validates survival times
  if (!is.numeric(surv_time) || any(surv_time < 0, na.rm = TRUE))
  {
    log_stop("'surv_time' must be a numeric vector containing non-negative values")
  }

  # Validates survival event
  if (!is.numeric(surv_event)) {log_stop("Argument 'surv_event' must be numeric")}
  unique_events <- sort(unique(stats::na.omit(surv_event)))
  if (!setequal(unique_events, c(0, 1))) {log_stop("Argument 'surv_event' must contain only 0 and 1 values")}

  # Validates alpha
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1)
  {
    log_stop("Argument 'alpha' must be a numeric value between 0 and 1")
  }

  # Validates na.rm
  if (!is.logical(na.rm) || length(na.rm) != 1) {log_stop("Argument 'na.rm' must be TRUE or FALSE")}

  # Matches statistical test argument
  method_test <- match.arg(method_test)

  # Builds temporary dataset
  df <- data.frame(x = x, surv_time = surv_time, surv_event = surv_event)

  # Removes missing values if requested
  if (na.rm) {df <- stats::na.omit(df)}

  # Validates remaining observations
  if (nrow(df) < 3)
  {
    log_warning("Insufficient observations to fit Cox model")
    return(NULL)
  }

  # Fits Cox model safely
  model <- tryCatch(survival::coxph(survival::Surv(surv_time, surv_event) ~ x, data = df), error = function(e) NULL)

  # Stops if model fitting failed
  if (is.null(model)) {return(NULL)}

  # Extracts coefficients
  coef <- stats::coef(model)[1]
  se <- sqrt(diag(stats::vcov(model)))[1]

  # Selects statistical test
  stat <- switch(method_test, logrank = model$score, likelihood.ratio = 2 * diff(model$loglik), wald = model$wald.test)

  # Calculates p-value
  p.value <- stats::pchisq(stat, df = 1, lower.tail = FALSE)

  # Confidence interval multiplier
  z <- stats::qnorm(alpha / 2, lower.tail = FALSE)

  # Returns structured results
  return(list(hazard.ratio = exp(coef), coef = coef, se = se, lower = exp(coef - z * se),
              upper = exp(coef + z * se), p.value = p.value, n = model$n, model = model))
}

#' Perform bootstrap resampling for multivariate Cox regression
#'
#' @description Runs bootstrap-based variable stability analysis for multivariate Cox proportional hazards regression.
#' Used in \code{clinprog_multiCox_test()}
#'
#' @param data Data.frame containing survival and clinical variables.
#' Must contain columns \code{OS}, \code{OS.time}, and \code{score_group}.
#' @param covariates Character. Vector containing selected covariates.
#' @param all_covariates Character. Vector containing all available clinical covariates.
#' @param bootstrap Numeric. Number of bootstrap iterations (default: \code{100}).
#' @param bootstrap_freq Numeric. Minimum frequency to retain variables after bootstrap resampling (default: \code{0.25}).
#' @param boot_p_value_cutoff Numeric. P-value threshold used to define prognosis groups (default: \code{0.05}).
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{table}: Bootstrap frequency table.
#'   \item \code{covariates}: Selected covariates after bootstrap filtering.
#'   \item \code{bootstrap.used}: Logical indicating whether bootstrap was successfully used.
#' }
#'
#' @keywords internal
clinprog_resampling <- function(data, covariates, all_covariates,
                                bootstrap = 100, bootstrap_freq = 0.25, boot_p_value_cutoff = 0.1)
{
  # Internal helper to summarize why bootstrap iterations were dropped
  .summarize_rejections <- function(rejection_reasons, min_prop)
  {
    if (length(rejection_reasons) == 0) return(invisible(NULL))

    # Makes sure every reason gets exactly 2 fields separated by tab
    rejection_reasons <- vapply(rejection_reasons, function(x) {
      if (grepl("\t", x, fixed = TRUE)) x else paste0(x, "\tNA")
    }, character(1))

    parts <- do.call(rbind, strsplit(rejection_reasons, "\t", fixed = TRUE))
    df <- data.frame(col = parts[, 1], val = parts[, 2], stringsAsFactors = FALSE)

    log_warning("Some covariates caused bootstrap iterations to be dropped (review 'clinical_data' input):")
    for (col in unique(df$col))
    {
      sub <- df[df$col == col, , drop = FALSE]
      n <- nrow(sub)

      if (all(sub$val == "non-binary")) {
        log_warning("    '", col, "' non-binary (", n, " iterations)")
      } else {
        vals <- suppressWarnings(as.numeric(sub$val))
        min_val <- min(vals, na.rm = TRUE)
        log_warning("    '", col, "' min_prop = ", min_val, " (< cutoff: ", min_prop, ")  [", n, " iterations]")
      }
    }
  }

  # Validates input data
  required_cols <- c("OS", "OS.time", "score_group")
  if (!all(required_cols %in% colnames(data))) {log_stop("Input data must contain columns: 'OS' and 'OS.time'")}

  # Validates covariates
  if (!is.character(covariates) || length(covariates) < 1) {log_stop("'covariates' must be a character vector")}
  if (!is.character(all_covariates) || length(all_covariates) < 1) {log_stop("'all_covariates' must be a character vector")}

  # Validates bootstrap iterations
  if (!is.numeric(bootstrap) || length(bootstrap) != 1 || bootstrap < 1) {log_stop("'bootstrap' must be a positive numeric value")}

  # Validates bootstrap frequency threshold
  if (!is.numeric(bootstrap_freq) || length(bootstrap_freq) != 1 || bootstrap_freq <= 0 || bootstrap_freq > 1)
  {
    log_stop("'bootstrap_freq' must be a numeric value between 0 and 1")
  }

  # Validates p-value cutoff
  if (!is.numeric(boot_p_value_cutoff) || length(boot_p_value_cutoff) != 1 || boot_p_value_cutoff <= 0 || boot_p_value_cutoff >= 1)
  {
    log_stop("'boot_p_value_cutoff' must be a numeric value between 0 and 1")
  }

  # Builds Cox formula
  multi_formula <- stats::as.formula(paste("survival::Surv(OS.time, OS) ~", paste(covariates, collapse = " + ")))

  # Internal helper to extract Cox results
  extract_boot_cox_results <- function(model, boot_p_value_cutoff)
  {
    coef_df <- as.data.frame(stats::coef(summary(model)))
    ci_df <- as.data.frame(summary(model)$conf.int)

    res <- cbind(variable = rownames(coef_df), coefficient = coef_df$coef, hazard.ratio.raw = ci_df$`exp(coef)`,
                 Cox.pvalue = coef_df$`Pr(>|z|)`, lower.ci = ci_df$`lower .95`, upper.ci = ci_df$`upper .95`)
    res <- as.data.frame(res, stringsAsFactors = FALSE)

    res$coefficient <- as.numeric(res$coefficient)
    res$Cox.pvalue <- as.numeric(res$Cox.pvalue)
    res$prognosis <- NA
    res$prognosis[!is.na(res$Cox.pvalue) & res$Cox.pvalue < boot_p_value_cutoff & res$coefficient < 0] <- "better"
    res$prognosis[!is.na(res$Cox.pvalue) & res$Cox.pvalue < boot_p_value_cutoff & res$coefficient > 0] <- "worse"

    return(res[, c("variable", "prognosis")])
  }

  # Generates bootstrap samples
  bootstrap_data <- sjstats::bootstrap(data = data, n = bootstrap, size = 0.7)

  tables_list <- list()
  valid_iterations <- 0
  current_iteration <- 1

  rejection_reasons <- character()
  while (valid_iterations < bootstrap && current_iteration <= length(bootstrap_data$strap))
  {
    boot_df <- clinprog_bootstrap(raw_data = data, bootstrap_data = bootstrap_data, iteration = current_iteration, min_prop = 0.1)
    current_iteration <- current_iteration + 1

    # Skips invalid bootstrap samples
    if (length(boot_df) == 0)
    {
      reason <- attr(boot_df, "reason")
      if (!is.null(reason)) rejection_reasons <- c(rejection_reasons, reason)
      next
    }

    # Fits multivariate Cox model
    model <- tryCatch(survival::coxph(multi_formula, data = boot_df), error = function(e) NULL)
    if (is.null(model)) {next}

    # Extracts iteration results
    tables_list[[length(tables_list) + 1]] <- extract_boot_cox_results(model = model, boot_p_value_cutoff = boot_p_value_cutoff)
    valid_iterations <- valid_iterations + 1
  }

  # Ensures valid bootstrap results exist
  if (length(tables_list) == 0)
  {
    log_warning("Bootstrap resampling failed.")
    .summarize_rejections(rejection_reasons, min_prop = 0.1)
    log_warning("Returning original covariates.")
    return(list(table = NULL, covariates = covariates, bootstrap.used = FALSE))
  }

  # Merges bootstrap results
  merged_table <- clinprog_merge(tables_list = tables_list, covariates = all_covariates)

  # Selects frequent covariates
  frequency_cutoff <- bootstrap * bootstrap_freq
  selected_covariates <- merged_table$covariate[merged_table$frequency >= frequency_cutoff]

  # Ensures score_group remains included
  selected_covariates <- unique(c("score_group", selected_covariates))

  # Prints 1x at the end the variables that made iterations fail
  .summarize_rejections(rejection_reasons, min_prop = 0.1)

  return(list(table = merged_table, covariates = selected_covariates, bootstrap.used = TRUE))
}

#' Validate bootstrap resampled datasets for multivariate Cox regression
#'
#' @description Validates whether covariates in a bootstrapped dataset remain binary and balanced for multivariate Cox regression.
#'
#' @param raw_data Data.frame used as source for bootstrap resampling.
#' @param bootstrap_data Bootstrap object generated by \code{sjstats::bootstrap()}.
#' @param iteration Numeric. Bootstrap iteration index.
#' @param min_prop Numeric. Minimum proportion for the least abundant category in binary variables (default: \code{0.1}).
#'
#' @return A bootstrap-resampled data.frame if valid, otherwise \code{NULL}.
#'
#' @keywords internal
clinprog_bootstrap <- function(raw_data, bootstrap_data, iteration, min_prop = 0.1)
{
  # Validates input data
  if (!is.data.frame(raw_data)) {log_stop("'raw_data' must be a data.frame")}
  required_cols <- c("OS", "OS.time")
  if (!all(required_cols %in% colnames(raw_data))) {log_stop("'raw_data' must contain columns: 'OS' and 'OS.time'")}

  # Validates bootstrap data
  if (!is.list(bootstrap_data) || is.null(bootstrap_data$strap))
  {
    log_stop("'bootstrap_data' must be a valid bootstrap object generated by sjstats::bootstrap()")
  }

  # Validates iteration
  if (!is.numeric(iteration) || length(iteration) != 1 || iteration < 1) {log_stop("'iteration' must be a positive numeric value")}
  if (iteration > length(bootstrap_data$strap)) {log_stop("'iteration' exceeds the number of available bootstrap samples")}

  # Validates minimum proportion
  if (!is.numeric(min_prop) || length(min_prop) != 1 || min_prop <= 0 || min_prop >= 0.5)
  {
    log_stop("'min_prop' must be a numeric value between 0 and 0.5")
  }

  # Extracts bootstrap sample indices
  boot_indices <- bootstrap_data$strap[[iteration]]$id

  # Builds bootstrap dataset
  bootstrap_df <- as.data.frame(raw_data[boot_indices, , drop = FALSE])

  # Checks binary balance for covariates
  covariate_cols <- setdiff(colnames(bootstrap_df), c("OS", "OS.time", "score", "score_group"))
  for (col in covariate_cols)
  {
    current_var <- stats::na.omit(bootstrap_df[[col]])

    # Ensures binary variables
    if (length(current_var) == 0 || length(unique(current_var)) != 2)
    {
      return(structure(list(), reason = paste0("'", col, "' non-binary")))
    }

    # Computes category proportions
    proportions <- prop.table(table(current_var))

    # Ensures minimum category proportion
    if (min(proportions) < min_prop)
    {
      return(structure(list(), reason = paste0(col, "\t", round(min(proportions), 4))))
    }
  }

  return(bootstrap_df)
}

#' Merge bootstrap multivariate Cox regression results
#'
#' @description Summarizes bootstrap multivariate Cox regression results
#' by counting how frequently each covariate was selected across bootstrap iterations.
#'
#' @param tables_list List containing multivariate Cox regression result tables.
#' @param covariates Character vector containing original clinical covariates.
#'
#' @return A data.frame containing covariate selection frequencies across bootstrap iterations.
#'
#' @keywords internal
clinprog_merge <- function(tables_list, covariates)
{
  # Validates tables list
  if (!is.list(tables_list) || length(tables_list) == 0) {log_stop("'tables_list' must be a non-empty list")}

  # Validates covariates
  if (!is.character(covariates) || length(covariates) < 1) {log_stop("'covariates' must be a character vector")}

  # Extracts significant variables from each iteration
  cox_tables <- lapply(tables_list, function(result_table)
  {
    if (!"prognosis" %in% colnames(result_table)) {log_stop("All tables in 'tables_list' must contain a 'prognosis' column")}
    result_table <- result_table[!is.na(result_table$prognosis), , drop = FALSE]
    if (nrow(result_table) == 0) {return(NULL)}
    dplyr::count(result_table, variable)
  })

  # Removes empty results
  cox_tables <- Filter(Negate(is.null), cox_tables)

  if (length(cox_tables) == 0)
  {
    log_warning("No valid bootstrap iterations available")
    return(NULL)
  }

  # Combines all bootstrap results
  tmp_table <- data.table::rbindlist(cox_tables, use.names = TRUE, fill = TRUE, idcol = "bootstrap.id")

  # Counts covariate frequencies
  final_table <- dplyr::count(tmp_table, variable)
  colnames(final_table) <- c("covariate", "frequency")

  # Matches covariate names to original variables
  original_covariates <- c("score_group", covariates)

  matched_covariates <- vapply(final_table$covariate, function(variable)
  {
    matched <- original_covariates[vapply(original_covariates,
                                          function(x) grepl(paste0("^", x), variable),
                                          FUN.VALUE = logical(1))]
    if (length(matched) > 0) matched[[1]] else variable
  }, FUN.VALUE = character(1))

  final_table$covariate <- matched_covariates

  return(final_table)
}
