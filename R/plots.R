utils::globalVariables(c("feature", "coefficient", "covariate", "frequency"))

#' Default ggplot2 theme for clinprog visualizations
#'
#' @description Internal helper function that provides a consistent ggplot2 theme used across clinprog plots
#' Can also be applied to custom plots for visual consistency.
#'
#' @return A \code{ggplot} theme object.
#' @export
theme_clinprog <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      text = ggplot2::element_text(face = "plain", colour = "black"),
      axis.text = ggplot2::element_text(face = "plain", colour = "black"),
      legend.text = ggplot2::element_text(face = "plain", colour = "black"),
      legend.title = ggplot2::element_text(face = "plain", colour = "black"),
      axis.ticks = ggplot2::element_line(colour = "black"),
      axis.line = ggplot2::element_line(colour = "black")
    )
}

###### REGRESSION (MODULE I) ######

#' Lollipop plot for clinprog signature
#'
#' @description Generates a lollipop plot of top features ranked by coefficient.
#'
#' @param signature A data.frame with 'feature' and 'coefficient'.
#' @param outprefix Character. Output prefix for saving the plot (optional).
#' @param theme A ggplot2 theme object. Defaults to \code{theme_clinprog()}.
#' @param width Numeric. Plot width in inches (Default: \code{8})
#' @param height Numeric. Plot height in inches (Default: \code{5})
#' @param dpi Numeric. Plot DPI resolution (Default: \code{300})
#'
#' @return A \code{ggplot} object.
#' @export
plot_lollipop <- function(signature, outprefix = NULL, theme = theme_clinprog(), width = 8, height = 5, dpi = 300) {

  message("Building lollipop plot...")

  # Validates input
  if (!all(c("feature", "coefficient") %in% colnames(signature))) {stop("'signature' must contain 'feature' and 'coefficient' columns")}

  # Validates theme
  if (!inherits(theme, "theme")) {stop("'theme' must be a valid ggplot2 theme object")}

  # Validates plot parameters
  if (!is.numeric(dpi) || length(dpi) != 1 || dpi <= 0) {stop("'dpi' must be a single positive numeric value")}
  if (!is.numeric(width) || length(width) != 1 || width <= 0) {stop("'width' must be a single positive numeric value")}
  if (!is.numeric(height) || length(height) != 1 || height <= 0) {stop("'height' must be a single positive numeric value")}

  # Prepares data
  tt <- signature[stats::complete.cases(signature[, c("feature", "coefficient"), drop = FALSE]), , drop = FALSE]
  tt <- tt[tt$coefficient != 0, , drop = FALSE]

  # Checks if signature is still valid
  if (nrow(tt) == 0) {stop("No valid coefficients available for plotting")}

  # Selects top 10 features
  tt <- dplyr::slice_max(tt, order_by = abs(.data$coefficient), n = 10, with_ties = FALSE)

  # Orders features by coefficient values
  tt$feature <- forcats::fct_reorder(tt$feature, tt$coefficient)

  # Creates direction column to map color
  tt$Direction <- ifelse(tt$coefficient > 0, "Positive", "Negative")

  # Color map
  color_map <- c("Negative" = "#1A9850", "Positive" = "#D73027")

  # Makes plot
  ll <- ggplot2::ggplot(tt, ggplot2::aes(x = .data$feature, y = .data$coefficient)) +
    ggplot2::geom_segment(ggplot2::aes(x = .data$feature, xend = .data$feature, y = 0, yend = .data$coefficient),
                          linewidth = 0.8, color = "grey60", alpha = 0.8) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
    ggplot2::geom_point(ggplot2::aes(color = .data$Direction), size = 4) +
    ggplot2::scale_color_manual(values = color_map, name = "Prognosis",
                                labels = c("Negative" = "Favorable (Better)", "Positive" = "Unfavorable (Worse)")) +
    ggplot2::labs(x = NULL, y = "Coefficient") + ggplot2::coord_flip() + theme +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color = "grey90", linewidth = 0.5, linetype = "solid"),
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   legend.position = "top",
                   legend.title = ggplot2::element_text(size = 10),
                   legend.text = ggplot2::element_text(size = 8),
                   panel.border = ggplot2::element_blank())

  # Saves if requested
  if (!is.null(outprefix)) {
    if (!is.character(outprefix) || length(outprefix) != 1) {stop("'outprefix' must be a single character string")}
    fname <- paste0(outprefix, "_lollipop.pdf")
    ggplot2::ggsave(filename = fname, plot = ll, device = "pdf", width = width, height = height, dpi = dpi)
  }

  return(ll)
}

#' Histogram plot for clinprog signature
#'
#' @description Generates a histogram of coefficient distribution from a clinprog signature.
#'
#' @param signature A data.frame with 'feature' and 'coefficient'.
#' @param outprefix Character. Output prefix for saving the plot (optional).
#' @param theme A ggplot2 theme object. Defaults to \code{theme_clinprog()}.
#' @param binwidth Numeric. Width of the histogram bins (Default: \code{NULL}).
#' @param width Numeric. Plot width in inches (Default: \code{8})
#' @param height Numeric. Plot height in inches (Default: \code{5})
#' @param dpi Numeric. Plot DPI resolution (Default: \code{300})
#'
#' @return A \code{ggplot} object.
#' @export
plot_histogram <- function(signature, outprefix = NULL, theme = theme_clinprog(),
                             binwidth = NULL, width = 8, height = 5, dpi = 300) {

  message("Building histogram plot...")

  # Validates input
  if (!all(c("feature", "coefficient") %in% colnames(signature))) {stop("'signature' must contain 'feature' and 'coefficient' columns")}

  # Validates theme
  if (!inherits(theme, "theme")) {stop("'theme' must be a valid ggplot2 theme object")}

  # Validates binwidth if provided by user
  if (!is.null(binwidth))
  {
    if (!is.numeric(binwidth) || length(binwidth) != 1 || binwidth <= 0)
    {
      stop("'binwidth' must be a single positive numeric value.")
    }
  }

  # Validates plot sizes
  if (!is.numeric(dpi) || length(dpi) != 1 || dpi <= 0) {stop("'dpi' must be a single positive numeric value")}
  if (!is.numeric(width) || length(width) != 1 || width <= 0) {stop("'width' must be a single positive numeric value")}
  if (!is.numeric(height) || length(height) != 1 || height <= 0) {stop("'height' must be a single positive numeric value")}

  # Prepares data
  tt <- signature[stats::complete.cases(signature[, c("feature", "coefficient"), drop = FALSE]), , drop = FALSE]
  tt <- tt[tt$coefficient != 0, , drop = FALSE]

  # Checks if signature is still valid
  if (nrow(tt) == 0) {stop("No valid coefficients available for plotting")}

  # Dynamically calculates binwidth if NULL (using Sturges' Rule for small N)
  if (is.null(binwidth))
  {
    coef_range <- max(tt$coefficient) - min(tt$coefficient)
    # TODO: is it necessary?
    n_obs <- nrow(tt)

    # Calculates optimal number of bins (Sturges' Rule)
    num_bins <- grDevices::nclass.Sturges(tt$coefficient)

    # Fallback safety if range is 0 or num_bins calculation fails
    binwidth <- if (coef_range > 0) coef_range / num_bins else 0.05
  }

  # Makes plot
  pl <- ggplot2::ggplot(tt, ggplot2::aes(x = .data$coefficient)) +
    ggplot2::geom_histogram(binwidth = binwidth, fill = "#1c9099", color = "white", linewidth = 0.3, alpha = 0.8) +
    ggplot2::geom_density(ggplot2::aes(y = ggplot2::after_stat(.data$density) * nrow(tt) * binwidth),
                          color = "#016c59", linewidth = 0.8, alpha = 0.8) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::labs(x = "Coefficient", y = "Counts") + theme +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank())

  # Saves if requested
  if (!is.null(outprefix)) {
    if (!is.character(outprefix) || length(outprefix) != 1) {stop("'outprefix' must be a single character string")}
    fname <- paste0(outprefix, "_histogram.pdf")
    ggplot2::ggsave(filename = fname, plot = pl, device = "pdf", width = width, height = height, dpi = dpi)
  }

  return(pl)
}

###### SURVIVAL (MODULE II) ######

#' Generate ROC curve plot for clinprog scores
#'
#' @description Generates a time-dependent ROC curve plot
#'
#' @param x Either:
#' \itemize{
#'   \item The full list returned by \code{clinprog_cutoff_roc()}
#'   \item An \code{optimal.cutpoints} object
#' }
#' @param outprefix Character. Output prefix for saving the plot (optional).
#' If \code{NULL}, the plot is not written to disk.
#' @param width Numeric. Plot width in inches (Default: \code{7}).
#' @param height Numeric. Plot height in inches (Default: \code{6}).
#'
#' @return A ROC curve generated by \code{OptimalCutpoints::plot.optimal.cutpoints()}.
#' @export
plot_roc <- function(x, outprefix = NULL, width = 7, height = 6) {

  message("Building ROC curve...")

  # Extracts optimal cutpoint object
  if (is.list(x) && "optimal_cutpoint" %in% names(x)) {roc_obj <- x$optimal_cutpoint} else {roc_obj <- x}

  # Validates plot sizes
  if (!is.numeric(width) || length(width) != 1 || width <= 0) {stop("'width' must be a single positive numeric value")}
  if (!is.numeric(height) || length(height) != 1 || height <= 0) {stop("'height' must be a single positive numeric value")}

  # Opens PDF device if requested
  if (!is.null(outprefix))
  {
    if (!is.character(outprefix) || length(outprefix) != 1) {stop("'outprefix' must be a single character string")}
    grDevices::pdf(file = paste0(outprefix, "_roc_curve.pdf"), width = width, height = height)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  # Creates ROC plot (which = 1), but not the PROC plot (which = 2)
  OptimalCutpoints::plot.optimal.cutpoints(x = roc_obj, legend = TRUE, which = 1, col = "blue", bg = "white")

  # Captures base R plot object
  plot_obj <- grDevices::recordPlot()

  return(plot_obj)
}

#' Generate Schoenfeld residual plots for clinprog Cox models
#'
#' @description Creates Schoenfeld residual plots from proportional hazards assumption tests.
#' For multivariate Cox models, the custom internal \code{clinprog_ggcoxzph()} is used to additionally display
#' global and individual Schoenfeld test p-values.
#'
#' @param x Either:
#' \itemize{
#'   \item A \code{cox.zph} object
#'   \item A list returned by \code{clinprog_schoenfeld()}
#' }
#' @param is_multi Logical. Whether the model corresponds to a multivariate Cox analysis (default: \code{FALSE}).
#' @param outprefix Character. Output prefix for saving the plot (optional).
#' If \code{NULL}, the plot is not written to disk.
#' @param individual Logical. If \code{TRUE} and \code{outprefix} is provided, saves each Schoenfeld plot separately.
#' @param theme A ggplot2 theme object. Defaults to \code{theme_clinprog()}.
#' @param width Numeric. Plot width in inches (optional). If \code{NULL}, the width is set automatically.
#' @param height Numeric. Plot height in inches (optional). If \code{NULL}, the height is set automatically.
#' @param ... Additional arguments passed to plotting functions.
#'
#' @return a list of \code{ggplot} objects.
#' @export
plot_ph <- function(x, is_multi = FALSE, outprefix = NULL, individual = FALSE,
                          theme = theme_clinprog(), width = NULL, height = NULL, ...) {

  message("Building proportional hazards plot...")

  # Extracts cox.zph object
  if (inherits(x, "cox.zph"))
  {
    ph_object <- x
  } else if (is.list(x) && "object" %in% names(x)) {
    ph_object <- x$object
  } else {stop("Input must be a 'cox.zph' object or a valid clinprog_schoenfeld() result")}

  # Validates theme
  if (!inherits(theme, "theme")) {stop("'theme' must be a valid ggplot2 theme object")}

  # Validates arguments
  if (!is.logical(is_multi) || length(is_multi) != 1) {stop("'is_multi' must be TRUE or FALSE")}
  if (!is.logical(individual) || length(individual) != 1 || is.na(individual))
  {
    stop("'individual' must be TRUE or FALSE")
  }

  # Validates outprefix
  if (!is.null(outprefix))
  {
    if (!is.character(outprefix) || length(outprefix) != 1)
    {
      stop("'outprefix' must be NULL or a single character string")
    }
  }

  # Cross-validation: individual only makes sense with outprefix
  if (individual && is.null(outprefix))
  {
    warning("'individual = TRUE' requires 'outprefix' to be provided. Ignoring 'individual'.")
    individual <- FALSE
  }

  # Validates plot sizes
  if (!is.null(width))
  {
    if (!is.numeric(width) || length(width) != 1 || width <= 0)
    {
      stop("'width' must be NULL or a single positive numeric value")
    }
  }

  if (!is.null(height))
  {
    if (!is.numeric(height) || length(height) != 1 || height <= 0)
    {
      stop("'height' must be NULL or a single positive numeric value")
    }
  }

  # Generates plots
  if (is_multi)
  {
    ph_plots <- clinprog_ggcoxzph(fit = ph_object, ggtheme = theme, ...)
  } else {ph_plots <- survminer::ggcoxzph(fit = ph_object, ggtheme = theme, ...)}

  # Renames variables in plot labels
  ph_plots <- lapply(ph_plots, function(p)
  {
    if (!is.null(p$labels$y)) {p$labels$y <- gsub("score_group", "Score", p$labels$y, fixed = TRUE)}
    return(p)
  })

  # Extracts number of plots
  n_plots <- length(ph_plots)

  # Dynamically defines number of columns
  if (n_plots <= 2) {ncol_plot <- n_plots}
  else if (n_plots <= 4) {ncol_plot <- 2}
  else if (n_plots <= 9) {ncol_plot <- 3}
  else {ncol_plot <- 4}

  # Dynamically defines number of rows
  nrow_plot <- ceiling(n_plots / ncol_plot)

  # Automatically defines figure dimensions if not provided
  if (is.null(width)) {width <- 6 * ncol_plot}
  if (is.null(height)) {height <- 4 * nrow_plot}

  # Combines plots into a single patchwork object
  combined_plot <- patchwork::wrap_plots(ph_plots, ncol = ncol_plot)

  # Saves plots if requested
  if (!is.null(outprefix))
  {
    filename <- paste0(outprefix, "_ph_assumptions_plot.pdf")
    grDevices::pdf(file = filename, width = width, height = height)
    on.exit(grDevices::dev.off(), add = TRUE)
    print(combined_plot)

    # Saves each variable individually (optional)
    if (individual)
    {
      # Sanitizes variable names for safe file names
      safe_names <- gsub("[^A-Za-z0-9_\\-]", "_", names(ph_plots))

      # Uses single-plot dimensions for the individual PDFs
      ind_width  <- if (!is.null(width))  min(width,  6) else 6
      ind_height <- if (!is.null(height)) min(height, 4) else 4

      for (i in seq_along(ph_plots))
      {
        fname_i <- paste0(outprefix, "_ph_assumptions_", safe_names[i], ".pdf")
        ggplot2::ggsave(filename = fname_i, plot = ph_plots[[i]], device = "pdf",
                        width = ind_width, height = ind_height)
      }
    }
  }

  return(combined_plot)
}

#' Customized Schoenfeld residual plots for clinprog multivariate models
#'
#' @description Internal plotting helper used by \code{plot_ph()} to generate custom Schoenfeld residual plots
#'
#' This function is based on \code{survminer::ggcoxzph()}, with modifications to:
#' \itemize{
#'   \item Include global proportional hazards test p-values in plot titles
#'   \item Improve visualization for multivariate Cox regression models
#'   \item Return plot lists compatible with clinprog plotting workflows
#' }
#'
#' @param fit A \code{cox.zph} object generated by \code{survival::cox.zph()}.
#' @param resid Logical. Whether to display Schoenfeld residual points (default: \code{TRUE}).
#' @param se Logical. Whether to display confidence bands (default: \code{TRUE}).
#' @param df Numeric. Degrees of freedom used in spline smoothing (default: \code{4}).
#' @param nsmo Numeric. Number of spline smoothing points (default: \code{40}).
#' @param var Variables to plot. Can be numeric indices or variable names.
#' @param point.col Point color for residuals (default: \code{"red"}).
#' @param point.size Numeric. Point size (default: \code{1}).
#' @param point.shape Numeric. Point shape (default: \code{19}).
#' @param point.alpha Numeric. Point transparency (default: \code{1}).
#' @param caption Optional plot caption.
#' @param ggtheme ggplot2 theme object (default: \code{survminer::theme_survminer()}).
#' @param ... Additional arguments passed to \code{ggpubr::ggpar()}.
#'
#' @return A list of \code{ggplot} objects of class \code{"ggcoxzph"}.
#' @keywords internal
clinprog_ggcoxzph <- function(fit, resid = T, se = T, df = 4, nsmo = 40, var, point.col = "red", point.size = 1,
                            point.shape = 19, point.alpha = 1, caption = NULL,
                            ggtheme = survminer::theme_survminer(), ...) {
  x <- fit
  if (!methods::is(x, "cox.zph")) {stop("Can't handle an object of class ", class(x))}

  xx <- x$x
  yy <- x$y
  d <- nrow(yy)
  df <- max(df)
  nvar <- ncol(yy)
  pred.x <- seq(from = min(xx), to = max(xx), length = nsmo)
  temp <- c(pred.x, xx)
  lmat <- splines::ns(temp, df = df, intercept = T)
  pmat <- lmat[1:nsmo, ]
  xmat <- lmat[-(1:nsmo), ]
  qmat <- qr(xmat)

  if (qmat$rank < df) {stop("Spline fit is singular, try a smaller degrees of freedom")}

  if (se)
  {
    bk <- backsolve(qmat$qr[1:df, 1:df], diag(df))
    xtx <- bk %*% t(bk)
    seval <- d * ((pmat %*% xtx) * pmat) %*% rep(1, df)
  }

  ylab <- paste("Beta(t) for", dimnames(yy)[[2]])

  if (missing(var))
  {
    var <- 1:nvar
  } else {
    if (is.character(var)) {var <- match(var, dimnames(yy)[[2]])}
    if (any(is.na(var)) || max(var) > nvar || min(var) < 1) {stop("Invalid variable requested")}
  }

  if (x$transform == "log")
  {
    xx <- exp(xx)
    pred.x <- exp(pred.x)
  } else if (x$transform != "identity") {
    xtime <- as.numeric(dimnames(yy)[[1]])
    indx <- !duplicated(xx)
    apr1 <- stats::approx(xx[indx], xtime[indx], seq(min(xx), max(xx), length = 17)[2 * (1:8)])
    temp <- signif(apr1$y, 2)
    apr2 <- stats::approx(xtime[indx], xx[indx], temp)
    xaxisval <- apr2$y
    xaxislab <- rep("", 8)
    for (i in 1:8) xaxislab[i] <- format(temp[i])
  }

  plots <- list()
  lapply(var, function(i) {
    invisible(round(x$table[i, 3],4) -> pval)
    invisible(round(x$table[nrow(x$table), 3],4) -> global)

    ggplot2::ggplot() + ggplot2::labs(title = paste0('Global Schoenfeld Test p: ', global),
                                      subtitle = paste0('Individual Schoenfeld Test p: ', pval)) +
      ggtheme + ggplot2::theme(plot.title = ggplot2::element_text(hjust = .5, vjust = .5, face = "bold",
                                                                  margin = ggplot2::margin(0, 0, 10, 0)),
                               plot.subtitle = ggplot2::element_text(hjust = 0, vjust = .5, face = "plain",
                                                                     margin = ggplot2::margin(10, 0, 10, 0))) -> gplot

    y <- yy[, i]
    yhat <- as.vector(pmat %*% qr.coef(qmat, y))

    if (resid) {yr <- range(yhat, y)} else {yr <- range(yhat)}

    if (se)
    {
      temp <- as.vector(2 * sqrt(x$var[i, i] * seval))
      yup <- yhat + temp
      ylow <- yhat - temp
      yr <- range(yr, yup, ylow)
    }

    if (x$transform == "identity") {
      gplot + ggplot2::geom_line(ggplot2::aes(x = pred.x, y = yhat)) + ggplot2::xlab("Time") +
        ggplot2::ylab(ylab[i]) + ggplot2::ylim(yr) -> gplot
    } else if (x$transform == "log") {
      gplot + ggplot2::geom_line(ggplot2::aes(x = log(pred.x), y = yhat)) + ggplot2::xlab("Time") +
        ggplot2::ylab(ylab[i]) + ggplot2::ylim(yr)  -> gplot
    } else {
      gplot + ggplot2::geom_line(ggplot2::aes(x = pred.x, y = yhat)) + ggplot2::xlab("Time") + ggplot2::ylab(ylab[i]) +
        ggplot2::scale_x_continuous(breaks = xaxisval, labels = xaxislab) + ggplot2::ylim(yr) -> gplot
    }

    if (resid)
    {
      gplot <- gplot + ggplot2::geom_point(ggplot2::aes(x = xx, y = y), col = point.col,
                                           shape = point.shape, size = point.size, alpha = point.alpha)
    }

    if (se)
    {
      gplot <- gplot + ggplot2::geom_line(ggplot2::aes(x = pred.x, y = yup), lty = "dashed") +
        ggplot2::geom_line(ggplot2::aes( x = pred.x, y = ylow), lty = "dashed")
    }

    ggpubr::ggpar(gplot, ...)
  }) -> plots

  names(plots) <- var
  class(plots) <- c("ggcoxzph", "ggsurv", "list")

  # case of multivariate Cox
  if ("GLOBAL" %in% rownames(x$table)) {global_p <- x$table["GLOBAL", 3]} else {global_p <- NULL}

  attr(plots, "global_pval") <- global_p
  attr(plots, "caption") <- caption
  plots
}

#' Generate Kaplan-Meier survival plot for clinprog scores
#'
#' @description Creates a Kaplan-Meier survival curve using categorized clinprog scores generated by
#' \code{run_survival()}. Input data must already contain a binary \code{score_group} variable.
#'
#' @param data Data.frame containing survival information and categorized scores.
#' Must contain columns \code{OS}, \code{OS.time}, and \code{score_group}.
#' @param cutoff Numeric. Cutoff value used to categorize scores. Displayed in legend labels (optional).
#' @param outprefix Character. Output prefix for saving the plot (optional).
#' If \code{NULL}, the plot is not written to disk.
#' @param pval Logical, Numeric or Character. Value to display in the plot. Typically extracted from \code{clinprog_uniCox_model()} (optional).
#' @param palette Character vector. Color palette for Kaplan-Meier curves (default: \code{c("#D73027", "#1A9850")}).
#' @param ylab Character. Title for the Y-axis (default: \code{"Survival probability"}).
#' @param xlab Character. Title for the X-axis (default: \code{"Time"}).
#' @param title Character. Title for the plot (default: \code{NULL}).
#' @param legend.title Character. Title for the legend (default: \code{"clinprog groups"}).
#' @param theme ggplot2 theme used in the Kaplan-Meier plot (default: \code{survminer::theme_survminer()}).
#' @param tables.theme ggplot2 theme used in the risk table (default: \code{survminer::theme_cleantable()}).
#' @param width Numeric. Plot width in inches (default: \code{8}).
#' @param height Numeric. Plot height in inches (default: \code{5}).
#'
#' @return A Kaplan-Meier plot generated by \code{survminer::ggsurvplot()}.
#' @export
plot_km <- function(data, cutoff = NULL, outprefix = NULL, pval = FALSE, palette = c("#D73027", "#1A9850"),
                      ylab = "Survival probability", xlab = "Time", title = NULL, legend.title = "Score cutoff",
                      theme = survminer::theme_survminer(), tables.theme = survminer::theme_cleantable(),
                      width = 8, height = 5) {

  message("Building Kaplan-Meier curves...")

  # Validates input
  required_cols <- c("OS", "OS.time", "score_group")
  if (!all(required_cols %in% colnames(data))) {stop("Input data must contain columns: 'OS', 'OS.time', and 'score_group'")}

  # Validates p-value parameter
  if (!(is.logical(pval) || is.numeric(pval) || is.character(pval))) {stop("'pval' must be logical, numeric, or character")}
  if (is.logical(pval) && length(pval) != 1) {stop("'pval' must be a single logical value")}
  if (is.character(pval) && length(pval) != 1) {stop("'pval' must be a single character string")}
  if (is.numeric(pval))
  {
    if (length(pval) != 1 || pval < 0 || pval > 1) {stop("'pval' must be a single numeric value between 0 and 1")}
  }

  # Validates plot parameters
  if (!is.null(cutoff)) {if (!is.numeric(cutoff) || length(cutoff) != 1) {stop("'cutoff' must be a single numeric value")}}
  if (!is.character(palette) || length(palette) != 2) {stop("'palette' must be a character vector of length 2")}
  if (!is.character(ylab) || length(ylab) != 1) {stop("'ylab' must be a single character string")}
  if (!is.character(xlab) || length(xlab) != 1) {stop("'xlab' must be a single character string")}
  if (!is.null(title) && (!is.character(title) || length(title) != 1))
  {
    stop("'title' must be NULL or a single character string")
  }
  if (!is.null(legend.title) && (!is.character(legend.title) || length(legend.title) != 1))
  {
    stop("'legend.title' must be NULL or a single character string")
  }

  # Validates plot sizes
  if (!is.numeric(width) || length(width) != 1 || width <= 0) {stop("'width' must be a single positive numeric value")}
  if (!is.numeric(height) || length(height) != 1 || height <= 0) {stop("'height' must be a single positive numeric value")}

  # Ensures at least two groups exist
  if (length(unique(stats::na.omit(data$score_group))) < 2)
  {
    warning("Data could not be partitioned into low/high scores")
    return(NULL)
  }

  # Fits Kaplan-Meier model
  fit <- survival::survfit(survival::Surv(OS.time, OS) ~ score_group, data = data)

  # Creates legend labels
  if (!is.null(cutoff))
  {
    legend_labels <- c(paste0("score > ", round(cutoff, 4)), paste0("score <= ", round(cutoff, 4)))
  } else {legend_labels <- unique(as.character(stats::na.omit(data$score_group)))}

  # Generates Kaplan-Meier plot
  km_plot <- suppressMessages(suppressWarnings(survminer::ggsurvplot(
    fit = fit,
    data = data,
    risk.table = "abs_pct",
    surv.scale = "percent",
    palette = palette,
    ylab = ylab,
    ylim = c(0, 1),
    break.y.by = 0.25,
    tables.height = 0.2,
    risk.table.title = "Number at risk (%)",
    tables.y.text = FALSE,
    xlab = xlab,
    tables.theme = tables.theme,
    ggtheme = theme,
    conf.int = FALSE,
    linetype = 1,
    censor.shape = 73,
    censor = TRUE,
    censor.size = 4,
    pval = pval,
    font.legend = 14,
    font.x = 18,
    font.y = 18,
    font.tickslab = 14,
    pval.size = 5,
    pval.coord = c(0, 0.1),
    title = title,
    legend = c(0.85, 0.9),
    legend.title = legend.title,
    linewidth = 1,
    fontsize = 3,
    axes.offset = TRUE,
    surv.median.line = "none",
    legend.labs = legend_labels
  )))

  # Adjusts risk table formatting
  km_plot$table <- km_plot$table + ggplot2::theme(plot.title = ggplot2::element_text(size = 14, hjust = 0),
                                                  plot.margin = ggplot2::margin(5, 5, 5, 30))

  # Saves plot
  if (!is.null(outprefix))
  {
    if (!is.character(outprefix) || length(outprefix) != 1) {stop("'outprefix' must be a single character string")}
    grDevices::pdf(file = paste0(outprefix, "_km_plot.pdf"), width = width, height = height, onefile = FALSE)
    on.exit(grDevices::dev.off(), add = TRUE)
    suppressMessages(suppressWarnings(print(km_plot)))
  }

  return(km_plot)
}

#' Generate forest plot for multivariate Cox regression model
#'
#' @description Creates a forest plot from a multivariate Cox proportional hazards model.
#'
#' @param model_object A fitted \code{survival::coxph} model object.
#' @param outprefix Character. Output prefix for saving the plot (optional).
#' @param panels List. Custom forestmodel panels. If \code{NULL}, default clinprog panels are used (optional).
#' @param hr_breaks Numeric. Vector defining hazard ratio axis breaks (default: \code{c(0.25, 0.5, 1, 2, 4, 8)}).
#' @param width Numeric. Plot width in inches (default: \code{8}).
#' @param height Numeric. Plot height in inches (default: \code{5}).
#'
#' @return A forest plot object generated by \code{forestmodel::forest_model()}.
#' @export
plot_forest <- function(model_object, outprefix = NULL, panels = NULL,
                          hr_breaks = c(0.25, 0.5, 1, 2, 4, 8), width = 8, height = 5) {

  message("Building forest plot...")

  # Validates model object
  if (!inherits(model_object, "coxph")) {stop("'model_object' must be a valid 'survival::coxph' object")}

  # Validates custom panel
  if (!is.null(panels) && !is.list(panels)) {stop("'panels' must be NULL or a list")}

  # Validates HR breaks
  if (!is.numeric(hr_breaks) || length(hr_breaks) < 1 || any(hr_breaks <= 0)) {stop("'hr_breaks' must contain positive numeric values")}

  # Validates plot sizes
  if (!is.numeric(width) || length(width) != 1 || width <= 0) {stop("'width' must be a single positive numeric value")}
  if (!is.numeric(height) || length(height) != 1 || height <= 0) {stop("'height' must be a single positive numeric value")}

  # Uses default custom forestmodel panels if none provided
  if (is.null(panels))
  {
    panels <- list(list(width = 0.01),
                   list(width = 0.1, display = ~ifelse(variable == "score_group", "Score", variable),
                        fontface = "plain", heading = "Variable"),
                   list(width = 0.1, display = ~level, fontface = "italic", heading = "Group"),
                   list(width = 0.05, display = ~n, hjust = 1, fontface = "plain", heading = "N"),
                   list(width = 0.01, item = "vline", hjust = 0.5),
                   list(width = 0.7, item = "forest", hjust = 0.5, heading = "Hazard Ratio", linetype = "dashed", line_x = 0),
                   list(width = 0.01, item = "vline", hjust = 0.5),
                   list(width = 0.05, fontface = "plain", heading = "HR (95% CI)",
                        display = ~ifelse(reference, "Reference",
                                          sprintf("%0.2f (%0.2f - %0.2f)",
                                                  trans(estimate), trans(conf.low), trans(conf.high))), display_na = NA),
                   list(width = 0.05, fontface = "plain",
                        display = ~ifelse(reference, "", ifelse(p.value < 0.0001, "<0.0001",
                                                                round(x = p.value, digits = 4))),
                        display_na = NA, hjust = 1, heading = "p-value"),
                   list(width = 0.01))
  }

  # Extracts model confidence intervals
  sum_model <- summary(model_object)
  conf_int <- sum_model$conf.int
  hr_min <- min(conf_int[, "lower .95"], na.rm = TRUE)
  hr_max <- max(conf_int[, "upper .95"], na.rm = TRUE)

  # Avoids invalid values
  hr_min <- max(hr_min, 1e-6)

  # Clips extreme values
  hr_min <- max(hr_min, 0.001)
  hr_max <- min(hr_max, 100)

  # Creates symmetric log scale around HR = 1
  max_range <- max(abs(log(hr_min)), abs(log(hr_max)))

  # Defines symmetrical boundaries
  max_range <- round(max_range, 2)
  limits_log <- c(-max_range, max_range)

  # Adjusts HR breaks to plotting range
  hr_breaks <- hr_breaks[hr_breaks >= exp(limits_log[1]) & hr_breaks <= exp(limits_log[2])]
  if (length(hr_breaks) < 2) {hr_breaks <- c(0.5, 1, 2)}
  breaks_log <- log(hr_breaks)

  # Saves plot
  if (!is.null(outprefix))
  {
    if (!is.character(outprefix) || length(outprefix) != 1) {stop("'outprefix' must be a single character string")}
    grDevices::pdf(file = paste0(outprefix, "_forest_plot.pdf"), width = width, height = height, onefile = FALSE)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  # Generates forest plot
  forest_plot <- suppressMessages(suppressWarnings(forestmodel::forest_model(model = model_object, exponentiate = TRUE,
                                                                             panels = panels, breaks = breaks_log,
                                                                             limits = limits_log,
                                                                             factor_separate_line = FALSE,
                                                                             recalculate_width = TRUE,
                                                                             recalculate_height = TRUE)))
  if (!is.null(outprefix)) {suppressMessages(suppressWarnings(print(forest_plot)))}

  return(forest_plot)
}

#' Generate bootstrap covariate frequency barplot
#'
#' @description Creates a barplot with the frequency of covariates selected in multivariate Cox regression with bootstrap.
#'
#' @param plot_df Data.frame generated by \code{clinprog_merge()} or extracted from \code{clinprog_multiCox_test()}
#' bootstrap results. Must contain columns \code{covariate} and \code{frequency}.
#' @param outprefix Character. Output prefix for saving the plot (optional).
#' @param frequency.threshold Numeric. Frequency threshold (in \%) displayed as a horizontal dashed
#' line (default: \code{25}). Always interpreted as a percentage between 0-100, regardless of \code{type}.
#' @param type Character. How to display frequencies: \code{\"relative\"} (percentage, default) or
#' \code{\"absolute\"} (raw counts). Only these two values are accepted.
#' @param n_bootstraps Integer. Total number of bootstrap iterations (minimum value: 2). Required when
#' \code{type = \"absolute\"}. When \code{type = \"relative\"} and supplied, absolute frequencies are
#' converted to percentages via \code{frequency / n_bootstraps * 100}.
#' @param theme A ggplot2 theme object. Defaults to \code{theme_clinprog()}.
#' @param bar.width Numeric. Width of bars (default: \code{0.5}).
#' @param ylab Character. Title for the Y-axis. If \code{NULL}, a sensible label is chosen automatically based on \code{type}.
#' @param xlab Character. Title for the X-axis (default: empty string).
#' @param width Numeric. Plot width in inches (default: \code{8}).
#' @param height Numeric. Plot height in inches (default: \code{5}).
#'
#' @return A \code{ggplot} object.
#' @export
plot_barplot <- function(plot_df, outprefix = NULL, frequency.threshold = 25, type = c("relative", "absolute"),
                           n_bootstraps = NULL, theme = theme_clinprog(), bar.width = 0.5,
                           ylab = NULL, xlab = "", width = 8, height = 5)
{

  message("Building bar plot...")

  # Resolves 'type' (validates that it is one of the accepted values)
  type <- match.arg(type)

  # Validates input data
  if (!is.data.frame(plot_df)) {stop("'plot_df' must be a data.frame")}
  required_cols <- c("covariate", "frequency")
  if (!all(required_cols %in% colnames(plot_df))) {stop("'plot_df' must contain columns: 'covariate' and 'frequency'")}

  # Validates frequency threshold (always in %)
  if (!is.numeric(frequency.threshold) || length(frequency.threshold) != 1 ||
      frequency.threshold < 0 || frequency.threshold > 100)
  {
    stop("'frequency.threshold' must be a numeric value (percentage) between 0 and 100")
  }

  # Validates bootstraps
  if (!is.null(n_bootstraps))
  {
    if (!is.numeric(n_bootstraps) || length(n_bootstraps) != 1 || n_bootstraps < 2 || n_bootstraps %% 1 != 0)
    {
      stop("'n_bootstraps' must be NULL or a single integer value >= 2")
    }
  }

  # Enforces: type = "absolute" requires "n_bootstraps" (otherwise the threshold cannot be positioned on the axis)
  if (type == "absolute" && is.null(n_bootstraps))
  {
    stop("'n_bootstraps' must be provided when 'type = absolute'.")
  }

  # Validates bar width
  if (!is.numeric(bar.width) || length(bar.width) != 1 || bar.width <= 0) {stop("'bar.width' must be a single positive numeric value")}

  # Validates theme
  if (!inherits(theme, "theme")) {stop("'theme' must be a valid ggplot2 theme object")}

  # Validates axis labels (ylab NULL = auto; xlab must be a string)
  if (!is.null(ylab) && (!is.character(ylab) || length(ylab) != 1)) {stop("'ylab' must be NULL or a single character string")}
  if (!is.character(xlab) || length(xlab) != 1) {stop("'xlab' must be a single character string")}

  # Validates plot sizes
  if (!is.numeric(width) || length(width) != 1 || width <= 0) {stop("'width' must be a single positive numeric value")}
  if (!is.numeric(height) || length(height) != 1 || height <= 0) {stop("'height' must be a single positive numeric value")}

  # Substitutes 'score_group' by 'Score'
  plot_df$covariate <- gsub("^score_group$", "Score", plot_df$covariate)

  # Ensures frequency is numeric and non-negative
  plot_df$frequency <- as.numeric(plot_df$frequency)
  if (anyNA(plot_df$frequency)) {stop("'frequency' column contains NA values")}
  if (any(plot_df$frequency < 0)) {stop("'frequency' values must be non-negative")}

  # Resolves units: computes 'freq_pct' (always in %) for ordering and threshold comparison, and 'freq_display' (Y axis)
  if (type == "relative")
  {
    if (!is.null(n_bootstraps))
    {
      # Converts absolute counts --> percentages
      freq_pct <- plot_df$frequency / n_bootstraps * 100
      if (any(freq_pct > 100, na.rm = TRUE)) {stop("Computed relative frequencies > 100%. Check 'n_bootstraps'.")}
    }
    else
    {
      # Assumes 'frequency' is already a percentage
      freq_pct <- plot_df$frequency
      if (any(freq_pct > 100, na.rm = TRUE))
      {
        stop("'frequency' values exceed 100 but 'n_bootstraps' was not provided. ",
                 "Either pass 'n_bootstraps' or make sure 'frequency' is already a percentage.")
      }
    }
    freq_display <- freq_pct
    y_max <- 100
    y_brk <- seq(0, 100, by = 20)
    if (is.null(ylab)) ylab <- "Frequency (%)"
  }
  else {  # type == "absolute"
    # n_bootstraps is guaranteed non-NULL here (checked above)
    freq_display <- plot_df$frequency
    freq_pct <- plot_df$frequency / n_bootstraps * 100
    y_max <- ceiling(max(freq_display, na.rm = TRUE) * 1.05)
    if (!is.finite(y_max) || y_max <= 0) y_max <- 1
    y_brk <- pretty(c(0, y_max))
    if (is.null(ylab)) ylab <- "Frequency (count)"
  }

  # Attaches internal columns used for plotting/ordering
  plot_df$freq_pct <- freq_pct
  plot_df$freq_display <- freq_display

  # Orders by percentage (stable across 'type' choices)
  plot_df <- plot_df[order(plot_df$freq_pct, decreasing = TRUE), , drop = FALSE]

  # Positions the threshold line on the correct axis scale. 'frequency.threshold' is always in %; convert to counts when needed.
  if (type == "absolute")
  {
    threshold_display <- frequency.threshold / 100 * n_bootstraps
  } else {
    threshold_display <- frequency.threshold
  }

  # Generates barplot
  final_plot <- ggplot2::ggplot(data = plot_df,
                                ggplot2::aes(x = stats::reorder(.data$covariate, .data$freq_display),
                                             y = .data$freq_display)) +
    ggplot2::geom_col(width = bar.width, color = "white", fill = "#1c9099", linewidth = 0.3, alpha = 0.8) +
    ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
    ggplot2::geom_hline(yintercept = threshold_display, color = "grey40", linetype = "dashed", linewidth = 0.5) +
    ggplot2::scale_y_continuous(breaks = y_brk, limits = c(0, y_max), expand = c(0, 0)) +
    ggplot2::coord_flip() + theme +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 0, hjust = 1),
                   panel.grid.major.x = ggplot2::element_line(colour = "grey90"),
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank())

  # Saves plot
  if (!is.null(outprefix))
  {
    if (!is.character(outprefix) || length(outprefix) != 1) {stop("'outprefix' must be a single character string")}
    grDevices::pdf(file = paste0(outprefix, "_bootstrap_barplot.pdf"), width = width, height = height, onefile = FALSE)
    on.exit(grDevices::dev.off(), add = TRUE)
    print(final_plot)
  }

  return(final_plot)
}

#' Generate a swimmer plot for clinprog score groups
#'
#' @description Creates a swimmer plot (a.k.a. swimlane plot) where each
#' horizontal bar represents one patient. The bar length corresponds to the
#' follow-up time (from time = 0 up to the observed \code{time_col}), and the
#' bar color encodes whether the patient's score is below or equal (low) or
#' strictly above (high) the specified \code{cutoff}. At the end of each bar,
#' a symbol indicates the patient's status at last follow-up: a circle for
#' alive/censored and a triangle for dead/event (shapes are configurable).
#'
#' @param data Data.frame containing clinical/survival information. Must
#' contain at least the columns referenced by \code{group_col},
#' \code{score_col}, \code{os_col}, and \code{time_col}.
#' @param cutoff Numeric. Cutoff used to dichotomize the score into
#' \code{"Low"} and \code{"High"}.
#' @param group_col Character. Column name with the dichotomized score group.
#' Validated for existence, but the actual coloring is derived from
#' \code{score_col} and \code{cutoff} (Default: \code{"score_group"}).
#' @param score_col Character. Column name with the numeric score (Default:
#' \code{"score"}). Used together with \code{cutoff} to define the Low/High
#' color group.
#' @param os_col Character. Column name for OS status (Default: \code{"OS"}).
#' Values are expected to be 0/1 (0 = Alive/censored, 1 = Dead/event).
#' @param time_col Character. Column name for follow-up time (Default:
#' \code{"OS.time"}).
#' @param id_col Character or NULL. Column name used as patient ID for the
#' Y-axis. If \code{NULL} (Default), row names are used when they differ
#' from the default integer sequence; otherwise an internal index is
#' generated.
#' @param outprefix Character. Output prefix for saving the plot (Default:
#' \code{NULL}). If \code{NULL}, the plot is not written to disk.
#' @param palette Character vector of length 2. Colors for below/equal and
#' above cutoff (Default: \code{c("#1A9850", "#D73027")}).
#' @param ylab Character. Title for the Y-axis (Default: \code{"Patients"}).
#' @param xlab Character. Title for the X-axis (Default:
#' \code{"Follow-up time"}).
#' @param title Character. Plot title (Default: \code{NULL}).
#' @param legend_title Character. Title for the color legend (Default:
#' \code{"Score cutoff"}).
#' @param theme A ggplot2 theme object. Defaults to \code{theme_clinprog()}.
#' @param width Numeric. Plot width in inches (Default: \code{8}).
#' @param height Numeric. Plot height in inches (Default: \code{6}).
#' @param show_labels Logical or character vector. If \code{NULL} (Default)
#' or \code{FALSE}, no patient labels are shown on the Y-axis. If
#' \code{TRUE}, all patients are labeled. If a character vector, only the
#' matching patient IDs are labeled (others are blank).
#' @param sort_by Character. Order patients along the Y-axis by follow-up
#' \code{"time"} (Default), \code{"score"}, or keep original order
#' \code{"none"}.
#' @param time_cutoff Numeric or \code{NULL} (Default). Optional vertical
#' dashed line at a given follow-up time.
#' @param facet_by Character or \code{NULL} (Default). Optional column name
#' used to facet the plot.
#' @param alive_shape Numeric. Point shape for alive/censored patients
#' (Default: \code{16}, filled circle).
#' @param dead_shape Numeric. Point shape for dead/event patients (Default:
#' \code{17}, filled triangle).
#' @param alive_color Character. Color for alive/censored patients
#' (Default: \code{"#2C7FB8"}, blue).
#' @param dead_color Character. Color for dead/event patients
#' (Default: \code{"#F28E2B"}, orange).
#' @param point_size Numeric. Size of the status symbols (Default: \code{1}).
#' @param bar_height Numeric. Height of the swimmer bars, between 0 and 1
#' (Default: \code{0.7}).
#' @param show_legend Logical. Whether to display the legend (Default:
#' \code{TRUE}).
#'
#' @return A \code{ggplot} object.
#' @importFrom rlang .data
#' @export
plot_swimmer <- function(
    data,
    cutoff,
    group_col = "score_group",
    score_col = "score",
    os_col = "OS",
    time_col = "OS.time",
    id_col = NULL,
    outprefix = NULL,
    palette = c("#1A9850", "#D73027"),
    ylab = "Patients",
    xlab = "Follow-up time",
    title = NULL,
    legend_title = "Score cutoff",
    theme = theme_clinprog(),
    width = 8,
    height = 6,
    show_labels = NULL,
    sort_by = c("time", "score", "none"),
    time_cutoff = NULL,
    facet_by = NULL,
    alive_shape = 16,
    dead_shape = 17,
    alive_color = "#2C7FB8",
    dead_color = "#F28E2B",
    point_size = 1,
    bar_height = 0.7,
    show_legend = TRUE
) {
  log_message("Building swimmer plot...")

  sort_by <- match.arg(sort_by)

  # Validates input data

  if (!is.data.frame(data)) {
    log_stop("Argument 'data' must be a data.frame")
  }

  if (!is.character(group_col) || length(group_col) != 1) {
    log_stop("Argument 'group_col' must be a single character string")
  }

  if (!is.character(score_col) || length(score_col) != 1) {
    log_stop("Argument 'score_col' must be a single character string")
  }

  if (!is.character(os_col) || length(os_col) != 1) {
    log_stop("Argument 'os_col' must be a single character string")
  }

  if (!is.character(time_col) || length(time_col) != 1) {
    log_stop("Argument 'time_col' must be a single character string")
  }

  required_cols <- c(
    group_col,
    score_col,
    os_col,
    time_col
  )

  if (!all(required_cols %in% colnames(data))) {
    missing <- setdiff(required_cols, colnames(data))

    log_stop(
      "Argument 'data' is missing required column(s): ",
      paste(missing, collapse = ", ")
    )
  }

  if (!is.numeric(data[[score_col]])) {
    log_stop(
      paste0(
        "Column '",
        score_col,
        "' must be numeric."
      )
    )
  }

  if (!is.numeric(data[[time_col]])) {
    log_stop(
      paste0(
        "Column '",
        time_col,
        "' must be numeric."
      )
    )
  }

  # Validates cutoff

  if (missing(cutoff) || is.null(cutoff)) {
    log_stop(
      "Argument 'cutoff' must be provided (numeric scalar)."
    )
  }

  if (!is.numeric(cutoff) ||
      length(cutoff) != 1 ||
      !is.finite(cutoff)) {
    log_stop(
      "Argument 'cutoff' must be a single finite numeric value"
    )
  }

  # Validates palette

  if (!is.character(palette) || length(palette) < 2) {
    log_stop(
      "Argument 'palette' must be a character vector with at least 2 colors"
    )
  }

  if (anyNA(palette[1:2])) {
    log_stop(
      "Argument 'palette' must not contain NA values"
    )
  }

  # Validates theme

  if (!inherits(theme, "theme")) {
    log_stop(
      "Argument 'theme' must be a valid ggplot2 theme object"
    )
  }

  # Validates labels

  if (!is.null(ylab) &&
      (!is.character(ylab) || length(ylab) != 1)) {
    log_stop(
      "Argument 'ylab' must be NULL or a single character string"
    )
  }

  if (!is.null(xlab) &&
      (!is.character(xlab) || length(xlab) != 1)) {
    log_stop(
      "Argument 'xlab' must be NULL or a single character string"
    )
  }

  if (!is.null(title) &&
      (!is.character(title) || length(title) != 1)) {
    log_stop(
      "Argument 'title' must be NULL or a single character string"
    )
  }

  if (!is.null(legend_title) &&
      (!is.character(legend_title) || length(legend_title) != 1)) {
    log_stop(
      "Argument 'legend_title' must be NULL or a single character string"
    )
  }

  # Validates plot sizes

  if (!is.numeric(width) ||
      length(width) != 1 ||
      width <= 0) {
    log_stop(
      "Argument 'width' must be a single positive numeric value"
    )
  }

  if (!is.numeric(height) ||
      length(height) != 1 ||
      height <= 0) {
    log_stop(
      "Argument 'height' must be a single positive numeric value"
    )
  }

  # Validates visual parameters

  if (!is.numeric(alive_shape) ||
      length(alive_shape) != 1) {
    log_stop(
      "Argument 'alive_shape' must be a single numeric value"
    )
  }

  if (!is.numeric(dead_shape) ||
      length(dead_shape) != 1) {
    log_stop(
      "Argument 'dead_shape' must be a single numeric value"
    )
  }

  if (!is.character(alive_color) ||
      length(alive_color) != 1) {
    log_stop(
      "Argument 'alive_color' must be a single color string"
    )
  }

  if (!is.character(dead_color) ||
      length(dead_color) != 1) {
    log_stop(
      "Argument 'dead_color' must be a single color string"
    )
  }

  if (!is.numeric(point_size) ||
      length(point_size) != 1 ||
      point_size <= 0) {
    log_stop(
      "Argument 'point_size' must be a single positive numeric value"
    )
  }

  if (!is.numeric(bar_height) ||
      length(bar_height) != 1 ||
      bar_height <= 0 ||
      bar_height > 1) {
    log_stop(
      "Argument 'bar_height' must be a single numeric value in (0, 1]"
    )
  }

  if (!is.logical(show_legend) ||
      length(show_legend) != 1 ||
      is.na(show_legend)) {
    log_stop(
      "Argument 'show_legend' must be TRUE or FALSE"
    )
  }

  # Validates optional time cutoff

  if (!is.null(time_cutoff)) {
    if (!is.numeric(time_cutoff) ||
        length(time_cutoff) != 1 ||
        !is.finite(time_cutoff)) {
      log_stop(
        "Argument 'time_cutoff' must be NULL or a single finite numeric value"
      )
    }
  }

  # Validates facet_by

  if (!is.null(facet_by)) {
    if (!is.character(facet_by) ||
        length(facet_by) != 1) {
      log_stop(
        "Argument 'facet_by' must be NULL or a single character string"
      )
    }

    if (!facet_by %in% colnames(data)) {
      log_stop(
        paste0(
          "Argument 'facet_by' column '",
          facet_by,
          "' not found in data."
        )
      )
    }
  }

  # Validates id_col

  if (!is.null(id_col)) {
    if (!is.character(id_col) ||
        length(id_col) != 1) {
      log_stop(
        "Argument 'id_col' must be NULL or a single character string"
      )
    }

    if (!id_col %in% colnames(data)) {
      log_stop(
        paste0(
          "Argument 'id_col' column '",
          id_col,
          "' not found in data."
        )
      )
    }
  }

  # Validates show_labels

  if (!is.null(show_labels) &&
      !is.logical(show_labels) &&
      !is.character(show_labels)) {
    log_stop(
      "Argument 'show_labels' must be NULL, TRUE/FALSE, or a character vector of patient IDs"
    )
  }

  if (is.logical(show_labels) &&
      length(show_labels) != 1) {
    log_stop(
      "Argument 'show_labels' must be a single logical value"
    )
  }

  if (is.character(show_labels) &&
      length(show_labels) == 0) {
    log_stop(
      "Argument 'show_labels' character vector must not be empty"
    )
  }

  # Cross-validations

  n_patients_check <- nrow(data)

  if (!is.null(facet_by) &&
      isTRUE(show_labels)) {
    log_warning(
      "Combining 'facet_by' with 'show_labels = TRUE' may produce crowded Y axes. ",
      "Consider passing a subset of IDs to 'show_labels'."
    )
  }

  if (is.null(facet_by) &&
      isTRUE(show_labels) &&
      n_patients_check > 50) {
    log_warning(
      "Showing labels for ",
      n_patients_check,
      " patients may be unreadable. ",
      "Consider subsetting via 'show_labels'."
    )
  }

  if (!is.null(time_cutoff)) {
    max_time_check <- suppressWarnings(
      max(data[[time_col]], na.rm = TRUE)
    )

    if (is.finite(max_time_check) &&
        time_cutoff > max_time_check) {
      log_warning(
        "'time_cutoff' (",
        time_cutoff,
        ") exceeds the maximum observed follow-up time (",
        round(max_time_check, 2),
        ")."
      )
    }
  }

  # Prepares data

  temp_df <- data[
    !is.na(data[[os_col]]) &
      !is.na(data[[time_col]]) &
      !is.na(data[[score_col]]),
    ,
    drop = FALSE
  ]

  if (nrow(temp_df) == 0) {
    log_stop(
      "No valid rows available after removing NA values."
    )
  }

  os_vals <- unique(
    stats::na.omit(temp_df[[os_col]])
  )

  if (!all(os_vals %in% c(0, 1))) {
    log_stop(
      paste0(
        "Column '",
        os_col,
        "' must contain only 0 (Alive) and 1 (Dead) values."
      )
    )
  }

  if (any(temp_df[[time_col]] < 0, na.rm = TRUE)) {
    log_stop(
      paste0(
        "Column '",
        time_col,
        "' contains negative values."
      )
    )
  }

  # Constructs patient IDs

  if (!is.null(id_col)) {
    temp_df$.patient_id <- as.character(
      temp_df[[id_col]]
    )
  } else if (
             !is.null(rownames(data)) &&
               any(
                   rownames(data) !=
                     as.character(seq_len(nrow(data)))
               )
             ) {
    temp_df$.patient_id <- rownames(temp_df)
  } else {
    temp_df$.patient_id <- as.character(
      seq_len(nrow(temp_df))
    )
  }

  if (anyDuplicated(temp_df$.patient_id)) {
    log_warning(
      "Duplicated patient IDs detected. Appending row index to make them unique."
    )

    temp_df$.patient_id <- paste0(
      temp_df$.patient_id,
      "_",
      seq_len(nrow(temp_df))
    )
  }

  # Defines Low/High group based on cutoff and score_col

  temp_df$.score_group <- ifelse(
    temp_df[[score_col]] > cutoff,
    "High",
    "Low"
  )

  temp_df$.score_group <- factor(
    temp_df$.score_group,
    levels = c("Low", "High")
  )

  if (length(unique(temp_df$.score_group)) < 2) {
    log_warning(
      "All patients fall in the same score group given the provided 'cutoff'. ",
      "Only one color will appear."
    )
  }

  temp_df$.status <- factor(
    temp_df[[os_col]],
    levels = c(0, 1),
    labels = c("Alive", "Dead")
  )

  temp_df$.time <- temp_df[[time_col]]

  # Ordering of patients on the Y axis

  if (sort_by == "time") {
    ord <- order(
      temp_df$.time,
      decreasing = FALSE
    )
  } else if (sort_by == "score") {
    ord <- order(
      temp_df[[score_col]],
      decreasing = FALSE
    )
  } else {
    ord <- seq_len(nrow(temp_df))
  }

  temp_df <- temp_df[
    ord,
    ,
    drop = FALSE
  ]

  temp_df$.patient_factor <- factor(
    temp_df$.patient_id,
    levels = temp_df$.patient_id
  )

  # Determines which labels to show

  all_levels <- levels(
    temp_df$.patient_factor
  )

  label_levels <- stats::setNames(
    rep("", length(all_levels)),
    all_levels
  )

  if (is.null(show_labels) ||
      (is.logical(show_labels) &&
       isFALSE(show_labels))) {
    show_labels_flag <- FALSE

  } else if (
             is.logical(show_labels) &&
               isTRUE(show_labels)
             ) {
    label_levels[] <- all_levels
    show_labels_flag <- TRUE

  } else if (is.character(show_labels)) {
    valid_ids <- intersect(
      show_labels,
      all_levels
    )

    if (length(valid_ids) == 0) {
      log_warning(
        "None of the IDs in 'show_labels' matched patient IDs. ",
        "No labels will be shown."
      )

      show_labels_flag <- FALSE

    } else {
      if (length(valid_ids) < length(show_labels)) {
        unmatched <- setdiff(
          show_labels,
          valid_ids
        )

        log_warning(
          "Some IDs in 'show_labels' did not match any patient: ",
          paste(unmatched, collapse = ", ")
        )
      }

      label_levels[valid_ids] <- valid_ids
      show_labels_flag <- TRUE
    }

  } else {
    show_labels_flag <- FALSE
  }

  # Maps

  color_map <- c(
    "Low" = palette[1],
    "High" = palette[2]
  )

  shape_map <- c(
    "Alive" = alive_shape,
    "Dead" = dead_shape
  )

  status_color_map <- c(
    "Alive" = alive_color,
    "Dead" = dead_color
  )

  # Builds the plot

  half_h <- bar_height / 2

  p <- ggplot2::ggplot(
    temp_df,
    ggplot2::aes(
      xmin = 0,
      xmax = .data$.time,
      ymin = as.numeric(.data$.patient_factor) - half_h,
      ymax = as.numeric(.data$.patient_factor) + half_h,
      fill = .data$.score_group
    )
  ) +
    ggplot2::geom_rect(
      color = NA
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = .data$.time,
        y = as.numeric(.data$.patient_factor),
        color = .data$.status,
        shape = .data$.status
      ),
      size = point_size,
      stroke = 0.3
    ) +
    ggplot2::scale_fill_manual(
      values = color_map,
      name = legend_title,
      labels = c(
        "Low" = paste0(
          "Low (<= ",
          round(cutoff, 4),
          ")"
        ),
        "High" = paste0(
          "High (> ",
          round(cutoff, 4),
          ")"
        )
      )
    ) +
    ggplot2::scale_color_manual(
      values = status_color_map,
      name = "Status",
      labels = c(
        "Alive" = "Alive / Censored",
        "Dead" = "Dead / Event"
      )
    ) +
    ggplot2::scale_shape_manual(
      values = shape_map,
      name = "Status",
      labels = c(
        "Alive" = "Alive / Censored",
        "Dead" = "Dead / Event"
      )
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(
        mult = c(0.01, 0.03)
      ),
      breaks = scales::pretty_breaks(n = 5)
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        override.aes = list(
          shape = 22,
          color = NA,
          size = 3,
          stroke = 0
        ),
        order = 1
      ),
      color = ggplot2::guide_legend(
        override.aes = list(
          shape = c(alive_shape, dead_shape),
          size = 3
        ),
        order = 2
      ),
      shape = "none"
    ) +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = title
    )

  # Y-axis: labels & breaks depend on facet presence

  if (is.null(facet_by)) {
    p <- p +
      ggplot2::scale_y_continuous(
        breaks = seq_along(all_levels),
        labels = label_levels,
        expand = ggplot2::expansion(mult = 0.01)
      )
  } else {
    y_label_fun <- function(pos) {
      idx <- round(pos)

      ifelse(
        idx >= 1 &
          idx <= length(all_levels),
        label_levels[idx],
        ""
      )
    }

    p <- p +
      ggplot2::scale_y_continuous(
        breaks = seq_along(all_levels),
        labels = y_label_fun,
        expand = ggplot2::expansion(mult = 0.01)
      )

  }

  if (!is.null(time_cutoff)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = time_cutoff,
        linetype = "dashed",
        color = "grey40",
        linewidth = 0.5
      )
  }

  if (!is.null(facet_by)) {
    p <- p +
      ggplot2::facet_grid(
        stats::as.formula(
          paste(facet_by, "~ .")
        ),
        scales = "free_y",
        space = "free_y"
      )
  }

  p <- p +
    theme +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(
        color = "grey90",
        linewidth = 0.4
      ),
      panel.border = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(
        size = 8,
        hjust = 1
      ),
      legend.position = if (show_legend) "top" else "none",
      legend.key.size = ggplot2::unit(0.4, "cm"),
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 8)
    )

  if (!show_labels_flag) {
    p <- p +
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      )
  }

  # Saves if requested

  if (!is.null(outprefix)) {
    if (!is.character(outprefix) ||
      length(outprefix) != 1) {
      log_stop(
        "Argument 'outprefix' must be a single character string"
      )
    }

    fname <- paste0(
      outprefix,
      "_swimmer_plot.pdf"
    )

    ggplot2::ggsave(
      filename = fname,
      plot = p,
      device = "pdf",
      width = width,
      height = height
    )
  }

  return(p)
}

#' Generate feature expression boxplots by prognostic group in a grid
#'
#' @description Creates a single-page PDF containing a grid of boxplots for
#' multiple genes/transcripts. Optionally, each panel can be faceted by an
#' external grouping variable. Individual per-feature PDFs can also be written
#' to disk.
#'
#' @param data Data.frame containing expression data and metadata columns.
#' @param signature A data.frame with \code{feature} and \code{coefficient}
#'   (default: \code{NULL}).
#' @param group_col Character. The name of the column used for grouping
#'   (default: \code{"score_group"}).
#' @param facet_by Character or \code{NULL} (default). Optional column name
#'   used to facet each panel (e.g. \code{"OS"}).
#' @param exclude_cols Character vector. Columns to exclude from the gene list
#'   (default: \code{c("OS", "OS.time", "score", "score_group")}).
#' @param palette Character vector. Colors for the \code{group_col} levels
#'   (default: \code{c("#D73027", "#1A9850")}).
#' @param test Character. Statistical test to compare expression between the
#'   two \code{group_col} levels: \code{"wilcox"} (Mann-Whitney, default),
#'   \code{"t.test"}, or \code{"none"}.
#' @param paired Logical. Whether to perform a paired test (default:
#'   \code{FALSE}). Ignored when \code{test = "none"}.
#' @param pvalue_position Character. Where to place the p-value:
#'   \code{"subtitle"} (default) or \code{"inside"}. When \code{facet_by} is
#'   set, \code{"inside"} draws the p-value of each facet level inside its
#'   own panel.
#' @param add_pvalue Logical. Whether to compute and display the p-value
#'   (default: \code{TRUE}).
#' @param show_points Logical. Whether to show jittered points over the
#'   boxplots (default: \code{FALSE}).
#' @param show_n Logical. Whether to print the sample size per group inside
#'   each panel, just above the x axis (default: \code{FALSE}).
#' @param max_features Numeric. Max features to plot (default: \code{NULL}).
#' @param norm_exp Logical. Whether to apply log2 normalization on expression
#'   data (default: \code{FALSE}).
#' @param outprefix Character. Output prefix for saving the plot (optional).
#' @param individual Logical. If \code{TRUE} and \code{outprefix} is provided,
#'   saves each feature boxplot as its own PDF file named
#'   \code{{outprefix}_feature_{feature}.pdf}. The combined grid is still
#'   saved as \code{{outprefix}_feature_expression.pdf} (default:
#'   \code{FALSE}).
#' @param theme A ggplot2 theme object. Defaults to \code{theme_clinprog()}.
#' @param ylab Character. Title for the Y-axis (default: \code{NULL}).
#' @param xlab Character. Title for the X-axis (default: \code{"Score"}).
#' @param width Numeric. Plot width in inches (default: \code{NULL}).
#' @param height Numeric. Plot height in inches (default: \code{NULL}).
#'
#' @return A \code{ggplot} object.
#' @importFrom rlang .data
#' @importFrom dplyr %>%
#' @export
plot_boxplot <- function(
  data,
  signature = NULL,
  group_col = "score_group",
  facet_by = NULL,
  outprefix = NULL,
  theme = theme_clinprog(),
  exclude_cols = c("OS", "OS.time", "score", "score_group"),
  palette = c("#D73027", "#1A9850"),
  test = c("wilcox", "t.test", "none"),
  paired = FALSE,
  pvalue_position = c("subtitle", "inside"),
  add_pvalue = TRUE,
  show_points = FALSE,
  show_n = FALSE,
  max_features = NULL,
  norm_exp = FALSE,
  individual = FALSE,
  width = NULL,
  height = NULL,
  ylab = NULL,
  xlab = "Score"
)
{
  log_message("Building feature expression boxplots...")

  # Resolves multiple-choice arguments
  test <- match.arg(test)
  pvalue_position <- match.arg(pvalue_position)

  # Validates input data
  if (!is.data.frame(data)) {
    log_stop("'data' must be a data.frame")
  }

  if (!is.character(group_col) || length(group_col) != 1) {
    log_stop("'group_col' must be a single character string")
  }

  if (!group_col %in% colnames(data)) {
    log_stop(
      paste0(
        "'data' must contain the grouping column: '",
        group_col,
        "'"
      )
    )
  }

  # Validates signature if provided
  if (!is.null(signature))
  {
    if (!is.data.frame(signature)) {
      log_stop("'signature' must be a data.frame")
    }

    if (!all(c("feature", "coefficient") %in% colnames(signature)))
    {
      log_stop(
        "'signature' must contain 'feature' and 'coefficient' columns"
      )
    }

    if (!all(signature$feature %in% colnames(data)))
    {
      log_stop(
        "All features in 'signature' must be present in 'data' column names"
      )
    }
  }

  # Validates excluded columns
  if (!is.character(exclude_cols)) {
    log_stop("'exclude_cols' must be a character vector")
  }

  # Validates facet_by
  if (!is.null(facet_by))
  {
    if (!is.character(facet_by) || length(facet_by) != 1)
    {
      log_stop(
        "'facet_by' must be NULL or a single character string"
      )
    }

    if (!facet_by %in% colnames(data)) {
      log_stop(
        paste0(
          "'facet_by' column '",
          facet_by,
          "' not found in 'data'."
        )
      )
    }
  }

  # Validates colors
  if (!is.null(palette) && !is.character(palette))
  {
    log_stop(
      "'palette' must be a character vector of valid color names/codes or NULL"
    )
  }

  if (!is.null(palette) && anyNA(palette)) {
    log_stop("'palette' must not contain NA values")
  }

  # Validates p-value display
  if (!is.logical(add_pvalue) ||
      length(add_pvalue) != 1 ||
      is.na(add_pvalue))
  {
    log_stop("'add_pvalue' must be a single logical value")
  }

  # Validates normalization expression
  if (!is.logical(norm_exp) ||
      length(norm_exp) != 1 ||
      is.na(norm_exp))
  {
    log_stop("'norm_exp' must be a single logical value")
  }

  # Validates flags
  if (!is.logical(paired) ||
      length(paired) != 1 ||
      is.na(paired))
  {
    log_stop("'paired' must be TRUE or FALSE")
  }

  if (!is.logical(individual) ||
      length(individual) != 1 ||
      is.na(individual))
  {
    log_stop("'individual' must be TRUE or FALSE")
  }

  if (!is.logical(show_n) ||
      length(show_n) != 1 ||
      is.na(show_n))
  {
    log_stop("'show_n' must be TRUE or FALSE")
  }

  # Validates theme
  if (!inherits(theme, "theme")) {
    log_stop("'theme' must be a valid ggplot2 theme object")
  }

  # Validates labels for the axis
  if (!is.null(ylab) &&
      (!is.character(ylab) || length(ylab) != 1))
  {
    log_stop("'ylab' must be a single character string or NULL")
  }

  if (!is.character(xlab) || length(xlab) != 1) {
    log_stop("'xlab' must be a single character string")
  }

  # Validates plot sizes
  if (!is.null(width) &&
      (!is.numeric(width) || length(width) != 1 || width <= 0))
  {
    log_stop(
      "'width' must be a single positive numeric value or NULL"
    )
  }

  if (!is.null(height) &&
      (!is.numeric(height) || length(height) != 1 || height <= 0))
  {
    log_stop(
      "'height' must be a single positive numeric value or NULL"
    )
  }

  # Validates logical point display
  if (!is.logical(show_points) ||
      length(show_points) != 1 ||
      is.na(show_points))
  {
    log_stop("'show_points' must be a single logical value")
  }

  # Validates max_features
  if (!is.null(max_features))
  {
    if (!is.numeric(max_features) ||
        length(max_features) != 1 ||
        max_features <= 0 ||
        max_features %% 1 != 0)
    {
      log_stop(
        "'max_features' must be a single positive integer or NULL"
      )
    }
  }

  # Validates outprefix
  if (!is.null(outprefix))
  {
    if (!is.character(outprefix) || length(outprefix) != 1)
    {
      log_stop(
        "'outprefix' must be NULL or a single character string"
      )
    }
  }

  # Cross-validation: paired only makes sense with a real test
  if (paired && test == "none")
  {
    log_warning(
      "'paired = TRUE' has no effect when 'test = \"none\"'. ",
      "Ignoring 'paired'."
    )
    paired <- FALSE
  }

  # Cross-validation: individual only makes sense with outprefix
  if (individual && is.null(outprefix))
  {
    log_warning(
      "'individual = TRUE' requires 'outprefix' to be provided. ",
      "Ignoring 'individual'."
    )
    individual <- FALSE
  }

  # Identifies gene columns (all columns except the excluded ones)
  gene_cols <- setdiff(colnames(data), exclude_cols)

  if (length(gene_cols) == 0) {
    log_stop(
      "No gene columns found after removing 'exclude_cols'. ",
      "Please check your data."
    )
  }

  # Handles max_features filtering and warnings
  if (!is.null(max_features))
  {
    if (length(gene_cols) > max_features)
    {
      if (!is.null(signature))
      {
        # Selects top max features based on absolute coefficient values
        log_message(
          paste0(
            "Selecting ",
            max_features,
            " features based on signature coefficients."
          )
        )

        gene_cols <- signature %>%
          dplyr::slice_max(
            order_by = abs(.data$coefficient),
            n = max_features,
            with_ties = FALSE
          ) %>%
          dplyr::pull(.data$feature)
      } else {
        # Selects first max features
        log_message(
          paste0(
            "Selecting first ",
            max_features,
            " features."
          )
        )

        gene_cols <- utils::head(gene_cols, max_features)
      }
    } else {
      log_message(
        paste0(
          "Data does not contain more than ",
          max_features,
          " features (",
          length(gene_cols),
          "). Plotting all features."
        )
      )
    }
  } else {
    if (length(gene_cols) > 16)
    {
      log_warning(
        paste0(
          "Data contains ",
          length(gene_cols),
          " features. Plotting 16 features to avoid cluttered layouts."
        )
      )

      if (!is.null(signature))
      {
        # Selects top 16 based on absolute coefficient values
        log_message(
          "Selecting 16 features based on signature coefficients."
        )

        gene_cols <- signature %>%
          dplyr::slice_max(
            order_by = abs(.data$coefficient),
            n = 16,
            with_ties = FALSE
          ) %>%
          dplyr::pull(.data$feature)
      } else {
        # Selects first 16 max features
        log_message("Selecting first 16 features.")
        gene_cols <- utils::head(gene_cols, 16)
      }
    }
  }

  # Validates if selected gene columns are numeric
  non_numeric_genes <- gene_cols[
    !vapply(data[gene_cols], is.numeric, logical(1))
  ]

  if (length(non_numeric_genes) > 0)
  {
    log_warning(
      paste(
        "The following gene columns are not numeric and will be skipped:",
        paste(non_numeric_genes, collapse = ", ")
      )
    )

    gene_cols <- setdiff(gene_cols, non_numeric_genes)
  }

  if (length(gene_cols) == 0) {
    log_stop("No valid numeric gene columns remaining to plot.")
  }

  # Ensures grouping column is a factor for proper categorical plotting
  data[[group_col]] <- as.factor(data[[group_col]])
  group_levels <- levels(data[[group_col]])
  n_groups <- length(group_levels)

  # Checks color length if provided
  if (!is.null(palette) &&
      is.null(names(palette)) &&
      length(palette) < n_groups)
  {
    log_stop(
      paste0(
        "'palette' must contain at least ",
        n_groups,
        " colors for the groups in '",
        group_col,
        "'"
      )
    )
  }

  # Defines Y-axis label if NULL
  if (is.null(ylab)) {
    ylab <- if (norm_exp) "log2(Expression + 1)" else "Expression"
  }

  # Facet variable levels and friendly labels
  facet_levels <- NULL
  facet_labels <- NULL

  if (!is.null(facet_by))
  {
    raw_facet_vals <- stats::na.omit(as.character(data[[facet_by]]))

    if (length(raw_facet_vals) < 1)
    {
      log_stop(
        paste0(
          "'facet_by' column '",
          facet_by,
          "' has no valid values."
        )
      )
    }

    facet_levels <- unique(raw_facet_vals)

    # Friendly labels for OS status (0 = Alive, 1 = Dead)
    if (identical(facet_by, "OS"))
    {
      numeric_vals <- suppressWarnings(
        as.numeric(raw_facet_vals)
      )

      if (!anyNA(numeric_vals) &&
          all(numeric_vals %in% c(0, 1)))
      {
        facet_levels <- c("0", "1")
        facet_labels <- c(
          "0" = "Alive",
          "1" = "Dead"
        )
      }
    }
  }

  # Final facet labels (used for diagnostics)
  facet_final_levels <- if (!is.null(facet_labels)) {
    unname(facet_labels[facet_levels])
  } else {
    facet_levels
  }

  # List to store the generated plots
  plot_list <- list()

  # Makes plot for each feature
  for (gene in gene_cols)
  {
    # Creates a temporary data.frame for ggplot2 to avoid CRAN notes
    # on tidy evaluation
    if (norm_exp)
    {
      # Applies log2 normalization on expression data if requested
      temp_df <- data.frame(
        Group = data[[group_col]],
        Expression = log2(data[[gene]] + 1),
        stringsAsFactors = FALSE
      )
    } else {
      # No log2 normalization on expression data
      temp_df <- data.frame(
        Group = data[[group_col]],
        Expression = data[[gene]],
        stringsAsFactors = FALSE
      )
    }

    if (!is.null(facet_by)) {
      temp_df$Facet <- as.character(data[[facet_by]])
    }

    # Removes missing values
    keep <- !is.na(temp_df$Expression) &
      !is.na(temp_df$Group)

    if (!is.null(facet_by)) {
      keep <- keep & !is.na(temp_df$Facet)
    }

    temp_df <- temp_df[keep, , drop = FALSE]

    if (nrow(temp_df) == 0)
    {
      log_warning(
        paste0(
          "Skipping '",
          gene,
          "': no complete observations."
        )
      )
      next
    }

    # Restores Group as factor with original level order
    temp_df$Group <- factor(
      temp_df$Group,
      levels = group_levels
    )

    # Ensures at least 2 groups exist after NA removal
    if (nlevels(droplevels(temp_df$Group)) < 2)
    {
      log_warning(
        paste0(
          "Skipping '",
          gene,
          "': less than 2 groups after removing NAs."
        )
      )
      next
    }

    # Optional faceting
    if (!is.null(facet_by))
    {
      temp_df$Facet <- factor(
        temp_df$Facet,
        levels = facet_levels,
        labels = facet_labels
      )

      # Compare against the FINAL labels (post-rename)
      missing_facets <- setdiff(
        facet_final_levels,
        unique(as.character(temp_df$Facet))
      )

      if (length(missing_facets) > 0)
      {
        log_warning(
          paste0(
            "Feature '",
            gene,
            "' has no observations for facet level(s): ",
            paste(missing_facets, collapse = ", ")
          )
        )
      }
    }

    # Statistical tests between the two group_col levels
    subtitle_text <- NULL
    inside_annot <- NULL

    if (add_pvalue && test != "none")
    {
      # Helper that runs the test on a subset and returns label + p-value
      run_test <- function(sub_df, label)
      {
        groups <- split(
          sub_df$Expression,
          sub_df$Group
        )

        if (length(groups) != 2 ||
            any(lengths(groups) < 2))
        {
          return(NULL)
        }

        use_paired <- paired

        if (use_paired &&
            length(groups[[1]]) != length(groups[[2]]))
        {
          log_warning(
            paste0(
              "'",
              label,
              "': 'paired = TRUE' but group sizes differ (",
              length(groups[[1]]),
              " vs ",
              length(groups[[2]]),
              "). Falling back to 'paired = FALSE'."
            )
          )

          use_paired <- FALSE
        }

        res <- tryCatch({
          if (test == "wilcox") {
            stats::wilcox.test(
              groups[[1]],
              groups[[2]],
              paired = use_paired
            )
          } else {
            stats::t.test(
              groups[[1]],
              groups[[2]],
              paired = use_paired
            )
          }
        }, error = function(e) NULL)

        if (is.null(res)) {
          return(NULL)
        }

        p_val <- res$p.value
        test_label <- if (test == "wilcox") {
          "MW"
        } else {
          "t-test"
        }

        if (use_paired) {
          test_label <- paste0(
            test_label,
            " (paired)"
          )
        }

        p_str <- if (is.na(p_val)) {
          "NA"
        } else if (p_val < 0.001) {
          "p < 0.001"
        } else {
          sprintf("p = %.3f", p_val)
        }

        list(
          label = test_label,
          p_str = p_str,
          full = paste0(
            test_label,
            " ",
            p_str
          )
        )
      }

      if (is.null(facet_by))
      {
        # Single test per feature
        test_res <- run_test(
          temp_df,
          gene
        )

        if (!is.null(test_res))
        {
          subtitle_text <- test_res$full

          if (pvalue_position == "inside")
          {
            inside_annot <- data.frame(
              x = 1.5,
              y = Inf,
              label = test_res$full,
              stringsAsFactors = FALSE
            )
          }
        } else {
          log_warning(
            paste0(
              "Skipping test for '",
              gene,
              "': at least one group has < 2 observations."
            )
          )
        }
      } else {
        # One test per facet level
        stat_texts <- character(0)
        annot_rows <- list()

        for (flev in facet_final_levels)
        {
          sub_df <- temp_df[
            as.character(temp_df$Facet) == flev,
            ,
            drop = FALSE
          ]

          if (nrow(sub_df) == 0) {
            next
          }

          test_res <- run_test(
            sub_df,
            paste0(gene, " | ", flev)
          )

          if (!is.null(test_res))
          {
            stat_texts <- c(
              stat_texts,
              test_res$full
            )

            annot_rows[[length(annot_rows) + 1]] <- data.frame(
              Facet = flev,
              x = 1.5,
              y = Inf,
              label = test_res$full,
              stringsAsFactors = FALSE
            )
          } else {
            log_warning(
              paste0(
                "Skipping test for '",
                gene,
                "' in facet '",
                flev,
                "': at least one group has < 2 observations."
              )
            )
          }
        }

        if (length(stat_texts) > 0) {
          subtitle_text <- paste(
            stat_texts,
            collapse = " | "
          )
        }

        if (length(annot_rows) > 0)
        {
          inside_annot <- do.call(
            rbind,
            annot_rows
          )

          inside_annot$Facet <- factor(
            inside_annot$Facet,
            levels = facet_final_levels
          )
        }
      }
    }

    # The X axis is ALWAYS the group itself
    temp_df$X_lab <- factor(
      temp_df$Group,
      levels = group_levels
    )

    # Prepares the annotation data.frame for sample sizes
    n_annot <- NULL

    if (show_n)
    {
      # 5% of the data range as offset below the minimum
      y_min_data <- min(
        temp_df$Expression,
        na.rm = TRUE
      )

      y_max_data <- max(
        temp_df$Expression,
        na.rm = TRUE
      )

      y_offset <- (
        y_max_data - y_min_data
      ) * 0.05

      y_n_pos <- y_min_data - y_offset

      if (is.null(facet_by))
      {
        n_per_group <- table(
          temp_df$Group
        )

        n_annot <- data.frame(
          X_lab = factor(
            names(n_per_group),
            levels = group_levels
          ),
          y = y_n_pos,
          label = paste0(
            "(n = ",
            as.integer(n_per_group),
            ")"
          ),
          stringsAsFactors = FALSE
        )
      } else {
        n_per_combo <- as.data.frame(
          table(
            Group = temp_df$Group,
            Facet = temp_df$Facet
          ),
          stringsAsFactors = FALSE
        )

        n_per_combo <- n_per_combo[
          n_per_combo$Freq > 0,
          ,
          drop = FALSE
        ]

        n_annot <- data.frame(
          X_lab = factor(
            n_per_combo$Group,
            levels = group_levels
          ),
          Facet = factor(
            n_per_combo$Facet,
            levels = facet_final_levels
          ),
          y = y_n_pos,
          label = paste0(
            "(n = ",
            as.integer(n_per_combo$Freq),
            ")"
          ),
          stringsAsFactors = FALSE
        )
      }
    }

    # Base plot
    if (!is.null(facet_by))
    {
      p <- ggplot2::ggplot(
        temp_df,
        ggplot2::aes(
          x = .data$X_lab,
          y = .data$Expression,
          fill = .data$Group
        )
      ) +
        ggplot2::geom_boxplot(
          outlier.shape = NA,
          linewidth = 0.5,
          alpha = 0.7,
          color = "black"
        ) +
        ggplot2::facet_wrap(
          ~ .data$Facet,
          scales = "free_x",
          drop = TRUE
        )
    } else {
      p <- ggplot2::ggplot(
        temp_df,
        ggplot2::aes(
          x = .data$X_lab,
          y = .data$Expression,
          fill = .data$Group
        )
      ) +
        ggplot2::geom_boxplot(
          outlier.shape = NA,
          linewidth = 0.5,
          alpha = 0.7,
          color = "black"
        )
    }

    # Applies custom colors if specified
    if (!is.null(palette)) {
      p <- p +
        ggplot2::scale_fill_manual(
          values = palette,
          guide = "none"
        )
    }

    # Adds jitter points to boxplots
    if (show_points)
    {
      p <- p +
        ggplot2::geom_jitter(
          width = 0.2,
          size = 1.2,
          alpha = 0.5,
          color = "black"
        )
    }

    # Adds N annotations inside the panel
    if (!is.null(n_annot))
    {
      p <- p +
        ggplot2::geom_text(
          data = n_annot,
          ggplot2::aes(
            x = .data$X_lab,
            y = .data$y,
            label = .data$label
          ),
          inherit.aes = FALSE,
          vjust = 1,
          size = 3
        )
    }

    # p-value inside panel
    if (add_pvalue &&
        test != "none" &&
        pvalue_position == "inside" &&
        !is.null(inside_annot))
    {
      p <- p +
        ggplot2::geom_text(
          data = inside_annot,
          ggplot2::aes(
            x = .data$x,
            y = .data$y,
            label = .data$label
          ),
          inherit.aes = FALSE,
          hjust = 0.5,
          vjust = 1.2,
          fontface = "italic",
          size = 3.5
        )
    }

    # Adds labs to plots
    p <- p +
      ggplot2::labs(
        x = xlab,
        y = ylab,
        title = gene,
        subtitle = if (
          !is.null(subtitle_text) &&
          pvalue_position == "subtitle"
        ) {
          subtitle_text
        } else {
          NULL
        }
      ) +
      theme +
      ggplot2::theme(
        legend.position = "none",
        axis.title = ggplot2::element_text(
          face = "plain",
          colour = "black",
          size = 12
        ),
        axis.text = ggplot2::element_text(
          face = "plain",
          colour = "black",
          size = 12
        ),
        plot.title = ggplot2::element_text(
          hjust = 0.5,
          face = "plain",
          size = 14
        ),
        plot.subtitle = ggplot2::element_text(
          hjust = 0.5,
          face = "italic",
          size = 10
        ),
        strip.background = ggplot2::element_rect(
          fill = "gray90",
          color = NA
        ),
        strip.text = ggplot2::element_text(
          face = "plain",
          size = 11
        ),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank()
      )

    # Adds new plot to list of feature plots
    plot_list[[gene]] <- p
  }

  if (length(plot_list) == 0) {
    log_stop("No valid feature plots could be generated.")
  }

  # Extracts number of plots
  n_plots <- length(plot_list)

  # Dynamically defines number of columns
  if (n_plots <= 2) {
    ncol_plot <- n_plots
  } else if (n_plots <= 4) {
    ncol_plot <- 2
  } else if (n_plots <= 9) {
    ncol_plot <- 3
  } else {
    ncol_plot <- 4
  }

  # Dynamically defines number of rows
  nrow_plot <- ceiling(
    n_plots / ncol_plot
  )

  # Automatically defines figure dimensions
  if (is.null(width)) {
    width <- 4 * ncol_plot
  }

  if (is.null(height)) {
    height <- 4 * nrow_plot
  }

  # Combines plots into a single patchwork object
  combined_plot <- patchwork::wrap_plots(
    plot_list,
    ncol = ncol_plot
  )

  # Saves the single-page grid PDF if an outprefix is provided
  if (!is.null(outprefix))
  {
    grDevices::pdf(
      file = paste0(
        outprefix,
        "_feature_expression.pdf"
      ),
      width = width,
      height = height
    )

    on.exit(
      grDevices::dev.off(),
      add = TRUE
    )

    print(combined_plot)

    # Saves each feature individually (optional)
    if (individual)
    {
      # Sanitizes feature names for safe file names
      safe_names <- gsub(
        "[^A-Za-z0-9_\\-]",
        "_",
        names(plot_list)
      )

      # Uses single-plot dimensions for individual PDFs
      ind_width <- if (!is.null(width)) {
        min(width, 6)
      } else {
        6
      }

      ind_height <- if (!is.null(height)) {
        min(height, 5)
      } else {
        5
      }

      for (i in seq_along(plot_list))
      {
        fname_i <- paste0(
          outprefix,
          "_feature_",
          safe_names[i],
          ".pdf"
        )

        ggplot2::ggsave(
          filename = fname_i,
          plot = plot_list[[i]],
          device = "pdf",
          width = ind_width,
          height = ind_height
        )
      }
    }
  }

  # Returns the combined plot object
  return(combined_plot)
}

#' Generate boxplots of prognostic score by clinical covariates
#'
#' @description Creates a single-page PDF containing a grid of boxplots, one per
#'   clinical covariate (typically dichotomized, e.g. Age: low/high,
#'   Race: white/non_white, Menopause: pre/post). Each boxplot compares the
#'   continuous \code{score} across the categories of a given covariate.
#'   Optionally, plots can be faceted by a grouping variable (e.g. \code{OS})
#'   and the score can be tested between the two covariate levels using a
#'   Mann-Whitney (Wilcoxon) or t-test.
#'
#' @param data Data.frame containing clinical information. Must contain the
#'   \code{score_col} and at least one clinical covariate column. Sample/patient
#'   IDs are taken from \code{id_col} if provided, otherwise from row names
#'   (when they are not the default integer sequence).
#' @param score_col Character. Name of the numeric score column
#'   (default: \code{"score"}).
#' @param exclude_cols Character vector. Columns to exclude from the covariate
#'   list (default: \code{c("OS", "OS.time", "score", "score_group")}).
#' @param facet_by Character or \code{NULL} (default). Optional column name
#'   used to facet the plot (e.g. \code{"OS"}). When set, boxplot colors
#'   follow the facet variable instead of the clinical covariate.
#' @param palette Character vector. Colors for the boxplots. When
#'   \code{facet_by = NULL}, the first two colors are used for the two
#'   covariate levels (default: \code{c("#1A9850", "#D73027")}). When
#'   faceting, colors are assigned to the facet levels.
#' @param test Character. Statistical test to compare score between the two
#'   covariate levels: \code{"wilcox"} (Mann-Whitney, default),
#'   \code{"t.test"}, or \code{"none"}.
#' @param paired Logical. Whether to perform a paired test (default:
#'   \code{FALSE}). Ignored when \code{test = "none"}.
#' @param pvalue_position Character. Where to place the p-value:
#'   \code{"subtitle"} (default) or \code{"inside"}. When \code{facet_by} is
#'   set, \code{"inside"} draws the p-value of each facet level inside its
#'   own panel.
#' @param add_pvalue Logical. Whether to compute and display the p-value
#'   (default: \code{TRUE}).
#' @param show_points Logical. Whether to overlay jittered points
#'   (default: \code{FALSE}).
#' @param show_n Logical. Whether to print the sample size per category inside
#'   each panel, just above the x axis. When \code{facet_by} is set, the N is
#'   computed within each facet level (default: \code{FALSE}).
#' @param order_by_median Logical. If \code{TRUE} (default), x axis categories
#'   are reordered by the median score.
#' @param max_features Integer or \code{NULL}. Maximum number of covariates to
#'   plot. If the data contains more, the first \code{max_features} (in column
#'   order) are kept. If \code{NULL} (default) and more than 16 covariates are
#'   present, only the first 16 are plotted with a warning.
#' @param id_col Character or \code{NULL}. Optional column with sample IDs. If
#'   \code{NULL} (default), row names are used when they are not the default
#'   integer sequence.
#' @param outprefix Character. Output prefix for saving the plot (optional).
#' @param individual Logical. If \code{TRUE} and \code{outprefix} is provided,
#'   saves each covariate boxplot as its own PDF file named
#'   \code{{outprefix}_clinical_{covariate}.pdf}. The combined grid is still
#'   saved as \code{{outprefix}_clinical_boxplots.pdf} (default:
#'   \code{FALSE}).
#' @param theme A ggplot2 theme object. Defaults to \code{theme_clinprog()}.
#' @param ylab Character. Title for the Y-axis (default: \code{"Score"}).
#' @param xlab Character. Title for the X-axis (default: \code{NULL}).
#' @param title Character or \code{NULL}. Optional overall plot title.
#' @param width Numeric. Plot width in inches (default: \code{NULL},
#'   auto-computed).
#' @param height Numeric. Plot height in inches (default: \code{NULL},
#'   auto-computed).
#'
#' @return A \code{patchwork} object containing the clinical covariate
#'   boxplots.
#' @importFrom rlang .data
#' @importFrom dplyr %>%
#' @export
plot_clinics <- function(
    data,
    score_col = "score",
    exclude_cols = c("OS", "OS.time", "score", "score_group"),
    facet_by = NULL,
    palette = c("#1A9850", "#D73027"),
    test = c("wilcox", "t.test", "none"),
    paired = FALSE,
    pvalue_position = c("subtitle", "inside"),
    add_pvalue = TRUE,
    show_points = FALSE,
    show_n = FALSE,
    order_by_median = TRUE,
    max_features = NULL,
    id_col = NULL,
    outprefix = NULL,
    individual = FALSE,
    theme = theme_clinprog(),
    ylab = "Score",
    xlab = NULL,
    title = NULL,
    width = NULL,
    height = NULL
) {

  log_message("Building clinical covariate boxplots...")

  # Resolves multiple-choice arguments
  test <- match.arg(test)
  pvalue_position <- match.arg(pvalue_position)

  # Input validation
  if (!is.data.frame(data)) {
    log_stop("'data' must be a data.frame")
  }

  # score_col
  if (!is.character(score_col) || length(score_col) != 1) {
    log_stop("'score_col' must be a single character string")
  }

  if (!score_col %in% colnames(data)) {
    log_stop(paste0("Column '", score_col, "' not found in 'data'."))
  }

  if (!is.numeric(data[[score_col]])) {
    log_stop(paste0("Column '", score_col, "' must be numeric."))
  }

  # exclude_cols
  if (!is.character(exclude_cols)) {
    log_stop("'exclude_cols' must be a character vector")
  }

  # facet_by
  if (!is.null(facet_by)) {
    if (!is.character(facet_by) || length(facet_by) != 1) {
      log_stop("'facet_by' must be NULL or a single character string")
    }

    if (!facet_by %in% colnames(data)) {
      log_stop(
        paste0(
          "'facet_by' column '",
          facet_by,
          "' not found in 'data'."
        )
      )
    }
  }

  # palette
  if (!is.character(palette) || length(palette) < 2) {
    log_stop(
      "'palette' must be a character vector with at least 2 colors"
    )
  }

  if (anyNA(palette)) {
    log_stop("'palette' must not contain NA values")
  }

  # flags
  if (!is.logical(add_pvalue) ||
      length(add_pvalue) != 1 ||
      is.na(add_pvalue)) {
    log_stop("'add_pvalue' must be TRUE or FALSE")
  }

  if (!is.logical(show_points) ||
      length(show_points) != 1 ||
      is.na(show_points)) {
    log_stop("'show_points' must be TRUE or FALSE")
  }

  if (!is.logical(show_n) ||
      length(show_n) != 1 ||
      is.na(show_n)) {
    log_stop("'show_n' must be TRUE or FALSE")
  }

  if (!is.logical(order_by_median) ||
      length(order_by_median) != 1 ||
      is.na(order_by_median)) {
    log_stop("'order_by_median' must be TRUE or FALSE")
  }

  if (!is.logical(paired) ||
      length(paired) != 1 ||
      is.na(paired)) {
    log_stop("'paired' must be TRUE or FALSE")
  }

  if (!is.logical(individual) ||
      length(individual) != 1 ||
      is.na(individual)) {
    log_stop("'individual' must be TRUE or FALSE")
  }

  # theme
  if (!inherits(theme, "theme")) {
    log_stop("'theme' must be a valid ggplot2 theme object")
  }

  # labels
  if (!is.null(ylab) &&
      (!is.character(ylab) || length(ylab) != 1)) {
    log_stop("'ylab' must be NULL or a single character string")
  }

  if (!is.null(xlab) &&
      (!is.character(xlab) || length(xlab) != 1)) {
    log_stop("'xlab' must be NULL or a single character string")
  }

  if (!is.null(title) &&
      (!is.character(title) || length(title) != 1)) {
    log_stop("'title' must be NULL or a single character string")
  }

  # plot sizes
  if (!is.null(width) &&
      (!is.numeric(width) || length(width) != 1 || width <= 0)) {
    log_stop("'width' must be NULL or a single positive numeric value")
  }

  if (!is.null(height) &&
      (!is.numeric(height) || length(height) != 1 || height <= 0)) {
    log_stop("'height' must be NULL or a single positive numeric value")
  }

  # max_features
  if (!is.null(max_features)) {
    if (!is.numeric(max_features) ||
        length(max_features) != 1 ||
        max_features <= 0 ||
        max_features %% 1 != 0) {
      log_stop(
        "'max_features' must be NULL or a single positive integer"
      )
    }
  }

  # id_col
  if (!is.null(id_col)) {
    if (!is.character(id_col) || length(id_col) != 1) {
      log_stop(
        "'id_col' must be NULL or a single character string"
      )
    }

    if (!id_col %in% colnames(data)) {
      log_stop(
        paste0(
          "'id_col' column '",
          id_col,
          "' not found in 'data'."
        )
      )
    }
  }

  # outprefix
  if (!is.null(outprefix)) {
    if (!is.character(outprefix) || length(outprefix) != 1) {
      log_stop(
        "'outprefix' must be NULL or a single character string"
      )
    }
  }

  # Cross-validation: paired only makes sense with a real test
  if (paired && test == "none") {
    log_warning(
      "'paired = TRUE' has no effect when 'test = \"none\"'. ",
      "Ignoring 'paired'."
    )
    paired <- FALSE
  }

  # Cross-validation: individual only makes sense with outprefix
  if (individual && is.null(outprefix)) {
    log_warning(
      "'individual = TRUE' requires 'outprefix' to be provided. ",
      "Ignoring 'individual'."
    )
    individual <- FALSE
  }

  # Ensures the score column is never treated as a covariate
  exclude_cols <- unique(c(exclude_cols, score_col))

  if (!is.null(facet_by)) {
    exclude_cols <- unique(c(exclude_cols, facet_by))
  }

  if (!is.null(id_col)) {
    exclude_cols <- unique(c(exclude_cols, id_col))
  }

  covariate_cols <- setdiff(colnames(data), exclude_cols)

  if (length(covariate_cols) == 0) {
    log_stop(
      "No covariate columns found after removing 'exclude_cols'. ",
      "Please check your data."
    )
  }

  # Drops numeric covariates
  is_categorical <- vapply(
    data[covariate_cols],
    function(x) {
      is.factor(x) || is.character(x) || is.logical(x)
    },
    logical(1)
  )

  non_categorical <- covariate_cols[!is_categorical]

  if (length(non_categorical) > 0) {
    log_warning(
      "The following covariates are not categorical and will be skipped: ",
      paste(non_categorical, collapse = ", ")
    )

    covariate_cols <- covariate_cols[is_categorical]
  }

  if (length(covariate_cols) == 0) {
    log_stop(
      "No valid categorical covariate columns remain to plot."
    )
  }

  # Enforces dichotomization (2 levels)
  n_levels <- vapply(
    data[covariate_cols],
    function(x) {
      length(unique(stats::na.omit(x)))
    },
    integer(1)
  )

  bad_covs <- covariate_cols[n_levels != 2]

  if (length(bad_covs) > 0) {
    log_warning(
      "The following covariates do not have exactly 2 levels and ",
      "will be skipped: ",
      paste(
        sprintf(
          "%s (%d levels)",
          bad_covs,
          n_levels[bad_covs]
        ),
        collapse = ", "
      )
    )

    covariate_cols <- covariate_cols[n_levels == 2]
  }

  if (length(covariate_cols) == 0) {
    log_stop(
      "No dichotomized covariates remain after filtering."
    )
  }

  # Handles max_features
  if (!is.null(max_features)) {
    if (length(covariate_cols) > max_features) {
      log_message(
        paste0(
          "Selecting first ",
          max_features,
          " covariates."
        )
      )

      covariate_cols <- utils::head(
        covariate_cols,
        max_features
      )
    }
  } else if (length(covariate_cols) > 16) {
    log_warning(
      paste0(
        "Data contains ",
        length(covariate_cols),
        " covariates. Plotting the first 16 to avoid cluttered layouts."
      )
    )

    covariate_cols <- utils::head(covariate_cols, 16)
  }

  # Builds patient IDs
  if (!is.null(id_col)) {
    patient_ids <- as.character(data[[id_col]])
  } else if (
    !is.null(rownames(data)) &&
    any(rownames(data) != as.character(seq_len(nrow(data))))
  ) {
    patient_ids <- rownames(data)
  } else {
    patient_ids <- as.character(seq_len(nrow(data)))
  }

  # Facet variable levels
  facet_levels <- NULL
  facet_labels <- NULL

  if (!is.null(facet_by)) {
    raw_facet_vals <- stats::na.omit(
      as.character(data[[facet_by]])
    )

    if (length(raw_facet_vals) < 1) {
      log_stop(
        paste0(
          "'facet_by' column '",
          facet_by,
          "' has no valid values."
        )
      )
    }

    facet_levels <- unique(raw_facet_vals)

    # Friendly labels for OS status (0 = Alive, 1 = Dead)
    if (identical(facet_by, "OS")) {
      numeric_vals <- suppressWarnings(
        as.numeric(raw_facet_vals)
      )

      if (!anyNA(numeric_vals) &&
          all(numeric_vals %in% c(0, 1))) {
        facet_levels <- c("0", "1")
        facet_labels <- c(
          "0" = "Alive",
          "1" = "Dead"
        )
      }
    }

    if (length(facet_levels) > length(palette)) {
      log_stop(
        paste0(
          "'palette' must contain at least ",
          length(facet_levels),
          " colors to color the '",
          facet_by,
          "' levels."
        )
      )
    }
  }

  # Final facet labels
  facet_final_levels <- if (!is.null(facet_labels)) {
    unname(facet_labels[facet_levels])
  } else {
    facet_levels
  }

  # Color map keyed by final facet labels
  if (!is.null(facet_by)) {
    key_levels <- if (!is.null(facet_labels)) {
      unname(facet_labels[facet_levels])
    } else {
      facet_levels
    }

    color_map <- stats::setNames(
      palette[seq_along(key_levels)],
      key_levels
    )
  }

  # Builds one boxplot per covariate
  plot_list <- list()

  for (cov in covariate_cols) {

    # Temporary data.frame with only the needed columns
    temp_df <- data.frame(
      Score = data[[score_col]],
      Cov = as.character(data[[cov]]),
      Patient = patient_ids,
      stringsAsFactors = FALSE
    )

    if (!is.null(facet_by)) {
      temp_df$Facet <- as.character(data[[facet_by]])
    }

    # Removes NAs
    keep <- !is.na(temp_df$Score) &
      !is.na(temp_df$Cov)

    if (!is.null(facet_by)) {
      keep <- keep & !is.na(temp_df$Facet)
    }

    temp_df <- temp_df[keep, , drop = FALSE]

    if (nrow(temp_df) == 0) {
      log_warning(
        paste0(
          "Skipping '",
          cov,
          "': no complete observations."
        )
      )
      next
    }

    # Factorizes the covariate with sorted levels
    cov_levels <- sort(unique(temp_df$Cov))

    temp_df$Cov <- factor(
      temp_df$Cov,
      levels = cov_levels
    )

    # Optional: order categories by median score
    if (order_by_median && length(cov_levels) > 1) {
      med_order <- tapply(
        temp_df$Score,
        temp_df$Cov,
        stats::median,
        na.rm = TRUE
      )

      temp_df$Cov <- factor(
        temp_df$Cov,
        levels = names(sort(med_order))
      )
    }

    cov_levels <- levels(temp_df$Cov)

    # Optional faceting
    if (!is.null(facet_by)) {
      temp_df$Facet <- factor(
        temp_df$Facet,
        levels = facet_levels,
        labels = facet_labels
      )

      missing_facets <- setdiff(
        facet_final_levels,
        unique(as.character(temp_df$Facet))
      )

      if (length(missing_facets) > 0) {
        log_warning(
          paste0(
            "Covariate '",
            cov,
            "' has no observations for facet level(s): ",
            paste(missing_facets, collapse = ", ")
          )
        )
      }
    }

    # Ensures both categories are still present after NA removal
    if (nlevels(droplevels(temp_df$Cov)) < 2) {
      log_warning(
        paste0(
          "Skipping '",
          cov,
          "': less than 2 covariate levels after removing NAs."
        )
      )
      next
    }

    # Statistical tests between the two covariate levels
    subtitle_text <- NULL
    inside_annot <- NULL

    if (add_pvalue && test != "none") {

      run_test <- function(sub_df, label) {

        groups <- split(
          sub_df$Score,
          sub_df$Cov
        )

        if (length(groups) != 2 ||
            any(lengths(groups) < 2)) {
          return(NULL)
        }

        use_paired <- paired

        if (
          use_paired &&
          length(groups[[1]]) != length(groups[[2]])
        ) {
          log_warning(
            paste0(
              "'",
              label,
              "': 'paired = TRUE' but group sizes differ (",
              length(groups[[1]]),
              " vs ",
              length(groups[[2]]),
              "). Falling back to 'paired = FALSE'."
            )
          )

          use_paired <- FALSE
        }

        res <- tryCatch(
          {
            if (test == "wilcox") {
              stats::wilcox.test(
                groups[[1]],
                groups[[2]],
                paired = use_paired
              )
            } else {
              stats::t.test(
                groups[[1]],
                groups[[2]],
                paired = use_paired
              )
            }
          },
          error = function(e) NULL
        )

        if (is.null(res)) {
          return(NULL)
        }

        p_val <- res$p.value

        test_label <- if (test == "wilcox") {
          "MW"
        } else {
          "t-test"
        }

        if (use_paired) {
          test_label <- paste0(
            test_label,
            " (paired)"
          )
        }

        p_str <- if (is.na(p_val)) {
          "NA"
        } else if (p_val < 0.001) {
          "p < 0.001"
        } else {
          sprintf(
            "p = %.3f",
            p_val
          )
        }

        list(
          label = test_label,
          p_str = p_str,
          full = paste0(
            test_label,
            " ",
            p_str
          )
        )
      }

      if (is.null(facet_by)) {

        test_res <- run_test(
          temp_df,
          cov
        )

        if (!is.null(test_res)) {
          subtitle_text <- test_res$full

          if (pvalue_position == "inside") {
            inside_annot <- data.frame(
              x = 1.5,
              y = Inf,
              label = test_res$full,
              stringsAsFactors = FALSE
            )
          }
        } else {
          log_warning(
            paste0(
              "Skipping test for '",
              cov,
              "': at least one group has < 2 observations."
            )
          )
        }

      } else {

        # One test per facet level
        stat_texts <- character(0)
        annot_rows <- list()

        for (flev in facet_final_levels) {

          sub_df <- temp_df[
            as.character(temp_df$Facet) == flev,
            ,
            drop = FALSE
          ]

          if (nrow(sub_df) == 0) {
            next
          }

          test_res <- run_test(
            sub_df,
            paste0(cov, " | ", flev)
          )

          if (!is.null(test_res)) {

            stat_texts <- c(
              stat_texts,
              test_res$full
            )

            annot_rows[[length(annot_rows) + 1]] <-
              data.frame(
                Facet = flev,
                x = 1.5,
                y = Inf,
                label = test_res$full,
                stringsAsFactors = FALSE
              )

          } else {

            log_warning(
              paste0(
                "Skipping test for '",
                cov,
                "' in facet '",
                flev,
                "': at least one group has < 2 observations."
              )
            )
          }
        }

        if (length(stat_texts) > 0) {
          subtitle_text <- paste(
            stat_texts,
            collapse = " | "
          )
        }

        if (length(annot_rows) > 0) {

          inside_annot <- do.call(
            rbind,
            annot_rows
          )

          inside_annot$Facet <- factor(
            inside_annot$Facet,
            levels = facet_final_levels
          )
        }
      }
    }

    # N annotations
    n_annot <- NULL

    if (show_n) {

      y_min_data <- min(
        temp_df$Score,
        na.rm = TRUE
      )

      y_max_data <- max(
        temp_df$Score,
        na.rm = TRUE
      )

      y_offset <- (
        y_max_data - y_min_data
      ) * 0.05

      y_n_pos <- y_min_data - y_offset

      if (is.null(facet_by)) {

        n_per_cat <- table(temp_df$Cov)

        n_annot <- data.frame(
          Cov = factor(
            names(n_per_cat),
            levels = cov_levels
          ),
          y = y_n_pos,
          label = paste0(
            "(n = ",
            as.integer(n_per_cat),
            ")"
          ),
          stringsAsFactors = FALSE
        )

      } else {

        n_per_combo <- as.data.frame(
          table(
            Cov = temp_df$Cov,
            Facet = temp_df$Facet
          ),
          stringsAsFactors = FALSE
        )

        n_per_combo <- n_per_combo[
          n_per_combo$Freq > 0,
          ,
          drop = FALSE
        ]

        n_annot <- data.frame(
          Cov = factor(
            n_per_combo$Cov,
            levels = cov_levels
          ),
          Facet = factor(
            n_per_combo$Facet,
            levels = facet_final_levels
          ),
          y = y_n_pos,
          label = paste0(
            "(n = ",
            as.integer(n_per_combo$Freq),
            ")"
          ),
          stringsAsFactors = FALSE
        )
      }
    }

    # Base plot
    if (!is.null(facet_by)) {

      p <- ggplot2::ggplot(
        temp_df,
        ggplot2::aes(
          x = .data$Cov,
          y = .data$Score,
          fill = .data$Facet
        )
      ) +
        ggplot2::geom_boxplot(
          outlier.shape = NA,
          linewidth = 0.5,
          alpha = 0.7,
          color = "black"
        ) +
        ggplot2::scale_fill_manual(
          values = color_map
        ) +
        ggplot2::facet_grid(
          ~ .data$Facet,
          scales = "free_x"
        )

    } else {

      p <- ggplot2::ggplot(
        temp_df,
        ggplot2::aes(
          x = .data$Cov,
          y = .data$Score,
          fill = .data$Cov
        )
      ) +
        ggplot2::geom_boxplot(
          outlier.shape = NA,
          linewidth = 0.5,
          alpha = 0.7,
          color = "black"
        ) +
        ggplot2::scale_fill_manual(
          values = palette[
            seq_len(nlevels(temp_df$Cov))
          ],
          guide = "none"
        )
    }

    # Jittered points
    if (show_points) {
      p <- p +
        ggplot2::geom_jitter(
          width = 0.2,
          size = 1.2,
          alpha = 0.5,
          color = "black"
        )
    }

    # Adds N annotations
    if (!is.null(n_annot)) {
      p <- p +
        ggplot2::geom_text(
          data = n_annot,
          ggplot2::aes(
            x = .data$Cov,
            y = .data$y,
            label = .data$label
          ),
          inherit.aes = FALSE,
          vjust = 1,
          size = 3
        )
    }

    # p-value inside panel
    if (
      add_pvalue &&
      test != "none" &&
      pvalue_position == "inside" &&
      !is.null(inside_annot)
    ) {
      p <- p +
        ggplot2::geom_text(
          data = inside_annot,
          ggplot2::aes(
            x = .data$x,
            y = .data$y,
            label = .data$label
          ),
          inherit.aes = FALSE,
          hjust = 0.5,
          vjust = 1.2,
          fontface = "italic",
          size = 3.5
        )
    }

    # Titles and theme
    p <- p +
      ggplot2::labs(
        x = xlab,
        y = ylab,
        title = cov,
        subtitle =
          if (
            !is.null(subtitle_text) &&
            pvalue_position == "subtitle"
          ) {
            subtitle_text
          } else {
            NULL
          }
      ) +
      theme +
      ggplot2::theme(
        legend.position = "none",
        axis.title = ggplot2::element_text(
          face = "plain",
          colour = "black",
          size = 12
        ),
        axis.text = ggplot2::element_text(
          face = "plain",
          colour = "black",
          size = 11
        ),
        plot.title = ggplot2::element_text(
          hjust = 0.5,
          face = "plain",
          size = 14
        ),
        plot.subtitle = ggplot2::element_text(
          hjust = 0.5,
          face = "italic",
          size = 10
        ),
        strip.background = ggplot2::element_rect(
          fill = "gray90",
          color = NA
        ),
        strip.text = ggplot2::element_text(
          face = "plain",
          size = 11
        ),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank()
      )

    plot_list[[cov]] <- p
  }

  if (length(plot_list) == 0) {
    log_stop(
      "No valid covariate plots could be generated."
    )
  }

  # Combines and optionally saves
  n_plots <- length(plot_list)

  if (n_plots <= 2) {
    ncol_plot <- n_plots
  } else if (n_plots <= 4) {
    ncol_plot <- 2
  } else if (n_plots <= 9) {
    ncol_plot <- 3
  } else {
    ncol_plot <- 4
  }

  nrow_plot <- ceiling(
    n_plots / ncol_plot
  )

  if (is.null(width)) {
    width <- 4 * ncol_plot
  }

  if (is.null(height)) {
    height <- 4 * nrow_plot
  }

  combined_plot <- patchwork::wrap_plots(
    plot_list,
    ncol = ncol_plot
  )

  if (!is.null(title)) {
    combined_plot <- combined_plot +
      patchwork::plot_annotation(
        title = title
      )
  }

  # Saves combined grid
  if (!is.null(outprefix)) {

    grDevices::pdf(
      file = paste0(
        outprefix,
        "_clinical_boxplots.pdf"
      ),
      width = width,
      height = height
    )

    on.exit(
      grDevices::dev.off(),
      add = TRUE
    )

    print(combined_plot)

    # Saves each covariate individually
    if (individual) {

      safe_names <- gsub(
        "[^A-Za-z0-9_\\-]",
        "_",
        names(plot_list)
      )

      ind_width <- if (!is.null(width)) {
        min(width, 6)
      } else {
        6
      }

      ind_height <- if (!is.null(height)) {
        min(height, 5)
      } else {
        5
      }

      for (i in seq_along(plot_list)) {

        fname_i <- paste0(
          outprefix,
          "_clinical_",
          safe_names[i],
          ".pdf"
        )

        ggplot2::ggsave(
          filename = fname_i,
          plot = plot_list[[i]],
          device = "pdf",
          width = ind_width,
          height = ind_height
        )
      }
    }
  }

  return(combined_plot)
}

#' Generate signature wordcloud weighted by expression and coefficient
#'
#' @description Creates a wordcloud of features present in a signature. The
#'   size of each name is proportional to the absolute product of its mean
#'   expression and coefficient. Colors indicate directionality (positive vs
#'   negative coefficients).
#'
#' @param data Data.frame containing sample expression data and metadata
#'   columns.
#' @param signature Data.frame containing columns \code{feature} and
#'   \code{coefficient}.
#' @param feature_col Character. Column name in \code{signature} for feature
#'   names (default: \code{"feature"}).
#' @param coef_col Character. Column name in \code{signature} for model
#'   coefficients (default: \code{"coefficient"}).
#' @param pos_color Character. Color for positive coefficients (default:
#'   \code{"#D73027"}).
#' @param neg_color Character. Color for negative coefficients (default:
#'   \code{"#1A9850"}).
#' @param outprefix Character. Output prefix for saving the plot (optional).
#' @param theme A ggplot2 theme object. Defaults to \code{theme_clinprog()}.
#' @param width Numeric. Plot width in inches (default: \code{NULL}).
#' @param height Numeric. Plot height in inches (default: \code{NULL}).
#' @param min_alpha Numeric. Minimum alpha transparency value between 0 and 1
#'   (default: \code{0.5}).
#' @param max_alpha Numeric. Maximum alpha transparency value between 0 and 1
#'   (default: \code{1.0}).
#' @param max_size Numeric. Maximum size parameter for wordcloud text size
#'   scaling (default: \code{NULL}).
#'
#' @return A \code{ggplot} object.
#' @importFrom rlang .data
#' @export
plot_wordcloud <- function(
    data,
    signature,
    feature_col = "feature",
    coef_col = "coefficient",
    pos_color = "#D73027",
    neg_color = "#1A9850",
    outprefix = NULL,
    theme = theme_clinprog(),
    width = NULL,
    height = NULL,
    max_size = NULL,
    min_alpha = 0.5,
    max_alpha = 1.0
) {
  log_message("Building signature wordcloud...")

  # Validate input data.frames
  if (!is.data.frame(data)) {
    log_stop("'data' must be a data.frame")
  }

  if (!is.data.frame(signature)) {
    log_stop("'signature' must be a data.frame")
  }

  # Validate signature column names
  if (!feature_col %in% colnames(signature)) {
    log_stop(
      paste0(
        "'signature' must contain column: '",
        feature_col,
        "'"
      )
    )
  }

  if (!coef_col %in% colnames(signature)) {
    log_stop(
      paste0(
        "'signature' must contain column: '",
        coef_col,
        "'"
      )
    )
  }

  # Validate theme
  if (!inherits(theme, "theme")) {
    log_stop("'theme' must be a valid ggplot2 theme object")
  }

  # Validate colors
  if (!is.character(pos_color) || length(pos_color) != 1) {
    log_stop("'pos_color' must be a single color string")
  }

  if (!is.character(neg_color) || length(neg_color) != 1) {
    log_stop("'neg_color' must be a single color string")
  }

  # Validate plot dimensions and sizes
  if (!is.null(width) &&
      (!is.numeric(width) || length(width) != 1 || width <= 0)) {
    log_stop(
      "'width' must be a single positive numeric value or NULL"
    )
  }

  if (!is.null(height) &&
      (!is.numeric(height) || length(height) != 1 || height <= 0)) {
    log_stop(
      "'height' must be a single positive numeric value or NULL"
    )
  }

  if (!is.null(max_size) &&
      (!is.numeric(max_size) || length(max_size) != 1 || max_size <= 0)) {
    log_stop(
      "'max_size' must be a single positive numeric value or NULL"
    )
  }

  # Validate alpha limits
  if (!is.numeric(min_alpha) ||
      length(min_alpha) != 1 ||
      min_alpha < 0 ||
      min_alpha > 1) {
    log_stop(
      "'min_alpha' must be a single numeric value between 0 and 1"
    )
  }

  if (!is.numeric(max_alpha) ||
      length(max_alpha) != 1 ||
      max_alpha < 0 ||
      max_alpha > 1) {
    log_stop(
      "'max_alpha' must be a single numeric value between 0 and 1"
    )
  }

  if (min_alpha > max_alpha) {
    log_stop("'min_alpha' cannot be greater than 'max_alpha'")
  }

  # Extract feature names and coefficients
  features <- as.character(signature[[feature_col]])
  coefs <- as.numeric(signature[[coef_col]])

  # Check matching features in data
  common_features <- intersect(features, colnames(data))

  if (length(common_features) == 0) {
    log_stop(
      "None of the signature features were found in 'data' column names."
    )
  }

  if (length(common_features) < length(features)) {
    missing <- setdiff(features, common_features)

    log_warning(
      paste0(
        length(missing),
        " signature feature(s) missing from data and skipped: ",
        paste(missing, collapse = ", ")
      )
    )
  }

  # Filter signature data to present features
  signature_subset <- signature[
    signature[[feature_col]] %in% common_features,
    ,
    drop = FALSE
  ]

  # Calculate mean expression per feature across samples
  mean_expr <- colMeans(
    data[, signature_subset[[feature_col]], drop = FALSE],
    na.rm = TRUE
  )

  # Build summary data.frame for plotting
  df_cloud <- data.frame(
    Gene = signature_subset[[feature_col]],
    Coef = signature_subset[[coef_col]],
    MeanExpr = mean_expr[signature_subset[[feature_col]]],
    stringsAsFactors = FALSE
  )

  # Calculate metric product and absolute weight for size
  df_cloud$WeightProduct <- df_cloud$MeanExpr * df_cloud$Coef
  df_cloud$AbsWeight <- abs(df_cloud$WeightProduct)
  df_cloud$Direction <- ifelse(
    df_cloud$Coef >= 0,
    "Positive",
    "Negative"
  )

  # Handle edge-case where all weights are 0 or NA
  if (all(is.na(df_cloud$AbsWeight)) ||
      all(df_cloud$AbsWeight == 0)) {
    log_stop(
      "All calculated feature weights are zero or NA. Cannot build wordcloud."
    )
  }

  # Dynamically define max_size and min_size based on feature count
  if (is.null(max_size)) {
    n_genes <- nrow(df_cloud)

    if (n_genes == 1) {
      max_size <- 16
      min_size <- 16
    } else if (n_genes <= 3) {
      max_size <- 22
      min_size <- 12
    } else if (n_genes <= 15) {
      max_size <- 24
      min_size <- 8
    } else {
      max_size <- pmin(
        pmax(48 / (n_genes^0.35), 6),
        20
      )
      min_size <- pmin(
        pmax(max_size * 0.25, 2),
        4
      )
    }
  } else {
    min_size <- pmax(max_size * 0.25, 2)
  }

  # Define default figure dimensions
  if (is.null(width)) {
    width <- 6
  }

  if (is.null(height)) {
    height <- 4
  }

  # Generate wordcloud
  final_plot <- ggplot2::ggplot(
    df_cloud,
    ggplot2::aes(
      label = .data$Gene,
      size = .data$AbsWeight,
      color = .data$Direction,
      alpha = .data$AbsWeight
    )
  ) +
    ggwordcloud::geom_text_wordcloud_area(
      rm_outside = FALSE,
      show.legend = TRUE,
      eccentricity = 0.95,
      grid_margin = 1,
      shape = "circle",
      grid_size = 4,
      key_glyph = ggplot2::draw_key_point
    ) +
    ggplot2::scale_size_continuous(
      range = c(min_size, max_size),
      guide = "none"
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Positive" = pos_color,
        "Negative" = neg_color
      ),
      name = "Feature direction"
    ) +
    ggplot2::scale_alpha_continuous(
      range = c(min_alpha, max_alpha),
      guide = "none"
    ) +
    theme +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.5
      ),
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.position = "right"
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(
          shape = 16,
          size = 4,
          alpha = 1
        )
      )
    )

  # Save plot to PDF if outprefix is provided
  if (!is.null(outprefix)) {
    if (!is.character(outprefix) || length(outprefix) != 1) {
      log_stop("'outprefix' must be a single character string")
    }

    grDevices::pdf(
      file = paste0(outprefix, "_signature_wordcloud.pdf"),
      width = width,
      height = height
    )

    on.exit(grDevices::dev.off(), add = TRUE)
    print(final_plot)
  }

  return(final_plot)
}

#' Generate score barplot by OS group
#'
#' @description Creates a barplot of patient scores grouped by overall
#'   survival status. Bars are colored according to whether the score is below
#'   or above the specified cutoff. A Pearson's chi-square test is used to
#'   assess the association between OS status and score group.
#'
#' @param data Data.frame containing clinical/survival data.
#' @param palette Character vector of length at least 2. Colors for below and
#'   above cutoff (default: \code{c("#1A9850", "#D73027")}).
#' @param outprefix Character. Output prefix for saving the plot (optional).
#' @param score_cutoff Numeric. Threshold used to define low/high score groups
#'   (default: \code{0}).
#' @param os_col Character. Column name for OS status (default: \code{"OS"}).
#' @param score_col Character. Column name for score values
#'   (default: \code{"score"}).
#' @param theme A ggplot2 theme object. Defaults to \code{theme_clinprog()}.
#' @param xlab Character. Title for the X-axis (default: \code{"Patients"}).
#' @param ylab Character. Title for the Y-axis (default: \code{"Score"}).
#' @param width Numeric. Plot width in inches (default: \code{8}).
#' @param height Numeric. Plot height in inches (default: \code{5}).
#'
#' @return A \code{ggplot} object.
#' @importFrom rlang .data
#' @export
plot_barplot_score <- function(
    data,
    palette = c("#1A9850", "#D73027"),
    outprefix = NULL,
    score_cutoff = 0,
    os_col = "OS",
    score_col = "score",
    theme = theme_clinprog(),
    xlab = "Patients",
    ylab = "Score",
    width = 8,
    height = 5
) {

  log_message("Building score barplot by OS status...")

  # Validate input
  if (!is.data.frame(data)) {
    log_stop("'data' must be a data.frame")
  }

  if (!os_col %in% colnames(data)) {
    log_stop(
      paste0("Column '", os_col, "' not found in data.")
    )
  }

  if (!score_col %in% colnames(data)) {
    log_stop(
      paste0("Column '", score_col, "' not found in data.")
    )
  }

  if (!is.numeric(data[[score_col]])) {
    log_stop(
      paste0("Column '", score_col, "' must be numeric.")
    )
  }

  # Validate theme
  if (!inherits(theme, "theme")) {
    log_stop("'theme' must be a valid ggplot2 theme object")
  }

  # Validate score cutoff
  if (!is.numeric(score_cutoff) || length(score_cutoff) != 1) {
    log_stop("'score_cutoff' must be a single numeric value.")
  }

  # Validate color palette
  if (length(palette) < 2) {
    log_stop("'palette' must contain at least 2 colors.")
  }

  # Validate plot dimensions
  if (!is.numeric(width) || length(width) != 1 || width <= 0) {
    log_stop("'width' must be a single positive numeric value")
  }

  if (!is.numeric(height) || length(height) != 1 || height <= 0) {
    log_stop("'height' must be a single positive numeric value")
  }

  # Validate plot axes
  if (!is.character(ylab) || length(ylab) != 1) {
    log_stop("'ylab' must be a single character string")
  }

  if (!is.character(xlab) || length(xlab) != 1) {
    log_stop("'xlab' must be a single character string")
  }

  # Prepare data
  temp_df <- data[
    !is.na(data[[os_col]]) & !is.na(data[[score_col]]),
    ,
    drop = FALSE
  ]

  # Format OS group labels
  temp_df$OS_Group <- factor(
    temp_df[[os_col]],
    levels = c(0, 1),
    labels = c("Alive", "Dead")
  )

  # Define status based on threshold
  temp_df$Color_Group <- ifelse(
    temp_df[[score_col]] >= score_cutoff,
    "High",
    "Low"
  )

  temp_df$Color_Group <- factor(
    temp_df$Color_Group,
    levels = c("Low", "High")
  )

  # Calculate score relative to cutoff
  temp_df$score_diff <- temp_df[[score_col]] - score_cutoff

  # Calculate Pearson's Chi-Square test
  contingency_tab <- table(
    temp_df$OS_Group,
    temp_df$Color_Group
  )

  chisq_res <- suppressWarnings(
    stats::chisq.test(contingency_tab)
  )

  pval <- chisq_res$p.value

  # Format p-value
  p_formatted <- if (pval < 0.001) {
    "p < 0.001"
  } else {
    paste0("p = ", round(pval, 3))
  }

  p_text <- paste0("Chi-Square ", p_formatted)

  # Manual color mapping
  color_map <- c(
    "Low" = palette[1],
    "High" = palette[2]
  )

  # Order samples within each OS group
  temp_df <- temp_df[
    order(temp_df$OS_Group, temp_df[[score_col]]),
    ,
    drop = FALSE
  ]

  temp_df$Sample_ID <- factor(seq_len(nrow(temp_df)))

  # Position annotation relative to current data limits
  max_y <- max(temp_df$score_diff, na.rm = TRUE)
  min_y <- min(temp_df$score_diff, na.rm = TRUE)

  y_pos <- max_y - (max_y - min_y) * 0.05

  annot_df <- data.frame(
    OS_Group = factor(
      "Alive",
      levels = levels(temp_df$OS_Group)
    ),
    Sample_ID = factor(
      5,
      levels = levels(temp_df$Sample_ID)
    ),
    score_diff = y_pos,
    label = p_text
  )

  # Create plot
  p <- ggplot2::ggplot(
    temp_df,
    ggplot2::aes(
      x = .data$Sample_ID,
      y = .data$score_diff,
      fill = .data$Color_Group
    )
  ) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::scale_fill_manual(
      values = color_map,
      name = "Score cutoff",
      labels = c("Low", "High")
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) x + round(score_cutoff, 4)
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "black",
      linewidth = 0.6
    ) +
    ggplot2::facet_grid(
      ~ OS_Group,
      scales = "free_x",
      space = "free_x"
    ) +
    ggplot2::labs(
      x = xlab,
      y = ylab
    ) +
    ggplot2::geom_text(
      data = annot_df,
      ggplot2::aes(
        x = .data$Sample_ID,
        y = .data$score_diff,
        label = .data$label
      ),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = 1,
      fontface = "italic",
      size = 4
    ) +
    theme +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(
        face = "plain",
        colour = "black",
        size = 14
      ),
      axis.text = ggplot2::element_text(
        face = "plain",
        colour = "black",
        size = 12
      ),
      legend.title = ggplot2::element_text(
        face = "plain",
        colour = "black",
        size = 12
      ),
      legend.text = ggplot2::element_text(
        face = "plain",
        colour = "black",
        size = 10
      ),
      panel.grid = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(
        fill = "gray90",
        color = NA
      ),
      strip.text = ggplot2::element_text(
        face = "plain",
        size = 12
      )
    )

  # Save PDF if outprefix is provided
  if (!is.null(outprefix)) {
    if (!is.character(outprefix) || length(outprefix) != 1) {
      log_stop("'outprefix' must be a single character string")
    }

    grDevices::pdf(
      file = paste0(outprefix, "_score_barplot.pdf"),
      width = width,
      height = height
    )

    on.exit(grDevices::dev.off(), add = TRUE)
    print(p)
  }

  return(p)
}

#' Generate line plots for score vs follow-up time by OS status
#'
#' @param data Data.frame containing clinical/survival data.
#' @param palette Character vector of length 2. Colors for below and above
#'   cutoff (Default: \code{c("#1A9850", "#D73027")}).
#' @param outprefix Character. Output prefix for saving the plot (optional).
#' @param score_cutoff Numeric. Threshold to define low/high score color scheme
#'   (Default: \code{0}).
#' @param os_col Character. Column name for OS status (Default: \code{"OS"}).
#' @param time_col Character. Column name for follow-up time
#'   (Default: \code{"OS.time"}).
#' @param score_col Character. Column name for score values
#'   (Default: \code{"score"}).
#' @param theme A ggplot2 theme object. Defaults to \code{theme_clinprog()}.
#' @param xlab Character. Title for the X-axis (Default: \code{"Followup"}).
#' @param ylab Character. Title for the Y-axis (Default: \code{"Score"}).
#' @param width Numeric. Plot width in inches (Default: \code{12}).
#' @param height Numeric. Plot height in inches (Default: \code{6}).
#'
#' @return A \code{ggplot} object.
#' @importFrom rlang .data
#' @export
plot_lineplot_score <- function(
    data,
    palette = c("#1A9850", "#D73027"),
    outprefix = NULL,
    score_cutoff = 0,
    os_col = "OS",
    time_col = "OS.time",
    score_col = "score",
    theme = theme_clinprog(),
    xlab = "Followup",
    ylab = "Score",
    width = 12,
    height = 6
) {

  log_message("Building score vs follow-up time line plots by OS status...")

  # Validate input
  if (!is.data.frame(data)) {
    log_stop("'data' must be a data.frame")
  }

  if (!os_col %in% colnames(data)) {
    log_stop(paste0("Column '", os_col, "' not found in data."))
  }

  if (!time_col %in% colnames(data)) {
    log_stop(paste0("Column '", time_col, "' not found in data."))
  }

  if (!score_col %in% colnames(data)) {
    log_stop(paste0("Column '", score_col, "' not found in data."))
  }

  if (!is.numeric(data[[score_col]])) {
    log_stop(paste0("Column '", score_col, "' must be numeric."))
  }

  if (!is.numeric(data[[time_col]])) {
    log_stop(paste0("Column '", time_col, "' must be numeric."))
  }

  # Validate theme
  if (!inherits(theme, "theme")) {
    log_stop("'theme' must be a valid ggplot2 theme object")
  }

  # Validate score cutoff
  if (!is.numeric(score_cutoff) || length(score_cutoff) != 1) {
    log_stop("'score_cutoff' must be a single numeric value.")
  }

  # Validate color palette
  if (length(palette) < 2) {
    log_stop("'palette' must contain at least 2 colors.")
  }

  # Validate plot dimensions
  if (!is.numeric(width) || length(width) != 1 || width <= 0) {
    log_stop("'width' must be a single positive numeric value")
  }

  if (!is.numeric(height) || length(height) != 1 || height <= 0) {
    log_stop("'height' must be a single positive numeric value")
  }

  # Validate plot axes
  if (!is.character(ylab) || length(ylab) != 1) {
    log_stop("'ylab' must be a single character string")
  }

  if (!is.character(xlab) || length(xlab) != 1) {
    log_stop("'xlab' must be a single character string")
  }

  # Prepare data
  temp_df <- data[
    !is.na(data[[os_col]]) &
      !is.na(data[[score_col]]) &
      !is.na(data[[time_col]]),
    ,
    drop = FALSE
  ]

  # Format OS group labels (0 = Alive, 1 = Dead)
  temp_df$OS_Group <- factor(
    temp_df[[os_col]],
    levels = c(0, 1),
    labels = c("Alive", "Dead")
  )

  # Define status based on threshold for point colors
  temp_df$Color_Group <- ifelse(
    temp_df[[score_col]] >= score_cutoff,
    "High",
    "Low"
  )

  temp_df$Color_Group <- factor(
    temp_df$Color_Group,
    levels = c("Low", "High")
  )

  # Calculate score relative to cutoff
  temp_df$score_diff <- temp_df[[score_col]] - score_cutoff

  # Set names for manual color mapping
  color_map <- c(
    "Low" = palette[1],
    "High" = palette[2]
  )

  # Order data by follow-up time
  temp_df <- temp_df[
    order(temp_df$OS_Group, temp_df[[time_col]]),
    ,
    drop = FALSE
  ]

  # Create line plot
  max_time <- max(
    round(temp_df[[time_col]], 0),
    na.rm = TRUE
  )

  p <- ggplot2::ggplot(
    temp_df,
    ggplot2::aes(
      x = .data[[time_col]],
      y = .data$score_diff
    )
  ) +
    ggplot2::geom_line(
      color = "grey50",
      alpha = 0.7,
      linewidth = 0.3
    ) +
    ggplot2::geom_point(
      ggplot2::aes(color = .data$Color_Group),
      size = 1.5,
      alpha = 0.7
    ) +
    ggplot2::scale_color_manual(
      values = color_map,
      name = "Score cutoff",
      labels = c("Low", "High")
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, max_time),
      breaks = round(seq(0, max_time, length.out = 5))
    ) +
    ggplot2::scale_y_continuous(
      labels = function(y) y + round(score_cutoff, 4)
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "black",
      linewidth = 0.5
    ) +
    ggplot2::facet_grid(
      ~ OS_Group,
      scales = "free_x"
    ) +
    ggplot2::labs(
      x = xlab,
      y = ylab
    ) +
    theme +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(
        face = "plain",
        colour = "black",
        size = 14
      ),
      axis.text = ggplot2::element_text(
        face = "plain",
        colour = "black",
        size = 12
      ),
      legend.title = ggplot2::element_text(
        face = "plain",
        colour = "black",
        size = 12
      ),
      legend.text = ggplot2::element_text(
        face = "plain",
        colour = "black",
        size = 10
      ),
      strip.background = ggplot2::element_rect(
        fill = "gray90",
        color = NA
      ),
      strip.text = ggplot2::element_text(
        face = "plain",
        size = 12
      )
    )

  # Save PDF if outprefix is passed
  if (!is.null(outprefix)) {
    if (!is.character(outprefix) || length(outprefix) != 1) {
      log_stop("'outprefix' must be a single character string")
    }

    grDevices::pdf(
      file = paste0(outprefix, "_lineplot_score_followup.pdf"),
      width = width,
      height = height
    )

    on.exit(grDevices::dev.off(), add = TRUE)

    print(p)
  }

  return(p)
}

#' Generate gene/transcript expression scatter plots vs follow-up time in a grid
#'
#' @description Creates a single-page PDF containing a grid of scatter plots
#'   for multiple features showing expression vs follow-up time, faceted by OS
#'   status. Spearman correlation coefficients and p-values are automatically
#'   calculated and displayed.
#'
#' @param data Data.frame containing expression and clinical/survival data.
#' @param signature A data.frame with `feature` and `coefficient`
#'   (Default: `NULL`).
#' @param os_col Character. Column name for OS status (Default: `"OS"`).
#' @param time_col Character. Column name for follow-up time
#'   (Default: `"OS.time"`).
#' @param outprefix Character. Output prefix for saving the plot (optional).
#' @param theme A ggplot2 theme object. Defaults to `theme_clinprog()`.
#' @param exclude_cols Character vector. Columns to exclude from the feature
#'   list (Default: `c("OS", "OS.time", "score", "score_group")`).
#' @param palette Character vector. Color palette for OS groups
#'   (Default: `c("#1A9850", "#D73027")`).
#' @param add_stat Logical. Whether to perform and display Spearman
#'   correlation test (Default: `TRUE`).
#' @param max_features Numeric. Maximum number of features to plot
#'   (Default: `NULL`).
#' @param norm_exp Logical. Whether to apply log2 normalization on expression
#'   data (Default: `FALSE`).
#' @param width Numeric. Plot width in inches (Default: `NULL`).
#' @param height Numeric. Plot height in inches (Default: `NULL`).
#' @param ylab Character. Title for the Y-axis (Default: `NULL`).
#' @param xlab Character. Title for the X-axis (Default: `"Followup"`).
#' @param individual Logical. If `TRUE` and `outprefix` is provided, saves
#'   each feature scatter plot as its own PDF file. The combined grid is still
#'   saved as `<outprefix>_feature_scatter.pdf` (Default: `FALSE`).
#'
#' @return A patchwork object containing the feature scatter plots.
#' @importFrom rlang .data
#' @importFrom dplyr %>%
#' @export
plot_scatter <- function(
    data,
    signature = NULL,
    os_col = "OS",
    time_col = "OS.time",
    outprefix = NULL,
    theme = theme_clinprog(),
    exclude_cols = c("OS", "OS.time", "score", "score_group"),
    palette = c("#1A9850", "#D73027"),
    add_stat = TRUE,
    max_features = NULL,
    norm_exp = FALSE,
    width = NULL,
    height = NULL,
    ylab = NULL,
    xlab = "Followup",
    individual = FALSE
) {

  log_message(
    "Building feature expression vs follow-up time scatter plots..."
  )

  # Validate input data
  if (!is.data.frame(data)) {
    log_stop("'data' must be a data.frame")
  }

  if (!os_col %in% colnames(data)) {
    log_stop(paste0("Column '", os_col, "' not found in data."))
  }

  if (!time_col %in% colnames(data)) {
    log_stop(paste0("Column '", time_col, "' not found in data."))
  }

  if (!is.numeric(data[[time_col]])) {
    log_stop(paste0("Column '", time_col, "' must be numeric."))
  }

  # Validate signature
  if (!is.null(signature)) {
    if (!is.data.frame(signature)) {
      log_stop("'signature' must be a data.frame")
    }

    if (!all(c("feature", "coefficient") %in% colnames(signature))) {
      log_stop(
        "'signature' must contain 'feature' and 'coefficient' columns"
      )
    }

    if (!all(signature$feature %in% colnames(data))) {
      log_stop(
        "All features in 'signature' must be present in 'data' column names"
      )
    }
  }

  # Validate excluded columns
  if (!is.character(exclude_cols)) {
    log_stop("'exclude_cols' must be a character vector")
  }

  # Validate palette
  if (!is.null(palette) &&
      (!is.character(palette) || length(palette) < 2)) {
    log_stop(
      "'palette' must be a character vector with at least 2 valid colors or NULL"
    )
  }

  # Validate statistical display
  if (!is.logical(add_stat) || length(add_stat) != 1) {
    log_stop("'add_stat' must be a single logical value")
  }

  # Validate normalization
  if (!is.logical(norm_exp) || length(norm_exp) != 1) {
    log_stop("'norm_exp' must be a single logical value")
  }

  # Validate individual output
  if (!is.logical(individual) ||
      length(individual) != 1 ||
      is.na(individual)) {
    log_stop("'individual' must be TRUE or FALSE")
  }

  # Validate theme
  if (!inherits(theme, "theme")) {
    log_stop("'theme' must be a valid ggplot2 theme object")
  }

  # Validate axis labels
  if (!is.null(ylab) &&
      (!is.character(ylab) || length(ylab) != 1)) {
    log_stop("'ylab' must be a single character string or NULL")
  }

  if (!is.character(xlab) || length(xlab) != 1) {
    log_stop("'xlab' must be a single character string")
  }

  # Validate plot dimensions
  if (!is.null(width) &&
      (!is.numeric(width) || length(width) != 1 || width <= 0)) {
    log_stop("'width' must be a single positive numeric value or NULL")
  }

  if (!is.null(height) &&
      (!is.numeric(height) || length(height) != 1 || height <= 0)) {
    log_stop("'height' must be a single positive numeric value or NULL")
  }

  # Validate max_features
  if (!is.null(max_features)) {
    if (!is.numeric(max_features) ||
        length(max_features) != 1 ||
        max_features <= 0 ||
        max_features %% 1 != 0) {
      log_stop(
        "'max_features' must be a single positive integer or NULL"
      )
    }
  }

  # Validate outprefix
  if (!is.null(outprefix)) {
    if (!is.character(outprefix) || length(outprefix) != 1) {
      log_stop(
        "'outprefix' must be NULL or a single character string"
      )
    }
  }

  # individual only makes sense with outprefix
  if (individual && is.null(outprefix)) {
    log_warning(
      "'individual = TRUE' requires 'outprefix' to be provided. ",
      "Ignoring 'individual'."
    )
    individual <- FALSE
  }

  # Identify feature columns
  gene_cols <- setdiff(colnames(data), exclude_cols)

  if (length(gene_cols) == 0) {
    log_stop(
      "No feature columns found after removing 'exclude_cols'."
    )
  }

  # Handle max_features
  if (!is.null(max_features)) {
    if (length(gene_cols) > max_features) {
      if (!is.null(signature)) {
        log_message(
          paste0(
            "Selecting ", max_features,
            " features based on signature coefficients."
          )
        )

        gene_cols <- signature %>%
          dplyr::slice_max(
            order_by = abs(.data$coefficient),
            n = max_features,
            with_ties = FALSE
          ) %>%
          dplyr::pull(.data$feature)
      } else {
        log_message(
          paste0(
            "Selecting ", max_features, " features."
          )
        )

        gene_cols <- utils::head(gene_cols, max_features)
      }
    }
  } else {
    if (length(gene_cols) > 8) {
      log_warning(
        paste0(
          "Data contains ", length(gene_cols),
          " features. Plotting top 8 features to avoid cluttered layouts."
        )
      )

      if (!is.null(signature)) {
        log_message(
          "Selecting top 8 features based on signature coefficients."
        )

        gene_cols <- signature %>%
          dplyr::slice_max(
            order_by = abs(.data$coefficient),
            n = 8,
            with_ties = FALSE
          ) %>%
          dplyr::pull(.data$feature)
      } else {
        log_message("Selecting first 8 features.")
        gene_cols <- utils::head(gene_cols, 8)
      }
    }
  }

  # Validate selected feature columns
  non_numeric_genes <- gene_cols[
    !vapply(data[gene_cols], is.numeric, logical(1))
  ]

  if (length(non_numeric_genes) > 0) {
    log_warning(
      paste(
        "The following feature columns are not numeric and will be skipped:",
        paste(non_numeric_genes, collapse = ", ")
      )
    )

    gene_cols <- setdiff(gene_cols, non_numeric_genes)
  }

  if (length(gene_cols) == 0) {
    log_stop(
      "No valid numeric feature columns remaining to plot."
    )
  }

  # Define Y-axis label
  if (is.null(ylab)) {
    ylab <- if (norm_exp) {
      "log2(Expression + 1)"
    } else {
      "Expression"
    }
  }

  # Generate plots
  plot_list <- list()

  for (gene in gene_cols) {

    temp_df <- data.frame(
      OS_Val = data[[os_col]],
      Time = data[[time_col]],
      Expression = if (norm_exp) {
        log2(data[[gene]] + 1)
      } else {
        data[[gene]]
      }
    )

    # Remove NAs
    temp_df <- temp_df[
      !is.na(temp_df$OS_Val) &
        !is.na(temp_df$Time) &
        !is.na(temp_df$Expression),
      ,
      drop = FALSE
    ]

    # Format OS factor
    temp_df$OS_Group <- factor(
      temp_df$OS_Val,
      levels = c(0, 1),
      labels = c("Alive", "Dead")
    )

    # Calculate Spearman correlation
    subtitle_text <- NULL

    if (add_stat) {
      stat_texts <- NULL

      for (grp in c("Alive", "Dead")) {
        sub_df <- temp_df[temp_df$OS_Group == grp, ]

        if (nrow(sub_df) >= 3) {
          test_res <- stats::cor.test(
            sub_df$Time,
            sub_df$Expression,
            method = "spearman",
            alternative = "two.sided",
            exact = FALSE,
            continuity = FALSE
          )

          r_val <- test_res$estimate
          p_val <- test_res$p.value

          p_str <- if (is.na(p_val)) {
            "NA"
          } else if (p_val < 0.001) {
            "p < 0.001"
          } else {
            sprintf("p = %.3f", p_val)
          }

          stat_texts <- c(
            stat_texts,
            sprintf("rho = %.2f (%s)", r_val, p_str)
          )
        } else {
          log_warning(
            paste(
              "Sub dataframe length for group '", grp,
              "' is less than 3 and will be skipped."
            )
          )
        }
      }

      if (length(stat_texts) > 0) {
        subtitle_text <- paste(stat_texts, collapse = " | ")
      }
    }

    # Map colors for OS groups
    color_map <- c(
      "Alive" = palette[1],
      "Dead" = palette[2]
    )

    # Generate scatter plot
    p <- ggplot2::ggplot(
      temp_df,
      ggplot2::aes(
        x = .data$Time,
        y = .data$Expression,
        color = .data$OS_Group
      )
    ) +
      ggplot2::geom_point(
        alpha = 0.5,
        size = 2
      ) +
      ggplot2::geom_smooth(
        method = "lm",
        formula = y ~ x,
        se = FALSE,
        linewidth = 0.8
      ) +
      ggplot2::scale_color_manual(
        values = color_map
      ) +
      ggplot2::facet_grid(
        ~ .data$OS_Group,
        scales = "free_x"
      ) +
      ggplot2::labs(
        x = xlab,
        y = ylab,
        title = gene,
        subtitle = subtitle_text
      ) +
      theme +
      ggplot2::theme(
        legend.position = "none",
        axis.title = ggplot2::element_text(
          face = "plain",
          colour = "black",
          size = 12
        ),
        axis.text = ggplot2::element_text(
          face = "plain",
          colour = "black",
          size = 12
        ),
        panel.grid = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(
          hjust = 0.5,
          face = "plain",
          size = 14
        ),
        plot.subtitle = ggplot2::element_text(
          hjust = 0.5,
          face = "italic",
          size = 12
        ),
        strip.background = ggplot2::element_rect(
          fill = "gray90",
          color = NA
        ),
        strip.text = ggplot2::element_text(
          face = "plain",
          size = 12
        )
      )

    plot_list[[gene]] <- p
  }

  # Determine grid dimensions
  n_plots <- length(plot_list)

  if (n_plots <= 2) {
    ncol_plot <- 1
  } else if (n_plots <= 4) {
    ncol_plot <- 2
  } else {
    ncol_plot <- 2
  }

  nrow_plot <- ceiling(n_plots / ncol_plot)

  # Automatic figure dimensions
  if (is.null(width)) {
    width <- 8 * ncol_plot
  }

  if (is.null(height)) {
    height <- 4 * nrow_plot
  }

  # Combine plots
  combined_plot <- patchwork::wrap_plots(
    plot_list,
    ncol = ncol_plot
  )

  # Save PDF
  if (!is.null(outprefix)) {
    grDevices::pdf(
      file = paste0(outprefix, "_feature_scatter.pdf"),
      width = width,
      height = height
    )

    on.exit(grDevices::dev.off(), add = TRUE)

    print(combined_plot)

    # Save individual plots
    if (individual) {
      safe_names <- gsub(
        "[^A-Za-z0-9_\\-]",
        "_",
        names(plot_list)
      )

      ind_width <- if (!is.null(width)) {
        min(width, 8)
      } else {
        8
      }

      ind_height <- if (!is.null(height)) {
        min(height, 4)
      } else {
        4
      }

      for (i in seq_along(plot_list)) {
        fname_i <- paste0(
          outprefix,
          "_feature_scatter_",
          safe_names[i],
          ".pdf"
        )

        ggplot2::ggsave(
          filename = fname_i,
          plot = plot_list[[i]],
          device = "pdf",
          width = ind_width,
          height = ind_height
        )
      }
    }
  }

  return(combined_plot)
}
