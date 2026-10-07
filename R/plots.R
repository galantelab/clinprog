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
  tt <- signature[stats::complete.cases(signature), , drop = FALSE]
  tt <- tt[tt$coefficient != 0, , drop = FALSE]

  # Checks if signature is still valid
  if (nrow(tt) == 0) {stop("No valid coefficients available for plotting")}

  # Selects top 10 features
  tt <- dplyr::slice_max(tt, order_by = abs(coefficient), n = 10, with_ties = FALSE)

  # Orders features by coefficient values
  tt$feature <- forcats::fct_reorder(tt$feature, tt$coefficient)

  # Makes plot
  ll <- ggplot2::ggplot(tt, ggplot2::aes(x = feature, y = coefficient)) +
    ggplot2::geom_segment(ggplot2::aes(x = feature, xend = feature, y = 0, yend = coefficient),
                          linewidth = 1.2, color = "grey") +
    ggplot2::geom_point(size = 3, colour = "#1c9099") + theme +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

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
#' @param width Numeric. Plot width in inches (Default: \code{8})
#' @param height Numeric. Plot height in inches (Default: \code{5})
#' @param dpi Numeric. Plot DPI resolution (Default: \code{300})
#'
#' @return A \code{ggplot} object.
#' @export
plot_histogram <- function(signature, outprefix = NULL, theme = theme_clinprog(), width = 8, height = 5, dpi = 300) {

  message("Building histogram plot...")

  # Validates input
  if (!all(c("feature", "coefficient") %in% colnames(signature))) {stop("'signature' must contain 'feature' and 'coefficient' columns")}

  # Validates theme
  if (!inherits(theme, "theme")) {stop("'theme' must be a valid ggplot2 theme object")}

  # Validates plot sizes
  if (!is.numeric(dpi) || length(dpi) != 1 || dpi <= 0) {stop("'dpi' must be a single positive numeric value")}
  if (!is.numeric(width) || length(width) != 1 || width <= 0) {stop("'width' must be a single positive numeric value")}
  if (!is.numeric(height) || length(height) != 1 || height <= 0) {stop("'height' must be a single positive numeric value")}

  # Prepares data
  tt <- signature[stats::complete.cases(signature), , drop = FALSE]
  tt <- tt[tt$coefficient != 0, , drop = FALSE]

  # Checks if signature is still valid
  if (nrow(tt) == 0) {stop("No valid coefficients available for plotting")}

  # Makes plot
  pl <- ggplot2::ggplot(tt, ggplot2::aes(x = coefficient)) +
    ggplot2::geom_histogram(binwidth = 0.005, alpha = 1) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::labs(x = "Coefficient", y = NULL) + theme

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
#' @param theme A ggplot2 theme object. Defaults to \code{theme_clinprog()}.
#' @param width Numeric. Plot width in inches (optional). If \code{NULL}, the width is set automatically.
#' @param height Numeric. Plot height in inches (optional). If \code{NULL}, the height is set automatically.
#' @param ... Additional arguments passed to plotting functions.
#'
#' @return a list of \code{ggplot} objects.
#' @export
plot_ph <- function(x, is_multi = FALSE, outprefix = NULL,
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
    if (!is.null(p$labels$y)) {p$labels$y <- gsub("score_group", "Score", p$labels$y)}
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
  if (is.null(width)) {width <- 4 * ncol_plot}
  if (is.null(height)) {height <- 4 * nrow_plot}

  # Combines plots into a single patchwork object
  ph_plots <- patchwork::wrap_plots(ph_plots, ncol = ncol_plot)

  # Saves plots if requested
  if (!is.null(outprefix))
  {
    if (!is.character(outprefix) || length(outprefix) != 1) {stop("'outprefix' must be a single character string")}
    filename <- paste0(outprefix, "_ph_assumptions_plot.pdf")
    grDevices::pdf(file = filename, width = width, height = height)
    on.exit(grDevices::dev.off(), add = TRUE)
    print(ph_plots)
  }

  return(ph_plots)
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
                      ylab = "Survival probability", xlab = "Time", title = NULL, legend.title = "clinprog groups",
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
    pval.coord = c(0, 0.05),
    title = title,
    legend = c(0.8, 0.9),
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
#' @param frequency.threshold Numeric. Frequency threshold displayed as a horizontal dashed line (default: \code{25}).
#' @param theme A ggplot2 theme object. Defaults to \code{theme_clinprog()}.
#' @param bar.width Numeric. Width of bars (default: \code{0.5}).
#' @param ylab Character. Title for the Y-axis (default: \code{Frequency (\%)}).
#' @param xlab Character. Title for the X-axis (default: empty string).
#' @param width Numeric. Plot width in inches (default: \code{8}).
#' @param height Numeric. Plot height in inches (default: \code{5}).
#'
#' @return A \code{ggplot} object.
#' @export
plot_barplot <- function(plot_df, outprefix = NULL, frequency.threshold = 25, theme = theme_clinprog(),
                           bar.width = 0.5, ylab = "Frequency (%)", xlab = "", width = 8, height = 5)
{

  message("Building bar plot...")

  # Validates input data
  if (!is.data.frame(plot_df)) {stop("'plot_df' must be a data.frame")}
  required_cols <- c("covariate", "frequency")
  if (!all(required_cols %in% colnames(plot_df))) {stop("'plot_df' must contain columns: 'covariate' and 'frequency'")}

  # Validates frequency threshold
  if (!is.numeric(frequency.threshold) || length(frequency.threshold) != 1 ||
      frequency.threshold < 0 || frequency.threshold > 100)
  {
    stop("'frequency.threshold' must be a numeric value between 0 and 100")
  }

  # Validates bar width
  if (!is.numeric(bar.width) || length(bar.width) != 1 || bar.width <= 0) {stop("'bar.width' must be a single positive numeric value")}

  # Validates theme
  if (!inherits(theme, "theme")) {stop("'theme' must be a valid ggplot2 theme object")}

  # Validates labels for the axis
  if (!is.character(ylab) || length(ylab) != 1) {stop("'ylab' must be a single character string")}
  if (!is.character(xlab) || length(xlab) != 1) {stop("'xlab' must be a single character string")}

  # Validates plot sizes
  if (!is.numeric(width) || length(width) != 1 || width <= 0) {stop("'width' must be a single positive numeric value")}
  if (!is.numeric(height) || length(height) != 1 || height <= 0) {stop("'height' must be a single positive numeric value")}

  # Ensures frequency is numeric
  plot_df$frequency <- as.numeric(plot_df$frequency)
  if (any(is.na(plot_df$frequency))) {stop("'frequency' column contains NA values")}
  if (any(plot_df$frequency < 0 || plot_df$frequency > 100)) {stop("'frequency' values must be between 0 and 100")}

  # Sorts variables by decreasing frequency
  plot_df <- plot_df[order(plot_df$frequency, decreasing = TRUE), , drop = FALSE]

  # Generates barplot
  final_plot <- ggplot2::ggplot(data = plot_df, ggplot2::aes(x = stats::reorder(covariate, -frequency), y = frequency)) +
    ggplot2::geom_col(width = bar.width, color = "grey", fill = "grey") +
    ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
    ggplot2::geom_hline(yintercept = frequency.threshold, color = "#BB1136", linetype = "dashed", linewidth = 0.5) +
    ggplot2::scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100)) +
    theme + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1))

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
