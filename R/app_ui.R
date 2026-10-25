#' clinprog Interactive Suite User Interface
#'
#' @title User Interface (UI) for the clinprog Shiny Application
#'
#' @description
#' Builds the graphical user interface for the clinprog interactive suite
#' using the \code{bslib} package with a navbar-based navigation structure
#' (\code{page_navbar}). It provides controls for input datasets, analytical
#' parameters, and rendering outputs across Module I (Regression), Module II
#' (Survival), and THE integrative workflow (complete pipeline).
#'
#' @details
#' The layout is partitioned into three main navigation panels:
#' \itemize{
#'   \item \bold{Module I (Regression):} Selects prognostic molecular signatures using LASSO/Cox regression coupled with bootstrapping.
#'   \item \bold{Module II (Survival):} Performs univariate and multivariate survival analyses (Kaplan-Meier estimates, Cox models) using an existing signature.
#'   \item \bold{Integrative Workflow (Complete Pipeline):} Runs an end-to-end integrated workflow connecting signature selection directly to patient survival evaluation.
#' }
#' The UI relies on the \code{flatly} bootswatch theme and supports dynamic logo rendering and image exports.
#'
#' @return A \code{shiny.tag.list} object defining the layout of the Shiny application.
#'
#' @seealso \code{\link{app_server}}, \code{\link{run_app}}
#'
#' @examples
#' \dontrun{
#' # Inspect the UI structure:
#' ui_structure <- app_ui()
#' }
#'
#' @export
app_ui <- function() {
  # Helper: attaches a native HTML tooltip to any label/text
  tip <- function(label, tooltip) {
    shiny::tags$span(label, title = tooltip)
  }

  # Helper: builds an author card with circular photo (or initials fallback)
  author_card <- function(name, photo_file, bio, role = NULL) {
    # Try to locate the photo across possible package paths
    photo_path <- system.file(paste0("figures/authors/", photo_file), package = "clinprog")
    if (photo_path == "" || !file.exists(photo_path)) {
      photo_path <- paste0("inst/figures/authors/", photo_file)
    }

    # Photo tag (image or initials fallback)
    if (file.exists(photo_path)) {
      photo_tag <- shiny::tags$img(
        src = base64enc::dataURI(file = photo_path, mime = "image/jpeg"),
        style = "width: 110px; height: 110px; border-radius: 50%; object-fit: cover; border: 3px solid #18bc9c; margin-bottom: 10px;"
      )
    } else {
      # Fallback: initials in a circular gray badge
      initials <- paste0(
        substr(name, 1, 1),
        substr(sub("^\\S+\\s+", "", name), 1, 1)
      )
      photo_tag <- shiny::tags$div(
        initials,
        style = paste0(
          "width: 110px; height: 110px; border-radius: 50%; ",
          "background: #dee2e6; color: #495057; ",
          "display: flex; align-items: center; justify-content: center; ",
          "font-size: 34px; font-weight: 600; ",
          "border: 3px solid #18bc9c; margin-bottom: 10px;"
        )
      )
    }

    shiny::div(
      class = "author-card",
      style = "text-align: center; padding: 15px; background: #ffffff; border-radius: 8px; box-shadow: 0 1px 4px rgba(0,0,0,0.08); height: 100%;",
      photo_tag,
      shiny::tags$h5(name, style = "margin: 5px 0 2px 0; font-weight: 600; color: #2c3e50;"),
      if (!is.null(role)) shiny::tags$p(role, style = "margin: 0 0 8px 0; font-size: 11px; color: #18bc9c; font-weight: 600; text-transform: uppercase; letter-spacing: 0.5px;"),
      shiny::tags$p(bio, style = "margin: 0; font-size: 11px; color: #495057; line-height: 1.4; text-align: justify;")
    )
  }

  logo_path <- system.file("figures/hexagon-sticker.jpeg", package = "clinprog")
  if (logo_path == "" || !file.exists(logo_path)) {
    logo_path <- "inst/figures/hexagon-sticker.jpeg"
  }

  logo_tag <- if (file.exists(logo_path)) {
    shiny::tags$img(
      src = base64enc::dataURI(file = logo_path, mime = "image/jpeg"),
      style = "height: 50px; width: auto; margin-right: 10px; margin-top: 0px; margin-bottom: 0px; vertical-align: middle; flex-shrink: 0;"
    )
  } else {
    NULL
  }

  # Lab logo (loaded from inst/figures)
  lab_logo_path <- system.file("figures/logo_galanteslab.png", package = "clinprog")
  if (lab_logo_path == "" || !file.exists(lab_logo_path)) {
    lab_logo_path <- "inst/figures/logo_galanteslab.png"
  }

  lab_logo_tag <- if (file.exists(lab_logo_path)) {
    shiny::tags$a(
      href = "https://www.bioinfo.mochsl.org.br/",
      target = "_blank",
      shiny::tags$img(
        src = base64enc::dataURI(file = lab_logo_path, mime = "image/png"),
        style = "height: 32px; width: auto; vertical-align: middle;"
      )
    )
  } else {
    NULL
  }

  # GitHub logo (inline SVG, white octocat)
  github_logo_tag <- shiny::tags$a(
    href = "https://github.com/galantelab/clinprog",
    target = "_blank",
    shiny::tags$svg(
      height = "32",
      width = "32",
      viewBox = "0 0 16 16",
      fill = "#ffffff",
      style = "vertical-align: middle;",
      shiny::tags$path(
        `fill-rule` = "evenodd",
        d = "M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0 0 16 8c0-4.42-3.58-8-8-8z"
      )
    )
  )

  bslib::page_navbar(
    title = shiny::div(
      style = "display: flex; align-items: center; justify-content: space-between; width: 100%; padding-right: 15px;",
      shiny::div(
        style = "display: flex; align-items: center;",
        logo_tag,
        shiny::div(
          style = "display: flex; flex-direction: column; justify-content: center; line-height: 1.1;",
          shiny::tags$span(
            "clinprog Interactive Suite",
            style = "font-size: 30px; color: #ffffff; font-weight: 600; white-space: nowrap;"
          ),
          shiny::tags$span(
            "an R package for discovering and validating prognostic molecular signatures from high-dimensional transcriptomic data using bootstrap resampling and penalized Cox regression (LASSO)",
            style = "font-size: 11px; color: #dcdcdc; font-weight: 400; line-height: 1.25; white-space: normal; max-width: 620px;"
          )
        )
      ),
      shiny::div(
        style = "display: flex; align-items: center; gap: 12px; flex-shrink: 0;",
        lab_logo_tag,
        github_logo_tag
      )
    ),
    theme = bslib::bs_theme(
      bootswatch = "flatly",
      base_font = bslib::font_google("Lato"),
      heading_font = bslib::font_google("Lato")
    ) %>%
      bslib::bs_add_rules(
        list(
          ".clinprog-notification-success { background-color: #d4edda !important; color: #155724 !important; border-color: #c3e6cb !important; }",
          ".dataTables_wrapper { padding-bottom: 75px !important; }",
          "body { padding-bottom: 100px !important; }",
          ".tab-content, .tab-pane, .main-container, .container-fluid, .sidebarLayout, .main-panel, .row, .col-sm-9, .about-section { padding-bottom: 100px !important; }",
          ".navbar { padding-top: 15px !important; padding-bottom: 15px !important; min-height: 50px !important; height: auto !important; }",
          ".navbar .container-fluid { display: flex !important; align-items: center !important; justify-content: flex-start !important; padding-top: 0 !important; padding-bottom: 0 !important; }",
          ".navbar-brand { display: flex !important; align-items: center !important; padding-top: 0 !important; padding-bottom: 0 !important; margin-top: 0 !important; margin-bottom: 0 !important; margin-right: 25px !important; height: auto !important; flex: 1 1 auto !important; }",
          ".navbar-nav { display: flex !important; align-items: center !important; margin-top: 0 !important; margin-bottom: 0 !important; }",
          ".navbar-nav .nav-link { font-size: 25px !important; font-weight: 500 !important; padding-top: 4px !important; padding-bottom: 4px !important; display: flex !important; align-items: center !important; line-height: 1 !important; }",
          ".navbar-nav .nav-link.active { border-bottom: 3px solid #18bc9c !important; }",
          ".sidebarLayout { padding-top: 0px !important; margin-top: -10px !important; }",
          ".dataTables_wrapper { overflow-x: auto; }",
          ".dataTable { width: 100% !important; }",
          ".shiny-notification:has(.clinprog-notification-success) { background-color: #d4edda !important; border: none !important; box-shadow: 0 2px 6px rgba(0,0,0,0.1) !important; padding: 8px 12px !important; }",
          ".clinprog-notification-success { color: #155724 !important; font-weight: 500; }",
          "#shiny-notification-panel { bottom: 90px !important; right: 20px !important; z-index: 9999 !important; }",
          ".shiny-notification { z-index: 9999 !important; }",
          ".well { background-color: #f8f9fa; }",
          ".plot-container { overflow: auto; }",
          ".shiny-plot-output { transition: transform 0.1s ease; transform-origin: center center; cursor: grab; }",
          ".file-status-text { font-size: 11px; color: #6c757d; margin-top: -10px; margin-bottom: 10px; font-style: italic; display: block; }",
          ".clinprog-spinner { border: 3px solid #f3f3f3; border-top: 3px solid #18bc9c; border-radius: 50%; width: 18px; height: 18px; animation: clinprog-spin 1s linear infinite; flex-shrink: 0; }",
          "@keyframes clinprog-spin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }",
          ".clinprog-citation-panel { background-color: #e8f4f8; border-top: 1px solid #d0e4ec; padding: 6px 20px; font-size: 11px; color: #2c3e50; line-height: 1.4; }",
          ".clinprog-citation-panel a { color: #0056b3; text-decoration: underline; }",
          ".clinprog-citation-panel a:hover { color: #003d82; }",
          ".about-section { max-width: 950px; margin: 0 auto; padding: 15px 10px; }",
          ".about-section h3 { color: #2c3e50; margin-top: 0; }",
          ".about-section p { font-size: 14px; line-height: 1.6; color: #495057; text-align: justify; }",
          ".about-section ul { font-size: 14px; line-height: 1.6; color: #495057; }"
        )
      ),

    # ================================================================
    # MODULE I: REGRESSION
    # ================================================================
    bslib::nav_panel(
      title = "Module I: Regression",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::h4("1. Data Input"),
          shiny::radioButtons(
            "src_data_reg", tip("Expression Data Source:",
                                "Select the source of the expression and survival data"),
            choices = c("Use Toy Expression and Survival Data" = "toy", "Upload File (.tsv/.csv)" = "upload"),
            selected = "toy"
          ),
          shiny::conditionalPanel(
            condition = "input.src_data_reg == 'upload'",
            shiny::fileInput("file_data_reg", tip("Upload Expression File:",
                                                  "Upload a TSV/CSV/TXT file with samples as rows, survival (OS, OS.time) as first columns, and features afterwards"),
                             accept = c(".tsv", ".csv", ".txt")),
            shiny::uiOutput("status_file_data_reg"),
            shiny::helpText("Tip: in some Linux browsers the file dialog may require you to click the file once and then press 'Open' (instead of double-clicking it).")
          ),
          shiny::hr(),
          shiny::h4("2. Parameters"),
          shiny::textInput("reg_outprefix", tip("Output Prefix:",
                                                "Prefix used to name every output file generated by this module"),
                           value = "clinprog_reg"),
          shiny::checkboxInput("reg_normExp", tip("Log2 Transform Expression",
                                                  "Transforms expression values in input data with log2(expression+1)"),
                               value = FALSE),
          shiny::numericInput("reg_bootstrap", tip("Bootstrap Iterations:",
                                                   "Number of bootstrap iterations (higher = more robust, but slower)"),
                              value = 10, min = 1, step = 1),
          shiny::numericInput("reg_groupsize", tip("Group Size (features/iteration):",
                                                   "Number of features sampled in each bootstrap iteration"),
                              value = 10, min = 1, step = 1),
          shiny::numericInput("reg_percentagefilter", tip("Correlation Filter Threshold (0-1):",
                                                          "Max proportion of highly correlated feature pairs allowed per iteration (Spearman > 0.80)"),
                              value = 0.3, min = 0, max = 1, step = 0.05),
          shiny::numericInput("reg_variancefilter", tip("Variance Filter Threshold (0-1):",
                                                        "Removes features whose normalized variance is below this threshold"),
                              value = 0.01, min = 0, max = 1, step = 0.005),
          shiny::selectInput("reg_type", tip("Feature Type:",
                                             "Feature type: 'gene' or 'transcript' (affects the internal coefficient cutoff)"),
                             choices = c("gene", "transcript"), selected = "gene"),
          shiny::checkboxInput("reg_use_min_signature", tip("Search for Minimum Signature (Top-k Prefix Search)",
                                                            "Searches the smallest top-k prefix of the signature that still yields log-rank p < 0.05 on the same dataset"),
                               value = FALSE),
          shiny::conditionalPanel(
            condition = "input.reg_use_min_signature == true",
            shiny::numericInput("reg_min_signature_size", tip("Minimum Signature Size (>= 1):",
                                                              "Minimum number of features to start the top-k search"),
                                value = 2, min = 1, step = 1),
            shiny::selectInput("reg_min_signature_cutoff", tip("Cutoff Strategy for Minimum Signature:",
                                                               "Strategy used to split scores into low/high groups during the minimum signature search"),
                               choices = c("median", "mean", "signal", "roc"),
                               selected = "median"),
            shiny::helpText("Finds the smallest top-k prefix of the signature with log-rank p < 0.05 (in-sample heuristic; see tutorial).")
          ),
          shiny::uiOutput("ui_reg_followup"),
          shiny::numericInput("reg_seed", tip("Random Seed:",
                                              "Random seed for reproducibility of every stochastic step"),
                              value = 17),
          shiny::checkboxInput("reg_force", tip("Skip Survival Checks (Override PH filters)",
                                                "If checked, bypasses survival-related sanity filters (use with caution)"),
                               value = FALSE),
          shiny::hr(),
          shiny::actionButton("btn_run_reg", "Run Module I (Regression)", class = "btn-primary w-100"),
          shiny::br(), shiny::br(),
          shiny::downloadButton("dl_reg_zip", "Download All Module I Outputs (.zip)", class = "btn-success w-100")
        ),
        shiny::mainPanel(
          shiny::h3("Selected Molecular Signature"),
          DT::DTOutput("out_table_reg_sig"),
          shiny::hr(),
          shiny::h3("Diagnostic Plots"),
          shiny::tabsetPanel(
            shiny::tabPanel(
              "Histogram",
              shiny::fluidRow(
                shiny::column(3,
                              shiny::wellPanel(
                                shiny::h5("Dimensions"),
                                shiny::sliderInput("reg_hist_width", "Width (in):",
                                                   min = 4, max = 16, value = 8, step = 0.5),
                                shiny::sliderInput("reg_hist_height", "Height (in):",
                                                   min = 3, max = 12, value = 5, step = 0.5),
                                shiny::hr(),
                                shiny::h5("Download"),
                                shiny::selectInput("reg_hist_format", "Format:",
                                                   choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                                   selected = "png"),
                                shiny::conditionalPanel(
                                  condition = "input.reg_hist_format == 'png'",
                                  shiny::numericInput("reg_hist_dpi", "DPI:",
                                                      value = 300, min = 100, max = 1000, step = 50)
                                ),
                                shiny::downloadButton("dl_reg_hist", "Download",
                                                      class = "btn-primary w-100")
                              )
                ),
                shiny::column(9,
                              shiny::plotOutput("out_plot_reg_hist", height = "500px")
                )
              )
            ),
            shiny::tabPanel(
              "Lollipop",
              shiny::fluidRow(
                shiny::column(3,
                              shiny::wellPanel(
                                shiny::h5("Dimensions"),
                                shiny::sliderInput("reg_lolli_width", "Width (in):",
                                                   min = 4, max = 16, value = 8, step = 0.5),
                                shiny::sliderInput("reg_lolli_height", "Height (in):",
                                                   min = 3, max = 12, value = 5, step = 0.5),
                                shiny::hr(),
                                shiny::h5("Download"),
                                shiny::selectInput("reg_lolli_format", "Format:",
                                                   choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                                   selected = "png"),
                                shiny::conditionalPanel(
                                  condition = "input.reg_lolli_format == 'png'",
                                  shiny::numericInput("reg_lolli_dpi", "DPI:",
                                                      value = 300, min = 100, max = 1000, step = 50)
                                ),
                                shiny::downloadButton("dl_reg_lolli", "Download",
                                                      class = "btn-primary w-100")
                              )
                ),
                shiny::column(9,
                              shiny::plotOutput("out_plot_reg_lolli", height = "500px")
                )
              )
            )
          )
        )
      )
    ),

    # ================================================================
    # MODULE II: SURVIVAL
    # ================================================================
    bslib::nav_panel(
      title = "Module II: Survival",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::h4("1. Data Input"),
          shiny::radioButtons(
            "src_data_surv", tip("Expression Data Source:",
                                 "Select the source of the expression and survival data"),
            choices = c("Use Toy Expression and Survival Data" = "toy", "Upload File (.tsv/.csv)" = "upload"),
            selected = "toy"
          ),
          shiny::conditionalPanel(
            condition = "input.src_data_surv == 'upload'",
            shiny::fileInput("file_data_surv", tip("Upload Expression File:",
                                                   "Upload a TSV/CSV/TXT file with samples as rows and survival columns first"),
                             accept = c(".tsv", ".csv", ".txt")),
            shiny::uiOutput("status_file_data_surv"),
            shiny::helpText("Tip: in some Linux browsers the file dialog may require you to click the file once and then press 'Open' (instead of double-clicking it).")
          ),
          shiny::fileInput("file_sig_surv", tip("Signature File (.tsv/.txt):",
                                                "Upload a signature file with 'feature' and 'coefficient' columns (leave blank to reuse Module I)"),
                           accept = c(".tsv", ".csv", ".txt")),
          shiny::uiOutput("status_file_sig_surv"),
          shiny::helpText("Leave blank to automatically use the signature generated in Module I (if executed)."),
          shiny::hr(),
          shiny::h4("2. Multivariate Analysis Options"),
          shiny::checkboxInput("surv_multivariate", tip("Enable Multivariate Cox Analysis",
                                                        "Enables multivariate Cox analysis with clinical covariates"),
                               value = FALSE),
          shiny::conditionalPanel(
            condition = "input.surv_multivariate == true",
            shiny::radioButtons(
              "src_clin_surv", tip("Clinical Data Source:",
                                   "Select the source of the clinical data"),
              choices = c("Use Toy Clinical Data" = "toy", "Upload File (.tsv/.csv)" = "upload"),
              selected = "toy"
            ),
            shiny::conditionalPanel(
              condition = "input.src_clin_surv == 'upload'",
              shiny::fileInput("file_clin_surv", tip("Upload Clinical File:",
                                                     "Upload a TSV/CSV/TXT file with binary clinical covariates"),
                               accept = c(".tsv", ".csv", ".txt")),
              shiny::uiOutput("status_file_clin_surv"),
              shiny::helpText("Tip: in some Linux browsers the file dialog may require you to click the file once and then press 'Open' (instead of double-clicking it).")
            )
          ),
          shiny::hr(),
          shiny::h4("3. Parameters"),
          shiny::textInput("surv_outprefix", tip("Output Prefix:",
                                                 "Prefix used to name every output file generated by this module"),
                           value = "clinprog_surv"),
          shiny::checkboxInput("surv_normExp", tip("Log2 Transform Expression",
                                                   "Transforms expression values in input data with log2(expression+1)"),
                               value = FALSE),
          shiny::numericInput("surv_bootstrap", tip("Multivariate Bootstrap Iterations:",
                                                    "Number of bootstrap iterations used to select stable clinical covariates"),
                              value = 1, min = 1, step = 1),
          shiny::numericInput("surv_variancefilter", tip("Variance Filter Threshold:",
                                                         "Minimum normalized variance of follow-up time required"),
                              value = 0.01, min = 0, max = 1, step = 0.005),
          shiny::selectInput("surv_score_cutoff", tip("Score Cutoff Strategy:",
                                                      "Strategy used to split patients into low/high score groups (median, mean, signal or ROC)"),
                             choices = c("median", "mean", "signal", "roc"), selected = "median"),
          shiny::numericInput("surv_p_cutoff", tip("Univariate P-value Cutoff:",
                                                   "Univariate p-value threshold to select clinical covariates for multivariate analysis"),
                              value = 0.2, min = 0, max = 1, step = 0.05),
          shiny::uiOutput("ui_surv_followup"),
          shiny::numericInput("surv_seed", tip("Random Seed:",
                                               "Random seed for reproducibility of every stochastic step"),
                              value = 17),
          shiny::checkboxInput("surv_force", tip("Skip Survival Checks",
                                                 "If checked, bypasses survival-related sanity filters (use with caution)"),
                               value = FALSE),
          shiny::hr(),
          shiny::actionButton("btn_run_surv", "Run Module II (Survival)", class = "btn-primary w-100"),
          shiny::br(), shiny::br(),
          shiny::downloadButton("dl_surv_zip", "Download All Module II Outputs (.zip)", class = "btn-success w-100")
        ),
        shiny::mainPanel(
          shiny::tabsetPanel(
            shiny::tabPanel("Plots",
                            shiny::tabsetPanel(
                              shiny::tabPanel("Wordcloud",
                                              shiny::fluidRow(
                                                shiny::column(3,
                                                              shiny::wellPanel(
                                                                shiny::h5("Dimensions"),
                                                                shiny::sliderInput("surv_wordcloud_width", "Width (in):",
                                                                                   min = 4, max = 16, value = 8, step = 0.5),
                                                                shiny::sliderInput("surv_wordcloud_height", "Height (in):",
                                                                                   min = 3, max = 12, value = 5, step = 0.5),
                                                                shiny::hr(),
                                                                shiny::h5("Download"),
                                                                shiny::selectInput("surv_wordcloud_format", "Format:",
                                                                                   choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                                                                   selected = "png"),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.surv_wordcloud_format == 'png'",
                                                                  shiny::numericInput("surv_wordcloud_dpi", "DPI:",
                                                                                      value = 300, min = 100, max = 1000, step = 50)
                                                                ),
                                                                shiny::downloadButton("dl_surv_wordcloud", "Download",
                                                                                      class = "btn-primary w-100")
                                                              )
                                                ),
                                                shiny::column(9,
                                                              shiny::plotOutput("out_plot_wordcloud", height = "500px")
                                                )
                                              )
                              ),
                              shiny::tabPanel("Boxplot",
                                              shiny::fluidRow(
                                                shiny::column(3,
                                                              shiny::wellPanel(
                                                                shiny::h5("Dimensions"),
                                                                shiny::sliderInput("surv_box_width", "Width (in):",
                                                                                   min = 4, max = 16, value = 8, step = 0.5),
                                                                shiny::sliderInput("surv_box_height", "Height (in):",
                                                                                   min = 3, max = 12, value = 5, step = 0.5),
                                                                shiny::hr(),
                                                                shiny::h5("Download"),
                                                                shiny::selectInput("surv_box_format", "Format:",
                                                                                   choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                                                                   selected = "png"),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.surv_box_format == 'png'",
                                                                  shiny::numericInput("surv_box_dpi", "DPI:",
                                                                                      value = 300, min = 100, max = 1000, step = 50)
                                                                ),
                                                                shiny::downloadButton("dl_surv_box", "Download",
                                                                                      class = "btn-primary w-100")
                                                              )
                                                ),
                                                shiny::column(9,
                                                              shiny::plotOutput("out_plot_box", height = "500px")
                                                )
                                              )
                              ),
                              shiny::tabPanel("Scatter Plot",
                                              shiny::fluidRow(
                                                shiny::column(3,
                                                              shiny::wellPanel(
                                                                shiny::h5("Dimensions"),
                                                                shiny::sliderInput("surv_scatter_width", "Width (in):",
                                                                                   min = 4, max = 16, value = 8, step = 0.5),
                                                                shiny::sliderInput("surv_scatter_height", "Height (in):",
                                                                                   min = 3, max = 12, value = 5, step = 0.5),
                                                                shiny::hr(),
                                                                shiny::h5("Download"),
                                                                shiny::selectInput("surv_scatter_format", "Format:",
                                                                                   choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                                                                   selected = "png"),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.surv_scatter_format == 'png'",
                                                                  shiny::numericInput("surv_scatter_dpi", "DPI:",
                                                                                      value = 300, min = 100, max = 1000, step = 50)
                                                                ),
                                                                shiny::downloadButton("dl_surv_scatter", "Download",
                                                                                      class = "btn-primary w-100")
                                                              )
                                                ),
                                                shiny::column(9,
                                                              shiny::plotOutput("out_plot_scatter", height = "500px")
                                                )
                                              )
                              ),
                              shiny::tabPanel("Score Bar Plot",
                                              shiny::fluidRow(
                                                shiny::column(3,
                                                              shiny::wellPanel(
                                                                shiny::h5("Dimensions"),
                                                                shiny::sliderInput("surv_score_bar_width", "Width (in):",
                                                                                   min = 4, max = 16, value = 8, step = 0.5),
                                                                shiny::sliderInput("surv_score_bar_height", "Height (in):",
                                                                                   min = 3, max = 12, value = 5, step = 0.5),
                                                                shiny::hr(),
                                                                shiny::h5("Download"),
                                                                shiny::selectInput("surv_score_bar_format", "Format:",
                                                                                   choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                                                                   selected = "png"),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.surv_score_bar_format == 'png'",
                                                                  shiny::numericInput("surv_score_bar_dpi", "DPI:",
                                                                                      value = 300, min = 100, max = 1000, step = 50)
                                                                ),
                                                                shiny::downloadButton("dl_surv_score_bar", "Download",
                                                                                      class = "btn-primary w-100")
                                                              )
                                                ),
                                                shiny::column(9,
                                                              shiny::plotOutput("out_plot_score_bar", height = "500px")
                                                )
                                              )
                              ),
                              shiny::tabPanel("Line Plot",
                                              shiny::fluidRow(
                                                shiny::column(3,
                                                              shiny::wellPanel(
                                                                shiny::h5("Dimensions"),
                                                                shiny::sliderInput("surv_line_width", "Width (in):",
                                                                                   min = 4, max = 16, value = 8, step = 0.5),
                                                                shiny::sliderInput("surv_line_height", "Height (in):",
                                                                                   min = 3, max = 12, value = 5, step = 0.5),
                                                                shiny::hr(),
                                                                shiny::h5("Download"),
                                                                shiny::selectInput("surv_line_format", "Format:",
                                                                                   choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                                                                   selected = "png"),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.surv_line_format == 'png'",
                                                                  shiny::numericInput("surv_line_dpi", "DPI:",
                                                                                      value = 300, min = 100, max = 1000, step = 50)
                                                                ),
                                                                shiny::downloadButton("dl_surv_line", "Download",
                                                                                      class = "btn-primary w-100")
                                                              )
                                                ),
                                                shiny::column(9,
                                                              shiny::plotOutput("out_plot_line", height = "500px")
                                                )
                                              )
                              ),
                              shiny::tabPanel("PH Assumptions Plot",
                                              shiny::fluidRow(
                                                shiny::column(3,
                                                              shiny::wellPanel(
                                                                shiny::h5("Dimensions"),
                                                                shiny::sliderInput("surv_ph_width", "Width (in):",
                                                                                   min = 4, max = 16, value = 8, step = 0.5),
                                                                shiny::sliderInput("surv_ph_height", "Height (in):",
                                                                                   min = 3, max = 12, value = 5, step = 0.5),
                                                                shiny::hr(),
                                                                shiny::h5("Download"),
                                                                shiny::selectInput("surv_ph_format", "Format:",
                                                                                   choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                                                                   selected = "png"),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.surv_ph_format == 'png'",
                                                                  shiny::numericInput("surv_ph_dpi", "DPI:",
                                                                                      value = 300, min = 100, max = 1000, step = 50)
                                                                ),
                                                                shiny::downloadButton("dl_surv_ph", "Download",
                                                                                      class = "btn-primary w-100")
                                                              )
                                                ),
                                                shiny::column(9,
                                                              shiny::plotOutput("out_plot_ph", height = "500px")
                                                )
                                              )
                              ),
                              shiny::tabPanel("ROC Curve",
                                              shiny::fluidRow(
                                                shiny::column(3,
                                                              shiny::wellPanel(
                                                                shiny::h5("Dimensions"),
                                                                shiny::sliderInput("surv_roc_width", "Width (in):",
                                                                                   min = 4, max = 16, value = 8, step = 0.5),
                                                                shiny::sliderInput("surv_roc_height", "Height (in):",
                                                                                   min = 3, max = 12, value = 5, step = 0.5),
                                                                shiny::hr(),
                                                                shiny::h5("Download"),
                                                                shiny::selectInput("surv_roc_format", "Format:",
                                                                                   choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                                                                   selected = "png"),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.surv_roc_format == 'png'",
                                                                  shiny::numericInput("surv_roc_dpi", "DPI:",
                                                                                      value = 300, min = 100, max = 1000, step = 50)
                                                                ),
                                                                shiny::downloadButton("dl_surv_roc", "Download",
                                                                                      class = "btn-primary w-100")
                                                              )
                                                ),
                                                shiny::column(9,
                                                              shiny::plotOutput("out_plot_roc", height = "500px")
                                                )
                                              )
                              ),
                              shiny::tabPanel("Kaplan-Meier",
                                              shiny::fluidRow(
                                                shiny::column(3,
                                                              shiny::wellPanel(
                                                                shiny::h5("Dimensions"),
                                                                shiny::sliderInput("surv_km_width", "Width (in):",
                                                                                   min = 4, max = 16, value = 8, step = 0.5),
                                                                shiny::sliderInput("surv_km_height", "Height (in):",
                                                                                   min = 3, max = 12, value = 5, step = 0.5),
                                                                shiny::hr(),
                                                                shiny::h5("Download"),
                                                                shiny::selectInput("surv_km_format", "Format:",
                                                                                   choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                                                                   selected = "png"),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.surv_km_format == 'png'",
                                                                  shiny::numericInput("surv_km_dpi", "DPI:",
                                                                                      value = 300, min = 100, max = 1000, step = 50)
                                                                ),
                                                                shiny::downloadButton("dl_surv_km", "Download",
                                                                                      class = "btn-primary w-100")
                                                              )
                                                ),
                                                shiny::column(9,
                                                              shiny::plotOutput("out_plot_km", height = "500px")
                                                )
                                              )
                              ),
                              shiny::tabPanel("Swimmer Plot",
                                              shiny::fluidRow(
                                                shiny::column(3,
                                                              shiny::wellPanel(
                                                                shiny::h5("Dimensions"),
                                                                shiny::sliderInput("surv_swim_width", "Width (in):",
                                                                                   min = 4, max = 16, value = 8, step = 0.5),
                                                                shiny::sliderInput("surv_swim_height", "Height (in):",
                                                                                   min = 3, max = 12, value = 5, step = 0.5),
                                                                shiny::hr(),
                                                                shiny::h5("Download"),
                                                                shiny::selectInput("surv_swim_format", "Format:",
                                                                                   choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                                                                   selected = "png"),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.surv_swim_format == 'png'",
                                                                  shiny::numericInput("surv_swim_dpi", "DPI:",
                                                                                      value = 300, min = 100, max = 1000, step = 50)
                                                                ),
                                                                shiny::downloadButton("dl_surv_swim", "Download",
                                                                                      class = "btn-primary w-100")
                                                              )
                                                ),
                                                shiny::column(9,
                                                              shiny::plotOutput("out_plot_swim", height = "500px")
                                                )
                                              )
                              ),
                              shiny::tabPanel("Clinical Boxplots",
                                              shiny::fluidRow(
                                                shiny::column(3,
                                                              shiny::wellPanel(
                                                                shiny::h5("Dimensions"),
                                                                shiny::sliderInput("surv_clinics_width", "Width (in):",
                                                                                   min = 4, max = 16, value = 8, step = 0.5),
                                                                shiny::sliderInput("surv_clinics_height", "Height (in):",
                                                                                   min = 3, max = 12, value = 5, step = 0.5),
                                                                shiny::hr(),
                                                                shiny::h5("Download"),
                                                                shiny::selectInput("surv_clinics_format", "Format:",
                                                                                   choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                                                                   selected = "png"),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.surv_clinics_format == 'png'",
                                                                  shiny::numericInput("surv_clinics_dpi", "DPI:",
                                                                                      value = 300, min = 100, max = 1000, step = 50)
                                                                ),
                                                                shiny::downloadButton("dl_surv_clinics", "Download",
                                                                                      class = "btn-primary w-100")
                                                              )
                                                ),
                                                shiny::column(9,
                                                              shiny::plotOutput("out_plot_clinics", height = "500px")
                                                )
                                              )
                              ),
                              shiny::tabPanel("Forest Plot",
                                              shiny::fluidRow(
                                                shiny::column(3,
                                                              shiny::wellPanel(
                                                                shiny::h5("Dimensions"),
                                                                shiny::sliderInput("surv_forest_width", "Width (in):",
                                                                                   min = 4, max = 16, value = 8, step = 0.5),
                                                                shiny::sliderInput("surv_forest_height", "Height (in):",
                                                                                   min = 3, max = 12, value = 5, step = 0.5),
                                                                shiny::hr(),
                                                                shiny::h5("Download"),
                                                                shiny::selectInput("surv_forest_format", "Format:",
                                                                                   choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                                                                   selected = "png"),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.surv_forest_format == 'png'",
                                                                  shiny::numericInput("surv_forest_dpi", "DPI:",
                                                                                      value = 300, min = 100, max = 1000, step = 50)
                                                                ),
                                                                shiny::downloadButton("dl_surv_forest", "Download",
                                                                                      class = "btn-primary w-100")
                                                              )
                                                ),
                                                shiny::column(9,
                                                              shiny::plotOutput("out_plot_forest", height = "500px")
                                                )
                                              )
                              ),
                              shiny::tabPanel("Bootstrap Barplot",
                                              shiny::fluidRow(
                                                shiny::column(3,
                                                              shiny::wellPanel(
                                                                shiny::h5("Dimensions"),
                                                                shiny::sliderInput("surv_bar_width", "Width (in):",
                                                                                   min = 4, max = 16, value = 8, step = 0.5),
                                                                shiny::sliderInput("surv_bar_height", "Height (in):",
                                                                                   min = 3, max = 12, value = 5, step = 0.5),
                                                                shiny::hr(),
                                                                shiny::h5("Download"),
                                                                shiny::selectInput("surv_bar_format", "Format:",
                                                                                   choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                                                                   selected = "png"),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.surv_bar_format == 'png'",
                                                                  shiny::numericInput("surv_bar_dpi", "DPI:",
                                                                                      value = 300, min = 100, max = 1000, step = 50)
                                                                ),
                                                                shiny::downloadButton("dl_surv_bar", "Download",
                                                                                      class = "btn-primary w-100")
                                                              )
                                                ),
                                                shiny::column(9,
                                                              shiny::plotOutput("out_plot_bar", height = "500px")
                                                )
                                              )
                              )
                            )
            ),
            shiny::tabPanel("Tables",
                            shiny::tabsetPanel(
                              shiny::tabPanel("Univariate Analysis",
                                              DT::DTOutput("out_tbl_uni")),
                              shiny::tabPanel("Multivariate Analysis",
                                              DT::DTOutput("out_tbl_multi")),
                              shiny::tabPanel("Patient Scores",
                                              DT::DTOutput("out_tbl_score"))
                            )
            )
          )
        )
      )
    ),

    # ================================================================
    # INTEGRATIVE WORKFLOW: COMPLETE PIPELINE
    # ================================================================
    bslib::nav_panel(
      title = "Integrative Workflow: Complete Pipeline",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::h4("1. Data Input"),
          shiny::radioButtons(
            "src_data_comp", tip("Expression Data Source:",
                                 "Select the source of the expression and survival data"),
            choices = c("Use Toy Expression and Survival Data" = "toy", "Upload File (.tsv/.csv)" = "upload"),
            selected = "toy"
          ),
          shiny::conditionalPanel(
            condition = "input.src_data_comp == 'upload'",
            shiny::fileInput("file_data_comp", tip("Upload Expression File:",
                                                   "Upload a TSV/CSV/TXT file with samples as rows and survival columns first"),
                             accept = c(".tsv", ".csv", ".txt")),
            shiny::uiOutput("status_file_data_comp"),
            shiny::helpText("Tip: in some Linux browsers the file dialog may require you to click the file once and then press 'Open' (instead of double-clicking it).")
          ),
          shiny::hr(),
          shiny::h4("2. Multivariate Analysis Options"),
          shiny::checkboxInput("comp_multivariate", tip("Enable Multivariate Cox Analysis",
                                                        "Enables multivariate Cox analysis with clinical covariates"),
                               value = FALSE),
          shiny::conditionalPanel(
            condition = "input.comp_multivariate == true",
            shiny::radioButtons(
              "src_clin_comp", tip("Clinical Data Source:",
                                   "Select the source of the clinical data"),
              choices = c("Use Toy Clinical Data" = "toy", "Upload File (.tsv/.csv)" = "upload"),
              selected = "toy"
            ),
            shiny::conditionalPanel(
              condition = "input.src_clin_comp == 'upload'",
              shiny::fileInput("file_clin_comp", tip("Upload Clinical File:",
                                                     "Upload a TSV/CSV/TXT file with binary clinical covariates"),
                               accept = c(".tsv", ".csv", ".txt")),
              shiny::uiOutput("status_file_clin_comp"),
              shiny::helpText("Tip: in some Linux browsers the file dialog may require you to click the file once and then press 'Open' (instead of double-clicking it).")
            )
          ),
          shiny::hr(),
          shiny::h4("3. Parameters"),
          shiny::textInput("comp_outprefix", tip("Output Prefix:",
                                                 "Prefix used to name every output file generated by the complete pipeline"),
                           value = "clinprog_comp"),
          shiny::checkboxInput("comp_normExp", tip("Log2 Transform Expression",
                                                   "Transforms expression values in input data with log2(expression+1)"),
                               value = FALSE),
          shiny::numericInput("comp_bootstrap", tip("Bootstrap Iterations:",
                                                    "Number of bootstrap iterations (applied to both regression and survival steps)"),
                              value = 10, min = 1, step = 1),
          shiny::numericInput("comp_groupsize", tip("Group Size:",
                                                    "Number of features sampled in each bootstrap iteration"),
                              value = 10, min = 1, step = 1),
          shiny::numericInput("comp_percentagefilter", tip("Correlation Filter Threshold (0-1):",
                                                           "Max proportion of highly correlated feature pairs allowed per iteration (Spearman > 0.80)"),
                              value = 0.3, min = 0, max = 1, step = 0.05),
          shiny::numericInput("comp_variancefilter", tip("Variance Filter Threshold (0-1):",
                                                         "Removes features whose normalized variance is below this threshold"),
                              value = 0.01, min = 0, max = 1, step = 0.005),
          shiny::selectInput("comp_type", tip("Feature Type:",
                                              "Feature type: 'gene' or 'transcript' (affects the internal coefficient cutoff)"),
                             choices = c("gene", "transcript"), selected = "gene"),
          shiny::checkboxInput("comp_use_min_signature", tip("Search for Minimum Signature (Top-k Prefix Search)",
                                                             "Searches the smallest top-k prefix of the signature that still yields log-rank p < 0.05 on the same dataset"),
                               value = FALSE),
          shiny::conditionalPanel(
            condition = "input.comp_use_min_signature == true",
            shiny::numericInput("comp_min_signature_size", tip("Minimum Signature Size (>= 1):",
                                                               "Minimum number of features to start the top-k search"),
                                value = 2, min = 1, step = 1),
            shiny::selectInput("comp_min_signature_cutoff", tip("Cutoff Strategy for Minimum Signature:",
                                                                "Strategy used to split scores into low/high groups during the minimum signature search"),
                               choices = c("median", "mean", "signal", "roc"),
                               selected = "median"),
            shiny::helpText("Finds the smallest top-k prefix of the signature with log-rank p < 0.05 (in-sample heuristic).")
          ),
          shiny::selectInput("comp_score_cutoff", tip("Score Cutoff Strategy:",
                                                      "Strategy used to split patients into low/high score groups (median, mean, signal or ROC)"),
                             choices = c("median", "mean", "signal", "roc"), selected = "median"),
          shiny::numericInput("comp_p_cutoff", tip("Univariate P-value Cutoff:",
                                                   "Univariate p-value threshold to select clinical covariates for multivariate analysis"),
                              value = 0.2, min = 0, max = 1, step = 0.05),
          shiny::uiOutput("ui_comp_followup"),
          shiny::numericInput("comp_seed", tip("Random Seed:",
                                               "Random seed for reproducibility of every stochastic step"),
                              value = 17),
          shiny::checkboxInput("comp_force", tip("Skip Survival Checks",
                                                 "If checked, bypasses survival-related sanity filters (use with caution)"),
                               value = FALSE),
          shiny::hr(),
          shiny::actionButton("btn_run_comp", "Run Complete Workflow", class = "btn-primary w-100"),
          shiny::br(), shiny::br(),
          shiny::downloadButton("dl_comp_zip", "Download All Complete Outputs (.zip)", class = "btn-success w-100")
        ),
        shiny::mainPanel(
          shiny::tabsetPanel(
            shiny::tabPanel("Signature Table",
                            DT::DTOutput("out_table_comp_sig")
            ),
            shiny::tabPanel("Plots",
                            shiny::tabsetPanel(
                              shiny::tabPanel("Histogram",
                                              shiny::fluidRow(
                                                shiny::column(3,
                                                              shiny::wellPanel(
                                                                shiny::h5("Dimensions"),
                                                                shiny::sliderInput("comp_hist_width", "Width (in):",
                                                                                   min = 4, max = 16, value = 8, step = 0.5),
                                                                shiny::sliderInput("comp_hist_height", "Height (in):",
                                                                                   min = 3, max = 12, value = 5, step = 0.5),
                                                                shiny::hr(),
                                                                shiny::h5("Download"),
                                                                shiny::selectInput("comp_hist_format", "Format:",
                                                                                   choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                                                                   selected = "png"),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.comp_hist_format == 'png'",
                                                                  shiny::numericInput("comp_hist_dpi", "DPI:",
                                                                                      value = 300, min = 100, max = 1000, step = 50)
                                                                ),
                                                                shiny::downloadButton("dl_comp_hist", "Download",
                                                                                      class = "btn-primary w-100")
                                                              )
                                                ),
                                                shiny::column(9,
                                                              shiny::plotOutput("out_plot_comp_hist", height = "500px")
                                                )
                                              )
                              ),
                              shiny::tabPanel("Lollipop",
                                              shiny::fluidRow(
                                                shiny::column(3,
                                                              shiny::wellPanel(
                                                                shiny::h5("Dimensions"),
                                                                shiny::sliderInput("comp_lolli_width", "Width (in):",
                                                                                   min = 4, max = 16, value = 8, step = 0.5),
                                                                shiny::sliderInput("comp_lolli_height", "Height (in):",
                                                                                   min = 3, max = 12, value = 5, step = 0.5),
                                                                shiny::hr(),
                                                                shiny::h5("Download"),
                                                                shiny::selectInput("comp_lolli_format", "Format:",
                                                                                   choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                                                                   selected = "png"),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.comp_lolli_format == 'png'",
                                                                  shiny::numericInput("comp_lolli_dpi", "DPI:",
                                                                                      value = 300, min = 100, max = 1000, step = 50)
                                                                ),
                                                                shiny::downloadButton("dl_comp_lolli", "Download",
                                                                                      class = "btn-primary w-100")
                                                              )
                                                ),
                                                shiny::column(9,
                                                              shiny::plotOutput("out_plot_comp_lolli", height = "500px")
                                                )
                                              )
                              ),
                              shiny::tabPanel("Wordcloud",
                                              shiny::fluidRow(
                                                shiny::column(3,
                                                              shiny::wellPanel(
                                                                shiny::h5("Dimensions"),
                                                                shiny::sliderInput("comp_wordcloud_width", "Width (in):",
                                                                                   min = 4, max = 16, value = 8, step = 0.5),
                                                                shiny::sliderInput("comp_wordcloud_height", "Height (in):",
                                                                                   min = 3, max = 12, value = 5, step = 0.5),
                                                                shiny::hr(),
                                                                shiny::h5("Download"),
                                                                shiny::selectInput("comp_wordcloud_format", "Format:",
                                                                                   choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                                                                   selected = "png"),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.comp_wordcloud_format == 'png'",
                                                                  shiny::numericInput("comp_wordcloud_dpi", "DPI:",
                                                                                      value = 300, min = 100, max = 1000, step = 50)
                                                                ),
                                                                shiny::downloadButton("dl_comp_wordcloud", "Download",
                                                                                      class = "btn-primary w-100")
                                                              )
                                                ),
                                                shiny::column(9,
                                                              shiny::plotOutput("out_plot_comp_wordcloud", height = "500px")
                                                )
                                              )
                              ),
                              shiny::tabPanel("Boxplot",
                                              shiny::fluidRow(
                                                shiny::column(3,
                                                              shiny::wellPanel(
                                                                shiny::h5("Dimensions"),
                                                                shiny::sliderInput("comp_box_width", "Width (in):",
                                                                                   min = 4, max = 16, value = 8, step = 0.5),
                                                                shiny::sliderInput("comp_box_height", "Height (in):",
                                                                                   min = 3, max = 12, value = 5, step = 0.5),
                                                                shiny::hr(),
                                                                shiny::h5("Download"),
                                                                shiny::selectInput("comp_box_format", "Format:",
                                                                                   choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                                                                   selected = "png"),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.comp_box_format == 'png'",
                                                                  shiny::numericInput("comp_box_dpi", "DPI:",
                                                                                      value = 300, min = 100, max = 1000, step = 50)
                                                                ),
                                                                shiny::downloadButton("dl_comp_box", "Download",
                                                                                      class = "btn-primary w-100")
                                                              )
                                                ),
                                                shiny::column(9,
                                                              shiny::plotOutput("out_plot_comp_box", height = "500px")
                                                )
                                              )
                              ),
                              shiny::tabPanel("Scatter Plot",
                                              shiny::fluidRow(
                                                shiny::column(3,
                                                              shiny::wellPanel(
                                                                shiny::h5("Dimensions"),
                                                                shiny::sliderInput("comp_scatter_width", "Width (in):",
                                                                                   min = 4, max = 16, value = 8, step = 0.5),
                                                                shiny::sliderInput("comp_scatter_height", "Height (in):",
                                                                                   min = 3, max = 12, value = 5, step = 0.5),
                                                                shiny::hr(),
                                                                shiny::h5("Download"),
                                                                shiny::selectInput("comp_scatter_format", "Format:",
                                                                                   choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                                                                   selected = "png"),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.comp_scatter_format == 'png'",
                                                                  shiny::numericInput("comp_scatter_dpi", "DPI:",
                                                                                      value = 300, min = 100, max = 1000, step = 50)
                                                                ),
                                                                shiny::downloadButton("dl_comp_scatter", "Download",
                                                                                      class = "btn-primary w-100")
                                                              )
                                                ),
                                                shiny::column(9,
                                                              shiny::plotOutput("out_plot_comp_scatter", height = "500px")
                                                )
                                              )
                              ),
                              shiny::tabPanel("Score Bar Plot",
                                              shiny::fluidRow(
                                                shiny::column(3,
                                                              shiny::wellPanel(
                                                                shiny::h5("Dimensions"),
                                                                shiny::sliderInput("comp_score_bar_width", "Width (in):",
                                                                                   min = 4, max = 16, value = 8, step = 0.5),
                                                                shiny::sliderInput("comp_score_bar_height", "Height (in):",
                                                                                   min = 3, max = 12, value = 5, step = 0.5),
                                                                shiny::hr(),
                                                                shiny::h5("Download"),
                                                                shiny::selectInput("comp_score_bar_format", "Format:",
                                                                                   choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                                                                   selected = "png"),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.comp_score_bar_format == 'png'",
                                                                  shiny::numericInput("comp_score_bar_dpi", "DPI:",
                                                                                      value = 300, min = 100, max = 1000, step = 50)
                                                                ),
                                                                shiny::downloadButton("dl_comp_score_bar", "Download",
                                                                                      class = "btn-primary w-100")
                                                              )
                                                ),
                                                shiny::column(9,
                                                              shiny::plotOutput("out_plot_comp_score_bar", height = "500px")
                                                )
                                              )
                              ),
                              shiny::tabPanel("Line Plot",
                                              shiny::fluidRow(
                                                shiny::column(3,
                                                              shiny::wellPanel(
                                                                shiny::h5("Dimensions"),
                                                                shiny::sliderInput("comp_line_width", "Width (in):",
                                                                                   min = 4, max = 16, value = 8, step = 0.5),
                                                                shiny::sliderInput("comp_line_height", "Height (in):",
                                                                                   min = 3, max = 12, value = 5, step = 0.5),
                                                                shiny::hr(),
                                                                shiny::h5("Download"),
                                                                shiny::selectInput("comp_line_format", "Format:",
                                                                                   choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                                                                   selected = "png"),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.comp_line_format == 'png'",
                                                                  shiny::numericInput("comp_line_dpi", "DPI:",
                                                                                      value = 300, min = 100, max = 1000, step = 50)
                                                                ),
                                                                shiny::downloadButton("dl_comp_line", "Download",
                                                                                      class = "btn-primary w-100")
                                                              )
                                                ),
                                                shiny::column(9,
                                                              shiny::plotOutput("out_plot_comp_line", height = "500px")
                                                )
                                              )
                              ),
                              shiny::tabPanel("PH Assumptions Plot",
                                              shiny::fluidRow(
                                                shiny::column(3,
                                                              shiny::wellPanel(
                                                                shiny::h5("Dimensions"),
                                                                shiny::sliderInput("comp_ph_width", "Width (in):",
                                                                                   min = 4, max = 16, value = 8, step = 0.5),
                                                                shiny::sliderInput("comp_ph_height", "Height (in):",
                                                                                   min = 3, max = 12, value = 5, step = 0.5),
                                                                shiny::hr(),
                                                                shiny::h5("Download"),
                                                                shiny::selectInput("comp_ph_format", "Format:",
                                                                                   choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                                                                   selected = "png"),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.comp_ph_format == 'png'",
                                                                  shiny::numericInput("comp_ph_dpi", "DPI:",
                                                                                      value = 300, min = 100, max = 1000, step = 50)
                                                                ),
                                                                shiny::downloadButton("dl_comp_ph", "Download",
                                                                                      class = "btn-primary w-100")
                                                              )
                                                ),
                                                shiny::column(9,
                                                              shiny::plotOutput("out_plot_comp_ph", height = "500px")
                                                )
                                              )
                              ),
                              shiny::tabPanel("ROC Curve",
                                              shiny::fluidRow(
                                                shiny::column(3,
                                                              shiny::wellPanel(
                                                                shiny::h5("Dimensions"),
                                                                shiny::sliderInput("comp_roc_width", "Width (in):",
                                                                                   min = 4, max = 16, value = 8, step = 0.5),
                                                                shiny::sliderInput("comp_roc_height", "Height (in):",
                                                                                   min = 3, max = 12, value = 5, step = 0.5),
                                                                shiny::hr(),
                                                                shiny::h5("Download"),
                                                                shiny::selectInput("comp_roc_format", "Format:",
                                                                                   choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                                                                   selected = "png"),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.comp_roc_format == 'png'",
                                                                  shiny::numericInput("comp_roc_dpi", "DPI:",
                                                                                      value = 300, min = 100, max = 1000, step = 50)
                                                                ),
                                                                shiny::downloadButton("dl_comp_roc", "Download",
                                                                                      class = "btn-primary w-100")
                                                              )
                                                ),
                                                shiny::column(9,
                                                              shiny::plotOutput("out_plot_comp_roc", height = "500px")
                                                )
                                              )
                              ),
                              shiny::tabPanel("Kaplan-Meier",
                                              shiny::fluidRow(
                                                shiny::column(3,
                                                              shiny::wellPanel(
                                                                shiny::h5("Dimensions"),
                                                                shiny::sliderInput("comp_km_width", "Width (in):",
                                                                                   min = 4, max = 16, value = 8, step = 0.5),
                                                                shiny::sliderInput("comp_km_height", "Height (in):",
                                                                                   min = 3, max = 12, value = 5, step = 0.5),
                                                                shiny::hr(),
                                                                shiny::h5("Download"),
                                                                shiny::selectInput("comp_km_format", "Format:",
                                                                                   choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                                                                   selected = "png"),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.comp_km_format == 'png'",
                                                                  shiny::numericInput("comp_km_dpi", "DPI:",
                                                                                      value = 300, min = 100, max = 1000, step = 50)
                                                                ),
                                                                shiny::downloadButton("dl_comp_km", "Download",
                                                                                      class = "btn-primary w-100")
                                                              )
                                                ),
                                                shiny::column(9,
                                                              shiny::plotOutput("out_plot_comp_km", height = "500px")
                                                )
                                              )
                              ),
                              shiny::tabPanel("Swimmer Plot",
                                              shiny::fluidRow(
                                                shiny::column(3,
                                                              shiny::wellPanel(
                                                                shiny::h5("Dimensions"),
                                                                shiny::sliderInput("comp_swim_width", "Width (in):",
                                                                                   min = 4, max = 16, value = 8, step = 0.5),
                                                                shiny::sliderInput("comp_swim_height", "Height (in):",
                                                                                   min = 3, max = 12, value = 5, step = 0.5),
                                                                shiny::hr(),
                                                                shiny::h5("Download"),
                                                                shiny::selectInput("comp_swim_format", "Format:",
                                                                                   choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                                                                   selected = "png"),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.comp_swim_format == 'png'",
                                                                  shiny::numericInput("comp_swim_dpi", "DPI:",
                                                                                      value = 300, min = 100, max = 1000, step = 50)
                                                                ),
                                                                shiny::downloadButton("dl_comp_swim", "Download",
                                                                                      class = "btn-primary w-100")
                                                              )
                                                ),
                                                shiny::column(9,
                                                              shiny::plotOutput("out_plot_comp_swim", height = "500px")
                                                )
                                              )
                              ),
                              shiny::tabPanel("Forest Plot",
                                              shiny::fluidRow(
                                                shiny::column(3,
                                                              shiny::wellPanel(
                                                                shiny::h5("Dimensions"),
                                                                shiny::sliderInput("comp_forest_width", "Width (in):",
                                                                                   min = 4, max = 16, value = 8, step = 0.5),
                                                                shiny::sliderInput("comp_forest_height", "Height (in):",
                                                                                   min = 3, max = 12, value = 5, step = 0.5),
                                                                shiny::hr(),
                                                                shiny::h5("Download"),
                                                                shiny::selectInput("comp_forest_format", "Format:",
                                                                                   choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                                                                   selected = "png"),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.comp_forest_format == 'png'",
                                                                  shiny::numericInput("comp_forest_dpi", "DPI:",
                                                                                      value = 300, min = 100, max = 1000, step = 50)
                                                                ),
                                                                shiny::downloadButton("dl_comp_forest", "Download",
                                                                                      class = "btn-primary w-100")
                                                              )
                                                ),
                                                shiny::column(9,
                                                              shiny::plotOutput("out_plot_comp_forest", height = "500px")
                                                )
                                              )
                              ),
                              shiny::tabPanel("Clinical Boxplots",
                                              shiny::fluidRow(
                                                shiny::column(3,
                                                              shiny::wellPanel(
                                                                shiny::h5("Dimensions"),
                                                                shiny::sliderInput("comp_clinics_width", "Width (in):",
                                                                                   min = 4, max = 16, value = 8, step = 0.5),
                                                                shiny::sliderInput("comp_clinics_height", "Height (in):",
                                                                                   min = 3, max = 12, value = 5, step = 0.5),
                                                                shiny::hr(),
                                                                shiny::h5("Download"),
                                                                shiny::selectInput("comp_clinics_format", "Format:",
                                                                                   choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                                                                   selected = "png"),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.comp_clinics_format == 'png'",
                                                                  shiny::numericInput("comp_clinics_dpi", "DPI:",
                                                                                      value = 300, min = 100, max = 1000, step = 50)
                                                                ),
                                                                shiny::downloadButton("dl_comp_clinics", "Download",
                                                                                      class = "btn-primary w-100")
                                                              )
                                                ),
                                                shiny::column(9,
                                                              shiny::plotOutput("out_plot_comp_clinics", height = "500px")
                                                )
                                              )
                              ),
                              shiny::tabPanel("Bootstrap Barplot",
                                              shiny::fluidRow(
                                                shiny::column(3,
                                                              shiny::wellPanel(
                                                                shiny::h5("Dimensions"),
                                                                shiny::sliderInput("comp_bar_width", "Width (in):",
                                                                                   min = 4, max = 16, value = 8, step = 0.5),
                                                                shiny::sliderInput("comp_bar_height", "Height (in):",
                                                                                   min = 3, max = 12, value = 5, step = 0.5),
                                                                shiny::hr(),
                                                                shiny::h5("Download"),
                                                                shiny::selectInput("comp_bar_format", "Format:",
                                                                                   choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                                                                                   selected = "png"),
                                                                shiny::conditionalPanel(
                                                                  condition = "input.comp_bar_format == 'png'",
                                                                  shiny::numericInput("comp_bar_dpi", "DPI:",
                                                                                      value = 300, min = 100, max = 1000, step = 50)
                                                                ),
                                                                shiny::downloadButton("dl_comp_bar", "Download",
                                                                                      class = "btn-primary w-100")
                                                              )
                                                ),
                                                shiny::column(9,
                                                              shiny::plotOutput("out_plot_comp_bar", height = "500px")
                                                )
                                              )
                              )
                            )
            ),
            shiny::tabPanel("Tables",
                            shiny::tabsetPanel(
                              shiny::tabPanel("Univariate Analysis",
                                              DT::DTOutput("out_comp_tbl_uni")),
                              shiny::tabPanel("Multivariate Analysis",
                                              DT::DTOutput("out_comp_tbl_multi")),
                              shiny::tabPanel("Patient Scores",
                                              DT::DTOutput("out_comp_tbl_score"))
                            )
            )
          )
        )
      )
    ),

    # ================================================================
    # ABOUT
    # ================================================================
    bslib::nav_panel(
      title = "About",
      shiny::tabsetPanel(
        # ---------- Overview ----------
        shiny::tabPanel(
          "Overview",
          shiny::div(
            class = "about-section",
            shiny::tags$h3("Overview"),
            shiny::tags$p(
              "clinprog is a flexible, easy-to-use algorithm to identify and validate genes or transcripts signatures whose expression are highly correlated with patient survival. This tool innovates by using a multivariate strategy with penalized Cox regression (Lasso method) combined with a bootstrap approach, presenting robust convergence of the regression procedure, and a variety of statistical tests for signature score. clinprog comprises two modules developed in R (version 4.0.4). The regression module provides functionality for obtaining gene/transcript signatures from a given set of samples. In turn, the survival module provides functionality for producing, applying and validating a score, which is calculated based on the obtained signature, in patient datasets. In this module, a different set of samples may be provided for validation purposes, and clinical variables may also be taken into account. Moreover, clinprog also has the execution option complete, which integratively executes the two aforementioned modules."
            ),
            shiny::div(
              style = "text-align: center; margin-top: 25px;",
              shiny::tags$img(
                src = {
                  wp <- system.file("figures/reboot_workflow.png", package = "clinprog")
                  if (wp == "" || !file.exists(wp)) wp <- "inst/figures/reboot_workflow.png"
                  if (file.exists(wp)) base64enc::dataURI(file = wp, mime = "image/png") else ""
                },
                style = "max-width: 100%; height: auto; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                alt = "clinprog workflow overview"
              )
            )
          )
        ),
        # ---------- Versions ----------
        shiny::tabPanel(
          "Versions",
          shiny::div(
            class = "about-section",
            shiny::tags$h3("Versions"),
            shiny::uiOutput("about_version")
          )
        ),
        # ---------- News ----------
        shiny::tabPanel(
          "News",
          shiny::div(
            class = "about-section",
            shiny::tags$h3("News"),
            shiny::uiOutput("about_news")
          )
        ),
        # ---------- FAQ ----------
        shiny::tabPanel(
          "FAQ",
          shiny::div(
            class = "about-section",
            shiny::tags$h3("Frequently Asked Questions"),
            bslib::accordion(
              open = FALSE,
              bslib::accordion_panel(
                "1. What input formats does clinprog accept, and when is each one required?",
                shiny::tags$p("clinprog accepts tab-separated (.tsv), comma-separated (.csv), or plain text (.txt) files. For the regression module, only the expression/survival file is required. For the survival module, a signature file is also needed (which can be generated by Module I or uploaded). For multivariate analysis, an additional clinical file is required with binary covariates.")
              ),
              bslib::accordion_panel(
                "2. How does the signature file work across modules?",
                shiny::tags$p("In Module I (Regression), clinprog generates a signature automatically (features + coefficients). In Module II (Survival) and in the Complete Pipeline, you can either upload a signature file manually (TSV with 'feature' and 'coefficient' columns) or reuse the one generated in Module I, which is cached in the current session.")
              ),
              bslib::accordion_panel(
                "3. What is the difference between 'gene' and 'transcript'?",
                shiny::tags$p("This parameter defines the internal coefficient cutoff used during bootstrap filtering. 'gene' uses a cutoff of 0.0035 and 'transcript' uses 0.011, reflecting different noise profiles of gene-level versus transcript-level expression data. Choose 'gene' if your input columns are gene symbols, and 'transcript' if they are transcript identifiers (e.g., ENST IDs).")
              ),
              bslib::accordion_panel(
                "4. How many bootstrap iterations should I use?",
                shiny::tags$p("For quick exploration, 10-100 iterations are usually enough. For robust signatures, 500-1000+ iterations are recommended. Higher values substantially increase runtime. The bootstrap step is the main bottleneck of Module I.")
              ),
              bslib::accordion_panel(
                "5. What is the 'Search for Minimum Signature' (top-k prefix search) algorithm?",
                shiny::tags$p("When enabled, clinprog ranks features by decreasing absolute coefficient and tests progressively larger prefixes (top-k) of the full signature. For each prefix, it computes a clinprog score and fits a univariate Cox model. The search stops at the smallest k whose log-rank p-value is below 0.05. This is an in-sample heuristic - the p-value is optimistic and should not be treated as formal validation.")
              ),
              bslib::accordion_panel(
                "6. What is the ROC curve cutoff strategy?",
                shiny::tags$p("The ROC strategy uses a time-dependent ROC curve (via survivalROC) to determine the optimal score cutoff that maximizes the Youden index (sensitivity + specificity - 1). Patients above the cutoff are classified as 'high', below as 'low'. This is more robust than median/mean when the score distribution is skewed or when the event rate is imbalanced.")
              ),
              bslib::accordion_panel(
                "7. Why is my analysis taking so long?",
                shiny::tags$p("Runtime is dominated by: (1) the number of bootstrap iterations, (2) the size of the dataset (samples x features), (3) whether the minimum signature search is enabled (fits multiple Cox models sequentially), and (4) whether multivariate analysis with bootstrap is enabled. For exploratory runs, reduce bootstrap and groupsize.")
              ),
              bslib::accordion_panel(
                "8. How do I download results, tables, and plots?",
                shiny::tags$p("Every table and plot has its own download button in the corresponding tab. Additionally, each module has a 'Download All ... (.zip)' button at the bottom of the sidebar, which packages all outputs into a single ZIP archive for convenience.")
              ),
              bslib::accordion_panel(
                "9. How do I cite clinprog?",
                shiny::tags$p("clinprog builds on the Reboot method. Please cite the original publication: Reboot: a straightforward approach to identify genes and splicing isoforms associated with cancer patient prognosis. Felipe R. C. dos Santos*, Gabriela D. A. Guardia*, Filipe F. dos Santos*, Daniel T. Ohara, and Pedro A. F. Galante. DOI: 10.1093/narcan/zcab024. PMID: 34316711. The full citation is also always visible in the app's footer.")
              ),
              bslib::accordion_panel(
                "10. Can I upload my own data?",
                shiny::tags$p("Yes. Every module allows you to switch from the toy datasets to your own files. Upload expression/survival files in TSV/CSV/TXT format with samples as rows, survival columns (OS, OS.time) first, and features afterwards. For multivariate analysis, an additional clinical file with binary covariates is required.")
              ),
              bslib::accordion_panel(
                "11. How do I report bugs or get in touch?",
                shiny::tags$p("Please open an issue on our GitHub repository: ", shiny::tags$a(href = "https://github.com/galantelab/clinprog/issues", target = "_blank", "github.com/galantelab/clinprog/issues"), ". For general questions or collaboration, visit the lab website: ", shiny::tags$a(href = "https://www.bioinfo.mochsl.org.br/", target = "_blank", "bioinfo.mochsl.org.br"), ".")
              )
            )
          )
        ),
        # ---------- Authors ----------
        shiny::tabPanel(
          "Authors",
          shiny::div(
            class = "about-section",
            shiny::tags$h3("Authors"),
            shiny::fluidRow(
              shiny::column(3, author_card(
                "Filipe Ferreira dos Santos",
                "filipe.jpg",
                "Hello, science enthusiasts! I hold a Bachelor's degree in Biological Sciences (UNIFESP), Master's in Bioinformatics (Health Sciences) at Hospital Sirio-Libanes and Ph.D in Biochemistry and Molecular Biology (IQ-USP). I had the opportunity to complete a sandwich undergraduate program in Bioinformatics at the University of Cincinnati (Science without Borders) and a research exchange at UCSD, both in the USA. As a postdoctoral researcher (Young Scientist) at the Galante's lab, I love studying the molecular genetics of cancer and infectious diseases, using OMICs data, biostatistics, and programming to develop tools and databases. Beyond DNA and genes, I also love beach and (e-)sports, barbecues (and more) with family and friends, reading, and being around animals. Welcome!"
              )),
              shiny::column(3, author_card(
                "Felipe Rodolfo Camargo dos Santos",
                "felipe.jpg",
                "Over 10 years of experience in computational biology. Currently a postdoctoral associate at the Broad Institute of MIT and Harvard developing cutting-edge pipelines to analyze single-cell morphology through high-content imaging under chemical perturbations with genetic anchoring, applied to metabolic diseases. Ph.D in Computational Biology (USP), in collaboration with Hospital Sirio-Libanes, investigating the mechanisms of cancer progression and lethality in human patients, focusing on protein-coding genes, isoforms, and non-coding transcripts. Undergraduate at USP, with a 1-year exchange at the University of Melbourne through the 'Science Without Borders' program. Proficient in R and Python, with experience in cloud computing, machine learning, high-performance computing, and containerization tools. Beyond science, I am a strong advocate for climate change awareness, environmental sustainability, and animal equality."
              )),
              shiny::column(3, author_card(
                "Gabriela Der Agopian Guardia",
                "gabriela.jpg",
                "Ph.D in Bioinformatics (USP). Currently dedicated to studying the impacts of alternative splicing on tumorigenesis at the Molecular Oncology Center of the Sirio-Libanes Institute of Teaching and Research."
              )),
              shiny::column(3, author_card(
                "Thiago Luiz Araujo Miller",
                "thiago.jpg",
                "Bachelor's degree in Biotechnology Engineering (UNESP - Assis campus) and a Ph.D in Sciences (IQ-USP). Currently a postdoctoral researcher at the Galante's lab."
              ))
            ),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(3, author_card(
                "Matheus Pinheiro Moratto de Castro",
                "matheus.jpg",
                "Ph.D student (IQ-USP). Bachelor of Biomedical Sciences at the Institute of Biomedical Sciences (ICB-USP) with 3 scientific initiations: (i) Galante's lab at the Research and Teaching Institute of Hospital Sirio-Libanes (retrocopies in the genome of patients with cancer); (ii-iii) Eukaryotic Gene Expression Regulation laboratory at ICB-USP (search for new drugs and their effects on the regulation of gene expression and splicing of tumor cells). Has experience in Genetics, Molecular Biology and Bioinformatics, with emphasis on Human and Medical Genetics and Eukaryotic Gene Expression Regulation."
              )),
              shiny::column(3, author_card(
                "Daniel Takatori Ohara",
                "daniel.jpg",
                "I work with cloud computing on AWS administering the account of the Institute of Research at Hospital Sirio-Libanes. I am a software developer, server administrator (Linux: Fedora, CentOS, Ubuntu), and network administrator. My main interests lie in Linux server administration, PHP development, and relational database design with MySQL. My technical expertise spans multiple operating systems (Linux, Windows, macOS), programming languages (PHP, PHP-Gtk, Perl, Python, HTML, JavaScript, Java, C), databases (MySQL, PostgreSQL, Interbase, Firebird, Informix, Oracle), and frameworks (CakePHP, Django)."
              )),
              shiny::column(3, author_card(
                "Pedro Alexandre Favoretto Galante",
                "pedro.jpg",
                "I am a scientist! My academic background began with a Bachelor's degree in Molecular Sciences (USP) and a Ph.D in Biochemistry and Molecular Biology (USP). I then completed a postdoctoral fellowship at Harvard University (USA). I served as a Full Researcher at the Ludwig Institute for Cancer Research and was a board member of the Brazilian Association for Bioinformatics and Computational Biology. I am currently a Senior Researcher at the Sirio-Libanes Institute of Teaching and Research, where I also serve as Associate Coordinator of the Graduate Program in Health Sciences at Hospital Sirio-Libanes. I am also passionate about cycling (both MTB and road) and physical activity in general. My daily routine is to lead this fantastic group (Galante's lab) of exceptional scientists across our many research projects - such as the one presented here."
              ))
            )
          )
        )
      )
    ),

    # ================================================================
    # HEADER - JavaScript injected via header parameter (NOT as direct child)
    # ================================================================
    header = shiny::tags$head(
      shiny::tags$script(shiny::HTML("
    $(document).on('shiny:connected', function() {
      // State management for each plot
      var plotStates = {};

      // Initialize state for a plot element
      function initPlotState(el) {
        var id = el.id;
        if (!plotStates[id]) {
          plotStates[id] = {
            scale: 1,
            translateX: 0,
            translateY: 0,
            isDragging: false,
            startX: 0,
            startY: 0,
            lastTranslateX: 0,
            lastTranslateY: 0
          };
        }
        return plotStates[id];
      }

      // Apply complete transform (scale + translate)
      function applyTransform(el) {
        var id = el.id;
        var state = plotStates[id];
        if (!state) return;

        var transform = 'translate(' + state.translateX + 'px, ' + state.translateY + 'px) scale(' + state.scale + ')';
        el.style.transform = transform;
        el.style.transformOrigin = 'center center';

        // Change cursor based on zoom state
        if (state.scale > 1.05) {
          el.style.cursor = 'grab';
        } else {
          el.style.cursor = 'default';
        }
      }

      // Zoom event with mouse-centered zoom
      document.addEventListener('wheel', function(e) {
        var target = e.target;
        var plotElement = $(target).closest('.shiny-plot-output');

        if (plotElement.length > 0) {
          e.preventDefault();
          e.stopPropagation();

          var el = plotElement[0];
          var state = initPlotState(el);

          // Calculate mouse position relative to plot
          var rect = el.getBoundingClientRect();
          var mouseX = e.clientX - rect.left;
          var mouseY = e.clientY - rect.top;

          // Center of image relative to current viewport
          var centerX = rect.width / 2;
          var centerY = rect.height / 2;

          // Zoom factor
          var delta = e.deltaY > 0 ? -0.1 : 0.1;
          var oldScale = state.scale;
          var newScale = Math.min(Math.max(state.scale + delta, 0.5), 3);

          if (newScale === oldScale) return;

          // Calculate new translate to keep point under mouse fixed
          var scaleRatio = newScale / oldScale;
          state.translateX = state.translateX + (mouseX - centerX) * (1 - scaleRatio);
          state.translateY = state.translateY + (mouseY - centerY) * (1 - scaleRatio);
          state.scale = newScale;

          applyTransform(el);
        }
      }, { passive: false, capture: true });

      // Mouse events for drag (pan)
      $(document).on('mousedown', '.shiny-plot-output', function(e) {
        var el = this;
        var state = initPlotState(el);

        // Only activate drag if zoomed enough
        if (state.scale <= 1.05) return;

        state.isDragging = true;
        state.startX = e.clientX;
        state.startY = e.clientY;
        state.lastTranslateX = state.translateX;
        state.lastTranslateY = state.translateY;
        el.style.cursor = 'grabbing';
        e.preventDefault();
      });

      $(document).on('mousemove', function(e) {
        // Check if any plot is being dragged
        for (var id in plotStates) {
          var state = plotStates[id];
          if (state.isDragging) {
            var el = document.getElementById(id);
            if (!el) continue;

            var dx = e.clientX - state.startX;
            var dy = e.clientY - state.startY;

            state.translateX = state.lastTranslateX + dx;
            state.translateY = state.lastTranslateY + dy;

            applyTransform(el);
          }
        }
      });

      $(document).on('mouseup', function(e) {
        for (var id in plotStates) {
          var state = plotStates[id];
          if (state.isDragging) {
            state.isDragging = false;
            var el = document.getElementById(id);
            if (el) {
              el.style.cursor = (state.scale > 1.05) ? 'grab' : 'default';
            }
          }
        }
      });

      // Reset zoom and pan on double click
      $(document).on('dblclick', '.shiny-plot-output', function(e) {
        var el = this;
        var state = initPlotState(el);
        state.scale = 1;
        state.translateX = 0;
        state.translateY = 0;
        applyTransform(el);
      });

      // Custom message handler for reset button
      Shiny.addCustomMessageHandler('resetZoom', function(message) {
        $('.shiny-plot-output').each(function() {
          var el = this;
          var state = initPlotState(el);
          state.scale = 1;
          state.translateX = 0;
          state.translateY = 0;
          applyTransform(el);
        });
      });
    });
  "))
    ),

    # ================================================================
    # FOOTER - Two-row layout: citation panel + quote/Reset Zoom
    # ================================================================
    footer = shiny::tags$footer(
      style = "
    position: fixed;
    bottom: 0;
    left: 0;
    right: 0;
    background: transparent;
    border-top: none;
    padding: 0;
    display: flex;
    flex-direction: column;
    z-index: 100;
    box-shadow: 0 -2px 5px rgba(0,0,0,0.05);
  ",
      shiny::div(
        class = "clinprog-citation-panel",
        shiny::HTML(
          paste0(
            "<strong>Citation:</strong> Reboot: a straightforward approach to identify genes and splicing isoforms associated with cancer patient prognosis. ",
            "Felipe R. C. dos Santos*, Gabriela D. A. Guardia*, Filipe F. dos Santos*, Daniel T. Ohara, and Pedro A. F. Galante. ",
            "DOI: <a href='https://academic.oup.com/narcancer/article/3/2/zcab024/6299998' target='_blank'>10.1093/narcan/zcab024</a>. ",
            "PMID: <a href='https://pubmed.ncbi.nlm.nih.gov/34316711/' target='_blank'>34316711</a>. ",
            "| <a href='https://galantelab.github.io/clinprog/' target='_blank'><strong>Documentation</strong></a>"
          )
        )
      ),
      shiny::div(
        style = "
      background: linear-gradient(to right, #f8f9fa, #e9ecef);
      border-top: 1px solid #dee2e6;
      padding: 2px 15px;
      display: flex;
      justify-content: space-between;
      align-items: center;
      font-size: 10px;
      color: #6c757d;
      font-style: italic;
      height: 24px;
      overflow: hidden;
      white-space: nowrap;
      text-overflow: ellipsis;
    ",
        shiny::div(
          style = "display: flex; align-items: center; gap: 15px; width: 100%;",
          shiny::uiOutput("inspirational_quote"),
          shiny::actionButton(
            "reset_zoom_btn",
            "Reset Zoom",
            class = "btn btn-secondary btn-xs",
            style = "pointer-events: auto; font-size: 10px; padding: 2px 8px; margin: 0; height: 22px; line-height: 1; flex-shrink: 0;"
          )
        )
      )
    )
  )
}
