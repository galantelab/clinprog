#' clinprog Application Server Backend
#'
#' @title Server Logic for the clinprog Shiny Application
#'
#' @description
#' Implements the reactive server-side processing for the clinprog interactive suite.
#' Manages dynamic data inputs, computational triggers for Modules I, II, and III,
#' interactive output table rendering, figure displays, and automated download handlers
#' for tables, plots, and compressed ZIP archives.
#'
#' @param input Standard Shiny input object containing UI controls.
#' @param output Standard Shiny output object for sending plots, tables, and UI to client.
#' @param session Standard Shiny session object managing client environment state.
#'
#' @importFrom rlang .data
#' @export
app_server <- function(input, output, session) {

  # Helper function for DPI rounding - fixed: rounds to nearest 50, min 100, max 1000
  round_dpi <- function(val) {
    if (is.null(val) || !is.numeric(val)) return(val)
    # Clamp to valid range
    val <- max(100, min(1000, val))
    # Round to nearest 50
    return(round(val / 50) * 50)
  }

  # Convert any object strictly to a pure R data.frame
  as_strict_dataframe <- function(x) {
    if (is.null(x)) return(NULL)
    df <- as.data.frame(x, stringsAsFactors = FALSE)
    class(df) <- "data.frame"
    return(df)
  }

  # Simulated toy tables bundled with the package. Genes and transcripts share the
  # same patients and survival data, so either one can be used for follow-up limits
  get_toy_expression <- function(feature_type = "gene") {
    toy <- if (identical(feature_type, "transcript")) {
      clinprog::toy_transcript_expression
    } else {
      clinprog::toy_gene_expression
    }
    as_strict_dataframe(toy)
  }

  # Chooses the toy table (genes or transcripts) that contains the signature features
  toy_type_for_signature <- function(sig) {
    if (any(sig$feature %in% colnames(clinprog::toy_transcript_expression))) "transcript" else "gene"
  }

  load_table_data <- function(file_input) {
    if (is.null(file_input)) return(NULL)
    df <- read_table(file_input$datapath, sep = "	", row_names = TRUE)
    return(as_strict_dataframe(df))
  }

  # Save plot in multiple formats (PNG, PDF, SVG) with custom dimensions
  save_plot_format <- function(plot_obj, file_path, format = "png",
                               width = 8, height = 6, dpi = 300) {
    if (is.null(plot_obj)) return(NULL)

    if (format == "png") {
      grDevices::png(file_path, width = width, height = height, units = "in", res = dpi)
    } else if (format == "pdf") {
      grDevices::pdf(file_path, width = width, height = height)
    } else if (format == "svg") {
      if (requireNamespace("svglite", quietly = TRUE)) {
        svglite::svglite(file_path, width = width, height = height)
      } else {
        shiny::showNotification(
          "Package 'svglite' is required for SVG export. Please install it with: install.packages('svglite')",
          type = "warning",
          duration = 10
        )
        grDevices::png(file_path, width = width, height = height, units = "in", res = dpi)
      }
    } else {
      stop("Format not supported. Use 'png', 'pdf' or 'svg'.")
    }

    # Plot the object
    if (inherits(plot_obj, "grob") || inherits(plot_obj, "ggplot") || inherits(plot_obj, "ggcoxzph")) {
      print(plot_obj)
    } else if (is.list(plot_obj) && !is.null(plot_obj$plot)) {
      print(plot_obj$plot)
    } else {
      try(print(plot_obj), silent = TRUE)
    }

    grDevices::dev.off()
  }

  # Reconstruct ROC ggplot following Quarto output specifications
  build_roc_ggplot <- function(surv_obj) {
    if (is.null(surv_obj)) return(NULL)

    s_data <- if (!is.null(surv_obj$survival) && is.list(surv_obj$survival) && !is.null(surv_obj$survival$roc_result)) {
      surv_obj$survival
    } else {
      surv_obj
    }

    if (!is.null(s_data) && !is.null(s_data$roc_result) && !is.null(s_data$roc_result$optimal_cutpoint$data)) {
      roc_data <- s_data$roc_result$optimal_cutpoint$data

      roc_obj <- pROC::roc(
        response = roc_data$OS,
        predictor = roc_data$score,
        direction = s_data$roc_result$direction,
        quiet = TRUE
      )

      roc_df <- data.frame(
        Specificity = rev(roc_obj$specificities),
        Sensitivity = rev(roc_obj$sensitivities)
      )

      roc_plot <- ggplot2::ggplot(roc_df, ggplot2::aes(x = 1 - .data$Specificity, y = .data$Sensitivity)) +
        ggplot2::geom_line(linewidth = 0.5, color = "blue") +
        ggplot2::geom_abline(slope = 1, intercept = 0, color = "gray") +
        ggplot2::coord_equal() +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          panel.grid = ggplot2::element_blank(),
          axis.line = ggplot2::element_line(color = "black"),
          axis.ticks = ggplot2::element_line(color = "black")
        ) +
        ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
        ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
        ggplot2::labs(
          title = NULL,
          subtitle = paste0("AUC = ", round(as.numeric(s_data$roc_result$auc), 3)),
          x = "1 - Specificity",
          y = "Sensitivity"
        )

      return(roc_plot)
    }

    return(NULL)
  }

  get_max_followup <- function(src, file_input) {
    df <- if (src == "toy") {
      get_toy_expression("gene")
    } else {
      if (is.null(file_input)) return(3650)
      load_table_data(file_input)
    }
    if (!is.null(df) && "OS.time" %in% colnames(df)) {
      return(as.numeric(max(df$OS.time, na.rm = TRUE)))
    }
    return(3650)
  }

  render_clinprog_dt <- function(df_data_expr, first_col_name = NULL, fix_multivariate_score = FALSE,
                               options = NULL, extensions = NULL, filename_base = NULL) {
    DT::renderDT(
      server = FALSE,
      {
        raw_val <- tryCatch(df_data_expr(), error = function(e) NULL)

        # If no data, show message
        if (is.null(raw_val)) {
          return(DT::datatable(
            data.frame(Status = "No results available. Please run the analysis first."),
            rownames = FALSE,
            options = list(dom = 't', pageLength = 1)
          ))
        }

        df <- as_strict_dataframe(raw_val)

        if (is.null(df) || nrow(df) == 0) {
          return(DT::datatable(
            data.frame(Status = "No results available."),
            rownames = FALSE,
            options = list(dom = 't', pageLength = 1)
          ))
        }

        if (fix_multivariate_score && "variable" %in% colnames(df)) {
          df$variable <- gsub("^score_group$", "score", df$variable)
        }

        if (!is.null(first_col_name)) {
          rnames <- rownames(raw_val)
          if (!is.null(rnames) && !all(rnames == as.character(seq_len(nrow(raw_val))))) {
            df <- cbind(stats::setNames(data.frame(rnames), first_col_name), df)
          }
        }

        # Determine the base filename for downloads
        base_filename <- if (!is.null(filename_base)) {
          filename_base
        } else {
          "clinprog_data"
        }

        # Default options for all tables with Buttons
        default_options <- list(
          pageLength = 10,
          scrollX = TRUE,
          autoWidth = TRUE,
          dom = 'Blfrtip',
          lengthMenu = list(c(10, 25, 50), c("10", "25", "50")),
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          ),
          buttons = list(
            list(extend = 'copy', text = 'Copy'),
            list(extend = 'csv', text = 'CSV', filename = base_filename),
            list(extend = 'csv', text = 'TSV', fieldSeparator = '\t', extension = '.tsv', filename = base_filename),
            list(extend = 'excel', text = 'Excel', filename = base_filename),
            list(extend = 'print', text = 'Print')
          )
        )

        # Merge with user-provided options if any
        final_options <- if (!is.null(options)) {
          utils::modifyList(default_options, options)
        } else {
          default_options
        }

        # Handle extensions properly - ONLY include if not NULL
        final_extensions <- NULL
        if (!is.null(extensions)) {
          if (is.character(extensions)) {
            final_extensions <- extensions
          } else if (is.list(extensions)) {
            if (is.null(names(extensions))) {
              final_extensions <- unlist(extensions)
            } else {
              final_extensions <- extensions
            }
          }
        } else {
          # Default: include Buttons extension for all tables
          final_extensions <- 'Buttons'
        }

        # Create datatable with extensions only if present
        dt <- if (!is.null(final_extensions)) {
          DT::datatable(
            df,
            rownames = FALSE,
            extensions = final_extensions,
            options = final_options,
            class = "cell-border stripe hover compact",
            filter = 'top'
          )
        } else {
          DT::datatable(
            df,
            rownames = FALSE,
            options = final_options,
            class = "cell-border stripe hover compact",
            filter = 'top'
          )
        }

        # Only apply formatRound if there are numeric columns
        numeric_cols <- which(vapply(df, is.numeric, FUN.VALUE = logical(1)))
        if (length(numeric_cols) > 0) {
          dt <- DT::formatRound(dt, columns = numeric_cols, digits = 4)
        }

        dt
      })
  }

  # -------------------------------------------------------------------
  # FILE UPLOAD STATUS INDICATORS
  # -------------------------------------------------------------------
  # Render a small text below each fileInput showing whether the upload
  # completed or is still pending.
  make_file_status <- function(file_input) {
    if (is.null(file_input)) {
      "No file selected yet."
    } else {
      paste0("File uploaded: ", file_input$name,
             " (", round(file_input$size / 1024, 1), " KB). Ready to run.")
    }
  }

  output$status_file_data_reg <- shiny::renderUI({
    shiny::tags$span(class = "file-status-text", make_file_status(input$file_data_reg))
  })

  output$status_file_data_surv <- shiny::renderUI({
    shiny::tags$span(class = "file-status-text", make_file_status(input$file_data_surv))
  })

  output$status_file_sig_surv <- shiny::renderUI({
    shiny::tags$span(class = "file-status-text", make_file_status(input$file_sig_surv))
  })

  output$status_file_clin_surv <- shiny::renderUI({
    shiny::tags$span(class = "file-status-text", make_file_status(input$file_clin_surv))
  })

  output$status_file_data_comp <- shiny::renderUI({
    shiny::tags$span(class = "file-status-text", make_file_status(input$file_data_comp))
  })

  output$status_file_clin_comp <- shiny::renderUI({
    shiny::tags$span(class = "file-status-text", make_file_status(input$file_clin_comp))
  })

  output$ui_reg_followup <- shiny::renderUI({
    max_val <- get_max_followup(input$src_data_reg, input$file_data_reg)
    shiny::numericInput("reg_followup",
                        shiny::tags$span("Follow-up Limit (time unit):",
                                         title = "Maximum follow-up time; values above are administratively censored"),
                        value = max_val, min = 1, step = 10)
  })

  output$ui_surv_followup <- shiny::renderUI({
    max_val <- get_max_followup(input$src_data_surv, input$file_data_surv)
    shiny::numericInput("surv_followup",
                        shiny::tags$span("Follow-up Limit (time unit):",
                                         title = "Maximum follow-up time; values above are administratively censored"),
                        value = max_val, min = 1, step = 10)
  })

  output$ui_comp_followup <- shiny::renderUI({
    max_val <- get_max_followup(input$src_data_comp, input$file_data_comp)
    shiny::numericInput("comp_followup",
                        shiny::tags$span("Follow-up Limit (time unit):",
                                         title = "Maximum follow-up time; values above are administratively censored"),
                        value = max_val, min = 1, step = 10)
  })

  module1_signature <- shiny::reactiveVal(NULL)

  # -------------------------------------------------------------------
  # INSPIRATIONAL QUOTES
  # -------------------------------------------------------------------
  inspirational_quotes <- list(
    "The important thing is not to stop questioning. Curiosity has its own reason for existing." = "Albert Einstein",
    "Science is a way of thinking much more than it is a body of knowledge." = "Carl Sagan",
    "The most beautiful thing we can experience is the mysterious. It is the source of all true art and science." = "Albert Einstein",
    "Nothing in life is to be feared, it is only to be understood. Now is the time to understand more, so that we may fear less." = "Marie Curie",
    "The greatest enemy of knowledge is not ignorance, it is the illusion of knowledge." = "Stephen Hawking",
    "Science knows no country, because knowledge belongs to humanity, and it is the torch which illuminates the world." = "Louis Pasteur",
    "What we know is a drop, what we don't know is an ocean." = "Isaac Newton",
    "The scientist is not a person who gives the right answers, he's one who asks the right questions." = "Claude Levi-Strauss",
    "In the middle of difficulty lies opportunity." = "Albert Einstein",
    "It is not the strongest of the species that survives, nor the most intelligent, but the one most responsive to change." = "Charles Darwin",
    "The most incomprehensible thing about the world is that it is comprehensible." = "Albert Einstein",
    "Science without religion is lame, religion without science is blind." = "Albert Einstein",
    "The universe is under no obligation to make sense to you." = "Neil deGrasse Tyson",
    "Somewhere, something incredible is waiting to be known." = "Carl Sagan",
    "We are a way for the cosmos to know itself." = "Carl Sagan",
    "The good thing about science is that it's true whether or not you believe in it." = "Neil deGrasse Tyson",
    "Discovery consists of seeing what everybody has seen and thinking what nobody has thought." = "Albert Szent-Gyorgyi",
    "The most beautiful experience we can have is the mysterious. It is the fundamental emotion that stands at the cradle of true art and true science." = "Albert Einstein",
    "Nature uses only the longest threads to weave her patterns, so that each small piece of her fabric reveals the organization of the entire tapestry." = "Richard Feynman",
    "For a successful technology, reality must take precedence over public relations, for nature cannot be fooled." = "Richard Feynman",
    "A scientist is a person who never stops asking why." = "Barbara McClintock",
    "If you know what you are doing, you are not learning." = "Barbara McClintock",
    "The best way to have a good idea is to have lots of ideas." = "Linus Pauling",
    "Satisfaction of one's curiosity is one of the greatest sources of happiness in life." = "Linus Pauling",
    "The only thing I know is that I know nothing." = "Socrates",
    "The more I learn, the more I realize how much I don't know." = "Albert Einstein"
  )

  current_quote <- shiny::reactiveVal()

  shiny::observe({
    if (is.null(current_quote())) {
      quote_text <- sample(names(inspirational_quotes), 1)
      current_quote(list(text = quote_text, author = inspirational_quotes[[quote_text]]))
    }
  })

  shiny::observe({
    shiny::invalidateLater(45000)
    quote_text <- sample(names(inspirational_quotes), 1)
    current_quote(list(text = quote_text, author = inspirational_quotes[[quote_text]]))
  })

  output$inspirational_quote <- shiny::renderUI({
    shiny::req(current_quote())
    shiny::tags$span(
      style = "display: flex; align-items: center; gap: 8px; pointer-events: none; font-size: 11px;",
      shiny::tags$span("\U0001F9E0", style = "font-size: 14px; font-style: normal;"),
      shiny::tags$span(
        paste0('"', current_quote()$text, '" \u2014 ', current_quote()$author),
        style = "color: #6c757d;"
      )
    )
  })

  # -------------------------------------------------------------------
  # ABOUT - Versions (dynamic)
  # -------------------------------------------------------------------
  output$about_version <- shiny::renderUI({
    pkg_version <- tryCatch(
      as.character(utils::packageVersion("clinprog")),
      error = function(e) "unknown"
    )

    shiny::tagList(
      shiny::tags$p(
        shiny::tags$strong("Current version: "), pkg_version
      ),
      shiny::tags$p(
        shiny::tags$strong("Initial release: "), "2026-09-25"
      ),
      shiny::tags$hr(),
      shiny::tags$p(
        shiny::tags$em("Release notes for each version are listed in the News tab.")
      )
    )
  })

  # -------------------------------------------------------------------
  # ABOUT - News (dynamic from NEWS.md)
  # -------------------------------------------------------------------
  output$about_news <- shiny::renderUI({
    news_text <- NULL

    # Strategy 1: NEWS.md shipped with the installed package
    news_path <- system.file("NEWS.md", package = "clinprog")

    # Strategy 2: local dev path (devtools::load_all)
    if (news_path == "" || !file.exists(news_path)) {
      local_path <- "NEWS.md"
      if (file.exists(local_path)) news_path <- local_path
    }

    if (news_path != "" && file.exists(news_path)) {
      news_lines <- readLines(news_path, warn = FALSE)
      # Remove empty lines at start/end
      news_lines <- news_lines[!grepl("^\\s*$", news_lines)]

      if (length(news_lines) > 0) {
        return(shiny::tags$div(
          shiny::tags$p(
            shiny::tags$em("Extracted from the package NEWS.md file:")
          ),
          shiny::tags$pre(
            style = "background-color: #f7f7f7; padding: 12px; border-radius: 6px; font-size: 12px; line-height: 1.5; white-space: pre-wrap; max-height: 500px; overflow-y: auto;",
            paste(news_lines, collapse = "\n")
          )
        ))
      }
    }

    # Fallback
    shiny::tags$div(
      shiny::tags$p("No NEWS.md file was found for this package version."),
      shiny::tags$p(
        "For the latest release notes and changelog, please visit the ",
        shiny::tags$a(href = "https://github.com/galantelab/clinprog", target = "_blank", "GitHub repository"),
        "."
      )
    )
  })

  # -------------------------------------------------------------------
  # MODULE I SERVER LOGIC
  # -------------------------------------------------------------------

  # Option A: warn user the moment they enable the minimum signature search
  shiny::observeEvent(input$reg_use_min_signature, {
    if (isTRUE(input$reg_use_min_signature)) {
      shiny::showNotification(
        "Minimum signature search enabled. This may significantly increase runtime, as multiple Cox models will be fitted sequentially.",
        type = "warning",
        duration = 8
      )
    }
  }, ignoreInit = TRUE)

  res_reg <- shiny::eventReactive(input$btn_run_reg, {
    shiny::req(input$src_data_reg)

    # Notify the user that the analysis is about to start (with spinner).
    notify_id <- shiny::showNotification(
      shiny::tagList(
        shiny::tags$div(
          style = "display: flex; align-items: center; gap: 10px;",
          shiny::tags$div(class = "clinprog-spinner"),
          shiny::tags$span("Analysis starting... please wait. Do not click Run again.")
        )
      ),
      type = "message", duration = NULL, closeButton = TRUE
    )
    on.exit(shiny::removeNotification(notify_id), add = TRUE)

    # Option B: persistent notification when minimum signature search is active (no spinner)
    min_notify_id <- NULL
    if (isTRUE(input$reg_use_min_signature)) {
      min_notify_id <- shiny::showNotification(
        "Minimum signature search is enabled. The analysis is still running, please be patient.",
        type = "warning",
        duration = NULL,
        closeButton = TRUE
      )
      on.exit(shiny::removeNotification(min_notify_id), add = TRUE)
    }

    full_data <- if (input$src_data_reg == "toy") {
      get_toy_expression(input$reg_type)
    } else {
      shiny::req(input$file_data_reg)
      load_table_data(input$file_data_reg)
    }

    shiny::validate(shiny::need(is.data.frame(full_data) && nrow(full_data) > 0, "Invalid expression dataset."))

    res <- NULL
    shiny::withProgress(message = "Executing Module I (Regression)...", value = 0.3, {
      tryCatch({
        res <- tempfile() |> (\(td) {
          dir.create(td)
          withr::with_dir(td, {
            run_regression(
              data = full_data,
              output_prefix = input$reg_outprefix,
              norm_exp = input$reg_normExp,
              bootstrap = input$reg_bootstrap,
              group_size = input$reg_groupsize,
              percentage_filter = input$reg_percentagefilter,
              variance_filter = input$reg_variancefilter,
              feature_type = input$reg_type,
              min_signature_size = if (isTRUE(input$reg_use_min_signature)) input$reg_min_signature_size else NULL,
              min_signature_cutoff = if (isTRUE(input$reg_use_min_signature)) input$reg_min_signature_cutoff else NULL,
              max_follow_up = input$reg_followup,
              skip_survival_checks = input$reg_force,
              save_plots = TRUE,
              save_table = FALSE,
              save_json = FALSE,
              save_rds = FALSE,
              report_format = FALSE,
              verbose = 1,
              seed = input$reg_seed,
              save_log = FALSE
            )
          })
        })()
        shiny::incProgress(0.7, detail = "Done!")
        shiny::showNotification(
          shiny::tags$div(class = "clinprog-notification-success", "Module I executed successfully!"),
          type = NULL
        )
      }, error = function(e) {
        shiny::showNotification(paste("Error in Module I:", e$message), type = "error", duration = 10)
      })
    })

    if (!is.null(res)) {
      module1_signature(as_strict_dataframe(res$signature))
    }
    return(res)
  })

  output$out_table_reg_sig <- render_clinprog_dt(
    function() {
      shiny::req(res_reg())
      df <- as_strict_dataframe(res_reg()$signature)
      df <- cbind(Feature = rownames(df), df)
      rownames(df) <- NULL
      df
    },
    first_col_name = NULL,
    options = list(
      pageLength = 15,
      scrollX = TRUE,
      autoWidth = TRUE,
      dom = 'Blfrtip',
      lengthMenu = list(c(10, 15, 25, 50), c("10", "15", "25", "50")),
      columnDefs = list(
        list(className = 'dt-center', targets = '_all'),
        list(width = '200px', targets = 0)
      ),
      buttons = list(
        list(extend = 'copy', text = 'Copy'),
        list(extend = 'csv', text = 'CSV', filename = 'clinprog_signature'),
        list(extend = 'csv', text = 'TSV', fieldSeparator = '\t', extension = '.tsv', filename = 'clinprog_signature'),
        list(extend = 'excel', text = 'Excel', filename = 'clinprog_signature'),
        list(extend = 'print', text = 'Print')
      )
    ),
    extensions = 'Buttons',
    filename_base = "clinprog_signature"
  )

  # Module I - Histogram
  output$out_plot_reg_hist <- shiny::renderPlot({
    shiny::req(res_reg())
    res_reg()$plots$hist_plot
  }, width = function() { input$reg_hist_width * 100 },
  height = function() { input$reg_hist_height * 100 })

  # DPI rounding for Histogram - fixed: rounds to nearest 50, min 100, max 1000
  shiny::observeEvent(input$reg_hist_dpi, {
    new_val <- round_dpi(input$reg_hist_dpi)
    if (!is.null(new_val) && new_val != input$reg_hist_dpi) {
      shiny::updateNumericInput(session, "reg_hist_dpi", value = new_val)
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  output$dl_reg_hist <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$reg_hist_format,
                    "png" = "png",
                    "pdf" = "pdf",
                    "svg" = "svg")
      paste0(input$reg_outprefix, "_histogram.", ext)
    },
    content = function(file) {
      shiny::req(res_reg())
      save_plot_format(
        res_reg()$plots$hist_plot,
        file,
        format = input$reg_hist_format,
        width = input$reg_hist_width,
        height = input$reg_hist_height,
        dpi = if (input$reg_hist_format == "png") input$reg_hist_dpi else 300
      )
    }
  )

  # Module I - Lollipop
  output$out_plot_reg_lolli <- shiny::renderPlot({
    shiny::req(res_reg())
    res_reg()$plots$lolli_plot
  }, width = function() { input$reg_lolli_width * 100 },
  height = function() { input$reg_lolli_height * 100 })

  # DPI rounding for Lollipop - fixed: rounds to nearest 50, min 100, max 1000
  shiny::observeEvent(input$reg_lolli_dpi, {
    new_val <- round_dpi(input$reg_lolli_dpi)
    if (!is.null(new_val) && new_val != input$reg_lolli_dpi) {
      shiny::updateNumericInput(session, "reg_lolli_dpi", value = new_val)
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  output$dl_reg_lolli <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$reg_lolli_format,
                    "png" = "png",
                    "pdf" = "pdf",
                    "svg" = "svg")
      paste0(input$reg_outprefix, "_lollipop.", ext)
    },
    content = function(file) {
      shiny::req(res_reg())
      save_plot_format(
        res_reg()$plots$lolli_plot,
        file,
        format = input$reg_lolli_format,
        width = input$reg_lolli_width,
        height = input$reg_lolli_height,
        dpi = if (input$reg_lolli_format == "png") input$reg_lolli_dpi else 300
      )
    }
  )

  output$dl_reg_zip <- shiny::downloadHandler(
    filename = function() { paste0(input$reg_outprefix, "_module1_all.zip") },
    content = function(file) {
      tmpdir <- tempfile()
      dir.create(tmpdir)
      res <- res_reg()
      utils::write.table(res$signature, file.path(tmpdir, "signature.tsv"), sep="\t", quote=FALSE, row.names=TRUE)
      save_plot_format(res$plots$hist_plot, file.path(tmpdir, "hist_plot.png"), format = "png", width = 8, height = 6, dpi = 300)
      save_plot_format(res$plots$lolli_plot, file.path(tmpdir, "lolli_plot.png"), format = "png", width = 8, height = 6, dpi = 300)
      zip::zip(zipfile = file, files = dir(tmpdir, full.names = TRUE), mode = "cherry-pick")
    }
  )

  # -------------------------------------------------------------------
  # MODULE II SERVER LOGIC
  # -------------------------------------------------------------------
  res_surv <- shiny::eventReactive(input$btn_run_surv, {
    shiny::req(input$src_data_surv)

    # Notify the user that the analysis is about to start (with spinner).
    notify_id <- shiny::showNotification(
      shiny::tagList(
        shiny::tags$div(
          style = "display: flex; align-items: center; gap: 10px;",
          shiny::tags$div(class = "clinprog-spinner"),
          shiny::tags$span("Analysis starting... please wait. Do not click Run again.")
        )
      ),
      type = "message", duration = NULL, closeButton = TRUE
    )
    on.exit(shiny::removeNotification(notify_id), add = TRUE)

    # Strictly coerce primary data matrix
    full_data <- if (input$src_data_surv == "toy") {
      df <- get_toy_expression("gene")
      class(df) <- "data.frame"
      df
    } else {
      shiny::req(input$file_data_surv)
      df <- load_table_data(input$file_data_surv)
      class(df) <- "data.frame"
      df
    }

    # Safety check
    if (!is.data.frame(full_data)) {
      full_data <- as.data.frame(full_data, stringsAsFactors = FALSE)
      class(full_data) <- "data.frame"
    }

    if (!all(c("OS", "OS.time") %in% colnames(full_data))) {
      shiny::showNotification("Data must contain 'OS' and 'OS.time' columns.", type = "error")
      shiny::validate(shiny::need(FALSE, "Data must contain 'OS' and 'OS.time' columns."))
    }

    shiny::validate(shiny::need(is.data.frame(full_data) && nrow(full_data) > 0, "Invalid expression dataset."))

    sig_in <- if (!is.null(input$file_sig_surv)) {
      df <- as_strict_dataframe(read_table(input$file_sig_surv$datapath, row_names = FALSE))
      class(df) <- "data.frame"
      df
    } else if (!is.null(module1_signature())) {
      df <- as_strict_dataframe(module1_signature())
      class(df) <- "data.frame"
      df
    } else {
      shiny::validate(shiny::need(FALSE, "Please upload a signature file or run Module I first."))
    }

    if (!all(c("feature", "coefficient") %in% colnames(sig_in))) {
      shiny::showNotification("Signature must contain 'feature' and 'coefficient' columns.", type = "error")
      shiny::validate(shiny::need(FALSE, "Signature must contain 'feature' and 'coefficient' columns."))
    }

    shiny::validate(shiny::need(is.data.frame(sig_in) && nrow(sig_in) > 0, "Invalid gene signature."))

    # Toy data: uses the simulated table (genes or transcripts) that matches the signature
    if (identical(input$src_data_surv, "toy")) {
      full_data <- get_toy_expression(toy_type_for_signature(sig_in))
      class(full_data) <- "data.frame"
    }

    is_multi <- isTRUE(input$surv_multivariate)

    clin_in <- NULL
    if (is_multi) {
      shiny::req(input$src_clin_surv)
      clin_in <- if (input$src_clin_surv == "toy") {
        df <- as_strict_dataframe(clinprog::toy_clinics)
        class(df) <- "data.frame"
        df
      } else {
        shiny::req(input$file_clin_surv)
        df <- load_table_data(input$file_clin_surv)
        class(df) <- "data.frame"
        df
      }
      shiny::validate(shiny::need(is.data.frame(clin_in) && nrow(clin_in) > 0, "Invalid clinical dataset for multivariate analysis."))
    }

    res <- NULL
    shiny::withProgress(message = "Executing Module II (Survival)...", value = 0.3, {
      tryCatch({
        res <- tempfile() |> (\(td) {
          dir.create(td)
          withr::with_dir(td, {

            # Prepare arguments list
            args_list <- list(
              data = full_data,
              signature = sig_in,
              output_prefix = input$surv_outprefix,
              norm_exp = input$surv_normExp,
              run_multivariate = is_multi,
              score_cutoff = input$surv_score_cutoff,
              variance_filter = input$surv_variancefilter,
              p_value_cutoff = input$surv_p_cutoff,
              bootstrap = input$surv_bootstrap,
              max_follow_up = input$surv_followup,
              skip_survival_checks = input$surv_force,
              save_plots = TRUE,
              save_table = FALSE,
              save_json = FALSE,
              save_rds = FALSE,
              report_format = FALSE,
              verbose = 1,
              seed = input$surv_seed,
              save_log = FALSE,
              clinical_data = if (is_multi && !is.null(clin_in)) clin_in else NULL
            )

            do.call(run_survival, args_list)
          })
        })()
        shiny::incProgress(0.7, detail = "Done!")
        shiny::showNotification(
          shiny::tags$div(class = "clinprog-notification-success", "Module II executed successfully!"),
          type = NULL
        )
      }, error = function(e) {
        shiny::showNotification(paste("Error in Module II:", e$message), type = "error", duration = 10)
      })
    })

    return(res)
  })

  # Module II plots - all with dimension and format support

  # Helper function to create DPI observer for Module II - fixed
  observe_dpi_ii <- function(input_id) {
    shiny::observeEvent(input[[input_id]], {
      new_val <- round_dpi(input[[input_id]])
      if (!is.null(new_val) && new_val != input[[input_id]]) {
        shiny::updateNumericInput(session, input_id, value = new_val)
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  }

  # Apply DPI rounding to all Module II plots
  observe_dpi_ii("surv_km_dpi")
  observe_dpi_ii("surv_forest_dpi")
  observe_dpi_ii("surv_box_dpi")
  observe_dpi_ii("surv_scatter_dpi")
  observe_dpi_ii("surv_score_bar_dpi")
  observe_dpi_ii("surv_line_dpi")
  observe_dpi_ii("surv_wordcloud_dpi")
  observe_dpi_ii("surv_ph_dpi")
  observe_dpi_ii("surv_roc_dpi")
  observe_dpi_ii("surv_swim_dpi")
  observe_dpi_ii("surv_clinics_dpi")
  observe_dpi_ii("surv_bar_dpi")

  # Module II - Wordcloud
  output$out_plot_wordcloud <- shiny::renderPlot({
    shiny::req(res_surv())
    res_surv()$plots$wordcloud_plot
  }, width = function() { input$surv_wordcloud_width * 100 },
  height = function() { input$surv_wordcloud_height * 100 })

  output$dl_surv_wordcloud <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$surv_wordcloud_format,
                    "png" = "png",
                    "pdf" = "pdf",
                    "svg" = "svg")
      paste0("wordcloud_plot.", ext)
    },
    content = function(f) {
      shiny::req(res_surv())
      save_plot_format(
        res_surv()$plots$wordcloud_plot,
        f,
        format = input$surv_wordcloud_format,
        width = input$surv_wordcloud_width,
        height = input$surv_wordcloud_height,
        dpi = if (input$surv_wordcloud_format == "png") input$surv_wordcloud_dpi else 300
      )
    }
  )

  # Module II - Boxplot
  output$out_plot_box <- shiny::renderPlot({
    shiny::req(res_surv())
    res_surv()$plots$box_plot
  }, width = function() { input$surv_box_width * 100 },
  height = function() { input$surv_box_height * 100 })

  output$dl_surv_box <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$surv_box_format,
                    "png" = "png",
                    "pdf" = "pdf",
                    "svg" = "svg")
      paste0("box_plot.", ext)
    },
    content = function(f) {
      shiny::req(res_surv())
      save_plot_format(
        res_surv()$plots$box_plot,
        f,
        format = input$surv_box_format,
        width = input$surv_box_width,
        height = input$surv_box_height,
        dpi = if (input$surv_box_format == "png") input$surv_box_dpi else 300
      )
    }
  )

  # Module II - Scatter Plot
  output$out_plot_scatter <- shiny::renderPlot({
    shiny::req(res_surv())
    res_surv()$plots$scatter_plot
  }, width = function() { input$surv_scatter_width * 100 },
  height = function() { input$surv_scatter_height * 100 })

  output$dl_surv_scatter <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$surv_scatter_format,
                    "png" = "png",
                    "pdf" = "pdf",
                    "svg" = "svg")
      paste0("scatter_plot.", ext)
    },
    content = function(f) {
      shiny::req(res_surv())
      save_plot_format(
        res_surv()$plots$scatter_plot,
        f,
        format = input$surv_scatter_format,
        width = input$surv_scatter_width,
        height = input$surv_scatter_height,
        dpi = if (input$surv_scatter_format == "png") input$surv_scatter_dpi else 300
      )
    }
  )

  # Module II - Score Bar Plot
  output$out_plot_score_bar <- shiny::renderPlot({
    shiny::req(res_surv())
    res_surv()$plots$score_bar_plot
  }, width = function() { input$surv_score_bar_width * 100 },
  height = function() { input$surv_score_bar_height * 100 })

  output$dl_surv_score_bar <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$surv_score_bar_format,
                    "png" = "png",
                    "pdf" = "pdf",
                    "svg" = "svg")
      paste0("score_bar_plot.", ext)
    },
    content = function(f) {
      shiny::req(res_surv())
      save_plot_format(
        res_surv()$plots$score_bar_plot,
        f,
        format = input$surv_score_bar_format,
        width = input$surv_score_bar_width,
        height = input$surv_score_bar_height,
        dpi = if (input$surv_score_bar_format == "png") input$surv_score_bar_dpi else 300
      )
    }
  )

  # Module II - Line Plot
  output$out_plot_line <- shiny::renderPlot({
    shiny::req(res_surv())
    res_surv()$plots$line_plot
  }, width = function() { input$surv_line_width * 100 },
  height = function() { input$surv_line_height * 100 })

  output$dl_surv_line <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$surv_line_format,
                    "png" = "png",
                    "pdf" = "pdf",
                    "svg" = "svg")
      paste0("line_plot.", ext)
    },
    content = function(f) {
      shiny::req(res_surv())
      save_plot_format(
        res_surv()$plots$line_plot,
        f,
        format = input$surv_line_format,
        width = input$surv_line_width,
        height = input$surv_line_height,
        dpi = if (input$surv_line_format == "png") input$surv_line_dpi else 300
      )
    }
  )

  # Module II - PH Assumptions Plot
  output$out_plot_ph <- shiny::renderPlot({
    shiny::req(res_surv())
    res_surv()$plots$ph_plot
  }, width = function() { input$surv_ph_width * 100 },
  height = function() { input$surv_ph_height * 100 })

  output$dl_surv_ph <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$surv_ph_format,
                    "png" = "png",
                    "pdf" = "pdf",
                    "svg" = "svg")
      paste0("ph_plot.", ext)
    },
    content = function(f) {
      shiny::req(res_surv())
      save_plot_format(
        res_surv()$plots$ph_plot,
        f,
        format = input$surv_ph_format,
        width = input$surv_ph_width,
        height = input$surv_ph_height,
        dpi = if (input$surv_ph_format == "png") input$surv_ph_dpi else 300
      )
    }
  )

  # Module II - ROC Curve
  output$out_plot_roc <- shiny::renderPlot({
    res <- res_surv()
    shiny::req(res)
    roc_plot <- build_roc_ggplot(res)
    if (is.null(roc_plot)) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "ROC Curve is only generated when\n'Score Cutoff Strategy' is set to 'roc'.", cex = 1.2, col = "gray40")
    } else {
      print(roc_plot)
    }
  }, width = function() { input$surv_roc_width * 100 },
  height = function() { input$surv_roc_height * 100 })

  output$dl_surv_roc <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$surv_roc_format,
                    "png" = "png",
                    "pdf" = "pdf",
                    "svg" = "svg")
      paste0("roc_plot.", ext)
    },
    content = function(f) {
      shiny::req(res_surv())
      save_plot_format(
        build_roc_ggplot(res_surv()),
        f,
        format = input$surv_roc_format,
        width = input$surv_roc_width,
        height = input$surv_roc_height,
        dpi = if (input$surv_roc_format == "png") input$surv_roc_dpi else 300
      )
    }
  )

  # Module II - Kaplan-Meier
  output$out_plot_km <- shiny::renderPlot({
    shiny::req(res_surv())
    res_surv()$plots$km_plot
  }, width = function() { input$surv_km_width * 100 },
  height = function() { input$surv_km_height * 100 })

  output$dl_surv_km <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$surv_km_format,
                    "png" = "png",
                    "pdf" = "pdf",
                    "svg" = "svg")
      paste0("km_plot.", ext)
    },
    content = function(f) {
      shiny::req(res_surv())
      save_plot_format(
        res_surv()$plots$km_plot,
        f,
        format = input$surv_km_format,
        width = input$surv_km_width,
        height = input$surv_km_height,
        dpi = if (input$surv_km_format == "png") input$surv_km_dpi else 300
      )
    }
  )

  # Module II - Swimmer Plot (always generated)
  output$out_plot_swim <- shiny::renderPlot({
    shiny::req(res_surv())
    res_surv()$plots$swim_plot
  }, width = function() { input$surv_swim_width * 100 },
  height = function() { input$surv_swim_height * 100 })

  output$dl_surv_swim <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$surv_swim_format,
                    "png" = "png",
                    "pdf" = "pdf",
                    "svg" = "svg")
      paste0("swimmer_plot.", ext)
    },
    content = function(f) {
      shiny::req(res_surv())
      save_plot_format(
        res_surv()$plots$swim_plot,
        f,
        format = input$surv_swim_format,
        width = input$surv_swim_width,
        height = input$surv_swim_height,
        dpi = if (input$surv_swim_format == "png") input$surv_swim_dpi else 300
      )
    }
  )

  # Module II - Clinical Boxplots (only if multivariate)
  output$out_plot_clinics <- shiny::renderPlot({
    shiny::req(res_surv())
    p <- res_surv()$plots$clinics_box_plot
    if (is.null(p)) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "Clinical boxplots are only generated when\nmultivariate analysis is enabled and succeeds.", cex = 1.2, col = "gray40")
    } else {
      print(p)
    }
  }, width = function() { input$surv_clinics_width * 100 },
  height = function() { input$surv_clinics_height * 100 })

  output$dl_surv_clinics <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$surv_clinics_format,
                    "png" = "png",
                    "pdf" = "pdf",
                    "svg" = "svg")
      paste0("clinical_boxplots.", ext)
    },
    content = function(f) {
      shiny::req(res_surv())
      p <- res_surv()$plots$clinics_box_plot
      if (!is.null(p)) {
        save_plot_format(
          p,
          f,
          format = input$surv_clinics_format,
          width = input$surv_clinics_width,
          height = input$surv_clinics_height,
          dpi = if (input$surv_clinics_format == "png") input$surv_clinics_dpi else 300
        )
      }
    }
  )

  # Module II - Forest Plot (only if multivariate)
  output$out_plot_forest <- shiny::renderPlot({
    shiny::req(res_surv())
    p <- res_surv()$plots$forest_plot
    if (is.null(p)) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "Forest plot is only generated when\nmultivariate analysis is enabled and succeeds.", cex = 1.2, col = "gray40")
    } else {
      print(p)
    }
  }, width = function() { input$surv_forest_width * 100 },
  height = function() { input$surv_forest_height * 100 })

  output$dl_surv_forest <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$surv_forest_format,
                    "png" = "png",
                    "pdf" = "pdf",
                    "svg" = "svg")
      paste0("forest_plot.", ext)
    },
    content = function(f) {
      shiny::req(res_surv())
      p <- res_surv()$plots$forest_plot
      if (!is.null(p)) {
        save_plot_format(
          p,
          f,
          format = input$surv_forest_format,
          width = input$surv_forest_width,
          height = input$surv_forest_height,
          dpi = if (input$surv_forest_format == "png") input$surv_forest_dpi else 300
        )
      }
    }
  )

  # Module II - Bootstrap Barplot (only if multivariate + bootstrap succeeded)
  output$out_plot_bar <- shiny::renderPlot({
    shiny::req(res_surv())
    p <- res_surv()$plots$bar_plot
    if (is.null(p)) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "Bootstrap barplot is only generated when multivariate\nanalysis with successful bootstrap resampling is performed.", cex = 1.2, col = "gray40")
    } else {
      print(p)
    }
  }, width = function() { input$surv_bar_width * 100 },
  height = function() { input$surv_bar_height * 100 })

  output$dl_surv_bar <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$surv_bar_format,
                    "png" = "png",
                    "pdf" = "pdf",
                    "svg" = "svg")
      paste0("bootstrap_barplot.", ext)
    },
    content = function(f) {
      shiny::req(res_surv())
      p <- res_surv()$plots$bar_plot
      if (!is.null(p)) {
        save_plot_format(
          p,
          f,
          format = input$surv_bar_format,
          width = input$surv_bar_width,
          height = input$surv_bar_height,
          dpi = if (input$surv_bar_format == "png") input$surv_bar_dpi else 300
        )
      }
    }
  )

  # Module II Tables - fixed pagination with server-side processing
  output$out_tbl_uni <- render_clinprog_dt(
    function() {
      shiny::req(res_surv())
      res_surv()$survival$univariate$table
    },
    first_col_name = NULL,
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      autoWidth = TRUE,
      dom = 'Blfrtip',
      lengthMenu = list(c(10, 25, 50), c("10", "25", "50")),
      buttons = list(
        list(extend = 'copy', text = 'Copy'),
        list(extend = 'csv', text = 'CSV', filename = 'univariate_results'),
        list(extend = 'csv', text = 'TSV', fieldSeparator = '\t', extension = '.tsv', filename = 'univariate_results'),
        list(extend = 'excel', text = 'Excel', filename = 'univariate_results'),
        list(extend = 'print', text = 'Print')
      )
    ),
    filename_base = "univariate_results"
  )

  output$out_tbl_multi <- render_clinprog_dt(
    function() {
      shiny::req(res_surv())
      res_surv()$survival$multivariate$table
    },
    first_col_name = NULL,
    fix_multivariate_score = TRUE,
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      autoWidth = TRUE,
      dom = 'Blfrtip',
      lengthMenu = list(c(10, 25, 50), c("10", "25", "50")),
      buttons = list(
        list(extend = 'copy', text = 'Copy'),
        list(extend = 'csv', text = 'CSV', filename = 'multivariate_results'),
        list(extend = 'csv', text = 'TSV', fieldSeparator = '\t', extension = '.tsv', filename = 'multivariate_results'),
        list(extend = 'excel', text = 'Excel', filename = 'multivariate_results'),
        list(extend = 'print', text = 'Print')
      )
    ),
    filename_base = "multivariate_results"
  )

  output$out_tbl_score <- render_clinprog_dt(
    function() {
      shiny::req(res_surv())
      res <- res_surv()
      if (!is.null(res$clinical_data) && nrow(res$clinical_data) > 0) res$clinical_data else res$expression_data
    },
    first_col_name = "Samples",
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      autoWidth = TRUE,
      dom = 'Blfrtip',
      lengthMenu = list(c(10, 25, 50), c("10", "25", "50")),
      buttons = list(
        list(extend = 'copy', text = 'Copy'),
        list(extend = 'csv', text = 'CSV', filename = 'patient_scores'),
        list(extend = 'csv', text = 'TSV', fieldSeparator = '\t', extension = '.tsv', filename = 'patient_scores'),
        list(extend = 'excel', text = 'Excel', filename = 'patient_scores'),
        list(extend = 'print', text = 'Print')
      )
    ),
    filename_base = "patient_scores"
  )

  output$dl_surv_zip <- shiny::downloadHandler(
    filename = function() { paste0(input$surv_outprefix, "_module2_all.zip") },
    content = function(file) {
      tmpdir <- tempfile()
      dir.create(tmpdir)
      res <- res_surv()

      for (pname in names(res$plots)) {
        if (!is.null(res$plots[[pname]])) {
          p_obj <- if (pname == "roc_plot") build_roc_ggplot(res) else res$plots[[pname]]
          if (!is.null(p_obj)) {
            save_plot_format(p_obj, file.path(tmpdir, paste0(pname, ".png")), format = "png", width = 8, height = 6, dpi = 300)
          }
        }
      }
      if (!is.null(res$survival$univariate$table)) {
        utils::write.table(res$survival$univariate$table, file.path(tmpdir, "univariate_results.tsv"), sep="\t", quote=FALSE, row.names=TRUE)
      }
      if (!is.null(res$survival$multivariate$table)) {
        utils::write.table(res$survival$multivariate$table, file.path(tmpdir, "multivariate_results.tsv"), sep="\t", quote=FALSE, row.names=TRUE)
      }
      tb_score <- if (!is.null(res$clinical_data) && nrow(res$clinical_data) > 0) res$clinical_data else res$expression_data
      if (!is.null(tb_score)) {
        utils::write.table(tb_score, file.path(tmpdir, "patient_scores.tsv"), sep="\t", quote=FALSE, row.names=TRUE)
      }

      zip::zip(zipfile = file, files = dir(tmpdir, full.names = TRUE), mode = "cherry-pick")
    }
  )

  # -------------------------------------------------------------------
  # COMPLETE PIPELINE SERVER LOGIC
  # -------------------------------------------------------------------

  # Option A: warn user the moment they enable the minimum signature search
  shiny::observeEvent(input$comp_use_min_signature, {
    if (isTRUE(input$comp_use_min_signature)) {
      shiny::showNotification(
        "Minimum signature search enabled. This may significantly increase runtime, as multiple Cox models will be fitted sequentially.",
        type = "warning",
        duration = 8
      )
    }
  }, ignoreInit = TRUE)

  res_comp <- shiny::eventReactive(input$btn_run_comp, {
    shiny::req(input$src_data_comp)

    # Notify the user that the analysis is about to start (with spinner).
    notify_id <- shiny::showNotification(
      shiny::tagList(
        shiny::tags$div(
          style = "display: flex; align-items: center; gap: 10px;",
          shiny::tags$div(class = "clinprog-spinner"),
          shiny::tags$span("Analysis starting... please wait. Do not click Run again.")
        )
      ),
      type = "message", duration = NULL, closeButton = TRUE
    )
    on.exit(shiny::removeNotification(notify_id), add = TRUE)

    # Option B: persistent notification when minimum signature search is active (no spinner)
    min_notify_id <- NULL
    if (isTRUE(input$comp_use_min_signature)) {
      min_notify_id <- shiny::showNotification(
        "Minimum signature search is enabled. The analysis is still running, please be patient.",
        type = "warning",
        duration = NULL,
        closeButton = TRUE
      )
      on.exit(shiny::removeNotification(min_notify_id), add = TRUE)
    }

    full_data <- if (input$src_data_comp == "toy") {
      df <- get_toy_expression(input$comp_type)
      class(df) <- "data.frame"
      df
    } else {
      shiny::req(input$file_data_comp)
      df <- load_table_data(input$file_data_comp)
      class(df) <- "data.frame"
      df
    }

    if (!is.data.frame(full_data)) {
      full_data <- as.data.frame(full_data, stringsAsFactors = FALSE)
      class(full_data) <- "data.frame"
    }

    if (!all(c("OS", "OS.time") %in% colnames(full_data))) {
      shiny::showNotification("Data must contain 'OS' and 'OS.time' columns.", type = "error")
      shiny::validate(shiny::need(FALSE, "Data must contain 'OS' and 'OS.time' columns."))
    }

    shiny::validate(shiny::need(is.data.frame(full_data) && nrow(full_data) > 0, "Invalid expression dataset."))

    is_multi <- isTRUE(input$comp_multivariate)

    clin_in <- NULL
    if (is_multi) {
      shiny::req(input$src_clin_comp)
      clin_in <- if (input$src_clin_comp == "toy") {
        df <- as_strict_dataframe(clinprog::toy_clinics)
        class(df) <- "data.frame"
        df
      } else {
        shiny::req(input$file_clin_comp)
        df <- load_table_data(input$file_clin_comp)
        class(df) <- "data.frame"
        df
      }
      shiny::validate(shiny::need(is.data.frame(clin_in) && nrow(clin_in) > 0, "Invalid clinical dataset for multivariate analysis."))
    }

    res <- NULL
    shiny::withProgress(message = "Executing Complete Pipeline...", value = 0.2, {
      tryCatch({
        res <- tempfile() |> (\(td) {
          dir.create(td)
          withr::with_dir(td, {
            args_list <- list(
              data = full_data,
              output_prefix = input$comp_outprefix,
              norm_exp = input$comp_normExp,
              bootstrap = input$comp_bootstrap,
              group_size = input$comp_groupsize,
              percentage_filter = input$comp_percentagefilter,
              variance_filter = input$comp_variancefilter,
              feature_type = input$comp_type,
              min_signature_size = if (isTRUE(input$comp_use_min_signature)) input$comp_min_signature_size else NULL,
              min_signature_cutoff = if (isTRUE(input$comp_use_min_signature)) input$comp_min_signature_cutoff else NULL,
              run_multivariate = is_multi,
              score_cutoff = input$comp_score_cutoff,
              p_value_cutoff = input$comp_p_cutoff,
              max_follow_up = input$comp_followup,
              skip_survival_checks = input$comp_force,
              save_plots = TRUE,
              save_table = FALSE,
              save_json = FALSE,
              save_rds = FALSE,
              report_format = FALSE,
              verbose = 1,
              seed = input$comp_seed,
              save_log = FALSE,
              clinical_data = if (is_multi && !is.null(clin_in)) clin_in else NULL
            )

            do.call(run_complete, args_list)
          })
        })()
        shiny::incProgress(0.8, detail = "Done!")
        shiny::showNotification(
          shiny::tags$div(class = "clinprog-notification-success", "Complete Pipeline executed successfully!"),
          type = NULL
        )
      }, error = function(e) {
        shiny::showNotification(paste("Error in Complete Pipeline:", e$message), type = "error", duration = 10)
      })
    })

    return(res)
  })

  # Complete Pipeline - Signature Table
  output$out_table_comp_sig <- render_clinprog_dt(
    function() {
      shiny::req(res_comp())
      df <- as_strict_dataframe(res_comp()$regression$signature)
      df <- cbind(Feature = rownames(df), df)
      rownames(df) <- NULL
      df
    },
    first_col_name = NULL,
    options = list(
      pageLength = 15,
      scrollX = TRUE,
      autoWidth = TRUE,
      dom = 'Blfrtip',
      lengthMenu = list(c(10, 15, 25, 50), c("10", "15", "25", "50")),
      columnDefs = list(
        list(className = 'dt-center', targets = '_all'),
        list(width = '200px', targets = 0)
      ),
      buttons = list(
        list(extend = 'copy', text = 'Copy'),
        list(extend = 'csv', text = 'CSV', filename = 'complete_signature'),
        list(extend = 'csv', text = 'TSV', fieldSeparator = '\t', extension = '.tsv', filename = 'complete_signature'),
        list(extend = 'excel', text = 'Excel', filename = 'complete_signature'),
        list(extend = 'print', text = 'Print')
      )
    ),
    extensions = 'Buttons',
    filename_base = "complete_signature"
  )

  # Complete Pipeline plots - all with dimension and format support

  # Helper function to create DPI observer for complete pipeline
  observe_dpi_iii <- function(input_id) {
    shiny::observeEvent(input[[input_id]], {
      new_val <- round_dpi(input[[input_id]])
      if (!is.null(new_val) && new_val != input[[input_id]]) {
        shiny::updateNumericInput(session, input_id, value = new_val)
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  }

  # Apply DPI rounding to all complete pipeline plots
  observe_dpi_iii("comp_hist_dpi")
  observe_dpi_iii("comp_lolli_dpi")
  observe_dpi_iii("comp_wordcloud_dpi")
  observe_dpi_iii("comp_box_dpi")
  observe_dpi_iii("comp_scatter_dpi")
  observe_dpi_iii("comp_score_bar_dpi")
  observe_dpi_iii("comp_line_dpi")
  observe_dpi_iii("comp_ph_dpi")
  observe_dpi_iii("comp_roc_dpi")
  observe_dpi_iii("comp_km_dpi")
  observe_dpi_iii("comp_swim_dpi")
  observe_dpi_iii("comp_forest_dpi")
  observe_dpi_iii("comp_clinics_dpi")
  observe_dpi_iii("comp_bar_dpi")

  # Complete Pipeline - Histogram
  output$out_plot_comp_hist <- shiny::renderPlot({
    shiny::req(res_comp())
    res_comp()$regression$plots$hist_plot
  }, width = function() { input$comp_hist_width * 100 },
  height = function() { input$comp_hist_height * 100 })

  output$dl_comp_hist <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$comp_hist_format,
                    "png" = "png",
                    "pdf" = "pdf",
                    "svg" = "svg")
      paste0("hist_plot.", ext)
    },
    content = function(f) {
      shiny::req(res_comp())
      save_plot_format(
        res_comp()$regression$plots$hist_plot,
        f,
        format = input$comp_hist_format,
        width = input$comp_hist_width,
        height = input$comp_hist_height,
        dpi = if (input$comp_hist_format == "png") input$comp_hist_dpi else 300
      )
    }
  )

  # Complete Pipeline - Lollipop
  output$out_plot_comp_lolli <- shiny::renderPlot({
    shiny::req(res_comp())
    res_comp()$regression$plots$lolli_plot
  }, width = function() { input$comp_lolli_width * 100 },
  height = function() { input$comp_lolli_height * 100 })

  output$dl_comp_lolli <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$comp_lolli_format,
                    "png" = "png",
                    "pdf" = "pdf",
                    "svg" = "svg")
      paste0("lolli_plot.", ext)
    },
    content = function(f) {
      shiny::req(res_comp())
      save_plot_format(
        res_comp()$regression$plots$lolli_plot,
        f,
        format = input$comp_lolli_format,
        width = input$comp_lolli_width,
        height = input$comp_lolli_height,
        dpi = if (input$comp_lolli_format == "png") input$comp_lolli_dpi else 300
      )
    }
  )

  # Complete Pipeline - Wordcloud
  output$out_plot_comp_wordcloud <- shiny::renderPlot({
    shiny::req(res_comp())
    res_comp()$survival$plots$wordcloud_plot
  }, width = function() { input$comp_wordcloud_width * 100 },
  height = function() { input$comp_wordcloud_height * 100 })

  output$dl_comp_wordcloud <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$comp_wordcloud_format,
                    "png" = "png",
                    "pdf" = "pdf",
                    "svg" = "svg")
      paste0("wordcloud.", ext)
    },
    content = function(f) {
      shiny::req(res_comp())
      save_plot_format(
        res_comp()$survival$plots$wordcloud_plot,
        f,
        format = input$comp_wordcloud_format,
        width = input$comp_wordcloud_width,
        height = input$comp_wordcloud_height,
        dpi = if (input$comp_wordcloud_format == "png") input$comp_wordcloud_dpi else 300
      )
    }
  )

  # Complete Pipeline - Boxplot
  output$out_plot_comp_box <- shiny::renderPlot({
    shiny::req(res_comp())
    res_comp()$survival$plots$box_plot
  }, width = function() { input$comp_box_width * 100 },
  height = function() { input$comp_box_height * 100 })

  output$dl_comp_box <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$comp_box_format,
                    "png" = "png",
                    "pdf" = "pdf",
                    "svg" = "svg")
      paste0("box_plot.", ext)
    },
    content = function(f) {
      shiny::req(res_comp())
      save_plot_format(
        res_comp()$survival$plots$box_plot,
        f,
        format = input$comp_box_format,
        width = input$comp_box_width,
        height = input$comp_box_height,
        dpi = if (input$comp_box_format == "png") input$comp_box_dpi else 300
      )
    }
  )

  # Complete Pipeline - Scatter Plot
  output$out_plot_comp_scatter <- shiny::renderPlot({
    shiny::req(res_comp())
    res_comp()$survival$plots$scatter_plot
  }, width = function() { input$comp_scatter_width * 100 },
  height = function() { input$comp_scatter_height * 100 })

  output$dl_comp_scatter <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$comp_scatter_format,
                    "png" = "png",
                    "pdf" = "pdf",
                    "svg" = "svg")
      paste0("scatter_plot.", ext)
    },
    content = function(f) {
      shiny::req(res_comp())
      save_plot_format(
        res_comp()$survival$plots$scatter_plot,
        f,
        format = input$comp_scatter_format,
        width = input$comp_scatter_width,
        height = input$comp_scatter_height,
        dpi = if (input$comp_scatter_format == "png") input$comp_scatter_dpi else 300
      )
    }
  )

  # Complete Pipeline - Score Bar Plot
  output$out_plot_comp_score_bar <- shiny::renderPlot({
    shiny::req(res_comp())
    res_comp()$survival$plots$score_bar_plot
  }, width = function() { input$comp_score_bar_width * 100 },
  height = function() { input$comp_score_bar_height * 100 })

  output$dl_comp_score_bar <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$comp_score_bar_format,
                    "png" = "png",
                    "pdf" = "pdf",
                    "svg" = "svg")
      paste0("score_bar.", ext)
    },
    content = function(f) {
      shiny::req(res_comp())
      save_plot_format(
        res_comp()$survival$plots$score_bar_plot,
        f,
        format = input$comp_score_bar_format,
        width = input$comp_score_bar_width,
        height = input$comp_score_bar_height,
        dpi = if (input$comp_score_bar_format == "png") input$comp_score_bar_dpi else 300
      )
    }
  )

  # Complete Pipeline - Line Plot
  output$out_plot_comp_line <- shiny::renderPlot({
    shiny::req(res_comp())
    res_comp()$survival$plots$line_plot
  }, width = function() { input$comp_line_width * 100 },
  height = function() { input$comp_line_height * 100 })

  output$dl_comp_line <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$comp_line_format,
                    "png" = "png",
                    "pdf" = "pdf",
                    "svg" = "svg")
      paste0("line_plot.", ext)
    },
    content = function(f) {
      shiny::req(res_comp())
      save_plot_format(
        res_comp()$survival$plots$line_plot,
        f,
        format = input$comp_line_format,
        width = input$comp_line_width,
        height = input$comp_line_height,
        dpi = if (input$comp_line_format == "png") input$comp_line_dpi else 300
      )
    }
  )

  # Complete Pipeline - PH Assumptions Plot
  output$out_plot_comp_ph <- shiny::renderPlot({
    shiny::req(res_comp())
    res_comp()$survival$plots$ph_plot
  }, width = function() { input$comp_ph_width * 100 },
  height = function() { input$comp_ph_height * 100 })

  output$dl_comp_ph <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$comp_ph_format,
                    "png" = "png",
                    "pdf" = "pdf",
                    "svg" = "svg")
      paste0("ph_plot.", ext)
    },
    content = function(f) {
      shiny::req(res_comp())
      save_plot_format(
        res_comp()$survival$plots$ph_plot,
        f,
        format = input$comp_ph_format,
        width = input$comp_ph_width,
        height = input$comp_ph_height,
        dpi = if (input$comp_ph_format == "png") input$comp_ph_dpi else 300
      )
    }
  )

  # Complete Pipeline - ROC Curve
  output$out_plot_comp_roc <- shiny::renderPlot({
    res <- res_comp()
    shiny::req(res)
    roc_plot <- build_roc_ggplot(res$survival)
    if (is.null(roc_plot)) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "ROC Curve is only generated when\n'Score Cutoff Strategy' is set to 'roc'.", cex = 1.2, col = "gray40")
    } else {
      print(roc_plot)
    }
  }, width = function() { input$comp_roc_width * 100 },
  height = function() { input$comp_roc_height * 100 })

  output$dl_comp_roc <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$comp_roc_format,
                    "png" = "png",
                    "pdf" = "pdf",
                    "svg" = "svg")
      paste0("roc_plot.", ext)
    },
    content = function(f) {
      shiny::req(res_comp())
      save_plot_format(
        build_roc_ggplot(res_comp()$survival),
        f,
        format = input$comp_roc_format,
        width = input$comp_roc_width,
        height = input$comp_roc_height,
        dpi = if (input$comp_roc_format == "png") input$comp_roc_dpi else 300
      )
    }
  )

  # Complete Pipeline - Kaplan-Meier
  output$out_plot_comp_km <- shiny::renderPlot({
    shiny::req(res_comp())
    res_comp()$survival$plots$km_plot
  }, width = function() { input$comp_km_width * 100 },
  height = function() { input$comp_km_height * 100 })

  output$dl_comp_km <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$comp_km_format,
                    "png" = "png",
                    "pdf" = "pdf",
                    "svg" = "svg")
      paste0("km_plot.", ext)
    },
    content = function(f) {
      shiny::req(res_comp())
      save_plot_format(
        res_comp()$survival$plots$km_plot,
        f,
        format = input$comp_km_format,
        width = input$comp_km_width,
        height = input$comp_km_height,
        dpi = if (input$comp_km_format == "png") input$comp_km_dpi else 300
      )
    }
  )

  # Complete Pipeline - Swimmer Plot (always generated)
  output$out_plot_comp_swim <- shiny::renderPlot({
    shiny::req(res_comp())
    res_comp()$survival$plots$swim_plot
  }, width = function() { input$comp_swim_width * 100 },
  height = function() { input$comp_swim_height * 100 })

  output$dl_comp_swim <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$comp_swim_format,
                    "png" = "png",
                    "pdf" = "pdf",
                    "svg" = "svg")
      paste0("swimmer_plot.", ext)
    },
    content = function(f) {
      shiny::req(res_comp())
      save_plot_format(
        res_comp()$survival$plots$swim_plot,
        f,
        format = input$comp_swim_format,
        width = input$comp_swim_width,
        height = input$comp_swim_height,
        dpi = if (input$comp_swim_format == "png") input$comp_swim_dpi else 300
      )
    }
  )

  # Complete Pipeline - Forest Plot (only if multivariate)
  output$out_plot_comp_forest <- shiny::renderPlot({
    shiny::req(res_comp())
    p <- res_comp()$survival$plots$forest_plot
    if (is.null(p)) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "Forest plot is only generated when\nmultivariate analysis is enabled and succeeds.", cex = 1.2, col = "gray40")
    } else {
      print(p)
    }
  }, width = function() { input$comp_forest_width * 100 },
  height = function() { input$comp_forest_height * 100 })

  output$dl_comp_forest <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$comp_forest_format,
                    "png" = "png",
                    "pdf" = "pdf",
                    "svg" = "svg")
      paste0("forest_plot.", ext)
    },
    content = function(f) {
      shiny::req(res_comp())
      p <- res_comp()$survival$plots$forest_plot
      if (!is.null(p)) {
        save_plot_format(
          p,
          f,
          format = input$comp_forest_format,
          width = input$comp_forest_width,
          height = input$comp_forest_height,
          dpi = if (input$comp_forest_format == "png") input$comp_forest_dpi else 300
        )
      }
    }
  )

  # Complete Pipeline - Clinical Boxplots (only if multivariate)
  output$out_plot_comp_clinics <- shiny::renderPlot({
    shiny::req(res_comp())
    p <- res_comp()$survival$plots$clinics_box_plot
    if (is.null(p)) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "Clinical boxplots are only generated when\nmultivariate analysis is enabled and succeeds.", cex = 1.2, col = "gray40")
    } else {
      print(p)
    }
  }, width = function() { input$comp_clinics_width * 100 },
  height = function() { input$comp_clinics_height * 100 })

  output$dl_comp_clinics <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$comp_clinics_format,
                    "png" = "png",
                    "pdf" = "pdf",
                    "svg" = "svg")
      paste0("clinical_boxplots.", ext)
    },
    content = function(f) {
      shiny::req(res_comp())
      p <- res_comp()$survival$plots$clinics_box_plot
      if (!is.null(p)) {
        save_plot_format(
          p,
          f,
          format = input$comp_clinics_format,
          width = input$comp_clinics_width,
          height = input$comp_clinics_height,
          dpi = if (input$comp_clinics_format == "png") input$comp_clinics_dpi else 300
        )
      }
    }
  )

  # Complete Pipeline - Bootstrap Barplot (only if multivariate + bootstrap succeeded)
  output$out_plot_comp_bar <- shiny::renderPlot({
    shiny::req(res_comp())
    p <- res_comp()$survival$plots$bar_plot
    if (is.null(p)) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "Bootstrap barplot is only generated when multivariate\nanalysis with successful bootstrap resampling is performed.", cex = 1.2, col = "gray40")
    } else {
      print(p)
    }
  }, width = function() { input$comp_bar_width * 100 },
  height = function() { input$comp_bar_height * 100 })

  output$dl_comp_bar <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$comp_bar_format,
                    "png" = "png",
                    "pdf" = "pdf",
                    "svg" = "svg")
      paste0("bootstrap_barplot.", ext)
    },
    content = function(f) {
      shiny::req(res_comp())
      p <- res_comp()$survival$plots$bar_plot
      if (!is.null(p)) {
        save_plot_format(
          p,
          f,
          format = input$comp_bar_format,
          width = input$comp_bar_width,
          height = input$comp_bar_height,
          dpi = if (input$comp_bar_format == "png") input$comp_bar_dpi else 300
        )
      }
    }
  )

  # Complete Pipeline Tables - fixed pagination with server-side processing
  output$out_comp_tbl_uni <- render_clinprog_dt(
    function() {
      shiny::req(res_comp())
      res_comp()$survival$survival$univariate$table
    },
    first_col_name = NULL,
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      autoWidth = TRUE,
      dom = 'Blfrtip',
      lengthMenu = list(c(10, 25, 50), c("10", "25", "50")),
      buttons = list(
        list(extend = 'copy', text = 'Copy'),
        list(extend = 'csv', text = 'CSV', filename = 'complete_univariate'),
        list(extend = 'csv', text = 'TSV', fieldSeparator = '\t', extension = '.tsv', filename = 'complete_univariate'),
        list(extend = 'excel', text = 'Excel', filename = 'complete_univariate'),
        list(extend = 'print', text = 'Print')
      )
    ),
    filename_base = "complete_univariate"
  )

  output$out_comp_tbl_multi <- render_clinprog_dt(
    function() {
      shiny::req(res_comp())
      res_comp()$survival$survival$multivariate$table
    },
    first_col_name = NULL,
    fix_multivariate_score = TRUE,
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      autoWidth = TRUE,
      dom = 'Blfrtip',
      lengthMenu = list(c(10, 25, 50), c("10", "25", "50")),
      buttons = list(
        list(extend = 'copy', text = 'Copy'),
        list(extend = 'csv', text = 'CSV', filename = 'complete_multivariate'),
        list(extend = 'csv', text = 'TSV', fieldSeparator = '\t', extension = '.tsv', filename = 'complete_multivariate'),
        list(extend = 'excel', text = 'Excel', filename = 'complete_multivariate'),
        list(extend = 'print', text = 'Print')
      )
    ),
    filename_base = "complete_multivariate"
  )

  output$out_comp_tbl_score <- render_clinprog_dt(
    function() {
      shiny::req(res_comp())
      res <- res_comp()$survival
      if (!is.null(res$clinical_data) && nrow(res$clinical_data) > 0) res$clinical_data else res$expression_data
    },
    first_col_name = "Samples",
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      autoWidth = TRUE,
      dom = 'Blfrtip',
      lengthMenu = list(c(10, 25, 50), c("10", "25", "50")),
      buttons = list(
        list(extend = 'copy', text = 'Copy'),
        list(extend = 'csv', text = 'CSV', filename = 'complete_scores'),
        list(extend = 'csv', text = 'TSV', fieldSeparator = '\t', extension = '.tsv', filename = 'complete_scores'),
        list(extend = 'excel', text = 'Excel', filename = 'complete_scores'),
        list(extend = 'print', text = 'Print')
      )
    ),
    filename_base = "complete_scores"
  )

  output$dl_comp_sig <- shiny::downloadHandler(filename = "complete_signature.tsv", content = function(f) utils::write.table(res_comp()$regression$signature, f, sep="\t", quote=F, row.names=TRUE))

  output$dl_comp_zip <- shiny::downloadHandler(
    filename = function() { paste0(input$comp_outprefix, "_complete_all.zip") },
    content = function(file) {
      tmpdir <- tempfile()
      dir.create(tmpdir)
      res <- res_comp()

      utils::write.table(res$regression$signature, file.path(tmpdir, "signature.tsv"), sep="\t", quote=FALSE, row.names=TRUE)

      for (pname in names(res$regression$plots)) {
        if (!is.null(res$regression$plots[[pname]])) {
          save_plot_format(res$regression$plots[[pname]], file.path(tmpdir, paste0("reg_", pname, ".png")), format = "png", width = 8, height = 6, dpi = 300)
        }
      }
      for (pname in names(res$survival$plots)) {
        if (!is.null(res$survival$plots[[pname]])) {
          p_obj <- if (pname == "roc_plot") build_roc_ggplot(res$survival) else res$survival$plots[[pname]]
          if (!is.null(p_obj)) {
            save_plot_format(p_obj, file.path(tmpdir, paste0("surv_", pname, ".png")), format = "png", width = 8, height = 6, dpi = 300)
          }
        }
      }
      if (!is.null(res$survival$survival$univariate$table)) {
        utils::write.table(res$survival$survival$univariate$table, file.path(tmpdir, "univariate_results.tsv"), sep="\t", quote=FALSE, row.names=TRUE)
      }
      if (!is.null(res$survival$survival$multivariate$table)) {
        utils::write.table(res$survival$survival$multivariate$table, file.path(tmpdir, "multivariate_results.tsv"), sep="\t", quote=FALSE, row.names=TRUE)
      }
      tb_score <- if (!is.null(res$survival$clinical_data) && nrow(res$survival$clinical_data) > 0) res$survival$clinical_data else res$survival$expression_data
      if (!is.null(tb_score)) {
        utils::write.table(tb_score, file.path(tmpdir, "patient_scores.tsv"), sep="\t", quote=FALSE, row.names=TRUE)
      }
      zip::zip(zipfile = file, files = dir(tmpdir, full.names = TRUE), mode = "cherry-pick")
    }
  )

  # -------------------------------------------------------------------
  # GLOBAL ZOOM RESET BUTTON
  # -------------------------------------------------------------------
  shiny::observeEvent(input$reset_zoom_btn, {
    session$sendCustomMessage("resetZoom", list(target = "all"))
  })
}
