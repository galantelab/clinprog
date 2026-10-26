# clinprog 0.5.0

* Added the `clinprog-tutorial` vignette, a step-by-step guide to the three workflows
  (signature discovery, survival analysis and the complete pipeline).
* Added a Shiny application (`run_app()`) with an interactive interface to run the analyses
  locally.
* Added a Quarto report template used by `write_report()` to produce PDF or HTML summaries.
* The command-line script `inst/scripts/clinprog.R` now accepts short options.
* The Dockerfile now installs Quarto and its dependencies, so reports can be generated
  inside the container.
* Fixed `write_report()` failing when the package is installed in a read-only location
  (for example, inside a Docker image).
* Fixed `run_survival()` when drawing the bootstrap barplot of the multivariate analysis.
* Updated the reference manual of the main functions.

# clinprog 0.4.0

* Replaced the toy datasets with new simulated ones that contain planted prognostic signal,
  so examples and tests can check that the expected features are recovered.
* Removed the `seed` argument from `run_survival()`.
* Added `data.table (>= 1.13.2)` to the dependencies.
* Added a Dockerfile with Quarto.
* Added `tests/testthat/test-clinprog.R` and merged the bootstrap-rejection tests into the
  basic tests.

# clinprog 0.3.0

## New features

* Added the `norm_exp` option to apply a log2(exp + 1) transformation to expression data.
* Added minimum signature selection (`min_signature_size` and `min_signature_cutoff`), which
  searches for the smallest top-ranked subset of the signature that still gives a significant
  log-rank test.
* Added score cutoff strategies to the survival analysis (`score_cutoff`).
* Updated the bootstrap procedure of the survival analysis.
* Added new plots: `plot_swimmer()`, `plot_boxplot()`, `plot_clinics()`, `plot_wordcloud()`,
  `plot_barplot_score()`, `plot_lineplot_score()` and `plot_scatter()`.
* Updated `plot_lollipop()`, `plot_histogram()`, `plot_ph()`, `plot_km()` and
  `plot_barplot()`.
* Added logging with a `verbose` control to the main workflow functions. Input validation
  now reports errors through the log.
* Integrated the import and export functions into the package workflow.
* Restored the diagnostic messages for rejected bootstrap iterations.
* Added `rlang` to the dependencies.

## Changes

* The public arguments of all main functions now use snake_case, for example `norm_exp`,
  `group_size`, `output_prefix` and `max_follow_up`. The former `force` argument is now
  `skip_survival_checks`.

## Bug fixes

* The bootstrap seed now advances after rejected attempts, which makes results reproducible
  regardless of the number of rejections.
* Fixed the bootstrap procedure when zero iterations were left to run.
* Fixed the file extension of reports written by `write_report()`.
* Fixed a double normalization of the expression data in the plotting functions.
* Fixed `clusterExport` use when running the bootstrap in parallel.

# clinprog 0.2.0

* Renamed `write_clinprog_signature()` to `write_signature()`.
* Updated the README.

# clinprog 0.1.0

* First version of clinprog, a CRAN-ready package that continues the Reboot project
  (last version REBOOT 1.2.0).

# REBOOT 1.2.0

* Added `rebootRegression()` for signature discovery
* Added `rebootSurvival()` for survival analysis
* Added `rebootComplete()` for end-to-end pipeline
* Shiny application with interactive interface
* Quarto summary report
* Comprehensive vignette tutorial
* Descriptive plotting functions
* Export functions for tables, plots, metadata, and logs
