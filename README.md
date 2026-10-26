# clinprog

[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License: GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](LICENSE)

**clinprog** (**Clin**ical **Prog**nosis) is an R package for discovering and evaluating prognostic molecular signatures from high-dimensional gene- or transcript-expression data.

The workflow combines feature filtering, bootstrap resampling, penalized Cox regression (LASSO), and survival analysis. It supports analyses at the gene and transcript (splicing-isoform) levels.

> **Rebrand:** `clinprog` is the successor to the package formerly known as [Reboot (v1.2.0)](https://github.com/galantelab/reboot), with a redesigned R API (snake_case arguments), reproducible bootstrap settings, parallel execution, a command-line interface (CLI), report generation, and a Shiny application.

## Overview

The main analysis functions implement two modules:

| Function | Module | Purpose |
| --- | --- | --- |
| `run_regression()` | I: regression | Discovers a molecular signature associated with overall survival. |
| `run_survival()` | II: survival | Calculates signature scores and evaluates their association with survival, optionally adjusting for clinical covariates. |
| `run_complete()` | I + II | Runs both modules and passes the discovered signature to the survival analysis. |

You can run the modules separately or use the integrated workflow:

```text
Expression + survival data                 Expression + survival data
          │                                          │
          ▼                                          ▼
   run_regression()                           run_complete()
          │                                          │
          │  Molecular signature                     ├── Signature discovery
          ▼                                          │
    run_survival()                                   └── Survival validation
          │
          ▼
  Prognostic evaluation
```

## Installation

Install the development version from GitHub:

```r
install.packages("remotes")
remotes::install_github("galantelab/clinprog")
```

Once `clinprog` is available on CRAN, it can be installed with:

```r
install.packages("clinprog")
```

PDF and HTML reports are rendered with [Quarto](https://quarto.org/). If you want to generate reports, also install the R package `quarto` and a Quarto installation; PDF reports additionally need a LaTeX distribution such as TinyTeX (`quarto install tinytex`). See [Results and output files](#results-and-output-files).

## Input data

The main R functions accept **data frames**, not file paths. Import TSV/CSV or other files before calling the R API (for example with `read_table()`, described [below](#reading-and-writing-data)). The CLI handles tabular-file import for command-line workflows.

### Expression and survival data

Supply one row per sample. Row names should be unique sample identifiers. The first two columns represent overall survival, followed by numeric gene- or transcript-expression columns:

| Column | Meaning |
| --- | --- |
| `OS` | Event indicator: `0` = censored, `1` = event. |
| `OS.time` | Follow-up time. Its unit should be consistent across the dataset and with `max_follow_up`, if set. |
| Remaining columns | Numeric expression measurements for genes or transcripts. |

Example:

```text
            OS   OS.time   Gene1   Gene2   Gene3
Sample1      1      1200    52.3     3.1     0.0
Sample2      0       980    24.4     1.8     0.5
Sample3      1       760    41.0     2.7     1.2
```

The functions interpret the **first two columns by position** as the event and time columns and standardize their names to `OS` and `OS.time`. Using those names in the input makes the structure easier to read.

For example, import a TSV while preserving molecular feature names:

```r
expression_data <- read.delim(
  "expression_data.tsv",
  row.names = 1,
  check.names = FALSE
)
```

Choose `feature_type = "gene"` when the feature columns represent genes, or `feature_type = "transcript"` when they represent transcripts. The `norm_exp = TRUE` option applies `log2(expression + 1)` to expression columns. Use `norm_exp = TRUE` for raw expression values, and keep the setting consistent between signature discovery and survival evaluation.

Feature columns must not contain only zeros, and every sample must have a unique identifier.

### Clinical data

For multivariate survival analysis, provide a clinical data frame whose row names match the sample identifiers in the expression/survival data. Each clinical variable must contain exactly two categories.

```r
clinical_data <- read.delim(
  "clinical_data.tsv",
  row.names = 1,
  check.names = FALSE
)
```

```text
            Age    Gender   Race        Histotype            Grade
Sample1     Low    Female   White       Astrocytoma          G2
Sample2     High   Male     Not white   Oligodendroglioma    G3
```

The clinical data are optional unless `run_multivariate = TRUE`.

## Quick start

The package includes small, **simulated** datasets for examples and tests. They do not contain real patient data:

- `toy_gene_expression`: overall-survival data with gene-expression features.
- `toy_transcript_expression`: overall-survival data with transcript-expression features.
- `toy_clinics`: simulated binary clinical covariates with matching sample identifiers.

The toy datasets contain known, planted structure: genes named `TOYRISK*` and `TOYPROT*` (and transcripts `MSTRG.9001.*` and `MSTRG.9002.*`) are associated with worse and better prognosis, respectively, so they should appear in the signature.

Load the gene-level data and discover a signature:

```r
library(clinprog)
data("toy_gene_expression", package = "clinprog")

gene_signature <- run_regression(
  data = toy_gene_expression,
  output_prefix = file.path(tempdir(), "gene_demo"),
  feature_type = "gene",
  norm_exp = TRUE,
  bootstrap = 30,
  group_size = 10,
  skip_survival_checks = TRUE,
  n_cores = 1,
  seed = 123
)

print(gene_signature)
summary(gene_signature)
head(as.data.frame(gene_signature))
```

Evaluate that signature:

```r
gene_survival <- run_survival(
  data = toy_gene_expression,
  signature = gene_signature,
  output_prefix = file.path(tempdir(), "gene_demo_survival"),
  norm_exp = TRUE,
  score_cutoff = "median",
  skip_survival_checks = TRUE
)

print(gene_survival)
summary(gene_survival)
plot(gene_survival)
```

The integrated workflow runs both modules:

```r
complete_result <- run_complete(
  data = toy_gene_expression,
  output_prefix = file.path(tempdir(), "gene_demo_complete"),
  feature_type = "gene",
  norm_exp = TRUE,
  bootstrap = 30,
  group_size = 10,
  run_multivariate = FALSE,
  skip_survival_checks = TRUE,
  n_cores = 1,
  seed = 123
)

summary(complete_result)
```

For transcript-level analysis, use the transcript dataset and set `feature_type = "transcript"`:

```r
data("toy_transcript_expression", package = "clinprog")

transcript_signature <- run_regression(
  data = toy_transcript_expression,
  output_prefix = file.path(tempdir(), "transcript_demo"),
  feature_type = "transcript",
  norm_exp = TRUE,
  bootstrap = 30,
  group_size = 10,
  skip_survival_checks = TRUE,
  n_cores = 1,
  seed = 123
)
```

To request multivariate survival analysis, load `toy_clinics` and provide it with `run_multivariate = TRUE`:

```r
data("toy_clinics", package = "clinprog")

gene_survival_multi <- run_survival(
  data = toy_gene_expression,
  signature = gene_signature,
  norm_exp = TRUE,
  run_multivariate = TRUE,
  clinical_data = toy_clinics,
  score_cutoff = "median",
  bootstrap = 100,
  skip_survival_checks = TRUE
)
```

The toy datasets were validated with `skip_survival_checks = TRUE`; keep the default (`FALSE`) for real data. Because the datasets are deliberately small and simulated, a multivariate run may produce warnings or be unable to retain a covariate after model assumptions are checked. Use appropriately sized and validated data for scientific analyses.

A longer, step-by-step guide is in the package vignette (see [Vignettes](#vignettes)).

## Main options

The main functions share many arguments:

| Argument | Purpose |
| --- | --- |
| `output_prefix` | Prefix for files written by the analysis. |
| `norm_exp` | Applies `log2(expression + 1)` to expression columns when `TRUE`. |
| `bootstrap` | Positive number of bootstrap iterations requested. |
| `group_size` | Number of features sampled per regression iteration. |
| `percentage_filter` | Correlation-filter threshold used during regression. |
| `variance_filter` | Variance-filter threshold. |
| `max_follow_up` | Optional maximum follow-up time, in the same units as `OS.time`. |
| `feature_type` | `"gene"` or `"transcript"`; used by signature discovery. |
| `min_signature_size` | Optional search for a shorter signature with a significant univariate log-rank result on the same dataset. |
| `score_cutoff` | Score grouping strategy for survival analysis: `"median"`, `"mean"`, `"signal"`, or `"roc"`. |
| `run_multivariate` | Enables clinical-covariate-adjusted survival analysis. |
| `clinical_data` | Clinical data frame required when multivariate analysis is enabled. |
| `p_value_cutoff` | Univariate p-value threshold for selecting clinical covariates for multivariate analysis. |
| `skip_survival_checks` | Overrides the filtering steps related to survival assumptions. Keep the default (`FALSE`) for real data. |
| `save_plots`, `save_table` | Write plots or tabular outputs to disk. |
| `save_json`, `save_rds` | Save metadata or the result object. |
| `report_format` | `FALSE` disables a report; `TRUE` or `"PDF"` creates PDF; `"HTML"` creates HTML. |
| `n_cores` | Number of CPU cores for regression bootstrap execution. |
| `seed` | Integer seed used by `run_regression()` for stochastic regression operations. It does not control stochastic procedures in `run_survival()`. |
| `save_log`, `verbose` | Control log-file writing and progress messages. |

See the function help pages for complete details and defaults:

```r
?run_regression
?run_survival
?run_complete
```

The minimum-signature search uses the same dataset employed for signature discovery. Treat it as a parsimony heuristic, not as independent validation.

### Choosing parameters and troubleshooting

A few settings have a large effect on whether a signature is found:

- **Use `norm_exp = TRUE` for raw gene expression values.** A feature enters the signature only if its average bootstrap coefficient passes a minimum threshold, and on raw values (hundreds to thousands) the coefficients are too small to reach it. The error `No signature found: coefficients below threshold` is the usual symptom.
- **Use enough bootstrap iterations.** With `bootstrap = 1`, the result depends on which few features are drawn at random. The examples use `bootstrap = 30`; use more for real analyses.
- **Keep enough features after filtering.** If fewer features than `group_size` remain, a single regression is run instead of the bootstrap, which can end with `No signature found: all coefficients are zero`. Check the log line about the variance filter. With `norm_exp = TRUE` the variance is computed after the log2 transformation, so lowering `variance_filter` (for example to `0.001`) can recover features.
- **Input checks:** columns containing only zeros are rejected, and the proportion of events must be adequate unless `skip_survival_checks = TRUE`.

The vignette has a longer troubleshooting section.

## Reproducibility and parallel execution

Bootstrap analyses can be computationally intensive. Use `n_cores` to run the regression bootstrap in parallel, and `seed` to make it reproducible:

```r
run_regression(
  data = expression_data,
  bootstrap = 100,
  n_cores = 4,
  seed = 42
)
```

The same seed is intended to produce the same signature across repeated runs, including when changing between serial and parallel execution. The seed is stored in the returned object and in the exported metadata, together with the function call, the parameters and the package version, so an analysis can be reproduced later.

## Results and output files

The functions return S3 objects:

| Class | Returned by | Main contents |
| --- | --- | --- |
| `clinprog_signature` | `run_regression()` | Signature table, optional plots, parameters, metadata, and call. |
| `clinprog_survival` | `run_survival()` | Signature, sample-level scores, survival models/results, optional clinical and ROC results, plots, parameters, and metadata. |
| `clinprog_complete` | `run_complete()` | Regression result, survival result, shared parameters, metadata, and call. |

Explore results with standard methods:

```r
print(gene_signature)
summary(gene_signature)
coef(gene_signature)
as.data.frame(gene_signature)
plot(gene_signature)
```

When requested, outputs are written using `output_prefix`. Depending on the module and options, these can include signature/score tables, univariate and multivariate survival tables, plots, JSON metadata, an RDS object, logs, and a PDF or HTML report.

PDF/HTML reports are rendered with Quarto. Report generation therefore requires the R package `quarto`, a working Quarto installation, and the report template included with the package. PDF reports also need LaTeX (for example TinyTeX).

## Plotting

`clinprog` provides plotting functions based on `ggplot2` that return plot objects or base-graphics recordings, depending on the plot:

- Signature discovery: `plot_histogram()`, `plot_lollipop()`.
- Survival and model diagnostics: `plot_km()`, `plot_ph()`, `plot_roc()`, `plot_forest()`, `plot_swimmer()`.
- Expression, score, and clinical displays: `plot_barplot()`, `plot_boxplot()`, `plot_clinics()`, `plot_scatter()`, `plot_wordcloud()`, `plot_barplot_score()`, `plot_lineplot_score()`.

Use `plot()` on a result object to access supported visualizations; the available plot types depend on the result object and the analyses that were performed. `theme_clinprog()` provides the package's default `ggplot2` theme.

## Reading and writing data

`read_table()` imports delimited tables; by default, it treats the first column as row names. `read_rds()` loads a saved clinprog result object. Export helpers include:

- `write_signature()` and `write_score()` for signature and score tables;
- `write_univariate()` and `write_multivariate()` for survival results;
- `write_metadata()` for JSON metadata;
- `write_rds()` for result objects;
- `write_report()` for PDF/HTML reports;
- `write_log()` for the run log.

These functions can be used independently of the main workflow, to save individual analysis components.

## Command-line interface

The installed package includes a command-line front-end that runs the three workflows from a terminal, without an interactive R session. It reads tabular files and passes the imported data frames to the R API.

The script is installed at:

```r
system.file("scripts", "clinprog.R", package = "clinprog")
```

Inspect the supported commands and options with `--help`:

```bash
CLI_SCRIPT="$(Rscript -e 'cat(system.file("scripts", "clinprog.R", package = "clinprog"))')"
Rscript "$CLI_SCRIPT" --help
```

The CLI provides the same three workflows as the R API:

```bash
Rscript "$CLI_SCRIPT" regression --help
Rscript "$CLI_SCRIPT" survival --help
Rscript "$CLI_SCRIPT" complete --help
```

Use the help output for the exact option names accepted by the installed version, including the short options. They map to the R arguments described above (for example, the bootstrap, group size, number of cores and seed options).

## Shiny application

The package provides a Shiny application for interactive analysis. Launch it from R with:

```r
clinprog::run_app()
```

Users who prefer a double-click launcher can use the scripts in the [`launchers/`](launchers/) folder of this repository (Linux, macOS and Windows). They are not part of the installed package; see [`launchers/README.md`](launchers/README.md) for instructions.

## Docker

A `Dockerfile` is provided in the repository for a reproducible containerized environment. It includes Quarto, so reports can also be generated inside the container.

### Build the image

From the repository root:

```bash
docker build -t clinprog:latest .
```

### Run an analysis

Mount your working directory inside the container, and run the CLI:

```bash
docker run --rm \
  -u $(id -u):$(id -g) \
  -v "$(pwd)":/data \
  clinprog:latest \
  clinprog --help
```

Replace `--help` with the workflow and options you need (see the CLI help). Input and output paths must be inside the mounted `/data` directory.

## Example files and datasets

The package includes simulated input files under `inst/extdata/`. Their installed locations can be queried with:

```r
system.file("extdata", "toy_gene_expression.tsv", package = "clinprog")
system.file("extdata", "toy_transcript_expression.tsv", package = "clinprog")
system.file("extdata", "toy_clinics.tsv", package = "clinprog")
```

The source generator and a description of how the data were built are in [`data-raw/`](data-raw/); it uses a fixed seed so that the toy datasets can be regenerated consistently. The data are simulated and are not derived from TCGA or other patient cohorts.

## Vignettes

Browse the package tutorial from R:

```r
browseVignettes("clinprog")
```

The source is in [`vignettes/clinprog-tutorial.Rmd`](vignettes/clinprog-tutorial.Rmd). The package also ships a complete example script, `system.file("scripts", "run_example.R", package = "clinprog")`.

## Repository structure

```text
clinprog/
├── R/                  # Package source code (including the Shiny app)
├── data/               # Example R datasets (.rda)
├── data-raw/           # Script and notes used to generate the toy datasets
├── inst/
│   ├── extdata/        # Example input files (.tsv)
│   ├── figures/        # Figures used by the Shiny app
│   ├── fixtures/       # Ground truth for the toy datasets (used in tests)
│   ├── quarto/         # Report template and styling
│   └── scripts/        # Command-line front-end and example script
├── launchers/          # Double-click launchers for the Shiny app (not in the package)
├── tests/              # Automated tests
├── vignettes/          # Package tutorial
├── DESCRIPTION
├── NAMESPACE
├── NEWS.md
├── Dockerfile
└── README.md
```

## Citation

If you use `clinprog` in your research, cite the original publication describing the Reboot method:

> Felipe R. C. dos Santos, Gabriela D. A. Guardia, Filipe F. dos Santos, Daniel T. Ohara, and Pedro A. F. Galante. *Reboot: a straightforward approach to identify genes and splicing isoforms associated with cancer patient prognosis.* **NAR Cancer** 3(2), June 2021, zcab024.
> https://doi.org/10.1093/narcan/zcab024

## Authors

`clinprog` was developed by:

- Filipe Ferreira dos Santos
- Felipe Rodolfo Camargo dos Santos
- Gabriela Der Agopian Guardia
- Thiago Luiz Araujo Miller
- Daniel Takatori Ohara
- Pedro Alexandre Favoretto Galante

## License

`clinprog` is free and open-source software distributed under the GNU General Public License, version 3 or later (GPL-3). See [`LICENSE`](LICENSE) for the full license text.
