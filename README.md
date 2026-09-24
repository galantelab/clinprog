# clinprog

[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License: GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](LICENSE)

**clinprog** (**Clin**ical **Prog**nosis) is an R package for discovering and validating prognostic molecular signatures from high-dimensional transcriptomic data.

The package combines **bootstrap resampling**, **penalized Cox regression (LASSO)**, and survival analysis to identify molecular features associated with patient outcome and evaluate their prognostic value.

> **Rebrand:** `clinprog` is the successor to the package formerly known as [Reboot (v1.2.0)](https://github.com/galantelab/reboot), with a redesigned R API, reproducible bootstrap settings, parallel execution support, and a dedicated command-line interface (CLI).

---

## Overview

`clinprog` is organized around three main analysis functions:

| Function           | Purpose                                                                      |
| ------------------ | ---------------------------------------------------------------------------- |
| `run_regression()` | Discover a molecular survival signature using bootstrap LASSO Cox regression |
| `run_survival()`   | Evaluate the prognostic value of a molecular signature                       |
| `run_complete()`   | Run the complete discovery and validation workflow                           |

The workflow can be used either **step by step** or as a single integrated analysis:

```text
Expression + survival data
          │
          ▼
   run_regression()
          │
          │  Molecular signature
          ▼
    run_survival()
          │
          ▼
 Prognostic evaluation
```

For a complete end-to-end analysis:

```text
Expression + survival data
          │
          ▼
    run_complete()
          │
          ├── Signature discovery
          │
          └── Survival validation
```

---

## Installation

### From GitHub

The current development version can be installed directly from GitHub:

```r
install.packages("remotes")
remotes::install_github("galantelab/clinprog")
```

Alternatively, using `devtools`:

```r
install.packages("devtools")
devtools::install_github("galantelab/clinprog")
```

### From CRAN

Once `clinprog` is available on CRAN:

```r
install.packages("clinprog")
```

---

## Basic Usage

The R API accepts **data frames**, rather than file paths. Importing data from TSV, CSV, or other formats is left to the user.

For example:

```r
expression_data <- read.delim(
  "expression_data.tsv",
  row.names = 1,
  check.names = FALSE
)

clinical_data <- read.delim(
  "clinical_data.tsv",
  row.names = 1,
  check.names = FALSE
)
```

### Module I — Signature Discovery

`run_regression()` identifies molecular features associated with survival using bootstrap-based LASSO Cox regression.

```r
library(clinprog)

reg_result <- run_regression(
  data = expression_data,
  outprefix = "my_analysis",
  bootstrap = 100,
  groupsize = 10,
  percentagefilter = 0.3,
  variancefilter = 0.01,
  seed = 42,
  ncores = 4,
  plots = TRUE
)
```

The result is a `clinprog_signature` object containing the selected molecular signature and associated analysis information.

### Module II — Survival Validation

The resulting signature can be passed directly to `run_survival()`:

```r
surv_result <- run_survival(
  data = expression_data,
  signature = reg_result,
  outprefix = "my_survival",
  multivariate = TRUE,
  clindata = clinical_data,
  roc = TRUE,
  plots = TRUE
)
```

Alternatively, `signature` can be supplied as a data frame containing a previously generated signature.

### Integrated Workflow

For convenience, `run_complete()` combines signature discovery and survival validation:

```r
complete_result <- run_complete(
  data = expression_data,
  outprefix = "my_complete_analysis",
  bootstrap = 100,
  groupsize = 10,
  multivariate = TRUE,
  clindata = clinical_data,
  seed = 42,
  ncores = 4,
  plots = TRUE
)
```

This is equivalent to running the two modules sequentially while automatically passing the generated signature from the regression step to the survival analysis.

---

## Reproducibility and Parallel Execution

Bootstrap analyses can be computationally intensive. `clinprog` supports parallel execution through the `ncores` argument:

```r
run_regression(
  data = expression_data,
  bootstrap = 100,
  ncores = 4
)
```

The `seed` argument can be used to make bootstrap analyses reproducible:

```r
run_regression(
  data = expression_data,
  bootstrap = 100,
  ncores = 4,
  seed = 42
)
```

The same seed is intended to produce the same analysis result across repeated runs, including when changing between serial and parallel execution.

---

## Input Data

### Expression and Survival Data

The main input must be a data frame containing sample-level survival information together with numeric molecular measurements.

Each row represents one sample or patient, identified by its row name.

Required survival columns:

| Column    | Description                                   |
| --------- | --------------------------------------------- |
| `OS`      | Event indicator (`0` = censored, `1` = event) |
| `OS.time` | Follow-up time                                |

Additional columns contain gene or transcript expression values.

Example:

```text
            OS   OS.time   Transcript1   Transcript2   Transcript3
Sample1      1      1200           5.2           3.1           0.0
Sample2      0       980           2.4           1.8           0.5
Sample3      1       760           4.1           2.7           1.2
```

Feature columns should contain numeric expression measurements.

### Clinical Data

Clinical data can optionally be supplied to `run_survival()` or `run_complete()` when multivariate analysis is requested:

```r
surv_result <- run_survival(
  data = expression_data,
  signature = reg_result,
  multivariate = TRUE,
  clindata = clinical_data
)
```

Clinical and expression data must use matching sample identifiers.

---

## Analysis Outputs

The analysis functions return structured S3 objects:

| Class                | Description                                            |
| -------------------- | ------------------------------------------------------ |
| `clinprog_signature` | Molecular signature generated by the regression module |
| `clinprog_survival`  | Survival analysis and prognostic evaluation            |
| `clinprog_complete`  | Combined result from the complete workflow             |

The returned objects can be inspected using standard R methods such as:

```r
print(reg_result)
summary(reg_result)
coef(reg_result)
```

Graphical and tabular outputs can also be generated directly from the returned objects.

---

## Visualization

`clinprog` provides a collection of plotting functions based on `ggplot2`.

### Signature Discovery

```r
plot_histogram()
plot_lollipop()
```

### Survival Analysis

```r
plot_km()
plot_ph()
plot_roc()
plot_forest()
plot_barplot()
```

All plotting functions return editable `ggplot2` objects.

A package-wide plotting theme is also available:

```r
theme_clinprog()
```

---

## Input/Output Utilities

The package provides helper functions for reading and writing analysis results.

### Reading

```r
read_table()
read_rds()
```

### Writing

```r
write_signature()
write_score()
write_univariate()
write_multivariate()
write_metadata()
write_rds()
write_report()
```

These functions can be used to save individual analysis components independently of the main workflow.

---

## Command-Line Interface

`clinprog` includes a command-line front-end for running analyses directly from a terminal without opening an interactive R session.

The CLI handles file import and passes the resulting data to the `clinprog` R API.

### Running the CLI

After installing the package, the script can be called directly with `Rscript`:

```bash
Rscript \
  -e 'cat(system.file("scripts", "clinprog.R", package="clinprog"))'
```

For convenience, a shell alias can be created:

```bash
alias clinprog='Rscript -e '\''source(system.file("scripts", "clinprog.R", package="clinprog"))'\'''
```

Add the alias to `~/.bashrc` or `~/.zshrc`, then reload the shell:

```bash
source ~/.bashrc
```

The CLI provides the same three workflows as the R API:

```bash
clinprog regression ...
clinprog survival ...
clinprog complete ...
```

For example:

```bash
clinprog regression \
  --filein expression_data.tsv \
  --outprefix results/regression \
  --bootstrap 100 \
  --groupsize 10 \
  --ncores 4 \
  --seed 42
```

A complete analysis can be run with:

```bash
clinprog complete \
  --filein expression_data.tsv \
  --clinical clinical_data.tsv \
  --outprefix results/complete \
  --bootstrap 100 \
  --groupsize 10 \
  --ncores 4 \
  --seed 42 \
  --multivariate
```

Run:

```bash
clinprog --help
```

for the complete list of available options.

---

## Docker

A pre-configured `Dockerfile` is provided for running `clinprog` in a reproducible containerized environment.

### Build the image

From the repository root:

```bash
docker build -t clinprog:latest .
```

### Run an analysis

Mount your working directory inside the container:

```bash
docker run --rm \
  -u $(id -u):$(id -g) \
  -v "$(pwd)":/data \
  clinprog:latest \
  clinprog complete \
  --filein /data/expression_data.tsv \
  --clinical /data/clinical_data.tsv \
  --outprefix /data/results/complete \
  --bootstrap 100 \
  --groupsize 10 \
  --ncores 4 \
  --seed 42 \
  --multivariate
```

The container provides a consistent software environment for the analysis and is particularly useful when reproducing results across systems.

---

## Toy Data and Examples

`clinprog` includes small example datasets that can be loaded directly in R:

```r
data(toy_expression)
data(toy_clinics)
```

For example:

```r
library(clinprog)

data(toy_expression)
data(toy_clinics)

result <- run_complete(
  data = toy_expression,
  outprefix = "toy_analysis",
  bootstrap = 10,
  groupsize = 10,
  multivariate = TRUE,
  clindata = toy_clinics,
  seed = 42,
  ncores = 1,
  plots = TRUE
)
```

Example input files are also included in the package under `inst/extdata/`.

---

## Documentation

The package includes a tutorial vignette covering the main workflow and analysis steps.

From an installed package:

```r
browseVignettes("clinprog")
```

The source of the tutorial is available in:

```text
vignettes/clinprog-tutorial.Rmd
```

---

## Repository Structure

The repository is organized as follows:

```text
clinprog/
├── R/                  # Package source code
├── data/               # Example R datasets
├── inst/
│   ├── extdata/        # Example input files
│   ├── quarto/         # Report template and styling
│   └── scripts/        # Command-line front-end
├── tests/              # Automated tests
├── vignettes/          # Package tutorial
├── DESCRIPTION
├── NAMESPACE
├── Dockerfile
└── README.md
```

---

## Citation

If you use `clinprog` in your research, please cite the original publication describing the Reboot method:

> Felipe R. C. dos Santos, Gabriela D. A. Guardia, Filipe F. dos Santos, Daniel T. Ohara, and Pedro A. F. Galante. *Reboot: a straightforward approach to identify genes and splicing isoforms associated with cancer patient prognosis.* **NAR Cancer** 3(2), June 2021, zcab024.
> https://doi.org/10.1093/narcan/zcab024

---

## Authors

`clinprog` was developed by:

* Filipe Ferreira dos Santos
* Felipe Rodolfo Camargo dos Santos
* Gabriela Der Agopian Guardia
* Daniel Takatori Ohara
* Pedro Alexandre Favoretto Galante
* Thiago Luiz Araujo Miller

---

## License

`clinprog` is free and open-source software distributed under the GNU General Public License, version 3 or later (GPL-3).

See the [`LICENSE`](LICENSE) file for the full license text.

