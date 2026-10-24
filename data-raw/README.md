# Toy dataset for clinprog

This folder documents how the datasets shipped with **clinprog** were created.
Everything is **simulated** with a fixed seed. No TCGA data are included or
derived, so there are no data-access or redistribution concerns.

## Files produced

| File | Location in the package | Content |
|---|---|---|
| `toy_clinics.tsv` | `inst/extdata/` | Clinical table: `Patient_ID`, `Age`, `Gender`, `Race`, `Histotype`, `Grade` |
| `toy_gene_expression.tsv` | `inst/extdata/` | `Patient_ID`, `OS`, `OS.time` + gene expression (RSEM-like, 4 decimals) |
| `toy_transcript_expression.tsv` | `inst/extdata/` | `Patient_ID`, `OS`, `OS.time` + transcript expression (TPM-like, 6 decimals, sparse) |
| `toy_truth.tsv` | `inst/fixtures/` | Ground truth: which features carry which planted effect (for tests) |
| `toy_clinics`, `toy_gene_expression`, `toy_transcript_expression` | `data/` (`.rda`) | The TSVs read back with the package's own `read_table()`; patient IDs are row names |

`OS` is 1 = dead, 0 = alive/censored. `OS` and `OS.time` are identical in the
gene and transcript files. Patient IDs have the form `TCGA-ZZ-####`
(site code `ZZ` is fake).

## How to regenerate

From the package root:

```r
source("data-raw/toy_dataset.R")
```

The script writes the TSVs to `inst/extdata/` and the truth file to
`inst/fixtures/`. Then rebuild the `.rda` files from those TSVs (so `data/`
and `inst/extdata/` cannot drift apart):

```r
toy_clinics               <- read_table("inst/extdata/toy_clinics.tsv")
toy_gene_expression       <- read_table("inst/extdata/toy_gene_expression.tsv")
toy_transcript_expression <- read_table("inst/extdata/toy_transcript_expression.tsv")
usethis::use_data(toy_clinics, toy_gene_expression,
                  toy_transcript_expression, compress = "xz", overwrite = TRUE)
```

Always regenerate all files together. Changing any option or the seed changes
every value. If you change the number of patients or features, update the
dimensions in `R/data.R`.

## Design

**Patients and clinical data.** 150 patients. Binary categorical variables with
the same labels as the real TCGA-LGG annotations. `Race` is unbalanced on
purpose (about 15% `Not white`).

**Survival.** Two independent latent factors drive the hazard (Weibull,
shape 1.2, baseline median about 1200 days):
a *risk* factor (higher is worse) and a *protective* factor (higher is better),
plus smaller effects of `Grade == "G3"` and `Age == "High"`.
Censoring is uniform between 300 and 2000 days. The script prints the number
of events. `clinprog` requires the event proportion to be between 20% and 80%
unless `skip_survival_checks = TRUE`.

**Expression.** Counts come from a negative binomial with feature-specific
means, library-size variation and moderate dispersion, then scaled to look like
RSEM values (genes) or sparse TPM values (transcripts, 30% extra zeros).

**Planted signal** (default options):

| Features | Effect |
|---|---|
| `TOYRISK1-8`, `MSTRG.9001.1-4` | Increase with the risk factor |
| `TOYPROT1-8`, `MSTRG.9002.1-4` | Increase with the protective factor |
| `TOYHIST1-3`, `MSTRG.9003.1-2` | Higher in Oligodendroglioma |
| `TOYSEX1` | Higher in males |
| `TOYGRADE1-2`, `MSTRG.9004.1` | Higher in Grade 3 |
| All other features | Null background (genes: the 7 real symbols `A1BG`, `A1CF`, `A2LD1`, `A2ML1`, `A2M`, `A4GALT`, `A4GNT` and `TOYG###`; transcripts: random `MSTRG.x.y`) |

Because the effects act through shared latent factors, features of the same
group are correlated with each other, like a real signature.

**Deliberate edge cases.**
- `TOYLOWVAR` and `MSTRG.9999.1`: a single non-zero value (near-zero variance; removed by the variance filter).
- Gene `TOYG005`: one large outlier value.
- One patient with a higher library size.
- One patient with very short follow-up (`OS.time` = 8, censored).
- One patient with all-zero transcript expression, written as `0.0` as in the real files (option `zero_patient`).
- Unbalanced `Race` groups.

## Constraints from clinprog that shaped the data

These are the reasons the data look the way they do. If `clinprog` changes,
revisit them.

1. **No all-zero columns.** The input check rejects them, so a guard in the
   script gives any such column one small non-zero value. Near-zero variance is
   tested with a single non-zero value instead.
2. **Coefficient threshold.** A feature is kept only if its mean bootstrap
   coefficient is at least 0.0035 (genes) or 0.011 (transcripts). On raw gene
   values (hundreds to thousands) coefficients are far too small, so gene
   examples use `norm_exp = TRUE`.
3. **Variance filter.** It keeps features whose `x / max(x)` has variance above
   `variance_filter` (default 0.01). With `norm_exp = TRUE` this is computed
   after the log2 transform, which compresses high-expression genes. Gene means
   are therefore low and widely spread (planted genes 20-200), so that planted
   genes survive the default filter. Very large outliers or library sizes also
   shrink this variance, so they are kept mild.
4. **Sufficient features.** If fewer features than `group_size` survive the
   filters, `clinprog` runs a single regression, which can return all zeros.
5. **Effect size.** The log-scale slope of planted features (`beta_x`) is 0.7.
   Much larger values give heavy tails that the variance filter removes.
6. **Correlation filter.** Bootstrap iterations whose feature subset has too
   many highly correlated pairs are discarded. With the default settings the
   planted groups are not large enough to trigger this.

## Options (top of `toy_dataset.R`)

| Option | Default | Meaning |
|---|---|---|
| `n_pat` | 150 | Patients |
| `n_bg_genes`, `n_bg_tx` | 30, 40 | Null background genes / transcripts |
| `n_risk`, `n_prot` | 8, 8 | Prognostic genes (risk / protective) |
| `n_risk_tx`, `n_prot_tx` | 4, 4 | Prognostic transcripts (risk / protective) |
| `beta_x` | 0.7 | Log-scale slope of prognostic features |
| `zero_patient` | `TRUE` | Include one patient with all-zero transcripts |
| `seed` | 20261024 | Random seed |

With the defaults there are 53 genes and 52 transcripts.

## Settings used to validate the data

The data were checked with `bootstrap = 30`, `group_size = 10`,
`norm_exp = TRUE` and `skip_survival_checks = TRUE`, for both genes and
transcripts. The expected signatures contain several `TOYRISK*` (positive
coefficients) and `TOYPROT*` (negative coefficients) features. A few background
features may also pass the low coefficient threshold, so tests should check
that planted features are present, not that the signature equals them exactly.

