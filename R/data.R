#' Simulated clinical dataset for clinprog
#'
#' @description
#' A small, fully simulated clinical table intended for examples, tests and
#' vignettes. It mimics the structure of TCGA-LGG clinical annotations but
#' contains no real patient data.
#'
#' @details
#' Each row is one (fake) patient. Row names match the identifiers in
#' \code{\link{toy_gene_expression}} and \code{\link{toy_transcript_expression}}.
#' All clinical variables are categorical and binary.
#'
#' @format
#' A \code{data.frame} with 150 rows and 5 columns (patient IDs are the row names):
#' \describe{
#'   \item{(row names)}{Unique, TCGA-like patient identifiers (site code ZZ). In the raw TSV they are the \code{Patient_ID} column.}
#'   \item{Age}{Character. \code{"Low"} or \code{"High"}.}
#'   \item{Gender}{Character. \code{"Female"} or \code{"Male"}.}
#'   \item{Race}{Character. \code{"White"} or \code{"Not white"} (unbalanced on purpose).}
#'   \item{Histotype}{Character. \code{"Astrocytoma"} or \code{"Oligodendroglioma"}.}
#'   \item{Grade}{Character. \code{"G2"} or \code{"G3"}.}
#' }
#'
#' @source
#' Simulated with \code{data-raw/toy_dataset.R} (fixed seed).
#' The raw TSV file is in \code{system.file("extdata", "toy_clinics.tsv", package = "clinprog")}.
#'
#' @usage data(toy_clinics)
#'
#' @examples
#' data(toy_clinics)
#' dim(toy_clinics)
#' head(toy_clinics)
#'
#' @keywords datasets
"toy_clinics"

#' Simulated gene expression dataset with survival data for clinprog
#'
#' @description
#' A small, fully simulated gene expression matrix (RSEM-like, non-integer
#' values) together with overall survival, for examples, tests and vignettes.
#'
#' @details
#' Each row is one patient, identified by its row name. The first two columns
#' are the survival variables (\code{OS}, \code{OS.time}) and the remaining
#' columns are genes (HGNC-style symbols).
#'
#' The data contain known, planted structure, so results can be checked:
#' genes \code{TOYRISK1-8} are higher and \code{TOYPROT1-8} lower in patients
#' with worse prognosis; \code{TOYHIST1-3}, \code{TOYSEX1} and
#' \code{TOYGRADE1-2} are associated with histotype, gender and grade,
#' respectively. All other genes are null background. Edge cases are included
#' on purpose: a near-zero-variance gene (\code{TOYLOWVAR}, a single non-zero
#' value), one outlier value, one sample with a higher library size, and one
#' patient with very short follow-up.
#'
#' Expression values are on a raw (not log) scale. For these data, use
#' \code{norm_exp = TRUE} when running the regression on genes.
#'
#' @format
#' A \code{data.frame} with 150 rows and 55 columns (patient IDs are the row names):
#' \describe{
#'   \item{(row names)}{Patient identifiers (raw TSV column \code{Patient_ID}); match \code{\link{toy_clinics}}.}
#'   \item{OS}{Integer. Overall survival status (0 = alive/censored, 1 = dead).}
#'   \item{OS.time}{Integer. Overall survival time in days.}
#'   \item{A1BG, A1CF, ...}{Numeric. Expression values, 53 genes.}
#' }
#'
#' @source
#' Simulated with \code{data-raw/toy_dataset.R} (fixed seed); not derived from TCGA.
#' Raw file: \code{system.file("extdata", "toy_gene_expression.tsv", package = "clinprog")}.
#'
#' @usage data(toy_gene_expression)
#'
#' @examples
#' data(toy_gene_expression)
#' dim(toy_gene_expression)
#' head(toy_gene_expression[, 1:6])
#'
#' @keywords datasets
"toy_gene_expression"

#' Simulated transcript expression dataset with survival data for clinprog
#'
#' @description
#' A small, fully simulated transcript (splicing isoform) expression table
#' (TPM-like, sparse) together with overall survival.
#'
#' @details
#' Same layout as \code{\link{toy_gene_expression}}, with transcripts named
#' \code{MSTRG.<gene>.<isoform>}. \code{OS} and \code{OS.time} are identical to
#' those in \code{toy_gene_expression}. Transcripts \code{MSTRG.9001.*} (risk)
#' and \code{MSTRG.9002.*} (protective) carry planted prognostic signal;
#' \code{MSTRG.9003.*} and \code{MSTRG.9004.1} are associated with histotype and
#' grade. \code{MSTRG.9999.1} has a single non-zero value, and one patient has zero expression
#' for every transcript.
#'
#' @format
#' A \code{data.frame} with 150 rows and 54 columns (patient IDs are the row names):
#' \describe{
#'   \item{(row names)}{Patient identifiers (raw TSV column \code{Patient_ID}); match \code{\link{toy_clinics}}.}
#'   \item{OS}{Integer. Overall survival status (0 = alive/censored, 1 = dead).}
#'   \item{OS.time}{Integer. Overall survival time in days.}
#'   \item{MSTRG.*}{Numeric. Expression values, 52 transcripts.}
#' }
#'
#' @source
#' Simulated with \code{data-raw/toy_dataset.R} (fixed seed); not derived from TCGA.
#' Raw file: \code{system.file("extdata", "toy_transcript_expression.tsv", package = "clinprog")}.
#'
#' @usage data(toy_transcript_expression)
#'
#' @examples
#' data(toy_transcript_expression)
#' dim(toy_transcript_expression)
#' head(toy_transcript_expression[, 1:6])
#'
#' @keywords datasets
"toy_transcript_expression"
