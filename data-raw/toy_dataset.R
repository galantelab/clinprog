# data-raw/toy_dataset.R
# Generates a fully SYNTHETIC toy dataset (no TCGA data) with planted signal.
# Outputs: toy_clinics.tsv, toy_gene_expression.tsv, toy_transcript_expression.tsv
#          toy_truth.tsv -> inst/fixtures/ (ground truth for tests)
# Tuned with clinprog options: bootstrap = 30, group_size = 10, norm_exp = TRUE

## ---- Options --------------------------------------------------------------
n_pat        <- 150  # patients
n_bg_genes   <- 30   # null background genes (>= 12; first 7 use real symbols)
n_bg_tx      <- 40   # null background transcripts
n_risk       <- 8    # prognostic genes, higher in worse prognosis
n_prot       <- 8    # prognostic genes, higher in better prognosis
n_risk_tx    <- 4    # prognostic transcripts (risk)
n_prot_tx    <- 4    # prognostic transcripts (protective)
zero_patient <- TRUE # one patient with all-zero transcripts (a ROW, not a column);
                     # set FALSE if your pipeline rejects it
beta_x       <- 0.7  # log-scale slope of prognostic features on the latent factors;
                     # much above 1 gives heavy tails that clinprog's variance filter (x / max(x)) removes
seed         <- 20261024
out_dir      <- "inst/extdata"
## Resulting size: genes = n_bg_genes + n_risk + n_prot + 7
##                 transcripts = n_bg_tx + n_risk_tx + n_prot_tx + 4
## -----------------------------------------------------------------------------

stopifnot(n_bg_genes >= 12, n_pat >= 20)
set.seed(seed)
dir.create(out_dir, FALSE, TRUE)
pid <- sprintf("TCGA-ZZ-%04d", sort(sample(1000:9999, n_pat))) # fake site code ZZ

## ---- 1. Clinics -----------------------------------------------------------
clin <- data.frame(
  Patient_ID = pid,
  Age        = sample(c("Low", "High"), n_pat, TRUE),
  Gender     = sample(c("Female", "Male"), n_pat, TRUE),
  Race       = sample(c("White", "Not white"), n_pat, TRUE, prob = c(.85, .15)), # unbalanced
  Histotype  = sample(c("Astrocytoma", "Oligodendroglioma"), n_pat, TRUE, prob = c(.65, .35)),
  stringsAsFactors = FALSE
)
clin$Grade <- ifelse(runif(n_pat) < ifelse(clin$Histotype == "Astrocytoma", .6, .4), "G3", "G2")

## ---- 2. Two independent latent factors and survival -------------------------
u <- as.numeric(scale(rnorm(n_pat)))   # latent RISK factor       (higher = worse)
v <- as.numeric(scale(rnorm(n_pat)))   # latent PROTECTIVE factor (higher = better)
lp <- 1.0 * u - 1.0 * v + 0.5 * (clin$Grade == "G3") + 0.4 * (clin$Age == "High")
k  <- 1.2; lambda <- log(2) / 1200^k   # baseline median ~1200 days
T_event <- (-log(runif(n_pat)) / (lambda * exp(lp)))^(1 / k)
T_cens  <- runif(n_pat, 300, 2000)  # events ~55-65% (clinprog requires 20-80%)
OS      <- as.integer(T_event <= T_cens)            # 1 = dead, 0 = alive
OS.time <- pmax(1L, as.integer(ceiling(pmin(T_event, T_cens))))
OS[1] <- 0L; OS.time[1] <- 8L                       # edge case: very short follow-up
message("events: ", sum(OS), " of ", n_pat)

## ---- 3. Expression simulator ------------------------------------------------
X <- cbind(u = u, v = v,
           oligo = as.numeric(clin$Histotype == "Oligodendroglioma"),
           male  = as.numeric(clin$Gender == "Male"),
           g3    = as.numeric(clin$Grade == "G3"))

simulate <- function(spec, lib) {
  B    <- as.matrix(spec[, c("u", "v", "oligo", "male", "g3")])
  mu   <- exp(X %*% t(B)) * rep(spec$mu, each = n_pat) * lib
  size <- rep(runif(nrow(spec), 10, 30), each = n_pat)
  matrix(rnbinom(length(mu), mu = mu, size = size), n_pat,
         dimnames = list(pid, spec$name))
}
mk <- function(name, mu, u = 0, v = 0, oligo = 0, male = 0, g3 = 0)
  data.frame(name, u, v, oligo, male, g3, mu)

## Pipeline rejects columns containing only zeros: give any such column
## one small non-zero value (never in row 3, the optional all-zero patient).
fix_zero_cols <- function(m, val) {
  for (j in which(colSums(m != 0) == 0))
    m[sample(setdiff(seq_len(n_pat), 3), 1), j] <- val
  m
}

## NOTE: clinprog's variance filter works on x / max(x) AFTER log2(x+1) when norm_exp = TRUE.
## High-mean genes get compressed and are removed, so gene means are kept low/wide here
## (planted genes 20-200) so that the default variance_filter = 0.01 keeps them.
## ---- 4. Genes -----------------------------------------------------------------
bg_genes <- c("A1BG", "A1CF", "A2LD1", "A2ML1", "A2M", "A4GALT", "A4GNT",
              sprintf("TOYG%03d", seq_len(n_bg_genes - 7)))
gene_spec <- rbind(
  mk(bg_genes,                   exp(rnorm(length(bg_genes), 3.5, 1.8))),  # wide range, like real RSEM
  mk(paste0("TOYRISK", seq_len(n_risk)), runif(n_risk, 20, 200), u = beta_x),
  mk(paste0("TOYPROT", seq_len(n_prot)), runif(n_prot, 20, 200), v = beta_x),
  mk(paste0("TOYHIST", 1:3),     runif(3, 20, 200), oligo = 1.5),
  mk("TOYSEX1",                  80,                  male = 1.2),
  mk(paste0("TOYGRADE", 1:2),    runif(2, 20, 200), g3 = 1.0),
  mk("TOYLOWVAR",                0)   # near-zero variance (one non-zero value, set below)
)
lib_g <- exp(rnorm(n_pat, 0, 0.25)); lib_g[7] <- 2   # edge case: high library size (kept mild: clinprog's variance filter divides by the column max)
gm <- simulate(gene_spec, lib_g)
gm <- round(gm * exp(rnorm(length(gm), 0, 0.1)), 4)  # RSEM-like non-integers
gm[10, "TOYG005"] <- round(max(gm[, "TOYG005"]) * 20, 4) # edge case: outlier
gm[20, "TOYLOWVAR"] <- 1.2345                            # edge case: near-zero variance
gm <- fix_zero_cols(gm, 1.0)

## ---- 5. Transcripts -----------------------------------------------------------
bg_tx <- unique(paste0("MSTRG.", sample(1:5000, n_bg_tx * 2), ".", sample(1:20, n_bg_tx * 2, TRUE)))[seq_len(n_bg_tx)]
tx_spec <- rbind(
  mk(bg_tx,                      exp(rnorm(n_bg_tx, 1.5, 1.2))),
  mk(paste0("MSTRG.9001.", seq_len(n_risk_tx)), runif(n_risk_tx, 10, 40), u = beta_x),
  mk(paste0("MSTRG.9002.", seq_len(n_prot_tx)), runif(n_prot_tx, 10, 40), v = beta_x),
  mk(paste0("MSTRG.9003.", 1:2), runif(2, 10, 40), oligo = 1.5),
  mk("MSTRG.9004.1",             20,               g3 = 1.0),
  mk("MSTRG.9999.1",             0)   # near-zero variance (one non-zero value, set below)
)
tm <- simulate(tx_spec, exp(rnorm(n_pat, 0, 0.2)))
tm <- tm * rlnorm(length(tm), 0, 0.5) * 0.05          # TPM-like scale
tm[rbinom(length(tm), 1, 0.3) == 1] <- 0              # extra sparsity
tm <- round(tm, 6)
tm[20, "MSTRG.9999.1"] <- 0.012345                    # edge case: near-zero variance
tm <- fix_zero_cols(tm, 0.01)                         # no all-zero columns allowed
if (zero_patient) tm[3, ] <- 0                        # edge case: all-zero patient (row)

## ---- 6. Write files ------------------------------------------------------------
wr <- function(df, f) write.table(df, file.path(out_dir, f), sep = "\t",
                                  quote = FALSE, row.names = FALSE)
wr(clin, "toy_clinics.tsv")
wr(data.frame(Patient_ID = pid, OS, OS.time, gm, check.names = FALSE),
   "toy_gene_expression.tsv")

tm_chr <- apply(tm, 2, function(x) sprintf("%.6f", x))
if (zero_patient) tm_chr[3, ] <- "0.0"                # mimic real "0.0" row
wr(data.frame(Patient_ID = pid, OS, OS.time, tm_chr, check.names = FALSE,
              stringsAsFactors = FALSE), "toy_transcript_expression.tsv")

cols <- c("u", "v", "oligo", "male", "g3")
truth <- rbind(
  data.frame(file = "gene",       feature = gene_spec$name, gene_spec[, cols]),
  data.frame(file = "transcript", feature = tx_spec$name,   tx_spec[, cols]))
truth <- truth[rowSums(abs(truth[, cols])) > 0, ]
names(truth)[3:7] <- c("risk_logFC", "protective_logFC", "oligo_logFC",
                       "male_logFC", "grade3_logFC")
dir.create("inst/fixtures", FALSE, TRUE)
write.table(truth, file.path("inst/fixtures", "toy_truth.tsv"), sep = "\t",
            quote = FALSE, row.names = FALSE)
message("genes: ", ncol(gm), "; transcripts: ", ncol(tm))

