##### PREPARE R ENV #####
# Installs clinprog R package
# install.packages("../clinprog_0.3.0.tar.gz", repos = NULL, type = "source")

# Loads clinprog R package
library(clinprog)
#########################

# Loads Shiny app to run analyses with a GUI locally (no server)
run_app()

##### OUTPUT DIRECTORY #####
# All files below are written to a temporary directory. Change it to keep them
out_dir <- tempdir()
############################

##### LOADS TOY DATASETS #####
# Loads internal (simulated) datasets
# Alternatively, users can run read_table() --> see vignette (Import utilities) for details
data("toy_gene_expression")
data("toy_clinics")

# Users have the option to load previous results saved in RDS
# result_complete <- read_rds(file = "toy_testComplete.rds")
##############################

##### EXECUTES clinprog #####
# Runs clinprog complete workflow --> modules I (regression) + II (survival)
# 'toy_gene_expression' has raw expression values, hence norm_exp = TRUE.
# The toy datasets were validated with skip_survival_checks = TRUE;
# keep the default (FALSE) for real data.
result_complete <- run_complete(
  data = toy_gene_expression,
  output_prefix = file.path(out_dir, "toy_testComplete"),
  norm_exp = TRUE,
  bootstrap = 30,
  group_size = 10,
  percentage_filter = 0.3,
  variance_filter = 0.01,
  max_follow_up = NULL,
  feature_type = "gene",
  min_signature_size = 2,
  min_signature_cutoff = "roc",
  run_multivariate = TRUE,
  clinical_data = toy_clinics,
  score_cutoff = "roc",
  p_value_cutoff = 0.2,
  skip_survival_checks = TRUE,
  save_plots = TRUE,
  save_table = TRUE,
  save_json = TRUE,
  save_rds = TRUE,
  report_format = FALSE,
  verbose = 1,
  n_cores = 1,
  seed = 123,
  save_log = TRUE
)
###########################

##### INSPECT OBJECTS #####
# Explores the "clinprog_complete" S3 object
result_complete$regression  # "clinprog_signature" S3 object
result_complete$survival    # "clinprog_survival" S3 object
result_complete$params
result_complete$metadata
result_complete$call

# Tests S3 methods
print(result_complete)
print(summary(result_complete))
summary(result_complete)
coef(result_complete)
plot(result_complete)
###########################

##### FIGURES & TABLES #####
prefix <- file.path(out_dir, "extra_toy_testComplete")
surv <- result_complete$survival

### MODULE I - REGRESSION

# Exports plots manually
plot_histogram(result_complete$regression$signature, output_prefix = prefix)
plot_lollipop(result_complete$regression$signature, output_prefix = prefix)

# Exports signature manually
write_signature(result_complete$regression, paste0(prefix, "_signature.txt"))

### MODULE II - SURVIVAL

# Exports main plots manually
plot_boxplot(data = surv$expression_data, signature = surv$signature, output_prefix = prefix)
plot_scatter(data = surv$expression_data, signature = surv$signature, output_prefix = prefix)
plot_wordcloud(data = surv$expression_data, signature = surv$signature, output_prefix = prefix)
plot_barplot_score(data = surv$expression_data, score_cutoff_value = surv$score_cutoff_value,
                   output_prefix = prefix)
plot_lineplot_score(data = surv$expression_data, score_cutoff_value = surv$score_cutoff_value,
                    output_prefix = prefix)
plot_roc(x = surv$roc_result, output_prefix = prefix)

uni <- surv$survival$univariate$table
plot_km(data = surv$expression_data, score_cutoff_value = surv$score_cutoff_value,
        p_value = paste0("p = ", uni$log.rank.pvalue, "\nHR: ", uni$hazard.ratio),
        output_prefix = prefix)
plot_swimmer(data = surv$expression_data, score_cutoff_value = surv$score_cutoff_value,
             facet_by = "score_group", output_prefix = prefix)

# Multivariate-only plots
plot_ph(x = surv$ph_result, is_multi = !is.null(surv$survival$multivariate), output_prefix = prefix)
plot_forest(model_object = surv$survival$multivariate$model, output_prefix = prefix)
plot_clinics(data = surv$clinical_data, output_prefix = prefix)

# Only available when the multivariate analysis used bootstrap resampling
if (!is.null(surv$survival$boot_table)) {
  plot_barplot(plot_df = surv$survival$boot_table, type = "absolute",
               bootstrap = 100, output_prefix = prefix)
}

# Exports updated signature manually
write_signature(surv, paste0(prefix, "_signature_updated.tsv"))

# Exports scores (with or without clinical data) manually
write_score(surv, paste0(prefix, "_scoreCont.tsv"))
write_score(surv, paste0(prefix, "_scoreCat.tsv"), include_clinical = TRUE)

# Exports uni and multivariate survival results manually
write_univariate(surv, paste0(prefix, "_uniCox.txt"))
write_multivariate(surv, paste0(prefix, "_multiCox.txt"))

### EXTRA

# Exports metadata manually for reproducibility
write_metadata(module = "run_complete", output_prefix = prefix, command = "none",
               parameters = list(data = "data.frame", clinical_data = "data.frame",
                                 output_prefix = prefix, run_multivariate = TRUE, score_cutoff = "roc",
                                 group_size = 10, percentage_filter = 0.3, variance_filter = 0.01,
                                 max_follow_up = NULL, p_value_cutoff = 0.2, bootstrap = 30,
                                 feature_type = "gene", skip_survival_checks = TRUE, save_plots = TRUE,
                                 save_table = TRUE, save_json = TRUE, save_rds = TRUE,
                                 report_format = FALSE, save_log = TRUE))

# Recovers LOG
write_log(file = paste0(prefix, ".log"))

# Saves result in RDS for flexibility
write_rds(object = result_complete, file = paste0(prefix, ".rds"))

# Generates final report for collaborators (requires the 'quarto' package and Quarto installed)
write_report(object = result_complete, format = "PDF", file = paste0(prefix, "_report_complete.pdf"))
############################

##### VIGNETTE #####
# Explores the clinprog tutorial
browseVignettes("clinprog")
vignette("clinprog-tutorial", package = "clinprog")
####################
