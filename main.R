# ==============================================================================
# 107.258 Computational Statistics (WS 2026/27, TU Wien)
# Group 12 - Topic 12: Image Data (Fashion-MNIST: Sneaker vs. Ankle Boot)
# Master Pipeline Script: main.R
# ==============================================================================
# Note: Run from the project root (CompStat-Data-Image.Rproj).
# Only relative paths are used; do not call setwd().

set.seed(107258)

# Ensure output directories exist
dir.create(file.path("output", "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("output", "models"),  recursive = TRUE, showWarnings = FALSE)
dir.create("report",                       recursive = TRUE, showWarnings = FALSE)

# Helper to source pipeline modules cleanly once implemented
run_module <- function(script_path) {
  if (file.exists(script_path)) {
    message("Running module: ", script_path)
    source(script_path, local = FALSE)
  } else {
    message("Skipping (not yet implemented): ", script_path)
  }
}

# 1. Data preparation & strict train/test separation (fashion-mnist_test.csv locked until Jan 7)
run_module(file.path("R", "01_data_prep.R"))

# 2. Exploratory data analysis & visualization helpers
run_module(file.path("R", "02_eda_plots.R"))

# 3. Centered Thin-SVD, PCA scores, scree plot & 28x28 eigenimages
run_module(file.path("R", "03_pca_svd.R"))

# 4. Multivariate outlier detection (CovMcd, PcaHubert) & sensitivity diagnostics
run_module(file.path("R", "04_outliers_robust.R"))

# 5. Downstream classification (glm, glmnet, glmrob, raw-pixel baseline) & 5-fold CV
run_module(file.path("R", "05_models_cv.R"))

message("Pipeline finished.")
