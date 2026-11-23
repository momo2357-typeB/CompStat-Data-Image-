# ==============================================================================
# 107.258 Computational Statistics (WS 2026/27, TU Wien)
# Module: R/03_pca_svd.R
# Lead:   Justus Becker (PCA/SVD & Numerical Linear Algebra)
# ==============================================================================
# Purpose:
#   1) Load & center training pixel matrix X strictly on training observations
#   2) Compute Thin-SVD (X_c = U Sigma V^T) without forming X^T X
#   3) Select K components systematically (>= 80% variance + scree plot)
#   4) Reconstruct & plot leading columns of V as upright 28x28 eigenimages
#   5) Save PCA/SVD results to output/models/pca_svd.rds for downstream modules
# ==============================================================================

#' Load binary footwear training matrix (Class 7 = Sneaker, Class 9 = Ankle boot)
#'
#' Uses output/models/train_prepared.rds if available, otherwise falls back to
#' data/fashion-mnist_shoes_train.csv and normalizes pixel values to [0, 1].
#' 
#' 
load_training_data <- function(
    prep_path = file.path("output", "models", "train_prepared.rds"),
    csv_path  = file.path("data", "fashion-mnist_shoes_train.csv")
) {
  if (file.exists(prep_path)) {
    message("[03_pca_svd] Loading prepared training data from: ", prep_path)
    prep <- readRDS(prep_path)
    X_train <- prep$X_train
    y_train <- prep$y_train
  } else {
    stopifnot(file.exists(csv_path))
    message("[03_pca_svd] Loading fallback CSV from: ", csv_path)
    shoes_df <- read.csv(csv_path)

    # Defensive checks: expected 1 label column + 784 pixel columns, no NAs
    stopifnot(ncol(shoes_df) == 785L, sum(is.na(shoes_df)) == 0L)
    stopifnot(all(shoes_df$label %in% c(7L, 9L)))

    # Binary target: 0 = Sneaker (class 7), 1 = Ankle boot (class 9)
    y_train <- ifelse(shoes_df$label == 9L, 1L, 0L)
    X_train <- as.matrix(shoes_df[, -1L, drop = FALSE]) / 255
  }

  list(X = X_train, y = y_train)
}

#' Center pixel matrix column-wise (mean subtraction only; NO unit-variance scaling)
#'
#' Scaling by column SD is avoided because outer border pixels can have zero
#' variance and all pixels already share the same [0, 1] grayscale scale.
center_pixel_matrix <- function(X) {
  stopifnot(is.matrix(X), is.numeric(X))
  col_means  <- colMeans(X)
  X_centered <- scale(X, center = col_means, scale = FALSE)
  list(X_centered = X_centered, center = col_means)
}

# ------------------------------------------------------------------------------
# Step 1: Load and center training data
# ------------------------------------------------------------------------------
train_data <- load_training_data()
X_train    <- train_data$X
y_train    <- train_data$y

centered   <- center_pixel_matrix(X_train)
X_centered <- centered$X_centered
pixel_mean <- centered$center

# diagnostic summary
pixel_sds        <- apply(X_train, 2L, sd)
n_zero_var       <- sum(pixel_sds == 0)
n_near_zero_var  <- sum(pixel_sds < 0.01)

message(sprintf(
  "[03_pca_svd] Matrix dimensions: %d images x %d pixels (Sneakers: %d, Ankle boots: %d)",
  nrow(X_centered), ncol(X_centered), sum(y_train == 0L), sum(y_train == 1L)
))
message(sprintf(
  "[03_pca_svd] Value range before centering: [%.3f, %.3f] | max |colMean| after centering: %.2e",
  min(X_train), max(X_train), max(abs(colMeans(X_centered)))
))
message(sprintf(
  "[03_pca_svd] Zero-variance pixels (sd == 0): %d | Near-zero-variance pixels (sd < 0.01): %d",
  n_zero_var, n_near_zero_var
))
