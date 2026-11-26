# ==============================================================================
# 107.258 Computational Statistics (WS 2026/27, TU Wien)
# Module: R/03_pca_svd.R
# Lead:   Justus Becker (PCA/SVD & Numerical Linear Algebra)
# ==============================================================================
# Purpose:
#   1) Load & center training pixel matrix X strictly on training observations ( done )
#   2) Compute Thin-SVD (X_c = U Sigma V^T) without forming X^T X ( done )
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

# ------------------------------------------------------------------------------
# Step 2: Thin-SVD, variance explained, K selection (80% threshold) & Scree Plot
# ------------------------------------------------------------------------------

#' Compute Thin-SVD on centered training matrix X_c = U Sigma V^T
#'
#' Avoids explicitly forming the 784x784 Gram matrix X_c^T X_c (which squares
#' the condition number kappa(X^T X) = kappa(X)^2).
compute_thin_svd <- function(X_centered, var_threshold = 0.80) {
  stopifnot(is.matrix(X_centered), is.numeric(X_centered))
  stopifnot(var_threshold > 0 && var_threshold < 1)

  # Base R svd() computes the Thin-SVD by default (nu = min(n, p), nv = min(n, p))
  svd_res <- svd(X_centered)

  # Variance explained by each principal component: d_j^2 / sum(d^2)
  d             <- svd_res$d
  var_explained <- (d^2) / sum(d^2)
  cum_var       <- cumsum(var_explained)

  # Smallest K reaching the variance thresholds (80%, 90%, 95%)
  K_selected <- which(cum_var >= var_threshold)[1L]
  K_90       <- which(cum_var >= 0.90)[1L]
  K_95       <- which(cum_var >= 0.95)[1L]

  # PC scores Z = U %*% Sigma = X_centered %*% V (vectorized column scaling)
  scores <- sweep(svd_res$u, 2L, d, FUN = "*")
  colnames(scores) <- paste0("PC", seq_len(ncol(scores)))

  list(
    d             = d,
    v             = svd_res$v,
    scores        = scores,
    var_explained = var_explained,
    cum_var       = cum_var,
    K_80          = K_selected,
    K_90          = K_90,
    K_95          = K_95
  )
}

#' Save publication-ready Scree & Cumulative Variance plot to PNG
plot_scree_cumulative <- function(
    var_explained,
    cum_var,
    K_80,
    max_pc_scree = 30L,
    max_pc_cum   = 150L,
    out_path     = file.path("output", "figures", "fig_03_scree_cumulative.png")
) {
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

  png(filename = out_path, width = 1200, height = 520, res = 130)
  op <- par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3.2, 1.5))
  on.exit({
    par(op)
    dev.off()
  }, add = TRUE)

  # Left panel: Scree plot (individual variance explained for first 30 PCs)
  pcs_scree <- seq_len(min(max_pc_scree, length(var_explained)))
  plot(
    x    = pcs_scree,
    y    = var_explained[pcs_scree] * 100,
    type = "b",
    pch  = 19,
    cex  = 0.8,
    col  = "#1f4e79",
    xlab = "Principal Component (j)",
    ylab = "Variance Explained (%)",
    main = sprintf("Scree Plot (First %d PCs)", length(pcs_scree))
  )
  grid(col = "gray85", lty = "dotted")

  # Right panel: Cumulative variance explained with 80% threshold line
  pcs_cum <- seq_len(min(max_pc_cum, length(cum_var)))
  plot(
    x    = pcs_cum,
    y    = cum_var[pcs_cum] * 100,
    type = "l",
    lwd  = 2.2,
    col  = "#1f4e79",
    ylim = c(0, 100),
    xlab = "Number of Principal Components (K)",
    ylab = "Cumulative Variance Explained (%)",
    main = "Cumulative Variance Explained"
  )
  grid(col = "gray85", lty = "dotted")
  abline(h = 80, col = "#c0392b", lty = 2, lwd = 1.5)
  abline(v = K_80, col = "#c0392b", lty = 2, lwd = 1.5)
  points(x = K_80, y = cum_var[K_80] * 100, pch = 19, col = "#c0392b", cex = 1.1)
  text(
    x      = K_80 + 5,
    y      = 72,
    labels = sprintf("K = %d (%.1f%%)", K_80, cum_var[K_80] * 100),
    col    = "#c0392b",
    adj    = 0,
    font   = 2
  )

  invisible(out_path)
}

message("[03_pca_svd] Computing Thin-SVD on 12000 x 784 centered matrix...")
t_start <- proc.time()
pca_res <- compute_thin_svd(X_centered, var_threshold = 0.80)
elapsed <- (proc.time() - t_start)[["elapsed"]]

message(sprintf("[03_pca_svd] Thin-SVD finished in %.2f seconds.", elapsed))
message(sprintf(
  "[03_pca_svd] Top 6 PCs variance explained (%%): %s",
  paste(sprintf("PC%d=%.1f%%", 1:6, pca_res$var_explained[1:6] * 100), collapse = ", ")
))
message(sprintf(
  "[03_pca_svd] Components needed -> 80%%: K=%d (%.2f%%) | 90%%: K=%d | 95%%: K=%d",
  pca_res$K_80, pca_res$cum_var[pca_res$K_80] * 100, pca_res$K_90, pca_res$K_95
))

scree_file <- plot_scree_cumulative(
  var_explained = pca_res$var_explained,
  cum_var       = pca_res$cum_var,
  K_80          = pca_res$K_80
)
message("[03_pca_svd] Saved scree plot to: ", scree_file)

# ------------------------------------------------------------------------------
# Step 3: Reconstruct & plot leading columns of V as upright 28x28 Eigenimages
# ------------------------------------------------------------------------------

#' Reshape a 784-length loading vector into an upright 28x28 matrix for image()
#'
#' Base R image() places row 1 at the bottom and columns along the x-axis.
#' Applying t(apply(mat, 2, rev)) aligns Fashion-MNIST shoes upright.
vec_to_upright_matrix <- function(vec) {
  stopifnot(is.numeric(vec), length(vec) == 784L)
  mat <- matrix(vec, nrow = 28L, ncol = 28L, byrow = TRUE)
  t(apply(mat, 2L, rev))
}

#' Plot the first n_pcs eigenimages (columns of V) with a diverging color scale
plot_eigenimages <- function(
    V,
    var_explained,
    n_pcs    = 6L,
    out_path = file.path("output", "figures", "fig_03_eigenimages.png")
) {
  stopifnot(is.matrix(V), nrow(V) == 784L, n_pcs >= 1L, n_pcs <= ncol(V))
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

  # Diverging palette: Blue = negative loading, White = zero, Red = positive loading
  div_pal <- colorRampPalette(c("#2166ac", "#f7f7f7", "#b2182b"))(256L)

  png(filename = out_path, width = 1200, height = 820, res = 130)
  op <- par(mfrow = c(2, 3), mar = c(1.5, 1.5, 3.0, 1.5), oma = c(2.5, 0.5, 2.5, 0.5))
  on.exit({
    par(op)
    dev.off()
  }, add = TRUE)

  for (j in seq_len(n_pcs)) {
    eigen_mat <- vec_to_upright_matrix(V[, j])
    max_abs   <- max(abs(eigen_mat))

    image(
      x    = 1:28,
      y    = 1:28,
      z    = eigen_mat,
      zlim = c(-max_abs, max_abs),
      col  = div_pal,
      axes = FALSE,
      xlab = "",
      ylab = "",
      main = sprintf("Eigenimage PC%d (%.1f%% var.)", j, var_explained[j] * 100)
    )
    box(col = "gray40")
  }

  mtext("Leading 6 Eigenimages (Columns of V, 28x28)", outer = TRUE, side = 3, line = 0.6, cex = 1.25, font = 2)
  mtext("Color scale: Blue = negative loading | White = 0 | Red = positive loading", outer = TRUE, side = 1, line = 0.8, cex = 0.95, col = "gray30")

  invisible(out_path)
}

eigen_file <- plot_eigenimages(
  V             = pca_res$v,
  var_explained = pca_res$var_explained,
  n_pcs         = 6L
)
message("[03_pca_svd] Saved eigenimages plot to: ", eigen_file)

# Print class-wise mean PC1..PC6 scores to help interpret signs (Sneaker=0 vs Boot=1)
mean_sneaker <- colMeans(pca_res$scores[y_train == 0L, 1:6, drop = FALSE])
mean_boot    <- colMeans(pca_res$scores[y_train == 1L, 1:6, drop = FALSE])
message("[03_pca_svd] Mean PC1..PC6 scores (Sneaker vs. Ankle boot):")
for (j in 1:6) {
  message(sprintf(
    "  PC%d: Sneaker = %+6.2f | Ankle boot = %+6.2f | Diff (Boot - Sneaker) = %+6.2f",
    j, mean_sneaker[j], mean_boot[j], mean_boot[j] - mean_sneaker[j]
  ))
}

# ------------------------------------------------------------------------------
# Step 4: Numerical conditioning diagnostics & save pca_svd.rds for downstream use
# ------------------------------------------------------------------------------

#' Compute effective numerical rank and condition numbers kappa(X) vs kappa(X^T X)
compute_conditioning_summary <- function(d, tol = 1e-10) {
  stopifnot(is.numeric(d), length(d) >= 2L)
  d_pos   <- d[d > tol]
  kappa_X <- max(d_pos) / min(d_pos)

  list(
    effective_rank = length(d_pos),
    d_max          = max(d_pos),
    d_min_pos      = min(d_pos),
    d_smallest     = min(d),
    kappa_X        = kappa_X,
    kappa_XtX      = kappa_X^2
  )
}

cond_info <- compute_conditioning_summary(pca_res$d)
message(sprintf(
  "[03_pca_svd] Conditioning -> effective rank: %d/%d | kappa(X_c) = %.2e | kappa(X_c^T X_c) = %.2e",
  cond_info$effective_rank, length(pca_res$d), cond_info$kappa_X, cond_info$kappa_XtX
))

# Save compact PCA/SVD bundle for Roman (04_outliers_robust.R) and Lisa (05_models_cv.R)
pca_bundle <- list(
  center           = pixel_mean,
  d                = pca_res$d,
  var_explained    = pca_res$var_explained,
  cum_var          = pca_res$cum_var,
  K_80             = pca_res$K_80,
  K_90             = pca_res$K_90,
  K_95             = pca_res$K_95,
  v_top            = pca_res$v[, seq_len(pca_res$K_90), drop = FALSE],
  scores_K80       = pca_res$scores[, seq_len(pca_res$K_80), drop = FALSE],
  y_train          = y_train,
  n_zero_var       = n_zero_var,
  n_near_zero_var  = n_near_zero_var,
  conditioning     = cond_info
)

rds_path <- file.path("output", "models", "pca_svd.rds")
dir.create(dirname(rds_path), recursive = TRUE, showWarnings = FALSE)
saveRDS(pca_bundle, file = rds_path)
message("[03_pca_svd] Saved PCA/SVD bundle to: ", rds_path)
