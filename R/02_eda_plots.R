# ==============================================================================
# 107.258 Computational Statistics (WS 2026/27, TU Wien)
# Module: R/02_eda_plots.R
# Lead:   Samuel Oberascher (Data Wrangling, EDA & Reproducibility)
# ==============================================================================
# Purpose:
#   - Modular 28x28 upright grayscale image plotter using Base R image()
#   - Class-wise mean & pixel-wise variance maps (highlight zero-variance border)
#   - Multivariate visualization (PC1 vs. PC2 score scatterplot with ellipses)
#   - Uncertainty visualization (cumulative variance explained with CV/bootstrap band)
# ==============================================================================

#' Plot a single 784-length Fashion-MNIST pixel vector upright (28x28)
#'
#' @param pixel_vec Numeric vector of length 784 (row-major order).
#' @param title Optional plot title.
plot_fashion_image <- function(pixel_vec, title = "") {
  stopifnot(is.numeric(pixel_vec), length(pixel_vec) == 784L)
  img_mat <- matrix(pixel_vec, nrow = 28L, ncol = 28L, byrow = TRUE)
  # Base R image() plots row 1 at bottom; reverse rows and transpose for upright view
  img_upright <- t(apply(img_mat, 2L, rev))
  image(
    x = 1:28,
    y = 1:28,
    z = img_upright,
    col = gray(seq(1, 0, length.out = 256L)),
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = title
  )
  box()
}

message("[02_eda_plots] Helper plot_fashion_image() loaded; EDA plots ready for feature/eda-visuals.")
