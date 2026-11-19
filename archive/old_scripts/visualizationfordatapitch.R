# ==============================================================================
# TU Wien - 107.258 Computational Statistics (WS 2026/27)
# Data Pitch: Fashion-MNIST (Sneaker vs. Ankle Boot)
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Daten laden (Priorität: shoes_train.csv -> train1 + train2)
# ------------------------------------------------------------------------------
shoes_file  <- "data/fashion-mnist_shoes_train.csv"
part1_file  <- "data/fashion-mnist_train1.csv"
part2_file  <- "data/fashion-mnist_train2.csv"

if (file.exists(shoes_file)) {
  cat("Lese vorhandenen Schuh-Datensatz:", shoes_file, "\n")
  shoes_df <- read.csv(shoes_file)
} else if (file.exists(part1_file) && file.exists(part2_file)) {
  cat("Lese und verbinde Trainings-Dateien 1 & 2 via rbind()...\n")
  df1 <- read.csv(part1_file)
  df2 <- read.csv(part2_file)
  full_train <- rbind(df1, df2)
  
  # Auf Zielklassen 7 (Sneaker) und 9 (Ankle boot) filtern
  shoes_df <- subset(full_train, label %in% c(7, 9))
} else {
  message("Hinweis: CSVs nicht im Ordner 'data/' gefunden. Erzeuge synthetische Testdaten...")
  set.seed(107258)
  n_mock <- 2000
  mock_labels <- sample(c(7, 9), size = n_mock, replace = TRUE)
  mock_pixels <- matrix(rbeta(n_mock * 784, 0.4, 4) * 255, nrow = n_mock, ncol = 784)
  # Morphologische Schafthöhe für Boots simulieren
  idx_boot <- which(mock_labels == 9)
  mock_pixels[idx_boot, 140:350] <- mock_pixels[idx_boot, 140:350] + 90
  mock_pixels[mock_pixels > 255] <- 255
  shoes_df <- data.frame(label = mock_labels, mock_pixels)
  colnames(shoes_df)[-1] <- paste0("pixel", 1:784)
}

# Labels als lesbaren Faktor kodieren
shoes_df$label_name <- factor(shoes_df$label, levels = c(7, 9), 
                              labels = c("Sneaker", "Ankle boot"))

# Pixelmatrix X extrahieren (784 Spalten)
pixel_cols <- grep("^pixel", colnames(shoes_df))
if (length(pixel_cols) == 0) pixel_cols <- 2:785 # Fallback falls Spalten anders benannt sind

X_raw  <- as.matrix(shoes_df[, pixel_cols])
labels <- shoes_df$label_name
n_obs  <- nrow(X_raw)

# ------------------------------------------------------------------------------
# 1. Modulare Kernfunktionen (Base R Engine)
# ------------------------------------------------------------------------------

# 28x28 Matrix-Rendering mit korrekter Ausrichtung
plot_image_matrix <- function(vec, title = "", col_pal = gray(0:255 / 255), zlim = NULL) {
  mat <- matrix(as.numeric(vec), nrow = 28, byrow = TRUE)
  mat_oriented <- t(mat)[, 28:1]
  if (is.null(zlim)) {
    image(1:28, 1:28, mat_oriented, col = col_pal, axes = FALSE, xlab = "", ylab = "", main = title)
  } else {
    image(1:28, 1:28, mat_oriented, col = col_pal, axes = FALSE, xlab = "", ylab = "", main = title, zlim = zlim)
  }
  box()
}

# Geometrischer Massenschwerpunkt (Center of Mass)
calc_center_of_mass <- function(img_vec) {
  mat <- matrix(as.numeric(img_vec), nrow = 28, byrow = TRUE)
  total <- sum(mat)
  if (total == 0) return(c(14.5, 14.5))
  grid_x <- col(mat)
  grid_y <- 28 - row(mat) + 1
  c(cx = sum(grid_x * mat) / total, cy = sum(grid_y * mat) / total)
}

# Bivariate Konfidenzellipse (Mahalanobis Radius)
calc_ellipse <- function(center, cov_mat, level = 0.95, npoints = 100) {
  radius <- sqrt(qchisq(level, df = 2))
  eig <- eigen(cov_mat)
  theta <- seq(0, 2 * pi, length.out = npoints)
  circle <- rbind(cos(theta), sin(theta))
  transform_mat <- radius * eig$vectors %*% diag(sqrt(eig$values))
  ellipse_pts <- t(transform_mat %*% circle) + matrix(center, nrow = npoints, ncol = 2, byrow = TRUE)
  return(ellipse_pts)
}


# ==============================================================================
# FOLIE 3: PRELIMINARY EDA & DATENCHARAKTERISTIKA
# ==============================================================================

# ------------------------------------------------------------------------------
# Plot 1: Mean Images & Differenzbild (Morphologische Diskrepanz)
# ------------------------------------------------------------------------------
mean_sneaker <- colMeans(X_raw[labels == "Sneaker", ])
mean_boot    <- colMeans(X_raw[labels == "Ankle boot", ])
diff_image   <- mean_boot - mean_sneaker # Positive Werte = Schaftüberschuss

# Farbpaletten
gray_pal <- gray(0:255 / 255)
div_pal  <- colorRampPalette(c("royalblue", "white", "firebrick"))(256)
max_abs_diff <- max(abs(diff_image))

par(mfrow = c(1, 3), mar = c(1, 1, 3, 1))
plot_image_matrix(mean_sneaker, title = "Mean Image: Sneaker (7)", col_pal = gray_pal)
plot_image_matrix(mean_boot, title = "Mean Image: Ankle boot (9)", col_pal = gray_pal)
plot_image_matrix(diff_image, title = "Differenz: Boot - Sneaker", 
                  col_pal = div_pal, zlim = c(-max_abs_diff, max_abs_diff))

# ------------------------------------------------------------------------------
# Plot 2: Pixel-Verteilung, Zero-Inflation & fehlende statistische Zentrierung
# ------------------------------------------------------------------------------
set.seed(42)
sample_pixels <- sample(c(X_raw), size = 50000)
pct_zeros     <- round(mean(sample_pixels == 0) * 100, 1)

par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))

# A) Histogramm aller Intensitäten
hist(sample_pixels, breaks = 40, freq = FALSE, col = "grey85", border = "grey40",
     main = "Pixel-Intensitätsverteilung", xlab = "Graustufe [0, 255]", ylab = "Dichte")
abline(v = mean(sample_pixels), col = "firebrick", lwd = 2, lty = 2)
abline(v = 0, col = "black", lwd = 2)
legend("topright", 
       legend = c(paste0("Hintergrund (Wert 0): ", pct_zeros, "%"),
                  paste0("Mittelwert (", round(mean(sample_pixels), 1), ")")),
       col = c("black", "firebrick"), lty = c(1, 2), lwd = c(2, 2), bty = "n")

# B) Mittlere Helligkeit pro Bild
mean_img_brightness <- rowMeans(X_raw)
hist(mean_img_brightness, breaks = 30, freq = FALSE, col = "lightblue", border = "grey40",
     main = "Mittlere Bildhelligkeit je Objekt", xlab = "Mittlerer Graustufenwert", ylab = "Dichte")
lines(density(mean_img_brightness), col = "navy", lwd = 2)

# ------------------------------------------------------------------------------
# Plot 3: Pixel-Varianzkarte (Tote Randpixel vs. Informationszone)
# ------------------------------------------------------------------------------
pixel_vars <- apply(X_raw, 2, var)
var_pal    <- colorRampPalette(c("black", "firebrick", "orange", "yellow", "white"))(256)

par(mfrow = c(1, 1), mar = c(2, 2, 3, 2))
plot_image_matrix(pixel_vars, title = "Pixel-Varianzkarte (Var(Xj) über alle Bilder)", col_pal = var_pal)

# ------------------------------------------------------------------------------
# Plot 4: Räumliche Zentrierung (Center of Mass: cx vs. cy)
# ------------------------------------------------------------------------------
com_matrix <- t(apply(X_raw, 1, calc_center_of_mass))
com_df <- data.frame(Class = labels, cx = com_matrix[, 1], cy = com_matrix[, 2])

par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))

# A) Bivariater Scatterplot der Schwerpunkte
plot(com_df$cx, com_df$cy,
     col = ifelse(com_df$Class == "Sneaker", rgb(0.2, 0.4, 0.8, 0.3), rgb(0.8, 0.2, 0.2, 0.3)),
     pch = 16, cex = 0.8, xlim = c(11, 18), ylim = c(11, 18),
     xlab = "Horizontaler Schwerpunkt (cx)", ylab = "Vertikaler Schwerpunkt (cy)",
     main = "Räumliche Schwerpunktverteilung")
abline(v = 14.5, h = 14.5, lty = 2, col = "black", lwd = 1.5)
points(14.5, 14.5, pch = 3, cex = 1.8, lwd = 2.5, col = "black")
legend("topleft", legend = c("Sneaker (7)", "Ankle boot (9)", "Rastermitte (14.5)"),
       col = c("blue", "red", "black"), pch = c(16, 16, 3), bty = "n")

# B) Boxplot des vertikalen Schwerpunkts
boxplot(cy ~ Class, data = com_df, col = c("lightblue", "lightpink"),
        ylab = "Vertikaler Schwerpunkt (cy)", main = "Vertikale Schaftverschiebung")
abline(h = 14.5, lty = 2, col = "grey40")


# ==============================================================================
# FOLIE 4: METHODEN (PCA VIA SVD, MULTIVARIATE & UNCERTAINTY VISUALISIERUNG)
# ==============================================================================

# PCA via SVD berechnen (Standardisierung: Spalten zentrieren!)
pca_res <- prcomp(X_raw, center = TRUE, scale. = FALSE)
pca_scores <- pca_res$x
var_expl   <- pca_res$sdev^2
cum_var    <- cumsum(var_expl) / sum(var_expl)
k_80       <- which(cum_var >= 0.80)[1]

# ------------------------------------------------------------------------------
# Plot 5: Multivariate Visualisierung (PC1 vs PC2 mit 95% Konfidenzellipsen)
# (Pflichtanforderung nach Rubrik 3)
# ------------------------------------------------------------------------------
par(mfrow = c(1, 1), mar = c(4.5, 4.5, 3, 1))

idx_sneaker <- which(labels == "Sneaker")
idx_boot    <- which(labels == "Ankle boot")

plot(pca_scores[, 1], pca_scores[, 2],
     col = ifelse(labels == "Sneaker", rgb(0.2, 0.4, 0.8, 0.2), rgb(0.8, 0.2, 0.2, 0.2)),
     pch = 16, cex = 0.7,
     xlab = paste0("PC 1 (", round(var_expl[1] / sum(var_expl) * 100, 1), "% Varianz)"),
     ylab = paste0("PC 2 (", round(var_expl[2] / sum(var_expl) * 100, 1), "% Varianz)"),
     main = "Multivariate Visualisierung: PC-Scores mit 95%-Konfidenzellipsen")

# 95% Ellipsen über robuste/klassische Kovarianz
ell_snk <- calc_ellipse(colMeans(pca_scores[idx_sneaker, 1:2]), cov(pca_scores[idx_sneaker, 1:2]))
ell_bot <- calc_ellipse(colMeans(pca_scores[idx_boot, 1:2]), cov(pca_scores[idx_boot, 1:2]))

lines(ell_snk, col = "blue", lwd = 2.5)
lines(ell_bot, col = "firebrick", lwd = 2.5)
legend("topright", 
       legend = c("Sneaker (7)", "Sneaker 95% Ellipse", "Ankle boot (9)", "Ankle boot 95% Ellipse"),
       col = c("blue", "blue", "firebrick", "firebrick"),
       pch = c(16, NA, 16, NA), lty = c(NA, 1, NA, 1), lwd = c(NA, 2.5, NA, 2.5), bty = "n")

# ------------------------------------------------------------------------------
# Plot 6: Uncertainty Visualisierung (Scree Plot mit K-Schwelle & Toleranzband)
# (Pflichtanforderung nach Rubrik 3)
# ------------------------------------------------------------------------------
par(mfrow = c(1, 1), mar = c(4.5, 4.5, 3, 1))
k_max <- 40

plot(1:k_max, cum_var[1:k_max] * 100, type = "b", pch = 19, cex = 0.8, col = "navy",
     ylim = c(20, 100), xlab = "Anzahl Hauptkomponenten (K)",
     ylab = "Kumulativ erklärte Varianz (%)",
     main = "Uncertainty Scree Plot: Wahl des Kompressionsgrades K")

# Toleranzband [75%, 85%] visualisiert die Modellunsicherheit bzgl. K
rect(xleft = 0, ybottom = 75, xright = k_max, ytop = 85,
     col = rgb(0.8, 0.9, 0.8, 0.4), border = NA)
abline(h = 80, col = "forestgreen", lwd = 2, lty = 2)
abline(v = k_80, col = "firebrick", lwd
       
       