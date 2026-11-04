# ==============================================================================
# 107.258 Computational Statistics - WS 2026/27
# Group 12: Image Data (Fashion-MNIST)
# Generierung der überarbeiteten Pitch-Plots basierend auf den Projektdateien
# ==============================================================================

# 1. Datenimport mit relativen Pfaden
# ------------------------------------------------------------------------------
# Pfade relativ zum Arbeitsverzeichnis (oder Unterordner "data/") definieren
path_shoes  <- if (file.exists("fashion-mnist_shoes_train.csv")) {
  "fashion-mnist_shoes_train.csv"
} else if (file.exists("data/fashion-mnist_shoes_train.csv")) {
  "data/fashion-mnist_shoes_train.csv"
} else {
  NULL
}

if (!is.null(path_shoes)) {
  # Variante A: Vorgefilterter / kompakter Trainingsdatensatz (24 MB)
  message("Lade Trainingsdaten aus: ", path_shoes)
  raw_train <- read.csv(path_shoes)
} else {
  # Variante B: Zusammensetzen der beiden Trainingshälften via rbind()
  path_t1 <- if (file.exists("fashion-mnist_train1.csv")) "fashion-mnist_train1.csv" else "data/fashion-mnist_train1.csv"
  path_t2 <- if (file.exists("fashion-mnist_train2.csv")) "fashion-mnist_train2.csv" else "data/fashion-mnist_train2.csv"
  
  if (!file.exists(path_t1) || !file.exists(path_t2)) {
    stop("Trainingsdateien nicht gefunden! Bitte Arbeitsverzeichnis prüfen.")
  }
  
  message("Lade und verbinde train1 und train2 via rbind()...")
  t1 <- read.csv(path_t1)
  t2 <- read.csv(path_t2)
  raw_train <- rbind(t1, t2)
  rm(t1, t2); gc()
}

# 2. Subsetting & Skalierung (Sneaker = 7, Ankle Boot = 9)
# ------------------------------------------------------------------------------
subset_idx <- raw_train$label %in% c(7, 9)
train_sub  <- raw_train[subset_idx, ]

y <- train_sub$label
# Extrahieren der Pixelspalten (unabhängig davon, ob label an 1. Stelle steht)
pixel_cols <- grep("pixel", colnames(train_sub), value = TRUE)
if (length(pixel_cols) == 0) {
  X_raw <- as.matrix(train_sub[, colnames(train_sub) != "label"])
} else {
  X_raw <- as.matrix(train_sub[, pixel_cols])
}

# Normierung auf das Einheitsintervall [0, 1]
X <- X_raw / 255
n <- nrow(X)
p <- ncol(X)
img_dim <- 28

message(sprintf("Datensatz erfolgreich aufbereitet: n = %d Bilder, p = %d Pixel", n, p))

# Farbdefinitionen
col_palette_var <- hcl.colors(100, "YlOrRd", rev = TRUE)
col_sneaker     <- rgb(30, 100, 220, alpha = 110, maxColorValue = 255)
col_boot        <- rgb(200, 40, 40, alpha = 110, maxColorValue = 255)


# ==============================================================================
# PLOT 1 (Folie 4): Vollflächige Pixel-Varianzkarte (ohne Histogramm)
# ==============================================================================
png("Plot_Slide4_Pixel_Variance.png", width = 1600, height = 1200, res = 200)

par(mar = c(3, 3, 3, 5), bg = "white")

# Pixelweise empirische Varianz berechnen
pixel_var  <- apply(X, 2, var)
var_matrix <- matrix(pixel_var, nrow = img_dim, ncol = img_dim, byrow = TRUE)

# 90-Grad-Drehung für korrekte Ausrichtung in base R image()
var_matrix_rotated <- t(var_matrix[img_dim:1, ])

image(1:img_dim, 1:img_dim, var_matrix_rotated,
      col = col_palette_var,
      axes = FALSE, xlab = "", ylab = "",
      main = "Spatial Pixel Variance: Var(X_j) Across Footwear")

box(col = "gray40", lwd = 1.5)

# Farblegende auf der rechten Seite
par(xpd = TRUE)
fields_y <- seq(4, 25, length.out = 100)
for (i in 1:length(fields_y)) {
  rect(30, fields_y[i], 31.5, fields_y[i] + (fields_y[2] - fields_y[1]),
       col = col_palette_var[i], border = NA)
}
rect(30, 4, 31.5, 25 + (fields_y[2] - fields_y[1]), border = "gray40")
text(x = 32.2, y = c(4, 14.5, 25), 
     labels = c("0.0 (Constant)", "Medium", sprintf("%.2f (Max)", max(pixel_var))), 
     adj = 0, cex = 0.8)
text(x = 30.7, y = 26.8, labels = "Variance", cex = 0.85, font = 2)
par(xpd = FALSE)

dev.off()


# ==============================================================================
# PLOT 2 (Folie 5): Erweiterter Schwerpunkt-Scatterplot (ohne Boxplot)
# ==============================================================================
png("Plot_Slide5_Spatial_Centroids_Expanded.png", width = 1800, height = 1200, res = 200)

par(mar = c(4.5, 4.5, 3, 2), bg = "white")

# 28x28 Rasterkoordinaten
grid_x <- rep(1:img_dim, each = img_dim)
grid_y <- rep(1:img_dim, times = img_dim)

# Vektorisierte Berechnung des optischen Schwerpunkts je Bild
image_sums <- rowSums(X)
cx <- as.vector(X %*% grid_x) / image_sums
cy <- as.vector(X %*% grid_y) / image_sums

# Klassenmittelwerte (Zentroide)
mean_cx_7 <- mean(cx[y == 7]); mean_cy_7 <- mean(cy[y == 7])
mean_cx_9 <- mean(cx[y == 9]); mean_cy_9 <- mean(cy[y == 9])

plot(cx, cy, type = "n",
     xlim = c(12, 18), ylim = c(10, 18),
     xlab = "Horizontal Center of Mass (cx) [Pixels]",
     ylab = "Vertical Center of Mass (cy: 1 = Top, 28 = Bottom)",
     main = "Morphological Shift: Spatial Center of Mass Distribution",
     las = 1, cex.lab = 1.1)

grid(col = "gray85", lty = "dotted")

# Bildmittelpunkt (14.5, 14.5)
abline(v = 14.5, h = 14.5, lty = 2, col = "gray50", lwd = 1.2)

# Einzelbeobachtungen
points(cx[y == 7], cy[y == 7], pch = 16, col = col_sneaker, cex = 0.7)
points(cx[y == 9], cy[y == 9], pch = 16, col = col_boot, cex = 0.7)

# Klassenzentroide hervorheben
points(mean_cx_7, mean_cy_7, pch = 21, bg = "blue3", col = "white", cex = 2.4, lwd = 2)
points(mean_cx_9, mean_cy_9, pch = 21, bg = "red3", col = "white", cex = 2.4, lwd = 2)

# Morphologische Erläuterungspfeile passend zur Differenzkarte
arrows(13.2, 11.5, 13.2, 13.0, col = "red3", lwd = 2, length = 0.1)
text(13.3, 12.2, "Pull towards upper shaft\n(Ankle Boots: Class 9)", col = "red3", adj = 0, cex = 0.85, font = 2)

arrows(16.5, 16.5, 16.5, 14.8, col = "blue3", lwd = 2, length = 0.1)
text(16.4, 15.8, "Lower profile & sole mass\n(Sneakers: Class 7)", col = "blue3", adj = 1, cex = 0.85, font = 2)

legend("topleft",
       legend = c("Sneaker (Class 7)", "Ankle Boot (Class 9)", "Class Centroid (Mean)"),
       pch = c(16, 16, 21),
       col = c("blue3", "red3", "black"),
       pt.bg = c(NA, NA, "gray70"),
       pt.cex = c(1.1, 1.1, 1.4),
       bg = "white", box.col = "gray70", inset = 0.02)

dev.off()


# ==============================================================================
# PLOT 3 (Folie 6): PC-Scores mit 95%-Konfidenzellipsen (Rubrik-Pflicht)
# ==============================================================================
png("Plot_Slide6_PCA_Scores_Ellipses.png", width = 1800, height = 1200, res = 200)

par(mar = c(4.5, 4.5, 3, 2), bg = "white")

# Zentrierung der Matrix für die SVD (Rubrik-Standard)
X_centered <- scale(X, center = TRUE, scale = FALSE)

# Schnelle Thin-SVD via prcomp
pca_fit <- prcomp(X_centered, center = FALSE, scale. = FALSE, rank. = 2)
scores  <- pca_fit$x[, 1:2]
var_exp <- round(100 * (pca_fit$sdev[1:2]^2) / sum(apply(X_centered, 2, var)), 1)

# Bivariate 95%-Konfidenzellipsen (Mahalanobis-Niveaulinien)
calc_ellipse <- function(data_subset, level = 0.95, npoints = 100) {
  mu <- colMeans(data_subset)
  sigma <- cov(data_subset)
  radius <- sqrt(qchisq(level, df = 2))
  angles <- seq(0, 2 * pi, length.out = npoints)
  unit_circle <- cbind(cos(angles), sin(angles))
  chol_decomp <- chol(sigma)
  ellipse_pts <- t(mu + radius * t(unit_circle %*% chol_decomp))
  return(ellipse_pts)
}

ellipse_7 <- calc_ellipse(scores[y == 7, ])
ellipse_9 <- calc_ellipse(scores[y == 9, ])

plot(scores[, 1], scores[, 2], type = "n",
     xlab = paste0("PC 1 (", var_exp[1], "% Variance) - Overall Silhouette & Shaft Mass"),
     ylab = paste0("PC 2 (", var_exp[2], "% Variance) - Aspect Ratio & Heel Contour"),
     main = "Multivariate Structure: PC Scores with 95% Confidence Ellipses",
     las = 1, cex.lab = 1.1)

grid(col = "gray85", lty = "dotted")

points(scores[y == 7, 1], scores[y == 7, 2], pch = 16, col = col_sneaker, cex = 0.6)
points(scores[y == 9, 1], scores[y == 9, 2], pch = 16, col = col_boot, cex = 0.6)

lines(ellipse_7, col = "blue3", lwd = 2.5)
lines(ellipse_9, col = "red3", lwd = 2.5)

legend("topright",
       legend = c("Sneaker (Class 7)", "Sneaker 95% Ellipse", 
                  "Ankle Boot (Class 9)", "Ankle Boot 95% Ellipse"),
       pch = c(16, NA, 16, NA),
       lty = c(NA, 1, NA, 1),
       lwd = c(NA, 2.5, NA, 2.5),
       col = c("blue3", "blue3", "red3", "red3"),
       bg = "white", box.col = "gray70", inset = 0.02)

dev.off()

message("Alle 3 Abbildungen erfolgreich im Arbeitsverzeichnis erzeugt.")

