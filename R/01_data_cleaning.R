# ==============================================================================
# Computational Statistics (TU Wien, WS 2026/27)
# Topic 12: Image Data (Fashion-MNIST)
# Script: R/01_data_cleaning.R
# Purpose: Merge train splits, subset footwear, generate mean & variance images
# ==============================================================================

# 1. Modulare Visualisierungsfunktion (Base R image)
plot_fashion_image <- function(pixel_vector, title = "") {
  # Defensives Programmieren / Antibugging (Slides Further R topics, Folie 25)
  stopifnot(is.numeric(pixel_vector), length(pixel_vector) == 784)
  
  # 784-Vektor zeilenweise in 28x28 Matrix umformen
  img_matrix <- matrix(pixel_vector, nrow = 28, byrow = TRUE)
  
  # Orientierungskorrektur fuer das kartesische Koordinatensystem von image():
  # Zeilen von 28 auf 1 spiegeln (Top-to-Bottom) und transponieren
  img_corrected <- t(img_matrix[28:1, ])
  
  # High-Level Plotting mit Graustufen (0 = Weiss/Hintergrund, 1 = Schwarz/Vordergrund)
  image(
    x = 1:28,
    y = 1:28,
    z = img_corrected,
    col = gray(seq(1, 0, length.out = 256)),
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = title
  )
  box() # Rahmen zeichnen
}

# 2. Relative Pfade vom Projekt-Root definieren
path1 <- file.path("data", "fashion-mnist_train1.csv")
path2 <- file.path("data", "fashion-mnist_train2.csv")

# Antibugging: Existenz pruefen
stopifnot(file.exists(path1), file.exists(path2))

# 3. Beide CSV-Splits einlesen
train_raw1 <- read.csv(path1, header = TRUE)
train_raw2 <- read.csv(path2, header = TRUE)

# Spaltenstruktur vor dem Kombinieren abgleichen
stopifnot(identical(colnames(train_raw1), colnames(train_raw2)))

# Zeilenweises Zusammenfuehren via rbind() (Slides Basic data handling, Folie 17)
train_full <- rbind(train_raw1, train_raw2)
rm(train_raw1, train_raw2) # RAM sofort bereinigen
gc()

# 4. Pruefen auf fehlende Werte (Slides Basic data handling, Folie 36)
missing_count <- sum(!complete.cases(train_full))
stopifnot(missing_count == 0)

# 5. Binäre Task-Filterung: Klasse 7 (Sneaker) vs. Klasse 9 (Ankle Boot)
target_classes <- c(7, 9)
shoes_idx      <- train_full$label %in% target_classes
shoes_df       <- train_full[shoes_idx, ]
rm(train_full)
gc()

# 6. Normalisierung & Faktorkodierung
pixel_cols <- grep("^pixel", colnames(shoes_df))
X_train    <- as.matrix(shoes_df[, pixel_cols]) / 255.0
rownames(X_train) <- NULL

# Zielvariable als Faktor; Sneaker als Referenzbasis (0)
y_train <- factor(shoes_df$label, levels = c(7, 9), labels = c("Sneaker", "AnkleBoot"))
y_train <- relevel(y_train, ref = "Sneaker")

cat(sprintf("Trainingsmenge erfolgreich vereint: N = %d Schuhe, p = %d Pixel.\n", 
            nrow(X_train), ncol(X_train)))
print(table(y_train))

# ------------------------------------------------------------------------------
# 7. Visualisierung 1: Zufallsstichprobe von Einzelbildern
# ------------------------------------------------------------------------------
set.seed(42) # Reproduzierbare Stichprobe
idx_sneakers <- sample(which(y_train == "Sneaker"), 4)
idx_boots    <- sample(which(y_train == "AnkleBoot"), 4)

# 2 Zeilen, 4 Spalten Layout (Slides Basic descriptives, Folie 95)
par(mfrow = c(2, 4), mar = c(1, 1, 2.5, 1))

for (i in seq_along(idx_sneakers)) {
  plot_fashion_image(X_train[idx_sneakers[i], ], 
                     title = paste("Sneaker", i))
}

for (i in seq_along(idx_boots)) {
  plot_fashion_image(X_train[idx_boots[i], ], 
                     title = paste("Ankle Boot", i))
}

par(mfrow = c(1, 1)) # Layout zuruecksetzen

# ------------------------------------------------------------------------------
# 8. Visualisierung 2: Mean Images & Pixel-Varianzkarte (Fuer Pitch-Folie 3)
# ------------------------------------------------------------------------------
# Klassen-Durchschnitte via vektorisiertem colMeans()
mean_sneaker <- colMeans(X_train[y_train == "Sneaker", ])
mean_boot    <- colMeans(X_train[y_train == "AnkleBoot", ])

# Pixelweise empirische Varianz ueber alle 12.000 Schuhe (MARGIN = 2)
pixel_variance <- apply(X_train, 2, var)

# 1 Zeile, 3 Spalten nebeneinander ausgeben
par(mfrow = c(1, 3), mar = c(1, 1, 3, 1))

plot_fashion_image(mean_sneaker, title = "Mean Sneaker (Klasse 7)")
plot_fashion_image(mean_boot, title = "Mean Ankle Boot (Klasse 9)")
plot_fashion_image(pixel_variance, title = "Pixel-Varianzkarte")

par(mfrow = c(1, 1))

