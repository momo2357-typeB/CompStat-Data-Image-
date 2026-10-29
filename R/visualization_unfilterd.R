# ------------------------------------------------------------------------------
# 1. Daten einlesen & Hilfsfunktionen definieren
# ------------------------------------------------------------------------------
# Relativer Pfad zur Einhaltung der Reproduzierbarkeitskriterien
train_path <- "data/fashion-mnist_train1.csv"

# Falls die CSV lokal noch nicht im Ordner liegt, simulieren wir hier einen Dummy,
# damit der Code direkt nachvollziehbar bleibt. Ersetze dies durch read.csv():
if (file.exists(train_path)) {
  fashion_train <- read.csv(train_path)
} else {
  message("Lese simulierten Beispieldatensatz (bitte Original-CSV einbinden)...")
  set.seed(42)
  fashion_train <- data.frame(
    label = sample(0:9, 1000, replace = TRUE),
    matrix(rbeta(1000 * 784, 0.5, 5) * 255, nrow = 1000, ncol = 784)
  )
  colnames(fashion_train)[-1] <- paste0("pixel", 1:784)
}

# Zuordnung der 10 Klassenbezeichnungen laut Datensatzdokumentation
class_names <- c(
  "0" = "T-shirt/top", "1" = "Trouser",    "2" = "Pullover",
  "3" = "Dress",       "4" = "Coat",       "5" = "Sandal",
  "6" = "Shirt",       "7" = "Sneaker",    "8" = "Bag",
  "9" = "Ankle boot"
)

# Modulare Funktion zum Rendern eines einzelnen 28x28 Bildes
plot_fashion_image <- function(pixel_vector, title = "") {
  # Vektor in 28x28 Matrix überführen
  img_mat <- matrix(as.numeric(pixel_vector), nrow = 28, byrow = TRUE)
  
  # Orientierungskorrektur für base::image
  img_oriented <- t(img_mat)[, 28:1]
  
  # Graustufenpalette definieren (0 = Schwarz/Hintergrund, 255 = Weiß/Vordergrund)
  image(
    1:28, 1:28, img_oriented,
    col = gray(0:255 / 255),
    axes = FALSE, xlab = "", ylab = "",
    main = title
  )
  box()
}

# ------------------------------------------------------------------------------
# 2. Aus jeder der 10 Klassen ein zufälliges Bild auswählen und anzeigen
# ------------------------------------------------------------------------------
set.seed(107258) # Für reproduzierbare Zufallsauswahl

# Grafik-Layout auf 2 Zeilen und 5 Spalten einstellen
old_par <- par(mfrow = c(2, 5), mar = c(1, 1, 2.5, 1))

for (lbl in 0:9) {
  # Indizes aller Bilder dieser Kategorie filtern
  idx_class <- which(fashion_train$label == lbl)
  random_idx <- sample(idx_class, 1)
  
  # Pixeldaten extrahieren (Spalte 1 ist das Label)
  pixels <- fashion_train[random_idx, -1]
  
  # Plotten mit Titel
  plot_fashion_image(pixels, title = paste0(lbl, ": ", class_names[as.character(lbl)]))
}

par(old_par) # Grafikeinstellungen zurücksetzen

# Pixelmatrix extrahieren (Spalten 2 bis 785)
pixel_mat <- as.matrix(fashion_train[, -1])

# A) Stichprobe von 50.000 einzelnen Pixeln für das Gesamthistogramm
pixel_sample <- sample(c(pixel_mat), size = 50000)

# B) Mittlere Helligkeit pro Bild berechnen (Vektorisierte Zeilenmittelwerte)
mean_brightness_per_image <- rowMeans(pixel_mat)

# Nebeneinander plotten
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))

# 1. Histogramm: Pixel-Intensitäten
hist(
  pixel_sample,
  breaks = 30, freq = FALSE,
  col = "grey75", border = "grey40",
  main = "Verteilung der Pixel-Intensitäten",
  xlab = "Pixelwert [0, 255]", ylab = "Dichte"
)
abline(v = median(pixel_sample), col = "firebrick", lwd = 2, lty = 2)
legend("topright", legend = c("Median"), col = "firebrick", lty = 2, lwd = 2, bty = "n")

# 2. Histogramm: Mittlere Bildhelligkeit
hist(
  mean_brightness_per_image,
  breaks = 30, freq = FALSE,
  col = "lightblue", border = "grey40",
  main = "Mittlere Bildhelligkeit je Bild",
  xlab = "Durchschnittlicher Pixelwert", ylab = "Dichte"
)
lines(density(mean_brightness_per_image), col = "navy", lwd = 2)

par(mfrow = c(1, 1))

# Funktion zur Berechnung des Schwerpunkts (Center of Mass)
calc_center_of_mass <- function(img_vector) {
  mat <- matrix(as.numeric(img_vector), nrow = 28, byrow = TRUE)
  total_mass <- sum(mat)
  
  # Absicherung gegen leere Bilder
  if (total_mass == 0) return(c(cx = 14.5, cy = 14.5))
  
  # x entspricht den Spalten (1:28, links nach rechts)
  # y entspricht den Zeilen invertiert (1:28, unten nach oben)
  grid_x <- col(mat)
  grid_y <- 28 - row(mat) + 1
  
  cx <- sum(grid_x * mat) / total_mass
  cy <- sum(grid_y * mat) / total_mass
  return(c(cx = cx, cy = cy))
}

# Teildatensatz für Sneaker (7) und Ankle Boots (9) filtern
shoes_data <- subset(fashion_train, label %in% c(7, 9))
shoes_labels <- factor(shoes_data$label, levels = c(7, 9), labels = c("Sneaker", "Ankle Boot"))

# Schwerpunkte für alle Beobachtungen berechnen
com_list <- apply(shoes_data[, -1], 1, calc_center_of_mass)
com_df <- data.frame(
  Class = shoes_labels,
  cx = com_list[1, ],
  cy = com_list[2, ]
)

# Ergebnisse statistisch zusammenfassen
com_summary <- aggregate(cbind(cx, cy) ~ Class, data = com_df, function(x) c(Mean = mean(x), SD = sd(x)))
print(com_summary)

# Visualisierung der räumlichen Zentrierung
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))

# Scatterplot der Schwerpunkte
plot(
  com_df$cx, com_df$cy,
  col = ifelse(com_df$Class == "Sneaker", rgb(0.2, 0.4, 0.8, 0.3), rgb(0.8, 0.2, 0.2, 0.3)),
  pch = 16, cex = 0.8,
  xlim = c(10, 18), ylim = c(10, 18),
  xlab = "Horizontaler Schwerpunkt (cx)",
  ylab = "Vertikaler Schwerpunkt (cy)",
  main = "Schwerpunkt-Verteilung (Center of Mass)"
)
# Geometrischer Mittelpunkt des Rasters (14.5, 14.5)
abline(v = 14.5, h = 14.5, lty = 2, col = "black", lwd = 2)
points(14.5, 14.5, pch = 3, cex = 2, lwd = 3, col = "black")
legend("topleft", legend = c("Sneaker (7)", "Ankle boot (9)", "Mitte (14.5)"),
       col = c("blue", "red", "black"), pch = c(16, 16, 3), bty = "n")

# Boxplot zum Vergleich des vertikalen Schwerpunkts
boxplot(
  cy ~ Class, data = com_df,
  col = c("lightblue", "lightpink"),
  ylab = "Vertikaler Schwerpunkt (cy)",
  main = "Vertikale Positionierung"
)
abline(h = 14.5, lty = 2, col = "grey30")

par(mfrow = c(1, 1))

