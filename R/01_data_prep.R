# ==============================================================================
# 107.258 Computational Statistics (WS 2026/27, TU Wien)
# Module: R/01_data_prep.R
# Lead:   Samuel Oberascher (Data Wrangling, EDA & Reproducibility)
# ==============================================================================
# Purpose:
#   - Import training splits (data/fashion-mnist_train1.csv & train2.csv)
#   - Validate schema & missingness (complete.cases)
#   - Filter binary task: Class 7 (Sneaker) vs. Class 9 (Ankle boot)
#   - Normalize pixel intensities from [0, 255] to [0, 1]
#   - Create reproducible 5-fold CV indices strictly within training data
#
# CRITICAL RULE:
#   Do NOT read or inspect data/fashion-mnist_test.csv before Project Clinic
#   approval (07.01.2027). No setwd(); use relative paths only.
# ==============================================================================

message("[01_data_prep] Module placeholder ready for implementation on feature/data-wrangling.")

set.seed(123)

# Trainingsdaten laden (Testset bleibt unberührt)
train1 <- read.csv("data/fashion-mnist_train1.csv")
train2 <- read.csv("data/fashion-mnist_train2.csv")
train_raw <- rbind(train1, train2)
rm(train1, train2)

# Nur Sneaker (7) und Ankle boot (9) herausfiltern
shoes <- subset(train_raw, label %in% c(7, 9))
rownames(shoes) <- NULL
rm(train_raw)

# Pixelintensitäten auf [0, 1] skalieren
pixel_cols <- paste0("pixel", 1:784)
X <- as.matrix(shoes[, pixel_cols]) / 255

# Zielvektor (0 = Sneaker, 1 = Ankle boot) und Faktor-Labels erstellen
y <- ifelse(shoes$label == 9, 1, 0)
label <- factor(shoes$label, levels = c(7, 9), labels = c("Sneaker", "Ankle boot"))

# Stratifizierter 5-Fold-Cross-Validation-Split
n_folds <- 5
fold <- integer(nrow(X))

for (k in c(0, 1)) {
  idx <- which(y == k)
  fold[idx] <- sample(rep(1:n_folds, length.out = length(idx)))
}

# Plausibilitätsprüfung der Daten und Fold-Verteilung
stopifnot(
  !anyNA(X),
  nrow(X) == 12000,
  ncol(X) == 784,
  min(X) >= 0,
  max(X) <= 1,
  all(table(y, fold) == 1200)
)

# Aufbereitete Daten speichern
dir.create("output/models", recursive = TRUE, showWarnings = FALSE)

train_prepared <- list(
  X = X,
  y = y,
  label = label,
  fold = fold
)

saveRDS(train_prepared, "output/models/train_prepared.rds")