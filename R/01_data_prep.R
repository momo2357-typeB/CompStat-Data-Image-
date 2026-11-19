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
