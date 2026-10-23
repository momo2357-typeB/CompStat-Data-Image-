# Real-World Computational Statistics in R: Fashion-MNIST Image Classification

**Course:** 107.258 Computational Statistics (WS 2026/27, TU Wien)  
**Topic:** Topic 12 – Image Data (PCA/SVD on Zalando's Fashion-MNIST)  
**Instructors:** Prof. Klaus Nordhausen, Ass.-Prof. Laura Vana Gür  
**Team Members & Roles:**
- Member 1 (Data Lead): Data sourcing, cleaning, train/test split, data dictionary
- Member 2 (Modeling Lead): SVD, PCA score computation, logistic regression models
- Member 3 (Simulation & Optimization Lead): Robust covariance (CovMcd, PcaHubert), convergence analysis
- Member 4 (Documentation & Testing Lead): R Markdown report, README, relative path validation

---

## 1. Project Overview

This project conducts an end-to-end computational statistics analysis of high-dimensional image data ($p = 784$ features per observation) sourced from Zalando's Fashion-MNIST dataset.

### Core Objectives:
1. **Dimensionality Reduction:** Utilize Singular Value Decomposition (SVD/PCA) on centered pixel data to reduce dimensionality from $784$ down to $K \ll 784$ orthogonal components while retaining $\ge 80\%$ of total variance.
2. **Interpretability:** Reconstruct and interpret the leading principal components visually as $28 \times 28$ grayscale *eigenimages*.
3. **Downstream Classification:** Train and evaluate binary logistic regression models (`glm`, `glmnet`, and robust `glmrob`) on the extracted PC scores to classify morphologically similar footwear: **Sneaker (Class 7)** vs. **Ankle Boot (Class 9)**.
4. **Outlier Diagnostics & Sensitivity:** Detect multivariate leverage observations in the PC score space via Minimum Covariance Determinant (`CovMcd`) and compare standard PCA against robust PCA (`PcaHubert`).
5. **Numerical Considerations:** Analyze algorithmic convergence (Newton-Raphson / IRLS) and demonstrate the numerical stability of direct SVD over the formation of $X^\top X$.

---

## 2. Repository Structure

The project strictly follows relative path conventions to guarantee end-to-end reproducibility:

```text
├── .gitignore
├── README.md                  # Project overview, reproduction steps, data license
├── Computerstatistik.Rproj    # RStudio project file (anchors working directory)
├── main.R                     # Single master script executing the full pipeline
├── renv.lock                  # Exact dependency lockfile for R package versions
├── data/
│   ├── raw/                   # Raw dataset (fashion-mnist_train.csv, fashion-mnist_test.csv)
│   └── processed/             # Cleaned and standardized binary subsets (Sneaker vs. Ankle boot)
├── R/
│   ├── 01_data_io.R           # Modular functions: data import, type checking, validation
│   ├── 02_eda_plots.R         # Modular functions: 28x28 reshape and image plotting helpers
│   ├── 03_pca_svd.R           # Modular functions: SVD computation, scree plot, eigenimages
│   ├── 04_outliers.R          # Modular functions: CovMcd, Mahalanobis distances, PcaHubert
│   └── 05_modeling.R          # Modular functions: cross-validated GLM, GLMNET, and GLMROB fits
├── output/
│   ├── figures/               # Exported publication-ready plots (PNG/PDF)
│   └── models/                # Saved model fits and PCA decomposition objects (.rds)
└── report/
    ├── report.Rmd             # Reproducible 8–12 page scientific report
    ├── report.pdf             # Rendered final PDF deliverable
    └── slides_pitch.pdf       # 5-slide presentation deck for the Data Pitch (05.11.2026)
