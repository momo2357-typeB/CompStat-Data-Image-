# Real-World Computational Statistics in R: Fashion-MNIST Image Classification

**Course:** 107.258 Computational Statistics (WS 2026/27, TU Wien)  
**Group:** Group 12 — Topic 12: Image Data (Zalando Fashion-MNIST)  
**Instructors:** Prof. Klaus Nordhausen, Ass.-Prof. Laura Vana Gür  

## Team Members, Responsibilities & Deadlines

| Member | Focus Area | Primary Deliverables | Primary Branch | Target Deadline |
| :--- | :--- | :--- | :--- | :--- |
| **Samuel Oberascher** | Data Wrangling, EDA & Reproducibility | `R/01_data_prep.R` (`train_prepared.rds` incl. `[0, 1]` scaling & 5-fold CV split), `R/02_eda_plots.R` (mean/variance maps, histograms/boxplots, uncertainty visualization), Data Dictionary & license in `README.md`, report text (*Data & EDA*) | `feature/data-wrangling`, `feature/eda-visuals` | Data RDS: **25.11.2026** / EDA: **29.11.2026** |
| **Justus Becker** | Project Lead, PCA/SVD & Numerical Linear Algebra | Git setup, `.Rproj`, `main.R`, `R/03_pca_svd.R` (Thin-SVD, scree plot, 4–6 eigenimages, `pca_svd.rds`), PR reviews, report text (*PCA & Numerical Linear Algebra*) | `feature/pca-svd` | **06.12.2026** |
| **Romeo Agha Mohammadi Sareshgi** | Benchmarking, Report Integration & Ethics | Part 1: `microbenchmark` (`svd()` vs. `eigen(crossprod())`) & *Ethics & Limitations* text. Part 2: Integrate all team texts/plots/tables into `report/final_report.Rmd` (8–12 pp., `kable`/`texreg`, `sessionInfo()`) | `feature/report-rmd` | Part 1: **06.12.2026** / Draft: **03.01.2027** |
| **Roman Zulehner** | Outlier Detection, ROBPCA & Sensitivity (`glmrob`) | `R/04_outliers_robust.R` (`CovMcd` in PC score space, `PcaHubert` + flagged shoe plots, deviance residuals, Cook's distance, full `glmrob` & sensitivity comparison table), report text (*Outliers & Sensitivity*) | `feature/outliers-robust` | **20.12.2026** |
| **Lisa Stadler** | Downstream Modeling, 5-Fold CV & Optimization | `R/05_models_cv.R` (5-fold CV for `glm`, `glmnet`, raw-pixel baseline; ROC-AUC, PR-AUC, calibration plots, confusion matrix), report text (*Modeling & Optimization/Convergence*), final locked test-set evaluation (Sprint 5) | `feature/models-cv` | **20.12.2026** |

> **Team Rule:** Every member drafts the report text and interpretation for their own section so `final_report.Rmd` integration stays balanced. All work is merged into `main` via Pull Requests only.

---

## 1. Project Overview

This project conducts a reproducible computational statistics analysis of high-dimensional grayscale image data ($p = 784$ pixel features per $28 \times 28$ image) from Zalando's **Fashion-MNIST** dataset.

### Core Objectives
1. **Exploratory Data Analysis (EDA):** Upright $28 \times 28$ grayscale rendering, class-wise mean/variance maps, multivariate score visualizations, and variance uncertainty curves.
2. **Dimensionality Reduction (PCA via Thin-SVD):** Center the training pixel matrix and decompose $X = U \Sigma V^\top$ without forming $X^\top X$; select $K$ components ($\ge 80\%$ variance + CV) and interpret leading columns of $V$ as $28 \times 28$ *eigenimages*.
3. **Outlier Detection & Sensitivity Analysis:** Screen multivariate score outliers via `CovMcd` and `PcaHubert`, evaluate GLM influence diagnostics (deviance residuals, hat values, Cook's distance), and compare full, filtered, and robust (`glmrob`) fits.
4. **Downstream Binary Classification:** Classify **Sneaker (Class 7)** vs. **Ankle boot (Class 9)** using `glm`, `glmnet`, `glmrob`, and a raw-pixel baseline evaluated via 5-fold cross-validation.
5. **Numerical Linear Algebra & Optimization:** Analyze conditioning ($\kappa(X^\top X) = \kappa(X)^2$), catastrophic cancellation, IRLS / Newton-Raphson convergence, and the Hauck-Donner effect.

> **Strict Test-Set Lock:** `data/fashion-mnist_test.csv` remains strictly locked until after the Project Clinic (07.01.2027). All centering, SVD/PCA projections, outlier thresholds, and 5-fold CV are computed exclusively within the training partition.

---

## 2. Repository Structure

```text
├── .gitignore
├── CompStat-Data-Image.Rproj          # RStudio project file (anchors relative paths)
├── README.md
├── main.R                             # Master pipeline script (no setwd())
├── data/
│   ├── fashion-mnist_train1.csv       # Training split part 1
│   ├── fashion-mnist_train2.csv       # Training split part 2
│   ├── fashion-mnist_shoes_train.csv  # Pre-filtered binary subset (classes 7 & 9)
│   └── fashion-mnist_test.csv         # Locked hold-out test set (do not inspect pre-clinic)
├── R/
│   ├── 01_data_prep.R                 # Data import, cleaning, normalization, CV split
│   ├── 02_eda_plots.R                 # Upright 28x28 image() plotter & EDA visualizations
│   ├── 03_pca_svd.R                   # Centered Thin-SVD, scree plot, eigenimages, benchmarks
│   ├── 04_outliers_robust.R           # CovMcd, PcaHubert, Cook's distance, sensitivity table
│   └── 05_models_cv.R                 # glm, glmnet, glmrob, baseline & 5-fold CV metrics
├── output/
│   ├── figures/                       # Exported report figures (PNG/PDF)
│   └── models/                        # Saved PCA & model objects (.rds)
├── report/
│   └── final_report.Rmd               # Reproducible 8-12 page RMarkdown report
├── slides/                            # Presentation slides
└── archive/                           # Archived exploratory pitch scripts & plots
```

---

## 3. Data Source, License & Dictionary

- **Source:** Zalando Research Fashion-MNIST (`https://github.com/zalandoresearch/fashion-mnist`, CSV distribution via Kaggle).
- **License:** MIT License (Zalando SE).
- **Observations:** 60,000 training images (split across `fashion-mnist_train1.csv` and `fashion-mnist_train2.csv`; 12,000 footwear images in classes 7 & 9) and 10,000 locked test images (`fashion-mnist_test.csv`).

| Variable | Type | Range / Values | Description |
| :--- | :--- | :--- | :--- |
| `label` | Integer / Factor | `0`–`9` (Task: `7` Sneaker, `9` Ankle boot) | Ground-truth garment class |
| `pixel1` ... `pixel784` | Numeric | `0`–`255` (normalized to `[0, 1]`) | Row-major $28 \times 28$ grayscale intensity (`0` = background, `255` = foreground) |

---

## 4. Git & Branching Workflow

`main` is protected — **never push directly to `main`**. All work happens on the dedicated feature branch and is merged into `main` via Pull Request after review:

- `feature/data-wrangling`
- `feature/eda-visuals`
- `feature/pca-svd`
- `feature/outliers-robust`
- `feature/models-cv`
- `feature/report-rmd`

```bash
git fetch --all
git checkout feature/<your-branch>
git pull origin feature/<your-branch>
```

---

## 5. How to Reproduce

1. Open `CompStat-Data-Image.Rproj` in RStudio (or `cd` into the repository root).
2. Run the full pipeline using relative paths only:
   ```bash
   Rscript main.R
   ```
3. Knit the final report once all modules and outputs are generated:
   ```r
   rmarkdown::render("report/final_report.Rmd")
   ```
