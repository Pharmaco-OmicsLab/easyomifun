# EasyOmiFun

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![R: 4.4.3](https://img.shields.io/badge/R-4.4.3-276DC3.svg)](https://www.r-project.org/)
[![Python: 3.11.15](https://img.shields.io/badge/Python-3.11.15-3776AB.svg)](https://www.python.org/)
[![Node: >= 18](https://img.shields.io/badge/Node-%3E%3D%2018-339933.svg)](https://nodejs.org/)
[![TypeScript](https://img.shields.io/badge/TypeScript-5.x-3178C6.svg)](https://www.typescriptlang.org/)

An interactive, modular analytical framework designed for multi-omics biomarker identification, complemented with analysis modules for biological interpretation.


> ## 🚀 [Click Here to Launch the Live Web App (easyverse.app/easyomifun)](https://easyverse.app/easyomifun/)
> **No installation or setup required!** 
>
> End-to-end interactive platform for multi-omics biomarker discovery with machine learning-based feature selection methods, differential expression, and functional pathway enrichment interactively in your browser.

---

## Table of Contents

- [Overview](#overview)
- [Architecture](#architecture)
- [Key Modules](#key-modules)
- [Repository Structure](#repository-structure)
- [Sample Data](#sample-data)
- [Local Development & Code Inspection](#local-development--code-inspection)
  - [Prerequisites](#prerequisites)
  - [Step-by-Step Installation](#step-by-step-installation)
  - [Running the Application](#running-the-application)
- [Citation & Contact](#citation--contact)

---

## Overview

**EasyOmiFun** - An Open-source Analytical Platform for Rapid Omics Data Analysis and Clinically Actionable Biomarker Discovery

Lowering the technical barrier for scientists with user-friendly interface. Utilizing state-of-the-art machine learning algorithms to transform raw omics data into actionable insights.

---

## Architecture

The platform uses a decoupled client-server architecture:

```
┌─────────────────────────────────────────────────────────────┐
│                      EasyOmiFun Client                      │
│        (React 19 / TypeScript / Vite / Tailwind CSS)        │
└──────────────────────────────┬──────────────────────────────┘
                               │
                      HTTP REST / JSON
                               │
┌──────────────────────────────▼──────────────────────────────┐
│                    Analytical Backend                       │
│           (R Shiny REST API / Bioconductor Suite)           │
│  - DESeq2 / edgeR / limma       - glmnet / Boruta / ranger  │
│  - clusterProfiler / Reactome   - sva / ComBat batch corr.  │
│  - Python STABL integration     - rmarkdown / pandoc reports│
└─────────────────────────────────────────────────────────────┘
```

- **Frontend**: Single-page application (SPA) implemented in TypeScript and React, featuring responsive parameter controls, interactive charts (Recharts), and state management for multi-dataset tracking.
- **Backend**: High-performance R computational engine exposing dedicated REST endpoints for asynchronous processing, differential testing, meta-analyses, and machine learning models.

---

## Key Modules

### 1. Machine Learning Feature Selection
- **Supervised Biomarker Discovery**: `Boruta` all-relevant feature selection, and `STABL` stability selection for clinically relevant biomarkers discovery. Complemented with Lasso and ElasticNet (`glmnet`), Random Forest (`randomForest` / `ranger`), Gradient Boosting Machines (`gbm`), Support Vector Machines (`e1071`), .
- **Validation**: Cross-validation (k-fold, repeated k-fold) and external cohort testing with automated ROC-AUC evaluation and confusion matrix diagnostics.

### 2. Differential Expression Analysis (DEA)
- **Single-Cohort Analysis**: Standardized differential testing powered by `DESeq2`, `edgeR`, and `limma-voom`.
- **Multi-Dataset Meta-Analysis**: Combine effect sizes or p-values across distinct cohorts using Fixed Effect Models (FEM), Random Effect Models (REM), Fisher's combined probability, or Stouffer's method (`metapro` & `metafor`).
- **Interactive Visualizations**: High-resolution Volcano plots, MA plots, and ranked expression heatmaps.

### 3. Functional Pathway Enrichment
- **Over-Representation Analysis (ORA)** & **Gene Set Enrichment Analysis (GSEA)** via `clusterProfiler`.
- Supported ontology and pathway databases:
  - Gene Ontology (Biological Process, Molecular Function, Cellular Component)
  - KEGG Pathways
  - Reactome Pathways
  - MSigDB Hallmark Collections
- Visualizations: Dot plots, pathway bar charts, and gene-concept networks.

### 4. Data Processing & Quality Control
- **Probe/Feature Annotation**: Maps platform-specific probe IDs and Ensembl IDs to standard Gene Symbols.
- **Filtering & Imputation**: Custom zero/missing value thresholds, low-expression filtering, and missing-value imputation.
- **Normalization**: Supports platform-appropriate normalization strategies:
  - RNA-Seq: Regularized log transformation, edgeR TMM, CPM, VST.
  - Microarray: Quantile normalization, Variance stabilizing normalization.
- **Batch Effect Removal**: ComBat-based empirical Bayes batch correction (`sva`) with pre- and post-correction PCA visualizations.

### 5. Reproducibility & Export
- Formatted tabular data export (CSV, Excel, TSV) at all intermediate checkpoints.
- Automated summary reporting compiled into publication-ready Markdown and PDF formats.

---

## Repository Structure

```text
├── backend/
│   ├── app.R                    # Core R Shiny REST API server and routing
│   ├── processing.R             # Data filtering, normalization & batch correction
│   ├── de_analysis.R            # Differential expression and meta-analysis logic
│   ├── feature_selection.R      # Supervised ML algorithms and cross-validation
│   ├── enrichment.R             # ORA and GSEA pathway enrichment functions
│   ├── report_finalize.R        # Automated report rendering
│   ├── shared_utils.R           # Shared matrix operations and helper utilities
│   ├── install_packages.R       # Dependency installation script (CRAN & Bioconductor)
│   ├── md_to_pdf.py             # Script for Markdown to PDF conversion
│   └── sample_data/             # Test datasets for demonstration
│       ├── expression.csv       # Example gene expression matrix
│       ├── clinical.csv         # Sample clinical metadata
│       ├── gene_list.csv        # Candidate gene identifiers
│       └── ...
├── frontend/
│   ├── src/
│   │   ├── components/          # Modular UI components per analysis step
│   │   ├── pages/               # Primary module views (DE, Processing, FS, Enrichment)
│   │   ├── store/               # Application state store
│   │   ├── lib/                 # API interaction and parsing utilities
│   │   └── dataObject/          # Type definitions and data schemas
│   ├── package.json             # Frontend package declarations
│   ├── vite.config.ts           # Vite build and development configuration
│   └── tsconfig.json            # TypeScript configuration
└── .gitignore                   # Standard repository exclusion rules
```

---

## Sample Data

Demonstration datasets are provided in [`backend/sample_data/`](backend/sample_data):

- `expression.csv`: Normalized RNA-Seq count matrix (genes × samples).
- `clinical.csv`: Corresponding sample metadata with condition classes and covariates.
- `gene_list.csv` & `gene_list_logfc.csv`: Gene identifiers with log2 fold-changes for pathway enrichment benchmarks.
- `microarray_platforms.json`: Platform metadata definitions for probe mapping.

---

## Local Development & Code Inspection

### Prerequisites

- **Conda**: Miniconda, Anaconda, or Mamba
- **Node.js**: `≥ 18.x` with **npm** (or yarn / pnpm)
- **Git**

---

### Step-by-Step Installation

#### 1. Clone the Repository

```bash
git clone https://github.com/Pharmaco-OmicsLab/easyomifun.git
cd easyomifun
```

#### 2. Create and Activate Conda Environment

Create an isolated Conda environment containing R `4.4.3` and Python `3.11.15`:

```bash
conda create -n easyomifun python=3.11.15 r-base=4.4.3 -c conda-forge -y
conda activate easyomifun
```

#### 3. Install Python Dependencies (Exact Versions)

Install the exact versions of the required Python packages into the environment:

```bash
# Analytical dependencies via conda-forge
conda install -c conda-forge \
  numpy=2.4.6 \
  pandas=3.0.5 \
  scipy=1.17.1 \
  scikit-learn=1.9.0 \
  matplotlib=3.11.1 \
  statsmodels=0.14.6 \
  networkx=3.6.1 \
  cvxpy=1.9.2 \
  joblib=1.5.3 \
  seaborn=0.13.2 \
  openpyxl=3.1.5 \
  tqdm=4.70.0 \
  adjusttext=1.4.0 -y

# Pip packages
pip install knockpy==1.3.5
pip install https://github.com/gregbellan/Stabl/archive/refs/tags/v1.0.1-lw.zip
pip install fpdf2==2.8.8
```

##### Python Package Versions Reference
| Package | Version | Channel / Source | Purpose |
|---|---|---|---|
| `python` | `3.11.15` | conda-forge | Python Runtime |
| `numpy` | `2.4.6` | conda-forge | Matrix and numerical computation |
| `pandas` | `3.0.5` | conda-forge | Dataframe manipulation |
| `scipy` | `1.17.1` | conda-forge | Scientific computing & statistics |
| `scikit-learn` | `1.9.0` | conda-forge | Machine learning algorithms & metrics |
| `matplotlib` | `3.11.1` | conda-forge | Plotting engine |
| `statsmodels` | `0.14.6` | conda-forge | Statistical models |
| `networkx` | `3.6.1` | conda-forge | Graph data structures for STABL |
| `cvxpy` | `1.9.2` | conda-forge | Convex optimization solver |
| `joblib` | `1.5.3` | conda-forge | Multiprocessing & pipeline serialization |
| `seaborn` | `0.13.2` | conda-forge | Statistical visualization |
| `openpyxl` | `3.1.5` | conda-forge | Excel file parsing |
| `tqdm` | `4.70.0` | conda-forge | Progress bar utilities |
| `adjusttext` | `1.4.0` | conda-forge | Label collision avoidance |
| `knockpy` | `1.3.5` | PyPI (pip) | Model-X knockoffs for feature selection |
| `stabl` | `1.0.1-lw` | GitHub (pip) | Stability Selection biomarker discovery |
| `fpdf2` | `2.8.8` | PyPI (pip) | PDF report generation engine |

#### 4. Install R Dependencies (Exact CRAN & Bioconductor Versions)

Run the backend package installation script inside the activated environment:

```bash
Rscript backend/install_packages.R
```

> [!TIP]
> **Automatic `renv.lock` Support & Architecture Awareness (`arm64` / `x64`)**:
> - **Lockfile Integration**: If a `renv.lock` file is present in the repository root, `backend/install_packages.R` will automatically discover and parse it, locking all CRAN and Bioconductor packages to the exact versions declared in your lockfile. If no lockfile is present, it uses the built-in pinned versions listed below.
> - **Architecture Aware**: The installer automatically detects your operating system and hardware architecture (`arm64` Apple Silicon, `x86_64` Intel Mac, Windows `x64`, Linux). It configures architecture-appropriate binary package downloads (e.g., `mac.binary.big-sur-arm64` on Apple Silicon) from Posit Package Manager (PPM) and Bioconductor 3.20, with automatic fallback to native source compilation from CRAN archive tarballs if a pre-compiled binary is not available.
> - **Verification Check**: After installation, the script runs a self-test verification table confirming that each required package is loadable and matches the target version.

##### CRAN Package Versions Reference
| Package | Version | Purpose / Category |
|---|---|---|
| `shiny` | `1.14.0` | Analytical backend server & REST API |
| `jsonlite` | `2.0.0` | JSON serialization and API protocol |
| `yaml` | `2.3.12` | YAML parser |
| `callr` | `3.8.0` | Process execution |
| `later` | `1.4.8` | Asynchronous task scheduling |
| `rmarkdown` | `2.32` | Automated report compilation |
| `metapro` | `1.5.11` | Meta-analysis p-value combination |
| `metafor` | `5.0.1` | Meta-analysis models & forest plots |
| `caret` | `7.0.1` | Supervised model training & evaluation |
| `Boruta` | `9.0.0` | All-relevant feature selection |
| `randomForest` | `4.7.1.2` | Random Forest classifier |
| `e1071` | `1.7.17` | Support Vector Machines (SVM) |
| `glmnet` | `5.0` | Lasso & Elastic-Net regression |
| `MASS` | `7.3.64` | Statistical distributions & calculations |
| `ranger` | `0.18.0` | Fast Random Forest implementation |
| `gbm` | `2.3.1` | Generalized Boosted Regression Models |
| `pROC` | `1.19.1` | ROC curves & AUC computation |
| `ggthemes` | `6.0.0` | High-quality visual themes for ggplot2 |
| `segmented` | `2.2.1` | Segmented regression models |
| `ggplot2` | `4.0.3` | Data visualization |
| `ggridges` | `0.5.7` | Ridgeline density plots |
| `matrixStats` | `1.5.0` | Optimized matrix operations |
| `base64enc` | `0.1.6` | Base64 encoding for plot images |
| `zip` | `3.0.2` | Compression & archive generation |
| `writexl` | `2.0.1` | Excel file export |
| `openxlsx` | `4.2.9` | Advanced XLSX workbook generation |
| `pdftools` | `3.9.1` | PDF text & rendering utilities |
| `snow` | `0.4.4` | Cluster-based parallel processing |
| `doParallel` | `1.0.17` | Parallel computing backend |
| `foreach` | `1.5.2` | Foreach looping constructs |
| `iterators` | `1.0.14` | Iterators for foreach |
| `pheatmap` | `1.0.13` | Publication-ready heatmap rendering |
| `reticulate` | `1.47.0` | R interface to Python & STABL |

##### Bioconductor Package Versions Reference (Bioc 3.20)
| Package | Version | Purpose / Category |
|---|---|---|
| `BiocManager` | `1.30.25` | Bioconductor repository manager |
| `AnnotationDbi` | `1.68.0` | Gene and probe annotation interface |
| `DESeq2` | `1.46.0` | RNA-Seq differential expression analysis |
| `edgeR` | `4.4.2` | Differential gene expression for count data |
| `limma` | `3.62.2` | Microarray & RNA-Seq linear models |
| `clusterProfiler` | `4.14.6` | ORA and GSEA functional enrichment |
| `enrichplot` | `1.26.6` | Pathway enrichment visualizations |
| `ReactomePA` | `1.50.0` | Reactome pathway analysis |
| `reactome.db` | `1.89.0` | Reactome database annotations |
| `GO.db` | `3.20.0` | Gene Ontology database annotations |
| `msigdbr` | `26.1.1` | MSigDB molecular signatures database |
| `org.Hs.eg.db` | `3.20.0` | Genome-wide annotation for Human |
| `org.Mm.eg.db` | `3.20.0` | Genome-wide annotation for Mouse |
| `org.Rn.eg.db` | `3.20.0` | Genome-wide annotation for Rat |
| `org.Ss.eg.db` | `3.20.0` | Genome-wide annotation for Pig |
| `org.Gg.eg.db` | `3.20.0` | Genome-wide annotation for Chicken |
| `preprocessCore` | `1.68.0` | Microarray preprocessing & quantile normalization |
| `SummarizedExperiment` | `1.36.0` | Genomic assays & experiment data container |
| `sva` | `3.54.0` | ComBat batch effect removal |
| `vsn` | `3.74.0` | Variance stabilization normalization |
| `impute` | `1.80.0` | Microarray imputation |

#### 5. Frontend Setup

Install the Node dependencies for the user interface:

```bash
cd frontend
npm install
cd ..
```

---

### Running the Application

Run the backend and frontend in separate terminals:

#### Terminal 1: Start Analytical Backend (R Shiny REST API)

```bash
conda activate easyomifun
cd backend
Rscript app.R
```

> The server will start and listen on **`http://0.0.0.0:8080`**.

#### Terminal 2: Start Frontend Application (React + Vite)

```bash
cd frontend
VITE_ELECTRON=true npm run dev
```

> The Vite development server will start on **`http://localhost:5173`** and proxy `/api` requests to `http://localhost:8080`.

Open your browser and navigate to **`http://localhost:5173`** to access EasyOmiFun.

---

## Citation & Contact

If you use EasyOmiFun or its components in your research, please cite our corresponding manuscript:

> *Manuscript under review.* Citation details will be updated upon publication.

For questions, feedback, or collaborations, please reach out via:
- **Pharmaco-Omics Lab**: [pharmacoomicslab@gmail.com](mailto:pharmacoomicslab@gmail.com)
