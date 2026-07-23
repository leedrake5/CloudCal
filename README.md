# CloudCal

CloudCal is an open-source Shiny application for building and applying quantitative calibrations for X-ray fluorescence (XRF) spectrometers. It supports multiple instrument manufacturers, file formats, and calibration algorithms ranging from simple linear regression to advanced machine learning models.

![CloudCal Example](https://raw.githubusercontent.com/leedrake5/CloudCal/line_calculation/Images/rbexample.png)

[![DOI](https://zenodo.org/badge/78561304.svg)](https://zenodo.org/badge/latestdoi/78561304)

## Table of Contents
- [Features](#features)
- [Supported Instruments](#supported-instruments)
- [Supported File Formats](#supported-file-formats)
- [Calibration Models](#calibration-models)
- [Installation](#installation)
- [Quick Start](#quick-start)
- [Data Format Requirements](#data-format-requirements)
- [The .quant File Format](#the-quant-file-format)
- [How It Works](#how-it-works)
- [Citation](#citation)
- [References](#references)

---

## Features

### Spectral Analysis
- Interactive spectrum visualization with zoom and pan
- Automatic and manual energy calibration
- Spectral deconvolution using iterative least squares
- Multiple compression levels (100 eV, 50 eV, 25 eV bins)
- Log, exponential, and velocity transformations

### Line Analysis
- K-alpha, K-beta, L-alpha, L-beta, and M-line support for 88 elements (Ne through U)
- Multiple peak calculation methods: Gaussian, Split, First, Second
- Custom line definition support
- Covariance matrix visualization

### Normalization Options
- Time normalization
- Total counts normalization
- Compton scatter normalization (adjustable energy range)
- Region of Interest (ROI) normalization with Raw/Baseline/Net options
- LiveTime normalization for dead time correction

### Validation & Quality Control
- Cross-validation methods: k-fold, Bootstrap, 0.632 Bootstrap, Leave-One-Out (LOOCV), Out of Bag (OOB)
- Diagnostic plots: Residuals, Q-Q, Scale-Location, Cook's Distance
- 95% confidence bounds
- Automatic quality checks

### Data Export
- Custom .quant calibration files (open RDS format)
- PDF reports
- Excel-compatible worksheets
- Publication-quality plot exports
- Quantification results with uncertainty estimates

---

## Supported Instruments

| Manufacturer | Instruments/Software | Notes |
|--------------|---------------------|-------|
| **Bruker** | [Tracer series](https://www.bruker.com/en/products-and-solutions/elemental-analyzers/handheld-xrf-spectrometers/TRACER-5.html?source=bing&medium=cpc&campaign=GSN_%7C_North-America&content=02_-_GSN_%7C_Product_%7C_HMP_%7C_TRACER_5&s_kwcid=AL!14677!10!83425856060383!83426631305716&msclkid=86f5cc269aaf127e0936a54ea0627ea2&utm_source=bing&utm_medium=cpc&utm_campaign=GSN%20%7C%20North%20America%20%7C%20Products&utm_term=bruker%20tracer&utm_content=HMP%20%7C%20Product%20%7C%20TRACER%205) (IISD, IVSD, IIV+, 5i, 5g), Artax, Titan | PDZ v24 and v25 supported |
| **Evident/Olympus** | [Vanta series](https://ims.evidentscientific.com/en/xrf-analyzers/handheld) | Multi-beam support via aggregate CSV and JSON|
| **Thermo Fisher** | [Niton series](https://www.thermofisher.com/order/catalog/product/NITONXL5plus) | Aggregate CSV format |
| **SciAps** | [X-series](https://www.sciaps.com/products/xrf/x-series) | CSV export |
| **XGLab** | [Elio](https://www.bruker.com/en/products-and-solutions/elemental-analyzers/micro-xrf-spectrometers/elio.html?source=bing&medium=cpc&campaign=GSN_%7C_North-America&content=02_-_GSN_%7C_Product_%7C_Micro_XRF_%7C_Elio&s_kwcid=AL!14677!10!83632014544498!83632790257626&msclkid=345c7798e6c61dcb9f221ef0dde4d599&utm_source=bing&utm_medium=cpc&utm_campaign=GSN%20%7C%20North%20America%20%7C%20Products&utm_term=bruker%20elio&utm_content=XMA%20%7C%20Product%20%7C%20Micro%20XRF%20%7C%20Elio) | Three formats: .spt, .spx, .mca |
| **Cox Analytical** | [Itrax Core Scanner](https://www.coxsys.se/itrax-core-scanner) | SPE format with DFL calibration |
| **Generic** | Any XRF with CSV/JSON export | Standard column format |

---

## Supported File Formats

| Format | Extension | Description | Source |
|--------|-----------|-------------|--------|
| **CSV** | `.csv` | Comma-separated spectra or net counts | S1PXRF, Artax, SciAps, Generic |
| **JSON** | `.json` | JSON format spectra | Various |
| **PDZ** | `.pdz` | Bruker proprietary binary format | Tracer series (v24 & v25) |
| **SPX** | `.spx` | Artax/Elio spectra format | Artax, Elio |
| **SPT** | `.spt` | Elio spectrum format | XGLab Elio |
| **MCA** | `.mca` | Multi-channel analyzer format | Elio, Generic MCA |
| **SPE** | `.spe` | Itrax spectrum format | Cox Analytical Itrax |
| **DFL** | `.dfl` | Itrax detector calibration | Cox Analytical Itrax |
| **TXT** | `.txt` | Text format spectra | Various |
| **Net** | `.csv` | Net counts from Artax | Artax 7.4+ |

### Aggregate CSV Support
CloudCal automatically detects multi-beam CSV files from:
- **Niton**: Files containing "Main Range" in header
- **Olympus/Vanta**: Files containing "Exposure Number" or "exposition"

---

## Calibration Models

A detailed guide to every model type — how each one learns, when to use it, and what every stored parameter means — is in **[MODELS.md](MODELS.md)**.

### Traditional Models
| Model | Description | Best For |
|-------|-------------|----------|
| **Linear** | Simple linear regression | Single-element, matrix-matched standards |
| **Polynomial** | Second-order polynomial | Non-linear concentration relationships |
| **Lucas-Tooth** | Interelement effect correction | Multi-element matrices with interference |

### Machine Learning Models
| Model | Description | Configuration Options |
|-------|-------------|----------------------|
| **Random Forest** | Ensemble of decision trees | Trees (1-2000), features per split, bootstrap |
| **XGBoost** | Gradient boosted trees | Tree depth, learning rate, regularization, Bayesian optimization |
| **Neural Network (Intensities)** | Feed-forward network on peak intensities | Hidden layers (2-3), units, weight decay |
| **Neural Network (Spectra)** | Feed-forward network on full spectra | Hidden layers (2-3), units, weight decay |
| **Support Vector Machine** | Kernel-based regression | Cost, sigma, polynomial degree |
| **Bayesian (Intensities)** | Bayesian regression on intensities | Prior parameters |
| **Bayesian (Spectra)** | Bayesian regression on spectra | Prior parameters |
| **BART** | Bayesian Additive Regression Trees | Beta, nu parameters |

### Chemometric Models
| Model | Description | Configuration Options |
|-------|-------------|----------------------|
| **PLS** (Intensities / Spectra) | Partial least squares on latent components | Number of components |
| **Cubist** (Intensities / Spectra) | Rule-based committees with linear leaf models | Committees, neighbors |
| **Elastic Net** (Intensities / Spectra) | Sparse penalized linear regression (glmnet) | Alpha (ridge–lasso mix), lambda |
| **MARS** (Intensities / Spectra) | Piecewise-linear adaptive splines (earth) | Terms (nprune), interaction degree |

---

## Installation

### Requirements
- R (version 4.0 or later recommended)
- Internet connection for initial package installation

### Quick Install

1. **Install R** from https://www.r-project.org/

2. **Install required packages** by running in R:
```r
install.packages(c("shiny", "Rcpp"))
```

3. **Run CloudCal** directly from GitHub:
```r
shiny::runGitHub("leedrake5/CloudCal")
```

The first run will automatically install additional dependencies.

### Offline Installation

Download the repository and run locally:
```r
shiny::runApp("/path/to/CloudCal")
```

### Key Dependencies
CloudCal uses many R packages including:
- **Core**: shiny, ggplot2, dplyr, data.table
- **Machine Learning**: caret, randomForest, xgboost, nnet, kernlab
- **XRF Processing**: rPDZ (for Bruker PDZ files), Peaks, baseline
- **Parallel Computing**: parallel, doParallel, pbapply

---

## Quick Start

1. **Load Spectra**: Select your file type and upload spectrum files
2. **Define Lines**: Choose elements and peak calculation method in the Counts tab
3. **Add Concentrations**: Enter reference material values
4. **Build Calibration**: Select model type and train in Cal Curves tab
5. **Validate**: Review cross-validation metrics and diagnostic plots
6. **Apply**: Quantify unknowns in the Apply Calibration tab

---

## Data Format Requirements

### S1PXRF (Bruker Tracer)
1. Go to **Setup → Group Conversion**
2. Select directory with PDZ files
3. Choose **CSV** as output format
4. Enable **Replace duration with live time** (recommended)

### Artax
1. Process spectra normally
2. Go to **Export → All Results** (not "All Results to Excel")
3. This creates compatible CSV files

### PDZ Files (Direct Upload)
- **PDZ v24**: Classic Tracer format (IISD/IVSD/IIV+)
- **PDZ v25**: Tracer 5i format
- LiveTime normalization option available for dead time correction

### Itrax SPE Files
- Upload `.spe` spectrum files
- Optionally upload `.dfl` file for accurate energy calibration
- Without DFL, SPE header calibration values are used

### Generic CSV Format
Column structure: `Energy, Spectrum1, Spectrum2, ...` or melted format with `Energy, CPS, Spectrum` columns.

---

## The .quant File Format

The `.quant` file is an open-source RDS format containing all calibration data. To inspect:

```r
str(readRDS("your_calibration.quant"))
```

### Structure
```
$FileType        # Source data format (CSV, PDZ, etc.)
$Units           # Concentration units (%, ppm)
$LineDefaults    # Line definitions for peaks
$Spectra         # Full spectral data used for calibration
$Intensities     # Extracted element intensities
$Values          # Reference material concentrations
$Deconvoluted    # Output from xrftools linear deconvolution
  └─$Areas       # Net peaks 
  └─$Spectra     # Baseline-subtracted spectra
  └─$Baseline    # Baseline spectra (used for normalizations)
  └─$Parameters  # Arguments passed to deconvolution functions
$calList         # Per-element calibration models
  └─$Element
      ├─$Parameters
      │   ├─$CalTable    # Includes normalization instructions and model arguments
      ├─$Slope           # Lucas-Tooth/ML slope variables
      ├─$Intercept       # Lucas-Tooth/ML intercept variables
      ├─$StandardsUsed   # Standards included in model
      └─$Model           # Trained model object

```

---

## How It Works

CloudCal is based on the Lucas-Tooth and Price (1961) algorithm with extensions for modern machine learning:

### Lucas-Tooth Equation

C<sub>i</sub> = r<sub>0</sub> + I<sub>i</sub>[r<sub>i</sub> + Σ(r<sub>in</sub>I<sub>n</sub>)]

Where:
- **C<sub>i</sub>**: Concentration of element i
- **r<sub>0</sub>**: Intercept (empirical constant)
- **r<sub>i</sub>**: Slope coefficient for element i intensity
- **r<sub>in</sub>**: Interelement correction coefficient (effect of element n on element i)
- **I<sub>i</sub>**: Net intensity of element i
- **I<sub>n</sub>**: Net intensity of interfering element n

This extends simple linear regression (y = mx + b) by accounting for matrix effects where one element's fluorescence influences another's measured intensity.

### Why Calibrate?
1. **Estimate concentrations** from X-ray spectra
2. **Account for matrix variation** in different sample types
3. **Enable cross-instrument comparability** for reproducible results

---

## Citation

Please cite CloudCal in publications:

**Current Version:**
> Drake, B.L. 2025. CloudCal. GitHub. https://github.com/leedrake5/CloudCal. doi: 10.5281/zenodo.2596154

**Legacy Version (v3.0):**
> Drake, B.L. 2018. CloudCal v3.0. GitHub. https://github.com/leedrake5/CloudCal. doi: 10.5281/zenodo.2596154

---

## References

- Kuhn, M. 2008. Building predictive models in R using the caret package. Journal of Statistical Software 28(5): 1-26
- Liaw, A., Wiener, M. 2002. Classification and Regression by randomForest. R News 2(3): 18-22
- Lucas-Tooth, H.J., Price, B.J. 1961. A Mathematical Method for the Investigation of Interelement Effects in X-Ray Fluorescence Analysis. Metallurgia 64: 149-152
- Speakman, R.J., Shackley, M.S. 2013. Silo science and portable XRF in archaeology: a response to Frahm. Journal of Archaeological Science 40: 1435-1443
- Venables, W.N., Ripley, B.D. 2002. Modern Applied Statistics with S. Fourth Edition. Springer, New York. ISBN 0-387-95457-0

---

## Contributing

Issues and pull requests are welcome at https://github.com/leedrake5/CloudCal/issues

## License

See [LICENSE](LICENSE) file for details.
