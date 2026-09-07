# CloudCal Calibration Models

CloudCal offers 21 calibration model types, selected from the **Calibration Curve** dropdown on the calCurves page. They range from classical univariate regression to machine-learning and chemometric methods, and every one of them produces the same thing: a per-element model that converts measured X-ray intensity information into concentration. This document describes how each model learns, when it is a good choice, and what every stored parameter means. All models except XGBoost are trained through the caret framework (Kuhn 2008), which standardizes resampling, tuning, and prediction across packages; XGBoost is trained through its native engine (Chen & Guestrin 2016) so that its raw booster can be serialized into the `.quant` file.

Every fitted model is stored per element in `calList$<element>`, with its complete configuration snapshot in `$Parameters$CalTable`. That snapshot is what makes calibrations reproducible: reloading a `.quant` restores each element's model type, data preparation, and tuning ranges exactly as they were saved, independent of whatever other elements or models were configured in the same session.

## The two data shapes

Every machine-learning model comes in two flavors that differ in what the model sees:

- **Intensities models** (even-numbered types 4, 6, 8, 10, 12, 14, 16, 18, 20 plus the classical models) learn from the extracted peak intensities of the element lines you selected — typically a few dozen predictors such as `Fe.K.alpha`, `Rb.K.alpha`, plus optional `Total` counts and `Baseline` area. The slope-line selector controls which lines enter the model; for machine-learning types the default is all available lines, so the model can discover inter-element (matrix) relationships on its own.
- **Spectra models** (types 5, 7, 9, 11, 13, 15, 17, 19, 21) learn from the full binned spectrum — every energy channel in the chosen range becomes a predictor (for example 0.7–37 keV at 100 eV bins is 364 predictors). No line definitions are needed; the model decides which spectral regions matter. This is the more exploratory shape, and feature-importance output shows which energies the model actually used.

## Shared data-preparation settings

These options apply before any model sees the data, and they are stored in the CalTable so a saved calibration always reproduces its own preprocessing.

| CalTable variable | UI control | Meaning |
|---|---|---|
| `NormType` | Normalization | `1` = Time (counts per second as read from the file), `2` = Total Counts (each intensity or channel divided by the spectrum's total counts), `3` = ROI (divided by the summed counts in a user-chosen energy window, classically the Compton scatter peak) |
| `Min`, `Max` | ROI Min / Max | The energy window (keV) for ROI normalization |
| `ComptonType` | ROI Type | Whether the ROI is summed from the `Raw` spectrum, the SNIP `Baseline`, or the deconvolved `Net` spectrum |
| `LineType` | Line preference | `Narrow` (tight window around the line centroid), `Wide` (broader window), or `Area` (deconvolved peak areas) |
| `LineStructure` | Line calculation | `gaussian` (Gaussian-weighted window), `split` (asymmetric window), `first` / `second` (first- or second-order escape adjustments) |
| `GausBuffer`, `SplitBuffer` | Buffers | Window half-widths (keV) for the gaussian and split line calculations (defaults 0.02 and 0.1) |
| `DepTrans` | Concentration Transformation | Transform applied to the *dependent* variable before fitting and inverted at prediction: `None`, `Log` (fit log concentration, predict `exp`), `e` (fit `exp` concentration, predict `log`), `Scale` (min–max scale to 0–1, decoded with the stored `Scale$Min`/`Scale$Max`) |
| `Compress` | Compress (spectra models) | Spectrum binning: `100 eV`, `50 eV`, or `25 eV`. Finer bins mean more predictors |
| `Transformation` | Spectra Transformation (spectra models) | Per-channel transform before binning: `None`, `Log`, `e`, or `Velocity` (Hodder background-subtraction derivative, which emphasizes peak shape over continuum) |
| `EnergyRange` | Energy range (spectra models) | The keV window binned into predictors, stored as a `"lo-hi"` string (default `"0.7-37"`) |
| `Deconvolution`, `SmoothWidth`, `SmoothAlpha`, `DeconvolutionSigma`, `SmoothIter`, `SnipIter` | Deconvolution panel | Whether and how spectra were deconvolved (least-squares fitting with SNIP background), and the parameters used |
| `StandardsUsed` | Point selection on the cal curve | Logical mask of which standards were included when the model was fit |
| `Scale` | (internal) | The concentration min/max captured for the `Scale` dependent transformation |

## Shared training controls

All caret-trained models expose the same resampling machinery, stored in the CalTable:

| CalTable variable | UI control | Meaning |
|---|---|---|
| `ForestMetric` | Metric | The loss caret minimizes when choosing tuning values: `RMSE`, `Rsquared`, `MAE`, `logMAE`, or `SMAPE` |
| `ForestTC` | Training method | Resampling scheme: `cv` (k-fold cross-validation), `boot` (bootstrap), `boot632`, `optimism_boot`, `repeatedcv`, `LOOCV`, or `oob` (out-of-bag, tree ensembles only). `none` fits once without resampling |
| `ForestNumber` | Number | Folds (for CV) or resamples (for bootstrap) |
| `CVRepeats` | Repeats | Repeats when `ForestTC = "repeatedcv"` |
| — | Multicore Processing | `Single Core`, `Serialize` (PSOCK cluster), or `Fork` (Unix fork cluster) for parallel resampling. XGBoost additionally offers `OpenMP` threading. MARS always trains single-core (see type 20/21) |

Tuning ranges are stored as `"lo-hi"` strings (the same convention as `svmC = "1-5"`). When a calibration is saved, the winning value found by resampling (`bestTune`) is written back, so a reloaded calibration shows the tuned value rather than the original search range.

## The models

### 1. Linear

Ordinary least-squares regression of concentration on a single line intensity, `lm(Concentration ~ Intensity)`. This is the reference method for matrix-matched standards where one line tracks one element cleanly, and its confidence intervals are reported directly on the calibration curve. No tuning parameters.

### 2. Polynomial (Non-Linear)

Second-order polynomial regression on a single line intensity. Useful when self-absorption or detector saturation curves the intensity–concentration relationship at high concentrations, at the cost of poorer extrapolation beyond the calibrated range. No tuning parameters.

### 3. Lucas-Tooth

The inter-element correction model named for Lucas-Tooth & Price (1961). This is the right tool when absorption and enhancement by co-occurring elements bias a simple linear fit — the classic example being Fe absorption effects in geological matrices. It comes in two forms, chosen by the **Cross-product matrix correction (Lucas-Tooth 1961)** checkbox:

**Additive (default, checkbox off).** A linear model in which the analyte line's intensity is joined additively by the intensities of the selected slope lines, with optional intercept lines, `Concentration ~ Intensity + I_1 + I_2 + ...`. This is CloudCal's long-standing behavior and what every previously saved calibration continues to use.

**Classic 1961 (checkbox on).** The paper's equation (their eq. 1) fitted literally:

```
C = a + I·(κ₀ + Σ κ_x·I_x)
```

Each selected slope line enters as a cross-product of intensities — the fitted terms are `Intensity` (the κ₀ sensitivity) plus one `Slope_<line>` column per corrector holding `I·I_x`, so the model's coefficients read directly as the paper's x+2 constants: the regression intercept is `a`, the `Intensity` coefficient is `κ₀`, and each `Slope_<line>` coefficient is that line's influence coefficient `κ_x`. Selecting the analyte's own line as a slope (the default) produces the paper's quadratic self term `I²` — in their copper example, I_Cu appears inside its own correction. Intercept lines have no role in the 1961 equation, so the Intercept selector is hidden while the checkbox is on. Every column — analyte and correctors alike — is divided by the same normalization factor, so the products stay on one consistent scale across all three normalization types and every ROI source (Raw, SNIP, arPLS, E1, Net).

The companion **1961 intensity weighting (relative-error fit)** checkbox (on by default) reproduces how the paper actually solved the least squares: they divided the whole equation by the analyte intensity and minimized Σ((C_chem − C_calc)/I)², arguing that a 5% and a 30% sample deserve equal *fractional* accuracy. Fitting the expanded equation with weights 1/I² is algebraically the same solve — and it is also what conditions the design matrix, since the raw cross-product columns all share the factor I and would otherwise be nearly collinear (the reason naive attempts at this model break `lm`). Near-zero analyte intensities have their weights floored so a blank standard cannot dominate the fit. Note the relative-error rationale strictly applies when the Concentration Transformation is `None`; with `Log`/`e`/`Scale` the weights act on the transformed scale. If the selected terms are still collinear (for example a slope line identical to the analyte under Time normalization) the aliased terms are dropped, refit, and reported in a notification rather than left as NA coefficients.

The saved parameters record everything needed to reproduce the fit: `Slope` and `Intercept` list the lines used, and the CalTable's `LTCross` (`"Additive"`/`"Classic"`) and `LTWeight` (`"None"`/`"1961"`) record the mode — calibrations saved before these fields existed load as `Additive` and predict exactly as they always have.

### 4. Forest (Random Forest, Intensities) and 5. Rainforest (Random Forest, Spectra)

A bagged ensemble of decision trees (Breiman 2001), fit with the randomForest package (Liaw & Wiener 2002) via caret. Each tree sees a bootstrap sample of the standards and a random subset of predictors at each split; predictions average across trees. Forests are robust to irrelevant predictors and to outliers, which makes them a safe default for all-slopes intensity models and for full spectra ("Rainforest"). Their main limitation is extrapolation: a forest predicts a constant outside the calibrated concentration range. Parameters: `ForestTry` (`mtry`, predictors considered per split) and `ForestTrees` (`ntree`, ensemble size).

### 6. Neural Network Intensities and 7. Neural Network Spectra

Feed-forward neural networks. With one hidden layer the model is fit with nnet (Venables & Ripley 2002); selecting more layers switches to the neuralnet engine for deeper architectures. Networks can represent smooth non-linear response surfaces, but on calibration-sized data (tens of standards) they need the weight-decay regularization to avoid memorizing, and small networks can legitimately converge to near-constant predictions when the signal is weak. Parameters: `NeuralHL` (hidden layers), `NeuralHU` (units per layer, `"lo-hi"` range), `NeuralWD` (weight decay range), `NeuralMI` (maximum training iterations).

### 8. XGBoost Intensities and 9. XGBoost Spectra

Gradient-boosted trees fit with the native xgboost engine (Chen & Guestrin 2016). Unlike a forest's independent trees, boosting fits each new tree to the *residuals* of the ensemble so far, which lets it chase secondary structure — absorption edges, scatter regions — that bagged ensembles average away. In CloudCal's redundancy benchmarking this is precisely why XGBoost's learned feature importance differs from the forest/SVM cluster. The `xgbType` selector chooses the booster: `Tree` (gbtree), `DART` (dropout-regularized trees), or `Linear` (boosted penalized linear model). Hyperparameter ranges can be searched by grid or by Bayesian optimization. Parameters: `TreeMethod`, `TreeDepth`, `xgbEta` (learning rate), `xgbAlpha`/`xgbLambda` (L1/L2 regularization), `xgbGamma` (split penalty), `xgbSubSample`/`xgbColSample` (row/column subsampling), `xgbMinChild`, `xgbMaxDeltaStep`, and for DART `DropTree`/`SkipDrop`. The fitted booster is also stored raw (`rawModel`) so it survives serialization across xgboost versions.

### 10. Bayes Intensities and 11. Bayes Spectra

A family of Bayesian regressions chosen with the sub-type selector: **Linear** fits `bayesglm` from the arm package, a linear model with weakly-informative Cauchy priors that remains stable where ordinary regression is degenerate (Gelman et al. 2008); **Neural Net** fits a Bayesian-regularized neural network with brnn (Pérez-Rodríguez & Gianola 2013), which sets regularization strength by evidence maximization rather than cross-validation (MacKay 1992); **Tree** fits Bayesian Additive Regression Trees with bartMachine (Kapelner & Bleich 2016), a sum-of-trees model whose priors keep individual trees weak (Chipman, George & McCulloch 2010). BART is the second model family (after XGBoost) whose learned spectral structure departs from the forest/SVM cluster, thanks to those sparsity-inducing priors. Parameters: `bartK` (prior concentration-scale shrinkage), `bartBeta` and `bartNu` (tree-depth and error-variance priors). Note that `bayesglm` has no tuning grid, so it trains with `ForestTC = "none"` when predictors outnumber the standards in a fold.

### 12. Support Vector Intensities and 13. Support Vector Spectra

Support vector regression via kernlab (Karatzoglou et al. 2004). An SVM fits the flattest function that passes within a tolerance of the training points, with kernels providing non-linearity. The kernel selector offers `Linear`, `Polynomial`, and three radial-basis variants (`Radial`, `Radial Cost`, `Radial Sigma`) that differ in which of the cost and kernel-width parameters are tuned versus estimated. On smooth, collinear XRF data an RBF-SVM behaves much like a forest — a local smoother over spectral similarity — so treat it as an alternative within that cluster rather than an independent opinion. Parameters: `svmC` (cost), `svmSigma` (RBF kernel width), `svmDegree` and `svmScale` (polynomial kernel).

### 14. PLS Intensities and 15. PLS Spectra

Partial least squares regression (Wold 1975; Wold, Sjöström & Eriksson 2001), fit with the pls package (Mevik & Wehrens 2007). PLS projects the predictors onto a small number of latent components chosen to maximize covariance with concentration, then regresses on those components. It is the standard chemometric method for exactly this data shape — many collinear channels, few samples — and it learns something no tree ensemble can: global linear combinations across the whole spectrum. Because the final model is linear, it also extrapolates sensibly beyond the calibrated range, where tree models plateau. In CloudCal's redundancy benchmark PLS roughly doubled the out-of-fold R² of the forest on the same data. One parameter: `plsNComp`, the range of latent components searched (capped automatically at the data rank).

### 16. Cubist Intensities and 17. Cubist Spectra

A rule-based model committee descended from M5 (Quinlan 1992, 1993), fit with the Cubist package (Kuhn & Johnson 2013). Cubist partitions the standards with rules and fits a separate *linear* regression in each partition, then optionally corrects predictions using the nearest training neighbors. For XRF this is a natural fit: the rules act like matrix classes, and each class gets its own locally-linear calibration — a machine-learned cousin of running separate Lucas-Tooth calibrations per matrix type. Boosting-style `committees` give it a residual-chasing character, and its learned structure decorrelates from the forest/SVM cluster more than any other model except MARS. Parameters: `cubistCommittees` (number of boosted rule sets) and `cubistNeighbors` (0–9 nearest-neighbor correction).

### 18. Elastic Net Intensities and 19. Elastic Net Spectra

Penalized linear regression with the elastic-net penalty (Zou & Hastie 2005), fit with glmnet (Friedman, Hastie & Tibshirani 2010). The penalty blends lasso (which zeroes out unhelpful channels, performing automatic line selection) and ridge (which shares weight across correlated channels, the natural behavior for adjacent spectral bins). The result is a sparse, interpretable, fully linear calibration whose coefficients say directly which channels carry the signal — and, like PLS, it extrapolates linearly. Parameters: `glmnetAlpha` (0 = pure ridge, 1 = pure lasso) and `glmnetLambda` (overall penalty strength, searched on a log grid).

### 20. MARS Intensities and 21. MARS Spectra

Multivariate adaptive regression splines (Friedman 1991), fit with the earth package (Milborrow 2011). MARS builds a regression from hinge functions — piecewise-linear terms with automatically-chosen breakpoints — and can include products of hinges for interactions. The breakpoints land where the response changes regime (an absorption edge, the onset of self-absorption), so the model reads as a small set of interpretable linear segments. In CloudCal's redundancy benchmark MARS was simultaneously the *most accurate* and the *least redundant* model against the forest/SVM cluster, which earned it its place here. Parameters: `marsPrune` (`nprune`, maximum terms retained after backward pruning) and `marsDegree` (1 = additive, 2–3 allows interaction terms). MARS always trains single-core regardless of the multicore setting: its fits are sub-second, and caret's earth submodel loop is unreliable inside forked workers when other OpenMP runtimes are resident.

## Choosing among them

The chemometric additions (14–21) were chosen empirically: `tests/redundancy_benchmark.R` trains candidates and incumbents on identical cross-validation folds and correlates their out-of-fold *residuals* — what two models get wrong together measures how redundant one of them is. On obsidian test data the forest, SVM, and BART cluster tightly (residual correlations 0.91–0.95), XGBoost sits partly apart (~0.75–0.80), and the linear-family chemometric models both decorrelate from that cluster and outperform it in accuracy, because XRF intensity–concentration structure is fundamentally linear-latent. As practical guidance: start with Linear or Lucas-Tooth when you have good matrix-matched standards; reach for PLS or Elastic Net when many lines or full spectra carry the signal; try Cubist or MARS when you suspect distinct matrix regimes or threshold effects; use forests, XGBoost, or SVM when you want a robust non-parametric check; and remember that tree ensembles cannot extrapolate beyond the calibrated concentration range, while the linear-family models can.

## References

- Breiman, L. (2001). Random forests. *Machine Learning*, 45(1), 5–32.
- Chen, T., & Guestrin, C. (2016). XGBoost: A scalable tree boosting system. *Proceedings of the 22nd ACM SIGKDD International Conference on Knowledge Discovery and Data Mining*, 785–794.
- Chipman, H. A., George, E. I., & McCulloch, R. E. (2010). BART: Bayesian additive regression trees. *Annals of Applied Statistics*, 4(1), 266–298.
- Friedman, J. H. (1991). Multivariate adaptive regression splines. *Annals of Statistics*, 19(1), 1–67.
- Friedman, J., Hastie, T., & Tibshirani, R. (2010). Regularization paths for generalized linear models via coordinate descent. *Journal of Statistical Software*, 33(1), 1–22.
- Gelman, A., Jakulin, A., Pittau, M. G., & Su, Y.-S. (2008). A weakly informative default prior distribution for logistic and other regression models. *Annals of Applied Statistics*, 2(4), 1360–1383.
- Kapelner, A., & Bleich, J. (2016). bartMachine: Machine learning with Bayesian additive regression trees. *Journal of Statistical Software*, 70(4), 1–40.
- Karatzoglou, A., Smola, A., Hornik, K., & Zeileis, A. (2004). kernlab — an S4 package for kernel methods in R. *Journal of Statistical Software*, 11(9), 1–20.
- Kuhn, M. (2008). Building predictive models in R using the caret package. *Journal of Statistical Software*, 28(5), 1–26.
- Kuhn, M., & Johnson, K. (2013). *Applied Predictive Modeling*. Springer, New York.
- Liaw, A., & Wiener, M. (2002). Classification and regression by randomForest. *R News*, 2(3), 18–22.
- Lucas-Tooth, H. J., & Price, B. J. (1961). A mathematical method for the investigation of inter-element effects in X-ray fluorescence analysis. *Metallurgia*, 64(383), 149–152.
- MacKay, D. J. C. (1992). Bayesian interpolation. *Neural Computation*, 4(3), 415–447.
- Mevik, B.-H., & Wehrens, R. (2007). The pls package: Principal component and partial least squares regression in R. *Journal of Statistical Software*, 18(2), 1–23.
- Milborrow, S. (2011). *earth: Multivariate Adaptive Regression Splines*. R package (derived from mda::mars by T. Hastie and R. Tibshirani).
- Pérez-Rodríguez, P., & Gianola, D. (2013). *brnn: Bayesian regularization for feed-forward neural networks*. R package.
- Quinlan, J. R. (1992). Learning with continuous classes. *Proceedings of the 5th Australian Joint Conference on Artificial Intelligence*, 343–348.
- Quinlan, J. R. (1993). Combining instance-based and model-based learning. *Proceedings of the 10th International Conference on Machine Learning*, 236–243.
- Venables, W. N., & Ripley, B. D. (2002). *Modern Applied Statistics with S* (4th ed.). Springer, New York.
- Wold, H. (1975). Soft modelling by latent variables: The non-linear iterative partial least squares (NIPALS) approach. In *Perspectives in Probability and Statistics*, 117–142.
- Wold, S., Sjöström, M., & Eriksson, L. (2001). PLS-regression: A basic tool of chemometrics. *Chemometrics and Intelligent Laboratory Systems*, 58(2), 109–130.
- Zou, H., & Hastie, T. (2005). Regularization and variable selection via the elastic net. *Journal of the Royal Statistical Society: Series B*, 67(2), 301–320.
