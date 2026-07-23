# CloudCal model-redundancy benchmark
# ===================================
# Empirically measures how redundant candidate caret models (PLS, Cubist,
# glmnet, MARS/earth, Gaussian process) are against the forest/SVM-style
# learners already in CloudCal (rf, svmRadial, xgbTree, bartMachine).
#
# Method: identical CV folds for every model; collect out-of-fold predictions;
# redundancy = correlation of OUT-OF-FOLD RESIDUALS with each incumbent (what
# two models get wrong together is what makes one of them unnecessary). Also
# compares model-specific feature-importance rankings (Spearman) against the
# random forest's, where the model family defines its own importance (caret
# falls back to a model-agnostic filter for SVM/GP - excluded, it would fake
# similarity).
#
# Output: per data shape (Intensities / Spectra), OOF R2 per model, residual
# correlation vs the incumbent cluster, and an empirical non-redundancy score
# (1 - max residual correlation with any incumbent), averaged over elements.
#
# Usage: Rscript tests/redundancy_benchmark.R [quant] [n_elements] [seed]

args <- commandArgs(trailingOnly = TRUE)
QUANT <- if (length(args) >= 1) args[1] else
    stop("Usage: Rscript tests/redundancy_benchmark.R [quant] [n_elements] [seed]\n  [quant] is required: path to a CloudCal .quant calibration file.", call. = FALSE)
N_EL <- if (length(args) >= 2) as.integer(args[2]) else 6
SEED <- if (length(args) >= 3) as.integer(args[3]) else 42

APP_DIR <- normalizePath(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(), value = TRUE)[1])), ".."))
if (!file.exists(file.path(APP_DIR, "global.R"))) APP_DIR <- getwd()
setwd(APP_DIR)

options(java.parameters = "-Xmx2g")
# macOS: xgboost's libomp clashes with earth/plotmo's OpenMP after both load
# (OMP Error #13 assertion). Single-threaded OMP sidesteps it; n=40 doesn't care.
Sys.setenv(OMP_NUM_THREADS = "1", KMP_DUPLICATE_LIB_OK = "TRUE")
suppressMessages(suppressWarnings(source("global.R")))
suppressMessages(library(caret))

cal <- calRDS(QUANT, xgb_raw = FALSE, sort = TRUE)

## ------------------------------------------------------------------ targets
finite_n <- function(el) sum(is.finite(suppressWarnings(as.numeric(cal$Values[[el]]))))
els <- names(cal$calList)[names(cal$calList) %in% colnames(cal$Values)]
els <- els[order(-vapply(els, finite_n, numeric(1)))]
preferred <- c("Y.K.alpha", "Rb.K.alpha", "Sr.K.alpha", "Zr.K.alpha", "Fe.K.alpha", "K.K.alpha")
els <- unique(c(intersect(preferred, els), els))[seq_len(min(N_EL, length(els)))]
message("elements: ", paste(els, collapse = ", "))

## ------------------------------------------------------------------ features
lines_avail <- intersect(colnames(cal$Intensities), spectralLines)
X_int <- cal$Intensities[, c("Spectrum", lines_avail,
                             intersect(c("Total", "Baseline"), colnames(cal$Intensities)))]
X_spec <- rainforestDataPreGen(spectra = cal$Spectra, compress = "100 eV",
                               transformation = "None", energy.range = c(0.7, 37),
                               norm.type = 2, data.type = "Spectra")
shapes <- list(Intensities = X_int, Spectra = X_spec)

## ------------------------------------------------------------------ models
# incumbents (the forest-SVM learning cluster + the two known outliers)
# vs candidates. Small fixed/short grids: the question is representation,
# not squeezing the last decimal of RMSE.
MODELS <- list(
    rf      = list(method = "rf",           cand = FALSE, imp = TRUE,
                   grid = data.frame(mtry = 2), extra = list(ntree = 200)),
    svm     = list(method = "svmRadial",    cand = FALSE, imp = FALSE,
                   grid = expand.grid(sigma = c(0.001, 0.01), C = c(1, 10)), extra = list()),
    xgb     = list(method = "xgbTree",      cand = FALSE, imp = TRUE,
                   grid = expand.grid(nrounds = 50, max_depth = c(2, 4), eta = 0.3,
                                      gamma = 0, colsample_bytree = 1, min_child_weight = 1,
                                      subsample = 1), extra = list(verbosity = 0, nthread = 1)),
    bart    = list(method = "bartMachine",  cand = FALSE, imp = TRUE,
                   grid = expand.grid(num_trees = 50, k = 2, alpha = 0.95, beta = 2, nu = 3),
                   extra = list(verbose = FALSE, serialize = FALSE)),
    pls     = list(method = "pls",          cand = TRUE,  imp = TRUE,
                   grid = data.frame(ncomp = 1:10), extra = list()),
    cubist  = list(method = "cubist",       cand = TRUE,  imp = TRUE,
                   grid = expand.grid(committees = c(1, 10), neighbors = c(0, 5)), extra = list()),
    glmnet  = list(method = "glmnet",       cand = TRUE,  imp = TRUE,
                   grid = expand.grid(alpha = c(0.1, 0.5, 1), lambda = 10^seq(-3, 0, length.out = 5)),
                   extra = list()),
    mars    = list(method = "earth",        cand = TRUE,  imp = TRUE,
                   grid = expand.grid(nprune = c(3, 6, 10), degree = 1:2), extra = list()),
    gp      = list(method = "gaussprRadial", cand = TRUE, imp = FALSE,
                   grid = data.frame(sigma = c(0.001, 0.01, 0.1)), extra = list())
)

fit_one <- function(mm, X, y, folds){
    ctrl <- trainControl(method = "cv", index = folds, savePredictions = "final")
    do.call(caret::train, c(list(x = X, y = y, method = mm$method,
                                 trControl = ctrl, tuneGrid = mm$grid), mm$extra))
}

oof_pred <- function(fit, n){
    p <- fit$pred
    out <- rep(NA_real_, n)
    agg <- tapply(p$pred, p$rowIndex, mean)   # one OOF value per row
    out[as.integer(names(agg))] <- as.numeric(agg)
    out
}

## ------------------------------------------------------------------ run
res_rows <- list(); imp_rows <- list()
for (shape_nm in names(shapes)) {
    Xf <- shapes[[shape_nm]]
    for (el in els) {
        y_all <- suppressWarnings(as.numeric(cal$Values[[el]][
            match(as.character(Xf$Spectrum), as.character(cal$Values$Spectrum))]))
        keep <- is.finite(y_all)
        if (sum(keep) < 15) next
        X <- as.data.frame(Xf[keep, !colnames(Xf) %in% "Spectrum", drop = FALSE])
        X <- X[, vapply(X, function(cc) sd(cc, na.rm = TRUE) > 0, logical(1)), drop = FALSE]
        y <- y_all[keep]
        set.seed(SEED)
        folds <- createMultiFolds(y, k = 5, times = 1)

        preds <- list(); imps <- list()
        for (mn in names(MODELS)) {
            mm <- MODELS[[mn]]
            fit <- tryCatch(suppressWarnings(fit_one(mm, X, y, folds)), error = function(e) e)
            if (inherits(fit, "error")) {
                message(sprintf("  %s/%s %s: FIT ERROR %s", shape_nm, el, mn, conditionMessage(fit)))
                next
            }
            preds[[mn]] <- oof_pred(fit, length(y))
            if (mm$imp) imps[[mn]] <- tryCatch({
                vi <- varImp(fit, scale = FALSE)$importance
                setNames(vi[, 1], rownames(vi))
            }, error = function(e) NULL)
        }
        if (!length(preds)) next

        r2 <- vapply(preds, function(p){ ok <- is.finite(p)
            if (sum(ok) < 5 || sd(p[ok]) == 0) return(NA_real_)
            cor(p[ok], y[ok])^2 }, numeric(1))
        resid <- lapply(preds, function(p) p - y)

        incumbents <- intersect(c("rf", "svm", "xgb", "bart"), names(resid))
        for (mn in names(MODELS)) {
            if (!mn %in% names(resid)) next
            rc <- vapply(incumbents, function(inc){
                if (identical(inc, mn)) return(NA_real_)
                a <- resid[[mn]]; b <- resid[[inc]]; ok <- is.finite(a) & is.finite(b)
                if (sum(ok) < 5) return(NA_real_)
                suppressWarnings(cor(a[ok], b[ok])) }, numeric(1))
            res_rows[[length(res_rows) + 1]] <- data.frame(
                shape = shape_nm, element = el, model = mn, cand = MODELS[[mn]]$cand,
                R2 = r2[[mn]], t(rc), row.names = NULL, check.names = FALSE)
        }
        if (!is.null(imps$rf)) for (mn in setdiff(names(imps), "rf")) {
            common <- intersect(names(imps[[mn]]), names(imps$rf))
            if (length(common) >= 5)
                imp_rows[[length(imp_rows) + 1]] <- data.frame(
                    shape = shape_nm, element = el, model = mn,
                    imp_spearman_vs_rf = suppressWarnings(
                        cor(imps[[mn]][common], imps$rf[common], method = "spearman")))
        }
        message(sprintf("  %s / %-12s done (%s)", shape_nm, el,
                        paste(sprintf("%s=%.2f", names(r2), r2), collapse = " ")))
    }
}

## ------------------------------------------------------------------ report
res <- do.call(rbind, res_rows)
imp <- if (length(imp_rows)) do.call(rbind, imp_rows) else NULL
inc_cols <- intersect(c("rf", "svm", "xgb", "bart"), colnames(res))

message("\n== Mean over elements: OOF R2 | residual correlation with incumbents | non-redundancy ==")
for (shape_nm in unique(res$shape)) {
    message("\n-- ", shape_nm, " --")
    message(sprintf("%-8s %-5s %6s  %s  %8s %8s", "model", "cand", "R2",
                    paste(sprintf("r(%s)", inc_cols), collapse = "  "), "maxr", "nonred"))
    for (mn in names(MODELS)) {
        sub <- res[res$shape == shape_nm & res$model == mn, , drop = FALSE]
        if (!nrow(sub)) next
        mr <- vapply(inc_cols, function(cc) mean(sub[[cc]], na.rm = TRUE), numeric(1))
        # redundancy vs the OTHER incumbents for incumbent models; vs all for candidates
        maxr <- suppressWarnings(max(mr, na.rm = TRUE))
        message(sprintf("%-8s %-5s %6.2f  %s  %8.2f %8.2f", mn, MODELS[[mn]]$cand,
                        mean(sub$R2, na.rm = TRUE),
                        paste(sprintf("  %6.2f", mr), collapse = "  "),
                        maxr, 1 - maxr))
    }
}
if (!is.null(imp)) {
    message("\n== Feature-importance rank agreement with rf (Spearman, mean over elements) ==")
    for (shape_nm in unique(imp$shape)) {
        sub <- aggregate(imp_spearman_vs_rf ~ model, data = imp[imp$shape == shape_nm, ], FUN = mean)
        message("-- ", shape_nm, ": ",
                paste(sprintf("%s=%.2f", sub$model, sub$imp_spearman_vs_rf), collapse = "  "))
    }
}
out_csv <- file.path("tests", "redundancy_benchmark_results.csv")
write.csv(res, out_csv, row.names = FALSE)
message("\nper-element results written to ", out_csv)
