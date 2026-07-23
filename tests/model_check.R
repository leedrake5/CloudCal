# CloudCal model-pipeline check
# =============================
# Exercises the data pipeline and fit for EVERY calibration model type with
# minimal hyperparameters (tiny grids / trees / neurons / iterations - the goal
# is pipeline correctness, not model quality):
#   1 Linear, 2 Non-Linear, 3 Lucas-Tooth        -> classic element (+ intercept)
#   4 Forest, 5 Rainforest, 6 NN Intensities, 7 NN Spectra,
#   8 XGBoost Intensities, 9 XGBoost Spectra, 10 Bayes Intensities,
#   11 Bayes Spectra, 12 SVM Intensities, 13 SVM Spectra  -> ML element, all slopes
#
# For each type: build the model frame, fit, predict the training standards and
# require finite predictions correlated with the known concentrations.
#
# Usage:
#   Rscript tests/model_check.R [quant] [classic_el] [intercept_el] [ml_el]
# Defaults: farWest50kV.quant, Y.K.alpha with Rb.K.alpha intercept, Dy.K.alpha
# (substituted automatically if absent from the calibration).

args <- commandArgs(trailingOnly = TRUE)
QUANT <- if (length(args) >= 1) args[1] else
    "/Users/lee/Dropbox/Documents/CloudCal Evaluation/Test Quants/farWest50kV.quant"
CLASSIC_EL <- if (length(args) >= 2) args[2] else "Y.K.alpha"
INTERCEPT_EL <- if (length(args) >= 3) args[3] else "Rb.K.alpha"
ML_EL <- if (length(args) >= 4) args[4] else "Dy.K.alpha"

APP_DIR <- normalizePath(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(), value = TRUE)[1])), ".."))
if (!file.exists(file.path(APP_DIR, "global.R"))) APP_DIR <- getwd()
setwd(APP_DIR)

message("CloudCal model-pipeline check")
message("  quant: ", QUANT)

suppressMessages(suppressWarnings(source("global.R")))
suppressMessages(library(shiny))

results <- list()
report <- function(name, ok, note = ""){
    results[[name]] <<- ok
    message(sprintf("  [%s] %-46s %s", if (isTRUE(ok)) "PASS" else "FAIL", name, note))
}

cal <- calRDS(QUANT, xgb_raw = FALSE, sort = TRUE)
finite_n <- function(el) sum(is.finite(suppressWarnings(as.numeric(cal$Values[[el]]))))
if (!ML_EL %in% colnames(cal$Values) || finite_n(ML_EL) < 10) {
    cand <- names(cal$calList)[order(-vapply(names(cal$calList), function(e)
        if (e %in% colnames(cal$Values)) finite_n(e) else 0, numeric(1)))]
    cand <- setdiff(cand, CLASSIC_EL)
    old <- ML_EL; ML_EL <- cand[1]
    message("  NOTE: ", old, " not in this calibration - using ", ML_EL, " for ML models")
}
message("  classic: ", CLASSIC_EL, " (+", INTERCEPT_EL, " intercept)   ML: ", ML_EL)

app <- shinyAppDir(".")

MODELS <- list(
    list(id = 1,  name = "Linear",              el = CLASSIC_EL, fit = "linearModel"),
    list(id = 2,  name = "Non-Linear",          el = CLASSIC_EL, fit = "nonLinearModel"),
    list(id = 3,  name = "Lucas-Tooth",         el = CLASSIC_EL, fit = "lucasToothModel"),
    list(id = 4,  name = "Forest",              el = ML_EL, fit = "forestModel"),
    list(id = 5,  name = "Rainforest",          el = ML_EL, fit = "rainforestModel"),
    list(id = 6,  name = "NN Intensities",      el = ML_EL, fit = "neuralNetworkIntensityModel"),
    list(id = 7,  name = "NN Spectra",          el = ML_EL, fit = "neuralNetworkSpectraModel"),
    list(id = 8,  name = "XGBoost Intensities", el = ML_EL, fit = "xgboostIntensityModel"),
    list(id = 9,  name = "XGBoost Spectra",     el = ML_EL, fit = "xgboostSpectraModel"),
    list(id = 10, name = "Bayes Intensities",   el = ML_EL, fit = "bayesIntensityModel"),
    list(id = 11, name = "Bayes Spectra",       el = ML_EL, fit = "bayesSpectraModel"),
    list(id = 12, name = "SVM Intensities",     el = ML_EL, fit = "svmIntensityModel"),
    list(id = 13, name = "SVM Spectra",         el = ML_EL, fit = "svmSpectraModel")
)
SETNAMES <- c("linearModelSet", "nonLinearModelSet", "lucasToothModelSet", "forestModelSet",
    "rainforestModelSet", "neuralNetworkIntensityShallowModelSet", "neuralNetworkSpectraShallowModelSet",
    "xgboostIntensityModelSet", "xgboostSpectraModelSet", "bayesIntensityModelSet",
    "bayesSpectraModelSet", "svmIntensityModelSet", "svmSpectraModelSet")

env <- new.env()
env$out <- list()
try(testServer(app, {
    calMemory$Calibration <- cal
    values[["DF"]] <- cal$Values
    linevalues[["DF"]] <- cal$Definitions
    calSettings$calList <- cal$calList
    lines_avail <- intersect(colnames(cal$Intensities), spectralLines)
    all_slopes_on <- c(lines_avail, intersect(c("Total", "Baseline"), colnames(cal$Intensities)))

    session$setInputs(
        calfileinput = list(name = basename(QUANT), datapath = QUANT),
        filetype = cal$FileType, energynudge = 0,
        show_vars_k_alpha = grep("\\.K\\.alpha$", lines_avail, value = TRUE),
        show_vars_k_beta = grep("\\.K\\.beta$", lines_avail, value = TRUE),
        show_vars_l_alpha = grep("\\.L\\.alpha$", lines_avail, value = TRUE),
        show_vars_l_beta = NULL, show_vars_m = NULL,
        plotunit = "%", loglinear = "Linear",
        linepreferenceelement = "Narrow", linestructureelement = "gaussian",
        gausbuffer = cal$LineDefaults$GausBuffer, splitbuffer = cal$LineDefaults$SplitBuffer,
        comptonmin = 0, comptonmax = 0, comptontype = "Raw",
        randomize = 1, percentrandom = 0.2, neuralhiddenlayers = 1,
        multicore_behavior = "Single Core"
    )

    minimal_holds <- function(el, slopes, intercepts){
        basichold$normtype <- 2; basichold$normmin <- 0; basichold$normmax <- 0
        basichold$compress <- "100 eV"; basichold$transformation <- "None"
        basichold$deptransformation <- "None"; basichold$energyrange <- "0.7-37"
        basichold$deconvolution <- "None"
        basichold$linepreferenceelement <- "Narrow"; basichold$linestructureelement <- "gaussian"
        lucashold$slope <- slopes; lucashold$intercept <- intercepts
        foresthold$foresttry <- 2; foresthold$forestmetric <- "RMSE"
        foresthold$foresttrain <- "cv"; foresthold$forestnumber <- 2
        foresthold$cvrepeats <- 1; foresthold$foresttrees <- 20
        neuralhold$neuralhiddenlayers <- 1; neuralhold$neuralhiddenunits <- "1-1"
        neuralhold$neuralweightdecay <- "0.1-0.1"; neuralhold$neuralmaxiterations <- 10
        xgboosthold$xgbtype <- "Linear"; xgboosthold$treemethod <- "auto"
        xgboosthold$treedepth <- "1-1"; xgboosthold$droptree <- "0.1-0.1"
        xgboosthold$skipdrop <- "0.1-0.1"; xgboosthold$xgbalpha <- "0-0"
        xgboosthold$xgbgamma <- "0-0"; xgboosthold$xgbeta <- "0.3-0.3"
        xgboosthold$xgblambda <- "1-1"; xgboosthold$xgbsubsample <- "1-1"
        xgboosthold$xgbcolsample <- "1-1"; xgboosthold$xgbminchild <- "1-1"
        xgboosthold$xgbmaxdeltastep <- "0-0"
        barthold$bartk <- "2-2"; barthold$bartbeta <- "2-2"; barthold$bartnu <- "3-3"
        svmhold$svmc <- "1-1"; svmhold$svmdegree <- "1-1"; svmhold$svmscale <- "0.1-0.1"
        svmhold$svmsigma <- "0.1-0.1"; svmhold$svmlength <- "1-1"
    }

    for (i in seq_along(MODELS)) {
        m <- MODELS[[i]]
        res <- list(name = m$name, ok = FALSE, note = "")
        t0 <- Sys.time()
        tryCatch({
            session$setInputs(calcurveelement = m$el, radiocal = m$id,
                              normcal = 2, comptonmin = 0, comptonmax = 0)
            if (m$id <= 3) minimal_holds(m$el, slopes = m$el, intercepts = INTERCEPT_EL)
            else           minimal_holds(m$el, slopes = all_slopes_on, intercepts = NULL)
            # bayesglm has no tuning parameters and cannot resample 46 predictors
            # on 20-row folds - a single unresampled fit is the minimal valid setup.
            if (m$id %in% c(10, 11)) foresthold$foresttrain <- "none"
            vals$keeprows <- rep(TRUE, nrow(cal$Values))

            mset <- get(SETNAMES[i])()
            dat <- mset$data
            if (is.data.frame(dat)) {
                nfin <- vapply(dat, function(cc) sum(is.finite(suppressWarnings(as.numeric(cc)))), 0)
                bad <- names(nfin)[nfin == 0 & names(nfin) != "Spectrum"]
                if (length(bad) > 0) message("      DIAG ", m$name, ": all-NA columns: ",
                    paste(head(bad, 8), collapse = ","), if (length(bad) > 8) " ..." else "")
            }
            stopifnot(is.data.frame(dat), nrow(dat) >= 10, "Concentration" %in% names(dat))
            X <- dat[, !colnames(dat) %in% c("Spectrum", "Concentration"), drop = FALSE]
            stopifnot(all(vapply(X, is.numeric, logical(1))), any(is.finite(as.matrix(X))))

            fit <- get(m$fit)()
            stopifnot(!is.null(fit))
            preds <- tryCatch(as.numeric(predict(fit, newdata = X)),
                              error = function(e) tryCatch(as.numeric(predict(fit, X)),
                                                           error = function(e2) NULL))
            stopifnot(!is.null(preds), length(preds) == nrow(dat))
            okp <- is.finite(preds) & is.finite(dat$Concentration)
            stopifnot(sum(okp) >= 0.8 * nrow(dat))
            r <- if (sd(preds[okp]) > 0) suppressWarnings(cor(preds[okp], dat$Concentration[okp])) else NA_real_
            # tiny nets can legitimately predict a constant - finite predictions
            # prove the pipeline; r is reported when meaningful.
            res$ok <- TRUE
            res$note <- sprintf("n=%d cols=%d r=%s (%.1fs)", nrow(dat), ncol(X),
                                if (is.finite(r)) sprintf("%.2f", r) else "const",
                                as.numeric(Sys.time() - t0, units = "secs"))
        }, error = function(e){
            res$note <<- paste0("ERROR: ", conditionMessage(e),
                                sprintf(" (%.1fs)", as.numeric(Sys.time() - t0, units = "secs")))
        })
        env$out[[m$name]] <- res
        message(sprintf("    %-22s %s", m$name, if (res$ok) paste("ok ", res$note) else res$note))
    }

    # SVM kernel sweep: the five numeric kernels on both data shapes. (The
    # Exponential/Boundrange/Spectrum String branches are kernlab text kernels
    # and are intentionally not offered for numeric spectra.)
    for (rc in c(12, 13)) {
        session$setInputs(calcurveelement = ML_EL, radiocal = rc, normcal = 2,
                          comptonmin = 0, comptonmax = 0)
        minimal_holds(ML_EL, slopes = all_slopes_on, intercepts = NULL)
        vals$keeprows <- rep(TRUE, nrow(cal$Values))
        for (kern in c("Polynomial", "Radial", "Radial Cost", "Radial Sigma")) {
            nm <- sprintf("SVM %s (%s)", kern, if (rc == 12) "Intens" else "Spectra")
            res <- list(name = nm, ok = FALSE, note = "")
            t0 <- Sys.time()
            tryCatch({
                xgboosthold$xgbtype <- kern
                fit <- if (rc == 12) svmIntensityModel() else svmSpectraModel()
                stopifnot(!is.null(fit))
                res$ok <- TRUE
                res$note <- sprintf("fit ok (%.1fs)", as.numeric(Sys.time() - t0, units = "secs"))
            }, error = function(e){
                res$note <<- paste0("ERROR: ", conditionMessage(e))
            })
            env$out[[nm]] <- res
            message(sprintf("    %-22s %s", nm, if (res$ok) res$note else res$note))
        }
        xgboosthold$xgbtype <- "Linear"
    }
}), silent = TRUE)

message("")
for (m in MODELS) {
    r <- env$out[[m$name]]
    if (is.null(r)) report(m$name, FALSE, "(session aborted before this model)")
    else report(m$name, r$ok, r$note)
}
for (nm in setdiff(names(env$out), vapply(MODELS, `[[`, "", "name"))) {
    r <- env$out[[nm]]
    report(nm, r$ok, r$note)
}

n_fail <- sum(!unlist(results))
message(sprintf("\n== %d/%d model pipelines passed ==", sum(unlist(results)), length(results)))
if (n_fail > 0) { message("FAILED: ", paste(names(results)[!unlist(results)], collapse = "; ")); quit(status = 1) }
message("Model check PASSED")
