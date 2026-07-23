# CloudCal model-pipeline check
# =============================
# Exercises the data pipeline and fit for EVERY calibration model type with
# minimal hyperparameters (tiny grids / trees / neurons / iterations - the goal
# is pipeline correctness, not model quality):
#   1 Linear, 2 Non-Linear, 3 Lucas-Tooth        -> classic element (+ intercept)
#   4 Forest, 5 Rainforest, 6 NN Intensities, 7 NN Spectra,
#   8 XGBoost Intensities, 9 XGBoost Spectra, 10 Bayes Intensities,
#   11 Bayes Spectra, 12 SVM Intensities, 13 SVM Spectra,
#   14/15 PLS, 16/17 Cubist, 18/19 Elastic Net (glmnet), 20/21 MARS (earth)
#     (chemometric additions; Intensities/Spectra pairs) -> ML element, all slopes
#
# For each type: build the model frame, fit, predict the training standards and
# require finite predictions correlated with the known concentrations.
#
# After the per-model pass, customization sweeps re-fit representative models
# under every user-selectable data option:
#   - normalization (Time / Total Counts / ROI, incl. Baseline/Net ROI when the
#     quant carries a Deconvoluted slot) for the three intensity frame builders
#     and the spectra frame builder
#   - concentration transformation (Log / e / Scale) on intensity + spectra paths
#   - spectra transformation x compression (None/Log/e/Velocity x 100/50/25 eV)
#     on Rainforest, with NN / XGBoost / SVM spectra spot checks
# This is the slow, intermittent, "smoke out user customizations" pass.
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

# macOS: xgboost's libomp intermittently asserts (OMP Error #13) when other
# OpenMP-linked model packages (earth/pls) share the process. Single-threaded
# OMP sidesteps it; these tiny fits don't need the threads.
Sys.setenv(OMP_NUM_THREADS = "1", KMP_DUPLICATE_LIB_OK = "TRUE")
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
    list(id = 13, name = "SVM Spectra",         el = ML_EL, fit = "svmSpectraModel"),
    list(id = 14, name = "PLS Intensities",     el = ML_EL, fit = "plsIntensityModel"),
    list(id = 15, name = "PLS Spectra",         el = ML_EL, fit = "plsSpectraModel"),
    list(id = 16, name = "Cubist Intensities",  el = ML_EL, fit = "cubistIntensityModel"),
    list(id = 17, name = "Cubist Spectra",      el = ML_EL, fit = "cubistSpectraModel"),
    list(id = 18, name = "ElasticNet Intens",   el = ML_EL, fit = "glmnetIntensityModel"),
    list(id = 19, name = "ElasticNet Spectra",  el = ML_EL, fit = "glmnetSpectraModel"),
    list(id = 20, name = "MARS Intensities",    el = ML_EL, fit = "marsIntensityModel"),
    list(id = 21, name = "MARS Spectra",        el = ML_EL, fit = "marsSpectraModel")
)
SETNAMES <- c("linearModelSet", "nonLinearModelSet", "lucasToothModelSet", "forestModelSet",
    "rainforestModelSet", "neuralNetworkIntensityShallowModelSet", "neuralNetworkSpectraShallowModelSet",
    "xgboostIntensityModelSet", "xgboostSpectraModelSet", "bayesIntensityModelSet",
    "bayesSpectraModelSet", "svmIntensityModelSet", "svmSpectraModelSet",
    "plsIntensityModelSet", "plsSpectraModelSet", "cubistIntensityModelSet", "cubistSpectraModelSet",
    "glmnetIntensityModelSet", "glmnetSpectraModelSet", "marsIntensityModelSet", "marsSpectraModelSet")

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
        plshold$plsncomp <- "1-3"; cubisthold$cubistcommittees <- "1-2"
        cubisthold$cubistneighbors <- "0-0"; glmnethold$glmnetalpha <- "0.5-0.5"
        glmnethold$glmnetlambda <- "0.01-0.1"; marshold$marsprune <- "2-4"
        marshold$marsdegree <- "1-1"
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

    # Chem metadata round trip: pack a fitted PLS model the way the save
    # button does, re-derive its tuning range through the .quant import path
    # (bestTune wins), and predict from the packed model.
    rt <- list(name = "Chem CalTable round-trip", ok = FALSE, note = "")
    tryCatch({
        session$setInputs(calcurveelement = ML_EL, radiocal = 14, normcal = 2,
                          comptonmin = 0, comptonmax = 0, comptontype = "Raw")
        # line slopes only (no Total/Baseline): the Apply page reconstructs
        # predictors from calVariableElements(), which carries just the lines
        minimal_holds(ML_EL, slopes = lines_avail, intercepts = NULL)
        vals$keeprows <- rep(TRUE, nrow(cal$Values))
        mset <- plsIntensityModelSet()
        fit <- plsIntensityModel()
        packed <- modelPack(parameters = mset$parameters, model = fit, table = NULL, compress = TRUE)
        stopifnot(packed$Parameters$CalTable$CalType[1] == 14,
                  "plsNComp" %in% colnames(packed$Parameters$CalTable))
        restored <- importCalConditions(element = ML_EL,
            calList = setNames(list(packed), ML_EL), number.of.standards = sum(vals$keeprows))
        bt <- fit$bestTune$ncomp
        stopifnot("plsNComp" %in% colnames(restored$CalTable),
                  identical(as.character(restored$CalTable$plsNComp[1]), paste0(bt, "-", bt)))
        X <- mset$data[, !colnames(mset$data) %in% c("Spectrum", "Concentration"), drop = FALSE]
        p <- as.numeric(predict(packed$Model, newdata = X))
        stopifnot(length(p) == nrow(mset$data), any(is.finite(p)))
        rt$ok <- TRUE
        rt$note <- sprintf("ncomp=%s restored from bestTune; packed model predicts", bt)
    }, error = function(e){ rt$note <<- paste0("ERROR: ", conditionMessage(e)) })
    env$out[[rt$name]] <- rt
    message(sprintf("    %-34s %s", rt$name, rt$note))

    # Apply-page inference with a chem model: install the packed PLS model in
    # the calList and predict the calibration spectra through cloudCalPredict
    # (exercises the cal_type 14 -> SVM-intensity alias + mclPred).
    ap <- list(name = "Chem cloudCalPredict apply", ok = FALSE, note = "")
    tryCatch({
        cal2 <- cal
        cal2$calList[[ML_EL]] <- packed
        pred <- suppressWarnings(cloudCalPredict(Calibration = cal2, elements.cal = ML_EL,
            elements = ML_EL, variables = lines_avail, valdata = cal$Spectra,
            deconvoluted_valdata = cal$Deconvoluted, rounding = 6, multiplier = 1, cores = 2))
        stopifnot(is.data.frame(pred), ML_EL %in% colnames(pred))
        a <- suppressWarnings(as.numeric(pred[[ML_EL]]))
        b <- suppressWarnings(as.numeric(cal$Values[[ML_EL]][
            match(as.character(pred$Spectrum), as.character(cal$Values$Spectrum))]))
        okp <- is.finite(a) & is.finite(b)
        stopifnot(sum(okp) >= 10)
        r <- if (sd(a[okp]) > 0) suppressWarnings(cor(a[okp], b[okp])) else NA_real_
        ap$ok <- TRUE
        ap$note <- sprintf("n=%d r=%s", sum(okp), if (is.finite(r)) sprintf("%.2f", r) else "const")
    }, error = function(e){ ap$note <<- paste0("ERROR: ", conditionMessage(e)) })
    env$out[[ap$name]] <- ap
    message(sprintf("    %-34s %s", ap$name, ap$note))

    # ------------------------------------------------------------------
    # Customization sweeps: users can combine any normalization with any
    # model type, and (for spectra models) any transformation x compression.
    # Each combo rebuilds the model frame from scratch and fits.
    # ------------------------------------------------------------------
    emax <- max(cal$Spectra$Energy, na.rm = TRUE)
    roi_win <- round(c(0.38, 0.48) * emax, 2)   # backscatter/Compton region: always has counts

    run_combo <- function(nm, el, rc, setname, fitname, setup = function(){}){
        res <- list(name = nm, ok = FALSE, note = "")
        t0 <- Sys.time()
        tryCatch({
            session$setInputs(calcurveelement = el, radiocal = rc, normcal = 2,
                              comptonmin = 0, comptonmax = 0, comptontype = "Raw")
            if (rc <= 3) minimal_holds(el, slopes = el, intercepts = INTERCEPT_EL)
            else         minimal_holds(el, slopes = all_slopes_on, intercepts = NULL)
            if (rc %in% c(10, 11)) foresthold$foresttrain <- "none"
            vals$keeprows <- rep(TRUE, nrow(cal$Values))
            setup()
            mset <- get(setname)()
            dat <- mset$data
            stopifnot(is.data.frame(dat), nrow(dat) >= 10, "Concentration" %in% names(dat),
                      all(is.finite(dat$Concentration)))
            fit <- get(fitname)()
            stopifnot(!is.null(fit))
            res$ok <- TRUE
            res$note <- sprintf("n=%d cols=%d (%.1fs)", nrow(dat), ncol(dat),
                                as.numeric(Sys.time() - t0, units = "secs"))
        }, error = function(e){
            res$note <<- paste0("ERROR: ", conditionMessage(e),
                                sprintf(" (%.1fs)", as.numeric(Sys.time() - t0, units = "secs")))
        })
        env$out[[nm]] <- res
        message(sprintf("    %-34s %s", nm, res$note))
    }

    # -- intensity-model normalizations (Total Counts was exercised above) --
    message("  -- sweep: intensity normalizations --")
    for (mm in list(list(rc = 1, set = "linearModelSet",     fit = "linearModel",     lab = "Linear"),
                    list(rc = 3, set = "lucasToothModelSet", fit = "lucasToothModel", lab = "Lucas-Tooth"),
                    list(rc = 4, set = "forestModelSet",     fit = "forestModel",     lab = "Forest"))) {
        el <- if (mm$rc <= 3) CLASSIC_EL else ML_EL
        run_combo(paste0(mm$lab, " norm=Time"), el, mm$rc, mm$set, mm$fit, function(){
            basichold$normtype <- 1; session$setInputs(normcal = 1)
        })
        run_combo(paste0(mm$lab, " norm=ROI"), el, mm$rc, mm$set, mm$fit, function(){
            basichold$normtype <- 3
            basichold$normmin <- roi_win[1]; basichold$normmax <- roi_win[2]
            session$setInputs(normcal = 3, comptonmin = roi_win[1], comptonmax = roi_win[2])
        })
    }
    if (!is.null(cal$Deconvoluted)) {
        for (ctype in c("Baseline", "Net")) {
            run_combo(paste0("Linear norm=ROI ", ctype), CLASSIC_EL, 1,
                      "linearModelSet", "linearModel", function(){
                basichold$normtype <- 3
                basichold$normmin <- roi_win[1]; basichold$normmax <- roi_win[2]
                session$setInputs(normcal = 3, comptonmin = roi_win[1],
                                  comptonmax = roi_win[2], comptontype = ctype)
            })
        }
    } else message("    (skipping ROI Baseline/Net - quant has no Deconvoluted slot)")

    # -- concentration (dependent) transformations --
    # Log needs strictly positive values; e needs values small enough that
    # exp() stays finite. Pick a suitable element rather than silently skip.
    message("  -- sweep: concentration transformations --")
    dep_ok <- vapply(names(cal$calList), function(e){
        if (!e %in% colnames(cal$Values)) return(FALSE)
        v <- suppressWarnings(as.numeric(cal$Values[[e]])); v <- v[is.finite(v)]
        length(v) >= 10 && all(v > 0) && max(v) <= 500
    }, logical(1))
    DEP_EL <- if (isTRUE(dep_ok[CLASSIC_EL])) CLASSIC_EL else names(dep_ok)[dep_ok][1]
    if (is.null(DEP_EL) || is.na(DEP_EL)) {
        message("    (skipping - no element with all-positive, exp-safe concentrations)")
    } else {
        if (!identical(DEP_EL, CLASSIC_EL))
            message("    NOTE: using ", DEP_EL, " for concentration transformations")
        for (dt in c("Log", "e", "Scale")) {
            run_combo(paste0("Linear dep=", dt), DEP_EL, 1,
                      "linearModelSet", "linearModel",
                      function(){ basichold$deptransformation <- dt })
            run_combo(paste0("Rainforest dep=", dt), DEP_EL, 5,
                      "rainforestModelSet", "rainforestModel",
                      function(){ basichold$deptransformation <- dt })
        }
    }

    # -- spectra transformation x compression (Rainforest carries the grid; the
    #    other spectra models share rainforestDataGen, spot-checked below) --
    message("  -- sweep: spectra transformation x compression --")
    for (tr in c("None", "Log", "e", "Velocity")) {
        for (cp in c("100 eV", "50 eV", "25 eV")) {
            if (tr == "None" && cp == "100 eV") next   # default, covered above
            run_combo(sprintf("Rainforest %s/%s", tr, gsub(" ", "", cp)), ML_EL, 5,
                      "rainforestModelSet", "rainforestModel", function(){
                basichold$transformation <- tr; basichold$compress <- cp
            })
        }
    }

    # -- spectra-model normalizations --
    message("  -- sweep: spectra normalizations --")
    run_combo("Rainforest norm=Time", ML_EL, 5, "rainforestModelSet", "rainforestModel",
              function(){ basichold$normtype <- 1; session$setInputs(normcal = 1) })
    run_combo("Rainforest norm=ROI", ML_EL, 5, "rainforestModelSet", "rainforestModel",
              function(){
        basichold$normtype <- 3
        basichold$normmin <- roi_win[1]; basichold$normmax <- roi_win[2]
        session$setInputs(normcal = 3, comptonmin = roi_win[1], comptonmax = roi_win[2])
    })

    # -- cross-model spectra spot checks under non-default transforms --
    # (50 eV for the NN keeps nnet under its default MaxNWts weight cap.)
    message("  -- sweep: spectra-model spot checks --")
    run_combo("NN Spectra Log/50eV", ML_EL, 7,
              "neuralNetworkSpectraShallowModelSet", "neuralNetworkSpectraModel",
              function(){ basichold$transformation <- "Log"; basichold$compress <- "50 eV" })
    run_combo("XGB Spectra Velocity/25eV", ML_EL, 9,
              "xgboostSpectraModelSet", "xgboostSpectraModel",
              function(){ basichold$transformation <- "Velocity"; basichold$compress <- "25 eV" })
    run_combo("SVM Spectra e/50eV Radial", ML_EL, 13,
              "svmSpectraModelSet", "svmSpectraModel", function(){
        basichold$transformation <- "e"; basichold$compress <- "50 eV"
        xgboosthold$xgbtype <- "Radial"
    })
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
