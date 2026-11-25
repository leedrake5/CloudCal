# CloudCal pre-release check
# Smoke and regression test to run before releases:
# 1. Load a handheld .quant and verify its structure and repaired tables, including SpectraMetadata re-keyed from the upload index that older PDZ imports used
# 2. Verify the intensity-table builders (fast path == per-element path)
# 3. Drive a session: review cal curves per element and commit line edits, including an out-of-range definition, without the session failing
# 4. Build a new calibration from the raw spectra, fit a model, and re-fit it under each normalization and a scaled-concentration transform (the full grid is in tests/model_check.R)
# 5. Predict the calibration's own standards and compare to stored values
# 6. Standardless FP estimate sanity (raw estimate + closure)
# 7. Example files: every recognized import format reads and carries metadata (live time, detector, eV/ch where present), and the MCA and SEM-EDS (Esprit .txt) spectra deconvolute with physics defaults inferred from the file
# 8. High-energy K/L+M series split: a 200 kV / 450 um SDD spectrum keeps its Pb K-lines, El.K / El.L line names read their own series, and prediction re-runs E1 when training did
# 9. Spectra saved as R objects (.RData / .rda / .rds) import in every supported shape
# 10. Cross Validation: the Leave-One-Out plots, standards table and both downloads render for a Lucas-Tooth element in both forms, Use Cross-Validated Model saves the averaged refits, and the Random Split standards table holds exactly the held-out standards
# 11. Diagnostics: clicks, brushes and Cook's distance bars toggle the standard under the pointer for Linear and Lucas-Tooth, with and without standards lacking a certified value, and excluded standards stay on the plots so they can be included again
# 12. Cal Curves selection and saved standards: a click toggles one standard, the left-plot brush works, the Lucas-Tooth label shows the plotted r2, an element saved with no intercept lines predicts as in the app after reload, a PLS random split trains only on included standards, in-session masks follow standards across model types, and older .quant masks load the standards their models left out
# 13. Apply Calibration: older empty-intercept Lucas-Tooth models reproduce their fits, the Scale transform is decoded with the stored range, each element's saved Narrow Line Width is used, saves record the build and deconvolution algorithm, and the stale-model and spectra-alignment checks work
# 14. Olympus Delta CSV import: multi-exposure detection, per-exposure CPS and metadata, and the same file through a session
# 15. E1 from a loaded calibration: with no spectra imported, Deconvolute in Handheld mode runs E1 from the .quant's stored LiveTime and offers it as the ROI Type; Legacy mode and a blank kV fit without E1 instead of failing
# 16. Scatter-angle inference: the batch median over the brightest spectra recovers the Tracer's ~110-115 deg geometry even when the single brightest spectrum is declined, and the deconvolution stores it instead of the 135 deg default
# 17. Split line windows: L-alpha, L-beta, M and two-line K-beta sum their own line windows (not the whole spectrum, the pre-fix bug), K-alpha and one-line K-beta are unchanged, a split table saved before the fix is rebuilt on load, and models fitted on it are named for refitting until Update stamps them
# 18. XGBoost across xgboost and caret releases: Bayesian tuning reads xgb.cv's best iteration under xgboost 1.x and 3.x, rawModel is saved as UBJSON, calRDS rebuilds boosters this xgboost can't use from rawModel and names the rest, tuning grids match the installed caret, caret can train XGBoost (fails while caret is older than 7.1 with xgboost 3.x), XGBoost Linear is a real linear booster, the XGBoost Type control shows the type that trains, and the per-element Preprocess setting defaults to None and survives a reload
# Usage: Rscript tests/release_check.R [path/to/cal.quant] [path/to/spectra_dir] [fp_quant] [examples_dir] [high_energy_mca]
# Default paths point at local CloudCal Evaluation files; the exit status is nonzero if any check fails.

args <- commandArgs(trailingOnly = TRUE)
QUANT <- if (length(args) >= 1) args[1] else
    "/Users/lee/Dropbox/Documents/CloudCal Evaluation/Test Quants/farWest50kV.quant"
SPECTRA_DIR <- if (length(args) >= 2) args[2] else
    "/Users/lee/Dropbox/Documents/CloudCal Evaluation/Far West Obsidian/50 kv 30a Cu100 Ti25 Al300"
FP_QUANT <- if (length(args) >= 3) args[3] else
    "/Users/lee/Dropbox/Documents/CloudCal Evaluation/Test Quants/steel_template.quant"
EXAMPLES_DIR <- if (length(args) >= 4) args[4] else
    "/Users/lee/Dropbox/Documents/CloudCal Evaluation/Example Files"
HIGH_ENERGY_MCA <- if (length(args) >= 5) args[5] else
    "/Users/lee/Dropbox/Documents/CloudCal Evaluation/High Energy/AMIS0421.mca"

APP_DIR <- normalizePath(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(), value = TRUE)[1])), ".."))
if (!file.exists(file.path(APP_DIR, "global.R"))) APP_DIR <- getwd()
setwd(APP_DIR)

message("CloudCal release check")
message("  app:     ", APP_DIR)
message("  quant:   ", QUANT)
message("  spectra: ", SPECTRA_DIR)

suppressMessages(suppressWarnings(source("global.R")))
suppressMessages(library(shiny))

results <- list()
check <- function(name, expr){
    t0 <- Sys.time()
    ok <- tryCatch(isTRUE(expr), error = function(e){
        message("    ERROR in ", name, ": ", conditionMessage(e)); FALSE })
    dt <- round(as.numeric(Sys.time() - t0, units = "secs"), 1)
    results[[name]] <<- ok
    message(sprintf("  [%s] %-58s (%ss)", if (ok) "PASS" else "FAIL", name, dt))
    invisible(ok)
}

pdz_files <- list.files(SPECTRA_DIR, pattern = "\\.pdz$", ignore.case = TRUE, full.names = TRUE)
stopifnot(length(pdz_files) > 0)
inFile_all <- data.frame(name = basename(pdz_files), datapath = pdz_files, stringsAsFactors = FALSE)

message("\n-- 1. Calibration load & structure --")
t0 <- Sys.time()
cal <- calRDS(QUANT, xgb_raw = FALSE, sort = TRUE)
load_secs <- as.numeric(Sys.time() - t0, units = "secs")
elements_cal <- names(cal$calList)
val_elements <- names(cal$Values)[!names(cal$Values) %in% c("Include", "Spectrum")]

check("load completes in under 60 s", load_secs < 60)
check("core slots present", all(c("Spectra", "Intensities", "Values", "Definitions", "calList") %in% names(cal)))
check("all six intensity tables built", all(c("Intensities", "IntensitiesSplit", "IntensitiesFirst",
    "IntensitiesSecond", "WideIntensities", "WideIntensitiesSplit") %in% names(cal)))
check("intensity tables cover Values elements",
    all(val_elements %in% colnames(cal$Intensities)))
check("intensity rows match spectra", {
    sp <- sort(unique(as.character(cal$Spectra$Spectrum)))
    identical(sort(as.character(cal$Intensities$Spectrum)), sp)
})
check("every calList element has a model + CalTable", all(vapply(cal$calList, function(x)
    !is.null(x[[1]]$CalTable) && length(x) >= 2, logical(1))))
check("extensions strip only at the end of a name (\"_spectrum\" issue)",
    identical(spectrumNameVector(c("A_spectrum1", "Inspection_3", "B_spectrum2.spe", "c.pdz", "d.CSV")),
              c("A_spectrum1", "Inspection_3", "B_spectrum2", "c", "d")))
check("calRDS folds legacy mangled-name twins back onto their spectra", {
    # Recreate what legacy unanchored name stripping saved for a spectrum named "<s>_spectrum": its concentrations and intensities on a mangled twin "<s>ctrum", and the real-name row blank.
    leg <- cal; s <- as.character(leg$Values$Spectrum[1]); real <- paste0(s, "_spectrum"); twin <- paste0(s, "ctrum")
    leg$Spectra$Spectrum[leg$Spectra$Spectrum == s] <- real
    mangle <- function(df, blank){
        i <- which(df$Spectrum == s); tw <- df[i, , drop = FALSE]; tw$Spectrum <- twin
        num <- setdiff(names(df)[vapply(df, is.numeric, logical(1))], "Spectrum")
        df$Spectrum[i] <- real; df[i, num] <- blank
        rbind(df, tw)
    }
    leg$Values <- mangle(leg$Values, 0)
    leg$Intensities <- mangle(leg$Intensities, NA)
    fixed <- calRDS(Calibration = leg, xgb_raw = FALSE)
    el <- intersect(val_elements, colnames(cal$Intensities))[1]
    !twin %in% fixed$Values$Spectrum && !twin %in% fixed$Intensities$Spectrum &&
        isTRUE(all.equal(fixed$Values[[el]][fixed$Values$Spectrum == real], cal$Values[[el]][cal$Values$Spectrum == s])) &&
        isTRUE(all.equal(fixed$Intensities[[el]][fixed$Intensities$Spectrum == real], cal$Intensities[[el]][cal$Intensities$Spectrum == s]))
})
check("calRDS keys SpectraMetadata by spectrum name, re-keying older PDZ imports' upload-index rows", {
    # Older PDZ imports keyed rows "0", "1", ... (Shiny's upload index), so their LiveTime reached no spectrum. After loading, each spectrum must carry its own PDZ live time; rebuilding the index keys must round-trip, a wrong live-time order must fail the whole-count test and stay unchanged, and a second load must change nothing.
    sm <- cal$SpectraMetadata
    sp <- unique(as.character(cal$Spectra$Spectrum))
    pdz_lt <- deconvolution_livetime_lookup(readPDZMetadataProcess(inFile_all))
    old <- sm[match(sp, sm$Spectrum), , drop = FALSE]; old$Spectrum <- as.character(seq_along(sp) - 1L)
    bad <- old; bad$LiveTime <- rev(bad$LiveTime)
    all(sp %in% sm$Spectrum) && isTRUE(all.equal(unname(pdz_lt[sm$Spectrum]), sm$LiveTime)) &&
        identical(repairIndexedSpectraMetadata(old, cal$Spectra)$Spectrum, sp) &&
        identical(repairIndexedSpectraMetadata(bad, cal$Spectra)$Spectrum, bad$Spectrum) &&
        identical(calRDS(Calibration = cal, xgb_raw = FALSE)$SpectraMetadata, sm)
})

message("\n-- 2. Intensity table builders --")
check("fast builder == per-element builder (narrow gaussian)", {
    fastFn <- elementFrameFast
    f <- narrowLineTable(cal$Spectra, cal$Definitions, val_elements,
                         gaus_buffer = cal$LineDefaults$GausBuffer, allowParallel = FALSE)
    assign("elementFrameFast", function(...) NULL, envir = globalenv())
    s <- narrowLineTable(cal$Spectra, cal$Definitions, val_elements,
                         gaus_buffer = cal$LineDefaults$GausBuffer, allowParallel = FALSE)
    assign("elementFrameFast", fastFn, envir = globalenv())
    f2 <- f[order(f$Spectrum), sort(names(f))]; s2 <- s[order(s$Spectrum), sort(names(s))]
    rownames(f2) <- rownames(s2) <- NULL
    isTRUE(all.equal(f2, s2, check.attributes = FALSE, tolerance = 1e-8))
})
check("totalCountsGen matches aggregate", {
    o <- aggregate(CPS ~ Spectrum, data = cal$Spectra[, c("Spectrum", "CPS")], FUN = sum)
    colnames(o) <- c("Spectrum", "Total")
    isTRUE(all.equal(o, totalCountsGen(cal$Spectra), check.attributes = FALSE))
})
check("out-of-range definitions drop instead of crash", {
    defs <- data.frame(Name = c("Good", "Bad"), EnergyMin = c(10, 500), EnergyMax = c(11, 505),
                       stringsAsFactors = FALSE)
    p <- xrf_parse(range.table = defs, data = cal$Spectra)
    !is.null(p) && identical(attr(p, "dropped_lines"), "Bad") && "Good" %in% colnames(p)
})

message("\n-- 3. Session: cal review + line-table edits --")
app <- shinyAppDir(".")
sec3 <- new.env()
try(testServer(app, {
    calMemory$Calibration <- cal
    values[["DF"]] <- cal$Values
    linevalues[["DF"]] <- cal$Definitions
    calSettings$calList <- cal$calList
    lines_avail <- intersect(colnames(cal$Intensities), spectralLines)
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
        randomize = 1, percentrandom = 0.2
    )
    els <- head(names(cal$calList), 3)
    plot_ok <- label_ok <- val_ok <- logical(0)
    for (el in els) {
        ct <- tryCatch(cal$calList[[el]][[1]]$CalTable, error = function(e) NULL)
        session$setInputs(calcurveelement = el,
                          radiocal = if (is.null(ct)) 1 else ct$CalType[1],
                          normcal = if (is.null(ct)) 2 else ct$NormType[1],
                          comptonmin = if (is.null(ct)) 0 else ct$Min[1],
                          comptonmax = if (is.null(ct)) 0 else ct$Max[1])
        p <- tryCatch(calCurvePlotPre(), error = function(e) NULL)
        v <- tryCatch(valCurvePlotPre(), error = function(e) NULL)
        plot_ok <- c(plot_ok, !is.null(p) && !is.null(v))
        nm <- tryCatch({
            xs <- ""
            for (s in p$scales$scales) if ("x" %in% s$aesthetics && is.character(s$name)) xs <- s$name
            startsWith(xs, gsub("[.]", "", substr(el, 1, 2)))
        }, error = function(e) FALSE)
        label_ok <- c(label_ok, isTRUE(nm))
        vf <- tryCatch(valFrame(), error = function(e) NULL)
        val_ok <- c(val_ok, !is.null(vf) && nrow(vf) > 0 && any(is.finite(vf$Prediction)))
    }
    sec3$plots <- all(plot_ok); sec3$labels <- all(label_ok); sec3$vals <- all(val_ok)

    # cross-validation chain for the first element
    session$setInputs(calcurveelement = els[1])
    sec3$crossval <- !is.null(tryCatch(valCurvePlotRandom(), error = function(e) NULL))

    # Commit a line edit that includes an out-of-range definition; the session must survive it.
    defs <- cal$Definitions
    empty_row <- which(!nzchar(as.character(defs$Name)))[1]
    if (is.na(empty_row)) empty_row <- nrow(defs) + 1
    defs[empty_row, ] <- list("ReleaseCheckBadLine", 400, 405)
    linevalues[["DF"]] <- defs
    session$setInputs(linecommit = 1)
    sec3$commit <- is.data.frame(values[["DF"]]) && nrow(values[["DF"]]) > 0
}), silent = TRUE)
check("cal/val plots build for first 3 elements", isTRUE(sec3$plots))
check("axis labels match selected elements", isTRUE(sec3$labels))
check("validation frames contain finite predictions", isTRUE(sec3$vals))
check("cross-validation plot builds", isTRUE(sec3$crossval))
check("commit with out-of-range definition survives", isTRUE(sec3$commit))

message("\n-- 4. Session: new calibration from scratch --")
scratch_lines <- head(intersect(names(cal$calList), spectralLines), 4)
sec4 <- new.env()
try(testServer(app, {
    session$setInputs(
        filetype = "PDZ", energynudge = 0,
        file1 = inFile_all, pdzprep = TRUE,
        show_vars_k_alpha = scratch_lines,
        show_vars_k_beta = NULL, show_vars_l_alpha = NULL,
        show_vars_l_beta = NULL, show_vars_m = NULL,
        plotunit = "%", loglinear = "Linear",
        linepreferenceelement = "Narrow", linestructureelement = "gaussian",
        gausbuffer = 0.02, splitbuffer = 0.1,
        comptonmin = 0, comptonmax = 0, comptontype = "Raw",
        randomize = 1
    )
    dh <- tryCatch(dataHold(), error = function(e) NULL)
    sec4$spectra_loaded <- !is.null(dh) && nrow(dh) > 0
    session$setInputs(linecommit = 1)
    it <- calMemory$Calibration$Intensities
    sec4$intensities_built <- is.data.frame(it) && all(scratch_lines %in% colnames(it))
    if (isTRUE(sec4$intensities_built)) {
        # inject the known concentrations for the standards, then fit
        conc <- cal$Values
        it_specs <- as.character(it$Spectrum)
        m <- match(it_specs, as.character(conc$Spectrum))
        DF <- data.frame(Include = TRUE, Spectrum = it_specs, stringsAsFactors = FALSE)
        for (el in scratch_lines) DF[[el]] <- if (el %in% names(conc)) conc[[el]][m] else NA_real_
        values[["DF"]] <- DF
        session$elapse(800)   # let the concentrations debounce settle
        el <- scratch_lines[which.max(vapply(scratch_lines, function(e) sum(is.finite(DF[[e]])), 0))]
        session$setInputs(calcurveelement = el, radiocal = 1, normcal = 2)
        fit <- tryCatch(linearModel(), error = function(e) NULL)
        r2 <- tryCatch(summary(fit)$r.squared, error = function(e) NA_real_)
        sec4$model_fit <- is.finite(r2)
        sec4$model_sane <- is.finite(r2) && r2 > 0.3
        sec4$element <- el; sec4$r2 <- r2

        # Customization smoke test on the scratch model: each normalization plus a scaled-concentration fit (the full option grid is in tests/model_check.R).
        emax <- max(dh$Energy, na.rm = TRUE)
        roi_win <- round(c(0.38, 0.48) * emax, 2)   # backscatter region: always has counts
        fit_ok <- function() !is.null(tryCatch(linearModel(), error = function(e) NULL))
        basichold$normtype <- 1; session$setInputs(normcal = 1)
        sec4$norm_time <- fit_ok()
        basichold$normtype <- 3
        basichold$normmin <- roi_win[1]; basichold$normmax <- roi_win[2]
        session$setInputs(normcal = 3, comptonmin = roi_win[1], comptonmax = roi_win[2])
        sec4$norm_roi <- fit_ok()
        basichold$normtype <- 2
        session$setInputs(normcal = 2, comptonmin = 0, comptonmax = 0)
        basichold$deptransformation <- "Scale"
        sec4$dep_scale <- fit_ok()
        basichold$deptransformation <- "None"

        # "Train slopes" normalization sweep: the lowest AIC of Time, Total Counts and the best Compton window wins, and the reported window is the one the sweep chose.
        sec4$norm_sweep <- isTRUE(tryCatch({
            nl <- planktonVector()
            cc <- concentrationTable(); cc <- cc[complete.cases(cc[, el]), ]
            slt <- spectraLineTable(); slt <- slt[slt$Spectrum %in% holdFrame()$Spectrum, ]
            slt <- slt[complete.cases(cc[, el]), ]
            dn <- dataNorm(); dn <- dn[dn$Spectrum %in% cc$Spectrum, ]
            win <- optimal_norm_chain_xrf(dn, el, slt, cc, nl$Min, nl$Max)
            aics <- c(extractAIC(lm(cc[, el] ~ general_prep_xrf(slt, el)$Intensity, na.action = na.exclude), k = log(1))[2],
                      extractAIC(lm(cc[, el] ~ simple_tc_prep_xrf(dn, slt, el)$Intensity, na.action = na.exclude), k = log(1))[2],
                      attr(win, "aic"))
            bn <- bestNormVars()
            bn$Type == which.min(aics) && identical(bn$Compton, c(nl$Min[win], nl$Max[win]))
        }, error = function(e) FALSE))
    }
}), silent = TRUE)
if (is.null(sec4$element)) sec4$element <- "?"
if (is.null(sec4$r2)) sec4$r2 <- NA_real_
check("raw spectra load into a fresh session", isTRUE(sec4$spectra_loaded))
check("commit builds intensity table from scratch", isTRUE(sec4$intensities_built))
check("linear model fits on injected concentrations", isTRUE(sec4$model_fit))
check(sprintf("scratch model is sane (%s r2=%s)", sec4$element, signif(sec4$r2, 3)),
    isTRUE(sec4$model_sane))
check("scratch linear fits with Time normalization", isTRUE(sec4$norm_time))
check("scratch linear fits with ROI normalization", isTRUE(sec4$norm_roi))
check("scratch linear fits with scaled concentrations", isTRUE(sec4$dep_scale))
check("Train-slopes sweep picks the lowest-AIC normalization + its window", isTRUE(sec4$norm_sweep))

message("\n-- 5. Self-prediction against stored values --")
valdata <- readPDZProcess(inFile = inFile_all, gainshiftvalue = 0, advanced = FALSE,
                          pdzprep = TRUE, use_native_calibration = TRUE)
spec_chr <- as.character(valdata$Spectrum); u <- unique(spec_chr)
valdata$Spectrum <- make.names(u, unique = FALSE)[match(spec_chr, u)]
variables <- names(cal$Intensities)[!names(cal$Intensities) %in% c("Spectrum", "Total", "Baseline")]

pred <- tryCatch(suppressWarnings(cloudCalPredict(
    Calibration = cal, elements.cal = elements_cal, elements = elements_cal,
    variables = variables, valdata = valdata,
    deconvoluted_valdata = cal$Deconvoluted, rounding = 8, multiplier = 1)),
    error = function(e){ message("    cloudCalPredict ERROR: ", conditionMessage(e)); NULL })

check("cloudCalPredict returns a prediction table",
    is.data.frame(pred) && nrow(pred) > 0 && "Spectrum" %in% names(pred))

if (is.data.frame(pred)) {
    m <- match(as.character(pred$Spectrum), as.character(cal$Values$Spectrum))
    cors <- vapply(elements_cal, function(el){
        if (!el %in% names(pred) || !el %in% names(cal$Values)) return(NA_real_)
        a <- suppressWarnings(as.numeric(pred[[el]]))
        b <- suppressWarnings(as.numeric(cal$Values[[el]][m]))
        ok <- is.finite(a) & is.finite(b)
        if (sum(ok) < 5) return(NA_real_)
        suppressWarnings(cor(a[ok], b[ok]))
    }, numeric(1))
    cors <- cors[is.finite(cors)]
    message("    per-element self-prediction r: ",
        paste(names(cors), signif(cors, 2), collapse = "  "))
    check("self-prediction correlates for most elements (median r > 0.9)",
        length(cors) >= 5 && median(cors) > 0.9)
} else {
    check("self-prediction correlates for most elements (median r > 0.9)", FALSE)
}

message("\n-- 6. Standardless FP estimates (steel reference) --")
if (file.exists(FP_QUANT)) {
    steel <- calRDS(FP_QUANT, xgb_raw = FALSE, sort = TRUE)
    steel_lt <- tryCatch(deconvolution_livetime_lookup(steel$SpectraMetadata), error = function(e) NULL)
    # Full-FP mode (self-absorption, secondary fluorescence and abundance regularization) with instrument physics from the stored metadata, where holding the total at 1 gives mass fractions comparable to the certified standards; in relative mode phantom elements absorb most of the closed mass.
    steel_phys <- physicsFromValMetadata(steel$SpectraMetadata)
    steel_dec <- spectra_gls_deconvolute(steel$Spectra, cores = as.numeric(my.cores),
                                         mass = "full", livetime = steel_lt, physics = steel_phys)
    steel_closed <- fpMassClosure(steel_dec$Mass)

    check("FP mass estimates exist and closure sums to 1", {
        tots <- rowSums(as.matrix(steel_closed[, !colnames(steel_closed) %in% "Spectrum"]), na.rm = TRUE)
        is.data.frame(steel_dec$Mass) && all(abs(tots - 1) < 1e-8)
    })

    # Closed FP percentages vs certified values (steel Values are in percent).
    sv <- steel$Values[match(as.character(steel_closed$Spectrum), as.character(steel$Values$Spectrum)), ]
    el_lines <- setdiff(colnames(steel$Values), c("Include", "Spectrum", "C"))
    fp_cors <- c(); fp_mae <- c()
    for (ln in el_lines) {
        sym <- strsplit(ln, "[.]")[[1]][1]
        if (!sym %in% colnames(steel_closed)) next
        cert <- suppressWarnings(as.numeric(sv[[ln]])) / 100
        fp <- steel_closed[[sym]]
        ok <- is.finite(cert) & is.finite(fp)
        if (sum(ok) >= 8 && sd(cert[ok]) > 0) {
            fp_cors[ln] <- suppressWarnings(cor(cert[ok], fp[ok]))
            fp_mae[ln] <- mean(abs(fp[ok] - cert[ok])) * 100
        }
    }
    message("    closed FP vs certified r:   ",
        paste(names(fp_cors), signif(fp_cors, 2), collapse = "  "))
    message("    closed FP MAE (wt%):        ",
        paste(names(fp_mae), signif(fp_mae, 2), collapse = "  "))

    certFe <- suppressWarnings(as.numeric(sv$Fe.K.alpha)) / 100
    okFe <- is.finite(certFe) & is.finite(steel_closed$Fe)
    fe_med_err <- abs(median(steel_closed$Fe[okFe]) - median(certFe[okFe])) * 100
    fe_mae <- mean(abs(steel_closed$Fe[okFe] - certFe[okFe])) * 100
    message(sprintf("    Fe (matrix element): median closed %.2f%% vs certified %.2f%%, MAE %.1f wt%%",
        median(steel_closed$Fe[okFe]) * 100, median(certFe[okFe]) * 100, fe_mae))
    check("closed FP Fe %% matches certified (median within 5 wt%%, MAE < 8 wt%%)",
        sum(okFe) >= 8 && fe_med_err < 5 && fe_mae < 8)
    check("closed FP tracks certified composition (median r > 0.7)",
        length(fp_cors) >= 5 && is.finite(median(fp_cors, na.rm = TRUE)) &&
        median(fp_cors, na.rm = TRUE) > 0.7)
} else {
    message("    FP quant not found (", FP_QUANT, ") - falling back to closure sanity on validation data")
    check("FP mass estimates exist and closure sums to 1", {
        dec <- spectra_gls_deconvolute(valdata[valdata$Spectrum %in% unique(valdata$Spectrum)[1:4], ],
                                       cores = 2, mass = TRUE)
        closed <- fpMassClosure(dec$Mass)
        tots <- rowSums(as.matrix(closed[, !colnames(closed) %in% "Spectrum"]), na.rm = TRUE)
        is.data.frame(dec$Mass) && all(abs(tots - 1) < 1e-8)
    })
}

message("\n-- 7. Example-file zoo (import formats + inferred deconvolution defaults) --")
if (dir.exists(EXAMPLES_DIR)) {
    zoo <- list.files(EXAMPLES_DIR, full.names = TRUE)
    ext <- tolower(tools::file_ext(zoo))
    is_pmca_noext <- vapply(zoo[ext == ""], function(f)
        isTRUE(grepl("PMCA SPECTRUM", tryCatch(readLines(f, n = 1, warn = FALSE), error = function(e) ""))), logical(1))
    read_ok <- function(sp) is.data.frame(sp) && nrow(sp) > 0 &&
        all(c("Energy", "CPS", "Spectrum") %in% names(sp)) && any(is.finite(sp$CPS))

    pdzs <- zoo[ext == "pdz"]
    check(sprintf("all %d example PDZs read", length(pdzs)), {
        inF <- data.frame(name = basename(pdzs), datapath = pdzs, stringsAsFactors = FALSE)
        sp <- tryCatch(readPDZProcess(inFile = inF, gainshiftvalue = 0, advanced = FALSE,
                                      pdzprep = TRUE, use_native_calibration = TRUE), error = function(e) NULL)
        # Dual-beam PDZs yield one spectrum per beam (_1/_2), so require at least one spectrum per file rather than exactly one.
        read_ok(sp) && length(unique(sp$Spectrum)) >= length(pdzs)
    })

    # per-spectrum handheld CSV exports (skip the big aggregate niton/vanta files)
    csvs <- zoo[ext == "csv" & !grepl("niton|vanta", basename(zoo), ignore.case = TRUE)]
    check(sprintf("all %d per-spectrum CSVs read + metadata", length(csvs)), {
        all(vapply(csvs, function(f){
            sp <- tryCatch(csvFrame(filepath = f, filename = basename(f)), error = function(e) NULL)
            md <- tryCatch(csvFrameMetadata(filepath = f, filename = basename(f)), error = function(e) NULL)
            read_ok(sp) && is.data.frame(md)
        }, logical(1)))
    })

    aggs <- zoo[grepl("niton|vanta", basename(zoo), ignore.case = TRUE) & ext == "csv"]
    check(sprintf("aggregate CSVs (Niton/Vanta: %d) read", length(aggs)), {
        all(vapply(aggs, function(f){
            beams <- tryCatch(get_instrument_and_beams(f), error = function(e) NULL)
            if (is.null(beams)) return(FALSE)
            sp <- tryCatch(importCSVFrame(f, chosen_beam = beams$beams[1]), error = function(e) NULL)
            read_ok(sp)
        }, logical(1)))
    })

    mcas <- c(zoo[ext == "mca"], names(is_pmca_noext)[is_pmca_noext])
    zoo_decon <- list()
    check(sprintf("MCA/PMCA files (%d) read + metadata live time", length(mcas)), {
        all(vapply(mcas, function(f){
            sp <- tryCatch(readMCAData(filepath = f, filename = basename(f)), error = function(e) NULL)
            md <- tryCatch(mcaFrameMetadata(f, basename(f)), error = function(e) NULL)
            if (read_ok(sp)) zoo_decon[[basename(f)]] <<- list(sp = sp, md = md)
            # LiveTime must be positive: LIVE_TIME when valid, otherwise REAL_TIME when the export logged 0 (100% dead time).
            read_ok(sp) && is.data.frame(md) && is.finite(md$LiveTime[1]) && md$LiveTime[1] > 0
        }, logical(1)))
    })

    txts <- zoo[ext == "txt"]
    check(sprintf("TXT files (%d) read + metadata", length(txts)), {
        all(vapply(txts, function(f){
            sp <- tryCatch(readTXTData(filepath = f, filename = basename(f)), error = function(e) NULL)
            md <- tryCatch(txtFrameMetadata(f, basename(f)), error = function(e) NULL)
            if (!read_ok(sp) || !is.data.frame(md)) return(FALSE)
            # Esprit (SEM-EDS) exports need a live time, metadata that joins their spectrum by name, and electron physics inferred from it; they are deconvoluted with the MCA spectra below.
            if (isTRUE(md$Excitation[1] == "electron")) {
                zoo_decon[[basename(f)]] <<- list(sp = sp, md = md)
                return(is.finite(md$LiveTime[1]) && md$LiveTime[1] > 0 &&
                       identical(md$Spectrum[1], unique(sp$Spectrum)) &&
                       identical(deconvolution_infer_from_metadata(md)$mode, "sem"))
            }
            TRUE
        }, logical(1)))
    })

    check("MCA/TXT spectra deconvolute with file-inferred physics defaults", {
        length(zoo_decon) > 0 && all(vapply(zoo_decon, function(z){
            inf <- tryCatch(deconvolution_infer_from_metadata(z$md), error = function(e) NULL)
            phys <- tryCatch(instrument_deconv_defaults(
                mode = if (!is.null(inf$mode)) inf$mode else "legacy",
                kv = inf$kv, detector_type = inf$detector,
                environment = if (!is.null(inf$environment)) inf$environment else "air_pp"),
                error = function(e) list())
            lt <- tryCatch(deconvolution_livetime_lookup(z$md), error = function(e) NULL)
            dec <- tryCatch(spectra_gls_deconvolute(z$sp, cores = 1, physics = phys,
                                                    mass = "off", livetime = lt),
                            error = function(e) NULL)
            is.list(dec) && "Spectra" %in% names(dec)
        }, logical(1)))
    })

    leftover <- setdiff(zoo, c(pdzs, csvs, aggs, mcas, txts))
    if (length(leftover)) message("    (not exercised: ",
        paste(basename(leftover), collapse = ", "), ")")
} else {
    message("    (examples dir not found: ", EXAMPLES_DIR, " - skipping zoo)")
}

message("\n-- 8. High-energy K / L+M series split --")
check("split defaults: on for handheld / high-energy, absent for SEM and legacy", {
    identical(names(instrument_deconv_defaults("legacy", kv = 50, anode = "Rh")), ".mode") &&
        isTRUE(instrument_deconv_defaults("handheld", kv = 50, anode = "Rh")$split_series) &&
        isTRUE(instrument_deconv_defaults("high_energy", kv = 200, anode = "W")$split_series) &&
        is.null(instrument_deconv_defaults("sem", kv = 15)$split_series) &&
        "split_series" %in% names(formals(deconvolute_complete))
})
check("selector: off drops the key (old builds still load), legacy keeps its fit, restore rules", {
    hh <- instrument_deconv_defaults("handheld", kv = 50, anode = "Rh")
    off <- deconvolution_apply_split_series(hh, "off", "handheld")
    leg <- deconvolution_apply_split_series(instrument_deconv_defaults("legacy"), "on", "legacy")
    s <- deconvolution_split_selection
    is.null(off$split_series) && identical(off$.split_series, "off") &&
        is.null(leg$split_series) && identical(leg$.split_series, "on") &&
        isTRUE(deconvolution_apply_split_series(hh, NULL, "handheld")$split_series) &&
        s(list(.split_series = "auto"), TRUE) == "auto" && s(list(split_series = TRUE), TRUE) == "on" &&
        s(list(.mode = "handheld"), TRUE) == "off" && s(list(), FALSE) == "auto"
})
check("Areas keep <El>_L right after its element; scatter kept, background dropped", {
    a <- intensity_frame_deconvolution_convert(data.frame(
             element = c("scatter_compton", "Pb_L", "Fe", "Pb", "background_1", "scatter_rayleigh"),
             peak_area = c(10, 3, 1, 2, 99, 20), stringsAsFactors = FALSE), name = "s1")
    identical(names(a), c("Spectrum", "Fe", "Pb", "Pb_L", "Compton", "Rayleigh")) && a$Pb_L == 3
})
check("line mapping: K <- El, L/M <- El_L (unsplit row -> El), missing element -> NA", {
    ar <- data.frame(Spectrum = c("a", "b"), Fe = c(1, 2), Pb = c(5, 6), Pb_L = c(7, NA), Baseline = 9)
    it <- data.frame(Spectrum = c("a", "b"), Fe.K.alpha = 0, Fe.L.alpha = 0, Pb.K.alpha = 0, Pb.L.alpha = 0,
                     Pb.M.line = 0, Zr.K.alpha = 0, Total = 3)
    m <- deconvolutionIntensityFrame(ar, it)
    all(m$Fe.L.alpha == c(1, 2)) && all(m$Pb.K.alpha == c(5, 6)) && all(m$Pb.L.alpha == c(7, 6)) &&
        all(m$Pb.M.line == c(7, 6)) && all(is.na(m$Zr.K.alpha)) && all(m$Total == 3)
})
check("unsplit stored Areas map exactly as before (bare area on every line name)", {
    ar <- cal$Deconvoluted$Areas
    if (is.null(ar)) TRUE else {
        m <- deconvolutionIntensityFrame(ar, cal$Intensities); ln <- intersect(names(m), spectralLines)
        length(ln) > 0 && all(vapply(ln, function(l) isTRUE(all.equal(m[[l]],
            ar[[line_strip(l)]][match(m$Spectrum, ar$Spectrum)])), logical(1)))
    }
})
check("FP mass frame ignores <El>_L columns", {
    mf <- deconvolution_mass_frame(data.frame(Spectrum = "a", Fe = 100, Pb = 10, Pb_L = 5),
                                   physics = list(detector_type = "SDD", beam_energy_kev = 50))
    is.data.frame(mf) && !("Pb_L" %in% names(mf))
})
if (file.exists(HIGH_ENERGY_MCA)) {
    # AMIS0421 at 200 kV on a 450 um SDD: the PMCA export has no calibration block, so the energy axis is channel * 24.89 eV. Its Pb K-lines are barely visible to the detector (0.66% efficiency) but must still be fitted rather than absorbed by the E1 background.
    he_sp <- readMCAData(filepath = HIGH_ENERGY_MCA, filename = basename(HIGH_ENERGY_MCA))
    he_sp$Energy <- he_sp$Energy * 0.02489
    he_lt <- deconvolution_livetime_lookup(mcaFrameMetadata(HIGH_ENERGY_MCA, basename(HIGH_ENERGY_MCA)))
    he_phys <- instrument_deconv_defaults(mode = "high_energy", kv = 200, anode = "W", detector_type = "SDD",
                                          active_thickness_um = 450, filter = "Sn 2400")
    he_dec <- tryCatch(spectra_gls_deconvolute(he_sp, cores = 1, physics = he_phys, mass = "relative", livetime = he_lt),
                       error = function(e){ message("    ERROR: ", conditionMessage(e)); NULL })
    he_net <- function(col) if (!is.null(he_dec) && col %in% names(he_dec$Areas))
        he_dec$Areas[[col]][1] * he_lt[[1]] / 0.02489 else NA_real_   # area (cps*keV) -> net counts
    he_win <- function(df) sum(df$CPS[abs(df$Energy - 74.97) <= 0.1])
    message(sprintf("    Pb K %.0f, Pb L+M %.0f, Bi K %.0f net counts; fit/data at Pb K-alpha1 %.2f",
                    he_net("Pb"), he_net("Pb_L"), he_net("Bi"),
                    if (is.null(he_dec)) NA_real_ else he_win(he_dec$Spectra) / he_win(he_sp)))
    check("AMIS0421: E1 fired; Pb/Bi split into K + L templates", {
        !is.null(he_dec$E1) && all(c("Pb", "Pb_L", "Bi", "Bi_L") %in% names(he_dec$Areas)) &&
            isTRUE(he_dec$Parameters$Physics$split_series)
    })
    check("AMIS0421: Pb K-alpha1 modelled (fit/data 0.8-1.3 at 74.97 keV; was ~0.17)", {
        r <- he_win(he_dec$Spectra) / he_win(he_sp); is.finite(r) && r > 0.8 && r < 1.3
    })
    check("AMIS0421: Pb K > 50k net counts, Bi/Pb 0.05-0.2 (certified 0.107)", {
        is.finite(he_net("Pb")) && he_net("Pb") > 5e4 &&
            is.finite(he_net("Bi")) && he_net("Bi") / he_net("Pb") > 0.05 && he_net("Bi") / he_net("Pb") < 0.2
    })
    check("AMIS0421: Pb.K.alpha <- Pb, Pb.L.alpha / Pb.M.line <- Pb_L; $Mass has Pb, not Pb_L", {
        ifr <- data.frame(Spectrum = he_dec$Areas$Spectrum, Pb.K.alpha = 0, Pb.L.alpha = 0, Pb.M.line = 0,
                          Fe.K.alpha = 0, stringsAsFactors = FALSE)
        di <- deconvolutionIntensityFrame(he_dec$Areas, ifr)
        isTRUE(all.equal(di$Pb.K.alpha, he_dec$Areas$Pb)) && isTRUE(all.equal(di$Pb.L.alpha, he_dec$Areas$Pb_L)) &&
            isTRUE(all.equal(di$Pb.M.line, he_dec$Areas$Pb_L)) && isTRUE(all.equal(di$Fe.K.alpha, he_dec$Areas$Fe)) &&
            "Pb" %in% names(he_dec$Mass) && !("Pb_L" %in% names(he_dec$Mass))
    })
    check("AMIS0421: split off keeps the tied single template (no _L columns)", {
        off <- spectra_gls_deconvolute(he_sp, cores = 1, mass = "off", livetime = he_lt,
                   physics = deconvolution_apply_split_series(he_phys, "off", "high_energy"))
        !any(grepl("_L$", names(off$Areas))) && is.null(off$Parameters$Physics$split_series) &&
            identical(off$Parameters$Physics$.split_series, "off")
    })
    check("prediction live time follows training: NULL without E1, own / nominal with E1", {
        md <- data.frame(Spectrum = "AMIS0421.mca", LiveTime = 100, stringsAsFactors = FALSE)
        is.null(deconvolution_prediction_livetime(list(E1 = NULL), md)) &&
            identical(unname(deconvolution_prediction_livetime(he_dec, md)), 100) &&
            isTRUE(attr(deconvolution_prediction_livetime(he_dec, NULL, md), "nominal"))
    })
} else {
    message("    (high-energy MCA not found: ", HIGH_ENERGY_MCA, " - skipping)")
}

message("\n-- 9. Spectra saved as R objects (.RData / .rda / .rds) --")
rd_dir <- file.path(tempdir(), "cloudcal_rdata_check"); dir.create(rd_dir, showWarnings = FALSE)
rd_ids <- unique(cal$Spectra$Spectrum)[1:2]
rd_long <- cal$Spectra[cal$Spectra$Spectrum %in% rd_ids, c("Spectrum", "Energy", "CPS")]
rd_key <- function(d) { d <- d[order(d$Spectrum, d$Energy), c("Energy", "CPS", "Spectrum")]; rownames(d) <- NULL; d }
rd_ref <- rd_key(transform(rd_long, Spectrum = make.names(Spectrum)))
rd_wide <- reshape2::dcast(rd_long, Energy ~ Spectrum, value.var = "CPS")
rd_meta <- data.frame(Spectrum = rd_ids, LiveTime = c(30, 31), stringsAsFactors = FALSE)
rd_xrf <- tibble::tibble(.path = rd_ids, .spectra = lapply(rd_ids, function(i)
    tibble::tibble(energy_kev = rd_long$Energy[rd_long$Spectrum == i], cps = rd_long$CPS[rd_long$Spectrum == i])))
saveRDS(rd_long, file.path(rd_dir, "long.rds"))
save(rd_wide, file = file.path(rd_dir, "wide.RData"))
saveRDS(rd_xrf, file.path(rd_dir, "xrftools.rds"))
saveRDS(list(Spectra = rd_long, SpectraMetadata = rd_meta), file.path(rd_dir, "bundle.rda"))
saveRDS(rd_long[rd_long$Spectrum == rd_ids[1], c("Energy", "CPS")], file.path(rd_dir, "one spectrum.rds"))
writeLines("not R data", file.path(rd_dir, "garbage.rdata"))
rd_read <- function(f) readRDataSpectra(file.path(rd_dir, f), f)
check("RData import: long (saveRDS), wide (save), xrftools tibble and $Spectra bundle all round-trip", {
    all(vapply(c("long.rds", "wide.RData", "xrftools.rds", "bundle.rda"), function(f)
        isTRUE(all.equal(rd_key(rd_read(f)), rd_ref, check.attributes = FALSE)), logical(1)))
})
check("RData import: unlabelled spectrum named after its file; junk files fail cleanly", {
    identical(unique(rd_read("one spectrum.rds")$Spectrum), "one.spectrum") &&
        inherits(tryCatch(rd_read("garbage.rdata"), error = function(e) e), "error")
})
check("RData import: batch skips bad files, de-duplicates repeated ids, carries LiveTime metadata", {
    files <- c("long.rds", "bundle.rda", "garbage.rdata")
    d <- suppressWarnings(readRDataProcess(data.frame(name = files, datapath = file.path(rd_dir, files),
                                                      stringsAsFactors = FALSE)))
    md <- rdataFrameMetadata(file.path(rd_dir, "bundle.rda"), "bundle.rda")
    ch <- readRDataProcess(data.frame(name = "long.rds", datapath = file.path(rd_dir, "long.rds"),
                                      stringsAsFactors = FALSE), use_native_calibration = FALSE)
    length(unique(d$Spectrum)) == 4 && nrow(d) == 2 * nrow(rd_long) &&
        identical(md$LiveTime, c(30, 31)) && identical(md$Spectrum, make.names(rd_ids)) &&
        identical(ch$Energy[ch$Spectrum == make.names(rd_ids[1])], as.numeric(seq_len(sum(rd_long$Spectrum == rd_ids[1]))))
})
# Same files through the app: the "RData" file type loads spectra + LiveTime and commits an intensity table.
sec9 <- new.env()
try(testServer(app, {
    files <- c("bundle.rda", "wide.RData")
    session$setInputs(
        filetype = "RData", energynudge = 0,
        file1 = data.frame(name = files, datapath = file.path(rd_dir, files), stringsAsFactors = FALSE),
        show_vars_k_alpha = scratch_lines,
        show_vars_k_beta = NULL, show_vars_l_alpha = NULL,
        show_vars_l_beta = NULL, show_vars_m = NULL,
        plotunit = "%", loglinear = "Linear",
        linepreferenceelement = "Narrow", linestructureelement = "gaussian",
        gausbuffer = 0.02, splitbuffer = 0.1,
        comptonmin = 0, comptonmax = 0, comptontype = "Raw",
        randomize = 1
    )
    dh <- tryCatch(dataHold(), error = function(e) NULL)
    sec9$n_spectra <- if (is.data.frame(dh)) length(unique(dh$Spectrum)) else 0
    md <- tryCatch(myMetaData(), error = function(e) NULL)
    sec9$livetime <- is.data.frame(md) && "LiveTime" %in% names(md)
    session$setInputs(linecommit = 1)
    it <- calMemory$Calibration$Intensities
    sec9$intensities <- is.data.frame(it) && nrow(it) == sec9$n_spectra && all(scratch_lines %in% colnames(it))
}), silent = TRUE)
check("RData file type in a session: 4 spectra load (repeated ids kept apart), LiveTime metadata, intensities commit",
      identical(sec9$n_spectra, 4L) && isTRUE(sec9$livetime) && isTRUE(sec9$intensities))
unlink(rd_dir, recursive = TRUE)

message("\n-- 10. Cross Validation > Leave-One-Out page --")
# The element and model type are set in the first input batch, before anything else, so every output observer runs and the page is read the way a browser renders it.
lt_els <- names(cal$calList)[vapply(cal$calList, function(x)
    isTRUE(as.numeric(tryCatch(x[[1]]$CalTable$CalType[1], error = function(e) NA)) == 3), logical(1))]
loo_el <- c(intersect("Y.K.alpha", lt_els), lt_els)[1]
sec10 <- new.env()
if (!is.na(loo_el)) try(testServer(app, {
    p <- cal$calList[[loo_el]][[1]]
    ct <- p$CalTable
    calMemory$Calibration <- cal
    values[["DF"]] <- cal$Values
    linevalues[["DF"]] <- cal$Definitions
    calSettings$calList <- cal$calList
    lines_avail <- intersect(colnames(cal$Intensities), spectralLines)
    session$setInputs(
        calcurveelement = loo_el, radiocal = 3, normcal = as.numeric(ct$NormType[1]),
        calfileinput = list(name = basename(QUANT), datapath = QUANT),
        filetype = cal$FileType, energynudge = 0,
        show_vars_k_alpha = grep("\\.K\\.alpha$", lines_avail, value = TRUE),
        show_vars_k_beta = grep("\\.K\\.beta$", lines_avail, value = TRUE),
        show_vars_l_alpha = grep("\\.L\\.alpha$", lines_avail, value = TRUE),
        show_vars_l_beta = NULL, show_vars_m = NULL,
        plotunit = "%", loglinear = "Linear",
        linepreferenceelement = "Narrow", linestructureelement = "gaussian",
        gausbuffer = cal$LineDefaults$GausBuffer, splitbuffer = cal$LineDefaults$SplitBuffer,
        comptonmin = as.numeric(ct$Min[1]), comptonmax = as.numeric(ct$Max[1]),
        comptontype = as.character(ct$ComptonType[1]), randomize = 1, percentrandom = 0.2,
        calname = "release_check", imagesizeloo = "Small", useloomodel = FALSE
    )
    basichold$normtype <- as.numeric(ct$NormType[1])
    basichold$normmin <- as.numeric(ct$Min[1]); basichold$normmax <- as.numeric(ct$Max[1])
    basichold$deptransformation <- "None"
    lucashold$slope <- p$Slope; lucashold$intercept <- p$Intercept
    lucashold$ltcross <- "Additive"; lucashold$ltweight <- "1961"
    session$flushReact()
    sec10$plots <- tryCatch(!is.null(output$calcurveplotsloo$src) && !is.null(output$valcurveplotsloo$src), error = function(e) FALSE)
    sec10$table <- tryCatch(!is.null(output$loostandards), error = function(e) FALSE)
    sec10$tiff <- tryCatch(file.size(output$downloadcloudplotloo), error = function(e) 0)
    sec10$csv <- tryCatch(read.csv(output$downloadLooStandards, check.names = FALSE), error = function(e) NULL)
    sec10$loo_add <- looCurrentFits()$Table
    sec10$n_std <- length(looStandardIds())
    sec10$random_table <- tryCatch(!is.null(output$randomstandards), error = function(e) FALSE)
    sec10$random_csv <- tryCatch(read.csv(output$downloadRandomStandards, check.names = FALSE), error = function(e) NULL)
    sec10$random_held <- tryCatch(valFrameRandomized(), error = function(e) NULL)
    sec10$random_train <- tryCatch(as.character(calCurveFrameRandomized()$Spectrum), error = function(e) NULL)
    lucashold$ltcross <- "Classic"
    session$flushReact()
    sec10$plots_cls <- tryCatch(!is.null(output$calcurveplotsloo$src) && !is.null(output$valcurveplotsloo$src), error = function(e) FALSE)
    sec10$loo_cls <- looCurrentFits()$Table
    # Update, unticked then ticked
    session$setInputs(createcalelement = 1)
    sec10$saved_full <- isTRUE(all.equal(unname(coef(calMemory$Calibration$calList[[loo_el]]$Model)), unname(coef(elementModelGen()))))
    sec10$stamped <- identical(calMemory$Calibration$calList[[loo_el]]$Parameters$LineAlgorithm, LINE_INTENSITY_ALGORITHM)
    session$setInputs(useloomodel = TRUE)
    session$setInputs(createcalelement = 2)
    saved <- calMemory$Calibration$calList[[loo_el]]
    sec10$saved_cv <- isTRUE(all.equal(coef(saved$Model), coef(looCVModel()))) && isTRUE(all.equal(saved$Table, looValTable())) &&
        !isTRUE(all.equal(coef(saved$Model), coef(elementModelGen())))
    session$setInputs(useloomodel = FALSE, radiocal = 4)
    sec10$other_type <- tryCatch({ output$calcurveplotsloo; "rendered" }, error = function(e) conditionMessage(e))
    # Random Split under the Scale transformation (which new XGBoost models start with) must show concentrations in the calibration's units, as Cal Curves does
    basichold$deptransformation <- "Scale"
    session$flushReact()
    sec10$scaled_held <- tryCatch(valFrameRandomized(), error = function(e) NULL)
    sec10$scaled_train <- tryCatch(valFrameRandomizedRev(), error = function(e) NULL)
    basichold$deptransformation <- "None"
}), silent = TRUE)
check("Leave-one-out page: refit-lines and validation plots, standards table and plot download render",
      isTRUE(sec10$plots) && isTRUE(sec10$table) && isTRUE(sec10$tiff > 10000))
check("Leave-one-out page: standards CSV has one row per standard used, holding the left-out estimates",
      is.data.frame(sec10$csv) && nrow(sec10$csv) == sec10$n_std &&
      all(c("Spectrum", "Concentration", "Prediction", "Difference", "Relative") %in% colnames(sec10$csv)) &&
      isTRUE(all.equal(sec10$csv$Prediction, sec10$loo_add$LOO)))
check("Random Split standards: one row per held-out standard, none of them in the training set",
      isTRUE(sec10$random_table) && is.data.frame(sec10$random_csv) && is.data.frame(sec10$random_held) &&
      nrow(sec10$random_csv) == nrow(sec10$random_held) && nrow(sec10$random_csv) > 0 &&
      all(c("Spectrum", "Concentration", "Prediction", "Difference", "Relative") %in% colnames(sec10$random_csv)) &&
      isTRUE(all.equal(sec10$random_csv$Prediction, sec10$random_held$Prediction)) &&
      length(intersect(sec10$random_csv$Spectrum, sec10$random_train)) == 0 &&
      length(union(sec10$random_csv$Spectrum, sec10$random_train)) == sec10$n_std)
check("Leave-one-out page: follows the cross-product toggle",
      isTRUE(sec10$plots_cls) && is.data.frame(sec10$loo_cls) && nrow(sec10$loo_cls) == sec10$n_std &&
      !isTRUE(all.equal(sec10$loo_cls$LOO, sec10$loo_add$LOO)))
check("Use Cross-Validated Model: Update saves the averaged refits with the left-out table; unticked, the full fit",
      isTRUE(sec10$saved_full) && isTRUE(sec10$saved_cv))
check("Update stamps the saved model with the current line-intensity version", isTRUE(sec10$stamped))
check("Random Split under a Scale transformation shows the calibration's concentrations, not the 0-1 scaled ones", {
    actual <- function(vf){ v <- suppressWarnings(as.numeric(cal$Values[[loo_el]])); names(v) <- as.character(cal$Values$Spectrum); unname(v[as.character(vf$Spectrum)]) }
    h <- sec10$scaled_held; t <- sec10$scaled_train
    is.data.frame(h) && is.data.frame(t) && nrow(h) > 0 && nrow(t) > 0 &&
        isTRUE(all.equal(unname(h$Concentration), actual(h), tolerance = 1e-6)) && isTRUE(all.equal(unname(t$Concentration), actual(t), tolerance = 1e-6)) &&
        all(is.finite(h$Prediction)) && max(abs(h$Prediction)) <= 2 * max(abs(actual(t)), na.rm = TRUE)
})
check("Leave-one-out page: other model types point to the Random Split tab",
      grepl("Random Split", sec10$other_type))

message("\n-- 11. Diagnostics: point-and-click standard selection --")
# Clicks and brushes as Shiny sends them for ggplot2 3.x plots: data coordinates with the aesthetic mapping.
click_at <- function(x, y, xv, yv) list(x = x, y = y, mapping = list(x = xv, y = yv),
    domain = list(left = -1e9, right = 1e9, bottom = -1e9, top = 1e9), coords_css = list(x = 10, y = 10))
brush_around <- function(x, y, dx, dy, xv, yv) list(xmin = x - dx, xmax = x + dx, ymin = y - dy, ymax = y + dy,
    mapping = list(x = xv, y = yv), domain = list(left = -1e9, right = 1e9, bottom = -1e9, top = 1e9))
# Linear fits on a shuffled frame and Lucas-Tooth on a sorted one, using a Lucas-Tooth element's stored lines; the second pass blanks three certified values, so the element has fewer standards than spectra.
diag_el <- c(loo_el, names(cal$calList))[!is.na(c(loo_el, names(cal$calList)))][1]
sec11 <- new.env()
for (pass in c("all", "missing")) try(testServer(app, {
    cal_vals <- cal$Values
    if (pass == "missing") cal_vals[[diag_el]][c(2, 5, 9)] <- NA
    p <- cal$calList[[diag_el]][[1]]
    ct <- p$CalTable
    cal_list <- cal$calList
    for (e in names(cal_list)) cal_list[[e]][[1]]$StandardsUsed <- NULL
    calMemory$Calibration <- cal
    calMemory$Calibration$Values <- cal_vals
    values[["DF"]] <- cal_vals
    linevalues[["DF"]] <- cal$Definitions
    calSettings$calList <- cal_list
    lines_avail <- intersect(colnames(cal$Intensities), spectralLines)
    session$setInputs(
        calcurveelement = diag_el, radiocal = 1, normcal = as.numeric(ct$NormType[1]),
        calfileinput = list(name = basename(QUANT), datapath = QUANT),
        filetype = cal$FileType, energynudge = 0,
        show_vars_k_alpha = grep("\\.K\\.alpha$", lines_avail, value = TRUE),
        show_vars_k_beta = grep("\\.K\\.beta$", lines_avail, value = TRUE),
        show_vars_l_alpha = grep("\\.L\\.alpha$", lines_avail, value = TRUE),
        show_vars_l_beta = NULL, show_vars_m = NULL,
        plotunit = "%", loglinear = "Linear",
        linepreferenceelement = "Narrow", linestructureelement = "gaussian",
        gausbuffer = cal$LineDefaults$GausBuffer, splitbuffer = cal$LineDefaults$SplitBuffer,
        comptonmin = as.numeric(ct$Min[1]), comptonmax = as.numeric(ct$Max[1]),
        comptontype = as.character(ct$ComptonType[1]), randomize = 1, percentrandom = 0.2,
        calname = "release_check", useloomodel = FALSE
    )
    basichold$normtype <- as.numeric(ct$NormType[1])
    basichold$normmin <- as.numeric(ct$Min[1]); basichold$normmax <- as.numeric(ct$Max[1])
    basichold$deptransformation <- "None"
    lucashold$slope <- p$Slope; lucashold$intercept <- p$Intercept
    lucashold$ltcross <- "Additive"; lucashold$ltweight <- "1961"
    session$flushReact()
    used <- function() { pf <- predictFrame(); as.character(pf$Spectrum[alignKeep(vals$keeprows, pf) %in% TRUE]) }
    for (rc in c(1, 3)) {
        key <- paste0(pass, rc)
        session$setInputs(radiocal = rc)
        session$setInputs(exclude_reset_diag = rc)
        mf <- modelFrame(); pf <- predictFrame()
        kept <- which(mf$Included & is.finite(mf$.fitted))
        pred <- suppressWarnings(predict(elementModelGen(), newdata = pf[match(mf$Spectrum[kept], pf$Spectrum), , drop = FALSE]))
        sec11[[paste0(key, "_labels")]] <- nrow(mf) == nrow(pf) && length(kept) == nrow(pf) &&
            max(abs(pred - mf$.fitted[kept])) < 1e-8 * max(1, abs(mf$.fitted[kept]))
        k <- which.max(abs(mf$.resid)); target <- mf$Spectrum[k]
        session$setInputs(plot_residualsfitted_click = click_at(mf$.fitted[k], mf$.resid[k], ".fitted", ".resid"))
        mf2 <- modelFrame(); r <- which(mf2$Spectrum == target)
        sec11[[paste0(key, "_click")]] <- setequal(setdiff(pf$Spectrum, used()), target) && length(r) == 1 && !mf2$Included[r] &&
            all(is.finite(unlist(mf2[r, c(".fitted", ".resid", ".std.resid", ".hat", ".cooksd", "qq")])))
        session$setInputs(plot_qq_click = click_at(mf2$qq[r], mf2$.std.resid[r], "qq", ".std.resid"))
        sec11[[paste0(key, "_reinclude")]] <- length(used()) == nrow(pf)
        mf3 <- modelFrame(); a <- which.max(mf3$.hat); b <- which.max(mf3$.cooksd * (seq_len(nrow(mf3)) != a))
        session$setInputs(plot_scalelocation_brush = brush_around(mf3$.fitted[a], mf3$sqrt.std.resid[a], 1e-9, 1e-9, ".fitted", "sqrt.std.resid"),
            plot_cooksdistance_brush = brush_around(mf3$seq.cooksd[b], mf3$.cooksd[b] / 2, 0.1, 1e-9, "seq.cooksd", ".cooksd"))
        session$setInputs(exclude_toggle_diag = rc)
        sec11[[paste0(key, "_brush")]] <- setequal(setdiff(pf$Spectrum, used()), mf3$Spectrum[c(a, b)])
        session$setInputs(plot_scalelocation_brush = NULL, plot_cooksdistance_brush = NULL, exclude_reset_diag = 10 + rc)
        mf4 <- modelFrame(); c4 <- which.max(mf4$.cooksd)
        session$setInputs(plot_cooksdistance_click = click_at(mf4$seq.cooksd[c4] + 0.2, mf4$.cooksd[c4] / 2, "seq.cooksd", ".cooksd"))
        sec11[[paste0(key, "_bar")]] <- setequal(setdiff(pf$Spectrum, used()), mf4$Spectrum[c4])
        session$setInputs(exclude_reset_diag = 20 + rc)
    }
}), silent = TRUE)
for (pass in c("all", "missing")) for (rc in c(1, 3)) {
    key <- paste0(pass, rc)
    what <- sprintf("%s, %s", c("1" = "Linear", "3" = "Lucas-Tooth")[[as.character(rc)]],
        if (pass == "all") "every standard certified" else "three certified values missing")
    check(sprintf("Diagnostics (%s): each point carries its own standard's name", what), isTRUE(sec11[[paste0(key, "_labels")]]))
    check(sprintf("Diagnostics (%s): a click excludes only that standard, which stays on the plots", what), isTRUE(sec11[[paste0(key, "_click")]]))
    check(sprintf("Diagnostics (%s): clicking an excluded point includes it again", what), isTRUE(sec11[[paste0(key, "_reinclude")]]))
    check(sprintf("Diagnostics (%s): Toggle points flips exactly the brushed standards", what), isTRUE(sec11[[paste0(key, "_brush")]]))
    check(sprintf("Diagnostics (%s): clicking a Cook's distance bar toggles its standard", what), isTRUE(sec11[[paste0(key, "_bar")]]))
}

message("\n-- 12. Cal Curves selection, saved standards and labels --")
plot_ranges <- function(p) { pp <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]; list(x = pp$x.range, y = pp$y.range) }
label_r2 <- function(p) {
    lab <- Filter(function(l) inherits(l$geom, "GeomText"), p$layers)[[1]]$aes_params$label
    as.numeric(sub('.*"=" ~ "([^"]+)".*', "\\1", lab))
}
sec12 <- new.env()
# One session on a calibration file; body runs after the element and model type are set, with used()/excluded() reading the standards the current model is fit on.
run_session <- function(quant_path, el, setup_type, body) {
    cal_in <- calRDS(quant_path, xgb_raw = FALSE, sort = TRUE)
    try(testServer(app, {
        p <- cal_in$calList[[el]][[1]]
        ct <- p$CalTable
        calMemory$Calibration <- cal_in
        values[["DF"]] <- cal_in$Values
        linevalues[["DF"]] <- cal_in$Definitions
        calSettings$calList <- cal_in$calList
        lines_avail <- intersect(colnames(cal_in$Intensities), spectralLines)
        session$setInputs(
            calcurveelement = el, radiocal = setup_type, normcal = as.numeric(ct$NormType[1]),
            calfileinput = list(name = basename(quant_path), datapath = quant_path),
            filetype = cal_in$FileType, energynudge = 0,
            show_vars_k_alpha = grep("\\.K\\.alpha$", lines_avail, value = TRUE),
            show_vars_k_beta = grep("\\.K\\.beta$", lines_avail, value = TRUE),
            show_vars_l_alpha = grep("\\.L\\.alpha$", lines_avail, value = TRUE),
            show_vars_l_beta = grep("\\.L\\.beta$", lines_avail, value = TRUE), show_vars_m = NULL,
            plotunit = "%", loglinear = "Linear",
            linepreferenceelement = "Narrow", linestructureelement = "gaussian",
            gausbuffer = cal_in$LineDefaults$GausBuffer, splitbuffer = cal_in$LineDefaults$SplitBuffer,
            comptonmin = as.numeric(ct$Min[1]), comptonmax = as.numeric(ct$Max[1]),
            comptontype = as.character(ct$ComptonType[1]), randomize = 1, percentrandom = 0.2,
            neuralhiddenlayers = 1, multicore_behavior = "Single Core",
            calname = "release_check", useloomodel = FALSE, modelcompress = TRUE, notes = ""
        )
        basichold$normtype <- as.numeric(ct$NormType[1])
        basichold$normmin <- as.numeric(ct$Min[1]); basichold$normmax <- as.numeric(ct$Max[1])
        basichold$deptransformation <- "None"
        lucashold$slope <- p$Slope; lucashold$intercept <- p$Intercept
        lucashold$ltcross <- "Additive"; lucashold$ltweight <- "1961"
        session$flushReact(); session$elapse(1000)
        used <- function() { pf <- predictFrame(); as.character(pf$Spectrum[alignKeep(vals$keeprows, pf) %in% TRUE]) }
        excluded <- function() setdiff(as.character(predictFrame()$Spectrum), used())
        switch_type <- function(rc) { session$setInputs(radiocal = rc); session$flushReact(); session$elapse(1000) }
        eval(body)
    }), silent = TRUE)
}

# Linear clicks and brushes on the left Cal Curve, the Lucas-Tooth label, an Update with no intercept lines that is saved and reloaded, and a PLS random split with excluded standards.
run_session(QUANT, diag_el, 1, quote({
    cf <- calCurveFrame(); pr <- plot_ranges(calCurvePlot())
    x <- cf$Intensity; y <- cf$Concentration
    nx <- x / diff(pr$x); ny <- y / diff(pr$y)
    d <- sqrt(outer(nx, nx, "-")^2 + outer(ny, ny, "-")^2)
    diag(d) <- Inf
    k <- which.min(apply(d, 1, min))
    sec12$crowded <- min(d[k, ]) <= 0.02
    session$setInputs(plot_cal_click = click_at(x[k], y[k], "Intensity", "Concentration"))
    sec12$click_one <- setequal(excluded(), cf$Spectrum[k])
    session$setInputs(exclude_reset = 1)
    b <- which.max(x)
    session$setInputs(plot_cal_brush = brush_around(x[b], y[b], 1e-9 * max(abs(x)), 1e-9 * max(abs(y)), "Intensity", "Concentration"))
    session$setInputs(exclude_toggle = 1)
    sec12$brush_one <- setequal(excluded(), cf$Spectrum[b])
    session$setInputs(plot_cal_brush = NULL, exclude_reset = 2)
    switch_type(3)
    lucashold$slope <- p$Slope; lucashold$intercept <- NULL
    session$flushReact(); session$elapse(1000)
    vf <- calValFrame()
    sec12$lt_label <- c(shown = label_r2(calCurvePlot()), true = cor(vf$Intensity, vf$Concentration)^2)
    session$setInputs(createcalelement = 1); session$elapse(1500)
    vt <- calValTable()
    q <- calRDS(output$downloadModel, xgb_raw = FALSE, sort = TRUE)
    sec12$build <- q$Build$CloudCal
    variables <- names(q$Intensities)[!names(q$Intensities) %in% c("Spectrum", "Total", "Baseline")]
    pq <- suppressWarnings(cloudCalPredict(Calibration = q, elements.cal = diag_el, elements = diag_el, variables = variables,
        valdata = q$Spectra, deconvoluted_valdata = q$Deconvoluted, rounding = 10, multiplier = 1, cores = 1))
    got <- as.numeric(pq[[diag_el]][match(as.character(vt$Spectrum), as.character(pq$Spectrum))])
    sec12$no_intercept <- c(saved_null = is.null(q$calList[[diag_el]][[1]]$Intercept), max_diff = max(abs(got - vt$Prediction)))
    out <- as.character(predictFrame()$Spectrum[c(3, 11, 19)])
    vals$keeprows[out] <- FALSE
    switch_type(14)
    plshold$plsncomp <- "1-3"; foresthold$forestmetric <- "RMSE"; foresthold$foresttrain <- "cv"; foresthold$forestnumber <- 2; foresthold$cvrepeats <- 1
    lucashold$slope <- unique(c(diag_el, p$Slope))
    session$flushReact(); session$elapse(1000)
    held <- as.character(randomValTable()$Spectrum)
    pf <- predictFrame()
    want <- sort(pf$Concentration[pf$Spectrum %in% setdiff(used(), held)])
    trained <- sort(elementModelRandom()$trainingData$.outcome)
    sec12$random_ml <- length(held) > 0 && !any(held %in% out) && length(trained) == length(want) && isTRUE(all.equal(trained, want))
}))
check("Cal Curves: one click on a standard with a close neighbour toggles only that standard", isTRUE(sec12$crowded) && isTRUE(sec12$click_one))
check("Cal Curves: brush + Toggle points works on the left plot for Linear", isTRUE(sec12$brush_one))
check("Cal Curves: the Lucas-Tooth equation label shows the plotted r2, not 1",
      length(sec12$lt_label) == 2 && sec12$lt_label[["shown"]] < 0.9999 && abs(sec12$lt_label[["shown"]] - sec12$lt_label[["true"]]) < 0.002)
check("A Lucas-Tooth element saved with no intercept lines predicts as in the app after reload",
      length(sec12$no_intercept) == 2 && sec12$no_intercept[["saved_null"]] == 1 && sec12$no_intercept[["max_diff"]] < 1e-8)
check("A saved calibration records the CloudCal build that saved it", identical(sec12$build, CLOUDCAL_BUILD))
check("Random Split (PLS): the model trains on the included standards outside the held-out set", isTRUE(sec12$random_ml))

# A calibration built in this session has no Values in calMemory, only the standards table; the mask is still keyed by Spectrum, so an exclusion follows the standard across model types.
run_session(QUANT, diag_el, 1, quote({
    calMemory$Calibration$Values <- NULL
    for (e in names(calSettings$calList)) calSettings$calList[[e]][[1]]$StandardsUsed <- NULL
    session$setInputs(calcurveelement = setdiff(names(calSettings$calList), diag_el)[1]); session$elapse(1000)
    session$setInputs(calcurveelement = diag_el); session$flushReact(); session$elapse(1500)
    sec12$session_named <- !is.null(names(vals$keeprows))
    target <- as.character(predictFrame()$Spectrum[5])
    vals$keeprows[target] <- FALSE
    seen <- character(0)
    for (rc in c(3, 2, 1)) { switch_type(rc); seen <- c(seen, paste(excluded(), collapse = ",")) }
    sec12$session_types <- all(seen == target)
}))
check("In-session calibration: the standards mask is keyed by Spectrum and follows the standard across model types",
      isTRUE(sec12$session_named) && isTRUE(sec12$session_types))

# A file saved with a training seed but a positional mask: the mask follows the saved model type's frame order, then keeps its standard when the type changes.
seed_el <- names(cal$calList)[vapply(cal$calList, function(x) isTRUE(as.numeric(x[[1]]$CalTable$CalType[1]) == 1), logical(1))][1]
if (!is.na(seed_el)) {
    raw <- readRDS(QUANT)
    n_std <- sum(is.finite(suppressWarnings(as.numeric(raw$Values[[seed_el]]))))
    raw$RandomizeSeed <- 1
    raw$calList[[seed_el]][[1]]$StandardsUsed <- seq_len(n_std) != 7
    SEED_QUANT <- file.path(tempdir(), "seeded_positional.quant")
    saveRDS(raw, SEED_QUANT)
    run_session(SEED_QUANT, seed_el, 1, quote({
        session$elapse(1000)
        sec12$seed_named <- !is.null(names(vals$keeprows))
        at_load <- excluded()
        sec12$seed_load <- identical(at_load, as.character(predictFrame()$Spectrum[7]))
        switch_type(3)
        sec12$seed_switch <- identical(excluded(), at_load)
    }))
    check("Seeded .quant with a positional mask: named from the saved type's frame at load and kept across a type switch",
          isTRUE(sec12$seed_named) && isTRUE(sec12$seed_load) && isTRUE(sec12$seed_switch))
}

# Calibrations saved before the training seed was stored: positional masks cover the standards with values in Spectrum order.
LEGACY_QUANT <- file.path(dirname(QUANT), "MjlnorMudrockTrace.quant")
if (file.exists(LEGACY_QUANT)) {
    run_session(LEGACY_QUANT, "Ni.K.alpha", 3, quote({ sec12$legacy_ni <- sort(excluded()) }))
    run_session(LEGACY_QUANT, "Mn.K.alpha", 3, quote({
        seen <- character(0)
        for (rc in c(3, 1, 2)) { switch_type(rc); seen <- c(seen, paste(excluded(), collapse = ",")) }
        sec12$legacy_mn <- seen
    }))
    check("Older .quant: Ni loads with the standards its saved model left out (mask longer than the standards with values)",
          identical(sec12$legacy_ni, c("HR_07.YF", "HR_09.YF", "HR_61.YF")))
    check("Older .quant: Mn keeps HR_46 excluded under Lucas-Tooth, Linear and Non-Linear", length(sec12$legacy_mn) == 3 && all(sec12$legacy_mn == "HR_46.YF"))
}

message("\n-- 13. Apply Calibration reads saved settings --")
apply_cal <- suppressWarnings(calRDS(QUANT, null.strip = TRUE))
apply_vars <- order_elements(setdiff(names(apply_cal$Intensities), "Spectrum"))
apply_predict <- function(calib, el, ...) {
    pr <- suppressWarnings(cloudCalPredict(Calibration = calib, elements.cal = el, elements = el, variables = apply_vars, valdata = calib$Spectra,
        deconvoluted_valdata = calib$Deconvoluted, rounding = 12, multiplier = 1, cores = 1, ...))
    v <- suppressWarnings(as.numeric(pr[[el]])); names(v) <- as.character(pr$Spectrum); v
}
check("line tables take Total/Baseline once from the spectra's other columns (no .x/.y duplicates from the merge)", {
    lines <- data.frame(Spectrum = c("a", "b"), Fe.K.alpha = c(1, 2), Total = c(-1, -1), Baseline = c(-2, -2))
    other <- data.frame(Spectrum = c("a", "b"), Total = c(10, 20), Baseline = c(3, 4))
    m <- mergeOtherSpectraStuff(lines, other)
    setequal(colnames(m), c("Spectrum", "Fe.K.alpha", "Total", "Baseline")) && identical(m$Total, c(10, 20)) && identical(m$Baseline, c(3, 4))
})
check("Apply predicts intensity models when the variables include Total (calRDS adds it when it rebuilds intensity tables)", {
    lt_el <- names(apply_cal$calList)[vapply(apply_cal$calList, function(e) isTRUE(suppressWarnings(as.numeric(as.character(e$Parameters$CalTable$CalType[1]))) == 3), logical(1))][1]
    pr <- suppressWarnings(cloudCalPredict(Calibration = apply_cal, elements.cal = lt_el, elements = lt_el, variables = union(apply_vars, "Total"),
        valdata = apply_cal$Spectra, deconvoluted_valdata = apply_cal$Deconvoluted, rounding = 12, multiplier = 1, cores = 1))
    !is.na(lt_el) && nrow(pr) > 0 && all(is.finite(suppressWarnings(as.numeric(pr[[lt_el]]))))
})
# Each standard a stored lm was fit on should be predicted at its fitted value; standards are matched by certified concentration.
reproduces_fit <- function(calib, el) {
    m <- calib$calList[[el]]$Model; fb <- fitted(m); yb <- fb + residuals(m)
    conc <- suppressWarnings(as.numeric(calib$Values[[el]])); names(conc) <- calib$Values$Spectrum
    ap <- apply_predict(calib, el)
    dev <- vapply(seq_along(fb), function(i) { cand <- names(conc)[is.finite(conc) & abs(conc - yb[i]) <= 1e-7 * max(1, abs(yb[i]))]; v <- ap[cand]; if (!length(v)) NA else min(abs(v - fb[i])) }, numeric(1))
    mean(dev < 1e-6 * max(1, abs(fb)), na.rm = TRUE)
}
# Lucas-Tooth elements saved with an empty intercept as character(0) were fit with that form.
char0_els <- names(apply_cal$calList)[vapply(names(apply_cal$calList), function(e) { p <- apply_cal$calList[[e]]$Parameters
    isTRUE(as.numeric(p$CalTable$CalType[1]) == 3) && !is.null(p$Intercept) && length(p$Intercept) == 0 && inherits(apply_cal$calList[[e]]$Model, "lm") }, logical(1))]
if (length(char0_els)) check(sprintf("Apply: older Lucas-Tooth models saved with an empty intercept reproduce their fits (%d elements)", length(char0_els)),
    all(vapply(char0_els, function(e) reproduces_fit(apply_cal, e) >= 0.9, logical(1))))
check("Apply: the Scale concentration transform is decoded with the calibration's stored range", {
    d <- data.frame(Intensity = 1:20, Concentration = 3 + 0.5 * (1:20))
    d$Concentration <- scaleTransform(d$Concentration)
    m <- lm(Concentration ~ Intensity, d)
    got <- mclPred(m, data.frame(Intensity = c(2, 10)), "Scale", ymin = 3.5, ymax = 13, confidence = FALSE)
    isTRUE(all.equal(unname(got), c(4, 8)))
})
# A Narrow Line Width saved with an element is used for its intensities, rather than the 0.02 keV default.
width_el <- intersect(names(apply_cal$calList)[vapply(apply_cal$calList, function(x) identical(as.character(x$Parameters$CalTable$LineType[1]), "Narrow") &&
    as.numeric(x$Parameters$CalTable$NormType[1]) == 1 && isTRUE(as.numeric(x$Parameters$CalTable$CalType[1]) == 1), logical(1))], apply_vars)[1]
if (!is.na(width_el)) check("Apply: each element's intensities use its own saved Narrow Line Width", {
    wide <- apply_cal; wide$calList[[width_el]]$Parameters$CalTable$GausBuffer <- 0.05
    cl <- list(Narrow_gaussian = merge(narrowLineTable(apply_cal$Spectra, apply_cal$Definitions, apply_vars, gaus_buffer = 0.05), totalCountsGen(apply_cal$Spectra), by = "Spectrum", all = TRUE, sort = TRUE))
    attr(cl, "buffers") <- c(0.05, 0.1)
    a <- apply_predict(wide, width_el); b <- apply_predict(wide, width_el, count.list = cl); d <- apply_predict(apply_cal, width_el)
    isTRUE(all.equal(a, b[names(a)])) && !isTRUE(all.equal(a, d[names(a)]))
})

check("Saved calibrations record the CloudCal build and the deconvolution algorithm",
    isTRUE(identical(calibrationBuild()$CloudCal, CLOUDCAL_BUILD)) && isTRUE(spectra_gls_deconvolute(apply_cal$Spectra[apply_cal$Spectra$Spectrum %in% unique(apply_cal$Spectra$Spectrum)[1:2], ], cores = 1)$Parameters$Algorithm == DECONVOLUTION_ALGORITHM))
check("Spectra checks: same spectra align, a 2-channel offset is found", {
    s <- apply_cal$Spectra; step <- stats::median(diff(sort(unique(s$Energy))))
    moved <- s; moved$Energy <- moved$Energy + 2 * step
    spectraAlignmentCheck(s, s)$Shift == 0 && spectraAlignmentCheck(s, moved)$Shift == 2
})
check("Calibration self-check finds no stale models in the release quant", nrow(calibrationSelfCheck(apply_cal)) == 0)

message("\n-- 14. Olympus Delta CSV import --")
# A Delta export in miniature: BOM, header block, raw counts per exposure, one test with two exposures, one with one, and empty trailing columns as Delta writes them.
delta_file <- file.path(tempdir(), "delta_check.csv")
set.seed(14)
delta_counts <- matrix(rpois(2048 * 3, 40), ncol = 3)
delta_head <- rbind(
    c("TestID", "04_27_26 #141", "04_27_26 #142", "04_27_26 #142"),
    c("ExposureNum", "1", "1", "2"),
    c("Offset", "-0.026683", "-0.02704", "-0.02704"),
    c("Slope", "0.020074", "0.020074", "0.020074"),
    c("Livetime", "13.2526", "27.2728", "27.6195"),
    c("NumData", "2048", "2048", "2048"),
    c("TubeVoltageSet", "40", "40", "9.96997"),
    c("FilterPosition", "1", "1", "7"),
    c("TimeStamp", "27/04/2026 3:37", "27/04/2026 3:39", "27/04/2026 3:40"))
delta_rows <- rbind(delta_head, cbind("", matrix(as.character(delta_counts), ncol = 3)))
delta_rows <- cbind(delta_rows, "", "")
con <- file(delta_file, "wb"); writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
writeBin(charToRaw(paste0(apply(delta_rows, 1, paste, collapse = ","), "\r\n", collapse = "")), con); close(con)
check("Delta CSV: detected as a multi-exposure aggregate with exposures 1 and 2",
    identical(detect_csv_type(delta_file)$instrument, "Delta") && identical(uniqueBeams(delta_file), c("1", "2")))
check("Delta CSV: each exposure reads as CPS on its own energy axis, named by TestID as the app names spectra", {
    b1 <- importCSVFrame(delta_file, chosen_beam = "1"); b2 <- importCSVFrame(delta_file, chosen_beam = "2")
    s142 <- b1[b1$Spectrum == "X04_27_26..142", ]
    identical(unique(b1$Spectrum), c("X04_27_26..141", "X04_27_26..142")) && identical(unique(b2$Spectrum), "X04_27_26..142") &&
        nrow(s142) == 2048 && isTRUE(all.equal(s142$CPS, delta_counts[, 2] / 27.2728)) &&
        isTRUE(all.equal(range(s142$Energy), -0.02704 + 0.020074 * c(0, 2047))) &&
        isTRUE(all.equal(b2$CPS, delta_counts[, 3] / 27.6195))
})
check("Delta CSV: metadata follows the chosen exposure, one row per spectrum with its live time", {
    m1 <- csvFrameMetadata(delta_file, "delta_check.csv", chosen_beam = "1")
    m2 <- csvFrameMetadata(delta_file, "delta_check.csv", chosen_beam = "2")
    lt <- deconvolution_livetime_lookup(m1)
    identical(names(lt), unique(importCSVFrame(delta_file, chosen_beam = "1")$Spectrum)) &&
        identical(unname(lt), c(13.25, 27.27)) && all(m1$TubeVoltage == 40) && isTRUE(all.equal(m2$TubeVoltage, 9.96997)) &&
        identical(deconvolution_infer_from_metadata(m1)$evch_ev, 20.074)
})
# The same file through the app: choosing exposure 2 loads its one spectrum and that exposure's metadata, and intensities commit.
sec14 <- new.env()
try(testServer(app, {
    session$setInputs(
        filetype = "CSV", energynudge = 0, beamno = "2",
        file1 = data.frame(name = "delta_check.csv", datapath = delta_file, stringsAsFactors = FALSE),
        show_vars_k_alpha = scratch_lines,
        show_vars_k_beta = NULL, show_vars_l_alpha = NULL,
        show_vars_l_beta = NULL, show_vars_m = NULL,
        plotunit = "%", loglinear = "Linear",
        linepreferenceelement = "Narrow", linestructureelement = "gaussian",
        gausbuffer = 0.02, splitbuffer = 0.1,
        comptonmin = 0, comptonmax = 0, comptontype = "Raw",
        randomize = 1
    )
    dh <- tryCatch(dataHold(), error = function(e) NULL)
    sec14$spectra <- if (is.data.frame(dh)) unique(dh$Spectrum) else character(0)
    md <- tryCatch(myMetaData(), error = function(e) NULL)
    sec14$kv <- if (is.data.frame(md)) md$TubeVoltage else NA
    sec14$lt_match <- all(sec14$spectra %in% names(deconvolution_livetime_lookup(md)))
    session$setInputs(linecommit = 1)
    it <- calMemory$Calibration$Intensities
    sec14$intensities <- is.data.frame(it) && nrow(it) == 1 && all(scratch_lines %in% colnames(it))
}), silent = TRUE)
check("Delta CSV in a session: exposure 2 loads one spectrum whose live time matches by name, intensities commit",
      identical(sec14$spectra, "X04_27_26..142") && isTRUE(all.equal(sec14$kv, 9.96997)) && isTRUE(sec14$lt_match) && isTRUE(sec14$intensities))
unlink(delta_file)

message("\n-- 15. E1 from a loaded calibration (no spectra imported) --")
# A .quant loaded on its own must deconvolve with its stored LiveTime so E1 runs and reaches the normalization choices; live time used to come only from imported files, so E1 silently fell back to SNIP. Three spectra of the raw quant, keeping its upload-index metadata keys, keep the session fast.
sec15 <- new.env()
e1_quant <- tempfile(fileext = ".quant")
try({
    raw15 <- readRDS(QUANT)
    keep15 <- head(unique(as.character(raw15$Spectra$Spectrum)), 3)
    small15 <- raw15[intersect(c("FileType", "Units", "Definitions", "Notes"), names(raw15))]
    small15$Spectra <- raw15$Spectra[raw15$Spectra$Spectrum %in% keep15, ]
    small15$SpectraMetadata <- head(raw15$SpectraMetadata, 3)
    small15$Values <- raw15$Values[raw15$Values$Spectrum %in% keep15, ]
    saveRDS(small15, e1_quant)
    testServer(app, {
        session$setInputs(
            calfileinput = list(name = basename(e1_quant), datapath = e1_quant),
            filetype = small15$FileType, energynudge = 0,
            calcurveelement = names(cal$calList)[1], radiocal = 3, normcal = 3,
            comptonmin = 0, comptonmax = 0, comptontype = "Raw",
            deconvolutionwidth = 5, deconvolutionalpha = 2.5, deconvolutiondefaultsigma = 0.07,
            deconvolutionsmoothiter = 20, deconvolutionsnipiter = 20, deconvolutionscatterbg = TRUE,
            deconvolutionmode = "handheld", deconvolutionbeamenergy = 50, deconvolutiontubeanode = "Rh",
            deconvolutiondetector = "Auto", deconvolutionthickness = 450, deconvolutionmass = "relative",
            deconvolutiontubefilter = "Cu 100; Ti 25; Al 300", deconvolutionenvironment = "air_pp",
            deconvolutionsplitseries = "auto", deconvolutiongeometry = FALSE
        )
        session$setInputs(deconvolutebutton = 1)
        dec <- calMemory$Calibration$Deconvoluted
        sec15$e1 <- is.data.frame(dec$E1) && setequal(unique(dec$E1$Spectrum), keep15)
        sec15$roi <- grepl('value="E1"', tryCatch(as.character(output$comptonType$html), error = function(e) ""))
        sec15$geo_note <- tryCatch(as.character(output$deconvolutiongeometryinfo$html), error = function(e) "")
        session$setInputs(deconvolutionmode = "legacy", deconvolutebutton = 2)
        dec <- calMemory$Calibration$Deconvoluted
        sec15$legacy <- identical(dec$Parameters$Physics$.mode, "legacy") && is.null(dec$E1)
        # Blank kV with an anode set used to fail every spectrum (xrf_scatter_peaks needs a kV), leaving the previous fit in place; it must now store a Handheld fit without E1.
        session$setInputs(deconvolutionmode = "handheld", deconvolutionbeamenergy = NA, deconvolutebutton = 3)
        dec <- calMemory$Calibration$Deconvoluted
        sec15$blank_kv <- identical(dec$Parameters$Physics$.mode, "handheld") && is.null(dec$E1) && nrow(dec$Areas) == 3
    })
}, silent = TRUE)
unlink(e1_quant)
check("loaded .quant, Handheld + Rh + 50 kV: Deconvolute runs E1 from the stored LiveTime", isTRUE(sec15$e1))
check("E1 is then offered as the calibration's ROI Type", isTRUE(sec15$roi))
check("Legacy mode re-deconvolutes without E1 (no tube model)", isTRUE(sec15$legacy))
check("blank kV with an anode set fits without E1 instead of failing every spectrum", isTRUE(sec15$blank_kv))

message("\n-- 16. Scatter-angle inference (batch median) --")
# The Far West PDZs record 45 deg incidence and 65 deg take-off from the surface, so with tube and detector on opposite sides of the normal the scatter angle is ~110 deg. The inference used to read only the brightest spectrum, which it declines here (merged Compton region), leaving the 135 deg default.
sp16 <- cal$Spectra
tot16 <- tapply(sp16$CPS, sp16$Spectrum, sum)
bright16 <- names(tot16)[which.max(tot16)]
geo16 <- tryCatch(deconvolution_infer_batch_geometry(sp16, "Rh", 50, "SDD"), error = function(e) NULL)
check("batch inference: Far West scatter angle 105-125 deg, the median of most spectra", {
    isTRUE(geo16$confident) && geo16$scatter_angle_deg > 105 && geo16$scatter_angle_deg < 125 &&
        geo16$n_inferred >= 0.75 * geo16$n_tried && identical(geo16$n_tried, length(tot16))
})
check("the brightest spectrum alone is declined, so a single-spectrum inference would keep 135 deg", {
    solo <- xrf_infer_scatter_geometry(sp16$Energy[sp16$Spectrum == bright16], sp16$CPS[sp16$Spectrum == bright16],
                                       "Rh", 50, detector_type = "SDD")
    !isTRUE(solo$confident)
})
check("deconvolution with that spectrum in the batch stores an inferred angle, not 135 deg", {
    three16 <- c(bright16, setdiff(names(sort(tot16, decreasing = TRUE)), bright16)[1:2])
    phys16 <- instrument_deconv_defaults(mode = "handheld", kv = 50, anode = "Rh", active_thickness_um = 450,
                                         filter = "Cu 100; Ti 25; Al 300")
    dec16 <- spectra_gls_deconvolute(sp16[sp16$Spectrum %in% three16, ], cores = 1, physics = phys16, mass = "off",
                                     livetime = deconvolution_livetime_lookup(cal$SpectraMetadata))
    sg16 <- dec16$Parameters$Physics$.scatter_geometry
    isTRUE(sg16$inferred) && sg16$scatter_angle_deg > 105 && sg16$scatter_angle_deg < 125 && identical(sg16$n_tried, 3L) &&
        identical(dec16$Parameters$Physics$scatter_angle_deg, sg16$scatter_angle_deg)
})
check("the Spectrum page's geometry note reports the batch median (section 15 session)",
      isTRUE(grepl("median of [0-9]+ of 3 spectra", sec15$geo_note)))
check("a batch where every spectrum is declined keeps the default, with the reason and counts", {
    none16 <- deconvolution_infer_batch_geometry(sp16[sp16$Spectrum == bright16, ], "Rh", 50, "SDD")
    !isTRUE(none16$confident) && is.character(none16$reason) && !is.na(none16$reason) &&
        identical(none16$n_inferred, 0L) && identical(none16$n_tried, 1L)
})

message("\n-- 17. Split line windows --")
# From 2024-03 the Narrow split L-alpha, L-beta and M-line windows tested for a channel both below and above the line, which never matches, so every channel was kept: each L/M line read the whole-spectrum sum (same number for every element), and the split K-beta failed for elements with a K-beta2 line.
sp17 <- cal$Spectra; sp17$Spectrum <- as.character(sp17$Spectrum)
tot17 <- tapply(sp17$CPS, sp17$Spectrum, sum)
three17 <- sp17[sp17$Spectrum %in% head(unique(sp17$Spectrum), 3), ]
fl17 <- function(el) fluorescence.lines[fluorescence.lines$Symbol == el, ]
win17 <- function(d, e, b = 0.1){ keep <- Reduce(`|`, lapply(e, function(x) d$Energy >= x - b & d$Energy <= x + b))
    v <- tapply(ifelse(keep, d$CPS, 0), d$Spectrum, sum); as.numeric(v[as.character(unique(d$Spectrum))]) }
grab17 <- function(f, el) { r <- f(el, three17, "split"); as.numeric(r[[2]][match(unique(three17$Spectrum), r$Spectrum)]) }
check("split L-alpha / L-beta / M sum their two line windows, not the whole spectrum", {
    gd <- grab17(elementGrabLalpha, "Gd"); pb_lb <- grab17(elementGrabLbeta, "Pb"); pb_m <- grab17(elementGrabMalpha, "Pb")
    isTRUE(all.equal(gd, win17(three17, c(fl17("Gd")$La1, fl17("Gd")$La2)))) &&
        isTRUE(all.equal(pb_lb, win17(three17, c(fl17("Pb")$Lb1, fl17("Pb")$Lb3)))) &&
        isTRUE(all.equal(pb_m, win17(three17, c(fl17("Pb")$Ma1, fl17("Pb")$Mb)))) &&
        all(gd < 0.5 * tot17[unique(three17$Spectrum)]) && !isTRUE(all.equal(gd, grab17(elementGrabLalpha, "Sm")))
})
check("split K-beta: Kb1 + Kb2 windows for Zr (failed before); Fe's single window unchanged", {
    zr <- grab17(elementGrabKbeta, "Zr"); fe <- grab17(elementGrabKbeta, "Fe")
    fe_old <- { e <- fl17("Fe")$Kb1; v <- tapply(ifelse(three17$Energy > e - 0.1 & three17$Energy < e + 0.1, three17$CPS, 0), three17$Spectrum, sum)
        as.numeric(v[as.character(unique(three17$Spectrum))]) }
    isTRUE(all.equal(zr, win17(three17, c(fl17("Zr")$Kb1, fl17("Zr")$Kb2)))) && isTRUE(all.equal(fe, fe_old))
})
check("a split table saved before the fix (L/M columns = whole spectrum) is rebuilt on load", {
    leg17 <- cal
    lm_cols <- grep("[.](L[.]alpha|L[.]beta|M[.]line)$", names(leg17$IntensitiesSplit), value = TRUE)
    for (cn in lm_cols) leg17$IntensitiesSplit[[cn]] <- as.numeric(tot17[as.character(leg17$IntensitiesSplit$Spectrum)])
    fixed17 <- suppressMessages(calRDS(Calibration = leg17, xgb_raw = FALSE, sort = TRUE))
    by_spec <- function(t) t[order(as.character(t$Spectrum)), lm_cols, drop = FALSE]
    length(lm_cols) > 0 && isTRUE(all.equal(by_spec(fixed17$IntensitiesSplit), by_spec(cal$IntensitiesSplit), check.attributes = FALSE))
})
check("models fitted on the old split L/M windows are named for refitting until stamped; split K-alpha is not", {
    old17 <- cal
    old17$calList$Th.L.alpha$Parameters$CalTable$LineStructure <- "split"
    old17$calList$Rb.K.alpha$Parameters$CalTable$LineStructure <- "split"
    old17$calList$Rb.K.alpha$Parameters$Slope <- "Rb.K.alpha"; old17$calList$Rb.K.alpha$Parameters$Intercept <- NULL
    named <- splitLineRefitElements(old17)
    reload <- suppressMessages(calRDS(Calibration = old17, xgb_raw = FALSE))
    new17 <- old17; new17$calList$Th.L.alpha$Parameters <- stampLineAlgorithm(new17$calList$Th.L.alpha$Parameters)
    reload_new <- suppressMessages(calRDS(Calibration = new17, xgb_raw = FALSE))
    saved <- calListCompress(reload_new$calList)
    identical(named, "Th.L.alpha") && identical(splitLineRefitElements(reload), "Th.L.alpha") &&
        length(splitLineRefitElements(reload_new)) == 0 && identical(saved$Th.L.alpha$Parameters$LineAlgorithm, LINE_INTENSITY_ALGORITHM)
})

message("\n-- 18. XGBoost across xgboost and caret releases --")
# A small xgboost fit stands in for a calibration's XGBoost model. A booster saved under another xgboost release is simulated by a list-shaped booster that the installed xgboost can't predict with, and a copy it can't read by bytes in no model format.
set.seed(18)
X18 <- matrix(runif(240), 60, 4, dimnames = list(NULL, paste0("L", 1:4)))
y18 <- 3 * X18[, 1] + X18[, 2]^2 + rnorm(60, sd = 0.05)
d18 <- xgboost::xgb.DMatrix(X18, label = y18)
b18 <- xgboost::xgb.train(params = list(objective = "reg:squarederror", max_depth = 2, eta = 0.3, nthread = 1), data = d18, nrounds = 20, verbose = 0)
p18 <- predict(b18, xgboost::xgb.DMatrix(X18))
foreign18 <- structure(list(handle = NULL, raw = as.raw(1:8), xNames = colnames(X18), problemType = "Regression", obsLevels = NA), class = "xgb.Booster")
model18 <- list(method = "xgbTree", finalModel = foreign18, coefnames = colnames(X18), modelType = "Regression", bestTune = data.frame(nrounds = 20))
check("xgbBestIteration reads xgb.cv's best iteration (xgboost 1.x and 3.x) or the lowest test error", {
    cv18 <- suppressWarnings(xgboost::xgb.cv(params = list(objective = "reg:squarederror", eval_metric = "rmse", max_depth = 2, eta = 0.3, nthread = 1),
        data = d18, nrounds = 150, folds = split(seq_len(60), rep(1:5, length.out = 60)), early_stopping_rounds = 20, verbose = FALSE))
    xgbBestIteration(cv18) == which.min(cv18$evaluation_log$test_rmse_mean) &&
        identical(xgbBestIteration(list(best_iteration = 7L)), 7L) &&
        identical(xgbBestIteration(list(evaluation_log = data.frame(test_mae_mean = c(3, 1, 2)))), 2L)
})
check("XGBoost models are saved with a UBJSON rawModel, carrying its intercept, that this xgboost reads back", {
    raw18 <- modelPack(parameters = list(CalTable = data.frame(CalType = 8)), model = list(finalModel = b18), table = NULL, compress = FALSE)$rawModel
    is.raw(raw18) && rawToChar(raw18[1]) == "{" && isTRUE(abs(attr(raw18, "base_score") - xgbBaseScore(b18)) < 1e-6) &&
        isTRUE(all.equal(predict(xgbBoosterFromRaw(raw18), xgboost::xgb.DMatrix(X18)), p18, tolerance = 1e-6))
})
check("calRDS rebuilds an unusable XGBoost booster from rawModel and names models with no readable copy", {
    c18 <- cal
    xgb_entry <- function(entry, raw){ entry$Parameters$CalTable$CalType <- 8; entry$Model <- model18; entry$rawModel <- raw; entry }
    c18$calList$Rb.K.alpha <- xgb_entry(cal$calList$Rb.K.alpha, raw18)
    c18$calList$Th.L.alpha <- xgb_entry(cal$calList$Th.L.alpha, charToRaw("binf-legacy"))
    msgs18 <- character(0)
    out18 <- withCallingHandlers(calRDS(Calibration = c18, xgb_raw = FALSE),
        message = function(m){ msgs18 <<- c(msgs18, conditionMessage(m)); invokeRestart("muffleMessage") })
    fm <- out18$calList$Rb.K.alpha$Model$finalModel
    identical(xgbUnusableElements(c18), c("Rb.K.alpha", "Th.L.alpha")) && identical(xgbUnusableElements(out18), "Th.L.alpha") &&
        any(grepl("XGBoost models.*Th.L.alpha", msgs18)) && identical(attr(fm, "problemType"), "Regression") &&
        isTRUE(all.equal(predict(fm, xgboost::xgb.DMatrix(X18)), p18, tolerance = 1e-6))
})
check("tuning grids are conformed to the installed caret (fork-only columns dropped, missing ones defaulted)", {
    tuned <- as.character(caret::getModelInfo("xgbDART", regex = FALSE)[[1]]$parameters$parameter)
    g <- xgbTuneGrid(data.frame(nrounds = 20, max_depth = 2, eta = 0.3, gamma = 0, colsample_bytree = 1, min_child_weight = 1, subsample = 1, scale_pos_weight = 1, bogus = 5), "xgbDART")
    setequal(names(g), tuned) && nrow(g) == 1 && all(g$rate_drop == 0)
})
check("caret trains XGBoost with the installed xgboost, and a rebuilt booster predicts through caret", {
    problem <- xgbCaretProblem()
    if (!is.null(problem)) stop(problem)
    grid <- xgbTuneGrid(data.frame(nrounds = 20, max_depth = 2, eta = 0.3, gamma = 0, colsample_bytree = 1, min_child_weight = 1, subsample = 1), "xgbTree")
    fit18 <- caret::train(x = as.data.frame(X18), y = y18, method = "xgbTree", tuneGrid = grid, trControl = caret::trainControl(method = "cv", number = 3), nthread = 1)
    el <- list(Parameters = list(CalTable = data.frame(CalType = 8)), Model = fit18, rawModel = xgbBoosterRaw(fit18$finalModel))
    el$Model$finalModel <- foreign18
    rebuilt <- xgbRepairElement(el)$Model
    isTRUE(all.equal(as.numeric(predict(rebuilt, newdata = as.data.frame(X18))), as.numeric(predict(fit18, newdata = as.data.frame(X18))), tolerance = 1e-6))
})
check("XGBoost Linear fits a linear booster that uses eta, with the element's preprocessing stored in the model", {
    problem <- xgbCaretProblem()
    if (!is.null(problem)) stop(problem)
    lin18 <- function(eta, preprocess) caret::train(x = as.data.frame(X18), y = y18, method = xgbCaretMethod("xgbLinear"), preProcess = caretPreProcess(preprocess),
        tuneGrid = data.frame(nrounds = 30, lambda = 0.1, alpha = 0, eta = eta), trControl = caret::trainControl(method = "none"), nthread = 1)
    a <- lin18(0.3, "Center and Scale"); b <- lin18(0.05, "Center and Scale"); none <- lin18(0.3, "None")
    cfg <- xgboost::xgb.config(a$finalModel); if (is.character(cfg)) cfg <- jsonlite::fromJSON(cfg)
    pa <- as.numeric(predict(a, newdata = as.data.frame(X18))); pb <- as.numeric(predict(b, newdata = as.data.frame(X18)))
    identical(cfg$learner$gradient_booster$name, "gblinear") && max(abs(pa - pb)) > 1e-3 &&
        identical(a$preProcess$method$center, colnames(X18)) && is.null(none$preProcess) && cor(pa, y18) > 0.9
})
check("the XGBoost Type control shows the type that trains", {
    shown <- function(tag) sub('.*<option value="([^"]+)" selected>.*', "\\1", gsub("\n", " ", as.character(tag)))
    identical(shown(xgbTypeUI(8, c("Tree", "Linear"))), "Tree") && identical(shown(xgbTypeUI(9, c("Radial", "Dart"))), "Dart") &&
        identical(shown(xgbTypeUI(12, "Tree")), "Linear") && is.null(xgbTypeUI(3, "Tree"))
})
check("Tree Method offers only what the installed xgboost accepts; saved gpu_hist trains as hist under xgboost 3", {
    if (xgbReadOnlyBooster()) {
        !"gpu_hist" %in% xgbTreeMethods() && identical(xgbTreeMethod("gpu_hist"), "hist")
    } else {
        "gpu_hist" %in% xgbTreeMethods() && identical(xgbTreeMethod("gpu_hist"), "gpu_hist")
    } && identical(xgbTreeMethod("approx"), "approx") && identical(xgbTreeMethod(NULL), "auto")
})
check("XGBoost Alpha defaults to 0 and BART has its own alpha (0.95; elements saved as BART keep the alpha they trained with)", {
    bart_old <- cal$calList["Rb.K.alpha"]
    bart_old$Rb.K.alpha$Parameters$CalTable$CalType <- 10; bart_old$Rb.K.alpha$Parameters$CalTable$xgbType <- "Tree"
    bart_old$Rb.K.alpha$Parameters$CalTable$xgbAlpha <- "0.1-0.1"; bart_old$Rb.K.alpha$Parameters$CalTable$bartAlpha <- NULL
    xgb_old <- bart_old; xgb_old$Rb.K.alpha$Parameters$CalTable$CalType <- 8
    identical(calConditionsTable(cal.type = 8)$xgbAlpha, "0-0") && identical(calConditionsTable(cal.type = 10)$bartAlpha, "0.95-0.95") &&
        identical(defaultCalConditions("Rb.K.alpha", 10)$CalTable$xgbAlpha, "0-0") &&
        identical(importCalConditions("Rb.K.alpha", bart_old)$CalTable$bartAlpha, "0.1-0.1") &&
        identical(importCalConditions("Rb.K.alpha", xgb_old)$CalTable$bartAlpha, "0.95-0.95")
})
check("XGBoost penalty sliders reach small values and stretch to fit saved settings instead of clipping them", {
    attr_of <- function(tag, a) sub(paste0('.*data-', a, '="([^"]+)".*'), "\\1", gsub("\n", " ", as.character(tag)))
    a_new <- xgbAlphaUI(8, c(0, 0)); a_old <- xgbAlphaUI(8, c(0, 10)); g_old <- xgbGammaUI(8, c(0, 100), "Tree")
    identical(attr_of(a_new, "max"), "1") && identical(attr_of(a_new, "step"), "0.001") && identical(attr_of(a_old, "max"), "10") &&
        identical(attr_of(g_old, "max"), "100") && identical(attr_of(xgbGammaUI(8, c(0, 0), "Tree"), "step"), "0.01") &&
        is.null(xgbAlphaUI(10, c(0.95, 0.95), "Tree")) && !is.null(bartAlphaUI(10, c(0.95, 0.95), "Tree")) && is.null(bartAlphaUI(8, c(0.95, 0.95), "Tree"))
})
check("a model that predicts one value is named with its likely cause (linear Alpha limit, splitless trees, SVM tolerance)", {
    d18p <- xgboost::xgb.DMatrix(X18, label = y18)
    flat_lin <- xgboost::xgb.train(params = list(booster = "gblinear", objective = "reg:squarederror", alpha = 50, lambda = 0, nthread = 1), data = d18p, nrounds = 20, verbose = 0)
    flat_tree <- xgboost::xgb.train(params = list(objective = "reg:squarederror", gamma = 1e6, nthread = 1), data = d18p, nrounds = 5, verbose = 0)
    as_model <- function(b) list(finalModel = b, trainingData = data.frame(X18, .outcome = y18), preProcess = NULL, bestTune = data.frame(alpha = 50), method = "custom")
    lin_msg <- constantModelMessage(as_model(flat_lin), predict(flat_lin, xgboost::xgb.DMatrix(X18)))
    limit <- max(abs(colSums((y18 - mean(y18)) * X18))) / length(y18)
    tree_msg <- constantModelMessage(as_model(flat_tree), predict(flat_tree, xgboost::xgb.DMatrix(X18)))
    svm_msg <- constantModelMessage(list(method = "svmRadial"), rep(0.5, 10))
    grepl(paste0("Alpha must be below ", signif(limit, 2)), lin_msg, fixed = TRUE) && grepl("made no splits", tree_msg) &&
        grepl("SVMs ignore errors", svm_msg) && is.null(constantModelMessage(as_model(b18), p18))
})
check("Tuning presets set only tuning settings: Best general single values, Exploration ranges, never resampling, Preprocess, transforms or model type", {
    poisoned <- cal$calList$Rb.K.alpha$Parameters$CalTable
    poisoned$xgbAlpha <- "0-10"; poisoned$xgbLambda <- "0-100"; poisoned$bartAlpha <- "0.5-0.5"; poisoned$svmSigma <- "2-2"
    poisoned$ForestNumber <- 250; poisoned$Preprocess <- "Center and Scale"; poisoned$DepTrans <- "Scale"; poisoned$CalType <- 9
    defaults <- defaultCalConditions("Rb.K.alpha", 40)$CalTable
    best <- applyTuningCalTable(poisoned, tuningPresetCalTable("Best general", defaults))
    explore <- applyTuningCalTable(poisoned, tuningPresetCalTable("Exploration range", defaults))
    untouched <- function(t) t$ForestNumber == 250 && identical(t$Preprocess, "Center and Scale") && identical(t$DepTrans, "Scale") && t$CalType == 9
    identical(best$xgbAlpha, "0-0") && identical(best$xgbLambda, defaults$xgbLambda) && identical(best$bartAlpha, "0.95-0.95") && identical(best$svmSigma, defaults$svmSigma) &&
        identical(explore$xgbAlpha, "0-0.1") && identical(explore$TreeDepth, "2-8") && as.numeric(explore$ForestTrees) == 500 && identical(explore$svmSigma, defaults$svmSigma) &&
        untouched(best) && untouched(explore) && is.null(tuningPresetCalTable("Saved settings", defaults)) &&
        identical(tuningValue("TreeDepth", "5-5"), c(5, 5)) && identical(tuningValue("TreeMethod", "hist"), "hist")
})
check("decodeDependent inverts each dependent transformation (None, Log, e, Scale)", {
    x <- c(0.0002, 0.0011, 0.0064)
    isTRUE(all.equal(decodeDependent(scaleTransform(x), "Scale", min(x), max(x)), x)) && isTRUE(all.equal(decodeDependent(log(x), "Log"), x)) &&
        isTRUE(all.equal(decodeDependent(exp(x), "e"), x)) && identical(decodeDependent(x, "None"), x) && identical(decodeDependent(x, NA), x)
})
check("Preprocess is saved per element: None by default and for older calibrations, kept on reload", {
    old <- cal$calList["Rb.K.alpha"]; old$Rb.K.alpha$Parameters$CalTable$Preprocess <- NULL
    ranged <- old; ranged$Rb.K.alpha$Parameters$CalTable$Preprocess <- "Range"
    identical(calConditionsTable(cal.type = 8)$Preprocess, "None") &&
        identical(calConditionsTable(cal.type = 8, preprocess = "Center and Scale")$Preprocess, "Center and Scale") &&
        identical(importCalConditions("Rb.K.alpha", old)$CalTable$Preprocess, "None") &&
        identical(importCalConditions("Rb.K.alpha", ranged)$CalTable$Preprocess, "Range") &&
        is.null(caretPreProcess("None")) && identical(caretPreProcess("Center and Scale"), c("center", "scale")) && identical(caretPreProcess("bogus"), NULL)
})

n_fail <- sum(!unlist(results))
message(sprintf("\n== %d/%d checks passed ==", sum(unlist(results)), length(results)))
if (n_fail > 0) {
    message("FAILED: ", paste(names(results)[!unlist(results)], collapse = "; "))
    quit(status = 1)
}
message("Release check PASSED")
