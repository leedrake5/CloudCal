# CloudCal pre-release check
# Smoke and regression test to run before releases:
# 1. Load a handheld .quant and verify its structure and repaired tables
# 2. Verify the intensity-table builders (fast path == per-element path)
# 3. Drive a session: review cal curves per element and commit line edits, including an out-of-range definition, without the session failing
# 4. Build a new calibration from the raw spectra, fit a model, and re-fit it under each normalization and a scaled-concentration transform (the full grid is in tests/model_check.R)
# 5. Predict the calibration's own standards and compare to stored values
# 6. Standardless FP estimate sanity (raw estimate + closure)
# 7. Example files: every recognized import format reads and carries metadata (live time, detector, eV/ch where present), and the MCA and SEM-EDS (Esprit .txt) spectra deconvolute with physics defaults inferred from the file
# 8. High-energy K/L+M series split: a 200 kV / 450 um SDD spectrum keeps its Pb K-lines, El.K / El.L line names read their own series, and prediction re-runs E1 when training did
# 9. Spectra saved as R objects (.RData / .rda / .rds) import in every supported shape
# 10. Cross Validation: the Leave-One-Out plots, standards table and both downloads render for a Lucas-Tooth element in both forms, Use Cross-Validated Model saves the averaged refits, and the Random Split standards table holds exactly the held-out standards
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
    session$setInputs(useloomodel = TRUE)
    session$setInputs(createcalelement = 2)
    saved <- calMemory$Calibration$calList[[loo_el]]
    sec10$saved_cv <- isTRUE(all.equal(coef(saved$Model), coef(looCVModel()))) && isTRUE(all.equal(saved$Table, looValTable())) &&
        !isTRUE(all.equal(coef(saved$Model), coef(elementModelGen())))
    session$setInputs(useloomodel = FALSE, radiocal = 4)
    sec10$other_type <- tryCatch({ output$calcurveplotsloo; "rendered" }, error = function(e) conditionMessage(e))
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
check("Leave-one-out page: other model types point to the Random Split tab",
      grepl("Random Split", sec10$other_type))

n_fail <- sum(!unlist(results))
message(sprintf("\n== %d/%d checks passed ==", sum(unlist(results)), length(results)))
if (n_fail > 0) {
    message("FAILED: ", paste(names(results)[!unlist(results)], collapse = "; "))
    quit(status = 1)
}
message("Release check PASSED")
