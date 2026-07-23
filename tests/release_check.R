# CloudCal pre-release check
# ==========================
# One-source smoke/regression test to run before releases:
#   1. Load a handheld .quant and verify its structure and repaired tables
#   2. Verify the intensity-table builders (fast path == per-element path)
#   3. Drive a session: review cal curves per element, commit line edits
#      (including an out-of-range definition), and survive
#   4. Build a brand-new calibration from the raw spectra and fit a model,
#      then re-fit it under each normalization and a scaled-concentration
#      transform (the full customization grid lives in tests/model_check.R)
#   5. Predict the calibration's own standards and compare to stored values
#   6. Standardless FP estimate sanity (raw grams + closure)
#   7. Example-file zoo: every recognized import format reads, carries
#      metadata (live time, detector, eV/ch where present), and the MCA
#      spectra deconvolute with the physics defaults inferred from the file
#
# Usage:
#   Rscript tests/release_check.R [path/to/cal.quant] [path/to/spectra_dir] [fp_quant] [examples_dir]
# Defaults target the Far West 50 kV obsidian set.
# Exit status is nonzero if any check fails.

args <- commandArgs(trailingOnly = TRUE)
QUANT <- if (length(args) >= 1) args[1] else
    "/Users/lee/Dropbox/Documents/CloudCal Evaluation/Test Quants/farWest50kV.quant"
SPECTRA_DIR <- if (length(args) >= 2) args[2] else
    "/Users/lee/Dropbox/Documents/CloudCal Evaluation/Far West Obsidian/50 kv 30a Cu100 Ti25 Al300"
FP_QUANT <- if (length(args) >= 3) args[3] else
    "/Users/lee/Dropbox/Documents/CloudCal Evaluation/Test Quants/steel_template.quant"
EXAMPLES_DIR <- if (length(args) >= 4) args[4] else
    "/Users/lee/Dropbox/Documents/CloudCal Evaluation/Example Files"

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

## ------------------------------------------------------------------
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

## ------------------------------------------------------------------
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

## ------------------------------------------------------------------
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

    # commit an edit including an out-of-range definition: must survive
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

## ------------------------------------------------------------------
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

        # customization smoke on the scratch model: each normalization plus a
        # scaled-concentration fit (full option grid: tests/model_check.R)
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

## ------------------------------------------------------------------
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

## ------------------------------------------------------------------
message("\n-- 6. Standardless FP estimates (steel reference) --")
if (file.exists(FP_QUANT)) {
    steel <- calRDS(FP_QUANT, xgb_raw = FALSE, sort = TRUE)
    steel_lt <- tryCatch(deconvolution_livetime_lookup(steel$SpectraMetadata), error = function(e) NULL)
    # Full-FP fidelity (self-absorption + secondary fluorescence + abundance
    # regularization) with instrument physics from the stored metadata: this is
    # the mode where holding the total at 1 yields mass fractions comparable to
    # the certified standards. Relative mode leaves phantom elements (rare
    # earths etc.) absorbing most of the closed mass share.
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

## ------------------------------------------------------------------
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
        # dual-beam PDZs legitimately yield one spectrum per beam (_1/_2), so
        # require at least one spectrum per file rather than exactly one
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
            # LiveTime must be positive: LIVE_TIME when valid, REAL_TIME fallback
            # when the export logged 0 (100% dead time, e.g. the CdTe example)
            read_ok(sp) && is.data.frame(md) && is.finite(md$LiveTime[1]) && md$LiveTime[1] > 0
        }, logical(1)))
    })

    txts <- zoo[ext == "txt"]
    check(sprintf("TXT files (%d) read + metadata", length(txts)), {
        all(vapply(txts, function(f){
            sp <- tryCatch(readTXTData(filepath = f, filename = basename(f)), error = function(e) NULL)
            md <- tryCatch(txtFrameMetadata(f, basename(f)), error = function(e) NULL)
            read_ok(sp) && is.data.frame(md)
        }, logical(1)))
    })

    check("MCA spectra deconvolute with file-inferred physics defaults", {
        length(zoo_decon) > 0 && all(vapply(zoo_decon, function(z){
            inf <- tryCatch(deconvolution_infer_from_metadata(z$md), error = function(e) NULL)
            phys <- tryCatch(instrument_deconv_defaults(
                mode = if (!is.null(inf$mode)) inf$mode else "legacy",
                kv = inf$kv, detector_type = inf$detector, environment = "air_pp"),
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

## ------------------------------------------------------------------
n_fail <- sum(!unlist(results))
message(sprintf("\n== %d/%d checks passed ==", sum(unlist(results)), length(results)))
if (n_fail > 0) {
    message("FAILED: ", paste(names(results)[!unlist(results)], collapse = "; "))
    quit(status = 1)
}
message("Release check PASSED")
