fileloading <- "loaded"

###Lines


standard <- c("Spectrum", "Ca.K.alpha", "Ti.K.alpha", "Fe.K.alpha")

kalphaLines <- c("Na"="Na.K.alpha",  "Mg"="Mg.K.alpha", "Al"="Al.K.alpha", "Si"="Si.K.alpha", "P"="P.K.alpha", "S"="S.K.alpha", "Cl"="Cl.K.alpha", "Ar"="Ar.K.alpha", "K"="K.K.alpha", "Ca"="Ca.K.alpha", "Sc"="Sc.K.alpha", "Ti"="Ti.K.alpha", "V"="V.K.alpha", "Cr"="Cr.K.alpha", "Mn"="Mn.K.alpha", "Fe"="Fe.K.alpha", "Co"="Co.K.alpha", "Ni"="Ni.K.alpha", "Cu"="Cu.K.alpha", "Zn"="Zn.K.alpha", "Ga"="Ga.K.alpha", "Ge"="Ge.K.alpha", "As"="As.K.alpha", "Se"="Se.K.alpha", "Br"="Br.K.alpha", "Kr"="Kr.K.alpha", "Rb"="Rb.K.alpha", "Sr"="Sr.K.alpha", "Y"="Y.K.alpha", "Zr"="Zr.K.alpha", "Nb"="Nb.K.alpha", "Mo"="Mo.K.alpha", "Ru"="Ru.K.alpha", "Rh"="Rh.K.alpha", "Pd"="Pd.K.alpha", "Ag"="Ag.K.alpha", "Cd"="Cd.K.alpha", "In"="In.K.alpha", "Sn"="Sn.K.alpha", "Sb"="Sb.K.alpha", "Te"="Te.K.alpha", "I"="I.K.alpha", "Xe"="Xe.K.alpha", "Cs"="Cs.K.alpha", "Ba"="Ba.K.alpha", "La"="La.K.alpha", "Ce"="Ce.K.alpha", "Pr"="Pr.K.alpha", "Nd"="Nd.K.alpha", "Pm"="Pm.K.alpha", "Sm"="Sm.K.alpha", "Eu"="Eu.K.alpha", "Gd"="Gd.K.alpha", "Tb"="Tb.K.alpha", "Dy"="Dy.K.alpha", "Ho"="Ho.K.alpha", "Er"="Er.K.alpha", "Tm"="Tm.K.alpha", "Yb"="Yb.K.alpha", "Lu"="Lu.K.alpha", "Hf"="Hf.K.alpha", "Ta"="Ta.K.alpha", "W"="W.K.alpha", "Re"="Re.K.alpha", "Os"="Os.K.alpha", "Ir"="Ir.K.alpha", "Pt"="Pt.K.alpha", "Au"="Au.K.alpha", "Hg"="Hg.K.alpha", "Tl"="Tl.K.alpha", "Pb"="Pb.K.alpha", "Bi"="Bi.K.alpha", "Po"="Po.K.alpha", "At"="At.K.alpha", "Rn"="Rn.K.alpha", "Fr"="Fr.K.alpha", "Ra"="Ra.K.alpha", "Ac"="Ac.K.alpha", "Th"="Th.K.alpha", "Pa"="Pa.K.alpha", "U"="U.K.alpha")

kbetaLines <- c("Na"="Na.K.beta",  "Mg"="Mg.K.beta", "Al"="Al.K.beta", "Si"="Si.K.beta", "P"="P.K.beta", "S"="S.K.beta", "Cl"="Cl.K.beta", "Ar"="Ar.K.beta", "K"="K.K.beta", "Ca"="Ca.K.beta", "Sc"="Sc.K.beta", "Ti"="Ti.K.beta", "V"="V.K.beta", "Cr"="Cr.K.beta", "Mn"="Mn.K.beta", "Fe"="Fe.K.beta", "Co"="Co.K.beta", "Ni"="Ni.K.beta", "Cu"="Cu.K.beta", "Zn"="Zn.K.beta", "Ga"="Ga.K.beta", "Ge"="Ge.K.beta", "As"="As.K.beta", "Se"="Se.K.beta", "Br"="Br.K.beta", "Kr"="Kr.K.beta", "Rb"="Rb.K.beta", "Sr"="Sr.K.beta", "Y"="Y.K.beta", "Zr"="Zr.K.beta", "Nb"="Nb.K.beta", "Mo"="Mo.K.beta", "Ru"="Ru.K.beta", "Rh"="Rh.K.beta", "Pd"="Pd.K.beta", "Ag"="Ag.K.beta", "Cd"="Cd.K.beta", "In"="In.K.beta", "Sn"="Sn.K.beta", "Sb"="Sb.K.beta", "Te"="Te.K.beta", "I"="I.K.beta", "Xe"="Xe.K.beta", "Cs"="Cs.K.beta", "Ba"="Ba.K.beta", "La"="La.K.beta", "Ce"="Ce.K.beta", "Pr"="Pr.K.beta", "Nd"="Nd.K.beta", "Pm"="Pm.K.beta", "Sm"="Sm.K.beta", "Eu"="Eu.K.beta", "Gd"="Gd.K.beta", "Tb"="Tb.K.beta", "Dy"="Dy.K.beta", "Ho"="Ho.K.beta", "Er"="Er.K.beta", "Tm"="Tm.K.beta", "Yb"="Yb.K.beta", "Lu"="Lu.K.beta", "Hf"="Hf.K.beta", "Ta"="Ta.K.beta", "W"="W.K.beta", "Re"="Re.K.beta", "Os"="Os.K.beta", "Ir"="Ir.K.beta", "Pt"="Pt.K.beta", "Au"="Au.K.beta", "Hg"="Hg.K.beta", "Tl"="Tl.K.beta", "Pb"="Pb.K.beta", "Bi"="Bi.K.beta", "Po"="Po.K.beta", "At"="At.K.beta", "Rn"="Rn.K.beta", "Fr"="Fr.K.beta", "Ra"="Ra.K.beta", "Ac"="Ac.K.beta", "Th"="Th.K.beta", "Pa"="Pa.K.beta", "U"="U.K.beta")

lalphaLines <- c("Si"="Si.L.alpha", "P"="P.L.alpha", "S"="S.L.alpha", "Cl"="Cl.L.alpha", "Ar"="Ar.L.alpha", "K"="K.L.alpha", "Ca"="Ca.L.alpha", "Sc"="Sc.L.alpha", "Ti"="Ti.L.alpha", "V"="V.L.alpha", "Cr"="Cr.L.alpha", "Mn"="Mn.L.alpha", "Fe"="Fe.L.alpha", "Co"="Co.L.alpha", "Ni"="Ni.L.alpha", "Cu"="Cu.L.alpha", "Zn"="Zn.L.alpha", "Ga"="Ga.L.alpha", "Ge"="Ge.L.alpha", "As"="As.L.alpha", "Se"="Se.L.alpha", "Br"="Br.L.alpha", "Kr"="Kr.L.alpha", "Rb"="Rb.L.alpha", "Sr"="Sr.L.alpha", "Y"="Y.L.alpha", "Zr"="Zr.L.alpha", "Nb"="Nb.L.alpha", "Mo"="Mo.L.alpha", "Ru"="Ru.L.alpha", "Rh"="Rh.L.alpha", "Pd"="Pd.L.alpha", "Ag"="Ag.L.alpha", "Cd"="Cd.L.alpha", "In"="In.L.alpha", "Sn"="Sn.L.alpha", "Sb"="Sb.L.alpha", "Te"="Te.L.alpha", "I"="I.L.alpha", "Xe"="Xe.L.alpha", "Cs"="Cs.L.alpha", "Ba"="Ba.L.alpha", "La"="La.L.alpha", "Ce"="Ce.L.alpha", "Pr"="Pr.L.alpha", "Nd"="Nd.L.alpha", "Pm"="Pm.L.alpha", "Sm"="Sm.L.alpha", "Eu"="Eu.L.alpha", "Gd"="Gd.L.alpha", "Tb"="Tb.L.alpha", "Dy"="Dy.L.alpha", "Ho"="Ho.L.alpha", "Er"="Er.L.alpha", "Tm"="Tm.L.alpha", "Yb"="Yb.L.alpha", "Lu"="Lu.L.alpha", "Hf"="Hf.L.alpha", "Ta"="Ta.L.alpha", "W"="W.L.alpha", "Re"="Re.L.alpha", "Os"="Os.L.alpha", "Ir"="Ir.L.alpha", "Pt"="Pt.L.alpha", "Au"="Au.L.alpha", "Hg"="Hg.L.alpha", "Tl"="Tl.L.alpha", "Pb"="Pb.L.alpha", "Bi"="Bi.L.alpha", "Po"="Po.L.alpha", "At"="At.L.alpha", "Rn"="Rn.L.alpha", "Fr"="Fr.L.alpha", "Ra"="Ra.L.alpha", "Ac"="Ac.L.alpha", "Th"="Th.L.alpha", "Pa"="Pa.L.alpha", "U"="U.L.alpha")

lbetaLines <- c("Ca"="Ca.L.beta", "Sc"="Sc.L.beta", "Ti"="Ti.L.beta", "V"="V.L.beta", "Cr"="Cr.L.beta", "Mn"="Mn.L.beta", "Fe"="Fe.L.beta", "Co"="Co.L.beta", "Ni"="Ni.L.beta", "Cu"="Cu.L.beta", "Zn"="Zn.L.beta", "Ga"="Ga.L.beta", "Ge"="Ge.L.beta", "As"="As.L.beta", "Se"="Se.L.beta", "Br"="Br.L.beta", "Kr"="Kr.L.beta", "Rb"="Rb.L.beta", "Sr"="Sr.L.beta", "Y"="Y.L.beta", "Zr"="Zr.L.beta", "Nb"="Nb.L.beta", "Mo"="Mo.L.beta", "Ru"="Ru.L.beta", "Rh"="Rh.L.beta", "Pd"="Pd.L.beta", "Ag"="Ag.L.beta", "Cd"="Cd.L.beta", "In"="In.L.beta", "Sn"="Sn.L.beta", "Sb"="Sb.L.beta", "Te"="Te.L.beta", "I"="I.L.beta", "Xe"="Xe.L.beta", "Cs"="Cs.L.beta", "Ba"="Ba.L.beta", "La"="La.L.beta", "Ce"="Ce.L.beta", "Pr"="Pr.L.beta", "Nd"="Nd.L.beta", "Pm"="Pm.L.beta", "Sm"="Sm.L.beta", "Eu"="Eu.L.beta", "Gd"="Gd.L.beta", "Tb"="Tb.L.beta", "Dy"="Dy.L.beta", "Ho"="Ho.L.beta", "Er"="Er.L.beta", "Tm"="Tm.L.beta", "Yb"="Yb.L.beta", "Lu"="Lu.L.beta", "Hf"="Hf.L.beta", "Ta"="Ta.L.beta", "W"="W.L.beta", "Re"="Re.L.beta", "Os"="Os.L.beta", "Ir"="Ir.L.beta", "Pt"="Pt.L.beta", "Au"="Au.L.beta", "Hg"="Hg.L.beta", "Tl"="Tl.L.beta", "Pb"="Pb.L.beta", "Bi"="Bi.L.beta", "Po"="Po.L.beta", "At"="At.L.beta", "Rn"="Rn.L.beta", "Fr"="Fr.L.beta", "Ra"="Ra.L.beta", "Ac"="Ac.L.beta", "Th"="Th.L.beta", "Pa"="Pa.L.beta", "U"="U.L.beta")

mLines <- c("W"="W.M.line", "Re"="Re.M.line", "Os"="Os.M.line", "Re"="Re.M.line", "Ir"="Ir.M.line", "Pt"="Pt.M.line", "Au"="Au.M.line", "Hg"="Hg.M.line", "Tl"="Tl.M.line", "Pb"="Pb.M.line", "Bi"="Bi.M.line", "Th"="Th.M.line", "U"="U.M.line")

spectralLines <- c(paste0(names(kalphaLines), ".K.alpha"), paste0(names(kbetaLines), ".K.beta"), paste0(names(lalphaLines), ".L.alpha"), paste0(names(lbetaLines), ".L.beta"), paste0(names(mLines), ".M.line"))


remove_na <- function(vector){
  return(vector[!is.na(vector)])
}
remove_na <- cmpfun(remove_na)

find_row_with_string <- function(data, search_string) {
  # Find rows where the value in the first column contains the search_string
  matchingRows <- which(sapply(as.character(data[,1]), grepl, pattern = search_string))
  
  if (length(matchingRows) > 0) {
    return(matchingRows)
  } else {
    return(NULL)
  }
}
find_row_with_string <- cmpfun(find_row_with_string)

get_filetype <- function(path){
  # Get the file extension
  ext <- file_ext(path)
  
  # Remove the dot from the beginning of the extension
  if (substr(ext, 1, 1) == ".") {
    ext <- substr(ext, 2, nchar(ext))
  }
  
  # Return the extension in uppercase letters
  return(toupper(ext))
}

###Spectra Loading
read_csv_filename_x <- function(filename){
    ret <- read.csv(file=filename, sep=",", header=FALSE)
    return.res <- as.numeric(as.vector(ret[ret$V1 %in% "eV per channel",]$V2))/1000
    n <- nrow(ret)
    return.chan.counts <-as.numeric(as.vector(ret$V1[(n-2047):n]))
    return.energy <- return.chan.counts*return.res
    return(return.energy)
}
read_csv_filename_x <- cmpfun(read_csv_filename_x)

read_csv_filename_y <- function(filename){
    ret <- read.csv(file=filename, sep=",", header=FALSE)
    return.live.time <- as.numeric(as.vector(ret[ret$V1 %in% "Live Time",]$V2))
    n <- nrow(ret)
    return.counts <-as.numeric(as.vector(ret$V2[(n-2047):n]))
    return.cps <- return.counts/return.live.time
    return(return.cps)
}
read_csv_filename_y <- cmpfun(read_csv_filename_y)

csvFrameOld <- function(filepath, filename=NULL){
    if(is.null(filename)){
        filename <- as.character(basename(filepath))
    }
    filename <- make.names(filename)
    filename <- gsub(".csv", "", filename, ignore.case=TRUE)
    return(data.frame(Energy=read_csv_filename_x(filepath), CPS=read_csv_filename_y(filepath), Spectrum=rep(filename, length(read_csv_filename_x(filepath))), stringsAsFactors=FALSE))
}
csvFrameOld <- cmpfun(csvFrameOld)

csvFrameOG <- function(ret=NULL, filepath, filename=NULL, use_native_calibration=TRUE){
    if(is.null(filename)){
        filename <- as.character(basename(filepath))
    }
    filename <- make.names(filename)
    filename <- gsub(".csv", "", filename, ignore.case=TRUE)
    
    if(is.null(ret)){
        ret <- read.csv(file=filepath, sep=",", header=FALSE)
    }
    
    n <- nrow(ret)
    if(ret[nrow(ret), "V1"]=="2048"){
        ret[,"V1"] <- c(ret$V1[1:21], as.vector(seq(0, 2047, 1)))
    }
    
    return.res <- tryCatch(as.numeric(as.vector(ret[ret$V1 %in% "eV per channel",]$V2))/1000, error=function(e) 0.02)
    return.chan.counts <-as.numeric(as.vector(ret$V1[(n-2048):n]))
    return.energy <- if(use_native_calibration==TRUE){
            return.chan.counts*return.res
        } else if(use_native_calibration==FALSE){
            return.chan.counts
        }
    
    return.live.time <- round(as.numeric(as.vector(ret[ret$V1 %in% "Live Time",]$V2)), 2)
    return.counts <-as.numeric(as.vector(ret$V2[(n-2048):n]))
    return.cps <- return.counts/return.live.time
    
    spectra.frame <- data.frame(Energy=return.energy, CPS=return.cps, Spectrum=filename)
    spectra.frame <- spectra.frame[complete.cases(spectra.frame),]
    
    return(spectra.frame)
}


promote_first_row_to_header <- function(df) {
  if (nrow(df) < 1L)
    stop("Data frame has no rows to promote as header.")

  names(df) <- as.character(unlist(df[1, ]))
  df <- df[-1, , drop = FALSE]
  rownames(df) <- NULL
  df
}

csvFrameSimple <- function(ret=NULL, filepath, filename=NULL, use_native_calibration=TRUE){
    if(is.null(filename)){
        filename <- as.character(basename(filepath))
    }
    filename <- make.names(filename)
    filename <- gsub(".csv", "", filename, ignore.case=TRUE)
    
    if(is.null(ret)){
        ret <- read.csv(file=filepath, sep=",", header=TRUE)
    } else {
        ret <- promote_first_row_to_header(ret)
    }
    
    colnames(ret) <- c("Energy", "CPS")
    ret$Spectrum <- filename
    ret$Energy <- as.numeric(ret$Energy)
    ret$CPS <- as.numeric(ret$CPS)
    
    if(use_native_calibration==FALSE){
        ret$Energy <- seq(1, nrow(ret), 1)
    }

    return(ret)
}

csvFrame <- function(filepath, filename=NULL, use_native_calibration=TRUE){
    ret <- read.csv(file=filepath, sep=",", header=FALSE)
    if(ret[1,1]=="Energy (keV)"){
        csvFrameSimple(ret=ret, filepath=filepath, filename=filename, use_native_calibration=use_native_calibration)
    } else {
        csvFrameOG(ret=ret, filepath=filepath, filename=filename, use_native_calibration=use_native_calibration)
    }
}


csvFrameMetadata <- function(filepath, filename=NULL){
    if(is.null(filename)){
        filename <- as.character(basename(filepath))
    }
    filename <- make.names(filename)
    filename <- gsub(".csv", "", filename, ignore.case=TRUE)
    
    ret <- read.csv(file=filepath, sep=",", header=FALSE)
    
    result <- data.frame(Spectrum=filename, eVCh=as.numeric(as.vector(ret[ret$V1 %in% "eV per channel",]$V2))/1000, LiveTime=round(as.numeric(as.vector(ret[ret$V1 %in% "Live Time",]$V2)), 2))
    
    return(result)
}

fullSpectraDataTableProcess <- function(inFile=NULL, gainshiftvalue=0){
    
            
        if (is.null(inFile)) return(NULL)
        temp = inFile$name
        temp <- gsub(".csv", "", temp)
        id.seq <- seq(1, 2048,1)
        
        n <- length(temp)*id.seq
        
        myfiles.x = pblapply(inFile$datapath, read_csv_filename_x)
        myfiles.y = pblapply(inFile$datapath, read_csv_filename_y)
        
        xrf.x <- data.frame(id.seq, myfiles.x)
        colnames(xrf.x) <- c("ID", temp)
        xrf.y <- data.frame(id.seq, myfiles.y)
        colnames(xrf.y) <- c("ID", temp)
        
        xrf.x <- data.table(xrf.x)
        xrf.y <- data.table(xrf.y)
        
        energy.m <- xrf.x[, list(variable = names(.SD), value = unlist(.SD, use.names = F)), by = ID]
        cps.m <- xrf.y[, list(variable = names(.SD), value = unlist(.SD, use.names = F)), by = ID]
        
        spectra.frame <- data.frame(energy.m$value, cps.m$value, cps.m$variable)
        colnames(spectra.frame) <- c("Energy", "CPS", "Spectrum")
        data <- spectra.frame
            
    data$Energy <- data$Energy + gainshiftvalue
    
    return(data)
}

fullSpectraProcess <- function(inFile=NULL, gainshiftvalue=0, use_native_calibration=TRUE){
       
           if (is.null(inFile)) return(NULL)
           temp = inFile$name
           temp <- gsub(".csv", "", temp)
           id.seq <- seq(1, 2048,1)
           
           n <- length(temp)*id.seq
           
           n.seq <- seq(1, nrow(inFile), 1)
           
           data.list <- pblapply(n.seq, function(x) csvFrame(filepath=inFile[x, "datapath"], filename=inFile[x, "name"], use_native_calibration=use_native_calibration))
           data <- as.data.frame(data.table::rbindlist(data.list, use.names=TRUE, fill=TRUE), stringsAsFactors=FALSE)
       
       if(gainshiftvalue>0){
           data$Energy <- data$Energy + gainshiftvalue
       }
       
       
       return(data)
   }

fullSpectraMetadataProcess <- function(inFile=NULL){
       
           if (is.null(inFile)) return(NULL)
           temp = inFile$name
           temp <- gsub(".csv", "", temp)
           id.seq <- seq(1, 2048,1)
           
           n <- length(temp)*id.seq
           
           n.seq <- seq(1, nrow(inFile), 1)
           
           data.list <- pblapply(n.seq, function(x) csvFrameMetadata(filepath=inFile[x, "datapath"], filename=inFile[x, "name"]))
           data <- as.data.frame(data.table::rbindlist(data.list, use.names=TRUE, fill=TRUE), stringsAsFactors=FALSE)
       
       return(data)
   }


library(jsonlite)
library(dplyr)
library(tibble)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# 1) Read CloudCal JSON safely (single object, array of objects, or stream of objects)
safe_cloudcal_load <- function(filepath) {
  # Try the simple case first
  obj <- tryCatch(fromJSON(filepath, simplifyVector = FALSE), error = identity)
  if (!inherits(obj, "error")) {
    # Coerce to a list of "analyses"
    if (is.list(obj) && !is.null(obj$spectra)) return(list(obj))
    if (is.list(obj)) return(obj) # already a list/array of records
  }
  # Fallback: treat file as a stream of JSON objects (NDJSON or comma-separated)
  txt <- readLines(filepath, warn = FALSE)
  txt <- trimws(txt)
  txt <- txt[nzchar(txt)]
  # Heuristic: if we see multiple top-level objects without [ ... ],
  # wrap them into an array and parse.
  if (length(txt)) {
    joined <- paste(txt, collapse = "\n")
    if (!grepl("^\\s*\\[", joined) && grepl("\\}\\s*,?\\s*\\{", joined)) {
      joined <- paste0("[", joined, "]")
    }
    obj2 <- fromJSON(joined, simplifyVector = FALSE)
    if (is.list(obj2) && !is.null(obj2$spectra)) return(list(obj2))
    if (is.list(obj2)) return(obj2)
  }
  stop("Unrecognized JSON layout; could not parse as object, array, or stream.")
}

# 2) Pull all analyses that actually have spectra
.find_spectra_analyses <- function(records) {
  Filter(function(r) is.list(r) && !is.null(r$spectra) && length(r$spectra) > 0, records)
}

# 3) Choose the best analysis for plotting (prefer NORMAL/SAMPLE with spectra)
.choose_primary_analysis <- function(records) {
  cand <- .find_spectra_analyses(records)
  if (!length(cand)) stop("No analyses with spectra found.")
  # Prefer NORMAL/SAMPLE types; else first with spectra
  score <- vapply(cand, function(r) {
    tt <- toupper(as.character(r$analysis$testType %||% ""))
    if (tt %in% c("NORMAL","SAMPLE")) 2L else if (nzchar(tt)) 1L else 0L
  }, integer(1))
  cand[[ which.max(score) ]]
}

# 4) List exposure numbers (union across all analyses with spectra)
get_exposure_numbers <- function(filepath) {
  recs <- safe_cloudcal_load(filepath)
  cand <- .find_spectra_analyses(recs)
  if (!length(cand)) stop("No spectra entries in JSON.")
  exp_vec <- unlist(lapply(cand, function(r) vapply(r$spectra, function(s) as.numeric(s$exposureNumber %||% NA_real_), numeric(1))), use.names = FALSE)
  as.character(sort(unique(na.omit(exp_vec))))
}

# 5) Find first "Sample ID" recursively
.find_sample_id <- function(x) {
  if (is.list(x)) {
    if (!is.null(x[["Sample ID"]])) return(as.character(x[["Sample ID"]]))
    for (nm in names(x)) {
      got <- .find_sample_id(x[[nm]])
      if (!is.null(got)) return(got)
    }
  }
  NULL
}

# 6) Live time helper (be forgiving with key names)
.get_live_time <- function(s) {
  for (k in c("liveTimeFromHardware","liveTime","live_time","live_time_from_hardware")) {
    v <- s[[k]]
    if (!is.null(v)) {
      v <- as.numeric(v)
      if (!is.na(v) && v > 0) return(v)
    }
  }
  NA_real_
}

# 7) Get sample identifier from a record (prefer testInfo['Sample ID'], fallback to tidName)
.get_sample_name <- function(rec, fallback = "Unknown") {
  # First try testInfo$`Sample ID`
  sample_id <- rec$testInfo$`Sample ID`
  if (!is.null(sample_id) && nzchar(as.character(sample_id))) {
    return(as.character(sample_id))
  }
  # Fallback to analysis$tidName
  tid <- rec$analysis$tidName
  if (!is.null(tid) && nzchar(as.character(tid))) {
    return(as.character(tid))
  }
  # Final fallback
  fallback
}

# 8) Read one beam as Energy/CPS/Spectrum from a possibly multi-record file
readJSONFile <- function(filepath, chosen_beam = "1") {
  exposureNumber <- as.integer(chosen_beam)
  if (length(exposureNumber) != 1L || is.na(exposureNumber))
    stop("chosen_beam must be a single integer-like value")

  recs <- safe_cloudcal_load(filepath)
  rec  <- .choose_primary_analysis(recs)

  spectra <- rec$spectra
  if (!is.list(spectra) || !length(spectra)) stop("No spectra found.")

  beam_idx <- which(vapply(spectra, function(s) isTRUE(s$exposureNumber == exposureNumber), logical(1)))
  if (!length(beam_idx)) stop(sprintf("No spectrum with exposureNumber == %s found.", exposureNumber))
  s <- spectra[[beam_idx[1]]]

  counts <- as.numeric(s$data)
  if (!length(counts)) stop("Selected spectrum has no 'data' counts.")

  lt <- .get_live_time(s)
  if (is.na(lt) || lt <= 0) stop("Missing or non-positive live time.")

  e_off   <- as.numeric(s$energyOffset)
  e_slope <- as.numeric(s$energySlope)
  if (is.na(e_off) || is.na(e_slope)) stop("Missing energyOffset/energySlope.")

  ch <- seq_along(counts) - 1L
  tibble(
    Energy   = e_off + e_slope * ch,
    CPS      = counts / lt,
    Spectrum = .get_sample_name(rec, basename(filepath))
  )
}

# 9) Process all records in a JSON file for the chosen beam
# Returns combined data.frame with Energy, CPS, Spectrum columns
readJSONAllRecords <- function(filepath, chosen_beam = "1", filename = NULL) {
  exposureNumber <- as.integer(chosen_beam)
  if (length(exposureNumber) != 1L || is.na(exposureNumber))
    stop("chosen_beam must be a single integer-like value")

  recs <- safe_cloudcal_load(filepath)

  # Find all records with spectra (skip CALCHECK and other non-sample records)
  sample_recs <- Filter(function(r) {
    if (!is.list(r) || is.null(r$spectra) || length(r$spectra) == 0) return(FALSE)
    test_type <- toupper(as.character(r$analysis$testType %||% ""))
    # Include NORMAL and SAMPLE types, exclude CALCHECK
    test_type %in% c("NORMAL", "SAMPLE")
  }, recs)

  if (!length(sample_recs)) {
    stop("No NORMAL/SAMPLE records with spectra found in JSON file.")
  }

  # Process each record
  result_list <- lapply(sample_recs, function(rec) {
    spectra <- rec$spectra

    # Find spectrum with matching exposure number
    beam_idx <- which(vapply(spectra, function(s) isTRUE(s$exposureNumber == exposureNumber), logical(1)))

    # Skip this record if it doesn't have the requested beam
    if (!length(beam_idx)) return(NULL)

    s <- spectra[[beam_idx[1]]]

    counts <- as.numeric(s$data)
    if (!length(counts)) return(NULL)

    lt <- .get_live_time(s)
    if (is.na(lt) || lt <= 0) return(NULL)

    e_off   <- as.numeric(s$energyOffset)
    e_slope <- as.numeric(s$energySlope)
    if (is.na(e_off) || is.na(e_slope)) return(NULL)

    ch <- seq_along(counts) - 1L

    # Get sample name
    sample_name <- .get_sample_name(rec, paste0("Sample_", rec$analysis$testId %||% "Unknown"))

    data.frame(
      Energy   = e_off + e_slope * ch,
      CPS      = counts / lt,
      Spectrum = rep(sample_name, length(counts)),
      stringsAsFactors = FALSE
    )
  })

  # Remove NULL entries (records without the requested beam)
  result_list <- Filter(Negate(is.null), result_list)

  if (!length(result_list)) {
    stop(sprintf("No records found with exposureNumber == %s", exposureNumber))
  }

  # Combine all records
  result <- do.call(rbind, result_list)

  # Make spectrum names unique if there are duplicates
  result$Spectrum <- make.names(result$Spectrum, unique = FALSE)

  return(result)
}

# 10) Process JSON file(s) - wrapper for single or multiple files
readJSONProcess <- function(inFile = NULL, filepath = NULL, chosen_beam = "1") {
  # Handle both inFile (Shiny upload) and filepath (direct path) inputs
  if (!is.null(inFile)) {
    # Shiny file input - can be multiple files
    if (is.data.frame(inFile)) {
      # Multiple files uploaded
      result_list <- lapply(seq_len(nrow(inFile)), function(i) {
        tryCatch(
          readJSONAllRecords(inFile$datapath[i], chosen_beam = chosen_beam, filename = inFile$name[i]),
          error = function(e) {
            warning(paste("Error processing", inFile$name[i], ":", e$message))
            NULL
          }
        )
      })
      result_list <- Filter(Negate(is.null), result_list)
      if (!length(result_list)) stop("No valid JSON data could be processed.")
      result <- do.call(rbind, result_list)
    } else {
      # Single file
      result <- readJSONAllRecords(inFile$datapath, chosen_beam = chosen_beam, filename = inFile$name)
    }
  } else if (!is.null(filepath)) {
    # Direct filepath
    result <- readJSONAllRecords(filepath, chosen_beam = chosen_beam)
  } else {
    stop("Either inFile or filepath must be provided")
  }

  return(result)
}


netCountsProcess <- function(inFile=NULL){
    
        if (is.null(inFile)) return(NULL)
                
        n <- length(inFile$name)
        net.names <- gsub("\\@.*","",inFile$name)
        
        myfiles = pblapply(inFile$datapath,  read_csv_net)
        
        
        myfiles.frame.list <- pblapply(myfiles, data.frame, stringsAsFactors=FALSE)
        nms = unique(unlist(pblapply(myfiles.frame.list, names)))
        myfiles.frame <- as.data.frame(data.table::rbindlist(lapply(myfiles.frame.list, "[", nms), use.names=TRUE, fill=TRUE))
        myfiles.frame <- as.data.frame(sapply(myfiles.frame, as.numeric))
        
        united.frame <- data.frame(net.names, myfiles.frame)
        colnames(united.frame) <- c("Spectrum", names(myfiles.frame))
    
    united.frame <- as.data.frame(united.frame)
    return(united.frame)
    
}

not_all_na <- function(x) any(!is.na(x))
not_any_na <- function(x) all(!is.na(x))

importCSVFrame <- function(filepath, chosen_beam="1"){
    csv_import <- read.csv(filepath, header=F, stringsAsFactors=FALSE)
    
    if(csv_import[1, "V1"]=="Std#"){
        importCSVFrameBasic(filepath=filepath, chosen_beam=chosen_beam)
    } else if(csv_import[1, "V1"]=="sep="){
        importCSVFrameDetailed(csv_import=csv_import, chosen_beam=chosen_beam)
    } else {
        importCSVFrameNaive(csv_import=csv_import[,-1])
    }

}
importCSVFrame <- cmpfun(importCSVFrame)

importCSVFrame <- function(filepath, chosen_beam="1"){
    instrument <- get_instrument_and_beams(filepath)$instrument
    
    if(instrument=="Niton"){
        importNiton(filepath, chosen_beam=chosen_beam)
    } else if(instrument=="Olympus"){
        csv_import <- read.csv(filepath, header=F, stringsAsFactors=FALSE)
        importCSVFrameDetailed(csv_import, chosen_beam=chosen_beam)
    } else {
        csv_import <- read.csv(filepath, header=F, stringsAsFactors=FALSE)
        importCSVFrameNaive(csv_import=csv_import[,-1])
    }

}
importCSVFrame <- cmpfun(importCSVFrame)

importCSVFrameNaive <- function(filepath=NULL, csv_import=NULL){
    if(is.null(csv_import)){
        csv_import <- read.csv(filepath)[,-1]
    }
    
   
    spectra_frame <- reshape2::melt(csv_import, id="Energy")
    colnames(spectra_frame) <- c("Energy", "Spectrum", "CPS")
    return(spectra_frame)
}

importCSVFrameBasic <- function(filepath, chosen_beam="1"){
    csv.frame <- read.csv(filepath)
    metadata <- csv.frame[1,]
    spectra.data <- csv.frame[-1,]
    
    melt.frame <- reshape2::melt(spectra.data, id="Std.")
    data.frame(Energy=as.numeric(as.vector(melt.frame$Std.)), CPS=as.numeric(as.vector(melt.frame$value)), Spectrum=as.vector(melt.frame$variable), stringsAsFactors=FALSE)
}
importCSVFrameBasic <- cmpfun(importCSVFrameBasic)

uniqueBeamsDetailed <- function(csv_import, chosen_beam = "1") {
    # Remove columns where all values are NA
    csv_import <- csv_import %>% select_if(~ !all(is.na(.)))
    
    # Remove the first row if it's not needed (adjust as necessary)
     csv_import <- csv_import[-1, ]
    
    # Get the "Exposure Number" row
    exposure_row <- csv_import %>% filter(V1 == "Exposure Number")
    
    if (nrow(exposure_row) == 0) {
        stop("The row 'Exposure Number' was not found in the data.")
    }
    
    # Get exposure numbers from the exposure row, excluding the first column (V1)
    exposure_numbers <- as.character(unlist(exposure_row[1, -1]))
    
    # Identify columns that match the chosen beam
    matching_columns <- which(exposure_numbers == chosen_beam)
    
    if (length(matching_columns) == 0) {
        stop(paste("No data found for Exposure Number", chosen_beam))
    }
    
    # Keep the first column (V1) and the columns that match the chosen beam
    columns_to_keep <- c(1, matching_columns + 1)  # +1 to account for V1
    filtered_csv_import <- csv_import[, columns_to_keep]
    
    return(filtered_csv_import)
}
uniqueBeamsDetailed <- cmpfun(uniqueBeamsDetailed)

get_instrument_and_beams <- function(filepath) {
  # Read in the first few lines of the file to inspect the header.
  # (Adjust n as needed.)
  header_lines <- readLines(filepath, n = 20)
  
  # Check if any header line contains "Main Range" (Company 1)
  if (any(grepl("Main Range", header_lines))) {
    instrument <- "Niton"
    
    # For Company 1, we assume that the CSV has a header line with names like:
    # keV    418(Main Range)    418(Low Range)    418(High Range)
    #
    df <- read.csv(filepath, sep = ",", nrows = 20)
    
    # The beam names are embedded in the column names (skip the first column "keV").
    # Use a regular expression to extract the string inside the parentheses.
    beam_names <- unique(gsub("\\.", " ", sub("\\.$", "", sub("^[^.]+\\.", "", colnames(df)))))
    beam_names <- beam_names[!beam_names %in% "keV"]
    
  } else if (any(grepl("Exposure Number|exposition", header_lines, ignore.case = TRUE))) {
    instrument <- "Olympus"

    # For Company 2 the metadata is stored in a header block.
    # We need to find the row that begins with "Exposure Number" (English) or
    # "Numéro d'exposition" (French - note typographic apostrophe)
    # (Assuming the file is tab- or comma-delimited)
    df <- read.csv(filepath, sep = ",", header=FALSE, nrows = 20)

    # Find the row where the first column contains exposure number info
    exp_row_index <- which(df[, 1] == "Exposure Number")
    if (length(exp_row_index) == 0) {
      # Try French - use grep for typographic apostrophe compatibility
      exp_row_index <- grep("exposition", df[, 1], ignore.case = TRUE)[1]
    }

    if (length(exp_row_index) == 0 || is.na(exp_row_index)) {
      stop("Could not find a row with 'Exposure Number' or 'exposition' in the first column!")
    }
    
    # Extract that row (as a data frame row)
    exposure_row <- df[exp_row_index, ]

    # Get all unique values from that row, excluding the first column (the label)
    unique_beams <- unique(as.character(unlist(exposure_row[-1])))
    beam_names <- unique_beams[!is.na(unique_beams)]
    
    
  } else {
    stop("Unrecognized file format")
  }
  
  return(list(instrument = instrument, beams = beam_names))
}

uniqueBeams <- function(filepath){
    file_check <- get_instrument_and_beams(filepath)
    file_check$beams
}

pdzBeams <- function(filepath){
    # Returns available beam/spectrum numbers for PDZ files
    # Uses rPDZ::getPDZSpectrumCount to detect number of spectra
    num_spectra <- getPDZSpectrumCount(filepath)
    as.character(seq_len(num_spectra))
}

detect_csv_type <- function(filepath) {
    # Auto-detect CSV format to determine if beam selection is needed
    # Returns list with: type, instrument, needs_beam_selection
    header_lines <- readLines(filepath, n = 20)

    # Niton format (multi-beam) - has "Main Range" in header
    if (any(grepl("Main Range", header_lines))) {
        return(list(type = "aggregate", instrument = "Niton", needs_beam_selection = TRUE))
    }

    # Olympus/Vanta format (multi-beam) - has "Exposure Number" or "exposition"
    if (any(grepl("Exposure Number|exposition", header_lines, ignore.case = TRUE))) {
        return(list(type = "aggregate", instrument = "Olympus", needs_beam_selection = TRUE))
    }

    # Simple/single-spectrum CSV (SciApps, Bruker, generic)
    return(list(type = "simple", instrument = "Generic", needs_beam_selection = FALSE))
}

importNiton <- function(filepath, chosen_beam="Main Range"){
    csv_import <- read.csv(filepath)
    
    beam_cols <- grep(make.names(chosen_beam), names(csv_import), value = TRUE)
    csv_range <- csv_import[, beam_cols, drop = FALSE]
    data_list <- list()
    for(i in beam_cols){
        data_list[[i]] <- data.frame(Spectrum=i, Energy=csv_import$keV, CPS=as.numeric(csv_import[,i]))
    }
    spectra_frame <- as.data.frame(data.table::rbindlist(data_list))
    spectra_frame$Spectrum <- gsub(paste0(".", make.names(chosen_beam), "."), "", spectra_frame$Spectrum)
    spectra_frame
}

importCSVFrameDetailed <- function(csv_import, chosen_beam="1"){
    csv_import <- csv_import %>% select_if(not_all_na)
    csv_import <- csv_import[-1,]

    # Find exposure numbers row (English or French)
    # Note: French uses typographic apostrophe (') so we use grep as fallback
    exp_row_idx <- which(csv_import$V1 %in% c("Exposure Number", "Numéro d'exposition"))
    if(length(exp_row_idx) == 0) {
        # Fallback - look for any row containing "Exposure" or "exposition"
        exp_row_idx <- grep("Exposure Number|exposition", csv_import$V1, ignore.case = TRUE)[1]
    }
    beams <- as.vector((unlist(csv_import[exp_row_idx, -1])))
    unique_beams <- unique(beams)

    # Find data rows - check if first column values can be converted to numeric
    # Data rows either have energy values OR are empty/start with counts
    v1_numeric <- suppressWarnings(as.numeric(as.character(csv_import$V1)))
    data_row_mask <- !is.na(v1_numeric) | (csv_import$V1 == "" | is.na(csv_import$V1))

    # Filter to only rows that look like data (not metadata headers)
    # Data rows are after the header section - find first potential data row
    metadata_keywords <- c("Date", "Time", "Reading", "Exposure", "Energy", "Real Time",
                           "Live Time", "Probe", "Tube", "Filter", "Reset", "Counts",
                           "Rate", "Pressure", "Temperature", "Multiplier", "Step",
                           "Material", "Diameter", "Thickness", "Clip", "Pileup", "Width",
                           "Data", "sep=", "Numéro", "Pente", "Décalage", "Temps", "Sonde",
                           "Tension", "Filtre", "Réinitialiser", "Coups", "Taux", "Pression",
                           "Température", "Superpositions", "Rejets")
    is_metadata <- sapply(csv_import$V1, function(x) any(sapply(metadata_keywords, function(k) grepl(k, x, ignore.case = TRUE))))
    data_row_mask <- data_row_mask & !is_metadata

    csv_frame <- csv_import[data_row_mask, c(TRUE, beams == chosen_beam)]
    csv_index <- csv_import[, c(TRUE, beams == chosen_beam)]

    spectra.data <- as.data.frame(apply(csv_frame, 2, function(x) as.numeric(as.character(x))), stringsAsFactors=FALSE)
    colnames(spectra.data) <- csv_index[3,]
    colnames(spectra.data)[1] <- "Energy"

    # Check if Energy column has valid values or needs to be calculated
    energy_values <- spectra.data$Energy
    has_valid_energy <- !all(is.na(energy_values) | energy_values == 0)

    if (!has_valid_energy) {
        # Energy needs to be calculated from Energy Slope and Energy Offset
        # Look for these in metadata (English and French variants)
        slope_row_idx <- which(csv_import$V1 %in% c("Energy Slope", "Pente de tension", "Pente énergie"))
        offset_row_idx <- which(csv_import$V1 %in% c("Energy Offset", "Décalage de tension", "Décalage énergie"))

        if (length(slope_row_idx) > 0 && length(offset_row_idx) > 0) {
            # Get first non-NA value from the chosen beam columns
            slope_values <- as.numeric(as.character(unlist(csv_import[slope_row_idx[1], -1])))
            offset_values <- as.numeric(as.character(unlist(csv_import[offset_row_idx[1], -1])))

            # Use first valid slope and offset (they should be the same across columns)
            slope <- slope_values[!is.na(slope_values)][1]
            offset <- offset_values[!is.na(offset_values)][1]

            if (!is.na(slope) && !is.na(offset)) {
                # Calculate energy: Energy = Offset + Slope * channel_index (0-based)
                n_channels <- nrow(spectra.data)
                spectra.data$Energy <- offset + slope * (0:(n_channels - 1))
            }
        }
    }

    melt.frame <- reshape2::melt(spectra.data, id="Energy")
    data.frame(Energy=as.numeric(as.vector(melt.frame$Energy)), CPS=as.numeric(as.vector(melt.frame$value)), Spectrum=as.vector(melt.frame$variable), stringsAsFactors=FALSE)
}
importCSVFrameDetailed <- cmpfun(importCSVFrameDetailed)

extract_hitachi_energy_calibration <- function(line) {
  # Match pattern: Energy Calibration: -0.010000 keV 0.025033 keV/channel
  pattern <- "Energy Calibration:\\s*([-+]?[0-9]*\\.?[0-9]+)\\s*keV\\s*([-+]?[0-9]*\\.?[0-9]+)\\s*keV/channel"
  
  m <- regexec(pattern, line)
  matches <- regmatches(line, m)
  
  if (length(matches[[1]]) == 3) {
    intercept <- as.numeric(matches[[1]][2])
    slope <- as.numeric(matches[[1]][3])
    return(list(intercept = intercept, slope = slope))
  } else {
    warning("No valid energy calibration line found.")
    return(NULL)
  }
}

readTXTDataHitachi <- function(file=NULL, filepath, filename=NULL, use_native_calibration=TRUE){
    
        if(is.null(filename)){
            filename <- basename(filepath)
        }
        filename <- make.names(gsub(".txt", "", filename, ignore.case=TRUE))
        
        if(is.null(file)){
             text <- read.table(filepath, sep=",", fill=TRUE, header=FALSE)
         } else {
             text <- file
         }
        channels = seq(1, as.numeric(text[1,1])-1, 1)
        
        energy_cal <- extract_hitachi_energy_calibration(text[find_row_with_string(text, "Energy Calibration: "),])
        
        energies <- if(use_native_calibration==TRUE){
            channels*energy_cal$slope + energy_cal$intercept
        } else {
            channels
        }
        
        counts <- as.numeric(text[2:as.numeric(text[1,1])+1,])
        
        live_time <- as.numeric(gsub(" s", "", gsub("Live Time: ", "", text[as.numeric(find_row_with_string(text, "Live Time:")[1]),])))
        
        live_counts <- counts/live_time
    
    data.frame(Energy=energies, CPS=live_counts, Spectrum=filename)
}
    

readTXTDataOG <- function(file=NULL, filepath, filename=NULL, use_native_calibration=TRUE){
    if(is.null(filename)){
        filename <- basename(filepath)
    }
    filename <- make.names(gsub(".txt", "", filename, ignore.case=TRUE))
   
   if(is.null(file)){
        text <- read.table(filepath, sep=",", fill=TRUE, header=FALSE)
    } else {
        text <- file
    }
    
    channels <- seq(1, length(text$V1)-4, 1)
    counts <- as.numeric(as.character(text$V1[5:length(text$V1)]))
    filename.vector <- rep(filename, length(text$V1)-4)

    energy <- if(use_native_calibration==TRUE){
        channels*as.numeric(substr(gsub("Elin=", "", as.character(text$V1[2])), 1, 4))
    } else if(use_native_calibration==FALSE){
        channels
    }
    
    data.frame(Energy=energy, CPS=counts, Spectrum=filename.vector, stringsAsFactors=FALSE)
    
}
readTXTDataOG <- cmpfun(readTXTDataOG)

readTXTData <- function(filepath, filename=NULL, use_native_calibration=TRUE){

    file <- read.table(filepath, sep=",", fill=TRUE, header=FALSE)

    if(file[1,1]==2048){
        readTXTDataHitachi(file=file, filepath=filepath, filename=filename, use_native_calibration=use_native_calibration)
    } else {
        readTXTDataOG(file=file, filepath=filepath, filename=filename, use_native_calibration=use_native_calibration)
    }
    
}

readTXTProcess <- function(inFile=NULL, gainshiftvalue=0, use_native_calibration=TRUE){
    
        if (is.null(inFile)) return(NULL)
        
        n <- length(inFile$datapath)
        names <- inFile$name
        
        n.seq <- seq(1, nrow(inFile), 1)
        
        data.list <- pblapply(n.seq, function(x) readTXTData(filepath=inFile[x, "datapath"], filename=inFile[x, "name"], use_native_calibration=use_native_calibration))
        data <- as.data.frame(data.table::rbindlist(data.list, use.names=TRUE, fill=TRUE), stringsAsFactors=FALSE)
    
    if(gainshiftvalue>0){
        data$Energy <- data$Energy + gainshiftvalue
    }
    return(data)
    
}

read_csv_net <- function(filepath) {
    
    ret <- read.csv(file=filepath, sep=",", header=TRUE)
    element <- ret$Element
    line <- ret$Line
    net <- ret$Net
    background <- ret$Backgr.
    eline <- paste(element, line, sep="-")
    
    simple.table <- data.frame(net, stringsAsFactors=FALSE)
    colnames(simple.table) <- NULL
    simple.transpose <- as.data.frame(t(simple.table), stringsAsFactors=FALSE)
    colnames(simple.transpose) <- eline
    
    simple.transpose
    
}
read_csv_net <- cmpfun(read_csv_net)


readSPTData <- function(filepath, filename, use_native_calibration=TRUE){
    if(is.null(filename)){
        filename <- basename(filepath)
    }
    filename <- make.names(gsub(".spt", "", filename))
    filename.vector <- rep(filename, 4096)
    
    meta <- paste0(readLines(filepath, n=16),collapse=" ")
    meta.split <- strsplit(meta, " ")
    chan.1 <- as.numeric(meta.split[[1]][32])
    energy.1 <- as.numeric(sub(",", ".", meta.split[[1]][33], fixed = TRUE))
    chan.2 <- as.numeric(meta.split[[1]][34])
    energy.2 <- as.numeric(sub(",", ".", meta.split[[1]][35], fixed = TRUE))
    
    channels <- c(chan.1, chan.2)
    energies <- c(energy.1, energy.2)
    
    energy.cal <- lm(energies~ channels)
    
    time <- as.numeric(meta.split[[1]][17])/1000
    
    raw <- read.table(filepath, skip=16)
    cps <- raw[,1]/time
    newdata <- as.data.frame(seq(1, 4096, 1), stringsAsFactors=FALSE)
    colnames(newdata) <- "channels"
    energy <- if(use_native_calibration==TRUE){
        as.vector(predict.lm(energy.cal, newdata=newdata))
        } else if(use_native_calibration==FALSE){
            seq(1, 4096, 1)
        }
    energy2 <- newdata[,1]*summary(energy.cal)$coef[2]
    spectra.frame <- data.frame(energy, cps, filename.vector, stringsAsFactors=FALSE)
    colnames(spectra.frame) <- c("Energy", "CPS", "Spectrum")
    return(spectra.frame)
}
readSPTData <- cmpfun(readSPTData)

readElioProcess <- function(inFile=NULL, gainshiftvalue=0, use_native_calibration=TRUE){

        if (is.null(inFile)) return(NULL)
        
        n <- length(inFile$datapath)
        names <- inFile$name
        
        n.seq <- seq(1, nrow(inFile), 1)
        
        data.list <- pblapply(n.seq, function(x) readSPTData(filepath=inFile[x, "datapath"], filename=inFile[x, "name"], use_native_calibration=use_native_calibration))
        data <- as.data.frame(data.table::rbindlist(data.list, use.names=TRUE, fill=TRUE), stringsAsFactors=FALSE)
    
    if(gainshiftvalue>0){
        data$Energy <- data$Energy + gainshiftvalue
    }
    return(data)
}

# Parse DFL (Itrax settings) file for energy calibration parameters
# Returns list with slope (keV/channel) and offset (keV)
parseDFLFile <- function(dfl_path) {
    if (is.null(dfl_path) || !file.exists(dfl_path)) {
        return(list(slope = NULL, offset = NULL))
    }

    lines <- readLines(dfl_path, warn = FALSE)

    # Look for "keV/channel 2" and "energy offset 2" in [detector parameters] section
    slope <- NULL
    offset <- NULL

    for (line in lines) {
        line <- trimws(line)
        if (grepl("^keV/channel\\s*2\\s*=", line, ignore.case = TRUE)) {
            slope <- as.numeric(gsub(".*=\\s*", "", line))
        } else if (grepl("^energy\\s*offset\\s*2\\s*=", line, ignore.case = TRUE)) {
            offset <- as.numeric(gsub(".*=\\s*", "", line))
        }
    }

    list(slope = slope, offset = offset)
}

# Parse SPE file header to extract metadata
# Returns list with E0 (keV offset), E1 (keV/channel), livetime, and other metadata
parseSPEHeader <- function(filepath) {
    # Read first line which contains tab-separated header
    header_line <- readLines(filepath, n = 1, warn = FALSE)
    header_parts <- strsplit(header_line, "\t")[[1]]

    # Parse key-value pairs from header
    result <- list(E0 = 0, E1 = 1, livetime = 1, n_channels = 2048)

    i <- 1
    while (i < length(header_parts)) {
        key <- trimws(header_parts[i])
        if (i + 1 <= length(header_parts)) {
            value <- trimws(header_parts[i + 1])

            if (key == "E0") {
                result$E0 <- as.numeric(value)
            } else if (key == "E1") {
                result$E1 <- as.numeric(value)
            } else if (key == "livetime") {
                result$livetime <- as.numeric(value)
            } else if (key == "number_mca_channels") {
                result$n_channels <- as.integer(as.numeric(value))
            }
        }
        i <- i + 1
    }

    result
}

# Native SPE file reader - reads Itrax SPE files without itraxR dependency
readSPEData <- function(filepath, filename = NULL, dfl_path = NULL) {
    if (is.null(filename)) {
        filename <- basename(filepath)
    }
    filename <- make.names(gsub("\\.spe$", "", filename, ignore.case = TRUE))

    # Parse header for metadata
    header <- parseSPEHeader(filepath)
    livetime <- header$livetime
    if (is.na(livetime) || livetime <= 0) livetime <- 1

    # Get energy calibration - prefer DFL file if provided, otherwise use SPE header values
    dfl_params <- parseDFLFile(dfl_path)
    if (!is.null(dfl_params$slope) && !is.na(dfl_params$slope)) {
        slope <- dfl_params$slope
        offset <- if (!is.null(dfl_params$offset) && !is.na(dfl_params$offset)) dfl_params$offset else 0
    } else {
        # Use SPE header values (E0 = offset, E1 = slope in keV/channel)
        slope <- header$E1
        offset <- header$E0
    }

    # Read the full file to get spectral data
    # Data starts on line 2 (after header) with format: channel\tcounts
    all_lines <- readLines(filepath, warn = FALSE)

    # Skip header line, parse data lines
    data_lines <- all_lines[-1]

    # Parse channel and counts
    parsed <- lapply(data_lines, function(line) {
        parts <- strsplit(trimws(line), "\t")[[1]]
        if (length(parts) >= 2) {
            channel <- as.numeric(parts[1])
            counts <- as.numeric(parts[2])
            return(c(channel, counts))
        }
        return(c(NA, NA))
    })

    parsed_matrix <- do.call(rbind, parsed)
    channels <- parsed_matrix[, 1]
    counts <- parsed_matrix[, 2]

    # Remove any NA values
    valid <- !is.na(channels) & !is.na(counts)
    channels <- channels[valid]
    counts <- counts[valid]

    # Calculate energy from channel using calibration: Energy = offset + slope * channel
    # Note: Some formats use 0-indexed channels, some 1-indexed
    energy <- offset + slope * (channels - 1)

    # Calculate CPS (counts per second)
    cps <- counts / livetime

    # Create result data frame
    n_channels <- length(energy)
    filename_vector <- rep(filename, n_channels)

    results <- data.frame(
        Energy = energy,
        CPS = cps,
        Spectrum = filename_vector,
        stringsAsFactors = FALSE
    )

    return(results)
}

# Process multiple SPE files
readSPEProcess <- function(inFile = NULL, inEn = NULL) {
    if (is.null(inFile)) return(NULL)

    n.seq <- seq_len(nrow(inFile))

    # DFL path (can be NULL - will use SPE header calibration)
    dfl_path <- if (!is.null(inEn) && !is.null(inEn$datapath)) inEn$datapath else NULL

    data.list <- pblapply(n.seq, function(x) {
        tryCatch(
            readSPEData(
                filepath = inFile[x, "datapath"],
                filename = inFile[x, "name"],
                dfl_path = dfl_path
            ),
            error = function(e) {
                warning(sprintf("Failed to read SPE file %s: %s", inFile[x, "name"], e$message))
                NULL
            }
        )
    })

    # Remove NULL entries (failed reads)
    data.list <- Filter(Negate(is.null), data.list)

    if (length(data.list) == 0) {
        warning("No SPE files were successfully read")
        return(NULL)
    }

    data <- as.data.frame(data.table::rbindlist(data.list, use.names = TRUE, fill = TRUE), stringsAsFactors = FALSE)

    return(data)
}

readMCAData4096 <- function(filepath, filename=NULL, full=NULL, use_native_calibration=TRUE){
    if(is.null(filename)){
        filename <- basename(filepath)
    }
    filename <- make.names(gsub(".mca", "", filename))
    filename.vector <- rep(filename, 4096)
    
    if(is.null(full)){
        full <- read.csv(filepath, row.names=NULL)
    }
    
    chan.1.a.pre <- as.numeric(unlist(strsplit(gsub("# Calibration1: ", "", full[13,1]), " ")))
    chan.1.b.pre <- as.numeric(full[13,2])
    chan.2.a.pre <- as.numeric(unlist(strsplit(gsub("# Calibration2: ", "", full[14,1]), " ")))
    chan.2.b.pre <- as.numeric(full[14,2])

    
    chan.1 <- chan.1.a.pre[1]
    energy.1 <- chan.1.a.pre[2] + chan.1.b.pre/(10^nchar(chan.1.b.pre))
    chan.2 <- chan.2.a.pre[1]
    energy.2 <- chan.2.a.pre[2] + chan.2.b.pre/(10^nchar(chan.2.b.pre))
    
    channels <- c(chan.1, chan.2)
    energies <- c(energy.1, energy.2)
    
    energy.cal <- lm(energies~channels)
    
    time.1 <- as.numeric(gsub("# Live time: ", "", full[10,1], " "))
    time.2 <- as.numeric(full[10,2])
    time <- time.1 + time.2/(10^nchar(time.2))
    
    cps <- as.numeric(full[17:4112, 1])/time
    newdata <- as.data.frame(seq(1, 4096, 1), stringsAsFactors=FALSE)
    colnames(newdata) <- "channels"
    energy <- if(use_native_calibration==TRUE){
        as.vector(predict.lm(energy.cal, newdata=newdata))
    } else if(use_native_calibration==FALSE){
        seq(1, 4096, 1)
    }
    energy2 <- newdata[,1]*summary(energy.cal)$coef[2]
    spectra.frame <- data.frame(energy, cps, filename.vector, stringsAsFactors=FALSE)
    colnames(spectra.frame) <- c("Energy", "CPS", "Spectrum")
    return(spectra.frame)
}
readMCAData4096 <- cmpfun(readMCAData4096)

readPMCAData4096 <- function(filepath, filename=NULL, full=NULL, use_native_calibration=TRUE){
    if(is.null(filename)){
        filename <- basename(filepath)
    }
    filename <- make.names(gsub(".mca", "", filename))
    filename.vector <- rep(filename, 4096)
    
    if(is.null(full)){
        full <- read.csv(filepath, row.names=NULL)
    }
    
    if("LABEL - keV" %in% full[,1]){
        chan.a <- as.numeric(strsplit(full[find_row_with_string(full, "LABEL - keV")+1,], " ")[[1]])
        chan.b <- as.numeric(strsplit(full[find_row_with_string(full, "LABEL - keV")+2,], " ")[[1]])
    
        chan.1 <- chan.a[1]
        energy.1 <- chan.a[2]
        chan.2 <- chan.b[1]
        energy.2 <- chan.b[2]
    
        channels <- c(chan.1, chan.2)
        energies <- c(energy.1, energy.2)
    
        energy.cal <- lm(energies~channels)
    }
    
    time <- remove_na(as.numeric(strsplit(full[find_row_with_string(full, "LIVE_TIME"),], " ")[[1]]))
    if(time==0){
        time <- remove_na(as.numeric(strsplit(full[find_row_with_string(full, "REAL_TIME"),], " ")[[1]]))
    }
    
    cps <- as.numeric(full[(find_row_with_string(full, "<<DATA>>")+1):(find_row_with_string(full, "<<END>>")-1), 1])/time
    newdata <- as.data.frame(seq(1, length(cps), 1), stringsAsFactors=FALSE)
    colnames(newdata) <- "channels"
    if("LABEL - keV" %in% full[,1]){
        energy <- if(use_native_calibration==TRUE){
            as.vector(predict.lm(energy.cal, newdata=newdata))
        } else if(use_native_calibration==FALSE){
            seq(1, length(cps), 1)*0.025001
        }
    } else {
        energy <- seq(1, length(cps), 1)*0.025001
    }
    spectra.frame <- data.frame(energy, cps, filename.vector, stringsAsFactors=FALSE)
    colnames(spectra.frame) <- c("Energy", "CPS", "Spectrum")
    return(spectra.frame)
}
readPMCAData4096 <- cmpfun(readPMCAData4096)

readMinalyzeData4096 <- function(filepath, filename=NULL, full=NULL, use_native_calibration=TRUE){
    if(is.null(filename)){
        filename <- basename(filepath)
    }
    filename <- make.names(gsub(".mca", "", filename))
    filename.vector <- rep(filename, 4096)
    
    if(is.null(full)){
        full <- read.csv(filepath, row.names=NULL)
    }
    
    chan.a <- as.numeric(strsplit(full[find_row_with_string(full, "LABEL - keV")+1,], " ")[[1]])
    chan.b <- as.numeric(strsplit(full[find_row_with_string(full, "LABEL - keV")+2,], " ")[[1]])
    
    chan.1 <- chan.a[1]
    energy.1 <- chan.a[2]
    chan.2 <- chan.b[1]
    energy.2 <- chan.b[2]
    
    channels <- c(chan.1, chan.2)
    energies <- c(energy.1, energy.2)
    
    energy.cal <- lm(energies~channels)
    
    time <- remove_na(as.numeric(strsplit(full[find_row_with_string(full, "LIVE_TIME"),], " ")[[1]]))
    
    cps <- as.numeric(full[(find_row_with_string(full, "<<DATA>>")+1):(find_row_with_string(full, "<<END>>")-1), 1])/time
    newdata <- as.data.frame(seq(1, length(cps), 1), stringsAsFactors=FALSE)
    colnames(newdata) <- "channels"
    energy <- if(use_native_calibration==TRUE){
        as.vector(predict.lm(energy.cal, newdata=newdata))
    } else if(use_native_calibration==FALSE){
        seq(1, length(cps), 1)
    }
    spectra.frame <- data.frame(energy, cps, filename.vector, stringsAsFactors=FALSE)
    colnames(spectra.frame) <- c("Energy", "CPS", "Spectrum")
    return(spectra.frame)
}
readMinalyzeData4096 <- cmpfun(readMinalyzeData4096)

readMCAData2048 <- function(filepath, filename, full=NULL, use_native_calibration=TRUE){
    if(is.null(filename)){
        filename <- basename(filepath)
    }
    
    filename <- make.names(gsub(".mca", "", filename))
    filename.vector <- rep(filename, 2048)
    
    if(is.null(full)){
        full <- read.csv(filepath, row.names=NULL)
    }
    
    evch <- as.numeric(strsplit(full[2,1], " = ")[[1]][2])/1000
    evch_intercept <- as.numeric(strsplit(full[3,1], " = ")[[1]][2])/1000

    time <- as.numeric(strsplit(full[4,1], " = ")[[1]][2])

    cps <- as.numeric(full[13:nrow(full), 1])/time
    energy <- if(use_native_calibration==TRUE){
        seq(1, 2048, 1)*evch + evch_intercept
    } else if(use_native_calibration==FALSE){
        seq(1, 2048, 1)
    }
    spectra.frame <- data.frame(Energy=as.numeric(energy), CPS=as.numeric(cps), Spectrum=as.character(filename.vector), stringsAsFactors=FALSE)
    return(spectra.frame)
    
}
readMCAData2048 <- cmpfun(readMCAData2048)

readPMCAData2048 <- function(filepath, filename=NULL, full=NULL, use_native_calibration=TRUE){
    if(is.null(filename)){
        filename <- basename(filepath)
    }
    filename <- make.names(gsub(".mca", "", filename))
    filename.vector <- rep(filename, 2048)
    
    if(is.null(full)){
        full <- read.csv(filepath, row.names=NULL)
    }
    
    if("LABEL - keV" %in% full[,1]){
        chan.a <- as.numeric(strsplit(full[find_row_with_string(full, "LABEL - keV")+1,], " ")[[1]])
        chan.b <- as.numeric(strsplit(full[find_row_with_string(full, "LABEL - keV")+2,], " ")[[1]])
    
        chan.1 <- chan.a[1]
        energy.1 <- chan.a[2]
        chan.2 <- chan.b[1]
        energy.2 <- chan.b[2]
    
        channels <- c(chan.1, chan.2)
        energies <- c(energy.1, energy.2)
    
        energy.cal <- lm(energies~channels)
    }
    
    time <- remove_na(as.numeric(strsplit(full[find_row_with_string(full, "LIVE_TIME"),], " ")[[1]]))
    if(time==0){
        time <- remove_na(as.numeric(strsplit(full[find_row_with_string(full, "REAL_TIME"),], " ")[[1]]))
    }
    
    cps <- as.numeric(full[(find_row_with_string(full, "<<DATA>>")+1):(find_row_with_string(full, "<<END>>")-1), 1])/time
    newdata <- as.data.frame(seq(1, length(cps), 1), stringsAsFactors=FALSE)
    colnames(newdata) <- "channels"
    if("LABEL - keV" %in% full[,1]){
        energy <- if(use_native_calibration==TRUE){
            as.vector(predict.lm(energy.cal, newdata=newdata))
        } else if(use_native_calibration==FALSE){
            seq(1, length(cps), 1)
        }
    } else {
        energy <- seq(1, length(cps), 1)
    }
    spectra.frame <- data.frame(energy, cps, filename.vector, stringsAsFactors=FALSE)
    colnames(spectra.frame) <- c("Energy", "CPS", "Spectrum")
    return(spectra.frame)
}
readPMCAData2048 <- cmpfun(readPMCAData2048)

readMCAData <- function(filepath, filename=NULL, use_native_calibration=TRUE){
    full <- read.csv(filepath, row.names=NULL)

    spectra.frame <- if(nrow(full)<=3000){
        tryCatch(readMCAData2048(filepath=filepath, filename=filename, full=full, use_native_calibration=use_native_calibration), error=function(e) readPMCAData2048(filepath=filepath, filename=filename, full=full, use_native_calibration=use_native_calibration))
    } else if(nrow(full)>3000){
        if(colnames(full)[1]=="X..PMCA.SPECTRUM.."){
            if("DESCRIPTION - #This file is generated by MinalyzerCS" %in% full[,1]){
            readMinalyzeData4096(filepath=filepath, filename=filename, full=full, use_native_calibration=use_native_calibration)
            } else {
                readPMCAData4096(filepath=filepath, filename=filename, full=full, use_native_calibration=use_native_calibration)
            }
        } else {
            readMCAData4096(filepath=filepath, filename=filename, full=full, use_native_calibration=use_native_calibration)
        }
    }
    
    return(spectra.frame)
    
}
readMCAData <- cmpfun(readMCAData)

readMCAProcess <- function(inFile=NULL, gainshiftvalue=0, use_native_calibration=TRUE){
    
        if (is.null(inFile)) return(NULL)
        
        n <- length(inFile$datapath)
        names <- inFile$name
        
        n.seq <- seq(1, nrow(inFile), 1)
        
        data.list <- pblapply(n.seq, function(x) readMCAData(filepath=inFile[x, "datapath"], filename=inFile[x, "name"], use_native_calibration=use_native_calibration))

        data <- as.data.frame(data.table::rbindlist(data.list, use.names=TRUE, fill=TRUE), stringsAsFactors=FALSE)
            
    if(gainshiftvalue>0){
        data$Energy <- data$Energy + gainshiftvalue
    }
    return(data)
}

readSPXData <- function(filepath, filename, use_native_calibration=TRUE){
    if(is.null(filename)){
        filename <- basename(filepath)
    }
    
    filename <- make.names(gsub(".spx", "", filename))
    filename.vector <- rep(filename, 4096)
    
    xmlfile <- xmlTreeParse(filepath)
    xmllist <- xmlToList(xmlfile)
    channels.pre <- xmllist[["ClassInstance"]][["Channels"]][[1]]
    counts <- as.numeric(strsplit(channels.pre, ",", )[[1]])
    newdata <- as.data.frame(seq(1, 4096, 1), stringsAsFactors=FALSE)
    intercept <- as.numeric(xmllist[["ClassInstance"]][["ClassInstance"]][["CalibAbs"]])
    slope <- as.numeric(xmllist[["ClassInstance"]][["ClassInstance"]][["CalibLin"]])
    time <- as.numeric(xmllist[[2]][["TRTHeaderedClass"]][[3]][["LifeTime"]])/1000
    
    cps <- counts/time
    energy <- if(use_native_calibration==TRUE){
        newdata[,1]*slope+intercept
    } else if(use_native_calibration==FALSE){
        seq(1, 4096, 1)
    }
    
    spectra.frame <- data.frame(energy, cps, filename.vector, stringsAsFactors=FALSE)
    colnames(spectra.frame) <- c("Energy", "CPS", "Spectrum")
    return(spectra.frame)
    
}
readSPXData <- cmpfun(readSPXData)

readSPXProcess <- function(inFile=NULL, gainshiftvalue=0, use_native_calibration=TRUE){
    
        if (is.null(inFile)) return(NULL)
        
        n <- length(inFile$datapath)
        names <- inFile$name
        
        n.seq <- seq(1, nrow(inFile), 1)
        
        data.list <- pblapply(n.seq, function(x) readSPXData(filepath=inFile[x, "datapath"], filename=inFile[x, "name"], use_native_calibration=use_native_calibration))
        data <- as.data.frame(data.table::rbindlist(data.list, use.names=TRUE, fill=TRUE), stringsAsFactors=FALSE)
            
    if(gainshiftvalue>0){
        data$Energy <- data$Energy + gainshiftvalue
    }
    return(data)
}

readPDZ25DataExpiremental <- function(filepath, filename, use_native_calibration=TRUE){
    
    filename <- make.names(gsub(".pdz", "", filename))
    filename.vector <- rep(filename, 2048)
    
    nbrOfRecords <- 3000
    integers <- int_to_unit(readBin(con=filepath, what= "int", n=3000, endian="little"))
    floats <- readBin(con=filepath, what="float", size=4, n=nbrOfRecords, endian="little")
    integer.sub <- integers[124:2171]

    sequence <- seq(1, length(integer.sub), 1)

    time.est <- integers[144]/10

        channels <- sequence
        energy <- if(use_native_calibration==TRUE){
            sequence*.02
        } else if(use_native_calibration==FALSE){
            sequence
        }
        counts <- integer.sub/(integers[144]/10)
        
        unfold(data.frame(Energy=energy, CPS=counts, Spectrum=filename.vector, stringsAsFactors=FALSE))

}
readPDZ25DataExpiremental <- cmpfun(readPDZ25DataExpiremental)


readPDZ24DataExpiremental <- function(filepath, filename, use_native_calibration=TRUE){
    
    filename <- make.names(gsub(".pdz", "", filename))
    filename.vector <- rep(filename, 2048)
    
    nbrOfRecords <- 3000
    integers <- int_to_unit(readBin(con=filepath, what= "int", n=3000, endian="little"))
    floats <- readBin(con=filepath, what="float", size=4, n=nbrOfRecords, endian="little")
    integer.sub <- integers[90:2137]
    sequence <- seq(1, length(integer.sub), 1)
    
    time.est <- integer.sub[21]
    
    channels <- sequence
    energy <- if(use_native_calibration==TRUE){
        sequence*.02
    } else if(use_native_calibration==FALSE){
        sequence
    }    
    counts <- integer.sub/(integer.sub[21]/10)
    
    unfold(data.frame(Energy=energy, CPS=counts, Spectrum=filename.vector, stringsAsFactors=FALSE))
    
}
readPDZ24DataExpiremental <- cmpfun(readPDZ24DataExpiremental)


#Rcpp::sourceCpp("pdz.cpp")

readPDZ25Data <- function(filepath, filename=NULL, pdzprep=TRUE, use_native_calibration=TRUE){
    
    if(is.null(filename)){
        filename <- basename(filepath)
    }
    
    filename <- make.names(gsub(".pdz", "", filename))
    
    integers <- as.vector(readPDZ25(filepath))
    
    sequence <- seq(1, length(integers), 1)
    filename.vector <- rep(filename, length(integers))

    time.est <- integers[21]
    
    evch <- if(pdzprep==TRUE){
        as.numeric(readPDZ25eVCH(filepath))/1000
    } else if(pdzprep==FALSE){
        0.02
    }

    channels <- sequence
    energy <- if(use_native_calibration==TRUE){
        sequence*evch
    } else if(use_native_calibration==FALSE){
        sequence
    }
    counts <- if(pdzprep==TRUE){
        integers/as.numeric(readPDZ25LiveTime(filepath))
    } else if(pdzprep==FALSE){
        integers/(integers[21]/10)
    }
    result <- data.frame(Energy=energy, CPS=counts, Spectrum=filename.vector, stringsAsFactors=FALSE)
    
    final_result <-  if(sum(result$CPS)==0){
        NULL
    } else {
        result
    }
    
    return(result)
    
}
readPDZ25Data <- cmpfun(readPDZ25Data)


readPDZ25DataManual <- function(filepath, filename=NULL, binaryshift, pdzprep=TRUE, use_native_calibration=TRUE){
    
    if(is.null(filename)){
        filename <- basename(filepath)
    }
    
    filename <- make.names(gsub(".pdz", "", filename))
    
    nbrOfRecords <- 2048
    integers <- as.vector(readPDZ25(filepath))
    
    sequence <- seq(1, length(integers), 1)
    filename.vector <- rep(filename, length(integers))

    time.est <- integers[21]
    
    evch <- if(pdzprep==TRUE){
        readPDZ25eVCH(filepath)/1000
    } else if(pdzprep==FALSE){
        0.02
    }
    
    channels <- sequence
    energy <- if(use_native_calibration==TRUE){
        sequence*evch
    } else if(use_native_calibration==FALSE){
        sequence
    }
    counts <- if(pdzprep==TRUE){
        integers/readPDZ25LiveTime(filepath)
    } else if(pdzprep==FALSE){
        integers/(integers[21]/10)
    }
    data.frame(Energy=energy, CPS=counts, Spectrum=filename.vector, stringsAsFactors=FALSE)
    
}
readPDZ25DataManual <- cmpfun(readPDZ25DataManual)


readPDZ24Data<- function(filepath, filename=NULL, pdzprep=TRUE, use_native_calibration=TRUE){
    
    if(is.null(filename)){
        filename <- basename(filepath)
    }
    
    filename <- make.names(gsub(".pdz", "", filename))
    filename.vector <- rep(filename, 2048)
    
    nbrOfRecords <- 2048
    integers <- readPDZ24(filepath)
    sequence <- seq(1, length(integers), 1)
    
    time.est <- integers[21]
    
    evch <- if(pdzprep==TRUE){
        readPDZ24DoubleFetch(filepath, 50)/1000
    } else if(pdzprep==FALSE){
        0.02
    }
    
    channels <- sequence
    energy <- if(use_native_calibration==TRUE){
        sequence*evch
    } else if(use_native_calibration==FALSE){
        sequence
    }
    counts <- if(pdzprep==TRUE){
        integers/as.numeric(readPDZ24FloatFetch(filepath, 354))
    } else if(pdzprep==FALSE){
        integers/(integers[21]/10)
    }
    
    result <- data.frame(Energy=energy, CPS=counts, Spectrum=filename.vector, stringsAsFactors=FALSE)
    result$CPS[result$Energy < 0.6] <- 0
    #final_result <-  if(sum(result$CPS)==0){
    #     NULL
    # } else {
    #     result
    # }
     
     return(result)
    
}
readPDZ24Data <- cmpfun(readPDZ24Data)

readPDZManualData <- function(filepath, filename=NULL, pdzprep=TRUE, use_native_calibration=TRUE, start=362, size=2020){
    
    if(is.null(filename)){
        filename <- basename(filepath)
    }
    
    filename <- make.names(gsub(".pdz", "", filename))
    filename.vector <- rep(filename, 2020)
    
    nbrOfRecords <- 2020
    integers <- readPDZManual(filepath, start=start, size=size)
    sequence <- seq(1, length(integers), 1)
    
    time.est <- integers[21]
    
    evch <- if(pdzprep==TRUE){
        readPDZ24DoubleFetch(filepath, 50)/1000
    } else if(pdzprep==FALSE){
        0.02
    }
    
    channels <- sequence
    energy <- if(use_native_calibration==TRUE){
        sequence*evch
    } else if(use_native_calibration==FALSE){
        sequence
    }
    counts <- if(pdzprep==TRUE){
        integers/as.numeric(readPDZ24FloatFetch(filepath, 354))
    } else if(pdzprep==FALSE){
        integers/(integers[21]/10)
    }
    
    result <- data.frame(Energy=energy, CPS=counts, Spectrum=filename.vector, stringsAsFactors=FALSE)
    result$CPS[result$Energy < 0.6] <- 0
    
    #final_result <-  if(sum(result$CPS)==0){
    #     NULL
    # } else {
    #     result
    # }
     
     return(result)
    
}
readPDZManualData <- cmpfun(readPDZManualData)

readPDZDataUniversal <- function(filepath, filename=NULL, pdzprep=TRUE, use_native_calibration=TRUE, chosen_beam=NULL) {
    # Universal PDZ reader using rPDZ::getPDZDataFrames
    # Handles single, dual, and multi-spectrum files automatically
    # Returns combined data.frame with Energy, CPS, Spectrum columns
    # chosen_beam: NULL for all spectra, or "1", "2", etc. for specific beam

    if(is.null(filename)){
        filename <- basename(filepath)
    }
    filename <- make.names(gsub(".pdz", "", filename, ignore.case=TRUE))

    # Get all spectra from the file (returns list of data.frames with Energy, CPS)
    spectra_list <- getPDZDataFrames(filepath)
    num_spectra <- length(spectra_list)

    # Determine which spectra to process
    if(!is.null(chosen_beam) && chosen_beam != "") {
        beam_idx <- as.integer(chosen_beam)
        if(is.na(beam_idx) || beam_idx < 1 || beam_idx > num_spectra) {
            beam_idx <- 1  # Default to first spectrum if invalid
        }
        indices_to_process <- beam_idx
    } else {
        indices_to_process <- seq_along(spectra_list)
    }

    # Process selected spectrum/spectra
    result_list <- lapply(indices_to_process, function(i) {
        spec_df <- spectra_list[[i]]

        # Handle use_native_calibration option
        energy <- if(use_native_calibration == TRUE) {
            spec_df$Energy
        } else {
            seq(1, nrow(spec_df), 1)  # Just channel numbers
        }

        # CPS is already calculated by getPDZDataFrames (counts/livetime)
        # If pdzprep is FALSE, we'd need raw counts, but getPDZDataFrames always divides
        # For backward compatibility, we keep pdzprep but it's now always TRUE behavior
        cps <- spec_df$CPS

        # Create spectrum name (add suffix for multi-spectrum files when showing all)
        spec_name <- if(num_spectra > 1 && is.null(chosen_beam)) {
            paste0(filename, "_", i)
        } else {
            filename
        }

        data.frame(
            Energy = energy,
            CPS = cps,
            Spectrum = rep(spec_name, length(energy)),
            stringsAsFactors = FALSE
        )
    })

    # Combine all spectra into one data.frame
    result <- do.call(rbind, result_list)

    # Filter low energy noise (same as original functions)
    result$CPS[result$Energy < 0.6] <- 0

    return(result)
}
readPDZDataUniversal <- cmpfun(readPDZDataUniversal)

readPDZData <- function(filepath, filename=NULL, pdzprep=TRUE, use_native_calibration=TRUE, chosen_beam=NULL) {
    # Use the universal reader for all PDZ files
    # This auto-detects format and handles single/dual spectra
    readPDZDataUniversal(filepath, filename=filename, pdzprep=pdzprep, use_native_calibration=use_native_calibration, chosen_beam=chosen_beam)
}
readPDZData <- cmpfun(readPDZData)

singleFileLoader <- function(filepath, filetype=NULL, pdzprep=TRUE, use_native_calibration=TRUE){
                if(is.null(filetype)){
                    filetype <- get_filetype(filepath)
                }
                data <- if(filetype=="CSV"){
                    csvFrame(filepath=filepath, use_native_calibration=use_native_calibration)
                } else if(filetype=="Aggregate CSV File"){
                    importCSVFrameBasic(filepath=filepath)
                } else if(filetype=="TXT"){
                    readTXTData(filepath=filepath, use_native_calibration=use_native_calibration)
                } else if(filetype=="Net"){
                    read_csv_net(filepath=filepath)
                } else if(filetype=="Elio"){
                    readSPTData(filepath=filepath, filename=basename(filepath), use_native_calibration=use_native_calibration)
                }  else if(filetype=="MCA"){
                    readMCAData(filepath=filepath, use_native_calibration=use_native_calibration)
                }  else if(filetype=="SPX"){
                    readSPXData(filepath=filepath, use_native_calibration=use_native_calibration)
                }  else if(filetype=="PDZ"){
                    readPDZData(filepath=filepath, pdzprep=pdzprep, use_native_calibration=use_native_calibration)
                }
                
                
                data <- data[complete.cases(data),]
                data
        }

multipleFileLoader <- function(filepath, filetype=NULL, pdzprep=TRUE, allowParallel=FALSE, use_native_calibration=TRUE){
    data_list <- if(allowParallel==FALSE){
        lapply(filepath, function(x) tryCatch(singleFileLoader(filepath=x, filetype=filetype, pdzprep=pdzprep, use_native_calibration=use_native_calibration), error=function(e) NULL))
    } else if(allowParallel==TRUE){
        parallel::mclapply(filepath, function(x) tryCatch(singleFileLoader(filepath=x, filetype=filetype, pdzprep=pdzprep, use_native_calibration=use_native_calibration), error=function(e) NULL), mc.cores = as.integer(my.cores), mc.silent = TRUE)
    }

    data <- as.data.frame(data.table::rbindlist(data_list, use.names = T, fill = T))
    return(data)
}

multipleFileLoaderCommand <- function(filepath, filetype=NULL, pdzprep=TRUE, allowParallel=FALSE, use_native_calibration=TRUE){
    files <- list.files(path=filepath, ignore.case=TRUE, full.names=FALSE)
    data_list <- if(allowParallel==FALSE){
        lapply(files, function(x) tryCatch(singleFileLoader(filepath=paste0(filepath, x), filetype=filetype, pdzprep=pdzprep, use_native_calibration=use_native_calibration), error=function(e) NULL))
    } else if(allowParallel==TRUE){
        parallel::mclapply(files, function(x) tryCatch(singleFileLoader(filepath=paste0(filepath, x), filetype=filetype, pdzprep=pdzprep, use_native_calibration=use_native_calibration), error=function(e) NULL), mc.cores = as.integer(my.cores), mc.silent = TRUE)
    }

    data <- as.data.frame(data.table::rbindlist(data_list, use.names = T, fill = T))
    return(data)
}

readPDZMetadata <- function(filepath, filename=NULL) {
    # Universal PDZ metadata reader using rPDZ::getPDZMetadata
    # Returns data.frame with one row per spectrum in the file

    if(is.null(filename)){
        filename <- gsub(".pdz", "", basename(filepath))
    }
    filename <- make.names(filename, unique=FALSE)

    # Get comprehensive metadata from rPDZ
    meta <- getPDZMetadata(filepath)

    num_spectra <- meta$spectrum_count

    # Build data.frame with one row per spectrum
    result_list <- lapply(seq_len(num_spectra), function(i) {
        # Create spectrum name (add suffix for multi-spectrum files)
        spec_name <- if(num_spectra > 1) {
            paste0(filename, "_", i)
        } else {
            filename
        }

        # Per-spectrum metadata is in meta$spectra list (0-indexed in C++, but R list is 1-indexed)
        spec_meta <- meta$spectra[[i]]

        data.frame(
            Spectrum = spec_name,
            FormatVersion = meta$format_version,
            SpectrumIndex = i,
            TotalSpectra = num_spectra,
            eVCh = meta$eVCh,
            LiveTime = spec_meta$live_time_s,
            TubeVoltage = spec_meta$tube_voltage_kV,
            stringsAsFactors = FALSE
        )
    })

    metadata <- do.call(rbind, result_list)
    return(metadata)
}
readPDZMetadata <- cmpfun(readPDZMetadata)

readPDZProcess <- function(inFile=NULL, gainshiftvalue=0, advanced=FALSE, binaryshift=100, pdzprep=TRUE, use_native_calibration=TRUE, chosen_beam=NULL){

        if (is.null(inFile)) return(NULL)

        n <- length(inFile$datapath)
        names <- inFile$name

        n.seq <- seq(1, nrow(inFile), 1)

        if(advanced==FALSE){
            data.list <- pblapply(n.seq, function(x) readPDZData(filepath=inFile[x, "datapath"], filename=inFile[x, "name"], pdzprep=pdzprep, use_native_calibration=use_native_calibration, chosen_beam=chosen_beam))
            data <- as.data.frame(data.table::rbindlist(data.list, use.names=TRUE, fill=TRUE), stringsAsFactors=FALSE)
        } else if(advanced==TRUE){
            data.list <- pblapply(n.seq, function(x) readPDZ25DataManual(filepath=inFile[x, "datapath"], filename=inFile[x, "name"], binaryshift=binaryshift, pdzprep=pdzprep, use_native_calibration=use_native_calibration))
            data <- as.data.frame(data.table::rbindlist(data.list, use.names=TRUE, fill=TRUE), stringsAsFactors=FALSE)
        }

   if(gainshiftvalue>0){
       data$Energy <- data$Energy + gainshiftvalue
   }
    return(data)
}

readPDZMetadataProcess <- function(inFile=NULL){
    
        if (is.null(inFile)) return(NULL)
        
        n <- length(inFile$datapath)
        names <- inFile$name
        
        n.seq <- seq(1, nrow(inFile), 1)
        
        data.list <- pblapply(n.seq, function(x) readPDZMetadata(filepath=inFile[x, "datapath"], filename=inFile[x, "name"]))
        data <- as.data.frame(data.table::rbindlist(data.list, use.names=TRUE, fill=TRUE), stringsAsFactors=FALSE)

    return(data)
}

narrowLineTable <- function(spectra, definition.table, elements, gaus_buffer=0.02, allowParallel=TRUE){
    
    #not.elements <- elements[!elements %in% spectralLines]
    #elements <- elements[elements %in% spectralLines]
    #line.data <- elementFrame(data=spectra, elements=elements)

    
    #definition.table <- definition.table[complete.cases(definition.table),]
    #not.line.data <- tryCatch(xrf_parse(range.table = definition.table, data=spectra), error=function(e) NULL)

    
    #if(is.null(not.line.data)){
        #line.data
    #} else if(!is.null(not.line.data)){
        #merge(line.data, not.line.data, by="Spectrum")
    #}
   results <- elementFrame(data=spectra, range.table=definition.table, elements=elements, calculation="gaussian", gaus_buffer=gaus_buffer, allowParallel=allowParallel)
   #for(i in elements){
       #if(any(is.na(results[[i]]))){
         # Replace NA/empty values with 0
         #results[[i]][is.na(results[[i]])] <- 0
       #}
   #}
   results
}

narrowLineTableFirst <- function(spectra, definition.table, elements, gaus_buffer=0.02, allowParallel=TRUE){
    
    #not.elements <- elements[!elements %in% spectralLines]
    #elements <- elements[elements %in% spectralLines]
    #line.data <- elementFrame(data=spectra, elements=elements)

    
    #definition.table <- definition.table[complete.cases(definition.table),]
    #not.line.data <- tryCatch(xrf_parse(range.table = definition.table, data=spectra), error=function(e) NULL)

    
    #if(is.null(not.line.data)){
        #line.data
    #} else if(!is.null(not.line.data)){
        #merge(line.data, not.line.data, by="Spectrum")
    #}
   results <- elementFrame(data=spectra, range.table=definition.table, elements=elements, calculation="first", gaus_buffer=gaus_buffer, allowParallel=allowParallel)
   #for(i in elements){
       #if(any(is.na(results[[i]]))){
         # Replace NA/empty values with 0
         #results[[i]][is.na(results[[i]])] <- 0
       #}
   #}
   results
}

narrowLineTableSecond <- function(spectra, definition.table, elements, gaus_buffer=0.02, allowParallel=TRUE){
    
    #not.elements <- elements[!elements %in% spectralLines]
    #elements <- elements[elements %in% spectralLines]
    #line.data <- elementFrame(data=spectra, elements=elements)

    
    #definition.table <- definition.table[complete.cases(definition.table),]
    #not.line.data <- tryCatch(xrf_parse(range.table = definition.table, data=spectra), error=function(e) NULL)

    
    #if(is.null(not.line.data)){
        #line.data
    #} else if(!is.null(not.line.data)){
        #merge(line.data, not.line.data, by="Spectrum")
    #}
   results <- elementFrame(data=spectra, range.table=definition.table, elements=elements, calculation="second", gaus_buffer=gaus_buffer, allowParallel=allowParallel)
   #for(i in elements){
       #if(any(is.na(results[[i]]))){
         # Replace NA/empty values with 0
         #results[[i]][is.na(results[[i]])] <- 0
       #}
   #}
   results
}

narrowLineTableSplit <- function(spectra, definition.table, elements, split_buffer=0.1, allowParallel=TRUE){
    
    #not.elements <- elements[!elements %in% spectralLines]
    #elements <- elements[elements %in% spectralLines]
    #line.data <- elementFrame(data=spectra, elements=elements)

    
    #definition.table <- definition.table[complete.cases(definition.table),]
    #not.line.data <- tryCatch(xrf_parse(range.table = definition.table, data=spectra), error=function(e) NULL)

    
    #if(is.null(not.line.data)){
        #line.data
    #} else if(!is.null(not.line.data)){
        #merge(line.data, not.line.data, by="Spectrum")
    #}
   results <- elementFrame(data=spectra, range.table=definition.table, elements=elements, calculation="split", split_buffer=split_buffer, allowParallel=allowParallel)
   #for(i in elements){
       #if(any(is.na(results[[i]]))){
         # Replace NA/empty values with 0
         #results[[i]][is.na(results[[i]])] <- 0
       #}
   #}
   results
}

wideLineTable <- function(spectra, definition.table, elements, allowParallel=TRUE){
    
    #not.elements <- elements[!elements %in% spectralLines]
    #elements <- elements[elements %in% spectralLines]
    #line.data <- wideElementFrame(data=spectra, elements=elements)

    
    #definition.table <- definition.table[complete.cases(definition.table),]
    #not.line.data <- tryCatch(xrf_parse(range.table = definition.table, data=spectra), error=function(e) NULL)

    
    #if(is.null(not.line.data)){
    #    line.data
    #} else if(!is.null(not.line.data)){
    #    merge(line.data, not.line.data, by="Spectrum")
    #}
    
    results <- wideElementFrame(data=spectra, range.table=definition.table, elements=elements, calculation="gaussian", allowParallel=allowParallel)
    #for(i in elements){
        #if(any(is.na(results[[i]]))){
          # Replace NA/empty values with 0
          #results[[i]][is.na(results[[i]])] <- 0
        #}
    #}
    results
}

wideLineTableSplit <- function(spectra, definition.table, elements, split_buffer=0.1, allowParallel=TRUE){
    
    #not.elements <- elements[!elements %in% spectralLines]
    #elements <- elements[elements %in% spectralLines]
    #line.data <- wideElementFrame(data=spectra, elements=elements)

    
    #definition.table <- definition.table[complete.cases(definition.table),]
    #not.line.data <- tryCatch(xrf_parse(range.table = definition.table, data=spectra), error=function(e) NULL)

    
    #if(is.null(not.line.data)){
    #    line.data
    #} else if(!is.null(not.line.data)){
    #    merge(line.data, not.line.data, by="Spectrum")
    #}
    
    results <- wideElementFrame(data=spectra, range.table=definition.table, elements=elements, calculation="split", buffer=split_buffer, allowParallel=allowParallel)
    #for(i in elements){
        #if(any(is.na(results[[i]]))){
          # Replace NA/empty values with 0
          #results[[i]][is.na(results[[i]])] <- 0
        #}
    #}
    results
}

###Calibration Loading
calConditionsTable <- function(cal.type=NULL, line.type=NULL, line.structure=NULL, gaus.buffer=NULL, split.buffer=NULL, deconvolution=NULL, decon.sigma=NULL, smooth.width=NULL, smooth.alpha=NULL, smooth.iter=NULL, snip.iter=NULL, compress=NULL, transformation=NULL, dependent.transformation=NULL, energy.range=NULL, norm.type=NULL, norm.min=NULL, norm.max=NULL, compton.type=NULL, foresttry=NULL, forestmetric=NULL, foresttrain=NULL, forestnumber=NULL, cvrepeats=NULL, foresttrees=NULL, neuralhiddenlayers=NULL, neuralhiddenunits=NULL, neuralweightdecay=NULL, neuralmaxiterations=NULL, xgbtype=NULL, treemethod=NULL, treedepth=NULL, droptree=NULL, skipdrop=NULL, xgbalpha=NULL, xgbgamma=NULL, xgbeta=NULL, xgblambda=NULL, xgbsubsample=NULL, xgbcolsample=NULL, xgbminchild=NULL, xgbmaxdeltastep=NULL, xgbscaleposweight=NULL, bartk=NULL, bartbeta=NULL, bartnu=NULL, svmc=NULL, svmdegree=NULL, svmscale=NULL, svmsigma=NULL, svmlength=NULL){
    
    cal.type <- if(is.null(cal.type)){
        1
    } else if(!is.null(cal.type)){
        cal.type
    }
    
    line.type <- if(is.null(line.type)){
        "Narrow"
    } else if(!is.null(line.type)){
        line.type
    }
    
    line.structure <- if(is.null(line.structure)){
        "gaussian"
    } else if(!is.null(line.structure)){
        line.structure
    }
    
    gaus.buffer <- if(is.null(gaus.buffer)){
        0.02
    } else if(!is.null(gaus.buffer)){
        gaus.buffer
    }
    
    split.buffer <- if(is.null(split.buffer)){
        0.1
    } else if(!is.null(split.buffer)){
        split.buffer
    }
    
    deconvolution <- if(is.null(deconvolution)){
        "None"
    } else if(!is.null(deconvolution)){
        deconvolution
    }
    
    smooth.width <- if(is.null(smooth.width)){
        5
    } else if(!is.null(smooth.width)){
        smooth.width
    }
    
    smooth.alpha <- if(is.null(smooth.alpha)){
        2.5
    } else if(!is.null(smooth.alpha)){
        smooth.alpha
    }
    
    decon.sigma <- if(is.null(decon.sigma)){
        0.07
    } else if(!is.null(decon.sigma)){
        decon.sigma
    }
    
    smooth.iter <- if(is.null(smooth.iter)){
        20
    } else if(!is.null(smooth.iter)){
        smooth.iter
    }
    
    snip.iter <- if(is.null(snip.iter)){
        20
    } else if(!is.null(snip.iter)){
        snip.iter
    }
    
    compress <- if(is.null(compress)){
        "100 eV"
    } else if(!is.null(compress)){
        compress
    }
    
    transformation <- if(is.null(transformation)){
        "None"
    } else if(!is.null(transformation)){
        transformation
    }
    
    dependent.transformation <- if(is.null(dependent.transformation)){
        "None"
    } else if(!is.null(dependent.transformation)){
        dependent.transformation
    }
    
    energy.range <- if(is.null(energy.range)){
        "0.7-37"
    } else if(!is.null(energy.range)){
        energy.range
    }
    
    norm.type <- if(is.null(norm.type)){
        2
    } else if(!is.null(norm.type)){
        norm.type
    }
    
    norm.min <- if(is.null(norm.min)){
        11
    } else if(!is.null(norm.min)){
        norm.min
    }
    
    norm.max <- if(is.null(norm.max)){
        11.1
    } else if(!is.null(norm.max)){
        norm.max
    }
    
    compton.type <- if(is.null(compton.type)){
        "Raw"
    } else if(!is.null(compton.type)){
        compton.type
    }
    
    foresttry <- if(is.null(foresttry)){
        5
    } else if(!is.null(foresttry)){
        foresttry
    }
    
    forestmetric <- if(is.null(forestmetric)){
        "MAE"
    } else if(!is.null(forestmetric)){
        forestmetric
    }
    
    foresttrain <- if(is.null(foresttrain)){
        "boot"
    } else if(!is.null(foresttrain)){
        foresttrain
    }
    
    forestnumber <- if(is.null(forestnumber)){
        6
    } else if(!is.null(forestnumber)){
        forestnumber
    }
    
    cvrepeats <- if(is.null(cvrepeats)){
        1
    } else if(!is.null(cvrepeats)){
        cvrepeats
    }
    
    foresttrees <- if(is.null(foresttrees)){
        50
    } else if(!is.null(foresttrees)){
        foresttrees
    }
    
    neuralhiddenlayers <- if(is.null(neuralhiddenlayers)){
        1
    } else if(!is.null(neuralhiddenlayers)){
        neuralhiddenlayers
    }
    
    neuralhiddenunits <- if(is.null(neuralhiddenunits)){
        "1-2"
    } else if(!is.null(neuralhiddenunits)){
        neuralhiddenunits
    }
    
    neuralweightdecay <- if(is.null(neuralweightdecay)){
        "0.3-0.5"
    } else if(!is.null(neuralweightdecay)){
        neuralweightdecay
    }
    
    neuralmaxiterations <- if(is.null(neuralmaxiterations)){
        1000
    } else if(!is.null(neuralmaxiterations)){
        neuralmaxiterations
    }

    treemethod <- if(is.null(treemethod)){
        "auto"
    } else if(!is.null(treemethod)){
        treemethod
    }
    
    treedepth <- if(is.null(treedepth)){
        "5-5"
    } else if(!is.null(treedepth)){
        treedepth
    }
    
    droptree <- if(is.null(droptree)){
        "0.3-0.3"
    } else if(!is.null(droptree)){
        droptree
    } 
    
    skipdrop <- if(is.null(skipdrop)){
        "0.3-0.3"
    } else if(!is.null(skipdrop)){
        skipdrop
    }  
    
    xgbtype <- if(is.null(xgbtype)){
        "Tree"
    } else if(!is.null(xgbtype)){
        xgbtype
    }
    
    xgbalpha <- if(is.null(xgbalpha)){
        "0.1-0.1"
    } else if(!is.null(xgbalpha)){
        xgbalpha
    }
    
    xgbgamma <- if(is.null(xgbgamma)){
        "0-0"
    } else if(!is.null(xgbgamma)){
        xgbgamma
    }
    
    xgbeta <- if(is.null(xgbeta)){
        "0.1-0.1"
    } else if(!is.null(xgbeta)){
        xgbeta
    }
    
    xgblambda <- if(is.null(xgblambda)){
        "0.1-0.1"
    } else if(!is.null(xgblambda)){
        xgblambda
    }
    
    xgbsubsample <- if(is.null(xgbsubsample)){
        "0.6-0.6"
    } else if(!is.null(xgbsubsample)){
        xgbsubsample
    }
    
    xgbcolsample <- if(is.null(xgbcolsample)){
        "0.6-0.6"
    } else if(!is.null(xgbcolsample)){
        xgbcolsample
    }
    
    xgbminchild <- if(is.null(xgbminchild)){
        1
    } else if(!is.null(xgbminchild)){
        xgbminchild
    }

    xgbmaxdeltastep <- if(is.null(xgbmaxdeltastep)){
        0
    } else if(!is.null(xgbmaxdeltastep)){
        xgbmaxdeltastep
    }

    xgbscaleposweight <- if(is.null(xgbscaleposweight)){
        0
    } else if(!is.null(xgbscaleposweight)){
        xgbscaleposweight
    }
    
    bartk <- if(is.null(bartk)){
        "95-95"
    } else if(!is.null(bartk)){
        bartk
    }
    
    bartbeta <- if(is.null(bartbeta)){
        "2-2"
    } else if(!is.null(bartbeta)){
        bartbeta
    }
    
    bartnu <- if(is.null(bartnu)){
        "2-2"
    } else if(!is.null(bartnu)){
        bartnu
    }
    
    svmc <- if(is.null(svmc)){
        "2-2"
    } else if(!is.null(svmc)){
        svmc
    }
    
    svmdegree <- if(is.null(svmdegree)){
        "2-2"
    } else if(!is.null(svmdegree)){
        svmdegree
    }
    
    svmscale <- if(is.null(svmscale)){
        "2-2"
    } else if(!is.null(svmscale)){
        svmscale
    }
    
    svmsigma <- if(is.null(svmsigma)){
        "2-2"
    } else if(!is.null(svmsigma)){
        svmsigma
    }
    
    svmlength <- if(is.null(svmlength)){
        "2-2"
    } else if(!is.null(svmlength)){
        svmlength
    }
    
    
    
    cal.table <- data.frame(
                CalType=cal.type,
                LineType=line.type,
                LineStructure=line.structure,
                GausBuffer=gaus.buffer,
                SplitBuffer=split.buffer,
                Deconvolution=deconvolution,
                DeconvolutionSigma=decon.sigma,
                SmoothWidth=smooth.width,
                SmoothAlpha=smooth.alpha,
                SmoothIter=smooth.iter,
                SnipIter=snip.iter,
                Compress=compress,
                Transformation=transformation,
                EnergyRange=energy.range,
                NormType=norm.type,
                Min=norm.min,
                Max=norm.max,
                ComptonType=compton.type,
                DepTrans=dependent.transformation,
                ForestTry=foresttry,
                ForestMetric=forestmetric,
                ForestTC=foresttrain,
                ForestNumber=forestnumber,
                CVRepeats=cvrepeats,
                ForestTrees=foresttrees,
                NeuralHL=neuralhiddenlayers,
                NeuralHU=neuralhiddenunits,
                NeuralWD=neuralweightdecay,
                NeuralMI=neuralmaxiterations,
				TreeMethod=treemethod,
                TreeDepth=treedepth,
                DropTree=droptree,
                SkipDrop=skipdrop,
                xgbType=xgbtype,
                xgbAlpha=xgbalpha,
                xgbGamma=xgbgamma,
                xgbEta=xgbeta,
                xgbLambda=xgblambda,
                xgbSubSample=xgbsubsample,
                xgbColSample=xgbcolsample,
                xgbMinChild=xgbminchild,
                xgbMaxDeltaStep=xgbmaxdeltastep,
                xgbScalePosWeight=xgbscaleposweight,
                bartK=bartk,
                bartBeta=bartbeta,
                bartNu=bartnu,
                svmC=svmc,
                svmDegree=svmdegree,
                svmScale=svmscale,
                svmSigma=svmsigma,
                svmLength=svmlength,
                stringsAsFactors=FALSE)
                    
        return(cal.table)
}


calConditionsList <- function(cal.type=NULL, line.type=NULL, line.structure=NULL, gaus.buffer=NULL, split.buffer=NULL, deconvolution=NULL, decon.sigma=NULL, smooth.width=NULL, smooth.alpha=NULL, smooth.iter=NULL, snip.iter=NULL, compress=NULL, transformation=NULL, dependent.transformation=NULL, energy.range=NULL, norm.type=NULL, norm.min=NULL, norm.max=NULL, compton.type=NULL, foresttry=NULL, forestmetric=NULL, foresttrain=NULL, forestnumber=NULL, cvrepeats=NULL, foresttrees=NULL, neuralhiddenlayers=NULL, neuralhiddenunits=NULL, neuralweightdecay=NULL, neuralmaxiterations=NULL, treemethod=NULL, treedepth=NULL, droptree=NULL, skipdrop=NULL, xgbtype=NULL, xgbalpha=NULL, xgbgamma=NULL, xgbeta=NULL, xgblambda=NULL, xgbsubsample=NULL, xgbcolsample=NULL, xgbminchild=NULL, xgbmaxdeltastep=NULL, xgbscaleposweight=NULL, bartk=NULL, bartbeta=NULL, bartnu=NULL, svmc=NULL, svmdegree=NULL, svmscale=NULL, svmsigma=NULL, svmlength=NULL, use.standards=TRUE, slopes=NULL, intercept=NULL, scale=NULL){
    
    cal.table <- data.frame(
                CalType=cal.type,
                LineType=line.type,
                LineStructure=line.structure,
                GausBuffer=gaus.buffer,
                SplitBuffer=split.buffer,
                Deconvolution=deconvolution,
                DeconvolutionSigma=decon.sigma,
                SmoothWidth=smooth.width,
                SmoothAlpha=smooth.alpha,
                SmoothIter=smooth.iter,
                SnipIter=snip.iter,
                Compress=compress,
                Transformation=transformation,
                EnergyRange=energy.range,
                NormType=norm.type,
                Min=norm.min,
                Max=norm.max,
                ComptonType=compton.type,
                DepTrans=dependent.transformation,
                ForestTry=foresttry,
                ForestMetric=forestmetric,
                ForestTC=foresttrain,
                ForestNumber=forestnumber,
                CVRepeats=cvrepeats,
                ForestTrees=foresttrees,
                NeuralHL=neuralhiddenlayers,
                NeuralHU=neuralhiddenunits,
                NeuralWD=neuralweightdecay,
                NeuralMI=neuralmaxiterations,
				TreeMethod=treemethod,
                TreeDepth=treedepth,
                DropTree=droptree,
                SkipDrop=skipdrop,
                xgbType=xgbtype,
                xgbAlpha=xgbalpha,
                xgbGamma=xgbgamma,
                xgbEta=xgbeta,
                xgbLambda=xgblambda,
                xgbSubSample=xgbsubsample,
                xgbColSample=xgbcolsample,
                xgbMinChild=xgbminchild,
				 xgbMaxDeltaStep=xgbmaxdeltastep,
				 xgbScalePosWeight=xgbscaleposweight,
                bartK=bartk,
                bartBeta=bartbeta,
                bartNu=bartnu,
                svmC=svmc,
                svmDegree=svmdegree,
                svmScale=svmscale,
                svmSigma=svmsigma,
                svmLength=svmlength,
                stringsAsFactors=FALSE)
                
                cal.mode.list <- list(
                    CalTable=cal.table,
                    Slope=slopes,
                    Intercept=intercept,
                    StandardsUsed=use.standards,
                    Scale=scale)
                    
        return(cal.mode.list)
}

calConditionCompare <- function(cal.conditions.first, cal.conditions.second){
    
    cal.table.match <- identical(cal.conditions.first$CalTable, cal.conditions.second$CalTable)
    use.standards.match <- identical(cal.conditions.first$StandardsUsed, cal.conditions.second$StandardsUsed)
    all(cal.table.match, use.standards.match)
}

deleteCalConditions <- function(element, number.of.standards){
    cal.condition <- as.numeric(3)
    line.type=as.character("Narrow")
    line.structure <- if(element %in% spectralLines){
        as.character("gaussian")
    } else if(!element %in% spectralLines){
        as.character("gaussian")
    }
    gaus.buffer <- 0.02
    split.buffer <- 0.1
    deconvolution <- "None"
    decon.sigma <- 0.07
    smooth.width <- 5
    smooth.alpha <- 2.5
    smooth.iter <- 20
    snip.iter <- 20
    compress <- as.character("100 eV")
    transformation <- as.character("None")
    energy.range <- as.character("0.7-37")
    norm.condition <- as.numeric(1)
    norm.min <- as.numeric(11)
    norm.max <- as.numeric(11.2)
    compton.type <- as.character("Raw")
    dependent.transformation <- as.character("None")
    
    foresttry <- as.numeric(7)
    forestmetric <- as.character("MAE")
    foresttrain <- as.character("boot")
    forestnumber <- as.numeric(10)
    cvrepeats <- as.numeric(1)
    foresttrees <- as.numeric(100)
    neuralhiddenlayers <- as.numeric(1)
    neuralhiddenunits <- paste0(1, "-", 4)
    neuralweightdecay <- paste0(0.1, "-", 0.5)
    neuralmaxiterations <- as.numeric(1000)
	treemethod <- as.character("auto")
    xgbtype <- as.character("Tree")
    treedepth <- as.character("5-5")
    droptree <- as.character("0.3-0.3")
    skipdrop <- as.character("0.3-0.3")
    xgbalpha <- as.character("0.1-0.1")
    xgbgamma <- as.character("0-0")
    xgbeta <- as.character("0.1-0.1")
    xgblambda <- as.character("0.1-0.1")
    xgbsubsample <- as.character("0.6-0.6")
    xgbcolsample <- as.character("0.6-0.6")
    xgbminchild <- as.numeric(1)
	xgbmaxdeltastep <- as.numeric(0)
	xgbscaleposweight <- as.numeric(0)
    bartk <- as.character("95-95")
    bartbeta <- as.character("2-2")
    bartnu <- as.character("2-2")
    svmc <- as.character("2-2")
    svmdegree <- as.character("2-2")
    svmscale <- as.character("2-2")
    svmsigma <- as.character("2-2")
    svmlength <- as.character("2-2")

    cal.table <- data.frame(
    CalType=cal.condition,
    LineType=line.type,
    LineStructure=line.structure,
    GausBuffer=gaus.buffer,
    SplitBuffer=split.buffer,
    Deconvolution=deconvolution,
    Compress=compress,
    Transformation=transformation,
    EnergyRange=energy.range,
    NormType=norm.condition,
    Min=norm.min,
    Max=norm.max,
    ComptonType=compton.type,
    DepTrans=dependent.transformation,
    ForestTry=foresttry,
    ForestMetric=forestmetric,
    ForestTC=foresttrain,
    ForestNumber=forestnumber,
    CVRepeats=cvrepeats,
    ForestTrees=foresttrees,
    NeuralHL=neuralhiddenlayers,
    NeuralHU=neuralhiddenunits,
    NeuralWD=neuralweightdecay,
    NeuralMI=neuralmaxiterations,
	TreeMethod=treemethod,
    TreeDepth=treedepth,
    DropTree=droptree,
    SkipDrop=skipdrop,
    xgbType=xgbtype,
    xgbAlpha=xgbalpha,
    xgbGamma=xgbgamma,
    xgbEta=xgbeta,
    xgbLambda=xgblambda,
    xgbSubSample=xgbsubsample,
    xgbColSample=xgbcolsample,
    xgbMinChild=xgbminchild,
	xgbMaxDeltaStep=xgbmaxdeltastep,
	xgbScalePosWeight=xgbscaleposweight,
    bartK=bartk,
    bartBeta=bartbeta,
    bartNu=bartnu,
    svmC=svmc,
    svmDegree=svmdegree,
    svmScale=svmscale,
    svmSigma=svmsigma,
    svmLength=svmlength,
    Delete=TRUE,
    stringsAsFactors=FALSE)
    
    slope.corrections <- element
    
    intercept.corrections <- NULL
    
    standards.used <- rep(TRUE, number.of.standards)
    
    scale <- list(Min=0, Max=1)
    
    #standards.used <- vals$keeprows
    
    cal.mode.list <- list(CalTable=cal.table, Slope=slope.corrections, Intercept=intercept.corrections, StandardsUsed=standards.used, Scale=scale)
    return(cal.mode.list)
}

defaultCalConditions <- function(element, number.of.standards){
    cal.condition <- as.numeric(3)
    line.type=as.character("Narrow")
    line.structure <- if(element %in% spectralLines){
        as.character("gaussian")
    } else if(!element %in% spectralLines){
        as.character("gaussian")
    }
    gaus.buffer=0.02
    split.buffer=0.1
    deconvolution="None"
    decon.sigma <- 0.07
    smooth.width <- 5
    smooth.alpha <- 2.5
    smooth.iter <- 20
    snip.iter <- 20
    compress <- as.character("100 eV")
    transformation <- as.character("None")
    energy.range <- as.character("0.7-37")
    norm.condition <- as.numeric(1)
    norm.min <- as.numeric(11)
    norm.max <- as.numeric(11.2)
    compton.type <- as.character("Raw")
    dependent.transformation <- as.character("None")

    foresttry <- as.numeric(7)
    forestmetric <- as.character("MAE")
    foresttrain <- as.character("boot")
    forestnumber <- as.numeric(10)
    cvrepeats <- as.numeric(1)
    foresttrees <- as.numeric(100)
    neuralhiddenlayers <- as.numeric(1)
    neuralhiddenunits <- paste0(1, "-", 4)
    neuralweightdecay <- paste0(0.1, "-", 0.5)
    neuralmaxiterations <- as.numeric(1000)
    xgbtype <- as.character("Tree")
	treemethod <- "auto"
    treedepth <- as.character("5-5")
    droptree=as.character("0.3-0.3")
    skipdrop=as.character("0.3-0.3")
    xgbalpha <- as.character("0.1-0.1")
    xgbgamma <- as.character("0-0")
    xgbeta <- as.character("0.1-0.1")
    xgblambda <- as.character("0.1-0.1")
    xgbsubsample <- as.character("0.6-0.6")
    xgbcolsample <- as.character("0.6-0.6")
    xgbminchild <- as.numeric(1)
	xgbmaxdeltastep <- as.numeric(0)
	xgbscaleposweight <- as.numeric(0)
    bartk <- as.character("95-95")
    bartbeta <- as.character("2-2")
    bartnu <- as.character("2-2")
    svmc <- as.character("2-2")
    svmdegree <- as.character("2-2")
    svmscale <- as.character("2-2")
    svmsigma <- as.character("2-2")
    svmlength <- as.character("2-2")
    
    cal.table <- data.frame(
        CalType=cal.condition,
        LineType=line.type,
        LineStructure=line.structure,
        GausBuffer=gaus.buffer,
        SplitBuffer=split.buffer,
        Deconvolution=deconvolution,
        DeconvolutionSigma=decon.sigma,
        SmoothWidth=smooth.width,
        SmoothAlpha=smooth.alpha,
        SmoothIter=smooth.iter,
        SnipIter=snip.iter,
        Compress=compress,
        Transformation=transformation,
        EnergyRange=energy.range,
        NormType=norm.condition,
        Min=norm.min,
        Max=norm.max,
        ComptonType=compton.type,
        DepTrans=dependent.transformation,
        ForestTry=foresttry,
        ForestMetric=forestmetric,
        ForestTC=foresttrain,
        ForestNumber=forestnumber,
        CVRepeats=cvrepeats,
        ForestTrees=foresttrees,
        NeuralHL=neuralhiddenlayers,
        NeuralHU=neuralhiddenunits,
        NeuralWD=neuralweightdecay,
        NeuralMI=neuralmaxiterations,
		TreeMethod=treemethod,
        TreeDepth=treedepth,
        DropTree=droptree,
        SkipDrop=skipdrop,
        xgbType=xgbtype,
        xgbAlpha=xgbalpha,
        xgbGamma=xgbgamma,
        xgbEta=xgbeta,
        xgbLambda=xgblambda,
        xgbSubSample=xgbsubsample,
        xgbColSample=xgbcolsample,
        xgbMinChild=xgbminchild,
		xgbMaxDeltaStep=xgbmaxdeltastep,
		xgbScalePosWeight=xgbscaleposweight,
        bartK=bartk,
        bartBeta=bartbeta,
        bartNu=bartnu,
        svmC=svmc,
        svmDegree=svmdegree,
        svmScale=svmscale,
        svmSigma=svmsigma,
        svmLength=svmlength,
        stringsAsFactors=FALSE)
    
    slope.corrections <- element
    
    intercept.corrections <- NULL
    
    standards.used <- rep(TRUE, number.of.standards)
    
    scale <- list(Min=0, Max=1)
    
    #standards.used <- vals$keeprows
    
    cal.mode.list <- list(CalTable=cal.table, Slope=slope.corrections, Intercept=intercept.corrections, StandardsUsed=standards.used, Scale=scale)
    return(cal.mode.list)
}

importCalConditionsDetail <- function(element, calList, number.of.standards=NULL){
    
    number.of.standards <- if(is.null(number.of.standards)){
        length(calList[[element]][[1]]$StandardsUsed)
    } else if(!is.null(number.of.standards)){
        number.of.standards
    }
    
    default.cal.conditions <- defaultCalConditions(element=element, number.of.standards=number.of.standards)
    
    imported.cal.conditions <- calList[[element]][[1]]
    
    cal.condition <- if("CalType" %in% colnames(imported.cal.conditions$CalTable)){
         as.numeric(as.character(imported.cal.conditions$CalTable$CalType[1]))
    } else if(!"CalType" %in% colnames(imported.cal.conditions$CalTable)){
       default.cal.conditions$CalTable$CalType
    }
    
    line.condition <- if("LineType" %in% colnames(imported.cal.conditions$CalTable)){
         as.character(imported.cal.conditions$CalTable$LineType[1])
    } else if(!"LineType" %in% colnames(imported.cal.conditions$CalTable)){
       default.cal.conditions$CalTable$LineType
    }
    
    line.structure.condition <- if("LineStructure" %in% colnames(imported.cal.conditions$CalTable)){
         as.character(imported.cal.conditions$CalTable$LineStructure[1])
    } else if(!"LineStructure" %in% colnames(imported.cal.conditions$CalTable)){
       default.cal.conditions$CalTable$LineStructure
    }
    
    gaus.buffer <- if("GausBuffer" %in% colnames(imported.cal.conditions$CalTable)){
         as.numeric(imported.cal.conditions$CalTable$GausBuffer[1])
    } else if(!"GausBuffer" %in% colnames(imported.cal.conditions$CalTable)){
       default.cal.conditions$CalTable$GausBuffer
    }
    
    split.buffer <- if("SplitBuffer" %in% colnames(imported.cal.conditions$CalTable)){
         as.numeric(imported.cal.conditions$CalTable$SplitBuffer[1])
    } else if(!"SplitBuffer" %in% colnames(imported.cal.conditions$CalTable)){
       default.cal.conditions$CalTable$SplitBuffer
    }
    
    deconvolution <- if("Deconvolution" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$Deconvolution[1])
    } else if(!"Deconvolution" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$Deconvolution
    }
    
    decon.sigma <- if("DeconvolutionSigma" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$DeconvolutionSigma[1])
    } else if(!"DeconvolutionSigma" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$DeconvolutionSigma
    }
    
    smooth.width <- if("SmoothWidth" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$SmoothWidth[1])
    } else if(!"SmoothWidth" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$SmoothWidth
    }
    
    smooth.alpha <- if("SmoothAlpha" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$SmoothAlpha[1])
    } else if(!"SmoothAlpha" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$SmoothAlpha
    }
    
    smooth.iter <- if("SmoothIter" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$SmoothIter[1])
    } else if(!"SmoothIter" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$SmoothIter
    }
    
    snip.iter <- if("SnipIter" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$SnipIter[1])
    } else if(!"SnipIter" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$SnipIter
    }
    
    compress.condition <- if("Compress" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$Compress[1])
    } else if(!"Compress" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$Compress
    }
    
    transformation.condition <- if("Transformation" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$Transformation[1])
    } else if(!"Transformation" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$Transformation
    }
    
    energyrange.condition <- if("EnergyRange" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$EnergyRange[1])
    } else if(!"EnergyRange" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$EnergyRange
    }
    
    
    norm.condition <- if("NormType" %in% colnames(imported.cal.conditions$CalTable)){
        as.numeric(as.character(imported.cal.conditions$CalTable$NormType[1]))
    } else if(!"NormType" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$NormType
    }
    
    norm.min <- if("Min" %in% colnames(imported.cal.conditions$CalTable)){
        as.numeric(as.character(imported.cal.conditions$CalTable$Min[1]))
    } else if(!"Min" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$Min
    }
    
    norm.max <- if("Max" %in% colnames(imported.cal.conditions$CalTable)){
        as.numeric(as.character(imported.cal.conditions$CalTable$Max[1]))
    } else if(!"Max" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$Max
    }
    
    compton.type <- if("ComptonType" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$ComptonType[1])
    } else if(!"ComptonType" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$ComptonType
    }
    
    dependent.transformation <- if("DepTrans" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$DepTrans[1])
    } else if(!"DepTrans" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$DepTrans
    }
    
    foresttry <- if("ForestTry" %in% colnames(imported.cal.conditions$CalTable)){
        as.numeric(as.character(imported.cal.conditions$CalTable$ForestTry[1]))
    } else if(!"ForestTry" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$ForestTry
    }
    
    forestmetric <- if("ForestMetric" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$ForestMetric[1])
    } else if(!"ForestMetric" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$ForestMetric
    }
    
    foresttrain <- if("ForestTC" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$ForestTC[1])
    } else if(!"ForestTC" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$ForestTC
    }
    
    forestnumber <- if("ForestNumber" %in% colnames(imported.cal.conditions$CalTable)){
        as.numeric(as.character(imported.cal.conditions$CalTable$ForestNumber[1]))
    } else if(!"ForestNumber" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$ForestNumber
    }
    
    cvrepeats <- if("CVRepeats" %in% colnames(imported.cal.conditions$CalTable)){
        as.numeric(as.character(imported.cal.conditions$CalTable$CVRepeats[1]))
    } else if(!"CVRepeats" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$CVRepeats
    }
    
    foresttrees <- if("ForestTrees" %in% colnames(imported.cal.conditions$CalTable)){
        as.numeric(as.character(imported.cal.conditions$CalTable$ForestTrees[1]))
    } else if(!"ForestTrees" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$ForestTrees
    }
    
    neuralhiddenlayers <- if("NeuralHL" %in% colnames(imported.cal.conditions$CalTable)){
        as.numeric(as.character(imported.cal.conditions$CalTable$NeuralHL[1]))
    } else if(!"NeuralHL" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$NeuralHL
    }
    
    neuralhiddenunits <- if("NeuralHU" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==6 | cal.condition==7){
             paste0(calList[[element]][[2]]$bestTune$size, "-", calList[[element]][[2]]$bestTune$size)
        } else if(!cal.condition==6 | !cal.condition==7){
            as.character(imported.cal.conditions$CalTable$NeuralHU[1])
        }
    } else if(!"NeuralHU" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$NeuralHU
    }
    
    neuralweightdecay <- if("NeuralWD" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==6 | cal.condition==7){
            if(neuralhiddenlayers==1){
                paste0(calList[[element]][[2]]$bestTune$decay, "-", calList[[element]][[2]]$bestTune$decay)
            } else if(neuralhiddenlayers > 1){
                as.character(imported.cal.conditions$CalTable$NeuralWD[1])
            }
        } else if(!cal.condition==6 | !cal.condition==7){
            imported.cal.conditions$CalTable$NeuralWD
        }
    } else if(!"NeuralWD" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$NeuralWD
    }
    
    neuralmaxiterations <- if("NeuralMI" %in% colnames(imported.cal.conditions$CalTable)){
        as.numeric(as.character(imported.cal.conditions$CalTable$NeuralMI[1]))
    } else if(!"NeuralMI" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$NeuralMI
    }
    
    xgbtype <- if("xgbType" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$xgbType[1])
    } else if(!"xgbType" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$xgbType
    }

    treemethod <- if("TreeMethod" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$TreeMethod[1])
    } else if(!"TreeMethod" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$TreeMethod
    }
    
    treedepth <- if("TreeDepth" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==8 | cal.condition==9 && xgbtype=="Tree"){
            paste0(calList[[element]][[2]]$bestTune$max_depth, "-", calList[[element]][[2]]$bestTune$max_depth)
        } else if(!cal.condition==8 | !cal.condition==9 | xgbtype!="Tree"){
            default.cal.conditions$CalTable$TreeDepth
        }
    } else if(!"TreeDepth" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$TreeDepth
    }
    
    droptree <- if("DropTree" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==8 | cal.condition==9 && xgbtype=="Dart"){
            paste0(calList[[element]][[2]]$bestTune$drop_tree, "-", calList[[element]][[2]]$bestTune$drop_tree)
        } else if(!cal.condition==8 | !cal.condition==9 | xgbtype!="Dart"){
            default.cal.conditions$CalTable$DropTree
        }
    } else if(!"DropTree" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$DropTree
    } 
    
    skipdrop <- if("SkipDrop" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==8 | cal.condition==9 && xgbtype=="Dart"){
            paste0(calList[[element]][[2]]$bestTune$skip_drop, "-", calList[[element]][[2]]$bestTune$skip_drop)
        } else if(!cal.condition==8 | !cal.condition==9 | xgbtype!="Dart"){
            default.cal.conditions$CalTable$SkipDrop
        }
    } else if(!"SkipDrop" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$SkipDrop
    }  
    
    xgbalpha <- if("xgbAlpha" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==8 | cal.condition==9 && xgbtype=="Linear"){
            paste0(calList[[element]][[2]]$bestTune$alpha, "-", calList[[element]][[2]]$bestTune$alpha)
        } else if(!cal.condition==8 | !cal.condition==9){
            as.character(imported.cal.conditions$CalTable$xgbAlpha[1])
        }
    } else if(!"xgbAlpha" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$xgbAlpha
    }
    
    xgbgamma <- if("xgbGamma" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==8 | cal.condition==9 && xgbtype!="Linear"){
            paste0(calList[[element]][[2]]$bestTune$gamma, "-", calList[[element]][[2]]$bestTune$gamma)
        } else if(!cal.condition==8 | !cal.condition==9){
            as.character(imported.cal.conditions$CalTable$xgbGamma[1])
        }
    } else if(!"xgbGamma" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$xgbGamma
    }
    
    xgbeta <- if("xgbEta" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==8 | cal.condition==9){
            paste0(calList[[element]][[2]]$bestTune$eta, "-", calList[[element]][[2]]$bestTune$eta)
        } else if(!cal.condition==8 | !cal.condition==9){
            as.character(imported.cal.conditions$CalTable$xgbEta[1])
        }
    } else if(!"xgbEta" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$xgbEta
    }
    
    xgblambda <- if("xgbLambda" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==8 | cal.condition==9 && xgbtype=="Linear"){
            paste0(calList[[element]][[2]]$bestTune$lambda, "-", calList[[element]][[2]]$bestTune$lambda)
        } else if(!cal.condition==8 | !cal.condition==9){
            as.character(imported.cal.conditions$CalTable$xgbLambda[1])
        }
    } else if(!"xgbLambda" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$xgbLambda
    }
    
    xgbsubsample <- if("xgbSubSample" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==8 | cal.condition==9 && xgbtype!="Linear"){
            paste0(calList[[element]][[2]]$bestTune$subsample, "-", calList[[element]][[2]]$bestTune$subsample)
        } else if(!cal.condition==8 | !cal.condition==9){
            as.character(imported.cal.conditions$CalTable$xgbSubSample[1])
        }
    } else if(!"xgbSubSample" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$xgbSubSample
    }
    
    xgbcolsample <- if("xgbColSample" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==8 | cal.condition==9 && xgbtype!="Linear"){
            paste0(calList[[element]][[2]]$bestTune$colsample_bytree, "-", calList[[element]][[2]]$bestTune$colsample_bytree)
        } else if(!cal.condition==8 | !cal.condition==9){
            as.character(imported.cal.conditions$CalTable$xgbColSample[1])
        }
    } else if(!"xgbColSample" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$xgbColSample
    }

    xgbminchild <- if("xgbMinChild" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==8 | cal.condition==9 && xgbtype!="Linear"){
           calList[[element]][[2]]$bestTune$min_child_weight
        } else if(!cal.condition==8 | !cal.condition==9){
            as.numeric(as.character(imported.cal.conditions$CalTable$xgbMinChild[1]))
        }
    } else if(!"xgbMinChild" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$xgbMinChild
    }

    xgbmaxdeltastep <- if("xgbMaxDeltaStep" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==8 | cal.condition==9 && xgbtype!="Linear"){
           calList[[element]][[2]]$bestTune$max_delta_step
        } else if(!cal.condition==8 | !cal.condition==9){
            as.numeric(as.character(imported.cal.conditions$CalTable$xgbMaxDeltaStep[1]))
        }
    } else if(!"xgbMaxDeltaStep" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$xgbMaxDeltaStep
    }

    xgbscaleposweight <- if("xgbScalePosWeight" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==8 | cal.condition==9 && xgbtype!="Linear"){
           calList[[element]][[2]]$bestTune$scale_pos_weight
        } else if(!cal.condition==8 | !cal.condition==9){
            as.numeric(as.character(imported.cal.conditions$CalTable$xgbScalePosWeight[1]))
        }
    } else if(!"xgbScalePosWeight" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$xgbScalePosWeight
    }
    
    bartk <- if("bartK" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==10 | cal.condition==11 && xgbtype=="Tree"){
            paste0(pnorm(as.numeric(calList[[element]][[2]]$bestTune$k)), "-", pnorm(as.numeric(calList[[element]][[2]]$bestTune$k)))
        } else if(!cal.condition==10 | !cal.condition==11){
            as.character(imported.cal.conditions$CalTable$bartK[1])
        }
    } else if(!"bartK" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$bartK
    }
    
    bartbeta <- if("bartBeta" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==10 | cal.condition==11 && xgbtype=="Tree"){
            paste0(calList[[element]][[2]]$bestTune$beta, "-", calList[[element]][[2]]$bestTune$beta)
        } else if(!cal.condition==10 | !cal.condition==11){
            as.character(imported.cal.conditions$CalTable$bartBeta[1])
        }
    } else if(!"bartBeta" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$bartBeta
    }
    
    bartnu <- if("bartNu" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==10 | cal.condition==11 && xgbtype=="Tree"){
            paste0(calList[[element]][[2]]$bestTune$nu, "-", calList[[element]][[2]]$bestTune$nu)
        } else if(!cal.condition==10 | !cal.condition==11){
            as.character(imported.cal.conditions$CalTable$bartNu[1])
        }
    } else if(!"bartNu" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$bartNu
    }
    
    svmc <- if("svmC" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==12 | cal.condition==13){
            if("C" %in% names(calList[[element]][[2]]$bestTune)){
                paste0(calList[[element]][[2]]$bestTune$C, "-", calList[[element]][[2]]$bestTune$C)
            } else {
                as.character(imported.cal.conditions$CalTable$svmC[1])
            }
        } else if(cal.condition==12 | cal.condition==13){
            as.character(imported.cal.conditions$CalTable$svmC[1])
        }
    } else if(!"svmC" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$svmC
    }
    
    svmdegree <- if("svmDegree" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==12 | cal.condition==13 && xgbtype=="Polynomial"){
            if("degree" %in% names(calList[[element]][[2]]$bestTune)){
                paste0(calList[[element]][[2]]$bestTune$degree, "-", calList[[element]][[2]]$bestTune$degree)
            } else {
                as.character(imported.cal.conditions$CalTable$svmDegree[1])
            }
        } else if(!cal.condition==12 | !cal.condition==13){
            as.character(imported.cal.conditions$CalTable$svmDegree[1])
        }
    } else if(!"svmDegree" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$svmDegree
    }
    
    svmscale <- if("svmScale" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==12 | cal.condition==13 && xgbtype=="Polynomial"){
            if("scale" %in% names(calList[[element]][[2]]$bestTune)){
                paste0(calList[[element]][[2]]$bestTune$scale, "-", calList[[element]][[2]]$bestTune$scale)
            } else {
                as.character(imported.cal.conditions$CalTable$svmScale[1])
            }
        } else if(!cal.condition==12 | !cal.condition==13){
            as.character(imported.cal.conditions$CalTable$svmScale[1])
        }
    } else if(!"svmScale" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$svmScale
    }
    
    svmsigma <- if("svmSigma" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==12 | cal.condition==13){
            if(xgbtype=="Radial" | xgbtype=="Radial Cost" | xgbtype=="Radial Sigma"){
                if("sigma" %in% names(calList[[element]][[2]]$bestTune)){
                    paste0(calList[[element]][[2]]$bestTune$sigma, "-", calList[[element]][[2]]$bestTune$sigma)
                } else {
                    as.character(imported.cal.conditions$CalTable$svmSigma[1])
                }
            } else {
                as.character(imported.cal.conditions$CalTable$svmSigma[1])
            }
        } else if(!cal.condition==12 | !cal.condition==13){
            as.character(imported.cal.conditions$CalTable$svmSigma[1])
        }
    } else if(!"svmSigma" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$svmSigma
    }
    
    svmlength <- if("svmLength" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==12 | cal.condition==13){
            if(xgbtype=="Boundrange String"){
                if("length" %in% names(calList[[element]][[2]]$bestTune)){
                    paste0(calList[[element]][[2]]$bestTune$length, "-", calList[[element]][[2]]$bestTune$length)
                } else {
                    as.character(imported.cal.conditions$CalTable$svmLength[1])
                }
            } else {
                as.character(imported.cal.conditions$CalTable$svmLength[1])
            }
        } else if(!cal.condition==12 | !cal.condition==13){
            as.character(imported.cal.conditions$CalTable$svmLength[1])
        }
    } else if(!"svmLength" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$svmLength
    }
    
    slope.corrections <- if("Slope" %in% names(imported.cal.conditions)){
        if(is.null(imported.cal.conditions$Slope)){
            default.cal.conditions$Slope
        } else if(!is.null(imported.cal.conditions$Slope)){
            as.character(imported.cal.conditions$Slope)
        }
    } else if(!"Slope" %in% names(imported.cal.conditions)){
        default.cal.conditions$Slope
    }
    
    intercept.corrections <- if("Intercept" %in% names(imported.cal.conditions)){
        as.character(imported.cal.conditions$Intercept)
    } else if(!"Intercept" %in% names(imported.cal.conditions)){
        default.cal.conditions$Intercept
    }
    
    standards.used <- if("StandardsUsed" %in% names(imported.cal.conditions)){
        imported.cal.conditions$StandardsUsed
    } else if(!"StandardsUsed" %in% names(imported.cal.conditions)){
        default.cal.conditions$StandardsUsed
    }
    
    cal.table <- data.frame(
        CalType=cal.condition,
        LineType=line.condition,
        LineStructure=line.structure.condition,
        GausBuffer=gaus.buffer,
        SplitBuffer=split.buffer,
        Deconvolution=deconvolution,
        DeconvolutionSigma=decon.sigma,
        SmoothWidth=smooth.width,
        SmoothAlpha=smooth.alpha,
        SmoothIter=smooth.iter,
        SnipIter=snip.iter,
        Compress=compress.condition,
        Transformation=transformation.condition,
        EnergyRange=energyrange.condition,
        NormType=norm.condition,
        Min=norm.min,
        Max=norm.max,
        ComptonType=compton.type,
        DepTrans=dependent.transformation,
        ForestTry=foresttry,
        ForestMetric=forestmetric,
        ForestTC=foresttrain,
        ForestNumber=forestnumber,
        CVRepeats=cvrepeats,
        ForestTrees=foresttrees,
        NeuralHL=neuralhiddenlayers,
        NeuralHU=neuralhiddenunits,
        NeuralWD=neuralweightdecay,
        NeuralMI=neuralmaxiterations,
		TreeMethod=treemethod,
        TreeDepth=treedepth,
        DropTree=droptree,
        SkipDrop=skipdrop,
        xgbType=xgbtype,
        xgbAlpha=xgbalpha,
        xgbGamma=xgbgamma,
        xgbEta=xgbeta,
        xgbLambda=xgblambda,
        xgbSubSample=xgbsubsample,
        xgbColSample=xgbcolsample,
        xgbMinChild=xgbminchild,
		xgbMaxDeltaStep=xgbmaxdeltastep,
		xgbScalePosWeight=xgbscaleposweight,
        bartK=bartk,
        bartBeta=bartbeta,
        bartNu=bartnu,
        svmC=svmc,
        svmDegree=svmdegree,
        svmScale=svmscale,
        svmSigma=svmsigma,
        svmLength=svmlength,
        stringsAsFactors=FALSE)
        
        cal.mode.list <- list(CalTable=cal.table, Slope=slope.corrections, Intercept=intercept.corrections, StandardsUsed=standards.used)
        return(cal.mode.list)
}

importCalConditions <- function(element, calList, number.of.standards=NULL, temp=FALSE){
    
    number.of.standards <- if(is.null(number.of.standards)){
        length(calList[[element]][[1]]$StandardsUsed)
    } else if(!is.null(number.of.standards)){
        number.of.standards
    }
    
    default.cal.conditions <- defaultCalConditions(element=element, number.of.standards=number.of.standards)
    
    imported.cal.conditions <- calList[[element]][[1]]
    
    cal.condition <- if("CalType" %in% colnames(imported.cal.conditions$CalTable)){
        as.numeric(as.character(imported.cal.conditions$CalTable$CalType[1]))
    } else if(!"CalType" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$CalType
    }
    
    line.condition <- if("LineType" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$LineType[1])
    } else if(!"LineType" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$LineType
    }
    
    line.structure.condition <- if("LineStructure" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$LineStructure[1])
    } else if(!"LineStructure" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$LineStructure
    }
    
    gaus.buffer <- if("GausBuffer" %in% colnames(imported.cal.conditions$CalTable)){
         as.numeric(imported.cal.conditions$CalTable$GausBuffer[1])
    } else if(!"GausBuffer" %in% colnames(imported.cal.conditions$CalTable)){
       default.cal.conditions$CalTable$GausBuffer
    }
    
    split.buffer <- if("SplitBuffer" %in% colnames(imported.cal.conditions$CalTable)){
         as.numeric(imported.cal.conditions$CalTable$SplitBuffer[1])
    } else if(!"SplitBuffer" %in% colnames(imported.cal.conditions$CalTable)){
       default.cal.conditions$CalTable$SplitBuffer
    }
    
    deconvolution <- if("Deconvolution" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$Deconvolution[1])
    } else if(!"Deconvolution" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$Deconvolution
    }
    
    decon.sigma <- if("DeconvolutionSigma" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$DeconvolutionSigma[1])
    } else if(!"DeconvolutionSigma" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$DeconvolutionSigma
    }
    
    smooth.width <- if("SmoothWidth" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$SmoothWidth[1])
    } else if(!"SmoothWidth" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$SmoothWidth
    }
    
    smooth.alpha <- if("SmoothAlpha" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$SmoothAlpha[1])
    } else if(!"SmoothAlpha" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$SmoothAlpha
    }
    
    smooth.iter <- if("SmoothIter" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$SmoothIter[1])
    } else if(!"SmoothIter" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$SmoothIter
    }
    
    snip.iter <- if("SnipIter" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$SnipIter[1])
    } else if(!"SnipIter" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$SnipIter
    }
    
    compress.condition <- if("Compress" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$Compress[1])
    } else if(!"Compress" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$Compress
    }
    
    transformation.condition <- if("Transformation" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$Transformation[1])
    } else if(!"Transformation" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$Transformation
    }
    
    energyrange.condition <- if("EnergyRange" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$EnergyRange[1])
    } else if(!"EnergyRange" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$EnergyRange
    }
    
    
    norm.condition <- if("NormType" %in% colnames(imported.cal.conditions$CalTable)){
        as.numeric(as.character(imported.cal.conditions$CalTable$NormType[1]))
    } else if(!"NormType" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$NormType
    }
    
    norm.min <- if("Min" %in% colnames(imported.cal.conditions$CalTable)){
        as.numeric(as.character(imported.cal.conditions$CalTable$Min[1]))
    } else if(!"Min" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$Min
    }
    
    norm.max <- if("Max" %in% colnames(imported.cal.conditions$CalTable)){
        as.numeric(as.character(imported.cal.conditions$CalTable$Max[1]))
    } else if(!"Max" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$Max
    }
    
    compton.type <- if("ComptonType" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$ComptonType[1])
    } else if(!"ComptonType" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$ComptonType
    }
    
    dependent.transformation <- if("DepTrans" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$DepTrans[1])
    } else if(!"DepTrans" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$DepTrans
    }
    
    foresttry <- if("ForestTry" %in% colnames(imported.cal.conditions$CalTable)){
        as.numeric(as.character(imported.cal.conditions$CalTable$ForestTry[1]))
    } else if(!"ForestTry" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$ForestTry
    }
    
    forestmetric <- if("ForestMetric" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$ForestMetric[1])
    } else if(!"ForestMetric" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$ForestMetric
    }
    
    foresttrain <- if("ForestTC" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$ForestTC[1])
    } else if(!"ForestTC" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$ForestTC
    }
    
    forestnumber <- if("ForestNumber" %in% colnames(imported.cal.conditions$CalTable)){
        as.numeric(as.character(imported.cal.conditions$CalTable$ForestNumber[1]))
    } else if(!"ForestNumber" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$ForestNumber
    }
    
    cvrepeats <- if("CVRepeats" %in% colnames(imported.cal.conditions$CalTable)){
        as.numeric(as.character(imported.cal.conditions$CalTable$CVRepeats[1]))
    } else if(!"CVRepeats" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$CVRepeats
    }
    
    foresttrees <- if("ForestTrees" %in% colnames(imported.cal.conditions$CalTable)){
        as.numeric(as.character(imported.cal.conditions$CalTable$ForestTrees[1]))
    } else if(!"ForestTrees" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$ForestTrees
    }
    
    neuralhiddenlayers <- if("NeuralHL" %in% colnames(imported.cal.conditions$CalTable)){
        as.numeric(as.character(imported.cal.conditions$CalTable$NeuralHL[1]))
    } else if(!"NeuralHL" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$NeuralHL
    }
    
    neuralhiddenunits <- if("NeuralHU" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==6 | cal.condition==7){
            as.character(imported.cal.conditions$CalTable$NeuralHU[1])
        } else if(!cal.condition==6 | !cal.condition==7){
            as.character(imported.cal.conditions$CalTable$NeuralHU[1])
        }
    } else if(!"NeuralHU" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$NeuralHU
    }
    
    neuralweightdecay <- if("NeuralWD" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==6 | cal.condition==7){
            if(neuralhiddenlayers==1){
                as.character(imported.cal.conditions$CalTable$NeuralWD[1])
            } else if(neuralhiddenlayers > 1){
                as.character(imported.cal.conditions$CalTable$NeuralWD[1])
            }
        } else if(!cal.condition==6 | !cal.condition==7){
            imported.cal.conditions$CalTable$NeuralWD
        }
    } else if(!"NeuralWD" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$NeuralWD
    }
    
    neuralmaxiterations <- if("NeuralMI" %in% colnames(imported.cal.conditions$CalTable)){
        as.numeric(as.character(imported.cal.conditions$CalTable$NeuralMI[1]))
    } else if(!"NeuralMI" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$NeuralMI
    }
    
    xgbtype <- if("xgbType" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$xgbType[1])
    } else if(!"xgbType" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$xgbType
    }

    treemethod <- if("TreeMethod" %in% colnames(imported.cal.conditions$CalTable)){
        as.character(imported.cal.conditions$CalTable$TreeMethod[1])
    } else if(!"TreeMethod" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$TreeMethod
    }
    
    treedepth <- if("TreeDepth" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==8 | cal.condition==9 && xgbtype!="Linear"){
            as.character(imported.cal.conditions$CalTable$TreeDepth[1])
        } else if(!cal.condition==8 | !cal.condition==9 | xgbtype=="Linear"){
            default.cal.conditions$CalTable$TreeDepth
        }
    } else if(!"TreeDepth" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$TreeDepth
    }
    
    droptree <- if("DropTree" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==8 | cal.condition==9 && xgbtype=="Dart"){
            as.character(imported.cal.conditions$CalTable$DropTree[1])
        } else if(!cal.condition==8 | !cal.condition==9 | xgbtype!="Dart"){
            default.cal.conditions$CalTable$DropTree
        }
    } else if(!"DropTree" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$DropTree
    } 
    
    skipdrop <- if("SkipDrop" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==8 | cal.condition==9 && xgbtype=="Dart"){
            as.character(imported.cal.conditions$CalTable$SkipDrop[1])
        } else if(!cal.condition==8 | !cal.condition==9 | xgbtype!="Dart"){
            default.cal.conditions$CalTable$SkipDrop
        }
    } else if(!"DropTree" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$SkipDrop
    }  
    
    
    xgbalpha <- if("xgbAlpha" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==8 | cal.condition==9 && xgbtype=="Linear"){
            as.character(imported.cal.conditions$CalTable$xgbAlpha[1])
        } else if(!cal.condition==8 | !cal.condition==9){
            as.character(imported.cal.conditions$CalTable$xgbAlpha[1])
        }
    } else if(!"xgbAlpha" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$xgbAlpha
    }
    
    xgbgamma <- if("xgbGamma" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==8 | cal.condition==9 && xgbtype!="Linear"){
            as.character(imported.cal.conditions$CalTable$xgbGamma[1])
        } else if(!cal.condition==8 | !cal.condition==9){
            as.character(imported.cal.conditions$CalTable$xgbGamma[1])
        }
    } else if(!"xgbGamma" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$xgbGamma
    }
    
    xgbeta <- if("xgbEta" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==8 | cal.condition==9){
            as.character(imported.cal.conditions$CalTable$xgbEta[1])
        } else if(!cal.condition==8 | !cal.condition==9){
            as.character(imported.cal.conditions$CalTable$xgbEta[1])
        }
    } else if(!"xgbEta" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$xgbEta
    }
    
    xgblambda <- if("xgbLambda" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==8 | cal.condition==9 && xgbtype=="Linear"){
            as.character(imported.cal.conditions$CalTable$xgbLambda[1])
        } else if(!cal.condition==8 | !cal.condition==9){
            as.character(imported.cal.conditions$CalTable$xgbLambda[1])
        }
    } else if(!"xgbLambda" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$xgbLambda
    }
    
    xgbsubsample <- if("xgbSubSample" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==8 | cal.condition==9 && xgbtype!="Linear"){
            as.character(imported.cal.conditions$CalTable$xgbSubSample[1])
        } else if(!cal.condition==8 | !cal.condition==9){
            as.character(imported.cal.conditions$CalTable$xgbSubSample[1])
        }
    } else if(!"xgbSubSample" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$xgbSubSample
    }
    
    xgbcolsample <- if("xgbColSample" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==8 | cal.condition==9 && xgbtype!="Linear"){
            as.character(imported.cal.conditions$CalTable$xgbColSample[1])
        } else if(!cal.condition==8 | !cal.condition==9){
            as.character(imported.cal.conditions$CalTable$xgbColSample[1])
        }
    } else if(!"xgbColSample" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$xgbColSample
    }
    
    xgbminchild <- if("xgbMinChild" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==8 | cal.condition==9 && xgbtype!="Linear"){
            as.numeric(as.character(imported.cal.conditions$CalTable$xgbMinChild[1]))
        } else if(!cal.condition==8 | !cal.condition==9){
            as.numeric(as.character(imported.cal.conditions$CalTable$xgbMinChild[1]))
        }
    } else if(!"xgbMinChild" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$xgbMinChild
    }

    xgbmaxdeltastep <- if("xgbMaxDeltaStep" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==8 | cal.condition==9 && xgbtype!="Linear"){
            as.numeric(as.character(imported.cal.conditions$CalTable$xgbMaxDeltaStep[1]))
        } else if(!cal.condition==8 | !cal.condition==9){
            as.numeric(as.character(imported.cal.conditions$CalTable$xgbMaxDeltaStep[1]))
        }
    } else if(!"xgbMaxDeltaStep" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$xgbMaxDeltaStep
    }

    xgbscaleposweight <- if("xgbScalePosWeight" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==8 | cal.condition==9 && xgbtype!="Linear"){
            as.numeric(as.character(imported.cal.conditions$CalTable$xgbScalePosWeight[1]))
        } else if(!cal.condition==8 | !cal.condition==9){
            as.numeric(as.character(imported.cal.conditions$CalTable$xgbScalePosWeight[1]))
        }
    } else if(!"xgbScalePosWeight" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$xgbScalePosWeight
    }
    
    bartk <- if("bartK" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==10 | cal.condition==11 && xgbtype=="Tree"){
            paste0(pnorm(as.numeric(calList[[element]][[2]]$bestTune$k)), "-", pnorm(as.numeric(calList[[element]][[2]]$bestTune$k)))
        } else if(!cal.condition==10 | !cal.condition==11){
            as.character(imported.cal.conditions$CalTable$bartK[1])
        }
    } else if(!"bartK" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$bartK
    }
    
    bartbeta <- if("bartBeta" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==10 | cal.condition==11 && xgbtype=="Tree"){
            paste0(calList[[element]][[2]]$bestTune$beta, "-", calList[[element]][[2]]$bestTune$beta)
        } else if(!cal.condition==10 | !cal.condition==11){
            as.character(imported.cal.conditions$CalTable$bartBeta[1])
        }
    } else if(!"bartBeta" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$bartBeta
    }
    
    bartnu <- if("bartNu" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==10 | cal.condition==11 && xgbtype=="Tree"){
            paste0(calList[[element]][[2]]$bestTune$nu, "-", calList[[element]][[2]]$bestTune$nu)
        } else if(!cal.condition==10 | !cal.condition==11){
            as.character(imported.cal.conditions$CalTable$bartNu[1])
        }
    } else if(!"bartNu" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$bartNu
    }
    
    svmc <- if("svmC" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==12 | cal.condition==13){
            if("C" %in% names(calList[[element]][[2]]$bestTune)){
                paste0(calList[[element]][[2]]$bestTune$C, "-", calList[[element]][[2]]$bestTune$C)
            } else {
                as.character(imported.cal.conditions$CalTable$svmC[1])
            }
        } else if(!cal.condition==12 | !cal.condition==13){
            as.character(imported.cal.conditions$CalTable$svmC[1])
        }
    } else if(!"svmC" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$svmC
    }
    
    svmdegree <- if("svmDegree" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==12 | cal.condition==13 && xgbtype=="Polynomial"){
            if("degree" %in% names(calList[[element]][[2]]$bestTune)){
                paste0(calList[[element]][[2]]$bestTune$degree, "-", calList[[element]][[2]]$bestTune$degree)
            } else {
                as.character(imported.cal.conditions$CalTable$svmDegree[1])
            }
        } else if(!cal.condition==12 | !cal.condition==13){
            as.character(imported.cal.conditions$CalTable$svmDegree[1])
        }
    } else if(!"svmDegree" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$svmDegree
    }
    
    svmscale <- if("svmScale" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==12 | cal.condition==13 && xgbtype=="Polynomial"){
            if("scale" %in% names(calList[[element]][[2]]$bestTune)){
                paste0(calList[[element]][[2]]$bestTune$scale, "-", calList[[element]][[2]]$bestTune$scale)
            } else {
                as.character(imported.cal.conditions$CalTable$svmScale[1])
            }
        } else if(!cal.condition==12 | !cal.condition==13){
            as.character(imported.cal.conditions$CalTable$svmScale[1])
        }
    } else if(!"svmScale" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$svmScale
    }
    
    svmsigma <- if("svmSigma" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==12 | cal.condition==13){
            if(xgbtype=="Radial" | xgbtype=="Radial Cost" | xgbtype=="Radial Sigma"){
                if("sigma" %in% names(calList[[element]][[2]]$bestTune)){
                    paste0(calList[[element]][[2]]$bestTune$sigma, "-", calList[[element]][[2]]$bestTune$sigma)
                } else {
                    as.character(imported.cal.conditions$CalTable$svmSigma[1])
                }
            } else {
                as.character(imported.cal.conditions$CalTable$svmSigma[1])
            }
        } else if(!cal.condition==12 | !cal.condition==13){
            as.character(imported.cal.conditions$CalTable$svmSigma[1])
        }
    } else if(!"svmSigma" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$svmSigma
    }
    
    svmlength <- if("svmLength" %in% colnames(imported.cal.conditions$CalTable)){
        if(cal.condition==12 | cal.condition==13){
            if(xgbtype=="Boundrange String"){
                if("length" %in% names(calList[[element]][[2]]$bestTune)){
                    paste0(calList[[element]][[2]]$bestTune$length, "-", calList[[element]][[2]]$bestTune$length)
                } else {
                    as.character(imported.cal.conditions$CalTable$svmLength[1])
                }
            } else {
                as.character(imported.cal.conditions$CalTable$svmLength[1])
            }
        } else if(!cal.condition==12 | !cal.condition==13){
            as.character(imported.cal.conditions$CalTable$svmLength[1])
        }
    } else if(!"svmLength" %in% colnames(imported.cal.conditions$CalTable)){
        default.cal.conditions$CalTable$svmLength
    }
    
    slope.corrections <- if("Slope" %in% names(imported.cal.conditions)){
        if(is.null(imported.cal.conditions$Slope)){
            default.cal.conditions$Slope
        } else if(!is.null(imported.cal.conditions$Slope)){
            as.character(imported.cal.conditions$Slope)
        }
    } else if(!"Slope" %in% names(imported.cal.conditions)){
        default.cal.conditions$Slope
    }
    
    intercept.corrections <- if("Intercept" %in% names(imported.cal.conditions)){
        as.character(imported.cal.conditions$Intercept)
    } else if(!"Intercept" %in% names(imported.cal.conditions)){
        default.cal.conditions$Intercept
    }
    
    standards.used <- if("StandardsUsed" %in% names(imported.cal.conditions)){
        imported.cal.conditions$StandardsUsed
    } else if(!"StandardsUsed" %in% names(imported.cal.conditions)){
        default.cal.conditions$StandardsUsed
    }
    
    scale <- if("Scale" %in% names(imported.cal.conditions)){
        imported.cal.conditions$Scale
    } else if(!"Scale" %in% names(imported.cal.conditions)){
        default.cal.conditions$Scale
    }
    cal.table <- data.frame(
    CalType=cal.condition,
    LineType=line.condition,
    LineStructure=line.structure.condition,
    GausBuffer=gaus.buffer,
    SplitBuffer=split.buffer,
    Deconvolution=deconvolution,
    DeconvolutionSigma=decon.sigma,
    SmoothWidth=smooth.width,
    SmoothAlpha=smooth.alpha,
    SmoothIter=smooth.iter,
    SnipIter=snip.iter,
    Compress=compress.condition,
    Transformation=transformation.condition,
    EnergyRange=energyrange.condition,
    NormType=norm.condition,
    Min=norm.min,
    Max=norm.max,
    ComptonType=compton.type,
    DepTrans=dependent.transformation,
    ForestTry=foresttry,
    ForestMetric=forestmetric,
    ForestTC=foresttrain,
    ForestNumber=forestnumber,
    CVRepeats=cvrepeats,
    ForestTrees=foresttrees,
    NeuralHL=neuralhiddenlayers,
    NeuralHU=neuralhiddenunits,
    NeuralWD=neuralweightdecay,
    NeuralMI=neuralmaxiterations,
	TreeMethod=treemethod,
    TreeDepth=treedepth,
    DropTree=droptree,
    SkipDrop=skipdrop,
    xgbType=xgbtype,
    xgbAlpha=xgbalpha,
    xgbGamma=xgbgamma,
    xgbEta=xgbeta,
    xgbLambda=xgblambda,
    xgbSubSample=xgbsubsample,
    xgbColSample=xgbcolsample,
    xgbMinChild=xgbminchild,
	xgbMaxDeltaStep=xgbmaxdeltastep,
	xgbScalePosWeight=xgbscaleposweight,
    bartK=bartk,
    bartBeta=bartbeta,
    bartNu=bartnu,
    svmC=svmc,
    svmDegree=svmdegree,
    svmScale=svmscale,
    svmSigma=svmsigma,
    svmLength=svmlength,
    stringsAsFactors=FALSE)
    
    if(temp==TRUE){
        cal.table$Delete <- TRUE
    }
    
    cal.mode.list <- list(CalTable=cal.table, Slope=slope.corrections, Intercept=intercept.corrections, StandardsUsed=standards.used, Scale=scale)
    return(cal.mode.list)
}

calPre <- function(element.model.list, element, temp, env.strip=TRUE, xgb_raw=FALSE, xgb_unserialize=FALSE){
    
    temp.list <- list(element.model.list)
    names(temp.list) <- element
        
        if(xgb_raw==TRUE){
            if(element.model.list[[1]]$CalTable$CalType==8 | element.model.list[[1]]$CalTable$CalType==9){
                if("Model" %in% names(element.model.list)){
                    model.raw <-
                        tryCatch(
                            xgb.save.raw(
                                tryCatch(
                                    xgb.Booster.complete(element.model.list$Model$finalModel)
                                    , error=function(e) element.model.list$Model$finalModel))
                                , error=function(e) NULL)
                    } else {
                        model.raw <- NULL
                    }
                } else if(!"Model" %in% names(element.model.list)){
                    model.raw <-
                        tryCatch(
                            xgb.save.raw(
                                tryCatch(
                                    xgb.Booster.complete(element.model.list[[2]]$finalModel)
                                    , error=function(e) element.model.list[[2]]$finalModel))
                                , error=function(e) NULL)
                    } else {
                        model.raw <- NULL
                    }
                    
            element.model.list$rawModel <- model.raw
        }
        
        if(xgb_unserialize==TRUE){
            if(element.model.list[[1]]$CalTable$CalType==8 | element.model.list[[1]]$CalTable$CalType==9){
                    if("rawModel" %in% names(element.model.list)){
                        element.model.list$Model$finalModel$raw <- element.model.list$rawModel
                    }
            }
        }
        
        new.element.model.list <- if(!"Model" %in% names(element.model.list)){
            tryCatch(
            list(Parameters=importCalConditions(element=element,
                    calList=temp.list, temp=temp),
                    Model=element.model.list[[2]])
                    , error=function(e) NULL)
        } else if("Model" %in% names(element.model.list)){
            tryCatch(
            list(Parameters=importCalConditions(element=element,
                    calList=temp.list, temp=temp),
                    Model=element.model.list$Model)
                    , error=function(e) NULL)
        }
        
        if(is.na(new.element.model.list$Parameters$CalTable$LineType[1])){
            new.element.model.list$Parameters$CalTable$LineType[1] <- "Narrow"
        }
        
        if(is.na(new.element.model.list$Parameters$CalTable$LineStructure[1])){
                if(element %in% spectralLines){
                    new.element.model.list$Parameters$CalTable$LineStructure[1] <- "sum"
                } else if(!element %in% spectralLines){
                    new.element.model.list$Parameters$CalTable$LineStructure[1] <- "mean"
                }
            
        }
        
        if("rawModel" %in% names(element.model.list)){
            new.element.model.list$rawModel <- element.model.list$rawModel
        }
        
        if("ValidationSet" %in% names(element.model.list)){
            new.element.model.list$ValidationSet <- element.model.list$ValidationSet
        }
        
        if("Backup" %in% names(element.model.list)){
            new.element.model.list$Backup <- element.model.list$Backup
        }
        
        if("Score" %in% names(element.model.list)){
            new.element.model.list$Score <- element.model.list$Score
        }
        
        if("SystemicAdjust" %in% names(element.model.list)){
            new.element.model.list$SystemicAdjust <- element.model.list$SystemicAdjust
        }
        
        if("Table" %in% names(element.model.list)){
            new.element.model.list$Table <- element.model.list$Table
        }
        
        #if(!"Scale" %in% names(element.model.list[[1]])){
        #    element.model.list[[1]][["Scale"]] <- list(Min=0, Max=1)
        #}
                
        if(nrow(new.element.model.list$Parameters$CalTable)>1){
            new.element.model.list$Parameters$CalTable <- new.element.model.list$Parameters$CalTable[!duplicated(new.element.model.list$Parameters$CalTable), ]
        }
        
        if(env.strip==TRUE){
            new.element.model.list$Model <- strip_env(new.element.model.list$Model)
        }
            
    return(new.element.model.list)
        
}

all_slopes <- function(calibration){
    slope_list <- list()
    for(i in names(calibration$calList)){
        slope_list[[i]] <- calibration$calList[[i]]$Parameters$Slope
    }
    intercept_list <- list()
    for(i in names(calibration$calList)){
        intercept_list[[i]] <- calibration$calList[[i]]$Parameters$Intercept
    }
    elements <- names(calibration$calList)
    
    unique(unlist(list(elements, slope_list, intercept_list)))
}

intensity_fix <- function(calibration, keep_labels=TRUE){
    
    slope_list <- all_slopes(calibration)[!all_slopes(calibration) %in% c("Baseline", "Total")]
    intensity_names <- names(calibration$Intensities)
    
    missing_elements <- slope_list[!slope_list %in% intensity_names]
    variables <- c(intensity_names, missing_elements)
    variable_elements <- variables[variables %in% spectralLines]
    variable_custom <- variables[!variables %in% c(spectralLines, "Spectrum")]
    
    other_spectra_stuff <- totalCountsGen(calibration$Spectra)
    if("Deconvoluted" %in% names(calibration)){
        other_spectra_stuff <- merge(other_spectra_stuff, calibration$Deconvoluted$Areas$Baseline, all=T, sort=T)
    }
    
    if(length(variable_elements)>0){
        element.frame <- data.frame(elements=variable_elements, order=atomic_order_vector(variable_elements))
        organized_elements <- as.vector(element.frame[order(element.frame$order),]$elements)
        
        if(keep_labels==FALSE){
            if(length(missing_elements>=1)){
                calibration$Intensities <- narrowLineTable(spectra=calibration$Spectra, definition.table=calibration$Definitions, elements=c(organized_elements, variable_custom))[,-1]
                calibration$WideIntensities <- wideLineTable(spectra=calibration$Spectra, definition.table=calibration$Definitions, elements=c(organized_elements, variable_custom))[,-1]

            }
        } else if(keep_labels==TRUE){
            if(length(missing_elements>=1)){
                calibration$Intensities <- narrowLineTable(spectra=calibration$Spectra, definition.table=calibration$Definitions, elements=c(organized_elements, variable_custom))
                calibration$WideIntensities <- wideLineTable(spectra=calibration$Spectra, definition.table=calibration$Definitions, elements=c(organized_elements, variable_custom))

            }
        }
        
    }
    

    return(calibration)
}

# File extensions to remove from spectrum names
.file_extensions <- c(".pdz", ".PDZ", ".csv", ".CSV", ".spt", ".mca", ".spx", ".spe")

spectrumNameSingle <- function(spectrum_name){
    mgsub::mgsub(spectrum_name, .file_extensions, rep("", length(.file_extensions)))
}

spectrumNameVector <- function(spectrum_vector){
    # Process entire vector at once instead of element-by-element
    mgsub::mgsub(as.character(spectrum_vector), .file_extensions, rep("", length(.file_extensions)))
}

# Helper to normalize spectrum names in a data frame column
normalizeSpectrumColumn <- function(df, col = "Spectrum") {
    if (!is.null(df) && col %in% names(df)) {
        df[[col]] <- make.names(spectrumNameVector(df[[col]]), unique = FALSE)
    }
    df
}

# Helper to ensure OtherSpectraStuff exists in calibration
ensureOtherSpectraStuff <- function(Calibration, force_create = FALSE) {
    if (force_create || !"OtherSpectraStuff" %in% names(Calibration)) {
        Calibration$OtherSpectraStuff <- totalCountsGen(Calibration$Spectra)
        if ("Deconvoluted" %in% names(Calibration) &&
            "Baseline" %in% names(Calibration$Deconvoluted$Areas)) {
            Calibration$OtherSpectraStuff <- merge(
                Calibration$OtherSpectraStuff,
                Calibration$Deconvoluted$Areas[, c("Spectrum", "Baseline")],
                by = "Spectrum", all = TRUE, sort = TRUE
            )
        }
    }
    Calibration
}

# Helper to rebuild all intensity tables
# Parallelizes the 6 independent table builds on non-Windows systems with sufficient cores
rebuildIntensityTables <- function(Calibration, elements, allowParallel) {
    other <- Calibration$OtherSpectraStuff
    spectra <- Calibration$Spectra
    defs <- Calibration$Definitions
    gaus_buf <- Calibration$LineDefaults$GausBuffer
    split_buf <- Calibration$LineDefaults$SplitBuffer

    # Check if outer-level parallelism is beneficial:
    # - Need non-Windows (mclapply uses forking)
    # - Need enough cores to justify overhead
    # - User must have requested parallel processing
    n_cores <- as.numeric(my.cores)
    use_outer_parallel <- allowParallel && get_os() != "windows" && n_cores >= 4

    if (use_outer_parallel) {
        # When parallelizing at outer level, disable inner parallelism to avoid oversubscription
        # Use min(6, cores) since we have exactly 6 independent tasks
        outer_cores <- min(6, n_cores)

        # Define the 6 table-building tasks
        tasks <- list(
            Intensities = function() narrowLineTable(spectra, defs, elements, gaus_buffer = gaus_buf, allowParallel = FALSE),
            IntensitiesSplit = function() narrowLineTableSplit(spectra, defs, elements, split_buffer = split_buf, allowParallel = FALSE),
            IntensitiesFirst = function() narrowLineTableFirst(spectra, defs, elements, gaus_buffer = gaus_buf, allowParallel = FALSE),
            IntensitiesSecond = function() narrowLineTableSecond(spectra, defs, elements, gaus_buffer = gaus_buf, allowParallel = FALSE),
            WideIntensities = function() wideLineTable(spectra, defs, elements, allowParallel = FALSE),
            WideIntensitiesSplit = function() wideLineTableSplit(spectra, defs, elements, split_buffer = split_buf, allowParallel = FALSE)
        )

        # Run in parallel using mclapply (fork-based, Unix/macOS only)
        tables <- parallel::mclapply(tasks, function(f) f(), mc.cores = outer_cores)

        # Merge results with OtherSpectraStuff
        for (name in names(tables)) {
            Calibration[[name]] <- merge(tables[[name]], other, by = "Spectrum")
        }
    } else {
        # Sequential execution (Windows or low core count or parallel disabled)
        # Inner parallelism handles element-level parallelization
        Calibration$Intensities <- merge(
            narrowLineTable(spectra, defs, elements, gaus_buffer = gaus_buf, allowParallel = allowParallel),
            other, by = "Spectrum"
        )
        Calibration$IntensitiesSplit <- merge(
            narrowLineTableSplit(spectra, defs, elements, split_buffer = split_buf, allowParallel = allowParallel),
            other, by = "Spectrum"
        )
        Calibration$IntensitiesFirst <- merge(
            narrowLineTableFirst(spectra, defs, elements, gaus_buffer = gaus_buf, allowParallel = allowParallel),
            other, by = "Spectrum"
        )
        Calibration$IntensitiesSecond <- merge(
            narrowLineTableSecond(spectra, defs, elements, gaus_buffer = gaus_buf, allowParallel = allowParallel),
            other, by = "Spectrum"
        )
        Calibration$WideIntensities <- merge(
            wideLineTable(spectra, defs, elements, allowParallel = allowParallel),
            other, by = "Spectrum"
        )
        Calibration$WideIntensitiesSplit <- merge(
            wideLineTableSplit(spectra, defs, elements, split_buffer = split_buf, allowParallel = allowParallel),
            other, by = "Spectrum"
        )
    }

    Calibration
}

# Helper to sort and align Values/Spectra
sortAlignCalibration <- function(Calibration) {
    Calibration$Values <- Calibration$Values[order(Calibration$Values$Spectrum), ]
    Calibration$Spectra <- Calibration$Spectra[order(Calibration$Spectra$Spectrum, Calibration$Spectra$Energy), ]
    # Keep only matching spectra
    spec_names <- unique(Calibration$Spectra$Spectrum)
    val_names <- unique(Calibration$Values$Spectrum)
    Calibration$Values <- Calibration$Values[Calibration$Values$Spectrum %in% spec_names, ]
    Calibration$Spectra <- Calibration$Spectra[Calibration$Spectra$Spectrum %in% val_names, ]
    Calibration
}

calRDS <- function(calibration.directory=NULL, Calibration=NULL, null.strip=TRUE, env.strip=TRUE, temp=FALSE, extensions=FALSE, xgb_raw=FALSE, xgb_unserialize=FALSE, sort=FALSE, deconvolution=TRUE, rebuild=FALSE, allowParallel=TRUE){
    if(is.null(Calibration)){
        Calibration <- readRDS(calibration.directory)
    }
    
    
    # Normalize spectrum names across all tables
    Calibration$Spectra <- normalizeSpectrumColumn(Calibration$Spectra)
    Calibration$Values <- normalizeSpectrumColumn(Calibration$Values)

    # Normalize intensity tables
    intensity_tables <- c("Intensities", "IntensitiesSplit", "IntensitiesFirst",
                          "IntensitiesSecond", "WideIntensities", "WideIntensitiesSplit")
    for (tbl in intensity_tables) {
        if (tbl %in% names(Calibration)) {
            Calibration[[tbl]] <- normalizeSpectrumColumn(Calibration[[tbl]])
        }
    }

    # Normalize deconvoluted tables
    if ("Deconvoluted" %in% names(Calibration)) {
        Calibration$Deconvoluted$Areas <- normalizeSpectrumColumn(Calibration$Deconvoluted$Areas)
        Calibration$Deconvoluted$Spectra <- normalizeSpectrumColumn(Calibration$Deconvoluted$Spectra)
        if ("Baseline" %in% names(Calibration$Deconvoluted)) {
            Calibration$Deconvoluted$Baseline <- normalizeSpectrumColumn(Calibration$Deconvoluted$Baseline)
        }
    }
    
    elements <- names(Calibration$Values)[!names(Calibration$Values) %in% c("Include", "Spectrum")]

    if(!"LineDefaults" %in% names(Calibration)){
        Calibration$LineDefaults <- list(GausBuffer=0.02, SplitBuffer=0.1)
    }
    
    if("Deconvoluted" %in% names(Calibration)){
        if(!"Parameters" %in% names(Calibration$Deconvoluted)){
            Calibration$Deconvoluted$Parameters <- list(SmoothWidth=5, SmoothAlpha=2.5, DefaultSigma=0.07, SmoothIter=20, SnipIter=20)
        }
        if("Parameters" %in% names(Calibration$Deconvoluted)){
            if("Width" %in% names(Calibration$Deconvoluted$Parameters)){
                old_param <- Calibration$Deconvoluted$Parameters
                Calibration$Deconvoluted$Parameters <- list(SmoothWidth=old_param$Width, SmoothAlpha=old_param$Alpha, DefaultSigma=old_param$DefaultSigma, SmoothIter=old_param$SmoothIter, SnipIter=old_param$SnipIter)
            }
        }
    }

    # Backward compatibility: default Beam to "1" for old calibrations without beam info
    if(!"Beam" %in% names(Calibration)){
        Calibration$Beam <- "1"
    }

    if(rebuild==TRUE){
        Calibration <- sortAlignCalibration(Calibration)
        # Run deconvolution
        cores <- if (allowParallel) as.numeric(my.cores) else 1
        Calibration$Deconvoluted <- tryCatch(
            spectra_gls_deconvolute(Calibration$Spectra, cores = cores),
            error = function(e) spectra_gls_deconvolute(Calibration$Spectra, cores = 1)
        )
        Calibration <- ensureOtherSpectraStuff(Calibration, force_create = TRUE)
        Calibration <- rebuildIntensityTables(Calibration, elements, allowParallel)
    }



    if(sort==TRUE){
        Calibration <- sortAlignCalibration(Calibration)
        Calibration <- ensureOtherSpectraStuff(Calibration)
        Calibration <- rebuildIntensityTables(Calibration, elements, allowParallel)
    }

    if(deconvolution==TRUE && !"Deconvoluted" %in% names(Calibration)){
        cores <- if (allowParallel) as.numeric(my.cores) else 1
        Calibration$Deconvoluted <- tryCatch(
            spectra_gls_deconvolute(Calibration$Spectra, cores = cores),
            error = function(e) spectra_gls_deconvolute(Calibration$Spectra, cores = 1)
        )
    }

    # Ensure OtherSpectraStuff exists
    Calibration <- ensureOtherSpectraStuff(Calibration)
    
    tryCatch(if(Calibration$FileType=="Spectra"){Calibration$FileType <- "CSV"}, error=function(e) NULL)
    
    
    Calibration$Notes <- if(!is.null(Calibration[["Notes"]])){
        paste0(Calibration[["Notes"]], " Updated on ", Sys.time())
    } else if(is.null(Calibration[["Notes"]])){
        paste0("Updated on ", Sys.time())
    }
    
    
    if(extensions==TRUE){
        extensions <- c(".spx", ".PDZ", ".pdz", ".CSV", ".csv", ".spt", ".mca")
        Calibration[["Spectra"]]$Spectrum <- mgsub::mgsub(pattern=extensions, replacement=rep("", length(extensions)), string=as.character(Calibration[["Spectra"]]$Spectrum))
        Calibration[["Values"]]$Spectrum <- mgsub::mgsub(pattern=extensions, replacement=rep("", length(extensions)), string=as.character(Calibration[["Values"]]$Spectrum))
    }

    

    
    Calibration$Values <- valFrameCheck(Calibration$Values)
    Calibration$Intensities <- intensityFrameCheck(Calibration$Intensities)
    Calibration$Spectra <- spectraCheck(Calibration$Spectra)
    #Calibration$LinePreference <- if(is.null(Calibration$LinePreference)){
    #    "Narrow"
    #} else if(!is.null(Calibration$LinePreference)){
    #    Calibration$LinePreference
    #}
    
    if(null.strip==TRUE){
        null.list <- pbsapply(Calibration$calList, function(x) tryCatch(is.null(x[[2]]), error=function(e) NULL))
        tryCatch(for(i in names(Calibration$calList)){
            if(null.list[i]==TRUE){
                Calibration$calList[[i]] <- NULL
            }
        }, error=function(e) NULL)
        
        null.list2 <- pbsapply(Calibration$calList, function(x) tryCatch(length(x)<=1, error=function(e) NULL))
        tryCatch(for(i in names(Calibration$calList)){
            if(null.list2[i]==TRUE){
                Calibration$calList[[i]] <- NULL
            }
        }, error=function(e) NULL)

    }
    
    if(length(Calibration$calList) > 0){
        element_names <- order_elements(names(Calibration[["calList"]]))
        n_elements <- length(element_names)
        n_cores <- as.numeric(my.cores)

        # Check if outer-level parallelism is beneficial:
        # - Need non-Windows (mclapply uses forking)
        # - Need multiple elements to process
        # - User must have requested parallel processing
        use_parallel <- allowParallel && get_os() != "windows" && n_elements >= 2 && n_cores >= 2

        if (use_parallel) {
            outer_cores <- min(n_elements, n_cores)
            cal_list <- Calibration[["calList"]]

            calpre_list <- parallel::mclapply(element_names, function(x) {
                calPre(element = x, element.model.list = cal_list[[x]],
                       temp = temp, env.strip = env.strip,
                       xgb_raw = xgb_raw, xgb_unserialize = xgb_unserialize)
            }, mc.cores = outer_cores)

            names(calpre_list) <- element_names
            calpre <- calpre_list
        } else {
            # Sequential processing (Windows or few elements)
            calpre <- list()
            for(x in element_names){
                calpre[[x]] <- calPre(element = x, element.model.list = Calibration[["calList"]][[x]],
                                      temp = temp, env.strip = env.strip,
                                      xgb_raw = xgb_raw, xgb_unserialize = xgb_unserialize)
            }
        }

        Calibration$calList <- calpre
    }
    
    
    if(is.null(Calibration$Definitions)){
        Calibration$Definitions <- data.frame(
        Name=as.vector(as.character(rep("", 75))),
        EnergyMin=as.numeric(rep("", 75)),
        EnergyMax=as.numeric(rep("", 75)),
        stringsAsFactors = FALSE
        )
    }
    
    if(any(names(Calibration$calList) %in% spectralLines)){
        Calibration <- intensity_fix(Calibration)
    }
    
    
    

       
    
    
    return(Calibration)
}

#lmSEapprox <- function(calibration, element){
   #predictions <- cloudCalPredict(Calibration=calibration, count.list=list(calibration$), elements.cal=element, variables=names(calibration$Intensities)[!names(calibration$Intensities) %in% "Spectrum"], valdata=calibration$Intensities[,names(calibration$Intensities)[!names(calibration$Intensities) %in% "Spectrum"]], rounding=4, multiplier=1)

#}

#error_estimation <- function(element_model_list){
   #model <- element_model_list$Model
   #cal.type <- element_model_list$CalTable$CalType[1]

    #if(cal.type==1){

    #}

#}
