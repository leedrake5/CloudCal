###Spectra Loading
read_csv_filename_x <- function(filename){
    ret <- read.csv(file=filename, sep=",", header=FALSE)
    return.res <- as.numeric(as.vector(ret$V2[18]))/1000
    return.chan.counts <-as.numeric(as.vector(ret$V1[22:2069]))
    return.energy <- return.chan.counts*return.res
    return(return.energy)
}
read_csv_filename_x <- cmpfun(read_csv_filename_x)

read_csv_filename_y <- function(filename){
    ret <- read.csv(file=filename, sep=",", header=FALSE)
    return.live.time <- as.numeric(as.vector(ret$V2[10]))
    return.counts <- as.numeric(as.vector(ret$V2[22:2069]))
    return.cps <- return.counts/return.live.time
    return(return.cps)
}
read_csv_filename_y <- cmpfun(read_csv_filename_y)

csvFrame <- function(filepath, filename=NULL){
    if(is.null(filename)){
        filename <- as.character(basename(filepath))
    }
    filename <- make.names(filename)
    filename <- gsub(".csv", "", filename, ignore.case=TRUE)
    return(data.frame(Energy=read_csv_filename_x(filepath), CPS=read_csv_filename_y(filepath), Spectrum=rep(filename, length(read_csv_filename_x(filepath))), stringsAsFactors=FALSE))
}
csvFrame <- cmpfun(csvFrame)

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

fullSpectraProcess <- function(inFile=NULL, gainshiftvalue=0){
       
           if (is.null(inFile)) return(NULL)
           temp = inFile$name
           temp <- gsub(".csv", "", temp)
           id.seq <- seq(1, 2048,1)
           
           n <- length(temp)*id.seq
           
           n.seq <- seq(1, nrow(inFile), 1)
           
           data.list <- pblapply(n.seq, function(x) csvFrame(filepath=inFile[x, "datapath"], filename=inFile[x, "name"]))
           data <- do.call("rbind", data.list)
           data <- as.data.frame(data, stringsAsFactors=FALSE)
       
       if(gainshiftvalue>0){
           data$Energy <- data$Energy + gainshiftvalue
       }
       
       
       return(data)
   }

netCountsProcess <- function(inFile=NULL){
    
        if (is.null(inFile)) return(NULL)
                
        n <- length(inFile$name)
        net.names <- gsub("\\@.*","",inFile$name)
        
        myfiles = pblapply(inFile$datapath,  read_csv_net)
        
        
        myfiles.frame.list <- pblapply(myfiles, data.frame, stringsAsFactors=FALSE)
        nms = unique(unlist(pblapply(myfiles.frame.list, names)))
        myfiles.frame <- as.data.frame(do.call(rbind, lapply(myfiles.frame.list, "[", nms)))
        myfiles.frame <- as.data.frame(sapply(myfiles.frame, as.numeric))
        
        united.frame <- data.frame(net.names, myfiles.frame)
        colnames(united.frame) <- c("Spectrum", names(myfiles.frame))
    
    united.frame <- as.data.frame(united.frame)
    return(united.frame)
    
}

not_all_na <- function(x) any(!is.na(x))
not_any_na <- function(x) all(!is.na(x))

importCSVFrame <- function(filepath, choosen_beam="1"){
    csv_import <- read.csv("/Users/lee/Google Drive/Reply to Frahm 2019/Export Results from Vanta/beamspectra-804734-2019-09-28-15-29-14.csv", header=F, stringsAsFactors=FALSE)
    
    if(csv_import[1, "V1"]=="Std#"){
        importCSVFrameBasic(filepath)
    } else if(csv_import[1, "V1"]=="sep="){
        importCSVFrameDetailed(csv_import)
    }

}
importCSVFrame <- cmpfun(importCSVFrame)

importCSVFrameBasic <- function(filepath){
    csv.frame <- read.csv(filepath)
    metadata <- csv.frame[1,]
    spectra.data <- csv.frame[-1,]
    
    melt.frame <- reshape2::melt(spectra.data, id="Std.")
    data.frame(Energy=as.numeric(as.vector(melt.frame$Std.)), CPS=as.numeric(as.vector(melt.frame$value)), Spectrum=as.vector(melt.frame$variable), stringsAsFactors=FALSE)
}
importCSVFrameBasic <- cmpfun(importCSVFrameBasic)

uniqueBeamsDetailed <- function(csv_import){

    csv_import <- csv_import %>% select_if(not_all_na)
    csv_import <- csv_import[-1,]
    beams <- as.vector((unlist(csv_import[csv_import$V1=="Exposure Number",-1])))
    unique_beams <- unique(beams)
    return(unique_beams)
}
uniqueBeamsDetailed <- cmpfun(uniqueBeamsDetailed)

uniqueBeams <- function(filepath){
    csv_import <- read.csv("/Users/lee/Google Drive/Reply to Frahm 2019/Export Results from Vanta/beamspectra-804734-2019-09-28-15-29-14.csv", header=F, stringsAsFactors=FALSE)
    
    if(csv_import[1, "V1"]=="Std#"){
        "1"
    } else if(csv_import[1, "V1"]=="sep="){
        uniqueBeamsDetailed(csv_import)
    }
}


importCSVFrameDetailed <- function(csv_import, choosen_beam="1"){
    csv_import <- csv_import %>% select_if(not_all_na)
    csv_import <- csv_import[-1,]
    beams <- as.vector((unlist(csv_import[csv_import$V1=="Exposure Number",-1])))
    unique_beams <- unique(beams)
    csv_frame <- csv_import[complete.cases(as.numeric(csv_import$V1)),c(TRUE, beams==choosen_beam)]
    spectra.data <- as.data.frame(apply(csv_frame, 2, function(x) as.numeric(as.character(x))), stringsAsFactors=FALSE)
    
    melt.frame <- reshape2::melt(spectra.data, id="V1")
    data.frame(Energy=as.numeric(as.vector(melt.frame$V1)), CPS=as.numeric(as.vector(melt.frame$value)), Spectrum=as.vector(melt.frame$variable), stringsAsFactors=FALSE)
}
importCSVFrameDetailed <- cmpfun(importCSVFrameDetailed)


readTXTData <- function(filepath, filename){
    filename <- make.names(gsub(".txt", "", filename, ignore.case=TRUE))
    text <- read.table(filepath, sep=",", fill=TRUE, header=FALSE)
    channels <- seq(1, length(text$V1)-4, 1)
    counts <- as.numeric(as.character(text$V1[5:length(text$V1)]))
    filename.vector <- rep(filename, length(text$V1)-4)

    energy <- channels*as.numeric(substr(gsub("Elin=", "", as.character(text$V1[2])), 1, 4))
    
    data.frame(Energy=energy, CPS=counts, Spectrum=filename.vector, stringsAsFactors=FALSE)
    
}
readTXTData <- cmpfun(readTXTData)

readTXTProcess <- function(inFile=NULL, gainshiftvalue=0){
    
        if (is.null(inFile)) return(NULL)
        
        n <- length(inFile$datapath)
        names <- inFile$name
        
        n.seq <- seq(1, nrow(inFile), 1)
        
        data.list <- pblapply(n.seq, function(x) readTXTData(filepath=inFile[x, "datapath"], filename=inFile[x, "name"]))
        data <- do.call("rbind", data.list)
        data <- as.data.frame(data, stringsAsFactors=FALSE)
    
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


readSPTData <- function(filepath, filename){
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
    energy <- as.vector(predict.lm(energy.cal, newdata=newdata))
    energy2 <- newdata[,1]*summary(energy.cal)$coef[2]
    spectra.frame <- data.frame(energy, cps, filename.vector, stringsAsFactors=FALSE)
    colnames(spectra.frame) <- c("Energy", "CPS", "Spectrum")
    return(spectra.frame)
}
readSPTData <- cmpfun(readSPTData)

readElioProcess <- function(inFile=NULL, gainshiftvalue=0){
    
        inFile <- input$file1
        if (is.null(inFile)) return(NULL)
        
        n <- length(inFile$datapath)
        names <- inFile$name
        
        n.seq <- seq(1, nrow(inFile), 1)
        
        data.list <- pblapply(n.seq, function(x) readSPTData(filepath=inFile[x, "datapath"], filename=inFile[x, "name"]))
        data <- do.call("rbind", data.list)
        data <- as.data.frame(data, stringsAsFactors=FALSE)
    
    if(gainshiftvalue>0){
        data$Energy <- data$Energy + gainshiftvalue
    }
    return(data)
}


readMCAData <- function(filepath, filename){
    filename <- make.names(gsub(".mca", "", filename))
    filename.vector <- rep(filename, 4096)
    
    full <- read.csv(filepath, row.names=NULL)
    
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
    energy <- as.vector(predict.lm(energy.cal, newdata=newdata))
    energy2 <- newdata[,1]*summary(energy.cal)$coef[2]
    spectra.frame <- data.frame(energy, cps, filename.vector, stringsAsFactors=FALSE)
    colnames(spectra.frame) <- c("Energy", "CPS", "Spectrum")
    return(spectra.frame)
}
readMCAData <- cmpfun(readMCAData)

readMCAProcess <- function(inFile=NULL, gainshiftvalue=0){
    
        if (is.null(inFile)) return(NULL)
        
        n <- length(inFile$datapath)
        names <- inFile$name
        
        n.seq <- seq(1, nrow(inFile), 1)
        
        data.list <- pbmapply(function(datapath, name) { readMCAData(datapath,name) }, inFile$datapath, inFile$name)
        data <- do.call("rbind", data.list)
        data <- as.data.frame(data, stringsAsFactors=FALSE)
            
    if(gainshiftvalue>0){
        data$Energy <- data$Energy + gainshiftvalue
    }
    return(data)
}

readSPXData <- function(filepath, filename){
    
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
    energy <- newdata[,1]*slope+intercept
    
    spectra.frame <- data.frame(energy, cps, filename.vector, stringsAsFactors=FALSE)
    colnames(spectra.frame) <- c("Energy", "CPS", "Spectrum")
    return(spectra.frame)
    
}
readSPXData <- cmpfun(readSPXData)

readSPXProcess <- function(inFile=NULL, gainshiftvalue=0){
    
        if (is.null(inFile)) return(NULL)
        
        n <- length(inFile$datapath)
        names <- inFile$name
        
        n.seq <- seq(1, nrow(inFile), 1)
        
        data.list <- pblapply(n.seq, function(x) readSPXData(filepath=inFile[x, "datapath"], filename=inFile[x, "name"]))
        data <- do.call("rbind", data.list)
        data <- as.data.frame(data, stringsAsFactors=FALSE)
            
    if(gainshiftvalue>0){
        data$Energy <- data$Energy + gainshiftvalue
    }
    return(data)
}

readPDZ25DataExpiremental <- function(filepath, filename){
    
    filename <- make.names(gsub(".pdz", "", filename))
    filename.vector <- rep(filename, 2048)
    
    nbrOfRecords <- 3000
    integers <- int_to_unit(readBin(con=filepath, what= "int", n=3000, endian="little"))
    floats <- readBin(con=filepath, what="float", size=4, n=nbrOfRecords, endian="little")
    integer.sub <- integers[124:2171]

    sequence <- seq(1, length(integer.sub), 1)

    time.est <- integers[144]/10

        channels <- sequence
        energy <- sequence*.02
        counts <- integer.sub/(integers[144]/10)
        
        unfold(data.frame(Energy=energy, CPS=counts, Spectrum=filename.vector, stringsAsFactors=FALSE))

}
readPDZ25DataExpiremental <- cmpfun(readPDZ25DataExpiremental)


readPDZ24DataExpiremental <- function(filepath, filename){
    
    filename <- make.names(gsub(".pdz", "", filename))
    filename.vector <- rep(filename, 2048)
    
    nbrOfRecords <- 3000
    integers <- int_to_unit(readBin(con=filepath, what= "int", n=3000, endian="little"))
    floats <- readBin(con=filepath, what="float", size=4, n=nbrOfRecords, endian="little")
    integer.sub <- integers[90:2137]
    sequence <- seq(1, length(integer.sub), 1)
    
    time.est <- integer.sub[21]
    
    channels <- sequence
    energy <- sequence*.02
    counts <- integer.sub/(integer.sub[21]/10)
    
    unfold(data.frame(Energy=energy, CPS=counts, Spectrum=filename.vector, stringsAsFactors=FALSE))
    
}
readPDZ24DataExpiremental <- cmpfun(readPDZ24DataExpiremental)


#Rcpp::sourceCpp("pdz.cpp")

readPDZ25Data <- function(filepath, filename){
    
    filename <- make.names(gsub(".pdz", "", filename))
    filename.vector <- rep(filename, 2020)
    
    nbrOfRecords <- 2020
    integers <- readPDZ25(filepath, start=481, size=nbrOfRecords)
    
    sequence <- seq(1, length(integers), 1)
    
    time.est <- integers[21]

    channels <- sequence
    energy <- sequence*.02
    counts <- integers/(integers[144]/10)
    
    data.frame(Energy=energy, CPS=counts, Spectrum=filename.vector, stringsAsFactors=FALSE)
    
}
readPDZ25Data <- cmpfun(readPDZ25Data)


readPDZ25DataManual <- function(filepath, filename, binaryshift){
    
    filename <- make.names(gsub(".pdz", "", filename))
    filename.vector <- rep(filename, 2020)
    
    nbrOfRecords <- 2020
    integers <- readPDZ25(filepath, start=binaryshift, size=nbrOfRecords)
    
    sequence <- seq(1, length(integers), 1)
    
    time.est <- integers[21]
    
    channels <- sequence
    energy <- sequence*.02
    counts <- integers/(integers[144]/10)
    
    data.frame(Energy=energy, CPS=counts, Spectrum=filename.vector, stringsAsFactors=FALSE)
    
}
readPDZ25DataManual <- cmpfun(readPDZ25DataManual)


readPDZ24Data<- function(filepath, filename){
    
    filename <- make.names(gsub(".pdz", "", filename))
    filename.vector <- rep(filename, 2020)
    
    nbrOfRecords <- 2020
    integers <- readPDZ24(filepath, start=361, size=nbrOfRecords)
    sequence <- seq(1, length(integers), 1)
    
    time.est <- integers[21]
    
    channels <- sequence
    energy <- sequence*.02
    counts <- integers/(integers[21]/10)
    
    data.frame(Energy=energy, CPS=counts, Spectrum=filename.vector, stringsAsFactors=FALSE)
    
}
readPDZ24Data <- cmpfun(readPDZ24Data)



readPDZData <- function(filepath, filename=NULL) {
    
    if(is.null(filename)){
        filename <- basename(filepath)
    }
    
    
    nbrOfRecords <- 10000
    floats <- readBin(con=filepath, what="float", size=4, n=nbrOfRecords, endian="little")
    
    if(floats[[9]]=="5"){
        readPDZ25Data(filepath, filename)
    }else {
        readPDZ24Data(filepath, filename)
    }

    
}
readPDZ24Data <- cmpfun(readPDZ24Data)

readPDZProcess <- function(inFile=NULL, gainshiftvalue=0, advanced=FALSE, binaryshift=100){
    
        if (is.null(inFile)) return(NULL)
        
        n <- length(inFile$datapath)
        names <- inFile$name
        
        n.seq <- seq(1, nrow(inFile), 1)
        
        if(advanced==FALSE){
            data.list <- pblapply(n.seq, function(x) readPDZData(filepath=inFile[x, "datapath"], filename=inFile[x, "name"]))
            data <- do.call("rbind", data.list)
            data <- as.data.frame(data, stringsAsFactors=FALSE)
        } else if(advanced==TRUE){
            data.list <- pblapply(n.seq, function(x) readPDZ25DataManual(filepath=inFile[x, "datapath"], filename=inFile[x, "name"], binaryshift=binaryshift))
            data <- do.call("rbind", data.list)
            data <- as.data.frame(data, stringsAsFactors=FALSE)
        }
    
   if(gainshiftvalue>0){
       data$Energy <- data$Energy + gainshiftvalue
   }
    return(data)
}


###Calibration Loading
calPre <- function(element.model.list, element, temp){
    
    temp.list <- list(element.model.list)
    names(temp.list) <- element
    
    new.element.model.list <-
    #tryCatch(
        list(Parameters=importCalConditions(element=element,
        calList=temp.list, temp=temp),
        Model=element.model.list[[2]])
        #, error=function(e) NULL)
        
        if(is.na(new.element.model.list$Parameters$CalTable$LineType[1])){
            new.element.model.list$Parameters$CalTable$LineType[1] <- "Narrow"
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
        
        if(nrow(new.element.model.list$Parameters$CalTable)>1){
            new.element.model.list$Parameters$CalTable <- new.element.model.list$Parameters$CalTable[!duplicated(new.element.model.list$Parameters$CalTable), ]
        }
            
    return(new.element.model.list)
        
}

calRDS <- function(calibration.directory, null.strip=TRUE, temp=FALSE, extensions=FALSE){
    Calibration <- readRDS(calibration.directory)
    
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
        null.list <- sapply(Calibration$calList, function(x) is.null(x[[2]]))
        tryCatch(for(i in names(Calibration$calList)){
            if(null.list[i]==TRUE){
                Calibration$calList[[i]] <- NULL
            }
        }, error=function(e) NULL)

    }
    
    calpre <- pblapply(order_elements(names(Calibration[["calList"]])), function(x) tryCatch(calPre(element=x, element.model.list=Calibration[["calList"]][[x]], temp=temp), error=function(e) NULL))
    names(calpre) <- order_elements(names(Calibration[["calList"]]))
    
    Calibration$calList <- calpre
    
    if(is.null(Calibration$Definitions)){
        Calibration$Definitions <- data.frame(
        Name=as.vector(as.character(rep("", 75))),
        EnergyMin=as.numeric(rep("", 75)),
        EnergyMax=as.numeric(rep("", 75)),
        stringsAsFactors = FALSE
        )
    }
    
    return(Calibration)
}
