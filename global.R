get_os <- function(){
    sysinf <- Sys.info()
    if (!is.null(sysinf)){
        os <- sysinf['sysname']
        if (os == 'Darwin')
        os <- "osx"
    } else { ## mystery machine
        os <- .Platform$OS.type
        if (grepl("^darwin", R.version$os))
        os <- "osx"
        if (grepl("linux-gnu", R.version$os))
        os <- "linux"
    }
    tolower(os)
}

#options(repos = BiocInstaller::biocinstallRepos())
#getOption("repos")
options(download.file.method="libcurl", url.method="libcurl")
list.of.bioconductor <- c("graph", "RBGL", "Rgraphviz")
new.bioconductor <- list.of.bioconductor[!(list.of.bioconductor %in% installed.packages()[,"Package"])]
if(length(new.bioconductor)) source("https://www.bioconductor.org/biocLite.R")
if(length(new.bioconductor)) biocLite(new.bioconductor)


list.of.packages <- c("pbapply", "reshape2", "TTR", "dplyr", "ggtern",  "shiny", "rhandsontable", "random", "DT", "shinythemes", "broom", "shinyjs", "gridExtra", "dtplyr", "formattable", "XML", "corrplot", "scales", "rmarkdown", "markdown",  "httpuv", "stringi", "dplyr", "reticulate", "devtools", "randomForest", "caret", "data.table", "DescTools",  "doSNOW", "doParallel", "baseline",  "pls", "prospectr", "stringi", "ggplot2", "compiler", "itertools", "foreach", "grid", "nnet", "neuralnet")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/", dep = TRUE)


#if(packageVersion("ggplot2")!="2.2.1") devtools::install_version("ggplot2", version = "2.2.1", repos = "http://cran.us.r-project.org", checkBuilt=TRUE)



if("rPDZ" %in% installed.packages()[,"Package"]==FALSE && get_os()=="windows"){
    install.packages("http://www.xrf.guru/packages/rPDZ_1.0.zip", repos=NULL, type="win.binary")
} else if ("rPDZ" %in% installed.packages()[,"Package"]==FALSE && get_os()=="osx"){
    install.packages("http://www.xrf.guru/packages/rPDZ_1.0.tgz", repos=NULL)
} else if ("rPDZ" %in% installed.packages()[,"Package"]==FALSE && get_os()=="linux"){
    install.packages("http://www.xrf.guru/packages/rPDZ_1.0.tar.gz", repos=NULL)
}

library(rPDZ)


###update packages
#update.packages(repos='http://cran.rstudio.com/', ask=FALSE)

###Old ggplot2
#devtools::install_version("ggplot2", version = "2.2.1", repos = "http://cran.us.r-project.org", checkBuilt=TRUE)


#sudo su - -c "R -e \"install.packages(c('shiny', 'pbapply', 'reshape2', 'TTR', 'dplyr', 'ggtern', 'ggplot2', 'shiny', 'rhandsontable', 'random', 'data.table', 'DT', 'shinythemes', 'Cairo', 'broom', 'shinyjs', 'gridExtra', 'dtplyr', 'formattable', 'XML', 'corrplot', 'scales', 'rmarkdown', 'markdown', 'randomForest', 'doMC', 'caret'), repos='http://cran.rstudio.com/')\""

library(grid)
library(shiny)
library(ggplot2)
library(pbapply)
library(reshape2)
library(dplyr)
library(DT)
library(XML)
#library(gRbase)
library(reticulate)
library(Rcpp)
library(data.table)
library(compiler)
library(itertools)
library(foreach)
require(compiler)
library(doParallel)
library(parallel)
library(randomForest)
library(nnet)
library(neuralnet)
library(gridExtra)

enableJIT(3)

options(digits=4)

my.cores <- if(parallel::detectCores()>=3){
    paste0(parallel::detectCores()-2)
} else if(parallel::detectCores()<=2){
    "1"
}


layOut = function(...) {
    
    require(grid)
    
    x <- list(...)
    n <- max(sapply(x, function(x) max(x[[2]])))
    p <- max(sapply(x, function(x) max(x[[3]])))
    pushViewport(viewport(layout = grid.layout(n, p)))
    
    for (i in seq_len(length(x))) {
        print(x[[i]][[1]], vp = viewport(layout.pos.row = x[[i]][[2]],
        layout.pos.col = x[[i]][[3]]))
    }
}

k.lines.directory <- if(file.exists("data/K Line-Table 1.csv")){
    "data/K Line-Table 1.csv"
} else if(!file.exists("data/K Line-Table 1.csv")){
    "https://raw.githubusercontent.com/leedrake5/CloudCal/master/data/K%20Line-Table%201.csv"
}

l.lines.directory <- if(file.exists("data/L Line-Table 1.csv")){
    "data/L Line-Table 1.csv"
} else if(!file.exists("data/L Line-Table 1.csv")){
    "https://raw.githubusercontent.com/leedrake5/CloudCal/master/data/L%20Line-Table%201.csv"
}

fluorescence.lines.directory <- if(file.exists("data/FluorescenceLines.csv")){
    "data/FluorescenceLines.csv"
} else if(!file.exists("data/FluorescenceLines.csv")){
    "https://raw.githubusercontent.com/leedrake5/CloudCal/master/data/FluorescenceLines.csv"
}


######Load lines
k.lines <- read.csv(file=k.lines.directory, sep=",")
l.lines <- read.csv(file=l.lines.directory, sep=",")
fluorescence.lines <- read.csv(fluorescence.lines.directory, sep=",")


line_strip <- function(elements){
    elements <- gsub(".K.alpha", "", elements)
    elements <- gsub(".K.beta", "", elements)
    elements <- gsub(".L.alpha", "", elements)
    elements <- gsub(".L.beta", "", elements)
    elements <- gsub(".M.line", "", elements)
    elements <- gsub(".K12", "", elements)
    elements <- gsub(".L1", "", elements)
    elements
}
line_strip <- cmpfun(line_strip)

atomic_order <- function(element){
    subset(fluorescence.lines, Symbol==line_strip(element))$AtomicNumber
}
atomic_order <- cmpfun(atomic_order)


atomic_order_vector <- function(elements){
    unlist(lapply(elements, atomic_order))
}
atomic_order_vector <- cmpfun(atomic_order_vector)



element_line_pull <- function(element.line){
    element <- strsplit(x=element.line, split="\\.")[[1]][1]
    destination <- strsplit(x=element.line, split="\\.")[[1]][2]
    distance <- strsplit(x=element.line, split="\\.")[[1]][3]
    data.frame(ElementLine=element.line, Element=element, Orbital=destination, Line=distance, stringsAsFactors=FALSE)
}
element_line_pull <- cmpfun(element_line_pull)


Hodder.v.old <- function(y)
{
    
    n<-length(y)
    
    for(i in 1:(n-1)) {
        y[i] <- y[i+1] - y[i]
        y[1:(n-1)]
        y <- abs(y)
    }
    y <- c(0, y[1:(n-1)])
    
    return(y)
}
Hodder.v.old <- cmpfun(Hodder.v.old)


Hodder.v <- function(y)
{
    
    n<-length(y)
    
    for(i in 1:(n-1)) {
        y[i] <- (y[i+1] - y[i])/211
        y[1:(n-1)]
        
    }
    y <- c(0, y[1:(n-1)])
    
    return(y)
}
Hodder.v <- cmpfun(Hodder.v)


int_to_unit <- function (x, adjustment=2^32) {
    x <- as.numeric(x)
    signs <- sign(x)
    x[signs < 0] <- x[signs < 0] + adjustment
    x
}
int_to_unit <- cmpfun(int_to_unit)



recognize_fold <- function(spectrum){
    index <- which(Hodder.v(spectrum$CPS)<(-0.5))
    index[index %in% seq(41, 2040, 1)]
    
}
recognize_fold <- cmpfun(recognize_fold)


unfold_simple <- function(spectrum){
    
    index.seq <- recognize_fold(spectrum)
    
    spectrum$CPSNew <- ifelse(as.numeric(rownames(spectrum)) %in% index.seq, spectrum$CPS+211, spectrum$CPS)
    
    data.frame(Spectrum=spectrum$Spectrum, Energy=spectrum$Energy, CPS=spectrum$CPSNew, stringsAsFactors=FALSE)
    
}
unfold_simple <- cmpfun(unfold_simple)


unfold <- function(spectrum){
    
    first_unfold <- unfold_simple(spectrum)
    second_unfold <- unfold_simple(first_unfold)
    third_unfold <- unfold_simple(second_unfold)
    fourth_unfold <- unfold_simple(third_unfold)
    fourth_unfold
    
}
unfold <- cmpfun(unfold)




cal.lmsummary <-function(lm.object){
    res<-c(paste(as.character(summary(lm.object)$call),collapse=" "),
    length(lm.object$model),
    summary(lm.object)$r.squared,
    summary(lm.object)$adj.r.squared,
    summary(lm.object)$fstatistic,
    pf(summary(lm.object)$fstatistic[1],summary(lm.object)$fstatistic[2],summary(lm.object)$fstatistic[3],lower.tail=FALSE))
    names(res)<-c("Call","n", "R2","Adj. R2",
    "F-statistic","numdf","dendf","p-value")
    return(res)}
cal.lmsummary <- cmpfun(cal.lmsummary)


val.lmsummary <-function(lm.object){
    res<-c(paste(as.character(summary(lm.object)$call),collapse=" "),
    lm.object$coefficients[1],
    lm.object$coefficients[2],
    length(lm.object$model),
    summary(lm.object)$coefficients[2,2],
    summary(lm.object)$r.squared,
    summary(lm.object)$adj.r.squared,
    summary(lm.object)$fstatistic,
    pf(summary(lm.object)$fstatistic[1],summary(lm.object)$fstatistic[2],summary(lm.object)$fstatistic[3],lower.tail=FALSE))
    names(res)<-c("Call","Intercept","Slope","n","Slope SE","R2","Adj. R2",
    "F-statistic","numdf","dendf","p-value")
    return(res)}
val.lmsummary <- cmpfun(val.lmsummary)


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

csvFrame <- function(filepath, filename){
    filename <- gsub(".csv", "", filename, ignore.case=TRUE)
    data.frame(Energy=read_csv_filename_x(filepath), CPS=read_csv_filename_y(filepath), Spectrum=rep(filename, length(read_csv_filename_x(filepath))), stringsAsFactors=FALSE)
}
csvFrame <- cmpfun(csvFrame)


readTXTData <- function(filepath, filename){
    filename <- gsub(".txt", "", filename, ignore.case=TRUE)
    text <- read.table(filepath, sep=",", fill=TRUE, header=FALSE)
    channels <- seq(1, length(text$V1)-4, 1)
    counts <- as.numeric(as.character(text$V1[5:length(text$V1)]))
    filename.vector <- rep(filename, length(text$V1)-4)

    energy <- channels*as.numeric(substr(gsub("Elin=", "", as.character(text$V1[2])), 1, 4))
    
    data.frame(Energy=energy, CPS=counts, Spectrum=filename.vector, stringsAsFactors=FALSE)
    
}
readTXTData <- cmpfun(readTXTData)



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
    filename <- gsub(".spt", "", filename)
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



readMCAData <- function(filepath, filename){
    filename <- gsub(".mca", "", filename)
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



readSPXData <- function(filepath, filename){
    
    filename <- gsub(".spx", "", filename)
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


readPDZ25DataExpiremental <- function(filepath, filename){
    
    filename <- gsub(".pdz", "", filename)
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
    
    filename <- gsub(".pdz", "", filename)
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
    
    filename <- gsub(".pdz", "", filename)
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
    
    filename <- gsub(".pdz", "", filename)
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
    
    filename <- gsub(".pdz", "", filename)
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



readPDZData <- function(filepath, filename) {
    nbrOfRecords <- 10000

    
    floats <- readBin(con=filepath, what="float", size=4, n=nbrOfRecords, endian="little")
    
    if(floats[[9]]=="5"){
        readPDZ25Data(filepath, filename)
    }else {
        readPDZ24Data(filepath, filename)
    }

    
}
readPDZ24Data <- cmpfun(readPDZ24Data)






file.0 <- function(file) {
    if (length(file) > 0)
    {
    return(file)
    }else{
        return(levels(file))
    }
}
file.0 <- cmpfun(file.0)


is.0 <- function(cps, file) {
    file.0 <- function(file) {
        if (length(file) > 0)
        {
            return(file)
        }else{
            return(levels(file))
        }
    }
    if (length(cps) > 0)
    {
        hope <-data.frame(cps, file.0(file))
        return(hope)
    } else {
        empty <- rep(0, length(file.0(file)))
        framed <- data.frame(empty, file.0(file))
        return(framed)
    }
}
is.0 <- cmpfun(is.0)


dt_options <- reactive({
    # dynamically create options for `aoColumns` depending on how many columns are selected.
    toggles <- lapply(1:length(input$show_vars), function(x) list(bSearchable = F))
    # for `species` columns
    toggles[[length(toggles) + 1]] <- list(bSearchable = T)
    
    list(
    aoColumns = toggles,
    bFilter = 1, bSortClasses = 1,
    aLengthMenu = list(c(10,25,50, -1), list('10','25', '50', 'Todas')),
    iDisplayLength = 10
    )
})


ifrm <- function(obj, env = globalenv()) {
    obj <- deparse(substitute(obj))
    if(exists(obj, envir = env)) {
        rm(list = obj, envir = env)
    }
}

ifrm <- cmpfun(ifrm)


lmp <- function (modelobject) {
    if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
    f <- summary(modelobject)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
}
lmp <- cmpfun(lmp)


lm_eqn.old <- function(df){
    m <- lm(y ~ x, df);
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
    list(a = format(coef(m)[1], digits = 2),
    b = format(coef(m)[2], digits = 2),
    r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));
}
lm_eqn.old <- cmpfun(lm_eqn.old)


lm_eqn = function(m) {
    
    l <- list(a = as.numeric(format(coef(m)[1], digits = 2)),
    b = as.numeric(format(abs(coef(m)[2]), digits = 2)),
    r2 = format(summary(m)$r.squared, digits = 3));
    
        eq <- substitute(italic(C)[i] == a + b %.% italic(I)[i]*","~~italic(r)^2~"="~r2,l)
  
    
    as.character(as.expression(eq));
}
lm_eqn <- cmpfun(lm_eqn)


lm_eqn_poly = function(m) {
    
    l <- list(a = as.numeric(format(coef(m)[1], digits = 2)),
    b = as.numeric(format(abs(coef(m)[2]), digits = 2)),
    c = as.numeric(format(abs(coef(m)[3]), digits = 2)),
    r2 = format(summary(m)$r.squared, digits = 3));
    
        eq <- substitute(italic(C)[i] == a + c %.% italic(I)[i]^2 + b %.% italic(I)[i]*","~~italic(r)^2~"="~r2,l)
   
    
    as.character(as.expression(eq));
}
lm_eqn_poly <- cmpfun(lm_eqn_poly)


lm_eqn_val = function(m) {
    
    l <- list(a = as.numeric(format(coef(m)[1], digits = 2)),
    b = as.numeric(format(abs(coef(m)[2]), digits = 2)),
    r2 = format(summary(m)$r.squared, digits = 3));
    
        eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
   
    
    as.character(as.expression(eq));
}
lm_eqn_val <- cmpfun(lm_eqn_val)


numericInput2<-function (inputId, label, value = "",...)
{
    div(style="display:inline-block",
    tags$label(label, `for` = inputId),
    tags$input(id = inputId, type = "text", value = value,...))
}
numericInput2 <- cmpfun(numericInput2)


numericInputRow<-function (inputId, label, min, max,  value = "")
{
    div(style="display:inline-block",
    tags$label(label, `for` = inputId),
    tags$input(id = inputId, type = "text", value = value, class="input-mini", width='20%'))
}
numericInputRow <- cmpfun(numericInputRow)



diagPlot<-function(model){
    p1<-ggplot(model, aes(as.vector(.fitted), as.vector(.resid)))+geom_point()
    p1<-p1+stat_smooth(method="loess")+geom_hline(yintercept=0, col="red", linetype="dashed")
    p1<-p1+xlab("Fitted values")+ylab("Residuals")
    p1<-p1+ggtitle("Residual vs Fitted Plot")+theme_light()
    
    p2 <- ggplot(model, aes(qqnorm(.stdresid)[[1]], .stdresid))+geom_point(na.rm = TRUE)
    p2 <- p2+geom_abline()+xlab("Theoretical Quantiles")+ylab("Standardized Residuals")
    p2 <- p2+ggtitle("Normal Q-Q")+theme_bw()
    p2
    
    p3<-ggplot(model, aes(as.vector(.fitted), sqrt(abs(as.vector(.stdresid)))))+geom_point(na.rm=TRUE)
    p3<-p3+stat_smooth(method="loess", na.rm = TRUE)+xlab("Fitted Value")
    p3<-p3+ylab(expression(sqrt("|Standardized residuals|")))
    p3<-p3+ggtitle("Scale-Location")+theme_light()
    
    p4<-ggplot(model, aes(seq_along(as.vector(.cooksd)), as.vector(.cooksd)))+geom_bar(stat="identity", position="identity")
    p4<-p4+xlab("Obs. Number")+ylab("Cook's distance")
    p4<-p4+ggtitle("Cook's distance")+theme_light()
    
    p5<-ggplot(model, aes(as.vector(.hat), as.vector(.stdresid)))+geom_point(aes(size=as.vector(.cooksd)), na.rm=TRUE)
    p5<-p5+stat_smooth(method="loess", na.rm=TRUE)
    p5<-p5+xlab("Leverage")+ylab("Standardized Residuals")
    p5<-p5+ggtitle("Residual vs Leverage Plot")
    p5<-p5+scale_size_continuous("Cook's Distance", range=c(1,5))
    p5<-p5+theme_light()+theme(legend.position="bottom")
    
    p6<-ggplot(model, aes(as.vector(.hat), as.vector(.cooksd)))+geom_point(na.rm=TRUE)+stat_smooth(method="loess", na.rm=TRUE)
    p6<-p6+xlab("Leverage hii")+ylab("Cook's Distance")
    p6<-p6+ggtitle("Cook's dist vs Leverage hii/(1-hii)")
    p6<-p6+geom_abline(slope=seq(0,3,0.5), color="gray", linetype="dashed")
    p6<-p6+theme_light()
    
    return(list(rvfPlot=p1, qqPlot=p2, sclLocPlot=p3, cdPlot=p4, rvlevPlot=p5, cvlPlot=p6))
}

diagPlot <- cmpfun(diagPlot)


rbind.match.columns <- function(input1, input2) {
    n.input1 <- ncol(input1)
    n.input2 <- ncol(input2)
    
    if (n.input2 < n.input1) {
        TF.names <- which(names(input2) %in% names(input1))
        column.names <- names(input2[, TF.names])
    } else {
        TF.names <- which(names(input1) %in% names(input2))
        column.names <- names(input1[, TF.names])
    }
    
    return(rbind(input1[, column.names], input2[, column.names]))
}
rbind.match.columns <- cmpfun(rbind.match.columns)


strip_glm <- function(cm) {
    cm$y = c()
    cm$model = c()
    
    cm$residuals = c()
    cm$fitted.values = c()
    cm$effects = c()
    cm$qr$qr = c()
    cm$linear.predictors = c()
    cm$weights = c()
    cm$prior.weights = c()
    cm$data = c()
    
    
    cm$family$variance = c()
    cm$family$dev.resids = c()
    cm$family$aic = c()
    cm$family$validmu = c()
    cm$family$simulate = c()
    attr(cm$terms,".Environment") = c()
    attr(cm$formula,".Environment") = c()
    
    cm
}
strip_glm <- cmpfun(strip_glm)


merge_Sum <- function(.df1, .df2, .id_Columns, .match_Columns){
    merged_Columns <- unique(c(names(.df1),names(.df2)))
    merged_df1 <- data.frame(matrix(nrow=nrow(.df1), ncol=length(merged_Columns)), stringsAsFactors=FALSE)
    names(merged_df1) <- merged_Columns
    for (column in merged_Columns){
        if(column %in% .id_Columns | !column %in% names(.df2)){
            merged_df1[, column] <- .df1[, column]
        } else if (!column %in% names(.df1)){
            merged_df1[, column] <- .df2[match(.df1[, .match_Columns],.df2[, .match_Columns]), column]
        } else {
            df1_Values=.df1[, column]
            df2_Values=.df2[match(.df1[, .match_Columns],.df2[, .match_Columns]), column]
            df2_Values[is.na(df2_Values)] <- 0
            merged_df1[, column] <- df1_Values + df2_Values
        }
    }
    return(merged_df1)
}
merge_Sum <- cmpfun(merge_Sum)




parallel_prediction_stats <-function(object,newdata, ...)
{
    
    cl <- makePSOCKcluster(as.numeric(my.cores))
    registerDoParallel(cl)
    num_splits<-as.numeric(my.cores)
    split_testing<-sort(rank(1:nrow(newdata))%%num_splits)
    predictions<-foreach(i=unique(split_testing),
    .combine=c,.packages=c("stats")) %dopar% {
        as.numeric(predict(object,newdata=newdata[split_testing==i,], ...))
    }
    stopCluster(cl)
    predictions
}
parallel_prediction_stats <- cmpfun(parallel_prediction_stats)



parallel_prediction_caret <-function(object,newdata, ...)
{
    
    cl <- makePSOCKcluster(as.numeric(my.cores))
    registerDoParallel(cl)
    num_splits<-as.numeric(my.cores)
    split_testing<-sort(rank(1:nrow(newdata))%%num_splits)
    predictions<-foreach(i=unique(split_testing),
    .combine=c,.packages=c("caret")) %dopar% {
        as.numeric(predict(object,newdata=newdata[split_testing==i,], ...))
    }
    stopCluster(cl)
    predictions
}
parallel_prediction_caret <- cmpfun(parallel_prediction_caret)


GG_save_pdf = function(list, filename) {
    #start pdf
    pdf(filename)
    
    #loop
    for (p in list) {
        print(p)
    }
    
    #end pdf
    dev.off()
    
    invisible(NULL)
}
GG_save_pdf <- cmpfun(GG_save_pdf)


###Train Functions

pull_test <- function(a.vector, a.value.position){
    
    scaled <- scale(a.vector)[,1]
    
    value <- scaled[a.value.position]
    scale.vector <- scaled[-a.value.position]
    
    ZScore <- (value-mean(scale.vector))/sd(scale.vector)
    pvalue <- pnorm(-abs(ZScore))
    is.sig <- pvalue < 0.05
    
    data.frame(Value=a.vector[a.value.position], ZScore=ZScore, pvalue=pvalue, Sig=is.sig, stringsAsFactors=FALSE)
}
pull_test <- cmpfun(pull_test)


Z_frame <- function(a.vector){
    
    do.call("rbind", lapply(seq(1, length(a.vector), 1), function(x) pull_test(a.vector, x)))
}
Z_frame <- cmpfun(Z_frame)


Z_choose <- function(a.vector){
    
    full <- Z_frame(a.vector)
    full[full$Sig,]
    
}
Z_choose <- cmpfun(Z_choose)

variable_select_xrf <- function(intensities, values, analyte){
    
    control <- trainControl(method="cv", number=5)
    seed <- 7
    metric <- "RMSE"
    set.seed(seed)
    
    cal.table <- data.frame(intensities, Concentration=values[,analyte], stringsAsFactors=FALSE)
    fit.lm <- train(Concentration~., data=cal.table, method="lm", metric=metric, preProc=c("center", "scale"), trControl=control)
    importance <- varImp(fit.lm, scale=FALSE)
    importance.frame <- as.data.frame(importance$importance, stringsAsFactors=FALSE)
    elements <- rownames(importance$importance)
    elements[as.numeric(rownames(Z_choose(importance.frame$Overall)))]
    
}
variable_select_xrf <- cmpfun(variable_select_xrf)


variable_select_short_xrf <- function(importance){
    importance.frame <- as.data.frame(importance$importance, stringsAsFactors=FALSE)
    elements <- rownames(importance$importance)
    elements[as.numeric(rownames(Z_choose(importance.frame$Overall)))]
}
variable_select_short_xrf <- cmpfun(variable_select_short_xrf)


black.diamond.directory <- if(file.exists("data/blackdiamond.csv")){
    "data/blackdiamond.csv"
} else if(!file.exists("data/blackdiamond.csv")){
    "https://raw.githubusercontent.com/leedrake5/CloudCal/master/data/blackdiamond.csv"
}

black.diamond.melt.directory <- if(file.exists("data/blackdiamondmelt.csv")){
    "data/blackdiamondmelt.csv"
} else if(!file.exists("data/blackdiamondmelt.csv")){
    "https://raw.githubusercontent.com/leedrake5/CloudCal/master/data/blackdiamondmelt.csv"
}

black.diamond <- read.csv(black.diamond.directory, header=FALSE, sep=",")
black.diamond.melt <- read.csv(file=black.diamond.melt.directory, sep=",")



#k.lines[k.lines < 0.01] <- 1
#l.lines[l.lines < 0.01] <- 1

lines <- data.frame(k.lines, l.lines, stringsAsFactors=FALSE)

H.lines <- data.frame(lines$Ka1[1], lines$Ka2[1], lines$Kb1[1], lines$Kb2[1], lines$Kb3[1], lines$La1[1], lines$La2[1], lines$Lb1[1], lines$Lb2[1], lines$Lb3[1], lines$Lb4[1], lines$Lg1[1], lines$Lg2[1], lines$Lg3[1], lines$Ll[1], stringsAsFactors=FALSE)
colnames(H.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

He.lines <- data.frame(lines$Ka1[2], lines$Ka2[2], lines$Kb1[2], lines$Kb2[2], lines$Kb3[2], lines$La1[2], lines$La2[2], lines$Lb1[2], lines$Lb2[2], lines$Lb3[2], lines$Lb4[2], lines$Lg1[2], lines$Lg2[2], lines$Lg3[2], lines$Ll[2], stringsAsFactors=FALSE)
colnames(He.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Li.lines <- data.frame(lines$Ka1[3], lines$Ka2[3], lines$Kb1[3], lines$Kb2[3], lines$Kb3[3], lines$La1[3], lines$La2[3], lines$Lb1[3], lines$Lb2[3], lines$Lb3[3], lines$Lb4[3], lines$Lg1[3], lines$Lg2[3], lines$Lg3[3], lines$Ll[3], stringsAsFactors=FALSE)
colnames(Li.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Be.lines <- data.frame(lines$Ka1[4], lines$Ka2[4], lines$Kb1[4], lines$Kb2[4], lines$Kb3[4], lines$La1[4], lines$La2[4], lines$Lb1[4], lines$Lb2[4], lines$Lb3[4], lines$Lb4[4], lines$Lg1[4], lines$Lg2[4], lines$Lg3[4], lines$Ll[4], stringsAsFactors=FALSE)
colnames(Be.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

B.lines <- data.frame(lines$Ka1[5], lines$Ka2[5], lines$Kb1[5], lines$Kb2[5], lines$Kb3[5], lines$La1[5], lines$La2[5], lines$Lb1[5], lines$Lb2[5], lines$Lb3[5], lines$Lb4[5], lines$Lg1[5], lines$Lg2[5], lines$Lg3[5], lines$Ll[5], stringsAsFactors=FALSE)
colnames(B.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

C.lines <- data.frame(lines$Ka1[6], lines$Ka2[6], lines$Kb1[6], lines$Kb2[6], lines$Kb3[6], lines$La1[6], lines$La2[6], lines$Lb1[6], lines$Lb2[6], lines$Lb3[6], lines$Lb4[6], lines$Lg1[6], lines$Lg2[6], lines$Lg3[6], lines$Ll[6], stringsAsFactors=FALSE)
colnames(C.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

N.lines <- data.frame(lines$Ka1[7], lines$Ka2[7], lines$Kb1[7], lines$Kb2[7], lines$Kb3[7], lines$La1[7], lines$La2[7], lines$Lb1[7], lines$Lb2[7], lines$Lb3[7], lines$Lb4[7], lines$Lg1[7], lines$Lg2[7], lines$Lg3[7], lines$Ll[7], stringsAsFactors=FALSE)
colnames(N.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

O.lines <- data.frame(lines$Ka1[8], lines$Ka2[8], lines$Kb1[8], lines$Kb2[8], lines$Kb3[8], lines$La1[8], lines$La2[8], lines$Lb1[8], lines$Lb2[8], lines$Lb3[8], lines$Lb4[8], lines$Lg1[8], lines$Lg2[8], lines$Lg3[8], lines$Ll[8], stringsAsFactors=FALSE)
colnames(O.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

F.lines <- data.frame(lines$Ka1[9], lines$Ka2[9], lines$Kb1[9], lines$Kb2[9], lines$Kb3[9], lines$La1[9], lines$La2[9], lines$Lb1[9], lines$Lb2[9], lines$Lb3[9], lines$Lb4[9], lines$Lg1[9], lines$Lg2[9], lines$Lg3[9], lines$Ll[9], stringsAsFactors=FALSE)
colnames(F.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ne.lines <- data.frame(lines$Ka1[10], lines$Ka2[10], lines$Kb1[10], lines$Kb2[10], lines$Kb3[10], lines$La1[10], lines$La2[10], lines$Lb1[10], lines$Lb2[10], lines$Lb3[10], lines$Lb4[10], lines$Lg1[10], lines$Lg2[10], lines$Lg3[10], lines$Ll[10], stringsAsFactors=FALSE)
colnames(Ne.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Na.lines <- data.frame(lines$Ka1[11], lines$Ka2[11], lines$Kb1[11], lines$Kb2[11], lines$Kb3[11], lines$La1[11], lines$La2[11], lines$Lb1[11], lines$Lb2[11], lines$Lb3[11], lines$Lb4[11], lines$Lg1[11], lines$Lg2[11], lines$Lg3[11], lines$Ll[11], stringsAsFactors=FALSE)
colnames(Na.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Mg.lines <- data.frame(lines$Ka1[12], lines$Ka2[12], lines$Kb1[12], lines$Kb2[12], lines$Kb3[12], lines$La1[12], lines$La2[12], lines$Lb1[12], lines$Lb2[12], lines$Lb3[12], lines$Lb4[12], lines$Lg1[12], lines$Lg2[12], lines$Lg3[12], lines$Ll[12], stringsAsFactors=FALSE)
colnames(Mg.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Al.lines <- data.frame(lines$Ka1[13], lines$Ka2[13], lines$Kb1[13], lines$Kb2[13], lines$Kb3[13], lines$La1[13], lines$La2[13], lines$Lb1[13], lines$Lb2[13], lines$Lb3[13], lines$Lb4[13], lines$Lg1[13], lines$Lg2[13], lines$Lg3[13], lines$Ll[13], stringsAsFactors=FALSE)
colnames(Al.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Si.lines <- data.frame(lines$Ka1[14], lines$Ka2[14], lines$Kb1[14], lines$Kb2[14], lines$Kb3[14], lines$La1[14], lines$La2[14], lines$Lb1[14], lines$Lb2[14], lines$Lb3[14], lines$Lb4[14], lines$Lg1[14], lines$Lg2[14], lines$Lg3[14], lines$Ll[14], stringsAsFactors=FALSE)
colnames(Si.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

P.lines <- data.frame(lines$Ka1[15], lines$Ka2[15], lines$Kb1[15], lines$Kb2[15], lines$Kb3[15], lines$La1[15], lines$La2[15], lines$Lb1[15], lines$Lb2[15], lines$Lb3[15], lines$Lb4[15], lines$Lg1[15], lines$Lg2[15], lines$Lg3[15], lines$Ll[15], stringsAsFactors=FALSE)
colnames(P.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

S.lines <- data.frame(lines$Ka1[16], lines$Ka2[16], lines$Kb1[16], lines$Kb2[16], lines$Kb3[16], lines$La1[16], lines$La2[16], lines$Lb1[16], lines$Lb2[16], lines$Lb3[16], lines$Lb4[16], lines$Lg1[16], lines$Lg2[16], lines$Lg3[16], lines$Ll[16], stringsAsFactors=FALSE)
colnames(S.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Cl.lines <- data.frame(lines$Ka1[17], lines$Ka2[17], lines$Kb1[17], lines$Kb2[17], lines$Kb3[17], lines$La1[17], lines$La2[17], lines$Lb1[17], lines$Lb2[17], lines$Lb3[17], lines$Lb4[17], lines$Lg1[17], lines$Lg2[17], lines$Lg3[17], lines$Ll[17], stringsAsFactors=FALSE)
colnames(Cl.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ar.lines <- data.frame(lines$Ka1[18], lines$Ka2[18], lines$Kb1[18], lines$Kb2[18], lines$Kb3[18], lines$La1[18], lines$La2[18], lines$Lb1[18], lines$Lb2[18], lines$Lb3[18], lines$Lb4[18], lines$Lg1[18], lines$Lg2[18], lines$Lg3[18], lines$Ll[18], stringsAsFactors=FALSE)
colnames(Ar.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

K.lines <- data.frame(lines$Ka1[19], lines$Ka2[19], lines$Kb1[19], lines$Kb2[19], lines$Kb3[19], lines$La1[19], lines$La2[19], lines$Lb1[19], lines$Lb2[19], lines$Lb3[19], lines$Lb4[19], lines$Lg1[19], lines$Lg2[19], lines$Lg3[19], lines$Ll[19], stringsAsFactors=FALSE)
colnames(K.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ca.lines <- data.frame(lines$Ka1[20], lines$Ka2[20], lines$Kb1[20], lines$Kb2[20], lines$Kb3[20], lines$La1[20], lines$La2[20], lines$Lb1[20], lines$Lb2[20], lines$Lb3[20], lines$Lb4[20], lines$Lg1[20], lines$Lg2[20], lines$Lg3[20], lines$Ll[20], stringsAsFactors=FALSE)
colnames(Ca.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Sc.lines <- data.frame(lines$Ka1[21], lines$Ka2[21], lines$Kb1[21], lines$Kb2[21], lines$Kb3[21], lines$La1[21], lines$La2[21], lines$Lb1[21], lines$Lb2[21], lines$Lb3[21], lines$Lb4[21], lines$Lg1[21], lines$Lg2[21], lines$Lg3[21], lines$Ll[21], stringsAsFactors=FALSE)
colnames(Sc.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ti.lines <- data.frame(lines$Ka1[22], lines$Ka2[22], lines$Kb1[22], lines$Kb2[22], lines$Kb3[22], lines$La1[22], lines$La2[22], lines$Lb1[22], lines$Lb2[22], lines$Lb3[22], lines$Lb4[22], lines$Lg1[22], lines$Lg2[22], lines$Lg3[22], lines$Ll[22], stringsAsFactors=FALSE)
colnames(Ti.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

V.lines <- data.frame(lines$Ka1[23], lines$Ka2[23], lines$Kb1[23], lines$Kb2[23], lines$Kb3[23], lines$La1[23], lines$La2[23], lines$Lb1[23], lines$Lb2[23], lines$Lb3[23], lines$Lb4[23], lines$Lg1[23], lines$Lg2[23], lines$Lg3[23], lines$Ll[23], stringsAsFactors=FALSE)
colnames(V.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Cr.lines <- data.frame(lines$Ka1[24], lines$Ka2[24], lines$Kb1[24], lines$Kb2[24], lines$Kb3[24], lines$La1[24], lines$La2[24], lines$Lb1[24], lines$Lb2[24], lines$Lb3[24], lines$Lb4[24], lines$Lg1[24], lines$Lg2[24], lines$Lg3[24], lines$Ll[24], stringsAsFactors=FALSE)
colnames(Cr.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Mn.lines <- data.frame(lines$Ka1[25], lines$Ka2[25], lines$Kb1[25], lines$Kb2[25], lines$Kb3[25], lines$La1[25], lines$La2[25], lines$Lb1[25], lines$Lb2[25], lines$Lb3[25], lines$Lb4[25], lines$Lg1[25], lines$Lg2[25], lines$Lg3[25], lines$Ll[25], stringsAsFactors=FALSE)
colnames(Mn.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Fe.lines <- data.frame(lines$Ka1[26], lines$Ka2[26], lines$Kb1[26], lines$Kb2[26], lines$Kb3[26], lines$La1[26], lines$La2[26], lines$Lb1[26], lines$Lb2[26], lines$Lb3[26], lines$Lb4[26], lines$Lg1[26], lines$Lg2[26], lines$Lg3[26], lines$Ll[26], stringsAsFactors=FALSE)
colnames(Fe.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Co.lines <- data.frame(lines$Ka1[27], lines$Ka2[27], lines$Kb1[27], lines$Kb2[27], lines$Kb3[27], lines$La1[27], lines$La2[27], lines$Lb1[27], lines$Lb2[27], lines$Lb3[27], lines$Lb4[27], lines$Lg1[27], lines$Lg2[27], lines$Lg3[27], lines$Ll[27], stringsAsFactors=FALSE)
colnames(Co.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ni.lines <- data.frame(lines$Ka1[28], lines$Ka2[28], lines$Kb1[28], lines$Kb2[28], lines$Kb3[28], lines$La1[28], lines$La2[28], lines$Lb1[28], lines$Lb2[28], lines$Lb3[28], lines$Lb4[28], lines$Lg1[28], lines$Lg2[28], lines$Lg3[28], lines$Ll[28], stringsAsFactors=FALSE)
colnames(Ni.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Cu.lines <- data.frame(lines$Ka1[29], lines$Ka2[29], lines$Kb1[29], lines$Kb2[29], lines$Kb3[29], lines$La1[29], lines$La2[29], lines$Lb1[29], lines$Lb2[29], lines$Lb3[29], lines$Lb4[29], lines$Lg1[29], lines$Lg2[29], lines$Lg3[29], lines$Ll[29], stringsAsFactors=FALSE)
colnames(Cu.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Zn.lines <- data.frame(lines$Ka1[30], lines$Ka2[30], lines$Kb1[30], lines$Kb2[30], lines$Kb3[30], lines$La1[30], lines$La2[30], lines$Lb1[30], lines$Lb2[30], lines$Lb3[30], lines$Lb4[30], lines$Lg1[30], lines$Lg2[30], lines$Lg3[30], lines$Ll[30], stringsAsFactors=FALSE)
colnames(Zn.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ga.lines <- data.frame(lines$Ka1[31], lines$Ka2[31], lines$Kb1[31], lines$Kb2[31], lines$Kb3[31], lines$La1[31], lines$La2[31], lines$Lb1[31], lines$Lb2[31], lines$Lb3[31], lines$Lb4[31], lines$Lg1[31], lines$Lg2[31], lines$Lg3[31], lines$Ll[31], stringsAsFactors=FALSE)
colnames(Ga.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ge.lines <- data.frame(lines$Ka1[32], lines$Ka2[32], lines$Kb1[32], lines$Kb2[32], lines$Kb3[32], lines$La1[32], lines$La2[32], lines$Lb1[32], lines$Lb2[32], lines$Lb3[32], lines$Lb4[32], lines$Lg1[32], lines$Lg2[32], lines$Lg3[32], lines$Ll[32], stringsAsFactors=FALSE)
colnames(Ge.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

As.lines <- data.frame(lines$Ka1[33], lines$Ka2[33], lines$Kb1[33], lines$Kb2[33], lines$Kb3[33], lines$La1[33], lines$La2[33], lines$Lb1[33], lines$Lb2[33], lines$Lb3[33], lines$Lb4[33], lines$Lg1[33], lines$Lg2[33], lines$Lg3[33], lines$Ll[33], stringsAsFactors=FALSE)
colnames(As.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Se.lines <- data.frame(lines$Ka1[34], lines$Ka2[34], lines$Kb1[34], lines$Kb2[34], lines$Kb3[34], lines$La1[34], lines$La2[34], lines$Lb1[34], lines$Lb2[34], lines$Lb3[34], lines$Lb4[34], lines$Lg1[34], lines$Lg2[34], lines$Lg3[34], lines$Ll[34], stringsAsFactors=FALSE)
colnames(Se.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Br.lines <- data.frame(lines$Ka1[35], lines$Ka2[35], lines$Kb1[35], lines$Kb2[35], lines$Kb3[35], lines$La1[35], lines$La2[35], lines$Lb1[35], lines$Lb2[35], lines$Lb3[35], lines$Lb4[35], lines$Lg1[35], lines$Lg2[35], lines$Lg3[35], lines$Ll[35], stringsAsFactors=FALSE)
colnames(Br.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Kr.lines <- data.frame(lines$Ka1[36], lines$Ka2[36], lines$Kb1[36], lines$Kb2[36], lines$Kb3[36], lines$La1[36], lines$La2[36], lines$Lb1[36], lines$Lb2[36], lines$Lb3[36], lines$Lb4[36], lines$Lg1[36], lines$Lg2[36], lines$Lg3[36], lines$Ll[36], stringsAsFactors=FALSE)
colnames(Kr.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Rb.lines <- data.frame(lines$Ka1[37], lines$Ka2[37], lines$Kb1[37], lines$Kb2[37], lines$Kb3[37], lines$La1[37], lines$La2[37], lines$Lb1[37], lines$Lb2[37], lines$Lb3[37], lines$Lb4[37], lines$Lg1[37], lines$Lg2[37], lines$Lg3[37], lines$Ll[37], stringsAsFactors=FALSE)
colnames(Rb.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Sr.lines <- data.frame(lines$Ka1[38], lines$Ka2[38], lines$Kb1[38], lines$Kb2[38], lines$Kb3[38], lines$La1[38], lines$La2[38], lines$Lb1[38], lines$Lb2[38], lines$Lb3[38], lines$Lb4[38], lines$Lg1[38], lines$Lg2[38], lines$Lg3[38], lines$Ll[38], stringsAsFactors=FALSE)
colnames(Sr.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Y.lines <- data.frame(lines$Ka1[39], lines$Ka2[39], lines$Kb1[39], lines$Kb2[39], lines$Kb3[39], lines$La1[39], lines$La2[39], lines$Lb1[39], lines$Lb2[39], lines$Lb3[39], lines$Lb4[39], lines$Lg1[39], lines$Lg2[39], lines$Lg3[39], lines$Ll[39], stringsAsFactors=FALSE)
colnames(Y.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Zr.lines <- data.frame(lines$Ka1[40], lines$Ka2[40], lines$Kb1[40], lines$Kb2[40], lines$Kb3[40], lines$La1[40], lines$La2[40], lines$Lb1[40], lines$Lb2[40], lines$Lb3[40], lines$Lb4[40], lines$Lg1[40], lines$Lg2[40], lines$Lg3[40], lines$Ll[40], stringsAsFactors=FALSE)
colnames(Zr.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Nb.lines <- data.frame(lines$Ka1[41], lines$Ka2[41], lines$Kb1[41], lines$Kb2[41], lines$Kb3[41], lines$La1[41], lines$La2[41], lines$Lb1[41], lines$Lb2[41], lines$Lb3[41], lines$Lb4[41], lines$Lg1[41], lines$Lg2[41], lines$Lg3[41], lines$Ll[41], stringsAsFactors=FALSE)
colnames(Nb.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Mo.lines <- data.frame(lines$Ka1[42], lines$Ka2[42], lines$Kb1[42], lines$Kb2[42], lines$Kb3[42], lines$La1[42], lines$La2[42], lines$Lb1[42], lines$Lb2[42], lines$Lb3[42], lines$Lb4[42], lines$Lg1[42], lines$Lg2[42], lines$Lg3[42], lines$Ll[42], stringsAsFactors=FALSE)
colnames(Mo.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Tc.lines <- data.frame(lines$Ka1[43], lines$Ka2[43], lines$Kb1[43], lines$Kb2[43], lines$Kb3[43], lines$La1[43], lines$La2[43], lines$Lb1[43], lines$Lb2[43], lines$Lb3[43], lines$Lb4[43], lines$Lg1[43], lines$Lg2[43], lines$Lg3[43], lines$Ll[43], stringsAsFactors=FALSE)
colnames(Tc.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ru.lines <- data.frame(lines$Ka1[44], lines$Ka2[44], lines$Kb1[44], lines$Kb2[44], lines$Kb3[44], lines$La1[44], lines$La2[44], lines$Lb1[44], lines$Lb2[44], lines$Lb3[44], lines$Lb4[44], lines$Lg1[44], lines$Lg2[44], lines$Lg3[44], lines$Ll[44], stringsAsFactors=FALSE)
colnames(Ru.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Rh.lines <- data.frame(lines$Ka1[45], lines$Ka2[45], lines$Kb1[45], lines$Kb2[45], lines$Kb3[45], lines$La1[45], lines$La2[45], lines$Lb1[45], lines$Lb2[45], lines$Lb3[45], lines$Lb4[45], lines$Lg1[45], lines$Lg2[45], lines$Lg3[45], lines$Ll[45], stringsAsFactors=FALSE)
colnames(Rh.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Pd.lines <- data.frame(lines$Ka1[46], lines$Ka2[46], lines$Kb1[46], lines$Kb2[46], lines$Kb3[46], lines$La1[46], lines$La2[46], lines$Lb1[46], lines$Lb2[46], lines$Lb3[46], lines$Lb4[46], lines$Lg1[46], lines$Lg2[46], lines$Lg3[46], lines$Ll[46], stringsAsFactors=FALSE)
colnames(Pd.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ag.lines <- data.frame(lines$Ka1[47], lines$Ka2[47], lines$Kb1[47], lines$Kb2[47], lines$Kb3[47], lines$La1[47], lines$La2[47], lines$Lb1[47], lines$Lb2[47], lines$Lb3[47], lines$Lb4[47], lines$Lg1[47], lines$Lg2[47], lines$Lg3[47], lines$Ll[47], stringsAsFactors=FALSE)
colnames(Ag.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Cd.lines <- data.frame(lines$Ka1[48], lines$Ka2[48], lines$Kb1[48], lines$Kb2[48], lines$Kb3[48], lines$La1[48], lines$La2[48], lines$Lb1[48], lines$Lb2[48], lines$Lb3[48], lines$Lb4[48], lines$Lg1[48], lines$Lg2[48], lines$Lg3[48], lines$Ll[48], stringsAsFactors=FALSE)
colnames(Cd.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

In.lines <- data.frame(lines$Ka1[49], lines$Ka2[49], lines$Kb1[49], lines$Kb2[49], lines$Kb3[49], lines$La1[49], lines$La2[49], lines$Lb1[49], lines$Lb2[49], lines$Lb3[49], lines$Lb4[49], lines$Lg1[49], lines$Lg2[49], lines$Lg3[49], lines$Ll[49], stringsAsFactors=FALSE)
colnames(In.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Sn.lines <- data.frame(lines$Ka1[50], lines$Ka2[50], lines$Kb1[50], lines$Kb2[50], lines$Kb3[50], lines$La1[50], lines$La2[50], lines$Lb1[50], lines$Lb2[50], lines$Lb3[50], lines$Lb4[50], lines$Lg1[50], lines$Lg2[50], lines$Lg3[50], lines$Ll[50], stringsAsFactors=FALSE)
colnames(Sn.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Sb.lines <- data.frame(lines$Ka1[51], lines$Ka2[51], lines$Kb1[51], lines$Kb2[51], lines$Kb3[51], lines$La1[51], lines$La2[51], lines$Lb1[51], lines$Lb2[51], lines$Lb3[51], lines$Lb4[51], lines$Lg1[51], lines$Lg2[51], lines$Lg3[51], lines$Ll[51], stringsAsFactors=FALSE)
colnames(Sb.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Te.lines <- data.frame(lines$Ka1[52], lines$Ka2[52], lines$Kb1[52], lines$Kb2[52], lines$Kb3[52], lines$La1[52], lines$La2[52], lines$Lb1[52], lines$Lb2[52], lines$Lb3[52], lines$Lb4[52], lines$Lg1[52], lines$Lg2[52], lines$Lg3[52], lines$Ll[52], stringsAsFactors=FALSE)
colnames(Te.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

I.lines <- data.frame(lines$Ka1[53], lines$Ka2[53], lines$Kb1[53], lines$Kb2[53], lines$Kb3[53], lines$La1[53], lines$La2[53], lines$Lb1[53], lines$Lb2[53], lines$Lb3[53], lines$Lb4[53], lines$Lg1[53], lines$Lg2[53], lines$Lg3[53], lines$Ll[53], stringsAsFactors=FALSE)
colnames(I.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Xe.lines <- data.frame(lines$Ka1[54], lines$Ka2[54], lines$Kb1[54], lines$Kb2[54], lines$Kb3[54], lines$La1[54], lines$La2[54], lines$Lb1[54], lines$Lb2[54], lines$Lb3[54], lines$Lb4[54], lines$Lg1[54], lines$Lg2[54], lines$Lg3[54], lines$Ll[54], stringsAsFactors=FALSE)
colnames(Xe.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Cs.lines <- data.frame(lines$Ka1[55], lines$Ka2[55], lines$Kb1[55], lines$Kb2[55], lines$Kb3[55], lines$La1[55], lines$La2[55], lines$Lb1[55], lines$Lb2[55], lines$Lb3[55], lines$Lb4[55], lines$Lg1[55], lines$Lg2[55], lines$Lg3[55], lines$Ll[55], stringsAsFactors=FALSE)
colnames(Cs.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ba.lines <- data.frame(lines$Ka1[56], lines$Ka2[56], lines$Kb1[56], lines$Kb2[56], lines$Kb3[56], lines$La1[56], lines$La2[56], lines$Lb1[56], lines$Lb2[56], lines$Lb3[56], lines$Lb4[56], lines$Lg1[56], lines$Lg2[56], lines$Lg3[56], lines$Ll[56], stringsAsFactors=FALSE)
colnames(Ba.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

La.lines <- data.frame(lines$Ka1[57], lines$Ka2[57], lines$Kb1[57], lines$Kb2[57], lines$Kb3[57], lines$La1[57], lines$La2[57], lines$Lb1[57], lines$Lb2[57], lines$Lb3[57], lines$Lb4[57], lines$Lg1[57], lines$Lg2[57], lines$Lg3[57], lines$Ll[57], stringsAsFactors=FALSE)
colnames(La.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ce.lines <- data.frame(lines$Ka1[58], lines$Ka2[58], lines$Kb1[58], lines$Kb2[58], lines$Kb3[58], lines$La1[58], lines$La2[58], lines$Lb1[58], lines$Lb2[58], lines$Lb3[58], lines$Lb4[58], lines$Lg1[58], lines$Lg2[58], lines$Lg3[58], lines$Ll[58], stringsAsFactors=FALSE)
colnames(Ce.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Pr.lines <- data.frame(lines$Ka1[59], lines$Ka2[59], lines$Kb1[59], lines$Kb2[59], lines$Kb3[59], lines$La1[59], lines$La2[59], lines$Lb1[59], lines$Lb2[59], lines$Lb3[59], lines$Lb4[59], lines$Lg1[59], lines$Lg2[59], lines$Lg3[59], lines$Ll[59], stringsAsFactors=FALSE)
colnames(Pr.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Nd.lines <- data.frame(lines$Ka1[60], lines$Ka2[60], lines$Kb1[60], lines$Kb2[60], lines$Kb3[60], lines$La1[60], lines$La2[60], lines$Lb1[60], lines$Lb2[60], lines$Lb3[60], lines$Lb4[60], lines$Lg1[60], lines$Lg2[60], lines$Lg3[60], lines$Ll[60], stringsAsFactors=FALSE)
colnames(Nd.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Pm.lines <- data.frame(lines$Ka1[61], lines$Ka2[61], lines$Kb1[61], lines$Kb2[61], lines$Kb3[61], lines$La1[61], lines$La2[61], lines$Lb1[61], lines$Lb2[61], lines$Lb3[61], lines$Lb4[61], lines$Lg1[61], lines$Lg2[61], lines$Lg3[61], lines$Ll[61], stringsAsFactors=FALSE)
colnames(Pm.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Sm.lines <- data.frame(lines$Ka1[62], lines$Ka2[62], lines$Kb1[62], lines$Kb2[62], lines$Kb3[62], lines$La1[62], lines$La2[62], lines$Lb1[62], lines$Lb2[62], lines$Lb3[62], lines$Lb4[62], lines$Lg1[62], lines$Lg2[62], lines$Lg3[62], lines$Ll[62], stringsAsFactors=FALSE)
colnames(Sm.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Eu.lines <- data.frame(lines$Ka1[63], lines$Ka2[63], lines$Kb1[63], lines$Kb2[63], lines$Kb3[63], lines$La1[63], lines$La2[63], lines$Lb1[63], lines$Lb2[63], lines$Lb3[63], lines$Lb4[63], lines$Lg1[63], lines$Lg2[63], lines$Lg3[63], lines$Ll[63], stringsAsFactors=FALSE)
colnames(Eu.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Gd.lines <- data.frame(lines$Ka1[64], lines$Ka2[64], lines$Kb1[64], lines$Kb2[64], lines$Kb3[64], lines$La1[64], lines$La2[64], lines$Lb1[64], lines$Lb2[64], lines$Lb3[64], lines$Lb4[64], lines$Lg1[64], lines$Lg2[64], lines$Lg3[64], lines$Ll[64], stringsAsFactors=FALSE)
colnames(Gd.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Tb.lines <- data.frame(lines$Ka1[65], lines$Ka2[65], lines$Kb1[65], lines$Kb2[65], lines$Kb3[65], lines$La1[65], lines$La2[65], lines$Lb1[65], lines$Lb2[65], lines$Lb3[65], lines$Lb4[65], lines$Lg1[65], lines$Lg2[65], lines$Lg3[65], lines$Ll[65], stringsAsFactors=FALSE)
colnames(Tb.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Dy.lines <- data.frame(lines$Ka1[66], lines$Ka2[66], lines$Kb1[66], lines$Kb2[66], lines$Kb3[66], lines$La1[66], lines$La2[66], lines$Lb1[66], lines$Lb2[66], lines$Lb3[66], lines$Lb4[66], lines$Lg1[66], lines$Lg2[66], lines$Lg3[66], lines$Ll[66], stringsAsFactors=FALSE)
colnames(Dy.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ho.lines <- data.frame(lines$Ka1[67], lines$Ka2[67], lines$Kb1[67], lines$Kb2[67], lines$Kb3[67], lines$La1[67], lines$La2[67], lines$Lb1[67], lines$Lb2[67], lines$Lb3[67], lines$Lb4[67], lines$Lg1[67], lines$Lg2[67], lines$Lg3[67], lines$Ll[67], stringsAsFactors=FALSE)
colnames(Ho.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Er.lines <- data.frame(lines$Ka1[68], lines$Ka2[68], lines$Kb1[68], lines$Kb2[68], lines$Kb3[68], lines$La1[68], lines$La2[68], lines$Lb1[68], lines$Lb2[68], lines$Lb3[68], lines$Lb4[68], lines$Lg1[68], lines$Lg2[68], lines$Lg3[68], lines$Ll[68], stringsAsFactors=FALSE)
colnames(Er.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Tm.lines <- data.frame(lines$Ka1[69], lines$Ka2[69], lines$Kb1[69], lines$Kb2[69], lines$Kb3[69], lines$La1[69], lines$La2[69], lines$Lb1[69], lines$Lb2[69], lines$Lb3[69], lines$Lb4[69], lines$Lg1[69], lines$Lg2[69], lines$Lg3[69], lines$Ll[69], stringsAsFactors=FALSE)
colnames(Tm.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Yb.lines <- data.frame(lines$Ka1[70], lines$Ka2[70], lines$Kb1[70], lines$Kb2[70], lines$Kb3[70], lines$La1[70], lines$La2[70], lines$Lb1[70], lines$Lb2[70], lines$Lb3[70], lines$Lb4[70], lines$Lg1[70], lines$Lg2[70], lines$Lg3[70], lines$Ll[70], stringsAsFactors=FALSE)
colnames(Yb.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Lu.lines <- data.frame(lines$Ka1[71], lines$Ka2[71], lines$Kb1[71], lines$Kb2[71], lines$Kb3[71], lines$La1[71], lines$La2[71], lines$Lb1[71], lines$Lb2[71], lines$Lb3[71], lines$Lb4[71], lines$Lg1[71], lines$Lg2[71], lines$Lg3[71], lines$Ll[71], stringsAsFactors=FALSE)
colnames(Lu.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Hf.lines <- data.frame(lines$Ka1[72], lines$Ka2[72], lines$Kb1[72], lines$Kb2[72], lines$Kb3[72], lines$La1[72], lines$La2[72], lines$Lb1[72], lines$Lb2[72], lines$Lb3[72], lines$Lb4[72], lines$Lg1[72], lines$Lg2[72], lines$Lg3[72], lines$Ll[72], stringsAsFactors=FALSE)
colnames(Hf.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ta.lines <- data.frame(lines$Ka1[73], lines$Ka2[73], lines$Kb1[73], lines$Kb2[73], lines$Kb3[73], lines$La1[73], lines$La2[73], lines$Lb1[73], lines$Lb2[73], lines$Lb3[73], lines$Lb4[73], lines$Lg1[73], lines$Lg2[73], lines$Lg3[73], lines$Ll[73], stringsAsFactors=FALSE)
colnames(Ta.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

W.lines <- data.frame(lines$Ka1[74], lines$Ka2[74], lines$Kb1[74], lines$Kb2[74], lines$Kb3[74], lines$La1[74], lines$La2[74], lines$Lb1[74], lines$Lb2[74], lines$Lb3[74], lines$Lb4[74], lines$Lg1[74], lines$Lg2[74], lines$Lg3[74], lines$Ll[74], stringsAsFactors=FALSE)
colnames(W.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Re.lines <- data.frame(lines$Ka1[75], lines$Ka2[75], lines$Kb1[75], lines$Kb2[75], lines$Kb3[75], lines$La1[75], lines$La2[75], lines$Lb1[75], lines$Lb2[75], lines$Lb3[75], lines$Lb4[75], lines$Lg1[75], lines$Lg2[75], lines$Lg3[75], lines$Ll[75], stringsAsFactors=FALSE)
colnames(Re.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Os.lines <- data.frame(lines$Ka1[76], lines$Ka2[76], lines$Kb1[76], lines$Kb2[76], lines$Kb3[76], lines$La1[76], lines$La2[76], lines$Lb1[76], lines$Lb2[76], lines$Lb3[76], lines$Lb4[76], lines$Lg1[76], lines$Lg2[76], lines$Lg3[76], lines$Ll[76], stringsAsFactors=FALSE)
colnames(Os.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ir.lines <- data.frame(lines$Ka1[77], lines$Ka2[77], lines$Kb1[77], lines$Kb2[77], lines$Kb3[77], lines$La1[77], lines$La2[77], lines$Lb1[77], lines$Lb2[77], lines$Lb3[77], lines$Lb4[77], lines$Lg1[77], lines$Lg2[77], lines$Lg3[77], lines$Ll[77], stringsAsFactors=FALSE)
colnames(Ir.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Pt.lines <- data.frame(lines$Ka1[78], lines$Ka2[78], lines$Kb1[78], lines$Kb2[78], lines$Kb3[78], lines$La1[78], lines$La2[78], lines$Lb1[78], lines$Lb2[78], lines$Lb3[78], lines$Lb4[78], lines$Lg1[78], lines$Lg2[78], lines$Lg3[78], lines$Ll[78], stringsAsFactors=FALSE)
colnames(Pt.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Au.lines <- data.frame(lines$Ka1[79], lines$Ka2[79], lines$Kb1[79], lines$Kb2[79], lines$Kb3[79], lines$La1[79], lines$La2[79], lines$Lb1[79], lines$Lb2[79], lines$Lb3[79], lines$Lb4[79], lines$Lg1[79], lines$Lg2[79], lines$Lg3[79], lines$Ll[79], stringsAsFactors=FALSE)
colnames(Au.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Hg.lines <- data.frame(lines$Ka1[80], lines$Ka2[80], lines$Kb1[80], lines$Kb2[80], lines$Kb3[80], lines$La1[80], lines$La2[80], lines$Lb1[80], lines$Lb2[80], lines$Lb3[80], lines$Lb4[80], lines$Lg1[80], lines$Lg2[80], lines$Lg3[80], lines$Ll[80], stringsAsFactors=FALSE)
colnames(Hg.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Tl.lines <- data.frame(lines$Ka1[81], lines$Ka2[81], lines$Kb1[81], lines$Kb2[81], lines$Kb3[81], lines$La1[81], lines$La2[81], lines$Lb1[81], lines$Lb2[81], lines$Lb3[81], lines$Lb4[81], lines$Lg1[81], lines$Lg2[81], lines$Lg3[81], lines$Ll[81], stringsAsFactors=FALSE)
colnames(Tl.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Pb.lines <- data.frame(lines$Ka1[82], lines$Ka2[82], lines$Kb1[82], lines$Kb2[82], lines$Kb3[82], lines$La1[82], lines$La2[82], lines$Lb1[82], lines$Lb2[82], lines$Lb3[82], lines$Lb4[82], lines$Lg1[82], lines$Lg2[82], lines$Lg3[82], lines$Ll[82], stringsAsFactors=FALSE)
colnames(Pb.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Bi.lines <- data.frame(lines$Ka1[83], lines$Ka2[83], lines$Kb1[83], lines$Kb2[83], lines$Kb3[83], lines$La1[83], lines$La2[83], lines$Lb1[83], lines$Lb2[83], lines$Lb3[83], lines$Lb4[83], lines$Lg1[83], lines$Lg2[83], lines$Lg3[83], lines$Ll[83], stringsAsFactors=FALSE)
colnames(Bi.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Po.lines <- data.frame(lines$Ka1[84], lines$Ka2[84], lines$Kb1[84], lines$Kb2[84], lines$Kb3[84], lines$La1[84], lines$La2[84], lines$Lb1[84], lines$Lb2[84], lines$Lb3[84], lines$Lb4[84], lines$Lg1[84], lines$Lg2[84], lines$Lg3[84], lines$Ll[84], stringsAsFactors=FALSE)
colnames(Po.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

At.lines <- data.frame(lines$Ka1[85], lines$Ka2[85], lines$Kb1[85], lines$Kb2[85], lines$Kb3[85], lines$La1[85], lines$La2[85], lines$Lb1[85], lines$Lb2[85], lines$Lb3[85], lines$Lb4[85], lines$Lg1[85], lines$Lg2[85], lines$Lg3[85], lines$Ll[85], stringsAsFactors=FALSE)
colnames(At.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Rn.lines <- data.frame(lines$Ka1[86], lines$Ka2[86], lines$Kb1[86], lines$Kb2[86], lines$Kb3[86], lines$La1[86], lines$La2[86], lines$Lb1[86], lines$Lb2[86], lines$Lb3[86], lines$Lb4[86], lines$Lg1[86], lines$Lg2[86], lines$Lg3[86], lines$Ll[86], stringsAsFactors=FALSE)
colnames(Rn.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Fr.lines <- data.frame(lines$Ka1[87], lines$Ka2[87], lines$Kb1[87], lines$Kb2[87], lines$Kb3[87], lines$La1[87], lines$La2[87], lines$Lb1[87], lines$Lb2[87], lines$Lb3[87], lines$Lb4[87], lines$Lg1[87], lines$Lg2[87], lines$Lg3[87], lines$Ll[87], stringsAsFactors=FALSE)
colnames(Fr.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ra.lines <- data.frame(lines$Ka1[88], lines$Ka2[88], lines$Kb1[88], lines$Kb2[88], lines$Kb3[88], lines$La1[88], lines$La2[88], lines$Lb1[88], lines$Lb2[88], lines$Lb3[88], lines$Lb4[88], lines$Lg1[88], lines$Lg2[88], lines$Lg3[88], lines$Ll[88], stringsAsFactors=FALSE)
colnames(Ra.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ac.lines <- data.frame(lines$Ka1[89], lines$Ka2[89], lines$Kb1[89], lines$Kb2[89], lines$Kb3[89], lines$La1[89], lines$La2[89], lines$Lb1[89], lines$Lb2[89], lines$Lb3[89], lines$Lb4[89], lines$Lg1[89], lines$Lg2[89], lines$Lg3[89], lines$Ll[89], stringsAsFactors=FALSE)
colnames(Ac.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Th.lines <- data.frame(lines$Ka1[90], lines$Ka2[90], lines$Kb1[90], lines$Kb2[90], lines$Kb3[90], lines$La1[90], lines$La2[90], lines$Lb1[90], lines$Lb2[90], lines$Lb3[90], lines$Lb4[90], lines$Lg1[90], lines$Lg2[90], lines$Lg3[90], lines$Ll[90], stringsAsFactors=FALSE)
colnames(Th.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Pa.lines <- data.frame(lines$Ka1[91], lines$Ka2[91], lines$Kb1[91], lines$Kb2[91], lines$Kb3[91], lines$La1[91], lines$La2[91], lines$Lb1[91], lines$Lb2[91], lines$Lb3[91], lines$Lb4[91], lines$Lg1[91], lines$Lg2[91], lines$Lg3[91], lines$Ll[91], stringsAsFactors=FALSE)
colnames(Pa.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

U.lines <- data.frame(lines$Ka1[92], lines$Ka2[92], lines$Kb1[92], lines$Kb2[92], lines$Kb3[92], lines$La1[92], lines$La2[92], lines$Lb1[92], lines$Lb2[92], lines$Lb3[92], lines$Lb4[92], lines$Lg1[92], lines$Lg2[92], lines$Lg3[92], lines$Ll[92], stringsAsFactors=FALSE)
colnames(U.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Pu.lines <- data.frame(lines$Ka1[94], lines$Ka2[94], lines$Kb1[94], lines$Kb2[94], lines$Kb3[94], lines$La1[94], lines$La2[94], lines$Lb1[94], lines$Lb2[94], lines$Lb3[94], lines$Lb4[94], lines$Lg1[94], lines$Lg2[94], lines$Lg3[94], lines$Ll[94], stringsAsFactors=FALSE)
colnames(Pu.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

H.K <- c(H.lines$Ka1, H.lines$Ka2, H.lines$Kb1, H.lines$Kb2, H.lines$Kb3)
He.K <- c(He.lines$Ka1, He.lines$Ka2, He.lines$Kb1, He.lines$Kb2, He.lines$Kb3)
Li.K <- c(Li.lines$Ka1, Li.lines$Ka2, Li.lines$Kb1, Li.lines$Kb2, Li.lines$Kb3)
Be.K <- c(Be.lines$Ka1, Be.lines$Ka2, Be.lines$Kb1, Be.lines$Kb2, Be.lines$Kb3)
B.K <- c(B.lines$Ka1, B.lines$Ka2, B.lines$Kb1, B.lines$Kb2, B.lines$Kb3)
C.K <- c(C.lines$Ka1, C.lines$Ka2, C.lines$Kb1, C.lines$Kb2, C.lines$Kb3)
N.K <- c(N.lines$Ka1, N.lines$Ka2, N.lines$Kb1, N.lines$Kb2, N.lines$Kb3)
O.K <- c(O.lines$Ka1, O.lines$Ka2, O.lines$Kb1, O.lines$Kb2, O.lines$Kb3)
F.K <- c(F.lines$Ka1, F.lines$Ka2, F.lines$Kb1, F.lines$Kb2, F.lines$Kb3)
Ne.K <- c(Ne.lines$Ka1, Ne.lines$Ka2, Ne.lines$Kb1, Ne.lines$Kb2, Ne.lines$Kb3)
Na.K <- c(Na.lines$Ka1, Na.lines$Ka2, Na.lines$Kb1, Na.lines$Kb2, Na.lines$Kb3)
Mg.K <- c(Mg.lines$Ka1, Mg.lines$Ka2, Mg.lines$Kb1, Mg.lines$Kb2, Mg.lines$Kb3)
Al.K <- c(Al.lines$Ka1, Al.lines$Ka2, Al.lines$Kb1, Al.lines$Kb2, Al.lines$Kb3)
Si.K <- c(Si.lines$Ka1, Si.lines$Ka2, Si.lines$Kb1, Si.lines$Kb2, Si.lines$Kb3)
P.K <- c(P.lines$Ka1, P.lines$Ka2, P.lines$Kb1, P.lines$Kb2, P.lines$Kb3)
S.K <- c(S.lines$Ka1, S.lines$Ka2, S.lines$Kb1, S.lines$Kb2, S.lines$Kb3)
Cl.K <- c(Cl.lines$Ka1, Cl.lines$Ka2, Cl.lines$Kb1, Cl.lines$Kb2, Cl.lines$Kb3)
Ar.K <- c(Ar.lines$Ka1, Ar.lines$Ka2, Ar.lines$Kb1, Ar.lines$Kb2, Ar.lines$Kb3)
K.K <- c(K.lines$Ka1, K.lines$Ka2, K.lines$Kb1, K.lines$Kb2, K.lines$Kb3)
Ca.K <- c(Ca.lines$Ka1, Ca.lines$Ka2, Ca.lines$Kb1, Ca.lines$Kb2, Ca.lines$Kb3)
Sc.K <- c(Sc.lines$Ka1, Sc.lines$Ka2, Sc.lines$Kb1, Sc.lines$Kb2, Sc.lines$Kb3)
Ti.K <- c(Ti.lines$Ka1, Ti.lines$Ka2, Ti.lines$Kb1, Ti.lines$Kb2, Ti.lines$Kb3)
V.K <- c(V.lines$Ka1, V.lines$Ka2, V.lines$Kb1, V.lines$Kb2, V.lines$Kb3)
Cr.K <- c(Cr.lines$Ka1, Cr.lines$Ka2, Cr.lines$Kb1, Cr.lines$Kb2, Cr.lines$Kb3)
Mn.K <- c(Mn.lines$Ka1, Mn.lines$Ka2, Mn.lines$Kb1, Mn.lines$Kb2, Mn.lines$Kb3)
Fe.K <- c(Fe.lines$Ka1, Fe.lines$Ka2, Fe.lines$Kb1, Fe.lines$Kb2, Fe.lines$Kb3)
Co.K <- c(Co.lines$Ka1, Co.lines$Ka2, Co.lines$Kb1, Co.lines$Kb2, Co.lines$Kb3)
Ni.K <- c(Ni.lines$Ka1, Ni.lines$Ka2, Ni.lines$Kb1, Ni.lines$Kb2, Ni.lines$Kb3)
Cu.K <- c(Cu.lines$Ka1, Cu.lines$Ka2, Cu.lines$Kb1, Cu.lines$Kb2, Cu.lines$Kb3)
Zn.K <- c(Zn.lines$Ka1, Zn.lines$Ka2, Zn.lines$Kb1, Zn.lines$Kb2, Zn.lines$Kb3)
Ga.K <- c(Ga.lines$Ka1, Ga.lines$Ka2, Ga.lines$Kb1, Ga.lines$Kb2, Ga.lines$Kb3)
Ge.K <- c(Ge.lines$Ka1, Ge.lines$Ka2, Ge.lines$Kb1, Ge.lines$Kb2, Ge.lines$Kb3)
As.K <- c(As.lines$Ka1, As.lines$Ka2, As.lines$Kb1, As.lines$Kb2, As.lines$Kb3)
Se.K <- c(Se.lines$Ka1, Se.lines$Ka2, Se.lines$Kb1, Se.lines$Kb2, Se.lines$Kb3)
Br.K <- c(Br.lines$Ka1, Br.lines$Ka2, Br.lines$Kb1, Br.lines$Kb2, Br.lines$Kb3)
Kr.K <- c(Kr.lines$Ka1, Kr.lines$Ka2, Kr.lines$Kb1, Kr.lines$Kb2, Kr.lines$Kb3)
Rb.K <- c(Rb.lines$Ka1, Rb.lines$Ka2, Rb.lines$Kb1, Rb.lines$Kb2, Rb.lines$Kb3)
Sr.K <- c(Sr.lines$Ka1, Sr.lines$Ka2, Sr.lines$Kb1, Sr.lines$Kb2, Sr.lines$Kb3)
Y.K <- c(Y.lines$Ka1, Y.lines$Ka2, Y.lines$Kb1, Y.lines$Kb2, Y.lines$Kb3)
Zr.K <- c(Zr.lines$Ka1, Zr.lines$Ka2, Zr.lines$Kb1, Zr.lines$Kb2, Zr.lines$Kb3)
Nb.K <- c(Nb.lines$Ka1, Nb.lines$Ka2, Nb.lines$Kb1, Nb.lines$Kb2, Nb.lines$Kb3)
Mo.K <- c(Mo.lines$Ka1, Mo.lines$Ka2, Mo.lines$Kb1, Mo.lines$Kb2, Mo.lines$Kb3)
Tc.K <- c(Tc.lines$Ka1, Tc.lines$Ka2, Tc.lines$Kb1, Tc.lines$Kb2, Tc.lines$Kb3)
Ru.K <- c(Ru.lines$Ka1, Ru.lines$Ka2, Ru.lines$Kb1, Ru.lines$Kb2, Ru.lines$Kb3)
Rh.K <- c(Rh.lines$Ka1, Rh.lines$Ka2, Rh.lines$Kb1, Rh.lines$Kb2, Rh.lines$Kb3)
Pd.K <- c(Pd.lines$Ka1, Pd.lines$Ka2, Pd.lines$Kb1, Pd.lines$Kb2, Pd.lines$Kb3)
Ag.K <- c(Ag.lines$Ka1, Ag.lines$Ka2, Ag.lines$Kb1, Ag.lines$Kb2, Ag.lines$Kb3)
Cd.K <- c(Cd.lines$Ka1, Cd.lines$Ka2, Cd.lines$Kb1, Cd.lines$Kb2, Cd.lines$Kb3)
In.K <- c(In.lines$Ka1, In.lines$Ka2, In.lines$Kb1, In.lines$Kb2, In.lines$Kb3)
Sn.K <- c(Sn.lines$Ka1, Sn.lines$Ka2, Sn.lines$Kb1, Sn.lines$Kb2, Sn.lines$Kb3)
Sb.K <- c(Sb.lines$Ka1, Sb.lines$Ka2, Sb.lines$Kb1, Sb.lines$Kb2, Sb.lines$Kb3)
Te.K <- c(Te.lines$Ka1, Te.lines$Ka2, Te.lines$Kb1, Te.lines$Kb2, Te.lines$Kb3)
I.K <- c(I.lines$Ka1, I.lines$Ka2, I.lines$Kb1, I.lines$Kb2, I.lines$Kb3)
Xe.K <- c(Xe.lines$Ka1, Xe.lines$Ka2, Xe.lines$Kb1, Xe.lines$Kb2, Xe.lines$Kb3)
Cs.K <- c(Cs.lines$Ka1, Cs.lines$Ka2, Cs.lines$Kb1, Cs.lines$Kb2, Cs.lines$Kb3)
Ba.K <- c(Ba.lines$Ka1, Ba.lines$Ka2, Ba.lines$Kb1, Ba.lines$Kb2, Ba.lines$Kb3)
La.K <- c(La.lines$Ka1, La.lines$Ka2, La.lines$Kb1, La.lines$Kb2, La.lines$Kb3)
Ce.K <- c(Ce.lines$Ka1, Ce.lines$Ka2, Ce.lines$Kb1, Ce.lines$Kb2, Ce.lines$Kb3)
Pr.K <- c(Pr.lines$Ka1, Pr.lines$Ka2, Pr.lines$Kb1, Pr.lines$Kb2, Pr.lines$Kb3)
Nd.K <- c(Nd.lines$Ka1, Nd.lines$Ka2, Nd.lines$Kb1, Nd.lines$Kb2, Nd.lines$Kb3)
Pm.K <- c(Pm.lines$Ka1, Pm.lines$Ka2, Pm.lines$Kb1, Pm.lines$Kb2, Pm.lines$Kb3)
Sm.K <- c(Sm.lines$Ka1, Sm.lines$Ka2, Sm.lines$Kb1, Sm.lines$Kb2, Sm.lines$Kb3)
Eu.K <- c(Eu.lines$Ka1, Eu.lines$Ka2, Eu.lines$Kb1, Eu.lines$Kb2, Eu.lines$Kb3)
Gd.K <- c(Gd.lines$Ka1, Gd.lines$Ka2, Gd.lines$Kb1, Gd.lines$Kb2, Gd.lines$Kb3)
Tb.K <- c(Tb.lines$Ka1, Tb.lines$Ka2, Tb.lines$Kb1, Tb.lines$Kb2, Tb.lines$Kb3)
Dy.K <- c(Dy.lines$Ka1, Dy.lines$Ka2, Dy.lines$Kb1, Dy.lines$Kb2, Dy.lines$Kb3)
Ho.K <- c(Ho.lines$Ka1, Ho.lines$Ka2, Ho.lines$Kb1, Ho.lines$Kb2, Ho.lines$Kb3)
Er.K <- c(Er.lines$Ka1, Er.lines$Ka2, Er.lines$Kb1, Er.lines$Kb2, Er.lines$Kb3)
Tm.K <- c(Tm.lines$Ka1, Tm.lines$Ka2, Tm.lines$Kb1, Tm.lines$Kb2, Tm.lines$Kb3)
Yb.K <- c(Yb.lines$Ka1, Yb.lines$Ka2, Yb.lines$Kb1, Yb.lines$Kb2, Yb.lines$Kb3)
Lu.K <- c(Lu.lines$Ka1, Lu.lines$Ka2, Lu.lines$Kb1, Lu.lines$Kb2, Lu.lines$Kb3)
Hf.K <- c(Hf.lines$Ka1, Hf.lines$Ka2, Hf.lines$Kb1, Hf.lines$Kb2, Hf.lines$Kb3)
Ta.K <- c(Ta.lines$Ka1, Ta.lines$Ka2, Ta.lines$Kb1, Ta.lines$Kb2, Ta.lines$Kb3)
W.K <- c(W.lines$Ka1, W.lines$Ka2, W.lines$Kb1, W.lines$Kb2, W.lines$Kb3)
Re.K <- c(Re.lines$Ka1, Re.lines$Ka2, Re.lines$Kb1, Re.lines$Kb2, Re.lines$Kb3)
Os.K <- c(Os.lines$Ka1, Os.lines$Ka2, Os.lines$Kb1, Os.lines$Kb2, Os.lines$Kb3)
Ir.K <- c(Ir.lines$Ka1, Ir.lines$Ka2, Ir.lines$Kb1, Ir.lines$Kb2, Ir.lines$Kb3)
Pt.K <- c(Pt.lines$Ka1, Pt.lines$Ka2, Pt.lines$Kb1, Pt.lines$Kb2, Pt.lines$Kb3)
Au.K <- c(Au.lines$Ka1, Au.lines$Ka2, Au.lines$Kb1, Au.lines$Kb2, Au.lines$Kb3)
Hg.K <- c(Hg.lines$Ka1, Hg.lines$Ka2, Hg.lines$Kb1, Hg.lines$Kb2, Hg.lines$Kb3)
Tl.K <- c(Tl.lines$Ka1, Tl.lines$Ka2, Tl.lines$Kb1, Tl.lines$Kb2, Tl.lines$Kb3)
Pb.K <- c(Pb.lines$Ka1, Pb.lines$Ka2, Pb.lines$Kb1, Pb.lines$Kb2, Pb.lines$Kb3)
Bi.K <- c(Bi.lines$Ka1, Bi.lines$Ka2, Bi.lines$Kb1, Bi.lines$Kb2, Bi.lines$Kb3)
Po.K <- c(Po.lines$Ka1, Po.lines$Ka2, Po.lines$Kb1, Po.lines$Kb2, Po.lines$Kb3)
At.K <- c(At.lines$Ka1, At.lines$Ka2, At.lines$Kb1, At.lines$Kb2, At.lines$Kb3)
Rn.K <- c(Rn.lines$Ka1, Rn.lines$Ka2, Rn.lines$Kb1, Rn.lines$Kb2, Rn.lines$Kb3)
Fr.K <- c(Fr.lines$Ka1, Fr.lines$Ka2, Fr.lines$Kb1, Fr.lines$Kb2, Fr.lines$Kb3)
Ra.K <- c(Ra.lines$Ka1, Ra.lines$Ka2, Ra.lines$Kb1, Ra.lines$Kb2, Ra.lines$Kb3)
Ac.K <- c(Ac.lines$Ka1, Ac.lines$Ka2, Ac.lines$Kb1, Ac.lines$Kb2, Ac.lines$Kb3)
Th.K <- c(Th.lines$Ka1, Th.lines$Ka2, Th.lines$Kb1, Th.lines$Kb2, Th.lines$Kb3)
Pa.K <- c(Pa.lines$Ka1, Pa.lines$Ka2, Pa.lines$Kb1, Pa.lines$Kb2, Pa.lines$Kb3)
U.K <- c(U.lines$Ka1, U.lines$Ka2, U.lines$Kb1, U.lines$Kb2, U.lines$Kb3)



H.L <- c(H.lines$La1, H.lines$La2, H.lines$Lb1, H.lines$Lb2, H.lines$Lb3,  H.lines$Lb4, H.lines$Lg1, H.lines$Lg2, H.lines$Lg3, H.lines$Ll, H.lines$Lb4, H.lines$Lg1, H.lines$Lg2, H.lines$Lg3, H.lines$Ll)
He.L <- c(He.lines$La1, He.lines$La2, He.lines$Lb1, He.lines$Lb2, He.lines$Lb3,  He.lines$Lb4, He.lines$Lg1, He.lines$Lg2, He.lines$Lg3, He.lines$Ll)
Li.L <- c(Li.lines$La1, Li.lines$La2, Li.lines$Lb1, Li.lines$Lb2, Li.lines$Lb3,  Li.lines$Lb4, Li.lines$Lg1, Li.lines$Lg2, Li.lines$Lg3, Li.lines$Ll)
Be.L <- c(Be.lines$La1, Be.lines$La2, Be.lines$Lb1, Be.lines$Lb2, Be.lines$Lb3,  Be.lines$Lb4, Be.lines$Lg1, Be.lines$Lg2, Be.lines$Lg3, Be.lines$Ll)
B.L <- c(B.lines$La1, B.lines$La2, B.lines$Lb1, B.lines$Lb2, B.lines$Lb3,  B.lines$Lb4, B.lines$Lg1, B.lines$Lg2, B.lines$Lg3, B.lines$Ll)
C.L <- c(C.lines$La1, C.lines$La2, C.lines$Lb1, C.lines$Lb2, C.lines$Lb3,  C.lines$Lb4, C.lines$Lg1, C.lines$Lg2, C.lines$Lg3, C.lines$Ll)
N.L <- c(N.lines$La1, N.lines$La2, N.lines$Lb1, N.lines$Lb2, N.lines$Lb3,  N.lines$Lb4, N.lines$Lg1, N.lines$Lg2, N.lines$Lg3, N.lines$Ll)
O.L <- c(O.lines$La1, O.lines$La2, O.lines$Lb1, O.lines$Lb2, O.lines$Lb3,  O.lines$Lb4, O.lines$Lg1, O.lines$Lg2, O.lines$Lg3, O.lines$Ll)
F.L <- c(F.lines$La1, F.lines$La2, F.lines$Lb1, F.lines$Lb2, F.lines$Lb3,  F.lines$Lb4, F.lines$Lg1, F.lines$Lg2, F.lines$Lg3, F.lines$Ll)
Ne.L <- c(Ne.lines$La1, Ne.lines$La2, Ne.lines$Lb1, Ne.lines$Lb2, Ne.lines$Lb3,  Ne.lines$Lb4, Ne.lines$Lg1, Ne.lines$Lg2, Ne.lines$Lg3, Ne.lines$Ll)
Na.L <- c(Na.lines$La1, Na.lines$La2, Na.lines$Lb1, Na.lines$Lb2, Na.lines$Lb3,  Na.lines$Lb4, Na.lines$Lg1, Na.lines$Lg2, Na.lines$Lg3, Na.lines$Ll)
Mg.L <- c(Mg.lines$La1, Mg.lines$La2, Mg.lines$Lb1, Mg.lines$Lb2, Mg.lines$Lb3,  Mg.lines$Lb4, Mg.lines$Lg1, Mg.lines$Lg2, Mg.lines$Lg3, Mg.lines$Ll)
Al.L <- c(Al.lines$La1, Al.lines$La2, Al.lines$Lb1, Al.lines$Lb2, Al.lines$Lb3,  Al.lines$Lb4, Al.lines$Lg1, Al.lines$Lg2, Al.lines$Lg3, Al.lines$Ll)
Si.L <- c(Si.lines$La1, Si.lines$La2, Si.lines$Lb1, Si.lines$Lb2, Si.lines$Lb3,  Si.lines$Lb4, Si.lines$Lg1, Si.lines$Lg2, Si.lines$Lg3, Si.lines$Ll)
P.L <- c(P.lines$La1, P.lines$La2, P.lines$Lb1, P.lines$Lb2, P.lines$Lb3,  P.lines$Lb4, P.lines$Lg1, P.lines$Lg2, P.lines$Lg3, P.lines$Ll)
S.L <- c(S.lines$La1, S.lines$La2, S.lines$Lb1, S.lines$Lb2, S.lines$Lb3,  S.lines$Lb4, S.lines$Lg1, S.lines$Lg2, S.lines$Lg3, S.lines$Ll)
Cl.L <- c(Cl.lines$La1, Cl.lines$La2, Cl.lines$Lb1, Cl.lines$Lb2, Cl.lines$Lb3,  Cl.lines$Lb4, Cl.lines$Lg1, Cl.lines$Lg2, Cl.lines$Lg3, Cl.lines$Ll)
Ar.L <- c(Ar.lines$La1, Ar.lines$La2, Ar.lines$Lb1, Ar.lines$Lb2, Ar.lines$Lb3,  Ar.lines$Lb4, Ar.lines$Lg1, Ar.lines$Lg2, Ar.lines$Lg3, Ar.lines$Ll)
K.L <- c(K.lines$La1, K.lines$La2, K.lines$Lb1, K.lines$Lb2, K.lines$Lb3,  K.lines$Lb4, K.lines$Lg1, K.lines$Lg2, K.lines$Lg3, K.lines$Ll)
Ca.L <- c(Ca.lines$La1, Ca.lines$La2, Ca.lines$Lb1, Ca.lines$Lb2, Ca.lines$Lb3,  Ca.lines$Lb4, Ca.lines$Lg1, Ca.lines$Lg2, Ca.lines$Lg3, Ca.lines$Ll)
Sc.L <- c(Sc.lines$La1, Sc.lines$La2, Sc.lines$Lb1, Sc.lines$Lb2, Sc.lines$Lb3,  Sc.lines$Lb4, Sc.lines$Lg1, Sc.lines$Lg2, Sc.lines$Lg3, Sc.lines$Ll)
Ti.L <- c(Ti.lines$La1, Ti.lines$La2, Ti.lines$Lb1, Ti.lines$Lb2, Ti.lines$Lb3,  Ti.lines$Lb4, Ti.lines$Lg1, Ti.lines$Lg2, Ti.lines$Lg3, Ti.lines$Ll)
V.L <- c(V.lines$La1, V.lines$La2, V.lines$Lb1, V.lines$Lb2, V.lines$Lb3,  V.lines$Lb4, V.lines$Lg1, V.lines$Lg2, V.lines$Lg3, V.lines$Ll)
Cr.L <- c(Cr.lines$La1, Cr.lines$La2, Cr.lines$Lb1, Cr.lines$Lb2, Cr.lines$Lb3,  Cr.lines$Lb4, Cr.lines$Lg1, Cr.lines$Lg2, Cr.lines$Lg3, Cr.lines$Ll)
Mn.L <- c(Mn.lines$La1, Mn.lines$La2, Mn.lines$Lb1, Mn.lines$Lb2, Mn.lines$Lb3,  Mn.lines$Lb4, Mn.lines$Lg1, Mn.lines$Lg2, Mn.lines$Lg3, Mn.lines$Ll)
Fe.L <- c(Fe.lines$La1, Fe.lines$La2, Fe.lines$Lb1, Fe.lines$Lb2, Fe.lines$Lb3,  Fe.lines$Lb4, Fe.lines$Lg1, Fe.lines$Lg2, Fe.lines$Lg3, Fe.lines$Ll)
Co.L <- c(Co.lines$La1, Co.lines$La2, Co.lines$Lb1, Co.lines$Lb2, Co.lines$Lb3,  Co.lines$Lb4, Co.lines$Lg1, Co.lines$Lg2, Co.lines$Lg3, Co.lines$Ll)
Ni.L <- c(Ni.lines$La1, Ni.lines$La2, Ni.lines$Lb1, Ni.lines$Lb2, Ni.lines$Lb3,  Ni.lines$Lb4, Ni.lines$Lg1, Ni.lines$Lg2, Ni.lines$Lg3, Ni.lines$Ll)
Cu.L <- c(Cu.lines$La1, Cu.lines$La2, Cu.lines$Lb1, Cu.lines$Lb2, Cu.lines$Lb3,  Cu.lines$Lb4, Cu.lines$Lg1, Cu.lines$Lg2, Cu.lines$Lg3, Cu.lines$Ll)
Zn.L <- c(Zn.lines$La1, Zn.lines$La2, Zn.lines$Lb1, Zn.lines$Lb2, Zn.lines$Lb3,  Zn.lines$Lb4, Zn.lines$Lg1, Zn.lines$Lg2, Zn.lines$Lg3, Zn.lines$Ll)
Ga.L <- c(Ga.lines$La1, Ga.lines$La2, Ga.lines$Lb1, Ga.lines$Lb2, Ga.lines$Lb3,  Ga.lines$Lb4, Ga.lines$Lg1, Ga.lines$Lg2, Ga.lines$Lg3, Ga.lines$Ll)
Ge.L <- c(Ge.lines$La1, Ge.lines$La2, Ge.lines$Lb1, Ge.lines$Lb2, Ge.lines$Lb3,  Ge.lines$Lb4, Ge.lines$Lg1, Ge.lines$Lg2, Ge.lines$Lg3, Ge.lines$Ll)
As.L <- c(As.lines$La1, As.lines$La2, As.lines$Lb1, As.lines$Lb2, As.lines$Lb3,  As.lines$Lb4, As.lines$Lg1, As.lines$Lg2, As.lines$Lg3, As.lines$Ll)
Se.L <- c(Se.lines$La1, Se.lines$La2, Se.lines$Lb1, Se.lines$Lb2, Se.lines$Lb3,  Se.lines$Lb4, Se.lines$Lg1, Se.lines$Lg2, Se.lines$Lg3, Se.lines$Ll)
Br.L <- c(Br.lines$La1, Br.lines$La2, Br.lines$Lb1, Br.lines$Lb2, Br.lines$Lb3,  Br.lines$Lb4, Br.lines$Lg1, Br.lines$Lg2, Br.lines$Lg3, Br.lines$Ll)
Kr.L <- c(Kr.lines$La1, Kr.lines$La2, Kr.lines$Lb1, Kr.lines$Lb2, Kr.lines$Lb3,  Kr.lines$Lb4, Kr.lines$Lg1, Kr.lines$Lg2, Kr.lines$Lg3, Kr.lines$Ll)
Rb.L <- c(Rb.lines$La1, Rb.lines$La2, Rb.lines$Lb1, Rb.lines$Lb2, Rb.lines$Lb3,  Rb.lines$Lb4, Rb.lines$Lg1, Rb.lines$Lg2, Rb.lines$Lg3, Rb.lines$Ll)
Sr.L <- c(Sr.lines$La1, Sr.lines$La2, Sr.lines$Lb1, Sr.lines$Lb2, Sr.lines$Lb3,  Sr.lines$Lb4, Sr.lines$Lg1, Sr.lines$Lg2, Sr.lines$Lg3, Sr.lines$Ll)
Y.L <- c(Y.lines$La1, Y.lines$La2, Y.lines$Lb1, Y.lines$Lb2, Y.lines$Lb3,  Y.lines$Lb4, Y.lines$Lg1, Y.lines$Lg2, Y.lines$Lg3, Y.lines$Ll)
Zr.L <- c(Zr.lines$La1, Zr.lines$La2, Zr.lines$Lb1, Zr.lines$Lb2, Zr.lines$Lb3,  Zr.lines$Lb4, Zr.lines$Lg1, Zr.lines$Lg2, Zr.lines$Lg3, Zr.lines$Ll)
Nb.L <- c(Nb.lines$La1, Nb.lines$La2, Nb.lines$Lb1, Nb.lines$Lb2, Nb.lines$Lb3,  Nb.lines$Lb4, Nb.lines$Lg1, Nb.lines$Lg2, Nb.lines$Lg3, Nb.lines$Ll)
Mo.L <- c(Mo.lines$La1, Mo.lines$La2, Mo.lines$Lb1, Mo.lines$Lb2, Mo.lines$Lb3,  Mo.lines$Lb4, Mo.lines$Lg1, Mo.lines$Lg2, Mo.lines$Lg3, Mo.lines$Ll)
Tc.L <- c(Tc.lines$La1, Tc.lines$La2, Tc.lines$Lb1, Tc.lines$Lb2, Tc.lines$Lb3,  Tc.lines$Lb4, Tc.lines$Lg1, Tc.lines$Lg2, Tc.lines$Lg3, Tc.lines$Ll)
Ru.L <- c(Ru.lines$La1, Ru.lines$La2, Ru.lines$Lb1, Ru.lines$Lb2, Ru.lines$Lb3,  Ru.lines$Lb4, Ru.lines$Lg1, Ru.lines$Lg2, Ru.lines$Lg3, Ru.lines$Ll)
Rh.L <- c(Rh.lines$La1, Rh.lines$La2, Rh.lines$Lb1, Rh.lines$Lb2, Rh.lines$Lb3,  Rh.lines$Lb4, Rh.lines$Lg1, Rh.lines$Lg2, Rh.lines$Lg3, Rh.lines$Ll)
Pd.L <- c(Pd.lines$La1, Pd.lines$La2, Pd.lines$Lb1, Pd.lines$Lb2, Pd.lines$Lb3,  Pd.lines$Lb4, Pd.lines$Lg1, Pd.lines$Lg2, Pd.lines$Lg3, Pd.lines$Ll)
Ag.L <- c(Ag.lines$La1, Ag.lines$La2, Ag.lines$Lb1, Ag.lines$Lb2, Ag.lines$Lb3,  Ag.lines$Lb4, Ag.lines$Lg1, Ag.lines$Lg2, Ag.lines$Lg3, Ag.lines$Ll)
Cd.L <- c(Cd.lines$La1, Cd.lines$La2, Cd.lines$Lb1, Cd.lines$Lb2, Cd.lines$Lb3,  Cd.lines$Lb4, Cd.lines$Lg1, Cd.lines$Lg2, Cd.lines$Lg3, Cd.lines$Ll)
In.L <- c(In.lines$La1, In.lines$La2, In.lines$Lb1, In.lines$Lb2, In.lines$Lb3,  In.lines$Lb4, In.lines$Lg1, In.lines$Lg2, In.lines$Lg3, In.lines$Ll)
Sn.L <- c(Sn.lines$La1, Sn.lines$La2, Sn.lines$Lb1, Sn.lines$Lb2, Sn.lines$Lb3,  Sn.lines$Lb4, Sn.lines$Lg1, Sn.lines$Lg2, Sn.lines$Lg3, Sn.lines$Ll)
Sb.L <- c(Sb.lines$La1, Sb.lines$La2, Sb.lines$Lb1, Sb.lines$Lb2, Sb.lines$Lb3,  Sb.lines$Lb4, Sb.lines$Lg1, Sb.lines$Lg2, Sb.lines$Lg3, Sb.lines$Ll)
Te.L <- c(Te.lines$La1, Te.lines$La2, Te.lines$Lb1, Te.lines$Lb2, Te.lines$Lb3,  Te.lines$Lb4, Te.lines$Lg1, Te.lines$Lg2, Te.lines$Lg3, Te.lines$Ll)
I.L <- c(I.lines$La1, I.lines$La2, I.lines$Lb1, I.lines$Lb2, I.lines$Lb3,  I.lines$Lb4, I.lines$Lg1, I.lines$Lg2, I.lines$Lg3, I.lines$Ll)
Xe.L <- c(Xe.lines$La1, Xe.lines$La2, Xe.lines$Lb1, Xe.lines$Lb2, Xe.lines$Lb3,  Xe.lines$Lb4, Xe.lines$Lg1, Xe.lines$Lg2, Xe.lines$Lg3, Xe.lines$Ll)
Cs.L <- c(Cs.lines$La1, Cs.lines$La2, Cs.lines$Lb1, Cs.lines$Lb2, Cs.lines$Lb3,  Cs.lines$Lb4, Cs.lines$Lg1, Cs.lines$Lg2, Cs.lines$Lg3, Cs.lines$Ll)
Ba.L <- c(Ba.lines$La1, Ba.lines$La2, Ba.lines$Lb1, Ba.lines$Lb2, Ba.lines$Lb3,  Ba.lines$Lb4, Ba.lines$Lg1, Ba.lines$Lg2, Ba.lines$Lg3, Ba.lines$Ll)
La.L <- c(La.lines$La1, La.lines$La2, La.lines$Lb1, La.lines$Lb2, La.lines$Lb3,  La.lines$Lb4, La.lines$Lg1, La.lines$Lg2, La.lines$Lg3, La.lines$Ll)
Ce.L <- c(Ce.lines$La1, Ce.lines$La2, Ce.lines$Lb1, Ce.lines$Lb2, Ce.lines$Lb3,  Ce.lines$Lb4, Ce.lines$Lg1, Ce.lines$Lg2, Ce.lines$Lg3, Ce.lines$Ll)
Pr.L <- c(Pr.lines$La1, Pr.lines$La2, Pr.lines$Lb1, Pr.lines$Lb2, Pr.lines$Lb3,  Pr.lines$Lb4, Pr.lines$Lg1, Pr.lines$Lg2, Pr.lines$Lg3, Pr.lines$Ll)
Nd.L <- c(Nd.lines$La1, Nd.lines$La2, Nd.lines$Lb1, Nd.lines$Lb2, Nd.lines$Lb3,  Nd.lines$Lb4, Nd.lines$Lg1, Nd.lines$Lg2, Nd.lines$Lg3, Nd.lines$Ll)
Pm.L <- c(Pm.lines$La1, Pm.lines$La2, Pm.lines$Lb1, Pm.lines$Lb2, Pm.lines$Lb3,  Pm.lines$Lb4, Pm.lines$Lg1, Pm.lines$Lg2, Pm.lines$Lg3, Pm.lines$Ll)
Sm.L <- c(Sm.lines$La1, Sm.lines$La2, Sm.lines$Lb1, Sm.lines$Lb2, Sm.lines$Lb3,  Sm.lines$Lb4, Sm.lines$Lg1, Sm.lines$Lg2, Sm.lines$Lg3, Sm.lines$Ll)
Eu.L <- c(Eu.lines$La1, Eu.lines$La2, Eu.lines$Lb1, Eu.lines$Lb2, Eu.lines$Lb3,  Eu.lines$Lb4, Eu.lines$Lg1, Eu.lines$Lg2, Eu.lines$Lg3, Eu.lines$Ll)
Gd.L <- c(Gd.lines$La1, Gd.lines$La2, Gd.lines$Lb1, Gd.lines$Lb2, Gd.lines$Lb3,  Gd.lines$Lb4, Gd.lines$Lg1, Gd.lines$Lg2, Gd.lines$Lg3, Gd.lines$Ll)
Tb.L <- c(Tb.lines$La1, Tb.lines$La2, Tb.lines$Lb1, Tb.lines$Lb2, Tb.lines$Lb3,  Tb.lines$Lb4, Tb.lines$Lg1, Tb.lines$Lg2, Tb.lines$Lg3, Tb.lines$Ll)
Dy.L <- c(Dy.lines$La1, Dy.lines$La2, Dy.lines$Lb1, Dy.lines$Lb2, Dy.lines$Lb3,  Dy.lines$Lb4, Dy.lines$Lg1, Dy.lines$Lg2, Dy.lines$Lg3, Dy.lines$Ll)
Ho.L <- c(Ho.lines$La1, Ho.lines$La2, Ho.lines$Lb1, Ho.lines$Lb2, Ho.lines$Lb3,  Ho.lines$Lb4, Ho.lines$Lg1, Ho.lines$Lg2, Ho.lines$Lg3, Ho.lines$Ll)
Er.L <- c(Er.lines$La1, Er.lines$La2, Er.lines$Lb1, Er.lines$Lb2, Er.lines$Lb3,  Er.lines$Lb4, Er.lines$Lg1, Er.lines$Lg2, Er.lines$Lg3, Er.lines$Ll)
Tm.L <- c(Tm.lines$La1, Tm.lines$La2, Tm.lines$Lb1, Tm.lines$Lb2, Tm.lines$Lb3,  Tm.lines$Lb4, Tm.lines$Lg1, Tm.lines$Lg2, Tm.lines$Lg3, Tm.lines$Ll)
Yb.L <- c(Yb.lines$La1, Yb.lines$La2, Yb.lines$Lb1, Yb.lines$Lb2, Yb.lines$Lb3,  Yb.lines$Lb4, Yb.lines$Lg1, Yb.lines$Lg2, Yb.lines$Lg3, Yb.lines$Ll)
Lu.L <- c(Lu.lines$La1, Lu.lines$La2, Lu.lines$Lb1, Lu.lines$Lb2, Lu.lines$Lb3,  Lu.lines$Lb4, Lu.lines$Lg1, Lu.lines$Lg2, Lu.lines$Lg3, Lu.lines$Ll)
Hf.L <- c(Hf.lines$La1, Hf.lines$La2, Hf.lines$Lb1, Hf.lines$Lb2, Hf.lines$Lb3,  Hf.lines$Lb4, Hf.lines$Lg1, Hf.lines$Lg2, Hf.lines$Lg3, Hf.lines$Ll)
Ta.L <- c(Ta.lines$La1, Ta.lines$La2, Ta.lines$Lb1, Ta.lines$Lb2, Ta.lines$Lb3,  Ta.lines$Lb4, Ta.lines$Lg1, Ta.lines$Lg2, Ta.lines$Lg3, Ta.lines$Ll)
W.L <- c(W.lines$La1, W.lines$La2, W.lines$Lb1, W.lines$Lb2, W.lines$Lb3,  W.lines$Lb4, W.lines$Lg1, W.lines$Lg2, W.lines$Lg3, W.lines$Ll)
Re.L <- c(Re.lines$La1, Re.lines$La2, Re.lines$Lb1, Re.lines$Lb2, Re.lines$Lb3,  Re.lines$Lb4, Re.lines$Lg1, Re.lines$Lg2, Re.lines$Lg3, Re.lines$Ll)
Os.L <- c(Os.lines$La1, Os.lines$La2, Os.lines$Lb1, Os.lines$Lb2, Os.lines$Lb3,  Os.lines$Lb4, Os.lines$Lg1, Os.lines$Lg2, Os.lines$Lg3, Os.lines$Ll)
Ir.L <- c(Ir.lines$La1, Ir.lines$La2, Ir.lines$Lb1, Ir.lines$Lb2, Ir.lines$Lb3,  Ir.lines$Lb4, Ir.lines$Lg1, Ir.lines$Lg2, Ir.lines$Lg3, Ir.lines$Ll)
Pt.L <- c(Pt.lines$La1, Pt.lines$La2, Pt.lines$Lb1, Pt.lines$Lb2, Pt.lines$Lb3,  Pt.lines$Lb4, Pt.lines$Lg1, Pt.lines$Lg2, Pt.lines$Lg3, Pt.lines$Ll)
Au.L <- c(Au.lines$La1, Au.lines$La2, Au.lines$Lb1, Au.lines$Lb2, Au.lines$Lb3,  Au.lines$Lb4, Au.lines$Lg1, Au.lines$Lg2, Au.lines$Lg3, Au.lines$Ll)
Hg.L <- c(Hg.lines$La1, Hg.lines$La2, Hg.lines$Lb1, Hg.lines$Lb2, Hg.lines$Lb3,  Hg.lines$Lb4, Hg.lines$Lg1, Hg.lines$Lg2, Hg.lines$Lg3, Hg.lines$Ll)
Tl.L <- c(Tl.lines$La1, Tl.lines$La2, Tl.lines$Lb1, Tl.lines$Lb2, Tl.lines$Lb3,  Tl.lines$Lb4, Tl.lines$Lg1, Tl.lines$Lg2, Tl.lines$Lg3, Tl.lines$Ll)
Pb.L <- c(Pb.lines$La1, Pb.lines$La2, Pb.lines$Lb1, Pb.lines$Lb2, Pb.lines$Lb3,  Pb.lines$Lb4, Pb.lines$Lg1, Pb.lines$Lg2, Pb.lines$Lg3, Pb.lines$Ll)
Bi.L <- c(Bi.lines$La1, Bi.lines$La2, Bi.lines$Lb1, Bi.lines$Lb2, Bi.lines$Lb3,  Bi.lines$Lb4, Bi.lines$Lg1, Bi.lines$Lg2, Bi.lines$Lg3, Bi.lines$Ll)
Po.L <- c(Po.lines$La1, Po.lines$La2, Po.lines$Lb1, Po.lines$Lb2, Po.lines$Lb3,  Po.lines$Lb4, Po.lines$Lg1,  Po.lines$Lg2, Po.lines$Lg3, Po.lines$Ll)
At.L <- c(At.lines$La1, At.lines$La2, At.lines$Lb1, At.lines$Lb2, At.lines$Lb3,  At.lines$Lb4, At.lines$Lg1, At.lines$Lg2, At.lines$Lg3, At.lines$Ll)
Rn.L <- c(Rn.lines$La1, Rn.lines$La2, Rn.lines$Lb1, Rn.lines$Lb2, Rn.lines$Lb3,  Rn.lines$Lb4, Rn.lines$Lg1, Rn.lines$Lg2, Rn.lines$Lg3, Rn.lines$Ll)
Fr.L <- c(Fr.lines$La1, Fr.lines$La2, Fr.lines$Lb1, Fr.lines$Lb2, Fr.lines$Lb3,  Fr.lines$Lb4, Fr.lines$Lg1, Fr.lines$Lg2, Fr.lines$Lg3, Fr.lines$Ll)
Ra.L <- c(Ra.lines$La1, Ra.lines$La2, Ra.lines$Lb1, Ra.lines$Lb2, Ra.lines$Lb3,  Ra.lines$Lb4, Ra.lines$Lg1, Ra.lines$Lg2, Ra.lines$Lg3, Ra.lines$Ll)
Ac.L <- c(Ac.lines$La1, Ac.lines$La2, Ac.lines$Lb1, Ac.lines$Lb2, Ac.lines$Lb3,  Ac.lines$Lb4, Ac.lines$Lg1, Ac.lines$Lg2, Ac.lines$Lg3, Ac.lines$Ll)
Th.L <- c(Th.lines$La1, Th.lines$La2, Th.lines$Lb1, Th.lines$Lb2, Th.lines$Lb3,  Th.lines$Lb4, Th.lines$Lg1, Th.lines$Lg2, Th.lines$Lg3, Th.lines$Ll)
Pa.L <- c(Pa.lines$La1, Pa.lines$La2, Pa.lines$Lb1, Pa.lines$Lb2, Pa.lines$Lb3,  Pa.lines$Lb4, Pa.lines$Lg1, Pa.lines$Lg2, Pa.lines$Lg3, Pa.lines$Ll)
U.L <- c(U.lines$La1, U.lines$La2, U.lines$Lb1, U.lines$Lb2, U.lines$Lb3,  U.lines$Lb4, U.lines$Lg1, U.lines$Lg2, U.lines$Lg3, U.lines$Ll)
Pu.L <- c(Pu.lines$La1, Pu.lines$La2, Pu.lines$Lb1, Pu.lines$Lb2, Pu.lines$Lb3,  Pu.lines$Lb4, Pu.lines$Lg1, Pu.lines$Lg2, Pu.lines$Lg3, Pu.lines$Ll)


K.intensity <- c(56, 29, 9, 2, 5)
L.intensity <- c(68, 8, 78, 17, 40, 34, 17, 11, 15, 4)
Intensity <- c(K.intensity, L.intensity)

H.table <- data.frame(as.vector(t(H.lines)), Intensity, stringsAsFactors=FALSE)
colnames(H.table) <- c("Line", "Intensity")

He.table <- data.frame(as.vector(t(He.lines)), Intensity, stringsAsFactors=FALSE)
colnames(He.table) <- c("Line", "Intensity")

Li.table <- data.frame(as.vector(t(Li.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Li.table) <- c("Line", "Intensity")

Be.table <- data.frame(as.vector(t(Be.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Be.table) <- c("Line", "Intensity")

B.table <- data.frame(as.vector(t(B.lines)), Intensity, stringsAsFactors=FALSE)
colnames(B.table) <- c("Line", "Intensity")

C.table <- data.frame(as.vector(t(C.lines)), Intensity, stringsAsFactors=FALSE)
colnames(C.table) <- c("Line", "Intensity")

N.table <- data.frame(as.vector(t(N.lines)), Intensity, stringsAsFactors=FALSE)
colnames(N.table) <- c("Line", "Intensity")

O.table <- data.frame(as.vector(t(O.lines)), Intensity, stringsAsFactors=FALSE)
colnames(O.table) <- c("Line", "Intensity")

F.table <- data.frame(as.vector(t(F.lines)), Intensity, stringsAsFactors=FALSE)
colnames(F.table) <- c("Line", "Intensity")

Ne.table <- data.frame(as.vector(t(Ne.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Ne.table) <- c("Line", "Intensity")

Na.table <- data.frame(as.vector(t(Na.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Na.table) <- c("Line", "Intensity")

Na.table <- data.frame(as.vector(t(Na.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Na.table) <- c("Line", "Intensity")

Mg.table <- data.frame(as.vector(t(Mg.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Mg.table) <- c("Line", "Intensity")

Al.table <- data.frame(as.vector(t(Al.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Al.table) <- c("Line", "Intensity")

Si.table <- data.frame(as.vector(t(Si.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Si.table) <- c("Line", "Intensity")

P.table <- data.frame(as.vector(t(P.lines)), Intensity, stringsAsFactors=FALSE)
colnames(P.table) <- c("Line", "Intensity")

S.table <- data.frame(as.vector(t(S.lines)), Intensity, stringsAsFactors=FALSE)
colnames(S.table) <- c("Line", "Intensity")

Cl.table <- data.frame(as.vector(t(Cl.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Cl.table) <- c("Line", "Intensity")

Ar.table <- data.frame(as.vector(t(Ar.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Ar.table) <- c("Line", "Intensity")

K.table <- data.frame(as.vector(t(K.lines)), Intensity, stringsAsFactors=FALSE)
colnames(K.table) <- c("Line", "Intensity")

Ca.table <- data.frame(as.vector(t(Ca.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Ca.table) <- c("Line", "Intensity")

Sc.table <- data.frame(as.vector(t(Sc.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Sc.table) <- c("Line", "Intensity")

Ti.table <- data.frame(as.vector(t(Ti.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Ti.table) <- c("Line", "Intensity")

V.table <- data.frame(as.vector(t(V.lines)), Intensity, stringsAsFactors=FALSE)
colnames(V.table) <- c("Line", "Intensity")

Cr.table <- data.frame(as.vector(t(Cr.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Cr.table) <- c("Line", "Intensity")

Mn.table <- data.frame(as.vector(t(Mn.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Mn.table) <- c("Line", "Intensity")

Fe.table <- data.frame(as.vector(t(Fe.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Fe.table) <- c("Line", "Intensity")

Co.table <- data.frame(as.vector(t(Co.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Co.table) <- c("Line", "Intensity")

Ni.table <- data.frame(as.vector(t(Ni.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Ni.table) <- c("Line", "Intensity")

Cu.table <- data.frame(as.vector(t(Cu.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Cu.table) <- c("Line", "Intensity")

Zn.table <- data.frame(as.vector(t(Zn.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Zn.table) <- c("Line", "Intensity")

Ga.table <- data.frame(as.vector(t(Ga.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Ga.table) <- c("Line", "Intensity")

Ge.table <- data.frame(as.vector(t(Ge.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Ge.table) <- c("Line", "Intensity")

As.table <- data.frame(as.vector(t(As.lines)), Intensity, stringsAsFactors=FALSE)
colnames(As.table) <- c("Line", "Intensity")

Se.table <- data.frame(as.vector(t(Se.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Se.table) <- c("Line", "Intensity")

Br.table <- data.frame(as.vector(t(Br.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Br.table) <- c("Line", "Intensity")

Kr.table <- data.frame(as.vector(t(Kr.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Kr.table) <- c("Line", "Intensity")

Rb.table <- data.frame(as.vector(t(Rb.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Rb.table) <- c("Line", "Intensity")

Sr.table <- data.frame(as.vector(t(Sr.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Sr.table) <- c("Line", "Intensity")

Y.table <- data.frame(as.vector(t(Y.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Y.table) <- c("Line", "Intensity")

Zr.table <- data.frame(as.vector(t(Zr.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Zr.table) <- c("Line", "Intensity")

Nb.table <- data.frame(as.vector(t(Nb.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Nb.table) <- c("Line", "Intensity")

Mo.table <- data.frame(as.vector(t(Mo.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Mo.table) <- c("Line", "Intensity")

Tc.table <- data.frame(as.vector(t(Tc.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Tc.table) <- c("Line", "Intensity")

Ru.table <- data.frame(as.vector(t(Ru.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Ru.table) <- c("Line", "Intensity")

Rh.table <- data.frame(as.vector(t(Rh.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Rh.table) <- c("Line", "Intensity")

Pd.table <- data.frame(as.vector(t(Pd.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Pd.table) <- c("Line", "Intensity")

Ag.table <- data.frame(as.vector(t(Ag.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Ag.table) <- c("Line", "Intensity")

Cd.table <- data.frame(as.vector(t(Cd.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Cd.table) <- c("Line", "Intensity")

In.table <- data.frame(as.vector(t(In.lines)), Intensity, stringsAsFactors=FALSE)
colnames(In.table) <- c("Line", "Intensity")

Sn.table <- data.frame(as.vector(t(Sn.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Sn.table) <- c("Line", "Intensity")

Sb.table <- data.frame(as.vector(t(Sb.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Sb.table) <- c("Line", "Intensity")

Te.table <- data.frame(as.vector(t(Te.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Te.table) <- c("Line", "Intensity")

I.table <- data.frame(as.vector(t(I.lines)), Intensity, stringsAsFactors=FALSE)
colnames(I.table) <- c("Line", "Intensity")

Xe.table <- data.frame(as.vector(t(Xe.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Xe.table) <- c("Line", "Intensity")

Cs.table <- data.frame(as.vector(t(Cs.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Cs.table) <- c("Line", "Intensity")

Ba.table <- data.frame(as.vector(t(Ba.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Ba.table) <- c("Line", "Intensity")

La.table <- data.frame(as.vector(t(La.lines)), Intensity, stringsAsFactors=FALSE)
colnames(La.table) <- c("Line", "Intensity")

Ce.table <- data.frame(as.vector(t(Ce.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Ce.table) <- c("Line", "Intensity")

Pr.table <- data.frame(as.vector(t(Pr.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Pr.table) <- c("Line", "Intensity")

Nd.table <- data.frame(as.vector(t(Nd.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Nd.table) <- c("Line", "Intensity")

Pm.table <- data.frame(as.vector(t(Pm.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Pm.table) <- c("Line", "Intensity")

Sm.table <- data.frame(as.vector(t(Sm.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Sm.table) <- c("Line", "Intensity")

Eu.table <- data.frame(as.vector(t(Eu.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Eu.table) <- c("Line", "Intensity")

Gd.table <- data.frame(as.vector(t(Gd.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Gd.table) <- c("Line", "Intensity")

Tb.table <- data.frame(as.vector(t(Tb.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Tb.table) <- c("Line", "Intensity")

Dy.table <- data.frame(as.vector(t(Dy.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Dy.table) <- c("Line", "Intensity")

Ho.table <- data.frame(as.vector(t(Ho.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Ho.table) <- c("Line", "Intensity")

Er.table <- data.frame(as.vector(t(Er.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Er.table) <- c("Line", "Intensity")

Tm.table <- data.frame(as.vector(t(Tm.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Tm.table) <- c("Line", "Intensity")

Yb.table <- data.frame(as.vector(t(Yb.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Yb.table) <- c("Line", "Intensity")

Lu.table <- data.frame(as.vector(t(Lu.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Lu.table) <- c("Line", "Intensity")

Hf.table <- data.frame(as.vector(t(Hf.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Hf.table) <- c("Line", "Intensity")

Ta.table <- data.frame(as.vector(t(Ta.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Ta.table) <- c("Line", "Intensity")

W.table <- data.frame(as.vector(t(W.lines)), Intensity, stringsAsFactors=FALSE)
colnames(W.table) <- c("Line", "Intensity")

Re.table <- data.frame(as.vector(t(Re.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Re.table) <- c("Line", "Intensity")

Os.table <- data.frame(as.vector(t(Os.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Os.table) <- c("Line", "Intensity")

Ir.table <- data.frame(as.vector(t(Ir.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Ir.table) <- c("Line", "Intensity")

Pt.table <- data.frame(as.vector(t(Pt.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Pt.table) <- c("Line", "Intensity")

Au.table <- data.frame(as.vector(t(Au.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Au.table) <- c("Line", "Intensity")

Hg.table <- data.frame(as.vector(t(Hg.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Hg.table) <- c("Line", "Intensity")

Tl.table <- data.frame(as.vector(t(Tl.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Tl.table) <- c("Line", "Intensity")

Pb.table <- data.frame(as.vector(t(Pb.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Pb.table) <- c("Line", "Intensity")

Bi.table <- data.frame(as.vector(t(Bi.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Bi.table) <- c("Line", "Intensity")

Po.table <- data.frame(as.vector(t(Po.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Pb.table) <- c("Line", "Intensity")

At.table <- data.frame(as.vector(t(At.lines)), Intensity, stringsAsFactors=FALSE)
colnames(At.table) <- c("Line", "Intensity")

Rn.table <- data.frame(as.vector(t(Rn.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Rn.table) <- c("Line", "Intensity")

Fr.table <- data.frame(as.vector(t(Fr.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Fr.table) <- c("Line", "Intensity")

Ra.table <- data.frame(as.vector(t(Ra.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Ra.table) <- c("Line", "Intensity")

Ac.table <- data.frame(as.vector(t(Ac.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Ac.table) <- c("Line", "Intensity")

Th.table <- data.frame(as.vector(t(Th.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Th.table) <- c("Line", "Intensity")

Pa.table <- data.frame(as.vector(t(Pa.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Pa.table) <- c("Line", "Intensity")

U.table <- data.frame(as.vector(t(U.lines)), Intensity, stringsAsFactors=FALSE)
colnames(U.table) <- c("Line", "Intensity")

Pu.table <- data.frame(as.vector(t(Pu.lines)), Intensity, stringsAsFactors=FALSE)
colnames(Pu.table) <- c("Line", "Intensity")



spectralLines <- c("Ne.K.alpha", "Ne.K.beta", "Na.K.alpha", "Na.K.beta", "Mg.K.alpha", "Mg.K.beta", "Al.K.alpha", "Al.K.beta", "Si.K.alpha", "Si.K.beta", "P.K.alpha", "P.K.beta", "S.K.alpha", "S.K.beta", "Cl.K.alpha", "Cl.K.beta", "Ar.K.alpha", "Ar.K.beta", "K.K.alpha", "K.K.beta", "Ca.K.alpha", "Ca.K.beta", "Sc.K.alpha", "Sc.K.beta", "Ti.K.alpha", "Ti.K.beta", "V.K.alpha", "V.K.beta", "Cr.K.alpha", "Cr.K.beta", "Mn.K.alpha", "Mn.K.beta", "Fe.K.alpha", "Fe.K.beta", "Co.K.alpha", "Co.K.beta", "Ni.K.alpha", "Ni.K.beta", "Cu.K.alpha", "Cu.K.beta", "Zn.K.alpha", "Zn.K.beta", "Ga.K.alpha", "Ga.K.beta", "Ge.K.alpha", "Ge.K.beta", "As.K.alpha", "As.K.beta", "Se.K.alpha", "Se.K.beta", "Br.K.alpha", "Br.K.beta", "Kr.K.alpha", "Kr.K.beta", "Rb.K.alpha", "Rb.K.beta", "Sr.K.alpha", "Sr.K.beta", "Y.K.alpha", "Y.K.beta", "Zr.K.alpha", "Zr.K.beta", "Nb.K.alpha", "Nb.K.beta", "Mo.K.alpha", "Mo.K.beta", "Mo.L.alpha", "Mo.L.beta", "Ru.K.alpha", "Ru.K.beta", "Ru.L.alpha", "Ru.L.beta", "Rh.K.alpha", "Rh.K.beta", "Rh.L.alpha", "Rh.L.beta", "Pd.K.alpha", "Pd.K.beta", "Pd.L.alpha", "Pd.L.beta", "Ag.K.alpha", "Ag.K.beta", "Ag.L.alpha", "Ag.L.beta", "Cd.K.alpha", "Cd.K.beta", "Cd.L.alpha", "Cd.L.beta", "In.K.alpha", "In.K.beta", "In.L.alpha", "Sn.K.alpha", "Sn.K.beta", "Sn.L.alpha", "Sn.L.beta", "Sb.K.alpha", "Sb.K.beta", "Sb.L.alpha", "Sb.L.beta", "Te.K.alpha", "Te.K.beta", "Te.L.alpha", "Te.L.beta", "I.K.alpha", "I.K.beta", "I.L.alpha", "I.L.beta", "Xe.K.alpha", "Xe.K.beta", "Xe.L.alpha", "Xe.L.beta", "Cs.K.alpha", "Cs.K.beta", "Cs.L.alpha", "Cs.L.beta", "Ba.K.alpha", "Ba.K.beta", "Ba.L.alpha", "Ba.L.beta", "La.K.alpha", "La.K.beta", "La.L.alpha", "La.L.beta", "Ce.K.alpha", "Ce.K.beta", "Ce.L.alpha", "Ce.L.beta", "Pr.K.alpha", "Pr.K.beta", "Pr.L.alpha", "Pr.L.beta", "Nd.K.alpha", "Nd.K.beta", "Nd.L.alpha", "Nd.L.beta", "Pm.L.alpha", "Pm.L.beta", "Sm.L.alpha", "Sm.L.beta", "Eu.L.alpha", "Eu.L.beta", "Gd.L.alpha", "Gd.L.beta", "Tb.L.alpha", "Tb.L.beta", "Dy.L.alpha", "Dy.L.beta", "Ho.L.alpha", "Ho.L.beta", "Er.L.alpha", "Er.L.beta", "Tm.L.alpha", "Tm.L.beta", "Yb.L.alpha", "Yb.L.beta", "Lu.L.alpha", "Lu.L.beta", "Hf.L.alpha", "Hf.L.beta", "Ta.L.alpha", "Ta.L.beta", "W.L.alpha", "W.L.beta", "Re.L.alpha", "Re.L.beta", "Os.L.alpha", "Os.L.beta", "Ir.L.alpha", "Ir.L.beta", "Pt.L.alpha", "Pt.L.beta", "Au.L.alpha", "Au.L.beta", "Hg.L.alpha", "Hg.L.beta", "Tl.L.alpha", "Tl.L.beta", "Pb.L.alpha", "Pb.L.beta", "Bi.L.alpha", "Bi.L.beta", "Po.L.alpha", "Po.L.beta", "At.L.alpha", "At.L.beta", "Rn.L.alpha", "Rn.L.beta", "Fr.L.alpha", "Fr.L.beta", "Ra.L.alpha", "Ra.L.beta", "Ac.L.alpha", "Ac.L.beta", "Th.L.alpha", "Th.L.beta", "Pa.L.alpha", "Pa.L.beta", "U.L.alpha", "U.L.beta", "Pu.L.alpha", "Pu.L.beta", "Au.M.line", "Hg.M.line", "Pb.M.line", "U.M.line")

standard <- c("Spectrum", "Ca.K.alpha", "Ti.K.alpha", "Fe.K.alpha")

kalphaLines <- c("Na"="Na.K.alpha",  "Mg"="Mg.K.alpha", "Al"="Al.K.alpha", "Si"="Si.K.alpha", "P"="P.K.alpha", "S"="S.K.alpha", "Cl"="Cl.K.alpha", "Ar"="Ar.K.alpha", "K"="K.K.alpha", "Ca"="Ca.K.alpha", "Sc"="Sc.K.alpha", "Ti"="Ti.K.alpha", "V"="V.K.alpha", "Cr"="Cr.K.alpha", "Mn"="Mn.K.alpha", "Fe"="Fe.K.alpha", "Co"="Co.K.alpha", "Ni"="Ni.K.alpha", "Cu"="Cu.K.alpha", "Zn"="Zn.K.alpha", "Ga"="Ga.K.alpha", "Ge"="Ge.K.alpha", "As"="As.K.alpha", "Se"="Se.K.alpha", "Br"="Br.K.alpha", "Kr"="Kr.K.alpha", "Rb"="Rb.K.alpha", "Sr"="Sr.K.alpha", "Y"="Y.K.alpha", "Zr"="Zr.K.alpha", "Nb"="Nb.K.alpha", "Mo"="Mo.K.alpha", "Ru"="Ru.K.alpha", "Rh"="Rh.K.alpha", "Pd"="Pd.K.alpha", "Ag"="Ag.K.alpha", "Cd"="Cd.K.alpha", "In"="In.K.alpha", "Sn"="Sn.K.alpha", "Sb"="Sb.K.alpha", "Te"="Te.K.alpha", "I"="I.K.alpha", "Xe"="Xe.K.alpha", "Cs"="Cs.K.alpha", "Ba"="Ba.K.alpha", "La"="La.K.alpha", "Ce"="Ce.K.alpha", "Pr"="Pr.K.alpha", "Nd"="Nd.K.alpha")

kbetaLines <- c("Na"="Na.K.beta",  "Mg"="Mg.K.beta", "Al"="Al.K.beta", "Si"="Si.K.beta", "P"="P.K.beta", "S"="S.K.beta", "Cl"="Cl.K.beta", "Ar"="Ar.K.beta", "K"="K.K.beta", "Ca"="Ca.K.beta", "Sc"="Sc.K.beta", "Ti"="Ti.K.beta", "V"="V.K.beta", "Cr"="Cr.K.beta", "Mn"="Mn.K.beta", "Fe"="Fe.K.beta", "Co"="Co.K.beta", "Ni"="Ni.K.beta", "Cu"="Cu.K.beta", "Zn"="Zn.K.beta", "Ga"="Ga.K.beta", "Ge"="Ge.K.beta", "As"="As.K.beta", "Se"="Se.K.beta", "Br"="Br.K.beta", "Kr"="Kr.K.beta", "Rb"="Rb.K.beta", "Sr"="Sr.K.beta", "Y"="Y.K.beta", "Zr"="Zr.K.beta", "Nb"="Nb.K.beta", "Mo"="Mo.K.beta", "Ru"="Ru.K.beta", "Rh"="Rh.K.beta", "Pd"="Pd.K.beta", "Ag"="Ag.K.beta", "Cd"="Cd.K.beta", "In"="In.K.beta", "Sn"="Sn.K.beta", "Sb"="Sb.K.beta", "Te"="Te.K.beta", "I"="I.K.beta", "Xe"="Xe.K.beta", "Cs"="Cs.K.beta", "Ba"="Ba.K.beta", "La"="La.K.beta", "Ce"="Ce.K.beta", "Pr"="Pr.K.beta", "Nd"="Nd.K.beta")

lalphaLines <- c("Mo"="Mo.L.alpha", "Ru"="Ru.L.alpha", "Rh"="Rh.L.alpha", "Pd"="Pd.L.alpha", "Ag"="Ag.L.alpha", "Cd"="Cd.L.alpha", "In"="In.L.alpha", "Sn"="Sn.L.alpha", "Sb"="Sb.L.alpha", "Te"="Te.L.alpha", "I"="I.L.alpha", "Xe"="Xe.L.alpha", "Cs"="Cs.L.alpha", "Ba"="Ba.L.alpha", "La"="La.L.alpha", "Ce"="Ce.L.alpha", "Pr"="Pr.L.alpha", "Nd"="Nd.L.alpha", "Pm"="Pm.L.alpha", "Sm"="Sm.L.alpha", "Eu"="Eu.L.alpha", "Gd"="Gd.L.alpha", "Tb"="Tb.L.alpha", "Dy"="Dy.L.alpha", "Ho"="Ho.L.alpha", "Er"="Er.L.alpha", "Tm"="Tm.L.alpha", "Yb"="Yb.L.alpha", "Lu"="Lu.L.alpha", "Hf"="Hf.L.alpha", "Ta"="Ta.L.alpha", "W"="W.L.alpha", "Re"="Re.L.alpha", "Os"="Os.L.alpha", "Ir"="Ir.L.alpha", "Pt"="Pt.L.alpha", "Au"="Au.L.alpha", "Hg"="Hg.L.alpha", "Tl"="Tl.L.alpha", "Pb"="Pb.L.alpha", "Bi"="Bi.L.alpha", "Po"="Po.L.alpha", "At"="At.L.alpha", "Rn"="Rn.L.alpha", "Fr"="Fr.L.alpha", "Ra"="Ra.L.alpha", "Ac"="Ac.L.alpha", "Th"="Th.L.alpha", "Pa"="Pa.L.alpha", "U"="U.L.alpha")

lbetaLines <- c("Mo"="Mo.L.beta", "Ru"="Ru.L.beta", "Rh"="Rh.L.beta", "Pd"="Pd.L.beta", "Ag"="Ag.L.beta", "Cd"="Cd.L.beta", "In"="In.L.beta", "Sn"="Sn.L.beta", "Sb"="Sb.L.beta", "Te"="Te.L.beta", "I"="I.L.beta", "Xe"="Xe.L.beta", "Cs"="Cs.L.beta", "Ba"="Ba.L.beta", "La"="La.L.beta", "Ce"="Ce.L.beta", "Pr"="Pr.L.beta", "Nd"="Nd.L.beta", "Pm"="Pm.L.beta", "Sm"="Sm.L.beta", "Eu"="Eu.L.beta", "Gd"="Gd.L.beta", "Tb"="Tb.L.beta", "Dy"="Dy.L.beta", "Ho"="Ho.L.beta", "Er"="Er.L.beta", "Tm"="Tm.L.beta", "Yb"="Yb.L.beta", "Lu"="Lu.L.beta", "Hf"="Hf.L.beta", "Ta"="Ta.L.beta", "W"="W.L.beta", "Re"="Re.L.beta", "Os"="Os.L.beta", "Ir"="Ir.L.beta", "Pt"="Pt.L.beta", "Au"="Au.L.beta", "Hg"="Hg.L.beta", "Tl"="Tl.L.beta", "Pb"="Pb.L.beta", "Bi"="Bi.L.beta", "Po"="Po.L.beta", "At"="At.L.beta", "Rn"="Rn.L.beta", "Fr"="Fr.L.beta", "Ra"="Ra.L.beta", "Ac"="Ac.L.beta", "Th"="Th.L.beta", "Pa"="Pa.L.beta", "U"="U.L.beta")

mLines <- c("Au"="Au.M.line","Hg"="Hg.M.line", "Pb"="Pb.M.line", "U"="U.M.line")


elementGrabKalpha <- function(element, data) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    
    hold.cps <- subset(data$CPS, !(data$Energy < elementLine[6][1,]-0.02 | data$Energy > elementLine[5][1,]+0.02))
    hold.file <- subset(data$Spectrum, !(data$Energy < elementLine[6][1,]-0.02 | data$Energy > elementLine[5][1,]+0.02))
    hold.frame <- data.frame(is.0(hold.cps, hold.file), stringsAsFactors=FALSE)
    colnames(hold.frame) <- c("Counts", "Spectrum")
    hold.ag <- aggregate(list(hold.frame$Counts), by=list(hold.frame$Spectrum), FUN="sum")
    colnames(hold.ag) <- c("Spectrum", paste(element, "K-alpha", sep=" "))
    
    hold.ag
    
}
elementGrabKalpha <- cmpfun(elementGrabKalpha)


elementGrabKbeta <- function(element, data) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.cps <- if(elementLine[8][1,]!=0){
        subset(data$CPS, !(data$Energy < elementLine[7][1,]-0.02 | data$Energy > elementLine[8][1,]+0.02))
    } else if(elementLine[8][1,]==0){
        subset(data$CPS, !(data$Energy < elementLine[7][1,]-0.02 | data$Energy > elementLine[7][1,]+0.02))
    }
    
    
    hold.file <- if(elementLine[8][1,]!=0){
        subset(data$Spectrum, !(data$Energy < elementLine[7][1,]-0.02 | data$Energy > elementLine[8][1,]+0.02))
    } else if(elementLine[8][1,]==0){
            subset(data$Spectrum, !(data$Energy < elementLine[7][1,]-0.02 | data$Energy > elementLine[7][1,]+0.02))
    }
    hold.frame <- data.frame(is.0(hold.cps, hold.file), stringsAsFactors=FALSE)
    colnames(hold.frame) <- c("Counts", "Spectrum")
    hold.ag <- aggregate(list(hold.frame$Counts), by=list(hold.frame$Spectrum), FUN="sum")
    colnames(hold.ag) <- c("Spectrum", paste(element, "K-beta", sep=" "))
    
    hold.ag
    
}
elementGrabKbeta <- cmpfun(elementGrabKbeta)


elementGrabLalpha <- function(element, data) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    
    hold.cps <- subset(data$CPS, !(data$Energy < elementLine[11][1,]-0.02 | data$Energy > elementLine[10][1,]+0.02))
    hold.file <- subset(data$Spectrum, !(data$Energy < elementLine[11][1,]-0.02 | data$Energy > elementLine[10][,1]+0.02))
    hold.frame <- data.frame(is.0(hold.cps, hold.file), stringsAsFactors=FALSE)
    colnames(hold.frame) <- c("Counts", "Spectrum")
    hold.ag <- aggregate(list(hold.frame$Counts), by=list(hold.frame$Spectrum), FUN="sum")
    colnames(hold.ag) <- c("Spectrum", paste(element, "L-alpha", sep=" "))
    
    hold.ag
    
}
elementGrabLalpha <- cmpfun(elementGrabLalpha)


elementGrabLbeta <- function(element, data) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    
    hold.cps <- subset(data$CPS, !(data$Energy < elementLine[12][1,]-0.02 | data$Energy > elementLine[14][1,]+0.02))
    hold.file <- subset(data$Spectrum, !(data$Energy < elementLine[12][1,]-0.02 | data$Energy > elementLine[14][1,]+0.02))

    hold.frame <- data.frame(is.0(hold.cps, hold.file), stringsAsFactors=FALSE)
    colnames(hold.frame) <- c("Counts", "Spectrum")
    hold.ag <- aggregate(list(hold.frame$Counts), by=list(hold.frame$Spectrum), FUN="sum")
    colnames(hold.ag) <- c("Spectrum", paste(element, "L-beta", sep=" "))
    
    hold.ag
    
}
elementGrabLbeta <- cmpfun(elementGrabLbeta)

elementGrabMalpha <- function(element, data) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    
    hold.cps <- subset(data$CPS, !(data$Energy < elementLine[20][1,]-0.02 | data$Energy > elementLine[22][1,]+0.02))
    hold.file <- subset(data$Spectrum, !(data$Energy < elementLine[20][1,]-0.02 | data$Energy > elementLine[22][,1]+0.02))
    hold.frame <- data.frame(is.0(hold.cps, hold.file), stringsAsFactors=FALSE)
    colnames(hold.frame) <- c("Counts", "Spectrum")
    hold.ag <- aggregate(list(hold.frame$Counts), by=list(hold.frame$Spectrum), FUN="sum")
    colnames(hold.ag) <- c("Spectrum", paste(element, "M-line", sep=" "))
    
    hold.ag
    
}
elementGrabMalpha <- cmpfun(elementGrabMalpha)


elementGrabpre <- function(element.line, data) {
    
    element <- strsplit(x=element.line, split="\\.")[[1]][1]
    destination <- strsplit(x=element.line, split="\\.")[[1]][2]
    distance <- strsplit(x=element.line, split="\\.")[[1]][3]
    
    if(destination=="K" && distance=="alpha"){
        elementGrabKalpha(element, data)
    } else if(destination=="K" && distance=="beta"){
        elementGrabKbeta(element, data)
    } else if(destination=="L" && distance=="alpha"){
        elementGrabLalpha(element, data)
    } else if (destination=="L" && distance=="beta"){
        elementGrabLbeta(element, data)
    } else if (destination=="M" && distance=="line"){
        elementGrabMalpha(element, data)
    }
        
}
elementGrabpre <- cmpfun(elementGrabpre)




range_subset_xrf <- function(range.frame, data){
    
    new.data <- subset(data, Energy >= range.frame$EnergyMin & Energy <= range.frame$EnergyMax, drop=TRUE)
    newer.data <- aggregate(new.data, by=list(new.data$Spectrum), FUN=mean, na.rm=TRUE)[,c("Group.1", "CPS")]
    colnames(newer.data) <- c("Spectrum", as.character(range.frame$Name))
    newer.data
}
range_subset_xrf <- cmpfun(range_subset_xrf)


xrf_parse <- function(range.table, data){
    
    choice.lines <- range.table[complete.cases(range.table),]
    
    choice.list <- split(choice.lines, f=choice.lines$Name)
    names(choice.list) <- choice.lines[,"Name"]
    
    index <- choice.lines[,"Name"]
    
    selected.list <- lapply(index, function(x) range_subset_xrf(range.frame=choice.list[[x]], data=data))
    
    Reduce(function(...) merge(..., all=T), selected.list)
}
xrf_parse <- cmpfun(xrf_parse)



elementGrab <- function(element.line, data, range.table){
    
    is.element <- element.line %in% spectralLines
    
    if(is.element==TRUE){
        elementGrabpre(element.line, data)
    } else if(is.element==FALSE){
        xrf_parse(range.table, data)
    }

    
}
elementGrab <- cmpfun(elementGrab)


elementFrame <- function(data, elements){
    
    spectra.line.list <- lapply(elements, function(x) elementGrab(element.line=x, data=data))
    element.count.list <- lapply(spectra.line.list, '[', 2)
    
    spectra.line.vector <- as.numeric(unlist(element.count.list))
    
    dim(spectra.line.vector) <- c(length(spectra.line.list[[1]]$Spectrum), length(elements))
    
    spectra.line.frame <- data.frame(spectra.line.list[[1]]$Spectrum, spectra.line.vector, stringsAsFactors=FALSE)
    
    colnames(spectra.line.frame) <- c("Spectrum", elements)
    
    spectra.line.frame <- as.data.frame(spectra.line.frame, stringsAsFactors=FALSE)
    
    spectra.line.frame <- spectra.line.frame[order(as.character(spectra.line.frame$Spectrum)),]
    
    spectra.line.frame$Spectrum <- gsub(".pdz", "", spectra.line.frame$Spectrum)
    spectra.line.frame$Spectrum <- gsub(".csv", "", spectra.line.frame$Spectrum)
    spectra.line.frame$Spectrum <- gsub(".CSV", "", spectra.line.frame$Spectrum)
    spectra.line.frame$Spectrum <- gsub(".spt", "", spectra.line.frame$Spectrum)
    spectra.line.frame$Spectrum <- gsub(".mca", "", spectra.line.frame$Spectrum)
    spectra.line.frame$Spectrum <- gsub(".spx", "", spectra.line.frame$Spectrum)
    
    
    spectra.line.frame
    
}
elementFrame <- cmpfun(elementFrame)





####Normalize

element_norm <- function(data, element, min, max) {
    
    compton.norm <- subset(data$CPS, !(data$Energy < input$min | data$max > input$comptonmax))
    compton.file <- subset(data$Spectrum, !(data$Energy < input$min | data$Energy > input$max))
    compton.frame <- data.frame(is.0(compton.norm, compton.file))
    colnames(compton.frame) <- c("Compton", "Spectrum")
    compton.frame.ag <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")
    colnames(compton.frame.ag) <- c("Spectrum", "Compton")
    
    
}
element_norm <- cmpfun(element_norm)


####Cal Models

linear_simp_xrf <- function(concentration.table, spectra.line.table, element.line) {
    
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    
    predict.frame <- data.frame(concentration, intensit, stringsAsFactors=FALSE)
    colnames(predict.frame) <- c("Concentration", "Intensity")
    
    
    
    predict.intensity <- data.frame(predict.frame$Intensity, stringsAsFactors=FALSE)
    colnames(predict.intensity) <- c("Intensity")
    
    cal.lm <- lm(predict.frame$Concentration~predict.frame$Intensity)
    
    cal.lm
    
}
linear_simp_xrf <- cmpfun(linear_simp_xrf)


poly_simp_xrf <- function(concentration.table, spectra.line.table, element.line) {
    
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    
    predict.frame <- data.frame(concentration, intensity, stringsAsFactors=FALSE)
    colnames(predict.frame) <- c("Concentration", "Intensity")
    
    
    
    predict.intensity <- data.frame(predict.frame$Intensity, stringsAsFactors=FALSE)
    colnames(predict.intensity) <- c("Intensity")
    
    cal.lm.poly <- lm(predict.frame$Concentration~poly(predict.frame$Intensity, 2))
    
    cal.lm.poly
    
}
poly_simp_xrf <- cmpfun(poly_simp_xrf)


lucas_simp_xrf <- function(concentration.table, spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    lucas.intercept.table <- data.frame(rowSums(lucas.intercept.table.x[intercept.element.lines]), stringsAsFactors=FALSE)
    colnames(lucas.intercept.table) <- c("first")
    
    
    
    lucas.intercept <- lucas.intercept.table$first
    lucas.slope <- data.frame(lucas.slope.table[slope.element.lines], stringsAsFactors=FALSE)
    
    
    
    predict.frame.luk <- data.frame(concentration, ((1+intensity/(intensity+lucas.intercept))-lucas.intercept/(intensity+lucas.intercept)),lucas.slope, stringsAsFactors=FALSE)
    colnames(predict.frame.luk) <- c("Concentration", "Intensity", names(lucas.slope))
    
    
    
    predict.intensity.luk <- data.frame(predict.frame.luk$Intensity, lucas.slope, stringsAsFactors=FALSE)
    colnames(predict.intensity.luk) <- c("Intensity", names(lucas.slope))
    
    lucas.lm <- lm(Concentration~., data=predict.frame.luk)
    
    lucas.lm
    
    
}
lucas_simp_xrf <- cmpfun(lucas_simp_xrf)


linear_tc_xrf <- function(concentration.table, spectra.line.table, element.line) {
    
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    
    total.counts <- aggregate(CPS~Spectrum, data=data, sum)
    colnames(total.counts) <- c("Spectrum", "CPS")
    
    
    
    predict.frame.tc <- data.frame(concentration, intensity/total.counts$CPS, stringsAsFactors=FALSE)
    colnames(predict.frame.tc) <- c("Concentration", "Intensity")
    
    
    
    predict.intensity.tc <- data.frame(predict.frame.tc$Intensity, stringsAsFactors=FALSE)
    colnames(predict.intensity.tc) <- c("Intensity")
    
    cal.lm.tc <- lm(predict.frame.tc$Concentration~predict.frame.tc$Intensity)
    
    cal.lm.tc
    
}
linear_tc_xrf <- cmpfun(linear_tc_xrf)


poly_tc_xrf <- function(concentration.table, spectra.line.table, element.line) {
    
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    
    
    
    total.counts <- aggregate(CPS~Spectrum, data=data, sum)
    colnames(total.counts) <- c("Spectrum", "CPS")
    
    
    
    predict.frame.tc <- data.frame(concentration, intensity/total.counts$CPS, stringsAsFactors=FALSE)
    colnames(predict.frame.tc) <- c("Concentration", "Intensity")
    
    
    
    predict.intensity.tc <- data.frame(predict.frame.tc$Intensity, stringsAsFactors=FALSE)
    colnames(predict.intensity.tc) <- c("Intensity")
    
    cal.lm.poly.tc <- lm(predict.frame.tc$Concentration~poly(predict.frame.tc$Intensity, 2))
    
    cal.lm.poly.tc
    
    
    
}
poly_tc_xrf <- cmpfun(poly_tc_xrf)




lucas_tc_xrf <- function(concentration.table, spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    lucas.intercept.table.tc <- data.frame(rowSums(lucas.intercept.table.x[intercept.element.lines]), stringsAsFactors=FALSE)/total.counts$CPS
    colnames(lucas.intercept.table.tc) <- c("first")
    
    
    
    lucas.intercept.tc <- lucas.intercept.table.tc$first
    lucas.slope.tc <- data.frame(lucas.slope.table[slope.element.lines], stringsAsFactors=FALSE)/total.counts$CPS
    
    
    
    predict.frame.luc.tc <- data.frame(concentration, ((intensity/total.counts$CPS-lucas.intercept.tc)/(intensity/total.counts$CPS+lucas.intercept.tc)),lucas.slope.tc, stringsAsFactors=FALSE)
    colnames(predict.frame.luc.tc) <- c("Concentration", "Intensity", names(lucas.slope.tc))
    
    
    
    predict.intensity.luc.tc <- data.frame(predict.frame.luc.tc$Intensity, lucas.slope.tc, stringsAsFactors=FALSE)
    colnames(predict.intensity.luc.tc) <- c("Intensity", names(lucas.slope.tc))
    
    lucas.lm.tc <- lm(Concentration~., data=predict.frame.luc.tc)
    
    lucas.lm.tc
    
    
}
lucas_tc_xrf <- cmpfun(lucas_tc_xrf)

linear_comp_xrf <- function(data, concentration.table, spectra.line.table, element.line) {
    
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    
    compton.norm <- subset(data$CPS, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))
    compton.file <- subset(data$Spectrum, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))
    compton.frame <- data.frame(is.0(compton.norm, compton.file))
    colnames(compton.frame) <- c("Compton", "Spectrum")
    compton.frame.ag <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")
    colnames(compton.frame.ag) <- c("Spectrum", "Compton")
    
    predict.frame.comp <- data.frame(concentration, intensity/compton.frame.ag$Compton, stringsAsFactors=FALSE)
    colnames(predict.frame.comp) <- c("Concentration", "Intensity")
    
    
    
    predict.intensity.comp <- data.frame(predict.frame.comp$Intensity, stringsAsFactors=FALSE)
    colnames(predict.intensity.comp) <- c("Intensity")
    
    cal.lm.comp <- lm(predict.frame.comp$Concentration~predict.frame.comp$Intensity)
    
    cal.lm.comp
    
}
linear_comp_xrf <- cmpfun(linear_comp_xrf)


poly_comp_xrf <- function(data, concentration.table, spectra.line.table, element.line) {
    
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    
    compton.norm <- subset(data$CPS, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))
    compton.file <- subset(data$Spectrum, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))
    compton.frame <- data.frame(is.0(compton.norm, compton.file))
    colnames(compton.frame) <- c("Compton", "Spectrum")
    compton.frame.ag <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")
    colnames(compton.frame.ag) <- c("Spectrum", "Compton")
    
    predict.frame.comp <- data.frame(concentration, intensity/compton.frame.ag$Compton, stringsAsFactors=FALSE)
    colnames(predict.frame.comp) <- c("Concentration", "Intensity")
    
    
    
    predict.intensity.comp <- data.frame(predict.frame.comp$Intensity, stringsAsFactors=FALSE)
    colnames(predict.intensity.comp) <- c("Intensity")
    
    cal.lm.poly.comp <- lm(predict.frame.comp$Concentration~poly(predict.frame.comp$Intensity, 2))
    
    cal.lm.poly.comp
    
}
poly_comp_xrf <- cmpfun(poly_comp_xrf)


lucas_comp_xrf <- function(data, concentration.table, spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    
    compton.norm <- subset(data$CPS, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))
    compton.file <- subset(data$Spectrum, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))
    compton.frame <- data.frame(is.0(compton.norm, compton.file))
    colnames(compton.frame) <- c("Compton", "Spectrum")
    compton.frame.ag <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")
    colnames(compton.frame.ag) <- c("Spectrum", "Compton")
    
    
    
    lucas.intercept.table.comp <- data.frame(rowSums(lucas.intercept.table.x[intercept.element.lines]), stringsAsFactors=FALSE)/compton.frame.ag$Compton
    colnames(lucas.intercept.table.comp) <- c("first")
    
    
    
    lucas.intercept.comp <- lucas.intercept.table.comp$first
    lucas.slope.comp <- data.frame(lucas.slope.table[slope.element.lines], stringsAsFactors=FALSE)/compton.frame.ag$Compton
    
    
    
    
    predict.frame.luc.comp <- data.frame(concentration, ((1+intensity/compton.frame.ag$Compton)/(intensity/compton.frame.ag$Compton+lucas.intercept.comp)-lucas.intercept.comp/(intensity/compton.frame.ag$Compton+lucas.intercept.comp)),lucas.slope.comp, stringsAsFactors=FALSE)
    colnames(predict.frame.luc.comp) <- c("Concentration", "Intensity", names(lucas.slope.comp))
    
    
    
    predict.intensity.luc.comp <- data.frame(predict.frame.luc.comp$Intensity, lucas.slope.comp, stringsAsFactors=FALSE)
    colnames(predict.intensity.luc.comp) <- c("Intensity", names(lucas.slope.comp))
    
    lucas.lm.comp <- lm(Concentration~., data=predict.frame.luc.comp)
    
    lucas.lm.comp
    
}
lucas_comp_xrf <- cmpfun(lucas_comp_xrf)



###Spectra Manipulaton

james <- function(x) (abs(x)+x)/2
james.cp <- compiler::cmpfun(james)

spectra_summary_general <- function(spectra.frame, norm.type, norm.min, norm.max, compress){
    
    if(norm.type==1){
        spectra_simp_trans_xrf(spectra=spectra.frame, compress=compress)
    } else if(norm.type==2){
        spectra_tc_trans_xrf(spectra=spectra.frame, compress=compress)
    } else if(norm.type==3){
        spectra_comp_trans_xrf(spectra=spectra.frame, norm.min=norm.min, norm.max=norm.max, compress=compress)
    }
    
}
spectra_summary_general <- cmpfun(spectra_summary_general)



spectra_stats <- function(spectra.frame, norm.type, norm.min, norm.max, compress){
    
    
    data.processed <- spectra_summary_general(spectra.frame=spectra.frame, norm.type=norm.type, norm.min=norm.min, norm.max=norm.max, compress=compress)
    
    
    
    data.sum <- data.frame(
    Energy = data.processed$Energy,
    Min = apply(data.processed[,-1], 1, min),
    Max = apply(data.processed[,-1], 1, max),
    Mean = apply(data.processed[,-1], 1, mean),
    Median = apply(data.processed[,-1], 1, median),
    SD = apply(data.processed[,-1], 1, sd), stringsAsFactors=FALSE)
    
    data.sum$SDMin <- data.sum$Mean - data.sum$SD
    data.sum$SDMax <- data.sum$Mean + data.sum$SD
    data.sum$SD2Min <- data.sum$Mean - data.sum$SD*2
    data.sum$SD2Max <- data.sum$Mean + data.sum$SD*2
    
    
    
    data.sum <- as.data.frame(apply(data.sum, 2, james.cp), stringsAsFactors=FALSE)
    
    data.sum
    
}
spectra_stats <- cmpfun(spectra_stats)


###############
###Prep Data###
###############


###############
###Full Spectra##
###############


spectra_frame_xrf <- function(spectra){
    
    data <- reshape2::dcast(spectra, Spectrum~Energy, value.var="CPS")
    
    #test <- apply(test, 2, as.numeric)
    colnames(data) <- make.names(colnames(data))
    data[complete.cases(data),]
}
spectra_frame_xrf <- cmpfun(spectra_frame_xrf)



spectra_table_xrf <- function(spectra, concentration){
    
    data <- reshape2::dcast(spectra, Spectrum~Energy, value.var="CPS")
    data$Concentration <- concentration
    
    #test <- apply(test, 2, as.numeric)
    colnames(data) <- make.names(colnames(data))
    data[complete.cases(data),]
}
spectra_table_xrf <- cmpfun(spectra_table_xrf)


spectra_simp_prep_xrf <- function(spectra, energy.min=0.7, energy.max=37, compress=TRUE){
    
    if(is.null(energy.min)){energy.min <- 0.7}
    if(is.null(energy.max)){energy.max <- 37}
    if(is.null(compress)){compress <- TRUE}


    if(compress==TRUE){spectra$Energy <- round(spectra$Energy, 1)}
    if(compress==TRUE){spectra <- subset(spectra, !(spectra$Energy < energy.min | spectra$Energy > energy.max))}
    
    if(compress==FALSE){spectra$Energy <- round(spectra$Energy, 2)}
    if(compress==FALSE){spectra <- subset(spectra, !(spectra$Energy < energy.min | spectra$Energy > energy.max))}

    
    spectra <- data.table(spectra)
    spectra.aggregate <- spectra[, list(CPS=mean(CPS, na.rm = TRUE)), by = list(Spectrum,Energy)]
    
    data <- as.data.frame(dcast.data.table(spectra.aggregate, Spectrum~Energy, value.var="CPS"), stringsAsFactors=FALSE)
    
    #test <- apply(test, 2, as.numeric)
    colnames(data) <- make.names(colnames(data))
    do.call(data.frame,lapply(data, function(x) replace(x, is.infinite(x),0)))
    
}
spectra_simp_prep_xrf <- cmpfun(spectra_simp_prep_xrf)


spectra_tc_prep_xrf <- function(spectra, energy.min=0.7, energy.max=37, compress=TRUE){
    
    if(is.null(energy.min)){energy.min <- 0.7}
    if(is.null(energy.max)){energy.max <- 37}
    if(is.null(compress)){compress <- TRUE}
    
    if(compress==TRUE){spectra$Energy <- round(spectra$Energy, 1)}
    if(compress==TRUE){spectra <- subset(spectra, !(spectra$Energy < energy.min | spectra$Energy > energy.max))}
    
    if(compress==FALSE){spectra$Energy <- round(spectra$Energy, 2)}
    if(compress==FALSE){spectra <- subset(spectra, !(spectra$Energy < energy.min | spectra$Energy > energy.max))}
    
    spectra <- data.table(spectra)
    spectra.aggregate <- spectra[, list(CPS=mean(CPS, na.rm = TRUE)), by = list(Spectrum,Energy)]
    
    data <- as.data.frame(dcast.data.table(spectra.aggregate, Spectrum~Energy, value.var="CPS"), stringsAsFactors=FALSE)
    
    #test <- apply(test, 2, as.numeric)
    colnames(data) <- make.names(colnames(data))
    data <- data[complete.cases(data),]
    
    total.counts <- rowSums(data[,-1], na.rm=TRUE)
    
    data <- data.frame(Spectrum=data$Spectrum, data[,-1]/total.counts, stringsAsFactors=FALSE)
    do.call(data.frame,lapply(data, function(x) replace(x, is.infinite(x),0)))
    
}
spectra_tc_prep_xrf <- cmpfun(spectra_tc_prep_xrf)


spectra_comp_prep_xrf <- function(spectra, energy.min=0.7, energy.max=37, norm.min, norm.max, compress=TRUE){
    
    if(is.null(energy.min)){energy.min <- 0.7}
    if(is.null(energy.max)){energy.max <- 37}
    if(is.null(compress)){compress <- TRUE}
    
    compton.norm <- subset(spectra$CPS, !(spectra$Energy < norm.min | spectra$Energy > norm.max))
    compton.file <- subset(spectra$Spectrum, !(spectra$Energy < norm.min | spectra$Energy > norm.max))
    compton.frame <- data.frame(is.0(compton.norm, compton.file))
    colnames(compton.frame) <- c("Compton", "Spectrum")
    compton.frame.ag <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")
    colnames(compton.frame.ag) <- c("Spectrum", "Compton")
    
    
    if(compress==TRUE){spectra$Energy <- round(spectra$Energy, 1)}
    if(compress==TRUE){spectra <- subset(spectra, !(spectra$Energy < energy.min | spectra$Energy > energy.max))}
    
    if(compress==FALSE){spectra$Energy <- round(spectra$Energy, 2)}
    if(compress==FALSE){spectra <- subset(spectra, !(spectra$Energy < energy.min | spectra$Energy > energy.max))}
    
    spectra <- data.table(spectra)
    spectra.aggregate <- spectra[, list(CPS=mean(CPS, na.rm = TRUE)), by = list(Spectrum,Energy)]
    
    data <- as.data.frame(dcast.data.table(spectra.aggregate, Spectrum~Energy, value.var="CPS"), stringsAsFactors=FALSE)
    #test <- apply(test, 2, as.numeric)
    colnames(data) <- make.names(colnames(data))
    
    data <- data.frame(Spectrum=data$Spectrum, data[,-1]/compton.frame.ag$Compton, stringsAsFactors=FALSE)
    do.call(data.frame,lapply(data, function(x) replace(x, is.infinite(x),0)))
    
}
spectra_comp_prep_xrf <- cmpfun(spectra_comp_prep_xrf)



spectra_simp_trans_xrf <- function(spectra, energy.min=0.2, energy.max=40, compress=TRUE){
    
    if(is.null(energy.min)){energy.min <- 0.2}
    if(is.null(energy.max)){energy.max <- 40}
    if(is.null(compress)){compress <- TRUE}
    
    
    if(compress==TRUE){spectra$Energy <- round(spectra$Energy, 1)}
    if(compress==TRUE){spectra <- subset(spectra, !(spectra$Energy < energy.min | spectra$Energy > energy.max))}
    
    
    spectra <- data.table(spectra)
    spectra.aggregate <- spectra[, list(CPS=mean(CPS, na.rm = TRUE)), by = list(Spectrum,Energy)]
    
    data <- as.data.frame(dcast.data.table(spectra.aggregate, Spectrum~Energy, value.var="CPS"), stringsAsFactors=FALSE)
    
    #test <- apply(test, 2, as.numeric)
    colnames(data) <- make.names(colnames(data))
    first.pass <- do.call(data.frame,lapply(data, function(x) replace(x, is.infinite(x),0)))
    first.pass <- data.table(first.pass)
    
    
    
    first.pass.t <- as.data.frame(data.table::transpose(first.pass), stringsAsFactors=FALSE)
    names <- as.vector(unlist(first.pass.t[1,]))
    first.pass.t.frame <- first.pass.t[-1,]
    colnames(first.pass.t.frame) <- names
    first.pass.t.frame <- apply(first.pass.t.frame, 2, as.numeric)
    
    
    data.frame(Energy=as.numeric(gsub("X", "", colnames(data)))[-1], first.pass.t.frame, stringsAsFactors=FALSE)
    
}
spectra_simp_trans_xrf <- cmpfun(spectra_simp_trans_xrf)


spectra_tc_trans_xrf <- function(spectra, energy.min=0.7, energy.max=37, compress=TRUE){
    
    if(is.null(energy.min)){energy.min <- 0.7}
    if(is.null(energy.max)){energy.max <- 37}
    if(is.null(compress)){compress <- TRUE}
    
    if(compress==TRUE){spectra$Energy <- round(spectra$Energy, 1)}
    if(compress==TRUE){spectra <- subset(spectra, !(spectra$Energy < energy.min | spectra$Energy > energy.max))}
    
    spectra <- data.table(spectra)
    spectra.aggregate <- spectra[, list(CPS=mean(CPS, na.rm = TRUE)), by = list(Spectrum,Energy)]
    
    data <- as.data.frame(dcast.data.table(spectra.aggregate, Spectrum~Energy, value.var="CPS"), stringsAsFactors=FALSE)
    
    #test <- apply(test, 2, as.numeric)
    colnames(data) <- make.names(colnames(data))
    data <- data[complete.cases(data),]
    
    total.counts <- rowSums(data[,-1], na.rm=TRUE)
    
    data <- data.frame(Spectrum=data$Spectrum, data[,-1]/total.counts, stringsAsFactors=FALSE)
    first.pass <- do.call(data.frame,lapply(data, function(x) replace(x, is.infinite(x),0)))
    first.pass <- data.table(first.pass)
    
    
    
    first.pass.t <- as.data.frame(data.table::transpose(first.pass), stringsAsFactors=FALSE)
    names <- as.vector(unlist(first.pass.t[1,]))
    first.pass.t.frame <- first.pass.t[-1,]
    colnames(first.pass.t.frame) <- names
    first.pass.t.frame <- apply(first.pass.t.frame, 2, as.numeric)
    
    
    data.frame(Energy=as.numeric(gsub("X", "", colnames(data)))[-1], first.pass.t.frame, stringsAsFactors=FALSE)
}
spectra_tc_trans_xrf <- cmpfun(spectra_tc_trans_xrf)


spectra_comp_trans_xrf <- function(spectra, energy.min=0.7, energy.max=37, norm.min, norm.max, compress=TRUE){
    
    if(is.null(energy.min)){energy.min <- 0.7}
    if(is.null(energy.max)){energy.max <- 37}
    if(is.null(compress)){compress <- TRUE}
    
    compton.norm <- subset(spectra$CPS, !(spectra$Energy < norm.min | spectra$Energy > norm.max))
    compton.file <- subset(spectra$Spectrum, !(spectra$Energy < norm.min | spectra$Energy > norm.max))
    compton.frame <- data.frame(is.0(compton.norm, compton.file))
    colnames(compton.frame) <- c("Compton", "Spectrum")
    compton.frame.ag <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")
    colnames(compton.frame.ag) <- c("Spectrum", "Compton")
    
    
    if(compress==TRUE){spectra$Energy <- round(spectra$Energy, 1)}
    if(compress==TRUE){spectra <- subset(spectra, !(spectra$Energy < energy.min | spectra$Energy > energy.max))}
    
    spectra <- data.table(spectra)
    spectra.aggregate <- spectra[, list(CPS=mean(CPS, na.rm = TRUE)), by = list(Spectrum,Energy)]
    
    data <- as.data.frame(dcast.data.table(spectra.aggregate, Spectrum~Energy, value.var="CPS"), stringsAsFactors=FALSE)
    #test <- apply(test, 2, as.numeric)
    colnames(data) <- make.names(colnames(data))
    
    data <- data.frame(Spectrum=data$Spectrum, data[,-1]/compton.frame.ag$Compton, stringsAsFactors=FALSE)
    first.pass <- do.call(data.frame,lapply(data, function(x) replace(x, is.infinite(x),0)))
    first.pass <- data.table(first.pass)
    
    
    
    first.pass.t <- as.data.frame(data.table::transpose(first.pass), stringsAsFactors=FALSE)
    names <- as.vector(unlist(first.pass.t[1,]))
    first.pass.t.frame <- first.pass.t[-1,]
    colnames(first.pass.t.frame) <- names
    first.pass.t.frame <- apply(first.pass.t.frame, 2, as.numeric)
    
    
    data.frame(Energy=as.numeric(gsub("X", "", colnames(data)))[-1], first.pass.t.frame, stringsAsFactors=FALSE)
}
spectra_comp_trans_xrf <- cmpfun(spectra_comp_trans_xrf)


###############
###Prep Data###
###############


###############
###Raw Spectra##
###############


general_prep_xrf <- function(spectra.line.table, element.line) {
    
    intensity <- spectra.line.table[,element.line]
    
    
    data.frame(Intensity=spectra.line.table[,element.line], stringsAsFactors=FALSE)

}
general_prep_xrf <- cmpfun(general_prep_xrf)


simple_tc_prep_xrf <- function(data,spectra.line.table, element.line) {
    
    intensity <- spectra.line.table[,element.line]
    
    
    total.counts <- aggregate(CPS~Spectrum, data=data, sum)
    colnames(total.counts) <- c("Spectrum", "CPS")
    
    
    
    predict.frame.tc <- data.frame(Intensity=intensity/total.counts$CPS, stringsAsFactors=FALSE)
    
    predict.frame.tc
}
simple_tc_prep_xrf <- cmpfun(simple_tc_prep_xrf)


simple_comp_prep_xrf <- function(data, spectra.line.table, element.line, norm.min, norm.max) {
    
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    compton.norm <- subset(data$CPS, !(data$Energy < norm.min | data$Energy > norm.max))
    compton.file <- subset(data$Spectrum, !(data$Energy < norm.min | data$Energy > norm.max))
    compton.frame <- data.frame(is.0(compton.norm, compton.file))
    colnames(compton.frame) <- c("Compton", "Spectrum")
    compton.frame.ag <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")
    colnames(compton.frame.ag) <- c("Spectrum", "Compton")
    
    compton.frame.ag[compton.frame.ag ==0 ] <- 1

    
    predict.frame.comp <- data.frame(Intensity=intensity/compton.frame.ag$Compton, stringsAsFactors=FALSE)
    
    predict.frame.comp
    
}
simple_comp_prep_xrf <- cmpfun(simple_comp_prep_xrf)

spectra_summary_apply <- function(spectra.frame, normalization, min, max){
    
    new.spectrum <- if(normalization==1){
        spectra_simp_prep_xrf(spectra=spectra.frame, compress=FALSE)
    } else if(normalization==2){
        spectra_tc_prep_xrf(spectra=spectra.frame, compress=FALSE)
    } else if(normalization==3){
        spectra_comp_prep_xrf(spectra=spectra.frame, norm.min=min, norm.max=max, compress=FALSE)
    }
    
    newer.spectrum <- melt(new.spectrum, id.var="Spectrum")
    colnames(newer.spectrum) <- c("Spectrum", "Energy", "CPS")
    newer.spectrum$Energy <- as.numeric(gsub("X", "", newer.spectrum$Energy))
    newer.spectrum
}
spectra_summary_apply <- cmpfun(spectra_summary_apply)



###Prep Data



lucas_simp_prep_xrf <- function(spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lucas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none, stringsAsFactors=FALSE)
    colnames(lucas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lucas.slope.table <- data.frame(spectra.line.table, slope.none, stringsAsFactors=FALSE)
    colnames(lucas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    lucas.intercept.table <- data.frame(first=rowSums(lucas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")]), stringsAsFactors=FALSE)
    
    
    
    lucas.intercept <- lucas.intercept.table$first
    lucas.slope <- data.frame(lucas.slope.table[,slope.element.lines], stringsAsFactors=FALSE)
    colnames(lucas.slope) <- slope.element.lines
    
    
    
    predict.frame.luk <- data.frame(Intensity=((1+intensity/(intensity+lucas.intercept))-lucas.intercept/(intensity+lucas.intercept)),lucas.slope, stringsAsFactors=FALSE)
    
    predict.frame.luk
    
    
}
lucas_simp_prep_xrf <- cmpfun(lucas_simp_prep_xrf)


lucas_tc_prep_xrf <- function(data, spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    total.counts <- aggregate(CPS~Spectrum, data=data, sum)
    colnames(total.counts) <- c("Spectrum", "CPS")
    
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lucas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none, stringsAsFactors=FALSE)
    colnames(lucas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lucas.slope.table <- data.frame(spectra.line.table, slope.none, stringsAsFactors=FALSE)
    colnames(lucas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    lucas.intercept.table.tc <- data.frame(rowSums(lucas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")]), stringsAsFactors=FALSE)/total.counts$CPS
    colnames(lucas.intercept.table.tc) <- c("first")
    
    
    
    lucas.intercept.tc <- lucas.intercept.table.tc$first
    lucas.slope.tc <- data.frame(lucas.slope.table[,slope.element.lines], stringsAsFactors=FALSE)/total.counts$CPS
    colnames(lucas.slope.tc) <- slope.element.lines
    
    
    
    predict.intensity.luc.tc <- data.frame(Intensity=((1+intensity/(intensity+lucas.intercept.tc)-lucas.intercept.tc/(intensity+lucas.intercept.tc))),lucas.slope.tc, stringsAsFactors=FALSE)
    
    predict.intensity.luc.tc
}
lucas_tc_prep_xrf <- cmpfun(lucas_tc_prep_xrf)





lucas_comp_prep_xrf <- function(data, spectra.line.table, element.line, slope.element.lines, intercept.element.lines, norm.min, norm.max) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    
    compton.norm <- subset(data$CPS, !(data$Energy < norm.min | data$Energy > norm.max))
    compton.file <- subset(data$Spectrum, !(data$Energy < norm.min | data$Energy > norm.max))
    compton.frame <- data.frame(is.0(compton.norm, compton.file))
    colnames(compton.frame) <- c("Compton", "Spectrum")
    compton.frame.ag <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")
    colnames(compton.frame.ag) <- c("Spectrum", "Compton")
    compton.frame.ag[compton.frame.ag ==0 ] <- 1

    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lucas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none, stringsAsFactors=FALSE)
    colnames(lucas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lucas.slope.table <- data.frame(spectra.line.table, slope.none, stringsAsFactors=FALSE)
    colnames(lucas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    
    lucas.intercept.table.comp <- data.frame(rowSums(lucas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")])/compton.frame.ag$Compton, stringsAsFactors=FALSE)
    colnames(lucas.intercept.table.comp) <- c("first")
    
    
    
    lucas.intercept.comp <- lucas.intercept.table.comp$first
    lucas.slope.comp <- data.frame(lucas.slope.table[,slope.element.lines]/compton.frame.ag$Compton, stringsAsFactors=FALSE)
    colnames(lucas.slope.comp) <- slope.element.lines
    
    
    predict.frame.luc.comp <- data.frame(Intensity=((1+intensity/compton.frame.ag$Compton)/(intensity/compton.frame.ag$Compton+lucas.intercept.comp)-lucas.intercept.comp/(intensity/compton.frame.ag$Compton+lucas.intercept.comp)),lucas.slope.comp, stringsAsFactors=FALSE)
   
    predict.frame.luc.comp
}
lucas_comp_prep_xrf <- cmpfun(lucas_comp_prep_xrf)




###############
###Prep Data###
###############


###############
###Net Counts##
###############


general_prep_xrf_net <- function(spectra.line.table, element.line) {
    
    intensity <- spectra.line.table[,element.line]
    
    
    predict.frame <- data.frame(Intensity=intensity, stringsAsFactors=FALSE)
    
    predict.frame
}
general_prep_xrf_net <- cmpfun(general_prep_xrf_net)


simple_tc_prep_xrf_net <- function(data,spectra.line.table, element.line) {
    
    intensity <- spectra.line.table[,element.line]
    
    total.counts.net <- rowSums(spectra.line.table[,-1])
    total.counts <- data.frame(data$Spectrum, total.counts.net, stringsAsFactors=FALSE)
    colnames(total.counts) <- c("Spectrum", "CPS")
    
    
    
    predict.frame.tc <- data.frame(Intensity=intensity/total.counts$CPS, stringsAsFactors=FALSE)
    
    predict.frame.tc
}
simple_tc_prep_xrf_net <- cmpfun(simple_tc_prep_xrf_net)


simple_comp_prep_xrf_net <- function(data, spectra.line.table, element.line, norm.min, norm.max) {
    
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    compton.ag.fake.Spectrum <- data$Spectrum
    compton.ag.fake.Compton <- rep(1, length(data$Spectrum))
    compton.ag.fake <- data.frame(compton.ag.fake.Spectrum,compton.ag.fake.Compton, stringsAsFactors=FALSE)
    colnames(compton.ag.fake) <- c("Spectrum", "Compton")
    
    predict.frame.comp <- data.frame(Intensity=intensity/compton.ag.fake$Compton, stringsAsFactors=FALSE)

    predict.frame.comp
    
}
simple_comp_prep_xrf_net <- cmpfun(simple_comp_prep_xrf_net)



###Prep Data



lucas_simp_prep_xrf_net <- function(spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lucas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none, stringsAsFactors=FALSE)
    colnames(lucas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lucas.slope.table <- data.frame(spectra.line.table, slope.none, stringsAsFactors=FALSE)
    colnames(lucas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    lucas.intercept.table <- data.frame(rowSums(lucas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")]), stringsAsFactors=FALSE)
    colnames(lucas.intercept.table) <- c("first")
    
    
    
    lucas.intercept <- lucas.intercept.table$first
    lucas.slope <- data.frame(lucas.slope.table[,slope.element.lines], stringsAsFactors=FALSE)
    colnames(lucas.slope) <- slope.element.lines
    
    
    
    predict.frame.luk <- data.frame(Intensity=((1+intensity/(intensity+lucas.intercept))-lucas.intercept/(intensity+lucas.intercept)),lucas.slope, stringsAsFactors=FALSE)
    
    
    
    
    predict.frame.luk
    
    
}
lucas_simp_prep_xrf_net <- cmpfun(lucas_simp_prep_xrf_net)



lucas_tc_prep_xrf_net <- function(data, spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    total.counts.net <- rowSums(spectra.line.table[,-1])
    total.counts <- data.frame(data$Spectrum, total.counts.net, stringsAsFactors=FALSE)
    colnames(total.counts) <- c("Spectrum", "CPS")
    
    
    
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lucas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none, stringsAsFactors=FALSE)
    colnames(lucas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lucas.slope.table <- data.frame(spectra.line.table, slope.none, stringsAsFactors=FALSE)
    colnames(lucas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    
    lucas.intercept.table.tc <- data.frame(rowSums(lucas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")]), stringsAsFactors=FALSE)/total.counts$CPS
    colnames(lucas.intercept.table.tc) <- c("first")
    
    
    
    
    lucas.intercept.tc <- lucas.intercept.table.tc$first
    lucas.slope.tc <- data.frame(lucas.slope.table[,slope.element.lines], stringsAsFactors=FALSE)/total.counts$CPS
    colnames(lucas.slope.tc) <- slope.element.lines
    
    
    predict.intensity.luc.tc <- data.frame(Intensity=((1+intensity/(intensity+lucas.intercept.tc)-lucas.intercept.tc/(intensity+lucas.intercept.tc))),lucas.slope.tc, stringsAsFactors=FALSE)
    
    predict.intensity.luc.tc
}
lucas_tc_prep_xrf_net <- cmpfun(lucas_tc_prep_xrf_net)


lucas_comp_prep_xrf_net <- function(data, spectra.line.table, element.line, slope.element.lines, intercept.element.lines, norm.min, norm.max) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    
    
    compton.ag.fake.Spectrum <- data$Spectrum
    compton.ag.fake.Compton <- rep(1, length(data$Spectrum))
    compton.ag.fake <- data.frame(compton.ag.fake.Spectrum,compton.ag.fake.Compton, stringsAsFactors=FALSE)
    colnames(compton.ag.fake) <- c("Spectrum", "Compton")
    
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lucas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none, stringsAsFactors=FALSE)
    colnames(lucas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lucas.slope.table <- data.frame(spectra.line.table, slope.none, stringsAsFactors=FALSE)
    colnames(lucas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    
    lucas.intercept.table.comp <- data.frame(rowSums(lucas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")]), stringsAsFactors=FALSE)/compton.ag.fake$Compton
    colnames(lucas.intercept.table.comp) <- c("first")
    
    
    
    
    lucas.intercept.comp <- lucas.intercept.table.comp$first
    lucas.slope.comp <- data.frame(lucas.slope.table[,slope.element.lines], stringsAsFactors=FALSE)/compton.ag.fake$Compton
    colnames(lucas.slope.comp) <- slope.element.lines
    
    

    predict.frame.luc.comp <- data.frame(Intensity=((1+intensity/(intensity+lucas.intercept.comp)-lucas.intercept.comp/(intensity+lucas.intercept.comp))),lucas.slope.comp, stringsAsFactors=FALSE)
    
    
    predict.frame.luc.comp
}
lucas_comp_prep_xrf_net <- cmpfun(lucas_comp_prep_xrf_net)



blank.data.frame <- data.frame(rep(0, length(standard)), rep(0, length(standard)), rep(0, length(standard)), rep(0, length(standard)), rep(0, length(standard)), rep(0, length(standard)), rep(0, length(standard)), stringsAsFactors=FALSE)
colnames(blank.data.frame) <- standard


combos.xrf <- function(a.vector){
    
    so <- seq(from=2, to=length(a.vector), by=1)
    
    long <- pblapply(so, function(x) combnPrim(x=a.vector, m=x), cl=6L)
    and <- pblapply(long, function(x) plyr::alply(x, 2), cl=6L)
    thanks.for.all.the.fish <- do.call(list, unlist(and, recursive=FALSE))
    
    thanks.for.all.the.fish
    
}
combos.xrf <- cmpfun(combos.xrf)


create.frame.slopes.xrf <- function(element, slopes, values, intensities){
    values <- values[complete.cases(values[,element]),]
    intensities <- intensities[complete.cases(values[,element]),]
    
    data.frame(Value=values[,element],
    Intensity=intensities[,"Intensity"],
    intensities[,slopes], stringsAsFactors=FALSE)
    
}
create.frame.slopes.xrf <- cmpfun(create.frame.slopes.xrf)


create.frame.intercepts.xrf <- function(element, slopes, values, intensities){
    
    data.frame(Value=values[,element],
    Intensity=intensities[,"Intensity"],
    intensities[,slopes], stringsAsFactors=FALSE)
    
}
create.frame.intercepts.xrf <- cmpfun(create.frame.intercepts.xrf)



optimal_r_chain.xrf <- function(element, intensities, values, possible.slopes, keep){
    
    values <- values[complete.cases(values[,element]),]
    intensities <- intensities[complete.cases(values[,element]),]
    index <- seq(1, length(possible.slopes), 1)
    
    chain.lm <- pbapply::pblapply(possible.slopes, function(x) lm(Value~Intensity+., data=create.frame.slopes(element=element, slopes=x, values=values[keep,], intensities=intensities)[keep,]))
    
    #chain.predict <- pblapply(index, function(x) predict(object=chain.lm[[x]], newdata=create.frame.slopes(element=element, slopes=possible.slopes[[x]], values=values[keep,], intensities=intensities)[keep,], interval='confidence'))
    #chain.fits <- pblapply(chain.predict, function(x) data.frame(x)$fit)
    #val.lm <- pblapply(chain.fits, function(x) lm(values[,element]~x))
    
    aic <- lapply(chain.lm, function(x) extractAIC(x, k=log(length(possible.slopes)))[2])
    best <- chain.lm[[which.min(unlist(aic))]]
    best.aic <- unlist(aic)[which.min(unlist(aic))]
    #r.adj <- lapply(chain.lm, function(x) summary(x)$adj.r.squared)
    #best <- chain.lm[[which.max(unlist(r.adj))]]
    coef <- data.frame(best$coefficients, stringsAsFactors=FALSE)
    best.var <- rownames(coef)[3:length(rownames(coef))]
    
    simple.lm <- lm(Value~Intensity, data=create.frame.slopes(element=element, slopes=element, values=values, intensities=intensities)[keep,])
    #simple.predict <- as.data.frame(predict(simple.lm, newdata=create.frame.slopes(element=element, slopes=element, values=values[keep,], intensities=intensities)[keep,], interval='confidence'), interval='confidence')$fit
    #simple.val <- lm(values[,element]~simple.predict)
    simple.aic <- extractAIC(simple.lm, k=log(length(1)))[2]
    
    if(simple.aic <= best.aic){
           element
        } else if(best.aic < simple.aic){
           best.var
       }
    
    #best.var
}
optimal_r_chain.xrf <- cmpfun(optimal_r_chain.xrf)



optimal_norm_chain_xrf <- function(data, element, spectra.line.table, values, possible.mins, possible.maxs){
    
    index <- seq(1, length(possible.mins), 1)
    
    chain.lm <- pbapply::pblapply(index, function(x) lm(values[,element]~simple_comp_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=element, norm.min=possible.mins[x], norm.max=possible.maxs[x])$Intensity, na.action=na.exclude))
    aic <- lapply(chain.lm, function(x) extractAIC(x, k=log(length(1)))[2])
    best <- index[[which.min(unlist(aic))]]

    
    best
    
}
optimal_norm_chain_xrf <- cmpfun(optimal_norm_chain_xrf)


optimal_intercept_chain_xrf <- function(element, intensities, values, keep){
    
    
    chain.lm <- pbapply::pblapply(intensities, function(x) lm(values[,element]~Intensity, data=x[keep,]))
    aic <- lapply(chain.lm, function(x) extractAIC(x, k=log(1))[2])
    best <- chain.lm[[which.min(unlist(aic))]]
    coef <- data.frame(best$coefficients, stringsAsFactors=FALSE)
    best.var <- rownames(coef)[3:length(rownames(coef))]
    
    best.var
    
}
optimal_intercept_chain_xrf <- cmpfun(optimal_intercept_chain_xrf)


likely_intercepts_xrf <- function(element){
    
    if(element=="Na.K.alpha"){
        c("Cl.K.alpha", "Rh.L.alpha")
    } else if(element=="Mg.K.alpha"){
        c("Rh.L.alpha", "Al.K.alpha", "Cl.K.alpha")
    } else if(element=="Al.K.alpha"){
        c("Mg.K.alpha", "Si.K.alpha", "K.K.alpha")
    } else if(element=="Si.K.alpha"){
        c("Al.K.alpha", "Ca.K.alpha")
    } else if(element=="P.K.alpha"){
        c("Ca.K.alpha", "Si.K.alpha", "S.K.alpha")
    } else if(element=="S.K.alpha"){
        c("Rh.L.alpha", "P.K.alpha", "Cl.K.alpha")
    } else if(element=="Cl.K.alpha"){
        c("Rh.L.alpha", "S.K.alpha")
    } else if(element=="K.K.alpha"){
        c("Rh.L.alpha", "Ag.L.alpha", "Cd.L.alpha")
    } else if(element=="Ca.K.alpha"){
        c("K.K.alpha", "Ag.L.alpha", "Cd.L.alpha")
    } else if(element=="Sc.K.alpha"){
        c("Ca.K.alpha", "Cd.L.alpha")
    } else if(element=="Ti.K.alpha"){
        c("Ba.L.alpha", "Fe.K.alpha")
    } else if(element=="V.K.alpha"){
        c("Ba.L.alpha", "Ti.K.alpha")
    } else if(element=="Cr.K.alpha"){
        c("Ba.L.alpha", "V.K.alpha")
    } else if(element=="Mn.K.alpha"){
        c("Cr.K.alpha", "Ba.L.alpha")
    } else if(element=="Mn.K.alpha"){
        c("Cr.K.alpha", "Ba.L.alpha", "Fe.K.alpha")
    } else if(element=="Fe.K.alpha"){
        c("Mn.K.alpha", "K.K.alpha", "Cu.K.alpha", "Ca.K.alpha")
    } else if(element=="Co.K.alpha"){
        c("Fe.K.alpha", "Ca.K.alpha", "Zn.K.alpha")
    } else if(element=="Ni.K.alpha"){
        c("Co.K.alpha", "Ca.K.alpha", "Zn.K.alpha")
    } else if(element=="Cu.K.alpha"){
        c("Ni.K.alpha", "Zn.K.alpha")
    } else if(element=="Zn.K.alpha"){
        c("Cu.K.alpha", "Pb.L.alpha", "Au.L.alpha")
    } else if(element=="Ga.K.alpha"){
        c("Zn.K.alpha", "Au.L.alpha", "Pb.L.alpha")
    } else if(element=="As.K.alpha"){
        c("Pb.L.beta", "Cr.K.alpha")
    } else if(element=="Rb.K.alpha"){
        c("Th.L.alpha", "Fe.K.alpha")
    } else if(element=="Sr.K.alpha"){
        c("U.L.alpha", "Zr.K.alpha", "Co.K.alpha")
    } else if(element=="Y.K.alpha"){
        c("Rb.K.alpha", "Ni.K.alpha", "Nb.K.alpha")
    } else if(element=="Zr.K.alpha"){
        c("Sr.K.alpha", "Cu.K.alpha", "Mo.K.alpha")
    } else if(element=="Nb.K.alpha"){
        c("Y.K.alpha", "Cu.K.alpha", "Zn.K.alpha", "Rh.K.alpha")
    } else if(element=="Mo.K.alpha"){
        c("Zr.K.alpha", "Rh.K.alpha", "Zn.K.alpha")
    } else if(element=="Ag.K.alpha"){
        c("Rh.K.alpha", "Pd.K.alpha")
    } else if(element=="Cd.K.alpha"){
        c("Rh.K.alpha", "Pd.K.alpha")
    } else if(element=="Sn.K.alpha"){
        c("Rh.K.alpha", "Ag.K.alpha")
    } else if(element=="Sb.K.alpha"){
        c("Sn.K.alpha", "Rh.K.alpha")
    } else if(element=="Ba.L.alpha"){
        c("Ti.K.alpha", "Fe.K.alpha")
    } else if(element=="La.L.alpha"){
        c("Ti.K.alpha", "Fe.K.alpha")
    } else if(element=="Ce.L.alpha"){
        c("Ti.K.alpha", "V.K.alpha", "Ba.L.alpha", "Fe.K.alpha")
    } else if(element=="Nd.L.alpha"){
        c("Ti.K.alpha", "Cr.K.alpha", "V.K.alpha", "Ba.L.alpha", "Fe.K.alpha")
    } else if(element=="W.L.alpha"){
        c("Cu.K.alpha", "Ni.K.alpha")
    } else if(element=="Au.L.alpha"){
        c("Zn.K.alpha", "Ga.K.alpha", "W.L.alpha")
    } else if(element=="Hg.L.alpha"){
        c("Pb.L.alpha", "Au.L.alpha")
    } else if(element=="Pb.L.beta"){
        c("As.K.alpha", "Th.L.alpha")
    } else if(element=="Th.L.alpha"){
        c("Rb.K.alpha", "Pb.L.alpha")
    } else if(element=="U.L.alpha"){
        c("Rb.K.alpha", "Sr.K.alpha")
    }
}
likely_intercepts_xrf <- cmpfun(likely_intercepts_xrf)


peak_threshold_xrf <- function(spectrum){
    
    spectrum$Hodder <- Hodder.v(Hodder.v(spectrum$CPS))*-1
    spectrum$Peaks <- ifelse(spectrum$Hodder > 0, spectrum$Hodder, 0)
    spectrum$isPeak <- ifelse(log(spectrum$Peaks) > 1, TRUE, FALSE)
    ggplot(spectrum) + geom_line(aes(Energy, Peaks)) + theme_light() + scale_y_log10()
    ggplot(spectrum) + geom_line(aes(Energy, CPS)) + theme_light() + geom_point(data=spectrum[spectrum$isPeak,], aes(Energy, CPS), colour="red", alpha=0.5)
    
}
peak_threshold_xrf <- cmpfun(peak_threshold_xrf)



find_peaks_xrf <- function(spectrum){
    
    #spectrum$Hodder <- Hodder.v(spectrum$CPS)
    #spectrum$Peak <- ifelse(spectrum$Hodder < (-200), TRUE, FALSE)
    #ggplot(spectrum) + geom_line(aes(Energy, Hodder)) + theme_light() + geom_point(data=spectrum[spectrum$Peak,], aes(Energy, Hodder), colour="red", alpha=0.5)
    
    #spectrum$Hodder2 <- Hodder.v(spectrum$Hodder)
    #ggplot(spectrum) + geom_line(aes(Energy, Hodder2)) + theme_light()

    #spectrum$Peak <- ifelse(spectrum$Hodder2 < (-1), TRUE, FALSE)
    #ggplot(spectrum) + geom_line(aes(Energy, Hodder2)) + theme_light() + geom_point(data=spectrum[spectrum$Peak,], aes(Energy, Hodder2), colour="red", alpha=0.5)


    spectrum$Hodder <- Hodder.v(Hodder.v(spectrum$CPS))
    spectrum$Peak <- ifelse(spectrum$Hodder < (-1), TRUE, FALSE)
    data.frame(Energy=spectrum[spectrum$Peak,]$Energy, CPS=spectrum[spectrum$Peak,]$CPS, stringsAsFactors=FALSE)

}
find_peaks_xrf <- cmpfun(find_peaks_xrf)



####Custom Lines


range_subset <- function(range.frame, data){
    
    new.data <- subset(data, Energy >= range.frame$EnergyMin & Energy <= range.frame$EnergyMax, drop=TRUE)
    newer.data <- aggregate(new.data, by=list(new.data$Spectrum), FUN=mean, na.rm=TRUE)[,c("Group.1", "CPS")]
    colnames(newer.data) <- c("Spectrum", as.character(range.frame$Name))
    newer.data
}
range_subset <- cmpfun(range_subset)


xrf_parse <- function(range.table, data){
    
    choice.lines <- range.table[complete.cases(range.table),]
    
    choice.list <- split(choice.lines, f=choice.lines$Name)
    names(choice.list) <- choice.lines[,"Name"]
    
    index <- choice.lines[,"Name"]
    
    selected.list <- lapply(index, function(x) range_subset(range.frame=choice.list[[x]], data=data))
    
    Reduce(function(...) merge(..., all=T), selected.list)
}
xrf_parse <- cmpfun(xrf_parse)


###Unit Transformation

data_summarize <- function(xrf.table) {
    
    xrf.table
    
    xrf.table$Depth <- round(xrf.table$Depth, 1)
    #xrf.table <- subset(xrf.table, !(xrf.table$Depth < 5 | xrf.table$Depth > 37))
    
    xrf.table <- data.table(xrf.table)
    ###Neds work
    xrf.table.aggregate <- xrf.table[, list(CPS=mean(CPS, na.rm = TRUE)), by = list(Depth)]
    
    data <- as.data.frame(dcast.data.table(xrf.table.aggregate, Spectrum~Energy, value.var="CPS"), stringsAsFactors=FALSE)

    data
    
    
}
data_summarize <- cmpfun(data_summarize)


plot.nnet<-function(mod.in,nid=T,all.out=T,all.in=T,bias=T,wts.only=F,rel.rsc=5,
circle.cex=5,node.labs=T,var.labs=T,x.lab=NULL,y.lab=NULL,
line.stag=NULL,struct=NULL,cex.val=1,alpha.val=1,
circle.col='lightblue',pos.col='black',neg.col='grey',
bord.col='lightblue', max.sp = F,...){
    
    require(scales)
    
    #sanity checks
    if('mlp' %in% class(mod.in)) warning('Bias layer not applicable for rsnns object')
    if('numeric' %in% class(mod.in)){
        if(is.null(struct)) stop('Three-element vector required for struct')
        if(length(mod.in) != ((struct[1]*struct[2]+struct[2]*struct[3])+(struct[3]+struct[2])))
        stop('Incorrect length of weight matrix for given network structure')
    }
    if('train' %in% class(mod.in)){
        if('nnet' %in% class(mod.in$finalModel)){
            mod.in<-mod.in$finalModel
            warning('Using best nnet model from train output')
        } else if('nn' %in% class(mod.in$finalModel)){
            mod.o <- mod.in
            mod.in<-mod.in$finalModel
            warning('Using best nn model from train output')
        }
        else stop('Only nnet method can be used with train object')
    }
    
    #gets weights for neural network, output is list
    #if rescaled argument is true, weights are returned but rescaled based on abs value
    nnet.vals<-function(mod.in,nid,rel.rsc,struct.out=struct){
        
        require(scales)
        require(reshape)
        
        if('numeric' %in% class(mod.in)){
            struct.out<-struct
            wts<-mod.in
        }
        
        #neuralnet package
        if('nn' %in% class(mod.in)){
            struct.out<-unlist(lapply(mod.in$weights[[1]],ncol))
            struct.out<-struct.out[-length(struct.out)]
            struct.out<-c(
            length(mod.in$model.list$variables),
            struct.out,
            length(mod.in$model.list$response)
            )
            wts<-unlist(mod.in$weights[[1]])
        }
        
        #nnet package
        if('nnet' %in% class(mod.in)){
            struct.out<-mod.in$n
            wts<-mod.in$wts
        }
        
        #RSNNS package
        if('mlp' %in% class(mod.in)){
            struct.out<-c(mod.in$nInputs,mod.in$archParams$size,mod.in$nOutputs)
            hid.num<-length(struct.out)-2
            wts<-mod.in$snnsObject$getCompleteWeightMatrix()
            
            #get all input-hidden and hidden-hidden wts
            inps<-wts[grep('Input',row.names(wts)),grep('Hidden_2',colnames(wts)),drop=F]
            inps<-melt(rbind(rep(NA,ncol(inps)),inps))$value
            uni.hids<-paste0('Hidden_',1+seq(1,hid.num))
            for(i in 1:length(uni.hids)){
                if(is.na(uni.hids[i+1])) break
                tmp<-wts[grep(uni.hids[i],rownames(wts)),grep(uni.hids[i+1],colnames(wts)),drop=F]
                inps<-c(inps,melt(rbind(rep(NA,ncol(tmp)),tmp))$value)
            }
            
            #get connections from last hidden to output layers
            outs<-wts[grep(paste0('Hidden_',hid.num+1),row.names(wts)),grep('Output',colnames(wts)),drop=F]
            outs<-rbind(rep(NA,ncol(outs)),outs)
            
            #weight vector for all
            wts<-c(inps,melt(outs)$value)
            assign('bias',F,envir=environment(nnet.vals))
        }
        
        if(nid) wts<-rescale(abs(wts),c(1,rel.rsc))
        
        #convert wts to list with appropriate names
        hid.struct<-struct.out[-c(length(struct.out))]
        row.nms<-NULL
        for(i in 1:length(hid.struct)){
            if(is.na(hid.struct[i+1])) break
            row.nms<-c(row.nms,rep(paste('hidden',i,seq(1:hid.struct[i+1])),each=1+hid.struct[i]))
        }
        row.nms<-c(
        row.nms,
        rep(paste('out',seq(1:struct.out[length(struct.out)])),each=1+struct.out[length(struct.out)-1])
        )
        out.ls<-data.frame(wts,row.nms, stringsAsFactors=FALSE)
        out.ls$row.nms<-factor(row.nms,levels=unique(row.nms),labels=unique(row.nms))
        out.ls<-split(out.ls$wts,f=out.ls$row.nms)
        
        assign('struct',struct.out,envir=environment(nnet.vals))
        
        out.ls
        
    }
    
    wts<-nnet.vals(mod.in,nid=F)
    
    if(wts.only) return(wts)
    
    #circle colors for input, if desired, must be two-vector list, first vector is for input layer
    if(is.list(circle.col)){
        circle.col.inp<-circle.col[[1]]
        circle.col<-circle.col[[2]]
    } else circle.col.inp<-circle.col
    
    #initiate plotting
    x.range<-c(0,100)
    y.range<-c(0,100)
    #these are all proportions from 0-1
    if(is.null(line.stag)) line.stag<-0.011*circle.cex/2
    layer.x<-seq(0.17,0.9,length=length(struct))
    bias.x<-layer.x[-length(layer.x)]+diff(layer.x)/2
    bias.y<-0.95
    circle.cex<-circle.cex
    
    #get variable names from mod.in object
    #change to user input if supplied
    if('numeric' %in% class(mod.in)){
        x.names<-paste0(rep('X',struct[1]),seq(1:struct[1]))
        y.names<-paste0(rep('Y',struct[3]),seq(1:struct[3]))
    }
    if('mlp' %in% class(mod.in)){
        all.names<-mod.in$snnsObject$getUnitDefinitions()
        x.names<-all.names[grep('Input',all.names$unitName),'unitName']
        y.names<-all.names[grep('Output',all.names$unitName),'unitName']
    }
    if('nn' %in% class(mod.in)){
        x.names<-mod.in$model.list$variables
        y.names<-mod.in$model.list$respons
    }
    if('xNames' %in% names(mod.in)){
        x.names<-mod.in$xNames
        y.names<-if('nn' %in% class(mod.in)){
            attr(terms(mod.o),'factor')
        } else {
            attr(terms(mod.in),'factor')
        }
        
        y.names<-row.names(y.names)[!row.names(y.names) %in% x.names]
    }
    if(!'xNames' %in% names(mod.in) & 'nnet' %in% class(mod.in)){
        if(is.null(mod.in$call$formula)){
            x.names<-colnames(eval(mod.in$call$x))
            y.names<-colnames(eval(mod.in$call$y))
        }
        else{
            forms<-eval(mod.in$call$formula)
            x.names<-mod.in$coefnames
            facts<-attr(terms(mod.in),'factors')
            y.check<-mod.in$fitted
            if(ncol(y.check)>1) y.names<-colnames(y.check)
            else y.names<-as.character(forms)[2]
        }
    }
    #change variables names to user sub
    if(!is.null(x.lab)){
        if(length(x.names) != length(x.lab)) stop('x.lab length not equal to number of input variables')
        else x.names<-x.lab
    }
    if(!is.null(y.lab)){
        if(length(y.names) != length(y.lab)) stop('y.lab length not equal to number of output variables')
        else y.names<-y.lab
    }
    
    #initiate plot
    plot(x.range,y.range,type='n',axes=F,ylab='',xlab='',...)
    
    #function for getting y locations for input, hidden, output layers
    #input is integer value from 'struct'
    get.ys<-function(lyr, max_space = max.sp){
        if(max_space){
            spacing <- diff(c(0*diff(y.range),0.9*diff(y.range)))/lyr
        } else {
            spacing<-diff(c(0*diff(y.range),0.9*diff(y.range)))/max(struct)
        }
        
        seq(0.5*(diff(y.range)+spacing*(lyr-1)),0.5*(diff(y.range)-spacing*(lyr-1)),
        length=lyr)
    }
    
    #function for plotting nodes
    #'layer' specifies which layer, integer from 'struct'
    #'x.loc' indicates x location for layer, integer from 'layer.x'
    #'layer.name' is string indicating text to put in node
    layer.points<-function(layer,x.loc,layer.name,cex=cex.val){
        x<-rep(x.loc*diff(x.range),layer)
        y<-get.ys(layer)
        points(x,y,pch=21,cex=circle.cex,col=bord.col,bg=in.col)
        if(node.labs) text(x,y,paste(layer.name,1:layer,sep=''),cex=cex.val)
        if(layer.name=='I' & var.labs) text(x-line.stag*diff(x.range),y,x.names,pos=2,cex=cex.val)
        if(layer.name=='O' & var.labs) text(x+line.stag*diff(x.range),y,y.names,pos=4,cex=cex.val)
    }
    
    #function for plotting bias points
    #'bias.x' is vector of values for x locations
    #'bias.y' is vector for y location
    #'layer.name' is  string indicating text to put in node
    bias.points<-function(bias.x,bias.y,layer.name,cex,...){
        for(val in 1:length(bias.x)){
            points(
            diff(x.range)*bias.x[val],
            bias.y*diff(y.range),
            pch=21,col=bord.col,bg=in.col,cex=circle.cex
            )
            if(node.labs)
            text(
            diff(x.range)*bias.x[val],
            bias.y*diff(y.range),
            paste(layer.name,val,sep=''),
            cex=cex.val
            )
        }
    }
    
    #function creates lines colored by direction and width as proportion of magnitude
    #use 'all.in' argument if you want to plot connection lines for only a single input node
    layer.lines<-function(mod.in,h.layer,layer1=1,layer2=2,out.layer=F,nid,rel.rsc,all.in,pos.col,
    neg.col,...){
        
        x0<-rep(layer.x[layer1]*diff(x.range)+line.stag*diff(x.range),struct[layer1])
        x1<-rep(layer.x[layer2]*diff(x.range)-line.stag*diff(x.range),struct[layer1])
        
        if(out.layer==T){
            
            y0<-get.ys(struct[layer1])
            y1<-rep(get.ys(struct[layer2])[h.layer],struct[layer1])
            src.str<-paste('out',h.layer)
            
            wts<-nnet.vals(mod.in,nid=F,rel.rsc)
            wts<-wts[grep(src.str,names(wts))][[1]][-1]
            wts.rs<-nnet.vals(mod.in,nid=T,rel.rsc)
            wts.rs<-wts.rs[grep(src.str,names(wts.rs))][[1]][-1]
            
            cols<-rep(pos.col,struct[layer1])
            cols[wts<0]<-neg.col
            
            if(nid) segments(x0,y0,x1,y1,col=cols,lwd=wts.rs)
            else segments(x0,y0,x1,y1)
            
        }
        
        else{
            
            if(is.logical(all.in)) all.in<-h.layer
            else all.in<-which(x.names==all.in)
            
            y0<-rep(get.ys(struct[layer1])[all.in],struct[2])
            y1<-get.ys(struct[layer2])
            src.str<-paste('hidden',layer1)
            
            wts<-nnet.vals(mod.in,nid=F,rel.rsc)
            wts<-unlist(lapply(wts[grep(src.str,names(wts))],function(x) x[all.in+1]))
            wts.rs<-nnet.vals(mod.in,nid=T,rel.rsc)
            wts.rs<-unlist(lapply(wts.rs[grep(src.str,names(wts.rs))],function(x) x[all.in+1]))
            
            cols<-rep(pos.col,struct[layer2])
            cols[wts<0]<-neg.col
            
            if(nid) segments(x0,y0,x1,y1,col=cols,lwd=wts.rs)
            else segments(x0,y0,x1,y1)
            
        }
        
    }
    
    bias.lines<-function(bias.x,mod.in,nid,rel.rsc,all.out,pos.col,neg.col,...){
        
        if(is.logical(all.out)) all.out<-1:struct[length(struct)]
        else all.out<-which(y.names==all.out)
        
        for(val in 1:length(bias.x)){
            
            wts<-nnet.vals(mod.in,nid=F,rel.rsc)
            wts.rs<-nnet.vals(mod.in,nid=T,rel.rsc)
            
            if(val != length(bias.x)){
                wts<-wts[grep('out',names(wts),invert=T)]
                wts.rs<-wts.rs[grep('out',names(wts.rs),invert=T)]
                sel.val<-grep(val,substr(names(wts.rs),8,8))
                wts<-wts[sel.val]
                wts.rs<-wts.rs[sel.val]
            }
            
            else{
                wts<-wts[grep('out',names(wts))]
                wts.rs<-wts.rs[grep('out',names(wts.rs))]
            }
            
            cols<-rep(pos.col,length(wts))
            cols[unlist(lapply(wts,function(x) x[1]))<0]<-neg.col
            wts.rs<-unlist(lapply(wts.rs,function(x) x[1]))
            
            if(nid==F){
                wts.rs<-rep(1,struct[val+1])
                cols<-rep('black',struct[val+1])
            }
            
            if(val != length(bias.x)){
                segments(
                rep(diff(x.range)*bias.x[val]+diff(x.range)*line.stag,struct[val+1]),
                rep(bias.y*diff(y.range),struct[val+1]),
                rep(diff(x.range)*layer.x[val+1]-diff(x.range)*line.stag,struct[val+1]),
                get.ys(struct[val+1]),
                lwd=wts.rs,
                col=cols
                )
            }
            
            else{
                segments(
                rep(diff(x.range)*bias.x[val]+diff(x.range)*line.stag,struct[val+1]),
                rep(bias.y*diff(y.range),struct[val+1]),
                rep(diff(x.range)*layer.x[val+1]-diff(x.range)*line.stag,struct[val+1]),
                get.ys(struct[val+1])[all.out],
                lwd=wts.rs[all.out],
                col=cols[all.out]
                )
            }
            
        }
    }
    
    #use functions to plot connections between layers
    #bias lines
    if(bias) bias.lines(bias.x,mod.in,nid=nid,rel.rsc=rel.rsc,all.out=all.out,pos.col=alpha(pos.col,alpha.val),
    neg.col=alpha(neg.col,alpha.val))
    
    #layer lines, makes use of arguments to plot all or for individual layers
    #starts with input-hidden
    #uses 'all.in' argument to plot connection lines for all input nodes or a single node
    if(is.logical(all.in)){
        mapply(
        function(x) layer.lines(mod.in,x,layer1=1,layer2=2,nid=nid,rel.rsc=rel.rsc,
        all.in=all.in,pos.col=alpha(pos.col,alpha.val),neg.col=alpha(neg.col,alpha.val)),
        1:struct[1]
        )
    }
    else{
        node.in<-which(x.names==all.in)
        layer.lines(mod.in,node.in,layer1=1,layer2=2,nid=nid,rel.rsc=rel.rsc,all.in=all.in,
        pos.col=alpha(pos.col,alpha.val),neg.col=alpha(neg.col,alpha.val))
    }
    #connections between hidden layers
    lays<-split(c(1,rep(2:(length(struct)-1),each=2),length(struct)),
    f=rep(1:(length(struct)-1),each=2))
    lays<-lays[-c(1,(length(struct)-1))]
    for(lay in lays){
        for(node in 1:struct[lay[1]]){
            layer.lines(mod.in,node,layer1=lay[1],layer2=lay[2],nid=nid,rel.rsc=rel.rsc,all.in=T,
            pos.col=alpha(pos.col,alpha.val),neg.col=alpha(neg.col,alpha.val))
        }
    }
    #lines for hidden-output
    #uses 'all.out' argument to plot connection lines for all output nodes or a single node
    if(is.logical(all.out))
    mapply(
    function(x) layer.lines(mod.in,x,layer1=length(struct)-1,layer2=length(struct),out.layer=T,nid=nid,rel.rsc=rel.rsc,
    all.in=all.in,pos.col=alpha(pos.col,alpha.val),neg.col=alpha(neg.col,alpha.val)),
    1:struct[length(struct)]
    )
    else{
        node.in<-which(y.names==all.out)
        layer.lines(mod.in,node.in,layer1=length(struct)-1,layer2=length(struct),out.layer=T,nid=nid,rel.rsc=rel.rsc,
        pos.col=pos.col,neg.col=neg.col,all.out=all.out)
    }
    
    #use functions to plot nodes
    for(i in 1:length(struct)){
        in.col<-circle.col
        layer.name<-'H'
        if(i==1) { layer.name<-'I'; in.col<-circle.col.inp}
        if(i==length(struct)) layer.name<-'O'
        layer.points(struct[i],layer.x[i],layer.name)
    }
    
    if(bias) bias.points(bias.x,bias.y,'B')
    
}
plot.nnet <- cmpfun(plot.nnet)

###UI Choices

forestTryUI <- function(radiocal=3, neuralhiddenlayers=1, selection, maxsample){
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        sliderInput("foresttry", label="Sampling", min=2, max=maxsample-2, value=selection)
    }  else if(radiocal==5){
        sliderInput("foresttry", label="Sampling", min=2, max=maxsample-2, value=selection)
    } else if(radiocal==6 && neuralhiddenlayers == 1){
        NULL
    } else if(radiocal==6 && neuralhiddenlayers > 1){
        sliderInput("foresttry", label="Sampling", min=2, max=maxsample-2, value=selection)
    } else if(radiocal==7 && neuralhiddenlayers == 1){
        NULL
    } else if(radiocal==7 && neuralhiddenlayers > 1){
        sliderInput("foresttry", label="Sampling", min=2, max=maxsample-2, value=selection)
    }
}

forestMetricUI <- function(radiocal, selection){
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        selectInput("forestmetric", label="Metric", choices=c("Root Mean Square Error"="RMSE", "R2"="Rsquared", "Mean Absolute Error"="MAE", "Kappa"="Kappa", "Logarithmic Loss"="logLoss"), selected=selection)
    } else if(radiocal==5){
        selectInput("forestmetric", label="Metric", choices=c("Root Mean Square Error"="RMSE", "R2"="Rsquared", "Logarithmic Loss"="logLoss"), selected=selection)
    } else if(radiocal==6){
        selectInput("forestmetric", label="Metric", choices=c("Root Mean Square Error"="RMSE", "R2"="Rsquared", "Logarithmic Loss"="logLoss"), selected=selection)
    } else if(radiocal==7){
        selectInput("forestmetric", label="Metric", choices=c("Root Mean Square Error"="RMSE", "R2"="Rsquared", "Logarithmic Loss"="logLoss"), selected=selection)
    }
}

forestTrainUI <- function(radiocal, selection){
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        selectInput("foresttrain", label="Train Control", choices=c("k-fold Cross Validation"="cv", "Bootstrap"="boot", "0.632 Bootstrap"="boot632", "Optimism Bootstrap"="optimism_boot", "Repeated k-fold Cross Validation"="repeatedcv", "Leave One Out Cross Validation"="LOOCV", "Out of Bag Estimation"="oob"), selected=selection)
    }  else if(radiocal==5){
        selectInput("foresttrain", label="Train Control", choices=c("k-fold Cross Validation"="cv", "Bootstrap"="boot", "0.632 Bootstrap"="boot632", "Optimism Bootstrap"="optimism_boot", "Repeated k-fold Cross Validation"="repeatedcv", "Leave One Out Cross Validation"="LOOCV", "Out of Bag Estimation"="oob"), selected=selection)
    } else if(radiocal==6){
        selectInput("foresttrain", label="Train Control", choices=c("k-fold Cross Validation"="cv", "Bootstrap"="boot", "0.632 Bootstrap"="boot632", "Optimism Bootstrap"="optimism_boot", "Repeated k-fold Cross Validation"="repeatedcv", "Leave One Out Cross Validation"="LOOCV"), selected=selection)
    } else if(radiocal==7){
        selectInput("foresttrain", label="Train Control", choices=c("k-fold Cross Validation"="cv", "Bootstrap"="boot", "0.632 Bootstrap"="boot632", "Optimism Bootstrap"="optimism_boot", "Repeated k-fold Cross Validation"="repeatedcv", "Leave One Out Cross Validation"="LOOCV"), selected=selection)
    }
}

forestNumberUI <- function(radiocal, selection){
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        sliderInput("forestnumber", label="Iterations", min=5, max=500, value=selection)
    }  else if(radiocal==5){
        sliderInput("forestnumber", label="Iterations", min=5, max=500, value=selection)
    } else if(radiocal==6){
        sliderInput("forestnumber", label="Iterations", min=5, max=500, value=selection)
    } else if(radiocal==7){
        sliderInput("forestnumber", label="Iterations", min=5, max=500, value=selection)
    }
}

forestTreesUI <- function(radiocal, selection){
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        sliderInput("foresttrees", label="Trees", min=50, max=2000, value=selection)
    } else if(radiocal==5){
        sliderInput("foresttrees", label="Trees", min=50, max=2000, value=selection)
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    }
}

neuralHiddenLayersUI <- function(radiocal, selection){
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        sliderInput("neuralhiddenlayers", label="Hidden Layers", min=1, max=3, value=selection)
    } else if(radiocal==7){
        sliderInput("neuralhiddenlayers", label="Hidden Layers", min=1, max=3, value=selection)
    }
}

neuralHiddenUnitsUi <- function(radiocal, selection){
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        sliderInput("neuralhiddenunits", label="Hidden Units", min=1, max=10, value=selection)
    } else if(radiocal==7){
        sliderInput("neuralhiddenunits", label="Hidden Units", min=1, max=10, value=selection)
    }
}

neuralWeightDecayUI <- function(radiocal, selection, neuralhiddenlayers){
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6 && neuralhiddenlayers == 1){
        sliderInput("neuralweightdecay", label="Weight Decay", min=0.1, max=0.7, step=0.1, value=selection)
    } else if(radiocal==6 && neuralhiddenlayers > 1){
        NULL
    } else if(radiocal==7 && neuralhiddenlayers == 1){
        sliderInput("neuralweightdecay", label="Weight Decay", min=0.1, max=0.7, step=0.1, value=selection)
    } else if(radiocal==7 && neuralhiddenlayers > 1){
        NULL
    }
}

neuralMaxIterationsUI <- function(radiocal, selection, neuralhiddenlayers){
    if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6 && neuralhiddenlayers == 1){
        sliderInput("neuralmaxiterations", label="Max Iterations", min=50, max=2000, value=selection)
    } else if(radiocal==6 && neuralhiddenlayers > 1){
        NULL
    } else if(radiocal==7 && neuralhiddenlayers == 1){
        sliderInput("neuralmaxiterations", label="Max Iterations", min=50, max=2000, value=selection)
    } else if(radiocal==7 && neuralhiddenlayers > 1){
        NULL
    }
}

lineSubset <- function(spectra, definitions){
    xrf_parse(range.table=definitions, data=spectra)
}

spectraData <- function(spectra, element.lines.to.use, definitions){
    
    line.data <- elementFrame(data=spectra, elements=element.lines.to.use)
    
    table <- definitions
    table <- table[complete.cases(table),]
    
    line.subset <- lineSubset(spectra=spectra, definitions=definitions)
    
    result <- if(length(table[,1])==0){
        line.data
    } else if(length(table[,1])!=0){
        merge(line.data, line.subset, by="Spectrum")
    }
    
    return(result)
}

netData <- function(spectra, element.lines.to.use){
    
    net.data <- spectra
    
    elements <- element.lines.to.use
    
    
    net.data.partial <- net.data[,elements]
    net.data <- data.frame(net.data$Spectrum ,net.data.partial)
    colnames(net.data) <- c("Spectrum", elements)
    net.data <- net.data[order(as.character(net.data$Spectrum)),]
    
    net.data$Spectrum <- gsub(".csv", "", net.data$Spectrum)
    net.data$Spectrum <- gsub(".CSV", "", net.data$Spectrum)
    
    return(net.data)
    
}

holdFrame <- function(intensities, values, element){
    spectra.line.table <- intensities
    concentration.table <- values
    spectra.line.table$Spectrum <- concentration.table$Spectrum
    
    concentration.table <- concentration.table[concentration.table$Spectrum %in% spectra.line.table$Spectrum,]
    spectra.line.table <- spectra.line.table[spectra.line.table$Spectrum %in% concentration.table$Spectrum,]
    
    concentration <- as.vector(as.numeric(unlist(concentration.table[,element])))
    
    hold.frame <- data.frame(spectra.line.table, Concentration=concentration)
    
    return(hold.frame[complete.cases(hold.frame),])
}

spectrumSelect <- function(spectra, hold.frame){
    data <- spectra
    return(data[data$Spectrum %in% hold.frame$Spectrum, ])
}

predictIntensitySimpPre <- function(spectra, hold.frame, element, norm.type, norm.min=NULL, norm.max=NULL, data.type="Spectra"){
    
    data <- spectra
    spectra.line.table <- holdFrame()
    
    if(inorm.type==1){
        if(data.type=="Spectra"){
            general_prep_xrf(spectra.line.table=spectra.line.table, element.line=element)
        } else if(data.type=="Net"){
            general_prep_xrf_net(spectra.line.table=spectra.line.table, element.line=element)
        }
    } else if(norm.type==2){
        predict.intensity <- if(data.type=="Spectra"){
            simple_tc_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=element)
        } else if(data.type=="Net"){
            simple_tc_prep_xrf_net(data=data, spectra.line.table=spectra.line.table, element.line=element)
        }
    } else if(norm.type==3){
        predict.intensity <- if(data.type=="Spectra"){
            simple_comp_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=element, norm.min=norm.min, norm.max=norm.max)
        } else if(data.type=="Net"){
            simple_comp_prep_xrf_net(data=data, spectra.line.table=spectra.line.table, element.line=element, norm.min=norm.min, norm.max=norm.max)
        }
    }
    
    return(predict.intensity)
}


predictFrameSimp <- function(spectra, hold.frame, element, norm.type, norm.min=NULL, norm.max=NULL, data.type="Spectra"){
    
    data <- spectra
    spectra.line.table <- hold.frame
    
    predict.intensity.simp <- predictIntensitySimpPre(spectra=spectra, hold.frame=hold.frame, element=element, norm.type=norm.type, norm.min=norm.min, norm.max=norm.max, data.type=data.type)
    
    predict.frame.simp <- data.frame(predict.intensity.simp, spectra.line.table[,"Concentration"])
    colnames(predict.frame.simp) <- c(names(predict.intensity.simp), "Concentration")
    predict.frame.simp <- predict.frame.simp[complete.cases(predict.frame.simp$Concentration),]
    
    predict.frame.simp
    
}

predictIntensitySimp <- function(predict.frame){
    predict.frame[,!(colnames(predict.frame) %in% "Concentration")]
}

predictIntensityForestPre <- function(spectra, hold.frame, element, intercepts, norm.type, norm.min=NULL, norm.max=NULL, data.type="Spectra"){
    
    data <- spectra
    spectra.line.table <- hold.frame
    element.lines.to.use <- names(hold.frame)[!names(hold.frame) %in% c("Spectrum", "Concentration")]
    
    
    if(norm.type==1){
        if(data.type=="Spectra"){
            lucas_simp_prep_xrf(spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=element.lines.to.use, intercept.element.lines=intercepts)
        } else if(data.type=="Net"){
            lucas_simp_prep_xrf_net(spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=element.lines.to.use, intercept.element.lines=intercepts)
        }
    } else if(norm.type==2){
        predict.intensity <- if(data.type=="Spectra"){
            lucas_tc_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=element.lines.to.use, intercept.element.lines=intercepts)
        } else if(data.type=="Net"){
            lucas_tc_prep_xrf_net(data=data, spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=element.lines.to.use, intercept.element.lines=intercepts)
        }
    } else if(norm.type==3){
        predict.intensity <- if(data.type=="Spectra"){
            lucas_comp_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=element.lines.to.use, intercept.element.lines=intercepts, norm.min=norm.min, norm.max=nom.max)
        } else if(data.type=="Net"){
            lucas_comp_prep_xrf_net(data=data, spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=element.lines.to.use, intercept.element.lines=intercepts, norm.min=norm.min, norm.max=norm.max)
        }
    }
    
    return(predict.intensity)
}


predictFrameForest <- function(spectra, hold.frame, element, intercepts, norm.type, norm.min=NULL, norm.max=NULL, data.type="Spectra"){
    
    spectra.line.table <- hold.frame
    
    
    predict.intensity.forest <- predictIntensityForestPre(spectra=spectra, hold.frame=hold.frame, element=element, intercepts=intercepts, norm.type=norm.type, norm.min=norm.min, norm.max=norm.max, data.type=data.type)
    
    predict.frame.forest <- data.frame(predict.intensity.forest, Concentration=spectra.line.table[,"Concentration"])
    predict.frame.forest <- predict.frame.forest[complete.cases(predict.frame.forest$Concentration),]
    
    return(predict.frame.forest)
    
}

predictIntensityForest <- function(predict.frame){
    predict.frame[,!(colnames(predict.frame) %in% "Concentration")]
}

predictIntensityLucPre <- function(spectra, hold.frame, element, intercepts, slopes, norm.type, norm.min=NULL, norm.max=NULL, data.type="Spectra"){
    
    predict.intensity.forest <- predictIntensityForestPre(spectra=spectra, hold.frame=hold.frame, element=element, intercepts=intercepts, norm.type=norm.type, norm.min=norm.min, norm.max=norm.max, data.type=data.type)

    predict.intensity.forest[,c("Intensity", slopes)]
    
}

predictFrameLuc <- function(spectra, hold.frame, element, intercepts, slopes, norm.type, norm.min=NULL, norm.max=NULL, data.type="Spectra"){
    
    data <- spectra
    spectra.line.table <- hold.frame
    
    
    predict.intensity.luc <- predictIntensityLucPre(spectra=spectra, hold.frame=hold.frame, element=element, intercepts=intercepts, slopes=slopes, norm.type=norm.type, norm.min=norm.min, norm.max=norm.max, data.type=data.type)
    
    predict.frame.luc <- data.frame(predict.intensity.luc, spectra.line.table[,"Concentration"])
    predict.frame.luc <- predict.frame.luc[complete.cases(predict.frame.luc),]
    colnames(predict.frame.luc) <- c(names(predict.intensity.luc), "Concentration")
    predict.frame.luc <- predict.frame.luc[complete.cases(predict.frame.luc$Concentration),]
    
    return(predict.frame.luc)
}

predictIntensityLuc <- function(predict.frame){
    predict.frame[,!(colnames(predict.frame) %in% "Concentration")]
}

rainforestIntensityPre <- function(spectra, norm.type, norm.min=NULL, norm.max=NULL, data.type="Spectra"){
    data <- spectra
    
    spectra.data <- if(norm.type==1){
        if(data.type=="Spectra"){
            spectra_simp_prep_xrf(spectra=data)[,-1]
        } else if(data.type=="Net"){
            NULL
        }
    } else if(norm.type==2){
        if(data.type=="Spectra"){
            spectra_tc_prep_xrf(spectra=data)[,-1]
        } else if(data.type=="Net"){
            NULL
        }
    } else if(norm.type==3){
        if(data.type=="Spectra"){
            spectra_comp_prep_xrf(spectra=data, norm.min=norm.min, norm.max=norm.max)[,-1]
        } else if(data.type=="Net"){
            NULL
        }
    }
    
    
    return(spectra.data)
}


rainforestData <- function(spectra, hold.frame, norm.type, norm.min=NULL, norm.max=NULL, data.type="Spectra"){
    
    spectra.line.table <- hold.frame
    
    spectra.data <- rainforestIntensityPre(spectra=spectra, norm.type=norm.type, norm.min=norm.min, norm.max=norm.max, data.type=data.type)
    
    
    
    spectra.data$Concentration <- spectra.line.table[complete.cases(spectra.line.table[,"Concentration"]),]$Concentration
    spectra.data <- spectra.data[complete.cases(spectra.data$Concentration),]
    
    return(spectra.data)
}

rainforestIntensity <- function(rainforest.data){
    rainforest.data[,!(colnames(rainforest.data) %in% "Concentration")]
}


predictFrame <- function(cal.type, spectra){
    if (input$radiocal==1){
        predictFrameSimp()
    } else if(input$radiocal==2){
        predictFrameSimp()
    } else if(input$radiocal==3){
        predictFrameLuc()
    } else if(input$radiocal==4){
        predictFrameForest()
    } else if(input$radiocal==5){
        rainforestData()
    } else if(input$radiocal==6){
        predictFrameForest()
    } else if(input$radiocal==7){
        rainforestData()
    }
}


valFrame <- function(predict.intensity, predict.frame, element.model.list, cal.type){
    element.model <- element.model.list[[2]]
    
    
    if (cal.type==1){
        cal.est.conc.pred <- predict(object=element.model, newdata=predict.intensity, interval='confidence')
        cal.est.conc.tab <- data.frame(cal.est.conc.pred)
        cal.est.conc <- cal.est.conc.tab$fit
        
        val.frame <- data.frame(na.omit(predict.frame$Concentration), cal.est.conc)
        colnames(val.frame) <- c("Concentration", "Prediction")
    }
    
    if (cal.type==2){
        cal.est.conc.pred <- predict(object=element.model, newdata=predict.intensity, interval='confidence')
        cal.est.conc.tab <- data.frame(cal.est.conc.pred)
        cal.est.conc <- cal.est.conc.tab$fit
        
        val.frame <- data.frame(na.omit(predict.frame$Concentration), cal.est.conc)
        colnames(val.frame) <- c("Concentration", "Prediction")
    }
    
    if (cal.type==3){
        
        
        cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity, interval='confidence')
        cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
        cal.est.conc.luc <- cal.est.conc.tab$fit
        cal.est.conc.luc.up <- cal.est.conc.tab$upr
        cal.est.conc.luc.low <- cal.est.conc.tab$lwr
        
        
        val.frame <- data.frame(predict.frame$Concentration, predict.intensity$Intensity, cal.est.conc.luc, cal.est.conc.luc, cal.est.conc.luc.up, cal.est.conc.luc.low)
        colnames(val.frame) <- c("Concentration", "IntensityOrg", "Intensity", "Prediction", "Upper", "Lower")
    }
    
    if (cal.type==4){
        
        
        
        cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity)
        #cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
        #cal.est.conc.luc <- cal.est.conc.tab$fit
        #cal.est.conc.luc.up <- cal.est.conc.tab$upr
        #cal.est.conc.luc.low <- cal.est.conc.tab$lwr
        
        
        val.frame <- data.frame(predict.frame$Concentration, predict.intensity$Intensity, as.vector(cal.est.conc.pred.luc), as.vector(cal.est.conc.pred.luc))
        colnames(val.frame) <- c("Concentration", "IntensityOrg", "Intensity", "Prediction")
    }
    
    
    if (cal.type==5){
        
        
        cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity)
        #cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
        #cal.est.conc.luc <- cal.est.conc.tab$fit
        #cal.est.conc.luc.up <- cal.est.conc.tab$upr
        #cal.est.conc.luc.low <- cal.est.conc.tab$lwr
        
        
        val.frame <- data.frame(predict.frame$Concentration, as.vector(cal.est.conc.pred.luc), as.vector(cal.est.conc.pred.luc))
        colnames(val.frame) <- c("Concentration",  "Intensity", "Prediction")
    }
    
    
    if (cal.type==6){
        
        
        cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity)
        #cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
        #cal.est.conc.luc <- cal.est.conc.tab$fit
        #cal.est.conc.luc.up <- cal.est.conc.tab$upr
        #cal.est.conc.luc.low <- cal.est.conc.tab$lwr
        
        
        val.frame <- data.frame(predict.frame$Concentration, as.vector(cal.est.conc.pred.luc), as.vector(cal.est.conc.pred.luc))
        colnames(val.frame) <- c("Concentration",  "Intensity", "Prediction")
    }
    
    
    if (cal.type==7){
        
        
        cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity)
        #cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
        #cal.est.conc.luc <- cal.est.conc.tab$fit
        #cal.est.conc.luc.up <- cal.est.conc.tab$upr
        #cal.est.conc.luc.low <- cal.est.conc.tab$lwr
        
        
        val.frame <- data.frame(predict.frame$Concentration, as.vector(cal.est.conc.pred.luc), as.vector(cal.est.conc.pred.luc))
        colnames(val.frame) <- c("Concentration",  "Intensity", "Prediction")
    }
    
    
    
    
    return(val.frame)
}

calCurvePlot <- function(predict.frame, element.model.list, val.frame, element, cal.type, unit="%"){
    
    element.name <- if(element %in% spectralLines){
        gsub("[.]", "", substr(element, 1, 2))
    } else {
        element
    }
    
    intens <- " Counts per Second"
    norma <- " Normalized"
    norma.comp <- " Compton Normalized"
    norma.tc <- " Valid Counts Normalized"
    conen <- paste0(" ", unit)
    predi <- paste0(" Estimate ", unit)
    log <- "Log "
    
    
    intensity.name <- c(element.name, intens)
    concentration.name <- c(element.name, conen)
    prediction.name <- c(element.name, predi)
    
    use.standards <- element.model.list[[1]]$StandardsUsed
    element.model <- element.model.list[[2]]
    
    
    
    
    if(cal.type==1){
        calcurve.plot <- ggplot(data=predict.frame[use.standards, , drop = FALSE], aes(Intensity, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn(lm(Concentration~Intensity, predict.frame[use.standards, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        stat_smooth(method="lm", fullrange = TRUE) +
        geom_point() +
        geom_point(data = predict.frame[!use.standards, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
        scale_x_continuous(paste(element.name, intens), breaks=scales::pretty_breaks()) +
        scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
        coord_cartesian(expand = TRUE)
        
    }
    
    if(cal.type==2){
        calcurve.plot <- ggplot(data=predict.frame[use.standards, , drop = FALSE], aes(Intensity, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn_poly(lm(Concentration~Intensity + I(Intensity^2), predict.frame[use.standards, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        stat_smooth(method="lm", formula=y~poly(x,2)) +
        geom_point() +
        geom_point(data = predict.frame[!use.standards, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
        scale_x_continuous(paste(element.name, intens), breaks=scales::pretty_breaks()) +
        scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
        coord_cartesian(expand = TRUE)
        
    }
    
    if(cal.type==3){
        calcurve.plot <- ggplot(data=val.frame[use.standards, , drop = FALSE], aes(Intensity, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn(lm(Concentration~., val.frame[use.standards, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        geom_smooth(aes(x=Intensity, y=Concentration, ymin = Lower, ymax = Upper)) +
        geom_point() +
        geom_point(aes(Intensity, Concentration), data = val.frame[!use.standards, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
        scale_x_continuous(paste(element.name, norma), breaks=scales::pretty_breaks()) +
        scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
        coord_cartesian(expand = TRUE)
        
    }
    
    if(cal.type==4){
        calcurve.plot <- ggplot(data=val.frame[use.standards, , drop = FALSE], aes(Intensity, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn(lm(Concentration~., val.frame[use.standards, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        geom_smooth() +
        geom_point() +
        geom_point(aes(Intensity, Concentration), data = val.frame[!use.standards, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
        scale_x_continuous(paste(element.name, norma), breaks=scales::pretty_breaks()) +
        scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
        coord_cartesian(expand = TRUE)
        
    }
    
    if(cal.type==5){
        calcurve.plot <- ggplot(data=val.frame[use.standards, , drop = FALSE], aes(Intensity, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn(lm(Concentration~., val.frame[use.standards, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        geom_smooth() +
        geom_point() +
        geom_point(aes(Intensity, Concentration), data = val.frame[!use.standards, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
        scale_x_continuous(paste(element.name, norma), breaks=scales::pretty_breaks()) +
        scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
        coord_cartesian(expand = TRUE)
        
    }
    
    if(cal.type==6){
        
        calcurve.plot <- grobTree(plot.nnet(element.model,nid=T))
        
    }
    
    if(cal.type==7){
        
        calcurve.plot <- grobTree(plot.nnet(element.model,nid=T))
        
        
    }
    
    return(calcurve.plot)
}

valCurvePlot <- function(val.frame, element, element.model.list, unit){
    element.model <- elementModel()
    
    
    
    element.name <- if(element %in% spectralLines){
        gsub("[.]", "", substr(element, 1, 2))
    } else {
        element
    }
    
    intens <- " Counts per Second"
    norma <- " Normalized"
    norma.comp <- " Compton Normalized"
    norma.tc <- " Valid Counts Normalized"
    conen <- paste0(" ", unit)
    predi <- paste0(" Estimate ", unit)
    log <- "Log "
    
    intensity.name <- c(element.name, intens)
    concentration.name <- c(element.name, conen)
    prediction.name <- c(element.name, predi)
    val.frame <- valFrame()
    
    use.standards <- element.model.list[[1]]$StandardsUsed


    valcurve.plot <- ggplot(data=val.frame[use.standards, , drop = FALSE], aes(Prediction, Concentration)) +
    theme_bw() +
    annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame[use.standards, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
    geom_abline(intercept=0, slope=1, lty=2) +
    stat_smooth(method="lm") +
    geom_point() +
    geom_point(aes(Prediction, Concentration),  data = val.frame[!use.standards, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
    scale_x_continuous(paste(element.name, predi), breaks=scales::pretty_breaks()) +
    scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
    coord_cartesian(expand = TRUE)
    

    return(valcurve.plot)
}

modelSummary <- function(element.model, element.name){
    
    model.class <- if(element.model[[1]][["CalTable"]]$CalType==1){
        "Regression"
    } else if(element.model[[1]][["CalTable"]]$CalType==2){
        "Regression"
    } else if(element.model[[1]][["CalTable"]]$CalType==3){
        "Regression"
    } else if(element.model[[1]][["CalTable"]]$CalType==4){
        "Caret"
    } else if(element.model[[1]][["CalTable"]]$CalType==5){
        "Caret"
    } else if(element.model[[1]][["CalTable"]]$CalType==6){
        "Caret"
    } else if(element.model[[1]][["CalTable"]]$CalType==7){
        "Caret"
    }
    
    r2 <- if(model.class=="Regression"){
        summary(element.model[[2]])$r.squared
    } else if(model.class=="Caret"){
        element.model[[2]][["results"]]$Rsquared
    }
    
    data.frame(Element=element.name, R2=round(r2, 2))
}

calProgressSummary <- function(calList){
    element.names <- names(calList)
    
    cal.results.list <- lapply(element.names, function(x) modelSummary(element.model=calList[[x]], element.name=x))
    
    rbindlist(cal.results.list)
}
