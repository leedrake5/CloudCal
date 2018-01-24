list.of.packages <- c("pbapply", "reshape2", "TTR", "dplyr", "ggtern", "ggplot2", "shiny", "rhandsontable", "random", "data.table", "DT", "shinythemes", "Cairo", "broom", "shinyjs", "gridExtra", "dtplyr", "formattable", "XML")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)



library(shiny)
library(ggplot2)
library(pbapply)
library(reshape2)
library(dplyr)
library(DT)
library(XML)




options(digits=4)
options(warn=-1)
assign("last.warning", NULL, envir = baseenv())

Hodder.v <- function(y)
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



read_csv_filename_x <- function(filename){
    ret <- read.csv(file=filename, sep=",", header=FALSE)
    return.res <- as.numeric(as.vector(ret$V2[18]))/1000
    return.chan.counts <-as.numeric(as.vector(ret$V1[22:2069]))
    return.energy <- return.chan.counts*return.res
    return(return.energy)
}

read_csv_filename_y <- function(filename){
    ret <- read.csv(file=filename, sep=",", header=FALSE)
    return.live.time <- as.numeric(as.vector(ret$V2[10]))
    return.counts <- as.numeric(as.vector(ret$V2[22:2069]))
    return.cps <- return.counts/return.live.time
    return(return.cps)
}


read_csv_net <- function(filepath) {
    
    ret <- read.csv(file=filepath, sep=",", header=TRUE)
    element <- ret$Element
    line <- ret$Line
    net <- ret$Net
    background <- ret$Backgr.
    eline <- paste(element, line, sep="-")
    
    simple.table <- data.frame(net)
    colnames(simple.table) <- NULL
    simple.transpose <- as.data.frame(t(simple.table))
    colnames(simple.transpose) <- eline
    
    simple.transpose
    
}

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
    newdata <- as.data.frame(seq(1, 4096, 1))
    colnames(newdata) <- "channels"
    energy <- as.vector(predict.lm(energy.cal, newdata=newdata))
    energy2 <- newdata[,1]*summary(energy.cal)$coef[2]
    spectra.frame <- data.frame(energy, cps, filename.vector)
    colnames(spectra.frame) <- c("Energy", "CPS", "Spectrum")
    return(spectra.frame)
}


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
    newdata <- as.data.frame(seq(1, 4096, 1))
    colnames(newdata) <- "channels"
    energy <- as.vector(predict.lm(energy.cal, newdata=newdata))
    energy2 <- newdata[,1]*summary(energy.cal)$coef[2]
    spectra.frame <- data.frame(energy, cps, filename.vector)
    colnames(spectra.frame) <- c("Energy", "CPS", "Spectrum")
    return(spectra.frame)
}


readSPXData <- function(filepath, filename){
    
    filename <- gsub(".spx", "", filename)
    filename.vector <- rep(filename, 4096)
    
    xmlfile <- xmlTreeParse(filepath)
    xmllist <- xmlToList(xmlfile)
    channels.pre <- xmllist[["ClassInstance"]][["Channels"]][[1]]
    counts <- as.numeric(strsplit(channels.pre, ",", )[[1]])
    newdata <- as.data.frame(seq(1, 4096, 1))
    intercept <- as.numeric(xmllist[["ClassInstance"]][["ClassInstance"]][["CalibAbs"]])
    slope <- as.numeric(xmllist[["ClassInstance"]][["ClassInstance"]][["CalibLin"]])
    time <- as.numeric(xmllist[[2]][["TRTHeaderedClass"]][[3]][["LifeTime"]])/1000
    
    cps <- counts/time
    energy <- newdata[,1]*slope+intercept
    
    spectra.frame <- data.frame(energy, cps, filename.vector)
    colnames(spectra.frame) <- c("Energy", "CPS", "Spectrum")
    return(spectra.frame)
    
}


file.0 <- function(file) {
    if (length(file) > 0)
    {
    return(file)
    }else{
        return(levels(file))
    }
}

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

lmp <- function (modelobject) {
    if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
    f <- summary(modelobject)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
}

lm_eqn.old <- function(df){
    m <- lm(y ~ x, df);
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
    list(a = format(coef(m)[1], digits = 2),
    b = format(coef(m)[2], digits = 2),
    r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));
}

lm_eqn = function(m) {
    
    l <- list(a = format(coef(m)[1], digits = 2),
    b = format(abs(coef(m)[2]), digits = 2),
    r2 = format(summary(m)$r.squared, digits = 3));
    
        eq <- substitute(italic(C)[i] == a + b %.% italic(I)[i]*","~~italic(r)^2~"="~r2,l)
  
    
    as.character(as.expression(eq));
}

lm_eqn_poly = function(m) {
    
    l <- list(a = format(coef(m)[1], digits = 2),
    b = format(abs(coef(m)[2]), digits = 2),
    c = format(abs(coef(m)[3]), digits = 2),
    r2 = format(summary(m)$r.squared, digits = 3));
    
        eq <- substitute(italic(C)[i] == a + c %.% italic(I)[i]^2 + b %.% italic(I)[i]*","~~italic(r)^2~"="~r2,l)
   
    
    as.character(as.expression(eq));
}

lm_eqn_val = function(m) {
    
    l <- list(a = format(coef(m)[1], digits = 2),
    b = format(abs(coef(m)[2]), digits = 2),
    r2 = format(summary(m)$r.squared, digits = 3));
    
        eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
   
    
    as.character(as.expression(eq));
}

numericInput2<-function (inputId, label, value = "",...)
{
    div(style="display:inline-block",
    tags$label(label, `for` = inputId),
    tags$input(id = inputId, type = "text", value = value,...))
}

numericInputRow<-function (inputId, label, min, max,  value = "")
{
    div(style="display:inline-block",
    tags$label(label, `for` = inputId),
    tags$input(id = inputId, type = "text", value = value, class="input-mini", width='20%'))
}



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

merge_Sum <- function(.df1, .df2, .id_Columns, .match_Columns){
    merged_Columns <- unique(c(names(.df1),names(.df2)))
    merged_df1 <- data.frame(matrix(nrow=nrow(.df1), ncol=length(merged_Columns)))
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


black.diamond <- read.csv("data/blackdiamond.csv", header=FALSE, sep=",")
black.diamond.melt <- read.csv(file="data/blackdiamondmelt.csv")

######Load lines
k.lines <- read.csv(file="data/K Line-Table 1.csv", sep=",")
l.lines <- read.csv(file="data/L Line-Table 1.csv", sep=",")
fluorescence.lines <- read.csv("data/FluorescenceLines.csv")


#k.lines[k.lines < 0.01] <- 1
#l.lines[l.lines < 0.01] <- 1

lines <- data.frame(k.lines, l.lines)

H.lines <- data.frame(lines$Ka1[1], lines$Ka2[1], lines$Kb1[1], lines$Kb2[1], lines$Kb3[1], lines$La1[1], lines$La2[1], lines$Lb1[1], lines$Lb2[1], lines$Lb3[1], lines$Lb4[1], lines$Lg1[1], lines$Lg2[1], lines$Lg3[1], lines$Ll[1])
colnames(H.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

He.lines <- data.frame(lines$Ka1[2], lines$Ka2[2], lines$Kb1[2], lines$Kb2[2], lines$Kb3[2], lines$La1[2], lines$La2[2], lines$Lb1[2], lines$Lb2[2], lines$Lb3[2], lines$Lb4[2], lines$Lg1[2], lines$Lg2[2], lines$Lg3[2], lines$Ll[2])
colnames(He.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Li.lines <- data.frame(lines$Ka1[3], lines$Ka2[3], lines$Kb1[3], lines$Kb2[3], lines$Kb3[3], lines$La1[3], lines$La2[3], lines$Lb1[3], lines$Lb2[3], lines$Lb3[3], lines$Lb4[3], lines$Lg1[3], lines$Lg2[3], lines$Lg3[3], lines$Ll[3])
colnames(Li.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Be.lines <- data.frame(lines$Ka1[4], lines$Ka2[4], lines$Kb1[4], lines$Kb2[4], lines$Kb3[4], lines$La1[4], lines$La2[4], lines$Lb1[4], lines$Lb2[4], lines$Lb3[4], lines$Lb4[4], lines$Lg1[4], lines$Lg2[4], lines$Lg3[4], lines$Ll[4])
colnames(Be.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

B.lines <- data.frame(lines$Ka1[5], lines$Ka2[5], lines$Kb1[5], lines$Kb2[5], lines$Kb3[5], lines$La1[5], lines$La2[5], lines$Lb1[5], lines$Lb2[5], lines$Lb3[5], lines$Lb4[5], lines$Lg1[5], lines$Lg2[5], lines$Lg3[5], lines$Ll[5])
colnames(B.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

C.lines <- data.frame(lines$Ka1[6], lines$Ka2[6], lines$Kb1[6], lines$Kb2[6], lines$Kb3[6], lines$La1[6], lines$La2[6], lines$Lb1[6], lines$Lb2[6], lines$Lb3[6], lines$Lb4[6], lines$Lg1[6], lines$Lg2[6], lines$Lg3[6], lines$Ll[6])
colnames(C.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

N.lines <- data.frame(lines$Ka1[7], lines$Ka2[7], lines$Kb1[7], lines$Kb2[7], lines$Kb3[7], lines$La1[7], lines$La2[7], lines$Lb1[7], lines$Lb2[7], lines$Lb3[7], lines$Lb4[7], lines$Lg1[7], lines$Lg2[7], lines$Lg3[7], lines$Ll[7])
colnames(N.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

O.lines <- data.frame(lines$Ka1[8], lines$Ka2[8], lines$Kb1[8], lines$Kb2[8], lines$Kb3[8], lines$La1[8], lines$La2[8], lines$Lb1[8], lines$Lb2[8], lines$Lb3[8], lines$Lb4[8], lines$Lg1[8], lines$Lg2[8], lines$Lg3[8], lines$Ll[8])
colnames(O.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

F.lines <- data.frame(lines$Ka1[9], lines$Ka2[9], lines$Kb1[9], lines$Kb2[9], lines$Kb3[9], lines$La1[9], lines$La2[9], lines$Lb1[9], lines$Lb2[9], lines$Lb3[9], lines$Lb4[9], lines$Lg1[9], lines$Lg2[9], lines$Lg3[9], lines$Ll[9])
colnames(F.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ne.lines <- data.frame(lines$Ka1[10], lines$Ka2[10], lines$Kb1[10], lines$Kb2[10], lines$Kb3[10], lines$La1[10], lines$La2[10], lines$Lb1[10], lines$Lb2[10], lines$Lb3[10], lines$Lb4[10], lines$Lg1[10], lines$Lg2[10], lines$Lg3[10], lines$Ll[10])
colnames(Ne.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Na.lines <- data.frame(lines$Ka1[11], lines$Ka2[11], lines$Kb1[11], lines$Kb2[11], lines$Kb3[11], lines$La1[11], lines$La2[11], lines$Lb1[11], lines$Lb2[11], lines$Lb3[11], lines$Lb4[11], lines$Lg1[11], lines$Lg2[11], lines$Lg3[11], lines$Ll[11])
colnames(Na.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Mg.lines <- data.frame(lines$Ka1[12], lines$Ka2[12], lines$Kb1[12], lines$Kb2[12], lines$Kb3[12], lines$La1[12], lines$La2[12], lines$Lb1[12], lines$Lb2[12], lines$Lb3[12], lines$Lb4[12], lines$Lg1[12], lines$Lg2[12], lines$Lg3[12], lines$Ll[12])
colnames(Mg.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Al.lines <- data.frame(lines$Ka1[13], lines$Ka2[13], lines$Kb1[13], lines$Kb2[13], lines$Kb3[13], lines$La1[13], lines$La2[13], lines$Lb1[13], lines$Lb2[13], lines$Lb3[13], lines$Lb4[13], lines$Lg1[13], lines$Lg2[13], lines$Lg3[13], lines$Ll[13])
colnames(Al.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Si.lines <- data.frame(lines$Ka1[14], lines$Ka2[14], lines$Kb1[14], lines$Kb2[14], lines$Kb3[14], lines$La1[14], lines$La2[14], lines$Lb1[14], lines$Lb2[14], lines$Lb3[14], lines$Lb4[14], lines$Lg1[14], lines$Lg2[14], lines$Lg3[14], lines$Ll[14])
colnames(Si.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

P.lines <- data.frame(lines$Ka1[15], lines$Ka2[15], lines$Kb1[15], lines$Kb2[15], lines$Kb3[15], lines$La1[15], lines$La2[15], lines$Lb1[15], lines$Lb2[15], lines$Lb3[15], lines$Lb4[15], lines$Lg1[15], lines$Lg2[15], lines$Lg3[15], lines$Ll[15])
colnames(P.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

S.lines <- data.frame(lines$Ka1[16], lines$Ka2[16], lines$Kb1[16], lines$Kb2[16], lines$Kb3[16], lines$La1[16], lines$La2[16], lines$Lb1[16], lines$Lb2[16], lines$Lb3[16], lines$Lb4[16], lines$Lg1[16], lines$Lg2[16], lines$Lg3[16], lines$Ll[16])
colnames(S.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Cl.lines <- data.frame(lines$Ka1[17], lines$Ka2[17], lines$Kb1[17], lines$Kb2[17], lines$Kb3[17], lines$La1[17], lines$La2[17], lines$Lb1[17], lines$Lb2[17], lines$Lb3[17], lines$Lb4[17], lines$Lg1[17], lines$Lg2[17], lines$Lg3[17], lines$Ll[17])
colnames(Cl.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ar.lines <- data.frame(lines$Ka1[18], lines$Ka2[18], lines$Kb1[18], lines$Kb2[18], lines$Kb3[18], lines$La1[18], lines$La2[18], lines$Lb1[18], lines$Lb2[18], lines$Lb3[18], lines$Lb4[18], lines$Lg1[18], lines$Lg2[18], lines$Lg3[18], lines$Ll[18])
colnames(Ar.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

K.lines <- data.frame(lines$Ka1[19], lines$Ka2[19], lines$Kb1[19], lines$Kb2[19], lines$Kb3[19], lines$La1[19], lines$La2[19], lines$Lb1[19], lines$Lb2[19], lines$Lb3[19], lines$Lb4[19], lines$Lg1[19], lines$Lg2[19], lines$Lg3[19], lines$Ll[19])
colnames(K.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ca.lines <- data.frame(lines$Ka1[20], lines$Ka2[20], lines$Kb1[20], lines$Kb2[20], lines$Kb3[20], lines$La1[20], lines$La2[20], lines$Lb1[20], lines$Lb2[20], lines$Lb3[20], lines$Lb4[20], lines$Lg1[20], lines$Lg2[20], lines$Lg3[20], lines$Ll[20])
colnames(Ca.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Sc.lines <- data.frame(lines$Ka1[21], lines$Ka2[21], lines$Kb1[21], lines$Kb2[21], lines$Kb3[21], lines$La1[21], lines$La2[21], lines$Lb1[21], lines$Lb2[21], lines$Lb3[21], lines$Lb4[21], lines$Lg1[21], lines$Lg2[21], lines$Lg3[21], lines$Ll[21])
colnames(Sc.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ti.lines <- data.frame(lines$Ka1[22], lines$Ka2[22], lines$Kb1[22], lines$Kb2[22], lines$Kb3[22], lines$La1[22], lines$La2[22], lines$Lb1[22], lines$Lb2[22], lines$Lb3[22], lines$Lb4[22], lines$Lg1[22], lines$Lg2[22], lines$Lg3[22], lines$Ll[22])
colnames(Ti.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

V.lines <- data.frame(lines$Ka1[23], lines$Ka2[23], lines$Kb1[23], lines$Kb2[23], lines$Kb3[23], lines$La1[23], lines$La2[23], lines$Lb1[23], lines$Lb2[23], lines$Lb3[23], lines$Lb4[23], lines$Lg1[23], lines$Lg2[23], lines$Lg3[23], lines$Ll[23])
colnames(V.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Cr.lines <- data.frame(lines$Ka1[24], lines$Ka2[24], lines$Kb1[24], lines$Kb2[24], lines$Kb3[24], lines$La1[24], lines$La2[24], lines$Lb1[24], lines$Lb2[24], lines$Lb3[24], lines$Lb4[24], lines$Lg1[24], lines$Lg2[24], lines$Lg3[24], lines$Ll[24])
colnames(Cr.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Mn.lines <- data.frame(lines$Ka1[25], lines$Ka2[25], lines$Kb1[25], lines$Kb2[25], lines$Kb3[25], lines$La1[25], lines$La2[25], lines$Lb1[25], lines$Lb2[25], lines$Lb3[25], lines$Lb4[25], lines$Lg1[25], lines$Lg2[25], lines$Lg3[25], lines$Ll[25])
colnames(Mn.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Fe.lines <- data.frame(lines$Ka1[26], lines$Ka2[26], lines$Kb1[26], lines$Kb2[26], lines$Kb3[26], lines$La1[26], lines$La2[26], lines$Lb1[26], lines$Lb2[26], lines$Lb3[26], lines$Lb4[26], lines$Lg1[26], lines$Lg2[26], lines$Lg3[26], lines$Ll[26])
colnames(Fe.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Co.lines <- data.frame(lines$Ka1[27], lines$Ka2[27], lines$Kb1[27], lines$Kb2[27], lines$Kb3[27], lines$La1[27], lines$La2[27], lines$Lb1[27], lines$Lb2[27], lines$Lb3[27], lines$Lb4[27], lines$Lg1[27], lines$Lg2[27], lines$Lg3[27], lines$Ll[27])
colnames(Co.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ni.lines <- data.frame(lines$Ka1[28], lines$Ka2[28], lines$Kb1[28], lines$Kb2[28], lines$Kb3[28], lines$La1[28], lines$La2[28], lines$Lb1[28], lines$Lb2[28], lines$Lb3[28], lines$Lb4[28], lines$Lg1[28], lines$Lg2[28], lines$Lg3[28], lines$Ll[28])
colnames(Ni.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Cu.lines <- data.frame(lines$Ka1[29], lines$Ka2[29], lines$Kb1[29], lines$Kb2[29], lines$Kb3[29], lines$La1[29], lines$La2[29], lines$Lb1[29], lines$Lb2[29], lines$Lb3[29], lines$Lb4[29], lines$Lg1[29], lines$Lg2[29], lines$Lg3[29], lines$Ll[29])
colnames(Cu.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Zn.lines <- data.frame(lines$Ka1[30], lines$Ka2[30], lines$Kb1[30], lines$Kb2[30], lines$Kb3[30], lines$La1[30], lines$La2[30], lines$Lb1[30], lines$Lb2[30], lines$Lb3[30], lines$Lb4[30], lines$Lg1[30], lines$Lg2[30], lines$Lg3[30], lines$Ll[30])
colnames(Zn.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ga.lines <- data.frame(lines$Ka1[31], lines$Ka2[31], lines$Kb1[31], lines$Kb2[31], lines$Kb3[31], lines$La1[31], lines$La2[31], lines$Lb1[31], lines$Lb2[31], lines$Lb3[31], lines$Lb4[31], lines$Lg1[31], lines$Lg2[31], lines$Lg3[31], lines$Ll[31])
colnames(Ga.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ge.lines <- data.frame(lines$Ka1[32], lines$Ka2[32], lines$Kb1[32], lines$Kb2[32], lines$Kb3[32], lines$La1[32], lines$La2[32], lines$Lb1[32], lines$Lb2[32], lines$Lb3[32], lines$Lb4[32], lines$Lg1[32], lines$Lg2[32], lines$Lg3[32], lines$Ll[32])
colnames(Ge.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

As.lines <- data.frame(lines$Ka1[33], lines$Ka2[33], lines$Kb1[33], lines$Kb2[33], lines$Kb3[33], lines$La1[33], lines$La2[33], lines$Lb1[33], lines$Lb2[33], lines$Lb3[33], lines$Lb4[33], lines$Lg1[33], lines$Lg2[33], lines$Lg3[33], lines$Ll[33])
colnames(As.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Se.lines <- data.frame(lines$Ka1[34], lines$Ka2[34], lines$Kb1[34], lines$Kb2[34], lines$Kb3[34], lines$La1[34], lines$La2[34], lines$Lb1[34], lines$Lb2[34], lines$Lb3[34], lines$Lb4[34], lines$Lg1[34], lines$Lg2[34], lines$Lg3[34], lines$Ll[34])
colnames(Se.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Br.lines <- data.frame(lines$Ka1[35], lines$Ka2[35], lines$Kb1[35], lines$Kb2[35], lines$Kb3[35], lines$La1[35], lines$La2[35], lines$Lb1[35], lines$Lb2[35], lines$Lb3[35], lines$Lb4[35], lines$Lg1[35], lines$Lg2[35], lines$Lg3[35], lines$Ll[35])
colnames(Br.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Kr.lines <- data.frame(lines$Ka1[36], lines$Ka2[36], lines$Kb1[36], lines$Kb2[36], lines$Kb3[36], lines$La1[36], lines$La2[36], lines$Lb1[36], lines$Lb2[36], lines$Lb3[36], lines$Lb4[36], lines$Lg1[36], lines$Lg2[36], lines$Lg3[36], lines$Ll[36])
colnames(Kr.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Rb.lines <- data.frame(lines$Ka1[37], lines$Ka2[37], lines$Kb1[37], lines$Kb2[37], lines$Kb3[37], lines$La1[37], lines$La2[37], lines$Lb1[37], lines$Lb2[37], lines$Lb3[37], lines$Lb4[37], lines$Lg1[37], lines$Lg2[37], lines$Lg3[37], lines$Ll[37])
colnames(Rb.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Sr.lines <- data.frame(lines$Ka1[38], lines$Ka2[38], lines$Kb1[38], lines$Kb2[38], lines$Kb3[38], lines$La1[38], lines$La2[38], lines$Lb1[38], lines$Lb2[38], lines$Lb3[38], lines$Lb4[38], lines$Lg1[38], lines$Lg2[38], lines$Lg3[38], lines$Ll[38])
colnames(Sr.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Y.lines <- data.frame(lines$Ka1[39], lines$Ka2[39], lines$Kb1[39], lines$Kb2[39], lines$Kb3[39], lines$La1[39], lines$La2[39], lines$Lb1[39], lines$Lb2[39], lines$Lb3[39], lines$Lb4[39], lines$Lg1[39], lines$Lg2[39], lines$Lg3[39], lines$Ll[39])
colnames(Y.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Zr.lines <- data.frame(lines$Ka1[40], lines$Ka2[40], lines$Kb1[40], lines$Kb2[40], lines$Kb3[40], lines$La1[40], lines$La2[40], lines$Lb1[40], lines$Lb2[40], lines$Lb3[40], lines$Lb4[40], lines$Lg1[40], lines$Lg2[40], lines$Lg3[40], lines$Ll[40])
colnames(Zr.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Nb.lines <- data.frame(lines$Ka1[41], lines$Ka2[41], lines$Kb1[41], lines$Kb2[41], lines$Kb3[41], lines$La1[41], lines$La2[41], lines$Lb1[41], lines$Lb2[41], lines$Lb3[41], lines$Lb4[41], lines$Lg1[41], lines$Lg2[41], lines$Lg3[41], lines$Ll[41])
colnames(Nb.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Mo.lines <- data.frame(lines$Ka1[42], lines$Ka2[42], lines$Kb1[42], lines$Kb2[42], lines$Kb3[42], lines$La1[42], lines$La2[42], lines$Lb1[42], lines$Lb2[42], lines$Lb3[42], lines$Lb4[42], lines$Lg1[42], lines$Lg2[42], lines$Lg3[42], lines$Ll[42])
colnames(Mo.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Tc.lines <- data.frame(lines$Ka1[43], lines$Ka2[43], lines$Kb1[43], lines$Kb2[43], lines$Kb3[43], lines$La1[43], lines$La2[43], lines$Lb1[43], lines$Lb2[43], lines$Lb3[43], lines$Lb4[43], lines$Lg1[43], lines$Lg2[43], lines$Lg3[43], lines$Ll[43])
colnames(Tc.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ru.lines <- data.frame(lines$Ka1[44], lines$Ka2[44], lines$Kb1[44], lines$Kb2[44], lines$Kb3[44], lines$La1[44], lines$La2[44], lines$Lb1[44], lines$Lb2[44], lines$Lb3[44], lines$Lb4[44], lines$Lg1[44], lines$Lg2[44], lines$Lg3[44], lines$Ll[44])
colnames(Ru.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Rh.lines <- data.frame(lines$Ka1[45], lines$Ka2[45], lines$Kb1[45], lines$Kb2[45], lines$Kb3[45], lines$La1[45], lines$La2[45], lines$Lb1[45], lines$Lb2[45], lines$Lb3[45], lines$Lb4[45], lines$Lg1[45], lines$Lg2[45], lines$Lg3[45], lines$Ll[45])
colnames(Rh.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Pd.lines <- data.frame(lines$Ka1[46], lines$Ka2[46], lines$Kb1[46], lines$Kb2[46], lines$Kb3[46], lines$La1[46], lines$La2[46], lines$Lb1[46], lines$Lb2[46], lines$Lb3[46], lines$Lb4[46], lines$Lg1[46], lines$Lg2[46], lines$Lg3[46], lines$Ll[46])
colnames(Pd.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ag.lines <- data.frame(lines$Ka1[47], lines$Ka2[47], lines$Kb1[47], lines$Kb2[47], lines$Kb3[47], lines$La1[47], lines$La2[47], lines$Lb1[47], lines$Lb2[47], lines$Lb3[47], lines$Lb4[47], lines$Lg1[47], lines$Lg2[47], lines$Lg3[47], lines$Ll[47])
colnames(Ag.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Cd.lines <- data.frame(lines$Ka1[48], lines$Ka2[48], lines$Kb1[48], lines$Kb2[48], lines$Kb3[48], lines$La1[48], lines$La2[48], lines$Lb1[48], lines$Lb2[48], lines$Lb3[48], lines$Lb4[48], lines$Lg1[48], lines$Lg2[48], lines$Lg3[48], lines$Ll[48])
colnames(Cd.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

In.lines <- data.frame(lines$Ka1[49], lines$Ka2[49], lines$Kb1[49], lines$Kb2[49], lines$Kb3[49], lines$La1[49], lines$La2[49], lines$Lb1[49], lines$Lb2[49], lines$Lb3[49], lines$Lb4[49], lines$Lg1[49], lines$Lg2[49], lines$Lg3[49], lines$Ll[49])
colnames(In.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Sn.lines <- data.frame(lines$Ka1[50], lines$Ka2[50], lines$Kb1[50], lines$Kb2[50], lines$Kb3[50], lines$La1[50], lines$La2[50], lines$Lb1[50], lines$Lb2[50], lines$Lb3[50], lines$Lb4[50], lines$Lg1[50], lines$Lg2[50], lines$Lg3[50], lines$Ll[50])
colnames(Sn.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Sb.lines <- data.frame(lines$Ka1[51], lines$Ka2[51], lines$Kb1[51], lines$Kb2[51], lines$Kb3[51], lines$La1[51], lines$La2[51], lines$Lb1[51], lines$Lb2[51], lines$Lb3[51], lines$Lb4[51], lines$Lg1[51], lines$Lg2[51], lines$Lg3[51], lines$Ll[51])
colnames(Sb.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Te.lines <- data.frame(lines$Ka1[52], lines$Ka2[52], lines$Kb1[52], lines$Kb2[52], lines$Kb3[52], lines$La1[52], lines$La2[52], lines$Lb1[52], lines$Lb2[52], lines$Lb3[52], lines$Lb4[52], lines$Lg1[52], lines$Lg2[52], lines$Lg3[52], lines$Ll[52])
colnames(Te.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

I.lines <- data.frame(lines$Ka1[53], lines$Ka2[53], lines$Kb1[53], lines$Kb2[53], lines$Kb3[53], lines$La1[53], lines$La2[53], lines$Lb1[53], lines$Lb2[53], lines$Lb3[53], lines$Lb4[53], lines$Lg1[53], lines$Lg2[53], lines$Lg3[53], lines$Ll[53])
colnames(I.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Xe.lines <- data.frame(lines$Ka1[54], lines$Ka2[54], lines$Kb1[54], lines$Kb2[54], lines$Kb3[54], lines$La1[54], lines$La2[54], lines$Lb1[54], lines$Lb2[54], lines$Lb3[54], lines$Lb4[54], lines$Lg1[54], lines$Lg2[54], lines$Lg3[54], lines$Ll[54])
colnames(Xe.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Cs.lines <- data.frame(lines$Ka1[55], lines$Ka2[55], lines$Kb1[55], lines$Kb2[55], lines$Kb3[55], lines$La1[55], lines$La2[55], lines$Lb1[55], lines$Lb2[55], lines$Lb3[55], lines$Lb4[55], lines$Lg1[55], lines$Lg2[55], lines$Lg3[55], lines$Ll[55])
colnames(Cs.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ba.lines <- data.frame(lines$Ka1[56], lines$Ka2[56], lines$Kb1[56], lines$Kb2[56], lines$Kb3[56], lines$La1[56], lines$La2[56], lines$Lb1[56], lines$Lb2[56], lines$Lb3[56], lines$Lb4[56], lines$Lg1[56], lines$Lg2[56], lines$Lg3[56], lines$Ll[56])
colnames(Ba.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

La.lines <- data.frame(lines$Ka1[57], lines$Ka2[57], lines$Kb1[57], lines$Kb2[57], lines$Kb3[57], lines$La1[57], lines$La2[57], lines$Lb1[57], lines$Lb2[57], lines$Lb3[57], lines$Lb4[57], lines$Lg1[57], lines$Lg2[57], lines$Lg3[57], lines$Ll[57])
colnames(La.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ce.lines <- data.frame(lines$Ka1[58], lines$Ka2[58], lines$Kb1[58], lines$Kb2[58], lines$Kb3[58], lines$La1[58], lines$La2[58], lines$Lb1[58], lines$Lb2[58], lines$Lb3[58], lines$Lb4[58], lines$Lg1[58], lines$Lg2[58], lines$Lg3[58], lines$Ll[58])
colnames(Ce.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Pr.lines <- data.frame(lines$Ka1[59], lines$Ka2[59], lines$Kb1[59], lines$Kb2[59], lines$Kb3[59], lines$La1[59], lines$La2[59], lines$Lb1[59], lines$Lb2[59], lines$Lb3[59], lines$Lb4[59], lines$Lg1[59], lines$Lg2[59], lines$Lg3[59], lines$Ll[59])
colnames(Pr.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Nd.lines <- data.frame(lines$Ka1[60], lines$Ka2[60], lines$Kb1[60], lines$Kb2[60], lines$Kb3[60], lines$La1[60], lines$La2[60], lines$Lb1[60], lines$Lb2[60], lines$Lb3[60], lines$Lb4[60], lines$Lg1[60], lines$Lg2[60], lines$Lg3[60], lines$Ll[60])
colnames(Nd.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Pm.lines <- data.frame(lines$Ka1[61], lines$Ka2[61], lines$Kb1[61], lines$Kb2[61], lines$Kb3[61], lines$La1[61], lines$La2[61], lines$Lb1[61], lines$Lb2[61], lines$Lb3[61], lines$Lb4[61], lines$Lg1[61], lines$Lg2[61], lines$Lg3[61], lines$Ll[61])
colnames(Pm.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Sm.lines <- data.frame(lines$Ka1[62], lines$Ka2[62], lines$Kb1[62], lines$Kb2[62], lines$Kb3[62], lines$La1[62], lines$La2[62], lines$Lb1[62], lines$Lb2[62], lines$Lb3[62], lines$Lb4[62], lines$Lg1[62], lines$Lg2[62], lines$Lg3[62], lines$Ll[62])
colnames(Sm.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Eu.lines <- data.frame(lines$Ka1[63], lines$Ka2[63], lines$Kb1[63], lines$Kb2[63], lines$Kb3[63], lines$La1[63], lines$La2[63], lines$Lb1[63], lines$Lb2[63], lines$Lb3[63], lines$Lb4[63], lines$Lg1[63], lines$Lg2[63], lines$Lg3[63], lines$Ll[63])
colnames(Eu.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Gd.lines <- data.frame(lines$Ka1[64], lines$Ka2[64], lines$Kb1[64], lines$Kb2[64], lines$Kb3[64], lines$La1[64], lines$La2[64], lines$Lb1[64], lines$Lb2[64], lines$Lb3[64], lines$Lb4[64], lines$Lg1[64], lines$Lg2[64], lines$Lg3[64], lines$Ll[64])
colnames(Gd.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Tb.lines <- data.frame(lines$Ka1[65], lines$Ka2[65], lines$Kb1[65], lines$Kb2[65], lines$Kb3[65], lines$La1[65], lines$La2[65], lines$Lb1[65], lines$Lb2[65], lines$Lb3[65], lines$Lb4[65], lines$Lg1[65], lines$Lg2[65], lines$Lg3[65], lines$Ll[65])
colnames(Tb.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Dy.lines <- data.frame(lines$Ka1[66], lines$Ka2[66], lines$Kb1[66], lines$Kb2[66], lines$Kb3[66], lines$La1[66], lines$La2[66], lines$Lb1[66], lines$Lb2[66], lines$Lb3[66], lines$Lb4[66], lines$Lg1[66], lines$Lg2[66], lines$Lg3[66], lines$Ll[66])
colnames(Dy.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ho.lines <- data.frame(lines$Ka1[67], lines$Ka2[67], lines$Kb1[67], lines$Kb2[67], lines$Kb3[67], lines$La1[67], lines$La2[67], lines$Lb1[67], lines$Lb2[67], lines$Lb3[67], lines$Lb4[67], lines$Lg1[67], lines$Lg2[67], lines$Lg3[67], lines$Ll[67])
colnames(Ho.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Er.lines <- data.frame(lines$Ka1[68], lines$Ka2[68], lines$Kb1[68], lines$Kb2[68], lines$Kb3[68], lines$La1[68], lines$La2[68], lines$Lb1[68], lines$Lb2[68], lines$Lb3[68], lines$Lb4[68], lines$Lg1[68], lines$Lg2[68], lines$Lg3[68], lines$Ll[68])
colnames(Er.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Tm.lines <- data.frame(lines$Ka1[69], lines$Ka2[69], lines$Kb1[69], lines$Kb2[69], lines$Kb3[69], lines$La1[69], lines$La2[69], lines$Lb1[69], lines$Lb2[69], lines$Lb3[69], lines$Lb4[69], lines$Lg1[69], lines$Lg2[69], lines$Lg3[69], lines$Ll[69])
colnames(Tm.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Yb.lines <- data.frame(lines$Ka1[70], lines$Ka2[70], lines$Kb1[70], lines$Kb2[70], lines$Kb3[70], lines$La1[70], lines$La2[70], lines$Lb1[70], lines$Lb2[70], lines$Lb3[70], lines$Lb4[70], lines$Lg1[70], lines$Lg2[70], lines$Lg3[70], lines$Ll[70])
colnames(Yb.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Lu.lines <- data.frame(lines$Ka1[71], lines$Ka2[71], lines$Kb1[71], lines$Kb2[71], lines$Kb3[71], lines$La1[71], lines$La2[71], lines$Lb1[71], lines$Lb2[71], lines$Lb3[71], lines$Lb4[71], lines$Lg1[71], lines$Lg2[71], lines$Lg3[71], lines$Ll[71])
colnames(Lu.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Hf.lines <- data.frame(lines$Ka1[72], lines$Ka2[72], lines$Kb1[72], lines$Kb2[72], lines$Kb3[72], lines$La1[72], lines$La2[72], lines$Lb1[72], lines$Lb2[72], lines$Lb3[72], lines$Lb4[72], lines$Lg1[72], lines$Lg2[72], lines$Lg3[72], lines$Ll[72])
colnames(Hf.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ta.lines <- data.frame(lines$Ka1[73], lines$Ka2[73], lines$Kb1[73], lines$Kb2[73], lines$Kb3[73], lines$La1[73], lines$La2[73], lines$Lb1[73], lines$Lb2[73], lines$Lb3[73], lines$Lb4[73], lines$Lg1[73], lines$Lg2[73], lines$Lg3[73], lines$Ll[73])
colnames(Ta.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

W.lines <- data.frame(lines$Ka1[74], lines$Ka2[74], lines$Kb1[74], lines$Kb2[74], lines$Kb3[74], lines$La1[74], lines$La2[74], lines$Lb1[74], lines$Lb2[74], lines$Lb3[74], lines$Lb4[74], lines$Lg1[74], lines$Lg2[74], lines$Lg3[74], lines$Ll[74])
colnames(W.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Re.lines <- data.frame(lines$Ka1[75], lines$Ka2[75], lines$Kb1[75], lines$Kb2[75], lines$Kb3[75], lines$La1[75], lines$La2[75], lines$Lb1[75], lines$Lb2[75], lines$Lb3[75], lines$Lb4[75], lines$Lg1[75], lines$Lg2[75], lines$Lg3[75], lines$Ll[75])
colnames(Re.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Os.lines <- data.frame(lines$Ka1[76], lines$Ka2[76], lines$Kb1[76], lines$Kb2[76], lines$Kb3[76], lines$La1[76], lines$La2[76], lines$Lb1[76], lines$Lb2[76], lines$Lb3[76], lines$Lb4[76], lines$Lg1[76], lines$Lg2[76], lines$Lg3[76], lines$Ll[76])
colnames(Os.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ir.lines <- data.frame(lines$Ka1[77], lines$Ka2[77], lines$Kb1[77], lines$Kb2[77], lines$Kb3[77], lines$La1[77], lines$La2[77], lines$Lb1[77], lines$Lb2[77], lines$Lb3[77], lines$Lb4[77], lines$Lg1[77], lines$Lg2[77], lines$Lg3[77], lines$Ll[77])
colnames(Ir.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Pt.lines <- data.frame(lines$Ka1[78], lines$Ka2[78], lines$Kb1[78], lines$Kb2[78], lines$Kb3[78], lines$La1[78], lines$La2[78], lines$Lb1[78], lines$Lb2[78], lines$Lb3[78], lines$Lb4[78], lines$Lg1[78], lines$Lg2[78], lines$Lg3[78], lines$Ll[78])
colnames(Pt.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Au.lines <- data.frame(lines$Ka1[79], lines$Ka2[79], lines$Kb1[79], lines$Kb2[79], lines$Kb3[79], lines$La1[79], lines$La2[79], lines$Lb1[79], lines$Lb2[79], lines$Lb3[79], lines$Lb4[79], lines$Lg1[79], lines$Lg2[79], lines$Lg3[79], lines$Ll[79])
colnames(Au.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Hg.lines <- data.frame(lines$Ka1[80], lines$Ka2[80], lines$Kb1[80], lines$Kb2[80], lines$Kb3[80], lines$La1[80], lines$La2[80], lines$Lb1[80], lines$Lb2[80], lines$Lb3[80], lines$Lb4[80], lines$Lg1[80], lines$Lg2[80], lines$Lg3[80], lines$Ll[80])
colnames(Hg.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Tl.lines <- data.frame(lines$Ka1[81], lines$Ka2[81], lines$Kb1[81], lines$Kb2[81], lines$Kb3[81], lines$La1[81], lines$La2[81], lines$Lb1[81], lines$Lb2[81], lines$Lb3[81], lines$Lb4[81], lines$Lg1[81], lines$Lg2[81], lines$Lg3[81], lines$Ll[81])
colnames(Tl.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Pb.lines <- data.frame(lines$Ka1[82], lines$Ka2[82], lines$Kb1[82], lines$Kb2[82], lines$Kb3[82], lines$La1[82], lines$La2[82], lines$Lb1[82], lines$Lb2[82], lines$Lb3[82], lines$Lb4[82], lines$Lg1[82], lines$Lg2[82], lines$Lg3[82], lines$Ll[82])
colnames(Pb.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Bi.lines <- data.frame(lines$Ka1[83], lines$Ka2[83], lines$Kb1[83], lines$Kb2[83], lines$Kb3[83], lines$La1[83], lines$La2[83], lines$Lb1[83], lines$Lb2[83], lines$Lb3[83], lines$Lb4[83], lines$Lg1[83], lines$Lg2[83], lines$Lg3[83], lines$Ll[83])
colnames(Bi.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Po.lines <- data.frame(lines$Ka1[84], lines$Ka2[84], lines$Kb1[84], lines$Kb2[84], lines$Kb3[84], lines$La1[84], lines$La2[84], lines$Lb1[84], lines$Lb2[84], lines$Lb3[84], lines$Lb4[84], lines$Lg1[84], lines$Lg2[84], lines$Lg3[84], lines$Ll[84])
colnames(Po.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

At.lines <- data.frame(lines$Ka1[85], lines$Ka2[85], lines$Kb1[85], lines$Kb2[85], lines$Kb3[85], lines$La1[85], lines$La2[85], lines$Lb1[85], lines$Lb2[85], lines$Lb3[85], lines$Lb4[85], lines$Lg1[85], lines$Lg2[85], lines$Lg3[85], lines$Ll[85])
colnames(At.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Rn.lines <- data.frame(lines$Ka1[86], lines$Ka2[86], lines$Kb1[86], lines$Kb2[86], lines$Kb3[86], lines$La1[86], lines$La2[86], lines$Lb1[86], lines$Lb2[86], lines$Lb3[86], lines$Lb4[86], lines$Lg1[86], lines$Lg2[86], lines$Lg3[86], lines$Ll[86])
colnames(Rn.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Fr.lines <- data.frame(lines$Ka1[87], lines$Ka2[87], lines$Kb1[87], lines$Kb2[87], lines$Kb3[87], lines$La1[87], lines$La2[87], lines$Lb1[87], lines$Lb2[87], lines$Lb3[87], lines$Lb4[87], lines$Lg1[87], lines$Lg2[87], lines$Lg3[87], lines$Ll[87])
colnames(Fr.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ra.lines <- data.frame(lines$Ka1[88], lines$Ka2[88], lines$Kb1[88], lines$Kb2[88], lines$Kb3[88], lines$La1[88], lines$La2[88], lines$Lb1[88], lines$Lb2[88], lines$Lb3[88], lines$Lb4[88], lines$Lg1[88], lines$Lg2[88], lines$Lg3[88], lines$Ll[88])
colnames(Ra.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Ac.lines <- data.frame(lines$Ka1[89], lines$Ka2[89], lines$Kb1[89], lines$Kb2[89], lines$Kb3[89], lines$La1[89], lines$La2[89], lines$Lb1[89], lines$Lb2[89], lines$Lb3[89], lines$Lb4[89], lines$Lg1[89], lines$Lg2[89], lines$Lg3[89], lines$Ll[89])
colnames(Ac.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Th.lines <- data.frame(lines$Ka1[90], lines$Ka2[90], lines$Kb1[90], lines$Kb2[90], lines$Kb3[90], lines$La1[90], lines$La2[90], lines$Lb1[90], lines$Lb2[90], lines$Lb3[90], lines$Lb4[90], lines$Lg1[90], lines$Lg2[90], lines$Lg3[90], lines$Ll[90])
colnames(Th.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Pa.lines <- data.frame(lines$Ka1[91], lines$Ka2[91], lines$Kb1[91], lines$Kb2[91], lines$Kb3[91], lines$La1[91], lines$La2[91], lines$Lb1[91], lines$Lb2[91], lines$Lb3[91], lines$Lb4[91], lines$Lg1[91], lines$Lg2[91], lines$Lg3[91], lines$Ll[91])
colnames(Pa.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

U.lines <- data.frame(lines$Ka1[92], lines$Ka2[92], lines$Kb1[92], lines$Kb2[92], lines$Kb3[92], lines$La1[92], lines$La2[92], lines$Lb1[92], lines$Lb2[92], lines$Lb3[92], lines$Lb4[92], lines$Lg1[92], lines$Lg2[92], lines$Lg3[92], lines$Ll[92])
colnames(U.lines) <- c("Ka1", "Ka2", "Kb1", "Kb2", "Kb3", "La1", "La2", "Lb1", "Lb2", "Lb3", "Lb4", "Lg1", "Lg2", "Lg3", "Ll")

Pu.lines <- data.frame(lines$Ka1[94], lines$Ka2[94], lines$Kb1[94], lines$Kb2[94], lines$Kb3[94], lines$La1[94], lines$La2[94], lines$Lb1[94], lines$Lb2[94], lines$Lb3[94], lines$Lb4[94], lines$Lg1[94], lines$Lg2[94], lines$Lg3[94], lines$Ll[94])
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

H.table <- data.frame(as.vector(t(H.lines)), Intensity)
colnames(H.table) <- c("Line", "Intensity")

He.table <- data.frame(as.vector(t(He.lines)), Intensity)
colnames(He.table) <- c("Line", "Intensity")

Li.table <- data.frame(as.vector(t(Li.lines)), Intensity)
colnames(Li.table) <- c("Line", "Intensity")

Be.table <- data.frame(as.vector(t(Be.lines)), Intensity)
colnames(Be.table) <- c("Line", "Intensity")

B.table <- data.frame(as.vector(t(B.lines)), Intensity)
colnames(B.table) <- c("Line", "Intensity")

C.table <- data.frame(as.vector(t(C.lines)), Intensity)
colnames(C.table) <- c("Line", "Intensity")

N.table <- data.frame(as.vector(t(N.lines)), Intensity)
colnames(N.table) <- c("Line", "Intensity")

O.table <- data.frame(as.vector(t(O.lines)), Intensity)
colnames(O.table) <- c("Line", "Intensity")

F.table <- data.frame(as.vector(t(F.lines)), Intensity)
colnames(F.table) <- c("Line", "Intensity")

Ne.table <- data.frame(as.vector(t(Ne.lines)), Intensity)
colnames(Ne.table) <- c("Line", "Intensity")

Na.table <- data.frame(as.vector(t(Na.lines)), Intensity)
colnames(Na.table) <- c("Line", "Intensity")

Na.table <- data.frame(as.vector(t(Na.lines)), Intensity)
colnames(Na.table) <- c("Line", "Intensity")

Mg.table <- data.frame(as.vector(t(Mg.lines)), Intensity)
colnames(Mg.table) <- c("Line", "Intensity")

Al.table <- data.frame(as.vector(t(Al.lines)), Intensity)
colnames(Al.table) <- c("Line", "Intensity")

Si.table <- data.frame(as.vector(t(Si.lines)), Intensity)
colnames(Si.table) <- c("Line", "Intensity")

P.table <- data.frame(as.vector(t(P.lines)), Intensity)
colnames(P.table) <- c("Line", "Intensity")

S.table <- data.frame(as.vector(t(S.lines)), Intensity)
colnames(S.table) <- c("Line", "Intensity")

Cl.table <- data.frame(as.vector(t(Cl.lines)), Intensity)
colnames(Cl.table) <- c("Line", "Intensity")

Ar.table <- data.frame(as.vector(t(Ar.lines)), Intensity)
colnames(Ar.table) <- c("Line", "Intensity")

K.table <- data.frame(as.vector(t(K.lines)), Intensity)
colnames(K.table) <- c("Line", "Intensity")

Ca.table <- data.frame(as.vector(t(Ca.lines)), Intensity)
colnames(Ca.table) <- c("Line", "Intensity")

Sc.table <- data.frame(as.vector(t(Sc.lines)), Intensity)
colnames(Sc.table) <- c("Line", "Intensity")

Ti.table <- data.frame(as.vector(t(Ti.lines)), Intensity)
colnames(Ti.table) <- c("Line", "Intensity")

V.table <- data.frame(as.vector(t(V.lines)), Intensity)
colnames(V.table) <- c("Line", "Intensity")

Cr.table <- data.frame(as.vector(t(Cr.lines)), Intensity)
colnames(Cr.table) <- c("Line", "Intensity")

Mn.table <- data.frame(as.vector(t(Mn.lines)), Intensity)
colnames(Mn.table) <- c("Line", "Intensity")

Fe.table <- data.frame(as.vector(t(Fe.lines)), Intensity)
colnames(Fe.table) <- c("Line", "Intensity")

Co.table <- data.frame(as.vector(t(Co.lines)), Intensity)
colnames(Co.table) <- c("Line", "Intensity")

Ni.table <- data.frame(as.vector(t(Ni.lines)), Intensity)
colnames(Ni.table) <- c("Line", "Intensity")

Cu.table <- data.frame(as.vector(t(Cu.lines)), Intensity)
colnames(Cu.table) <- c("Line", "Intensity")

Zn.table <- data.frame(as.vector(t(Zn.lines)), Intensity)
colnames(Zn.table) <- c("Line", "Intensity")

Ga.table <- data.frame(as.vector(t(Ga.lines)), Intensity)
colnames(Ga.table) <- c("Line", "Intensity")

Ge.table <- data.frame(as.vector(t(Ge.lines)), Intensity)
colnames(Ge.table) <- c("Line", "Intensity")

As.table <- data.frame(as.vector(t(As.lines)), Intensity)
colnames(As.table) <- c("Line", "Intensity")

Se.table <- data.frame(as.vector(t(Se.lines)), Intensity)
colnames(Se.table) <- c("Line", "Intensity")

Br.table <- data.frame(as.vector(t(Br.lines)), Intensity)
colnames(Br.table) <- c("Line", "Intensity")

Kr.table <- data.frame(as.vector(t(Kr.lines)), Intensity)
colnames(Kr.table) <- c("Line", "Intensity")

Rb.table <- data.frame(as.vector(t(Rb.lines)), Intensity)
colnames(Rb.table) <- c("Line", "Intensity")

Sr.table <- data.frame(as.vector(t(Sr.lines)), Intensity)
colnames(Sr.table) <- c("Line", "Intensity")

Y.table <- data.frame(as.vector(t(Y.lines)), Intensity)
colnames(Y.table) <- c("Line", "Intensity")

Zr.table <- data.frame(as.vector(t(Zr.lines)), Intensity)
colnames(Zr.table) <- c("Line", "Intensity")

Nb.table <- data.frame(as.vector(t(Nb.lines)), Intensity)
colnames(Nb.table) <- c("Line", "Intensity")

Mo.table <- data.frame(as.vector(t(Mo.lines)), Intensity)
colnames(Mo.table) <- c("Line", "Intensity")

Tc.table <- data.frame(as.vector(t(Tc.lines)), Intensity)
colnames(Tc.table) <- c("Line", "Intensity")

Ru.table <- data.frame(as.vector(t(Ru.lines)), Intensity)
colnames(Ru.table) <- c("Line", "Intensity")

Rh.table <- data.frame(as.vector(t(Rh.lines)), Intensity)
colnames(Rh.table) <- c("Line", "Intensity")

Pd.table <- data.frame(as.vector(t(Pd.lines)), Intensity)
colnames(Pd.table) <- c("Line", "Intensity")

In.table <- data.frame(as.vector(t(In.lines)), Intensity)
colnames(In.table) <- c("Line", "Intensity")

Sn.table <- data.frame(as.vector(t(Sn.lines)), Intensity)
colnames(Sn.table) <- c("Line", "Intensity")

Sb.table <- data.frame(as.vector(t(Sb.lines)), Intensity)
colnames(Sb.table) <- c("Line", "Intensity")

Te.table <- data.frame(as.vector(t(Te.lines)), Intensity)
colnames(Te.table) <- c("Line", "Intensity")

I.table <- data.frame(as.vector(t(I.lines)), Intensity)
colnames(I.table) <- c("Line", "Intensity")

Xe.table <- data.frame(as.vector(t(Xe.lines)), Intensity)
colnames(Xe.table) <- c("Line", "Intensity")

Cs.table <- data.frame(as.vector(t(Cs.lines)), Intensity)
colnames(Cs.table) <- c("Line", "Intensity")

Ba.table <- data.frame(as.vector(t(Ba.lines)), Intensity)
colnames(Ba.table) <- c("Line", "Intensity")

La.table <- data.frame(as.vector(t(La.lines)), Intensity)
colnames(La.table) <- c("Line", "Intensity")

Ce.table <- data.frame(as.vector(t(Ce.lines)), Intensity)
colnames(Ce.table) <- c("Line", "Intensity")

Pr.table <- data.frame(as.vector(t(Pr.lines)), Intensity)
colnames(Pr.table) <- c("Line", "Intensity")

Nd.table <- data.frame(as.vector(t(Nd.lines)), Intensity)
colnames(Nd.table) <- c("Line", "Intensity")

Pm.table <- data.frame(as.vector(t(Pm.lines)), Intensity)
colnames(Pm.table) <- c("Line", "Intensity")

Sm.table <- data.frame(as.vector(t(Sm.lines)), Intensity)
colnames(Sm.table) <- c("Line", "Intensity")

Eu.table <- data.frame(as.vector(t(Eu.lines)), Intensity)
colnames(Eu.table) <- c("Line", "Intensity")

Gd.table <- data.frame(as.vector(t(Gd.lines)), Intensity)
colnames(Gd.table) <- c("Line", "Intensity")

Tb.table <- data.frame(as.vector(t(Tb.lines)), Intensity)
colnames(Tb.table) <- c("Line", "Intensity")

Dy.table <- data.frame(as.vector(t(Dy.lines)), Intensity)
colnames(Dy.table) <- c("Line", "Intensity")

Ho.table <- data.frame(as.vector(t(Ho.lines)), Intensity)
colnames(Ho.table) <- c("Line", "Intensity")

Er.table <- data.frame(as.vector(t(Er.lines)), Intensity)
colnames(Er.table) <- c("Line", "Intensity")

Tm.table <- data.frame(as.vector(t(Tm.lines)), Intensity)
colnames(Tm.table) <- c("Line", "Intensity")

Yb.table <- data.frame(as.vector(t(Yb.lines)), Intensity)
colnames(Yb.table) <- c("Line", "Intensity")

Lu.table <- data.frame(as.vector(t(Lu.lines)), Intensity)
colnames(Lu.table) <- c("Line", "Intensity")

Hf.table <- data.frame(as.vector(t(Hf.lines)), Intensity)
colnames(Hf.table) <- c("Line", "Intensity")

Ta.table <- data.frame(as.vector(t(Ta.lines)), Intensity)
colnames(Ta.table) <- c("Line", "Intensity")

W.table <- data.frame(as.vector(t(W.lines)), Intensity)
colnames(W.table) <- c("Line", "Intensity")

Re.table <- data.frame(as.vector(t(Re.lines)), Intensity)
colnames(Re.table) <- c("Line", "Intensity")

Os.table <- data.frame(as.vector(t(Os.lines)), Intensity)
colnames(Os.table) <- c("Line", "Intensity")

Ir.table <- data.frame(as.vector(t(Ir.lines)), Intensity)
colnames(Ir.table) <- c("Line", "Intensity")

Pt.table <- data.frame(as.vector(t(Pt.lines)), Intensity)
colnames(Pt.table) <- c("Line", "Intensity")

Au.table <- data.frame(as.vector(t(Au.lines)), Intensity)
colnames(Au.table) <- c("Line", "Intensity")

Hg.table <- data.frame(as.vector(t(Hg.lines)), Intensity)
colnames(Hg.table) <- c("Line", "Intensity")

Tl.table <- data.frame(as.vector(t(Tl.lines)), Intensity)
colnames(Tl.table) <- c("Line", "Intensity")

Pb.table <- data.frame(as.vector(t(Pb.lines)), Intensity)
colnames(Pb.table) <- c("Line", "Intensity")

Bi.table <- data.frame(as.vector(t(Bi.lines)), Intensity)
colnames(Bi.table) <- c("Line", "Intensity")

Po.table <- data.frame(as.vector(t(Po.lines)), Intensity)
colnames(Pb.table) <- c("Line", "Intensity")

At.table <- data.frame(as.vector(t(At.lines)), Intensity)
colnames(At.table) <- c("Line", "Intensity")

Rn.table <- data.frame(as.vector(t(Rn.lines)), Intensity)
colnames(Rn.table) <- c("Line", "Intensity")

Fr.table <- data.frame(as.vector(t(Fr.lines)), Intensity)
colnames(Fr.table) <- c("Line", "Intensity")

Ra.table <- data.frame(as.vector(t(Ra.lines)), Intensity)
colnames(Ra.table) <- c("Line", "Intensity")

Ac.table <- data.frame(as.vector(t(Ac.lines)), Intensity)
colnames(Ac.table) <- c("Line", "Intensity")

Th.table <- data.frame(as.vector(t(Th.lines)), Intensity)
colnames(Th.table) <- c("Line", "Intensity")

Pa.table <- data.frame(as.vector(t(Pa.lines)), Intensity)
colnames(Pa.table) <- c("Line", "Intensity")

U.table <- data.frame(as.vector(t(U.lines)), Intensity)
colnames(U.table) <- c("Line", "Intensity")

Pu.table <- data.frame(as.vector(t(Pu.lines)), Intensity)
colnames(Pu.table) <- c("Line", "Intensity")



spectralLines <- c("Ne.K.alpha", "Ne.K.beta", "Na.K.alpha", "Na.K.beta", "Mg.K.alpha", "Mg.K.beta", "Al.K.alpha", "Al.K.beta", "Si.K.alpha", "Si.K.beta", "P.K.alpha", "P.K.beta", "S.K.alpha", "S.K.beta", "Cl.K.alpha", "Cl.K.beta", "Ar.K.alpha", "Ar.K.beta", "K.K.alpha", "K.K.beta", "Ca.K.alpha", "Ca.K.beta", "Sc.K.alpha", "Sc.K.beta", "Ti.K.alpha", "Ti.K.beta", "V.K.alpha", "V.K.beta", "Cr.K.alpha", "Cr.K.beta", "Mn.K.alpha", "Mn.K.beta", "Fe.K.alpha", "Fe.K.beta", "Co.K.alpha", "Co.K.beta", "Ni.K.alpha", "Ni.K.beta", "Cu.K.alpha", "Cu.K.beta", "Zn.K.alpha", "Zn.K.beta", "Ga.K.alpha", "Ga.K.beta", "Ge.K.alpha", "Ge.K.beta", "As.K.alpha", "As.K.beta", "Se.K.alpha", "Se.K.beta", "Br.K.alpha", "Br.K.beta", "Kr.K.alpha", "Kr.K.beta", "Rb.K.alpha", "Rb.K.beta", "Sr.K.alpha", "Sr.K.beta", "Y.K.alpha", "Y.K.beta", "Zr.K.alpha", "Zr.K.beta", "Nb.K.alpha", "Nb.K.beta", "Mo.K.alpha", "Mo.K.beta", "Mo.L.alpha", "Mo.L.beta", "Ru.K.alpha", "Ru.K.beta", "Ru.L.alpha", "Ru.L.beta", "Rh.K.alpha", "Rh.K.beta", "Rh.L.alpha", "Rh.L.beta", "Pd.K.alpha", "Pd.K.beta", "Pd.L.alpha", "Pd.L.beta", "Ag.K.alpha", "Ag.K.beta", "Ag.L.alpha", "Ag.L.beta", "Cd.K.alpha", "Cd.K.beta", "Cd.L.alpha", "Cd.L.beta", " In.K.alpha", "In.K.beta", "In.L.alpha", "Sn.K.alpha", "Sn.K.beta", "Sn.L.alpha", "Sn.L.beta", "Sb.K.alpha", "Sb.K.beta", "Sb.L.alpha", "Sb.L.beta", "Te.K.alpha", "Te.K.beta", "Te.L.alpha", "Te.L.beta", "I.K.alpha", "I.K.beta", "I.L.alpha", "I.L.beta", "Xe.K.alpha", "Xe.K.beta", "Xe.L.alpha", "Xe.L.beta", "Cs.K.alpha", "Cs.K.beta", "Cs.L.alpha", "Cs.L.beta", "Ba.K.alpha", "Ba.K.beta", "Ba.L.alpha", "Ba.L.beta", "La.K.alpha", "La.K.beta", "La.L.alpha", "La.L.beta", "Ce.K.alpha", "Ce.K.beta", "Ce.L.alpha", "Ce.L.beta", "Pr.K.alpha", "Pr.K.beta", "Pr.L.alpha", "Pr.L.beta", "Nd.K.alpha", "Nd.K.beta", "Nd.L.alpha", "Nd.L.beta", "Pm.L.alpha", "Pm.L.beta", "Sm.L.alpha", "Sm.L.beta", "Eu.L.alpha", "Eu.L.beta", "Gd.L.alpha", "Gd.L.beta", "Tb.L.alpha", "Tb.L.beta", "Dy.L.alpha", "Dy.L.beta", "Ho.L.alpha", "Ho.L.beta", "Er.L.alpha", "Er.L.beta", "Tm.L.alpha", "Tm.L.beta", "Yb.L.alpha", "Yb.L.beta", "Lu.L.alpha", "Lu.L.beta", "Hf.L.alpha", "Hf.L.beta", "Ta.L.alpha", "Ta.L.beta", "W.L.alpha", "W.L.beta", "Re.L.alpha", "Re.L.beta", "Os.L.alpha", "Os.L.beta", "Ir.L.alpha", "Ir.L.beta", "Pt.L.alpha", "Pt.L.beta", "Au.L.alpha", "Au.L.beta", "Hg.L.alpha", "Hg.L.beta", "Tl.L.alpha", "Tl.L.beta", "Pb.L.alpha", "Pb.L.beta", "Bi.L.alpha", "Bi.L.beta", "Po.L.alpha", "Po.L.beta", "At.L.alpha", "At.L.beta", "Rn.L.alpha", "Rn.L.beta", "Fr.L.alpha", "Fr.L.beta", "Ra.L.alpha", "Ra.L.beta", "Ac.L.alpha", "Ac.L.beta", "Th.L.alpha", "Th.L.beta", "Pa.L.alpha", "Pa.L.beta", "U.L.alpha", "U.L.beta", "Pu.L.alpha", "Pu.L.beta", "Au.M.line", "Hg.M.line", "Pb.M.line", "U.M.line")

standard <- c("Spectrum", "Ca.K.alpha", "Ti.K.alpha", "Fe.K.alpha")

kalphaLines <- c("Na.K.alpha",  "Mg.K.alpha", "Al.K.alpha", "Si.K.alpha", "P.K.alpha", "S.K.alpha", "Cl.K.alpha", "Ar.K.alpha", "K.K.alpha", "Ca.K.alpha", "Sc.K.alpha", "Ti.K.alpha", "V.K.alpha", "Cr.K.alpha", "Mn.K.alpha", "Fe.K.alpha", "Co.K.alpha", "Ni.K.alpha", "Cu.K.alpha", "Zn.K.alpha", "Ga.K.alpha", "Ge.K.alpha", "As.K.alpha", "Se.K.alpha", "Br.K.alpha", "Kr.K.alpha", "Rb.K.alpha", "Sr.K.alpha", "Y.K.alpha", "Zr.K.alpha", "Nb.K.alpha", "Mo.K.alpha", "Ru.K.alpha", "Rh.K.alpha", "Pd.K.alpha", "Ag.K.alpha", "Cd.K.alpha", "In.K.alpha", "Sn.K.alpha", "Sb.K.alpha", "Te.K.alpha", "Te.K.beta", "I.K.alpha", "Xe.K.alpha", "Cs.K.alpha", "Ba.K.alpha", "La.K.alpha", "Ce.K.alpha", "Pr.K.alpha", "Nd.K.alpha")

kbetaLines <- c("Na.K.beta",  "Mg.K.beta", "Al.K.beta", "Si.K.beta", "P.K.beta", "S.K.beta", "Cl.K.beta", "Ar.K.beta", "K.K.beta", "Ca.K.beta", "Sc.K.beta", "Ti.K.beta", "V.K.beta", "Cr.K.beta", "Mn.K.beta", "Fe.K.beta", "Co.K.beta", "Ni.K.beta", "Cu.K.beta", "Zn.K.beta", "Ga.K.beta", "Ge.K.beta", "As.K.beta", "Se.K.beta", "Br.K.beta", "Kr.K.beta", "Rb.K.beta", "Sr.K.beta", "Y.K.beta", "Zr.K.beta", "Nb.K.beta", "Mo.K.beta", "Ru.K.beta", "Rh.K.beta", "Pd.K.beta", "Ag.K.beta", "Cd.K.beta", "In.K.beta", "Sn.K.beta", "Sb.K.beta", "Te.K.beta", "Te.K.beta", "I.K.beta", "Xe.K.beta", "Cs.K.beta", "Ba.K.beta", "La.K.beta", "Ce.K.beta", "Pr.K.beta", "Nd.K.beta")

lalphaLines <- c("Mo.L.alpha", "Ru.L.alpha", "Rh.L.alpha", "Pd.L.alpha", "Ag.L.alpha", "Cd.L.alpha", "In.L.alpha", "Sn.L.alpha", "Sb.L.alpha", "Te.L.alpha", "I.L.alpha", "Xe.L.alpha", "Cs.L.alpha", "Ba.L.alpha", "La.L.alpha", "Ce.L.alpha", "Pr.L.alpha", "Nd.L.alpha", "Pm.L.alpha", "Sm.L.alpha", "Eu.L.alpha", "Gd.L.alpha", "Tb.L.alpha", "Dy.L.alpha", "Ho.L.alpha", "Er.L.alpha", "Tm.L.alpha", "Yb.L.alpha", "Lu.L.alpha", "Hf.L.alpha", "Ta.L.alpha", "W.L.alpha", "Re.L.alpha", "Os.L.alpha", "Ir.L.alpha", "Pt.L.alpha", "Au.L.alpha", "Hg.L.alpha", "Tl.L.alpha", "Pb.L.alpha", "Bi.L.alpha", "Po.L.alpha", "At.L.alpha", "Rn.L.alpha", "Fr.L.alpha", "Ra.L.alpha", "Ac.L.alpha", "Th.L.alpha", "Pa.L.alpha", "U.L.alpha")

lbetaLines <- c("Mo.L.beta", "Ru.L.beta", "Rh.L.beta", "Pd.L.beta", "Ag.L.beta", "Cd.L.beta", "In.L.beta", "Sn.L.beta", "Sb.L.beta", "Te.L.beta", "I.L.beta", "Xe.L.beta", "Cs.L.beta", "Ba.L.beta", "La.L.beta", "Ce.L.beta", "Pr.L.beta", "Nd.L.beta", "Pm.L.beta", "Sm.L.beta", "Eu.L.beta", "Gd.L.beta", "Tb.L.beta", "Dy.L.beta", "Ho.L.beta", "Er.L.beta", "Tm.L.beta", "Yb.L.beta", "Lu.L.beta", "Hf.L.beta", "Ta.L.beta", "W.L.beta", "Re.L.beta", "Os.L.beta", "Ir.L.beta", "Pt.L.beta", "Au.L.beta", "Hg.L.beta", "Tl.L.beta", "Pb.L.beta", "Bi.L.beta", "Po.L.beta", "At.L.beta", "Rn.L.beta", "Fr.L.beta", "Ra.L.beta", "Ac.L.beta", "Th.L.beta", "Pa.L.beta", "U.L.beta")

mLines <- c("Au.M.line", "Hg.M.line", "Pb.M.line", "U.M.line")


elementGrabKalpha <- function(element, data) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    
    hold.cps <- subset(data$CPS, !(data$Energy < elementLine[6][1,]-0.02 | data$Energy > elementLine[5][1,]+0.02))
    hold.file <- subset(data$Spectrum, !(data$Energy < elementLine[6][1,]-0.02 | data$Energy > elementLine[5][1,]+0.02))
    hold.frame <- data.frame(is.0(hold.cps, hold.file))
    colnames(hold.frame) <- c("Counts", "Spectrum")
    hold.ag <- aggregate(list(hold.frame$Counts), by=list(hold.frame$Spectrum), FUN="sum")
    colnames(hold.ag) <- c("Spectrum", paste(element, "K-alpha", sep=" "))
    
    hold.ag
    
}

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
    hold.frame <- data.frame(is.0(hold.cps, hold.file))
    colnames(hold.frame) <- c("Counts", "Spectrum")
    hold.ag <- aggregate(list(hold.frame$Counts), by=list(hold.frame$Spectrum), FUN="sum")
    colnames(hold.ag) <- c("Spectrum", paste(element, "K-beta", sep=" "))
    
    hold.ag
    
}

elementGrabLalpha <- function(element, data) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    
    hold.cps <- subset(data$CPS, !(data$Energy < elementLine[11][1,]-0.02 | data$Energy > elementLine[10][1,]+0.02))
    hold.file <- subset(data$Spectrum, !(data$Energy < elementLine[11][1,]-0.02 | data$Energy > elementLine[10][,1]+0.02))
    hold.frame <- data.frame(is.0(hold.cps, hold.file))
    colnames(hold.frame) <- c("Counts", "Spectrum")
    hold.ag <- aggregate(list(hold.frame$Counts), by=list(hold.frame$Spectrum), FUN="sum")
    colnames(hold.ag) <- c("Spectrum", paste(element, "L-alpha", sep=" "))
    
    hold.ag
    
}

elementGrabLbeta <- function(element, data) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    
    hold.cps <- subset(data$CPS, !(data$Energy < elementLine[12][1,]-0.02 | data$Energy > elementLine[14][1,]+0.02))
    hold.file <- subset(data$Spectrum, !(data$Energy < elementLine[12][1,]-0.02 | data$Energy > elementLine[14][1,]+0.02))

    hold.frame <- data.frame(is.0(hold.cps, hold.file))
    colnames(hold.frame) <- c("Counts", "Spectrum")
    hold.ag <- aggregate(list(hold.frame$Counts), by=list(hold.frame$Spectrum), FUN="sum")
    colnames(hold.ag) <- c("Spectrum", paste(element, "L-beta", sep=" "))
    
    hold.ag
    
}

elementGrabMalpha <- function(element, data) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    
    hold.cps <- subset(data$CPS, !(data$Energy < elementLine[20][1,]-0.02 | data$Energy > elementLine[22][1,]+0.02))
    hold.file <- subset(data$Spectrum, !(data$Energy < elementLine[20][1,]-0.02 | data$Energy > elementLine[22][,1]+0.02))
    hold.frame <- data.frame(is.0(hold.cps, hold.file))
    colnames(hold.frame) <- c("Counts", "Spectrum")
    hold.ag <- aggregate(list(hold.frame$Counts), by=list(hold.frame$Spectrum), FUN="sum")
    colnames(hold.ag) <- c("Spectrum", paste(element, "M-lines", sep=" "))
    
    hold.ag
    
}

elementGrab <- function(element.line, data) {
    
    element <- strsplit(x=element.line, split="\\.")[[1]][1]
    destination <- strsplit(x=element.line, split="\\.")[[1]][2]
    distance <- strsplit(x=element.line, split="\\.")[[1]][3]
    
    elementSelection <- if(destination=="K" && distance=="alpha"){
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
    
    elementSelection
    
}





####Normalize

element.norm <- function(data, element, min, max) {
    
    compton.norm <- subset(data$CPS, !(data$Energy < input$min | data$max > input$comptonmax))
    compton.file <- subset(data$Spectrum, !(data$Energy < input$min | data$Energy > input$max))
    compton.frame <- data.frame(is.0(compton.norm, compton.file))
    colnames(compton.frame) <- c("Compton", "Spectrum")
    compton.frame.ag <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")
    colnames(compton.frame.ag) <- c("Spectrum", "Compton")
    
    
}

####Cal Models

linear.simp <- function(concentration.table, spectra.line.table, element.line) {
    
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    
    predict.frame <- data.frame(concentration, intensity)
    colnames(predict.frame) <- c("Concentration", "Intensity")
    
    
    
    predict.intensity <- data.frame(predict.frame$Intensity)
    colnames(predict.intensity) <- c("Intensity")
    
    cal.lm <- lm(predict.frame$Concentration~predict.frame$Intensity)
    
    cal.lm
    
}

poly.simp <- function(concentration.table, spectra.line.table, element.line) {
    
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    
    predict.frame <- data.frame(concentration, intensity)
    colnames(predict.frame) <- c("Concentration", "Intensity")
    
    
    
    predict.intensity <- data.frame(predict.frame$Intensity)
    colnames(predict.intensity) <- c("Intensity")
    
    cal.lm.poly <- lm(predict.frame$Concentration~poly(predict.frame$Intensity, 2))
    
    cal.lm.poly
    
}

lucas.simp <- function(concentration.table, spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    lucas.intercept.table <- data.frame(rowSums(lucas.intercept.table.x[intercept.element.lines]))
    colnames(lucas.intercept.table) <- c("first")
    
    
    
    lucas.intercept <- lucas.intercept.table$first
    lucas.slope <- data.frame(lucas.slope.table[slope.element.lines])
    
    
    
    predict.frame.luk <- data.frame(concentration, ((1+intensity/(intensity+lucas.intercept))-lucas.intercept/(intensity+lucas.intercept)),lucas.slope)
    colnames(predict.frame.luk) <- c("Concentration", "Intensity", names(lucas.slope))
    
    
    
    predict.intensity.luk <- data.frame(predict.frame.luk$Intensity, lucas.slope)
    colnames(predict.intensity.luk) <- c("Intensity", names(lucas.slope))
    
    lucas.lm <- lm(Concentration~., data=predict.frame.luk)
    
    lucas.lm
    
    
}


linear.tc <- function(concentration.table, spectra.line.table, element.line) {
    
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    
    total.counts <- aggregate(CPS~Spectrum, data=data, sum)
    colnames(total.counts) <- c("Spectrum", "CPS")
    
    
    
    predict.frame.tc <- data.frame(concentration, intensity/total.counts$CPS)
    colnames(predict.frame.tc) <- c("Concentration", "Intensity")
    
    
    
    predict.intensity.tc <- data.frame(predict.frame.tc$Intensity)
    colnames(predict.intensity.tc) <- c("Intensity")
    
    cal.lm.tc <- lm(predict.frame.tc$Concentration~predict.frame.tc$Intensity)
    
    cal.lm.tc
    
}

poly.tc <- function(concentration.table, spectra.line.table, element.line) {
    
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    
    
    
    total.counts <- aggregate(CPS~Spectrum, data=data, sum)
    colnames(total.counts) <- c("Spectrum", "CPS")
    
    
    
    predict.frame.tc <- data.frame(concentration, intensity/total.counts$CPS)
    colnames(predict.frame.tc) <- c("Concentration", "Intensity")
    
    
    
    predict.intensity.tc <- data.frame(predict.frame.tc$Intensity)
    colnames(predict.intensity.tc) <- c("Intensity")
    
    cal.lm.poly.tc <- lm(predict.frame.tc$Concentration~poly(predict.frame.tc$Intensity, 2))
    
    cal.lm.poly.tc
    
    
    
}




lucas.tc <- function(concentration.table, spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    lucas.intercept.table.tc <- data.frame(rowSums(lucas.intercept.table.x[intercept.element.lines]))/total.counts$CPS
    colnames(lucas.intercept.table.tc) <- c("first")
    
    
    
    lucas.intercept.tc <- lucas.intercept.table.tc$first
    lucas.slope.tc <- data.frame(lucas.slope.table[slope.element.lines])/total.counts$CPS
    
    
    
    predict.frame.luc.tc <- data.frame(concentration, ((intensity/total.counts$CPS-lucas.intercept.tc)/(intensity/total.counts$CPS+lucas.intercept.tc)),lucas.slope.tc)
    colnames(predict.frame.luc.tc) <- c("Concentration", "Intensity", names(lucas.slope.tc))
    
    
    
    predict.intensity.luc.tc <- data.frame(predict.frame.luc.tc$Intensity, lucas.slope.tc)
    colnames(predict.intensity.luc.tc) <- c("Intensity", names(lucas.slope.tc))
    
    lucas.lm.tc <- lm(Concentration~., data=predict.frame.luc.tc)
    
    lucas.lm.tc
    
    
}

linear.comp <- function(data, concentration.table, spectra.line.table, element.line) {
    
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    
    compton.norm <- subset(data$CPS, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))
    compton.file <- subset(data$Spectrum, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))
    compton.frame <- data.frame(is.0(compton.norm, compton.file))
    colnames(compton.frame) <- c("Compton", "Spectrum")
    compton.frame.ag <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")
    colnames(compton.frame.ag) <- c("Spectrum", "Compton")
    
    predict.frame.comp <- data.frame(concentration, intensity/compton.frame.ag$Compton)
    colnames(predict.frame.comp) <- c("Concentration", "Intensity")
    
    
    
    predict.intensity.comp <- data.frame(predict.frame.comp$Intensity)
    colnames(predict.intensity.comp) <- c("Intensity")
    
    cal.lm.comp <- lm(predict.frame.comp$Concentration~predict.frame.comp$Intensity)
    
    cal.lm.comp
    
}

poly.comp <- function(data, concentration.table, spectra.line.table, element.line) {
    
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    
    compton.norm <- subset(data$CPS, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))
    compton.file <- subset(data$Spectrum, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))
    compton.frame <- data.frame(is.0(compton.norm, compton.file))
    colnames(compton.frame) <- c("Compton", "Spectrum")
    compton.frame.ag <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")
    colnames(compton.frame.ag) <- c("Spectrum", "Compton")
    
    predict.frame.comp <- data.frame(concentration, intensity/compton.frame.ag$Compton)
    colnames(predict.frame.comp) <- c("Concentration", "Intensity")
    
    
    
    predict.intensity.comp <- data.frame(predict.frame.comp$Intensity)
    colnames(predict.intensity.comp) <- c("Intensity")
    
    cal.lm.poly.comp <- lm(predict.frame.comp$Concentration~poly(predict.frame.comp$Intensity, 2))
    
    cal.lm.poly.comp
    
}

lucas.comp <- function(data, concentration.table, spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    
    compton.norm <- subset(data$CPS, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))
    compton.file <- subset(data$Spectrum, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))
    compton.frame <- data.frame(is.0(compton.norm, compton.file))
    colnames(compton.frame) <- c("Compton", "Spectrum")
    compton.frame.ag <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")
    colnames(compton.frame.ag) <- c("Spectrum", "Compton")
    
    
    
    lucas.intercept.table.comp <- data.frame(rowSums(lucas.intercept.table.x[intercept.element.lines]))/compton.frame.ag$Compton
    colnames(lucas.intercept.table.comp) <- c("first")
    
    
    
    lucas.intercept.comp <- lucas.intercept.table.comp$first
    lucas.slope.comp <- data.frame(lucas.slope.table[slope.element.lines])/compton.frame.ag$Compton
    
    
    
    
    predict.frame.luc.comp <- data.frame(concentration, ((1+intensity/compton.frame.ag$Compton)/(intensity/compton.frame.ag$Compton+lucas.intercept.comp)-lucas.intercept.comp/(intensity/compton.frame.ag$Compton+lucas.intercept.comp)),lucas.slope.comp)
    colnames(predict.frame.luc.comp) <- c("Concentration", "Intensity", names(lucas.slope.comp))
    
    
    
    predict.intensity.luc.comp <- data.frame(predict.frame.luc.comp$Intensity, lucas.slope.comp)
    colnames(predict.intensity.luc.comp) <- c("Intensity", names(lucas.slope.comp))
    
    lucas.lm.comp <- lm(Concentration~., data=predict.frame.luc.comp)
    
    lucas.lm.comp
    
}



###############
###Prep Data###
###############


###############
###Raw Spectra##
###############


general.prep <- function(spectra.line.table, element.line) {
    
    intensity <- spectra.line.table[,element.line]
    
    
    predict.frame <- data.frame(intensity)
    colnames(predict.frame) <- c("Intensity")
    
    
    
    predict.intensity <- data.frame(predict.frame$Intensity)
    colnames(predict.intensity) <- c("Intensity")
    
    predict.intensity
}

simple.tc.prep <- function(data,spectra.line.table, element.line) {
    
    intensity <- spectra.line.table[,element.line]
    
    
    total.counts <- aggregate(CPS~Spectrum, data=data, sum)
    colnames(total.counts) <- c("Spectrum", "CPS")
    
    
    
    predict.frame.tc <- data.frame(intensity/total.counts$CPS)
    colnames(predict.frame.tc) <- c("Intensity")
    
    
    
    predict.intensity.tc <- data.frame(predict.frame.tc$Intensity)
    colnames(predict.intensity.tc) <- c("Intensity")
    
    predict.intensity.tc
}


simple.comp.prep <- function(data, spectra.line.table, element.line, norm.min, norm.max) {
    
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    compton.norm <- subset(data$CPS, !(data$Energy < norm.min | data$Energy > norm.max))
    compton.file <- subset(data$Spectrum, !(data$Energy < norm.min | data$Energy > norm.max))
    compton.frame <- data.frame(is.0(compton.norm, compton.file))
    colnames(compton.frame) <- c("Compton", "Spectrum")
    compton.frame.ag <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")
    colnames(compton.frame.ag) <- c("Spectrum", "Compton")
    
    predict.frame.comp <- data.frame( intensity/compton.frame.ag$Compton)
    colnames(predict.frame.comp) <- c("Intensity")
    
    
    
    predict.intensity.comp <- data.frame(predict.frame.comp$Intensity)
    colnames(predict.intensity.comp) <- c("Intensity")
    
    predict.intensity.comp
    
}



###Prep Data



lucas.simp.prep <- function(spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lucas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none)
    colnames(lucas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lucas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lucas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    lucas.intercept.table <- data.frame(rowSums(lucas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")]))
    colnames(lucas.intercept.table) <- c("first")
    
    
    
    lucas.intercept <- lucas.intercept.table$first
    lucas.slope <- data.frame(lucas.slope.table[,slope.element.lines])
    colnames(lucas.slope) <- slope.element.lines
    
    
    
    predict.frame.luk <- data.frame(((1+intensity/(intensity+lucas.intercept))-lucas.intercept/(intensity+lucas.intercept)),lucas.slope)
    colnames(predict.frame.luk) <- c("Intensity", names(lucas.slope))
    
    predict.frame.luk <- data.frame(((1+intensity/(intensity+lucas.intercept)-lucas.intercept/(intensity+lucas.intercept))),lucas.slope)
    colnames(predict.frame.luk) <- c("Intensity", names(lucas.slope))
    
    
    
    predict.intensity.luk <- data.frame(predict.frame.luk$Intensity, lucas.slope)
    colnames(predict.intensity.luk) <- c("Intensity", names(lucas.slope))
    
    predict.intensity.luk
    
    
}



lucas.tc.prep <- function(data, spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    total.counts <- aggregate(CPS~Spectrum, data=data, sum)
    colnames(total.counts) <- c("Spectrum", "CPS")
    
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lucas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none)
    colnames(lucas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lucas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lucas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    lucas.intercept.table.tc <- data.frame(rowSums(lucas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")]))/total.counts$CPS
    colnames(lucas.intercept.table.tc) <- c("first")
    
    
    
    lucas.intercept.tc <- lucas.intercept.table.tc$first
    lucas.slope.tc <- data.frame(lucas.slope.table[,slope.element.lines])/total.counts$CPS
    colnames(lucas.slope.tc) <- slope.element.lines
    
    
    
    predict.intensity.luc.tc <- data.frame(((1+intensity/(intensity+lucas.intercept.tc)-lucas.intercept.tc/(intensity+lucas.intercept.tc))),lucas.slope.tc)
    colnames(predict.intensity.luc.tc) <- c("Intensity", names(lucas.slope.tc))
    
    
    predict.intensity.luc.tc
}





lucas.comp.prep <- function(data, spectra.line.table, element.line, slope.element.lines, intercept.element.lines, norm.min, norm.max) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    
    compton.norm <- subset(data$CPS, !(data$Energy < norm.min | data$Energy > norm.max))
    compton.file <- subset(data$Spectrum, !(data$Energy < norm.min | data$Energy > norm.max))
    compton.frame <- data.frame(is.0(compton.norm, compton.file))
    colnames(compton.frame) <- c("Compton", "Spectrum")
    compton.frame.ag <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")
    colnames(compton.frame.ag) <- c("Spectrum", "Compton")
    
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lucas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none)
    colnames(lucas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lucas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lucas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    
    lucas.intercept.table.comp <- data.frame(rowSums(lucas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")])/compton.frame.ag$Compton)
    colnames(lucas.intercept.table.comp) <- c("first")
    
    
    
    lucas.intercept.comp <- lucas.intercept.table.comp$first
    lucas.slope.comp <- data.frame(lucas.slope.table[,slope.element.lines]/compton.frame.ag$Compton)
    colnames(lucas.slope.comp) <- slope.element.lines
    
    
    predict.frame.luc.comp <- data.frame(((1+intensity/compton.frame.ag$Compton)/(intensity/compton.frame.ag$Compton+lucas.intercept.comp)-lucas.intercept.comp/(intensity/compton.frame.ag$Compton+lucas.intercept.comp)),lucas.slope.comp)
    colnames(predict.frame.luc.comp) <- c("Intensity", names(lucas.slope.comp))
    
    
    
    predict.intensity.luc.comp <- data.frame(predict.frame.luc.comp$Intensity, lucas.slope.comp)
    colnames(predict.intensity.luc.comp) <- c("Intensity", names(lucas.slope.comp))
    
    
    predict.intensity.luc.comp
}




###############
###Prep Data###
###############


###############
###Net Counts##
###############


general.prep.net <- function(spectra.line.table, element.line) {
    
    intensity <- spectra.line.table[,element.line]
    
    
    predict.frame <- data.frame(intensity)
    colnames(predict.frame) <- c("Intensity")
    
    
    
    predict.intensity <- data.frame(predict.frame$Intensity)
    colnames(predict.intensity) <- c("Intensity")
    
    predict.intensity
}

simple.tc.prep.net <- function(data,spectra.line.table, element.line) {
    
    intensity <- spectra.line.table[,element.line]
    
    total.counts.net <- rowSums(spectra.line.table[,-1])
    total.counts <- data.frame(data$Spectrum, total.counts.net)
    colnames(total.counts) <- c("Spectrum", "CPS")
    
    
    
    predict.frame.tc <- data.frame(intensity/total.counts$CPS)
    colnames(predict.frame.tc) <- c("Intensity")
    
    
    
    predict.intensity.tc <- data.frame(predict.frame.tc$Intensity)
    colnames(predict.intensity.tc) <- c("Intensity")
    
    predict.intensity.tc
}


simple.comp.prep.net <- function(data, spectra.line.table, element.line, norm.min, norm.max) {
    
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    compton.ag.fake.Spectrum <- data$Spectrum
    compton.ag.fake.Compton <- rep(1, length(data$Spectrum))
    compton.ag.fake <- data.frame(compton.ag.fake.Spectrum,compton.ag.fake.Compton)
    colnames(compton.ag.fake) <- c("Spectrum", "Compton")
    
    predict.frame.comp <- data.frame( intensity/compton.ag.fake$Compton)
    colnames(predict.frame.comp) <- c("Intensity")
    
    
    
    predict.intensity.comp <- data.frame(predict.frame.comp$Intensity)
    colnames(predict.intensity.comp) <- c("Intensity")
    
    predict.intensity.comp
    
}



###Prep Data



lucas.simp.prep.net <- function(spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lucas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none)
    colnames(lucas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lucas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lucas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    lucas.intercept.table <- data.frame(rowSums(lucas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")]))
    colnames(lucas.intercept.table) <- c("first")
    
    
    
    lucas.intercept <- lucas.intercept.table$first
    lucas.slope <- data.frame(lucas.slope.table[,slope.element.lines])
    colnames(lucas.slope) <- slope.element.lines
    
    
    
    predict.frame.luk <- data.frame(((1+intensity/(intensity+lucas.intercept))-lucas.intercept/(intensity+lucas.intercept)),lucas.slope)
    colnames(predict.frame.luk) <- c("Intensity", names(lucas.slope))
    
    
    
    predict.intensity.luk <- data.frame(predict.frame.luk$Intensity, lucas.slope)
    colnames(predict.intensity.luk) <- c("Intensity", names(lucas.slope))
    
    predict.intensity.luk
    
    
}



lucas.tc.prep.net <- function(data, spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    total.counts.net <- rowSums(spectra.line.table[,-1])
    total.counts <- data.frame(data$Spectrum, total.counts.net)
    colnames(total.counts) <- c("Spectrum", "CPS")
    
    
    
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lucas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none)
    colnames(lucas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lucas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lucas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    
    lucas.intercept.table.tc <- data.frame(rowSums(lucas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")]))/total.counts$CPS
    colnames(lucas.intercept.table.tc) <- c("first")
    
    
    
    
    lucas.intercept.tc <- lucas.intercept.table.tc$first
    lucas.slope.tc <- data.frame(lucas.slope.table[,slope.element.lines])/total.counts$CPS
    colnames(lucas.slope.tc) <- slope.element.lines
    
    
    predict.intensity.luc.tc <- data.frame(((1+intensity/(intensity+lucas.intercept.tc)-lucas.intercept.tc/(intensity+lucas.intercept.tc))),lucas.slope.tc)
    colnames(predict.intensity.luc.tc) <- c("Intensity", names(lucas.slope.tc))
    
    
    predict.intensity.luc.tc
}


lucas.comp.prep.net <- function(data, spectra.line.table, element.line, slope.element.lines, intercept.element.lines, norm.min, norm.max) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    
    
    compton.ag.fake.Spectrum <- data$Spectrum
    compton.ag.fake.Compton <- rep(1, length(data$Spectrum))
    compton.ag.fake <- data.frame(compton.ag.fake.Spectrum,compton.ag.fake.Compton)
    colnames(compton.ag.fake) <- c("Spectrum", "Compton")
    
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lucas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none)
    colnames(lucas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lucas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lucas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    
    lucas.intercept.table.comp <- data.frame(rowSums(lucas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")]))/compton.ag.fake$Compton
    colnames(lucas.intercept.table.comp) <- c("first")
    
    
    
    
    lucas.intercept.comp <- lucas.intercept.table.comp$first
    lucas.slope.comp <- data.frame(lucas.slope.table[,slope.element.lines])/compton.ag.fake$Compton
    colnames(lucas.slope.comp) <- slope.element.lines
    
    
    
    predict.frame.luc.comp <- data.frame(((1+predict.frame.comp$Intensity/(predict.frame.comp$Intensity+lucas.intercept.comp)-lucas.intercept.comp/(predict.frame.comp$Intensity+lucas.intercept.comp))),lucas.slope.comp)
    colnames(predict.frame.luc.comp) <- c("Intensity", names(lucas.slope.comp))
    
    
    
    predict.intensity.luc.comp <- data.frame(predict.frame.luc.comp$Intensity, lucas.slope.comp)
    colnames(predict.intensity.luc.comp) <- c("Intensity", names(lucas.slope.comp))
    
    
    predict.intensity.luc.comp
}



blank.data.frame <- data.frame(rep(0, length(standard)), rep(0, length(standard)), rep(0, length(standard)), rep(0, length(standard)), rep(0, length(standard)), rep(0, length(standard)), rep(0, length(standard)))
colnames(blank.data.frame) <- standard





