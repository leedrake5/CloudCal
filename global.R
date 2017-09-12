library(shiny)
library(ggplot2)
library(pbapply)
library(reshape2)
library(dplyr)
library(DT)


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
    #cm$y = c()
    #cm$model = c()
    
    #cm$residuals = c()
    #cm$fitted.values = c()
    #cm$effects = c()
    #cm$qr$qr = c()
    #cm$linear.predictors = c()
    #cm$weights = c()
    #cm$prior.weights = c()
    #cm$data = c()
    
    
    #cm$family$variance = c()
    #cm$family$dev.resids = c()
    #cm$family$aic = c()
    #cm$family$validmu = c()
    #cm$family$simulate = c()
    attr(cm$terms,".Environment") = c()
    attr(cm$formula,".Environment") = c()
    
    cm
}


black.diamond <- read.csv("data/blackdiamond.csv", header=FALSE, sep=",")
black.diamond.melt <- read.csv(file="data/blackdiamondmelt.csv")

######Load lines
k.lines <- read.csv(file="data/K Line-Table 1.csv", sep=",")
l.lines <- read.csv(file="data/L Line-Table 1.csv", sep=",")
fluorescence.lines <- read.csv("data/FluorescenceLines.csv")




spectralLines <- c("Ne.K.alpha", "Ne.K.beta", "Na.K.alpha", "Na.K.beta", "Mg.K.alpha", "Mg.K.beta", "Al.K.alpha", "Al.K.beta", "Si.K.alpha", "Si.K.beta", "P.K.alpha", "P.K.beta", "S.K.alpha", "S.K.beta", "Cl.K.alpha", "Cl.K.beta", "Ar.K.alpha", "Ar.K.beta", "K.K.alpha", "K.K.beta", "Ca.K.alpha", "Ca.K.beta", "Sc.K.alpha", "Sc.K.beta", "Ti.K.alpha", "Ti.K.beta", "V.K.alpha", "V.K.beta", "Cr.K.alpha", "Cr.K.beta", "Mn.K.alpha", "Mn.K.beta", "Fe.K.alpha", "Fe.K.beta", "Co.K.alpha", "Co.K.beta", "Ni.K.alpha", "Ni.K.beta", "Cu.K.alpha", "Cu.K.beta", "Zn.K.alpha", "Zn.K.beta", "Ga.K.alpha", "Ga.K.beta", "Ge.K.alpha", "Ge.K.beta", "As.K.alpha", "As.K.beta", "Se.K.alpha", "Se.K.beta", "Br.K.alpha", "Br.K.beta", "Kr.K.alpha", "Kr.K.beta", "Rb.K.alpha", "Rb.K.beta", "Sr.K.alpha", "Sr.K.beta", "Y.K.alpha", "Y.K.beta", "Zr.K.alpha", "Zr.K.beta", "Nb.K.alpha", "Nb.K.beta", "Mo.K.alpha", "Mo.K.beta", "Mo.L.alpha", "Mo.L.beta", "Ru.K.alpha", "Ru.K.beta", "Ru.L.alpha", "Ru.L.beta", "Rh.K.alpha", "Rh.K.beta", "Rh.L.alpha", "Rh.L.beta", "Pd.K.alpha", "Pd.K.beta", "Pd.L.alpha", "Pd.L.beta", "Ag.K.alpha", "Ag.K.beta", "Ag.L.alpha", "Ag.L.beta", "Cd.K.alpha", "Cd.K.beta", "Cd.L.alpha", "Cd.L.beta", " In.K.alpha", "In.K.beta", "In.L.alpha", "Sn.K.alpha", "Sn.K.beta", "Sn.L.alpha", "Sn.L.beta", "Sb.K.alpha", "Sb.K.beta", "Sb.L.alpha", "Sb.L.beta", "Te.K.alpha", "Te.K.beta", "Te.L.alpha", "Te.L.beta", "I.K.alpha", "I.K.beta", "I.L.alpha", "I.L.beta", "Xe.K.alpha", "Xe.K.beta", "Xe.L.alpha", "Xe.L.beta", "Cs.K.alpha", "Cs.K.beta", "Cs.L.alpha", "Cs.L.beta", "Ba.K.alpha", "Ba.K.beta", "Ba.L.alpha", "Ba.L.beta", "La.K.alpha", "La.K.beta", "La.L.alpha", "La.L.beta", "Ce.K.alpha", "Ce.K.beta", "Ce.L.alpha", "Ce.L.beta", "Pr.K.alpha", "Pr.K.beta", "Pr.L.alpha", "Pr.L.beta", "Nd.K.alpha", "Nd.K.beta", "Nd.L.alpha", "Nd.L.beta", "Pm.L.alpha", "Pm.L.beta", "Sm.L.alpha", "Sm.L.beta", "Eu.L.alpha", "Eu.L.beta", "Gd.L.alpha", "Gd.L.beta", "Tb.L.alpha", "Tb.L.beta", "Dy.L.alpha", "Dy.L.beta", "Ho.L.alpha", "Ho.L.beta", "Er.L.alpha", "Er.L.beta", "Tm.L.alpha", "Tm.L.beta", "Yb.L.alpha", "Yb.L.beta", "Lu.L.alpha", "Lu.L.beta", "Hf.L.alpha", "Hf.L.beta", "Ta.L.alpha", "Ta.L.beta", "W.L.alpha", "W.L.beta", "Re.L.alpha", "Re.L.beta", "Os.L.alpha", "Os.L.beta", "Ir.L.alpha", "Ir.L.beta", "Pt.L.alpha", "Pt.L.beta", "Au.L.alpha", "Au.L.beta", "Hg.L.alpha", "Hg.L.beta", "Tl.L.alpha", "Tl.L.beta", "Pb.L.alpha", "Pb.L.beta", "Bi.L.alpha", "Bi.L.beta", "Po.L.alpha", "Po.L.beta", "At.L.alpha", "At.L.beta", "Rn.L.alpha", "Rn.L.beta", "Fr.L.alpha", "Fr.L.beta", "Ra.L.alpha", "Ra.L.beta", "Ac.L.alpha", "Ac.L.beta", "Th.L.alpha", "Th.L.beta", "Pa.L.alpha", "Pa.L.beta", "U.L.alpha", "U.L.beta", "Pu.L.alpha", "Pu.L.beta", "Au.M.line", "Hg.M.line", "Pb.M.line", "U.M.line")

standard <- c("Spectrum", "Ca.K.alpha", "Ti.K.alpha", "Fe.K.alpha")


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
    
    
    hold.cps <- subset(data$CPS, !(data$Energy < elementLine[7][1,]-0.02 | data$Energy > elementLine[8][1,]+0.02))
    hold.file <- subset(data$Spectrum, !(data$Energy < elementLine[7][1,]-0.02 | data$Energy > elementLine[8][1,]+0.02))
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

lukas.simp <- function(concentration.table, spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    lukas.intercept.table <- data.frame(rowSums(lukas.intercept.table.x[intercept.element.lines]))
    colnames(lukas.intercept.table) <- c("first")
    
    
    
    lukas.intercept <- lukas.intercept.table$first
    lukas.slope <- data.frame(lukas.slope.table[slope.element.lines])
    
    
    
    predict.frame.luk <- data.frame(concentration, ((1+intensity/(intensity+lukas.intercept))-lukas.intercept/(intensity+lukas.intercept)),lukas.slope)
    colnames(predict.frame.luk) <- c("Concentration", "Intensity", names(lukas.slope))
    
    
    
    predict.intensity.luk <- data.frame(predict.frame.luk$Intensity, lukas.slope)
    colnames(predict.intensity.luk) <- c("Intensity", names(lukas.slope))
    
    lukas.lm <- lm(Concentration~., data=predict.frame.luk)
    
    lukas.lm
    
    
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




lukas.tc <- function(concentration.table, spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    lukas.intercept.table.tc <- data.frame(rowSums(lukas.intercept.table.x[intercept.element.lines]))/total.counts$CPS
    colnames(lukas.intercept.table.tc) <- c("first")
    
    
    
    lukas.intercept.tc <- lukas.intercept.table.tc$first
    lukas.slope.tc <- data.frame(lukas.slope.table[slope.element.lines])/total.counts$CPS
    
    
    
    predict.frame.luk.tc <- data.frame(concentration, ((intensity/total.counts$CPS-lukas.intercept.tc)/(intensity/total.counts$CPS+lukas.intercept.tc)),lukas.slope.tc)
    colnames(predict.frame.luk.tc) <- c("Concentration", "Intensity", names(lukas.slope.tc))
    
    
    
    predict.intensity.luk.tc <- data.frame(predict.frame.luk.tc$Intensity, lukas.slope.tc)
    colnames(predict.intensity.luk.tc) <- c("Intensity", names(lukas.slope.tc))
    
    lukas.lm.tc <- lm(Concentration~., data=predict.frame.luk.tc)
    
    lukas.lm.tc

    
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

lukas.comp <- function(data, concentration.table, spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    lukas.intercept.table.comp <- data.frame(rowSums(lukas.intercept.table.x[intercept.element.lines]))/compton.frame.ag$Compton
    colnames(lukas.intercept.table.comp) <- c("first")
    
    
    
    lukas.intercept.comp <- lukas.intercept.table.comp$first
    lukas.slope.comp <- data.frame(lukas.slope.table[slope.element.lines])/compton.frame.ag$Compton
    
    
    
    
    predict.frame.luk.comp <- data.frame(concentration, ((1+intensity/compton.frame.ag$Compton)/(intensity/compton.frame.ag$Compton+lukas.intercept.comp)-lukas.intercept.comp/(intensity/compton.frame.ag$Compton+lukas.intercept.comp)),lukas.slope.comp)
    colnames(predict.frame.luk.comp) <- c("Concentration", "Intensity", names(lukas.slope.comp))
    
    
    
    predict.intensity.luk.comp <- data.frame(predict.frame.luk.comp$Intensity, lukas.slope.comp)
    colnames(predict.intensity.luk.comp) <- c("Intensity", names(lukas.slope.comp))
    
    lukas.lm.comp <- lm(Concentration~., data=predict.frame.luk.comp)
    
    lukas.lm.comp
    
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



lukas.simp.prep <- function(spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lukas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none)
    colnames(lukas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lukas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lukas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    lukas.intercept.table <- data.frame(rowSums(lukas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")]))
    colnames(lukas.intercept.table) <- c("first")
    
    
    
    lukas.intercept <- lukas.intercept.table$first
    lukas.slope <- data.frame(lukas.slope.table[,slope.element.lines])
    colnames(lukas.slope) <- slope.element.lines
    
    
    predict.frame.luk <- data.frame(((1+intensity/(intensity+lukas.intercept))-lukas.intercept/(intensity+lukas.intercept)),lukas.slope)
    colnames(predict.frame.luk) <- c("Intensity", names(lukas.slope))
    
    
    
    predict.intensity.luk <- data.frame(predict.frame.luk$Intensity, lukas.slope)
    colnames(predict.intensity.luk) <- c("Intensity", names(lukas.slope))
    
    predict.intensity.luk
    
    
}



lukas.tc.prep <- function(data, spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    total.counts <- aggregate(CPS~Spectrum, data=data, sum)
    colnames(total.counts) <- c("Spectrum", "CPS")
    
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lukas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none)
    colnames(lukas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lukas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lukas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    lukas.intercept.table.tc <- data.frame(rowSums(lukas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")]))/total.counts$CPS
    colnames(lukas.intercept.table.tc) <- c("first")
    
    
    
    lukas.intercept.tc <- lukas.intercept.table.tc$first
    lukas.slope.tc <- data.frame(lukas.slope.table[,slope.element.lines])/total.counts$CPS
    colnames(lukas.slope.tc) <- slope.element.lines
    
    
    test <- data.frame(lukas.slope.tc, total.counts$CPS)
    test2 <- data.frame(lukas.slope.tc, intensity)
    test3 <- data.frame(lukas.slope.tc, lukas.intercept.tc)
    
    
    predict.frame.luk.tc <- data.frame(((intensity/total.counts$CPS-lukas.intercept.tc)/(intensity/total.counts$CPS+lukas.intercept.tc)),lukas.slope.tc)
    colnames(predict.frame.luk.tc) <- c("Intensity", names(lukas.slope.tc))
    
    
    
    
    predict.intensity.luk.tc <- data.frame(predict.frame.luk.tc$Intensity, lukas.slope.tc)
    colnames(predict.intensity.luk.tc) <- c("Intensity", names(lukas.slope.tc))
    
    predict.intensity.luk.tc
}


lukas.comp.prep <- function(data, spectra.line.table, element.line, slope.element.lines, intercept.element.lines, norm.min, norm.max) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    
    compton.norm <- subset(data$CPS, !(data$Energy < norm.min | data$Energy > norm.max))
    compton.file <- subset(data$Spectrum, !(data$Energy < norm.min | data$Energy > norm.max))
    compton.frame <- data.frame(is.0(compton.norm, compton.file))
    colnames(compton.frame) <- c("Compton", "Spectrum")
    compton.frame.ag <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")
    colnames(compton.frame.ag) <- c("Spectrum", "Compton")
    
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lukas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none)
    colnames(lukas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lukas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lukas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    
    lukas.intercept.table.comp <- data.frame(rowSums(lukas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")]))/compton.frame.ag$Compton
    colnames(lukas.intercept.table.comp) <- c("first")
    
    
    
    lukas.intercept.comp <- lukas.intercept.table.comp$first
    lukas.slope.comp <- data.frame(lukas.slope.table[,slope.element.lines])/compton.frame.ag$Compton
    colnames(lukas.slope.comp) <- slope.element.lines
    
    
    predict.frame.luk.comp <- data.frame(((1+intensity/compton.frame.ag$Compton)/(intensity/compton.frame.ag$Compton+lukas.intercept.comp)-lukas.intercept.comp/(intensity/compton.frame.ag$Compton+lukas.intercept.comp)),lukas.slope.comp)
    colnames(predict.frame.luk.comp) <- c("Intensity", names(lukas.slope.comp))
    
    
    
    predict.intensity.luk.comp <- data.frame(predict.frame.luk.comp$Intensity, lukas.slope.comp)
    colnames(predict.intensity.luk.comp) <- c("Intensity", names(lukas.slope.comp))
    
    
    predict.intensity.luk.comp
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
    
    total.counts.net <- rowSums(spectra.line.table[length(spectra.line.table)])
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



lukas.simp.prep.net <- function(spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lukas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none)
    colnames(lukas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lukas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lukas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    lukas.intercept.table <- data.frame(rowSums(lukas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")]))
    colnames(lukas.intercept.table) <- c("first")
    
    
    
    lukas.intercept <- lukas.intercept.table$first
    lukas.slope <- data.frame(lukas.slope.table[,slope.element.lines])
    colnames(lukas.slope) <- slope.element.lines
    
    
    
    predict.frame.luk <- data.frame(((1+intensity/(intensity+lukas.intercept))-lukas.intercept/(intensity+lukas.intercept)),lukas.slope)
    colnames(predict.frame.luk) <- c("Intensity", names(lukas.slope))
    
    
    
    predict.intensity.luk <- data.frame(predict.frame.luk$Intensity, lukas.slope)
    colnames(predict.intensity.luk) <- c("Intensity", names(lukas.slope))
    
    predict.intensity.luk
    
    
}



lukas.tc.prep.net <- function(data, spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    total.counts.net <- rowSums(spectra.line.table[length(spectra.line.table)])
    total.counts <- data.frame(data$Spectrum, total.counts.net)
    colnames(total.counts) <- c("Spectrum", "CPS")
    
    
    
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lukas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none)
    colnames(lukas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lukas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lukas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    
    lukas.intercept.table.tc <- data.frame(rowSums(lukas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")]))/total.counts$CPS
    colnames(lukas.intercept.table.tc) <- c("first")
    
    
    
    
    lukas.intercept.tc <- lukas.intercept.table.tc$first
    lukas.slope.tc <- data.frame(lukas.slope.table[,slope.element.lines])/total.counts$CPS
    colnames(lukas.slope.tc) <- slope.element.lines
    
    
    predict.frame.luk.tc <- data.frame(((intensity/total.counts$CPS-lukas.intercept.tc)/(intensity/total.counts$CPS+lukas.intercept.tc)),lukas.slope.tc)
    colnames(predict.frame.luk.tc) <- c("Intensity", names(lukas.slope.tc))
    
    
    
    predict.intensity.luk.tc <- data.frame(predict.frame.luk.tc$Intensity, lukas.slope.tc)
    colnames(predict.intensity.luk.tc) <- c("Intensity", names(lukas.slope.tc))
    
    predict.intensity.luk.tc
}


lukas.comp.prep.net <- function(data, spectra.line.table, element.line, slope.element.lines, intercept.element.lines, norm.min, norm.max) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    
    
    compton.ag.fake.Spectrum <- data$Spectrum
    compton.ag.fake.Compton <- rep(1, length(data$Spectrum))
    compton.ag.fake <- data.frame(compton.ag.fake.Spectrum,compton.ag.fake.Compton)
    colnames(compton.ag.fake) <- c("Spectrum", "Compton")
    
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lukas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none)
    colnames(lukas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lukas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lukas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    
    lukas.intercept.table.comp <- data.frame(rowSums(lukas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")]))/compton.ag.fake$Compton
    colnames(lukas.intercept.table.comp) <- c("first")
    
    
    
    
    lukas.intercept.comp <- lukas.intercept.table.comp$first
    lukas.slope.comp <- data.frame(lukas.slope.table[,slope.element.lines])/compton.ag.fake$Compton
    colnames(lukas.slope.comp) <- slope.element.lines
    
    
    
    predict.frame.luk.comp <- data.frame(((1+intensity/compton.frame.ag$Compton)/(intensity/compton.frame.ag$Compton+lukas.intercept.comp)-lukas.intercept.comp/(intensity/compton.frame.ag$Compton+lukas.intercept.comp)),lukas.slope.comp)
    colnames(predict.frame.luk.comp) <- c("Intensity", names(lukas.slope.comp))
    
    
    
    predict.intensity.luk.comp <- data.frame(predict.frame.luk.comp$Intensity, lukas.slope.comp)
    colnames(predict.intensity.luk.comp) <- c("Intensity", names(lukas.slope.comp))
    
    
    predict.intensity.luk.comp
}


blank.data.frame <- data.frame(rep(0, length(standard)), rep(0, length(standard)), rep(0, length(standard)), rep(0, length(standard)), rep(0, length(standard)), rep(0, length(standard)), rep(0, length(standard)))
colnames(blank.data.frame) <- standard
