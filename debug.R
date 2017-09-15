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
    lukas.intercept.table.x <- data.frame(spectra.line.table, intercept.none)
    colnames(lukas.intercept.table.x) <- c(names(spectra.line.table), "None")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lukas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lukas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    lukas.intercept.table <- data.frame(rowSums(lukas.intercept.table.x[,c(intercept.element.lines, "None")]))
    colnames(lukas.intercept.table) <- c("first")
    
    
    
    lukas.intercept <- lukas.intercept.table$first
    lukas.slope <- data.frame(lukas.slope.table[,slope.element.lines])
    colnames(lukas.slope) <- slope.element.lines
    
    
    predict.frame.luk <- data.frame((intensity*lukas.intercept),lukas.slope)
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
    lukas.intercept.table.x <- data.frame(spectra.line.table, intercept.none)
    colnames(lukas.intercept.table.x) <- c(names(spectra.line.table), "None")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lukas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lukas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    lukas.intercept.table.tc <- data.frame(rowSums(lukas.intercept.table.x[,c(intercept.element.lines, "None")]))/total.counts$CPS
    colnames(lukas.intercept.table.tc) <- c("first")
    
    
    
    lukas.intercept.tc <- lukas.intercept.table.tc$first
    lukas.slope.tc <- data.frame(lukas.slope.table[,slope.element.lines])/total.counts$CPS
    colnames(lukas.slope.tc) <- slope.element.lines
    
    
    test <- data.frame(lukas.slope.tc, total.counts$CPS)
    test2 <- data.frame(lukas.slope.tc, intensity)
    test3 <- data.frame(lukas.slope.tc, lukas.intercept.tc)
    
    
    predict.frame.luk.tc <- data.frame((intensity/total.counts$CPS*lukas.intercept.tc),lukas.slope.tc)
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
    lukas.intercept.table.x <- data.frame(spectra.line.table, intercept.none)
    colnames(lukas.intercept.table.x) <- c(names(spectra.line.table), "None")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lukas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lukas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    
    lukas.intercept.table.comp <- data.frame(rowSums(lukas.intercept.table.x[,c(intercept.element.lines, "None")]))/compton.frame.ag$Compton
    colnames(lukas.intercept.table.comp) <- c("first")
    
    
    
    lukas.intercept.comp <- lukas.intercept.table.comp$first
    lukas.slope.comp <- data.frame(lukas.slope.table[,slope.element.lines])/compton.frame.ag$Compton
    colnames(lukas.slope.comp) <- slope.element.lines
    
    
    predict.frame.luk.comp <- data.frame((intensity/compton.frame.ag$Compton*lukas.intercept.comp),lukas.slope.comp)
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
    lukas.intercept.table.x <- data.frame(spectra.line.table, intercept.none)
    colnames(lukas.intercept.table.x) <- c(names(spectra.line.table), "None")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lukas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lukas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    lukas.intercept.table <- data.frame(rowSums(lukas.intercept.table.x[,c(intercept.element.lines, "None")]))
    colnames(lukas.intercept.table) <- c("first")
    
    
    
    lukas.intercept <- lukas.intercept.table$first
    lukas.slope <- data.frame(lukas.slope.table[,slope.element.lines])
    colnames(lukas.slope) <- slope.element.lines
    
    
    
    predict.frame.luk <- data.frame((intensity*lukas.intercept),lukas.slope)
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
    lukas.intercept.table.x <- data.frame(spectra.line.table, intercept.none)
    colnames(lukas.intercept.table.x) <- c(names(spectra.line.table), "None")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lukas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lukas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    
    lukas.intercept.table.tc <- data.frame(rowSums(lukas.intercept.table.x[,c(intercept.element.lines, "None")]))/total.counts$CPS
    colnames(lukas.intercept.table.tc) <- c("first")
    
    
    
    
    lukas.intercept.tc <- lukas.intercept.table.tc$first
    lukas.slope.tc <- data.frame(lukas.slope.table[,slope.element.lines])/total.counts$CPS
    colnames(lukas.slope.tc) <- slope.element.lines
    
    
    predict.frame.luk.tc <- data.frame((intensity/total.counts$CPS*lukas.intercept.tc),lukas.slope.tc)
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
    lukas.intercept.table.x <- data.frame(spectra.line.table, intercept.none)
    colnames(lukas.intercept.table.x) <- c(names(spectra.line.table), "None")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lukas.slope.table <- data.frame(spectra.line.table, slope.none)
    colnames(lukas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    
    lukas.intercept.table.comp <- data.frame(rowSums(lukas.intercept.table.x[,c(intercept.element.lines, "None")]))/compton.ag.fake$Compton
    colnames(lukas.intercept.table.comp) <- c("first")
    
    
    
    
    lukas.intercept.comp <- lukas.intercept.table.comp$first
    lukas.slope.comp <- data.frame(lukas.slope.table[,slope.element.lines])/compton.ag.fake$Compton
    colnames(lukas.slope.comp) <- slope.element.lines
    
    
    
    predict.frame.luk.comp <- data.frame((intensity/compton.ag.fake$Compton*lukas.intercept.comp),lukas.slope.comp)
    colnames(predict.frame.luk.comp) <- c("Intensity", names(lukas.slope.comp))
    
    
    
    predict.intensity.luk.comp <- data.frame(predict.frame.luk.comp$Intensity, lukas.slope.comp)
    colnames(predict.intensity.luk.comp) <- c("Intensity", names(lukas.slope.comp))
    
    
    predict.intensity.luk.comp
}


####Spectra Debug


linearTime <- readRDS("~/Desktop/Cal Variations/Spectra/linearTime.quant")
linearTC <- readRDS("~/Desktop/Cal Variations/Spectra/linearTC.quant")
linearCompton <- readRDS("~/Desktop/Cal Variations/Spectra/linearCompton.quant")

lukasTime <- readRDS("~/Desktop/Cal Variations/Spectra/lukasTime.quant")
lukasTC <- readRDS("~/Desktop/Cal Variations/Spectra/lukasTC.quant")
lukasCompton <- readRDS("~/Desktop/Cal Variations/Spectra/lukasCompton.quant")

###Net Debug
linearTime <- readRDS("~/Desktop/Cal Variations/Net/netLinearTime.quant")
linearTC <- readRDS("~/Desktop/Cal Variations/Net/netLinearTC.quant")
linearCompton <- readRDS("~/Desktop/Cal Variations/Net/netLinearCompton.quant")

lukasTime <- readRDS("~/Desktop/Cal Variations/Net/netLukasTime.quant")
lukasTC <- readRDS("~/Desktop/Cal Variations/Net/netLukasTC.quant")
lukasCompton <- readRDS("~/Desktop/Cal Variations/Net/netLukasCompton.quant")


valdata  <- linearTime$Spectra
count.table <- linearTime$Intensities
x <- "Ca.K12"

element.line <- x
spectra.line.table <- count.table
data <- valdata

the.cal <- linearTime[[6]]
timelinquant <- predict(
                    object=the.cal[[x]][[2]],
                    newdata=general.prep.net(
                        spectra.line.table=as.data.frame(
                            count.table
                            ),
                        element.line=x)
                )


the.cal <- linearTC[[6]]
tclinquant <- predict(
                    object=the.cal[[x]][[2]],
                    newdata=simple.tc.prep.net(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                            count.table
                            ),
                        element.line=x
                    )
                )
                

the.cal <- linearCompton[[6]]
complinquant <- predict(
                    object=the.cal[[x]][[2]],
                        newdata=simple.comp.prep.net(
                            data=valdata,
                            spectra.line.table=as.data.frame(
                                count.table
                                ),
                            element.line=x,
                            norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                            norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                            )
                )


the.cal <- lukasTime[[6]]
timelukquant <- predict(
                    object=the.cal[[x]][[2]],
                    newdata=lukas.simp.prep.net(
                        spectra.line.table=as.data.frame(
                            count.table
                            ),
                        element.line=x,
                        slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                        )
                 )

the.cal <- lukasTC[[6]]
tclukquant <- predict(
                    object=the.cal[[x]][[2]],
                    newdata=lukas.tc.prep.net(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                            count.table
                            ),
                        element.line=x,
                        slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                    )
                )

the.cal <- lukasCompton[[6]]
complukquant <-predict(
                    object=the.cal[[x]][[2]],
                    newdata=lukas.comp.prep.net(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                            count.table
                            ),
                        element.line=x,
                        slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                        norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                        norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                        )
                )







reportList <- reactiveValues()
reportList <- list()

observeEvent(input$createcalelement, {
    
    
    reportList <<- isolate(calPlotDownload())
    
    
})


observeEvent(input$createcal, {
    
    forReport <<- reportList
    
})




output$downloadReport <- downloadHandler(
filename <- function(){
    paste(input$calname, "pdf", sep=".")
},

content = function(file) {
    pdf(file = file)
    
    for (i in 1:length(forReport$reportList)) {
        print(forReport$reportList[[i]])
    }
    dev.off()
    
}
)
