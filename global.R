library(shiny)
library(ggplot2)
library(pbapply)
library(reshape2)
library(TTR)
library(dplyr)
library(shinyIncubator)
library(ggtern)
library(shinysky)
library(DT)




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



black.diamond <- read.csv("data/blackdiamond.csv", header=FALSE, sep=",")
black.diamond.melt <- read.csv(file="data/blackdiamondmelt.csv")

######Load lines
k.lines <- read.csv(file="data/K Line-Table 1.csv", sep=",")
l.lines <- read.csv(file="data/L Line-Table 1.csv", sep=",")

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

Li.absorption <- read.csv(file="data/Absorption-Li.csv")
Be.absorption <- read.csv(file="data/Absorption-Be.csv")
B.absorption <- read.csv(file="data/Absorption-B.csv")
C.absorption <- read.csv(file="data/Absorption-C.csv")
N.absorption <- read.csv(file="data/Absorption-N.csv")
O.absorption <- read.csv(file="data/Absorption-O.csv")
F.absorption <- read.csv(file="data/Absorption-F.csv")
Ne.absorption <- read.csv(file="data/Absorption-Ne.csv")
Na.absorption <- read.csv(file="data/Absorption-Na.csv")
Mg.absorption <- read.csv(file="data/Absorption-Mg.csv")
Al.absorption <- read.csv(file="data/Absorption-Al.csv")
Si.absorption <- read.csv(file="data/Absorption-Si.csv")
P.absorption <- read.csv(file="data/Absorption-P.csv")
S.absorption <- read.csv(file="data/Absorption-S.csv")
K.absorption <- read.csv(file="data/Absorption-K.csv")
Ca.absorption <- read.csv(file="data/Absorption-Ca.csv")
Sc.absorption <- read.csv(file="data/Absorption-Sc.csv")
Ti.absorption <- read.csv(file="data/Absorption-Ti.csv")
Cr.absorption <- read.csv(file="data/Absorption-Cr.csv")
Mn.absorption <- read.csv(file="data/Absorption-Mn.csv")
Fe.absorption <- read.csv(file="data/Absorption-Fe.csv")
Co.absorption <- read.csv(file="data/Absorption-Co.csv")
Ni.absorption <- read.csv(file="data/Absorption-Ni.csv")
Cu.absorption <- read.csv(file="data/Absorption-Cu.csv")
Zn.absorption <- read.csv(file="data/Absorption-Zn.csv")
Ga.absorption <- read.csv(file="data/Absorption-Ga.csv")
Ge.absorption <- read.csv(file="data/Absorption-Ge.csv")
As.absorption <- read.csv(file="data/Absorption-As.csv")
Se.absorption <- read.csv(file="data/Absorption-Se.csv")
Br.absorption <- read.csv(file="data/Absorption-Br.csv")
Kr.absorption <- read.csv(file="data/Absorption-Kr.csv")
Rb.absorption <- read.csv(file="data/Absorption-Rb.csv")
Sr.absorption <- read.csv(file="data/Absorption-Sr.csv")
Y.absorption <- read.csv(file="data/Absorption-Y.csv")
Zr.absorption <- read.csv(file="data/Absorption-Zr.csv")
Nb.absorption <- read.csv(file="data/Absorption-Nb.csv")
Mo.absorption <- read.csv(file="data/Absorption-Mo.csv")
Tc.absorption <- read.csv(file="data/Absorption-Tc.csv")
Ru.absorption <- read.csv(file="data/Absorption-Ru.csv")
Rh.absorption <- read.csv(file="data/Absorption-Rh.csv")
Pd.absorption <- read.csv(file="data/Absorption-Pd.csv")
Ag.absorption <- read.csv(file="data/Absorption-Ag.csv")
Cd.absorption <- read.csv(file="data/Absorption-Cd.csv")
In.absorption <- read.csv(file="data/Absorption-In.csv")
Sn.absorption <- read.csv(file="data/Absorption-Sn.csv")
Sb.absorption <- read.csv(file="data/Absorption-Sb.csv")
Te.absorption <- read.csv(file="data/Absorption-Te.csv")
I.absorption <- read.csv(file="data/Absorption-I.csv")
Xe.absorption <- read.csv(file="data/Absorption-Xe.csv")
Cs.absorption <- read.csv(file="data/Absorption-Cs.csv")
Ba.absorption <- read.csv(file="data/Absorption-Ba.csv")
La.absorption <- read.csv(file="data/Absorption-La.csv")
Ce.absorption <- read.csv(file="data/Absorption-Ce.csv")
Pr.absorption <- read.csv(file="data/Absorption-Pr.csv")
Nd.absorption <- read.csv(file="data/Absorption-Nd.csv")
Pm.absorption <- read.csv(file="data/Absorption-Pm.csv")
Sm.absorption <- read.csv(file="data/Absorption-Sm.csv")
Eu.absorption <- read.csv(file="data/Absorption-Eu.csv")
Gd.absorption <- read.csv(file="data/Absorption-Gd.csv")
Tb.absorption <- read.csv(file="data/Absorption-Tb.csv")
Dy.absorption <- read.csv(file="data/Absorption-Dy.csv")
Ho.absorption <- read.csv(file="data/Absorption-Ho.csv")
Er.absorption <- read.csv(file="data/Absorption-Er.csv")
Tm.absorption <- read.csv(file="data/Absorption-Tm.csv")
Yb.absorption <- read.csv(file="data/Absorption-Yb.csv")
Lu.absorption <- read.csv(file="data/Absorption-Lu.csv")
Hf.absorption <- read.csv(file="data/Absorption-Hf.csv")
Ta.absorption <- read.csv(file="data/Absorption-Ta.csv")
W.absorption <- read.csv(file="data/Absorption-W.csv")
Re.absorption <- read.csv(file="data/Absorption-Re.csv")
Os.absorption <- read.csv(file="data/Absorption-Os.csv")
Ir.absorption <- read.csv(file="data/Absorption-Ir.csv")
Pt.absorption <- read.csv(file="data/Absorption-Pt.csv")
Au.absorption <- read.csv(file="data/Absorption-Au.csv")
Hg.absorption <- read.csv(file="data/Absorption-Hg.csv")
Tl.absorption <- read.csv(file="data/Absorption-Tl.csv")
Pb.absorption <- read.csv(file="data/Absorption-Pb.csv")
Bi.absorption <- read.csv(file="data/Absorption-Bi.csv")
Po.absorption <- read.csv(file="data/Absorption-Po.csv")
At.absorption <- read.csv(file="data/Absorption-At.csv")
Rn.absorption <- read.csv(file="data/Absorption-Rn.csv")
Fr.absorption <- read.csv(file="data/Absorption-Fr.csv")
Ra.absorption <- read.csv(file="data/Absorption-Ra.csv")
Ac.absorption <- read.csv(file="data/Absorption-Ac.csv")
Th.absorption <- read.csv(file="data/Absorption-Th.csv")
Pa.absorption <- read.csv(file="data/Absorption-Pa.csv")
U.absorption <- read.csv(file="data/Absorption-U.csv")


spectra.line.fn <- function(data) {
    Ne.Ka.cps <- subset(data$CPS, !(data$Energy < Ne.K[2]-0.02 | data$Energy > Ne.K[1]+0.02))
    Ne.file <- subset(data$Spectrum, !(data$Energy < Ne.K[2]-0.02 | data$Energy > Ne.K[1]+0.02))
    Ne.Ka.frame <- data.frame(is.0(Ne.Ka.cps, Ne.file))
    colnames(Ne.Ka.frame) <- c("Counts", "Spectrum")
    Ne.Ka.ag <- aggregate(list(Ne.Ka.frame$Counts), by=list(Ne.Ka.frame$Spectrum), FUN="sum")
    colnames(Ne.Ka.ag) <- c("Spectrum", "Ne K-alpha")
    
    Ne.Kb.cps <- subset(data$CPS, !(data$Energy < Ne.K[3]-0.02 | data$Energy > Ne.K[3]+0.02))
    Ne.file <- subset(data$Spectrum, !(data$Energy < Ne.K[3]-0.02 | data$Energy > Ne.K[3]+0.02))
    Ne.Kb.frame <- data.frame(is.0(Ne.Kb.cps, Ne.file))
    colnames(Ne.Kb.frame) <- c("Counts", "Spectrum")
    Ne.Kb.ag <- aggregate(list(Ne.Kb.frame$Counts), by=list(Ne.Kb.frame$Spectrum), FUN="sum")
    colnames(Ne.Kb.ag) <- c("Spectrum", "Ne K-beta")
    
    Na.Ka.cps <- subset(data$CPS, !(data$Energy < Na.K[2]-0.02 | data$Energy > Na.K[1]+0.02))
    Na.file <- subset(data$Spectrum, !(data$Energy < Na.K[2]-0.02 | data$Energy > Na.K[1]+0.02))
    Na.Ka.frame <- data.frame(is.0(Na.Ka.cps, Na.file))
    colnames(Na.Ka.frame) <- c("Counts", "Spectrum")
    Na.Ka.ag <- aggregate(list(Na.Ka.frame$Counts), by=list(Na.Ka.frame$Spectrum), FUN="sum")
    colnames(Na.Ka.ag) <- c("Spectrum", "Na K-alpha")
    
    Na.Kb.cps <- subset(data$CPS, !(data$Energy < Na.K[3]-0.02 | data$Energy > Na.K[3]+0.02))
    Na.file <- subset(data$Spectrum, !(data$Energy < Na.K[3]-0.02 | data$Energy > Na.K[3]+0.02))
    Na.Kb.frame <- data.frame(is.0(Na.Kb.cps, Na.file))
    colnames(Na.Kb.frame) <- c("Counts", "Spectrum")
    Na.Kb.ag <- aggregate(list(Na.Kb.frame$Counts), by=list(Na.Kb.frame$Spectrum), FUN="sum")
    colnames(Na.Kb.ag) <- c("Spectrum", "Na K-beta")


    Mg.Ka.cps <- subset(data$CPS, !(data$Energy < Mg.K[2]-0.02 | data$Energy > Mg.K[1]+0.02))
    Mg.file <- subset(data$Spectrum, !(data$Energy < Mg.K[2]-0.02 | data$Energy > Mg.K[1]+0.02))
    Mg.Ka.frame <- data.frame(is.0(Mg.Ka.cps, Mg.file))
    colnames(Mg.Ka.frame) <- c("Counts", "Spectrum")
    Mg.Ka.ag <- aggregate(list(Mg.Ka.frame$Counts), by=list(Mg.Ka.frame$Spectrum), FUN="sum")
    colnames(Mg.Ka.ag) <- c("Spectrum", "Mg K-alpha")
    
    
    Mg.Kb.cps <- subset(data$CPS, !(data$Energy < Mg.K[3]-0.02 | data$Energy > Mg.K[3]+0.02))
    Mg.file <- subset(data$Spectrum, !(data$Energy < Mg.K[3]-0.02 | data$Energy > Mg.K[3]+0.02))
    Mg.Kb.frame <- data.frame(is.0(Mg.Kb.cps, Mg.file))
    colnames(Mg.Kb.frame) <- c("Counts", "Spectrum")
    Mg.Kb.ag <- aggregate(list(Mg.Kb.frame$Counts), by=list(Mg.Kb.frame$Spectrum), FUN="sum")
    colnames(Mg.Kb.ag) <- c("Spectrum", "Mg K-beta")

    
    Al.Ka.cps <- subset(data$CPS, !(data$Energy < Al.K[2]-0.02 | data$Energy > Al.K[1]+0.02))
    Al.file <- subset(data$Spectrum, !(data$Energy < Al.K[2]-0.02 | data$Energy > Al.K[1]+0.02))
    Al.Ka.frame <- data.frame(is.0(Al.Ka.cps, Al.file))
    colnames(Al.Ka.frame) <- c("Counts", "Spectrum")
    Al.Ka.ag <- aggregate(list(Al.Ka.frame$Counts), by=list(Al.Ka.frame$Spectrum), FUN="sum")
    colnames(Al.Ka.ag) <- c("Spectrum", "Al K-alpha")
    
    
    Al.Kb.cps <- subset(data$CPS, !(data$Energy < Al.K[3]-0.02 | data$Energy > Al.K[3]+0.02))
    Al.file <- subset(data$Spectrum, !(data$Energy < Al.K[3]-0.02 | data$Energy > Al.K[3]+0.02))
    Al.Kb.frame <- data.frame(is.0(Al.Kb.cps, Al.file))
    colnames(Al.Kb.frame) <- c("Counts", "Spectrum")
    Al.Kb.ag <- aggregate(list(Al.Kb.frame$Counts), by=list(Al.Kb.frame$Spectrum), FUN="sum")
    colnames(Al.Kb.ag) <- c("Spectrum", "Al K-beta")

    
    Si.Ka.cps <- subset(data$CPS, !(data$Energy < Si.K[2]-0.02 | data$Energy > Si.K[1]+0.02))
    Si.file <- subset(data$Spectrum, !(data$Energy < Si.K[2]-0.02 | data$Energy > Si.K[1]+0.02))
    Si.Ka.frame <- data.frame(is.0(Si.Ka.cps, Si.file))
    colnames(Si.Ka.frame) <- c("Counts", "Spectrum")
    Si.Ka.ag <- aggregate(list(Si.Ka.frame$Counts), by=list(Si.Ka.frame$Spectrum), FUN="sum")
    colnames(Si.Ka.ag) <- c("Spectrum", "Si K-alpha")
    
    
    Si.Kb.cps <- subset(data$CPS, !(data$Energy < Si.K[3]-0.02 | data$Energy > Si.K[3]+0.02))
    Si.file <- subset(data$Spectrum, !(data$Energy < Si.K[3]-0.02 | data$Energy > Si.K[3]+0.02))
    Si.Kb.frame <- data.frame(is.0(Si.Kb.cps, Si.file))
    colnames(Si.Kb.frame) <- c("Counts", "Spectrum")
    Si.Kb.ag <- aggregate(list(Si.Kb.frame$Counts), by=list(Si.Kb.frame$Spectrum), FUN="sum")
    colnames(Si.Kb.ag) <- c("Spectrum", "Si K-beta")
    
    
    P.Ka.cps <- subset(data$CPS, !(data$Energy < P.K[2]-0.02 | data$Energy > P.K[1]+0.02))
    P.file <- subset(data$Spectrum, !(data$Energy < P.K[2]-0.02 | data$Energy > P.K[1]+0.02))
    P.Ka.frame <- data.frame(is.0(P.Ka.cps, P.file))
    colnames(P.Ka.frame) <- c("Counts", "Spectrum")
    P.Ka.ag <- aggregate(list(P.Ka.frame$Counts), by=list(P.Ka.frame$Spectrum), FUN="sum")
    colnames(P.Ka.ag) <- c("Spectrum", "P K-alpha")
    
    P.Kb.cps <- subset(data$CPS, !(data$Energy < P.K[3]-0.02 | data$Energy > P.K[3]+0.02))
    P.file <- subset(data$Spectrum, !(data$Energy < P.K[3]-0.02 | data$Energy > P.K[3]+0.02))
    P.Kb.frame <- data.frame(is.0(P.Kb.cps, P.file))
    colnames(P.Kb.frame) <- c("Counts", "Spectrum")
    P.Kb.ag <- aggregate(list(P.Kb.frame$Counts), by=list(P.Kb.frame$Spectrum), FUN="sum")
    colnames(P.Kb.ag) <- c("Spectrum", "P K-beta")
    
    S.Ka.cps <- subset(data$CPS, !(data$Energy < S.K[2]-0.02 | data$Energy > S.K[1]+0.02))
    S.file <- subset(data$Spectrum, !(data$Energy < S.K[2]-0.02 | data$Energy > S.K[1]+0.02))
    S.Ka.frame <- data.frame(is.0(S.Ka.cps, S.file))
    colnames(S.Ka.frame) <- c("Counts", "Spectrum")
    S.Ka.ag <- aggregate(list(S.Ka.frame$Counts), by=list(S.Ka.frame$Spectrum), FUN="sum")
    colnames(S.Ka.ag) <- c("Spectrum", "S K-alpha")
    
    
    S.Kb.cps <- subset(data$CPS, !(data$Energy < S.K[3]-0.02 | data$Energy > S.K[3]+0.02))
    S.file <- subset(data$Spectrum, !(data$Energy < S.K[3]-0.02 | data$Energy > S.K[3]+0.02))
    S.Kb.frame <- data.frame(is.0(S.Kb.cps, S.file))
    colnames(S.Kb.frame) <- c("Counts", "Spectrum")
    S.Kb.ag <- aggregate(list(S.Kb.frame$Counts), by=list(S.Kb.frame$Spectrum), FUN="sum")
    colnames(S.Kb.ag) <- c("Spectrum", "S K-beta")

    
    Cl.Ka.cps <- subset(data$CPS, !(data$Energy < Cl.K[2]-0.02 | data$Energy > Cl.K[1]+0.02))
    Cl.file <- subset(data$Spectrum, !(data$Energy < Cl.K[2]-0.02 | data$Energy > Cl.K[1]+0.02))
    Cl.Ka.frame <- data.frame(is.0(Cl.Ka.cps, Cl.file))
    colnames(Cl.Ka.frame) <- c("Counts", "Spectrum")
    Cl.Ka.ag <- aggregate(list(Cl.Ka.frame$Counts), by=list(Cl.Ka.frame$Spectrum), FUN="sum")
    colnames(Cl.Ka.ag) <- c("Spectrum", "Cl K-alpha")
    
    Cl.Kb.cps <- subset(data$CPS, !(data$Energy < Cl.K[3]-0.02 | data$Energy > Cl.K[3]+0.02))
    Cl.file <- subset(data$Spectrum, !(data$Energy < Cl.K[3]-0.02 | data$Energy > Cl.K[3]+0.02))
    Cl.Kb.frame <- data.frame(is.0(Cl.Kb.cps, Cl.file))
    colnames(Cl.Kb.frame) <- c("Counts", "Spectrum")
    Cl.Kb.ag <- aggregate(list(Cl.Kb.frame$Counts), by=list(Cl.Kb.frame$Spectrum), FUN="sum")
    colnames(Cl.Kb.ag) <- c("Spectrum", "Cl K-beta")
    
    Ar.Ka.cps <- subset(data$CPS, !(data$Energy < Ar.K[2]-0.02 | data$Energy > Ar.K[1]+0.02))
    Ar.file <- subset(data$Spectrum, !(data$Energy < Ar.K[2]-0.02 | data$Energy > Ar.K[1]+0.02))
    Ar.Ka.frame <- data.frame(is.0(Ar.Ka.cps, Ar.file))
    colnames(Ar.Ka.frame) <- c("Counts", "Spectrum")
    Ar.Ka.ag <- aggregate(list(Ar.Ka.frame$Counts), by=list(Ar.Ka.frame$Spectrum), FUN="sum")
    colnames(Ar.Ka.ag) <- c("Spectrum", "Ar K-alpha")
    
    
    Ar.Kb.cps <- subset(data$CPS, !(data$Energy < Ar.K[3]-0.02 | data$Energy > Ar.K[3]+0.02))
    Ar.file <- subset(data$Spectrum, !(data$Energy < Ar.K[3]-0.02 | data$Energy > Ar.K[3]+0.02))
    Ar.Kb.frame <- data.frame(is.0(Ar.Kb.cps, Ar.file))
    colnames(Ar.Kb.frame) <- c("Counts", "Spectrum")
    Ar.Kb.ag <- aggregate(list(Ar.Kb.frame$Counts), by=list(Ar.Kb.frame$Spectrum), FUN="sum")
    colnames(Ar.Kb.ag) <- c("Spectrum", "Ar K-beta")

    
    K.Ka.cps <- subset(data$CPS, !(data$Energy < K.K[2]-0.02 | data$Energy > K.K[1]+0.02))
    K.file <- subset(data$Spectrum, !(data$Energy < K.K[2]-0.02 | data$Energy > K.K[1]+0.02))
    K.Ka.frame <- data.frame(is.0(K.Ka.cps, K.file))
    colnames(K.Ka.frame) <- c("Counts", "Spectrum")
    K.Ka.ag <- aggregate(list(K.Ka.frame$Counts), by=list(K.Ka.frame$Spectrum), FUN="sum")
    colnames(K.Ka.ag) <- c("Spectrum", "K K-alpha")
    
    K.Kb.cps <- subset(data$CPS, !(data$Energy < K.K[3]-0.02 | data$Energy > K.K[3]+0.02))
    K.file <- subset(data$Spectrum, !(data$Energy < K.K[3]-0.02 | data$Energy > K.K[3]+0.02))
    K.Kb.frame <- data.frame(is.0(K.Kb.cps, K.file))
    colnames(K.Kb.frame) <- c("Counts", "Spectrum")
    K.Kb.ag <- aggregate(list(K.Kb.frame$Counts), by=list(K.Kb.frame$Spectrum), FUN="sum")
    colnames(K.Kb.ag) <- c("Spectrum", "K K-beta")

    Ca.Ka.cps <- subset(data$CPS, !(data$Energy < Ca.K[2]-0.02 | data$Energy > Ca.K[1]+0.02))
    Ca.file <- subset(data$Spectrum, !(data$Energy < Ca.K[2]-0.02 | data$Energy > Ca.K[1]+0.02))
    Ca.Ka.frame <- data.frame(is.0(Ca.Ka.cps, Ca.file))
    colnames(Ca.Ka.frame) <- c("Counts", "Spectrum")
    Ca.Ka.ag <- aggregate(list(Ca.Ka.frame$Counts), by=list(Ca.Ka.frame$Spectrum), FUN="sum")
    colnames(Ca.Ka.ag) <- c("Spectrum", "Ca K-alpha")
    
    
    Ca.Kb.cps <- subset(data$CPS, !(data$Energy < Ca.K[3]-0.02 | data$Energy > Ca.K[3]+0.02))
    Ca.file <- subset(data$Spectrum, !(data$Energy < Ca.K[3]-0.02 | data$Energy > Ca.K[3]+0.02))
    Ca.Kb.frame <- data.frame(is.0(Ca.Kb.cps, Ca.file))
    colnames(Ca.Kb.frame) <- c("Counts", "Spectrum")
    Ca.Kb.ag <- aggregate(list(Ca.Kb.frame$Counts), by=list(Ca.Kb.frame$Spectrum), FUN="sum")
    colnames(Ca.Kb.ag) <- c("Spectrum", "Ca K-beta")
    
    Sc.Ka.cps <- subset(data$CPS, !(data$Energy < Sc.K[2]-0.02 | data$Energy > Sc.K[1]+0.02))
    Sc.file <- subset(data$Spectrum, !(data$Energy < Sc.K[2]-0.02 | data$Energy > Sc.K[1]+0.02))
    Sc.Ka.frame <- data.frame(is.0(Sc.Ka.cps, Sc.file))
    colnames(Sc.Ka.frame) <- c("Counts", "Spectrum")
    Sc.Ka.ag <- aggregate(list(Sc.Ka.frame$Counts), by=list(Sc.Ka.frame$Spectrum), FUN="sum")
    colnames(Sc.Ka.ag) <- c("Spectrum", "Sc K-alpha")
    
    Sc.Kb.cps <- subset(data$CPS, !(data$Energy < Sc.K[3]-0.02 | data$Energy > Sc.K[3]+0.02))
    Sc.file <- subset(data$Spectrum, !(data$Energy < Sc.K[3]-0.02 | data$Energy > Sc.K[3]+0.02))
    Sc.Kb.frame <- data.frame(is.0(Sc.Kb.cps, Sc.file))
    colnames(Sc.Kb.frame) <- c("Counts", "Spectrum")
    Sc.Kb.ag <- aggregate(list(Sc.Kb.frame$Counts), by=list(Sc.Kb.frame$Spectrum), FUN="sum")
    colnames(Sc.Kb.ag) <- c("Spectrum", "Sc K-beta")

    Ti.Ka.cps <- subset(data$CPS, !(data$Energy < Ti.K[2]-0.02 | data$Energy > Ti.K[1]+0.02))
    Ti.file <- subset(data$Spectrum, !(data$Energy < Ti.K[2]-0.02 | data$Energy > Ti.K[1]+0.02))
    Ti.Ka.frame <- data.frame(is.0(Ti.Ka.cps, Ti.file))
    colnames(Ti.Ka.frame) <- c("Counts", "Spectrum")
    Ti.Ka.ag <- aggregate(list(Ti.Ka.frame$Counts), by=list(Ti.Ka.frame$Spectrum), FUN="sum")
    colnames(Ti.Ka.ag) <- c("Spectrum", "Ti K-alpha")
    
    Ti.Kb.cps <- subset(data$CPS, !(data$Energy < Ti.K[3]-0.02 | data$Energy > Ti.K[3]+0.02))
    Ti.file <- subset(data$Spectrum, !(data$Energy < Ti.K[3]-0.02 | data$Energy > Ti.K[3]+0.02))
    Ti.Kb.frame <- data.frame(is.0(Ti.Kb.cps, Ti.file))
    colnames(Ti.Kb.frame) <- c("Counts", "Spectrum")
    Ti.Kb.ag <- aggregate(list(Ti.Kb.frame$Counts), by=list(Ti.Kb.frame$Spectrum), FUN="sum")
    colnames(Ti.Kb.ag) <- c("Spectrum", "Ti K-beta")
    
    V.Ka.cps <- subset(data$CPS, !(data$Energy < V.K[2]-0.02 | data$Energy > V.K[1]+0.02))
    V.file <- subset(data$Spectrum, !(data$Energy < V.K[2]-0.02 | data$Energy > V.K[1]+0.02))
    V.Ka.frame <- data.frame(is.0(V.Ka.cps, V.file))
    colnames(V.Ka.frame) <- c("Counts", "Spectrum")
    V.Ka.ag <- aggregate(list(V.Ka.frame$Counts), by=list(V.Ka.frame$Spectrum), FUN="sum")
    colnames(V.Ka.ag) <- c("Spectrum", "V K-alpha")
    
    
    V.Kb.cps <- subset(data$CPS, !(data$Energy < V.K[3]-0.02 | data$Energy > V.K[3]+0.02))
    V.file <- subset(data$Spectrum, !(data$Energy < V.K[3]-0.02 | data$Energy > V.K[3]+0.02))
    V.Kb.frame <- data.frame(is.0(V.Kb.cps, V.file))
    colnames(V.Kb.frame) <- c("Counts", "Spectrum")
    V.Kb.ag <- aggregate(list(V.Kb.frame$Counts), by=list(V.Kb.frame$Spectrum), FUN="sum")
    colnames(V.Kb.ag) <- c("Spectrum", "V K-beta")
    
    Cr.Ka.cps <- subset(data$CPS, !(data$Energy < Cr.K[2]-0.02 | data$Energy > Cr.K[1]+0.02))
    Cr.file <- subset(data$Spectrum, !(data$Energy < Cr.K[2]-0.02 | data$Energy > Cr.K[1]+0.02))
    Cr.Ka.frame <- data.frame(is.0(Cr.Ka.cps, Cr.file))
    colnames(Cr.Ka.frame) <- c("Counts", "Spectrum")
    Cr.Ka.ag <- aggregate(list(Cr.Ka.frame$Counts), by=list(Cr.Ka.frame$Spectrum), FUN="sum")
    colnames(Cr.Ka.ag) <- c("Spectrum", "Cr K-alpha")
    
    Cr.Kb.cps <- subset(data$CPS, !(data$Energy < Cr.K[3]-0.02 | data$Energy > Cr.K[3]+0.02))
    Cr.file <- subset(data$Spectrum, !(data$Energy < Cr.K[3]-0.02 | data$Energy > Cr.K[3]+0.02))
    Cr.Kb.frame <- data.frame(is.0(Cr.Kb.cps, Cr.file))
    colnames(Cr.Kb.frame) <- c("Counts", "Spectrum")
    Cr.Kb.ag <- aggregate(list(Cr.Kb.frame$Counts), by=list(Cr.Kb.frame$Spectrum), FUN="sum")
    colnames(Cr.Kb.ag) <- c("Spectrum", "Cr K-beta")

    Mn.Ka.cps <- subset(data$CPS, !(data$Energy < Mn.K[2]-0.02 | data$Energy > Mn.K[1]+0.02))
    Mn.file <- subset(data$Spectrum, !(data$Energy < Mn.K[2]-0.02 | data$Energy > Mn.K[1]+0.02))
    Mn.Ka.frame <- data.frame(is.0(Mn.Ka.cps, Mn.file))
    colnames(Mn.Ka.frame) <- c("Counts", "Spectrum")
    Mn.Ka.ag <- aggregate(list(Mn.Ka.frame$Counts), by=list(Mn.Ka.frame$Spectrum), FUN="sum")
    colnames(Mn.Ka.ag) <- c("Spectrum", "Mn K-alpha")
    
    Mn.Kb.cps <- subset(data$CPS, !(data$Energy < Mn.K[3]-0.02 | data$Energy > Mn.K[3]+0.02))
    Mn.file <- subset(data$Spectrum, !(data$Energy < Mn.K[3]-0.02 | data$Energy > Mn.K[3]+0.02))
    Mn.Kb.frame <- data.frame(is.0(Mn.Kb.cps, Mn.file))
    colnames(Mn.Kb.frame) <- c("Counts", "Spectrum")
    Mn.Kb.ag <- aggregate(list(Mn.Kb.frame$Counts), by=list(Mn.Kb.frame$Spectrum), FUN="sum")
    colnames(Mn.Kb.ag) <- c("Spectrum", "Mn K-beta")

    Fe.Ka.cps <- subset(data$CPS, !(data$Energy < Fe.K[2]-0.02 | data$Energy > Fe.K[1]+0.02))
    Fe.file <- subset(data$Spectrum, !(data$Energy < Fe.K[2]-0.02 | data$Energy > Fe.K[1]+0.02))
    Fe.Ka.frame <- data.frame(is.0(Fe.Ka.cps, Fe.file))
    colnames(Fe.Ka.frame) <- c("Counts", "Spectrum")
    Fe.Ka.ag <- aggregate(list(Fe.Ka.frame$Counts), by=list(Fe.Ka.frame$Spectrum), FUN="sum")
    colnames(Fe.Ka.ag) <- c("Spectrum", "Fe K-alpha")
    
    Fe.Kb.cps <- subset(data$CPS, !(data$Energy < Fe.K[3]-0.02 | data$Energy > Fe.K[3]+0.02))
    Fe.file <- subset(data$Spectrum, !(data$Energy < Fe.K[3]-0.02 | data$Energy > Fe.K[3]+0.02))
    Fe.Kb.frame <- data.frame(is.0(Fe.Kb.cps, Fe.file))
    colnames(Fe.Kb.frame) <- c("Counts", "Spectrum")
    Fe.Kb.ag <- aggregate(list(Fe.Kb.frame$Counts), by=list(Fe.Kb.frame$Spectrum), FUN="sum")
    colnames(Fe.Kb.ag) <- c("Spectrum", "Fe K-beta")
    
    Co.Ka.cps <- subset(data$CPS, !(data$Energy < Co.K[2]-0.02 | data$Energy > Co.K[1]+0.02))
    Co.file <- subset(data$Spectrum, !(data$Energy < Co.K[2]-0.02 | data$Energy > Co.K[1]+0.02))
    Co.Ka.frame <- data.frame(is.0(Co.Ka.cps, Co.file))
    colnames(Co.Ka.frame) <- c("Counts", "Spectrum")
    Co.Ka.ag <- aggregate(list(Co.Ka.frame$Counts), by=list(Co.Ka.frame$Spectrum), FUN="sum")
    colnames(Co.Ka.ag) <- c("Spectrum", "Co K-alpha")
    
    Co.Kb.cps <- subset(data$CPS, !(data$Energy < Co.K[3]-0.02 | data$Energy > Co.K[3]+0.02))
    Co.file <- subset(data$Spectrum, !(data$Energy < Co.K[3]-0.02 | data$Energy > Co.K[3]+0.02))
    Co.Kb.frame <- data.frame(is.0(Co.Kb.cps, Co.file))
    colnames(Co.Kb.frame) <- c("Counts", "Spectrum")
    Co.Kb.ag <- aggregate(list(Co.Kb.frame$Counts), by=list(Co.Kb.frame$Spectrum), FUN="sum")
    colnames(Co.Kb.ag) <- c("Spectrum", "Co K-beta")

    Ni.Ka.cps <- subset(data$CPS, !(data$Energy < Ni.K[2]-0.02 | data$Energy > Ni.K[1]+0.02))
    Ni.file <- subset(data$Spectrum, !(data$Energy < Ni.K[2]-0.02 | data$Energy > Ni.K[1]+0.02))
    Ni.Ka.frame <- data.frame(is.0(Ni.Ka.cps, Ni.file))
    colnames(Ni.Ka.frame) <- c("Counts", "Spectrum")
    Ni.Ka.ag <- aggregate(list(Ni.Ka.frame$Counts), by=list(Ni.Ka.frame$Spectrum), FUN="sum")
    colnames(Ni.Ka.ag) <- c("Spectrum", "Ni K-alpha")
    
    Ni.Kb.cps <- subset(data$CPS, !(data$Energy < Ni.K[3]-0.02 | data$Energy > Ni.K[3]+0.02))
    Ni.file <- subset(data$Spectrum, !(data$Energy < Ni.K[3]-0.02 | data$Energy > Ni.K[3]+0.02))
    Ni.Kb.frame <- data.frame(is.0(Ni.Kb.cps, Ni.file))
    colnames(Ni.Kb.frame) <- c("Counts", "Spectrum")
    Ni.Kb.ag <- aggregate(list(Ni.Kb.frame$Counts), by=list(Ni.Kb.frame$Spectrum), FUN="sum")
    colnames(Ni.Kb.ag) <- c("Spectrum", "Ni K-beta")
    
    Cu.Ka.cps <- subset(data$CPS, !(data$Energy < Cu.K[2]-0.02 | data$Energy > Cu.K[1]+0.02))
    Cu.file <- subset(data$Spectrum, !(data$Energy < Cu.K[2]-0.02 | data$Energy > Cu.K[1]+0.02))
    Cu.Ka.frame <- data.frame(is.0(Cu.Ka.cps, Cu.file))
    colnames(Cu.Ka.frame) <- c("Counts", "Spectrum")
    Cu.Ka.ag <- aggregate(list(Cu.Ka.frame$Counts), by=list(Cu.Ka.frame$Spectrum), FUN="sum")
    colnames(Cu.Ka.ag) <- c("Spectrum", "Cu K-alpha")
    
    Cu.Kb.cps <- subset(data$CPS, !(data$Energy < Cu.K[3]-0.02 | data$Energy > Cu.K[3]+0.02))
    Cu.file <- subset(data$Spectrum, !(data$Energy < Cu.K[3]-0.02 | data$Energy > Cu.K[3]+0.02))
    Cu.Kb.frame <- data.frame(is.0(Cu.Kb.cps, Cu.file))
    colnames(Cu.Kb.frame) <- c("Counts", "Spectrum")
    Cu.Kb.ag <- aggregate(list(Cu.Kb.frame$Counts), by=list(Cu.Kb.frame$Spectrum), FUN="sum")
    colnames(Cu.Kb.ag) <- c("Spectrum", "Cu K-beta")

    Zn.Ka.cps <- subset(data$CPS, !(data$Energy < Zn.K[2]-0.02 | data$Energy > Zn.K[1]+0.02))
    Zn.file <- subset(data$Spectrum, !(data$Energy < Zn.K[2]-0.02 | data$Energy > Zn.K[1]+0.02))
    Zn.Ka.frame <- data.frame(is.0(Zn.Ka.cps, Zn.file))
    colnames(Zn.Ka.frame) <- c("Counts", "Spectrum")
    Zn.Ka.ag <- aggregate(list(Zn.Ka.frame$Counts), by=list(Zn.Ka.frame$Spectrum), FUN="sum")
    colnames(Zn.Ka.ag) <- c("Spectrum", "Zn K-alpha")
    
    Zn.Kb.cps <- subset(data$CPS, !(data$Energy < Zn.K[3]-0.02 | data$Energy > Zn.K[3]+0.02))
    Zn.file <- subset(data$Spectrum, !(data$Energy < Zn.K[3]-0.02 | data$Energy > Zn.K[3]+0.02))
    Zn.Kb.frame <- data.frame(is.0(Zn.Kb.cps, Zn.file))
    colnames(Zn.Kb.frame) <- c("Counts", "Spectrum")
    Zn.Kb.ag <- aggregate(list(Zn.Kb.frame$Counts), by=list(Zn.Kb.frame$Spectrum), FUN="sum")
    colnames(Zn.Kb.ag) <- c("Spectrum", "Zn K-beta")
    
    Ga.Ka.cps <- subset(data$CPS, !(data$Energy < Ga.K[2]-0.02 | data$Energy > Ga.K[1]+0.02))
    Ga.file <- subset(data$Spectrum, !(data$Energy < Ga.K[2]-0.02 | data$Energy > Ga.K[1]+0.02))
    Ga.Ka.frame <- data.frame(is.0(Ga.Ka.cps, Ga.file))
    colnames(Ga.Ka.frame) <- c("Counts", "Spectrum")
    Ga.Ka.ag <- aggregate(list(Ga.Ka.frame$Counts), by=list(Ga.Ka.frame$Spectrum), FUN="sum")
    colnames(Ga.Ka.ag) <- c("Spectrum", "Ga K-alpha")
    
    Ga.Kb.cps <- subset(data$CPS, !(data$Energy < Ga.K[3]-0.02 | data$Energy > Ga.K[3]+0.02))
    Ga.file <- subset(data$Spectrum, !(data$Energy < Ga.K[3]-0.02 | data$Energy > Ga.K[3]+0.02))
    Ga.Kb.frame <- data.frame(is.0(Ga.Kb.cps, Ga.file))
    colnames(Ga.Kb.frame) <- c("Counts", "Spectrum")
    Ga.Kb.ag <- aggregate(list(Ga.Kb.frame$Counts), by=list(Ga.Kb.frame$Spectrum), FUN="sum")
    colnames(Ga.Kb.ag) <- c("Spectrum", "Ga K-beta")
    
    Ge.Ka.cps <- subset(data$CPS, !(data$Energy < Ge.K[2]-0.02 | data$Energy > Ge.K[1]+0.02))
    Ge.file <- subset(data$Spectrum, !(data$Energy < Ge.K[2]-0.02 | data$Energy > Ge.K[1]+0.02))
    Ge.Ka.frame <- data.frame(is.0(Ge.Ka.cps, Ge.file))
    colnames(Ge.Ka.frame) <- c("Counts", "Spectrum")
    Ge.Ka.ag <- aggregate(list(Ge.Ka.frame$Counts), by=list(Ge.Ka.frame$Spectrum), FUN="sum")
    colnames(Ge.Ka.ag) <- c("Spectrum", "Ge K-alpha")
    
    Ge.Kb.cps <- subset(data$CPS, !(data$Energy < Ge.K[3]-0.02 | data$Energy > Ge.K[3]+0.02))
    Ge.file <- subset(data$Spectrum, !(data$Energy < Ge.K[3]-0.02 | data$Energy > Ge.K[3]+0.02))
    Ge.Kb.frame <- data.frame(is.0(Ge.Kb.cps, Ge.file))
    colnames(Ge.Kb.frame) <- c("Counts", "Spectrum")
    Ge.Kb.ag <- aggregate(list(Ge.Kb.frame$Counts), by=list(Ge.Kb.frame$Spectrum), FUN="sum")
    colnames(Ge.Kb.ag) <- c("Spectrum", "Ge K-beta")
    
    As.Ka.cps <- subset(data$CPS, !(data$Energy < As.K[2]-0.02 | data$Energy > As.K[1]+0.02))
    As.file <- subset(data$Spectrum, !(data$Energy < As.K[2]-0.02 | data$Energy > As.K[1]+0.02))
    As.Ka.frame <- data.frame(is.0(As.Ka.cps, As.file))
    colnames(As.Ka.frame) <- c("Counts", "Spectrum")
    As.Ka.ag <- aggregate(list(As.Ka.frame$Counts), by=list(As.Ka.frame$Spectrum), FUN="sum")
    colnames(As.Ka.ag) <- c("Spectrum", "As K-alpha")
    
    As.Kb.cps <- subset(data$CPS, !(data$Energy < As.K[3]-0.02 | data$Energy > As.K[3]+0.02))
    As.file <- subset(data$Spectrum, !(data$Energy < As.K[3]-0.02 | data$Energy > As.K[3]+0.02))
    As.Kb.frame <- data.frame(is.0(As.Kb.cps, As.file))
    colnames(As.Kb.frame) <- c("Counts", "Spectrum")
    As.Kb.ag <- aggregate(list(As.Kb.frame$Counts), by=list(As.Kb.frame$Spectrum), FUN="sum")
    colnames(As.Kb.ag) <- c("Spectrum", "As K-beta")
    
    Se.Ka.cps <- subset(data$CPS, !(data$Energy < Se.K[2]-0.02 | data$Energy > Se.K[1]+0.02))
    Se.file <- subset(data$Spectrum, !(data$Energy < Se.K[2]-0.02 | data$Energy > Se.K[1]+0.02))
    Se.Ka.frame <- data.frame(is.0(Se.Ka.cps, Se.file))
    colnames(Se.Ka.frame) <- c("Counts", "Spectrum")
    Se.Ka.ag <- aggregate(list(Se.Ka.frame$Counts), by=list(Se.Ka.frame$Spectrum), FUN="sum")
    colnames(Se.Ka.ag) <- c("Spectrum", "Se K-alpha")
    
    Se.Kb.cps <- subset(data$CPS, !(data$Energy < Se.K[3]-0.02 | data$Energy > Se.K[3]+0.02))
    Se.file <- subset(data$Spectrum, !(data$Energy < Se.K[3]-0.02 | data$Energy > Se.K[3]+0.02))
    Se.Kb.frame <- data.frame(is.0(Se.Kb.cps, Se.file))
    colnames(Se.Kb.frame) <- c("Counts", "Spectrum")
    Se.Kb.ag <- aggregate(list(Se.Kb.frame$Counts), by=list(Se.Kb.frame$Spectrum), FUN="sum")
    colnames(Se.Kb.ag) <- c("Spectrum", "Se K-beta")
    
    Br.Ka.cps <- subset(data$CPS, !(data$Energy < Br.K[2]-0.02 | data$Energy > Br.K[1]+0.02))
    Br.file <- subset(data$Spectrum, !(data$Energy < Br.K[2]-0.02 | data$Energy > Br.K[1]+0.02))
    Br.Ka.frame <- data.frame(is.0(Br.Ka.cps, Br.file))
    colnames(Br.Ka.frame) <- c("Counts", "Spectrum")
    Br.Ka.ag <- aggregate(list(Br.Ka.frame$Counts), by=list(Br.Ka.frame$Spectrum), FUN="sum")
    colnames(Br.Ka.ag) <- c("Spectrum", "Br K-alpha")
    
    Br.Kb.cps <- subset(data$CPS, !(data$Energy < Br.K[3]-0.02 | data$Energy > Br.K[3]+0.02))
    Br.file <- subset(data$Spectrum, !(data$Energy < Br.K[3]-0.02 | data$Energy > Br.K[3]+0.02))
    Br.Kb.frame <- data.frame(is.0(Br.Kb.cps, Br.file))
    colnames(Br.Kb.frame) <- c("Counts", "Spectrum")
    Br.Kb.ag <- aggregate(list(Br.Kb.frame$Counts), by=list(Br.Kb.frame$Spectrum), FUN="sum")
    colnames(Br.Kb.ag) <- c("Spectrum", "Br K-beta")
    
    Kr.Ka.cps <- subset(data$CPS, !(data$Energy < Kr.K[2]-0.02 | data$Energy > Kr.K[1]+0.02))
    Kr.file <- subset(data$Spectrum, !(data$Energy < Kr.K[2]-0.02 | data$Energy > Kr.K[1]+0.02))
    Kr.Ka.frame <- data.frame(is.0(Kr.Ka.cps, Kr.file))
    colnames(Kr.Ka.frame) <- c("Counts", "Spectrum")
    Kr.Ka.ag <- aggregate(list(Kr.Ka.frame$Counts), by=list(Kr.Ka.frame$Spectrum), FUN="sum")
    colnames(Kr.Ka.ag) <- c("Spectrum", "Kr K-alpha")
    
    Kr.Kb.cps <- subset(data$CPS, !(data$Energy < Kr.K[3]-0.02 | data$Energy > Kr.K[3]+0.02))
    Kr.file <- subset(data$Spectrum, !(data$Energy < Kr.K[3]-0.02 | data$Energy > Kr.K[3]+0.02))
    Kr.Kb.frame <- data.frame(is.0(Kr.Kb.cps, Kr.file))
    colnames(Kr.Kb.frame) <- c("Counts", "Spectrum")
    Kr.Kb.ag <- aggregate(list(Kr.Kb.frame$Counts), by=list(Kr.Kb.frame$Spectrum), FUN="sum")
    colnames(Kr.Kb.ag) <- c("Spectrum", "Kr K-beta")
    
    Rb.Ka.cps <- subset(data$CPS, !(data$Energy < Rb.K[2]-0.02 | data$Energy > Rb.K[1]+0.02))
    Rb.file <- subset(data$Spectrum, !(data$Energy < Rb.K[2]-0.02 | data$Energy > Rb.K[1]+0.02))
    Rb.Ka.frame <- data.frame(is.0(Rb.Ka.cps, Rb.file))
    colnames(Rb.Ka.frame) <- c("Counts", "Spectrum")
    Rb.Ka.ag <- aggregate(list(Rb.Ka.frame$Counts), by=list(Rb.Ka.frame$Spectrum), FUN="sum")
    colnames(Rb.Ka.ag) <- c("Spectrum", "Rb K-alpha")
    
    Rb.Kb.cps <- subset(data$CPS, !(data$Energy < Rb.K[3]-0.02 | data$Energy > Rb.K[3]+0.02))
    Rb.file <- subset(data$Spectrum, !(data$Energy < Rb.K[3]-0.02 | data$Energy > Rb.K[3]+0.02))
    Rb.Kb.frame <- data.frame(is.0(Rb.Kb.cps, Rb.file))
    colnames(Rb.Kb.frame) <- c("Counts", "Spectrum")
    Rb.Kb.ag <- aggregate(list(Rb.Kb.frame$Counts), by=list(Rb.Kb.frame$Spectrum), FUN="sum")
    colnames(Rb.Kb.ag) <- c("Spectrum", "Rb K-beta")
    
    Sr.Ka.cps <- subset(data$CPS, !(data$Energy < Sr.K[2]-0.02 | data$Energy > Sr.K[1]+0.02))
    Sr.file <- subset(data$Spectrum, !(data$Energy < Sr.K[2]-0.02 | data$Energy > Sr.K[1]+0.02))
    Sr.Ka.frame <- data.frame(is.0(Sr.Ka.cps, Sr.file))
    colnames(Sr.Ka.frame) <- c("Counts", "Spectrum")
    Sr.Ka.ag <- aggregate(list(Sr.Ka.frame$Counts), by=list(Sr.Ka.frame$Spectrum), FUN="sum")
    colnames(Sr.Ka.ag) <- c("Spectrum", "Sr K-alpha")
    
    Sr.Kb.cps <- subset(data$CPS, !(data$Energy < Sr.K[3]-0.02 | data$Energy > Sr.K[3]+0.02))
    Sr.file <- subset(data$Spectrum, !(data$Energy < Sr.K[3]-0.02 | data$Energy > Sr.K[3]+0.02))
    Sr.Kb.frame <- data.frame(is.0(Sr.Kb.cps, Sr.file))
    colnames(Sr.Kb.frame) <- c("Counts", "Spectrum")
    Sr.Kb.ag <- aggregate(list(Sr.Kb.frame$Counts), by=list(Sr.Kb.frame$Spectrum), FUN="sum")
    colnames(Sr.Kb.ag) <- c("Spectrum", "Sr K-beta")
    
    Y.Ka.cps <- subset(data$CPS, !(data$Energy < Y.K[2]-0.02 | data$Energy > Y.K[1]+0.02))
    Y.file <- subset(data$Spectrum, !(data$Energy < Y.K[2]-0.02 | data$Energy > Y.K[1]+0.02))
    Y.Ka.frame <- data.frame(is.0(Y.Ka.cps, Y.file))
    colnames(Y.Ka.frame) <- c("Counts", "Spectrum")
    Y.Ka.ag <- aggregate(list(Y.Ka.frame$Counts), by=list(Y.Ka.frame$Spectrum), FUN="sum")
    colnames(Y.Ka.ag) <- c("Spectrum", "Y K-alpha")
    
    Y.Kb.cps <- subset(data$CPS, !(data$Energy < Y.K[3]-0.02 | data$Energy > Y.K[3]+0.02))
    Y.file <- subset(data$Spectrum, !(data$Energy < Y.K[3]-0.02 | data$Energy > Y.K[3]+0.02))
    Y.Kb.frame <- data.frame(is.0(Y.Kb.cps, Y.file))
    colnames(Y.Kb.frame) <- c("Counts", "Spectrum")
    Y.Kb.ag <- aggregate(list(Y.Kb.frame$Counts), by=list(Y.Kb.frame$Spectrum), FUN="sum")
    colnames(Y.Kb.ag) <- c("Spectrum", "Y K-beta")
    
    Zr.Ka.cps <- subset(data$CPS, !(data$Energy < Zr.K[2]-0.02 | data$Energy > Zr.K[1]+0.02))
    Zr.file <- subset(data$Spectrum, !(data$Energy < Zr.K[2]-0.02 | data$Energy > Zr.K[1]+0.02))
    Zr.Ka.frame <- data.frame(is.0(Zr.Ka.cps, Zr.file))
    colnames(Zr.Ka.frame) <- c("Counts", "Spectrum")
    Zr.Ka.ag <- aggregate(list(Zr.Ka.frame$Counts), by=list(Zr.Ka.frame$Spectrum), FUN="sum")
    colnames(Zr.Ka.ag) <- c("Spectrum", "Zr K-alpha")
    
    Zr.Kb.cps <- subset(data$CPS, !(data$Energy < Zr.K[3]-0.02 | data$Energy > Zr.K[3]+0.02))
    Zr.file <- subset(data$Spectrum, !(data$Energy < Zr.K[3]-0.02 | data$Energy > Zr.K[3]+0.02))
    Zr.Kb.frame <- data.frame(is.0(Zr.Kb.cps, Zr.file))
    colnames(Zr.Kb.frame) <- c("Counts", "Spectrum")
    Zr.Kb.ag <- aggregate(list(Zr.Kb.frame$Counts), by=list(Zr.Kb.frame$Spectrum), FUN="sum")
    colnames(Zr.Kb.ag) <- c("Spectrum", "Zr K-beta")
    
    Nb.Ka.cps <- subset(data$CPS, !(data$Energy < Nb.K[2]-0.02 | data$Energy > Nb.K[1]+0.02))
    Nb.file <- subset(data$Spectrum, !(data$Energy < Nb.K[2]-0.02 | data$Energy > Nb.K[1]+0.02))
    Nb.Ka.frame <- data.frame(is.0(Nb.Ka.cps, Nb.file))
    colnames(Nb.Ka.frame) <- c("Counts", "Spectrum")
    Nb.Ka.ag <- aggregate(list(Nb.Ka.frame$Counts), by=list(Nb.Ka.frame$Spectrum), FUN="sum")
    colnames(Nb.Ka.ag) <- c("Spectrum", "Nb K-alpha")
    
    Nb.Kb.cps <- subset(data$CPS, !(data$Energy < Nb.K[3]-0.02 | data$Energy > Nb.K[3]+0.02))
    Nb.file <- subset(data$Spectrum, !(data$Energy < Nb.K[3]-0.02 | data$Energy > Nb.K[3]+0.02))
    Nb.Kb.frame <- data.frame(is.0(Nb.Kb.cps, Nb.file))
    colnames(Nb.Kb.frame) <- c("Counts", "Spectrum")
    Nb.Kb.ag <- aggregate(list(Nb.Kb.frame$Counts), by=list(Nb.Kb.frame$Spectrum), FUN="sum")
    colnames(Nb.Kb.ag) <- c("Spectrum", "Nb K-beta")
    
    Mo.Ka.cps <- subset(data$CPS, !(data$Energy < Mo.K[2]-0.02 | data$Energy > Mo.K[1]+0.02))
    Mo.file <- subset(data$Spectrum, !(data$Energy < Mo.K[2]-0.02 | data$Energy > Mo.K[1]+0.02))
    Mo.Ka.frame <- data.frame(is.0(Mo.Ka.cps, Mo.file))
    colnames(Mo.Ka.frame) <- c("Counts", "Spectrum")
    Mo.Ka.ag <- aggregate(list(Mo.Ka.frame$Counts), by=list(Mo.Ka.frame$Spectrum), FUN="sum")
    colnames(Mo.Ka.ag) <- c("Spectrum", "Mo K-alpha")
    
    Mo.Kb.cps <- subset(data$CPS, !(data$Energy < Mo.K[3]-0.02 | data$Energy > Mo.K[3]+0.02))
    Mo.file <- subset(data$Spectrum, !(data$Energy < Mo.K[3]-0.02 | data$Energy > Mo.K[3]+0.02))
    Mo.Kb.frame <- data.frame(is.0(Mo.Kb.cps, Mo.file))
    colnames(Mo.Kb.frame) <- c("Counts", "Spectrum")
    Mo.Kb.ag <- aggregate(list(Mo.Kb.frame$Counts), by=list(Mo.Kb.frame$Spectrum), FUN="sum")
    colnames(Mo.Kb.ag) <- c("Spectrum", "Mo K-beta")
    
    Tc.Ka.cps <- subset(data$CPS, !(data$Energy < Tc.K[2]-0.02 | data$Energy > Tc.K[1]+0.02))
    Tc.file <- subset(data$Spectrum, !(data$Energy < Tc.K[2]-0.02 | data$Energy > Tc.K[1]+0.02))
    Tc.Ka.frame <- data.frame(is.0(Tc.Ka.cps, Tc.file))
    colnames(Tc.Ka.frame) <- c("Counts", "Spectrum")
    Tc.Ka.ag <- aggregate(list(Tc.Ka.frame$Counts), by=list(Tc.Ka.frame$Spectrum), FUN="sum")
    colnames(Tc.Ka.ag) <- c("Spectrum", "Tc K-alpha")
    
    Tc.Kb.cps <- subset(data$CPS, !(data$Energy < Tc.K[3]-0.02 | data$Energy > Tc.K[3]+0.02))
    Tc.file <- subset(data$Spectrum, !(data$Energy < Tc.K[3]-0.02 | data$Energy > Tc.K[3]+0.02))
    Tc.Kb.frame <- data.frame(is.0(Tc.Kb.cps, Tc.file))
    colnames(Tc.Kb.frame) <- c("Counts", "Spectrum")
    Tc.Kb.ag <- aggregate(list(Tc.Kb.frame$Counts), by=list(Tc.Kb.frame$Spectrum), FUN="sum")
    colnames(Tc.Kb.ag) <- c("Spectrum", "Tc K-beta")
    
    Ru.Ka.cps <- subset(data$CPS, !(data$Energy < Ru.K[2]-0.02 | data$Energy > Ru.K[1]+0.02))
    Ru.file <- subset(data$Spectrum, !(data$Energy < Ru.K[2]-0.02 | data$Energy > Ru.K[1]+0.02))
    Ru.Ka.frame <- data.frame(is.0(Ru.Ka.cps, Ru.file))
    colnames(Ru.Ka.frame) <- c("Counts", "Spectrum")
    Ru.Ka.ag <- aggregate(list(Ru.Ka.frame$Counts), by=list(Ru.Ka.frame$Spectrum), FUN="sum")
    colnames(Ru.Ka.ag) <- c("Spectrum", "Ru K-alpha")
    
    Ru.Kb.cps <- subset(data$CPS, !(data$Energy < Ru.K[3]-0.02 | data$Energy > Ru.K[3]+0.02))
    Ru.file <- subset(data$Spectrum, !(data$Energy < Ru.K[3]-0.02 | data$Energy > Ru.K[3]+0.02))
    Ru.Kb.frame <- data.frame(is.0(Ru.Kb.cps, Ru.file))
    colnames(Ru.Kb.frame) <- c("Counts", "Spectrum")
    Ru.Kb.ag <- aggregate(list(Ru.Kb.frame$Counts), by=list(Ru.Kb.frame$Spectrum), FUN="sum")
    colnames(Ru.Kb.ag) <- c("Spectrum", "Ru K-beta")
    
    Rh.Ka.cps <- subset(data$CPS, !(data$Energy < Rh.K[2]-0.02 | data$Energy > Rh.K[1]+0.02))
    Rh.file <- subset(data$Spectrum, !(data$Energy < Rh.K[2]-0.02 | data$Energy > Rh.K[1]+0.02))
    Rh.Ka.frame <- data.frame(is.0(Rh.Ka.cps, Rh.file))
    colnames(Rh.Ka.frame) <- c("Counts", "Spectrum")
    Rh.Ka.ag <- aggregate(list(Rh.Ka.frame$Counts), by=list(Rh.Ka.frame$Spectrum), FUN="sum")
    colnames(Rh.Ka.ag) <- c("Spectrum", "Rh K-alpha")
    
    Rh.Kb.cps <- subset(data$CPS, !(data$Energy < Rh.K[3]-0.02 | data$Energy > Rh.K[3]+0.02))
    Rh.file <- subset(data$Spectrum, !(data$Energy < Rh.K[3]-0.02 | data$Energy > Rh.K[3]+0.02))
    Rh.Kb.frame <- data.frame(is.0(Rh.Kb.cps, Rh.file))
    colnames(Rh.Kb.frame) <- c("Counts", "Spectrum")
    Rh.Kb.ag <- aggregate(list(Rh.Kb.frame$Counts), by=list(Rh.Kb.frame$Spectrum), FUN="sum")
    colnames(Rh.Kb.ag) <- c("Spectrum", "Rh K-beta")
    
    Pd.Ka.cps <- subset(data$CPS, !(data$Energy < Pd.K[2]-0.02 | data$Energy > Pd.K[1]+0.02))
    Pd.file <- subset(data$Spectrum, !(data$Energy < Pd.K[2]-0.02 | data$Energy > Pd.K[1]+0.02))
    Pd.Ka.frame <- data.frame(is.0(Pd.Ka.cps, Pd.file))
    colnames(Pd.Ka.frame) <- c("Counts", "Spectrum")
    Pd.Ka.ag <- aggregate(list(Pd.Ka.frame$Counts), by=list(Pd.Ka.frame$Spectrum), FUN="sum")
    colnames(Pd.Ka.ag) <- c("Spectrum", "Pd K-alpha")
    
    Pd.Kb.cps <- subset(data$CPS, !(data$Energy < Pd.K[3]-0.02 | data$Energy > Pd.K[3]+0.02))
    Pd.file <- subset(data$Spectrum, !(data$Energy < Pd.K[3]-0.02 | data$Energy > Pd.K[3]+0.02))
    Pd.Kb.frame <- data.frame(is.0(Pd.Kb.cps, Pd.file))
    colnames(Pd.Kb.frame) <- c("Counts", "Spectrum")
    Pd.Kb.ag <- aggregate(list(Pd.Kb.frame$Counts), by=list(Pd.Kb.frame$Spectrum), FUN="sum")
    colnames(Pd.Kb.ag) <- c("Spectrum", "Pd K-beta")
    
    Ag.Ka.cps <- subset(data$CPS, !(data$Energy < Ag.K[2]-0.02 | data$Energy > Ag.K[1]+0.02))
    Ag.file <- subset(data$Spectrum, !(data$Energy < Ag.K[2]-0.02 | data$Energy > Ag.K[1]+0.02))
    Ag.Ka.frame <- data.frame(is.0(Ag.Ka.cps, Ag.file))
    colnames(Ag.Ka.frame) <- c("Counts", "Spectrum")
    Ag.Ka.ag <- aggregate(list(Ag.Ka.frame$Counts), by=list(Ag.Ka.frame$Spectrum), FUN="sum")
    colnames(Ag.Ka.ag) <- c("Spectrum", "Ag K-alpha")
    
    Ag.Kb.cps <- subset(data$CPS, !(data$Energy < Ag.K[3]-0.02 | data$Energy > Ag.K[3]+0.02))
    Ag.file <- subset(data$Spectrum, !(data$Energy < Ag.K[3]-0.02 | data$Energy > Ag.K[3]+0.02))
    Ag.Kb.frame <- data.frame(is.0(Ag.Kb.cps, Ag.file))
    colnames(Ag.Kb.frame) <- c("Counts", "Spectrum")
    Ag.Kb.ag <- aggregate(list(Ag.Kb.frame$Counts), by=list(Ag.Kb.frame$Spectrum), FUN="sum")
    colnames(Ag.Kb.ag) <- c("Spectrum", "Ag K-beta")
    
    Cd.Ka.cps <- subset(data$CPS, !(data$Energy < Cd.K[2]-0.02 | data$Energy > Cd.K[1]+0.02))
    Cd.file <- subset(data$Spectrum, !(data$Energy < Cd.K[2]-0.02 | data$Energy > Cd.K[1]+0.02))
    Cd.Ka.frame <- data.frame(is.0(Cd.Ka.cps, Cd.file))
    colnames(Cd.Ka.frame) <- c("Counts", "Spectrum")
    Cd.Ka.ag <- aggregate(list(Cd.Ka.frame$Counts), by=list(Cd.Ka.frame$Spectrum), FUN="sum")
    colnames(Cd.Ka.ag) <- c("Spectrum", "Cd K-alpha")
    
    Cd.Kb.cps <- subset(data$CPS, !(data$Energy < Cd.K[3]-0.02 | data$Energy > Cd.K[3]+0.02))
    Cd.file <- subset(data$Spectrum, !(data$Energy < Cd.K[3]-0.02 | data$Energy > Cd.K[3]+0.02))
    Cd.Kb.frame <- data.frame(is.0(Cd.Kb.cps, Cd.file))
    colnames(Cd.Kb.frame) <- c("Counts", "Spectrum")
    Cd.Kb.ag <- aggregate(list(Cd.Kb.frame$Counts), by=list(Cd.Kb.frame$Spectrum), FUN="sum")
    colnames(Cd.Kb.ag) <- c("Spectrum", "Cd K-beta")
    
    In.Ka.cps <- subset(data$CPS, !(data$Energy < In.K[2]-0.02 | data$Energy > In.K[1]+0.02))
    In.file <- subset(data$Spectrum, !(data$Energy < In.K[2]-0.02 | data$Energy > In.K[1]+0.02))
    In.Ka.frame <- data.frame(is.0(In.Ka.cps, In.file))
    colnames(In.Ka.frame) <- c("Counts", "Spectrum")
    In.Ka.ag <- aggregate(list(In.Ka.frame$Counts), by=list(In.Ka.frame$Spectrum), FUN="sum")
    colnames(In.Ka.ag) <- c("Spectrum", "In K-alpha")
    
    In.Kb.cps <- subset(data$CPS, !(data$Energy < In.K[3]-0.02 | data$Energy > In.K[3]+0.02))
    In.file <- subset(data$Spectrum, !(data$Energy < In.K[3]-0.02 | data$Energy > In.K[3]+0.02))
    In.Kb.frame <- data.frame(is.0(In.Kb.cps, In.file))
    colnames(In.Kb.frame) <- c("Counts", "Spectrum")
    In.Kb.ag <- aggregate(list(In.Kb.frame$Counts), by=list(In.Kb.frame$Spectrum), FUN="sum")
    colnames(In.Kb.ag) <- c("Spectrum", "In K-beta")
    
    Sn.Ka.cps <- subset(data$CPS, !(data$Energy < Sn.K[2]-0.02 | data$Energy > Sn.K[1]+0.02))
    Sn.file <- subset(data$Spectrum, !(data$Energy < Sn.K[2]-0.02 | data$Energy > Sn.K[1]+0.02))
    Sn.Ka.frame <- data.frame(is.0(Sn.Ka.cps, Sn.file))
    colnames(Sn.Ka.frame) <- c("Counts", "Spectrum")
    Sn.Ka.ag <- aggregate(list(Sn.Ka.frame$Counts), by=list(Sn.Ka.frame$Spectrum), FUN="sum")
    colnames(Sn.Ka.ag) <- c("Spectrum", "Sn K-alpha")
    
    Sn.Kb.cps <- subset(data$CPS, !(data$Energy < Sn.K[3]-0.02 | data$Energy > Sn.K[3]+0.02))
    Sn.file <- subset(data$Spectrum, !(data$Energy < Sn.K[3]-0.02 | data$Energy > Sn.K[3]+0.02))
    Sn.Kb.frame <- data.frame(is.0(Sn.Kb.cps, Sn.file))
    colnames(Sn.Kb.frame) <- c("Counts", "Spectrum")
    Sn.Kb.ag <- aggregate(list(Sn.Kb.frame$Counts), by=list(Sn.Kb.frame$Spectrum), FUN="sum")
    colnames(Sn.Kb.ag) <- c("Spectrum", "Sn K-beta")
    
    Sb.Ka.cps <- subset(data$CPS, !(data$Energy < Sb.K[2]-0.02 | data$Energy > Sb.K[1]+0.02))
    Sb.file <- subset(data$Spectrum, !(data$Energy < Sb.K[2]-0.02 | data$Energy > Sb.K[1]+0.02))
    Sb.Ka.frame <- data.frame(is.0(Sb.Ka.cps, Sb.file))
    colnames(Sb.Ka.frame) <- c("Counts", "Spectrum")
    Sb.Ka.ag <- aggregate(list(Sb.Ka.frame$Counts), by=list(Sb.Ka.frame$Spectrum), FUN="sum")
    colnames(Sb.Ka.ag) <- c("Spectrum", "Sb K-alpha")
    
    Sb.Kb.cps <- subset(data$CPS, !(data$Energy < Sb.K[3]-0.02 | data$Energy > Sb.K[3]+0.02))
    Sb.file <- subset(data$Spectrum, !(data$Energy < Sb.K[3]-0.02 | data$Energy > Sb.K[3]+0.02))
    Sb.Kb.frame <- data.frame(is.0(Sb.Kb.cps, Sb.file))
    colnames(Sb.Kb.frame) <- c("Counts", "Spectrum")
    Sb.Kb.ag <- aggregate(list(Sb.Kb.frame$Counts), by=list(Sb.Kb.frame$Spectrum), FUN="sum")
    colnames(Sb.Kb.ag) <- c("Spectrum", "Sb K-beta")
    
    Te.Ka.cps <- subset(data$CPS, !(data$Energy < Te.K[2]-0.02 | data$Energy > Te.K[1]+0.02))
    Te.file <- subset(data$Spectrum, !(data$Energy < Te.K[2]-0.02 | data$Energy > Te.K[1]+0.02))
    Te.Ka.frame <- data.frame(is.0(Te.Ka.cps, Te.file))
    colnames(Te.Ka.frame) <- c("Counts", "Spectrum")
    Te.Ka.ag <- aggregate(list(Te.Ka.frame$Counts), by=list(Te.Ka.frame$Spectrum), FUN="sum")
    colnames(Te.Ka.ag) <- c("Spectrum", "Te K-alpha")
    
    Te.Kb.cps <- subset(data$CPS, !(data$Energy < Te.K[3]-0.02 | data$Energy > Te.K[3]+0.02))
    Te.file <- subset(data$Spectrum, !(data$Energy < Te.K[3]-0.02 | data$Energy > Te.K[3]+0.02))
    Te.Kb.frame <- data.frame(is.0(Te.Kb.cps, Te.file))
    colnames(Te.Kb.frame) <- c("Counts", "Spectrum")
    Te.Kb.ag <- aggregate(list(Te.Kb.frame$Counts), by=list(Te.Kb.frame$Spectrum), FUN="sum")
    colnames(Te.Kb.ag) <- c("Spectrum", "Te K-beta")
    
    I.Ka.cps <- subset(data$CPS, !(data$Energy < I.K[2]-0.02 | data$Energy > I.K[1]+0.02))
    I.file <- subset(data$Spectrum, !(data$Energy < I.K[2]-0.02 | data$Energy > I.K[1]+0.02))
    I.Ka.frame <- data.frame(is.0(I.Ka.cps, I.file))
    colnames(I.Ka.frame) <- c("Counts", "Spectrum")
    I.Ka.ag <- aggregate(list(I.Ka.frame$Counts), by=list(I.Ka.frame$Spectrum), FUN="sum")
    colnames(I.Ka.ag) <- c("Spectrum", "I K-alpha")
    
    I.Kb.cps <- subset(data$CPS, !(data$Energy < I.K[3]-0.02 | data$Energy > I.K[3]+0.02))
    I.file <- subset(data$Spectrum, !(data$Energy < I.K[3]-0.02 | data$Energy > I.K[3]+0.02))
    I.Kb.frame <- data.frame(is.0(I.Kb.cps, I.file))
    colnames(I.Kb.frame) <- c("Counts", "Spectrum")
    I.Kb.ag <- aggregate(list(I.Kb.frame$Counts), by=list(I.Kb.frame$Spectrum), FUN="sum")
    colnames(I.Kb.ag) <- c("Spectrum", "I K-beta")
    
    Xe.Ka.cps <- subset(data$CPS, !(data$Energy < Xe.K[2]-0.02 | data$Energy > Xe.K[1]+0.02))
    Xe.file <- subset(data$Spectrum, !(data$Energy < Xe.K[2]-0.02 | data$Energy > Xe.K[1]+0.02))
    Xe.Ka.frame <- data.frame(is.0(Xe.Ka.cps, Xe.file))
    colnames(Xe.Ka.frame) <- c("Counts", "Spectrum")
    Xe.Ka.ag <- aggregate(list(Xe.Ka.frame$Counts), by=list(Xe.Ka.frame$Spectrum), FUN="sum")
    colnames(Xe.Ka.ag) <- c("Spectrum", "Xe K-alpha")
    
    Xe.Kb.cps <- subset(data$CPS, !(data$Energy < Xe.K[3]-0.02 | data$Energy > Xe.K[3]+0.02))
    Xe.file <- subset(data$Spectrum, !(data$Energy < Xe.K[3]-0.02 | data$Energy > Xe.K[3]+0.02))
    Xe.Kb.frame <- data.frame(is.0(Xe.Kb.cps, Xe.file))
    colnames(Xe.Kb.frame) <- c("Counts", "Spectrum")
    Xe.Kb.ag <- aggregate(list(Xe.Kb.frame$Counts), by=list(Xe.Kb.frame$Spectrum), FUN="sum")
    colnames(Xe.Kb.ag) <- c("Spectrum", "Xe K-beta")
    
    Cs.Ka.cps <- subset(data$CPS, !(data$Energy < Cs.K[2]-0.02 | data$Energy > Cs.K[1]+0.02))
    Cs.file <- subset(data$Spectrum, !(data$Energy < Cs.K[2]-0.02 | data$Energy > Cs.K[1]+0.02))
    Cs.Ka.frame <- data.frame(is.0(Cs.Ka.cps, Cs.file))
    colnames(Cs.Ka.frame) <- c("Counts", "Spectrum")
    Cs.Ka.ag <- aggregate(list(Cs.Ka.frame$Counts), by=list(Cs.Ka.frame$Spectrum), FUN="sum")
    colnames(Cs.Ka.ag) <- c("Spectrum", "Cs K-alpha")
    
    Cs.Kb.cps <- subset(data$CPS, !(data$Energy < Cs.K[3]-0.02 | data$Energy > Cs.K[3]+0.02))
    Cs.file <- subset(data$Spectrum, !(data$Energy < Cs.K[3]-0.02 | data$Energy > Cs.K[3]+0.02))
    Cs.Kb.frame <- data.frame(is.0(Cs.Kb.cps, Cs.file))
    colnames(Cs.Kb.frame) <- c("Counts", "Spectrum")
    Cs.Kb.ag <- aggregate(list(Cs.Kb.frame$Counts), by=list(Cs.Kb.frame$Spectrum), FUN="sum")
    colnames(Cs.Kb.ag) <- c("Spectrum", "Cs K-beta")
    
    Ba.Ka.cps <- subset(data$CPS, !(data$Energy < Ba.K[2]-0.02 | data$Energy > Ba.K[1]+0.02))
    Ba.file <- subset(data$Spectrum, !(data$Energy < Ba.K[2]-0.02 | data$Energy > Ba.K[1]+0.02))
    Ba.Ka.frame <- data.frame(is.0(Ba.Ka.cps, Ba.file))
    colnames(Ba.Ka.frame) <- c("Counts", "Spectrum")
    Ba.Ka.ag <- aggregate(list(Ba.Ka.frame$Counts), by=list(Ba.Ka.frame$Spectrum), FUN="sum")
    colnames(Ba.Ka.ag) <- c("Spectrum", "Ba K-alpha")
    
    Ba.Kb.cps <- subset(data$CPS, !(data$Energy < Ba.K[3]-0.02 | data$Energy > Ba.K[3]+0.02))
    Ba.file <- subset(data$Spectrum, !(data$Energy < Ba.K[3]-0.02 | data$Energy > Ba.K[3]+0.02))
    Ba.Kb.frame <- data.frame(is.0(Ba.Kb.cps, Ba.file))
    colnames(Ba.Kb.frame) <- c("Counts", "Spectrum")
    Ba.Kb.ag <- aggregate(list(Ba.Kb.frame$Counts), by=list(Ba.Kb.frame$Spectrum), FUN="sum")
    colnames(Ba.Kb.ag) <- c("Spectrum", "Ba K-beta")
    
    La.Ka.cps <- subset(data$CPS, !(data$Energy < La.K[2]-0.02 | data$Energy > La.K[1]+0.02))
    La.file <- subset(data$Spectrum, !(data$Energy < La.K[2]-0.02 | data$Energy > La.K[1]+0.02))
    La.Ka.frame <- data.frame(is.0(La.Ka.cps, La.file))
    colnames(La.Ka.frame) <- c("Counts", "Spectrum")
    La.Ka.ag <- aggregate(list(La.Ka.frame$Counts), by=list(La.Ka.frame$Spectrum), FUN="sum")
    colnames(La.Ka.ag) <- c("Spectrum", "La K-alpha")
    
    La.Kb.cps <- subset(data$CPS, !(data$Energy < La.K[3]-0.02 | data$Energy > La.K[3]+0.02))
    La.file <- subset(data$Spectrum, !(data$Energy < La.K[3]-0.02 | data$Energy > La.K[3]+0.02))
    La.Kb.frame <- data.frame(is.0(La.Kb.cps, La.file))
    colnames(La.Kb.frame) <- c("Counts", "Spectrum")
    La.Kb.ag <- aggregate(list(La.Kb.frame$Counts), by=list(La.Kb.frame$Spectrum), FUN="sum")
    colnames(La.Kb.ag) <- c("Spectrum", "La K-beta")
    
    Ce.Ka.cps <- subset(data$CPS, !(data$Energy < Ce.K[2]-0.02 | data$Energy > Ce.K[1]+0.02))
    Ce.file <- subset(data$Spectrum, !(data$Energy < Ce.K[2]-0.02 | data$Energy > Ce.K[1]+0.02))
    Ce.Ka.frame <- data.frame(is.0(Ce.Ka.cps, Ce.file))
    colnames(Ce.Ka.frame) <- c("Counts", "Spectrum")
    Ce.Ka.ag <- aggregate(list(Ce.Ka.frame$Counts), by=list(Ce.Ka.frame$Spectrum), FUN="sum")
    colnames(Ce.Ka.ag) <- c("Spectrum", "Ce K-alpha")
    
    Ce.Kb.cps <- subset(data$CPS, !(data$Energy < Ce.K[3]-0.02 | data$Energy > Ce.K[3]+0.02))
    Ce.file <- subset(data$Spectrum, !(data$Energy < Ce.K[3]-0.02 | data$Energy > Ce.K[3]+0.02))
    Ce.Kb.frame <- data.frame(is.0(Ce.Kb.cps, Ce.file))
    colnames(Ce.Kb.frame) <- c("Counts", "Spectrum")
    Ce.Kb.ag <- aggregate(list(Ce.Kb.frame$Counts), by=list(Ce.Kb.frame$Spectrum), FUN="sum")
    colnames(Ce.Kb.ag) <- c("Spectrum", "Ce K-beta")
    
    Pr.Ka.cps <- subset(data$CPS, !(data$Energy < Pr.K[2]-0.02 | data$Energy > Pr.K[1]+0.02))
    Pr.file <- subset(data$Spectrum, !(data$Energy < Pr.K[2]-0.02 | data$Energy > Pr.K[1]+0.02))
    Pr.Ka.frame <- data.frame(is.0(Pr.Ka.cps, Pr.file))
    colnames(Pr.Ka.frame) <- c("Counts", "Spectrum")
    Pr.Ka.ag <- aggregate(list(Pr.Ka.frame$Counts), by=list(Pr.Ka.frame$Spectrum), FUN="sum")
    colnames(Pr.Ka.ag) <- c("Spectrum", "Pr K-alpha")
    
    Pr.Kb.cps <- subset(data$CPS, !(data$Energy < Pr.K[3]-0.02 | data$Energy > Pr.K[3]+0.02))
    Pr.file <- subset(data$Spectrum, !(data$Energy < Pr.K[3]-0.02 | data$Energy > Pr.K[3]+0.02))
    Pr.Kb.frame <- data.frame(is.0(Pr.Kb.cps, Pr.file))
    colnames(Pr.Kb.frame) <- c("Counts", "Spectrum")
    Pr.Kb.ag <- aggregate(list(Pr.Kb.frame$Counts), by=list(Pr.Kb.frame$Spectrum), FUN="sum")
    colnames(Pr.Kb.ag) <- c("Spectrum", "Pr K-beta")
    
    Nd.Ka.cps <- subset(data$CPS, !(data$Energy < Nd.K[2]-0.02 | data$Energy > Nd.K[1]+0.02))
    Nd.file <- subset(data$Spectrum, !(data$Energy < Nd.K[2]-0.02 | data$Energy > Nd.K[1]+0.02))
    Nd.Ka.frame <- data.frame(is.0(Nd.Ka.cps, Nd.file))
    colnames(Nd.Ka.frame) <- c("Counts", "Spectrum")
    Nd.Ka.ag <- aggregate(list(Nd.Ka.frame$Counts), by=list(Nd.Ka.frame$Spectrum), FUN="sum")
    colnames(Nd.Ka.ag) <- c("Spectrum", "Nd K-alpha")
    
    Nd.Kb.cps <- subset(data$CPS, !(data$Energy < Nd.K[3]-0.02 | data$Energy > Nd.K[3]+0.02))
    Nd.file <- subset(data$Spectrum, !(data$Energy < Nd.K[3]-0.02 | data$Energy > Nd.K[3]+0.02))
    Nd.Kb.frame <- data.frame(is.0(Nd.Kb.cps, Nd.file))
    colnames(Nd.Kb.frame) <- c("Counts", "Spectrum")
    Nd.Kb.ag <- aggregate(list(Nd.Kb.frame$Counts), by=list(Nd.Kb.frame$Spectrum), FUN="sum")
    colnames(Nd.Kb.ag) <- c("Spectrum", "Nd K-beta")
    
    Mo.La.cps <- subset(data$CPS, !(data$Energy < Mo.L[2]-0.02 | data$Energy > Mo.L[1]+0.02))
    Mo.file <- subset(data$Spectrum, !(data$Energy < Mo.L[2]-0.02 | data$Energy > Mo.L[1]+0.02))
    Mo.La.frame <- is.0(Mo.La.cps,Mo.file)
    colnames(Mo.La.frame) <- c("Counts", "Spectrum")
    Mo.La.ag <- aggregate(list(Mo.La.frame$Counts), by=list(Mo.La.frame$Spectrum), FUN="sum")
    colnames(Mo.La.ag) <- c("Spectrum", "Mo L-alpha")

    Tc.La.cps <- subset(data$CPS, !(data$Energy < Tc.L[2]-0.02 | data$Energy > Tc.L[1]+0.02))
    Tc.file <- subset(data$Spectrum, !(data$Energy < Tc.L[2]-0.02 | data$Energy > Tc.L[1]+0.02))
    Tc.La.frame <- is.0(Tc.La.cps,Tc.file)
    colnames(Tc.La.frame) <- c("Counts", "Spectrum")
    Tc.La.ag <- aggregate(list(Tc.La.frame$Counts), by=list(Tc.La.frame$Spectrum), FUN="sum")
    colnames(Tc.La.ag) <- c("Spectrum", "Tc L-alpha")
    
    Ru.La.cps <- subset(data$CPS, !(data$Energy < Ru.L[2]-0.02 | data$Energy > Ru.L[1]+0.02))
    Ru.file <- subset(data$Spectrum, !(data$Energy < Ru.L[2]-0.02 | data$Energy > Ru.L[1]+0.02))
    Ru.La.frame <- is.0(Ru.La.cps,Ru.file)
    colnames(Ru.La.frame) <- c("Counts", "Spectrum")
    Ru.La.ag <- aggregate(list(Ru.La.frame$Counts), by=list(Ru.La.frame$Spectrum), FUN="sum")
    colnames(Ru.La.ag) <- c("Spectrum", "Ru L-alpha")
    
    Rh.La.cps <- subset(data$CPS, !(data$Energy < Rh.L[2]-0.02 | data$Energy > Rh.L[1]+0.02))
    Rh.file <- subset(data$Spectrum, !(data$Energy < Rh.L[2]-0.02 | data$Energy > Rh.L[1]+0.02))
    Rh.La.frame <- data.frame(is.0(Rh.La.cps, Rh.file))
    colnames(Rh.La.frame) <- c("Counts", "Spectrum")
    Rh.La.ag <- aggregate(list(Rh.La.frame$Counts), by=list(Rh.La.frame$Spectrum), FUN="sum")
    colnames(Rh.La.ag) <- c("Spectrum", "Rh L-alpha")
    
    Pd.La.cps <- subset(data$CPS, !(data$Energy < Pd.L[2]-0.02 | data$Energy > Pd.L[1]+0.02))
    Pd.file <- subset(data$Spectrum, !(data$Energy < Pd.L[2]-0.02 | data$Energy > Pd.L[1]+0.02))
    Pd.La.frame <- data.frame(is.0(Pd.La.cps, Pd.file))
    colnames(Pd.La.frame) <- c("Counts", "Spectrum")
    Pd.La.ag <- aggregate(list(Pd.La.frame$Counts), by=list(Pd.La.frame$Spectrum), FUN="sum")
    colnames(Pd.La.ag) <- c("Spectrum", "Pd L-alpha")
    
    Ag.La.cps <- subset(data$CPS, !(data$Energy < Ag.L[2]-0.02 | data$Energy > Ag.L[1]+0.02))
    Ag.file <- subset(data$Spectrum, !(data$Energy < Ag.L[2]-0.02 | data$Energy > Ag.L[1]+0.02))
    Ag.La.frame <- data.frame(is.0(Ag.La.cps, Ag.file))
    colnames(Ag.La.frame) <- c("Counts", "Spectrum")
    Ag.La.ag <- aggregate(list(Ag.La.frame$Counts), by=list(Ag.La.frame$Spectrum), FUN="sum")
    colnames(Ag.La.ag) <- c("Spectrum", "Ag L-alpha")
    
    Cd.La.cps <- subset(data$CPS, !(data$Energy < Cd.L[2]-0.02 | data$Energy > Cd.L[1]+0.02))
    Cd.file <- subset(data$Spectrum, !(data$Energy < Cd.L[2]-0.02 | data$Energy > Cd.L[1]+0.02))
    Cd.La.frame <- data.frame(is.0(Cd.La.cps, Cd.file))
    colnames(Cd.La.frame) <- c("Counts", "Spectrum")
    Cd.La.ag <- aggregate(list(Cd.La.frame$Counts), by=list(Cd.La.frame$Spectrum), FUN="sum")
    colnames(Cd.La.ag) <- c("Spectrum", "Cd L-alpha")
    
    In.La.cps <- subset(data$CPS, !(data$Energy < In.L[2]-0.02 | data$Energy > In.L[1]+0.02))
    In.file <- subset(data$Spectrum, !(data$Energy < In.L[2]-0.02 | data$Energy > In.L[1]+0.02))
    In.La.frame <- data.frame(is.0(In.La.cps, In.file))
    colnames(In.La.frame) <- c("Counts", "Spectrum")
    In.La.ag <- aggregate(list(In.La.frame$Counts), by=list(In.La.frame$Spectrum), FUN="sum")
    colnames(In.La.ag) <- c("Spectrum", "In L-alpha")
    
    Sn.La.cps <- subset(data$CPS, !(data$Energy < Sn.L[2]-0.02 | data$Energy > Sn.L[1]+0.02))
    Sn.file <- subset(data$Spectrum, !(data$Energy < Sn.L[2]-0.02 | data$Energy > Sn.L[1]+0.02))
    Sn.La.frame <- data.frame(is.0(Sn.La.cps, Sn.file))
    colnames(Sn.La.frame) <- c("Counts", "Spectrum")
    Sn.La.ag <- aggregate(list(Sn.La.frame$Counts), by=list(Sn.La.frame$Spectrum), FUN="sum")
    colnames(Sn.La.ag) <- c("Spectrum", "Sn L-alpha")
    
    Sb.La.cps <- subset(data$CPS, !(data$Energy < Sb.L[2]-0.02 | data$Energy > Sb.L[1]+0.02))
    Sb.file <- subset(data$Spectrum, !(data$Energy < Sb.L[2]-0.02 | data$Energy > Sb.L[1]+0.02))
    Sb.La.frame <- data.frame(is.0(Sb.La.cps, Sb.file))
    colnames(Sb.La.frame) <- c("Counts", "Spectrum")
    Sb.La.ag <- aggregate(list(Sb.La.frame$Counts), by=list(Sb.La.frame$Spectrum), FUN="sum")
    colnames(Sb.La.ag) <- c("Spectrum", "Sb L-alpha")
    
    Te.La.cps <- subset(data$CPS, !(data$Energy < Te.L[2]-0.02 | data$Energy > Te.L[1]+0.02))
    Te.file <- subset(data$Spectrum, !(data$Energy < Te.L[2]-0.02 | data$Energy > Te.L[1]+0.02))
    Te.La.frame <- data.frame(is.0(Te.La.cps, Te.file))
    colnames(Te.La.frame) <- c("Counts", "Spectrum")
    Te.La.ag <- aggregate(list(Te.La.frame$Counts), by=list(Te.La.frame$Spectrum), FUN="sum")
    colnames(Te.La.ag) <- c("Spectrum", "Te L-alpha")
    
    I.La.cps <- subset(data$CPS, !(data$Energy < I.L[2]-0.02 | data$Energy > I.L[1]+0.02))
    I.file <- subset(data$Spectrum, !(data$Energy < I.L[2]-0.02 | data$Energy > I.L[1]+0.02))
    I.La.frame <- data.frame(is.0(I.La.cps, I.file))
    colnames(I.La.frame) <- c("Counts", "Spectrum")
    I.La.ag <- aggregate(list(I.La.frame$Counts), by=list(I.La.frame$Spectrum), FUN="sum")
    colnames(I.La.ag) <- c("Spectrum", "I L-alpha")
    
    Xe.La.cps <- subset(data$CPS, !(data$Energy < Xe.L[2]-0.02 | data$Energy > Xe.L[1]+0.02))
    Xe.file <- subset(data$Spectrum, !(data$Energy < Xe.L[2]-0.02 | data$Energy > Xe.L[1]+0.02))
    Xe.La.frame <- data.frame(is.0(Xe.La.cps, Xe.file))
    colnames(Xe.La.frame) <- c("Counts", "Spectrum")
    Xe.La.ag <- aggregate(list(Xe.La.frame$Counts), by=list(Xe.La.frame$Spectrum), FUN="sum")
    colnames(Xe.La.ag) <- c("Spectrum", "Xe L-alpha")
    
    Cs.La.cps <- subset(data$CPS, !(data$Energy < Cs.L[2]-0.02 | data$Energy > Cs.L[1]+0.02))
    Cs.file <- subset(data$Spectrum, !(data$Energy < Cs.L[2]-0.02 | data$Energy > Cs.L[1]+0.02))
    Cs.La.frame <- data.frame(is.0(Cs.La.cps, Cs.file))
    colnames(Cs.La.frame) <- c("Counts", "Spectrum")
    Cs.La.ag <- aggregate(list(Cs.La.frame$Counts), by=list(Cs.La.frame$Spectrum), FUN="sum")
    colnames(Cs.La.ag) <- c("Spectrum", "Cs L-alpha")
    
    Ba.La.cps <- subset(data$CPS, !(data$Energy < Ba.L[2]-0.02 | data$Energy > Ba.L[1]+0.02))
    Ba.file <- subset(data$Spectrum, !(data$Energy < Ba.L[2]-0.02 | data$Energy > Ba.L[1]+0.02))
    Ba.La.frame <- data.frame(is.0(Ba.La.cps, Ba.file))
    colnames(Ba.La.frame) <- c("Counts", "Spectrum")
    Ba.La.ag <- aggregate(list(Ba.La.frame$Counts), by=list(Ba.La.frame$Spectrum), FUN="sum")
    colnames(Ba.La.ag) <- c("Spectrum", "Ba L-alpha")
    
    La.La.cps <- subset(data$CPS, !(data$Energy < La.L[2]-0.02 | data$Energy > La.L[1]+0.02))
    La.file <- subset(data$Spectrum, !(data$Energy < La.L[2]-0.02 | data$Energy > La.L[1]+0.02))
    La.La.frame <- data.frame(is.0(La.La.cps, La.file))
    colnames(La.La.frame) <- c("Counts", "Spectrum")
    La.La.ag <- aggregate(list(La.La.frame$Counts), by=list(La.La.frame$Spectrum), FUN="sum")
    colnames(La.La.ag) <- c("Spectrum", "La L-alpha")
    
    Ce.La.cps <- subset(data$CPS, !(data$Energy < Ce.L[2]-0.02 | data$Energy > Ce.L[1]+0.02))
    Ce.file <- subset(data$Spectrum, !(data$Energy < Ce.L[2]-0.02 | data$Energy > Ce.L[1]+0.02))
    Ce.La.frame <- data.frame(is.0(Ce.La.cps, Ce.file))
    colnames(Ce.La.frame) <- c("Counts", "Spectrum")
    Ce.La.ag <- aggregate(list(Ce.La.frame$Counts), by=list(Ce.La.frame$Spectrum), FUN="sum")
    colnames(Ce.La.ag) <- c("Spectrum", "Ce L-alpha")
    
    Pr.La.cps <- subset(data$CPS, !(data$Energy < Pr.L[2]-0.02 | data$Energy > Pr.L[1]+0.02))
    Pr.file <- subset(data$Spectrum, !(data$Energy < Pr.L[2]-0.02 | data$Energy > Pr.L[1]+0.02))
    Pr.La.frame <- data.frame(is.0(Pr.La.cps, Pr.file))
    colnames(Pr.La.frame) <- c("Counts", "Spectrum")
    Pr.La.ag <- aggregate(list(Pr.La.frame$Counts), by=list(Pr.La.frame$Spectrum), FUN="sum")
    colnames(Pr.La.ag) <- c("Spectrum", "Pr L-alpha")
    
    Nd.La.cps <- subset(data$CPS, !(data$Energy < Nd.L[2]-0.02 | data$Energy > Nd.L[1]+0.02))
    Nd.file <- subset(data$Spectrum, !(data$Energy < Nd.L[2]-0.02 | data$Energy > Nd.L[1]+0.02))
    Nd.La.frame <- data.frame(is.0(Nd.La.cps, Nd.file))
    colnames(Nd.La.frame) <- c("Counts", "Spectrum")
    Nd.La.ag <- aggregate(list(Nd.La.frame$Counts), by=list(Nd.La.frame$Spectrum), FUN="sum")
    colnames(Nd.La.ag) <- c("Spectrum", "Nd L-alpha")
    
    Pm.La.cps <- subset(data$CPS, !(data$Energy < Pm.L[2]-0.02 | data$Energy > Pm.L[1]+0.02))
    Pm.file <- subset(data$Spectrum, !(data$Energy < Pm.L[2]-0.02 | data$Energy > Pm.L[1]+0.02))
    Pm.La.frame <- data.frame(is.0(Pm.La.cps, Pm.file))
    colnames(Pm.La.frame) <- c("Counts", "Spectrum")
    Pm.La.ag <- aggregate(list(Pm.La.frame$Counts), by=list(Pm.La.frame$Spectrum), FUN="sum")
    colnames(Pm.La.ag) <- c("Spectrum", "Pm L-alpha")
    
    Sm.La.cps <- subset(data$CPS, !(data$Energy < Sm.L[2]-0.02 | data$Energy > Sm.L[1]+0.02))
    Sm.file <- subset(data$Spectrum, !(data$Energy < Sm.L[2]-0.02 | data$Energy > Sm.L[1]+0.02))
    Sm.La.frame <- data.frame(is.0(Sm.La.cps, Sm.file))
    colnames(Sm.La.frame) <- c("Counts", "Spectrum")
    Sm.La.ag <- aggregate(list(Sm.La.frame$Counts), by=list(Sm.La.frame$Spectrum), FUN="sum")
    colnames(Sm.La.ag) <- c("Spectrum", "Sm L-alpha")
    
    Eu.La.cps <- subset(data$CPS, !(data$Energy < Eu.L[2]-0.02 | data$Energy > Eu.L[1]+0.02))
    Eu.file <- subset(data$Spectrum, !(data$Energy < Eu.L[2]-0.02 | data$Energy > Eu.L[1]+0.02))
    Eu.La.frame <- data.frame(is.0(Eu.La.cps, Eu.file))
    colnames(Eu.La.frame) <- c("Counts", "Spectrum")
    Eu.La.ag <- aggregate(list(Eu.La.frame$Counts), by=list(Eu.La.frame$Spectrum), FUN="sum")
    colnames(Eu.La.ag) <- c("Spectrum", "Eu L-alpha")
    
    Gd.La.cps <- subset(data$CPS, !(data$Energy < Gd.L[2]-0.02 | data$Energy > Gd.L[1]+0.02))
    Gd.file <- subset(data$Spectrum, !(data$Energy < Gd.L[2]-0.02 | data$Energy > Gd.L[1]+0.02))
    Gd.La.frame <- data.frame(is.0(Gd.La.cps, Gd.file))
    colnames(Gd.La.frame) <- c("Counts", "Spectrum")
    Gd.La.ag <- aggregate(list(Gd.La.frame$Counts), by=list(Gd.La.frame$Spectrum), FUN="sum")
    colnames(Gd.La.ag) <- c("Spectrum", "Gd L-alpha")
    
    Tb.La.cps <- subset(data$CPS, !(data$Energy < Tb.L[2]-0.02 | data$Energy > Tb.L[1]+0.02))
    Tb.file <- subset(data$Spectrum, !(data$Energy < Tb.L[2]-0.02 | data$Energy > Tb.L[1]+0.02))
    Tb.La.frame <- data.frame(is.0(Tb.La.cps, Tb.file))
    colnames(Tb.La.frame) <- c("Counts", "Spectrum")
    Tb.La.ag <- aggregate(list(Tb.La.frame$Counts), by=list(Tb.La.frame$Spectrum), FUN="sum")
    colnames(Tb.La.ag) <- c("Spectrum", "Tb L-alpha")
    
    Dy.La.cps <- subset(data$CPS, !(data$Energy < Dy.L[2]-0.02 | data$Energy > Dy.L[1]+0.02))
    Dy.file <- subset(data$Spectrum, !(data$Energy < Dy.L[2]-0.02 | data$Energy > Dy.L[1]+0.02))
    Dy.La.frame <- data.frame(is.0(Dy.La.cps, Dy.file))
    colnames(Dy.La.frame) <- c("Counts", "Spectrum")
    Dy.La.ag <- aggregate(list(Dy.La.frame$Counts), by=list(Dy.La.frame$Spectrum), FUN="sum")
    colnames(Dy.La.ag) <- c("Spectrum", "Dy L-alpha")
    
    Ho.La.cps <- subset(data$CPS, !(data$Energy < Ho.L[2]-0.02 | data$Energy > Ho.L[1]+0.02))
    Ho.file <- subset(data$Spectrum, !(data$Energy < Ho.L[2]-0.02 | data$Energy > Ho.L[1]+0.02))
    Ho.La.frame <- data.frame(is.0(Ho.La.cps, Ho.file))
    colnames(Ho.La.frame) <- c("Counts", "Spectrum")
    Ho.La.ag <- aggregate(list(Ho.La.frame$Counts), by=list(Ho.La.frame$Spectrum), FUN="sum")
    colnames(Ho.La.ag) <- c("Spectrum", "Ho L-alpha")
    
    Er.La.cps <- subset(data$CPS, !(data$Energy < Er.L[2]-0.02 | data$Energy > Er.L[1]+0.02))
    Er.file <- subset(data$Spectrum, !(data$Energy < Er.L[2]-0.02 | data$Energy > Er.L[1]+0.02))
    Er.La.frame <- data.frame(is.0(Er.La.cps, Er.file))
    colnames(Er.La.frame) <- c("Counts", "Spectrum")
    Er.La.ag <- aggregate(list(Er.La.frame$Counts), by=list(Er.La.frame$Spectrum), FUN="sum")
    colnames(Er.La.ag) <- c("Spectrum", "Er L-alpha")
    
    Tm.La.cps <- subset(data$CPS, !(data$Energy < Tm.L[2]-0.02 | data$Energy > Tm.L[1]+0.02))
    Tm.file <- subset(data$Spectrum, !(data$Energy < Tm.L[2]-0.02 | data$Energy > Tm.L[1]+0.02))
    Tm.La.frame <- data.frame(is.0(Tm.La.cps, Tm.file))
    colnames(Tm.La.frame) <- c("Counts", "Spectrum")
    Tm.La.ag <- aggregate(list(Tm.La.frame$Counts), by=list(Tm.La.frame$Spectrum), FUN="sum")
    colnames(Tm.La.ag) <- c("Spectrum", "Tm L-alpha")
    
    Yb.La.cps <- subset(data$CPS, !(data$Energy < Yb.L[2]-0.02 | data$Energy > Yb.L[1]+0.02))
    Yb.file <- subset(data$Spectrum, !(data$Energy < Yb.L[2]-0.02 | data$Energy > Yb.L[1]+0.02))
    Yb.La.frame <- data.frame(is.0(Yb.La.cps, Yb.file))
    colnames(Yb.La.frame) <- c("Counts", "Spectrum")
    Yb.La.ag <- aggregate(list(Yb.La.frame$Counts), by=list(Yb.La.frame$Spectrum), FUN="sum")
    colnames(Yb.La.ag) <- c("Spectrum", "Yb L-alpha")
    
    Lu.La.cps <- subset(data$CPS, !(data$Energy < Lu.L[2]-0.02 | data$Energy > Lu.L[1]+0.02))
    Lu.file <- subset(data$Spectrum, !(data$Energy < Lu.L[2]-0.02 | data$Energy > Lu.L[1]+0.02))
    Lu.La.frame <- data.frame(is.0(Lu.La.cps, Lu.file))
    colnames(Lu.La.frame) <- c("Counts", "Spectrum")
    Lu.La.ag <- aggregate(list(Lu.La.frame$Counts), by=list(Lu.La.frame$Spectrum), FUN="sum")
    colnames(Lu.La.ag) <- c("Spectrum", "Lu L-alpha")
    
    Hf.La.cps <- subset(data$CPS, !(data$Energy < Hf.L[2]-0.02 | data$Energy > Hf.L[1]+0.02))
    Hf.file <- subset(data$Spectrum, !(data$Energy < Hf.L[2]-0.02 | data$Energy > Hf.L[1]+0.02))
    Hf.La.frame <- data.frame(is.0(Hf.La.cps, Hf.file))
    colnames(Hf.La.frame) <- c("Counts", "Spectrum")
    Hf.La.ag <- aggregate(list(Hf.La.frame$Counts), by=list(Hf.La.frame$Spectrum), FUN="sum")
    colnames(Hf.La.ag) <- c("Spectrum", "Hf L-alpha")
    
    Ta.La.cps <- subset(data$CPS, !(data$Energy < Ta.L[2]-0.02 | data$Energy > Ta.L[1]+0.02))
    Ta.file <- subset(data$Spectrum, !(data$Energy < Ta.L[2]-0.02 | data$Energy > Ta.L[1]+0.02))
    Ta.La.frame <- data.frame(is.0(Ta.La.cps, Ta.file))
    colnames(Ta.La.frame) <- c("Counts", "Spectrum")
    Ta.La.ag <- aggregate(list(Ta.La.frame$Counts), by=list(Ta.La.frame$Spectrum), FUN="sum")
    colnames(Ta.La.ag) <- c("Spectrum", "Ta L-alpha")
    
    W.La.cps <- subset(data$CPS, !(data$Energy < W.L[2]-0.02 | data$Energy > W.L[1]+0.02))
    W.file <- subset(data$Spectrum, !(data$Energy < W.L[2]-0.02 | data$Energy > W.L[1]+0.02))
    W.La.frame <- data.frame(is.0(W.La.cps, W.file))
    colnames(W.La.frame) <- c("Counts", "Spectrum")
    W.La.ag <- aggregate(list(W.La.frame$Counts), by=list(W.La.frame$Spectrum), FUN="sum")
    colnames(W.La.ag) <- c("Spectrum", "W L-alpha")
    
    Re.La.cps <- subset(data$CPS, !(data$Energy < Re.L[2]-0.02 | data$Energy > Re.L[1]+0.02))
    Re.file <- subset(data$Spectrum, !(data$Energy < Re.L[2]-0.02 | data$Energy > Re.L[1]+0.02))
    Re.La.frame <- data.frame(is.0(Re.La.cps, Re.file))
    colnames(Re.La.frame) <- c("Counts", "Spectrum")
    Re.La.ag <- aggregate(list(Re.La.frame$Counts), by=list(Re.La.frame$Spectrum), FUN="sum")
    colnames(Re.La.ag) <- c("Spectrum", "Re L-alpha")
    
    Os.La.cps <- subset(data$CPS, !(data$Energy < Os.L[2]-0.02 | data$Energy > Os.L[1]+0.02))
    Os.file <- subset(data$Spectrum, !(data$Energy < Os.L[2]-0.02 | data$Energy > Os.L[1]+0.02))
    Os.La.frame <- data.frame(is.0(Os.La.cps, Os.file))
    colnames(Os.La.frame) <- c("Counts", "Spectrum")
    Os.La.ag <- aggregate(list(Os.La.frame$Counts), by=list(Os.La.frame$Spectrum), FUN="sum")
    colnames(Os.La.ag) <- c("Spectrum", "Os L-alpha")
    
    Ir.La.cps <- subset(data$CPS, !(data$Energy < Ir.L[2]-0.02 | data$Energy > Ir.L[1]+0.02))
    Ir.file <- subset(data$Spectrum, !(data$Energy < Ir.L[2]-0.02 | data$Energy > Ir.L[1]+0.02))
    Ir.La.frame <- data.frame(is.0(Ir.La.cps, Ir.file))
    colnames(Ir.La.frame) <- c("Counts", "Spectrum")
    Ir.La.ag <- aggregate(list(Ir.La.frame$Counts), by=list(Ir.La.frame$Spectrum), FUN="sum")
    colnames(Ir.La.ag) <- c("Spectrum", "Ir L-alpha")
    
    Pt.La.cps <- subset(data$CPS, !(data$Energy < Pt.L[2]-0.02 | data$Energy > Pt.L[1]+0.02))
    Pt.file <- subset(data$Spectrum, !(data$Energy < Pt.L[2]-0.02 | data$Energy > Pt.L[1]+0.02))
    Pt.La.frame <- data.frame(is.0(Pt.La.cps, Pt.file))
    colnames(Pt.La.frame) <- c("Counts", "Spectrum")
    Pt.La.ag <- aggregate(list(Pt.La.frame$Counts), by=list(Pt.La.frame$Spectrum), FUN="sum")
    colnames(Pt.La.ag) <- c("Spectrum", "Pt L-alpha")
    
    Au.La.cps <- subset(data$CPS, !(data$Energy < Au.L[2]-0.02 | data$Energy > Au.L[1]+0.02))
    Au.file <- subset(data$Spectrum, !(data$Energy < Au.L[2]-0.02 | data$Energy > Au.L[1]+0.02))
    Au.La.frame <- data.frame(is.0(Au.La.cps, Au.file))
    colnames(Au.La.frame) <- c("Counts", "Spectrum")
    Au.La.ag <- aggregate(list(Au.La.frame$Counts), by=list(Au.La.frame$Spectrum), FUN="sum")
    colnames(Au.La.ag) <- c("Spectrum", "Au L-alpha")
    
    Hg.La.cps <- subset(data$CPS, !(data$Energy < Hg.L[2]-0.02 | data$Energy > Hg.L[1]+0.02))
    Hg.file <- subset(data$Spectrum, !(data$Energy < Hg.L[2]-0.02 | data$Energy > Hg.L[1]+0.02))
    Hg.La.frame <- data.frame(is.0(Hg.La.cps, Hg.file))
    colnames(Hg.La.frame) <- c("Counts", "Spectrum")
    Hg.La.ag <- aggregate(list(Hg.La.frame$Counts), by=list(Hg.La.frame$Spectrum), FUN="sum")
    colnames(Hg.La.ag) <- c("Spectrum", "Hg L-alpha")
    
    Tl.La.cps <- subset(data$CPS, !(data$Energy < Tl.L[2]-0.02 | data$Energy > Tl.L[1]+0.02))
    Tl.file <- subset(data$Spectrum, !(data$Energy < Tl.L[2]-0.02 | data$Energy > Tl.L[1]+0.02))
    Tl.La.frame <- data.frame(is.0(Tl.La.cps, Tl.file))
    colnames(Tl.La.frame) <- c("Counts", "Spectrum")
    Tl.La.ag <- aggregate(list(Tl.La.frame$Counts), by=list(Tl.La.frame$Spectrum), FUN="sum")
    colnames(Tl.La.ag) <- c("Spectrum", "Tl L-alpha")
    
    Pb.La.cps <- subset(data$CPS, !(data$Energy < Pb.L[2]-0.02 | data$Energy > Pb.L[1]+0.02))
    Pb.file <- subset(data$Spectrum, !(data$Energy < Pb.L[2]-0.02 | data$Energy > Pb.L[1]+0.02))
    Pb.La.frame <- data.frame(is.0(Pb.La.cps, Pb.file))
    colnames(Pb.La.frame) <- c("Counts", "Spectrum")
    Pb.La.ag <- aggregate(list(Pb.La.frame$Counts), by=list(Pb.La.frame$Spectrum), FUN="sum")
    colnames(Pb.La.ag) <- c("Spectrum", "Pb L-alpha")
    
    Bi.La.cps <- subset(data$CPS, !(data$Energy < Bi.L[2]-0.02 | data$Energy > Bi.L[1]+0.02))
    Bi.file <- subset(data$Spectrum, !(data$Energy < Bi.L[2]-0.02 | data$Energy > Bi.L[1]+0.02))
    Bi.La.frame <- data.frame(is.0(Bi.La.cps, Bi.file))
    colnames(Bi.La.frame) <- c("Counts", "Spectrum")
    Bi.La.ag <- aggregate(list(Bi.La.frame$Counts), by=list(Bi.La.frame$Spectrum), FUN="sum")
    colnames(Bi.La.ag) <- c("Spectrum", "Bi L-alpha")
    
    Po.La.cps <- subset(data$CPS, !(data$Energy < Po.L[2]-0.02 | data$Energy > Po.L[1]+0.02))
    Po.file <- subset(data$Spectrum, !(data$Energy < Po.L[2]-0.02 | data$Energy > Po.L[1]+0.02))
    Po.La.frame <- data.frame(is.0(Po.La.cps, Po.file))
    colnames(Po.La.frame) <- c("Counts", "Spectrum")
    Po.La.ag <- aggregate(list(Po.La.frame$Counts), by=list(Po.La.frame$Spectrum), FUN="sum")
    colnames(Po.La.ag) <- c("Spectrum", "Po L-alpha")
    
    At.La.cps <- subset(data$CPS, !(data$Energy < At.L[2]-0.02 | data$Energy > At.L[1]+0.02))
    At.file <- subset(data$Spectrum, !(data$Energy < At.L[2]-0.02 | data$Energy > At.L[1]+0.02))
    At.La.frame <- data.frame(is.0(At.La.cps, At.file))
    colnames(At.La.frame) <- c("Counts", "Spectrum")
    At.La.ag <- aggregate(list(At.La.frame$Counts), by=list(At.La.frame$Spectrum), FUN="sum")
    colnames(At.La.ag) <- c("Spectrum", "At L-alpha")
    
    Rn.La.cps <- subset(data$CPS, !(data$Energy < Rn.L[2]-0.02 | data$Energy > Rn.L[1]+0.02))
    Rn.file <- subset(data$Spectrum, !(data$Energy < Rn.L[2]-0.02 | data$Energy > Rn.L[1]+0.02))
    Rn.La.frame <- data.frame(is.0(Rn.La.cps, Rn.file))
    colnames(Rn.La.frame) <- c("Counts", "Spectrum")
    Rn.La.ag <- aggregate(list(Rn.La.frame$Counts), by=list(Rn.La.frame$Spectrum), FUN="sum")
    colnames(Rn.La.ag) <- c("Spectrum", "Rn L-alpha")
    
    Fr.La.cps <- subset(data$CPS, !(data$Energy < Fr.L[2]-0.02 | data$Energy > Fr.L[1]+0.02))
    Fr.file <- subset(data$Spectrum, !(data$Energy < Fr.L[2]-0.02 | data$Energy > Fr.L[1]+0.02))
    Fr.La.frame <- data.frame(is.0(Fr.La.cps, Fr.file))
    colnames(Fr.La.frame) <- c("Counts", "Spectrum")
    Fr.La.ag <- aggregate(list(Fr.La.frame$Counts), by=list(Fr.La.frame$Spectrum), FUN="sum")
    colnames(Fr.La.ag) <- c("Spectrum", "Fr L-alpha")
    
    Ra.La.cps <- subset(data$CPS, !(data$Energy < Ra.L[2]-0.02 | data$Energy > Ra.L[1]+0.02))
    Ra.file <- subset(data$Spectrum, !(data$Energy < Ra.L[2]-0.02 | data$Energy > Ra.L[1]+0.02))
    Ra.La.frame <- data.frame(is.0(Ra.La.cps, Ra.file))
    colnames(Ra.La.frame) <- c("Counts", "Spectrum")
    Ra.La.ag <- aggregate(list(Ra.La.frame$Counts), by=list(Ra.La.frame$Spectrum), FUN="sum")
    colnames(Ra.La.ag) <- c("Spectrum", "Ra L-alpha")
    
    Ac.La.cps <- subset(data$CPS, !(data$Energy < Ac.L[2]-0.02 | data$Energy > Ac.L[1]+0.02))
    Ac.file <- subset(data$Spectrum, !(data$Energy < Ac.L[2]-0.02 | data$Energy > Ac.L[1]+0.02))
    Ac.La.frame <- data.frame(is.0(Ac.La.cps, Ac.file))
    colnames(Ac.La.frame) <- c("Counts", "Spectrum")
    Ac.La.ag <- aggregate(list(Ac.La.frame$Counts), by=list(Ac.La.frame$Spectrum), FUN="sum")
    colnames(Ac.La.ag) <- c("Spectrum", "Ac L-alpha")
    
    Th.La.cps <- subset(data$CPS, !(data$Energy < Th.L[2]-0.02 | data$Energy > Th.L[1]+0.02))
    Th.file <- subset(data$Spectrum, !(data$Energy < Th.L[2]-0.02 | data$Energy > Th.L[1]+0.02))
    Th.La.frame <- data.frame(is.0(Th.La.cps, Th.file))
    colnames(Th.La.frame) <- c("Counts", "Spectrum")
    Th.La.ag <- aggregate(list(Th.La.frame$Counts), by=list(Th.La.frame$Spectrum), FUN="sum")
    colnames(Th.La.ag) <- c("Spectrum", "Th L-alpha")
    
    Pa.La.cps <- subset(data$CPS, !(data$Energy < Pa.L[2]-0.02 | data$Energy > Pa.L[1]+0.02))
    Pa.file <- subset(data$Spectrum, !(data$Energy < Pa.L[2]-0.02 | data$Energy > Pa.L[1]+0.02))
    Pa.La.frame <- data.frame(is.0(Pa.La.cps, Pa.file))
    colnames(Pa.La.frame) <- c("Counts", "Spectrum")
    Pa.La.ag <- aggregate(list(Pa.La.frame$Counts), by=list(Pa.La.frame$Spectrum), FUN="sum")
    colnames(Pa.La.ag) <- c("Spectrum", "Pa L-alpha")
    
    U.La.cps <- subset(data$CPS, !(data$Energy < U.L[2]-0.02 | data$Energy > U.L[1]+0.02))
    U.file <- subset(data$Spectrum, !(data$Energy < U.L[2]-0.02 | data$Energy > U.L[1]+0.02))
    U.La.frame <- data.frame(is.0(U.La.cps, U.file))
    colnames(U.La.frame) <- c("Counts", "Spectrum")
    U.La.ag <- aggregate(list(U.La.frame$Counts), by=list(U.La.frame$Spectrum), FUN="sum")
    colnames(U.La.ag) <- c("Spectrum", "U L-alpha")
    
    Pu.La.cps <- subset(data$CPS, !(data$Energy < Pu.L[2]-0.02 | data$Energy > Pu.L[1]+0.02))
    Pu.file <- subset(data$Spectrum, !(data$Energy < Pu.L[2]-0.02 | data$Energy > Pu.L[1]+0.02))
    Pu.La.frame <- data.frame(is.0(Pu.La.cps, Pu.file))
    colnames(Pu.La.frame) <- c("Counts", "Spectrum")
    Pu.La.ag <- aggregate(list(Pu.La.frame$Counts), by=list(Pu.La.frame$Spectrum), FUN="sum")
    colnames(Pu.La.ag) <- c("Spectrum", "Pu L-alpha")
    
    Mo.Lb.cps <- subset(data$CPS, !(data$Energy < Mo.L[3]-0.02 | data$Energy > Mo.L[5]+0.02))
    Mo.file <- subset(data$Spectrum, !(data$Energy < Mo.L[3]-0.02 | data$Energy > Mo.L[5]+0.02))
    Mo.Lb.frame <- is.0(Mo.Lb.cps,Mo.file)
    colnames(Mo.Lb.frame) <- c("Counts", "Spectrum")
    Mo.Lb.ag <- aggregate(list(Mo.Lb.frame$Counts), by=list(Mo.Lb.frame$Spectrum), FUN="sum")
    colnames(Mo.Lb.ag) <- c("Spectrum", "Mo L-beta")
    
    Tc.Lb.cps <- subset(data$CPS, !(data$Energy < Tc.L[3]-0.02 | data$Energy > Tc.L[5]+0.02))
    Tc.file <- subset(data$Spectrum, !(data$Energy < Tc.L[3]-0.02 | data$Energy > Tc.L[5]+0.02))
    Tc.Lb.frame <- is.0(Tc.Lb.cps,Tc.file)
    colnames(Tc.Lb.frame) <- c("Counts", "Spectrum")
    Tc.Lb.ag <- aggregate(list(Tc.Lb.frame$Counts), by=list(Tc.Lb.frame$Spectrum), FUN="sum")
    colnames(Tc.Lb.ag) <- c("Spectrum", "Tc L-beta")
    
    Ru.Lb.cps <- subset(data$CPS, !(data$Energy < Ru.L[3]-0.02 | data$Energy > Ru.L[5]+0.02))
    Ru.file <- subset(data$Spectrum, !(data$Energy < Ru.L[3]-0.02 | data$Energy > Ru.L[5]+0.02))
    Ru.Lb.frame <- is.0(Ru.Lb.cps,Ru.file)
    colnames(Ru.Lb.frame) <- c("Counts", "Spectrum")
    Ru.Lb.ag <- aggregate(list(Ru.Lb.frame$Counts), by=list(Ru.Lb.frame$Spectrum), FUN="sum")
    colnames(Ru.Lb.ag) <- c("Spectrum", "Ru L-beta")
    
    Rh.Lb.cps <- subset(data$CPS, !(data$Energy < Rh.L[3]-0.02 | data$Energy > Rh.L[5]+0.02))
    Rh.file <- subset(data$Spectrum, !(data$Energy < Rh.L[3]-0.02 | data$Energy > Rh.L[5]+0.02))
    Rh.Lb.frame <- is.0(Rh.Lb.cps,Rh.file)
    colnames(Rh.Lb.frame) <- c("Counts", "Spectrum")
    Rh.Lb.ag <- aggregate(list(Rh.Lb.frame$Counts), by=list(Rh.Lb.frame$Spectrum), FUN="sum")
    colnames(Rh.Lb.ag) <- c("Spectrum", "Rh L-beta")
    
    Rh.Lb.cps <- subset(data$CPS, !(data$Energy < Rh.L[3]-0.02 | data$Energy > Rh.L[5]+0.02))
    Rh.file <- subset(data$Spectrum, !(data$Energy < Rh.L[3]-0.02 | data$Energy > Rh.L[5]+0.02))
    Rh.Lb.frame <- data.frame(is.0(Rh.Lb.cps, Rh.file))
    colnames(Rh.Lb.frame) <- c("Counts", "Spectrum")
    Rh.Lb.ag <- aggregate(list(Rh.Lb.frame$Counts), by=list(Rh.Lb.frame$Spectrum), FUN="sum")
    colnames(Rh.Lb.ag) <- c("Spectrum", "Rh L-beta")
    
    Pd.Lb.cps <- subset(data$CPS, !(data$Energy < Pd.L[3]-0.02 | data$Energy > Pd.L[5]+0.02))
    Pd.file <- subset(data$Spectrum, !(data$Energy < Pd.L[3]-0.02 | data$Energy > Pd.L[5]+0.02))
    Pd.Lb.frame <- data.frame(is.0(Pd.Lb.cps, Pd.file))
    colnames(Pd.Lb.frame) <- c("Counts", "Spectrum")
    Pd.Lb.ag <- aggregate(list(Pd.Lb.frame$Counts), by=list(Pd.Lb.frame$Spectrum), FUN="sum")
    colnames(Pd.Lb.ag) <- c("Spectrum", "Pd L-beta")
    
    Ag.Lb.cps <- subset(data$CPS, !(data$Energy < Ag.L[3]-0.02 | data$Energy > Ag.L[5]+0.02))
    Ag.file <- subset(data$Spectrum, !(data$Energy < Ag.L[3]-0.02 | data$Energy > Ag.L[5]+0.02))
    Ag.Lb.frame <- data.frame(is.0(Ag.Lb.cps, Ag.file))
    colnames(Ag.Lb.frame) <- c("Counts", "Spectrum")
    Ag.Lb.ag <- aggregate(list(Ag.Lb.frame$Counts), by=list(Ag.Lb.frame$Spectrum), FUN="sum")
    colnames(Ag.Lb.ag) <- c("Spectrum", "Ag L-beta")
    
    Cd.Lb.cps <- subset(data$CPS, !(data$Energy < Cd.L[3]-0.02 | data$Energy > Cd.L[5]+0.02))
    Cd.file <- subset(data$Spectrum, !(data$Energy < Cd.L[3]-0.02 | data$Energy > Cd.L[5]+0.02))
    Cd.Lb.frame <- data.frame(is.0(Cd.Lb.cps, Cd.file))
    colnames(Cd.Lb.frame) <- c("Counts", "Spectrum")
    Cd.Lb.ag <- aggregate(list(Cd.Lb.frame$Counts), by=list(Cd.Lb.frame$Spectrum), FUN="sum")
    colnames(Cd.Lb.ag) <- c("Spectrum", "Cd L-beta")
    
    In.Lb.cps <- subset(data$CPS, !(data$Energy < In.L[3]-0.02 | data$Energy > In.L[5]+0.02))
    In.file <- subset(data$Spectrum, !(data$Energy < In.L[3]-0.02 | data$Energy > In.L[5]+0.02))
    In.Lb.frame <- data.frame(is.0(In.Lb.cps, In.file))
    colnames(In.Lb.frame) <- c("Counts", "Spectrum")
    In.Lb.ag <- aggregate(list(In.Lb.frame$Counts), by=list(In.Lb.frame$Spectrum), FUN="sum")
    colnames(In.Lb.ag) <- c("Spectrum", "In L-beta")
    
    Sn.Lb.cps <- subset(data$CPS, !(data$Energy < Sn.L[3]-0.02 | data$Energy > Sn.L[5]+0.02))
    Sn.file <- subset(data$Spectrum, !(data$Energy < Sn.L[3]-0.02 | data$Energy > Sn.L[5]+0.02))
    Sn.Lb.frame <- data.frame(is.0(Sn.Lb.cps, Sn.file))
    colnames(Sn.Lb.frame) <- c("Counts", "Spectrum")
    Sn.Lb.ag <- aggregate(list(Sn.Lb.frame$Counts), by=list(Sn.Lb.frame$Spectrum), FUN="sum")
    colnames(Sn.Lb.ag) <- c("Spectrum", "Sn L-beta")
    
    Sb.Lb.cps <- subset(data$CPS, !(data$Energy < Sb.L[3]-0.02 | data$Energy > Sb.L[5]+0.02))
    Sb.file <- subset(data$Spectrum, !(data$Energy < Sb.L[3]-0.02 | data$Energy > Sb.L[5]+0.02))
    Sb.Lb.frame <- data.frame(is.0(Sb.Lb.cps, Sb.file))
    colnames(Sb.Lb.frame) <- c("Counts", "Spectrum")
    Sb.Lb.ag <- aggregate(list(Sb.Lb.frame$Counts), by=list(Sb.Lb.frame$Spectrum), FUN="sum")
    colnames(Sb.Lb.ag) <- c("Spectrum", "Sb L-beta")
    
    Te.Lb.cps <- subset(data$CPS, !(data$Energy < Te.L[3]-0.02 | data$Energy > Te.L[5]+0.02))
    Te.file <- subset(data$Spectrum, !(data$Energy < Te.L[3]-0.02 | data$Energy > Te.L[5]+0.02))
    Te.Lb.frame <- data.frame(is.0(Te.Lb.cps, Te.file))
    colnames(Te.Lb.frame) <- c("Counts", "Spectrum")
    Te.Lb.ag <- aggregate(list(Te.Lb.frame$Counts), by=list(Te.Lb.frame$Spectrum), FUN="sum")
    colnames(Te.Lb.ag) <- c("Spectrum", "Te L-beta")
    
    I.Lb.cps <- subset(data$CPS, !(data$Energy < I.L[3]-0.02 | data$Energy > I.L[5]+0.02))
    I.file <- subset(data$Spectrum, !(data$Energy < I.L[3]-0.02 | data$Energy > I.L[5]+0.02))
    I.Lb.frame <- data.frame(is.0(I.Lb.cps, I.file))
    colnames(I.Lb.frame) <- c("Counts", "Spectrum")
    I.Lb.ag <- aggregate(list(I.Lb.frame$Counts), by=list(I.Lb.frame$Spectrum), FUN="sum")
    colnames(I.Lb.ag) <- c("Spectrum", "I L-beta")
    
    Xe.Lb.cps <- subset(data$CPS, !(data$Energy < Xe.L[3]-0.02 | data$Energy > Xe.L[5]+0.02))
    Xe.file <- subset(data$Spectrum, !(data$Energy < Xe.L[3]-0.02 | data$Energy > Xe.L[5]+0.02))
    Xe.Lb.frame <- data.frame(is.0(Xe.Lb.cps, Xe.file))
    colnames(Xe.Lb.frame) <- c("Counts", "Spectrum")
    Xe.Lb.ag <- aggregate(list(Xe.Lb.frame$Counts), by=list(Xe.Lb.frame$Spectrum), FUN="sum")
    colnames(Xe.Lb.ag) <- c("Spectrum", "Xe L-beta")
    
    Cs.Lb.cps <- subset(data$CPS, !(data$Energy < Cs.L[3]-0.02 | data$Energy > Cs.L[5]+0.02))
    Cs.file <- subset(data$Spectrum, !(data$Energy < Cs.L[3]-0.02 | data$Energy > Cs.L[5]+0.02))
    Cs.Lb.frame <- data.frame(is.0(Cs.Lb.cps, Cs.file))
    colnames(Cs.Lb.frame) <- c("Counts", "Spectrum")
    Cs.Lb.ag <- aggregate(list(Cs.Lb.frame$Counts), by=list(Cs.Lb.frame$Spectrum), FUN="sum")
    colnames(Cs.Lb.ag) <- c("Spectrum", "Cs L-beta")
    
    Ba.Lb.cps <- subset(data$CPS, !(data$Energy < Ba.L[3]-0.02 | data$Energy > Ba.L[5]+0.02))
    Ba.file <- subset(data$Spectrum, !(data$Energy < Ba.L[3]-0.02 | data$Energy > Ba.L[5]+0.02))
    Ba.Lb.frame <- data.frame(is.0(Ba.Lb.cps, Ba.file))
    colnames(Ba.Lb.frame) <- c("Counts", "Spectrum")
    Ba.Lb.ag <- aggregate(list(Ba.Lb.frame$Counts), by=list(Ba.Lb.frame$Spectrum), FUN="sum")
    colnames(Ba.Lb.ag) <- c("Spectrum", "Ba L-beta")
    
    La.Lb.cps <- subset(data$CPS, !(data$Energy < La.L[3]-0.02 | data$Energy > La.L[5]+0.02))
    La.file <- subset(data$Spectrum, !(data$Energy < La.L[3]-0.02 | data$Energy > La.L[5]+0.02))
    La.Lb.frame <- data.frame(is.0(La.Lb.cps, La.file))
    colnames(La.Lb.frame) <- c("Counts", "Spectrum")
    La.Lb.ag <- aggregate(list(La.Lb.frame$Counts), by=list(La.Lb.frame$Spectrum), FUN="sum")
    colnames(La.Lb.ag) <- c("Spectrum", "La L-beta")
    
    Ce.Lb.cps <- subset(data$CPS, !(data$Energy < Ce.L[3]-0.02 | data$Energy > Ce.L[5]+0.02))
    Ce.file <- subset(data$Spectrum, !(data$Energy < Ce.L[3]-0.02 | data$Energy > Ce.L[5]+0.02))
    Ce.Lb.frame <- data.frame(is.0(Ce.Lb.cps, Ce.file))
    colnames(Ce.Lb.frame) <- c("Counts", "Spectrum")
    Ce.Lb.ag <- aggregate(list(Ce.Lb.frame$Counts), by=list(Ce.Lb.frame$Spectrum), FUN="sum")
    colnames(Ce.Lb.ag) <- c("Spectrum", "Ce L-beta")
    
    Pr.Lb.cps <- subset(data$CPS, !(data$Energy < Pr.L[3]-0.02 | data$Energy > Pr.L[5]+0.02))
    Pr.file <- subset(data$Spectrum, !(data$Energy < Pr.L[3]-0.02 | data$Energy > Pr.L[5]+0.02))
    Pr.Lb.frame <- data.frame(is.0(Pr.Lb.cps, Pr.file))
    colnames(Pr.Lb.frame) <- c("Counts", "Spectrum")
    Pr.Lb.ag <- aggregate(list(Pr.Lb.frame$Counts), by=list(Pr.Lb.frame$Spectrum), FUN="sum")
    colnames(Pr.Lb.ag) <- c("Spectrum", "Pr L-beta")
    
    Nd.Lb.cps <- subset(data$CPS, !(data$Energy < Nd.L[3]-0.02 | data$Energy > Nd.L[5]+0.02))
    Nd.file <- subset(data$Spectrum, !(data$Energy < Nd.L[3]-0.02 | data$Energy > Nd.L[5]+0.02))
    Nd.Lb.frame <- data.frame(is.0(Nd.Lb.cps, Nd.file))
    colnames(Nd.Lb.frame) <- c("Counts", "Spectrum")
    Nd.Lb.ag <- aggregate(list(Nd.Lb.frame$Counts), by=list(Nd.Lb.frame$Spectrum), FUN="sum")
    colnames(Nd.Lb.ag) <- c("Spectrum", "Nd L-beta")
    
    Pm.Lb.cps <- subset(data$CPS, !(data$Energy < Pm.L[3]-0.02 | data$Energy > Pm.L[5]+0.02))
    Pm.file <- subset(data$Spectrum, !(data$Energy < Pm.L[3]-0.02 | data$Energy > Pm.L[5]+0.02))
    Pm.Lb.frame <- data.frame(is.0(Pm.Lb.cps, Pm.file))
    colnames(Pm.Lb.frame) <- c("Counts", "Spectrum")
    Pm.Lb.ag <- aggregate(list(Pm.Lb.frame$Counts), by=list(Pm.Lb.frame$Spectrum), FUN="sum")
    colnames(Pm.Lb.ag) <- c("Spectrum", "Pm L-beta")
    
    Sm.Lb.cps <- subset(data$CPS, !(data$Energy < Sm.L[3]-0.02 | data$Energy > Sm.L[5]+0.02))
    Sm.file <- subset(data$Spectrum, !(data$Energy < Sm.L[3]-0.02 | data$Energy > Sm.L[5]+0.02))
    Sm.Lb.frame <- data.frame(is.0(Sm.Lb.cps, Sm.file))
    colnames(Sm.Lb.frame) <- c("Counts", "Spectrum")
    Sm.Lb.ag <- aggregate(list(Sm.Lb.frame$Counts), by=list(Sm.Lb.frame$Spectrum), FUN="sum")
    colnames(Sm.Lb.ag) <- c("Spectrum", "Sm L-beta")
    
    Eu.Lb.cps <- subset(data$CPS, !(data$Energy < Eu.L[3]-0.02 | data$Energy > Eu.L[5]+0.02))
    Eu.file <- subset(data$Spectrum, !(data$Energy < Eu.L[3]-0.02 | data$Energy > Eu.L[5]+0.02))
    Eu.Lb.frame <- data.frame(is.0(Eu.Lb.cps, Eu.file))
    colnames(Eu.Lb.frame) <- c("Counts", "Spectrum")
    Eu.Lb.ag <- aggregate(list(Eu.Lb.frame$Counts), by=list(Eu.Lb.frame$Spectrum), FUN="sum")
    colnames(Eu.Lb.ag) <- c("Spectrum", "Eu L-beta")
    
    Gd.Lb.cps <- subset(data$CPS, !(data$Energy < Gd.L[3]-0.02 | data$Energy > Gd.L[5]+0.02))
    Gd.file <- subset(data$Spectrum, !(data$Energy < Gd.L[3]-0.02 | data$Energy > Gd.L[5]+0.02))
    Gd.Lb.frame <- data.frame(is.0(Gd.Lb.cps, Gd.file))
    colnames(Gd.Lb.frame) <- c("Counts", "Spectrum")
    Gd.Lb.ag <- aggregate(list(Gd.Lb.frame$Counts), by=list(Gd.Lb.frame$Spectrum), FUN="sum")
    colnames(Gd.Lb.ag) <- c("Spectrum", "Gd L-beta")
    
    Tb.Lb.cps <- subset(data$CPS, !(data$Energy < Tb.L[3]-0.02 | data$Energy > Tb.L[5]+0.02))
    Tb.file <- subset(data$Spectrum, !(data$Energy < Tb.L[3]-0.02 | data$Energy > Tb.L[5]+0.02))
    Tb.Lb.frame <- data.frame(is.0(Tb.Lb.cps, Tb.file))
    colnames(Tb.Lb.frame) <- c("Counts", "Spectrum")
    Tb.Lb.ag <- aggregate(list(Tb.Lb.frame$Counts), by=list(Tb.Lb.frame$Spectrum), FUN="sum")
    colnames(Tb.Lb.ag) <- c("Spectrum", "Tb L-beta")
    
    Dy.Lb.cps <- subset(data$CPS, !(data$Energy < Dy.L[3]-0.02 | data$Energy > Dy.L[5]+0.02))
    Dy.file <- subset(data$Spectrum, !(data$Energy < Dy.L[3]-0.02 | data$Energy > Dy.L[5]+0.02))
    Dy.Lb.frame <- data.frame(is.0(Dy.Lb.cps, Dy.file))
    colnames(Dy.Lb.frame) <- c("Counts", "Spectrum")
    Dy.Lb.ag <- aggregate(list(Dy.Lb.frame$Counts), by=list(Dy.Lb.frame$Spectrum), FUN="sum")
    colnames(Dy.Lb.ag) <- c("Spectrum", "Dy L-beta")
    
    Ho.Lb.cps <- subset(data$CPS, !(data$Energy < Ho.L[3]-0.02 | data$Energy > Ho.L[5]+0.02))
    Ho.file <- subset(data$Spectrum, !(data$Energy < Ho.L[3]-0.02 | data$Energy > Ho.L[5]+0.02))
    Ho.Lb.frame <- data.frame(is.0(Ho.Lb.cps, Ho.file))
    colnames(Ho.Lb.frame) <- c("Counts", "Spectrum")
    Ho.Lb.ag <- aggregate(list(Ho.Lb.frame$Counts), by=list(Ho.Lb.frame$Spectrum), FUN="sum")
    colnames(Ho.Lb.ag) <- c("Spectrum", "Ho L-beta")
    
    Er.Lb.cps <- subset(data$CPS, !(data$Energy < Er.L[3]-0.02 | data$Energy > Er.L[5]+0.02))
    Er.file <- subset(data$Spectrum, !(data$Energy < Er.L[3]-0.02 | data$Energy > Er.L[5]+0.02))
    Er.Lb.frame <- data.frame(is.0(Er.Lb.cps, Er.file))
    colnames(Er.Lb.frame) <- c("Counts", "Spectrum")
    Er.Lb.ag <- aggregate(list(Er.Lb.frame$Counts), by=list(Er.Lb.frame$Spectrum), FUN="sum")
    colnames(Er.Lb.ag) <- c("Spectrum", "Er L-beta")
    
    Tm.Lb.cps <- subset(data$CPS, !(data$Energy < Tm.L[3]-0.02 | data$Energy > Tm.L[5]+0.02))
    Tm.file <- subset(data$Spectrum, !(data$Energy < Tm.L[3]-0.02 | data$Energy > Tm.L[5]+0.02))
    Tm.Lb.frame <- data.frame(is.0(Tm.Lb.cps, Tm.file))
    colnames(Tm.Lb.frame) <- c("Counts", "Spectrum")
    Tm.Lb.ag <- aggregate(list(Tm.Lb.frame$Counts), by=list(Tm.Lb.frame$Spectrum), FUN="sum")
    colnames(Tm.Lb.ag) <- c("Spectrum", "Tm L-beta")
    
    Yb.Lb.cps <- subset(data$CPS, !(data$Energy < Yb.L[3]-0.02 | data$Energy > Yb.L[5]+0.02))
    Yb.file <- subset(data$Spectrum, !(data$Energy < Yb.L[3]-0.02 | data$Energy > Yb.L[5]+0.02))
    Yb.Lb.frame <- data.frame(is.0(Yb.Lb.cps, Yb.file))
    colnames(Yb.Lb.frame) <- c("Counts", "Spectrum")
    Yb.Lb.ag <- aggregate(list(Yb.Lb.frame$Counts), by=list(Yb.Lb.frame$Spectrum), FUN="sum")
    colnames(Yb.Lb.ag) <- c("Spectrum", "Yb L-beta")
    
    Lu.Lb.cps <- subset(data$CPS, !(data$Energy < Lu.L[3]-0.02 | data$Energy > Lu.L[5]+0.02))
    Lu.file <- subset(data$Spectrum, !(data$Energy < Lu.L[3]-0.02 | data$Energy > Lu.L[5]+0.02))
    Lu.Lb.frame <- data.frame(is.0(Lu.Lb.cps, Lu.file))
    colnames(Lu.Lb.frame) <- c("Counts", "Spectrum")
    Lu.Lb.ag <- aggregate(list(Lu.Lb.frame$Counts), by=list(Lu.Lb.frame$Spectrum), FUN="sum")
    colnames(Lu.Lb.ag) <- c("Spectrum", "Lu L-beta")
    
    Hf.Lb.cps <- subset(data$CPS, !(data$Energy < Hf.L[3]-0.02 | data$Energy > Hf.L[5]+0.02))
    Hf.file <- subset(data$Spectrum, !(data$Energy < Hf.L[3]-0.02 | data$Energy > Hf.L[5]+0.02))
    Hf.Lb.frame <- data.frame(is.0(Hf.Lb.cps, Hf.file))
    colnames(Hf.Lb.frame) <- c("Counts", "Spectrum")
    Hf.Lb.ag <- aggregate(list(Hf.Lb.frame$Counts), by=list(Hf.Lb.frame$Spectrum), FUN="sum")
    colnames(Hf.Lb.ag) <- c("Spectrum", "Hf L-beta")
    
    Ta.Lb.cps <- subset(data$CPS, !(data$Energy < Ta.L[3]-0.02 | data$Energy > Ta.L[5]+0.02))
    Ta.file <- subset(data$Spectrum, !(data$Energy < Ta.L[3]-0.02 | data$Energy > Ta.L[5]+0.02))
    Ta.Lb.frame <- data.frame(is.0(Ta.Lb.cps, Ta.file))
    colnames(Ta.Lb.frame) <- c("Counts", "Spectrum")
    Ta.Lb.ag <- aggregate(list(Ta.Lb.frame$Counts), by=list(Ta.Lb.frame$Spectrum), FUN="sum")
    colnames(Ta.Lb.ag) <- c("Spectrum", "Ta L-beta")
    
    W.Lb.cps <- subset(data$CPS, !(data$Energy < W.L[3]-0.02 | data$Energy > W.L[5]+0.02))
    W.file <- subset(data$Spectrum, !(data$Energy < W.L[3]-0.02 | data$Energy > W.L[5]+0.02))
    W.Lb.frame <- data.frame(is.0(W.Lb.cps, W.file))
    colnames(W.Lb.frame) <- c("Counts", "Spectrum")
    W.Lb.ag <- aggregate(list(W.Lb.frame$Counts), by=list(W.Lb.frame$Spectrum), FUN="sum")
    colnames(W.Lb.ag) <- c("Spectrum", "W L-beta")
    
    Re.Lb.cps <- subset(data$CPS, !(data$Energy < Re.L[3]-0.02 | data$Energy > Re.L[5]+0.02))
    Re.file <- subset(data$Spectrum, !(data$Energy < Re.L[3]-0.02 | data$Energy > Re.L[5]+0.02))
    Re.Lb.frame <- data.frame(is.0(Re.Lb.cps, Re.file))
    colnames(Re.Lb.frame) <- c("Counts", "Spectrum")
    Re.Lb.ag <- aggregate(list(Re.Lb.frame$Counts), by=list(Re.Lb.frame$Spectrum), FUN="sum")
    colnames(Re.Lb.ag) <- c("Spectrum", "Re L-beta")
    
    Os.Lb.cps <- subset(data$CPS, !(data$Energy < Os.L[3]-0.02 | data$Energy > Os.L[5]+0.02))
    Os.file <- subset(data$Spectrum, !(data$Energy < Os.L[3]-0.02 | data$Energy > Os.L[5]+0.02))
    Os.Lb.frame <- data.frame(is.0(Os.Lb.cps, Os.file))
    colnames(Os.Lb.frame) <- c("Counts", "Spectrum")
    Os.Lb.ag <- aggregate(list(Os.Lb.frame$Counts), by=list(Os.Lb.frame$Spectrum), FUN="sum")
    colnames(Os.Lb.ag) <- c("Spectrum", "Os L-beta")
    
    Ir.Lb.cps <- subset(data$CPS, !(data$Energy < Ir.L[3]-0.02 | data$Energy > Ir.L[5]+0.02))
    Ir.file <- subset(data$Spectrum, !(data$Energy < Ir.L[3]-0.02 | data$Energy > Ir.L[5]+0.02))
    Ir.Lb.frame <- data.frame(is.0(Ir.Lb.cps, Ir.file))
    colnames(Ir.Lb.frame) <- c("Counts", "Spectrum")
    Ir.Lb.ag <- aggregate(list(Ir.Lb.frame$Counts), by=list(Ir.Lb.frame$Spectrum), FUN="sum")
    colnames(Ir.Lb.ag) <- c("Spectrum", "Ir L-beta")
    
    Pt.Lb.cps <- subset(data$CPS, !(data$Energy < Pt.L[3]-0.02 | data$Energy > Pt.L[5]+0.02))
    Pt.file <- subset(data$Spectrum, !(data$Energy < Pt.L[3]-0.02 | data$Energy > Pt.L[5]+0.02))
    Pt.Lb.frame <- data.frame(is.0(Pt.Lb.cps, Pt.file))
    colnames(Pt.Lb.frame) <- c("Counts", "Spectrum")
    Pt.Lb.ag <- aggregate(list(Pt.Lb.frame$Counts), by=list(Pt.Lb.frame$Spectrum), FUN="sum")
    colnames(Pt.Lb.ag) <- c("Spectrum", "Pt L-beta")
    
    Au.Lb.cps <- subset(data$CPS, !(data$Energy < Au.L[3]-0.02 | data$Energy > Au.L[5]+0.02))
    Au.file <- subset(data$Spectrum, !(data$Energy < Au.L[3]-0.02 | data$Energy > Au.L[5]+0.02))
    Au.Lb.frame <- data.frame(is.0(Au.Lb.cps, Au.file))
    colnames(Au.Lb.frame) <- c("Counts", "Spectrum")
    Au.Lb.ag <- aggregate(list(Au.Lb.frame$Counts), by=list(Au.Lb.frame$Spectrum), FUN="sum")
    colnames(Au.Lb.ag) <- c("Spectrum", "Au L-beta")
    
    Hg.Lb.cps <- subset(data$CPS, !(data$Energy < Hg.L[3]-0.02 | data$Energy > Hg.L[5]+0.02))
    Hg.file <- subset(data$Spectrum, !(data$Energy < Hg.L[3]-0.02 | data$Energy > Hg.L[5]+0.02))
    Hg.Lb.frame <- data.frame(is.0(Hg.Lb.cps, Hg.file))
    colnames(Hg.Lb.frame) <- c("Counts", "Spectrum")
    Hg.Lb.ag <- aggregate(list(Hg.Lb.frame$Counts), by=list(Hg.Lb.frame$Spectrum), FUN="sum")
    colnames(Hg.Lb.ag) <- c("Spectrum", "Hg L-beta")
    
    Tl.Lb.cps <- subset(data$CPS, !(data$Energy < Tl.L[3]-0.02 | data$Energy > Tl.L[5]+0.02))
    Tl.file <- subset(data$Spectrum, !(data$Energy < Tl.L[3]-0.02 | data$Energy > Tl.L[5]+0.02))
    Tl.Lb.frame <- data.frame(is.0(Tl.Lb.cps, Tl.file))
    colnames(Tl.Lb.frame) <- c("Counts", "Spectrum")
    Tl.Lb.ag <- aggregate(list(Tl.Lb.frame$Counts), by=list(Tl.Lb.frame$Spectrum), FUN="sum")
    colnames(Tl.Lb.ag) <- c("Spectrum", "Tl L-beta")
    
    Pb.Lb.cps <- subset(data$CPS, !(data$Energy < Pb.L[3]-0.02 | data$Energy > Pb.L[5]+0.02))
    Pb.file <- subset(data$Spectrum, !(data$Energy < Pb.L[3]-0.02 | data$Energy > Pb.L[5]+0.02))
    Pb.Lb.frame <- data.frame(is.0(Pb.Lb.cps, Pb.file))
    colnames(Pb.Lb.frame) <- c("Counts", "Spectrum")
    Pb.Lb.ag <- aggregate(list(Pb.Lb.frame$Counts), by=list(Pb.Lb.frame$Spectrum), FUN="sum")
    colnames(Pb.Lb.ag) <- c("Spectrum", "Pb L-beta")
    
    Bi.Lb.cps <- subset(data$CPS, !(data$Energy < Bi.L[3]-0.02 | data$Energy > Bi.L[5]+0.02))
    Bi.file <- subset(data$Spectrum, !(data$Energy < Bi.L[3]-0.02 | data$Energy > Bi.L[5]+0.02))
    Bi.Lb.frame <- data.frame(is.0(Bi.Lb.cps, Bi.file))
    colnames(Bi.Lb.frame) <- c("Counts", "Spectrum")
    Bi.Lb.ag <- aggregate(list(Bi.Lb.frame$Counts), by=list(Bi.Lb.frame$Spectrum), FUN="sum")
    colnames(Bi.Lb.ag) <- c("Spectrum", "Bi L-beta")
    
    Po.Lb.cps <- subset(data$CPS, !(data$Energy < Po.L[3]-0.02 | data$Energy > Po.L[5]+0.02))
    Po.file <- subset(data$Spectrum, !(data$Energy < Po.L[3]-0.02 | data$Energy > Po.L[5]+0.02))
    Po.Lb.frame <- data.frame(is.0(Po.Lb.cps, Po.file))
    colnames(Po.Lb.frame) <- c("Counts", "Spectrum")
    Po.Lb.ag <- aggregate(list(Po.Lb.frame$Counts), by=list(Po.Lb.frame$Spectrum), FUN="sum")
    colnames(Po.Lb.ag) <- c("Spectrum", "Po L-beta")
    
    At.Lb.cps <- subset(data$CPS, !(data$Energy < At.L[3]-0.02 | data$Energy > At.L[5]+0.02))
    At.file <- subset(data$Spectrum, !(data$Energy < At.L[3]-0.02 | data$Energy > At.L[5]+0.02))
    At.Lb.frame <- data.frame(is.0(At.Lb.cps, At.file))
    colnames(At.Lb.frame) <- c("Counts", "Spectrum")
    At.Lb.ag <- aggregate(list(At.Lb.frame$Counts), by=list(At.Lb.frame$Spectrum), FUN="sum")
    colnames(At.Lb.ag) <- c("Spectrum", "At L-beta")
    
    Rn.Lb.cps <- subset(data$CPS, !(data$Energy < Rn.L[3]-0.02 | data$Energy > Rn.L[5]+0.02))
    Rn.file <- subset(data$Spectrum, !(data$Energy < Rn.L[3]-0.02 | data$Energy > Rn.L[5]+0.02))
    Rn.Lb.frame <- data.frame(is.0(Rn.Lb.cps, Rn.file))
    colnames(Rn.Lb.frame) <- c("Counts", "Spectrum")
    Rn.Lb.ag <- aggregate(list(Rn.Lb.frame$Counts), by=list(Rn.Lb.frame$Spectrum), FUN="sum")
    colnames(Rn.Lb.ag) <- c("Spectrum", "Rn L-beta")
    
    Fr.Lb.cps <- subset(data$CPS, !(data$Energy < Fr.L[3]-0.02 | data$Energy > Fr.L[5]+0.02))
    Fr.file <- subset(data$Spectrum, !(data$Energy < Fr.L[3]-0.02 | data$Energy > Fr.L[5]+0.02))
    Fr.Lb.frame <- data.frame(is.0(Fr.Lb.cps, Fr.file))
    colnames(Fr.Lb.frame) <- c("Counts", "Spectrum")
    Fr.Lb.ag <- aggregate(list(Fr.Lb.frame$Counts), by=list(Fr.Lb.frame$Spectrum), FUN="sum")
    colnames(Fr.Lb.ag) <- c("Spectrum", "Fr L-beta")
    
    Ra.Lb.cps <- subset(data$CPS, !(data$Energy < Ra.L[3]-0.02 | data$Energy > Ra.L[5]+0.02))
    Ra.file <- subset(data$Spectrum, !(data$Energy < Ra.L[3]-0.02 | data$Energy > Ra.L[5]+0.02))
    Ra.Lb.frame <- data.frame(is.0(Ra.Lb.cps, Ra.file))
    colnames(Ra.Lb.frame) <- c("Counts", "Spectrum")
    Ra.Lb.ag <- aggregate(list(Ra.Lb.frame$Counts), by=list(Ra.Lb.frame$Spectrum), FUN="sum")
    colnames(Ra.Lb.ag) <- c("Spectrum", "Ra L-beta")
    
    Ac.Lb.cps <- subset(data$CPS, !(data$Energy < Ac.L[3]-0.02 | data$Energy > Ac.L[5]+0.02))
    Ac.file <- subset(data$Spectrum, !(data$Energy < Ac.L[3]-0.02 | data$Energy > Ac.L[5]+0.02))
    Ac.Lb.frame <- data.frame(is.0(Ac.Lb.cps, Ac.file))
    colnames(Ac.Lb.frame) <- c("Counts", "Spectrum")
    Ac.Lb.ag <- aggregate(list(Ac.Lb.frame$Counts), by=list(Ac.Lb.frame$Spectrum), FUN="sum")
    colnames(Ac.Lb.ag) <- c("Spectrum", "Ac L-beta")
    
    Th.Lb.cps <- subset(data$CPS, !(data$Energy < Th.L[3]-0.02 | data$Energy > Th.L[5]+0.02))
    Th.file <- subset(data$Spectrum, !(data$Energy < Th.L[3]-0.02 | data$Energy > Th.L[5]+0.02))
    Th.Lb.frame <- data.frame(is.0(Th.Lb.cps, Th.file))
    colnames(Th.Lb.frame) <- c("Counts", "Spectrum")
    Th.Lb.ag <- aggregate(list(Th.Lb.frame$Counts), by=list(Th.Lb.frame$Spectrum), FUN="sum")
    colnames(Th.Lb.ag) <- c("Spectrum", "Th L-beta")
    
    Pa.Lb.cps <- subset(data$CPS, !(data$Energy < Pa.L[3]-0.02 | data$Energy > Pa.L[5]+0.02))
    Pa.file <- subset(data$Spectrum, !(data$Energy < Pa.L[3]-0.02 | data$Energy > Pa.L[5]+0.02))
    Pa.Lb.frame <- data.frame(is.0(Pa.Lb.cps, Pa.file))
    colnames(Pa.Lb.frame) <- c("Counts", "Spectrum")
    Pa.Lb.ag <- aggregate(list(Pa.Lb.frame$Counts), by=list(Pa.Lb.frame$Spectrum), FUN="sum")
    colnames(Pa.Lb.ag) <- c("Spectrum", "Pa L-beta")
    
    U.Lb.cps <- subset(data$CPS, !(data$Energy < U.L[3]-0.02 | data$Energy > U.L[5]+0.02))
    U.file <- subset(data$Spectrum, !(data$Energy < U.L[3]-0.02 | data$Energy > U.L[5]+0.02))
    U.Lb.frame <- data.frame(is.0(U.Lb.cps, U.file))
    colnames(U.Lb.frame) <- c("Counts", "Spectrum")
    U.Lb.ag <- aggregate(list(U.Lb.frame$Counts), by=list(U.Lb.frame$Spectrum), FUN="sum")
    colnames(U.Lb.ag) <- c("Spectrum", "U L-beta")
    
    Pu.Lb.cps <- subset(data$CPS, !(data$Energy < Pu.L[3]-0.02 | data$Energy > Pu.L[5]+0.02))
    Pu.file <- subset(data$Spectrum, !(data$Energy < Pu.L[3]-0.02 | data$Energy > Pu.L[5]+0.02))
    Pu.Lb.frame <- data.frame(is.0(Pu.Lb.cps, Pu.file))
    colnames(Pu.Lb.frame) <- c("Counts", "Spectrum")
    Pu.Lb.ag <- aggregate(list(Pu.Lb.frame$Counts), by=list(Pu.Lb.frame$Spectrum), FUN="sum")
    colnames(Pu.Lb.ag) <- c("Spectrum", "Pu L-beta")

spectra.lines <- Reduce(function(x, y) merge(x, y, all=TRUE), list(Ne.Ka.ag, Ne.Kb.ag, Na.Ka.ag, Na.Kb.ag, Mg.Ka.ag, Mg.Kb.ag, Al.Ka.ag, Al.Kb.ag, Si.Ka.ag, Si.Kb.ag, P.Ka.ag, P.Kb.ag, S.Ka.ag, S.Kb.ag, Cl.Ka.ag, Cl.Kb.ag, Ar.Ka.ag, Ar.Kb.ag, K.Ka.ag, K.Kb.ag, Ca.Ka.ag, Ca.Kb.ag, Sc.Ka.ag, Sc.Kb.ag, Ti.Ka.ag, Ti.Kb.ag, V.Ka.ag, V.Kb.ag, Cr.Ka.ag, Cr.Kb.ag, Mn.Ka.ag, Mn.Kb.ag, Fe.Ka.ag, Fe.Kb.ag, Co.Ka.ag, Co.Kb.ag, Ni.Ka.ag, Ni.Kb.ag, Cu.Ka.ag, Cu.Kb.ag, Zn.Ka.ag, Zn.Kb.ag, Ga.Ka.ag, Ga.Kb.ag, Ge.Ka.ag, Ge.Kb.ag, As.Ka.ag, As.Kb.ag, Se.Ka.ag, Se.Kb.ag, Br.Ka.ag, Br.Kb.ag, Kr.Ka.ag, Kr.Kb.ag, Rb.Ka.ag, Rb.Kb.ag, Sr.Ka.ag, Sr.Kb.ag, Y.Kb.ag, Y.Ka.ag, Zr.Ka.ag, Zr.Kb.ag, Nb.Ka.ag, Nb.Kb.ag, Mo.Ka.ag, Mo.Kb.ag, Mo.La.ag, Mo.Lb.ag, Ru.Ka.ag, Ru.Kb.ag, Ru.La.ag, Ru.Lb.ag, Rh.Ka.ag, Rh.Kb.ag, Rh.La.ag, Rh.Lb.ag, Pd.Ka.ag, Pd.Kb.ag, Pd.La.ag, Pd.Lb.ag, Ag.Ka.ag, Ag.Kb.ag, Ag.La.ag, Ag.Lb.ag, Cd.Ka.ag, Cd.Kb.ag, Cd.La.ag, Cd.Lb.ag,  In.Ka.ag, In.Kb.ag, In.La.ag, Sn.Ka.ag, Sn.Kb.ag, Sn.La.ag, Sn.Lb.ag, Sb.Ka.ag, Sb.Kb.ag, Sb.La.ag, Sb.Lb.ag, Te.Ka.ag, Te.Kb.ag, Te.La.ag, Te.Lb.ag, I.Ka.ag, I.Kb.ag, I.La.ag, I.Lb.ag, Xe.Ka.ag, Xe.Kb.ag, Xe.La.ag, Xe.Lb.ag, Cs.Ka.ag, Cs.Kb.ag, Cs.La.ag, Cs.Lb.ag, Ba.Ka.ag, Ba.Kb.ag, Ba.La.ag, Ba.Lb.ag, La.Ka.ag, La.Kb.ag, La.La.ag, La.Lb.ag, Ce.Ka.ag, Ce.Kb.ag, Ce.La.ag, Ce.Lb.ag, Pr.Ka.ag, Pr.Kb.ag, Pr.La.ag, Pr.Lb.ag, Nd.Ka.ag, Nd.Kb.ag, Nd.La.ag, Nd.Lb.ag, Pm.La.ag, Pm.Lb.ag, Sm.La.ag, Sm.Lb.ag, Eu.La.ag, Eu.Lb.ag, Gd.La.ag, Gd.Lb.ag, Tb.La.ag, Tb.Lb.ag, Dy.La.ag, Dy.Lb.ag, Ho.La.ag, Ho.Lb.ag, Er.La.ag, Er.Lb.ag, Tm.La.ag, Tm.Lb.ag, Yb.La.ag, Yb.Lb.ag, Lu.La.ag, Lu.Lb.ag, Hf.La.ag, Hf.Lb.ag, Ta.La.ag, Ta.Lb.ag, W.La.ag, W.Lb.ag, Re.La.ag, Re.Lb.ag, Os.La.ag, Os.Lb.ag, Ir.La.ag, Ir.Lb.ag, Pt.La.ag, Pt.Lb.ag, Au.La.ag, Au.Lb.ag, Hg.La.ag, Hg.Lb.ag, Tl.La.ag, Tl.Lb.ag, Pb.La.ag, Pb.Lb.ag, Bi.La.ag, Bi.Lb.ag, Po.La.ag, Po.Lb.ag, At.La.ag, At.Lb.ag, Rn.La.ag, Rn.Lb.ag, Fr.La.ag, Fr.Lb.ag, Ra.La.ag, Ra.Lb.ag, Ac.La.ag, Ac.Lb.ag, Th.La.ag, Th.Lb.ag, Pa.La.ag, Pa.Lb.ag, U.La.ag, U.Lb.ag, Pu.La.ag, Pu.Lb.ag))

spectra.lines <- data.frame(spectra.lines)
return(spectra.lines)

}

standard <- c("Spectrum", "Ca.K.alpha", "Ti.K.alpha", "Fe.K.alpha", "Cu.K.alpha", "Zn.K.alpha", "Pb.L.alpha")

standin <- c("Ca.K.alpha", "Ti.K.alpha")
#standard <- c("Spectrum", "Ca.K.alpha", "Ti.K.alpha", "Fe.K.alpha", "Cu.K.alpha", "Zn.K.alpha", "As.K.alpha", "Rb.K.alpha", "Sr.K.alpha", "Y.K.alpha", "Zr.K.alpha", "Nb.K.alpha", "Mo.K.alpha", "Pb.L.beta", "Th.L.alpha", "U.L.alpha")

data <- NULL
if (is.null(data)){ data <- black.diamond.melt}


spectra.line.table.1 <- spectra.line.fn(black.diamond.melt)

x1 <- c("one", "two", "three")
x2 <- c(0, 0, 0)
x3 <- c(0, 0, 0)
x4 <- c(0, 0, 0)
x5 <- c(0, 0, 0)
x6 <- c(0, 0, 0)
x7 <- c(0, 0, 0)

empty.line.table.1 <- data.frame(x1, x2, x3, x4, x5, x6, x7)
colnames(empty.line.table.1) <- standard


concentration.table <- NULL
if (is.null(concentration.table)){ data <- empty.line.table.1}

spectra.line.table <- spectra.line.fn(data)
spectra.line.table
 unique.spec <- seq(1, length(spectra.line.table$Spectrum), 1)
spectra.line.scale <- c("Scale", names(spectra.line.table))


null <- rep(1, length(spectra.line.table$Spectrum))

spectra.line.table.norm <- data.frame(null, spectra.line.table)
colnames(spectra.line.table.norm) <- c("None", names(spectra.line.table))
spectra.line.table.norm








