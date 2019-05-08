library(shiny)
library(ggplot2)
library(pbapply)
library(reshape2)
library(dplyr)
library(DT)
library(gridExtra)
library(rhandsontable)
library(broom)
library(shinyjs)
library(formattable)
library(markdown)
library(rmarkdown)
library(XML)
library(corrplot)
library(scales)
library(caret)
library(randomForest)
library(DescTools)
library(prospectr)
library(pls)
library(baseline)
library(doParallel)
pdf(NULL)


options(shiny.maxRequestSize=30*1024^40)

shinyServer(function(input, output, session) {
    
    

    calFileContents <- reactive(label="calFileContents", {
        
        existingCalFile <- input$calfileinput
        
        if (is.null(existingCalFile)) return(NULL)
        
        
        Calibration <- readRDS(existingCalFile$datapath)
        
        
        
        
        
        Calibration[["Values"]]$Spectrum <- gsub(".spx", "", Calibration[["Values"]]$Spectrum)
        Calibration[["Values"]]$Spectrum <- gsub(".pdz", "", Calibration[["Values"]]$Spectrum)
        Calibration[["Values"]]$Spectrum <- gsub(".CSV", "", Calibration[["Values"]]$Spectrum)
        Calibration[["Values"]]$Spectrum <- gsub(".csv", "", Calibration[["Values"]]$Spectrum)
        Calibration[["Values"]]$Spectrum <- gsub(".spt", "", Calibration[["Values"]]$Spectrum)
        Calibration[["Values"]]$Spectrum <- gsub(".mca", "", Calibration[["Values"]]$Spectrum)
        
        Calibration[["Spectra"]]$Spectrum <- gsub(".spx", "", Calibration[["Spectra"]]$Spectrum)
        Calibration[["Spectra"]]$Spectrum <- gsub(".pdz", "", Calibration[["Spectra"]]$Spectrum)
        Calibration[["Spectra"]]$Spectrum <- gsub(".CSV", "", Calibration[["Spectra"]]$Spectrum)
        Calibration[["Spectra"]]$Spectrum <- gsub(".csv", "", Calibration[["Spectra"]]$Spectrum)
        Calibration[["Spectra"]]$Spectrum <- gsub(".spt", "", Calibration[["Spectra"]]$Spectrum)
        Calibration[["Spectra"]]$Spectrum <- gsub(".mca", "", Calibration[["Spectra"]]$Spectrum)
        

        
        Calibration
        
    })
    
    calMemory <- reactiveValues()
    calMemory$Calibration <- NULL
    
    oldCalCompatibility <- reactive(label="oldCalCompatibility", {
        choice <- if(!is.null(input$calfileinput) && calFileContents()[["FileType"]]=="Spectra"){
            "CSV"
        } else if(!is.null(input$calfileinput) && calFileContents()[["FileType"]]!="Spectra"){
            calFileContents()[["FileType"]]
        } else if(is.null(input$calfileinput)){
            "CSV"
        }
        
        choice
    })
    
    output$filetypeui <- renderUI({
        if(is.null(input$calfileinput)){
            selectInput("filetype", label="Filetype", c("CSV", "TXT", "Net", "Elio", "MCA", "SPX", "PDZ"), selected="CSV")
        } else if(!is.null(input$calfileinput)){
            selectInput("filetype", label="Filetype", c("CSV", "TXT", "Net", "Elio", "MCA", "SPX", "PDZ"), selected=oldCalCompatibility())
        }
        
    })
    
    output$filegrab <- renderUI({
        req(input$filetype)
        if(input$filetype=="CSV") {
            fileInput('file1', 'Choose CSV', multiple=TRUE,
            accept=c(".csv"))
        } else if(input$filetype=="TXT") {
            fileInput('file1', 'Choose TXT', multiple=TRUE,
            accept=c(".txt"))
        } else if(input$filetype=="Net") {
            fileInput('file1', 'Choose Net Counts', multiple=TRUE,
            accept=c(".csv"))
        } else if(input$filetype=="Elio") {
            fileInput('file1', 'Choose Elio Spectra', multiple=TRUE,
            accept=c(".spt"))
        } else if(input$filetype=="MCA") {
            fileInput('file1', 'Choose MCA File', multiple=TRUE,
            accept=c(".mca"))
        } else if(input$filetype=="SPX") {
            fileInput('file1', 'Choose Artax File', multiple=TRUE,
            accept=c(".spx"))
        } else if(input$filetype=="PDZ") {
            fileInput('file1', 'Choose PDZ File', multiple=TRUE,
            accept=c(".pdz"))
        }
        
    })
    
    
    output$variancespectrumui <- renderUI({
        
        if(input$showlegend==FALSE){
            checkboxInput('variancespectrum', "Variance Spectrum", value=TRUE)
        } else if(input$showlegend==TRUE){
            p()
        }
        
    })
    
    
    
    output$gainshiftui <- renderUI({
        
        if(input$advanced==TRUE){
            numericInput('gainshift', "Gain Shift (keV)", min=-1, max=1, value=0)
        } else {
            p()
        }
        
    })
    
    
    output$binaryui <- renderUI({
        
        if(input$advanced==TRUE && input$filetype=="PDZ"){
            numericInput('binaryshift', "Binary Shift (bits)", min=0, max=1000, value=361)
        } else {
            p()
        }
        
    })
    
    
    binaryHold <- reactive({
        
        if(input$advanced==TRUE){
            input$binaryshift
        } else if(input$advanced==FALSE){
            500
        }
        
    })
    
    
    gainshiftHold <- reactive({
        
        if(input$advanced==TRUE){
            input$gainshift
        } else if(input$advanced==FALSE){
            0
        }
        
    })
    
    
    fullSpectraDataTable <- reactive(label="fullSpectraDataTable", {
        
        
        withProgress(message = 'Processing Data', value = 0, {
            
            inFile <- input$file1
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
            
            
            incProgress(1/n)
            Sys.sleep(0.1)
        })
        
        data$Energy <- data$Energy + gainshiftHold()
        
        data
    })
    
    
    
    fullSpectra <- reactive(label="fullSpectra", {
        req(input$file1)
        
        withProgress(message = 'Processing Data', value = 0, {
            
            inFile <- input$file1
            if (is.null(inFile)) return(NULL)
            temp = inFile$name
            temp <- gsub(".csv", "", temp)
            id.seq <- seq(1, 2048,1)
            
            n <- length(temp)*id.seq
            
            n.seq <- seq(1, length(inFile$name), 1)
            
            
            data <- pblapply(n.seq, function(x) csvFrame(filepath=inFile$datapath[x], filename=inFile$name[x]))
            data <- do.call("rbind", data)
            
            
            incProgress(1/n)
            Sys.sleep(0.1)
        })
        
        data$Energy <- data$Energy + gainshiftHold()
        
        data
    })
    
    
    netCounts <- reactive(label="netCounts", {
        req(input$file1)
        withProgress(message = 'Processing Data', value = 0, {
            
            
            inFile <- input$file1
            if (is.null(inFile)) return(NULL)
            
            #inName <- inFile$name
            #inPath <- inFile$datapath
            
            #inList <- list(inName, inPath)
            #names(inList) <- c("inName", "inPath")
            
            
            n <- length(inFile$name)
            net.names <- gsub("\\@.*","",inFile$name)
            
            myfiles = pblapply(inFile$datapath,  read_csv_net)
            
            
            myfiles.frame.list <- pblapply(myfiles, data.frame, stringsAsFactors=FALSE)
            nms = unique(unlist(pblapply(myfiles.frame.list, names)))
            myfiles.frame <- as.data.frame(do.call(rbind, lapply(myfiles.frame.list, "[", nms)))
            myfiles.frame <- as.data.frame(sapply(myfiles.frame, as.numeric))
            
            
            #myfiles.frame$Spectrum <- net.names
            
            united.frame <- data.frame(net.names, myfiles.frame)
            colnames(united.frame) <- c("Spectrum", names(myfiles.frame))
            #united.frame$None <- rep(1, length(united.frame$Spectrum))
            
            
            incProgress(1/n)
            Sys.sleep(0.1)
        })
        
        united.frame <- as.data.frame(united.frame)
        united.frame
        
    })
    
    
    readTXT <- reactive(label="readTXT", {
        req(input$file1)
        withProgress(message = 'Processing Data', value = 0, {
            
            inFile <- input$file1
            if (is.null(inFile)) return(NULL)
            
            n <- length(inFile$datapath)
            names <- inFile$name
            
            myfiles.frame <- as.data.frame(do.call(rbind, lapply(seq(1, n, 1), function(x) readTXTData(filepath=inFile$datapath[x], filename=inFile$name[x]))))
            
            incProgress(1/n)
            Sys.sleep(0.1)
        })
        
        myfiles.frame$Energy <- myfiles.frame$Energy + gainshiftHold()
        
        myfiles.frame
        
    })
    
    readElio <- reactive(label="readElio", {
        req(input$file1)
        withProgress(message = 'Processing Data', value = 0, {
            
            inFile <- input$file1
            if (is.null(inFile)) return(NULL)
            
            n <- length(inFile$datapath)
            names <- inFile$name
            
            myfiles.frame <- as.data.frame(do.call(rbind, lapply(seq(1, n, 1), function(x) readSPTData(filepath=inFile$datapath[x], filename=inFile$name[x]))))
            
            
            incProgress(1/n)
            Sys.sleep(0.1)
        })
        
        myfiles.frame$Energy <- myfiles.frame$Energy + gainshiftHold()
        
        myfiles.frame
        
        
    })
    
    
    readMCA <- reactive(label="readMCA", {
        req(input$file1)
        withProgress(message = 'Processing Data', value = 0, {
            
            inFile <- input$file1
            if (is.null(inFile)) return(NULL)
            
            n <- length(inFile$datapath)
            names <- inFile$name
            
            myfiles.frame <- as.data.frame(do.call(rbind, lapply(seq(1, n, 1), function(x) readMCAData(filepath=inFile$datapath[x], filename=inFile$name[x]))))
            
            
            incProgress(1/n)
            Sys.sleep(0.1)
        })
        
        myfiles.frame$Energy <- myfiles.frame$Energy + gainshiftHold()
        
        myfiles.frame
        
        
    })
    
    
    readSPX <- reactive(label="readSPX", {
        req(input$file1)
        withProgress(message = 'Processing Data', value = 0, {
            
            inFile <- input$file1
            if (is.null(inFile)) return(NULL)
            
            n <- length(inFile$datapath)
            names <- inFile$name
            
            myfiles.frame <- as.data.frame(do.call(rbind, lapply(seq(1, n, 1), function(x) readSPXData(filepath=inFile$datapath[x], filename=inFile$name[x]))))
            
            
            incProgress(1/n)
            Sys.sleep(0.1)
        })
        
        myfiles.frame$Energy <- myfiles.frame$Energy + gainshiftHold()
        
        myfiles.frame
        
        
    })
    
    
    readPDZ <- reactive(label="readPDZ", {
        req(input$file1)
        withProgress(message = 'Processing Data', value = 0, {
            
            inFile <- input$file1
            if (is.null(inFile)) return(NULL)
            
            n <- length(inFile$datapath)
            names <- inFile$name
            
            if(input$advanced==FALSE){
                myfiles.frame <- as.data.frame(do.call(rbind, lapply(seq(1, n, 1), function(x) readPDZData(filepath=inFile$datapath[x], filename=inFile$name[x]))))
            } else if(input$advanced==TRUE){
                myfiles.frame <- as.data.frame(do.call(rbind, lapply(seq(1, n, 1), function(x) readPDZ25DataManual(filepath=inFile$datapath[x], filename=inFile$name[x], binaryshift=binaryHold()))))
                
            }
            
            
            incProgress(1/n)
            Sys.sleep(0.1)
        })
        
        myfiles.frame$Energy <- myfiles.frame$Energy + gainshiftHold()
        
        myfiles.frame
        
        
    })
    
    
    
        calListPrep <- reactive(label="calListPrep", {
            calpre <- lapply(names(calFileContents()[["calList"]]), function(x) list(importCalConditions(element=x, calList=calFileContents()[["calList"]]), calFileContents()[["calList"]][[x]][[2]]))
            names(calpre) <- names(calFileContents()[["calList"]])
            calpre
        })
        
        observeEvent(!is.null(input$file1) | !is.null(input$calfileinput), {
            if(is.null(input$calfileinput) && is.null(input$file1)){
                calMemory$Calibration <- NULL
            } else if(!is.null(input$calfileinput) && is.null(input$file1)){
                calMemory$Calibration <- calFileContents()
            } else if(!is.null(input$calfileinput) && !is.null(input$file1)){
                calMemory$Calibration$calList <- calFileContents()
            } else if(is.null(input$calfileinput) && !is.null(input$file1)){
                calMemory$Calibration <- NULL
            }
            
            if(is.null(input$calfileinput) && is.null(input$file1)){
                calMemory$Calibration <- NULL
            } else if(!is.null(input$calfileinput) && is.null(input$file1)){
                calMemory$Calibration$calList <- calListPrep()
            } else if(!is.null(input$calfileinput) && !is.null(input$file1)){
                calMemory$Calibration$calList <- NULL
            } else if(is.null(input$calfileinput) && !is.null(input$file1)){
                calMemory$Calibration <- NULL
            }
        })
        

        

      
      blankNotes <- reactive(label="blankNotes", {
          
          "Add your notes here"
          
      })
      
      calNotes <- reactive(label="calNotes", {
          
          if(!is.null(calMemory$Calibration[["Notes"]])){
              calMemory$Calibration[["Notes"]]
          } else if(is.null(calMemory$Calibration[["Notes"]])){
              blankNotes()
          }
          
      })
      
      
      
      
      notesObject <- reactive(label="notesObject", {
          
          if(is.null(input$calfileinput)){
              blankNotes()
          } else if(!is.null(input$calfileinput)){
              calNotes()
          }
          
      })
      
      output$notesui <- renderUI({
          
          
          textAreaInput('notes', "Notes", placeholder=notesObject(), width="600px", height="500px")
          
          
      })
        
        myData <- reactive(label="myData", {
            req(input$filetype)
                data <- if(input$filetype=="CSV"){
                    fullSpectra()
                } else if(input$filetype=="TXT"){
                    readTXT()
                } else if(input$filetype=="Net"){
                    netCounts()
                } else if(input$filetype=="Elio"){
                    readElio()
                }  else if(input$filetype=="MCA"){
                    readMCA()
                }  else if(input$filetype=="SPX"){
                    readSPX()
                }  else if(input$filetype=="PDZ"){
                    readPDZ()
                }

            data
        })
        
        output$spectraframestuff <- renderDataTable({
            dataHold()
        })
        
        
        
        dataHold <- reactive(label="dataHold", {
            data <- if(is.null(calMemory$Calibration$calList)){
                myData()
            } else if(!is.null(calMemory$Calibration$calList)){
                calMemory$Calibration[["Spectra"]]
            }
            
            data <- data[order(as.character(data$Spectrum)),]
            
            data$Spectrum <- gsub(".pdz", "", data$Spectrum)
            data$Spectrum <- gsub(".csv", "", data$Spectrum)
            data$Spectrum <- gsub(".CSV", "", data$Spectrum)
            data$Spectrum <- gsub(".spt", "", data$Spectrum)
            data$Spectrum <- gsub(".mca", "", data$Spectrum)
            data$Spectrum <- gsub(".spx", "", data$Spectrum)
            
            data
            
        })
        
        observeEvent(input$linecommit, {
            if(!"Spectra" %in% names(calMemory$Calibration)){
                calMemory$Calibration[["Spectra"]] <- dataHold()
            }
        })
        
        
        dataCount <- reactive(label="dataCount", {
            req(input$file1)
            inFile <- input$file1
            
            if(is.null(calMemory$Calibration)){
                length(inFile$datapath)
            }else if(!is.null(calMemory$Calibration) && is.null(input$file1)){
                length(calMemory$Calibration$Intensities)
            }else if(!is.null(calMemory$Calibration) && !is.null(input$file1)){
                length(inFile$datapath)
            }
        })
        
        
        
        
        # Return the requested dataset
        datasetInput <- reactive({
            req(input$element)
            switch(input$element,
            "H.table" = H.table,
            "He.table" = He.table,
            "Li.table" = Li.table,
            "Be.table" = Be.table,
            "B.table" = B.table,
            "C.table" = C.table,
            "N.table" = N.table,
            "O.table" = O.table,
            "F.table" = F.table,
            "Ne.table" = Ne.table,
            "Na.table" = Na.table,
            "Mg.table" = Mg.table,
            "Al.table" = Al.table,
            "Si.table" = Si.table,
            "P.table" = P.table,
            "S.table" = S.table,
            "Cl.table" = Cl.table,
            "Ar.table" = Ar.table,
            "K.table" = K.table,
            "Ca.table" = Ca.table,
            "Sc.table" = Sc.table,
            "Ti.table" = Ti.table,
            "V.table" = V.table,
            "Cr.table" = Cr.table,
            "Mn.table" = Mn.table,
            "Fe.table" = Fe.table,
            "Co.table" = Co.table,
            "Ni.table" = Ni.table,
            "Cu.table" = Cu.table,
            "Zn.table" = Zn.table,
            "Ga.table" = Ga.table,
            "Ge.table" = Ge.table,
            "As.table" = As.table,
            "Se.table" = Se.table,
            "Br.table" = Br.table,
            "Kr.table" = Kr.table,
            "Rb.table" = Rb.table,
            "Sr.table" = Sr.table,
            "Y.table" = Y.table,
            "Zr.table" = Zr.table,
            "Nb.table" = Nb.table,
            "Mo.table" = Mo.table,
            "Tc.table" = Tc.table,
            "Ru.table" = Ru.table,
            "Rh.table" = Rh.table,
            "Pd.table" = Pd.table,
            "Ag.table" = Ag.table,
            "Cd.table" = Cd.table,
            "In.table" = In.table,
            "Sn.table" = Sn.table,
            "Sb.table" = Sb.table,
            "Te.table" = Te.table,
            "I.table" = I.table,
            "Xe.table" = Xe.table,
            "Cs.table" = Cs.table,
            "Ba.table" = Ba.table,
            "La.table" = La.table,
            "Ce.table" = Ce.table,
            "Pr.table" = Pr.table,
            "Nd.table" = Nd.table,
            "Pm.table" = Pm.table,
            "Sm.table" = Sm.table,
            "Eu.table" = Eu.table,
            "Gd.table" = Gd.table,
            "Tb.table" = Tb.table,
            "Dy.table" = Dy.table,
            "Ho.table" = Ho.table,
            "Er.table" = Er.table,
            "Tm.table" = Tm.table,
            "Yb.table" = Yb.table,
            "Lu.table" = Lu.table,
            "Hf.table" = Hf.table,
            "Ta.table" = Ta.table,
            "W.table" = W.table,
            "Re.table" = Re.table,
            "Os.table" = Os.table,
            "Ir.table" = Ir.table,
            "Pt.table" = Pt.table,
            "Au.table" = Au.table,
            "Hg.table" = Hg.table,
            "Tl.table" = Tl.table,
            "Pb.table" = Pb.table,
            "Bi.table" = Bi.table,
            "Po.table" = Po.table,
            "At.table" = At.table,
            "Rn.table" = Rn.table,
            "Fr.table" = Fr.table,
            "Ra.table" = Ra.table,
            "Ac.table" = Ac.table,
            "Th.table" = Th.table,
            "Pa.table" = Pa.table,
            "U.table" = U.table)
        })
        
        observeEvent(input$actionplot, {
            
            # Expression that generates a histogram. The expression is
            # wrapped in a call to renderPlot to indicate that:
            #
            #  1) It is "reactive" and therefore should re-execute automatically
            #     when inputs change
            #  2) Its output type is a plot
            ranges <- reactiveValues(x = NULL, y = NULL)
            
            
            spectraSummary <- reactive({
                
                spectra_stats(
                    spectra.frame=dataHold(),
                    norm.type=input$normspectra,
                    norm.min=input$comptonminspectra,
                    norm.max=input$comptonmaxspectra,
                    compress="100 eV"
                    )

            })
            
            
            spectraPlotData <- reactive({
                spectra_summary_apply(spectra.frame=dataHold(), normalization=input$normspectra, min=input$comptonminspectra, max=input$comptonmaxspectra)

            })
            
            spectraWithLabels <- reactive({
                
                data <- spectraPlotData()
                
                
                id.seq <- seq(1, 2048,1)
                
                n <- length(data$Energy)
                
                element <- datasetInput()
                intensity.norm <- (element$Intensity/max(element$Intensity))*max(data$CPS)
                element$Intensity <- intensity.norm
                intensity.base <- (element$Intensity/max(element$Intensity))
                
                
                qplot(data$Energy, data$CPS, xlab = "Energy (keV)", ylab = "Counts per Second", geom="line", colour=data$Spectrum) +
                theme_light()+
                theme(legend.position="bottom") +
                geom_segment(data=element, aes(x=Line, xend=Line, y = 0, yend=Intensity), colour="grey50", linetype=2)  +
                scale_colour_discrete("Spectrum") +
                coord_cartesian(xlim = ranges$x, ylim = ranges$y)

                
            })
            
            
            spectraNoLabels <- reactive({
                
                data <- spectraPlotData()
                
                
                id.seq <- seq(1, 2048,1)
                
                n <- length(data$Energy)
                
                element <- datasetInput()
                intensity.norm <- (element$Intensity/max(element$Intensity))*max(data$CPS)
                element$Intensity <- intensity.norm
                intensity.base <- (element$Intensity/max(element$Intensity))
                
                qplot(data$Energy, data$CPS, xlab = "Energy (keV)", ylab = "Counts per Second", geom="line", colour=data$Spectrum) +
                theme_light()+
                theme(legend.position="bottom") +
                geom_segment(data=element, aes(x=Line, xend=Line, y = 0, yend=Intensity), colour="grey50", linetype=2)  +
                scale_colour_discrete("Spectrum") +
                coord_cartesian(xlim = ranges$x, ylim = ranges$y) +
                guides(colour=FALSE)
                
            })
            
            
            spectraSummaryPlot <- reactive({
                
                data <- spectraPlotData()
                
                data.summary <- spectraSummary()
                
                id.seq <- seq(1, 2048,1)
                
                n <- length(data$Energy)
                
                element <- datasetInput()
                intensity.norm <- (element$Intensity/max(element$Intensity))*max(data.summary$Mean)
                element$Intensity <- intensity.norm
                intensity.base <- (element$Intensity/max(element$Intensity))
                
                
                ggplot(data.summary) +
                geom_ribbon(aes(x=Energy, ymin=Min, ymax=Max), alpha=0.2, fill="red") +
                geom_line(aes(Energy, Mean), lty=2) +
                geom_segment(data=element, aes(x=Line, xend=Line, y = 0, yend=Intensity), colour="grey50", linetype=2)  +
                scale_x_continuous("Energy (keV)", breaks=scales::pretty_breaks()) +
                scale_y_continuous("Counts per Second") +
                coord_cartesian(xlim = ranges$x, ylim = ranges$y) +
                theme_light()

                
            })
            
            
            plotInput <- reactive({
                
                if(input$showlegend==TRUE){
                    spectraWithLabels()
                } else if(input$showlegend==FALSE && input$variancespectrum==FALSE){
                    spectraNoLabels()
                } else if(input$showlegend==FALSE && input$variancespectrum==TRUE){
                    spectraSummaryPlot()
                }

            })
            
            
            output$distPlot <- renderPlot({
                
                plotInput()
                
                
            })
            
            # When a double-click happens, check if there's a brush on the plot.
            # If so, zoom to the brush bounds; if not, reset the zoom.
            observeEvent(input$cropspectra, {
                data <- dataHold()
                brush <- input$plot1_brush
                if (!is.null(brush)) {
                    ranges$x <- c(brush$xmin, brush$xmax)
                    ranges$y <- c(brush$ymin, brush$ymax)
                    
                } else {
                    ranges$x <- NULL
                    ranges$y <- NULL
                }
                
                
                
            })
            
            output$downloadPlot <- downloadHandler(
            filename = function() { paste(input$dataset, '.png', sep='') },
            content = function(file) {
                ggsave(file,plotInput(), width=10, height=10)
            }
            )
            
            
            
            
            
            
            
            
        })
        
        
        
        
        
        
        
        standardElements <- reactive({
            req(input$filetype)
            spectra.line.table <- dataHold()
            
            
            if(is.null(calMemory$Calibration$Intensities) && input$filetype=="CSV"){
                standard
            } else if(is.null(calMemory$Calibration$Intensities) && input$filetype=="TXT"){
                standard
            } else if(is.null(calMemory$Calibration$Intensities) && input$filetype=="Elio"){
                standard
            }  else if(is.null(calMemory$Calibration$Intensities) && input$filetype=="MCA"){
                standard
            }  else if(is.null(calMemory$Calibration$Intensities) && input$filetype=="SPX"){
                standard
            }  else if(is.null(calMemory$Calibration$Intensities) && input$filetype=="PDZ"){
                standard
            } else if(is.null(calMemory$Calibration$Intensities) && input$filetype=="Net"){
                colnames(spectra.line.table[2:4])
            } else if(!is.null(calMemory$Calibration$Intensities)){
                names(calMemory$Calibration$Intensities)
            }
            
        })
        
        standardLines <- reactive({
            req(input$filetype)
            spectra.line.table <- dataHold()
            
            n <- length(names(spectra.line.table))
            
            
            choices <- if(input$filetype=="CSV"){
                spectralLines
            } else if(input$filetype=="TXT"){
                spectralLines
            } else if(input$filetype=="Elio"){
                spectralLines
            }  else if(input$filetype=="MCA"){
                spectralLines
            }  else if(input$filetype=="SPX"){
                spectralLines
            }  else if(input$filetype=="PDZ"){
                spectralLines
            } else if(input$filetype=="Net"){
                colnames(spectra.line.table[2:n])
            }
            
            choices
            
            
        })
        
        
        
        
        
        selectedKalpha <- reactive({
            if(is.null(calMemory$Calibration$Intensities)){
                c("Ca.K.alpha", "Ti.K.alpha", "Fe.K.alpha")
            } else if(!is.null(calMemory$Calibration$Intensities)){
                as.vector(subset(selectedElementsCalpre(), Orbital=="K" & Line=="alpha")$ElementLine)
            }
            
        })
        
        
        selectedKbeta <- reactive({
            if(is.null(calMemory$Calibration$Intensities)){
                NULL
            } else if(!is.null(calMemory$Calibration$Intensities)){
                as.vector(subset(selectedElementsCalpre(), Orbital=="K" & Line=="beta")$ElementLine)
            }
            
        })
        
        selectedLalpha <- reactive({
            if(is.null(calMemory$Calibration$Intensities)){
                "Rh.L.alpha"
            } else if(!is.null(calMemory$Calibration$Intensities)){
                as.vector(subset(selectedElementsCalpre(), Orbital=="L" & Line=="alpha")$ElementLine)
            }
            
        })
        
        selectedLbeta <- reactive({
            if(is.null(calMemory$Calibration$Intensities)){
                "Pb.L.beta"
            } else if(!is.null(calMemory$Calibration$Intensities)){
                as.vector(subset(selectedElementsCalpre(), Orbital=="L" & Line=="beta")$ElementLine)
            }
            
        })
        
        selectedMLine <- reactive({
            if(is.null(calMemory$Calibration$Intensities)){
                NULL
            } else if(!is.null(calMemory$Calibration$Intensities)){
                as.vector(subset(selectedElementsCalpre(), Orbital=="M" & Line=="line")$Element)
            }
            
        })
        
        output$checkboxElements <-  renderUI({
            
            checkboxGroupInput("show_vars", label="Elemental lines to show:",
            choices = standardLines(), selected = selectedElements())
            
        })
        
        output$checkboxElementsKalpha <-  renderUI({
            selectInput("show_vars_k_alpha", label="K-alpha",
            choices = kalphaLines, selected = selectedKalpha() , multiple=TRUE)
            
        })
        
        output$checkboxElementsKbeta <-  renderUI({
            selectInput("show_vars_k_beta", label="K-beta",
            choices = kbetaLines, selected = selectedKbeta(), multiple=TRUE)
            
        })
        
        output$checkboxElementsLalpha <-  renderUI({
            selectInput("show_vars_l_alpha", label="L-alpha",
            choices = lalphaLines, selected = selectedLalpha(), multiple=TRUE)
            
        })
        
        output$checkboxElementsLbeta <-  renderUI({
            selectInput("show_vars_l_beta", label="L-beta",
            choices = lbetaLines, selected = selectedLbeta(), multiple=TRUE)
            
        })
        
        output$checkboxElementsM <-  renderUI({
            selectInput("show_vars_m", label="M",
            choices = mLines, selected = selectedMLine(), multiple=TRUE)
            
        })
        
        
        
        
        linevalues <- reactiveValues()
        
        
        lineInput <- reactive({
            
            blank.frame <- data.frame(
            Name=as.vector(as.character(rep("", 25))),
            EnergyMin=as.numeric(rep("", 25)),
            EnergyMax=as.numeric(rep("", 25)),
            stringsAsFactors = FALSE
            )
            
            blank.frame
            
        })
        
        
        lineInputCal <- reactive({
            
            
            calMemory$Calibration[["Definitions"]]
            
            
        })
        
        lineTableInput <- reactive({
            

            if(is.null(calMemory$Calibration)){
                lineInput()
            } else if(!is.null(calMemory$Calibration) && "Definitions" %in% names(calMemory$Calibration)){
                lineInputCal()
            } else if(!is.null(calMemory$Calibration) && !"Definitions" %in% ls(calMemory$Calibration)){
               lineInput()
            }
            
        })
        
        observe({
            if (!is.null(input$hotline)) {
                DF <- hot_to_r(input$hotline)
            } else {
                DF <- lineTableInput()
            }
            linevalues[["DF"]] <- DF
        })
        
        eventReactive(input$linecommit,{
            
            linevalues[["DF"]] <- lineTableInput()
            
        })
        
        
        ## Handsontable
        
        output$hotline <- renderRHandsontable({
            
            DF <- linevalues[["DF"]]
            
            
            
            
            rhandsontable(DF) %>% hot_col(1:length(DF), renderer=htmlwidgets::JS("safeHtmlRenderer"))
            
            
        })
        
        
        observeEvent(input$resethotableline, {
            
            linevalues[["DF"]] <- NULL
            
            linevalues[["DF"]] <- lineTableInput()
            
            
        })
        
        
        
        # randomInterList <- reactive({
        #   if (is.null(input$intercept_vars))
        #   paste(,2)
        #   else
        #   input$intercept_vars
        #})
        
        
        #randomSlopeList <- reactive({
        #   if (is.null(input$intercept_vars))
        #   paste(,2)
        #   else
        #   input$slope_vars
        #})
        
        #output$nullintercept <- randomInterList()
        
        #output$nullslope <- randomSlopeList()
        
        lineSubset <- reactive({
            req(linevalues[["DF"]], dataHold())
            xrf_parse(range.table = linevalues[["DF"]], data=dataHold())
            
        })
        
        
        output$LineValues <- renderDataTable({
            lineSubset()
        })
        
        
        
        
        
        testfFrame <- reactive({
            
            k.alpha.lines <- kalphaLines[input$show_vars_k_alpha]
            k.beta.lines <- kbetaLines[input$show_vars_k_beta]
            l.alpha.lines <- lalphaLines[input$show_vars_l_alpha]
            l.beta.lines <- lbetaLines[input$show_vars_l_beta]
            m.lines <- mLines[input$show_vars_m]
            
            k.alpha.lines <- k.alpha.lines[!is.na(k.alpha.lines)]
            k.beta.lines <- k.beta.lines[!is.na(k.beta.lines)]
            l.alpha.lines <- l.alpha.lines[!is.na(l.alpha.lines)]
            l.beta.lines <- l.beta.lines[!is.na(l.beta.lines)]
            m.lines <- m.lines[!is.na(m.lines)]
            
            choosen.elements <- c(input$show_vars_k_alpha, input$show_vars_k_beta, input$show_vars_l_alpha, input$show_vars_l_beta, input$show_vars_m)
            
            
            element.frame <- data.frame(elements=choosen.elements, order=atomic_order_vector(choosen.elements))
            
            element.frame
        })
        
        selectedElements <- reactive({
            
            req(input$show_vars_k_alpha)
            
            k.alpha.lines <- kalphaLines[input$show_vars_k_alpha]
            k.beta.lines <- kbetaLines[input$show_vars_k_beta]
            l.alpha.lines <- lalphaLines[input$show_vars_l_alpha]
            l.beta.lines <- lbetaLines[input$show_vars_l_beta]
            m.lines <- mLines[input$show_vars_m]
            
            k.alpha.lines <- k.alpha.lines[!is.na(k.alpha.lines)]
            k.beta.lines <- k.beta.lines[!is.na(k.beta.lines)]
            l.alpha.lines <- l.alpha.lines[!is.na(l.alpha.lines)]
            l.beta.lines <- l.beta.lines[!is.na(l.beta.lines)]
            m.lines <- m.lines[!is.na(m.lines)]
            
            choosen.elements <- c(input$show_vars_k_alpha, input$show_vars_k_beta, input$show_vars_l_alpha, input$show_vars_l_beta, input$show_vars_m)
            
            element.frame <- data.frame(elements=choosen.elements, order=atomic_order_vector(choosen.elements))
            as.vector(element.frame[order(element.frame$order),]$elements)
            
            
        })
        
        selectedElementsCalpre <- reactive({
            
            element.lines <- names(calMemory$Calibration$Intensities)
            
            
            rbindlist(lapply(element.lines, element_line_pull))
            
        })
        
        selectedElementsCalpost <- reactive({
            table <- linevalues[["DF"]]
            table <- table[complete.cases(table),]
            
            element.lines <- ls(calMemory$Calibration$Intensities)
            
            
            c(do.call("rbind", lapply(element.lines, element_line_pull)),  as.vector(table$Name))
            
        })
        
        
        selectedElementsCal <- reactive({
            
            table <- linevalues[["DF"]]
            table <- table[complete.cases(table),]
            
            if(length(table[,1])==0){
                selectedElementsCalpre()
            } else if(length(table[,1])!=0){
                selectedElementsCalpost()
            }
            
        })
        
        elementallinestousepre <- reactive({
            req(linevalues[["DF"]])
            table <- linevalues[["DF"]]
            table <- table[complete.cases(table),]
            
            #c(selectedElements()_k_alpha, selectedElements()_k_beta, selectedElements()_l_alpha, selectedElements()_l_beta, selectedElements()_m)
            
            selectedElements()
            
            
        })
        
        elementallinestousepost <- reactive({
            table <- linevalues[["DF"]]
            table <- table[complete.cases(table),]
            
            
            
            c(selectedElements(),  as.vector(table$Name))
            
        })
        
        
        elementallinestouse <- reactive({
            table <- linevalues[["DF"]]
            table <- table[complete.cases(table),]
            
            if(length(table[,1])==0){
                elementallinestousepre()
            } else if(length(table[,1])!=0){
                elementallinestousepost()
            }
            
            
        })
        
        
        
        spectraData <- reactive({
            req(dataHold(), elementallinestousepre(), linevalues[["DF"]])
            line.data <- elementFrame(data=dataHold(), elements=elementallinestousepre())
            
            table <- linevalues[["DF"]]
            table <- table[complete.cases(table),]
            
            if(length(table[,1])==0){
                line.data
            } else if(length(table[,1])!=0){
                merge(line.data, lineSubset(), by="Spectrum")
            }
            
        })
        
        netData <- reactive({
            
            net.data <- dataHold()
            
            elements <- elementallinestouse()
            
            
            net.data.partial <- net.data[,elements]
            net.data <- data.frame(net.data$Spectrum ,net.data.partial)
            colnames(net.data) <- c("Spectrum", elements)
            net.data <- net.data[order(as.character(net.data$Spectrum)),]
            
            net.data$Spectrum <- gsub(".csv", "", net.data$Spectrum)
            net.data$Spectrum <- gsub(".CSV", "", net.data$Spectrum)
            
            net.data
            
        })
        
        
        
        
        calDataType <- reactive({
            
            select.line.table <- if(input$filetype=="CSV"){
                spectraData()
            } else if(input$filetype=="TXT"){
                spectraData()
            } else if(input$filetype=="Elio"){
                spectraData()
            }  else if(input$filetype=="MCA"){
                spectraData()
            }  else if(input$filetype=="SPX"){
                spectraData()
            }  else if(input$filetype=="PDZ"){
                spectraData()
            } else if(input$filetype=="Net"){
                netData()
            }
            
        })
        
        
        

        
        observeEvent(input$linecommit, priority = 2, {
            calMemory$Calibration$Intensities <- if(input$filetype=="CSV"){
                spectraData()
            } else if(input$filetype=="TXT"){
                spectraData()
            } else if(input$filetype=="Elio"){
                spectraData()
            }  else if(input$filetype=="MCA"){
                spectraData()
            }  else if(input$filetype=="SPX"){
                spectraData()
            }  else if(input$filetype=="PDZ"){
                spectraData()
            } else if(input$filetype=="Net"){
                netData()
            }
        })
        
        tableInput <- reactive({
            
            elements <- elementallinestouse()
            
            
            select.line.table <- calMemory$Calibration$Intensities
            
            rounded <- round(select.line.table[,elements], digits=0)
            full <- data.frame(select.line.table$Spectrum, rounded)
            colnames(full) <- c("Spectrum", elements)
            
            full
        })
        
        
        output$mytable1 <- renderDataTable({
            
            base.table <- tableInput()[,-1]
            rownames(base.table) <- tableInput()$Spectrum
            base.table
            
        })

        
        covarPlotLine <- reactive({
            data.table <- calMemory$Calibration$Intensities
            correlations <- cor(data.table[,-1])
            if(input$linecovarnumber==FALSE){
                corrplot::corrplot(correlations, method="circle")
            } else if(input$linecovarnumber==TRUE){
                corrplot::corrplot(correlations, method="number", number.digits=1)
            }
        })
        
        output$covarianceplot <- renderPlot({
            
            covarPlotLine()
            
        })
        
        output$download_covarlines <- downloadHandler(
        filename = function() { paste(paste(c(input$calname, "Line_Correlations"), collapse=''), '.tiff',  sep='') },
        content = function(file) {
            ggsave(file,covarPlotLine(), device="tiff", compression="lzw",  dpi=300, width=18, height=7)
        }
        )
        
        
        output$downloadData <- downloadHandler(
        filename = function() { paste(input$dataset, '.csv', sep=',') },
        content = function(file
        ) {
            write.csv(spectraData(), file)
        }
        )
        
        
        outVar <- reactive({
            input$linecommit
            
            myelements <- elementallinestouse()
            
            result <- if(is.null(myelements)){
                "Ca.K.alpha"
            }else{
                myelements
            }
            
            result
            
            
        })
        
        outVaralt <- reactive({
            input$linecommit
            
            
            myelements <- c(elementallinestouse())
            
            
            if(is.null(myelements)){
                paste("Ca.K.alpha")
            }else{
                myelements
            }
            
        })
        
        outVaralt2 <- reactive({
            input$linecommit
            
            
            myelements <- c(elementallinestouse())
            
            
            if(is.null(myelements)){
                paste("Ca.K.alpha")
            }else{
                myelements[! myelements %in% c(input$calcurveelement)]
            }
            
        })
        
        output$inVar2 <- renderUI({
            selectInput(inputId = "calcurveelement", label = h4("Element"), choices =  outVar())
        })
        
        
        hotableInputBlank <- reactive({
            
            elements <- elementallinestouse()
            
            
            
            
            spectra.line.table <- calMemory$Calibration$Intensities
            
            empty.line.table <- spectra.line.table[,elements] * 0.0000
            
            #empty.line.table$Spectrum <- spectra.line.table$Spectrum
            
            hold.frame <- data.frame(spectra.line.table$Spectrum, empty.line.table)
            colnames(hold.frame) <- c("Spectrum", elements)
            
            hold.frame <- as.data.frame(hold.frame)
            
            
            
            hold.frame
            
            
        })
        
        hotableInputCal <- reactive({
            
            elements <- elementallinestouse()
            
            
            
            
            spectra.line.table <- calMemory$Calibration$Intensities
            
            empty.line.table <- spectra.line.table[,elements] * 0.0000
            
            #empty.line.table$Spectrum <- spectra.line.table$Spectrum
            
            hold.frame <- data.frame(spectra.line.table$Spectrum, empty.line.table)
            colnames(hold.frame) <- c("Spectrum", elements)
            
            hold.frame <- as.data.frame(hold.frame)
            
            
            value.frame <- calMemory$Calibration$Values
            
            #anna <- rbind(hold.frame, value.frame)
            
            #temp.table <- data.table(anna)[,list(result = sum(result)), elements]
            
            #as.data.frame(temp.table)
            
            #element.matches <- elements[elements %in% ls(value.frame)]
            
            #merge_Sum(.df1=hold.frame, .df2=value.frame, .id_Columns="Spectrum",  .match_Columns=element.matches)
            
            
            #data.frame(calMemory$Calibration$Values, hold.frame[,! names(hold.frame) %in% names(calMemory$Calibration$Values)])
            
            hold.frame.reduced <- hold.frame[2:length(hold.frame)]
            value.frame.reduced <- if(colnames(calMemory$Calibration$Values)[1]=="Spectrum"){
                value.frame[2:length(value.frame)]
            }else if(colnames(calMemory$Calibration$Values)[1]=="Include"){
                value.frame[3:length(value.frame)]
            }
            
            rownames(hold.frame.reduced) <- hold.frame$Spectrum
            rownames(value.frame.reduced) <- value.frame$Spectrum
            
            
            hotable.new = hold.frame.reduced %>% add_rownames %>%
            full_join(value.frame.reduced %>% add_rownames) %>%
            group_by(rowname) %>%
            summarise_all(funs(sum(., na.rm = FALSE)))
            
            colnames(hotable.new)[1] <- "Spectrum"
            
            hotable.new$Spectrum <- gsub(".pdz", "", hotable.new$Spectrum)
            hotable.new$Spectrum <- gsub(".csv", "", hotable.new$Spectrum)
            hotable.new$Spectrum <- gsub(".CSV", "", hotable.new$Spectrum)
            hotable.new$Spectrum <- gsub(".spt", "", hotable.new$Spectrum)
            hotable.new$Spectrum <- gsub(".mca", "", hotable.new$Spectrum)
            hotable.new$Spectrum <- gsub(".spx", "", hotable.new$Spectrum)
            
            hotable.new
            
            
        })
        
        hotableInput <- reactive({
            
            
            hotable.data <- if(is.null(calMemory$Calibration$Values)){
                hotableInputBlank()
            }else if(!is.null(calMemory$Calibration$Values)){
                hotableInputCal()
            }
            
            
            
            hotable.new <- if(is.null(calMemory$Calibration$Values)){
                data.frame(Include=rep(TRUE, length(hotable.data$Spectrum)), hotable.data)
            } else if(!is.null(calMemory$Calibration$Values) && colnames(calMemory$Calibration$Values)[1]=="Spectrum"){
                data.frame(Include=rep(TRUE, length(hotable.data$Spectrum)), hotable.data)
            } else if(!is.null(calMemory$Calibration$Values) && colnames(calMemory$Calibration$Values)[1]=="Include"){
                data.frame(calMemory$Calibration$Values)
            }
            
            
            
        })
        
        values <- reactiveValues()
        
        observe({
            if (!is.null(input$hot)) {
                DF <- hot_to_r(input$hot)
            } else {
                if (input$linecommit)
                DF <- hotableInput()
                else
                DF <- values[["DF"]]
            }
            values[["DF"]] <- DF
        })
        
        eventReactive(input$linecommit,  {
            
            values[["DF"]] <- hotableInput()
            
        })
        
        
        ## Handsontable
        
        output$hot <- renderRHandsontable({
            
            DF <- values[["DF"]]
            
            DF <- DF[order(as.character(DF$Spectrum)),]
            
            
            
            if(!is.null(DF))
            rhandsontable(DF, digits=9) %>% hot_col(2:length(DF), renderer=htmlwidgets::JS("safeHtmlRenderer"))
            
            
        })
        
        
        observeEvent(input$resethotable, {
            
            values[["DF"]] <- NULL
            
            values[["DF"]] <- hotableInput()
            
        })
        
        #observeEvent(!is.null(values[["DF"]]), {
        #        calMemory$Calibration[["Spectra"]] <- dataHold()
        #})
        
        covarPlotValues <- reactive({
            data.table <- values[["DF"]]
            correlations <- cor(data.table[,3:length(data.table)], use="pairwise.complete.obs")
            if(input$conccovarnumber==FALSE){
                corrplot::corrplot(correlations, method="circle")
            } else if(input$conccovarnumber==TRUE){
                corrplot::corrplot(correlations, method="number", number.digits=1)
            }
        })
        
        output$covarianceplotvalues <- renderPlot({
            covarPlotValues()
        })
        
        
        output$download_covarvalues <- downloadHandler(
        filename = function() { paste(paste(c(input$calname, "_Value_Correlations"), collapse=''), '.tiff',  sep='') },
        content = function(file) {
            tiff(file, compression="lzw",  width=18, height=18)
            covarPlotValues()
            dev.off()
        }
        )
        
        
        
        
        keepRowsFrame <- reactive({
            
            spectra.stuff <- values[["DF"]]
            rows <- vals$keeprows
            
            the.frame <- data.frame(Spectrum=spectra.stuff$Spectrum, Standards=rows)
            the.frame
            
        })
        
        
        output$whichrowstokeep <- renderRHandsontable({
            
            DF <- keepRowsFrame()
            
            DF <- DF[order(as.character(DF$Spectrum)),]
            
            
            
            if (!is.null(DF))
            rhandsontable(DF) %>% hot_col(2:length(DF), renderer=htmlwidgets::JS("safeHtmlRenderer"))
            
            
        })
        

        
        
        #if(input$hotableprocess2){vals$keeprows <- vals$keeprows[dropStandard()]}
        
        
        output$temp <- renderTable({
            
            as.data.frame(vals$keeprows)
            
        })
        
        
        dataType <- reactive({
            req(input$filetype)
            if(input$filetype=="CSV"){
                "Spectra"
            } else if(input$filetype=="TXT"){
                "Spectra"
            } else if(input$filetype=="Elio"){
                "Spectra"
            }  else if(input$filetype=="MCA"){
                "Spectra"
            }  else if(input$filetype=="SPX"){
                "Spectra"
            }  else if(input$filetype=="PDZ"){
                "Spectra"
            } else if (input$filetype=="Net"){
                "Net"
            }
            
        })
        
        
        
        
        concentrationTable <- reactive({
            
            concentration.table <- as.data.frame(values[["DF"]], stringsAsFactors=FALSE)
            concentration.table[concentration.table==""] <- NA
            concentration.table[values[["DF"]]$Include,]
            
        })
        
        spectraLineTable <- reactive({
            
            spectra.line.table <- if(dataType()=="Spectra"){
                calMemory$Calibration$Intensities[values[["DF"]]$Include,]
            }else if(dataType()=="Net"){
                calMemory$Calibration$Intensities[values[["DF"]]$Include,]
            }
            
            
            spectra.line.table <- spectra.line.table[order(as.character(spectra.line.table$Spectrum)),]
            spectra.line.table <- spectra.line.table[complete.cases(spectra.line.table),]
            spectra.line.table[ rowSums(spectra.line.table[,-1])!=0, ]
            
            
        })
        
        
        holdFrame <- reactive({
            req(input$radiocal, input$calcurveelement)
            req(concentrationTable(), spectraLineTable())
            spectra.line.table <- spectraLineTable()
            concentration.table <- concentrationTable()
            
            concentration.table <- concentration.table[concentration.table$Spectrum %in% spectra.line.table$Spectrum,]
            spectra.line.table <- spectra.line.table[spectra.line.table$Spectrum %in% concentration.table$Spectrum,]

            
            concentration <- as.vector(as.numeric(unlist(concentration.table[,input$calcurveelement])))
            
            hold.frame <- data.frame(spectra.line.table, Concentration=concentration)
            
            hold.frame[complete.cases(hold.frame),]
        })
        
        dataNorm <- reactive({
            req(input$radiocal, input$calcurveelement)
            data <- dataHold()
            hold.frame <- holdFrame()
            
            data.spec <- data$Spectrum
            hold.spec <- hold.frame$Spectrum
            result <- data[data.spec %in% hold.spec, ]
            result
        })
        
        # randomInterList <- reactive({
        #   if (is.null(input$intercept_vars))
        #   paste(,2)
        #   else
        #   input$intercept_vars
        #})
        
        
        #randomSlopeList <- reactive({
        #   if (is.null(input$intercept_vars))
        #   paste(,2)
        #   else
        #   input$slope_vars
        #})
        
        #output$nullintercept <- randomInterList()
        
        #output$nullslope <- randomSlopeList()
        
        
        
        
        
        #####Set Defaults
        
        
        
        ####Set up Standards
        elementHold <- reactive({
            
            if(is.null(input$calcurveelement)==TRUE){
                ls(dataHold())[1]
            } else if(!is.null(input$calcurveelement)==TRUE){
                input$calcurveelement
            }
            
        })
        



        calListSettingsLoad <- reactive({
            if(is.null(calMemory$Calibration)){
                NULL
            } else if(!is.null(calMemory$Calibration) && is.null(input$file1)){
                calMemory$Calibration$calList
            } else if(!is.null(calMemory$Calibration) && !is.null(input$file1)){
                calMemory$Calibration$calList
            }
        })
        
        calSettings <- reactiveValues()
        
        observeEvent(input$linecommit, {
            calSettings$calList <- if(is.null(calMemory$Calibration)){
                NULL
            } else if(!is.null(calMemory$Calibration) && is.null(input$file1)){
                calMemory$Calibration$calList
            } else if(!is.null(calMemory$Calibration) && !is.null(input$file1)){
                calMemory$Calibration$calList
            }
        })
        

        

        calConditions <- reactiveValues()
        observeEvent(input$calcurveelement, {
            req(input$calcurveelement)
            
            calConditions$hold <- if(input$calcurveelement %in% names(calSettings$calList)){
                calSettings$calList[[input$calcurveelement]][[1]]
            } else if(!input$calcurveelement %in% names(calSettings$calList)){
                defaultCalConditions(element=input$calcurveelement, number.of.standards=dataCount())
            }
            
            #if(!is.null(input$calcurveelement) && is.null(calSettings[[input$calcurveelemet]])){
            #   calSettings[[input$calcurveelemet]][[1]] <<- calConditions$hold
            #} else if(!is.null(input$calcurveelement) && !is.null(calSettings[[input$calcurveelemet]])){
            #   calSettings[[input$calcurveelemet]][[1]] <<- calMemory$Calibration$calList[[input$calcurveelement]][[1]]
            #}
            
        })
        
        observeEvent(input$radiocal, {
            calConditions$hold[["CalTable"]]$CalType <<- as.numeric(input$radiocal)
        })
        
        observeEvent(input$compress, {
            calConditions$hold[["CalTable"]]$Compress <<- input$compress
        })
        
        observeEvent(input$transformation, {
            calConditions$hold[["CalTable"]]$Transformation <<- as.character(input$transformation)
        })
        
        observeEvent(input$energyrange, {
            calConditions$hold[["CalTable"]]$EnergyRange <<- paste0(input$energyrange[1], "-", input$energyrange[2])
        })
        
        observeEvent(input$normtype, {
            calConditions$hold[["CalTable"]]$NormType <<- as.numeric(input$normcal)
        })
        
        observeEvent(input$comptonmin, {
            calConditions$hold[["CalTable"]]$Min <<- as.numeric(input$comptonmin)
        })
        
        observeEvent(input$comptonmax, {
            calConditions$hold[["CalTable"]]$Max <<- as.numeric(input$comptonmax)
        })
        
        observeEvent(input$foresttry, {
            calConditions$hold[["CalTable"]]$ForestTry <<- as.numeric(input$foresttry)
        })
        
        observeEvent(input$forestmetric, {
            calConditions$hold[["CalTable"]]$ForestMetric <<- as.character(input$forestmetric)
        })
        
        observeEvent(input$foresttrain, {
            calConditions$hold[["CalTable"]]$ForestTC <<- as.character(input$foresttrain)
        })
        
        observeEvent(input$forestnumber, {
            calConditions$hold[["CalTable"]]$ForestNumber <<- as.numeric(input$forestnumber)
        })
        
        observeEvent(input$foresttrees, {
            calConditions$hold[["CalTable"]]$ForestTrees <<- as.numeric(input$foresttrees)
        })
        
        observeEvent(input$neuralhiddenlayers, {
            calConditions$hold[["CalTable"]]$NeuralHL <<- as.numeric(input$neuralhiddenlayers)
        })
        
        observeEvent(input$neuralhiddenunits, {
            calConditions$hold[["CalTable"]]$NeuralHU <<- paste0(input$neuralhiddenunits[1], "-", input$neuralhiddenunits[2])
        })
        
        observeEvent(input$neuralweightdecay, {
            calConditions$hold[["CalTable"]]$NeuralWD <<- paste0(input$neuralweightdecay[1], "-", input$neuralweightdecay[2])
        })
        
        observeEvent(input$neuralmaxiterations, {
            calConditions$hold[["CalTable"]]$NeuralMI <<- as.numeric(input$neuralmaxiterations)
        })
        
        observeEvent(input$treedepth, {
            calConditions$hold[["CalTable"]]$TreeDepth <<- paste0(input$treedepth[1], "-", input$treedepth[2])
        })
        
        observeEvent(input$xgbeta, {
            calConditions$hold[["CalTable"]]$xgbEta <<- paste0(input$xgbeta[1], "-", input$xgbeta[2])
        })
        
        observeEvent(input$xgbgamma, {
            calConditions$hold[["CalTable"]]$xgbGamma <<- paste0(input$xgbgamma[1], "-", input$xgbgamma[2])
        })
        
        observeEvent(input$xgbsubsample, {
            calConditions$hold[["CalTable"]]$xgbSubSample <<- paste0(input$xgbsubsample[1], "-", input$xgbsubsample[2])
        })
        
        observeEvent(input$xgbcolsample, {
            calConditions$hold[["CalTable"]]$xgbColSample <<- paste0(input$xgbcolsample[1], "-", input$xgbcolsample[2])
        })
        
        observeEvent(input$xgbminchild, {
            calConditions$hold[["CalTable"]]$xgbMinChild <<- as.numeric(input$xgbminchild)
        })
        
        
        calFileStandards <- reactive({
            if(is.null((input$calcurveelement))){
                NULL
            } else if(!is.null((input$calcurveelement))){
                if(!is.null(calSettings$calList) && !"StandardsUsed" %in% names(calSettings$calList[[input$calcurveelement]][[1]])){
                    rep(TRUE, dataCount())
                } else if(!is.null(calSettings$calList) && "StandardsUsed" %in% names(calSettings$calList[[input$calcurveelement]][[1]])){
                    calSettings$calList[[input$calcurveelement]][[1]]$StandardsUsed
                } else if(is.null(calSettings$calList)){
                    rep(TRUE, dataCount())
                }
            }
 
        })
        
        
        
        
        vals <- reactiveValues()
        
        observeEvent(input$calcurveelement, {
        vals$keeprows <- if(!is.null(calMemory$Calibration)){
            calFileStandards()
        }else{
            rep(TRUE, dataCount())
        }
        })

        
        
        inVar3Selectedpre <- reactive({
            if(!"Intercept" %in% names(calSettings$calList[[input$calcurveelement]][[1]])){
                calConditions$hold$Intercept
            } else if("Intercept" %in% names(calSettings$calList[[input$calcurveelement]][[1]])){
                calSettings$calList[[input$calcurveelement]][[1]]$Intercept
            }
        })
        
        
        
        
        inVar4Selectedpre <- reactive({
            if(!"Slope" %in% names(calSettings$calList[[input$calcurveelement]][[1]])){
                calConditions$hold$Slope
            } else if("Slope" %in% names(calSettings$calList[[input$calcurveelement]][[1]])){
                calSettings$calList[[input$calcurveelement]][[1]]$Slope
            }
        })
        
        
        
        
        ########Machine Learning: Normalization
        
        normMinPre <- reactive({
            if(!"Min" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calConditions$hold[["CalTable"]][["Min"]]
            } else if("Min" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calSettings$calList[[input$calcurveelement]][[1]]$CalTable$Min[1]
            }
        })
        
        normMaxPre <- reactive({
            if(!"Max" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calConditions$hold[["CalTable"]][["Max"]]
            } else if("Max" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calSettings$calList[[input$calcurveelement]][[1]]$CalTable$Max[1]
            }
        })
        
        
        
        
        planktonVector <- reactive({
            
            #0.7, 0.9
            #2.5, 2.8
            #11.0, 11.2
            #18.4, 19.4
            #19.5, 22
            #21, 22
            #30, 35
            #35, 40
            
            mins <- c(0.7, 2.5, 11.0, 18.4, 19.5, 21, 30, 35)
            maxs <- c(0.9, 2.8, 11.2, 19.4, 22, 22, 35, 40)
            
            norm.list <- list(mins, maxs)
            names(norm.list) <- c("Min", "Max")
            norm.list
            
            
        })
        
        bestNormVars <- reactive({
            
            norm.list <- planktonVector()
            
            element <- input$calcurveelement
            
            choices <- elementallinestouse()
            spectra.line.table <- spectraLineTable()
            data <- dataNorm()
            concentration.table <- concentrationTable()
            
            index <- seq(1, length(norm.list[[1]]), 1)
            
            
            concentration.table <- concentration.table[complete.cases(concentration.table[,input$calcurveelement]),]
            
            
            
            spectra.line.table <- spectraLineTable()[spectraLineTable()$Spectrum %in% holdFrame()$Spectrum, ]
            
            #spectra.line.table <- spectra.line.table[spectra.line.table$Spectrum %in% concentration.table$Spectrum, ]
            
            spectra.line.table <- spectra.line.table[complete.cases(concentration.table[, element]),]
            
            data <- data[data$Spectrum %in% concentration.table$Spectrum, ]
            
            
            time.bic <- if(dataType()=="Spectra"){
                extractAIC(lm(concentration.table[, input$calcurveelement]~general_prep_xrf(spectra.line.table, input$calcurveelement)$Intensity, na.action=na.exclude), k=log(length(1)))[2]
            } else if(dataType()=="Net"){
                extractAIC(lm(concentration.table[, input$calcurveelement]~general_prep_xrf_net(spectra.line.table, input$calcurveelement)$Intensity, na.action=na.exclude), k=log(length(1)))[2]
            }
            
            tc.bic <- if(dataType()=="Spectra"){
                extractAIC(lm(concentration.table[, input$calcurveelement]~simple_tc_prep_xrf(data, spectra.line.table, input$calcurveelement)$Intensity, na.action=na.exclude), k=log(length(1)))[2]
            } else if(dataType()=="Net"){
                extractAIC(lm(concentration.table[, input$calcurveelement]~simple_tc_prep_xrf_net(data, spectra.line.table, input$calcurveelement)$Intensity, na.action=na.exclude), k=log(length(1)))[2]
            }
            
            comp.bic <- if(dataType()=="Spectra"){
                optimal_norm_chain_xrf(data=data, element=element, spectra.line.table=spectra.line.table, values=concentration.table, possible.mins=norm.list[["Min"]], possible.maxs=norm.list[["Max"]])
            } else if(dataType()=="Net"){
                time.bic
            }
            
            norm.chain <- c(time.bic, tc.bic, comp.bic)
            type.chain <- c(1, 2, 3)
            
            best <- index[[which.min(unlist(norm.chain))]]
            best.comp <- c(planktonVector()[["Min"]][best], planktonVector()[["Max"]][best])
            best.type <- type.chain[which.min(unlist(norm.chain))]
            result.list <- list(best.type, best.comp)
            names(result.list) <- c("Type", "Compton")
            result.list
        })
        
        
        normhold <- reactiveValues()
        
        observeEvent(input$calcurveelement, {
            normhold$norms <- c(normMinPre(), normMaxPre())
            normhold$normtype <- calNormSelectionpre()
        })
        
        
        observeEvent(input$trainSlope, {
            
            isolate(normhold$norms[1] <- bestNormVars()[["Compton"]][1])
            isolate(normhold$norms[2] <- bestNormVars()[["Compton"]][2])
            isolate(normhold$normtype <- bestNormVars()[["Type"]])
            
        })
        
        calNormSelection <- reactive({
            normhold$normtype
        })
        
        normMinSelection <- reactive({
            normhold$norms[1]
        })
        
        normMaxSelection <- reactive({
            normhold$norms[2]
        })
        
        
        
        
        calNormSelectionpre <- reactive({
            if(!"NormType" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calConditions$hold[["CalTable"]][["NormType"]]
            } else if("NormType" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calSettings$calList[[input$calcurveelement]][[1]]$CalTable$NormType[1]
            }
        })
        
        
        output$normTypeInput <- renderUI({
            req(input$radiocal)
            selectInput("normcal", label = "Normalization",
            choices = list("Time" = 1, "Total Counts" = 2, "Compton" = 3),
            selected = calNormSelection())
            
            
        })
        
        
        output$comptonMinInput <- renderUI({
            req(input$radiocal)
            numericInput('comptonmin', label=h6("Min"), step=0.001, value=normMinSelection(), min=0, max=50, width='30%')
            
        })
        
        output$comptonMaxInput <- renderUI({
            req(input$radiocal)
            numericInput('comptonmax', label=h6("Max"), step=0.001, value=normMaxSelection(), min=0, max=50, width='30%')
            
        })
        
        
        #####Machine Learning: Intercepts
        
        
        
        cephlopodVector <- reactive({
            
            combos_mod.xrf <- function(a.vector){
                
                so <- seq(from=1, to=length(a.vector), by=1)
                
                #long <- pblapply(so, function(x) gRbase::combnPrim(x=a.vector, m=x), cl=6L)
                and <- pblapply(long, function(x) plyr::alply(x, 2), cl=6L)
                thanks.for.all.the.fish <- do.call(list, unlist(and, recursive=FALSE))
                
                thanks.for.all.the.fish
                
            }
            combos_mod.xrf <- cmpfun(combos_mod.xrf)

            
            if(!is.null(likely_intercepts_xrf(input$calcurveelement))){
                combos_mod.xrf(likely_intercepts_xrf(input$calcurveelement))
            } else if(is.null(likely_intercepts_xrf(input$calcurveelement))){
                c("Rh.K.alpha", "Rh.L.alpha")
            }
            
        })
        
        
        bestInterceptVars <- reactive({
            
            element <- input$calcurveelement
            
            choices <- elementallinestouse()
            
            spectra.line.table <- if(all(cephlopodVector() %in% colnames(spectraLineTable()))==TRUE){
                spectraLineTable()
            } else if(all(cephlopodVector() %in% colnames(spectraLineTable()))==FALSE){
                merge(spectraLineTable(), elementFrame(data=dataHold(), elements=cephlopodVector()[cephlopodVector() %in% colnames(spectraLineTable())]))
            }
            
            data <- dataNorm()
            concentration.table <- concentrationTable()
            
            
            spectra.line.table <- spectra.line.table[spectra.line.table$Spectrum %in% holdFrame()$Spectrum, ]
            
            
            predict.intensity.list <- if(input$normcal==1){
                pblapply(cephlopodVector(), function(x) lucas_simp_prep_xrf(spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=element, intercept.element.lines=c(element, x)))
            } else if(input$normcal==2){
                pblapply(cephlopodVector(), function(x) lucas_tc_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=element, intercept.element.lines=c(element, x)))
            } else if(input$normcal==3){
                pblapply(cephlopodVector(), function(x) lucas_comp_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=element, intercept.element.lines=c(element, x), norm.min=input$comptonmin, norm.max=input$comptonmax))
            }
            
            optimal_intercept_chain_xrf(element=element, intensities=predict.intensity.list, values=concentration.table, keep=vals$keeprows)
            
            
        })
        
        
        intercepthold <- reactiveValues()
        intercepthold$intercepts <- NULL
        
        observeEvent(input$calcurveelement, {
            
            isolate(intercepthold$intercepts <- inVar3Selectedpre())
            
        })
        
        
        #observeEvent(input$trainSlope, {
        
        #isolate(intercepthold$intercepts <- bestInterceptVars())
        
        #})
        
        
        inVar3Selected <- reactive({
            
            intercepthold$intercepts
            
            
        })
        
        output$inVar3 <- renderUI({
            req(input$radiocal)
            
            if(input$radiocal==3 | input$radiocal==4 | input$radiocal==6 | input$radiocal==8){
                selectInput(inputId = "intercept_vars", label = h4("Intercept"), choices =  outVaralt2(), selected=inVar3Selected(), multiple=TRUE)
            } else if(input$radiocal!=3 | input$radiocal!=4 | input$radiocal!=6 | input$radiocal!=8){
                NULL
            }
        })
        
        
        
        ####Machine Learning: Slope
        
        slopeImportance <- reactive({
            varImp(forestModel(), scale=FALSE)
        })
       
        slopeImportanceFrame <- reactive({
            
            forest.imp <- slopeImportance()$importance
            colnames(forest.imp) <- "Importance"
            forest.imp$Element <- rownames(forest.imp)
            forest.imp
            
        })
        
        slopeImportancePlot <- reactive({
            
            ggplot(slopeImportanceFrame(), aes(reorder(Element, Importance), Importance)) + geom_bar(stat="identity", position="dodge") + theme_light() + coord_flip() + scale_x_discrete("Element")

        })
        
        
        rainForestImportance <- reactive({
            
            
            forest.imp <- as.data.frame(varImp(elementModel(), scale=FALSE)$importance)
            
            
        })
        
        
        
        importanceranges <- reactiveValues(x = NULL, y = NULL)
        
        
        
        
        
        
        
        
        importanceFrame <- reactive({
            
            importance.frame <- rainForestImportance()
            colnames(importance.frame) <- c("Importance")
            importance.frame$Energy <- as.numeric(gsub("X", "", rownames(importance.frame)))
            importance.frame
        })

            
        
        rainForestImportancePlot <- reactive({
            
            importance.frame <- importanceFrame()
            
            
            element <- datasetInputVar()
            intensity.norm <- (element$Intensity/max(element$Intensity))*max(importance.frame$Importance)
            intensity.base <- (element$Intensity/max(element$Intensity))
            
            ggplot(importance.frame) +
            geom_line(aes(Energy, Importance)) +
            geom_segment(data=element, aes(x=Line, xend=Line, y = 0, yend=intensity.norm), colour="grey50", linetype=2)  +
            theme_light() +
            scale_x_continuous("Energy (keV)", breaks=scales::pretty_breaks()) +
            scale_y_continuous(paste0(input$calcurveelement, " Importance"), breaks=scales::pretty_breaks()) +
            coord_cartesian(xlim = importanceranges$x, ylim = importanceranges$y, expand = TRUE)
            
            
        })
        
        # When a double-click happens, check if there's a brush on the plot.
        # If so, zoom to the brush bounds; if not, reset the zoom.
        observeEvent(input$cropvar, {
            data <- dataHold()
            brush <- input$plot_var_brush
            if (!is.null(brush)) {
                importanceranges$x <- c(brush$xmin, brush$xmax)
                importanceranges$y <- c(brush$ymin, brush$ymax)
                
            } else {
                importanceranges$x <- NULL
                importanceranges$y <- NULL
            }
            
            
            
        })
        
        variablesPlot <- reactive({
            
            if(calType()!=5){
                slopeImportancePlot()
            } else if(calType()==5){
                rainForestImportancePlot()
            }
            
        })
        
        
        
        
        output$varelementui <- renderUI({
            
            selectInput(
            "elementvar", "Element:",
            choices=c("(Ne) Neon" = "Ne",
            "(Na) Sodium" = "Na",
            "(Mg) Magnesium" = "Mg",
            "(Al) Aluminum" = "Al",
            "(Si) Silicon" = "Si",
            "(P)  Phosphorous" = "P",
            "(S)  Sulfur" = "S",
            "(Cl) Chlorine" = "Cl",
            "(Ar) Argon" = "Ar",
            "(K)  Potassium" = "K",
            "(Ca) Calcium" = "Ca",
            "(Sc) Scandium" = "Sc",
            "(Ti) Titanium" = "Ti",
            "(V) Vanadium" = "V",
            "(Cr) Chromium" = "Cr",
            "(Mn) Manganese" = "Mn",
            "(Fe) Iron" = "Fe",
            "(Co) Cobalt" = "Co",
            "(Ni) Nickel" = "Ni",
            "(Cu) Copper" = "Cu",
            "(Zn) Zinc"= "Zn",
            "(Ga) Gallium" = "Ga",
            "(Ge) Germanium" = "Ge",
            "(As) Arsenic" = "As",
            "(Se) Selenium" = "Se",
            "(Br) Bromium" = "Br",
            "(Kr) Krypton" = "Kr",
            "(Rb) Rubidium" = "Rb",
            "(Sr) Strontium" = "Sr",
            "(Y)  Yttrium" = "Y",
            "(Zr) Zirconium" = "Zr",
            "(Nb) Niobium" = "Nb",
            "(Mo) Molybdenum" = "Mo",
            "(Tc) Technicium" = "Tc",
            "(Ru) Ruthenium" = "Ru",
            "(Rh) Rhodium" = "Rh",
            "(Pd) Paladium" = "Pd",
            "(Ag) Silver" = "Ag",
            "(Cd) Cadmium" = "Cd",
            "(In) Indium" = "In",
            "(Sn) Tin" = "Sn",
            "(Sb) Antimony" = "Sb",
            "(Te) Tellerium" = "Te",
            "(I) Iodine" = "I",
            "(Xe) Xenon" = "Xe",
            "(Cs) Cesium" = "Cs",
            "(Ba) Barium" = "Ba",
            "(La) Lanthanum" = "La",
            "(Ce) Cerium" = "Ce",
            "(Pr) Praeseodymeum" = "Pr",
            "(Nd) Neodymeum" = "Nd",
            "(Pr) Promethium" = "Pr",
            "(Sm) Samarium" = "Sm",
            "(Eu) Europium" = "Eu",
            "(Gd) Gadolinium" = "Gd",
            "(Tb) Terbium" = "Tb",
            "(Dy) Dysprosium" = "Dy",
            "(Ho) Holmium" = "Ho",
            "(Er) Erbium" = "Er",
            "(Tm) Thullium" = "Tm",
            "(Yb) Ytterbium" = "Yb",
            "(Lu) Lutetium" = "Lu",
            "(Hf) Halfnium" = "Hf",
            "(Ta) Tantalum" = "Ta",
            "(W)  Tungsten" = "W",
            "(Re) Rhenium" = "Re",
            "(Os) Osmium" = "Os",
            "(Ir) Irridium" = "Ir",
            "(Pt) Platinum" = "Pt",
            "(Au) Gold" = "Au",
            "(Hg) Mercury" = "Hg",
            "(Tl) Thallium" = "Tl",
            "(Pb) Lead" = "Pb",
            "(Bi) Bismuth" = "Bi",
            "(Po) Polonium" = "Po",
            "(At) Astatine" = "At",
            "(Rn) Radon" = "Rn",
            "(Fr) Francium" = "Fr",
            "(Ra) Radium" = "Ra",
            "(Ac) Actinum" = "Ac",
            "(Th) Thorium" = "Th",
            "(Pa) Proactinum" = "Pa",
            "(U)  Uranium" = "U"),
            selected=strsplit(x=input$calcurveelement, split="\\.")[[1]][1])
            
        })
        
        
        
        # Return the requested dataset
        datasetInputVar <- reactive({
            switch(input$elementvar,
            "H" = H.table,
            "He" = He.table,
            "Li" = Li.table,
            "Be" = Be.table,
            "B" = B.table,
            "C" = C.table,
            "N" = N.table,
            "O" = O.table,
            "F" = F.table,
            "Ne" = Ne.table,
            "Na" = Na.table,
            "Mg" = Mg.table,
            "Al" = Al.table,
            "Si" = Si.table,
            "P" = P.table,
            "S" = S.table,
            "Cl" = Cl.table,
            "Ar" = Ar.table,
            "K" = K.table,
            "Ca" = Ca.table,
            "Sc" = Sc.table,
            "Ti" = Ti.table,
            "V" = V.table,
            "Cr" = Cr.table,
            "Mn" = Mn.table,
            "Fe" = Fe.table,
            "Co" = Co.table,
            "Ni" = Ni.table,
            "Cu" = Cu.table,
            "Zn" = Zn.table,
            "Ga" = Ga.table,
            "Ge" = Ge.table,
            "As" = As.table,
            "Se" = Se.table,
            "Br" = Br.table,
            "Kr" = Kr.table,
            "Rb" = Rb.table,
            "Sr" = Sr.table,
            "Y" = Y.table,
            "Zr" = Zr.table,
            "Nb" = Nb.table,
            "Mo" = Mo.table,
            "Tc" = Tc.table,
            "Ru" = Ru.table,
            "Rh" = Rh.table,
            "Pd" = Pd.table,
            "Ag" = Ag.table,
            "Cd" = Cd.table,
            "In" = In.table,
            "Sn" = Sn.table,
            "Sb" = Sb.table,
            "Te" = Te.table,
            "I" = I.table,
            "Xe" = Xe.table,
            "Cs" = Cs.table,
            "Ba" = Ba.table,
            "La" = La.table,
            "Ce" = Ce.table,
            "Pr" = Pr.table,
            "Nd" = Nd.table,
            "Pm" = Pm.table,
            "Sm" = Sm.table,
            "Eu" = Eu.table,
            "Gd" = Gd.table,
            "Tb" = Tb.table,
            "Dy" = Dy.table,
            "Ho" = Ho.table,
            "Er" = Er.table,
            "Tm" = Tm.table,
            "Yb" = Yb.table,
            "Lu" = Lu.table,
            "Hf" = Hf.table,
            "Ta" = Ta.table,
            "W" = W.table,
            "Re" = Re.table,
            "Os" = Os.table,
            "Ir" = Ir.table,
            "Pt" = Pt.table,
            "Au" = Au.table,
            "Hg" = Hg.table,
            "Tl" = Tl.table,
            "Pb" = Pb.table,
            "Bi" = Bi.table,
            "Po" = Po.table,
            "At" = At.table,
            "Rn" = Rn.table,
            "Fr" = Fr.table,
            "Ra" = Ra.table,
            "Ac" = Ac.table,
            "Th" = Th.table,
            "Pa" = Pa.table,
            "U" = U.table)
        })
        
        
        output$importanceplot <- renderPlot({
            
            variablesPlot()
            
        })
        
        
        output$hover_info_variable <- renderUI({
            req(input$radiocal)
            if(calType()==5){
                
                point.table <- importanceFrame()
                
                hover <- input$plot_hover_variable
                point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
                if (nrow(point) == 0) return(NULL)
                
                # calculate point position INSIDE the image as percent of total dimensions
                # from left (horizontal) and from top (vertical)
                left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
                top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
                
                # calculate distance from left and bottom side of the picture in pixels
                left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
                top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
                
                
                # create style property fot tooltip
                # background color is set so tooltip is a bit transparent
                # z-index is set so we are sure are tooltip will be on top
                style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                "left:", left_px + 2, "px; top:", top_px + 2, "px;")
                
                # actual tooltip created as wellPanel
                wellPanel(
                style = style,
                p(HTML(paste0("Energy:", " ", round(point$Energy, 2)))),
                p(HTML(paste0("Importance:", " ", round(point$Importance, 1))))
                )
            } else if(calType()!=5){
                NULL
            }
        })
        
        output$variablePlot <- downloadHandler(
        filename = function() { paste0(input$calname, "_", input$calcurveelemenet , '_Variables', '.tiff', sep='') },
        content = function(file) {
            ggsave(file,variablesPlot(), width=plotDimensions()[1], height=plotDimensions()[2], device="tiff", compression="lzw",  dpi=300)
        }
        )
        
        
        
        
        
        fishVector <- reactive({
            
            combos_mod_xrf <- function(a.vector){
                
                so <- seq(from=2, to=input$nvariables, by=1)
                
                #long <- pblapply(so, function(x) gRbase::combnPrim(x=a.vector, m=x), cl=6L)
                and <- pblapply(long, function(x) plyr::alply(x, 2), cl=6L)
                thanks.for.all.the.fish <- do.call(list, unlist(and, recursive=FALSE))
                thanks.for.all.the.fish <- pblapply(thanks.for.all.the.fish, function(x) c(input$calcurveelement, x))
                
                thanks.for.all.the.fish
                
            }
            combos_mod_xrf <- cmpfun(combos_mod_xrf)

            
            fit.lm <- caretSlope()
            
            first.combos <- c(elementallinestouse()[!elementallinestouse() %in% input$calcurveelement])
            
            coef.frame <- as.data.frame(summary(fit.lm)$coefficients)
            sig.frame <- subset(coef.frame, coef.frame[,4] < 0.05)
            
            second.combos <- first.combos[c(first.combos %in% rownames(sig.frame)[c(!rownames(sig.frame) %in% "(Intercept)")])]
            
            
            
            combos_mod(second.combos)
            
            
        })
        
        
        bestSlopeVars <- reactive({
            
            element <- input$calcurveelement
            
            choices <- elementallinestouse()
            spectra.line.table <- spectraLineTable()
            data <- dataNorm()
            concentration.table <- concentrationTable()
            
            #concentration.table[complete.cases(concentration.table[,input$calcurveelement]),]
            
            #index <- complete.cases(concentration.table[,input$calcurveelement])
            
            
            spectra.line.table <- spectraLineTable()[spectraLineTable()$Spectrum %in% holdFrame()$Spectrum, ]
            
            spectra.line.table <- spectra.line.table[spectra.line.table$Spectrum %in% concentration.table$Spectrum, ]
            
            #spectra.line.table <- spectra.line.table[complete.cases(concentration.table[, element]),]
            
            data <- data[data$Spectrum %in% concentration.table$Spectrum, ]
            
            
            
            
            predict.intensity <- if(input$normcal==1){
                if(dataType()=="Spectra"){
                    lucas_simp_prep_xrf(spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=choices, intercept.element.lines=input$intercept_vars)
                } else if(dataType()=="Net"){
                    lucas_simp_prep_xrf_net(spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=choices, intercept.element.lines=input$intercept_vars)
                }
            } else if(input$normcal==2){
                predict.intensity <- if(dataType()=="Spectra"){
                    lucas_tc_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=choices, intercept.element.lines=input$intercept_vars)
                } else if(dataType()=="Net"){
                    lucas_tc_prep_xrf_net(data=data, spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=choices, intercept.element.lines=input$intercept_vars)
                }
            } else if(input$normcal==3){
                predict.intensity <- if(dataType()=="Spectra"){
                    lucas_comp_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=choices, intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
                } else if(dataType()=="Net"){
                    lucas_comp_prep_xrf_net(data=data, spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=choices, intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
                }
            }
            
            
            
            #optimal_r_chain(element=element, intensities=predict.intensity, values= concentration.table, possible.Slope=fishVector(), keep=vals$keeprows)
            
            results <- variable_select_short_xrf(slopeImportance())
            
            c(input$calcurveelement, results[!results %in% input$calcurveelement])
            
            
        })
        
        slopehold <- reactiveValues()
        slopehold$Slope <- NULL
        
        observeEvent(input$calcurveelement, {
            
            isolate(slopehold$Slope <- inVar4Selectedpre())
            
        })
        observeEvent(input$trainSlope, {
            
            isolate(slopehold$Slope <- bestSlopeVars())
            
        })
        
        
        inVar4Selected <- reactive({
            
            slopehold$Slope
            
            
        })
        
        
        
        output$inVar4 <- renderUI({
            req(input$radiocal)
            if(input$radiocal==3){
                selectInput(inputId = "slope_vars", label = h4("Slope"), choices =  outVaralt(), selected=inVar4Selected(), multiple=TRUE)
            } else if(input$radiocal!=3){
                NULL
            }
        })
        
        
    
        
        #####Machine Learning: Cal Type
        
        
        
        calTypeSelectionPre <- reactive({
            if(!"CalType" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calConditions$hold[["CalTable"]][["CalType"]]
            } else if("CalType" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calSettings$calList[[input$calcurveelement]][[1]]$CalTable$CalType[1]
            }
        })
        
        calhold <- reactiveValues()
        
        observeEvent(input$calcurveelement, {
            calhold$caltype <<- NULL
            calhold$caltype <<- calTypeSelectionPre()
        })
        
        
        observeEvent(input$trainSlope, {
            
            isolate(calhold$caltype <<- bestCalType())
            
        })
        
        calTypeSelection <- reactive({
            calhold$caltype
        })
        
        output$calTypeInput <- renderUI({
            req(input$calcurveelement)
            selectInput("radiocal", label = "Calibration Curve",
            choices = list("Linear" = 1, "Non-Linear" = 2, "Lucas-Tooth" = 3, "Forest" = 4, "Rainforest"=5, "Neural Network Intensities"=6, "Neural Network Spectra"=7, "XGBoost Intensities"=8, "XGBoost Spectra"=9),
            selected = calTypeSelection())
            
            
        })
        
        

        
        holdFrameCal <- reactive({
            req(input$radiocal, input$calcurveelement)
            hold.frame <- holdFrame()
            hold.frame
        })
        
        dataNormCal <- reactive({
            req(input$radiocal, input$calcurveelement)
            data <- dataNorm()
            hold.frame <-holdFrameCal()
            data.norm <- data[data$Spectrum %in% hold.frame$Spectrum, ]
            
            data.norm
        })

        linearParameters <- reactive(label="linearParameters", {
            req(input$radiocal, input$calcurveelement)
            list(CalTable=calConditionsTable(cal.type=1, norm.type=input$normcal, norm.min=input$comptonmin, norm.max=input$comptonmax), StandardsUsed=vals$keeprows)
        })
        linearModelData <- reactive(label="linearModelData", {
            req(input$radiocal, input$calcurveelement)
            predictFrameSimpGen(spectra=dataNormCal(), hold.frame=holdFrameCal(), element=input$calcurveelement,  norm.type=nonLinearParameters()$CalTable$NormType, norm.min=nonLinearParameters()$CalTable$Min, norm.max=nonLinearParameters()$CalTable$Max, data.type=dataType())
        })
        linearModelSet <- reactive(label="linearModelSet", {
            req(input$radiocal, input$calcurveelement)
            list(data=linearModelData(), parameters=linearParameters())
        })
        linearModel <- reactive(label="nonLinearModel", {
            
            predict.frame <- linearModelSet()$data[linearModelSet()$parameters$StandardsUsed,]
            
            l.model <- tryCatch(lm(Concentration~Intensity, data=predict.frame, na.action=na.omit), error=function(e) NULL)
            
            l.model
            
        })
        
        nonLinearParameters <- reactive(label="nonLinearParameters", {
            req(input$radiocal, input$calcurveelement)
            list(CalTable=calConditionsTable(cal.type=2, norm.type=input$normcal, norm.min=input$comptonmin, norm.max=input$comptonmax), StandardsUsed=vals$keeprows)
        })
        nonLinearModelData <- reactive(label="nonLinearModelData", {
            req(input$radiocal, input$calcurveelement)
            predictFrameSimpGen(spectra=dataNormCal(), hold.frame=holdFrameCal(), element=input$calcurveelement,  norm.type=nonLinearParameters()$CalTable$NormType, norm.min=nonLinearParameters()$CalTable$Min, norm.max=nonLinearParameters()$CalTable$Max, data.type=dataType())
        })
        nonLinearModelSet <- reactive(label="nonLinearModelSet", {
            req(input$radiocal, input$calcurveelement)
            list(data=nonLinearModelData(), parameters=nonLinearParameters())
        })
        nonLinearModel <- reactive(label="nonLinearModel", {
            
            predict.frame <- nonLinearModelSet()$data[nonLinearModelSet()$parameters$StandardsUsed,]
            
            nl.model <- tryCatch(lm(Concentration~Intensity + I(Intensity^2), data=predict.frame, na.action=na.omit), error=function(e) NULL)
            
            nl.model
            
        })
        
        lucasToothParameters <- reactive(label="lucasToothParameters", {
            req(input$radiocal, input$calcurveelement)
            list(CalTable=calConditionsTable(cal.type=3, norm.type=input$normcal, norm.min=input$comptonmin, norm.max=input$comptonmax), Slope=input$slope_vars, Intercept=input$intercept_vars, StandardsUsed=vals$keeprows)
        })
        lucasToothModelData <- reactive(label="lucasToothModelData", {
            req(input$radiocal, input$calcurveelement)
            predictFrameLucGen(spectra=dataNormCal(), hold.frame=holdFrameCal(), element=input$calcurveelement, intercepts=lucasToothParameters()$Intercept, slopes=lucasToothParameters()$Slope, norm.type=lucasToothParameters()$CalTable$NormType, norm.min=lucasToothParameters()$CalTable$Min, norm.max=lucasToothParameters()$CalTable$Max, data.type=dataType())
        })
        lucasToothModelSet <- reactive(label="lucasToothModelSet", {
            req(input$radiocal, input$calcurveelement)
            list(data=lucasToothModelData(), parameters=lucasToothParameters())
        })
        lucasToothModel <- reactive(label="lucasToothModel", {
            
            predict.frame <- lucasToothModelSet()$data[lucasToothModelSet()$parameters$StandardsUsed,]
            
            lc.model <- tryCatch(lm(Concentration~., data=predict.frame, na.action=na.omit), error=function(e) NULL)

            lc.model
            
        })
        
        forestParameters <- reactive(label="forestParameters", {
            req(input$radiocal, input$calcurveelement)
            cvrepeats <- if(input$foresttrain=="repeatedcv"){
                input$cvrepeats
            } else if(input$foresttrain!="repeatedcv"){
                1
            }
            list(CalTable=calConditionsTable(cal.type=4, norm.type=input$normcal, norm.min=input$comptonmin, norm.max=input$comptonmax, foresttry=input$foresttry, forestmetric=input$forestmetric, foresttrain=input$foresttrain, forestnumber=input$forestnumber, cvrepeats=cvrepeats, foresttrees=input$foresttrees), Intercept=input$intercept_vars, StandardsUsed=vals$keeprows)
        })
        forestModelData <- reactive(label="forestModelData", {
            req(input$radiocal, input$calcurveelement)
            predictFrameForestGen(spectra=dataNormCal(), hold.frame=holdFrameCal(), element=input$calcurveelement, intercepts=forestParameters()$Intercept, norm.type=forestParameters()$CalTable$NormType, norm.min=forestParameters()$CalTable$Min, norm.max=forestParameters()$CalTable$Max, data.type=dataType())
        })
        forestModelSet <- reactive(label="forestModelSet", {
            req(input$radiocal, input$calcurveelement)
            list(data=forestModelData(), parameters=forestParameters())
        })
        forestModel <- reactive(label="forestModel", {
            req(input$radiocal, input$calcurveelement)
            predict.frame <- forestModelSet()$data[forestModelSet()$parameters$StandardsUsed,]
            parameters <- forestModelSet()$parameters$CalTable
            
            rf.grid <- expand.grid(.mtry=parameters$ForestTry)
            
            metricModel <- if(parameters$ForestMetric=="RMSE" | parameters$ForestMetric=="Rsquared"){
                defaultSummary
            } else if(parameters$ForestMetric=="MAE"){
                maeSummary
            } else if(parameters$ForestMetric=="logMAE"){
                logmaeSummary
            } else if(parameters$ForestMetric=="SMAPE"){
                smapeSummary
            }
            
            tune_control <- if(parameters$ForestTC!="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                summaryFunction=metricModel)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                summaryFunction=metricModel)
            }
            
            
            cl <- if(get_os()=="windows"){
                parallel::makePSOCKcluster(as.numeric(my.cores))
            } else if(get_os()!="windows"){
                parallel::makeForkCluster(as.numeric(my.cores))
            }
            registerDoParallel(cl)
            
            rf_model <- tryCatch(caret::train(Concentration~.,data=predict.frame,method="rf", type="Regression",
            trControl=tune_control, ntree=parameters$ForestTrees,
            prox=TRUE,allowParallel=TRUE, importance=TRUE, metric=parameters$ForestMetric, tuneGrid=rf.grid, na.action=na.omit, trim=TRUE), error=function(e) NULL)
            
            stopCluster(cl)
            rf_model
            
        })
        
        rainforestParameters <- reactive(label="rainforestParameters", {
            req(input$radiocal, input$calcurveelement)
            cvrepeats <- if(input$foresttrain=="repeatedcv"){
                input$cvrepeats
            } else if(input$foresttrain!="repeatedcv"){
                1
            }
            list(CalTable=calConditionsTable(cal.type=5, compress=input$compress, transformation=input$transformation, energy.range=paste0(input$energyrange[1], "-", input$energyrange[2]), norm.type=input$normcal, norm.min=input$comptonmin, norm.max=input$comptonmax, foresttry=input$foresttry, forestmetric=input$forestmetric, foresttrain=input$foresttrain, forestnumber=input$forestnumber, cvrepeats=cvrepeats, foresttrees=input$foresttrees), StandardsUsed=vals$keeprows)
        })
        rainforestModelData <- reactive(label="rainforestModelData", {
            req(input$radiocal, input$calcurveelement)
            rainforestDataGen(spectra=dataNormCal(), compress=rainforestParameters()$CalTable$Compress, transformation=rainforestParameters()$CalTable$Transformation, energy.range=as.numeric(unlist(strsplit(as.character(rainforestParameters()$CalTable$EnergyRange), "-"))), hold.frame=holdFrameCal(), norm.type=rainforestParameters()$CalTable$NormType, norm.min=rainforestParameters()$CalTable$Min, norm.max=rainforestParameters()$CalTable$Max, data.type=dataType())
        })
        #rainforestModelSetlist <- reactiveValues()
        #observeEvent(input$createcalelement, priority=150, {
        #    rainforestModelSet$list <- list(data=rainforestModelData(), parameters=rainforestParameters())
        #})
        
        rainforestModelSet <- reactive(label="rainforestModelSet", {
            req(input$radiocal, input$calcurveelement)
            list(data=rainforestModelData(), parameters=rainforestParameters())
        })
        rainforestModel <- reactive(label="rainforestModel", {
            req(input$radiocal, input$calcurveelement)
            data <- rainforestModelSet()$data[rainforestModelSet()$parameters$StandardsUsed,]
            parameters <- rainforestModelSet()$parameters$CalTable
            
            rf.grid <- expand.grid(.mtry=parameters$ForestTry)
            
            metricModel <- if(parameters$ForestMetric=="RMSE" | parameters$ForestMetric=="Rsquared"){
                defaultSummary
            } else if(parameters$ForestMetric=="MAE"){
                maeSummary
            } else if(parameters$ForestMetric=="logMAE"){
                logmaeSummary
            } else if(parameters$ForestMetric=="SMAPE"){
                smapeSummary
            }
            
            tune_control <- if(parameters$ForestTC!="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                summaryFunction=metricModel)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                summaryFunction=metricModel)
            }
            
            
            cl <- if(get_os()=="windows"){
                parallel::makePSOCKcluster(as.numeric(my.cores))
            } else if(get_os()!="windows"){
                parallel::makeForkCluster(as.numeric(my.cores))
            }
            registerDoParallel(cl)
            
            rf_model <- tryCatch(caret::train(Concentration~.,data=data[,-1], method="rf", type="Regression",
            trControl=tune_control, ntree=parameters$ForestTrees,
            prox=TRUE,allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=rf.grid, na.action=na.omit, importance=TRUE, trim=TRUE), error=function(e) NULL)
            
            
            stopCluster(cl)
            rf_model
            
        })
        
        neuralNetworkIntensityShallowParameters <- reactive(label="neuralNetworkIntensityShallowParameters", {
            req(input$radiocal, input$calcurveelement)
            cvrepeats <- if(input$foresttrain=="repeatedcv"){
                input$cvrepeats
            } else if(input$foresttrain!="repeatedcv"){
                1
            }
            list(CalTable=calConditionsTable(cal.type=6, norm.type=input$normcal, norm.min=input$comptonmin, norm.max=input$comptonmax, forestmetric=input$forestmetric, foresttrain=input$foresttrain, forestnumber=input$forestnumber, cvrepeats=cvrepeats, neuralhiddenlayers=input$neuralhiddenlayers, neuralhiddenunits=paste0(input$neuralhiddenunits[1], "-", input$neuralhiddenunits[2]), neuralweightdecay=paste0(input$neuralweightdecay[1], "-", input$neuralweightdecay[2]), neuralmaxiterations=input$neuralmaxiterations), Intercept=input$intercept_vars, StandardsUsed=vals$keeprows)
        })
        neuralNetworkIntensityShallowModelData <- reactive(label="neuralNetworkIntensityShallowModelData", {
            req(input$radiocal, input$calcurveelement)
            predictFrameForestGen(spectra=dataNormCal(), hold.frame=holdFrameCal(), element=input$calcurveelement, intercepts=neuralNetworkIntensityShallowParameters()$Intercept, norm.type=neuralNetworkIntensityShallowParameters()$CalTable$NormType, norm.min=neuralNetworkIntensityShallowParameters()$CalTable$Min, norm.max=neuralNetworkIntensityShallowParameters()$CalTable$Max, data.type=dataType())
        })
        neuralNetworkIntensityShallowModelSet <- reactive(label="neuralNetworkIntensityShallowModelSet", {
            req(input$radiocal, input$calcurveelement)
            list(data=neuralNetworkIntensityShallowModelData(), parameters=neuralNetworkIntensityShallowParameters())
        })
        neuralNetworkIntensityShallow <- reactive(label="neuralNetworkIntensityShallow", {
            req(input$radiocal, input$calcurveelement)
            
            predict.frame <- neuralNetworkIntensityShallowModelSet()$data[neuralNetworkIntensityShallowModelSet()$parameters$StandardsUsed,]
            parameters <- neuralNetworkIntensityShallowModelSet()$parameters$CalTable
            
            
            weightdecay.vec <- as.numeric(unlist(strsplit(as.character(parameters$NeuralWD), "-")))
            hiddenunits.vec <- as.numeric(unlist(strsplit(as.character(parameters$NeuralHU), "-")))
            
            
            nn.grid <- expand.grid(
            .decay = seq(weightdecay.vec[1], weightdecay.vec[2], 0.1),
            .size = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1))
            
            metricModel <- if(parameters$ForestMetric=="RMSE" | parameters$ForestMetric=="Rsquared"){
                defaultSummary
            } else if(parameters$ForestMetric=="MAE"){
                maeSummary
            } else if(parameters$ForestMetric=="logMAE"){
                logmaeSummary
            } else if(parameters$ForestMetric=="SMAPE"){
                smapeSummary
            }
            
            tune_control <- if(parameters$ForestTC!="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                summaryFunction=metricModel)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                summaryFunction=metricModel)
            }
            
            cl <- if(get_os()=="windows"){
                parallel::makePSOCKcluster(as.numeric(my.cores))
            } else if(get_os()!="windows"){
                parallel::makeForkCluster(as.numeric(my.cores))
            }
            registerDoParallel(cl)
            
            nn_model <- tryCatch(caret::train(Concentration~.,data=predict.frame, method="nnet", linout=TRUE, trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, na.action=na.omit, importance=TRUE, tuneGrid=nn.grid, maxit=parameters$NeuralMI, trace=F, trim=TRUE), error=function(e) NULL)
            
            
            stopCluster(cl)
            nn_model
            
        })
        
        neuralNetworkIntensityDeepParameters <- reactive(label="neuralNetworkIntensityDeepParameters", {
            req(input$radiocal, input$calcurveelement)
            cvrepeats <- if(input$foresttrain=="repeatedcv"){
                input$cvrepeats
            } else if(input$foresttrain!="repeatedcv"){
                1
            }
            list(CalTable=calConditionsTable(cal.type=6, norm.type=input$normcal, norm.min=input$comptonmin, norm.max=input$comptonmax, foresttry=input$foresttry, forestmetric=input$forestmetric, foresttrain=input$foresttrain, forestnumber=input$forestnumber, cvrepeats=cvrepeats, neuralhiddenlayers=input$neuralhiddenlayers, neuralhiddenunits=paste0(input$neuralhiddenunits[1], "-", input$neuralhiddenunits[2])), Intercept=input$intercept_vars, StandardsUsed=vals$keeprows)
        })
        neuralNetworkIntensityDeepModelData <- reactive(label="neuralNetworkIntensityDeepModelData", {
            req(input$radiocal, input$calcurveelement)
            predictFrameForestGen(spectra=dataNormCal(), hold.frame=holdFrameCal(), element=input$calcurveelement, intercepts=neuralNetworkIntensityDeepParameters()$Intercept, norm.type=neuralNetworkIntensityDeepParameters()$CalTable$NormType, norm.min=neuralNetworkIntensityDeepParameters()$CalTable$Min, norm.max=neuralNetworkIntensityDeepParameters()$CalTable$Max, data.type=dataType())
        })
        neuralNetworkIntensityDeepModelSet <- reactive(label="neuralNetworkIntensityDeepModelSet", {
            req(input$radiocal, input$calcurveelement)
            list(data=neuralNetworkIntensityDeepModelData(), parameters=neuralNetworkIntensityDeepParameters())
        })
        neuralNetworkIntensityDeep <- reactive(label="neuralNetworkIntensityDeep", {
            req(input$radiocal, input$calcurveelement)
            
            predict.frame <- neuralNetworkIntensityDeepModelSet()$data[neuralNetworkIntensityDeepModelSet()$parameters$StandardsUsed,]
            parameters <- neuralNetworkIntensityDeepModelSet()$parameters$CalTable
            
            
            hiddenunits.vec <- as.numeric(unlist(strsplit(as.character(parameters$NeuralHU), "-")))
            
            nn.grid <- if(parameters$NeuralHL == 2){
                expand.grid(
                .layer1 = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1),
                .layer2 = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1),
                .layer3 = c(0)
                )
            } else if(parameters$NeuralHL == 3){
                expand.grid(
                .layer1 = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1),
                .layer2 = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1),
                .layer3 = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1)
                )
            }
            
            metricModel <- if(parameters$ForestMetric=="RMSE" | parameters$ForestMetric=="Rsquared"){
                defaultSummary
            } else if(parameters$ForestMetric=="MAE"){
                maeSummary
            } else if(parameters$ForestMetric=="logMAE"){
                logmaeSummary
            } else if(parameters$ForestMetric=="SMAPE"){
                smapeSummary
            }
            
            tune_control <- if(parameters$ForestTC!="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                summaryFunction=metricModel,
                verboseIter = TRUE,
                allowParallel = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                summaryFunction=metricModel,
                verboseIter = TRUE,
                allowParallel = TRUE)
            }
            
            cl <- if(get_os()=="windows"){
                parallel::makePSOCKcluster(as.numeric(my.cores))
            } else if(get_os()!="windows"){
                parallel::makeForkCluster(as.numeric(my.cores))
            }
            registerDoParallel(cl)
            
            f <- as.formula(paste("Concentration ~", paste(names(predict.frame)[!names(predict.frame) %in% "Concentration"], collapse = " + ")))
            
            nn_model <- tryCatch(caret::train(f,data=predict.frame, method="neuralnet", rep=parameters$ForestTry, trControl=tune_control, metric=parameters$ForestMetric, na.action=na.omit,  tuneGrid=nn.grid, linear.output=TRUE), error=function(e) NULL)
            
            
            stopCluster(cl)
            nn_model
            
        })
        
        neuralNetworkIntensityModelSet <- reactive(label="neuralNetworkIntensityModel", {
            if(input$neuralhiddenlayers == 1){
                neuralNetworkIntensityShallowModelSet()
            } else if(input$neuralhiddenlayers > 1){
                neuralNetworkIntensityDeepModelSet()
            }
        })
        
        neuralNetworkIntensityModel <- reactive(label="neuralNetworkIntensityModel", {
            if(input$neuralhiddenlayers == 1){
                neuralNetworkIntensityShallow()
            } else if(input$neuralhiddenlayers > 1){
                neuralNetworkIntensityDeep()
            }
        })
        
        neuralNetworkSpectraShallowParameters <- reactive(label="neuralNetworkSpectraShallowParameters", {
            req(input$radiocal, input$calcurveelement)
            cvrepeats <- if(input$foresttrain=="repeatedcv"){
                input$cvrepeats
            } else if(input$foresttrain!="repeatedcv"){
                1
            }
            list(CalTable=calConditionsTable(cal.type=7, compress=input$compress, transformation=input$transformation, energy.range=paste0(input$energyrange[1], "-", input$energyrange[2]), norm.type=input$normcal, norm.min=input$comptonmin, norm.max=input$comptonmax, forestmetric=input$forestmetric, foresttrain=input$foresttrain, forestnumber=input$forestnumber, cvrepeats=cvrepeats, neuralhiddenlayers=input$neuralhiddenlayers, neuralhiddenunits=paste0(input$neuralhiddenunits[1], "-", input$neuralhiddenunits[2]), neuralweightdecay=paste0(input$neuralweightdecay[1], "-", input$neuralweightdecay[2]), neuralmaxiterations=input$neuralmaxiterations), StandardsUsed=vals$keeprows)
        })
        neuralNetworkSpectraShallowModelData <- reactive(label="neuralNetworkSpectraShallowModelData", {
            req(input$radiocal, input$calcurveelement)
            rainforestDataGen(spectra=dataNormCal(), compress=neuralNetworkSpectraShallowParameters()$CalTable$Compress, transformation=neuralNetworkSpectraShallowParameters()$CalTable$Transformation, energy.range=as.numeric(unlist(strsplit(as.character(neuralNetworkSpectraShallowParameters()$CalTable$EnergyRange), "-"))), hold.frame=holdFrameCal(), norm.type=neuralNetworkSpectraShallowParameters()$CalTable$NormType, norm.min=neuralNetworkSpectraShallowParameters()$CalTable$Min, norm.max=neuralNetworkSpectraShallowParameters()$CalTable$Max, data.type=dataType())
        })
        neuralNetworkSpectraShallowModelSet <- reactive(label="neuralNetworkSpectraShallowModelSet", {
            req(input$radiocal, input$calcurveelement)
            list(data=neuralNetworkSpectraShallowModelData(), parameters=neuralNetworkSpectraShallowParameters())
        })
        neuralNetworkSpectraShallow <- reactive(label="neuralNetworkSpectraShallow", {
            req(input$radiocal, input$calcurveelement)
            
            data <- neuralNetworkSpectraShallowModelSet()$data[neuralNetworkSpectraShallowModelSet()$parameters$StandardsUsed,]
            parameters <- neuralNetworkSpectraShallowModelSet()$parameters$CalTable
            
            weightdecay.vec <- as.numeric(unlist(strsplit(as.character(parameters$NeuralWD), "-")))
            hiddenunits.vec <- as.numeric(unlist(strsplit(as.character(parameters$NeuralHU), "-")))
            
            
            nn.grid <- expand.grid(
            .decay = seq(weightdecay.vec[1], weightdecay.vec[2], 0.1),
            .size = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1))
            
            metricModel <- if(parameters$ForestMetric=="RMSE" | parameters$ForestMetric=="Rsquared"){
                defaultSummary
            } else if(parameters$ForestMetric=="MAE"){
                maeSummary
            } else if(parameters$ForestMetric=="logMAE"){
                logmaeSummary
            } else if(parameters$ForestMetric=="SMAPE"){
                smapeSummary
            }
            
            tune_control <- if(parameters$ForestTC!="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                summaryFunction=metricModel)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                summaryFunction=metricModel)
            }
            
            cl <- if(get_os()=="windows"){
                parallel::makePSOCKcluster(as.numeric(my.cores))
            } else if(get_os()!="windows"){
                parallel::makeForkCluster(as.numeric(my.cores))
            }
            registerDoParallel(cl)
            
            nn_model <- tryCatch(caret::train(Concentration~.,data=data[,-1], method="nnet", linout=TRUE, trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, na.action=na.omit, importance=TRUE, tuneGrid=nn.grid, maxit=parameters$NeuralMI, trace=F, trim=TRUE), error=function(e) NULL)
            
            
            stopCluster(cl)
            nn_model
            
        })
        
        neuralNetworkSpectraDeepParameters <- reactive(label="neuralNetworkSpectraDeepParameters", {
            req(input$radiocal, input$calcurveelement)
            cvrepeats <- if(input$foresttrain=="repeatedcv"){
                input$cvrepeats
            } else if(input$foresttrain!="repeatedcv"){
                1
            }
            list(CalTable=calConditionsTable(cal.type=7, compress=input$compress, transformation=input$transformation, energy.range=paste0(input$energyrange[1], "-", input$energyrange[2]), norm.type=input$normcal, norm.min=input$comptonmin, norm.max=input$comptonmax, foresttry=input$foresttry, forestmetric=input$forestmetric, foresttrain=input$foresttrain, forestnumber=input$forestnumber, cvrepeats=cvrepeats, neuralhiddenlayers=input$neuralhiddenlayers, neuralhiddenunits=paste0(input$neuralhiddenunits[1], "-", input$neuralhiddenunits[2])), StandardsUsed=vals$keeprows)
        })
        neuralNetworkSpectraDeepModelData <- reactive(label="neuralNetworkSpectraDeepModelData", {
            req(input$radiocal, input$calcurveelement)
            rainforestDataGen(spectra=dataNormCal(), compress=neuralNetworkSpectraDeepParameters()$CalTable$Compress, transformation=neuralNetworkSpectraDeepParameters()$CalTable$Transformation, energy.range=as.numeric(unlist(strsplit(as.character(neuralNetworkSpectraDeepParameters()$CalTable$EnergyRange), "-"))), hold.frame=holdFrameCal(), norm.type=neuralNetworkSpectraDeepParameters()$CalTable$NormType, norm.min=neuralNetworkSpectraDeepParameters()$CalTable$Min, norm.max=neuralNetworkSpectraDeepParameters()$CalTable$Max, data.type=dataType())
        })
        neuralNetworkSpectraDeepModelSet <- reactive(label="neuralNetworkSpectraDeepModelSet", {
            req(input$radiocal, input$calcurveelement)
            list(data=neuralNetworkSpectraDeepModelData(), parameters=neuralNetworkSpectraDeepParameters())
        })
        neuralNetworkSpectraDeep <- reactive(label="neuralNetworkSpectraDeep", {
            req(input$radiocal, input$calcurveelement)
            
            data <- neuralNetworkSpectraDeepModelSet()$data[neuralNetworkSpectraDeepModelSet()$parameters$StandardsUsed,]
            parameters <- neuralNetworkSpectraDeepModelSet()$parameters$CalTable
            
            hiddenunits.vec <- as.numeric(unlist(strsplit(as.character(parameters$NeuralHU), "-")))
            
            nn.grid <- if(parameters$NeuralHL == 2){
                expand.grid(
                .layer1 = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1),
                .layer2 = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1),
                .layer3 = c(0)
                )
            } else if(parameters$NeuralHL == 3){
                expand.grid(
                .layer1 = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1),
                .layer2 = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1),
                .layer3 = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1)
                )
            }
            
            metricModel <- if(parameters$ForestMetric=="RMSE" | parameters$ForestMetric=="Rsquared"){
                defaultSummary
            } else if(parameters$ForestMetric=="MAE"){
                maeSummary
            } else if(parameters$ForestMetric=="logMAE"){
                logmaeSummary
            } else if(parameters$ForestMetric=="SMAPE"){
                smapeSummary
            }
            
            tune_control <- if(parameters$ForestTC!="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                summaryFunction=metricModel,
                verboseIter = TRUE,
                allowParallel = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                summaryFunction=metricModel,
                verboseIter = TRUE,
                allowParallel = TRUE)
            }
            
            cl <- if(get_os()=="windows"){
                parallel::makePSOCKcluster(as.numeric(my.cores))
            } else if(get_os()!="windows"){
                parallel::makeForkCluster(as.numeric(my.cores))
            }
            registerDoParallel(cl)
            
            f <- as.formula(paste("Concentration ~", paste(names(data)[!names(data) %in% "Concentration"], collapse = " + ")))
            nn_model <- tryCatch(caret::train(f,data=data[,-1], method="neuralnet", rep=parameters$ForestTry, trControl=tune_control, metric=parameters$ForestMetric, na.action=na.omit, tuneGrid=nn.grid, linear.output=TRUE), error=function(e) NULL)
            
            
            stopCluster(cl)
            nn_model
            
        })
        
        neuralNetworkSpectraModelSet <- reactive(label="neuralNetworkSpectraModelSet", {
            if(input$neuralhiddenlayers == 1){
                neuralNetworkSpectraShallowModelSet()
            } else if(input$neuralhiddenlayers > 1){
                neuralNetworkSpectraDeepModelSet()
            }
        })
        
        neuralNetworkSpectraModel <- reactive(label="neuralNetworkSpectraModel", {
            
            if(input$neuralhiddenlayers == 1){
                neuralNetworkSpectraShallow()
            } else if(input$neuralhiddenlayers > 1){
                neuralNetworkSpectraDeep()
            }
            
        })
        
        xgboostIntensityParameters <- reactive(label="xgboostIntensityParameters", {
            req(input$radiocal, input$calcurveelement)
            cvrepeats <- if(input$foresttrain=="repeatedcv"){
                input$cvrepeats
            } else if(input$foresttrain!="repeatedcv"){
                1
            }
            list(CalTable=calConditionsTable(cal.type=8, norm.type=input$normcal, norm.min=input$comptonmin, norm.max=input$comptonmax, foresttrees=input$foresttrees, forestmetric=input$forestmetric, foresttrain=input$foresttrain, forestnumber=input$forestnumber, cvrepeats=cvrepeats, treedepth=paste0(input$treedepth[1], "-", input$treedepth[2]), xgbeta=paste0(input$xgbeta[1], "-", input$xgbeta[2]), xgbgamma=paste0(input$xgbgamma[1], "-", input$xgbgamma[2]), xgbsubsample=paste0(input$xgbsubsample[1], "-", input$xgbsubsample[2]), xgbcolsample=paste0(input$xgbcolsample[1], "-", input$xgbcolsample[2]), xgbminchild=input$xgbminchild), Intercept=input$intercept_vars, StandardsUsed=vals$keeprows)
        })
        xgboostIntensityModelData <- reactive(label="xgboostIntensityModelData", {
            req(input$radiocal, input$calcurveelement)
            predictFrameForestGen(spectra=dataNormCal(), hold.frame=holdFrameCal(), element=input$calcurveelement, intercepts=xgboostIntensityParameters()$Intercept, norm.type=xgboostIntensityParameters()$CalTable$NormType, norm.min=xgboostIntensityParameters()$CalTable$Min, norm.max=xgboostIntensityParameters()$CalTable$Max, data.type=dataType())
        })
        xgboostIntensityModelSet <- reactive(label="xgboostIntensityModelSet", {
            req(input$radiocal, input$calcurveelement)
            list(data=xgboostIntensityModelData(), parameters=xgboostIntensityParameters())
        })
        xgboostIntensityModel <- reactive(label="xgboostIntensityModel", {
            req(input$radiocal, input$calcurveelement)
            
            predict.frame <- xgboostIntensityModelSet()$data[xgboostIntensityModelSet()$parameters$StandardsUsed,]
            parameters <- xgboostIntensityModelSet()$parameters$CalTable
            
            
            tree.depth.vec <- as.numeric(unlist(strsplit(as.character(parameters$TreeDepth), "-")))
            xgbeta.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbEta), "-")))
            xgbgamma.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbGamma), "-")))
            xgbsubsample.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbSubSample), "-")))
            xgbcolsample.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbColSample), "-")))
            
            
            xgbGrid <- expand.grid(
            nrounds = seq(50, parameters$ForestTrees, by=parameters$ForestTrees/5),
            max_depth = seq(tree.depth.vec[1], tree.depth.vec[2], by=5),
            eta = seq(xgbeta.vec[1], xgbeta.vec[2], by=0.1),
            gamma=seq(xgbgamma.vec[1], xgbgamma.vec[2], by=0.1),
            colsample_bytree = seq(xgbcolsample.vec[1], xgbcolsample.vec[2], by=0.1),
            subsample = seq(xgbsubsample.vec[1], xgbsubsample.vec[2], by=0.1),
            min_child_weight = parameters$xgbMinChild
            )
            
            metricModel <- if(parameters$ForestMetric=="RMSE" | parameters$ForestMetric=="Rsquared"){
                defaultSummary
            } else if(parameters$ForestMetric=="MAE"){
                maeSummary
            } else if(parameters$ForestMetric=="logMAE"){
                logmaeSummary
            } else if(parameters$ForestMetric=="SMAPE"){
                smapeSummary
            }
            
            tune_control <- if(parameters$ForestTC!="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                summaryFunction=metricModel,
                verboseIter = TRUE,
                allowParallel = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                summaryFunction=metricModel,
                verboseIter = TRUE,
                allowParallel = TRUE)
            }
            
            cl <- if(get_os()=="windows"){
                parallel::makePSOCKcluster(as.numeric(my.cores))
            } else if(get_os()!="windows"){
                parallel::makeForkCluster(as.numeric(my.cores))
            }
            registerDoParallel(cl)
            
            xgb_model <- tryCatch(caret::train(Concentration~., data=predict.frame, trControl = tune_control, tuneGrid = xgbGrid, metric=parameters$ForestMetric, method = "xgbTree", na.action=na.omit), error=function(e) NULL)
            
            
            stopCluster(cl)
            xgb_model
            
        })
        
        xgboostSpectraParameters <- reactive(label="xgboostSpectraParameters", {
            req(input$radiocal, input$calcurveelement)
            cvrepeats <- if(input$foresttrain=="repeatedcv"){
                input$cvrepeats
            } else if(input$foresttrain!="repeatedcv"){
                1
            }
            list(CalTable=calConditionsTable(cal.type=9, compress=input$compress, transformation=input$transformation, energy.range=paste0(input$energyrange[1], "-", input$energyrange[2]), norm.type=input$normcal, norm.min=input$comptonmin, norm.max=input$comptonmax, foresttrees=input$foresttrees, forestmetric=input$forestmetric, foresttrain=input$foresttrain, forestnumber=input$forestnumber, cvrepeats=cvrepeats, treedepth=paste0(input$treedepth[1], "-", input$treedepth[2]), xgbeta=paste0(input$xgbeta[1], "-", input$xgbeta[2]), xgbgamma=paste0(input$xgbgamma[1], "-", input$xgbgamma[2]), xgbsubsample=paste0(input$xgbsubsample[1], "-", input$xgbsubsample[2]), xgbcolsample=paste0(input$xgbcolsample[1], "-", input$xgbcolsample[2]), xgbminchild=input$xgbminchild), StandardsUsed=vals$keeprows)
        })
        xgboostSpectraModelData <- reactive(label="xgboostSpectraModelData", {
            req(input$radiocal, input$calcurveelement)
            rainforestDataGen(spectra=dataNormCal(), compress=xgboostSpectraParameters()$CalTable$Compress, transformation=xgboostSpectraParameters()$CalTable$Transformation, energy.range=as.numeric(unlist(strsplit(as.character(xgboostSpectraParameters()$CalTable$EnergyRange), "-"))), hold.frame=holdFrameCal(), norm.type=xgboostSpectraParameters()$CalTable$NormType, norm.min=xgboostSpectraParameters()$CalTable$Min, norm.max=xgboostSpectraParameters()$CalTable$Max, data.type=dataType())
        })
        xgboostSpectraModelSet <- reactive(label="xgboostSpectraModelSet", {
            req(input$radiocal, input$calcurveelement)
            list(data=xgboostSpectraModelData(), parameters=xgboostSpectraParameters())
        })
        xgboostSpectraModel <- reactive(label="xgboostSpectraModel", {
            req(input$radiocal, input$calcurveelement)
            
            data <- xgboostSpectraModelSet()$data[xgboostSpectraModelSet()$parameters$StandardsUsed,]
            parameters <- xgboostSpectraModelSet()$parameters$CalTable
            
            tree.depth.vec <- as.numeric(unlist(strsplit(as.character(parameters$TreeDepth), "-")))
            xgbeta.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbEta), "-")))
            xgbgamma.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbGamma), "-")))
            xgbsubsample.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbSubSample), "-")))
            xgbcolsample.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbColSample), "-")))
            
            
            xgbGrid <- expand.grid(
            nrounds = seq(50, parameters$ForestTrees, by=parameters$ForestTrees/5),
            max_depth = seq(tree.depth.vec[1], tree.depth.vec[2], by=5),
            eta = seq(xgbeta.vec[1], xgbeta.vec[2], by=0.1),
            gamma=seq(xgbgamma.vec[1], xgbgamma.vec[2], by=0.1),
            colsample_bytree = seq(xgbcolsample.vec[1], xgbcolsample.vec[2], by=0.1),
            subsample = seq(xgbsubsample.vec[1], xgbsubsample.vec[2], by=0.1),
            min_child_weight = parameters$xgbMinChild
            )
            
            metricModel <- if(parameters$ForestMetric=="RMSE" | parameters$ForestMetric=="Rsquared"){
                defaultSummary
            } else if(parameters$ForestMetric=="MAE"){
                maeSummary
            } else if(parameters$ForestMetric=="logMAE"){
                logmaeSummary
            } else if(parameters$ForestMetric=="SMAPE"){
                smapeSummary
            }
            
            tune_control <- if(parameters$ForestTC!="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                summaryFunction=metricModel,
                verboseIter = TRUE,
                allowParallel = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                summaryFunction=metricModel,
                verboseIter = TRUE,
                allowParallel = TRUE)
            }
            
            cl <- if(get_os()=="windows"){
                parallel::makePSOCKcluster(as.numeric(my.cores))
            } else if(get_os()!="windows"){
                parallel::makeForkCluster(as.numeric(my.cores))
            }
            registerDoParallel(cl)
            
            xgb_model <- tryCatch(caret::train(Concentration~., data=data[,-1], trControl = tune_control, tuneGrid = xgbGrid, metric=parameters$ForestMetric, method = "xgbTree", na.action=na.omit), error=function(e) NULL)
            
            
            stopCluster(cl)
            xgb_model
            
        })
        
        isMCL <- reactive({
            req(input$radiocal)
            if(input$radiocal==1){
                FALSE
            } else if(input$radiocal==2){
                FALSE
            } else if(input$radiocal==3){
                FALSE
            } else if(input$radiocal==4){
                TRUE
            } else if(input$radiocal==5){
                TRUE
            } else if(input$radiocal==6){
                TRUE
            } else if(input$radiocal==7){
                TRUE
            } else if(input$radiocal==8){
                TRUE
            } else if(input$radiocal==9){
                TRUE
            }
        })
        
        bestCalTypeFrame <- reactive({
            
            concentration.table <- concentrationTable()
            data <- dataNorm()
            spectra.line.table <- spectraLineTable()
            
            
            #concentration.table <- concentration.table[complete.cases(concentration.table[, input$calcurveelement]),]
            
            #spectra.line.table <- spectra.line.table[complete.cases(concentration.table[, input$calcurveelement]),]
            #data2 <- data[data$Spectrum %in% concentration.table$Spectrum, ]
            
            predict.intensity.simp <- predictIntensitySimp()
            
            predict.intensity.luc <- predictIntensityLuc()
            
            predict.intensity.forest <- predictIntensityForest()
            
            spectra.data <- rainforestData()
            spectra.data <- spectra.data[complete.cases(spectra.data),]
            
            spectra.data <- spectra.data[ , !(names(spectra.data) %in% "Concentration")]
            
            
            
            predict.frame.simp <- predictFrameSimp()
            predict.frame.forest <- predictFrameForest()
            predict.frame.luc <- predictFrameLuc()
            predict.frame.rainforest <- rainforestData()
            
            cal.lm.simp <- simpleLinearModel()
            lm.predict <- predict(cal.lm.simp, newdata=predict.frame.simp)
            lm.sum <- summary(lm(predict.frame.simp$Concentration~lm.predict, na.action=na.exclude))
            
            cal.lm.two <- nonLinearModel()
            lm2.predict <- predict(cal.lm.two, newdata=predict.frame.simp)
            lm2.sum <- summary(lm(predict.frame.simp$Concentration~lm2.predict, na.action=na.exclude))
            
            cal.lm.luc <- lucasToothModel()
            lucas.predict <- predict(cal.lm.luc, newdata=predict.frame.luc)
            lucas.sum <- summary(lm(predict.frame.luc$Concentration~lucas.predict, na.action=na.exclude))
            
            cal.lm.forest <- forestModel()
            forest.predict <- predict(cal.lm.forest, newdata=predict.frame.forest)
            forest.sum <- summary(lm(predict.frame.forest$Concentration~forest.predict, na.action=na.exclude))
            
            cal.lm.rainforest <- rainforestModel()
            rainforest.predict <- predict(cal.lm.rainforest, newdata=predict.frame.rainforest)
            rainforest.sum <- summary(lm(predict.frame.rainforest$Concentration~rainforest.predict, na.action=na.exclude))
            
            
            model.frame <- data.frame(Model = c("Linear", "Non-Linear", "Lucas-Tooth", "Forest", "Rainforest"),
            valSlope = round(c(lm.sum$coef[2], lm2.sum$coef[2], lucas.sum$coef[2], forest.sum$coef[2], rainforest.sum$coef[2]), 2),
            R2 = round(c(lm.sum$adj.r.squared, lm2.sum$adj.r.squared, lucas.sum$adj.r.squared, forest.sum$adj.r.squared, rainforest.sum$adj.r.squared), 2),
            Score = round(c(lm.sum$adj.r.squared*lm.sum$coef[2], lm2.sum$adj.r.squared*lm2.sum$coef[2], lucas.sum$adj.r.squared*lucas.sum$coef[2], forest.sum$adj.r.squared*forest.sum$coef[2], rainforest.sum$adj.r.squared*rainforest.sum$coef[2]), 2),
            Rank = round(abs(1-c(lm.sum$adj.r.squared*lm.sum$coef[2], lm2.sum$adj.r.squared*lm2.sum$coef[2], lucas.sum$adj.r.squared*lucas.sum$coef[2], forest.sum$adj.r.squared*forest.sum$coef[2], rainforest.sum$adj.r.squared*rainforest.sum$coef[2])), 2),
            Code=c(1, 2, 3, 4, 5))
            
            
            model.frame <- model.frame %>%  arrange(Rank)
            
            #model.frame[order(model.frame, model.frame$Rank),]
            model.frame
            
        })
        
        
        bestCalType <- reactive({
            
            model.frame <- bestCalTypeFrame()
            
            model.frame <- subset(model.frame, !(model.frame$valSlope < 0.8 | model.frame$valSlope > 1.2))
            model.frame[Closest(model.frame$Score, 1, which=TRUE),6]
            
        })
        
        
        output$models <- renderDataTable({
            
            bestCalTypeFrame()
            
        })
        
        
        testing2 <- reactive({
            
            predictIntensity()
            
        })
        
        
        testing <- reactive({
            predict.frame <- predictFrame()[complete.cases(predictFrame()$Concentration),]
            cal.lm.forest <- randomForest(Concentration~., data=predict.frame, na.action=na.omit)
            
            forest.predict <- predict(cal.lm.forest, new.data=predict.frame, proximity=FALSE)
            as.data.frame(forest.predict)
            
        })
        
        output$testtable <- renderDataTable({
            
            predictFrame()
            
        })
        
        output$testtable2 <- renderDataTable({
            
            testing2()
            
        })
        
        

        
        
        calTransformationPre <- reactive({
            if(!"Transformation" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calConditions$hold[["CalTable"]][["Transformation"]]
            } else if("Transformation" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calSettings$calList[[input$calcurveelement]][[1]]$CalTable$Transformation[1]
            }
        })
        
        output$transformationui <- renderUI({
            req(input$radiocal)
            transformationUI(radiocal=input$radiocal, selection=basicTransformation())
        })
        
        calEnergyRangePre <- reactive({
            if(!"EnergyRange" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character( calConditions$hold$CalTable$EnergyRange[1]), "-")))
                
            } else if("EnergyRange" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calSettings$calList[[input$calcurveelement]][[1]]$CalTable$EnergyRange[1]), "-")))
            }
        })
        
        output$energyrangeui <- renderUI({
            req(input$radiocal)
            energyRangeUI(radiocal=input$radiocal, selection=basicEnergyRange())
        })
        
        calCompressPre <- reactive({
            if(!"Compress" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calConditions$hold[["CalTable"]][["Compress"]]
            } else if("Compress" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calSettings$calList[[input$calcurveelement]][[1]]$CalTable$Compress[1]
            }
        })
        
        output$compressui <- renderUI({
            req(input$radiocal)
            compressUI(radiocal=input$radiocal, selection=basicCompress())
        })

        
        

        
        ###Machine Learning Parameters
        
        maxSample <- reactive({
            
            if(input$radiocal==4){
                length(tableInput()[,-1])
            } else if(input$radiocal==5){
                300
            }else if(input$radiocal==6){
                length(tableInput()[,-1])
            } else if(input$radiocal==7){
                300
            } else if(input$radiocal==8){
                length(tableInput()[,-1])
            } else if(input$radiocal==9){
                300
            }
            
        })
        
        
        defaultSample <- reactive({
            
            floor(sqrt(ncol(predictIntensity())))
            
        })
        
        calForestTrySelectionpre <- reactive({
            if(!"ForestTry" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calConditions$hold[["CalTable"]]["ForestTry"]
            } else if("ForestTry" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calSettings$calList[[input$calcurveelement]][[1]]$CalTable$ForestTry[1]
            }
        })
        
        
        calForestMetricSelectionpre <- reactive({
            if(!"ForestMetric" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calConditions$hold[["CalTable"]]["ForestMetric"]
            } else if("ForestMetric" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calSettings$calList[[input$calcurveelement]][[1]]$CalTable$ForestMetric[1]
            }
        })
        
        calForestTCSelectionpre <- reactive({
            if(!"ForestTC" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calConditions$hold[["CalTable"]]["ForestTC"]
            } else if("ForestTC" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calSettings$calList[[input$calcurveelement]][[1]]$CalTable$ForestTC[1]
            }
        })
        
        
        calForestNumberSelectionpre <- reactive({
            if(!"ForestNumber" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calConditions$hold[["CalTable"]]["ForestNumber"]
            } else if("ForestNumber" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calSettings$calList[[input$calcurveelement]][[1]]$CalTable$ForestNumber[1]
            }
        })
        
        calCVRepeatsSelectionpre <- reactive({
            if(!"CVRepeats" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calConditions$hold[["CalTable"]]["CVRepeats"]
            } else if("CVRepeats" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calSettings$calList[[input$calcurveelement]][[1]]$CalTable$CVRepeats[1]
            }
        })
        
        
        calForestTreeSelectionpre <- reactive({
            if(!"ForestTrees" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calConditions$hold[["CalTable"]]["ForestTrees"]
            } else if("ForestNumber" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calSettings$calList[[input$calcurveelement]][[1]]$CalTable$ForestTrees[1]
            }
        })
        
        calHiddenLayersSelectionpre <- reactive({
            if(!"NeuralHL" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calConditions$hold[["CalTable"]]["NeuralHL"]
            } else if("NeuralHL" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calSettings$calList[[input$calcurveelement]][[1]]$CalTable$NeuralHL[1]
            }
        })
        
        calHiddenUnitsSelectionpre <- reactive({
            if(!"NeuralHU" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calConditions$hold[["CalTable"]]["NeuralHU"]), "-")))
            } else if("NeuralHU" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calSettings$calList[[input$calcurveelement]][[1]]$CalTable$NeuralHU[1]), "-")))
            }
        })
        
        calWeightDecaySelectionpre <- reactive({
            if(!"NeuralWD" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calConditions$hold[["CalTable"]]["NeuralWD"]), "-")))
            } else if("NeuralWD" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calSettings$calList[[input$calcurveelement]][[1]]$CalTable$NeuralWD[1]), "-")))
            }
        })
        
        calMaxIterationsSelectionpre <- reactive({
            if(!"NeuralMI" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calConditions$hold[["CalTable"]]["NeuralMI"]
            } else if("NeuralMI" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calSettings$calList[[input$calcurveelement]][[1]]$CalTable$NeuralMI[1]
            }
        })
        
        calTreeDepthSelectionpre <- reactive({
            if(!"TreeDepth" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calConditions$hold[["CalTable"]]["TreeDepth"]), "-")))
            } else if("TreeDepth" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calSettings$calList[[input$calcurveelement]][[1]]$CalTable$TreeDepth[1]), "-")))
            }
        })
        
        calXGBEtaSelectionpre <- reactive({
            if(!"xgbEta" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calConditions$hold[["CalTable"]]["xgbEta"]), "-")))
            } else if("xgbEta" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calSettings$calList[[input$calcurveelement]][[1]]$CalTable$xgbEta[1]), "-")))
            }
        })
        
        calXGBGammaSelectionpre <- reactive({
            if(!"xgbGamma" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calConditions$hold[["CalTable"]]["xgbGamma"]), "-")))
            } else if("xgbGamma" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calSettings$calList[[input$calcurveelement]][[1]]$CalTable$xgbGamma[1]), "-")))
            }
        })
        
        calXGBSubSampleSelectionpre <- reactive({
            if(!"xgbSubSample" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calConditions$hold[["CalTable"]]["xgbSubSample"]), "-")))
            } else if("xgbSubSample" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calSettings$calList[[input$calcurveelement]][[1]]$CalTable$xgbSubSample[1]), "-")))
            }
        })
        
        calXGBColSampleSelectionpre <- reactive({
            if(!"xgbColSample" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calConditions$hold[["CalTable"]]["xgbColSample"]), "-")))
            } else if("xgbColSample" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calSettings$calList[[input$calcurveelement]][[1]]$CalTable$xgbColSample[1]), "-")))
            }
        })
        
        calXGBMinChildSelectionpre <- reactive({
            if(!"xgbMinChild" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calConditions$hold[["CalTable"]]["xgbMinChild"]
            } else if("xgbMinChild" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calSettings$calList[[input$calcurveelement]][[1]]$CalTable$xgbMinChild[1]
            }
        })
        
        basichold <- reactiveValues()
        foresthold <- reactiveValues()
        neuralhold <- reactiveValues()
        xgboosthold <- reactiveValues()

        
        observeEvent(input$calcurveelement, {
            basichold$normtype <- calNormSelectionpre()
            basichold$normmin <- normMinPre()
            basichold$normmax <- normMaxPre()
            basichold$compress <- calCompressPre()
            basichold$transformation <- calTransformationPre()
            basichold$energyrange <- calEnergyRangePre()
            foresthold$try <- calForestTrySelectionpre()
            foresthold$metric <- calForestMetricSelectionpre()
            foresthold$train <- calForestTCSelectionpre()
            foresthold$number <- calForestNumberSelectionpre()
            foresthold$cvrepeats <- calCVRepeatsSelectionpre()
            foresthold$trees <- calForestTreeSelectionpre()
            neuralhold$hiddenlayers <- calHiddenLayersSelectionpre()
            neuralhold$hiddenunits <- calHiddenUnitsSelectionpre()
            neuralhold$weightdecay <- calWeightDecaySelectionpre()
            neuralhold$maxiterations <- calMaxIterationsSelectionpre()
            xgboosthold$treedepth <- calTreeDepthSelectionpre()
            xgboosthold$xgbeta <- calXGBEtaSelectionpre()
            xgboosthold$xgbgamma <- calXGBGammaSelectionpre()
            xgboosthold$xgbsubsample <- calXGBSubSampleSelectionpre()
            xgboosthold$xgbcolsample <- calXGBColSampleSelectionpre()
            xgboosthold$xgbminchild <- calXGBMinChildSelectionpre()
        })
        


        
        #observeEvent(input$trainSlope, {
            
            #    isolate(calhold$caltype <- bestCalType())
            
            #})
            
            
            
        basicNormType <- reactive({
            basichold$normtype
        })
        
        basicNormMin <- reactive({
            basichold$normmin
        })
        
        basicNormMax <- reactive({
            basichold$normmax
        })
        
        basicCompress <- reactive({
            basichold$compress
        })
        
        basicTransformation <- reactive({
            basichold$transformation
        })
        
        basicEnergyRange <- reactive({
            basichold$energyrange
        })
        
        forestTrySelection <- reactive({
            foresthold$try
        })
        
        forestMetricSelection <- reactive({
            foresthold$metric
        })
        
        forestTrainSelection <- reactive({
            foresthold$train
        })
        
        forestNumberSelection <- reactive({
            foresthold$number
        })
        
        cvRepeatsSelection <- reactive({
            foresthold$cvrepeats
        })
        
        forestTreeSelection <- reactive({
            foresthold$trees
        })
        
        neuralHiddenLayersSelection <- reactive({
            neuralhold$hiddenlayers
        })
        
        neuralHiddenUnitsSelection <- reactive({
            neuralhold$hiddenunits
        })
        
        neuralWeightDecaySelection <- reactive({
            neuralhold$weightdecay
        })
        
        neuralMaxIterationsSelection <- reactive({
            neuralhold$maxiterations
        })
        
        xgboostTreeDepthSelection <- reactive({
            xgboosthold$treedepth
        })
        
        xgboostEtaSelection <- reactive({
            xgboosthold$xgbeta
        })
        
        xgboostGammaSelection <- reactive({
            xgboosthold$xgbgamma
        })
        
        xgboostSubSampleSelection <- reactive({
            xgboosthold$xgbsubsample
        })
        
        xgboostColSampleSelection <- reactive({
            xgboosthold$xgbcolsample
        })
        
        xgboostMinChildSelection <- reactive({
            xgboosthold$xgbminchild
        })
        
        

        
        output$mclrunui <- renderUI({
            req(input$radiocal)
            if(input$radiocal==1){
                NULL
            } else if(input$radiocal==2){
                NULL
            } else if(input$radiocal==3){
                NULL
            } else if(input$radiocal==4){
                actionButton("mclrun", "Run Model")
            }  else if(input$radiocal==5){
                actionButton("mclrun", "Run Model")
            } else if(input$radiocal==6){
                actionButton("mclrun", "Run Model")
            } else if(input$radiocal==7){
                actionButton("mclrun", "Run Model")
            } else if(input$radiocal==8){
                actionButton("mclrun", "Run Model")
            } else if(input$radiocal==9){
                actionButton("mclrun", "Run Model")
            }
            
        })
        
        output$foresttryui <- renderUI({
            req(input$radiocal)
            if(input$radiocal==6 | input$radiocal==7){
                forestTryUI(radiocal=input$radiocal, neuralhiddenlayers=input$neuralhiddenlayers, selection=forestTrySelection(), maxsample=maxSample())
            } else {
                forestTryUI(radiocal=input$radiocal, neuralhiddenlayers=NULL, selection=forestTrySelection(), maxsample=maxSample())
            }
        })
        
        
        output$forestmetricui <- renderUI({
            req(input$radiocal)
            forestMetricUI(radiocal=input$radiocal, selection=forestMetricSelection())
        })
        
        
        output$foresttrainui <- renderUI({
            req(input$radiocal)
            forestTrainUI(radiocal=input$radiocal, selection=forestTrainSelection())
        })
        
        output$forestnumberui <- renderUI({
            req(input$radiocal, input$foresttrain)
            forestNumberUI(radiocal=input$radiocal, selection=forestNumberSelection())
        })
        
        output$cvrepeatsui <- renderUI({
            req(input$radiocal)
            cvRepeatsUI(radiocal=input$radiocal, foresttrain=input$foresttrain, selection=cvRepeatsSelection())
        })
        
        
        output$foresttreesui <- renderUI({
            req(input$radiocal)
            forestTreesUI(radiocal=input$radiocal, selection=forestTreeSelection())
        })
        
        output$neuralhiddenlayersui <- renderUI({
            req(input$radiocal)
            neuralHiddenLayersUI(radiocal=input$radiocal, selection=neuralHiddenLayersSelection())
        })
        
        output$neuralhiddenunitsui <- renderUI({
            req(input$radiocal)
            neuralHiddenUnitsUi(radiocal=input$radiocal, selection=neuralHiddenUnitsSelection())
        })
        
        output$neuralweightdecayui <- renderUI({
            req(input$radiocal)
            neuralWeightDecayUI(radiocal=input$radiocal, selection=neuralWeightDecaySelection(), neuralhiddenlayers=input$neuralhiddenlayers)
        })
        
        output$neuralmaxiterationsui <- renderUI({
            req(input$radiocal)
            neuralMaxIterationsUI(radiocal=input$radiocal, selection=neuralMaxIterationsSelection(), neuralhiddenlayers=input$neuralhiddenlayers)
        })
        
        output$treedepthui <- renderUI({
            req(input$radiocal)
            treeDepthUI(radiocal=input$radiocal, selection=xgboostTreeDepthSelection())
        })
        
        output$xgbetaui <- renderUI({
            req(input$radiocal)
            xgbEtaUI(radiocal=input$radiocal, selection=xgboostEtaSelection())
        })
        
        output$xgbgammaui <- renderUI({
            req(input$radiocal)
            xgbGammaUI(radiocal=input$radiocal, selection=xgboostGammaSelection())
        })
        
        output$xgbsubsampleui <- renderUI({
            req(input$radiocal)
            xgbSubSampleUI(radiocal=input$radiocal, selection=xgboostSubSampleSelection())
        })
        
        output$xgbcolsampleui <- renderUI({
            req(input$radiocal)
            xgbColSampleUI(radiocal=input$radiocal, selection=xgboostColSampleSelection())
        })
        
        output$xgbminchildui <- renderUI({
            req(input$radiocal)
            xgbMinChildUI(radiocal=input$radiocal, selection=xgboostMinChildSelection())
        })
        
        
        
        
        predictFramePre <- reactive({
            
            intensity <- holdFrame()$Intensity
            
            concentration <- holdFrame()$Concentration
            
            predict.frame <- data.frame(concentration, intensity)
            colnames(predict.frame) <- c("Concentration", "Intensity")
            
            
            predict.frame[complete.cases(predict.frame),]
            
            
        })
        
        
        
        predictIntensity <- reactive(label="predictIntensity",{
            req(input$radiocal)
            predict.intensity <- if (input$radiocal==1){
                linearModelSet()$data[,!colnames(linearModelSet()$data) %in% c("Spectrum", "Concentration")]
            } else if(input$radiocal==2){
                nonLinearModelSet()$data[,!colnames(nonLinearModelSet()$data) %in% c("Spectrum", "Concentration")]
            } else if(input$radiocal==3){
                lucasToothModelSet()$data[,!colnames(lucasToothModelSet()$data) %in% c("Spectrum", "Concentration")]
            } else if(input$radiocal==4){
                forestModelSet()$data[,!colnames(forestModelSet()$data) %in% c("Spectrum", "Concentration")]
            } else if(input$radiocal==5){
                rainforestModelSet()$data[,!colnames(rainforestModelSet()$data) %in% c("Spectrum", "Concentration")]
            } else if(input$radiocal==6 && input$neuralhiddenlayers==1){
                neuralNetworkIntensityShallowModelSet()$data[,!colnames(neuralNetworkIntensityShallowModelSet()$data) %in% c("Spectrum", "Concentration")]
            } else if(input$radiocal==6 && input$neuralhiddenlayers > 1){
                neuralNetworkIntensityDeepModelSet()$data[,!colnames(neuralNetworkIntensityDeepModelSet()$data) %in% c("Spectrum", "Concentration")]
            } else if(input$radiocal==7 && input$neuralhiddenlayers==1){
                neuralNetworkSpectraShallowModelSet()$data[,!colnames(neuralNetworkSpectraShallowModelSet()$data) %in% c("Spectrum", "Concentration")]
            } else if(input$radiocal==7 && input$neuralhiddenlayers > 1){
                neuralNetworkSpectraDeepModelSet()$data[,!colnames(neuralNetworkSpectraDeepModelSet()$data) %in% c("Spectrum", "Concentration")]
            } else if(input$radiocal==8){
                xgboostIntensityModelSet()$data[,!colnames(xgboostIntensityModelSet()$data) %in% c("Spectrum", "Concentration")]
            } else if(input$radiocal==9){
                xgboostSpectraModelSet()$data[,!colnames(xgboostSpectraModelSet()$data) %in% c("Spectrum", "Concentration")]
            }
            predict.intensity
            
        })
        
        
        output$testingagain <- renderDataTable({
            predictIntensity()
            
        })
        
        
        predictFrame <- reactive(label="predictFrame",{
            req(input$radiocal, input$calcurveelement)
            if (input$radiocal==1){
                linearModelSet()$data
            } else if(input$radiocal==2){
                nonLinearModelSet()$data
            } else if(input$radiocal==3){
                lucasToothModelSet()$data
            } else if(input$radiocal==4){
                forestModelSet()$data
            } else if(input$radiocal==5){
                rainforestModelSet()$data
            } else if(input$radiocal==6 && input$neuralhiddenlayers==1){
                neuralNetworkIntensityShallowModelSet()$data
            } else if(input$radiocal==6 && input$neuralhiddenlayers > 1){
                neuralNetworkIntensityDeepModelSet()$data
            } else if(input$radiocal==7 && input$neuralhiddenlayers==1){
                neuralNetworkSpectraShallowModelSet()$data
            } else if(input$radiocal==7 && input$neuralhiddenlayers > 1){
                neuralNetworkSpectraDeepModelSet()$data
            } else if(input$radiocal==8){
                xgboostIntensityModelSet()$data
            } else if(input$radiocal==9){
                xgboostSpectraModelSet()$data
            }
        })
        
        modelParameters <- reactive({
            req(input$radiocal, input$calcurveelement)
            if (input$radiocal==1){
                linearModelSet()$parameters
            } else if(input$radiocal==2){
                nonLinearModelSet()$parameters
            } else if(input$radiocal==3){
                lucasToothModelSet()$parameters
            } else if(input$radiocal==4){
                forestModelSet()$parameters
            } else if(input$radiocal==5){
                rainforestModelSet()$parameters
            } else if(input$radiocal==6 && input$neuralhiddenlayers==1){
                neuralNetworkIntensityShallowModelSet()$parameters
            } else if(input$radiocal==6 && input$neuralhiddenlayers > 1){
                neuralNetworkIntensityDeepModelSet()$parameters
            } else if(input$radiocal==7 && input$neuralhiddenlayers==1){
                neuralNetworkSpectraShallowModelSet()$parameters
            } else if(input$radiocal==7 && input$neuralhiddenlayers > 1){
                neuralNetworkSpectraDeepModelSet()$parameters
            } else if(input$radiocal==8){
                xgboostIntensityModelSet()$parameters
            } else if(input$radiocal==9){
                xgboostSpectraModelSet()$parameters
            }
        })
        
        
        predictFrameName <- reactive({
            
            predict.frame <- predictFrame()[ vals$keeprows, , drop = FALSE]
            spectra.line.table <- spectraLineTable()[ vals$keeprows, , drop = FALSE]
            
            predict.frame.name <- data.frame(spectra.line.table$Spectrum, predict.frame)
            colnames(predict.frame.name) <- c("Spectrum", names(predict.frame))
            predict.frame.name
            
        })
        
        
        
        calCurveFrame <- reactive({
            
            predictFrame()
            
        })
        
        
        elementModelGen <- reactive(label="elementModelGen",{
            req(input$radiocal, input$calcurveelement)
            if(input$radiocal==1){
                linearModel()
            } else if(input$radiocal==2){
                nonLinearModel()
            } else if(input$radiocal==3){
                lucasToothModel()
            } else if(input$radiocal==4){
                forestModel()
            } else if(input$radiocal==5){
                rainforestModel()
            } else if(input$radiocal==6){
                neuralNetworkIntensityModel()
            } else if(input$radiocal==7){
                neuralNetworkSpectraModel()
            } else if(input$radiocal==8){
                xgboostIntensityModel()
            } else if(input$radiocal==9){
                xgboostSpectraModel()
            }
        })
        
        #observeEvent(input$calcurveelement, priority=101, {
        #    if(!input$calcurveelement %in% names(calMemory$Calibration$calList)){
        #       calMemory$Calibration$calList[[input$calcurveelement]] <- list(lucasToothModelSet()$parameters, lucasToothModel())

        #   }
        #})
        
        observeEvent(input$createcalelement, priority=100, {
            calMemory$Calibration$calList[[input$calcurveelement]] <- NULL
                calMemory$Calibration$calList[[input$calcurveelement]] <- isolate(list(modelParameters(), elementModelGen()))
        })
        
        output$usecalsep <- renderUI({
            if(!is.null(calMemory$Calibration)){
                tags$hr()
            } else if(is.null(calMemory$Calibration)){
                NULL
            }
        })
        

        
        elementModel <- reactive({
            if(isMCL()==FALSE){
                elementModelGen()
            } else if(isMCL()==TRUE){
                calMemory$Calibration$calList[[input$calcurveelement]][[2]]
            }
        })


        valFrame <- reactive(label="valFrame",{
            req(input$radiocal)
            
            if (input$radiocal==1){

                cal.est.conc.pred <- predict(object=elementModelGen(), newdata=predictIntensity(), interval='confidence')
                cal.est.conc.tab <- data.frame(cal.est.conc.pred)
                cal.est.conc <- cal.est.conc.tab$fit
                
                val.frame <- data.frame(na.omit(predictFrame()$Concentration), cal.est.conc)
                colnames(val.frame) <- c("Concentration", "Prediction")
            }
            
            if (input$radiocal==2){
                
                cal.est.conc.pred <- predict(object=elementModelGen(), newdata=predictIntensity(), interval='confidence')
                cal.est.conc.tab <- data.frame(cal.est.conc.pred)
                cal.est.conc <- cal.est.conc.tab$fit
                
                val.frame <- data.frame(na.omit(predictFrame()$Concentration), cal.est.conc)
                colnames(val.frame) <- c("Concentration", "Prediction")
            }
            
            if (input$radiocal==3){
                

                cal.est.conc.pred.luc <- predict(object=elementModelGen() , newdata=predictIntensity(), interval='confidence')
                cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
                cal.est.conc.luc <- cal.est.conc.tab$fit
                cal.est.conc.luc.up <- cal.est.conc.tab$upr
                cal.est.conc.luc.low <- cal.est.conc.tab$lwr
                
                
                val.frame <- data.frame(predictFrame()$Concentration, predictIntensity()$Intensity, cal.est.conc.luc, cal.est.conc.luc, cal.est.conc.luc.up, cal.est.conc.luc.low)
                colnames(val.frame) <- c("Concentration", "IntensityOrg", "Intensity", "Prediction", "Upper", "Lower")
            }
            
            if (input$radiocal==4){
                
                val.frame <- tryCatch(mclValGen(model=elementModel(), data=predictIntensity(), predict.frame=predictFrame()), error=function(e) NULL)
            }
            
            
            if (input$radiocal==5){
                
                val.frame <- tryCatch(mclValGen(model=elementModel(), data=predictIntensity(), predict.frame=predictFrame()), error=function(e) NULL)
            }
            
            
            if (input$radiocal==6){
                
                val.frame <- tryCatch(mclValGen(model=elementModel(), data=predictIntensity(), predict.frame=predictFrame()), error=function(e) NULL)
            }
            
            
            if (input$radiocal==7){
                
                val.frame <- tryCatch(mclValGen(model=elementModel(), data=predictIntensity(), predict.frame=predictFrame()), error=function(e) NULL)
            }
            
            if (input$radiocal==8){
                
                
                val.frame <- tryCatch(mclValGen(model=elementModel(), data=predictIntensity(), predict.frame=predictFrame()), error=function(e) NULL)
            }
            
            
            if (input$radiocal==9){
                
            val.frame <- tryCatch(mclValGen(model=elementModel(), data=predictIntensity(), predict.frame=predictFrame()), error=function(e) NULL)
            
            }
            
            
            
            if(is.null(val.frame)){
                data.frame(Concentration=predictFrame()$Concentration, Intensity=rep(0, length(predictFrame()$Concentration), Prediction=rep(0, length(predictFrame()$Concentration))), stringsAsFactors=FALSE)
            } else if(!is.null(val.frame)){
                val.frame
            }
            
        })
        
        valFrameVal <- reactiveValues()
        valFrameVal$val.frame <- NULL
        
        observeEvent(input$calcurveelement, priority=77, {
            valFrameVal$val.frame <- valFrame()
        })

        observeEvent(input$createcalelement, priority=97, {
            valFrameVal$val.frame <- valFrame()
        })
        
        
        
        calValFrame <- reactive({
            
            valFrame()
            
        })
        
        
        calType <- reactive(label="calType",{
            req(input$radiocal)
            if(input$radiocal==1){
                1
            } else if(input$radiocal==2){
                2
            } else if(input$radiocal==3){
                3
            } else if(input$radiocal==4){
                3
            } else if(input$radiocal==5){
                5
            } else if(input$radiocal==6){
                3
            } else if(input$radiocal==7){
                5
            } else if(input$radiocal==8){
                3
            } else if(input$radiocal==9){
                5
            }
            
        })
        
    
        
        rangescalcurve <- reactiveValues(x = NULL, y = NULL)
        
        
        
        calCurvePlotPre <- reactive(label="calCurvePlotPre",{
            req(input$radiocal)
            element.name <- if(input$calcurveelement %in% spectralLines){
                gsub("[.]", "", substr(input$calcurveelement, 1, 2))
            } else {
                input$calcurveelement
            }
            
            intens <- " Counts per Second"
            norma <- " Normalized"
            norma.comp <- " Compton Normalized"
            norma.tc <- " Valid Counts Normalized"
            conen <- paste0(" ", input$plotunit)
            predi <- paste0(" Estimate ", input$plotunit)
            log <- "Log "
            
            
            intensity.name <- c(element.name, intens)
            concentration.name <- c(element.name, conen)
            prediction.name <- c(element.name, predi)
            
            

            
            
            if(input$radiocal==1){
                calcurve.plot <- if(input$loglinear=="Linear"){
                    tryCatch(ggplot(data=predictFrame()[ vals$keeprows, , drop = FALSE], aes(Intensity, Concentration)) +
                    theme_light() +
                    annotate("text", label=lm_eqn(lm(Concentration~Intensity, predictFrame()[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                    stat_smooth(method="lm", fullrange = TRUE) +
                    geom_point() +
                    geom_point(data = predictFrame()[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
                    scale_x_continuous(paste(element.name, intens), breaks=scales::pretty_breaks()) +
                    scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                    coord_cartesian(xlim = rangescalcurve$x, ylim = rangescalcurve$y, expand = TRUE), error=function(e) NULL)
                } else if(input$loglinear=="Log"){
                    tryCatch(ggplot(data=predictFrame()[ vals$keeprows, , drop = FALSE], aes(Intensity, Concentration)) +
                    theme_light() +
                    annotate("text", label=lm_eqn(lm(Concentration~Intensity, predictFrame()[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                    stat_smooth(method="lm", fullrange = TRUE) +
                    geom_point() +
                    geom_point(data = predictFrame()[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
                    scale_x_log10(paste("Log ", element.name, intens), breaks=scales::pretty_breaks()) +
                    scale_y_log10(paste("Log ", element.name, conen), breaks=scales::pretty_breaks()) +
                    coord_cartesian(xlim = rangescalcurve$x, ylim = rangescalcurve$y, expand = TRUE), error=function(e) NULL)
                }
            }
            
            if(input$radiocal==2){
                
                calcurve.plot <- if(input$loglinear=="Linear"){
                    tryCatch(ggplot(data=predictFrame()[ vals$keeprows, , drop = FALSE], aes(Intensity, Concentration)) +
                    theme_light() +
                    annotate("text", label=lm_eqn(lm(Concentration~Intensity, predictFrame()[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                    stat_smooth(method="lm", fullrange = TRUE) +
                    geom_point() +
                    geom_point(data = predictFrame()[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
                    scale_x_continuous(paste(element.name, intens), breaks=scales::pretty_breaks()) +
                    scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                    coord_cartesian(xlim = rangescalcurve$x, ylim = rangescalcurve$y, expand = TRUE), error=function(e) NULL)
                } else if(input$loglinear=="Log"){
                    tryCatch(ggplot(data=predictFrame()[ vals$keeprows, , drop = FALSE], aes(Intensity, Concentration)) +
                    theme_light() +
                    annotate("text", label=lm_eqn(lm(Concentration~Intensity, predictFrame()[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                    stat_smooth(method="lm", fullrange = TRUE) +
                    geom_point() +
                    geom_point(data = predictFrame()[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
                    scale_x_log10(paste("Log ", element.name, intens), breaks=scales::pretty_breaks()) +
                    scale_y_log10(paste("Log ", element.name, conen), breaks=scales::pretty_breaks()) +
                    coord_cartesian(xlim = rangescalcurve$x, ylim = rangescalcurve$y, expand = TRUE), error=function(e) NULL)
                }
            }
            
            if(input$radiocal==3){
                
                calcurve.plot <- if(input$loglinear=="Linear"){
                    tryCatch(ggplot(data=valFrame()[ vals$keeprows, , drop = FALSE], aes(Intensity, Concentration)) +
                    theme_light() +
                    annotate("text", label=lm_eqn(lm(Concentration~., valFrame()[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                    geom_smooth(aes(x=Intensity, y=Concentration, ymin = Lower, ymax = Upper)) +
                    geom_point() +
                    geom_point(aes(Intensity, Concentration), data = valFrame()[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
                    scale_x_continuous(paste(element.name, norma), breaks=scales::pretty_breaks()) +
                    scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                    coord_cartesian(xlim = rangescalcurve$x, ylim = rangescalcurve$y, expand = TRUE), error=function(e) NULL)
                } else if(input$loglinear=="Log"){
                    tryCatch(ggplot(data=valFrame()[ vals$keeprows, , drop = FALSE], aes(Intensity, Concentration)) +
                    theme_light() +
                    annotate("text", label=lm_eqn(lm(Concentration~., valFrame()[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                    geom_smooth(aes(x=Intensity, y=Concentration, ymin = Lower, ymax = Upper)) +
                    geom_point() +
                    geom_point(aes(Intensity, Concentration), data = valFrame()[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
                    scale_x_log10(paste("Log ", element.name, intens), breaks=scales::pretty_breaks()) +
                    scale_y_log10(paste("Log ", element.name, conen), breaks=scales::pretty_breaks()) +
                    coord_cartesian(xlim = rangescalcurve$x, ylim = rangescalcurve$y, expand = TRUE), error=function(e) NULL)
                }
            }
            
            if(input$radiocal==4){
                calcurve.plot <- if(input$loglinear=="Linear"){
                    tryCatch(ggplot(data=valFrameVal$val.frame[ vals$keeprows, , drop = FALSE], aes(Intensity, Concentration)) +
                    theme_light() +
                    annotate("text", label=lm_eqn(lm(Concentration~., valFrameVal$val.frame[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                    geom_smooth() +
                    geom_point() +
                    geom_point(aes(Intensity, Concentration), data = valFrameVal$val.frame[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
                    scale_x_continuous(paste(element.name, norma), breaks=scales::pretty_breaks()) +
                    scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                    coord_cartesian(xlim = rangescalcurve$x, ylim = rangescalcurve$y, expand = TRUE), error=function(e) NULL)
                } else if(input$loglinear=="Log"){
                    tryCatch(ggplot(data=valFrameVal$val.frame[ vals$keeprows, , drop = FALSE], aes(Intensity, Concentration)) +
                    theme_light() +
                    annotate("text", label=lm_eqn(lm(Concentration~., valFrameVal$val.frame[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                    geom_smooth() +
                    geom_point() +
                    geom_point(aes(Intensity, Concentration), data = valFrameVal$val.frame[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
                    scale_x_log10(paste("Log ", element.name, intens), breaks=scales::pretty_breaks()) +
                    scale_y_log10(paste("Log ", element.name, conen), breaks=scales::pretty_breaks()) +
                    coord_cartesian(xlim = rangescalcurve$x, ylim = rangescalcurve$y, expand = TRUE), error=function(e) NULL)
                }
            }
            
            if(input$radiocal==5){
                
                calcurve.plot <- if(input$loglinear=="Linear"){
                    tryCatch(ggplot(data=valFrameVal$val.frame[ vals$keeprows, , drop = FALSE], aes(Intensity, Concentration)) +
                    theme_light() +
                    annotate("text", label=lm_eqn(lm(Concentration~., valFrameVal$val.frame[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                    geom_smooth() +
                    geom_point() +
                    geom_point(aes(Intensity, Concentration), data = valFrameVal$val.frame[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
                    scale_x_continuous(paste(element.name, norma), breaks=scales::pretty_breaks()) +
                    scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                    coord_cartesian(xlim = rangescalcurve$x, ylim = rangescalcurve$y, expand = TRUE), error=function(e) NULL)
                } else if(input$loglinear=="Log"){
                    tryCatch(ggplot(data=valFrameVal$val.frame[ vals$keeprows, , drop = FALSE], aes(Intensity, Concentration)) +
                    theme_light() +
                    annotate("text", label=lm_eqn(lm(Concentration~., valFrameVal$val.frame[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                    geom_smooth() +
                    geom_point() +
                    geom_point(aes(Intensity, Concentration), data = valFrameVal$val.frame[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
                    scale_x_log10(paste("Log ", element.name, intens), breaks=scales::pretty_breaks()) +
                    scale_y_log10(paste("Log ", element.name, conen), breaks=scales::pretty_breaks()) +
                    coord_cartesian(xlim = rangescalcurve$x, ylim = rangescalcurve$y, expand = TRUE), error=function(e) NULL)
                }
            }
            
            if(input$radiocal==6){
                calcurve.plot <- tryCatch(print(grobTree(plot.nnet(elementModel(),nid=T))), error=function(e) NULL)
            }
            
            if(input$radiocal==7){
                calcurve.plot <- tryCatch(print(grobTree(plot.nnet(elementModel(),nid=T))), error=function(e) NULL)
            }
            
            if(input$radiocal==8){
                
                calcurve.plot <- if(input$loglinear=="Linear"){
                    tryCatch(ggplot(data=valFrameVal$val.frame[ vals$keeprows, , drop = FALSE], aes(Intensity, Concentration)) +
                    theme_light() +
                    annotate("text", label=lm_eqn(lm(Concentration~., valFrameVal$val.frame[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                    geom_smooth() +
                    geom_point() +
                    geom_point(aes(Intensity, Concentration), data = valFrameVal$val.frame[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
                    scale_x_continuous(paste(element.name, norma), breaks=scales::pretty_breaks()) +
                    scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                    coord_cartesian(xlim = rangescalcurve$x, ylim = rangescalcurve$y, expand = TRUE), error=function(e) NULL)
                } else if(input$loglinear=="Log"){
                    tryCatch(ggplot(data=valFrameVal$val.frame[ vals$keeprows, , drop = FALSE], aes(Intensity, Concentration)) +
                    theme_light() +
                    annotate("text", label=lm_eqn(lm(Concentration~., valFrameVal$val.frame[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                    geom_smooth() +
                    geom_point() +
                    geom_point(aes(Intensity, Concentration), data = valFrameVal$val.frame[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
                    scale_x_log10(paste("Log ", element.name, intens), breaks=scales::pretty_breaks()) +
                    scale_y_log10(paste("Log ", element.name, conen), breaks=scales::pretty_breaks()) +
                    coord_cartesian(xlim = rangescalcurve$x, ylim = rangescalcurve$y, expand = TRUE), error=function(e) NULL)
                }
            }
            
            if(input$radiocal==9){
                
                calcurve.plot <- if(input$loglinear=="Linear"){
                    tryCatch(ggplot(data=valFrameVal$val.frame[ vals$keeprows, , drop = FALSE], aes(Intensity, Concentration)) +
                    theme_light() +
                    annotate("text", label=lm_eqn(lm(Concentration~., valFrameVal$val.frame[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                    geom_smooth() +
                    geom_point() +
                    geom_point(aes(Intensity, Concentration), data = valFrameVal$val.frame[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
                    scale_x_continuous(paste(element.name, norma), breaks=scales::pretty_breaks()) +
                    scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                    coord_cartesian(xlim = rangescalcurve$x, ylim = rangescalcurve$y, expand = TRUE), error=function(e) NULL)
                } else if(input$loglinear=="Log"){
                    tryCatch(ggplot(data=valFrameVal$val.frame[ vals$keeprows, , drop = FALSE], aes(Intensity, Concentration)) +
                    theme_light() +
                    annotate("text", label=lm_eqn(lm(Concentration~., valFrameVal$val.frame[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                    geom_smooth() +
                    geom_point() +
                    geom_point(aes(Intensity, Concentration), data = valFrameVal$val.frame[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
                    scale_x_log10(paste("Log ", element.name, intens), breaks=scales::pretty_breaks()) +
                    scale_y_log10(paste("Log ", element.name, conen), breaks=scales::pretty_breaks()) +
                    coord_cartesian(xlim = rangescalcurve$x, ylim = rangescalcurve$y, expand = TRUE), error=function(e) NULL)
                }
            }
            
            
            
            calcurve.plot
            
            
        })
        
        emptyCalCurve <- reactive(label="emptyCalCurve",{
            
            element.name <- if(input$calcurveelement %in% spectralLines){
                gsub("[.]", "", substr(input$calcurveelement, 1, 2))
            } else {
                input$calcurveelement
            }
            
            intens <- " Counts per Second"
            norma <- " Normalized"
            norma.comp <- " Compton Normalized"
            norma.tc <- " Valid Counts Normalized"
            conen <- paste0(" ", input$plotunit)
            predi <- paste0(" Estimate ", input$plotunit)
            log <- "Log "
            
            
            intensity.name <- c(element.name, intens)
            concentration.name <- c(element.name, conen)
            prediction.name <- c(element.name, predi)
            
            empty.plot <- if(input$loglinear=="Linear"){
                ggplot() +
                theme_light() +
                text(label="Click Run Model to Process", aes(x=0, y=50),  size=10) +
                scale_x_continuous(paste(element.name, predi), limits=c(0, 100), breaks=scales::pretty_breaks()) +
                scale_y_continuous(paste(element.name, conen), limits=c(0, 100), breaks=scales::pretty_breaks())
            } else if(input$loglinear=="Log"){
                ggplot() +
                theme_light() +
                text(label="Click Run Model to Process", aes(x=0, y=50),  size=10) +
                scale_x_log10(paste("Log ", element.name, predi), limits=c(0, 100), breaks=scales::pretty_breaks()) +
                scale_y_log10(paste("Log ", element.name, conen), limits=c(0, 100), breaks=scales::pretty_breaks())
            }
            
            empty.plot
        })
        
        processingCalCurve <- reactive(label="processingCalCurve",{
            
            element.name <- if(input$calcurveelement %in% spectralLines){
                gsub("[.]", "", substr(input$calcurveelement, 1, 2))
            } else {
                input$calcurveelement
            }
            
            intens <- " Counts per Second"
            norma <- " Normalized"
            norma.comp <- " Compton Normalized"
            norma.tc <- " Valid Counts Normalized"
            conen <- paste0(" ", input$plotunit)
            predi <- paste0(" Estimate ", input$plotunit)
            log <- "Log "
            
            
            intensity.name <- c(element.name, intens)
            concentration.name <- c(element.name, conen)
            prediction.name <- c(element.name, predi)
            
            
            empty.plot <- if(input$loglinear=="Linear"){
                ggplot() +
                theme_light() +
                text(label="Click Run Model to Process", aes(x=0, y=50),  size=10) +
                scale_x_continuous(paste(element.name, predi), limits=c(0, 100), breaks=scales::pretty_breaks()) +
                scale_y_continuous(paste(element.name, conen), limits=c(0, 100), breaks=scales::pretty_breaks())
            } else if(input$loglinear=="Log"){
                ggplot() +
                theme_light() +
                text(label="Click Run Model to Process", aes(x=0, y=50),  size=10) +
                scale_x_log10(paste("Log ", element.name, predi), limits=c(0, 100), breaks=scales::pretty_breaks()) +
                scale_y_log10(paste("Log ", element.name, conen), limits=c(0, 100), breaks=scales::pretty_breaks())
            }
            
            empty.plot
        })
        
        calCurvePlot <- reactive(label="calCurvePlot",{
            if(isMCL()==FALSE){
                calCurvePlotPre()
            } else if(isMCL()==TRUE){
                if(is.null(valFrameVal$val.frame)){
                    emptyCalCurve()
                } else if(!is.null(valFrameVal$val.frame)){
                    calCurvePlotPre()
                }
            }
            
        })
        
        observeEvent(input$cropcal, {
            brush <- input$plot_cal_brush
            if (!is.null(brush)) {
                rangescalcurve$x <- c(brush$xmin, brush$xmax)
                rangescalcurve$y <- c(brush$ymin, brush$ymax)
                
            } else {
                rangescalcurve$x <- NULL
                rangescalcurve$y <- NULL
            }
        })
        
        observeEvent(input$zerocal, {
            predict.frame <- if(calType()==1){
                predictFrame()
            } else if(calType()==2){
                predictFrame()
            } else if(calType()==3) {
                valFrame()
            } else if(calType()==5) {
                valFrame()
            }

                rangescalcurve$x <- c(0, max(predict.frame$Intensity))
                rangescalcurve$y <- c(0, max(predict.frame$Concentration))
        })
        
        reac <- reactive(list(bins = input$bins, column  = input$column))
        
        output$calcurveplots <- renderPlot({
            calCurvePlot()
        })
        
        
        rangesvalcurve <- reactiveValues(x = NULL, y = NULL)
        
        
        valCurvePlotPre <- reactive(label="valCurvePlotPre",{

            
            element.name <- if(input$calcurveelement %in% spectralLines){
                gsub("[.]", "", substr(input$calcurveelement, 1, 2))
            } else {
                input$calcurveelement
            }
            intens <- " Counts per Second"
            norma <- " Normalized"
            norma.comp <- " Compton Normalized"
            norma.tc <- " Valid Counts Normalized"
            conen <- paste0(" ", input$plotunit)
            predi <- paste0(" Estimate ", input$plotunit)
            log <- "Log "
            
            intensity.name <- c(element.name, intens)
            concentration.name <- c(element.name, conen)
            prediction.name <- c(element.name, predi)
            
            
            valcurve.plot <- if(input$loglinear=="Linear"){
                tryCatch(ggplot(data=valFrame()[ vals$keeprows, , drop = FALSE], aes(Prediction, Concentration)) +
                theme_light() +
                annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, valFrame()[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                geom_abline(intercept=0, slope=1, lty=2) +
                stat_smooth(method="lm") +
                geom_point() +
                geom_point(aes(Prediction, Concentration),  data = valFrame()[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
                scale_x_continuous(paste(element.name, predi), breaks=scales::pretty_breaks()) +
                scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                coord_cartesian(xlim = rangesvalcurve$x, ylim = rangesvalcurve$y, expand = TRUE), error=function(e) NULL)
            } else if(input$loglinear=="Log"){
                tryCatch(ggplot(data=valFrame()[ vals$keeprows, , drop = FALSE], aes(Prediction, Concentration)) +
                theme_light() +
                annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, valFrame()[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                geom_abline(intercept=0, slope=1, lty=2) +
                stat_smooth(method="lm") +
                geom_point() +
                geom_point(aes(Prediction, Concentration),  data = valFrame()[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
                scale_x_log10(paste("Log ", element.name, predi), breaks=scales::pretty_breaks()) +
                scale_y_log10(paste("Log ", element.name, conen), breaks=scales::pretty_breaks()) +
                coord_cartesian(xlim = rangesvalcurve$x, ylim = rangesvalcurve$y, expand = TRUE), error=function(e) NULL)
            }

            valcurve.plot
            
        })
        
        emptyValCurve <- reactive(label="emptyValCurve",{
            
            element.name <- if(input$calcurveelement %in% spectralLines){
                gsub("[.]", "", substr(input$calcurveelement, 1, 2))
            } else {
                input$calcurveelement
            }
            intens <- " Counts per Second"
            norma <- " Normalized"
            norma.comp <- " Compton Normalized"
            norma.tc <- " Valid Counts Normalized"
            conen <- paste0(" ", input$plotunit)
            predi <- paste0(" Estimate ", input$plotunit)
            log <- "Log "
            
            intensity.name <- c(element.name, intens)
            concentration.name <- c(element.name, conen)
            prediction.name <- c(element.name, predi)
            
            empty.plot <- if(input$loglinear=="Linear"){
                ggplot() +
                theme_light() +
                text(label="Click Run Model to Process", aes(x=0, y=50),  size=10) +
                geom_abline(intercept=0, slope=1, lty=2) +
                scale_x_continuous(paste(element.name, predi), limits=c(0, 100), breaks=scales::pretty_breaks()) +
                scale_y_continuous(paste(element.name, conen), limits=c(0, 100), breaks=scales::pretty_breaks())
            } else if(input$loglinear=="Log"){
                ggplot() +
                theme_light() +
                text(label="Click Run Model to Process", aes(x=0, y=50),  size=10) +
                geom_abline(intercept=0, slope=1, lty=2) +
                scale_x_log10(paste("Log ", element.name, predi), limits=c(0, 100), breaks=scales::pretty_breaks()) +
                scale_y_log10(paste("Log ", element.name, conen), limits=c(0, 100), breaks=scales::pretty_breaks())
            }
            
            empty.plot
        })
        
        processingValCurve <- reactive(label="processingValCurve",{
            
            element.name <- if(input$calcurveelement %in% spectralLines){
                gsub("[.]", "", substr(input$calcurveelement, 1, 2))
            } else {
                input$calcurveelement
            }
            intens <- " Counts per Second"
            norma <- " Normalized"
            norma.comp <- " Compton Normalized"
            norma.tc <- " Valid Counts Normalized"
            conen <- paste0(" ", input$plotunit)
            predi <- paste0(" Estimate ", input$plotunit)
            log <- "Log "
            
            intensity.name <- c(element.name, intens)
            concentration.name <- c(element.name, conen)
            prediction.name <- c(element.name, predi)
            
            empty.plot <- if(input$loglinear=="Linear"){
                ggplot() +
                theme_light() +
                text(label="Click Run Model to Process", aes(x=0, y=50),  size=10) +
                geom_abline(intercept=0, slope=1, lty=2) +
                scale_x_continuous(paste(element.name, predi), limits=c(0, 100), breaks=scales::pretty_breaks()) +
                scale_y_continuous(paste(element.name, conen), limits=c(0, 100), breaks=scales::pretty_breaks())
            } else if(input$loglinear=="Log"){
                ggplot() +
                theme_light() +
                text(label="Click Run Model to Process", aes(x=0, y=50),  size=10) +
                geom_abline(intercept=0, slope=1, lty=2) +
                scale_x_log10(paste("Log ", element.name, predi), limits=c(0, 100), breaks=scales::pretty_breaks()) +
                scale_y_log10(paste("Log ", element.name, conen), limits=c(0, 100), breaks=scales::pretty_breaks())
            }
            
            empty.plot
        })
        
        valCurvePlot <- reactive(label="valCurvePlot",{
            if(isMCL()==FALSE){
                valCurvePlotPre()
            } else if(isMCL()==TRUE){
                if(is.null(valFrameVal$val.frame)){
                    emptyValCurve()
                } else if(!is.null(valFrameVal$val.frame)){
                    valCurvePlotPre()
                }
            }
            
        })
        
        observeEvent(input$cropval, {
            brush <- input$plot_val_brush
            if (!is.null(brush)) {
                rangesvalcurve$x <- c(brush$xmin, brush$xmax)
                rangesvalcurve$y <- c(brush$ymin, brush$ymax)
                
            } else {
                rangesvalcurve$x <- NULL
                rangesvalcurve$y <- NULL
            }
        })
        
        observeEvent(input$zeroval, {
            predict.frame <- valFrame()
            
            rangesvalcurve$x <- c(0, max(predict.frame$Prediction))
            rangesvalcurve$y <- c(0, max(predict.frame$Concentration))
        })
        
        
        output$valcurveplots <- renderPlot({
            valCurvePlot()
            
        })
        
        
        calPlotDownload <- reactive({
            
            tryCatch(grid.arrange(calCurvePlot(), valCurvePlot(), ncol=2), error=function(e) NULL)
            
        })

        
        plotDimensions <- reactive({
            
            if(input$imagesize=="Small"){
                c(14, 4)
            } else if(input$imagesize=="Large"){
                c(20, 8)
            }
            
        })
        
        
        output$downloadcloudplot <- downloadHandler(
        filename = function() { paste(paste(c(input$calname, "_", input$calcurveelement), collapse=''), '.tiff',  sep='') },
        content = function(file) {
            ggsave(file,calPlotDownload(), device="tiff", compression="lzw",  dpi=300, width=plotDimensions()[1], height=plotDimensions()[2])
        }
        )
        
        
        calValTable <- reactive(label="calValTable",{
            
            standard.table <- valFrame()
            hold.frame <- holdFrame()
            
            standard.table.summary <- data.frame(hold.frame$Spectrum, as.numeric(as.character(standard.table$Concentration)), as.numeric(as.character(standard.table$Prediction)), as.numeric(as.character(standard.table$Concentration-standard.table$Prediction)), ((standard.table$Concentration-standard.table$Prediction)/standard.table$Concentration), stringsAsFactors=FALSE)
            colnames(standard.table.summary) <- c("Standard", "Concentration", "Prediction", "Difference", "Relative")
            
            standard.table.summary[,-1] <-round(standard.table.summary[,-1],4)
            standard.table.summary[,5] <- as.character(percent(standard.table.summary[,5]))
            
            this.table <- standard.table.summary
            this.table
            
        })
        
        
        output$standardsperformance <- DT::renderDataTable({
            
            
            standard.table <- calValTable()
            standard.table
            
        }, options =list(aoColumnDefs = list(list(sClass="alignRight",aTargets=c(list(2), list(3),list(4),list(5))))  ))
        
        
        randomizeData <- reactive(label="randomizeData",{
            
            cal.frame <- holdFrame()[complete.cases(holdFrame()[,"Concentration"]),]
            cal.frame <- cal.frame[ vals$keeprows, , drop = FALSE]
            total.number <- length(cal.frame[,1])
            sample.number <- total.number-round(input$percentrandom*total.number, 0)
            
            hold <- cal.frame[sample(nrow(cal.frame), sample.number),]
            cal.frame$Spectrum %in% hold$Spectrum
            
        })
        
        
        
        calCurveFrameRandomized <- reactive(label="calCurveFrameRandomized",{
            
            predict.frame <- predictFrame()
            predict.frame <- predict.frame[ vals$keeprows, , drop = FALSE]
            
            predict.frame[randomizeData(),]
            
        })
        
        
        linearModelRandom <- reactive(label="linearModelRandom",{
            predict.frame <- calCurveFrameRandomized()
            cal.lm <- lm(Concentration~Intensity, data=predict.frame)
            cal.lm
        })
        
        nonLinearModelRandom <- reactive(label="nonLinearModelRandom",{
            predict.frame <- calCurveFrameRandomized()
            cal.lm <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame)
            cal.lm
        })
        
        lucasToothModelRandom <- reactive(label="lucasToothModelRandom",{
            predict.frame <- calCurveFrameRandomized()
            cal.lm <- lm(Concentration~., data=predict.frame)
            cal.lm
        })
        
        forestModelRandom <- reactive(label="forestModelRandom",{
            
            predict.frame <- forestModelSet()$data[randomizeData(),]
            parameters <- forestModelSet()$parameters$CalTable
            
            rf.grid <- expand.grid(.mtry=parameters$ForestTry)
            
            
            cl <- if(get_os()=="windows"){
                parallel::makePSOCKcluster(as.numeric(my.cores))
            } else if(get_os()!="windows"){
                parallel::makeForkCluster(as.numeric(my.cores))
            }
            registerDoParallel(cl)
            
            rf_model<-caret::train(Concentration~.,data=predict.frame,method="rf", type="Regression",
            trControl=trainControl(method=parameters$ForestTC, number=parameters$ForestNumber), ntree=parameters$ForestTrees,
            prox=TRUE,allowParallel=TRUE, importance=TRUE, metric=parameters$ForestMetric, tuneGrid=rf.grid, na.action=na.omit, trim=TRUE)
            
            stopCluster(cl)
            rf_model
            
        })
        
        rainforestModelRandom <- reactive(label="rainforestModelRandom",{
            
            data <- rainforestModelSet$list()[randomizeData(),]
            parameters <- rainforestModelSet()$parameters$CalTable
            
            rf.grid <- expand.grid(.mtry=parameters$ForestTry)
            
            
            cl <- if(get_os()=="windows"){
                parallel::makePSOCKcluster(as.numeric(my.cores))
            } else if(get_os()!="windows"){
                parallel::makeForkCluster(as.numeric(my.cores))
            }
            registerDoParallel(cl)
            
            rf_model<-caret::train(Concentration~.,data=data, method="rf", type="Regression",
            trControl=trainControl(method=parameters$ForestTC, number=parameters$ForestNumber), ntree=parameters$ForestTrees,
            prox=TRUE,allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=rf.grid, na.action=na.omit, importance=TRUE, trim=TRUE)
            
            
            stopCluster(cl)
            rf_model
            
        })
        
        neuralNetworkIntensityShallowRandom <- reactive(label="neuralNetworkIntensityShallowRandom",{
            
            predict.frame <- neuralNetworkIntensityShallowModelSet()$data[randomizeData(),]
            parameters <- neuralNetworkIntensityShallowModelSet()$parameters$CalTable
            
            
            weightdecay.vec <- as.numeric(unlist(strsplit(as.character(parameters$NeuralWD), "-")))
            hiddenunits.vec <- as.numeric(unlist(strsplit(as.character(parameters$NeuralHU), "-")))
            
            
            nn.grid <- expand.grid(
            .decay = seq(weightdecay.vec[1], weightdecay.vec[2], 0.1),
            .size = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1))
            
            cl <- if(get_os()=="windows"){
                parallel::makePSOCKcluster(as.numeric(my.cores))
            } else if(get_os()!="windows"){
                parallel::makeForkCluster(as.numeric(my.cores))
            }
            registerDoParallel(cl)
            
            nn_model<-caret::train(Concentration~.,data=predict.frame, method="nnet", linout=TRUE, trControl=trainControl(method=parameters$ForestTC, number=parameters$ForestNumber), allowParallel=TRUE, metric=parameters$ForestMetric, na.action=na.omit, importance=TRUE, tuneGrid=nn.grid, maxit=parameters$NeuralMI, trace=F, trim=TRUE)
            
            
            stopCluster(cl)
            nn_model
            
        })
        
        neuralNetworkIntensityDeepRandom <- reactive(label="neuralNetworkIntensityDeepRandom",{
            
            predict.frame <- neuralNetworkIntensityDeepModelSet()$data[randomizeData(),]
            parameters <- neuralNetworkIntensityDeepModelSet()$parameters$CalTable
            
            
            hiddenunits.vec <- as.numeric(unlist(strsplit(as.character(parameters$NeuralHU), "-")))
            
            nn.grid <- if(parameters$NeuralHL == 2){
                expand.grid(
                .layer1 = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1),
                .layer2 = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1),
                .layer3 = c(0)
                )
            } else if(parameters$NeuralHL == 3){
                expand.grid(
                .layer1 = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1),
                .layer2 = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1),
                .layer3 = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1)
                )
            }
            
            cl <- if(get_os()=="windows"){
                parallel::makePSOCKcluster(as.numeric(my.cores))
            } else if(get_os()!="windows"){
                parallel::makeForkCluster(as.numeric(my.cores))
            }
            registerDoParallel(cl)
            
            f <- as.formula(paste("Concentration ~", paste(names(predict.frame)[!names(predict.frame) %in% "Concentration"], collapse = " + ")))
            
            nn_model<-caret::train(f,data=predict.frame, method="neuralnet", rep=parameters$ForestTry, trControl=trainControl(method=parameters$ForestTC, number=parameters$ForestNumber), metric=parameters$ForestMetric, na.action=na.omit,  tuneGrid=nn.grid, linear.output=TRUE)
            
            
            stopCluster(cl)
            nn_model
            
        })
        
        neuralNetworkSpectraShallowRandom <- reactive(label="neuralNetworkSpectraShallowRandom",{
            
            data <- neuralNetworkSpectraShallowModelSet()$data[randomizeData(),]
            parameters <- neuralNetworkSpectraShallowModelSet()$parameters$CalTable
            
            weightdecay.vec <- as.numeric(unlist(strsplit(as.character(parameters$NeuralWD), "-")))
            hiddenunits.vec <- as.numeric(unlist(strsplit(as.character(parameters$NeuralHU), "-")))
            
            
            nn.grid <- expand.grid(
            .decay = seq(weightdecay.vec[1], weightdecay.vec[2], 0.1),
            .size = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1))
            
            cl <- if(get_os()=="windows"){
                parallel::makePSOCKcluster(as.numeric(my.cores))
            } else if(get_os()!="windows"){
                parallel::makeForkCluster(as.numeric(my.cores))
            }
            registerDoParallel(cl)
            
            nn_model<-caret::train(Concentration~.,data=data, method="nnet", linout=TRUE, trControl=trainControl(method=parameters$ForestTC, number=parameters$ForestNumber), allowParallel=TRUE, metric=parameters$ForestMetric, na.action=na.omit, importance=TRUE, tuneGrid=nn.grid, maxit=parameters$NeuralMI, trace=F, trim=TRUE)
            
            
            stopCluster(cl)
            nn_model
            
        })
        
        neuralNetworkSpectraDeepRandom <- reactive(label="neuralNetworkSpectraDeepRandom",{
            
            data <- neuralNetworkSpectraDeepModelSet()$data[randomizeData(),]
            parameters <- neuralNetworkSpectraDeepModelSet()$parameters$CalTable
            
            hiddenunits.vec <- as.numeric(unlist(strsplit(as.character(parameters$NeuralHU), "-")))
            
            nn.grid <- if(parameters$NeuralHL == 2){
                expand.grid(
                .layer1 = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1),
                .layer2 = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1),
                .layer3 = c(0)
                )
            } else if(parameters$NeuralHL == 3){
                expand.grid(
                .layer1 = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1),
                .layer2 = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1),
                .layer3 = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1)
                )
            }
            
            cl <- if(get_os()=="windows"){
                parallel::makePSOCKcluster(as.numeric(my.cores))
            } else if(get_os()!="windows"){
                parallel::makeForkCluster(as.numeric(my.cores))
            }
            registerDoParallel(cl)
            
            f <- as.formula(paste("Concentration ~", paste(names(data)[!names(data) %in% "Concentration"], collapse = " + ")))
            nn_model<-caret::train(f,data=data, method="neuralnet", rep=parameters$ForestTry, trControl=trainControl(method=parameters$ForestTC, number=parameters$ForestNumber), metric=parameters$ForestMetric, na.action=na.omit, tuneGrid=nn.grid, linear.output=TRUE)
            
            
            stopCluster(cl)
            nn_model
            
        })
        
        xgboostIntensityModelRandom <- reactive(label="xgboostIntensityModelRandom",{
            
            predict.frame <- xgboostIntensityModelSet()$data[randomizeData(),]
            parameters <- xgboostIntensityModelSet()$parameters$CalTable
            
            
            tree.depth.vec <- as.numeric(unlist(strsplit(as.character(parameters$TreeDepth), "-")))
            xgbeta.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbEta), "-")))
            xgbgamma.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbGamma), "-")))
            xgbsubsample.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbSubSample), "-")))
            xgbcolsample.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbColSample), "-")))
            
            
            xgbGrid <- expand.grid(
            nrounds = seq(50, parameters$ForestTrees, by=parameters$ForestTrees/5),
            max_depth = seq(tree.depth.vec[1], tree.depth.vec[2], by=5),
            eta = seq(xgbeta.vec[1], xgbeta.vec[2], by=0.1),
            gamma=seq(xgbgamma.vec[1], xgbgamma.vec[2], by=0.1),
            colsample_bytree = seq(xgbcolsample.vec[1], xgbcolsample.vec[2], by=0.1),
            subsample = seq(xgbsubsample.vec[1], xgbsubsample.vec[2], by=0.1),
            min_child_weight = parameters$xgbMinChild
            )
            
            tune_control <- caret::trainControl(
            method = parameters$ForestTC,
            number = parameters$ForestNumber,
            verboseIter = TRUE,
            allowParallel = TRUE
            )
            
            cl <- if(get_os()=="windows"){
                parallel::makePSOCKcluster(as.numeric(my.cores))
            } else if(get_os()!="windows"){
                parallel::makeForkCluster(as.numeric(my.cores))
            }
            registerDoParallel(cl)
            
            xgb_model <- caret::train(Concentration~., data=predict.frame, trControl = tune_control, tuneGrid = xgbGrid, metric=parameters$ForestMetric, method = "xgbTree", na.action=na.omit)
            
            
            stopCluster(cl)
            xgb_model
            
        })
        
        xgboostSpectraModelRandom <- reactive(label="xgboostSpectraModelRandom",{
            
            data <- xgboostSpectraModelSet()$data[randomizeData(),]
            parameters <- xgboostSpectraModelSet()$parameters$CalTable
            
            tree.depth.vec <- as.numeric(unlist(strsplit(as.character(parameters$TreeDepth), "-")))
            xgbeta.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbEta), "-")))
            xgbgamma.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbGamma), "-")))
            xgbsubsample.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbSubSample), "-")))
            xgbcolsample.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbColSample), "-")))
            
            
            xgbGrid <- expand.grid(
            nrounds = seq(50, parameters$ForestTrees, by=parameters$ForestTrees/5),
            max_depth = seq(tree.depth.vec[1], tree.depth.vec[2], by=5),
            eta = seq(xgbeta.vec[1], xgbeta.vec[2], by=0.1),
            gamma=seq(xgbgamma.vec[1], xgbgamma.vec[2], by=0.1),
            colsample_bytree = seq(xgbcolsample.vec[1], xgbcolsample.vec[2], by=0.1),
            subsample = seq(xgbsubsample.vec[1], xgbsubsample.vec[2], by=0.1),
            min_child_weight = parameters$xgbMinChild
            )
            
            tune_control <- caret::trainControl(
            method = parameters$ForestTC,
            number = parameters$ForestNumber,
            verboseIter = TRUE,
            allowParallel = TRUE
            )
            
            cl <- if(get_os()=="windows"){
                parallel::makePSOCKcluster(as.numeric(my.cores))
            } else if(get_os()!="windows"){
                parallel::makeForkCluster(as.numeric(my.cores))
            }
            registerDoParallel(cl)
            
            xgb_model <- caret::train(Concentration~., data=data, trControl = tune_control, tuneGrid = xgbGrid, metric=parameters$ForestMetric, method = "xgbTree", na.action=na.omit)
            
            
            stopCluster(cl)
            xgb_model
            
        })
        
        elementModelRandom <- reactive(label="elementModelRandom",{

            if(input$radiocal==1){
                linearModelRandom()
            } else if(input$radiocal==2){
                nonLinearModelRandom()
            } else if(input$radiocal==3){
                lucasToothModelRandom()
            } else if(input$radiocal==4){
                forestModelRandom()
            } else if(input$radiocal==5){
                rainforestModelRandom()
            } else if(input$radiocal==6 && input$neuralhiddenlayers==1){
                neuralNetworkIntensityShallowRandom()
            } else if(input$radiocal==6 && input$neuralhiddenlayers>1){
                neuralNetworkIntensityDeepRandom()
            } else if(input$radiocal==7 && input$neuralhiddenlayers==1){
                neuralNetworkSpectraShallowRandom()
            } else if(input$radiocal==7 && input$neuralhiddenlayers>1){
                neuralNetworkSpectraShallowRandom()
            } else if(input$radiocal==8){
                xgboostIntensityModelRandom()
            } else if(input$radiocal==9){
                xgboostSpectraModelRandom()
            }

        })
        
        
        valFrameRandomized <- reactive(label="valFrameRandomized",{
            
            predict.intensity <- predictIntensity()[ vals$keeprows, , drop = FALSE]
            predict.frame <- predictFrame()[ vals$keeprows, , drop = FALSE]
            predict.frame.cal <- predict.frame[(randomizeData()), , drop = FALSE]
            predict.frame <- predict.frame[!(randomizeData()), , drop = FALSE]
            predict.frame <- subset(predict.frame, predict.frame$Concentration > my.min(predict.frame.cal[,"Concentration"]) & predict.frame$Concentration  < my.max(predict.frame.cal[,"Concentration"]))
            predict.intensity <- predict.frame[,!colnames(predict.frame) %in% "Concentration"]

            element.model <- elementModelRandom()
            
            
            
            if (input$radiocal==1){
                cal.est.conc.pred <- predict(object=element.model, newdata=predict.intensity, interval='confidence')
                cal.est.conc.tab <- data.frame(cal.est.conc.pred)
                cal.est.conc <- cal.est.conc.tab$fit
                
                val.frame <- data.frame(predict.frame$Concentration, cal.est.conc)
                colnames(val.frame) <- c("Concentration", "Prediction")
            }
            
            if (input$radiocal==2){
                cal.est.conc.pred <- predict(object=element.model, newdata=predict.intensity, interval='confidence')
                cal.est.conc.tab <- data.frame(cal.est.conc.pred)
                cal.est.conc <- cal.est.conc.tab$fit
                
                val.frame <- data.frame(predict.frame$Concentration, cal.est.conc)
                colnames(val.frame) <- c("Concentration", "Prediction")
            }
            
            if (input$radiocal==3){

                
                cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity, interval='confidence')
                cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
                cal.est.conc.luc <- cal.est.conc.tab$fit
                cal.est.conc.luc.up <- cal.est.conc.tab$upr
                cal.est.conc.luc.low <- cal.est.conc.tab$lwr
                
                
                val.frame <- data.frame(predict.frame$Concentration, predict.intensity$Intensity, cal.est.conc.luc, cal.est.conc.luc, cal.est.conc.luc.up, cal.est.conc.luc.low)
                colnames(val.frame) <- c("Concentration", "IntensityOrg", "Intensity", "Prediction", "Upper", "Lower")
            }
            
            if (input$radiocal==4){

                
                cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity)
                #cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
                #cal.est.conc.luc <- cal.est.conc.tab$fit
                #cal.est.conc.luc.up <- cal.est.conc.tab$upr
                #cal.est.conc.luc.low <- cal.est.conc.tab$lwr
                
                
                val.frame <- data.frame(na.omit(predict.frame$Concentration), predict.intensity$Intensity, as.vector(cal.est.conc.pred.luc), as.vector(cal.est.conc.pred.luc))
                colnames(val.frame) <- c("Concentration", "IntensityOrg", "Intensity", "Prediction")
            }
            
            if (input$radiocal==5){

                cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity)
                #cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
                #cal.est.conc.luc <- cal.est.conc.tab$fit
                #cal.est.conc.luc.up <- cal.est.conc.tab$upr
                #cal.est.conc.luc.low <- cal.est.conc.tab$lwr
                
                
                val.frame <- data.frame(predict.frame$Concentration, as.vector(cal.est.conc.pred.luc), as.vector(cal.est.conc.pred.luc))
                colnames(val.frame) <- c("Concentration", "Intensity", "Prediction")
            }
            
            if (input$radiocal==6){
                
                
                cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity)
                #cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
                #cal.est.conc.luc <- cal.est.conc.tab$fit
                #cal.est.conc.luc.up <- cal.est.conc.tab$upr
                #cal.est.conc.luc.low <- cal.est.conc.tab$lwr
                
                
                val.frame <- data.frame(na.omit(predict.frame$Concentration), predict.intensity$Intensity, as.vector(cal.est.conc.pred.luc), as.vector(cal.est.conc.pred.luc))
                colnames(val.frame) <- c("Concentration", "IntensityOrg", "Intensity", "Prediction")
            }
            
            if (input$radiocal==7){
                
                cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity)
                #cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
                #cal.est.conc.luc <- cal.est.conc.tab$fit
                #cal.est.conc.luc.up <- cal.est.conc.tab$upr
                #cal.est.conc.luc.low <- cal.est.conc.tab$lwr
                
                
                val.frame <- data.frame(predict.frame$Concentration, as.vector(cal.est.conc.pred.luc), as.vector(cal.est.conc.pred.luc))
                colnames(val.frame) <- c("Concentration", "Intensity", "Prediction")
            }
            
            if (input$radiocal==8){
                
                
                cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity)
                #cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
                #cal.est.conc.luc <- cal.est.conc.tab$fit
                #cal.est.conc.luc.up <- cal.est.conc.tab$upr
                #cal.est.conc.luc.low <- cal.est.conc.tab$lwr
                
                
                val.frame <- data.frame(na.omit(predict.frame$Concentration), predict.intensity$Intensity, as.vector(cal.est.conc.pred.luc), as.vector(cal.est.conc.pred.luc))
                colnames(val.frame) <- c("Concentration", "IntensityOrg", "Intensity", "Prediction")
            }
            
            if (input$radiocal==9){
                
                cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity)
                #cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
                #cal.est.conc.luc <- cal.est.conc.tab$fit
                #cal.est.conc.luc.up <- cal.est.conc.tab$upr
                #cal.est.conc.luc.low <- cal.est.conc.tab$lwr
                
                
                val.frame <- data.frame(predict.frame$Concentration, as.vector(cal.est.conc.pred.luc), as.vector(cal.est.conc.pred.luc))
                colnames(val.frame) <- c("Concentration", "Intensity", "Prediction")
            }
            
            
            
            
            val.frame
            
        })
        
        
        valFrameRandomizedRev <- reactive(label="valFrameRandomizedRev",{
            
            predict.intensity <- predictIntensity()[ vals$keeprows, , drop = FALSE]
            predict.frame <- predictFrame()[ vals$keeprows, , drop = FALSE]
            
            predict.intensity <- predict.intensity[(randomizeData()), ]
            predict.frame <- predict.frame[(randomizeData()), ]
            element.model <- elementModelRandom()
            
            
            
            if (input$radiocal==1){
                cal.est.conc.pred <- predict(object=element.model, newdata=predict.intensity, interval='confidence')
                cal.est.conc.tab <- data.frame(cal.est.conc.pred)
                cal.est.conc <- cal.est.conc.tab$fit
                
                val.frame <- data.frame(predict.frame$Concentration, cal.est.conc)
                colnames(val.frame) <- c("Concentration", "Prediction")
            }
            
            if (input$radiocal==2){
                cal.est.conc.pred <- predict(object=element.model, newdata=predict.intensity, interval='confidence')
                cal.est.conc.tab <- data.frame(cal.est.conc.pred)
                cal.est.conc <- cal.est.conc.tab$fit
                
                val.frame <- data.frame(predict.frame$Concentration, cal.est.conc)
                colnames(val.frame) <- c("Concentration", "Prediction")
            }
            
            if (input$radiocal==3){
                
                
                cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity, interval='confidence')
                cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
                cal.est.conc.luc <- cal.est.conc.tab$fit
                cal.est.conc.luc.up <- cal.est.conc.tab$upr
                cal.est.conc.luc.low <- cal.est.conc.tab$lwr
                
                
                val.frame <- data.frame(predict.frame$Concentration, predict.intensity$Intensity, cal.est.conc.luc, cal.est.conc.luc, cal.est.conc.luc.up, cal.est.conc.luc.low)
                colnames(val.frame) <- c("Concentration", "IntensityOrg", "Intensity", "Prediction", "Upper", "Lower")
            }
            
            if (input$radiocal==4){

                
                cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity, na.action=na.omit)
                
                
                
                val.frame <- data.frame(na.omit(predict.frame)$Concentration, predict.intensity$Intensity, as.vector(cal.est.conc.pred.luc), as.vector(cal.est.conc.pred.luc))
                colnames(val.frame) <- c("Concentration", "IntensityOrg", "Intensity", "Prediction")
            }
            
            if (input$radiocal==5){

                
                cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity, na.action=na.omit)
                
                
                
                val.frame <- data.frame(na.omit(predict.frame)$Concentration, as.vector(cal.est.conc.pred.luc), as.vector(cal.est.conc.pred.luc))
                colnames(val.frame) <- c("Concentration", "Intensity", "Prediction")
            }
            
            if (input$radiocal==6){
                
                
                cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity, na.action=na.omit)
                
                
                
                val.frame <- data.frame(na.omit(predict.frame)$Concentration, predict.intensity$Intensity, as.vector(cal.est.conc.pred.luc), as.vector(cal.est.conc.pred.luc))
                colnames(val.frame) <- c("Concentration", "IntensityOrg", "Intensity", "Prediction")
            }
            
            if (input$radiocal==7){
                
                
                cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity, na.action=na.omit)
                
                
                
                val.frame <- data.frame(na.omit(predict.frame)$Concentration, as.vector(cal.est.conc.pred.luc), as.vector(cal.est.conc.pred.luc))
                colnames(val.frame) <- c("Concentration", "Intensity", "Prediction")
            }
            
            if (input$radiocal==8){
                
                
                cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity, na.action=na.omit)
                
                
                
                val.frame <- data.frame(na.omit(predict.frame)$Concentration, predict.intensity$Intensity, as.vector(cal.est.conc.pred.luc), as.vector(cal.est.conc.pred.luc))
                colnames(val.frame) <- c("Concentration", "IntensityOrg", "Intensity", "Prediction")
            }
            
            if (input$radiocal==9){
                
                
                cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity, na.action=na.omit)
                
                
                
                val.frame <- data.frame(na.omit(predict.frame)$Concentration, as.vector(cal.est.conc.pred.luc), as.vector(cal.est.conc.pred.luc))
                colnames(val.frame) <- c("Concentration", "Intensity", "Prediction")
            }
            
            
            
            
            val.frame
            
        })
        
        rangescalcurverandom <- reactiveValues(x = NULL, y = NULL)
        
        
        calCurvePlotRandom <- reactive(label="valFrameRandomizedRev",{
            
            predict.frame <- calCurveFrameRandomized()
            element.model <- elementModelRandom()
            
            
            element.name <- if(input$calcurveelement %in% spectralLines){
                gsub("[.]", "", substr(input$calcurveelement, 1, 2))
            } else {
                input$calcurveelement
            }
            intens <- " Counts per Second"
            norma <- " Normalized"
            norma.comp <- " Compton Normalized"
            norma.tc <- " Valid Counts Normalized"
            conen <- paste0(" ", input$plotunit)
            predi <- paste0(" Estimate ", input$plotunit)
            
            intensity.name <- c(element.name, intens)
            concentration.name <- c(element.name, conen)
            prediction.name <- c(element.name, predi)
            
            
            if(input$radiocal==1){
                calcurve.plot <- ggplot(data=predict.frame, aes(Intensity, Concentration)) +
                theme_light() +
                annotate("text", label=lm_eqn(element.model), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                stat_smooth(method="lm", fullrange = TRUE) +
                geom_point() +
                scale_x_continuous(paste(element.name, intens), breaks=scales::pretty_breaks()) +
                scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                coord_cartesian(xlim = rangescalcurverandom$x, ylim = rangescalcurverandom$y, expand = TRUE)
                
            }
            
            if(input$radiocal==2){
                
                calcurve.plot <- ggplot(data=predict.frame, aes(Intensity, Concentration)) +
                theme_light() +
                annotate("text", label=lm_eqn_poly(element.model), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                stat_smooth(method="lm", formula=y~poly(x,2)) +
                geom_point() +
                scale_x_continuous(paste(element.name, intens), breaks=scales::pretty_breaks()) +
                scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                coord_cartesian(xlim = rangescalcurverandom$x, ylim = rangescalcurverandom$y, expand = TRUE)
            }
            
            if(input$radiocal==3){
                val.frame <- valFrameRandomizedRev()
                
                calcurve.plot <- ggplot(data=val.frame, aes(Intensity, Concentration)) +
                theme_light() +
                annotate("text", label=lm_eqn(element.model), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                geom_smooth(aes(x=Intensity, y=Concentration, ymin = Lower, ymax = Upper)) +
                geom_point() +
                scale_x_continuous(paste(element.name, norma), breaks=scales::pretty_breaks()) +
                scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                coord_cartesian(xlim = rangescalcurverandom$x, ylim = rangescalcurverandom$y, expand = TRUE)
            }
            
            if(input$radiocal==4){
                val.frame <- valFrameRandomizedRev()
                
                calcurve.plot <- ggplot(data=val.frame, aes(Intensity, Concentration)) +
                theme_light() +
                annotate("text", label=lm_eqn(lm(Concentration~., val.frame)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                geom_smooth() +
                geom_point() +
                scale_x_continuous(paste(element.name, norma), breaks=scales::pretty_breaks()) +
                scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                coord_cartesian(xlim = rangescalcurverandom$x, ylim = rangescalcurverandom$y, expand = TRUE)
            }
            
            if(input$radiocal==5){
                val.frame <- valFrameRandomizedRev()
                
                calcurve.plot <- ggplot(data=val.frame, aes(Intensity, Concentration)) +
                theme_light() +
                annotate("text", label=lm_eqn(lm(Concentration~., val.frame)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                geom_smooth() +
                geom_point() +
                scale_x_continuous(paste(element.name, norma), breaks=scales::pretty_breaks()) +
                scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                coord_cartesian(xlim = rangescalcurverandom$x, ylim = rangescalcurverandom$y, expand = TRUE)
            }
            
            if(input$radiocal==6){
                val.frame <- valFrameRandomizedRev()
                
                calcurve.plot <- ggplot(data=val.frame, aes(Intensity, Concentration)) +
                theme_light() +
                annotate("text", label=lm_eqn(lm(Concentration~., val.frame)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                geom_smooth() +
                geom_point() +
                scale_x_continuous(paste(element.name, norma), breaks=scales::pretty_breaks()) +
                scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                coord_cartesian(xlim = rangescalcurverandom$x, ylim = rangescalcurverandom$y, expand = TRUE)
            }
            
            if(input$radiocal==7){
                val.frame <- valFrameRandomizedRev()
                
                calcurve.plot <- ggplot(data=val.frame, aes(Intensity, Concentration)) +
                theme_light() +
                annotate("text", label=lm_eqn(lm(Concentration~., val.frame)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                geom_smooth() +
                geom_point() +
                scale_x_continuous(paste(element.name, norma), breaks=scales::pretty_breaks()) +
                scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                coord_cartesian(xlim = rangescalcurverandom$x, ylim = rangescalcurverandom$y, expand = TRUE)
            }
            
            if(input$radiocal==8){
                val.frame <- valFrameRandomizedRev()
                
                calcurve.plot <- ggplot(data=val.frame, aes(Intensity, Concentration)) +
                theme_light() +
                annotate("text", label=lm_eqn(lm(Concentration~., val.frame)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                geom_smooth() +
                geom_point() +
                scale_x_continuous(paste(element.name, norma), breaks=scales::pretty_breaks()) +
                scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                coord_cartesian(xlim = rangescalcurverandom$x, ylim = rangescalcurverandom$y, expand = TRUE)
            }
            
            if(input$radiocal==9){
                val.frame <- valFrameRandomizedRev()
                
                calcurve.plot <- ggplot(data=val.frame, aes(Intensity, Concentration)) +
                theme_light() +
                annotate("text", label=lm_eqn(lm(Concentration~., val.frame)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                geom_smooth() +
                geom_point() +
                scale_x_continuous(paste(element.name, norma), breaks=scales::pretty_breaks()) +
                scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                coord_cartesian(xlim = rangescalcurverandom$x, ylim = rangescalcurverandom$y, expand = TRUE)
            }
            
            calcurve.plot
            
            
        })
        
        observeEvent(input$cropcalrandom, {
            brush <- input$plot_cal_brush_random
            if (!is.null(brush)) {
                rangescalcurverandom$x <- c(brush$xmin, brush$xmax)
                rangescalcurverandom$y <- c(brush$ymin, brush$ymax)
                
            } else {
                rangescalcurverandom$x <- NULL
                rangescalcurverandom$y <- NULL
            }
        })
        
        observeEvent(input$zerocalrandom, {
            predict.frame <- if(calType()==1){
                calCurveFrameRandomized()
            } else if(calType()==2){
                calCurveFrameRandomized()
            } else if(calType()==3) {
                valFrameRandomizedRev()
            } else if(calType()==5) {
                valFrameRandomizedRev()
            }
            
            rangescalcurverandom$x <- c(0, max(predict.frame$Intensity))
            rangescalcurverandom$y <- c(0, max(predict.frame$Concentration))
        })
        
        
        
        output$calcurveplotsrandom <- renderPlot({
            calCurvePlotRandom()
        })
        
        
        rangesvalcurverandom <- reactiveValues(x = NULL, y = NULL)
        
        valCurvePlotRandom <- reactive(label="valCurvePlotRandom",{
            
            
            
            #element.name <- gsub("[.]", "", substr(input$calcurveelement, 1, 2))
            
            element.name <- if(input$calcurveelement %in% spectralLines){
                gsub("[.]", "", substr(input$calcurveelement, 1, 2))
            } else {
                input$calcurveelement
            }

            intens <- " Counts per Second"
            norma <- " Normalized"
            norma.comp <- " Compton Normalized"
            norma.tc <- " Valid Counts Normalized"
            conen <- paste0(" ", input$plotunit)
            predi <- paste0(" Estimate ", input$plotunit)
            
            intensity.name <- c(element.name, intens)
            concentration.name <- c(element.name, conen)
            prediction.name <- c(element.name, predi)
            
            val.frame <- valFrameRandomized()
            
            valcurve.plot <- ggplot(data=val.frame, aes(Prediction, Concentration)) +
            theme_light() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_abline(intercept=0, slope=1, lty=2) +
            stat_smooth(method="lm") +
            geom_point() +
            scale_x_continuous(paste(element.name, predi), breaks=scales::pretty_breaks()) +
            scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
            coord_cartesian(xlim = rangesvalcurverandom$x, ylim = rangesvalcurverandom$y, expand = TRUE)
            
            valcurve.plot
            
        })
        
        observeEvent(input$cropvalrandom, {
            brush <- input$plot_val_brush_random
            if (!is.null(brush)) {
                rangesvalcurverandom$x <- c(brush$xmin, brush$xmax)
                rangesvalcurverandom$y <- c(brush$ymin, brush$ymax)
                
            } else {
                rangesvalcurverandom$x <- NULL
                rangesvalcurverandom$y <- NULL
            }
        })
        
        observeEvent(input$zerovalrandom, {
            predict.frame <- calValFrame()
            
            
            rangesvalcurverandom$x <- c(0, max(predict.frame$Prediction))
            rangesvalcurverandom$y <- c(0, max(predict.frame$Concentration))
        })
        
        
        output$valcurveplotsrandom <- renderPlot({
            valCurvePlotRandom()
        })
        
        
        calPlotRandomDownload <- reactive({
            
            grid.arrange(calCurvePlotRandom(), valCurvePlotRandom(), ncol=2)
            
        })
        
        
        plotDimensionsRandom <- reactive({
            
            if(input$imagesizerandom=="Small"){
                c(7, 4)
            } else if(input$imagesizerandom=="Large"){
                c(14, 8)
            }
            
        })
        
        
        output$downloadcloudplotrandom <- downloadHandler(
        filename = function() { paste(paste(c(input$calname, "_", input$calcurveelement), collapse=''), '.tiff',  sep='') },
        content = function(file) {
            ggsave(file,calPlotRandomDownload(), device="tiff", compression="lzw",  dpi=300, width=plotDimensionsRandom()[1], height=plotDimensionsRandom()[2])
        }
        )
        
        bad <- reactive({
            
            concentration.table <- holdFrame()
            hold.table <- concentration.table[,c("Spectrum", "Concentration")]
            hold.table$Concentration[hold.table$Concentration==""] <- NA
            hold.table <- hold.table[complete.cases(hold.table), ]
            hold.table <- hold.table[!is.na(hold.table$Concentration), ]
            hold.table <- na.omit(hold.table)
            hold.table
        })
        
        output$bader <- renderDataTable({
            
            bad()
            
        })
        
        output$weird <- renderDataTable({
            predictIntensityForest()
            
        })
        
        ####CalCurves
        
        # Float over info
        output$hover_infocal <- renderUI({
            req(input$radiocal)
            point.table <- if(calType()==1){
                calCurveFrame()
            } else if(calType()==2){
                calCurveFrame()
            } else if(calType()==3) {
                calValFrame()
            } else if(calType()==5) {
                calValFrame()
            }
            
            concentration.table <- holdFrame()
            hold.table <- concentration.table[,c("Spectrum", "Concentration")]
            hold.table$Concentration[hold.table$Concentration==""] <- NA
            hold.table <- hold.table[complete.cases(hold.table), ]
            hold.table <- hold.table[!is.na(hold.table$Concentration), ]
            hold.table <- na.omit(hold.table)

            
            point.table$Spectrum <- hold.table["Spectrum"]
            
            
            hover <- input$plot_hovercal
            point <- nearPoints(point.table,  coordinfo=hover, xvar="Intensity", yvar="Concentration",  threshold = 5, maxpoints = 1, addDist = TRUE)
            if (nrow(point) == 0) return(NULL)
            
            
            
            
            # calculate point position INSIDE the image as percent of total dimensions
            # from left (horizontal) and from top (vertical)
            left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
            top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
            
            # calculate distance from left and bottom side of the picture in pixels
            left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
            top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
            
            
            # create style property fot tooltip
            # background color is set so tooltip is a bit transparent
            # z-index is set so we are sure are tooltip will be on top
            style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
            "left:", left_px + 2, "px; top:", top_px + 2, "px;")
            
            # actual tooltip created as wellPanel
            wellPanel(
            style = style,
            p(HTML(paste0(point$Spectrum
            
            )))
            )
        })
        
        
        # Float over info
        output$hover_infocal_random <- renderUI({
            req(input$radiocal)
            point.table <- if(calType()==1){
                calCurveFrame()
            } else if(calType()==2){
                calCurveFrame()
            } else if(calType()==3) {
                calValFrame()
            } else if(calType()==5) {
                calValFrame()
            }
            
            randomized <- randomizeData()
            
            
            point.table <- point.table[ vals$keeprows, , drop = FALSE]
            point.table <- point.table[randomized,]
            
            
            concentration.table <- holdFrame()
            
            concentration.table <- concentration.table[ vals$keeprows, , drop = FALSE]
            concentration.table <- concentration.table[randomized,]
            
            hold.table <- concentration.table[,c("Spectrum", input$calcurveelement)]
            colnames(hold.table) <- c("Spectrum", "Selection")
            
            
            point.table$Spectrum <- hold.table["Spectrum"]
            
            
            hover <- input$plot_hovercal_random
            point <- nearPoints(point.table,  coordinfo=hover, xvar="Intensity", yvar="Concentration",  threshold = 5, maxpoints = 1, addDist = TRUE)
            if (nrow(point) == 0) return(NULL)
            
            
            
            
            # calculate point position INSIDE the image as percent of total dimensions
            # from left (horizontal) and from top (vertical)
            left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
            top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
            
            # calculate distance from left and bottom side of the picture in pixels
            left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
            top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
            
            
            # create style property fot tooltip
            # background color is set so tooltip is a bit transparent
            # z-index is set so we are sure are tooltip will be on top
            style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
            "left:", left_px + 2, "px; top:", top_px + 2, "px;")
            
            # actual tooltip created as wellPanel
            wellPanel(
            style = style,
            p(HTML(paste0(point$Spectrum
            
            )))
            )
        })
        
        
        # Toggle points that are clicked
        observeEvent(input$plot_cal_click, {
            
            predict.frame <- if(calType()==1){
                calCurveFrame()
            } else if(calType()==2){
                calCurveFrame()
            } else if(calType()==3) {
                calValFrame()
            } else if(calType()==5) {
                calValFrame()
            }
            
            res <- nearPoints(predict.frame, xvar="Intensity", yvar="Concentration", input$plot_cal_click, allRows = TRUE)
            
            vals$keeprows <- xor(vals$keeprows, res$selected_)
        })
        
        
        # Toggle points that are brushed, when button is clicked
        observeEvent(input$exclude_toggle, {
            
            predict.frame <- if(calType()==3){
                calCurveFrame()
            } else if(calType()==2){
                calCurveFrame()
            } else if(calType()==3) {
                calValFrame()
            } else if(calType()==5) {
                calValFrame()
            }
            res <- brushedPoints(predict.frame, xvar="Intensity", yvar="Concentration", input$plot_cal_brush, allRows = TRUE)
            
            vals$keeprows <- xor(vals$keeprows, res$selected_)
        })
        
        # Reset all points
        observeEvent(input$exclude_reset, {
            
            predict.frame <- if(calType()==1){
                calCurveFrame()
            } else if(calType()==2){
                calCurveFrame()
            } else if(calType()==3) {
                calValFrame()
            } else if(calType()==5) {
                calValFrame()
            }
            vals$keeprows <- rep(TRUE, nrow(predict.frame))
        })
        
        # Reset all points on element change
        observeEvent(input$calcurveelement, {
            
            
            
            vals$keeprows <- calFileStandards()
            
            
            
        })
        
        
        
        
        ####ValCurves
        
        
        
        
        # Float over info
        output$hover_infoval <- renderUI({
            req(input$radiocal)
            point.table <- calValFrame()

            
            concentration.table <- holdFrame()
            hold.table <- concentration.table[,c("Spectrum", "Concentration")]
            hold.table$Concentration[hold.table$Concentration==""] <- NA
            hold.table <- hold.table[complete.cases(hold.table), ]
            hold.table <- hold.table[!is.na(hold.table$Concentration), ]
            hold.table <- na.omit(hold.table)

            point.table$Spectrum <- hold.table["Spectrum"]
            
            
            
            
            hover <- input$plot_hoverval
            point <- nearPoints(point.table,  coordinfo=hover,  xvar="Prediction", yvar="Concentration", threshold = 5, maxpoints = 1, addDist = TRUE)
            if (nrow(point) == 0) return(NULL)
            
            
            
            
            # calculate point position INSIDE the image as percent of total dimensions
            # from left (horizontal) and from top (vertical)
            left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
            top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
            
            # calculate distance from left and bottom side of the picture in pixels
            left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
            top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
            
            
            # create style property fot tooltip
            # background color is set so tooltip is a bit transparent
            # z-index is set so we are sure are tooltip will be on top
            style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
            "left:", left_px + 2, "px; top:", top_px + 2, "px;")
            
            # actual tooltip created as wellPanel
            wellPanel(
            style = style,
            p(HTML(paste0(point$Spectrum
            
            )))
            )
        })
        
        
        output$hover_infoval_random <- renderUI({
            req(input$radiocal)
            point.table <- calValFrame()
            
            randomized <- randomizeData()
            
            
            point.table <- point.table[ vals$keeprows, , drop = FALSE]
            point.table <- point.table[!(randomized),]
            
            concentration.table <- holdFrame()
            
            concentration.table <- concentration.table[ vals$keeprows, , drop = FALSE]
            concentration.table <- concentration.table[!(randomized),]
            concentration.table.rev <- concentration.table[(randomized),]
            
            hold.table <- concentration.table[,c("Spectrum", "Concentration")]
            colnames(hold.table) <- c("Spectrum", "Selection")
            
            
            point.table$Spectrum <- hold.table["Spectrum"]
            
            
            #point.table <- point.table[point.table$Concentration > min(concentration.table.rev[,"Concentration"], na.rm = TRUE) & point.table$Concentration < max(concentration.table.rev[,"Concentration"], na.rm = TRUE), ]
            
            
            
            hover <- input$plot_hoverval_random
            point <- nearPoints(point.table,  coordinfo=hover,  xvar="Prediction", yvar="Concentration", threshold = 5, maxpoints = 1, addDist = TRUE)
            if (nrow(point) == 0) return(NULL)
            
            
            
            
            # calculate point position INSIDE the image as percent of total dimensions
            # from left (horizontal) and from top (vertical)
            left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
            top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
            
            # calculate distance from left and bottom side of the picture in pixels
            left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
            top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
            
            
            # create style property fot tooltip
            # background color is set so tooltip is a bit transparent
            # z-index is set so we are sure are tooltip will be on top
            style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
            "left:", left_px + 2, "px; top:", top_px + 2, "px;")
            
            # actual tooltip created as wellPanel
            wellPanel(
            style = style,
            p(HTML(paste0(point$Spectrum
            
            )))
            )
        })
        
        # Toggle points that are clicked
        observeEvent(input$plot_val_click, {
            
            predict.frame <- calValFrame()
            
            res <- nearPoints(predict.frame, input$plot_val_click,  xvar="Prediction", yvar="Concentration", allRows = TRUE)
            
            vals$keeprows <- xor(vals$keeprows, res$selected_)
        })
        
        
        # Toggle points that are brushed, when button is clicked
        observeEvent(input$exclude_toggle, {
            predict.frame <- calValFrame()
            
            res <- brushedPoints(predict.frame, input$plot_val_brush,  xvar="Prediction", yvar="Concentration", allRows = TRUE)
            
            vals$keeprows <- xor(vals$keeprows, res$selected_)
        })
        
        # Reset all points
        observeEvent(input$exclude_reset, {
            predict.frame <- calValFrame()
            
            vals$keeprows <- rep(TRUE, nrow(predict.frame))
        })
        
        
        normalLM <- reactive(label="normalLM",{
            
            
            model <- calMemory$Calibration$calList[[input$calcurveelement]][[2]]

            
            model.frame <- as.data.frame(augment(model))
            
            model.frame$qq <- qqnorm(model.frame$.std.resid)[[1]]
            
            model.frame$sqrt.std.resid <- sqrt(abs(model.frame$.std.resid))
            
            model.frame$seq.cooksd <- seq_along(model.frame$.cooksd)
            
            #model.frame$Spectrum <- predictFrameName()$Spectrum
            
            
            
            model.frame
            
        })
        
        
        forestLM <- reactive(label="forestLM",{
            
            
            model <- lm(Concentration~Prediction, data=as.data.frame(calValTable()))
            
            model.frame <- as.data.frame(augment(model))
            
            model.frame$qq <- qqnorm(model.frame$.std.resid)[[1]]
            
            model.frame$sqrt.std.resid <- sqrt(abs(model.frame$.std.resid))
            
            model.frame$seq.cooksd <- seq_along(model.frame$.cooksd)
            
            #model.frame$Spectrum <- predictFrameName()$Spectrum
            
            
            
            model.frame
            
            
        })
        
        
        modelFrame <- reactive(label="modelFrame",{
            
            if(input$radiocal==1){
                normalLM()
            } else if(input$radiocal==2){
                normalLM()
            } else if(input$radiocal==3){
                normalLM()
            } else if(input$radiocal==4){
                forestLM()
            } else if(input$radiocal==5){
                forestLM()
            } else if(input$radiocal==6){
                forestLM()
            } else if(input$radiocal==7){
                forestLM()
            } else if(input$radiocal==8){
                forestLM()
            } else if(input$radiocal==9){
                forestLM()
            }
            
            
        })
        
        
        
        
        
        diagResidualsFitted <- reactive({
            
            model <- modelFrame()
            
            p1 <- ggplot(model[ vals$keeprows, , drop = FALSE], aes(.fitted, .resid)) +
            stat_smooth(method="loess") +
            geom_hline(yintercept=0, col="red", linetype="dashed") +
            scale_x_continuous("Fitted values", breaks=scales::pretty_breaks()) +
            scale_y_continuous("Residuals", breaks=scales::pretty_breaks()) +
            ggtitle("Residual vs Fitted Plot") +
            theme_light() +
            geom_point() +
            geom_point(data=model[ !vals$keeprows, , drop = FALSE], aes(.fitted, .resid), shape = 21, fill = "red", color = "black", alpha = 0.25)
            
            p1
            
        })
        
        output$residualsfitted <- renderPlot({
            
            diagResidualsFitted()
            
        })
        
        
        diagQQ <- reactive({
            
            model <- modelFrame()
            
            p2 <- ggplot(model[ vals$keeprows, , drop = FALSE], aes(qq, .std.resid))+geom_point(na.rm = TRUE) +
            geom_abline() +
            scale_x_continuous("Theoretical Quantiles", breaks=scales::pretty_breaks()) +
            scale_y_continuous("Standardized Residuals", breaks=scales::pretty_breaks()) +
            ggtitle("Normal Q-Q") +
            theme_light() +
            geom_point(data=model[ !vals$keeprows, , drop = FALSE], aes(qq, .std.resid), shape = 21, fill = "red", color = "black", alpha = 0.25)
            
            
            p2
            
        })
        
        
        output$qq <- renderPlot({
            
            diagQQ()
            
        })
        
        diagScaleLocation <- reactive({
            
            model <- modelFrame()
            
            
            p3 <- ggplot(model[ vals$keeprows, , drop = FALSE], aes(.fitted, sqrt.std.resid)) +
            stat_smooth(method="loess", na.rm = TRUE) +
            scale_x_continuous("Fitted Value", breaks=scales::pretty_breaks()) +
            scale_y_continuous(expression(sqrt("|Standardized residuals|")), breaks=scales::pretty_breaks()) +
            ggtitle("Scale-Location") +
            theme_light() +
            geom_point(na.rm=TRUE) +
            geom_point(data=model[ !vals$keeprows, , drop = FALSE], aes(.fitted, sqrt.std.resid), shape = 21, fill = "red", color = "black", alpha = 0.25)
            
            
            p3
            
            
        })
        
        
        output$scalelocation <- renderPlot({
            
            diagScaleLocation()
            
        })
        
        
        diagCooksDistance <- reactive({
            
            model <- modelFrame()
            
            p4 <- ggplot(model, aes(seq.cooksd, .cooksd)) +
            geom_bar(stat="identity", position="identity") +
            scale_x_continuous("Obs. Number", breaks=scales::pretty_breaks()) +
            scale_y_continuous("Cook's distance", breaks=scales::pretty_breaks()) +
            ggtitle("Cook's distance") +
            theme_light()
            
            p4
            
        })
        
        output$cooksdistance <- renderPlot({
            
            diagCooksDistance()
            
        })
        
        
        diagResidualLeverage <- reactive({
            
            model <- modelFrame()
            
            
            p5<-ggplot(model[ vals$keeprows, , drop = FALSE], aes(.hat, .std.resid))+
            geom_point(aes(size=.cooksd), na.rm=TRUE) +
            geom_point(data=model[ !vals$keeprows, , drop = FALSE], aes(.hat, .std.resid), shape = 21, fill = "red", color = "black", alpha = 0.25) +
            stat_smooth(method="loess", na.rm=TRUE) +
            scale_x_continuous("Leverage", breaks=scales::pretty_breaks()) +
            scale_y_continuous("Standardized Residuals", breaks=scales::pretty_breaks()) +
            ggtitle("Residual vs Leverage Plot") +
            scale_size_continuous("Cook's Distance", range=c(1,5)) +
            theme_light() +
            theme(legend.position="bottom")
            
            p5
            
        })
        
        output$residualleverage <- renderPlot({
            
            diagResidualLeverage()
            
        })
        
        
        diagCooksLeverage <- reactive({
            
            model <- modelFrame()
            
            
            p6 <- ggplot(model[ vals$keeprows, , drop = FALSE], aes(.hat, .cooksd)) +
            stat_smooth(method="loess", na.rm=TRUE) +
            scale_x_continuous("Leverage hii", breaks=scales::pretty_breaks()) +
            scale_y_continuous("Cook's Distance", breaks=scales::pretty_breaks()) +
            ggtitle("Cook's dist vs Leverage hii/(1-hii)") +
            geom_abline(slope=seq(0,3,0.5), color="gray", linetype="dashed") +
            theme_light() +
            geom_point(na.rm=TRUE) +
            geom_point(data=model[ vals$keeprows, , drop = FALSE], aes(.hat, .cooksd), shape = 21, fill = "red", color = "black", alpha = 0.25)
            
            p6
            
        })
        
        
        output$cooksleverage <- renderPlot({
            
            diagCooksLeverage()
            
        })
        
        
        diagPlotDownload <- reactive({
            
            grid.arrange(diagResidualsFitted(), diagQQ(),
            diagScaleLocation(), diagCooksDistance(),
            diagResidualLeverage(), diagCooksLeverage(),
            ncol=2, nrow=3)
            
        })
        
        
        output$diagplots <- downloadHandler(
        filename = function() { paste(input$calname, "_", input$calcurveelement, "_diag", ".tiff", sep='') },
        content = function(file) {
            ggsave(file,diagPlotDownload(), width=10, height=10, device="tiff", compression="lzw",  dpi=300, )
        }
        )
        
        #########Diagnostic Plot Controls#######
        ####Residuals Fitted
        # Float over info
        output$hover_inforesidualsfitted <- renderUI({
            req(input$radiocal)
            point.table <- modelFrame()
            
            
            hover <- input$plot_hoverresidualsfitted
            point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
            if (nrow(point) == 0) return(NULL)
            
            
            
            
            # calculate point position INSIDE the image as percent of total dimensions
            # from left (horizontal) and from top (vertical)
            left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
            top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
            
            # calculate distance from left and bottom side of the picture in pixels
            left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
            top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
            
            
            # create style property fot tooltip
            # background color is set so tooltip is a bit transparent
            # z-index is set so we are sure are tooltip will be on top
            style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
            "left:", left_px + 2, "px; top:", top_px + 2, "px;")
            
            # actual tooltip created as wellPanel
            wellPanel(
            style = style,
            p(HTML(paste0(point$Spectrum
            
            )))
            )
        })
        
        # Toggle points that are clicked
        observeEvent(input$plot_residualsfitted_click, {
            
            predict.frame <- modelFrame()
            
            res <- nearPoints(predict.frame, input$plot_residualsfitted_click, allRows = TRUE)
            
            vals$keeprows <- xor(vals$keeprows, res$selected_)
        })
        
        
        # Toggle points that are brushed, when button is clicked
        observeEvent(input$exclude_toggle_diag, {
            
            predict.frame <- modelFrame()
            
            res <- brushedPoints(predict.frame, input$plot_residualsfitted_brush, allRows = TRUE)
            
            vals$keeprows <- xor(vals$keeprows, res$selected_)
        })
        
        
        ####QQ Norm
        # Float over info
        output$hover_infoqq <- renderUI({
            req(input$radiocal)
            point.table <- modelFrame()
            
            
            hover <- input$plot_hoverqq
            point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
            if (nrow(point) == 0) return(NULL)
            
            
            
            
            # calculate point position INSIDE the image as percent of total dimensions
            # from left (horizontal) and from top (vertical)
            left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
            top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
            
            # calculate distance from left and bottom side of the picture in pixels
            left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
            top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
            
            
            # create style property fot tooltip
            # background color is set so tooltip is a bit transparent
            # z-index is set so we are sure are tooltip will be on top
            style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
            "left:", left_px + 2, "px; top:", top_px + 2, "px;")
            
            # actual tooltip created as wellPanel
            wellPanel(
            style = style,
            p(HTML(paste0(point$Spectrum
            
            )))
            )
        })
        
        # Toggle points that are clicked
        observeEvent(input$plot_qq_click, {
            
            predict.frame <- modelFrame()
            
            res <- nearPoints(predict.frame, input$plot_qq_click, allRows = TRUE)
            
            vals$keeprows <- xor(vals$keeprows, res$selected_)
        })
        
        
        # Toggle points that are brushed, when button is clicked
        observeEvent(input$exclude_toggle_diag, {
            
            predict.frame <- modelFrame()
            
            res <- brushedPoints(predict.frame, input$plot_qq_brush, allRows = TRUE)
            
            vals$keeprows <- xor(vals$keeprows, res$selected_)
        })
        
        
        ####Scaled Residuals
        # Float over info
        output$hover_infoscalelocation <- renderUI({
            req(input$radiocal)
            point.table <- modelFrame()
            
            
            hover <- input$plot_hoverscalelocation
            point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
            if (nrow(point) == 0) return(NULL)
            
            
            
            
            # calculate point position INSIDE the image as percent of total dimensions
            # from left (horizontal) and from top (vertical)
            left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
            top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
            
            # calculate distance from left and bottom side of the picture in pixels
            left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
            top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
            
            
            # create style property fot tooltip
            # background color is set so tooltip is a bit transparent
            # z-index is set so we are sure are tooltip will be on top
            style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
            "left:", left_px + 2, "px; top:", top_px + 2, "px;")
            
            # actual tooltip created as wellPanel
            wellPanel(
            style = style,
            p(HTML(paste0(point$Spectrum
            
            )))
            )
        })
        
        # Toggle points that are clicked
        observeEvent(input$plot_scalelocation_click, {
            
            predict.frame <- modelFrame()
            
            res <- nearPoints(predict.frame, input$plot_scalelocation_click, allRows = TRUE)
            
            vals$keeprows <- xor(vals$keeprows, res$selected_)
        })
        
        
        # Toggle points that are brushed, when button is clicked
        observeEvent(input$exclude_toggle_diag, {
            
            predict.frame <- modelFrame()
            
            res <- brushedPoints(predict.frame, input$plot_scalelocation_brush, allRows = TRUE)
            
            vals$keeprows <- xor(vals$keeprows, res$selected_)
        })
        
        
        ####Residuals Leverage
        # Float over info
        output$hover_inforesidualleverage <- renderUI({
            req(input$radiocal)
            point.table <- modelFrame()
            
            
            hover <- input$plot_hoverresidualleverage
            point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
            if (nrow(point) == 0) return(NULL)
            
            
            
            
            # calculate point position INSIDE the image as percent of total dimensions
            # from left (horizontal) and from top (vertical)
            left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
            top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
            
            # calculate distance from left and bottom side of the picture in pixels
            left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
            top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
            
            
            # create style property fot tooltip
            # background color is set so tooltip is a bit transparent
            # z-index is set so we are sure are tooltip will be on top
            style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
            "left:", left_px + 2, "px; top:", top_px + 2, "px;")
            
            # actual tooltip created as wellPanel
            wellPanel(
            style = style,
            p(HTML(paste0(point$Spectrum
            
            )))
            )
        })
        
        # Toggle points that are clicked
        observeEvent(input$plot_residualleverage_click, {
            
            predict.frame <- modelFrame()
            
            res <- nearPoints(predict.frame, input$plot_residualleverage_click, allRows = TRUE)
            
            vals$keeprows <- xor(vals$keeprows, res$selected_)
        })
        
        
        # Toggle points that are brushed, when button is clicked
        observeEvent(input$exclude_toggle_diag, {
            
            predict.frame <- modelFrame()
            
            res <- brushedPoints(predict.frame, input$plot_residualleverage_brush, allRows = TRUE)
            
            vals$keeprows <- xor(vals$keeprows, res$selected_)
        })
        
        
        ####Cooks Leverage
        # Float over info
        output$hover_infocooksleverage <- renderUI({
            req(input$radiocal)
            point.table <- modelFrame()
            
            
            hover <- input$plot_hovercooksleverage
            point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
            if (nrow(point) == 0) return(NULL)
            
            
            
            
            # calculate point position INSIDE the image as percent of total dimensions
            # from left (horizontal) and from top (vertical)
            left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
            top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
            
            # calculate distance from left and bottom side of the picture in pixels
            left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
            top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
            
            
            # create style property fot tooltip
            # background color is set so tooltip is a bit transparent
            # z-index is set so we are sure are tooltip will be on top
            style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
            "left:", left_px + 2, "px; top:", top_px + 2, "px;")
            
            # actual tooltip created as wellPanel
            wellPanel(
            style = style,
            p(HTML(paste0(point$Spectrum
            
            )))
            )
        })
        
        # Toggle points that are clicked
        observeEvent(input$plot_cooksleverage_click, {
            
            predict.frame <- modelFrame()
            
            res <- nearPoints(predict.frame, input$plot_cooksleverage_click, allRows = TRUE)
            
            vals$keeprows <- xor(vals$keeprows, res$selected_)
        })
        
        
        # Toggle points that are brushed, when button is clicked
        observeEvent(input$exclude_toggle_diag, {
            
            predict.frame <- modelFrame()
            
            res <- brushedPoints(predict.frame, input$plot_cooksleverage_brush, allRows = TRUE)
            
            vals$keeprows <- xor(vals$keeprows, res$selected_)
        })
        
        
        
        # Reset all points
        observeEvent(input$exclude_reset_diag, {
            
            predict.frame <- modelFrame()
            
            vals$keeprows <- rep(TRUE, nrow(predict.frame))
        })
        
        
        
        
        #output$downloadcal <- downloadHandler(
        #filename = function() { paste(input$dataset, '.csv', sep=',') },
        #content = function(file
        #) {
        #  write.csv(metadataForm(), file)
        # }
        #)
        
        
        
        ####alternative take
        
        nullList <- reactive({
            
            spectra.line.table <- spectraData()
            
            cal.vector <- elementallinestouse()
            cal.vector2 <- cal.vector[2:length(cal.vector)]
            cal.list <- as.list(cal.vector2)
            setNames(cal.list, cal.vector2)
            cal.list <- pblapply(cal.list, function(x) return(NULL))
            nullList <- cal.list
            
            
        })
        
        
        
        
        #rf2 <- reactiveValues()
        #observe({
        #    if(input$createcalelement > 0){
        #    calList[[input$calcurveelement]] <- elementModel()
        #    }
        #    rf2 <<- calList
        #})
        
        emptyList <- reactive({
            a.list <- list()
            a.list
        })
        
        
        
        
        
        

    
        
        calPlotList <- reactiveValues()
        observeEvent(input$createcalelement, priority=-1, {
            
            
            calPlotList[[input$calcurveelement]] <- isolate(calPlotDownload())
            
                calPlotList <<- calPlotList
            
            })
        
        diagPlotList <- reactiveValues()
        #observeEvent(input$createcalelement, {
        
        
        #diagPlotList[[input$calcurveelement]] <- isolate(diagPlotDownload())
        
        #diagPlotList <<- diagPlotList
        
        #})
        
        
        calibrationProgressSoFar <- reactive({
            
            calProgressSummary(calMemory$Calibration$calList)
            
        })
        
        output$caliibrationprogresstable <- renderDataTable({
            calibrationProgressSoFar()
        })
        
        
        spectraLineTableExport <- reactive({
            if(dataType()=="Spectra"){
                spectraData()[elementallinestouse()]
            } else if(dataType()=="Net"){
                dataHold()[elementallinestouse()]
            }
        })
        
        spectraExport <- reactive({
            if(dataType()=="Spectra"){
                dataHold()
            } else if(dataType()=="Net"){
                myData()
            }
        })
        
        calExport <- reactive({
            calBundle(filetype=input$filetype, units=input$unit, spectra=spectraExport(), intensities=spectraLineTableExport(), definitions=linevalues[["DF"]], values=values[["DF"]], notes=input$notes, calList=calMemory$Calibration$calList)
        })
        
        
        
        #observeEvent(input$createcal, {
        
        #CalibrationPlots$diagPlots <<- diagPlotList
        
        
        #})
        
        
        
        output$downloadModel <- downloadHandler(
        filename <- function(){
            paste(input$calname, "quant", sep=".")
        },
        
        content = function(file) {
            saveRDS(calExport(), file = file, compress="xz")
        }
        )
        
        
        output$downloadReport <- downloadHandler(
        function() { paste(paste(c(input$calname), collapse=''), '.pdf',  sep='') },
        content = function(file){
            ml = marrangeGrob(grobs=calPlotList, nrow=1, ncol=1)
            tryCatch(ggsave(file, ml, device="pdf", dpi=300, width=plotDimensions()[1], height=plotDimensions()[2]), error=function(e) NULL)
            
            dev.off()
        })




    
    
    ####Multiplots Here
    
    
    observeEvent(input$actionprocess_multi, {
        
        
        quantNames <- reactive({
            
            inFile <- input$calfileinput_multi
            if (is.null(inFile)) return(NULL)
            
            as.vector(gsub(".quant", "", inFile$name))
            
            
            
        })


        quantLoad <- reactive({
            
            inFile <- input$calfileinput_multi
            if (is.null(inFile)) return(NULL)
            
            cal.list <- lapply(inFile$datapath, readRDS)
            names(cal.list) <- quantNames()
            
            cal.list
            
        })
        
        calListMulti <- reactiveValues()
        
        
        observeEvent(input$actionprocess_multi, {
            isolate(calListMulti <<- quantLoad())
            
        })


    

        
        
        
        
        
        nQuant <- reactive({
            
            inFile <- input$calfileinput_multi
            if (is.null(inFile)) return(NULL)
            
            length(inFile$name)
            
        })
        

        
        
        output$defaultcalui <- renderUI({
            req(input$calfileinput_multi)
            inFile <- input$calfileinput_multi
            if (is.null(inFile)) return(NULL)
            
            names <- gsub(".quant", "", inFile$name)
            
            selectInput('defaultcal', "Active Cal", choices=quantNames(), selected=quantNames()[1], multiple=FALSE)
            
        })
        
        
        quantType <- reactive({
            
            filetype.vector <- sapply(calListMulti, "[[", "FileType")
            unique(filetype.vector)
            
        })
        
        
        
        dataTypeMulti <- reactive({
            if(quantType()=="CSV"){
                "Spectra"
            } else if(quantType()=="Spectra"){
                "Spectra"
            } else if(quantType()=="PDZ"){
                "Spectra"
            } else if(quantType()=="TXT"){
                "Spectra"
            } else if(quantType()=="Elio"){
                "Spectra"
            }  else if(quantType()=="MCA"){
                "Spectra"
            }  else if(quantType()=="SPX"){
                "Spectra"
            } else if(quantType()=="Net"){
                "Net"
            }
            
        })
        
        
        
        
        quantIntensities <- reactive({
            
            cal.list <- calListMulti
            
            cal.names <- quantNames()
            
            n <- length(cal.names)
            
            intensities.list <- lapply(cal.list, "[[", "Intensities")
            names(intensities.list) <- quantNames()
            
            intensities.list <- lapply(intensities.list, as.data.frame)
            names(intensities.list) <- quantNames()
            
            intensities.list
            
            #line <- seq(1, length(intensities.list), 1)
            
            #intensities.list <- lapply(line, function(x) data.frame(intensities.list[[x]], Instrument=cal.names[x]))
            #names(intensities.list) <- quantNames()
            #do.call("rbind", intensities.list)
            
        })
        
        quantValues <- reactive({
            
            cal.list <- calListMulti
            
            cal.names <- quantNames()
            
            n <- length(cal.names)
            
            values.list <- lapply(cal.list, "[[", "Values")
            names(values.list) <- quantNames()
            
            values.list <- lapply(values.list, as.data.frame)
            names(values.list) <- quantNames()
            
            values.list
            
            #line <- seq(1, length(values.list), 1)
            
            #values.list <- lapply(line, function(x) data.frame(values.list[[x]], Instrument=cal.names[x]))
            #names(values.list) <- quantNames()
            #do.call("rbind", values.list)
            
            
        })
        
        
        quantData <- reactive({
            
            cal.list <- calListMulti
            
            cal.names <- quantNames()
            
            n <- length(cal.names)
            
            data.list <- lapply(cal.list, "[[", "Spectra")
            names(data.list) <- quantNames()
            
            data.list <- lapply(data.list, as.data.frame)
            names(data.list) <- quantNames()
            
            data.list
            
            #line <- seq(1, length(data.list), 1)
            
            #data.list <- lapply(line, function(x) data.frame(data.list[[x]], Instrument=cal.names[x]))
            #names(data.list) <- quantNames()
            #do.call("rbind", data.list)
            
            data.list
            
            
        })
        
        
        quantCals <- reactive({
            
            cal.list <- calListMulti
            
            cal.names <- quantNames()
            
            n <- length(cal.names)
            
            quant.list <- lapply(cal.list, "[[", "calList")
            names(quant.list) <- quantNames()
            
            quant.list
            
            
        })
        
        
        elementallinestouseMulti <- reactive({
            
            
            colnames(quantIntensities()[[input$defaultcal]])
            
            
        })
        
        
        variablelinestouseMulti <- reactive({
            
            
            names(quantValues()[[input$defaultcal]])
            
            
        })
        
        
        outVarMulti <- reactive({
            
            myelements <- elementallinestouseMulti()
            
            result <- if(is.null(myelements)){
                "Ca.K.alpha"
            }else{
                myelements
            }
            
            result
            
            
        })
        
        outVaraltMulti <- reactive({
            
            
            myelements <- c(variablelinestouseMulti())
            
            
            if(is.null(myelements)){
                paste("Ca.K.alpha")
            }else{
                myelements
            }
            
        })
        
        outVaralt2Multi <- reactive({
            
            
            myelements <- c(variablelinestouseMulti())
            
            
            if(is.null(myelements)){
                paste("Ca.K.alpha")
            }else{
                myelements[! myelements %in% c(input$calcurveelement_multi)]
            }
            
        })
        
        output$inVar2_multi <- renderUI({
            selectInput(inputId = "calcurveelement_multi", label = h4("Element"), choices =  outVarMulti())
        })
        
        inVar3SelectedMulti <- reactive({

            calListMulti[[input$defaultcal]][["calList"]][[input$calcurveelement_multi]][[1]]$Intercept
            
            
            
        })
        
        
        output$inVar3_multi <- renderUI({
            
            selectInput(inputId = "intercept_vars_multi", label = h4("Intercept"), choices =  outVaralt2Multi(), selected=inVar3SelectedMulti(), multiple=TRUE)
        })
        
        inVar4SelectedMulti <- reactive({

            
            calListMulti[[input$defaultcal]][["calList"]][[input$calcurveelement_multi]][[1]]$Slope
            
        })
        
        output$inVar4_multi <- renderUI({
            selectInput(inputId = "slope_vars_multi", label = h4("Slope"), choices =  outVaraltMulti(), selected=inVar4SelectedMulti(), multiple=TRUE)
        })
        
        
        
        calTypeSelectionMulti <- reactive({

            calListMulti[[input$defaultcal]][["calList"]][[input$calcurveelement_multi]][[1]]$CalTable$CalType
            
            
        })
        
        
        forestMetricSelectionMulti <- reactive({
            
            
            calListMulti[[input$defaultcal]][["calList"]][[input$calcurveelement_multi]][[1]]$CalTable$ForestMetric
            
            
        })
        
        
        forestTrainSelectionMulti <- reactive({
            
            
            calListMulti[[input$defaultcal]][["calList"]][[input$calcurveelement_multi]][[1]]$CalTable$ForestTC
            
            
        })
        
        
        forestNumberSelectionMulti <- reactive({
            
            
            calListMulti[[input$defaultcal]][["calList"]][[input$calcurveelement_multi]][[1]]$CalTable$ForestNumber
            
            
        })
        
        
        forestTreeSelectionMulti <- reactive({
            
            
            calListMulti[[input$defaultcal]][["calList"]][[input$calcurveelement_multi]][[1]]$CalTable$ForestTrees
            
            
        })
        
        
        calNormSelectionMulti <- reactive({
            
            
            calListMulti[[input$defaultcal]][["calList"]][[input$calcurveelement_multi]][[1]]$CalTable$NormType
            
            
        })
        
        normMinSelectionMulti <- reactive({
        
            
            calListMulti[[input$defaultcal]][["calList"]][[input$calcurveelement_multi]][[1]]$CalTable$Min
            
            
        })
        
        normMaxSelectionMulti <- reactive({
            
            calListMulti[[input$defaultcal]][["calList"]][[input$calcurveelement_multi]][[1]]$CalTable$Max
            
        })
        
        
        
        
        
        output$calTypeInput_multi <- renderUI({
            
            selectInput("radiocal_multi", label = "Calibration Curve",
            choices = list("Linear" = 1, "Non-Linear" = 2, "Lucas-Tooth" = 3, "Forest" = 4, "Rainforest" = 5),
            selected = calTypeSelectionMulti())
            
            
        })
        
        
        output$normTypeInput_multi <- renderUI({
            
            selectInput("normcal_multi", label = "Normalization",
            choices = list("Time" = 1, "Total Counts" = 2, "Compton" = 3),
            selected = calNormSelectionMulti())
            
            
        })
        
        output$forestmetricui_multi <- renderUI({
            
            if(input$radiocal_multi==1){
                NULL
            } else if(input$radiocal_multi==2){
                NULL
            } else if(input$radiocal_multi==3){
                NULL
            } else if(input$radiocal_multi==4){
                selectInput("forestmetric_multi", label="Metric", choices=c("Root Mean Square Error"="RMSE", "R2"="Rsquared", "ROC Curve"="ROC", "Logarithmic Loss"="logLoss"), selected=forestMetricSelectionMulti())
            } else if(input$radiocal_multi==5){
                selectInput("forestmetric_multi", label="Metric", choices=c("Root Mean Square Error"="RMSE", "R2"="Rsquared", "ROC Curve"="ROC", "Logarithmic Loss"="logLoss"), selected=forestMetricSelectionMulti())
            }
            
        })
        
        
        output$foresttrainui_multi <- renderUI({
            
            if(input$radiocal_multi==1){
                NULL
            } else if(input$radiocal_multi==2){
                NULL
            } else if(input$radiocal_multi==3){
                NULL
            } else if(input$radiocal_multi==4){
                selectInput("foresttrain_multi", label="Train Control", choices=c("k-fold Cross Validation"="cv", "Bootstrap"="boot", "0.632 Bootstrap"="boot632", "Optimism Bootstrap"="optimism_boot", "Repeated k-fold Cross Validation"="repeatedcv", "Leave One Out Cross Validation"="LOOCV", "Out of Bag Estimation"="oob"), selected=forestTrainSelectionMulti())
            }  else if(input$radiocal_multi==5){
                selectInput("foresttrain_multi", label="Train Control", choices=c("k-fold Cross Validation"="cv", "Bootstrap"="boot", "0.632 Bootstrap"="boot632", "Optimism Bootstrap"="optimism_boot", "Repeated k-fold Cross Validation"="repeatedcv", "Leave One Out Cross Validation"="LOOCV", "Out of Bag Estimation"="oob"), selected=forestTrainSelectionMulti())
            }
            
        })
        
        output$forestnumberui_multi <- renderUI({
            
            if(input$radiocal_multi==1){
                NULL
            } else if(input$radiocal_multi==2){
                NULL
            } else if(input$radiocal_multi==3){
                NULL
            } else if(input$radiocal_multi==4){
                sliderInput("forestnumber_multi", label="Iterations", min=5, max=1000, value=forestNumberSelectionMulti())
            }  else if(input$radiocal_multi==5){
                sliderInput("forestnumber_multi", label="Iterations", min=5, max=1000, value=forestNumberSelectionMulti())
            }
            
        })
        
        
        
        output$foresttreesui_multi <- renderUI({
            
            if(input$radiocal_multi==1){
                NULL
            } else if(input$radiocal_multi==2){
                NULL
            } else if(input$radiocal_multi==3){
                NULL
            } else if(input$radiocal_multi==4){
                sliderInput("foresttrees_multi", label="Trees", min=5, max=1000, value=forestTreeSelectionMulti())
            }  else if(input$radiocal_multi==5){
                sliderInput("foresttrees_multi", label="Trees", min=5, max=1000, value=forestTreeSelectionMulti())
            }
            
        })
        
        
        output$comptonMinInput_multi <- renderUI({
            
            numericInput('comptonmin_multi', label=h6("Min"), step=0.001, value=normMinSelectionMulti(), min=0, max=50, width='30%')
            
        })
        
        output$comptonMaxInput_multi <- renderUI({
            
            numericInput('comptonmax_multi', label=h6("Max"), step=0.001, value=normMaxSelectionMulti(), min=0, max=50, width='30%')
            
        })
        
        

observeEvent(input$actionprocess2_multi, {

        
        calTypeMulti <- reactive({
            
            
            if(input$radiocal_multi==1){
                1
            } else if(input$radiocal_multi==2){
                2
            } else if(input$radiocal_multi==3){
                3
            } else if(input$radiocal_multi==4){
                3
            } else if(input$radiocal_multi==5){
                5
            }
            
        })
        
        
        
        
        elementHoldMulti <- reactive({
            
            if(is.null(input$calcurveelement_multi)==TRUE){
                 elementallinestouseMulti()[1]
            } else{
                input$calcurveelement_multi
            }
            
        })
        
        
        calFileStandardsMulti <- reactive({
            
            calstandardmulticheck <- function(cal, vals, element){
                if(is.null(cal[[element]][[1]][["StandardsUsed"]])){
                    as.vector(rep(TRUE, length(vals[element])))
                } else if(!is.null(cal[[element]][[1]][["StandardsUsed"]])){
                   cal[[element]][[1]][["StandardsUsed"]]
                }
                
            }
            calstandardmulticheck <- cmpfun(calstandardmulticheck)

            
           cal.list <- quantCals()
           
           val.list <- quantValues()
           
           cal.names <- quantNames()
           
           n <- length(cal.names)
           
           #element.cal.meta <- lapply(cal.list, "[[", elementHoldMulti())
           #element.cal.meta2 <- lapply(element.cal.meta, "[[", 1)
           #element.cal.meta3 <- lapply(element.cal.meta2, "[[", "StandardsUsed")
           #element.cal.meta4 <- lapply(element.cal.meta3, as.vector)

           
           #names(element.cal.meta4) <- quantNames()
           #element.cal.meta4
           
           element.cal.meta <- lapply(quantNames(), function(x) calstandardmulticheck(cal=quantCals()[[x]], vals=quantValues()[[x]], element=input$calcurveelement_multi))
           names(element.cal.meta) <- quantNames()
           element.cal.meta
           
        })
        
        
        
        
        
        
        vals_multi <- reactiveValues()
        
        
        vals_multi$keeprows <- calFileStandardsMulti()
        
        
        
        
        concentrationTableMulti <- reactive({
            
            concentration.table <- quantValues()
            cal.names <- names(quantValues())
            
            index <- seq(from=1, to=length(cal.names), by=1)

            
            concentration.list <- lapply(quantNames(),function(x) as.data.frame(concentration.table[[x]][order(as.character(concentration.table[[x]][,"Spectrum"])),]))
            names(concentration.list) <- quantNames()
            
            #concentration.list <- lapply(concentration.list, as.data.frame)
            #names(concentration.list) <- quantNames()
            
            concentration.list
            
        })




        
        spectraLineTableMulti <- reactive({
            
            spectra.line.table <- quantIntensities()
            cal.names <- names(spectra.line.table)
            
            index <- seq(from=1, to=length(cal.names), by=1)
            
            spectra.line.list <- lapply(quantNames(),function(x) data.frame(Spectrum=quantValues()[[x]][,"Spectrum"], spectra.line.table[[x]]))
            
                names(spectra.line.list) <- quantNames()
            
            #spectra.line.table$Spectrum <- quantValues()$Spectrum
            
            spectra.line.list <- lapply(quantNames(), function(x) as.data.frame(spectra.line.list[[x]][order(as.character(spectra.line.list[[x]][,"Spectrum"])),]))
            names(spectra.line.list) <- quantNames()
            
            spectra.line.list <- lapply(quantNames(), function(x) data.frame(spectra.line.list[[x]][rowSums(spectra.line.list[[x]][,-1])!=0,]))
            names(spectra.line.list) <- quantNames()

            
            
            spectra.line.list

            
        })
        

        
        
        holdFrameMulti <- reactive({
            
            spectra.line.table <- spectraLineTableMulti()
            concentration.table <- concentrationTableMulti()
            
            concentration.table <- lapply(quantNames(), function(x) data.frame(concentration.table[[x]][concentration.table[[x]]$Spectrum %in% spectra.line.table[[x]]$Spectrum,]))
            names(concentration.table) <- quantNames()

            spectra.line.table <- lapply(quantNames(), function(x) data.frame(spectra.line.table[[x]][spectra.line.table[[x]]$Spectrum %in% concentration.table[[x]]$Spectrum,]))
            names(spectra.line.table) <- quantNames()

            cal.names <- names(spectra.line.table)
            
            index <- seq(from=1, to=length(cal.names), by=1)
            
            hold.list <- lapply(quantNames(),function(x) data.frame(spectra.line.table[[x]], Concentration=as.vector(as.numeric(unlist(concentration.table[[x]][input$calcurveelement_multi])))))
            names(hold.list) <- quantNames()

            
            hold.list <- lapply(quantNames(),function(x) hold.list[[x]][complete.cases(hold.list[[x]]),])
            names(hold.list) <- quantNames()
            
            hold.list <- lapply(quantNames(),function(x) na.omit(hold.list[[x]]))
            names(hold.list) <- quantNames()
            
            hold.list <- lapply(quantNames(),function(x) hold.list[[x]][order(as.character(hold.list[[x]][,"Spectrum"])),])
            names(hold.list) <- quantNames()
            

            #hold.frame <- data.frame(spectra.names, concentration, intensity, instrument.names)
            #colnames(hold.frame) <- c("Spectrum", "Concentration", "Intensity", "Instrument")
            #hold.frame <- na.omit(hold.frame)
            
            #hold.frame <- hold.frame[order(as.character(hold.frame$Instrument), as.character(hold.frame$Spectrum)),]
            
            
            #hold.frame
            hold.list
            
        })
        


        
        
        dataNormMulti <- reactive({
            
            data <- quantData()
            cal.names <- names(data)
            
            index <- seq(from=1, to=length(cal.names), by=1)
            
            data <- lapply(quantNames(),function(x) as.data.frame(data[[x]][data[[x]][,"Spectrum"] %in% holdFrameMulti()[[x]][,"Spectrum"], ]))
            names(data) <- quantNames()
            
            data

        #data[paste0(data$Spectrum, data$Instrument) %in% paste0(holdFrameMulti()$Spectrum, holdFrameMulti()$Instrument), ]
            
            
        })
        
        
        
        
        predictIntensitySimpPreMulti <- reactive({
            
            data <- dataNormMulti()
            spectra.line.table <- holdFrameMulti()
            
            if(input$normcal_multi==1){
                predict.intensity <- if(dataTypeMulti()=="Spectra"){
                    lapply(quantNames(), function(x) general_prep_xrf(spectra.line.table=spectra.line.table[[x]], element.line=input$calcurveelement_multi))
                } else if(dataTypeMulti()=="Net"){
                    lapply(quantNames(), function(x) general_prep_xrf_net(spectra.line.table=spectra.line.table[[x]], element.line=input$calcurveelement_multi))
                }
            } else if(input$normcal_multi==2){
                predict.intensity <- if(dataTypeMulti()=="Spectra"){
                    lapply(quantNames(), function(x) simple_tc_prep_xrf(data=data[[x]], spectra.line.table=spectra.line.table[[x]], element.line=input$calcurveelement_multi))
                } else if(dataTypeMulti()=="Net"){
                    lapply(quantNames(), function(x) simple_tc_prep_xrf_net(data=data[[x]], spectra.line.table=spectra.line.table[[x]], element.line=input$calcurveelement_multi))
                }
            } else if(input$normcal_multi==3){
                predict.intensity <- if(dataTypeMulti()=="Spectra"){
                    lapply(quantNames(), function(x) simple_comp_prep_xrf(data=data[[x]], spectra.line.table=spectra.line.table[[x]], element.line=input$calcurveelement_multi, norm.min=input$comptonmin_multi, norm.max=input$comptonmax_multi))
                } else if(dataTypeMulti()=="Net"){
                    lapply(quantNames(), function(x) simple_comp_prep_xrf_net(data=data[[x]], spectra.line.table=spectra.line.table[[x]], element.line=input$calcurveelement_multi, norm.min=input$comptonmin_multi, norm.max=input$comptonmax_multi))
                }
            }
            
            names(predict.intensity) <- quantNames()
            predict.intensity
            
        })
        
        
        predictFrameSimpMulti <- reactive({
            
            data <- dataNormMulti()
            spectra.line.table <- holdFrameMulti()
            
            predict.intensity.simp <- predictIntensitySimpPreMulti()
            
            predict.frame.simp <- lapply(quantNames(), function(x) data.frame(predict.intensity.simp[[x]], Concentration=spectra.line.table[[x]][,"Concentration"]))
            names(predict.frame.simp) <- quantNames()
            
            predict.frame.simp <- lapply(quantNames(), function(x) predict.frame.simp[[x]][complete.cases(predict.frame.simp[[x]]$Concentration),])
            names(predict.frame.simp) <- quantNames()

            
            predict.frame.simp
            
        })
        
        predictIntensitySimpMulti <- reactive({
            
            predict.intensity <- lapply(quantNames(), function(x) data.frame(Intensity=predictFrameSimpMulti()[[x]][,1]))
            names(predict.intensity) <- quantNames()
            predict.intensity

        })
        
        simpleLinearModelMulti <- reactive({
            
            cal.lm <- lapply(quantNames(),function(x) lm(Concentration~Intensity, data=predictFrameSimpMulti()[[x]][vals_multi$keeprows[[x]],, drop=FALSE], , na.action=na.omit))
            names(cal.lm) <- quantNames()
            cal.lm

            
        })
        
        nonLinearModelMulti <- reactive({
            
            cal.lm <- lapply(quantNames(), function(x) lm(Concentration~Intensity + I(Intensity^2), data=predictFrameSimpMulti()[[x]][vals_multi$keeprows[[x]],, drop=FALSE], na.action=na.omit))
            names(cal.lm) <- quantNames()
            cal.lm

        })
        
        predictIntensityForestPreMulti <- reactive({
            
            data <- dataNormMulti()
            spectra.line.table <- holdFrameMulti()
            
            
            if(input$normcal_multi==1){
                predict.intensity <- if(dataTypeMulti()=="Spectra"){
                    lapply(quantNames(), function(x)  lucas_simp_prep_xrf(spectra.line.table=spectra.line.table[[x]], element.line=input$calcurveelement_multi, slope.element.lines=elementallinestouseMulti(), intercept.element.lines=input$intercept_vars_multi))
                } else if(dataTypeMulti()=="Net"){
                    lapply(quantNames(), function(x)  lucas_simp_prep_xrf_net(spectra.line.table=spectra.line.table[[x]], element.line=input$calcurveelement_multi, slope.element.lines=elementallinestouseMulti(), intercept.element.lines=input$intercept_vars_multi))
                }
            } else if(input$normcal_multi==2){
                predict.intensity <- if(dataTypeMulti()=="Spectra"){
                    lapply(quantNames(), function(x) lucas_tc_prep_xrf(data=data[[x]], spectra.line.table=spectra.line.table[[x]], element.line=input$calcurveelement_multi, slope.element.lines=elementallinestouseMulti(), intercept.element.lines=input$intercept_vars_multi))
                } else if(dataTypeMulti()=="Net"){
                    lapply(quantNames(), function(x) lucas_tc_prep_xrf_net(data=data[[x]], spectra.line.table=spectra.line.table[[x]], element.line=input$calcurveelement_multi, slope.element.lines=elementallinestouseMulti(), intercept.element.lines=input$intercept_vars_multi))
                }
            } else if(input$normcal_multi==3){
                predict.intensity <- if(dataTypeMulti()=="Spectra"){
                    lapply(quantNames(), function(x) lucas_comp_prep_xrf(data=data[[x]], spectra.line.table=spectra.line.table[[x]], element.line=input$calcurveelement_multi, slope.element.lines=elementallinestouseMulti(), intercept.element.lines=input$intercept_vars_multi, norm.min=input$comptonmin_multi, norm.max=input$comptonmax_multi))
                } else if(dataTypeMulti()=="Net"){
                    lapply(quantNames(), function(x) lucas_comp_prep_xrf_net(data=data[[x]], spectra.line.table=spectra.line.table[[x]], element.line=input$calcurveelement_multi, slope.element.lines=elementallinestouseMulti(), intercept.element.lines=input$intercept_vars_multi, norm.min=input$comptonmin_multi, norm.max=input$comptonmax_multi))
                }
            }
            
            names(predict.intensity) <- quantNames()
            predict.intensity
            
            
        })
        
        
        predictFrameForestMulti <- reactive({
            
            spectra.line.table <- holdFrameMulti()
            
            
            predict.intensity.forest <- predictIntensityForestPreMulti()
            
            predict.frame.forest <- lapply(quantNames(), function(x) data.frame(predict.intensity.forest[[x]], Concentration=spectra.line.table[[x]][,"Concentration"]))
            names(predict.frame.forest) <- quantNames()
            
            
            predict.frame.forest <- lapply(quantNames(), function(x) predict.frame.forest[[x]][complete.cases(predict.frame.forest[[x]]$Concentration),])
            names(predict.frame.forest) <- quantNames()
            
            predict.frame.forest
            
        })
        
        predictIntensityForestMulti <- reactive({
            
            predict.intensity <- lapply(quantNames(), function(x) predictFrameForestMulti()[[x]][,!(colnames(predictFrameForestMulti()[[x]]) %in% "Concentration")])
            names(predict.intensity) <- quantNames()
            predict.intensity
            
        })
        
        forestModelMulti <- reactive({
            
            #randomForest(Concentration~., data=predictFrameForest()[vals$keeprows,, drop=FALSE], na.action=na.omit, ntree=1000, nPerm=100)
            
            cor.mod <- if(length(quantNames())>as.numeric(my.cores)){
                as.numeric(my.cores)
            } else if(length(quantNames())<=as.numeric(my.cores)){
                length(quantNames())
            }
            
            cl <- if(get_os()=="windows"){
                parallel::makePSOCKcluster(as.numeric(my.cores))
            } else if(get_os()!="windows"){
                parallel::makeForkCluster(as.numeric(my.cores))
            }
            registerDoParallel(cl)
            cal.lm <- lapply(quantNames(), function(x) caret::train(Concentration~., data=predictFrameForestMulti()[[x]][vals_multi$keeprows[[x]],, drop=FALSE], method="rf", type="Regression",
            trControl=trainControl(method=input$foresttrain_multi, number=input$forestnumber_multi), ntree=input$foresttrees_multi,
            prox=TRUE,allowParallel=TRUE, metric=input$forestmetric_multi, na.action=na.omit, importance=TRUE, trim=TRUE))
            stopCluster(cl)
            names(cal.lm) <- quantNames()
            cal.lm
            
            
        })
        
        
        predictIntensityLucPreMulti <- reactive({
            
            
            predict.intensity <- lapply(quantNames(), function(x)  predictIntensityForestMulti()[[x]][,c("Intensity", input$slope_vars_multi)])
            names(predict.intensity) <- quantNames()
            predict.intensity
            
        })
        
        predictFrameLucMulti <- reactive({
            
            data <- dataNormMulti()
            spectra.line.table <- predictFrameForestMulti()
            
            
            predict.intensity.luc <- predictIntensityLucPreMulti()
            
            predict.frame.luc <- lapply(quantNames(), function(x)  data.frame(predict.intensity.luc[[x]], Concentration=spectra.line.table[[x]][,"Concentration"]))
            names(predict.frame.luc) <- quantNames()
            
            
            predict.frame.luc <- lapply(quantNames(), function(x) predict.frame.luc[[x]][complete.cases(predict.frame.luc[[x]]),])
            names(predict.frame.luc) <- quantNames()
            
            predict.frame.luc
            
        })
        
        predictIntensityLucMulti <- reactive({
            
            predict.intensity <- lapply(quantNames(), function(x) predictFrameLucMulti()[[x]][,!(colnames(predictFrameLucMulti()[[x]]) %in% "Concentration")])
            names(predict.intensity) <- quantNames()
            predict.intensity
            
            
        })
        
        
        lucasToothModelMulti <- reactive({
            
            cal.lm <- lapply(quantNames(), function(x) lm(Concentration~., data=predictFrameLucMulti()[[x]][vals_multi$keeprows[[x]],, drop=FALSE], na.action=na.omit))
            names(cal.lm) <- quantNames()
            cal.lm
            
        })
        
        
        rainforestIntensityPreMulti <- reactive({
            data <- dataNormMulti()
            
            spectra.data <- if(input$normcal_multi==1){
                if(dataTypeMulti()=="Spectra"){
                    lapply(quantNames(), function(x)  spectra_simp_prep_xrf(spectra=data[[x]])[,-1])
                } else if(dataTypeMulti()=="Net"){
                    NULL
                }
            } else if(input$normcal_multi==2){
                if(dataTypeMulti()=="Spectra"){
                    lapply(quantNames(), function(x) spectra_tc_prep_xrf(spectra=data[[x]])[,-1])
                } else if(dataTypeMulti()=="Net"){
                    NULL
                }
            } else if(input$normcal_multi==3){
                if(dataTypeMulti()=="Spectra"){
                    lapply(quantNames(), function(x) spectra_comp_prep_xrf(spectra=data[[x]], norm.min=input$comptonmin_multi, norm.max=input$comptonmax_multi)[,-1])
                } else if(dataTypeMulti()=="Net"){
                    NULL
                }
            }
            
            
            names(spectra.data) <- quantNames()
            spectra.data
            
        })
        
        
        rainforestDataMulti <- reactive({
            
            spectra.line.table <- holdFrameMulti()
            
            spectra.data <- rainforestIntensityPreMulti()
            
            spectra.data <-  lapply(quantNames(), function(x) data.frame(rainforestIntensityPreMulti()[[x]], Concentration=spectra.line.table[[x]][,"Concentration"]))
            names(spectra.data) <- quantNames()


            spectra.data <- lapply(quantNames(), function(x) spectra.data[[x]][complete.cases(spectra.data[[x]]$Concentration),])
            names(spectra.data) <- quantNames()
            
            spectra.data
            
        })
        
        rainforestIntensityMulti <- reactive({
            
            spectra.data <- lapply(quantNames(), function(x) rainforestDataMulti()[[x]][,!(colnames(rainforestDataMulti()[[x]]) %in% "Concentration")])
            names(spectra.data) <- quantNames()
            spectra.data

        })
        
        
        rainforestModelMulti <- reactive({
            
            #randomForest(Concentration~., data=rainforestData(), na.action=na.omit, ntree=1000, nPerm=100)
            
            
            
            cor.mod <- if(length(quantNames())>as.numeric(my.cores)){
                as.numeric(my.cores)
            } else if(length(quantNames())<=as.numeric(my.cores)){
                length(quantNames())
            }
            
            cl <- if(get_os()=="windows"){
                parallel::makePSOCKcluster(as.numeric(my.cores))
            } else if(get_os()!="windows"){
                parallel::makeForkCluster(as.numeric(my.cores))
            }
            registerDoParallel(cl)
            cal.lm <- lapply(quantNames(),function(x) caret::train(Concentration~., data=rainforestDataMulti()[[x]][vals_multi$keeprows[[x]],, drop=FALSE], method="rf", type="Regression",
            trControl=trainControl(method=input$foresttrain_multi, number=input$forestnumber_multi), ntree=input$foresttrees_multi,
            prox=TRUE,allowParallel=TRUE, metric=input$forestmetric_multi, na.action=na.omit, importance=TRUE, trim=TRUE))
            stopCluster(cl)
            names(cal.lm) <- quantNames()
            cal.lm
            
            
        })



        predictFramePreMultiMulti <- reactive({
            

            
            concentration <- lapply(holdFrameMulti(), function(x) as.vector(x[,"Concentration"]))
            names(concentration) <- quantNames()
            
            intensity <- lapply(holdFrameMulti(), function(x) as.vector(x[,input$calcurveelement_multi]))
            names(intensity) <- quantNames()

            predict.frame <- lapply(quantNames(),function(x) data.frame(Concentration=concentration[[x]], Intensity==intensity[[x]]))
            names(predict.frame) <- quantNames()

            #data.frame(concentration, intensity)
            #colnames(predict.frame) <- c("Concentration", "Intensity")
            
            
            predict.frame
            
            
        })
        


        predictFrameMulti <- reactive({
            
            if (input$radiocal_multi==1){
                predictFrameSimpMulti()
            } else if(input$radiocal_multi==2){
                predictFrameSimpMulti()
            } else if(input$radiocal_multi==3){
                predictFrameLucMulti()
            } else if(input$radiocal_multi==4){
                predictFrameForestMulti()
            } else if(input$radiocal_multi==5){
                rainforestDataMulti()
            }
            
        })
        
        predictIntensityMulti <- reactive({
            
            
            if (input$radiocal_multi==1){
                predictIntensitySimpMulti()
            } else if(input$radiocal_multi==2){
                predictIntensitySimpMulti()
            } else if(input$radiocal_multi==3){
                predictIntensityLucMulti()
            } else if(input$radiocal_multi==4){
                predictIntensityForestMulti()
            } else if(input$radiocal_multi==5){
                rainforestIntensityMulti()
            }

            
        })
        
        output$moretesting <- renderDataTable({
            predictIntensityMulti()[[1]]
            
        })
        
        
        
        
        
        ####Machine Learning: Slopes
        
        caretSlopeMulti <- reactive({
            
            element <- input$calcurveelement_multi
            
            # prepare simple test suite
            control <- trainControl(method=input$foresttrain_multi, number=input$forestmetric_multi)
            seed <- 7
            metric <- input$forestmetric_multi
            set.seed(seed)
            
            data <- dataNormMulti()
            concentration.table <- concentrationTableMulti()
            
            concentration.table <- lapply(quantNames(), function(x)  concentration.table[[x]][complete.cases(concentration.table[[x]][input$calcurveelement_multi]),])
            names(concentration.table) <- quantNames()
            
            
            
            #spectra.line.table <- spectraLineTable()[spectraLineTable()$Spectrum %in% holdFrame()$Spectrum, ]
            
            spectra.line.table <- lapply(quantNames(), function(x) spectraLineTableMulti()[[x]][spectraLineTableMulti()[[x]]$Spectrum %in% concentration.table[[x]]$Spectrum, ])
            names(spectra.line.table) <- quantNames()
            
            #spectra.line.table <- spectraLineTable()[complete.cases(concentration.table[, element]),]
            
            data <- lapply(quantNames(), function(x) data[[x]][data[[x]]$Spectrum %in% concentration.table[[x]]$Spectrum, ])
            names(data) <- quantNames()
            
            
            cal.table <- predictFrameMulti()
            
            #cal.table <- cal.table[,!colnames(cal.table) %in% "Intensity"]
            #for(i in 1:length(quantNames())){
            #cal.table[[i]]$Concentration <- concentration.table[[i]][,input$calcurveelement_multi]
            #}
            
            cor.mod <- if(length(quantNames())>as.numeric(my.cores)){
                as.numeric(my.cores)
            } else if(length(quantNames())<=as.numeric(my.cores)){
                length(quantNames())
            }
            
            cl <- if(get_os()=="windows"){
                parallel::makePSOCKcluster(as.numeric(my.cores))
            } else if(get_os()!="windows"){
                parallel::makeForkCluster(as.numeric(my.cores))
            }
            registerDoParallel(cl)
            train_model <- lapply(quantNames(), function(x) caret::train(Concentration~., data=cal.table[[x]][,-1], method="rf", metric=metric, trControl=control, allowParallel=TRUE, prox=TRUE, importance=TRUE, trim=TRUE))
            
            stopCluster(cl)
            names(train_model) <- quantNames()
            train_model
            
        })
        
        
        slopeImportanceMulti <- reactive({
            
            forest.imp <- lapply(quantNames(), function(x) varImp(forestModelMulti()[[x]], scale=FALSE)$importance)
            names(forest.imp) <- quantNames()
            
            for(i in 1:length(quantNames())){
                colnames(forest.imp[[i]]) <- "Importance"
            }
            
            for(i in 1:length(quantNames())){
                forest.imp[[i]]$Element <- rownames(forest.imp[[i]])
            }
            
            for(i in 1:length(quantNames())){
                forest.imp[[i]]$Instrument <- rep(quantNames()[[i]], length(forest.imp[[i]][,1]))
            }
            
            do.call("rbind", forest.imp)
            
        })
        
        slopeImportancePlotMulti <- reactive({
            
            ggplot(slopeImportanceMulti(), aes(reorder(Element, Importance), Importance, fill=Instrument)) +
            geom_bar(stat="identity", position="dodge") +
            theme_light() +
            coord_flip() +
            scale_x_discrete("Element", breaks=scales::pretty_breaks()) +
            scale_y_discrete(breaks=scales::pretty_breaks())
            
        })
        
        
        rainForestImportanceMulti <- reactive({
            
            
            result <- lapply(quantNames(), function(x) as.data.frame(varImp(elementModelMulti()[[x]], scale=FALSE)$importance))
            names(result) <- quantNames()
            result
            
        })
        
        
        importanceFrameMulti <- reactive({
            
            importance.frame <- rainForestImportanceMulti()
            
            importance.frame <- lapply(quantNames(), function(x) data.frame(
            Importance=importance.frame[[x]][,1],
            Energy=as.numeric(gsub("X", "", rownames(importance.frame[[x]]))),
            Instrument=rep(x, length(importance.frame[[x]][,1]))
            )
            )
            names(importance.frame) <- quantNames()

            do.call("rbind", importance.frame)
            
        })
        
        
        
        importanceranges_multi <- reactiveValues(x = NULL, y = NULL)
        
        
        
        
        
        
        
        rainForestImportancePlotMulti <- reactive({
            
            importance.frame <- importanceFrameMulti()
            
            
            element <- datasetInputVarMulti()
            intensity.norm <- (element$Intensity/max(element$Intensity))*max(importance.frame$Importance)
            intensity.base <- (element$Intensity/max(element$Intensity))
            
            ggplot(importance.frame) +
            geom_line(aes(Energy, Importance, colour=Instrument, lty=Instrument)) +
            geom_segment(data=element, aes(x=Line, xend=Line, y = 0, yend=intensity.norm), colour="grey50", linetype=2)  +
            theme_light() +
            scale_x_continuous("Energy (keV)", breaks=scales::pretty_breaks()) +
            scale_y_continuous(paste0(input$calcurveelement_multi, " Importance"), breaks=scales::pretty_breaks()) +
            coord_cartesian(xlim = importanceranges_multi$x, ylim = importanceranges_multi$y, expand = TRUE)

        })
        
        # When a double-click happens, check if there's a brush on the plot.
        # If so, zoom to the brush bounds; if not, reset the zoom.
        observeEvent(input$cropvar_multi, {
            brush <- input$plot_var_brush_multi
            if (!is.null(brush)) {
                importanceranges_multi$x <- c(brush$xmin, brush$xmax)
                importanceranges_multi$y <- c(brush$ymin, brush$ymax)
                
            } else {
                importanceranges_multi$x <- NULL
                importanceranges_multi$y <- NULL
            }
            
            
            
        })
        
        variablesPlotMulti <- reactive({
            
            if(calTypeMulti()!=5){
                plot(slopeImportancePlotMulti())
            } else if(calTypeMulti()==5){
                rainForestImportancePlotMulti()
            }
            
        })
        
        
        
        
        output$varelementui_multi <- renderUI({
            
            selectInput(
            "elementvar_multi", "Element:",
            choices=c("(Ne) Neon" = "Ne",
            "(Na) Sodium" = "Na",
            "(Mg) Magnesium" = "Mg",
            "(Al) Aluminum" = "Al",
            "(Si) Silicon" = "Si",
            "(P)  Phosphorous" = "P",
            "(S)  Sulfur" = "S",
            "(Cl) Chlorine" = "Cl",
            "(Ar) Argon" = "Ar",
            "(K)  Potassium" = "K",
            "(Ca) Calcium" = "Ca",
            "(Sc) Scandium" = "Sc",
            "(Ti) Titanium" = "Ti",
            "(Cr) Chromium" = "Cr",
            "(Mn) Manganese" = "Mn",
            "(Fe) Iron" = "Fe",
            "(Co) Cobalt" = "Co",
            "(Ni) Nickel" = "Ni",
            "(Cu) Copper" = "Cu",
            "(Zn) Zinc"= "Zn",
            "(Ga) Gallium" = "Ga",
            "(Ge) Germanium" = "Ge",
            "(As) Arsenic" = "As",
            "(Se) Selenium" = "Se",
            "(Br) Bromium" = "Br",
            "(Kr) Krypton" = "Kr",
            "(Rb) Rubidium" = "Rb",
            "(Sr) Strontium" = "Sr",
            "(Y)  Yttrium" = "Y",
            "(Zr) Zirconium" = "Zr",
            "(Nb) Niobium" = "Nb",
            "(Mo) Molybdenum" = "Mo",
            "(Tc) Technicium" = "Tc",
            "(Ru) Ruthenium" = "Ru",
            "(Rh) Rhodium" = "Rh",
            "(Pd) Paladium" = "Pd",
            "(Ag) Silver" = "Ag",
            "(Cd) Cadmium" = "Cd",
            "(In) Indium" = "In",
            "(Sn) Tin" = "Sn",
            "(Sb) Antimony" = "Sb",
            "(Te) Tellerium" = "Te",
            "(I) Iodine" = "I",
            "(Xe) Xenon" = "Xe",
            "(Cs) Cesium" = "Cs",
            "(Ba) Barium" = "Ba",
            "(Ce) Cerium" = "Ce",
            "(Pr) Praeseodymeum" = "Pr",
            "(Nd) Neodymeum" = "Nd",
            "(Pr) Promethium" = "Pr",
            "(Sm) Samarium" = "Sm",
            "(Eu) Europium" = "Eu",
            "(Gd) Gadolinium" = "Gd",
            "(Tb) Terbium" = "Tb",
            "(Dy) Dysprosium" = "Dy",
            "(Ho) Holmium" = "Ho",
            "(Er) Erbium" = "Er",
            "(Tm) Thullium" = "Tm",
            "(Yb) Ytterbium" = "Yb",
            "(Lu) Lutetium" = "Lu",
            "(Hf) Halfnium" = "Hf",
            "(Ta) Tantalum" = "Ta",
            "(W)  Tungsten" = "W",
            "(Re) Rhenium" = "Re",
            "(Os) Osmium" = "Os",
            "(Ir) Irridium" = "Ir",
            "(Pt) Platinum" = "Pt",
            "(Au) Gold" = "Au",
            "(Tl) Thallium" = "Tl",
            "(Pb) Lead" = "Pb",
            "(Bi) Bismuth" = "Bi",
            "(Po) Polonium" = "Po",
            "(At) Astatine" = "At",
            "(Rn) Radon" = "Rn",
            "(Fr) Francium" = "Fr",
            "(Ra) Radium" = "Ra",
            "(Ac) Actinum" = "Ac",
            "(Th) Thorium" = "Th",
            "(Pa) Proactinum" = "Pa",
            "(U)  Uranium" = "U"),
            selected=strsplit(x=input$calcurveelement_multi, split="\\.")[[1]][1])
            
        })
        
        
        
        # Return the requested dataset
        datasetInputVarMulti <- reactive({
            switch(input$elementvar_multi,
            "H" = H.table,
            "He" = He.table,
            "Li" = Li.table,
            "Be" = Be.table,
            "B" = B.table,
            "C" = C.table,
            "N" = N.table,
            "O" = O.table,
            "F" = F.table,
            "Ne" = Ne.table,
            "Na" = Na.table,
            "Mg" = Mg.table,
            "Al" = Al.table,
            "Si" = Si.table,
            "P" = P.table,
            "S" = S.table,
            "Cl" = Cl.table,
            "Ar" = Ar.table,
            "K" = K.table,
            "Ca" = Ca.table,
            "Sc" = Sc.table,
            "Ti" = Ti.table,
            "V" = V.table,
            "Cr" = Cr.table,
            "Mn" = Mn.table,
            "Fe" = Fe.table,
            "Co" = Co.table,
            "Ni" = Ni.table,
            "Cu" = Cu.table,
            "Zn" = Zn.table,
            "Ga" = Ga.table,
            "Ge" = Ge.table,
            "As" = As.table,
            "Se" = Se.table,
            "Br" = Br.table,
            "Kr" = Kr.table,
            "Rb" = Rb.table,
            "Sr" = Sr.table,
            "Y" = Y.table,
            "Zr" = Zr.table,
            "Nb" = Nb.table,
            "Mo" = Mo.table,
            "Tc" = Tc.table,
            "Ru" = Ru.table,
            "Rh" = Rh.table,
            "Pd" = Pd.table,
            "Ag" = Ag.table,
            "Cd" = Cd.table,
            "In" = In.table,
            "Sn" = Sn.table,
            "Sb" = Sb.table,
            "Te" = Te.table,
            "I" = I.table,
            "Xe" = Xe.table,
            "Cs" = Cs.table,
            "Ba" = Ba.table,
            "La" = La.table,
            "Ce" = Ce.table,
            "Pr" = Pr.table,
            "Nd" = Nd.table,
            "Pm" = Pm.table,
            "Sm" = Sm.table,
            "Eu" = Eu.table,
            "Gd" = Gd.table,
            "Tb" = Tb.table,
            "Dy" = Dy.table,
            "Ho" = Ho.table,
            "Er" = Er.table,
            "Tm" = Tm.table,
            "Yb" = Yb.table,
            "Lu" = Lu.table,
            "Hf" = Hf.table,
            "Ta" = Ta.table,
            "W" = W.table,
            "Re" = Re.table,
            "Os" = Os.table,
            "Ir" = Ir.table,
            "Pt" = Pt.table,
            "Au" = Au.table,
            "Hg" = Hg.table,
            "Tl" = Tl.table,
            "Pb" = Pb.table,
            "Bi" = Bi.table,
            "Po" = Po.table,
            "At" = At.table,
            "Rn" = Rn.table,
            "Fr" = Fr.table,
            "Ra" = Ra.table,
            "Ac" = Ac.table,
            "Th" = Th.table,
            "Pa" = Pa.table,
            "U" = U.table)
        })
        
        
        output$importanceplot_multi <- renderPlot({
            
            variablesPlotMulti()
            
        })
        
        
        output$hover_info_variable_multi <- renderUI({
            if(calTypeMulti()==5){
                
                point.table <- importanceFrameMulti()
                
                hover <- input$plot_hover_variable_multi
                point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
                if (nrow(point) == 0) return(NULL)
                
                # calculate point position INSIDE the image as percent of total dimensions
                # from left (horizontal) and from top (vertical)
                left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
                top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
                
                # calculate distance from left and bottom side of the picture in pixels
                left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
                top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
                
                
                # create style property fot tooltip
                # background color is set so tooltip is a bit transparent
                # z-index is set so we are sure are tooltip will be on top
                style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                "left:", left_px + 2, "px; top:", top_px + 2, "px;")
                
                # actual tooltip created as wellPanel
                wellPanel(
                style = style,
                p(HTML(paste0("Energy:", " ", round(point$Energy, 2)))),
                p(HTML(paste0("Importance:", " ", round(point$Importance, 1)))),
                p(HTML(paste0("Instrument:", " ",point$Instrument, 1)))
                )
            } else if(calTypeMulti()!=5){
                NULL
            }
        })
        
        output$variablePlot_multi <- downloadHandler(
        filename = function() { paste0(input$calname_multi, "_", input$calcurveelemenet_multi , '_Variables', '.tiff', sep='') },
        content = function(file) {
            ggsave(file,variablesPlotMulti(), width=plotDimensions()[1], height=plotDimensions()[2], device="tiff", compression="lzw",  dpi=300)
        }
        )
        
 
        
        
        calCurveFrameMulti <- reactive({
            

            
            cal.frame <- predictFrameMulti()
            
            cal.frame <- lapply(quantNames(),function(x) data.frame(cal.frame[[x]], Instrument=x))
            names(cal.frame) <- quantNames()
            
            do.call("rbind", cal.frame)
            
            
        })
        
        
        
        elementModelMulti <- reactive({
            
            cal.lm <- if(input$radiocal_multi==1){
                simpleLinearModelMulti()
            } else if(input$radiocal_multi==2){
                nonLinearModelMulti()
            } else if(input$radiocal_multi==3){
                lucasToothModelMulti()
            } else if(input$radiocal_multi==4){
                forestModelMulti()
            } else if(input$radiocal_multi==5){
                rainforestModelMulti()
            }
            
            names(cal.lm) <- quantNames()

            
            cal.lm
            
        })
        
        
        
        
        valFrameMulti <- reactive({
            
            

            
            predict.intensity <- predictIntensityMulti()
            predict.frame <- predictFrameMulti()
            element.model <- elementModelMulti()
            
            
            if (input$radiocal_multi==1){
                cal.est.conc.pred <- lapply(quantNames(),function(x) predict(object=element.model[[x]], newdata=predict.frame[[x]], interval='confidence'))
                names(cal.est.conc.pred) <- quantNames()
                cal.est.conc.tab <- lapply(cal.est.conc.pred, data.frame)
                names(cal.est.conc.tab) <- quantNames()
                cal.est.conc <- lapply(cal.est.conc.tab, function(x) as.vector(x[,"fit"]))
                names(cal.est.conc) <- quantNames()
                
                
                val.frame <- lapply(quantNames(),function(x) data.frame(Concentration=predict.frame[[x]][,"Concentration"], Prediction=cal.est.conc[[x]]))
                names(val.frame) <- quantNames()
                
                #data.frame(predict.frame$Concentration, cal.est.conc)
                #colnames(val.frame) <- c("Concentration", "Prediction")
            }
            
            if (input$radiocal_multi==2){
                cal.est.conc.pred <- lapply(quantNames(),function(x) predict(object=element.model[[x]], newdata=predict.frame[[x]], interval='confidence'))
                names(cal.est.conc.pred) <- quantNames()
                cal.est.conc.tab <- lapply(cal.est.conc.pred, data.frame)
                names(cal.est.conc.tab) <- quantNames()
                cal.est.conc <- lapply(cal.est.conc.tab, function(x) as.vector(x[,"fit"]))
                names(cal.est.conc) <- quantNames()
                
                
                val.frame <- lapply(quantNames(),function(x) data.frame(Concentration=predict.frame[[x]][,"Concentration"], Prediction=cal.est.conc[[x]]))
                names(val.frame) <- quantNames()
                
                #data.frame(predict.frame$Concentration, cal.est.conc)
                #colnames(val.frame) <- c("Concentration", "Prediction")
            }
            
            if (input$radiocal_multi==3){
        
                
                cal.est.conc.pred.luc <- lapply(quantNames(),function(x) predict(object=element.model[[x]], newdata=predict.intensity[[x]], interval='confidence'))
                names(cal.est.conc.pred.luc) <- quantNames()
                cal.est.conc.tab <- lapply(cal.est.conc.pred.luc, data.frame)
                names(cal.est.conc.tab) <- quantNames()
                cal.est.conc.luc <- lapply(cal.est.conc.tab, function(x) as.vector(x[,"fit"]))
                names(cal.est.conc.luc) <- quantNames()
                cal.est.conc.luc.up <- lapply(cal.est.conc.tab, function(x) as.vector(x[,"upr"]))
                names(cal.est.conc.luc.up) <- quantNames()
                cal.est.conc.luc.low <- lapply(cal.est.conc.tab, function(x) as.vector(x[,"lwr"]))
                names(cal.est.conc.luc.low) <- quantNames()

                
                val.frame <- lapply(quantNames(),function(x)
                data.frame(
                Concentration=predict.frame[[x]][,"Concentration"],
                IntensityOrg=predict.intensity[[x]],
                Intensity=cal.est.conc.luc[[x]],
                Prediction=cal.est.conc.luc[[x]],
                Upper=cal.est.conc.luc.up[[x]],
                Lower=cal.est.conc.luc.low[[x]]
                ))
                names(val.frame) <- quantNames()
                
                
                #val.frame <- data.frame(predict.frame$Concentration, predict.intensity$Intensity, lucas.x, cal.est.conc.luc, cal.est.conc.luc.up, cal.est.conc.luc.low)
                #colnames(val.frame) <- c("Concentration", "Intensity", "Intensity", "Prediction", "Upper", "Lower")
            }
            
            
            
            
        
            
            if (input$radiocal_multi==4){

                
                cal.est.conc.pred.luc <- pblapply(quantNames(),function(x) predict(object=element.model[[x]], newdata=predict.intensity[[x]]), cl=my.cores)
                names(cal.est.conc.pred.luc) <- quantNames()
                cal.est.conc.luc <- lapply(cal.est.conc.pred.luc, function(x) as.vector(x))
                names(cal.est.conc.luc) <- quantNames()

                
                val.frame <- pblapply(quantNames(),function(x)
                data.frame(
                Concentration=predict.frame[[x]][,"Concentration"],
                IntensityOrg=predict.intensity[[x]],
                Intensity=cal.est.conc.luc[[x]],
                Prediction=cal.est.conc.luc[[x]]
                ), cl=my.cores)
                names(val.frame) <- quantNames()
                
                
                #val.frame <- data.frame(predict.frame$Concentration, predict.intensity$Intensity, lucas.x, cal.est.conc.luc, cal.est.conc.luc.up, cal.est.conc.luc.low)
                #colnames(val.frame) <- c("Concentration", "Intensity", "Intensity", "Prediction", "Upper", "Lower")
            }
            
            
            if (input$radiocal_multi==5){

                
                cal.est.conc.pred.luc <- pblapply(quantNames(),function(x) predict(object=element.model[[x]], newdata=predict.intensity[[x]]), cl=my.cores)
                names(cal.est.conc.pred.luc) <- quantNames()
                cal.est.conc.luc <- lapply(cal.est.conc.pred.luc, function(x) as.vector(x))
                names(cal.est.conc.luc) <- quantNames()
                
                
                val.frame <- pblapply(quantNames(),function(x)
                data.frame(
                Concentration=na.omit(predict.frame[[x]][,"Concentration"]),
                Intensity=cal.est.conc.luc[[x]],
                Prediction=cal.est.conc.luc[[x]]
                ), cl=my.cores)
                names(val.frame) <- quantNames()
                
                
                #val.frame <- data.frame(predict.frame$Concentration, predict.intensity$Intensity, lucas.x, cal.est.conc.luc, cal.est.conc.luc.up, cal.est.conc.luc.low)
                #colnames(val.frame) <- c("Concentration", "Intensity", "Intensity", "Prediction", "Upper", "Lower")
            }
            
            
            
            val.frame <- lapply(quantNames(),function(x) data.frame(val.frame[[x]], Instrument=x))
            names(val.frame) <- quantNames()
            do.call("rbind", val.frame)
            
            #val.frame$Instrument <- holdFrameMulti()$Instrument
            
            
        })
        

        
        
        calValFrameMulti <- reactive({
            
            valFrameMulti()
            
        })
        
        
        rangescalcurve_multi <- reactiveValues(x = NULL, y = NULL)
        
        output$tabletest <- renderDataTable({
            as.data.frame(calCurveFrameMulti())
            
        })
        
        calCurvePlotMulti <- reactive({
            
            predict.frame <- calCurveFrameMulti()
            element.model <- elementModelMulti()[[input$defaultcal]]
            val.frame <- valFrameMulti()
            
            element.name <- if(input$calcurveelement_multi %in% spectralLines){
                gsub("[.]", "", substr(input$calcurveelement_multi, 1, 2))
            } else {
                input$calcurveelement_multi
            }
            
            
            intens <- " Counts per Second"
            norma <- " Normalized"
            norma.comp <- " Compton Normalized"
            norma.tc <- " Valid Counts Normalized"
            conen <- paste0(" ", input$plotunitmulti)
            predi <- paste0(" Estimate ", input$plotunitmulti)
            log <- "Log "
            
            
            intensity.name <- c(element.name, intens)
            concentration.name <- c(element.name, conen)
            prediction.name <- c(element.name, predi)
            
            
            if(input$radiocal_multi==1){
                calcurve.plot <- ggplot(data=predict.frame[ unlist(vals_multi$keeprows), , drop = FALSE], aes(Intensity, Concentration, colour=Instrument, shape=Instrument)) +
                theme_light() +
                annotate("text", label=lm_eqn(lm(Concentration~Intensity, predict.frame[ unlist(unlist(vals_multi$keeprows), use.names=FALSE), , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                geom_point() +
                geom_point(data = predict.frame[!unlist(vals_multi$keeprows), , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
                stat_smooth(method="lm", fullrange = TRUE, aes(fill=Instrument), alpha=0.1) +
                scale_x_continuous(paste(element.name, intens), breaks=scales::pretty_breaks()) +
                scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                coord_cartesian(xlim = rangescalcurve_multi$x, ylim = rangescalcurve_multi$y, expand = TRUE)
                
            }
            
            if(input$radiocal_multi==2){
                calcurve.plot <- ggplot(data=predict.frame[ unlist(vals_multi$keeprows), , drop = FALSE], aes(Intensity, Concentration, colour=Instrument, shape=Instrument)) +
                theme_light() +
                annotate("text", label=lm_eqn_poly(lm(Concentration~Intensity + I(Intensity^2), predict.frame[ unlist(vals_multi$keeprows), , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                geom_point() +
                geom_point(data = predict.frame[!unlist(vals_multi$keeprows), , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
                stat_smooth(method="lm", formula=y~poly(x,2), aes(fill=Instrument), alpha=0.1) +
                scale_x_continuous(paste(element.name, intens), breaks=scales::pretty_breaks()) +
                scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                coord_cartesian(xlim = rangescalcurve_multi$x, ylim = rangescalcurve_multi$y, expand = TRUE)
                
            }
            
            if(input$radiocal_multi==3){
                calcurve.plot <- ggplot(data=val.frame[ unlist(vals_multi$keeprows), , drop = FALSE], aes(Intensity, Concentration, colour=Instrument, shape=Instrument)) +
                theme_light() +
                annotate("text", label=lm_eqn(lm(Concentration~., val.frame[ unlist(vals_multi$keeprows), , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                geom_point() +
                geom_point(aes(Intensity, Concentration), data = val.frame[!unlist(vals_multi$keeprows), , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
                geom_smooth(aes(x=Intensity, y=Concentration, ymin = Lower, ymax = Upper, fill=Instrument), alpha=0.1) +
                scale_x_continuous(paste(element.name, norma), breaks=scales::pretty_breaks()) +
                scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                coord_cartesian(xlim = rangescalcurve_multi$x, ylim = rangescalcurve_multi$y, expand = TRUE)
                
            }
            
            
            if(input$radiocal_multi==4){
                calcurve.plot <- ggplot(data=val.frame[ unlist(vals_multi$keeprows), , drop = FALSE], aes(Intensity, Concentration, colour=Instrument, shape=Instrument)) +
                theme_light() +
                annotate("text", label=lm_eqn(lm(Concentration~., val.frame[ unlist(vals_multi$keeprows), , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                geom_point() +
                geom_point(aes(Intensity, Concentration), data = val.frame[!unlist(vals_multi$keeprows), , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
                geom_smooth(alpha=0.1) +
                scale_x_continuous(paste(element.name, norma), breaks=scales::pretty_breaks()) +
                scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                coord_cartesian(xlim = rangescalcurve_multi$x, ylim = rangescalcurve_multi$y, expand = TRUE)
                
            }
            
            
            if(input$radiocal_multi==5){
                calcurve.plot <- ggplot(data=val.frame[ unlist(vals_multi$keeprows), , drop = FALSE], aes(Intensity, Concentration, colour=Instrument, shape=Instrument)) +
                theme_light() +
                annotate("text", label=lm_eqn(lm(Concentration~., val.frame[ unlist(vals_multi$keeprows), , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                geom_point() +
                geom_point(aes(Intensity, Concentration), data = val.frame[!unlist(vals_multi$keeprows), , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
                geom_smooth(alpha=0.1) +
                scale_x_continuous(paste(element.name, norma), breaks=scales::pretty_breaks()) +
                scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                coord_cartesian(xlim = rangescalcurve_multi$x, ylim = rangescalcurve_multi$y, expand = TRUE)
                
            }
            
            
            
            calcurve.plot
            
            
        })
        
        
        
        observeEvent(input$cropcalmulti, {
            brush <- input$plot_cal_brush_multi
            if (!is.null(brush)) {
                rangescalcurve_multi$x <- c(brush$xmin, brush$xmax)
                rangescalcurve_multi$y <- c(brush$ymin, brush$ymax)
                
            } else {
                rangescalcurve_multi$x <- NULL
                rangescalcurve_multi$y <- NULL
            }
        })
        
        observeEvent(input$zerocalmulti, {
            
            predict.frame <- if(calTypeMulti()==1){
                calCurveFrameMulti()
            } else if(calTypeMulti()==2){
                calCurveFrameMulti()
            } else if(calTypeMulti()==3) {
                valFrameMulti()
            } else if(calTypeMulti()==5) {
                valFrameMulti()
            }

            rangescalcurve_multi$x <- c(0, max(predict.frame$Intensity))
            rangescalcurve_multi$y <- c(0, max(predict.frame$Concentration))
        })
        
        
        output$calcurveplots_multi <- renderPlot({
            calCurvePlotMulti()
        })
        
        
        rangesvalcurve_multi <- reactiveValues(x = NULL, y = NULL)
        
        
        valCurvePlotMulti <- reactive({
            
            predict.intensity <- predictIntensityMulti()
            predict.frame <- predictFrameMulti()
            element.model <- elementModelMulti()[[input$defaultcal]]
            
            
            
            element.name <- if(input$calcurveelement_multi %in% spectralLines){
                gsub("[.]", "", substr(input$calcurveelement_multi, 1, 2))
            } else {
                input$calcurveelement_multi
            }
            
            intens <- " Counts per Second"
            norma <- " Normalized"
            norma.comp <- " Compton Normalized"
            norma.tc <- " Valid Counts Normalized"
            conen <- paste0(" ", input$plotunitmulti)
            predi <- paste0(" Estimate ", input$plotunitmulti)
            log <- "Log "
            
            intensity.name <- c(element.name, intens)
            concentration.name <- c(element.name, conen)
            prediction.name <- c(element.name, predi)
            val.frame <- valFrameMulti()
            
            
            valcurve.plot <- ggplot(data=val.frame[ unlist(vals_multi$keeprows), , drop = FALSE], aes(Prediction, Concentration, colour=Instrument, shape=Instrument)) +
            theme_light() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame[ unlist(vals_multi$keeprows), , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_abline(intercept=0, slope=1, lty=2) +
            stat_smooth(method="lm", aes(fill=Instrument), alpha=0.1) +
            geom_point() +
            geom_point(aes(Prediction, Concentration),  data = val.frame[!unlist(vals_multi$keeprows), , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
            scale_x_continuous(paste(element.name, predi), breaks=scales::pretty_breaks()) +
            scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
            coord_cartesian(xlim = rangesvalcurve_multi$x, ylim = rangesvalcurve_multi$y, expand = TRUE)
            
            
            
            
            
            valcurve.plot
            
        })
        
        
        observeEvent(input$cropvalmulti, {
            brush <- input$plot_val_brush_multi
            if (!is.null(brush)) {
                rangesvalcurve_multi$x <- c(brush$xmin, brush$xmax)
                rangesvalcurve_multi$y <- c(brush$ymin, brush$ymax)
                
            } else {
                rangesvalcurve_multi$x <- NULL
                rangesvalcurve_multi$y <- NULL
            }
        })
        
        observeEvent(input$zerovalmulti, {
            
            predict.frame <- valFrameMulti()
            
            
            rangesvalcurve_multi$x <- c(0, max(predict.frame$Prediction))
            rangesvalcurve_multi$y <- c(0, max(predict.frame$Concentration))
        })
        
        
        output$valcurveplots_multi <- renderPlot({
            valCurvePlotMulti()
        })
        
        

        
        
        calValTableMulti <- reactive({
            
            standard.table <- valFrameMulti()
            
            concentration.table <- do.call("rbind", holdFrameMulti())
            
            hold.table <- as.vector(concentration.table[,"Spectrum"])
            
            
            standard.table$Spectrum <- hold.table

            
            standard.table.summary <- data.frame(standard.table$Instrument, standard.table$Spectrum, standard.table$Concentration, standard.table$Prediction, standard.table$Concentration-standard.table$Prediction, ((standard.table$Concentration-standard.table$Prediction)/standard.table$Concentration))
            colnames(standard.table.summary) <- c("Instrument", "Standard", "Concentration", "Prediction", "Difference", "Relative")
            
            standard.table.summary[,3:length(standard.table.summary)] <-round(standard.table.summary[,3:length(standard.table.summary)],4)
            standard.table.summary[,5] <- as.character(percent(standard.table.summary[,5]))
            
            this.table <- DT::datatable(standard.table.summary)
            this.table
            
        })
        
        
        output$standardsperformance_multi <- DT::renderDataTable({
            
            
            standard.table <- if(input$switchmulti==FALSE){
                calValTableMulti()
            } else if(input$switchmulti==TRUE){
                calValTableRandomMulti()
            }
            
            standard.table
            
        }, options =list(aoColumnDefs = list(list(sClass="alignRight",aTargets=c(list(2), list(3),list(4),list(5))))  ))
        
        
        randomizeDataMulti <- reactive({
            
            cal.frame <- holdFrameMulti()[[input$defaultcal]]
            cal.frame <- cal.frame[ vals_multi$keeprows[[input$defaultcal]], , drop = FALSE]
            total.number <- length(cal.frame[,1])
            sample.number <- total.number-round(input$percentrandom_multi*total.number, 0)
            
            hold <- cal.frame[sample(nrow(cal.frame), sample.number),]
            cal.frame$Spectrum %in% hold$Spectrum
            
        })
        
        
        
        standardNamesRandomized <- reactive({
            
            cal.frame <- spectraLineTableMulti()[[input$defaultcal]]
            cal.frame <- cal.frame[ vals_multi$keeprows[[input$defaultcal]], , drop = FALSE]
            total.number <- length(cal.frame[,1])
            sample.number <- total.number-round(input$percentrandom_multi*total.number, 0)
            
            hold <- cal.frame[randomizeDataMulti(),]
            cal.frame$Spectrum[cal.frame$Spectrum %in% hold$Spectrum]
            
        })
        
        
        
        holdFrameRandomMulti <- reactive({
            
            predict.frame <- holdFrameMulti()
            
            
            
            
            predict.frame <- lapply(quantNames(),function(x) as.data.frame(predict.frame[[x]][ vals_multi$keeprows[[x]], ]))
            names(predict.frame) <- quantNames()
            
            predict.frame <- lapply(quantNames(),function(x) as.data.frame(predict.frame[[x]][!randomizeDataMulti(),]))
            names(predict.frame) <- quantNames()
            
            
            predict.frame
            
        })

        
        
        
        predictFrameRandomMulti <- reactive({
            
            predict.frame <- predictFrameMulti()
            

            
            
            predict.frame <- lapply(quantNames(),function(x) as.data.frame(predict.frame[[x]][ vals_multi$keeprows[[x]], ]))
            names(predict.frame) <- quantNames()
            
            predict.frame <- lapply(quantNames(),function(x) as.data.frame(predict.frame[[x]][randomizeDataMulti(),]))
            names(predict.frame) <- quantNames()
            
            
            predict.frame
            
        })

        
        calCurveFrameRandomizedMulti <- reactive({
            

            
            predict.frame <- predictFrameRandomMulti()
            
            predict.frame <- lapply(quantNames(),function(x) data.frame(predict.frame[[x]], Instrument=x))
            names(predict.frame) <- quantNames()
            
            
            do.call("rbind", predict.frame)

            
        })
        

        
        
        elementModelRandomMulti <- reactive({
            
            predict.frame <- predictFrameRandomMulti()
            
            

            
            predict.list <- predict.frame
            
            
            
            if (input$radiocal_multi==1){
                cal.lm <- lapply(quantNames(),function(x) lm(Concentration~Intensity, data=predict.list[[x]]))
            }
            
            
            if (input$radiocal_multi==2){
                cal.lm <- lapply(quantNames(), function(x) lm(Concentration~Intensity + I(Intensity^2), data=predict.list[[x]]))
            }
            
            if (input$radiocal_multi==3){
                cal.lm <- lapply(quantNames(),function(x) lm(Concentration~., data=predict.list[[x]]))
            }
            
            if (input$radiocal_multi==4){
                #cal.lm <- lapply(quantNames(),function(x) randomForest(Concentration~., data=predict.list[[x]], na.action=na.omit))
                cor.mod <- if(length(quantNames())>as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else if(length(quantNames())<=as.numeric(my.cores)){
                    length(quantNames())
                }
                
                cl <- if(get_os()=="windows"){
                    parallel::makePSOCKcluster(as.numeric(my.cores))
                } else if(get_os()!="windows"){
                    parallel::makeForkCluster(as.numeric(my.cores))
                }
                registerDoParallel(cl)
                cal.lm <- lapply(quantNames(),function(x) caret::train(Concentration~., data=predict.list[[x]][ vals_multi$keeprows[[x]], ,drop = FALSE], method="rf", type="Regression",
                trControl=trainControl(method=input$foresttrain_multi, number=input$forestnumber_multi), ntree=input$foresttrees_multi,
                prox=TRUE, metric=input$forestmetric_multi, allowParallel=TRUE, na.action=na.omit, importance=TRUE, trim=TRUE))
                stopCluster
            }
            
            if (input$radiocal_multi==5){
                #cal.lm <- lapply(quantNames(),function(x) randomForest(Concentration~., data=predict.list[[x]], na.action=na.omit))
                cor.mod <- if(length(quantNames())>as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else if(length(quantNames())<=as.numeric(my.cores)){
                    length(quantNames())
                }
                
                cl <- if(get_os()=="windows"){
                    parallel::makePSOCKcluster(as.numeric(my.cores))
                } else if(get_os()!="windows"){
                    parallel::makeForkCluster(as.numeric(my.cores))
                }
                registerDoParallel(cl)
                cal.lm <- lapply(quantNames(),function(x) caret::train(Concentration~., data=predict.list[[x]][ vals_multi$keeprows[[x]], ,drop = FALSE], method="rf", type="Regression",
                trControl=trainControl(method=input$foresttrain_multi, number=input$forestnumber_multi), ntree=input$foresttrees_multi,
                prox=TRUE, allowParallel=TRUE, metric=input$forestmetric_multi,  na.action=na.omit, importance=TRUE, trim=TRUE))
                stopCluster
            }
            
            names(cal.lm) <- quantNames()
            
            
            cal.lm
            
        })
        
        
        
        
        valFrameRandomizedMulti <- reactive({
            
            concentration.table.rev <- predictFrameRandomMulti()
            


            
            predict.intensity <- lapply(quantNames(),function(x) data.frame(predictIntensityMulti()[[x]][ vals_multi$keeprows[[x]], , drop = FALSE]))
            names(predict.intensity) <- quantNames()

            
            predict.frame <- lapply(quantNames(),function(x) data.frame(predictFrameMulti()[[x]][ vals_multi$keeprows[[x]], , drop = FALSE]))
            names(predict.frame) <- quantNames()

            
            predict.intensity <- lapply(quantNames(),function(x) data.frame( predict.intensity[[x]][!(randomizeDataMulti()), , drop = FALSE]))
            names(predict.intensity) <- quantNames()

            
            predict.frame <- lapply(quantNames(),function(x) data.frame( predict.frame[[x]][!(randomizeDataMulti()), , drop = FALSE]))
            names(predict.frame) <- quantNames()
            

            element.model <- elementModelRandomMulti()
            
            
            
            if (input$radiocal_multi==1){
                cal.est.conc.pred <- lapply(quantNames(),function(x) predict(object=element.model[[x]], newdata=predict.frame[[x]], interval='confidence'))
                names(cal.est.conc.pred) <- quantNames()

                
                cal.est.conc.tab <- lapply(cal.est.conc.pred, data.frame)
                names(cal.est.conc.tab) <- quantNames()

                cal.est.conc <- lapply(cal.est.conc.tab, function(x) as.vector(x[,"fit"]))
                names(cal.est.conc) <- quantNames()


                val.frame <- lapply(quantNames(),function(x) data.frame(Concentration=predict.frame[[x]][,"Concentration"], Prediction=cal.est.conc[[x]]))
                names(val.frame) <- quantNames()
            }
            
            if (input$radiocal_multi==2){
                cal.est.conc.pred <- lapply(quantNames(),function(x) predict(object=element.model[[x]], newdata=predict.frame[[x]], interval='confidence'))
                names(cal.est.conc.pred) <- quantNames()
                
                
                cal.est.conc.tab <- lapply(cal.est.conc.pred, data.frame)
                names(cal.est.conc.tab) <- quantNames()
                
                cal.est.conc <- lapply(cal.est.conc.tab, function(x) as.vector(x[,"fit"]))
                names(cal.est.conc) <- quantNames()
                
                
                val.frame <- lapply(quantNames(),function(x) data.frame(Concentration=predict.frame[[x]][,"Concentration"], Prediction=cal.est.conc[[x]]))
                names(val.frame) <- quantNames()
            }
            
            if (input$radiocal_multi==3){

                cal.est.conc.pred.luc <- lapply(quantNames(),function(x) predict(object=element.model[[x]], newdata=predict.intensity[[x]], interval='confidence'))
                names(cal.est.conc.pred.luc) <- quantNames()

                cal.est.conc.tab <- lapply(cal.est.conc.pred.luc, data.frame)
                names(cal.est.conc.tab) <- quantNames()

                cal.est.conc.luc <- lapply(cal.est.conc.tab, function(x) as.vector(x[,"fit"]))
                names(cal.est.conc.luc) <- quantNames()

                cal.est.conc.luc.up <- lapply(cal.est.conc.tab, function(x) as.vector(x[,"upr"]))
                names(cal.est.conc.luc.up) <- quantNames()

                cal.est.conc.luc.low <- lapply(cal.est.conc.tab, function(x) as.vector(x[,"lwr"]))
                names(cal.est.conc.luc.low) <- quantNames()

                
                val.frame <- lapply(quantNames(),function(x)
                data.frame(
                Concentration=predict.frame[[x]][,"Concentration"],
                Intensity=predict.intensity[[x]],
                Intensity=cal.est.conc.luc[[x]],
                Prediction=cal.est.conc.luc[[x]],
                Upper=cal.est.conc.luc.up[[x]],
                Lower=cal.est.conc.luc.low[[x]]
                ))
                
                names(val.frame) <- quantNames()
            }
            
            
            
            if (input$radiocal_multi==4){

                
                cal.est.conc.pred.luc <- lapply(quantNames(),function(x) predict(object=element.model[[x]], newdata=predict.intensity[[x]]))
                names(cal.est.conc.pred.luc) <- quantNames()
                
                cal.est.conc.luc <- lapply(cal.est.conc.pred.luc, function(x) as.vector(x))
                names(cal.est.conc.luc) <- quantNames()


                
                
                val.frame <- pblapply(quantNames(),function(x)
                data.frame(
                Concentration=na.omit(predict.frame[[x]])[,"Concentration"],
                Intensity=predict.intensity[[x]],
                Intensity=cal.est.conc.luc[[x]],
                Prediction=cal.est.conc.luc[[x]]
                ), cl=my.cores)
                
                names(val.frame) <- quantNames()
            }
            
            
            
            if (input$radiocal_multi==5){

                
                cal.est.conc.pred.luc <- lapply(quantNames(),function(x) predict(object=element.model[[x]], newdata=predict.intensity[[x]]))
                names(cal.est.conc.pred.luc) <- quantNames()
                
                cal.est.conc.luc <- lapply(cal.est.conc.pred.luc, function(x) as.vector(x))
                names(cal.est.conc.luc) <- quantNames()
                
                
                
                
                val.frame <- pblapply(quantNames(),function(x)
                data.frame(
                Concentration=na.omit(predict.frame[[x]])[,"Concentration"],
                Intensity=cal.est.conc.luc[[x]],
                Prediction=cal.est.conc.luc[[x]]
                ), cl=my.cores)
                
                names(val.frame) <- quantNames()
            }
            
            
            
            
            val.frame <- lapply(quantNames(),function(x) data.frame(val.frame[[x]], Instrument=x))
            names(val.frame) <- quantNames()
            
            val.frame <- lapply(quantNames(), function(x) as.data.frame(
            val.frame[[x]][val.frame[[x]][, "Concentration"] > min(concentration.table.rev[[x]][,"Concentration"], na.rm = TRUE) & val.frame[[x]][, "Concentration"] < max(concentration.table.rev[[x]][,"Concentration"], na.rm = TRUE), ]))
            names(val.frame) <- quantNames()

            
            do.call("rbind", val.frame)

        })
        
        
        
        valFrameRandomizedRevMulti <- reactive({
            

            
            
            predict.intensity <- lapply(quantNames(),function(x) data.frame(predictIntensityMulti()[[x]][ vals_multi$keeprows[[x]], , drop = FALSE]))
            names(predict.intensity) <- quantNames()

            
            predict.frame <- lapply(quantNames(),function(x) data.frame(predictFrameMulti()[[x]][ vals_multi$keeprows[[x]], , drop = FALSE]))
            names(predict.frame) <- quantNames()

            
            predict.intensity <- lapply(quantNames(),function(x) data.frame( predict.intensity[[x]][(randomizeDataMulti()), , drop = FALSE]))
            names(predict.intensity) <- quantNames()

            predict.frame <- lapply(quantNames(),function(x) data.frame( predict.frame[[x]][(randomizeDataMulti()), , drop = FALSE]))
            names(predict.frame) <- quantNames()

            element.model <- elementModelRandomMulti()
            
            
            
            if (input$radiocal_multi==1){
                cal.est.conc.pred <- lapply(quantNames(),function(x) predict(object=element.model[[x]], newdata=predict.intensity[[x]], interval='confidence'))
                names(cal.est.conc.pred) <- quantNames()

                cal.est.conc.tab <- lapply(cal.est.conc.pred, data.frame)
                names(cal.est.conc.tab) <- quantNames()

                cal.est.conc <- lapply(cal.est.conc.tab, function(x) as.vector(x[,"fit"]))
                names(cal.est.conc) <- quantNames()

                
                val.frame <- lapply(quantNames(),function(x) data.frame(Concentration=predict.frame[[x]][,"Concentration"], Prediction=cal.est.conc[[x]]))
                names(val.frame) <- quantNames()
            }
            
            if (input$radiocal_multi==2){
                cal.est.conc.pred <- lapply(quantNames(),function(x) predict(object=element.model[[x]], newdata=predict.intensity[[x]], interval='confidence'))
                names(cal.est.conc.pred) <- quantNames()
                
                cal.est.conc.tab <- lapply(cal.est.conc.pred, data.frame)
                names(cal.est.conc.tab) <- quantNames()
                
                cal.est.conc <- lapply(cal.est.conc.tab, function(x) as.vector(x[,"fit"]))
                names(cal.est.conc) <- quantNames()
                
                
                val.frame <- lapply(quantNames(),function(x) data.frame(Concentration=predict.frame[[x]][,"Concentration"], Prediction=cal.est.conc[[x]]))
                names(val.frame) <- quantNames()
            }
            
            if (input$radiocal_multi==3){
  
                
                cal.est.conc.pred.luc <- lapply(quantNames(),function(x) predict(object=element.model[[x]], newdata=predict.intensity[[x]], interval='confidence'))
                names(cal.est.conc.pred.luc) <- quantNames()

                cal.est.conc.tab <- lapply(cal.est.conc.pred.luc, data.frame)
                names(cal.est.conc.tab) <- quantNames()

                cal.est.conc.luc <- lapply(cal.est.conc.tab, function(x) as.vector(x[,"fit"]))
                names(cal.est.conc.luc) <- quantNames()

                cal.est.conc.luc.up <- lapply(cal.est.conc.tab, function(x) as.vector(x[,"upr"]))
                names(cal.est.conc.luc.up) <- quantNames()

                cal.est.conc.luc.low <- lapply(cal.est.conc.tab, function(x) as.vector(x[,"lwr"]))
                names(cal.est.conc.luc.low) <- quantNames()

                
                val.frame <- lapply(quantNames(),function(x)
                data.frame(
                Concentration=predict.frame[[x]][,"Concentration"],
                Intensity=predict.intensity[[x]],
                Intensity=cal.est.conc.luc[[x]],
                Prediction=cal.est.conc.luc[[x]],
                Upper=cal.est.conc.luc.up[[x]],
                Lower=cal.est.conc.luc.low[[x]]
                ))
                
                names(val.frame) <- quantNames()
            }
            
            
            if (input$radiocal_multi==4){

                
                cal.est.conc.pred.luc <- lapply(quantNames(),function(x) predict(object=element.model[[x]], newdata=predict.intensity[[x]]))
                names(cal.est.conc.pred.luc) <- quantNames()
                
                cal.est.conc.luc <- lapply(cal.est.conc.pred.luc, function(x) as.vector(x))
                names(cal.est.conc.luc) <- quantNames()


                
                
                val.frame <- lapply(quantNames(),function(x)
                data.frame(
                Concentration=predict.frame[[x]][,"Concentration"],
                Intensity=predict.intensity[[x]],
                Intensity=cal.est.conc.luc[[x]],
                Prediction=cal.est.conc.luc[[x]]
                ))
                
                names(val.frame) <- quantNames()
            }
            
            
            if (input$radiocal_multi==5){
                
                cal.est.conc.pred.luc <- lapply(quantNames(),function(x) predict(object=element.model[[x]], newdata=predict.intensity[[x]], interval='confidence'))
                names(cal.est.conc.pred.luc) <- quantNames()
                
                cal.est.conc.luc <- lapply(cal.est.conc.pred.luc, function(x) as.vector(x))
                names(cal.est.conc.luc) <- quantNames()
                
                
                
                
                val.frame <- lapply(quantNames(),function(x)
                data.frame(
                Concentration=na.omit(predict.frame[[x]])[,"Concentration"],
                Intensity=cal.est.conc.luc[[x]],
                Prediction=cal.est.conc.luc[[x]]
                ))
                
                names(val.frame) <- quantNames()
            }
            
            
            
            
            val.frame <- lapply(quantNames(),function(x) data.frame(val.frame[[x]], Instrument=x))
            names(val.frame) <- quantNames()
            do.call("rbind", val.frame)
            
        })
        
        
        rangescalcurverandom_multi <- reactiveValues(x = NULL, y = NULL)
        
        
        calCurvePlotRandomMulti <- reactive({
            
            predict.frame <- calCurveFrameRandomizedMulti()
            element.model <- elementModelRandomMulti()[[input$defaultcal]]
            
            
            
            element.name <- if(input$calcurveelement_multi %in% spectralLines){
                gsub("[.]", "", substr(input$calcurveelement_multi, 1, 2))
            } else {
                input$calcurveelement_multi
            }


            intens <- " Counts per Second"
            norma <- " Normalized"
            norma.comp <- " Compton Normalized"
            norma.tc <- " Valid Counts Normalized"
            conen <- paste0(" ", input$plotunitmulti)
            predi <- paste0(" Estimate ", input$plotunitmulti)
            
            intensity.name <- c(element.name, intens)
            concentration.name <- c(element.name, conen)
            prediction.name <- c(element.name, predi)
            
            
            if(input$radiocal_multi==1){
                calcurve.plot <- ggplot(data=predict.frame, aes(Intensity, Concentration, colour=Instrument, shape=Instrument)) +
                theme_light() +
                annotate("text", label=lm_eqn(element.model), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                stat_smooth(method="lm", fullrange = TRUE, aes(fill=Instrument), alpha=0.1) +
                geom_point() +
                scale_x_continuous(paste(element.name, intens), breaks=scales::pretty_breaks()) +
                scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                coord_cartesian(xlim = rangescalcurverandom_multi$x, ylim = rangescalcurverandom_multi$y, expand = TRUE)
                
            }
            
            if(input$radiocal_multi==2){
                
                calcurve.plot <- ggplot(data=predict.frame, aes(Intensity, Concentration, colour=Instrument, shape=Instrument)) +
                theme_light() +
                annotate("text", label=lm_eqn_poly(element.model), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                stat_smooth(method="lm", fullrange = TRUE, aes(fill=Instrument), alpha=0.1) +
                geom_point() +
                scale_x_continuous(paste(element.name, intens), breaks=scales::pretty_breaks()) +
                scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                coord_cartesian(xlim = rangescalcurverandom_multi$x, ylim = rangescalcurverandom_multi$y, expand = TRUE)
            }
            
            if(input$radiocal_multi==3){
                val.frame <- valFrameRandomizedRevMulti()

                calcurve.plot <- ggplot(data=val.frame, aes(Intensity, Concentration, colour=Instrument, shape=Instrument)) +
                theme_light() +
                annotate("text", label=lm_eqn(element.model), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                geom_smooth(aes(x=Intensity, y=Concentration, ymin = Lower, ymax = Upper, fill=Instrument), alpha=0.1) +
                geom_point() +
                scale_x_continuous(paste(element.name, norma), breaks=scales::pretty_breaks()) +
                scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                coord_cartesian(xlim = rangescalcurverandom_multi$x, ylim = rangescalcurverandom_multi$y, expand = TRUE)
            }
            
            
            if(input$radiocal_multi==4){
                val.frame <- valFrameRandomizedRevMulti()
                
                calcurve.plot <- ggplot(data=val.frame, aes(Intensity, Concentration, colour=Instrument, shape=Instrument)) +
                theme_light() +
                annotate("text", label=lm_eqn(lm(Concentration~., val.frame)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                geom_smooth(alpha=0.1) +
                geom_point() +
                scale_x_continuous(paste(element.name, norma), breaks=scales::pretty_breaks()) +
                scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                coord_cartesian(xlim = rangescalcurverandom_multi$x, ylim = rangescalcurverandom_multi$y, expand = TRUE)
            }
            
            
            if(input$radiocal_multi==5){
                val.frame <- valFrameRandomizedRevMulti()
                
                calcurve.plot <- ggplot(data=val.frame, aes(Intensity, Concentration, colour=Instrument, shape=Instrument)) +
                theme_light() +
                annotate("text", label=lm_eqn(lm(Concentration~., val.frame)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                geom_smooth(alpha=0.1) +
                geom_point() +
                scale_x_continuous(paste(element.name, norma), breaks=scales::pretty_breaks()) +
                scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                coord_cartesian(xlim = rangescalcurverandom_multi$x, ylim = rangescalcurverandom_multi$y, expand = TRUE)
            }
            
            calcurve.plot
            
            
        })
        
        
        
        calValTableRandomMulti <- reactive({
            
            standard.table <- valFrameRandomizedMulti()
            
            cal.frame <- spectraLineTableMulti()[[input$defaultcal]]
            cal.frame <- cal.frame[ vals_multi$keeprows[[input$defaultcal]], , drop = FALSE]
            total.number <- length(cal.frame[,1])
            sample.number <- total.number-round(input$percentrandom_multi*total.number, 0)
            
            
            concentration.table.rev <- predictFrameRandomMulti()
            
            
            predict.frame <- lapply(quantNames(),function(x) data.frame(holdFrameMulti()[[x]][ vals_multi$keeprows[[x]], , drop = FALSE]))
            names(predict.frame) <- quantNames()
            
            
            predict.frame <- lapply(quantNames(),function(x) data.frame( predict.frame[[x]][!(randomizeDataMulti()), , drop = FALSE]))
            names(predict.frame) <- quantNames()
            
            predict.frame <- lapply(quantNames(), function(x) as.data.frame(
            predict.frame[[x]][predict.frame[[x]][, "Concentration"] > min(concentration.table.rev[[x]][,"Concentration"], na.rm = TRUE) & predict.frame[[x]][, "Concentration"] < max(concentration.table.rev[[x]][,"Concentration"], na.rm = TRUE), ]))
            names(predict.frame) <- quantNames()
            
            hold.table <- do.call("rbind", predict.frame)
            
            
            standard.table$Spectrum <- hold.table[,"Spectrum"]
            
            standard.table.summary <- data.frame(standard.table$Instrument, standard.table$Spectrum, standard.table$Concentration, standard.table$Prediction, standard.table$Concentration-standard.table$Prediction, ((standard.table$Concentration-standard.table$Prediction)/standard.table$Concentration))
            colnames(standard.table.summary) <- c("Instrument", "Standard", "Concentration", "Prediction", "Difference", "Relative")
            
            standard.table.summary[,3:length(standard.table.summary)] <-round(standard.table.summary[,3:length(standard.table.summary)],4)
            standard.table.summary[,5] <- as.character(percent(standard.table.summary[,5]))
            
            this.table <- DT::datatable(standard.table.summary)
            this.table
            
        })
        
        
        observeEvent(input$cropcalmultirandom, {
            brush <- input$plot_cal_brush_random_multi
            if (!is.null(brush)) {
                rangescalcurverandom_multi$x <- c(brush$xmin, brush$xmax)
                rangescalcurverandom_multi$y <- c(brush$ymin, brush$ymax)
                
            } else {
                rangescalcurverandom_multi$x <- NULL
                rangescalcurverandom_multi$y <- NULL
            }
        })
        
        observeEvent(input$zerocalmultirandom, {
            
            predict.frame <- if(calTypeMulti()==1){
                calCurveFrameRandomizedMulti()
            } else if(calTypeMulti()==2){
                calCurveFrameRandomizedMulti()
            } else if(calTypeMulti()==3) {
                valFrameRandomizedMulti()
            } else if(calTypeMulti()==5) {
                valFrameRandomizedMulti()
            }
            
            rangescalcurverandom_multi$x <- c(0, max(predict.frame$Intensity))
            rangescalcurverandom_multi$y <- c(0, max(predict.frame$Concentration))
        })
        
        
        output$calcurveplotsrandom_multi <- renderPlot({
            calCurvePlotRandomMulti()
        })
        
        
        rangesvalcurverandom_multi <- reactiveValues(x = NULL, y = NULL)
        
        
        valCurvePlotRandomMulti <- reactive({
            
            
            
            element.name <- if(input$calcurveelement_multi %in% spectralLines){
                gsub("[.]", "", substr(input$calcurveelement_multi, 1, 2))
            } else {
                input$calcurveelement_multi
            }

            intens <- " Counts per Second"
            norma <- " Normalized"
            norma.comp <- " Compton Normalized"
            norma.tc <- " Valid Counts Normalized"
            conen <- paste0(" ", input$plotunitmulti)
            predi <- paste0(" Estimate ", input$plotunitmulti)
            
            intensity.name <- c(element.name, intens)
            concentration.name <- c(element.name, conen)
            prediction.name <- c(element.name, predi)
            
            val.frame <- valFrameRandomizedMulti()
            
            valcurve.plot <- ggplot(data=val.frame, aes(Prediction, Concentration, colour=Instrument, shape=Instrument)) +
            theme_light() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_abline(intercept=0, slope=1, lty=2) +
            stat_smooth(method="lm", aes(fill=Instrument), alpha=0.1) +
            geom_point() +
            scale_x_continuous(paste(element.name, predi), breaks=scales::pretty_breaks()) +
            scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
            coord_cartesian(xlim = rangesvalcurverandom_multi$x, ylim = rangesvalcurverandom_multi$y, expand = TRUE)
            
            valcurve.plot
            
        })
        
        observeEvent(input$cropvalmultirandom, {
            brush <- input$plot_val_brush_random_multi
            if (!is.null(brush)) {
                rangesvalcurverandom_multi$x <- c(brush$xmin, brush$xmax)
                rangesvalcurverandom_multi$y <- c(brush$ymin, brush$ymax)
                
            } else {
                rangesvalcurverandom_multi$x <- NULL
                rangesvalcurverandom_multi$y <- NULL
            }
        })
        
        observeEvent(input$zerocalmultirandom, {
            
            predict.frame <- valFrameRandomizedMulti()
            
            
            rangesvalcurverandom_multi$x <- c(0, max(predict.frame$Prediction))
            rangesvalcurverandom_multi$y <- c(0, max(predict.frame$Concentration))
        })
        
        output$valcurveplotsrandom_multi <- renderPlot({
            valCurvePlotRandomMulti()
        })
        
        
        
        ####CalCurves
        
        # Float over info
        output$hover_infocal_multi <- renderUI({
            
            point.table <- if(calTypeMulti()==1){
                calCurveFrameMulti()
            } else if(calTypeMulti()==2){
                calCurveFrameMulti()
            } else if(calTypeMulti()==3) {
                calValFrameMulti()
            } else if(calTypeMulti()==5) {
                calValFrameMulti()
            }
            
            
            concentration.table <- do.call("rbind", holdFrameMulti())
            hold.table <- concentration.table[,c("Spectrum", "Concentration")]
            hold.table$Concentration[hold.table$Concentration==""] <- NA
            hold.table <- hold.table[complete.cases(hold.table), ]
            hold.table <- hold.table[!is.na(hold.table$Concentration), ]
            hold.table <- na.omit(hold.table)
            #hold.table <- as.vector(concentration.table[,"Spectrum"])
            
            
            point.table$Spectrum <- hold.table[,"Spectrum"]

            
            
            
            #point.table$Spectrum <- hold.table["Spectrum"]
            
            hover <- input$plot_hovercal_multi
            point <- nearPoints(point.table,  coordinfo=hover, xvar="Intensity", yvar="Concentration",  threshold = 5, maxpoints = 1, addDist = TRUE)
            if (nrow(point) == 0) return(NULL)
            
            
            
            
            # calculate point position INSIDE the image as percent of total dimensions
            # from left (horizontal) and from top (vertical)
            left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
            top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
            
            # calculate distance from left and bottom side of the picture in pixels
            left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
            top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
            
            
            # create style property fot tooltip
            # background color is set so tooltip is a bit transparent
            # z-index is set so we are sure are tooltip will be on top
            style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
            "left:", left_px + 2, "px; top:", top_px + 2, "px;")
            
            # actual tooltip created as wellPanel
            wellPanel(
            style = style,
            p(HTML(paste0(point$Instrument))),
            p(HTML(paste0(point$Spectrum)))

            )
        })
        
        
        
        # Float over info
        output$hover_infocal_random_multi <- renderUI({
            

            
            point.table <- if(calTypeMulti()==1){
                calCurveFrameRandomizedMulti()
            } else if(calTypeMulti()==2){
                calCurveFrameRandomizedMulti()
            } else if(calTypeMulti()==3) {
                valFrameRandomizedRevMulti()
            } else if(calTypeMulti()==5) {
                valFrameRandomizedRevMulti()
            }
            
            
            
            
            predict.frame <- lapply(quantNames(),function(x) data.frame(holdFrameMulti()[[x]][ vals_multi$keeprows[[x]], ]))
            names(predict.frame) <- quantNames()
            
            
            predict.frame <- lapply(quantNames(),function(x) data.frame( predict.frame[[x]][(randomizeDataMulti()), ]))
            names(predict.frame) <- quantNames()
            
           
            
            concentration.table <- do.call("rbind", na.omit(predict.frame))
            hold.table <- concentration.table[,c("Spectrum", "Concentration")]
            hold.table$Concentration[hold.table$Concentration==""] <- NA
            hold.table <- hold.table[complete.cases(hold.table), ]
            hold.table <- hold.table[!is.na(hold.table$Concentration), ]
            hold.table <- na.omit(hold.table)
            #hold.table <- as.vector(concentration.table[,"Spectrum"])
            
            
            point.table$Spectrum <- hold.table[,"Spectrum"]

            
            
            
            #point.table$Spectrum <- hold.table["Spectrum"]
            
            hover <- input$plot_hovercal_random_multi
            point <- nearPoints(point.table,  coordinfo=hover, xvar="Intensity", yvar="Concentration",  threshold = 5, maxpoints = 1, addDist = TRUE)
            if (nrow(point) == 0) return(NULL)
            
            
            
            
            # calculate point position INSIDE the image as percent of total dimensions
            # from left (horizontal) and from top (vertical)
            left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
            top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
            
            # calculate distance from left and bottom side of the picture in pixels
            left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
            top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
            
            
            # create style property fot tooltip
            # background color is set so tooltip is a bit transparent
            # z-index is set so we are sure are tooltip will be on top
            style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
            "left:", left_px + 2, "px; top:", top_px + 2, "px;")
            
            # actual tooltip created as wellPanel
            wellPanel(
            style = style,
            p(HTML(paste0(point$Instrument))),
            p(HTML(paste0(point$Spectrum)))
            
            )
        })
        
        
        
        # Toggle points that are clicked
        observeEvent(input$plot_cal_click_multi, {
            
            predict.frame <- if(calTypeMulti()==1){
                calCurveFrameMulti()
            } else if(calTypeMulti()==2){
                calCurveFrameMulti()
            } else if(calTypeMulti()==3) {
                calValFrameMulti()
            } else if(calTypeMulti()==5) {
                calValFrameMulti()
            }
            
            res <- nearPoints(predict.frame, input$plot_cal_click_multi, xvar="Intensity", yvar="Concentration", allRows = TRUE)
            
            temprows <- xor(unlist(vals_multi$keeprows), res$selected_)
            
            vals_multi$keeprows <- relist(flesh=temprows, skeleton=vals_multi$keeprows)
        })
        
        
        
        # Toggle points that are brushed, when button is clicked
        observeEvent(input$exclude_toggle_multi, {
            
            predict.frame <- if(calTypeMulti()==1){
                calCurveFrameMulti()
            } else if(calTypeMulti()==2){
                calCurveFrameMulti()
            } else if(calTypeMulti()==3) {
                calValFrameMulti()
            } else if(calTypeMulti()==5) {
                calValFrameMulti()
            }
            res <- brushedPoints(predict.frame, input$plot_cal_brush_multi, xvar="Intensity", yvar="Concentration", allRows = TRUE)
            
            temprows <- xor(unlist(vals_multi$keeprows), res$selected_)
            
            vals_multi$keeprows <- relist(flesh=temprows, skeleton=vals_multi$keeprows)        })
        
        
        
        # Reset all points
        observeEvent(input$exclude_reset_multi, {
            
            predict.frame <- if(calTypeMulti()==1){
                calCurveFrameMulti()
            } else if(calTypeMulti()==2){
                calCurveFrameMulti()
            } else if(calTypeMulti()==3) {
                calValFrameMulti()
            } else if(calTypeMulti()==5) {
                calValFrameMulti()
            }
            vals_multi$keeprows <- calFileStandardsMulti()
        })
        
        
        
        # Reset all points on element change
        observeEvent(input$calcurveelement_multi, {
            
            
            
            vals_multi$keeprows <- calFileStandardsMulti()
            
        })
        
        
        
        ####ValCurves Multiplot
        
        
        
        
        # Float over info
        output$hover_infoval_multi <- renderUI({
            
            
            point.table <- valFrameMulti()
            concentration.table.rev <- predictFrameRandomMulti()

            
            concentration.table <- do.call("rbind", holdFrameMulti())
            
            hold.table <- concentration.table[,c("Spectrum", "Concentration")]
            hold.table$Concentration[hold.table$Concentration==""] <- NA
            hold.table <- hold.table[complete.cases(hold.table), ]
            hold.table <- hold.table[!is.na(hold.table$Concentration), ]
            hold.table <- na.omit(hold.table)
            #hold.table <- as.vector(concentration.table[,"Spectrum"])
            
            
            point.table$Spectrum <- hold.table[,"Spectrum"]

            
            
            hover <- input$plot_hoverval_multi
            point <- nearPoints(point.table,  coordinfo=hover, xvar="Prediction", yvar="Concentration",  threshold = 5, maxpoints = 1, addDist = TRUE)
            if (nrow(point) == 0) return(NULL)
            
            
            
            
            # calculate point position INSIDE the image as percent of total dimensions
            # from left (horizontal) and from top (vertical)
            left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
            top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
            
            # calculate distance from left and bottom side of the picture in pixels
            left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
            top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
            
            
            # create style property fot tooltip
            # background color is set so tooltip is a bit transparent
            # z-index is set so we are sure are tooltip will be on top
            style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
            "left:", left_px + 2, "px; top:", top_px + 2, "px;")
            
            # actual tooltip created as wellPanel
            wellPanel(
            style = style,
            p(HTML(paste0(point$Instrument))),
            p(HTML(paste0(point$Spectrum)))
            
            )
        })
        
        
        
        
        output$hover_infoval_random_multi <- renderUI({
            
            point.table <- valFrameRandomizedMulti()
            
            
            
            
            predict.frame <- lapply(quantNames(),function(x) data.frame(holdFrameMulti()[[x]][ vals_multi$keeprows[[x]], ]))
            names(predict.frame) <- quantNames()
            
            
            predict.frame <- lapply(quantNames(),function(x) data.frame( predict.frame[[x]][(randomizeDataMulti()), ]))
            names(predict.frame) <- quantNames()
            
            
            
            concentration.table <- do.call("rbind", na.omit(holdFrameRandomMulti()))
            hold.table <- concentration.table[,c("Spectrum", "Concentration")]
            hold.table$Concentration[hold.table$Concentration==""] <- NA
            hold.table <- hold.table[complete.cases(hold.table), ]
            hold.table <- hold.table[!is.na(hold.table$Concentration), ]
            hold.table <- na.omit(hold.table)
            #hold.table <- as.vector(concentration.table[,"Spectrum"])
            
            
            point.table$Spectrum <- hold.table[,"Spectrum"]
            
            
            hover <- input$plot_hoverval_random_multi
            point <- nearPoints(point.table,  coordinfo=hover, xvar="Prediction", yvar="Concentration",  threshold = 5, maxpoints = 1, addDist = TRUE)
            if (nrow(point) == 0) return(NULL)
            
            
            
            
            # calculate point position INSIDE the image as percent of total dimensions
            # from left (horizontal) and from top (vertical)
            left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
            top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
            
            # calculate distance from left and bottom side of the picture in pixels
            left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
            top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
            
            
            # create style property fot tooltip
            # background color is set so tooltip is a bit transparent
            # z-index is set so we are sure are tooltip will be on top
            style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
            "left:", left_px + 2, "px; top:", top_px + 2, "px;")
            
            # actual tooltip created as wellPanel
            wellPanel(
            style = style,
            p(HTML(paste0(point$Instrument))),
            p(HTML(paste0(point$Spectrum)))
            
            )
        })
        
        
        
        # Toggle points that are clicked
        observeEvent(input$plot_val_click_multi, {
            
            predict.frame <- calValFrameMulti()
            
            res <- nearPoints(predict.frame, input$plot_val_click_multi, xvar="Prediction", yvar="Concentration", allRows = TRUE)
            
            temprows <- xor(unlist(vals_multi$keeprows), res$selected_)
            
            vals_multi$keeprows <- relist(flesh=temprows, skeleton=vals_multi$keeprows)        })
        
        
        
        # Toggle points that are brushed, when button is clicked
        observeEvent(input$exclude_toggle_multi, {
            predict.frame <- calValFrameMulti()
            
            res <- brushedPoints(predict.frame, input$plot_val_brush_multi, xvar="Prediction", yvar="Concentration",  allRows = TRUE)
            
            temprows <- xor(unlist(vals_multi$keeprows), res$selected_)
            
            vals_multi$keeprows <- relist(flesh=temprows, skeleton=vals_multi$keeprows)        })
        
        
        
        # Reset all points
        observeEvent(input$exclude_reset_multi, {
            predict.frame <- calValFrameMulti()
            
            vals_multi$keeprows <- calFileStandardsMulti()
        })
        
        
        calPlotDownloadMulti <- reactive({
            
            if(input$switchmulti==FALSE){
                grid.arrange(calCurvePlotMulti(), valCurvePlotMulti(), ncol=2)
            }else if(input$switchmulti==TRUE){
                grid.arrange(calCurvePlotRandomMulti(), valCurvePlotRandomMulti(), ncol=2)
                
            }
            
            
        })
        
        plotDimensionsMulti <- reactive({
            
            if(input$imagesize_multi=="Small"){
                c(10, 4)
            } else if(input$imagesize_multi=="Large"){
                c(20, 8)
            }
            
        })
        
        
        output$downloadcloudplot_multi <- downloadHandler(
        filename = function() { paste(paste(c(input$calname, "_", input$calcurveelement_multi), collapse=''), '.tiff',  sep='') },
        content = function(file) {
            ggsave(file,calPlotDownloadMulti(), device="tiff", compression="lzw",  dpi=300, width=plotDimensionsMulti()[1], height=plotDimensionsMulti()[2])
        }
        )
        
        
        
        calPlotDownloadMulti_val <- reactive({

                grid.arrange(calCurvePlotRandomMulti(), valCurvePlotRandomMulti(), ncol=2)
                
            
            
            
        })
        
        
        output$downloadcloudplot_multi_val <- downloadHandler(
        filename = function() { paste(paste(c(input$calname, "_", input$calcurveelement_multi), collapse=''), '.tiff',  sep='') },
        content = function(file) {
            ggsave(file,calPlotDownloadMulti_val(), device="tiff", compression="lzw",  dpi=300, width=plotDimensionsMulti()[1], height=plotDimensionsMulti()[2])
        }
        )
        
        




calConditionsMulti <- reactiveValues()


observeEvent(input$createcalelement_multi, {
    
    cal.condition <- input$radiocal_multi
    norm.condition <- input$normcal_multi
    
    norm.min <- print(input$comptonmin_multi)
    norm.max <- print(input$comptonmax_multi)
    
    forestmetric <- if(input$radiocal_multi==4 | input$radiocal_multi==5){
        as.character(input$forestmetric_multi)
    } else if(input$radiocal_multi!=4 | input$radiocal_multi!=5){
        as.character("RMSE")
    }
    
    foresttrain <- if(input$radiocal_multi==4 | input$radiocal_multi==5){
        as.character(input$foresttrain_multi)
    } else if(input$radiocal_multi!=4 | input$radiocal_multi!=5){
        as.character("cv")
    }
    
    forestnumber <- if(input$radiocal_multi==4 | input$radiocal_multi==5){
        as.numeric(input$forestnumber_multi)
    } else if(input$radiocal!=4 | input$radiocal!=5){
        as.numeric(10)
    }
    
    foresttrees <- if(input$radiocal_multi==4 | input$radiocal_multi==5){
        as.numeric(input$foresttrees_multi)
    } else if(input$radiocal_multi!=4 | input$radiocal_multi!=5){
        as.numeric(15)
    }

    
    cal.table <- data.frame(cal.condition, norm.condition, norm.min, norm.max, forestmetric, foresttrain, forestnumber, foresttrees)
    colnames(cal.table) <- c("CalType", "NormType", "Min", "Max", "ForestMetric", "ForestTC", "ForestNumber", "ForestTrees")
    
    slope.corrections <- input$slope_vars_multi
    intercept.corrections <- input$intercept_vars_multi
    
    standards.used <- 1
    
    cal.mode.list <- list(cal.table, slope.corrections, intercept.corrections, standards.used)
    names(cal.mode.list) <- c("CalTable", "Slope", "Intercept", "StandardsUsed")
    
    calConditionsMulti <<- cal.mode.list
    
})




        


        
        observeEvent(input$createcalelement_multi, {
            
           lapply(quantNames(), function(x)
            calListMulti[[x]][["calList"]][[input$calcurveelement_multi]] <<- list(isolate(calConditionsMulti), isolate(strip_glm(elementModelMulti()[[x]]))))
            
            
            
        })

observeEvent(input$createcalelement_multi, {
    
    lapply(quantNames(), function(x)
           calListMulti[[x]][["calList"]][[input$calcurveelement_multi]][["CalTable"]][["StandardsUsed"]] <<- isolate(as.vector(vals_multi$keeprows[[x]])))
      
    
    
})



emptyList <- reactive({
    a.list <- list()
    a.list
})

calPlotListMulti <- reactiveValues()
calPlotListMulti <- emptyList()
observeEvent(input$createcalelement_multi, {
    
        calPlotListMulti[[input$calcurveelement_multi]] <- isolate(calPlotDownloadMulti())

    
    calPlotListMulti <<- calPlotListMulti
    
})

CalibrationPlotsMulti <- reactiveValues()
observeEvent(input$createcal_multi, {
    
    CalibrationPlotsMulti$calCurves <<- calPlotListMulti
    
    
})

#observeEvent(input$createcal, {

#CalibrationPlots$diagPlots <<- diagPlotList


#})



output$downloadModel_multi <- downloadHandler(
filename <- function(){
    paste(input$defaultcal, "quant", sep=".")
},

content = function(file) {
    saveRDS(calListMulti[[input$defaultcal]], file = file, compress="xz")
}
)


output$downloadReport_multi <- downloadHandler(
function() { paste("summarycurves", '.pdf',  sep='') },
content = function(file){
    ml = marrangeGrob(grobs=CalibrationPlotsMulti$calCurves, nrow=1, ncol=1)
    ggsave(file, ml, device="pdf", dpi=300, width=plotDimensionsMulti()[1], height=plotDimensionsMulti()[2])
    
    dev.off()
})






        

})
        
        
    })
    
    
    output$filevalgrab <- renderUI({
        
        if(input$valfiletype=="CSV") {
            fileInput('loadvaldata', 'Choose CSV', multiple=TRUE,
            accept=c(".csv"))
        } else if(input$valfiletype=="TXT") {
            fileInput('loadvaldata', 'Choose TXT', multiple=TRUE,
            accept=c(".txt"))
        } else if(input$valfiletype=="Net") {
            fileInput('loadvaldata', 'Choose Net Counts', multiple=TRUE,
            accept=c(".csv"))
        } else if(input$valfiletype=="Elio") {
            fileInput('loadvaldata', 'Choose Elio Spectra', multiple=TRUE,
            accept=c(".spt"))
        } else if(input$valfiletype=="MCA") {
            fileInput('loadvaldata', 'Choose MCA File', multiple=TRUE,
            accept=c(".mca"))
        } else if(input$valfiletype=="SPX") {
            fileInput('loadvaldata', 'Choose Artax File', multiple=TRUE,
            accept=c(".spx"))
        } else if(input$valfiletype=="PDZ") {
            fileInput('loadvaldata', 'Choose PDZ File', multiple=TRUE,
            accept=c(".pdz"))
        }
        
    })
    
    
    output$valfiletypeui <- renderUI({
        
        if(is.null(input$calfileinput2)){
            selectInput("valfiletype", label="Filetype", c("CSV", "TXT", "Net", "Elio", "MCA", "SPX", "PDZ"), selected="CSV")
        } else if(!is.null(input$calfileinput2)){
            selectInput("valfiletype", label="Filetype", c("CSV", "TXT", "Net", "Elio", "MCA", "SPX", "PDZ"), selected=calFileContents2()[["FileType"]])
        
        }
    
})
    
    calFileContents2 <- reactive({
        
        existingCalFile <- input$calfileinput2
        
        if (is.null(existingCalFile)) return(NULL)
        
        
        Calibration <- readRDS(existingCalFile$datapath)
        
        elements <- names(Calibration$calList)
        
        Calibration$calList <- lapply(elements, function(x) list(importCalConditions(element=x, calList=Calibration[["calList"]]), Calibration[["calList"]][[x]][[2]]))
        names(Calibration$calList) <- elements
        
        Calibration
        
    })
    
    

    
    
    observeEvent(input$processvalspectra, {
        
        fullValSpectra <- reactive({
            
            
            withProgress(message = 'Processing Data', value = 0, {
                
                inFile <- input$loadvaldata
                if (is.null(inFile)) return(NULL)
                temp = inFile$name
                temp <- gsub(".csv", "", temp)
                id.seq <- seq(1, 2048,1)
                
                n <- length(temp)*id.seq
                
                n.seq <- seq(1, length(inFile$name), 1)
                
                
                data <- pblapply(n.seq, function(x) csvFrame(filepath=inFile$datapath[x], filename=inFile$name[x]))
                data <- do.call("rbind", data)
                
                
                incProgress(1/n)
                Sys.sleep(0.1)
            })
            
            data$Energy <- data$Energy + gainshiftHold()
            
            data
        })
        
        readValTXT <- reactive({
            
            withProgress(message = 'Processing Data', value = 0, {
                
                inFile <- input$loadvaldata
                if (is.null(inFile)) return(NULL)
                
                n <- length(inFile$datapath)
                names <- inFile$name
                
                myfiles.frame <- as.data.frame(do.call(rbind, lapply(seq(1, n, 1), function(x) readTXTData(filepath=inFile$datapath[x], filename=inFile$name[x]))))
                
                incProgress(1/n)
                Sys.sleep(0.1)
            })
            
            myfiles.frame$Energy <- myfiles.frame$Energy + gainshiftHold()
            
            myfiles.frame
            
        })
        
        
        netValCounts <- reactive({
            
            withProgress(message = 'Processing Data', value = 0, {
                
                
                inFile <- input$loadvaldata
                if (is.null(inFile)) return(NULL)
                
                #inName <- inFile$name
                #inPath <- inFile$datapath
                
                #inList <- list(inName, inPath)
                #names(inList) <- c("inName", "inPath")
                
                
                n <- length(inFile$name)
                net.names <- gsub("\\@.*","",inFile$name)
                
                myfiles = pblapply(inFile$datapath,  read_csv_net)
                
                
                myfiles.frame.list <- pblapply(myfiles, data.frame, stringsAsFactors=FALSE)
                nms = unique(unlist(pblapply(myfiles.frame.list, names)))
                myfiles.frame <- as.data.frame(do.call(rbind, lapply(myfiles.frame.list, "[", nms)))
                myfiles.frame <- as.data.frame(sapply(myfiles.frame, as.numeric))
                
                
                #myfiles.frame$Spectrum <- net.names
                
                united.frame <- data.frame(net.names, myfiles.frame)
                colnames(united.frame) <- c("Spectrum", names(myfiles.frame))
                #united.frame$None <- rep(1, length(united.frame$Spectrum))
                
                
                incProgress(1/n)
                Sys.sleep(0.1)
            })
            
            united.frame <- as.data.frame(united.frame)
            united.frame
            
        })
        
        readValElio <- reactive({
            
            withProgress(message = 'Processing Data', value = 0, {
                
                inFile <- input$loadvaldata
                if (is.null(inFile)) return(NULL)
                
                n <- length(inFile$datapath)
                names <- inFile$name
                
                myfiles.frame <- as.data.frame(do.call(rbind, lapply(seq(1, n, 1), function(x) readSPTData(filepath=inFile$datapath[x], filename=inFile$name[x]))))
                
                
                incProgress(1/n)
                Sys.sleep(0.1)
            })
            
            
            myfiles.frame
            
            
        })
        
        readValMCA <- reactive({
            
            withProgress(message = 'Processing Data', value = 0, {
                
                inFile <- input$loadvaldata
                if (is.null(inFile)) return(NULL)
                
                n <- length(inFile$datapath)
                names <- inFile$name
                
                myfiles.frame <- as.data.frame(do.call(rbind, lapply(seq(1, n, 1), function(x) readMCAData(filepath=inFile$datapath[x], filename=inFile$name[x]))))
                
                
                incProgress(1/n)
                Sys.sleep(0.1)
            })
            
            
            myfiles.frame
            
            
        })
        
        readValSPX <- reactive({
            
            withProgress(message = 'Processing Data', value = 0, {
                
                inFile <- input$loadvaldata
                if (is.null(inFile)) return(NULL)
                
                n <- length(inFile$datapath)
                names <- inFile$name
                
                myfiles.frame <- as.data.frame(do.call(rbind, lapply(seq(1, n, 1), function(x) readSPXData(filepath=inFile$datapath[x], filename=inFile$name[x]))))
                
                
                incProgress(1/n)
                Sys.sleep(0.1)
            })
            
            myfiles.frame
            
            
        })
            
            
            readvalPDZ <- reactive({
                
                withProgress(message = 'Processing Data', value = 0, {
                    
                    inFile <- input$loadvaldata
                    if (is.null(inFile)) return(NULL)
                    
                    n <- length(inFile$datapath)
                    names <- inFile$name
                    
                    myfiles.frame <- as.data.frame(do.call(rbind, lapply(seq(1, n, 1), function(x) readPDZData(filepath=inFile$datapath[x], filename=inFile$name[x]))))
                    
                    
                    incProgress(1/n)
                    Sys.sleep(0.1)
                })
                
                myfiles.frame$Energy <- myfiles.frame$Energy + gainshiftHold()
                
                myfiles.frame
                
                
            })
            
            

        
        
        myValData <- reactive({
            
            data <- if(input$valfiletype=="CSV"){
                fullValSpectra()
            } else if(input$valfiletype=="TXT"){
                readValTXT()
            } else if(input$valfiletype=="Net"){
                netValCounts()
            } else if(input$valfiletype=="Elio") {
                readValElio()
            }  else if(input$valfiletype=="MCA") {
                readValMCA()
            }  else if(input$valfiletype=="SPX") {
                readValSPX()
            }  else if(input$valfiletype=="PDZ") {
                readvalPDZ()
            }
            
            data
            
        })
        
        
        
      
        
        valdata <- myValData()

        
        
        output$contents2 <- renderTable({
            
            
            
            myValData()
            
        })
        
        calValHold <- reactive({
            

            calFileContents2()[["calList"]]
            
            
            


        })
        
        calVariables <- reactive({
            

                calFileContents2()$Intensities

            
            
        })
        
        calValElements <- reactive({
            calList <- calValHold()
            valelements <- ls(calList)
            
            valelements.simp <- gsub(".K.alpha", "", valelements)
            valelements.simp <- gsub(".K.beta", "", valelements.simp)
            valelements.simp <- gsub(".L.alpha", "", valelements.simp)
            valelements.simp <- gsub(".L.beta", "", valelements.simp)
            valelements.simp <- gsub(".M.line", "", valelements.simp)

            
            valelements <- as.vector(as.character(valelements[match(as.character(fluorescence.lines$Symbol), valelements.simp)]))
            
            valelements <- c(valelements, ls(calList)[!(ls(calList) %in% valelements)])

            
            valelements
        })
        
        calVariableElements <- reactive({
            variables <- calVariables()
            variableelements <- ls(variables)
            
            #variableelements.simp <- gsub(".K.alpha", "", variableelements)
            #variableelements.simp <- gsub(".K.beta", "", variableelements)
            #variableelements.simp <- gsub(".L.alpha", "", variableelements)
            #variableelements.simp <- gsub(".L.beta", "", variableelements)
            #variableelements.simp <- gsub(".M.line", "", variableelements)
            
            #variableelements <- as.vector(as.character(na.omit(variableelements[match(as.character(fluorescence.lines$Symbol), variableelements.simp)])))

            variableelements
        })
        
        
        
        calDefinitions <- reactive({
            
            if(!is.null(calFileContents2()[["Definitions"]])){
                calFileContents2()[["Definitions"]]
            } else if(is.null(calFileContents2()[["Definitions"]])){
                NULL
            }
            
        })
        
        valDataType <- reactive({
            
            if(input$valfiletype=="CSV"){
                "Spectra"
            } else if(input$valfiletype=="TXT"){
                "Spectra"
            } else if(input$valfiletype=="Net"){
                "Net"
            } else if(input$valfiletype=="Elio") {
                "Spectra"
            } else if(input$valfiletype=="SPX") {
                "Spectra"
            } else if(input$valfiletype=="MCA") {
                "Spectra"
            } else if(input$valfiletype=="PDZ") {
                "Spectra"
            }
            
        })
        
        
  
  
        
        
        tableInputValCounts <- reactive({
            valelements <- calValElements()
            variableelements <- calVariableElements()
            val.data <- myValData()
            
            if(valDataType()=="Spectra"){spectra.line.list <- lapply(valelements, function(x) elementGrab(element.line=x, data=val.data, range.table=calDefinitions()))}
            if(valDataType()=="Spectra"){element.count.list <- lapply(spectra.line.list, '[', 2)}
            
            
            
            if(valDataType()=="Spectra"){spectra.line.vector <- as.numeric(unlist(element.count.list))}
            
            if(valDataType()=="Spectra"){dim(spectra.line.vector) <- c(length(spectra.line.list[[1]]$Spectrum), length(valelements))}
            
            if(valDataType()=="Spectra"){spectra.line.frame <- data.frame(spectra.line.list[[1]]$Spectrum, spectra.line.vector)}
            
            if(valDataType()=="Spectra"){colnames(spectra.line.frame) <- c("Spectrum", valelements)}
            
            if(valDataType()=="Spectra"){spectra.line.frame <- as.data.frame(spectra.line.frame)}
            
            if(valDataType()=="Spectra"){spectra.line.frame}
            
            if(valDataType()=="Spectra"){val.line.table <- spectra.line.frame[, c("Spectrum", valelements), drop = FALSE]}
            
            
            if(valDataType()=="Net"){val.line.table <- val.data[c("Spectrum", valelements), drop=FALSE]}
                
                
                val.line.table


        })
        

        
        
        fullInputValCounts <- reactive({
            valelements <- calValElements()
            variableelements <- calVariableElements()
            val.data <- myValData()
            
            if(valDataType()=="Spectra" ){spectra.line.list <- lapply(variableelements, function(x) elementGrab(element.line=x, data=val.data, range.table=calDefinitions()))}
            if(valDataType()=="Spectra"){element.count.list <- lapply(spectra.line.list, `[`, 2)}
            
            
            if(valDataType()=="Spectra"){spectra.line.vector <- as.numeric(unlist(element.count.list))}
            
            if(valDataType()=="Spectra"){dim(spectra.line.vector) <- c(length(spectra.line.list[[1]]$Spectrum), length(variableelements))}
            
            if(valDataType()=="Spectra"){spectra.line.frame <- data.frame(spectra.line.list[[1]]$Spectrum, spectra.line.vector)}
            
            if(valDataType()=="Spectra"){colnames(spectra.line.frame) <- c("Spectrum", variableelements)}
            
            if(valDataType()=="Spectra"){spectra.line.frame <- as.data.frame(spectra.line.frame)}
            
            if(valDataType()=="Spectra"){val.line.table <- spectra.line.frame[c("Spectrum", variableelements)]}
            
            if(valDataType()=="Net"){val.line.table <- val.data}
            
            
            val.line.table
        })

        
        
        output$myvaltable1 <- renderDataTable({
            
            fullInputValCounts()
            
        })
        
        
        tableInputValQuant <- reactive({
            cloudCalPredict(Calibration=calFileContents2(), count.table=data.frame(fullInputValCounts()), elements.cal=calValElements(), variables=calVariableElements(), valdata=myValData(), rounding=input$resultrounding, multiplier==input$multiplier)
        })
        
        
        output$roundingui <- renderUI({
            
            if(input$multiplier==10000){
                sliderInput('resultrounding', "Round Results", min=0, max=10, value=0)
            } else if(input$multiplier!=10000){
                sliderInput('resultrounding', "Round Results", min=0, max=10, value=4)
            }

            
        })
        
        output$myvaltable2 <- renderDataTable({
            
            tableInputValQuant()
            
        })
        
        
        # valtest <- lapply(valelements, function(x) predict(calsList[[x]], as.data.frame(val.line.table[x])))
        
        output$downloadValData <- downloadHandler(
        filename = function() { paste(input$quantifiedname, "_ValData", '.csv', sep='', collapse='') },
        content = function(file
        ) {
            write.csv(tableInputValQuant(), file)
        }
        )
        
        
  


})

 })
