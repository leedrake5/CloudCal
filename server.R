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
#library(prospectr)
library(pls)
#library(baseline)
library(doParallel)
pdf(NULL)


options(shiny.maxRequestSize=30*1024^40)

shinyServer(function(input, output, session) {
    
    

    calFileContents <- reactive(label="calFileContents", {
        
        existingCalFile <- input$calfileinput
        
        if (is.null(existingCalFile)) return(NULL)
        
        
        Calibration <- calRDS(existingCalFile$datapath)
        
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
            selectInput("filetype", label="Filetype", c("CSV", "Aggregate CSV File", "TXT", "Net", "Elio", "MCA", "SPX", "PDZ"), selected="CSV")
        } else if(!is.null(input$calfileinput)){
            selectInput("filetype", label="Filetype", c("CSV", "Aggregate CSV File", "TXT", "Net", "Elio", "MCA", "SPX", "PDZ"), selected=oldCalCompatibility())
        }
        
    })
    
    
    output$filegrab <- renderUI({
        req(input$filetype)
        if(input$filetype=="CSV") {
            fileInput('file1', 'Choose CSV', multiple=TRUE,
            accept=c(".csv"))
        } else if(input$filetype=="Aggregate CSV File") {
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
    
    output$beamnoui <- renderUI({
        if(input$filetype=="Aggregate CSV File"){
            selectInput("beamno", "Choose Beam", uniqueBeams(input$file1))
        } else if(input$filetype!="Aggregate CSV File"){
            NULL
        }
    })
    
    
    fullSpectraDataTable <- reactive(label="fullSpectraDataTable", {
        req(input$file1)
        
        fullSpectraDataTableProcess(inFile=input$file1, gainshiftvalue=gainshiftHold())
        
    })
    
    
    
    fullSpectra <- reactive(label="fullSpectra", {
        req(input$file1)
        
        fullSpectraProcess(inFile=input$file1, gainshiftvalue=gainshiftHold())

    })
    
    importedCSV <- reactive(label="importedCSV", {
        req(input$file1)
        
            inFile <- input$file1
            if (is.null(inFile)) return(NULL)
            
            importCSVFrame(filepath=inFile$datapath)
    })
    
    
    netCounts <- reactive(label="netCounts", {
        req(input$file1)
        
        netCountsProcess(ineFile=input$file1)
        
    })
    
    
    readTXT <- reactive(label="readTXT", {
        req(input$file1)
        
        readTXTProcess(inFile=input$file1, gainshiftvalue=gainshiftHold())
        
    })
    
    readElio <- reactive(label="readElio", {
        req(input$file1)
        
        readElioProcess(inFile=input$file1, gainshiftvalue=gainshiftHold())
        
    })
    
    
    readMCA <- reactive(label="readMCA", {
        req(input$file1)
       
       readMCAProcess(inFile=input$file1, gainshiftvalue=gainshiftHold())
        
    })
    
    
    readSPX <- reactive(label="readSPX", {
        req(input$file1)
        
        readSPXProcess(inFile=input$file1, gainshiftvalue=gainshiftHold())
        
    })
    
    
    readPDZ <- reactive(label="readPDZ", {
        req(input$file1)
        
        binaryshiftvalue <- tryCatch(binaryHold(), error=function(e) NULL)
        
        readPDZProcess(inFile=input$file1, gainshiftvalue=gainshiftHold(), advanced=input$advanced, binaryshift=binaryshiftvalue)
        
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
                calMemory$Calibration <- calFileContents()
            } else if(is.null(input$calfileinput) && !is.null(input$file1)){
                calMemory$Calibration <- NULL
            }
            
            if(is.null(input$calfileinput) && is.null(input$file1)){
                calMemory$Calibration <- NULL
            } else if(!is.null(input$calfileinput) && is.null(input$file1)){
                calMemory$Calibration <- defaultCalList(calMemory$Calibration)
            } else if(!is.null(input$calfileinput) && !is.null(input$file1)){
                calMemory$Calibration <- defaultCalList(calMemory$Calibration, temp=TRUE)
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
                } else if(input$filetype=="Aggregate CSV File"){
                    importedCSV()
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
                if(is.null(input$file1)){
                    calMemory$Calibration[["Spectra"]]
                } else if(!is.null(input$file1)){
                    myData()
                }
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
                just_spectra_summary_apply(spectra.frame=dataHold(), normalization=input$normspectra, min=input$comptonminspectra, max=input$comptonmaxspectra)

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
            } else if(is.null(calMemory$Calibration$Intensities) && input$filetype=="Aggregate CSV File"){
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
            } else if(input$filetype=="Aggregate CSV File"){
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
            Name=as.vector(as.character(rep("", 75))),
            EnergyMin=as.numeric(rep("", 75)),
            EnergyMax=as.numeric(rep("", 75)),
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
        
        wideSpectraData <- reactive({
            req(dataHold(), elementallinestousepre(), linevalues[["DF"]])
            line.data <- wideElementFrame(data=dataHold(), elements=elementallinestousepre())
            
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
            } else if(input$filetype=="Aggregate CSV File"){
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
            } else if(input$filetype=="Aggregate CSV File"){
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
            
            calMemory$Calibration$WideIntensities <- if(input$filetype=="CSV"){
                wideSpectraData()
            } else if(input$filetype=="Aggregate CSV File"){
                wideSpectraData()
            } else if(input$filetype=="TXT"){
                wideSpectraData()
            } else if(input$filetype=="Elio"){
                wideSpectraData()
            }  else if(input$filetype=="MCA"){
                wideSpectraData()
            }  else if(input$filetype=="SPX"){
                wideSpectraData()
            }  else if(input$filetype=="PDZ"){
                wideSpectraData()
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
        
        wideTableInput <- reactive({
            
            elements <- elementallinestouse()
            
            
            select.line.table <- calMemory$Calibration$WideIntensities
            
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
        
        output$mytable2 <- renderDataTable({
            
            base.table <- wideTableInput()[,-1]
            rownames(base.table) <- wideTableInput()$Spectrum
            base.table
            
        })
        
        output$linetypeui <- renderUI({
            selectInput('linetype', "Choose Line Definition", choices=c("Narrow", "Wide"), selected=calMemory$Calibration$LinePreference)
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
        
        covarPlotLineWide <- reactive({
            data.table <- calMemory$Calibration$WideIntensities
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
        
        output$widecovarianceplot <- renderPlot({
            
            covarPlotLineWide()
            
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
            
            hold.frame <- data.frame(Include=rep(TRUE, length(empty.line.table[,1])), Spectrum=spectra.line.table$Spectrum, empty.line.table)
            #colnames(hold.frame) <- c("Spectrum", elements)
            
            hold.frame <- as.data.frame(hold.frame, stringsAsFactors=FALSE)
            
            
            
            hold.frame
            
            
        })
        
        hotableInputCal <- reactive({
            
            elements <- elementallinestouse()
            
            
            
            
            spectra.line.table <- calMemory$Calibration$Intensities
            value.frame <- calMemory$Calibration$Values

            empty.line.table <- spectra.line.table[,elements] * 0.0000
            
            #empty.line.table$Spectrum <- spectra.line.table$Spectrum
            
            hold.frame <- data.frame(Spectrum=spectra.line.table$Spectrum, empty.line.table, stringsAsFactors=FALSE)
            #colnames(hold.frame) <- c("Spectrum", elements)
            
            #hold.frame <- as.data.frame(hold.frame)
            
            
            
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
            
            #hotable.new <- hotable.new[hotable.new$Spectrum %in% unique(calMemory$Calibration$Spectra$Spectrum)]
            old.vals <- calMemory$Calibration$Values
            old.vals <- old.vals[old.vals$Spectrum %in% hotable.new$Spectrum,]
            
            
            hotable.new <- if(is.null(calMemory$Calibration$Values)){
                data.frame(Include=rep(TRUE, length(hotable.new$Spectrum)), hotable.new, stringsAsFactors=FALSE)
            } else if(!is.null(calMemory$Calibration$Values) && colnames(calMemory$Calibration$Values)[1]=="Spectrum"){
                data.frame(Include=rep(TRUE, length(hotable.new$Spectrum)), hotable.new, stringsAsFactors=FALSE)
            } else if(!is.null(calMemory$Calibration$Values) && colnames(calMemory$Calibration$Values)[1]=="Include"){
                data.frame(Include=rep(TRUE, nrow(hotable.new)), hotable.new, stringsAsFactors=FALSE)
            }
            
            hotable.new
            
            
        })
        
        hotableInput <- reactive({
            
            
            hotable.data <- if(is.null(calMemory$Calibration$Values)){
                hotableInputBlank()
            } else if(!is.null(calMemory$Calibration$Values)){
                hotableInputCal()
            }
            
            
            hotable.new <- hotable.data
            
            hotable.new
            
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
            rhandsontable(DF, digits=12) %>% hot_col(2:length(DF), renderer=htmlwidgets::JS("safeHtmlRenderer"))
            
            
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
            } else if(input$filetype=="Aggregate CSV File"){
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
            valFrameCheck(concentration.table[values[["DF"]]$Include,])

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
        
        spectraLineTableWide <- reactive({
            
            spectra.line.table <- if(dataType()=="Spectra"){
                calMemory$Calibration$WideIntensities[values[["DF"]]$Include,]
            }else if(dataType()=="Net"){
                calMemory$Calibration$WideIntensities[values[["DF"]]$Include,]
            }
            
            
            spectra.line.table <- spectra.line.table[order(as.character(spectra.line.table$Spectrum)),]
            spectra.line.table <- spectra.line.table[complete.cases(spectra.line.table),]
            spectra.line.table[ rowSums(spectra.line.table[,-1])!=0, ]
            
            
        })
        
        
        holdFrame <- reactive({
            req(concentrationTable(), spectraLineTable())
            spectra.line.table <- if(input$linepreferenceelement=="Narrow"){
                spectraLineTable()
            } else if(input$linepreferenceelement=="Wide"){
                spectraLineTableWide()
            }
            concentration.table <- concentrationTable()
            
            concentration.table <- concentration.table[concentration.table$Spectrum %in% spectra.line.table$Spectrum,]
            spectra.line.table <- spectra.line.table[spectra.line.table$Spectrum %in% concentration.table$Spectrum,]

            
            concentration <- as.vector(as.numeric(unlist(concentration.table[,input$calcurveelement])))
            
            hold.frame <- data.frame(spectra.line.table, Concentration=concentration)
            
            hold.frame[complete.cases(hold.frame),]
        })
        
        dataNorm <- reactive({
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
        
        
        
        output$linepreferenceelementui <- renderUI({
            req(input$linetype)
            selectInput('linepreferenceelement', "Choose Line Definition", choices=c("Narrow", "Wide"), selected=input$linetype)
            
        })
        
        
        
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
        
        observeEvent(input$bartk, {
            calConditions$hold[["CalTable"]]$bartK <<- paste0(input$bartk[1], "-", input$bartk[2])
        })
        
        observeEvent(input$bartbeta, {
            calConditions$hold[["CalTable"]]$bartBeta <<- paste0(input$bartbeta[1], "-", input$bartbeta[2])
        })
        
        observeEvent(input$bartnu, {
            calConditions$hold[["CalTable"]]$bartNu <<- paste0(input$bartnu[1], "-", input$bartnu[2])
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

        
        
        calInterceptSelectionPre <- reactive({
            if(!"Intercept" %in% names(calSettings$calList[[input$calcurveelement]][[1]])){
                calConditions$hold$Intercept
            } else if("Intercept" %in% names(calSettings$calList[[input$calcurveelement]][[1]])){
                calSettings$calList[[input$calcurveelement]][[1]]$Intercept
            }
        })
        
        
        
        
        calSlopeSelectionPre <- reactive({
            req(input$radiocal, input$calcurveelement)
            if(input$radiocal==3){
                calSettings$calList[[input$calcurveelement]][[1]]$Slope
            } else if(input$radiocal==4 | input$radiocal==6 | input$radiocal==8 | input$radiocal==10 | input$radiocal==12){
                if(!"Slope" %in% names(calSettings$calList[[input$calcurveelement]][[1]])){
                    outVaralt()
                } else if("Slope" %in% names(calSettings$calList[[input$calcurveelement]][[1]])){
                    if( length(calSettings$calList[[input$calcurveelement]][[1]]$Slope)<=1){
                        outVaralt()
                    } else if( length(calSettings$calList[[input$calcurveelement]][[1]]$Slope)>1){
                        calSettings$calList[[input$calcurveelement]][[1]]$Slope
                    }
                }
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
        
        
        observeEvent(input$trainslopes, {
            
            isolate(basichold$normtype <- bestNormVars()[["Type"]])
            isolate(basichold$normmin <- bestNormVars()[["Compton"]][1])
            isolate(basichold$normmax <- bestNormVars()[["Compton"]][2])
            
        })
        
        calNormSelection <- reactive({
            normhold$normtype
        })
        
        normMinSelection <- reactive({
            basichold$normmin
        })
        
        normMaxSelection <- reactive({
            basichold$normmax
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
        

        #observeEvent(input$trainslopes, {
        
        #isolate(intercepthold$intercepts <- bestInterceptVars())
        
        #})
        
        
        inVar3Selected <- reactive({
            
            lucashold$intercept
            
            
        })
        
        output$inVar3 <- renderUI({
            req(input$radiocal)
            
            if(input$radiocal==3 | input$radiocal==4 | input$radiocal==6 | input$radiocal==8 | input$radiocal==10 | input$radiocal==12){
                selectInput(inputId = "intercept_vars", label = h4("Intercept"), choices =  outVaralt2(), selected=inVar3Selected(), multiple=TRUE)
            } else if(input$radiocal!=3 | input$radiocal!=4 | input$radiocal!=6 | input$radiocal!=8 | input$radiocal!=10 | input$radiocal!=12){
                NULL
            }
        })
        
        
        
        ####Machine Learning: Slope
        
        output$multicore_behavior_ui <- renderUI({
            require(input$radiocal)
            default.behavior <- if(get_os()=="windows"){
                "Serialize"
            } else if(get_os()!="windows"){
                "Fork"
            }
            
            tryCatch(if(input$radiocal==1){
                NULL
            } else if(input$radiocal==2){
                NULL
            } else if(input$radiocal==3){
                NULL
            } else if(input$radiocal==4){
                selectInput("multicore_behavior", "Multicore Processing", choices=c("Single Core", "Serialize", "Fork"), selected=default.behavior)
            } else if(input$radiocal==5){
                selectInput("multicore_behavior", "Multicore Processing", choices=c("Single Core", "Serialize", "Fork"), selected=default.behavior)
            } else if(input$radiocal==6){
                selectInput("multicore_behavior", "Multicore Processing", choices=c("Single Core", "Serialize", "Fork"), selected=default.behavior)
            } else if(input$radiocal==7){
                selectInput("multicore_behavior", "Multicore Processing", choices=c("Single Core", "Serialize", "Fork"), selected=default.behavior)
            } else if(input$radiocal==8){
                selectInput("multicore_behavior", "Multicore Processing", choices=c("Single Core", "Serialize", "Fork", "OpenMP"), selected=default.behavior)
            } else if(input$radiocal==9){
                selectInput("multicore_behavior", "Multicore Processing", choices=c("Single Core", "Serialize", "Fork", "OpenMP"), selected=default.behavior)
            } else if(input$radiocal==10){
                selectInput("multicore_behavior", "Multicore Processing", choices=c("Single Core", "Serialize", "Fork"), selected="Single Core")
            } else if(input$radiocal==11){
                selectInput("multicore_behavior", "Multicore Processing", choices=c("Single Core", "Serialize", "Fork"), selected="Single Core")
            } else if(input$radiocal==12){
                selectInput("multicore_behavior", "Multicore Processing", choices=c("Single Core", "Serialize", "Fork"), selected="Single Core")
            } else if(input$radiocal==13){
                selectInput("multicore_behavior", "Multicore Processing", choices=c("Single Core", "Serialize", "Fork"), selected="Single Core")
            }, error=function(e)                 selectInput("multicore_behavior", "Multicore Processing", choices=c("Single Core", "Serialize", "Fork"), selected="Single Core"))
                
            
        })
        
        forestTemp <- reactive(label="forestModelData", {
            req(input$radiocal, input$calcurveelement)
            predictFrameForestGen(spectra=dataNorm(), hold.frame=holdFrame(), element=input$calcurveelement, intercepts=input$intercept_vars, norm.type=input$normcal, norm.min=input$comptonmin, norm.max=input$comptonmax, data.type=dataType())
        })

        
        slopeImportance <- reactive({
            if(isMCL()==TRUE){
                varImp(elementModel(), scale=FALSE)
            } else if(isMCL()==FALSE){
                predict.frame <- forestTemp()
                
                rf.grid <- expand.grid(.mtry=5)
                
                
                tune_control <-
                    caret::trainControl(
                    method = "cv",
                    number = 5)
                
                cl <- if(get_os()=="windows"){
                    parallel::makePSOCKcluster(as.numeric(my.cores))
                } else if(get_os()!="windows"){
                    parallel::makeForkCluster(as.numeric(my.cores))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                rf_model <- tryCatch(caret::train(Concentration~.,data=predict.frame,method="rf", type="Regression",
                trControl=tune_control, ntree=100,
                prox=TRUE,allowParallel=TRUE, importance=TRUE, metric="RMSE", tuneGrid=rf.grid, na.action=na.omit, trim=TRUE), error=function(e) NULL)
                
                stopCluster(cl)
                varImp(rf_model, scale=FALSE)
            }
            
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
            
            
            
            
            predict.intensity <- if(basichold$normtype==1){
                if(dataType()=="Spectra"){
                    lucas_simp_prep_xrf(spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=choices, intercept.element.lines=basichold$intercept)
                } else if(dataType()=="Net"){
                    lucas_simp_prep_xrf_net(spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=choices, intercept.element.lines=basichold$intercept)
                }
            } else if(basichold$normtype==2){
                predict.intensity <- if(dataType()=="Spectra"){
                    lucas_tc_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=choices, intercept.element.lines=basichold$intercept)
                } else if(dataType()=="Net"){
                    lucas_tc_prep_xrf_net(data=data, spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=choices, intercept.element.lines=basichold$intercept)
                }
            } else if(basichold$normtype==3){
                predict.intensity <- if(dataType()=="Spectra"){
                    lucas_comp_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=choices, intercept.element.lines=basichold$intercept, norm.min=basichold$normmin, norm.max=basichold$normmax)
                } else if(dataType()=="Net"){
                    lucas_comp_prep_xrf_net(data=data, spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=choices, intercept.element.lines=basichold$intercept, norm.min=basichold$hormmin, norm.max=basichold$normmax)
                }
            }
            
            
            
            #optimal_r_chain(element=element, intensities=predict.intensity, values= concentration.table, possible.Slope=fishVector(), keep=vals$keeprows)
            
            results <- variable_select_short_xrf(slopeImportance())
            
            c(input$calcurveelement, results[!results %in% input$calcurveelement])
            
            
        })
        
        
        observeEvent(input$trainslopes, {
            
            isolate(lucashold$slope <- if(bestCalType()==3){
                bestSlopeVars()
            })
            
        })
        
        
        inVar4Selected <- reactive({
            req(input$radiocal)
            
            if(is.null(lucashold$slope)){
                lucashold$slope <- if(input$radiocal==3){
                    input$calcurveelement
                } else if(input$radiocal==4 | input$radiocal==6 | input$radiocal==8 | input$radiocal==10 | input$radiocal==12){
                    outVaralt()
                }
                
                
            }
            
            lucashold$slope
            
        })
        
        
        
        output$inVar4 <- renderUI({
            req(input$radiocal)
            
            if(input$radiocal==3 | input$radiocal==4 | input$radiocal==6 | input$radiocal==8 | input$radiocal==10 | input$radiocal==12){
                selectInput(inputId = "slope_vars", label = h4("Slope"), choices =  outVaralt(), selected=inVar4Selected(), multiple=TRUE)
            } else if(input$radiocal!=3 | input$radiocal!=4 | input$radiocal!=6 | input$radiocal!=8 | input$radiocal!=10 | input$radiocal!=12){
                NULL
            }
        })
        
        
        output$addallslopesui <- renderUI({
            req(input$radiocal)
            
            addAllSlopeUI(radiocal=input$radiocal)
        })
        
        output$removeallslopesui <- renderUI({
            req(input$radiocal)
            
            removeAllSlopeUI(radiocal=input$radiocal)
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
        
        
        observeEvent(input$trainslopes, {
            
            isolate(calhold$caltype <<- bestCalType())
            
        })
        
        calTypeSelection <- reactive({
            calhold$caltype
        })
        
        output$calTypeInput <- renderUI({
            req(input$calcurveelement)
            selectInput("radiocal", label = "Calibration Curve",
            choices = list("Linear" = 1, "Non-Linear" = 2, "Lucas-Tooth" = 3, "Forest" = 4, "Rainforest"=5, "Neural Network Intensities"=6, "Neural Network Spectra"=7, "XGBoost Intensities"=8, "XGBoost Spectra"=9, "Bayes Intensities"=10, "Bayes Spectra"=11, "Support Vector Intensities"=12, "Support Vector Spectra"=13),
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
            list(CalTable=calConditionsTable(cal.type=1, line.type=input$linepreferenceelement, norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation()), StandardsUsed=vals$keeprows)
        })
        linearModelData <- reactive(label="linearModelData", {
            predictFrameSimpGen(spectra=dataNormCal(), hold.frame=holdFrameCal(), dependent.transformation=linearParameters()$CalTable$DepTrans, element=input$calcurveelement,  norm.type=linearParameters()$CalTable$NormType, norm.min=linearParameters()$CalTable$Min, norm.max=linearParameters()$CalTable$Max, data.type=dataType())
        })
        linearModelSet <- reactive(label="linearModelSet", {
            list(data=predictFrameCheck(linearModelData()), parameters=linearParameters())
        })
        linearModel <- reactive(label="nonLinearModel", {
            
            predict.frame <- linearModelSet()$data[linearModelSet()$parameters$StandardsUsed,]
            
            l.model <- lm(Concentration~Intensity, data=predict.frame, na.action=na.omit)
            
            l.model
            
        })
        
        nonLinearParameters <- reactive(label="nonLinearParameters", {
            list(CalTable=calConditionsTable(cal.type=2, line.type=input$linepreferenceelement, norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation()), StandardsUsed=vals$keeprows)
        })
        nonLinearModelData <- reactive(label="nonLinearModelData", {
            predictFrameSimpGen(spectra=dataNormCal(), hold.frame=holdFrameCal(), dependent.transformation=nonLinearParameters()$CalTable$DepTrans, element=input$calcurveelement,  norm.type=nonLinearParameters()$CalTable$NormType, norm.min=nonLinearParameters()$CalTable$Min, norm.max=nonLinearParameters()$CalTable$Max, data.type=dataType())
        })
        nonLinearModelSet <- reactive(label="nonLinearModelSet", {
            list(data=predictFrameCheck(nonLinearModelData()), parameters=nonLinearParameters())
        })
        nonLinearModel <- reactive(label="nonLinearModel", {
            
            predict.frame <- nonLinearModelSet()$data[nonLinearModelSet()$parameters$StandardsUsed,]
            
            nl.model <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame, na.action=na.omit)
            
            nl.model
            
        })
        
        lucasToothParameters <- reactive(label="lucasToothParameters", {
            list(CalTable=calConditionsTable(cal.type=3, line.type=input$linepreferenceelement, norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation()), Slope=lucasSlope(), Intercept=lucasIntercept(), StandardsUsed=vals$keeprows)
        })
        lucasToothModelData <- reactive(label="lucasToothModelData", {
            predictFrameLucGen(spectra=dataNormCal(), hold.frame=holdFrameCal(), dependent.transformation=lucasToothParameters()$CalTable$DepTrans, element=input$calcurveelement, intercepts=lucasToothParameters()$Intercept, slopes=lucasToothParameters()$Slope, norm.type=lucasToothParameters()$CalTable$NormType, norm.min=lucasToothParameters()$CalTable$Min, norm.max=lucasToothParameters()$CalTable$Max, data.type=dataType())
        })
        lucasToothModelSet <- reactive(label="lucasToothModelSet", {
            list(data=predictFrameCheck(lucasToothModelData()), parameters=lucasToothParameters())
        })
        lucasToothModel <- reactive(label="lucasToothModel", {
            
            predict.frame <- lucasToothModelSet()$data[lucasToothModelSet()$parameters$StandardsUsed,]
            
            lc.model <- lm(Concentration~., data=predict.frame, na.action=na.omit)
            
            lc.model
            
        })
        
        output$parallelmethodui <- renderUI({
            
            style <- if(get_os()=="windows"){
                "Serialize"
            } else if(get_os()=="osx"){
                "Fork"
            } else if(get_os()=="linux"){
                "OpenMP"
            }
            
            selectInput("parallelmethod", "Parallel Processing Type", choices=c("Serialize", "Fork", "OpenMP"), selected=style)
        })
        
        forestParameters <- reactive(label="forestParameters", {
            cvrepeats <- if(foresthold$foresttrain=="repeatedcv"){
                foresthold$cvrepeats
            } else if(foresthold$foresttrain!="repeatedcv"){
                1
            }
            list(CalTable=calConditionsTable(cal.type=4, line.type=input$linepreferenceelement, norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation(), foresttry=forestTrySelection(), forestmetric=forestMetricSelection(), foresttrain=forestTrainSelection(), forestnumber=forestNumberSelection(), cvrepeats=cvrepeats, foresttrees=forestTreeSelection()), Slope=lucasSlope(), Intercept=lucasIntercept(), StandardsUsed=vals$keeprows)
        })
        forestModelData <- reactive(label="forestModelData", {
            predictFrameForestGen(spectra=dataNormCal(), hold.frame=holdFrameCal(), dependent.transformation=forestParameters()$CalTable$DepTrans, element=input$calcurveelement, intercepts=forestParameters()$Intercept, slopes=forestParameters()$Slope, norm.type=forestParameters()$CalTable$NormType, norm.min=forestParameters()$CalTable$Min, norm.max=forestParameters()$CalTable$Max, data.type=dataType())
        })
        forestModelSet <- reactive(label="forestModelSet", {
            list(data=predictFrameCheck(forestModelData()), parameters=forestParameters())
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
                number = parameters$ForestNumber)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            if(input$multicore_behavior=="Single Core"){
                rf_model <- caret::train(Concentration~.,data=predict.frame, method="rf", type="Regression", trControl=tune_control, ntree=parameters$ForestTrees, prox=TRUE, importance=TRUE, metric=parameters$ForestMetric, tuneGrid=rf.grid, na.action=na.omit, trim=TRUE)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                rf_model <- caret::train(Concentration~.,data=predict.frame, method="rf", type="Regression", trControl=tune_control, ntree=parameters$ForestTrees, prox=TRUE,allowParallel=TRUE, importance=TRUE, metric=parameters$ForestMetric, tuneGrid=rf.grid, na.action=na.omit, trim=TRUE)
                
                stopCluster(cl)
            }
            
            rf_model
            
        })
        
        rainforestParameters <- reactive(label="rainforestParameters", {
            cvrepeats <- if(foresthold$foresttrain=="repeatedcv"){
                foresthold$cvrepeats
            } else if(foresthold$foresttrain!="repeatedcv"){
                1
            }
            energyrange <- basicEnergyRange()
            list(CalTable=calConditionsTable(cal.type=5, line.type=input$linepreferenceelement, compress=basicCompress(), transformation=basicTransformation(), energy.range=paste0(energyrange[1], "-", energyrange[2]), norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation(), foresttry=forestTrySelection(), forestmetric=forestMetricSelection(), foresttrain=forestTrainSelection(), forestnumber=forestNumberSelection(), cvrepeats=cvrepeats, foresttrees=forestTreeSelection()), StandardsUsed=vals$keeprows)
        })
        rainforestModelData <- reactive(label="rainforestModelData", {
            rainforestDataGen(spectra=dataNormCal(), compress=rainforestParameters()$CalTable$Compress, transformation=rainforestParameters()$CalTable$Transformation, dependent.transformation=rainforestParameters()$CalTable$DepTrans, energy.range=as.numeric(unlist(strsplit(as.character(rainforestParameters()$CalTable$EnergyRange), "-"))), hold.frame=holdFrameCal(), norm.type=rainforestParameters()$CalTable$NormType, norm.min=rainforestParameters()$CalTable$Min, norm.max=rainforestParameters()$CalTable$Max, data.type=dataType())
        })
        #rainforestModelSetlist <- reactiveValues()
        #observeEvent(input$createcalelement, priority=150, {
        #    rainforestModelSet$list <- list(data=rainforestModelData(), parameters=rainforestParameters())
        #})
        
        rainforestModelSet <- reactive(label="rainforestModelSet", {
            list(data=predictFrameCheck(rainforestModelData()), parameters=rainforestParameters())
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
                number = parameters$ForestNumber)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                rf_model <- caret::train(Concentration~.,data=data[,-1], method="rf", type="Regression", trControl=tune_control, ntree=parameters$ForestTrees, prox=TRUE, metric=parameters$ForestMetric, tuneGrid=rf.grid, na.action=na.omit, importance=TRUE, trim=TRUE)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                rf_model <- caret::train(Concentration~.,data=data[,-1], method="rf", type="Regression", trControl=tune_control, ntree=parameters$ForestTrees, prox=TRUE,allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=rf.grid, na.action=na.omit, importance=TRUE, trim=TRUE)

                stopCluster(cl)
            }
            rf_model
            
        })
        
        neuralNetworkIntensityShallowParameters <- reactive(label="neuralNetworkIntensityShallowParameters", {
            cvrepeats <- if(foresthold$foresttrain=="repeatedcv"){
                foresthold$cvrepeats
            } else if(foresthold$foresttrain!="repeatedcv"){
                1
            }
            hiddenunits <- neuralHiddenUnitsSelection()
            weightdecay <- neuralWeightDecaySelection()
            list(CalTable=calConditionsTable(cal.type=6, line.type=input$linepreferenceelement, norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation(), forestmetric=forestMetricSelection(), foresttrain=forestTrainSelection(), forestnumber=forestNumberSelection(), cvrepeats=cvrepeats, neuralhiddenlayers=neuralHiddenLayersSelection(), neuralhiddenunits=paste0(hiddenunits[1], "-", hiddenunits[2]), neuralweightdecay=paste0(weightdecay[1], "-", weightdecay[2]), neuralmaxiterations=neuralMaxIterationsSelection()), Slope=lucasSlope(), Intercept=lucasIntercept(), StandardsUsed=vals$keeprows)
        })
        neuralNetworkIntensityShallowModelData <- reactive(label="neuralNetworkIntensityShallowModelData", {
            predictFrameForestGen(spectra=dataNormCal(), hold.frame=holdFrameCal(), dependent.transformation=neuralNetworkIntensityShallowParameters()$CalTable$DepTrans, element=input$calcurveelement, intercepts=neuralNetworkIntensityShallowParameters()$Intercept, slopes=neuralNetworkIntensityShallowParameters()$Slope, norm.type=neuralNetworkIntensityShallowParameters()$CalTable$NormType, norm.min=neuralNetworkIntensityShallowParameters()$CalTable$Min, norm.max=neuralNetworkIntensityShallowParameters()$CalTable$Max, data.type=dataType())
        })
        neuralNetworkIntensityShallowModelSet <- reactive(label="neuralNetworkIntensityShallowModelSet", {
            list(data=predictFrameCheck(neuralNetworkIntensityShallowModelData()), parameters=neuralNetworkIntensityShallowParameters())
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
                number = parameters$ForestNumber)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats)
            }
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            if(input$multicore_behavior=="Single Core"){
                nn_model <- caret::train(Concentration~.,data=predict.frame, method="nnet", linout=TRUE, trControl=tune_control, metric=parameters$ForestMetric, na.action=na.omit, importance=TRUE, tuneGrid=nn.grid, maxit=parameters$NeuralMI, trace=F, trim=TRUE)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                nn_model <- caret::train(Concentration~.,data=predict.frame, method="nnet", linout=TRUE, trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, na.action=na.omit, importance=TRUE, tuneGrid=nn.grid, maxit=parameters$NeuralMI, trace=F, trim=TRUE)

                stopCluster(cl)
            }
            nn_model
            
        })
        
        neuralNetworkIntensityDeepParameters <- reactive(label="neuralNetworkIntensityDeepParameters", {
            cvrepeats <- if(foresthold$foresttrain=="repeatedcv"){
                foresthold$cvrepeats
            } else if(foresthold$foresttrain!="repeatedcv"){
                1
            }
            hiddenunits <- neuralHiddenUnitsSelection()
            list(CalTable=calConditionsTable(cal.type=6, line.type=input$linepreferenceelement, norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation(), foresttry=forestTrySelection(), forestmetric=forestMetricSelection(), foresttrain=forestTrainSelection(), forestnumber=forestNumberSelection(), cvrepeats=cvrepeats, neuralhiddenlayers=neuralHiddenLayersSelection(), neuralhiddenunits=paste0(hiddenunits[1], "-", hiddenunits[2])), Slope=lucasSlope(), Intercept=lucasIntercept(), StandardsUsed=vals$keeprows)
        })
        neuralNetworkIntensityDeepModelData <- reactive(label="neuralNetworkIntensityDeepModelData", {
            predictFrameForestGen(spectra=dataNormCal(), hold.frame=holdFrameCal(), dependent.transformation=neuralNetworkIntensityDeepParameters()$CalTable$DepTrans, element=input$calcurveelement, intercepts=neuralNetworkIntensityDeepParameters()$Intercept, slopes=neuralNetworkIntensityDeepParameters()$Slope, norm.type=neuralNetworkIntensityDeepParameters()$CalTable$NormType, norm.min=neuralNetworkIntensityDeepParameters()$CalTable$Min, norm.max=neuralNetworkIntensityDeepParameters()$CalTable$Max, data.type=dataType())
        })
        neuralNetworkIntensityDeepModelSet <- reactive(label="neuralNetworkIntensityDeepModelSet", {
            list(data=predictFrameCheck(neuralNetworkIntensityDeepModelData()), parameters=neuralNetworkIntensityDeepParameters())
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            f <- as.formula(paste("Concentration ~", paste(names(predict.frame)[!names(predict.frame) %in% "Concentration"], collapse = " + ")))

            
            if(input$multicore_behavior=="Single Core"){
                nn_model <- caret::train(f,data=predict.frame, method="neuralnet", rep=parameters$ForestTry, trControl=tune_control, metric=parameters$ForestMetric, na.action=na.omit,  tuneGrid=nn.grid, linear.output=TRUE)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                nn_model <- caret::train(f,data=predict.frame, method="neuralnet", rep=parameters$ForestTry, trControl=tune_control, metric=parameters$ForestMetric, na.action=na.omit,  tuneGrid=nn.grid, allowParallel=TRUE, linear.output=TRUE)

                stopCluster(cl)
            }
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
            cvrepeats <- if(foresthold$foresttrain=="repeatedcv"){
                foresthold$cvrepeats
            } else if(foresthold$foresttrain!="repeatedcv"){
                1
            }
            energyrange <- basicEnergyRange()
            hiddenunits <- neuralHiddenUnitsSelection()
            weightdecay <- neuralWeightDecaySelection()
            list(CalTable=calConditionsTable(cal.type=7, line.type=input$linepreferenceelement, compress=basicCompress(), transformation=basicTransformation(), energy.range=paste0(energyrange[1], "-", energyrange[2]), norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation(), forestmetric=forestMetricSelection(), foresttrain=forestTrainSelection(), forestnumber=forestNumberSelection(), cvrepeats=cvrepeats, neuralhiddenlayers=neuralHiddenLayersSelection(), neuralhiddenunits=paste0(hiddenunits[1], "-", hiddenunits[2]), neuralweightdecay=paste0(weightdecay[1], "-", weightdecay[2]), neuralmaxiterations=neuralMaxIterationsSelection()), StandardsUsed=vals$keeprows)
        })
        neuralNetworkSpectraShallowModelData <- reactive(label="neuralNetworkSpectraShallowModelData", {
            rainforestDataGen(spectra=dataNormCal(), compress=neuralNetworkSpectraShallowParameters()$CalTable$Compress, transformation=neuralNetworkSpectraShallowParameters()$CalTable$Transformation, dependent.transformation=neuralNetworkSpectraShallowParameters()$CalTable$DepTrans, energy.range=as.numeric(unlist(strsplit(as.character(neuralNetworkSpectraShallowParameters()$CalTable$EnergyRange), "-"))), hold.frame=holdFrameCal(), norm.type=neuralNetworkSpectraShallowParameters()$CalTable$NormType, norm.min=neuralNetworkSpectraShallowParameters()$CalTable$Min, norm.max=neuralNetworkSpectraShallowParameters()$CalTable$Max, data.type=dataType())
        })
        neuralNetworkSpectraShallowModelSet <- reactive(label="neuralNetworkSpectraShallowModelSet", {
            list(data=predictFrameCheck(neuralNetworkSpectraShallowModelData()), parameters=neuralNetworkSpectraShallowParameters())
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
                number = parameters$ForestNumber)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats)
            }
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                nn_model <- caret::train(Concentration~.,data=data[,-1], method="nnet", linout=TRUE, trControl=tune_control, metric=parameters$ForestMetric, na.action=na.omit, importance=TRUE, tuneGrid=nn.grid, maxit=parameters$NeuralMI, trace=F, trim=TRUE)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                nn_model <- caret::train(Concentration~.,data=data[,-1], method="nnet", linout=TRUE, trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, na.action=na.omit, importance=TRUE, tuneGrid=nn.grid, maxit=parameters$NeuralMI, trace=F, trim=TRUE)
                stopCluster(cl)
            }
            nn_model
            
        })
        
        neuralNetworkSpectraDeepParameters <- reactive(label="neuralNetworkSpectraDeepParameters", {
            cvrepeats <- if(foresthold$foresttrain=="repeatedcv"){
                foresthold$cvrepeats
            } else if(foresthold$foresttrain!="repeatedcv"){
                1
            }
            energyrange <- basicEnergyRange()
            hiddenunits <- neuralHiddenUnitsSelection()
            list(CalTable=calConditionsTable(cal.type=7, line.type=input$linepreferenceelement, compress=basicCompress(), transformation=basicTransformation(), energy.range=paste0(energyrange[1], "-", energyrange[2]), norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation(), foresttry=forestTrySelection(), forestmetric=forestMetricSelection(), foresttrain=forestTrainSelection(), forestnumber=forestNumberSelection(), cvrepeats=cvrepeats, neuralhiddenlayers=neuralHiddenLayersSelection(), neuralhiddenunits=paste0(hiddenunits[1], "-", hiddenunits[2])), StandardsUsed=vals$keeprows)
        })
        neuralNetworkSpectraDeepModelData <- reactive(label="neuralNetworkSpectraDeepModelData", {
            rainforestDataGen(spectra=dataNormCal(), compress=neuralNetworkSpectraDeepParameters()$CalTable$Compress, transformation=neuralNetworkSpectraDeepParameters()$CalTable$Transformation, dependent.transformation=neuralNetworkSpectraDeepParameters()$CalTable$DepTrans, energy.range=as.numeric(unlist(strsplit(as.character(neuralNetworkSpectraDeepParameters()$CalTable$EnergyRange), "-"))), hold.frame=holdFrameCal(), norm.type=neuralNetworkSpectraDeepParameters()$CalTable$NormType, norm.min=neuralNetworkSpectraDeepParameters()$CalTable$Min, norm.max=neuralNetworkSpectraDeepParameters()$CalTable$Max, data.type=dataType())
        })
        neuralNetworkSpectraDeepModelSet <- reactive(label="neuralNetworkSpectraDeepModelSet", {
            list(data=predictFrameCheck(neuralNetworkSpectraDeepModelData()), parameters=neuralNetworkSpectraDeepParameters())
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            f <- as.formula(paste("Concentration ~", paste(names(data)[!names(data) %in% "Concentration"], collapse = " + ")))
            
            if(input$multicore_behavior=="Single Core"){
                nn_model <- caret::train(f,data=data[,-1], method="neuralnet", rep=parameters$ForestTry, trControl=tune_control, metric=parameters$ForestMetric, na.action=na.omit, tuneGrid=nn.grid, linear.output=TRUE)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                nn_model <- caret::train(f,data=data[,-1], method="neuralnet", rep=parameters$ForestTry, trControl=tune_control, metric=parameters$ForestMetric, na.action=na.omit, tuneGrid=nn.grid, linear.output=TRUE, allowParallel=TRUE)
                stopCluster(cl)
            }
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
        
        xgbtreeIntensityParameters <- reactive(label="xgbtreeIntensityParameters", {
            cvrepeats <- if(foresthold$foresttrain=="repeatedcv"){
                foresthold$cvrepeats
            } else if(foresthold$foresttrain!="repeatedcv"){
                1
            }
            treedepth <- xgboostTreeDepthSelection()
            eta <- xgboostEtaSelection()
            gamma <- xgboostGammaSelection()
            subsample <- xgboostSubSampleSelection()
            colsample <- xgboostColSampleSelection()
            list(CalTable=calConditionsTable(cal.type=8, line.type=input$linepreferenceelement, norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation(), foresttrees=forestTreeSelection(), forestmetric=forestMetricSelection(), foresttrain=forestTrainSelection(), forestnumber=forestNumberSelection(), cvrepeats=cvrepeats, xgbtype=xgboosthold$xgbtype, treedepth=paste0(treedepth[1], "-", treedepth[2]), xgbeta=paste0(eta[1], "-", eta[2]), xgbgamma=paste0(gamma[1], "-", gamma[2]), xgbsubsample=paste0(subsample[1], "-", subsample[2]), xgbcolsample=paste0(colsample[1], "-", colsample[2]), xgbminchild=xgboostMinChildSelection()), Slope=lucasSlope(), Intercept=lucasIntercept(), StandardsUsed=vals$keeprows)
        })
        xgbtreeIntensityModelData <- reactive(label="xgboostIntensityModelData", {
            predictFrameForestGen(spectra=dataNormCal(), hold.frame=holdFrameCal(), dependent.transformation=xgbtreeIntensityParameters()$CalTable$DepTrans, element=input$calcurveelement, intercepts=xgbtreeIntensityParameters()$Intercept, slopes=xgbtreeIntensityParameters()$Slope, norm.type=xgbtreeIntensityParameters()$CalTable$NormType, norm.min=xgbtreeIntensityParameters()$CalTable$Min, norm.max=xgbtreeIntensityParameters()$CalTable$Max, data.type=dataType())
        })
        xgbtreeIntensityModelSet <- reactive(label="xgbtreeIntensityModelSet", {
            list(data=predictFrameCheck(xgbtreeIntensityModelData()), parameters=xgbtreeIntensityParameters())
        })
        xgbtreeIntensityModel <- reactive(label="xgtreeIntensityModel", {
            req(input$radiocal, input$calcurveelement)
            
            predict.frame <- xgbtreeIntensityModelSet()$data[xgbtreeIntensityModelSet()$parameters$StandardsUsed,]
            parameters <- xgbtreeIntensityModelSet()$parameters$CalTable
            
            
            tree.depth.vec <- as.numeric(unlist(strsplit(as.character(parameters$TreeDepth), "-")))
            xgbeta.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbEta), "-")))
            xgbgamma.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbGamma), "-")))
            xgbsubsample.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbSubSample), "-")))
            xgbcolsample.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbColSample), "-")))
            
            
            xgbGrid <- expand.grid(
            nrounds = parameters$ForestTrees,
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
                
            
            if(input$multicore_behavior=="Single Core"){
                xgb_model <- caret::train(Concentration~., data=predict.frame, trControl = tune_control, tuneGrid = xgbGrid, metric=parameters$ForestMetric, method = "xgbTree", na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                xgb_model <- caret::train(Concentration~., data=predict.frame, trControl = tune_control, tuneGrid = xgbGrid, metric=parameters$ForestMetric, method = "xgbTree", na.action=na.omit, allowParallel=TRUE)
                stopCluster(cl)
            } else if(input$multicore_behavior=="OpenMP"){
                xgb_model <- caret::train(Concentration~., data=predict.frame, trControl = tune_control, tuneGrid = xgbGrid, metric=parameters$ForestMetric, method = "xgbTree", na.action=na.omit, nthread=as.numeric(cores.to.use))
            }
            
            xgb_model
            
        })
        
        xgblinearIntensityParameters <- reactive(label="xgblinearIntensityParameters", {
            cvrepeats <- if(foresthold$foresttrain=="repeatedcv"){
                foresthold$cvrepeats
            } else if(foresthold$foresttrain!="repeatedcv"){
                1
            }
            eta <- xgboostEtaSelection()
            alpha <- xgboostAlphaSelection()
            lambda <- xgboostLambdaSelection()
            list(CalTable=calConditionsTable(cal.type=8, line.type=input$linepreferenceelement, norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation(), foresttrees=forestTreeSelection(), forestmetric=forestMetricSelection(), foresttrain=forestTrainSelection(), forestnumber=forestNumberSelection(), cvrepeats=cvrepeats, xgbtype=xgboosthold$xgbtype, xgbalpha=paste0(alpha[1], "-", alpha[2]), xgbeta=paste0(eta[1], "-", eta[2]), xgblambda=paste0(lambda[1], "-", lambda[2])), Slope=lucasSlope(), Intercept=lucasIntercept(), StandardsUsed=vals$keeprows)
        })
        xgblinearIntensityModelData <- reactive(label="xgblinearIntensityModelData", {
            predictFrameForestGen(spectra=dataNormCal(), hold.frame=holdFrameCal(), dependent.transformation=xgblinearIntensityParameters()$CalTable$DepTrans, element=input$calcurveelement, intercepts=xgblinearIntensityParameters()$Intercept, slopes=xgblinearIntensityParameters()$Slope, norm.type=xgblinearIntensityParameters()$CalTable$NormType, norm.min=xgblinearIntensityParameters()$CalTable$Min, norm.max=xgblinearIntensityParameters()$CalTable$Max, data.type=dataType())
        })
        xgblinearIntensityModelSet <- reactive(label="xgblinearIntensityModelSet", {
            list(data=predictFrameCheck(xgblinearIntensityModelData()), parameters=xgblinearIntensityParameters())
        })
        xgblinearIntensityModel <- reactive(label="xglinearIntensityModel", {
            req(input$radiocal, input$calcurveelement)
            
            predict.frame <- xgblinearIntensityModelSet()$data[xgblinearIntensityModelSet()$parameters$StandardsUsed,]
            parameters <- xgblinearIntensityModelSet()$parameters$CalTable
            
            
            xgbalpha.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbAlpha), "-")))
            xgbeta.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbEta), "-")))
            xgblambda.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbLambda), "-")))
            
            xgbGrid <- expand.grid(
            nrounds = parameters$ForestTrees,
            alpha=seq(xgbalpha.vec[1], xgbalpha.vec[2], by=0.1),
            eta = seq(xgbeta.vec[1], xgbeta.vec[2], by=0.1),
            lambda = seq(xgblambda.vec[1], xgblambda.vec[2], by=0.1)
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
                
                
            if(input$multicore_behavior=="Single Core"){
                xgb_model <- caret::train(Concentration~., data=predict.frame, trControl = tune_control, tuneGrid = xgbGrid, metric=parameters$ForestMetric, method = "xgbLinear", na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                xgb_model <- caret::train(Concentration~., data=predict.frame, trControl = tune_control, tuneGrid = xgbGrid, metric=parameters$ForestMetric, method = "xgbLinear", na.action=na.omit, allowParallel=TRUE)
                stopCluster(cl)
            } else if(input$multicore_behavior=="OpenMP"){
                xgb_model <- caret::train(Concentration~., data=predict.frame, trControl = tune_control, tuneGrid = xgbGrid, metric=parameters$ForestMetric, method = "xgbLinear", na.action=na.omit, nthread=as.numeric(cores.to.use))
            }
            
            xgb_model
            
        })
        
        
        xgboostIntensityModelSet <- reactive(label="xgboostIntensityModelSet", {
            if(xgboosthold$xgbtype=="Tree"){
                xgbtreeIntensityModelSet()
            } else if(xgboosthold$xgbtype=="Linear"){
                xgblinearIntensityModelSet()
            }
        })
        
        xgboostIntensityModel <- reactive(label="xgboostIntensityModel", {
            
            if(xgboosthold$xgbtype=="Tree"){
                xgbtreeIntensityModel()
            } else if(xgboosthold$xgbtype=="Linear"){
                xgblinearIntensityModel()
            }
            
        })
        
        xgbtreeSpectraParameters <- reactive(label="xgbtreeSpectraParameters", {
            cvrepeats <- if(foresthold$foresttrain=="repeatedcv"){
                cvRepeatsSelection()
            } else if(foresthold$foresttrain!="repeatedcv"){
                1
            }
            energy.range <- basicEnergyRange()
            treedepth <- xgboostTreeDepthSelection()
            eta <- xgboostEtaSelection()
            gamma <- xgboostGammaSelection()
            subsample <- xgboostSubSampleSelection()
            colsample <- xgboostColSampleSelection()
            list(CalTable=calConditionsTable(cal.type=9, line.type=input$linepreferenceelement, compress=basichold$compress, transformation=basichold$transformation, energy.range=paste0(energy.range[1], "-", energy.range[2]), norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation(), foresttrees=forestTreeSelection(), forestmetric=forestMetricSelection(), foresttrain=forestTrainSelection(), forestnumber=forestNumberSelection(), cvrepeats=cvrepeats, xgbtype=xgboosthold$xgbtype, treedepth=paste0(treedepth[1], "-", treedepth[2]), xgbeta=paste0(eta[1], "-", eta[2]), xgbgamma=paste0(gamma[1], "-", gamma[2]), xgbsubsample=paste0(subsample[1], "-", subsample[2]), xgbcolsample=paste0(colsample[1], "-", colsample[2]), xgbminchild=xgboostMinChildSelection()), StandardsUsed=vals$keeprows)
        })
        xgbtreeSpectraModelData <- reactive(label="xgbtreeSpectraModelData", {
            rainforestDataGen(spectra=dataNormCal(), compress=xgbtreeSpectraParameters()$CalTable$Compress, transformation=xgbtreeSpectraParameters()$CalTable$Transformation, dependent.transformation=xgbtreeSpectraParameters()$CalTable$DepTrans,  energy.range=as.numeric(unlist(strsplit(as.character(xgbtreeSpectraParameters()$CalTable$EnergyRange), "-"))), hold.frame=holdFrameCal(), norm.type=xgbtreeSpectraParameters()$CalTable$NormType, norm.min=xgbtreeSpectraParameters()$CalTable$Min, norm.max=xgbtreeSpectraParameters()$CalTable$Max, data.type=dataType())
        })
        xgbtreeSpectraModelSet <- reactive(label="xgbtreeSpectraModelSet", {
            list(data=predictFrameCheck(xgbtreeSpectraModelData()), parameters=xgbtreeSpectraParameters())
        })
        xgbtreeSpectraModel <- reactive(label="xgbtreeSpectraModel", {
            req(input$radiocal, input$calcurveelement)
            
            data <- xgbtreeSpectraModelSet()$data[xgbtreeSpectraModelSet()$parameters$StandardsUsed,]
            parameters <- xgbtreeSpectraModelSet()$parameters$CalTable
            
            tree.depth.vec <- as.numeric(unlist(strsplit(as.character(parameters$TreeDepth), "-")))
            xgbeta.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbEta), "-")))
            xgbgamma.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbGamma), "-")))
            xgbsubsample.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbSubSample), "-")))
            xgbcolsample.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbColSample), "-")))
            
            
            xgbGrid <- expand.grid(
            nrounds = parameters$ForestTrees,
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
                
            
                
            if(input$multicore_behavior=="Single Core"){
                xgb_model <- caret::train(Concentration~., data=data[,-1], trControl = tune_control, tuneGrid = xgbGrid, metric=parameters$ForestMetric, method = "xgbTree", na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                xgb_model <- caret::train(Concentration~., data=data[,-1], trControl = tune_control, tuneGrid = xgbGrid, metric=parameters$ForestMetric, method = "xgbTree", na.action=na.omit, allowParallel=TRUE)
                stopCluster(cl)
            } else if(input$multicore_behavior=="OpenMP"){
                xgb_model <- caret::train(Concentration~., data=data[,-1], trControl = tune_control, tuneGrid = xgbGrid, metric=parameters$ForestMetric, method = "xgbTree", na.action=na.omit, nthread=as.numeric(cores.to.use))
            }
            
            xgb_model
            
        })
        
        xgblinearSpectraParameters <- reactive(label="xgblinearSpectraParameters", {
            cvrepeats <- if(foresthold$foresttrain=="repeatedcv"){
                cvRepeatsSelection()
            } else if(foresthold$foresttrain!="repeatedcv"){
                1
            }
            energy.range <- basicEnergyRange()
            eta <- xgboostEtaSelection()
            alpha <- xgboostAlphaSelection()
            lambda <- xgboostLambdaSelection()
            list(CalTable=calConditionsTable(cal.type=9, line.type=input$linepreferenceelement, compress=basichold$compress, transformation=basichold$transformation, energy.range=paste0(energy.range[1], "-", energy.range[2]), norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation(), foresttrees=forestTreeSelection(), forestmetric=forestMetricSelection(), foresttrain=forestTrainSelection(), forestnumber=forestNumberSelection(), cvrepeats=cvrepeats, xgbtype=xgboosthold$xgbtype, xgbalpha=paste0(alpha[1], "-", alpha[2]), xgbeta=paste0(eta[1], "-", eta[2]), xgblambda=paste0(lambda[1], "-", lambda[2])), StandardsUsed=vals$keeprows)
        })
        xgblinearSpectraModelData <- reactive(label="xgblinearSpectraModelData", {
            rainforestDataGen(spectra=dataNormCal(), compress=xgblinearSpectraParameters()$CalTable$Compress, transformation=xgblinearSpectraParameters()$CalTable$Transformation, dependent.transformation=xgblinearSpectraParameters()$CalTable$DepTrans,  energy.range=as.numeric(unlist(strsplit(as.character(xgblinearSpectraParameters()$CalTable$EnergyRange), "-"))), hold.frame=holdFrameCal(), norm.type=xgblinearSpectraParameters()$CalTable$NormType, norm.min=xgblinearSpectraParameters()$CalTable$Min, norm.max=xgblinearSpectraParameters()$CalTable$Max, data.type=dataType())
        })
        xgblinearSpectraModelSet <- reactive(label="xgblinearSpectraModelSet", {
            list(data=predictFrameCheck(xgblinearSpectraModelData()), parameters=xgblinearSpectraParameters())
        })
        xgblinearSpectraModel <- reactive(label="xgblinearSpectraModel", {
            req(input$radiocal, input$calcurveelement)
            
            data <- xgblinearSpectraModelSet()$data[xgblinearSpectraModelSet()$parameters$StandardsUsed,]
            parameters <- xgblinearSpectraModelSet()$parameters$CalTable
            
            xgbalpha.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbAlpha), "-")))
            xgbeta.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbEta), "-")))
            xgblambda.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbLambda), "-")))
            
            
            xgbGrid <- expand.grid(
            nrounds = parameters$ForestTrees,
            alpha=seq(xgbalpha.vec[1], xgbalpha.vec[2], by=0.1),
            eta = seq(xgbeta.vec[1], xgbeta.vec[2], by=0.1),
            lambda = seq(xgblambda.vec[1], xgblambda.vec[2], by=0.1)
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
                
            if(input$multicore_behavior=="Single Core"){
                xgb_model <- caret::train(Concentration~., data=data[,-1], trControl = tune_control, tuneGrid = xgbGrid, metric=parameters$ForestMetric, method = "xgbLinear", na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                xgb_model <- caret::train(Concentration~., data=data[,-1], trControl = tune_control, tuneGrid = xgbGrid, metric=parameters$ForestMetric, method = "xgbLinear", na.action=na.omit, allowParallel=TRUE)
                stopCluster(cl)
            } else if(input$multicore_behavior=="OpenMP"){
                xgb_model <- caret::train(Concentration~., data=data[,-1], trControl = tune_control, tuneGrid = xgbGrid, metric=parameters$ForestMetric, method = "xgbLinear",  na.action=na.omit, nthread=as.numeric(cores.to.use))
            }
            
            xgb_model
            
        })
        
        xgboostSpectraModelSet <- reactive(label="xgboostSpectraModelSet", {
            if(xgboosthold$xgbtype=="Tree"){
                xgbtreeSpectraModelSet()
            } else if(xgboosthold$xgbtype=="Linear"){
                xgblinearSpectraModelSet()
            }
        })
        
        xgboostSpectraModel <- reactive(label="xgboostSpectraModel", {
            
            if(xgboosthold$xgbtype=="Tree"){
                xgbtreeSpectraModel()
            } else if(xgboosthold$xgbtype=="Linear"){
                xgblinearSpectraModel()
            }
            
        })
        
        
        bartMachineIntensityParameters <- reactive(label="bartMachineIntensityParameters", {
            alpha <- xgboostAlphaSelection()
            beta <- bartBetaSelection()
            nu <- bartNuSelection()
            k <- bartKSelection()
            cvrepeats <- if(foresthold$foresttrain=="repeatedcv"){
                foresthold$cvrepeats
            } else if(foresthold$foresttrain!="repeatedcv"){
                1
            }
            list(CalTable=calConditionsTable(cal.type=10, line.type=input$linepreferenceelement, norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation(),  forestmetric=forestMetricSelection(), foresttrain=forestTrainSelection(), forestnumber=forestNumberSelection(), cvrepeats=cvrepeats, xgbtype=xgboosthold$xgbtype, foresttrees=forestTreeSelection()), bartK <- paste0(k[1], "-", k[2]), xgbalpha=paste0(alpha[1], "-", alpha[2]), bartbeta=paste0(beta[1], "-", beta[2]), bartnu=paste0(nu[1], "-", nu[2]), Slope=lucasSlope(), Intercept=lucasIntercept(), StandardsUsed=vals$keeprows)
        })
        bartMachineIntensityModelData <- reactive(label="bartMachineIntensityModelData", {
            predictFrameForestGen(spectra=dataNormCal(), hold.frame=holdFrameCal(), dependent.transformation=bartMachineIntensityParameters()$CalTable$DepTrans, element=input$calcurveelement, intercepts=bartMachineIntensityParameters()$Intercept, slopes=bartMachineIntensityParameters()$Slope, norm.type=bartMachineIntensityParameters()$CalTable$NormType, norm.min=bartMachineIntensityParameters()$CalTable$Min, norm.max=bartMachineIntensityParameters()$CalTable$Max, data.type=dataType())
        })
        bartMachineIntensityModelSet <- reactive(label="bartMachineIntensityModelSet", {
            list(data=predictFrameCheck(bartMachineIntensityModelData()), parameters=bartMachineIntensityParameters())
        })
        bartMachineIntensityModel <- reactive(label="bartMachineIntensityModel", {
            req(input$radiocal, input$calcurveelement)
            predict.frame <- bartMachineIntensityModelSet()$data[bartMachineIntensityModelSet()$parameters$StandardsUsed,]
            parameters <- bartMachineIntensityModelSet()$parameters$CalTable
            
            xgbalpha.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbAlpha), "-")))
            k.vec <- dnorminv(1-(as.numeric(unlist(strsplit(as.character(parameters$bartK), "-")))/100))
            bartbeta.vec <- as.numeric(unlist(strsplit(as.character(parameters$bartBeta), "-")))
            bartnu.vec <- as.numeric(unlist(strsplit(as.character(parameters$bartNu), "-")))
            
            bart.grid <- expand.grid(
                num_trees=parameters$ForestTrees,
                alpha=seq(xgbalpha.vec[1], xgbalpha.vec[2], by=0.1),
                beta=seq(bartbeta.vec[1], bartbeta.vec[2], by=0.1),
                nu=seq(bartnu.vec[1], bartnu.vec[2], by=0.1),
                k=seq(k.vec[1], k.vec[2], by=0.5))
            
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
                verboseIter = TRUE,
                allowParallel = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE,
                allowParallel = TRUE)
            }
            
            
            
            bart_model <- caret::train(Concentration~., data=predict.frame, method="bartMachine", trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=bart.grid, na.action=na.omit, serialize = TRUE)
            
            stopCluster(cl)
            bart_model
            
        })
        
        
        bayesLinearIntensityParameters <- reactive(label="bayesLinearIntensityParameters", {
            cvrepeats <- if(foresthold$foresttrain=="repeatedcv"){
                foresthold$cvrepeats
            } else if(foresthold$foresttrain!="repeatedcv"){
                1
            }
            list(CalTable=calConditionsTable(cal.type=10, line.type=input$linepreferenceelement, norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation(),  forestmetric=forestMetricSelection(), foresttrain=forestTrainSelection(), forestnumber=forestNumberSelection(), cvrepeats=cvrepeats, xgbtype=xgboosthold$xgbtype), Slope=lucasSlope(), Intercept=lucasIntercept(), StandardsUsed=vals$keeprows)
        })
        bayesLinearIntensityModelData <- reactive(label="bayesLinearIntensityModelData", {
            predictFrameForestGen(spectra=dataNormCal(), hold.frame=holdFrameCal(), dependent.transformation=bayesLinearIntensityParameters()$CalTable$DepTrans, element=input$calcurveelement, intercepts=bayesLinearIntensityParameters()$Intercept, slopes=bayesLinearIntensityParameters()$Slope, norm.type=bayesLinearIntensityParameters()$CalTable$NormType, norm.min=bayesLinearIntensityParameters()$CalTable$Min, norm.max=bayesLinearIntensityParameters()$CalTable$Max, data.type=dataType())
        })
        bayesLinearIntensityModelSet <- reactive(label="bayesLinearIntensityModelSet", {
            list(data=predictFrameCheck(bayesLinearIntensityModelData()), parameters=bayesLinearIntensityParameters())
        })
        bayesLinearIntensityModel <- reactive(label="bayesLinearIntensityModel", {
            req(input$radiocal, input$calcurveelement)
            predict.frame <- bayesLinearIntensityModelSet()$data[bayesLinearIntensityModelSet()$parameters$StandardsUsed,]
            parameters <- bayesLinearIntensityModelSet()$parameters$CalTable
            
            
            bart.grid <- NULL
            
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
                        
            if(input$multicore_behavior=="Single Core"){
                bart_model <- caret::train(Concentration~., data=predict.frame, method="bayesglm", trControl=tune_control, metric=parameters$ForestMetric, na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                bart_model <- caret::train(Concentration~., data=predict.frame, method="bayesglm", trControl=tune_control, metric=parameters$ForestMetric, na.action=na.omit, allowParallel=TRUE)
                stopCluster(cl)
            }
            bart_model
            
        })
        
        bayesNeuralNetIntensityParameters <- reactive(label="bayesNeuralNetIntensityParameters", {
            hiddenunits <- neuralHiddenUnitsSelection()
            cvrepeats <- if(foresthold$foresttrain=="repeatedcv"){
                foresthold$cvrepeats
            } else if(foresthold$foresttrain!="repeatedcv"){
                1
            }
            list(CalTable=calConditionsTable(cal.type=10, line.type=input$linepreferenceelement, norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation(),  forestmetric=forestMetricSelection(), foresttrain=forestTrainSelection(), forestnumber=forestNumberSelection(), cvrepeats=cvrepeats, xgbtype=xgboosthold$xgbtype, neuralhiddenunits=paste0(hiddenunits[1], "-", hiddenunits[2])), Slope=lucasSlope(), Intercept=lucasIntercept(), StandardsUsed=vals$keeprows)
        })
        bayesNeuralNetModelData <- reactive(label="bayesNeuralNetModelData", {
            predictFrameForestGen(spectra=dataNormCal(), hold.frame=holdFrameCal(), dependent.transformation=bayesNeuralNetIntensityParameters()$CalTable$DepTrans, element=input$calcurveelement, intercepts=bayesNeuralNetIntensityParameters()$Intercept, slopes=bayesNeuralNetIntensityParameters()$Slope, norm.type=bayesNeuralNetIntensityParameters()$CalTable$NormType, norm.min=bayesNeuralNetIntensityParameters()$CalTable$Min, norm.max=bayesNeuralNetIntensityParameters()$CalTable$Max, data.type=dataType())
        })
        bayesNeuralNetIntensityModelSet <- reactive(label="bayesNeuralNetIntensityModelSet", {
            list(data=predictFrameCheck(bayesNeuralNetModelData()), parameters=bayesNeuralNetIntensityParameters())
        })
        bayesNeuralNetIntensityModel <- reactive(label="bayesNeuralNetIntensityModel", {
            req(input$radiocal, input$calcurveelement)
            predict.frame <- bartMachineIntensityModelSet()$data[bartMachineIntensityModelSet()$parameters$StandardsUsed,]
            parameters <- bartMachineIntensityModelSet()$parameters$CalTable
            
            
            hiddenunits.vec <- as.numeric(unlist(strsplit(as.character(parameters$NeuralHU), "-")))
            
            
            bart.grid <- expand.grid(
            neurons = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1))
            
            
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                bart_model <- caret::train(Concentration~.,data=predict.frame, method="brnn", trControl=tune_control, importance=TRUE, metric=parameters$ForestMetric, tuneGrid=bart.grid, na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                bart_model <- caret::train(Concentration~.,data=predict.frame, method="brnn", trControl=tune_control, allowParallel=TRUE, importance=TRUE, metric=parameters$ForestMetric, tuneGrid=bart.grid, na.action=na.omit)
                stopCluster(cl)
            }
            bart_model
            
        })
        
        bayesIntensityModelSet <- reactive(label="bayesIntensityModelSet", {
            if(xgboosthold$xgbtype=="Tree"){
                bartMachineIntensityModelSet()
            } else if(xgboosthold$xgbtype=="Linear"){
                bayesLinearIntensityModelSet()
            } else if(xgboosthold$xgbtype=="Neural Net"){
                bayesNeuralNetIntensityModelSet()
            }
        })
        
        bayesIntensityModel <- reactive(label="bayesIntensityModel", {
            
            if(xgboosthold$xgbtype=="Tree"){
                bartMachineIntensityModel()
            } else if(xgboosthold$xgbtype=="Linear"){
                bayesLinearIntensityModel()
            } else if(xgboosthold$xgbtype=="Neural Net"){
                bayesNeuralNetIntensityModel()
            }
            
        })
        
        
        bartMachineSpectraParameters <- reactive(label="bartMachineSpectraParameters", {
            alpha <- xgboostAlphaSelection()
            beta <- bartBetaSelection()
            nu <- bartNuSelection()
            k <- bartKSelection()
            cvrepeats <- if(foresthold$foresttrain=="repeatedcv"){
                foresthold$cvrepeats
            } else if(foresthold$foresttrain!="repeatedcv"){
                1
            }
            energyrange <- basicEnergyRange()
            list(CalTable=calConditionsTable(cal.type=11, line.type=input$linepreferenceelement, compress=basicCompress(), transformation=basicTransformation(), energy.range=paste0(energyrange[1], "-", energyrange[2]), norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation(), foresttry=forestTrySelection(), forestmetric=forestMetricSelection(), foresttrain=forestTrainSelection(), forestnumber=forestNumberSelection(), cvrepeats=cvrepeats, xgbtype=xgboosthold$xgbtype, foresttrees=forestTreeSelection()), bartK <- paste0(k[1], "-", k[2]), xgbalpha=paste0(alpha[1], "-", alpha[2]), bartbeta=paste0(beta[1], "-", beta[2]), bartnu=paste0(nu[1], "-", nu[2]), StandardsUsed=vals$keeprows)
        })
        bartMachineSpectraModelData <- reactive(label="bartMachineSpectraModelData", {
            rainforestDataGen(spectra=dataNormCal(), compress=bartMachineSpectraParameters()$CalTable$Compress, transformation=bartMachineSpectraParameters()$CalTable$Transformation, dependent.transformation=bartMachineSpectraParameters()$CalTable$DepTrans, energy.range=as.numeric(unlist(strsplit(as.character(bartMachineSpectraParameters()$CalTable$EnergyRange), "-"))), hold.frame=holdFrameCal(), norm.type=bartMachineSpectraParameters()$CalTable$NormType, norm.min=bartMachineSpectraParameters()$CalTable$Min, norm.max=bartMachineSpectraParameters()$CalTable$Max, data.type=dataType())
        })
        #rainforestModelSetlist <- reactiveValues()
        #observeEvent(input$createcalelement, priority=150, {
        #    rainforestModelSet$list <- list(data=rainforestModelData(), parameters=rainforestParameters())
        #})
        
        bartMachineSpectraModelSet <- reactive(label="bartMachineSpectraModelSet", {
            list(data=predictFrameCheck(bartMachineSpectraModelData()), parameters=bartMachineSpectraParameters())
        })
        bartMachineSpectraModel <- reactive(label="bartMachineSpectraModel", {
            req(input$radiocal, input$calcurveelement)
            data <- bartMachineSpectraModelSet()$data[bartMachineSpectraModelSet()$parameters$StandardsUsed,]
            parameters <- bartMachineSpectraModelSet()$parameters$CalTable
            
            xgbalpha.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbAlpha), "-")))
            k.vec <- dnorminv(1-(as.numeric(unlist(strsplit(as.character(parameters$bartK), "-")))/100))
            bartbeta.vec <- as.numeric(unlist(strsplit(as.character(parameters$bartBeta), "-")))
            bartnu.vec <- as.numeric(unlist(strsplit(as.character(parameters$bartNu), "-")))
            
            bart.grid <- expand.grid(
                num_trees=parameters$ForestTrees,
                alpha=seq(xgbalpha.vec[1], xgbalpha.vec[2], by=0.1),
                beta=seq(bartbeta.vec[1], bartbeta.vec[2], by=0.1),
                nu=seq(bartnu.vec[1], bartnu.vec[2], by=0.1),
                k=seq(k.vec[1], k.vec[2], by=0.5))
            
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
                verboseIter = TRUE,
                allowParallel = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE,
                allowParallel = TRUE)
            }
            
            
            
            bart_model <- caret::train(Concentration~.,data=data[,-1], method="bartMachine", trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=bart.grid, na.action=na.omit, serialize = TRUE)
            
            
            bart_model
            
        })
        
        bayesLinearSpectraParameters <- reactive(label="bayesLinearSpectraParameters", {
            cvrepeats <- if(foresthold$foresttrain=="repeatedcv"){
                foresthold$cvrepeats
            } else if(foresthold$foresttrain!="repeatedcv"){
                1
            }
            energyrange <- basicEnergyRange()
            list(CalTable=calConditionsTable(cal.type=11, line.type=input$linepreferenceelement, compress=basicCompress(), transformation=basicTransformation(), energy.range=paste0(energyrange[1], "-", energyrange[2]), norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation(), foresttry=forestTrySelection(), forestmetric=forestMetricSelection(), foresttrain=forestTrainSelection(), forestnumber=forestNumberSelection(), cvrepeats=cvrepeats, xgbtype=xgboosthold$xgbtype), StandardsUsed=vals$keeprows)
        })
        bayesLinearSpectraModelData <- reactive(label="bayesLinearSpectraModelData", {
            rainforestDataGen(spectra=dataNormCal(), compress=bayesLinearSpectraParameters()$CalTable$Compress, transformation=bayesLinearSpectraParameters()$CalTable$Transformation, dependent.transformation=bayesLinearSpectraParameters()$CalTable$DepTrans, energy.range=as.numeric(unlist(strsplit(as.character(bayesLinearSpectraParameters()$CalTable$EnergyRange), "-"))), hold.frame=holdFrameCal(), norm.type=bayesLinearSpectraParameters()$CalTable$NormType, norm.min=bayesLinearSpectraParameters()$CalTable$Min, norm.max=bayesLinearSpectraParameters()$CalTable$Max, data.type=dataType())
        })
        #rainforestModelSetlist <- reactiveValues()
        #observeEvent(input$createcalelement, priority=150, {
        #    rainforestModelSet$list <- list(data=rainforestModelData(), parameters=rainforestParameters())
        #})
        
        bayesLinearSpectraModelSet <- reactive(label="bayesLinearSpectraModelSet", {
            list(data=predictFrameCheck(bayesLinearSpectraModelData()), parameters=bayesLinearSpectraParameters())
        })
        bayesLinearSpectraModel <- reactive(label="bayesLinearSpectraModel", {
            req(input$radiocal, input$calcurveelement)
            data <- bayesLinearSpectraModelSet()$data[bayesLinearSpectraModelSet()$parameters$StandardsUsed,]
            parameters <- bayesLinearSpectraModelSet()$parameters$CalTable
            
            
            bart.grid <- NULL
            
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                bart_model <- caret::train(Concentration~.,data=data[,-1], method="bayesglm", trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=bart.grid)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                bart_model <- caret::train(Concentration~.,data=data[,-1], method="bayesglm", trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=bart.grid)
                stopCluster(cl)
            }
            bart_model
            
        })
        
        bayesNeuralNetSpectraParameters <- reactive(label="bayesNeuralNetSpectraParameters", {
            hiddenunits <- neuralHiddenUnitsSelection()
            cvrepeats <- if(foresthold$foresttrain=="repeatedcv"){
                foresthold$cvrepeats
            } else if(foresthold$foresttrain!="repeatedcv"){
                1
            }
            energyrange <- basicEnergyRange()
            list(CalTable=calConditionsTable(cal.type=11, line.type=input$linepreferenceelement, compress=basicCompress(), transformation=basicTransformation(), energy.range=paste0(energyrange[1], "-", energyrange[2]), norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation(), foresttry=forestTrySelection(), forestmetric=forestMetricSelection(), foresttrain=forestTrainSelection(), forestnumber=forestNumberSelection(), xgbtype=xgboosthold$xgbtype, neuralhiddenunits=paste0(hiddenunits[1], "-", hiddenunits[2]), cvrepeats=cvrepeats), StandardsUsed=vals$keeprows)
        })
        bayesNeuralNetSpectraModelData <- reactive(label="bayesNeuralNetSpectraModelData", {
            rainforestDataGen(spectra=dataNormCal(), compress=bayesNeuralNetSpectraParameters()$CalTable$Compress, transformation=bayesNeuralNetSpectraParameters()$CalTable$Transformation, dependent.transformation=bayesNeuralNetSpectraParameters()$CalTable$DepTrans, energy.range=as.numeric(unlist(strsplit(as.character(bayesNeuralNetSpectraParameters()$CalTable$EnergyRange), "-"))), hold.frame=holdFrameCal(), norm.type=bayesNeuralNetSpectraParameters()$CalTable$NormType, norm.min=bayesNeuralNetSpectraParameters()$CalTable$Min, norm.max=bayesNeuralNetSpectraParameters()$CalTable$Max, data.type=dataType())
        })
        bayesNeuralNetSpectraModelSet <- reactive(label="bayesNeuralNetSpectraModelSet", {
            list(data=predictFrameCheck(bayesNeuralNetSpectraModelData()), parameters=bayesNeuralNetSpectraParameters())
        })
        bayesNeuralNetSpectraModel <- reactive(label="bayesNeuralNetSpectraModel", {
            req(input$radiocal, input$calcurveelement)
            data <- bayesNeuralNetSpectraModelSet()$data[bayesNeuralNetSpectraModelSet()$parameters$StandardsUsed,]
            parameters <- bayesNeuralNetSpectraModelSet()$parameters$CalTable
            
            
            hiddenunits.vec <- as.numeric(unlist(strsplit(as.character(parameters$NeuralHU), "-")))
            
            
            bart.grid <- expand.grid(
            neurons = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1))
            
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            
            if(input$multicore_behavior=="Single Core"){
                bart_model <- caret::train(Concentration~.,data=data[,-1], method="brnn", trControl=tune_control, importance=TRUE, metric=parameters$ForestMetric, tuneGrid=bart.grid, na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                bart_model <- caret::train(Concentration~.,data=data[,-1], method="brnn", trControl=tune_control, allowParallel=TRUE, importance=TRUE, metric=parameters$ForestMetric, tuneGrid=bart.grid, na.action=na.omit)
                stopCluster(cl)
            }
            bart_model
            
        })
        
        bayesSpectraModelSet <- reactive(label="bayesSpectraModelSet", {
            if(xgboosthold$xgbtype=="Tree"){
                bartMachineSpectraModelSet()
            } else if(xgboosthold$xgbtype=="Linear"){
                bayesLinearSpectraModelSet()
            } else if(xgboosthold$xgbtype=="Neural Net"){
                bayesNeuralNetSpectraModelSet()
            }
        })
        
        bayesSpectraModel <- reactive(label="bayesSpectraModel", {
            
            if(xgboosthold$xgbtype=="Tree"){
                bartMachineSpectraModel()
            } else if(xgboosthold$xgbtype=="Linear"){
                bayesLinearSpectraModel()
            } else if(xgboosthold$xgbtype=="Neural Net"){
                bayesNeuralNetSpectraModel()
            }
            
        })
        
        
        
        svmLinearIntensityParameters <- reactive(label="svmLinearIntensityParameters", {
            C <- svmCSelection()
            cvrepeats <- if(foresthold$foresttrain=="repeatedcv"){
                foresthold$cvrepeats
            } else if(foresthold$foresttrain!="repeatedcv"){
                1
            }
            list(CalTable=calConditionsTable(cal.type=12, line.type=input$linepreferenceelement, norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation(),  forestmetric=forestMetricSelection(), foresttrain=forestTrainSelection(), forestnumber=forestNumberSelection(), cvrepeats=cvrepeats, xgbtype=xgboosthold$xgbtype, svmc=paste0(C[1], "-", C[2])), Slope=lucasSlope(), Intercept=lucasIntercept(), StandardsUsed=vals$keeprows)
        })
        svmLinearIntensityModelData <- reactive(label="svmLinearIntensityModelData", {
            predictFrameForestGen(spectra=dataNormCal(), hold.frame=holdFrameCal(), dependent.transformation=svmLinearIntensityParameters()$CalTable$DepTrans, element=input$calcurveelement, intercepts=svmLinearIntensityParameters()$Intercept, slopes=svmLinearIntensityParameters()$Slope, norm.type=svmLinearIntensityParameters()$CalTable$NormType, norm.min=svmLinearIntensityParameters()$CalTable$Min, norm.max=svmLinearIntensityParameters()$CalTable$Max, data.type=dataType())
        })
        svmLinearIntensityModelSet <- reactive(label="svmLinearIntensityModelSet", {
            list(data=predictFrameCheck(svmLinearIntensityModelData()), parameters=svmLinearIntensityParameters())
        })
        svmLinearIntensityModel <- reactive(label="svmLinearIntensityModel", {
            req(input$radiocal, input$calcurveelement)
            predict.frame <- svmLinearIntensityModelSet()$data[svmLinearIntensityModelSet()$parameters$StandardsUsed,]
            parameters <- svmLinearIntensityModelSet()$parameters$CalTable
            
            
            svmc.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmC), "-")))
            
            
            svm.grid <- expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1))
            
            
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                svm_model <- caret::train(Concentration~.,data=predict.frame, method="svmLinear", trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                svm_model <- caret::train(Concentration~.,data=predict.frame, method="svmLinear", trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
                stopCluster(cl)
            }
            svm_model
            
        })
        
        svmPolyIntensityParameters <- reactive(label="svmPolyIntensityParameters", {
            C <- svmCSelection()
            degree <- svmDegreeSelection()
            scale <- svmScaleSelection()
            cvrepeats <- if(foresthold$foresttrain=="repeatedcv"){
                foresthold$cvrepeats
            } else if(foresthold$foresttrain!="repeatedcv"){
                1
            }
            list(CalTable=calConditionsTable(cal.type=12, line.type=input$linepreferenceelement, norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation(),  forestmetric=forestMetricSelection(), foresttrain=forestTrainSelection(), forestnumber=forestNumberSelection(), cvrepeats=cvrepeats, xgbtype=xgboosthold$xgbtype, svmc=paste0(C[1], "-", C[2]), svmdegree=paste0(degree[1], "-", degree[2]), svmscale=paste0(scale[1], "-", scale[2])), Slope=lucasSlope(), Intercept=lucasIntercept(), StandardsUsed=vals$keeprows)
        })
        svmPolyIntensityModelData <- reactive(label="svmPolyIntensityModelData", {
            predictFrameForestGen(spectra=dataNormCal(), hold.frame=holdFrameCal(), dependent.transformation=svmPolyIntensityParameters()$CalTable$DepTrans, element=input$calcurveelement, intercepts=svmPolyIntensityParameters()$Intercept, slopes=svmPolyIntensityParameters()$Slope, norm.type=svmPolyIntensityParameters()$CalTable$NormType, norm.min=svmPolyIntensityParameters()$CalTable$Min, norm.max=svmPolyIntensityParameters()$CalTable$Max, data.type=dataType())
        })
        svmPolyIntensityModelSet <- reactive(label="svmPolyIntensityModelSet", {
            list(data=predictFrameCheck(svmPolyIntensityModelData()), parameters=svmPolyIntensityParameters())
        })
        svmPolyIntensityModel <- reactive(label="svmPolyIntensityModel", {
            req(input$radiocal, input$calcurveelement)
            predict.frame <- svmPolyIntensityModelSet()$data[svmPolyIntensityModelSet()$parameters$StandardsUsed,]
            parameters <- svmPolyIntensityModelSet()$parameters$CalTable
            
            
            svmc.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmC), "-")))
            svmdegree.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmDegree), "-")))
            svmscale.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmScale), "-")))

            
            svm.grid <- expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1),
            scale=seq(svmscale.vec[1], svmscale.vec[2], 1),
            degree=seq(svmdegree.vec[1], svmdegree.vec[2], 1))
            
            
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
                verboseIter = TRUE,
                allowParallel = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE,
                allowParallel = TRUE)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                svm_model <- caret::train(Concentration~.,data=predict.frame, method="svmPoly", trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                svm_model <- caret::train(Concentration~.,data=predict.frame, method="svmPoly", trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
                stopCluster(cl)
            }
            svm_model
            
        })
        
        svmRadialIntensityParameters <- reactive(label="svmRadialIntensityParameters", {
            C <- svmCSelection()
            sigma <- svmSigmaSelection()
            cvrepeats <- if(foresthold$foresttrain=="repeatedcv"){
                foresthold$cvrepeats
            } else if(foresthold$foresttrain!="repeatedcv"){
                1
            }
            list(CalTable=calConditionsTable(cal.type=12, line.type=input$linepreferenceelement, norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation(),  forestmetric=forestMetricSelection(), foresttrain=forestTrainSelection(), forestnumber=forestNumberSelection(), cvrepeats=cvrepeats, xgbtype=xgboosthold$xgbtype, svmc=paste0(C[1], "-", C[2]), svmsigma=paste0(sigma[1], "-", sigma[2])), Slope=lucasSlope(), Intercept=lucasIntercept(), StandardsUsed=vals$keeprows)
        })
        svmRadialIntensityModelData <- reactive(label="svmRadialIntensityModelData", {
            predictFrameForestGen(spectra=dataNormCal(), hold.frame=holdFrameCal(), dependent.transformation=svmRadialIntensityParameters()$CalTable$DepTrans, element=input$calcurveelement, intercepts=svmRadialIntensityParameters()$Intercept, slopes=svmRadialIntensityParameters()$Slope, norm.type=svmRadialIntensityParameters()$CalTable$NormType, norm.min=svmRadialIntensityParameters()$CalTable$Min, norm.max=svmRadialIntensityParameters()$CalTable$Max, data.type=dataType())
        })
        svmRadialIntensityModelSet <- reactive(label="svmRadialIntensityModelSet", {
            list(data=predictFrameCheck(svmRadialIntensityModelData()), parameters=svmRadialIntensityParameters())
        })
        svmRadialIntensityModel <- reactive(label="svmRadialIntensityModel", {
            req(input$radiocal, input$calcurveelement)
            predict.frame <- svmRadialIntensityModelSet()$data[svmRadialIntensityModelSet()$parameters$StandardsUsed,]
            parameters <- svmRadialIntensityModelSet()$parameters$CalTable
            
            
            svmc.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmC), "-")))
            svmsigma.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmSigma), "-")))
            
            svm.grid <- if(parameters$xgbType!="Radial Cost"){
                expand.grid(
                C = seq(svmc.vec[1], svmc.vec[2], 1),
                sigma=seq(svmsigma.vec[1], svmsigma.vec[2], 1))
            } else if(parameters$xgbType=="Radial Cost"){
                expand.grid(
                C = seq(svmc.vec[1], svmc.vec[2], 1))
            }
            
            svm.flavor <- if(parameters$xgbType=="Radial"){
                "svmRadial"
            } else if(parameters$xgbType=="Radial Cost"){
                "svmRadialCost"
            } else if(parameters$xgbType=="Radial Sigma"){
                "svmRadialSigma"
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                summaryFunction=metricModel,
                verboseIter = TRUE)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                svm_model <- caret::train(Concentration~.,data=predict.frame, method=svm.flavor, trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                svm_model <- caret::train(Concentration~.,data=predict.frame, method=svm.flavor, trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
                stopCluster(cl)
            }
            svm_model
            
        })
        
        svmBoundrangeIntensityParameters <- reactive(label="svmBoundrangeIntensityParameters", {
            C <- svmCSelection()
            length <- svmLengthSelection()
            cvrepeats <- if(foresthold$foresttrain=="repeatedcv"){
                foresthold$cvrepeats
            } else if(foresthold$foresttrain!="repeatedcv"){
                1
            }
            list(CalTable=calConditionsTable(cal.type=12, line.type=input$linepreferenceelement, norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation(),  forestmetric=forestMetricSelection(), foresttrain=forestTrainSelection(), forestnumber=forestNumberSelection(), cvrepeats=cvrepeats, xgbtype=xgboosthold$xgbtype, svmc=paste0(C[1], "-", C[2]),  svmlength=paste0(length[1], "-", length[2])), Slope=lucasSlope(), Intercept=lucasIntercept(), StandardsUsed=vals$keeprows)
        })
        svmBoundrangeIntensityModelData <- reactive(label="svmBoundrangeIntensityModelData", {
            predictFrameForestGen(spectra=dataNormCal(), hold.frame=holdFrameCal(), dependent.transformation=svmBoundrangeIntensityParameters()$CalTable$DepTrans, element=input$calcurveelement, intercepts=svmBoundrangeIntensityParameters()$Intercept, slopes=svmBoundrangeIntensityParameters()$Slope, norm.type=svmBoundrangeIntensityParameters()$CalTable$NormType, norm.min=svmBoundrangeIntensityParameters()$CalTable$Min, norm.max=svmBoundrangeIntensityParameters()$CalTable$Max, data.type=dataType())
        })
        svmBoundrangeIntensityModelSet <- reactive(label="svmBoundrangeIntensityModelSet", {
            list(data=predictFrameCheck(svmBoundrangeIntensityModelData()), parameters=svmBoundrangeIntensityParameters())
        })
        svmBoundrangeIntensityModel <- reactive(label="svmBoundrangeIntensityModel", {
            req(input$radiocal, input$calcurveelement)
            predict.frame <- svmBoundrangeIntensityModelSet()$data[svmBoundrangeIntensityModelSet()$parameters$StandardsUsed,]
            parameters <- svmBoundrangeIntensityModelSet()$parameters$CalTable
            
            
            svmc.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmC), "-")))
            svmlength.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmLength), "-")))
            
            svm.grid <- expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1),
            length=seq(svmlength.vec[1], svmlength.vec[2], 1))
            
            
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                svm_model <- caret::train(Concentration~.,data=predict.frame, method="svmBoundrangeString", trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                svm_model <- caret::train(Concentration~.,data=predict.frame, method="svmBoundrangeString", trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
                stopCluster(cl)
            }
            svm_model
            
        })
        
        svmExponentialIntensityParameters <- reactive(label="svmExponentialIntensityParameters", {
            C <- svmCSelection()
            lambda <- xgboostLambdaSelection()
            cvrepeats <- if(foresthold$foresttrain=="repeatedcv"){
                foresthold$cvrepeats
            } else if(foresthold$foresttrain!="repeatedcv"){
                1
            }
            list(CalTable=calConditionsTable(cal.type=12, line.type=input$linepreferenceelement, norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation(),  forestmetric=forestMetricSelection(), foresttrain=forestTrainSelection(), forestnumber=forestNumberSelection(), cvrepeats=cvrepeats, xgbtype=xgboosthold$xgbtype, svmc=paste0(C[1], "-", C[2]),  xgblambda=paste0(lambda[1], "-", lambda[2])), Slope=lucasSlope(), Intercept=lucasIntercept(), StandardsUsed=vals$keeprows)
        })
        svmExponentialIntensityModelData <- reactive(label="svmExponentialIntensityModelData", {
            predictFrameForestGen(spectra=dataNormCal(), hold.frame=holdFrameCal(), dependent.transformation=svmExponentialIntensityParameters()$CalTable$DepTrans, element=input$calcurveelement, intercepts=svmExponentialIntensityParameters()$Intercept, slopes=svmExponentialIntensityParameters()$Slope, norm.type=svmExponentialIntensityParameters()$CalTable$NormType, norm.min=svmExponentialIntensityParameters()$CalTable$Min, norm.max=svmExponentialIntensityParameters()$CalTable$Max, data.type=dataType())
        })
        svmExponentialIntensityModelSet <- reactive(label="svmExponentialIntensityModelSet", {
            list(data=predictFrameCheck(svmExponentialIntensityModelData()), parameters=svmExponentialIntensityParameters())
        })
        svmExponentialIntensityModel <- reactive(label="svmExponentialIntensityModel", {
            req(input$radiocal, input$calcurveelement)
            predict.frame <- svmExponentialIntensityModelSet()$data[svmExponentialIntensityModelSet()$parameters$StandardsUsed,]
            parameters <- svmExponentialIntensityModelSet()$parameters$CalTable
            
            
            svmc.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmC), "-")))
            xgblambda.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbLambda), "-")))
            
            svm.grid <- expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1),
            lambda=seq(xgblambda.vec[1], xgblambda.vec[2], 1))
            
            
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                svm_model <- caret::train(Concentration~.,data=predict.frame, method="svmExpoString", trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                svm_model <- caret::train(Concentration~.,data=predict.frame, method="svmExpoString", trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
                stopCluster(cl)
            }
            svm_model
            
        })
        
        svmSpectrumIntensityParameters <- reactive(label="svmSpectrumIntensityParameters", {
            C <- svmCSelection()
            lambda <- xgboostLambdaSelection()
            cvrepeats <- if(foresthold$foresttrain=="repeatedcv"){
                foresthold$cvrepeats
            } else if(foresthold$foresttrain!="repeatedcv"){
                1
            }
            list(CalTable=calConditionsTable(cal.type=12, line.type=input$linepreferenceelement, norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation(),  forestmetric=forestMetricSelection(), foresttrain=forestTrainSelection(), forestnumber=forestNumberSelection(), cvrepeats=cvrepeats, xgbtype=xgboosthold$xgbtype, svmc=paste0(C[1], "-", C[2]),  xgblambda=paste0(lambda[1], "-", lambda[2])), Slope=lucasSlope(), Intercept=lucasIntercept(), StandardsUsed=vals$keeprows)
        })
        svmSpectrumIntensityModelData <- reactive(label="svmSpectrumIntensityModelData", {
            predictFrameForestGen(spectra=dataNormCal(), hold.frame=holdFrameCal(), dependent.transformation=svmSpectrumIntensityParameters()$CalTable$DepTrans, element=input$calcurveelement, intercepts=svmSpectrumIntensityParameters()$Intercept, slopes=svmSpectrumIntensityParameters()$Slope, norm.type=svmSpectrumIntensityParameters()$CalTable$NormType, norm.min=svmSpectrumIntensityParameters()$CalTable$Min, norm.max=svmSpectrumIntensityParameters()$CalTable$Max, data.type=dataType())
        })
        svmSpectrumIntensityModelSet <- reactive(label="svmSpectrumIntensityModelSet", {
            list(data=predictFrameCheck(svmSpectrumIntensityModelData()), parameters=svmSpectrumIntensityParameters())
        })
        svmSpectrumIntensityModel <- reactive(label="svmSpectrumIntensityModel", {
            req(input$radiocal, input$calcurveelement)
            predict.frame <- svmSpectrumIntensityModelSet()$data[svmSpectrumIntensityModelSet()$parameters$StandardsUsed,]
            parameters <- svmSpectrumIntensityModelSet()$parameters$CalTable
            
            
            svmc.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmC), "-")))
            svmlength.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmLength), "-")))
            
            svm.grid <- expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1),
            length=seq(svmlength.vec[1], svmlength.vec[2], 1))
            
            
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                svm_model <- caret::train(Concentration~.,data=predict.frame, method="svmSpectrumString", trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                svm_model <- caret::train(Concentration~.,data=predict.frame, method="svmSpectrumString", trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
                stopCluster(cl)
            }
            svm_model
            
        })
        
        svmIntensityModelSet <- reactive(label="svmIntensityModelSet", {
            if(xgboosthold$xgbtype=="Linear"){
                svmLinearIntensityModelSet()
            } else if(xgboosthold$xgbtype=="Polynomial"){
                svmPolyIntensityModelSet()
            } else if(xgboosthold$xgbtype=="Exponential"){
                svmExponentialIntensityModelSet()
            } else if(xgboosthold$xgbtype=="Radial" | xgboosthold$xgbtype=="Radial Cost" | xgboosthold$xgbtype=="Radial Sigma"){
                svmRadialIntensityModelSet()
            } else if(xgboosthold$xgbtype=="Boundrange String"){
                svmBoundrangeIntensityModelSet()
            } else if(xgboosthold$xgbtype=="Spectrum String"){
                svmSpectrumIntensityModelSet()
            }
        })
        
        svmIntensityModel <- reactive(label="svmIntensityModel", {
            
            if(xgboosthold$xgbtype=="Linear"){
                svmLinearIntensityModel()
            } else if(xgboosthold$xgbtype=="Polynomial"){
                svmPolyIntensityModel()
            } else if(xgboosthold$xgbtype=="Exponential"){
                svmExponentialIntensityModel()
            } else if(xgboosthold$xgbtype=="Radial" | xgboosthold$xgbtype=="Radial Cost" | xgboosthold$xgbtype=="Radial Sigma"){
                svmRadialIntensityModel()
            } else if(xgboosthold$xgbtype=="Boundrange String"){
                svmBoundrangeIntensityModel()
            } else if(xgboosthold$xgbtype=="Spectrum String"){
                svmSpectrumIntensityModel()
            }
            
        })
        
        svmLinearSpectraParameters <- reactive(label="svmLinearSpectraParameters", {
            energyrange <- basicEnergyRange()
            C <- svmCSelection()
            cvrepeats <- if(foresthold$foresttrain=="repeatedcv"){
                foresthold$cvrepeats
            } else if(foresthold$foresttrain!="repeatedcv"){
                1
            }
            list(CalTable=calConditionsTable(cal.type=13, line.type=input$linepreferenceelement, compress=basicCompress(), transformation=basicTransformation(), energy.range=paste0(energyrange[1], "-", energyrange[2]), norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation(),  forestmetric=forestMetricSelection(), foresttrain=forestTrainSelection(), forestnumber=forestNumberSelection(), cvrepeats=cvrepeats, xgbtype=xgboosthold$xgbtype, svmc=paste0(C[1], "-", C[2])),  StandardsUsed=vals$keeprows)
        })
        svmLinearSpectraModelData <- reactive(label="svmLinearSpectraModelData", {
            rainforestDataGen(spectra=dataNormCal(), compress=svmLinearSpectraParameters()$CalTable$Compress, transformation=svmLinearSpectraParameters()$CalTable$Transformation, dependent.transformation=svmLinearSpectraParameters()$CalTable$DepTrans, energy.range=as.numeric(unlist(strsplit(as.character(svmLinearSpectraParameters()$CalTable$EnergyRange), "-"))), hold.frame=holdFrameCal(), norm.type=svmLinearSpectraParameters()$CalTable$NormType, norm.min=svmLinearSpectraParameters()$CalTable$Min, norm.max=svmLinearSpectraParameters()$CalTable$Max, data.type=dataType())
        })
        svmLinearSpectraModelSet <- reactive(label="svmLinearSpectraModelSet", {
            list(data=predictFrameCheck(svmLinearSpectraModelData()), parameters=svmLinearSpectraParameters())
        })
        svmLinearSpectraModel <- reactive(label="svmLinearSpectraModel", {
            req(input$radiocal, input$calcurveelement)
            data <- svmLinearSpectraModelSet()$data[svmLinearSpectraModelSet()$parameters$StandardsUsed,]
            parameters <- svmLinearSpectraModelSet()$parameters$CalTable
            
            
            svmc.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmC), "-")))
            
            
            svm.grid <- expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1))
            
            
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                svm_model <- caret::train(Concentration~.,data=data[,-1], method="svmLinear", trControl=tune_control,  metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                svm_model <- caret::train(Concentration~.,data=data[,-1], method="svmLinear", trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
                stopCluster(cl)
            }
            svm_model
            
        })
        
        svmPolySpectraParameters <- reactive(label="svmPolySpectraParameters", {
            energyrange <- basicEnergyRange()
            C <- svmCSelection()
            degree <- svmDegreeSelection()
            scale <- svmScaleSelection()
            cvrepeats <- if(foresthold$foresttrain=="repeatedcv"){
                foresthold$cvrepeats
            } else if(foresthold$foresttrain!="repeatedcv"){
                1
            }
            list(CalTable=calConditionsTable(cal.type=13, line.type=input$linepreferenceelement, compress=basicCompress(), transformation=basicTransformation(), energy.range=paste0(energyrange[1], "-", energyrange[2]), norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation(),  forestmetric=forestMetricSelection(), foresttrain=forestTrainSelection(), forestnumber=forestNumberSelection(), cvrepeats=cvrepeats, xgbtype=xgboosthold$xgbtype, svmc=paste0(C[1], "-", C[2]), svmdegree=paste0(degree[1], "-", degree[2]), svmscale=paste0(scale[1], "-", scale[2])),  StandardsUsed=vals$keeprows)
        })
        svmPolySpectraModelData <- reactive(label="svmPolySpectraModelData", {
            rainforestDataGen(spectra=dataNormCal(), compress=svmPolySpectraParameters()$CalTable$Compress, transformation=svmPolySpectraParameters()$CalTable$Transformation, dependent.transformation=svmPolySpectraParameters()$CalTable$DepTrans, energy.range=as.numeric(unlist(strsplit(as.character(svmPolySpectraParameters()$CalTable$EnergyRange), "-"))), hold.frame=holdFrameCal(), norm.type=svmPolySpectraParameters()$CalTable$NormType, norm.min=svmPolySpectraParameters()$CalTable$Min, norm.max=svmPolySpectraParameters()$CalTable$Max, data.type=dataType())
        })
        svmPolySpectraModelSet <- reactive(label="svmPolySpectraModelSet", {
            list(data=predictFrameCheck(svmPolySpectraModelData()), parameters=svmPolySpectraParameters())
        })
        svmPolySpectraModel <- reactive(label="svmPolyIntensityModel", {
            req(input$radiocal, input$calcurveelement)
            data <- svmPolySpectraModelSet()$data[svmPolySpectraModelSet()$parameters$StandardsUsed,]
            parameters <- svmPolySpectraModelSet()$parameters$CalTable
            
            
            svmc.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmC), "-")))
            svmdegree.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmDegree), "-")))
            svmscale.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmScale), "-")))

            
            svm.grid <- expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1),
            scale=seq(svmscale.vec[1], svmscale.vec[2], 1),
            degree=seq(svmdegree.vec[1], svmdegree.vec[2], 1))
            
            
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                svm_model <- caret::train(Concentration~., data=data[,-1], method="svmPoly", trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                svm_model <- caret::train(Concentration~., data=data[,-1], method="svmPoly", trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
                stopCluster(cl)
            }
            svm_model
            
        })
        
        svmRadialSpectraParameters <- reactive(label="svmRadialSpectraParameters", {
            energyrange <- basicEnergyRange()
            C <- svmCSelection()
            sigma <- svmSigmaSelection()
            cvrepeats <- if(foresthold$foresttrain=="repeatedcv"){
                foresthold$cvrepeats
            } else if(foresthold$foresttrain!="repeatedcv"){
                1
            }
            list(CalTable=calConditionsTable(cal.type=13, line.type=input$linepreferenceelement, compress=basicCompress(), transformation=basicTransformation(), energy.range=paste0(energyrange[1], "-", energyrange[2]), norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation(),  forestmetric=forestMetricSelection(), foresttrain=forestTrainSelection(), forestnumber=forestNumberSelection(), cvrepeats=cvrepeats, xgbtype=xgboosthold$xgbtype, svmc=paste0(C[1], "-", C[2]), svmsigma=paste0(sigma[1], "-", sigma[2])), StandardsUsed=vals$keeprows)
        })
        svmRadialSpectraModelData <- reactive(label="svmRadialSpectraModelData", {
            rainforestDataGen(spectra=dataNormCal(), compress=svmRadialSpectraParameters()$CalTable$Compress, transformation=svmRadialSpectraParameters()$CalTable$Transformation, dependent.transformation=svmRadialSpectraParameters()$CalTable$DepTrans, energy.range=as.numeric(unlist(strsplit(as.character(svmRadialSpectraParameters()$CalTable$EnergyRange), "-"))), hold.frame=holdFrameCal(), norm.type=svmRadialSpectraParameters()$CalTable$NormType, norm.min=svmRadialSpectraParameters()$CalTable$Min, norm.max=svmRadialSpectraParameters()$CalTable$Max, data.type=dataType())
        })
        svmRadialSpectraModelSet <- reactive(label="svmRadialSpectraModelSet", {
            list(data=predictFrameCheck(svmRadialSpectraModelData()), parameters=svmRadialSpectraParameters())
        })
        svmRadialSpectraModel <- reactive(label="svmRadialSpectraModel", {
            req(input$radiocal, input$calcurveelement)
            data <- svmRadialSpectraModelSet()$data[svmRadialSpectraModelSet()$parameters$StandardsUsed,]
            parameters <- svmRadialSpectraModelSet()$parameters$CalTable
            
            
            svmc.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmC), "-")))
            svmsigma.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmSigma), "-")))
            
            svm.grid <- if(parameters$xgbType!="Radial Cost"){
                expand.grid(
                C = seq(svmc.vec[1], svmc.vec[2], 1),
                sigma=seq(svmsigma.vec[1], svmsigma.vec[2], 1))
            } else if(parameters$xgbType=="Radial Cost"){
                expand.grid(
                C = seq(svmc.vec[1], svmc.vec[2], 1))
            }
            
            svm.flavor <- if(parameters$xgbType=="Radial"){
                "svmRadial"
            } else if(parameters$xgbType=="Radial Cost"){
                "svmRadialCost"
            } else if(parameters$xgbType=="Radial Sigma"){
                "svmRadialSigma"
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                svm_model <- caret::train(Concentration~.,data=data[,-1], method=svm.flavor, trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                svm_model <- caret::train(Concentration~.,data=data[,-1], method=svm.flavor, trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
                stopCluster(cl)
            }
            svm_model
            
        })
        
        svmBoundrangeSpectraParameters <- reactive(label="svmBoundrangeSpectraParameters", {
            energyrange <- basicEnergyRange()
            C <- svmCSelection()
            length <- svmLengthSelection()
            cvrepeats <- if(foresthold$foresttrain=="repeatedcv"){
                foresthold$cvrepeats
            } else if(foresthold$foresttrain!="repeatedcv"){
                1
            }
            list(CalTable=calConditionsTable(cal.type=13, line.type=input$linepreferenceelement, compress=basicCompress(), transformation=basicTransformation(), energy.range=paste0(energyrange[1], "-", energyrange[2]), norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation(),  forestmetric=forestMetricSelection(), foresttrain=forestTrainSelection(), forestnumber=forestNumberSelection(), cvrepeats=cvrepeats, xgbtype=xgboosthold$xgbtype, svmc=paste0(C[1], "-", C[2]),  svmlength=paste0(length[1], "-", length[2])),  StandardsUsed=vals$keeprows)
        })
        svmBoundrangeSpectraModelData <- reactive(label="svmBoundrangeSpectraModelData", {
            rainforestDataGen(spectra=dataNormCal(), compress=svmBoundrangeSpectraParameters()$CalTable$Compress, transformation=svmBoundrangeSpectraParameters()$CalTable$Transformation, dependent.transformation=svmBoundrangeSpectraParameters()$CalTable$DepTrans, energy.range=as.numeric(unlist(strsplit(as.character(svmBoundrangeSpectraParameters()$CalTable$EnergyRange), "-"))), hold.frame=holdFrameCal(), norm.type=svmBoundrangeSpectraParameters()$CalTable$NormType, norm.min=svmBoundrangeSpectraParameters()$CalTable$Min, norm.max=svmBoundrangeSpectraParameters()$CalTable$Max, data.type=dataType())
        })
        svmBoundrangeSpectraModelSet <- reactive(label="svmBoundrangeSpectraModelSet", {
            list(data=predictFrameCheck(svmBoundrangeSpectraModelData()), parameters=svmBoundrangeSpectraParameters())
        })
        svmBoundrangeSpectraModel <- reactive(label="svmBoundrangeSpectraModel", {
            req(input$radiocal, input$calcurveelement)
            data <- svmBoundrangeSpectraModelSet()$data[svmBoundrangeSpectraModelSet()$parameters$StandardsUsed,]
            parameters <- svmBoundrangeSpectraModelSet()$parameters$CalTable
            
            
            svmc.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmC), "-")))
            svmlength.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmLength), "-")))
            
            svm.grid <- expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1),
            length=seq(svmlength.vec[1], svmlength.vec[2], 1))
            
            
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            x_train <- matrix(data[,-1], ncol=1)
            colnames(x_train) <- "data"
            y_train <- data$Concentration
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                svm_model <- caret::train(x_train, y_train, method="svmBoundrangeString", trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                svm_model <- caret::train(x_train, y_train, method="svmBoundrangeString", trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
                stopCluster(cl)
            }
            svm_model
            
        })
        
        svmExponentialSpectraParameters <- reactive(label="svmExponentialSpectraParameters", {
            energyrange <- basicEnergyRange()
            C <- svmCSelection()
            lambda <- xgboostLambdaSelection()
            cvrepeats <- if(foresthold$foresttrain=="repeatedcv"){
                foresthold$cvrepeats
            } else if(foresthold$foresttrain!="repeatedcv"){
                1
            }
            list(CalTable=calConditionsTable(cal.type=13, line.type=input$linepreferenceelement, compress=basicCompress(), transformation=basicTransformation(), energy.range=paste0(energyrange[1], "-", energyrange[2]), norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation(),  forestmetric=forestMetricSelection(), foresttrain=forestTrainSelection(), forestnumber=forestNumberSelection(), cvrepeats=cvrepeats, xgbtype=xgboosthold$xgbtype, svmc=paste0(C[1], "-", C[2]),  xgblambda=paste0(lambda[1], "-", lambda[2])),  StandardsUsed=vals$keeprows)
        })
        svmExponentialSpectraModelData <- reactive(label="svmExponentialSpectraModelData", {
            rainforestDataGen(spectra=dataNormCal(), compress=svmExponentialSpectraParameters()$CalTable$Compress, transformation=svmExponentialSpectraParameters()$CalTable$Transformation, dependent.transformation=svmExponentialSpectraParameters()$CalTable$DepTrans, energy.range=as.numeric(unlist(strsplit(as.character(svmExponentialSpectraParameters()$CalTable$EnergyRange), "-"))), hold.frame=holdFrameCal(), norm.type=svmExponentialSpectraParameters()$CalTable$NormType, norm.min=svmExponentialSpectraParameters()$CalTable$Min, norm.max=svmExponentialSpectraParameters()$CalTable$Max, data.type=dataType())
        })
        svmExponentialSpectraModelSet <- reactive(label="svmExponentialSpectraModelSet", {
            list(data=predictFrameCheck(svmExponentialSpectraModelData()), parameters=svmExponentialIntensityParameters())
        })
        svmExponentialSpectraModel <- reactive(label="svmExponentialSpectraModel", {
            req(input$radiocal, input$calcurveelement)
            data <- svmExponentialSpectraModelSet()$data[svmExponentialSpectraModelSet()$parameters$StandardsUsed,]
            parameters <- svmExponentialSpectraModelSet()$parameters$CalTable
            
            
            svmc.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmC), "-")))
            xgblambda.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbLambda), "-")))
            
            svm.grid <- expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1),
            lambda=seq(xgblambda.vec[1], xgblambda.vec[2], 1))
            
            
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            x_train <- matrix(data[,-1], ncol=1)
            colnames(x_train) <- "data"
            y_train <- data$Concentration
            
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                svm_model <- caret::train(x_train, y_train, method="svmExpoString", type="Regression", trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                svm_model <- caret::train(x_train, y_train, method="svmExpoString", type="Regression", trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
                stopCluster(cl)
            }
            svm_model
            
        })
        
        svmSpectrumSpectraParameters <- reactive(label="svmSpectrumSpectraParameters", {
            energyrange <- basicEnergyRange()
            C <- svmCSelection()
            lambda <- xgboostLambdaSelection()
            cvrepeats <- if(foresthold$foresttrain=="repeatedcv"){
                foresthold$cvrepeats
            } else if(foresthold$foresttrain!="repeatedcv"){
                1
            }
            list(CalTable=calConditionsTable(cal.type=13, line.type=input$linepreferenceelement, compress=basicCompress(), transformation=basicTransformation(), energy.range=paste0(energyrange[1], "-", energyrange[2]), norm.type=basicNormType(), norm.min=basicNormMin(), norm.max=basicNormMax(), dependent.transformation=dependentTransformation(),  forestmetric=forestMetricSelection(), foresttrain=forestTrainSelection(), forestnumber=forestNumberSelection(), cvrepeats=cvrepeats, xgbtype=xgboosthold$xgbtype, svmc=paste0(C[1], "-", C[2]),  xgblambda=paste0(lambda[1], "-", lambda[2])),  StandardsUsed=vals$keeprows)
        })
        svmSpectrumSpectraModelData <- reactive(label="svmSpectrumSpectraModelData", {
            rainforestDataGen(spectra=dataNormCal(), compress=svmSpectrumSpectraParameters()$CalTable$Compress, transformation=svmSpectrumSpectraParameters()$CalTable$Transformation, dependent.transformation=svmSpectrumSpectraParameters()$CalTable$DepTrans, energy.range=as.numeric(unlist(strsplit(as.character(svmSpectrumSpectraParameters()$CalTable$EnergyRange), "-"))), hold.frame=holdFrameCal(), norm.type=svmSpectrumSpectraParameters()$CalTable$NormType, norm.min=svmSpectrumSpectraParameters()$CalTable$Min, norm.max=svmSpectrumSpectraParameters()$CalTable$Max, data.type=dataType())
        })
        svmSpectrumSpectraModelSet <- reactive(label="svmSpectrumIntensityModelSet", {
            list(data=predictFrameCheck(svmSpectrumSpectraModelData()), parameters=svmSpectrumSpectraParameters())
        })
        svmSpectrumSpectraModel <- reactive(label="svmSpectrumSpectraModel", {
            req(input$radiocal, input$calcurveelement)
            data <- svmSpectrumSpectraModelSet()$data[svmSpectrumSpectraModelSet()$parameters$StandardsUsed,]
            parameters <- svmSpectrumSpectraModelSet()$parameters$CalTable
            
            
            svmc.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmC), "-")))
            svmlength.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmLength), "-")))
            
            svm.grid <- expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1),
            length=seq(svmlength.vec[1], svmlength.vec[2], 1))
            
            
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            x_train <- matrix(data[,-1], ncol=1)
            colnames(x_train) <- "data"
            y_train <- data$Concentration
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                svm_model <- caret::train(x_train, y_train, method="svmSpectrumString", trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                svm_model <- caret::train(x_train, y_train, method="svmSpectrumString", trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
                stopCluster(cl)
            }
            svm_model
            
        })
        
        svmSpectraModelSet <- reactive(label="svmSpectraModelSet", {
            if(xgboosthold$xgbtype=="Linear"){
                svmLinearSpectraModelSet()
            } else if(xgboosthold$xgbtype=="Polynomial"){
                svmPolySpectraModelSet()
            } else if(xgboosthold$xgbtype=="Exponential"){
                svmExponentialSpectraModelSet()
            } else if(xgboosthold$xgbtype=="Radial" | xgboosthold$xgbtype=="Radial Cost" | xgboosthold$xgbtype=="Radial Sigma"){
                svmRadialSpectraModelSet()
            } else if(xgboosthold$xgbtype=="Boundrange String"){
                svmBoundrangeSpectraModelSet()
            } else if(xgboosthold$xgbtype=="Spectrum String"){
                svmSpectrumSpectraModelSet()
            }
        })
        
        svmSpectraModel <- reactive(label="svmSpectraModel", {
            
            if(xgboosthold$xgbtype=="Linear"){
                svmLinearSpectraModel()
            } else if(xgboosthold$xgbtype=="Polynomial"){
                svmPolySpectraModel()
            } else if(xgboosthold$xgbtype=="Exponential"){
                svmExponentialSpectraModel()
            } else if(xgboosthold$xgbtype=="Radial" | xgboosthold$xgbtype=="Radial Cost" | xgboosthold$xgbtype=="Radial Sigma"){
                svmRadialSpectraModel()
            } else if(xgboosthold$xgbtype=="Boundrange String"){
                svmBoundrangeSpectraModel()
            } else if(xgboosthold$xgbtype=="Spectrum String"){
                svmSpectrumSpectraModel()
            }
            
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
            } else if(input$radiocal==10){
                TRUE
            } else if(input$radiocal==11){
                TRUE
            } else if(input$radiocal==12){
                TRUE
            } else if(input$radiocal==13){
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
            
            predict.intensity.simp <- linearModelData()[,!colnames(linearModelData()) %in% c("Spectrum", "Concentration")]
            
            predict.intensity.luc <- lucasToothModelData()[,!colnames(lucasToothModelData()) %in% c("Spectrum", "Concentration")]
            
            predict.intensity.forest <- forestModelData()[,!colnames(forestModelData()) %in% c("Spectrum", "Concentration")]
            
            spectra.data <- rainforestModelData()[,!colnames(rainforestModelData()) %in% c("Spectrum", "Concentration")]
            
            lucashold$slope <- outVaralt()
            
            predict.frame.lin <- linearModelSet()$data[vals$keeprows,]
            predict.frame.nonlin <- nonLinearModelSet()$data[vals$keeprows,]
            predict.frame.luc <- lucasToothModelSet()$data[vals$keeprows,]
            predict.frame.forest <- forestModelSet()$data[vals$keeprows,]
            predict.frame.rainforest <- rainforestModelSet()$data[vals$keeprows,]
            predict.frame.neural.shallow.intens <- neuralNetworkIntensityShallowModelSet()$data[vals$keeprows,]
            predict.frame.neural.shallow.spectra <- neuralNetworkSpectraShallowModelSet()$data[vals$keeprows,]
            predict.frame.xgboost.intens <- xgboostIntensityModelSet()$data[vals$keeprows,]
            predict.frame.xgboost.spectra <- xgboostSpectraModelSet()$data[vals$keeprows,]
            predict.frame.bayes.intens <- bayesIntensityModelSet()$data[vals$keeprows,]
            predict.frame.bayes.spectra <- bayesSpectraModelSet()$data[vals$keeprows,]
            predict.frame.svm.intens <- svmIntensityModelSet()$data[vals$keeprows,]
            predict.frame.svm.spectra <- svmSpectraModelSet()$data[vals$keeprows,]
            
            cal.lm.simp <- linearModel()
            lm.predict <- tryCatch(predict(cal.lm.simp, newdata=predict.frame.lin), error=function(e) rep(0, length(predict.frame.lin$Concentration)))
            lm.sum <- tryCatch(summary(lm(predict.frame.lin$Concentration~lm.predict, na.action=na.exclude)), error=function(e) NULL)
            lm.r2 <- tryCatch(as.numeric(lm.sum$adj.r.squared), error=function(e) 0)
            lm.r2[!is.finite(lm.r2)] <- 0
            lm.slope <- tryCatch(as.numeric(lm.sum$coef[2]), error=function(e) 0)
            lm.slope[!is.finite(lm.slope)] <- 0

            cal.lm.two <- tryCatch(nonLinearModel(), error=function(e) NULL)
            lm2.predict <- tryCatch(predict(cal.lm.two, newdata=predict.frame.nonlin), error=function(e) rep(0, length(predict.frame.nonlin$Concentration)))
            lm2.sum <- tryCatch(summary(lm(predict.frame.lin$Concentration~lm2.predict, na.action=na.exclude)), error=function(e) NULL)
            lm2.r2 <- tryCatch(as.numeric(lm2.sum$adj.r.squared), error=function(e) 0)
            lm2.r2[!is.finite(lm2.r2)] <- 0
            lm2.slope <- tryCatch(as.numeric(lm2.sum$coef[2]), error=function(e) 0)
            lm2.slope[!is.finite(lm2.slope)] <- 0
            
            cal.lm.luc <- lucasToothModel()
            lucas.predict <- tryCatch(predict(cal.lm.luc, newdata=predict.frame.luc), error=function(e) rep(0, length(predict.frame.luc$Concentration)))
            lucas.sum <- tryCatch(summary(lm(predict.frame.luc$Concentration~lucas.predict, na.action=na.exclude)), error=function(e) NULL)
            lucas.r2 <- tryCatch(as.numeric(lucas.sum$adj.r.squared), error=function(e) 0)
            lucas.r2[!is.finite(lucas.r2)] <- 0
            lucas.slope <- tryCatch(as.numeric(lucas.sum$coef[2]), error=function(e) 0)
            lucas.slope[!is.finite(lucas.slope)] <- 0
            
            cal.lm.forest <- forestModel()
            forest.predict <- tryCatch(predict(cal.lm.forest, newdata=predict.frame.forest), error=function(e) rep(0, length(predict.frame.forest$Concentration)))
            forest.sum <- tryCatch(summary(lm(predict.frame.forest$Concentration~forest.predict, na.action=na.exclude)), error=function(e) NULL)
            forest.r2 <- tryCatch(as.numeric(max(cal.lm.forest[["results"]]$Rsquared)), error=function(e) 0)
            forest.r2[!is.finite(forest.r2)] <- 0
            forest.slope <- tryCatch(as.numeric(forest.sum$coef[2]), error=function(e) 0)
            forest.slope[!is.finite(forest.slope)] <- 0
            
            cal.lm.rainforest <- rainforestModel()
            rainforest.predict <- tryCatch(predict(cal.lm.rainforest, newdata=predict.frame.rainforest), error=function(e) rep(0, length(predict.frame.rainforest$Concentration)))
            rainforest.sum <- tryCatch(summary(lm(predict.frame.rainforest$Concentration~rainforest.predict)), error=function(e) NULL)
            rainforest.r2 <- tryCatch(as.numeric(max(cal.lm.rainforest[["results"]]$Rsquared)), error=function(e) 0)
            rainforest.r2[!is.finite(rainforest.r2)] <- 0
            rainforest.slope <- tryCatch(as.numeric(rainforest.sum$coef[2]), error=function(e) 0)
            rainforest.slope[!is.finite(rainforest.slope)] <- 0
            
            cal.lm.neural.shallow.intens <- neuralNetworkIntensityShallow()
            neural.shallow.intens.predict <- tryCatch(predict(cal.lm.neural.shallow.intens, newdata=predict.frame.neural.shallow.intens), error=function(e) rep(0, length(predict.frame.neural.shallow.intens$Concentration)))
            neural.shallow.intens.sum <- tryCatch(summary(lm(predict.frame.neural.shallow.intens$Concentration~neural.shallow.intens.predict, na.action=na.exclude)), error=function(e) NULL)
            neural.shallow.intens.r2 <- tryCatch(as.numeric(max(cal.lm.neural.shallow.intens[["results"]]$Rsquared)), error=function(e) 0)
            neural.shallow.intens.r2[!is.finite(neural.shallow.intens.r2)] <- 0
            neural.shallow.intens.slope <- tryCatch(as.numeric(neural.shallow.intens.sum$coef[2]), error=function(e) 0)
            neural.shallow.intens.slope[!is.finite(neural.shallow.intens.slope)] <- 0
            
            cal.lm.neural.shallow.spectra <- neuralNetworkSpectraShallow()
            neural.shallow.spectra.predict <- tryCatch(predict(cal.lm.neural.shallow.spectra, newdata=predict.frame.neural.shallow.spectra), error=function(e) rep(0, length(predict.frame.neural.shallow.spectra$Concentration)))
            neural.shallow.spectra.sum <- tryCatch(summary(lm(predict.frame.neural.shallow.spectra$Concentration~neural.shallow.spectra.predict, na.action=na.exclude)), error=function(e) NULL)
            neural.shallow.spectra.r2 <- tryCatch(as.numeric(max(cal.lm.neural.shallow.spectra[["results"]]$Rsquared)), error=function(e) 0)
            neural.shallow.spectra.r2[!is.finite(neural.shallow.spectra.r2)] <- 0
            neural.shallow.spectra.slope <- tryCatch(as.numeric(neural.shallow.spectra.sum$coef[2]), error=function(e) 0)
            neural.shallow.spectra.slope[!is.finite(neural.shallow.spectra.slope)] <- 0
            
            cal.lm.xgboost.intens <- xgboostIntensityModel()
            xgboost.intens.predict <- tryCatch(predict(cal.lm.xgboost.intens, newdata=predict.frame.xgboost.intens), error=function(e) rep(0, length(predict.frame.xgboost.intens$Concentration)))
            xgboost.intens.sum <- tryCatch(summary(lm(predict.frame.xgboost.intens$Concentration~xgboost.intens.predict, na.action=na.exclude)), error=function(e) NULL)
            xgboost.intens.r2 <- tryCatch(as.numeric(max(cal.lm.xgboost.intens[["results"]]$Rsquared)), error=function(e) 0)
            xgboost.intens.r2[!is.finite(xgboost.intens.r2)] <- 0
            xgboost.intens.slope <- tryCatch(as.numeric(xgboost.intens.sum$coef[2]), error=function(e) 0)
            xgboost.intens.slope[!is.finite(xgboost.intens.slope)] <- 0
            
            cal.lm.xgboost.spectra <- xgboostSpectraModel()
            xgboost.spectra.predict <- tryCatch(predict(cal.lm.xgboost.spectra, newdata=predict.frame.xgboost.spectra), error=function(e) rep(0, length(predict.frame.xgboost.spectra$Concentration)))
            xgboost.spectra.sum <- tryCatch(summary(lm(predict.frame.xgboost.spectra$Concentration~xgboost.spectra.predict, na.action=na.exclude)), error=function(e) NULL)
            xgboost.spectra.r2 <- tryCatch(as.numeric(max(cal.lm.xgboost.spectra[["results"]]$Rsquared)), error=function(e) 0)
            xgboost.spectra.r2[!is.finite(xgboost.spectra.r2)] <- 0
            xgboost.spectra.slope <- tryCatch(as.numeric(xgboost.spectra.sum$coef[2]), error=function(e) 0)
            xgboost.spectra.slope[!is.finite(xgboost.spectra.slope)] <- 0
            
            cal.lm.bayes.intens <- bayesIntensityModel()
            bayes.intens.predict <- tryCatch(predict(cal.lm.bayes.intens, newdata=predict.frame.bayes.intens), error=function(e) rep(0, length(predict.frame.bayes.intens$Concentration)))
            bayes.intens.sum <- tryCatch(summary(lm(predict.frame.bayes.intens$Concentration~bayes.intens.predict, na.action=na.exclude)), error=function(e) NULL)
            bayes.intens.r2 <- tryCatch(as.numeric(max(cal.lm.bayes.intens[["results"]]$Rsquared)), error=function(e) 0)
            bayes.intens.r2[!is.finite(bayes.intens.r2)] <- 0
            bayes.intens.slope <- tryCatch(as.numeric(bayes.intens.sum$coef[2]), error=function(e) 0)
            bayes.intens.slope[!is.finite(bayes.intens.slope)] <- 0
            
            cal.lm.bayes.spectra <- bayesSpectraModel()
            bayes.spectra.predict <- tryCatch(predict(cal.lm.bayes.spectra, newdata=predict.frame.bayes.spectra), error=function(e) rep(0, length(predict.frame.bayes.spectra$Concentration)))
            bayes.spectra.sum <- tryCatch(summary(lm(predict.frame.bayes.spectra$Concentration~bayes.spectra.predict, na.action=na.exclude)), error=function(e) NULL)
            bayes.spectra.r2 <- tryCatch(as.numeric(max(cal.lm.bayes.spectra[["results"]]$Rsquared)), error=function(e) 0)
            bayes.spectra.r2[!is.finite(bayes.spectra.r2)] <- 0
            bayes.spectra.slope <- tryCatch(as.numeric(bayes.spectra.sum$coef[2]), error=function(e) 0)
            bayes.spectra.slope[!is.finite(bayes.spectra.slope)] <- 0
            
            cal.lm.svm.intens <- svmIntensityModel()
            svm.intens.predict <- tryCatch(predict(cal.lm.svm.intens, newdata=predict.frame.svm.intens), error=function(e) rep(0, length(predict.frame.svm.intens$Concentration)))
            svm.intens.sum <- tryCatch(summary(lm(predict.frame.svm.intens$Concentration~svm.intens.predict, na.action=na.exclude)), error=function(e) NULL)
            svm.intens.r2 <- tryCatch(as.numeric(max(cal.lm.svm.intens[["results"]]$Rsquared)), error=function(e) 0)
            svm.intens.r2[!is.finite(svm.intens.r2)] <- 0
            svm.intens.slope <- tryCatch(as.numeric(svm.intens.sum$coef[2]), error=function(e) 0)
            svm.intens.slope[!is.finite(svm.intens.slope)] <- 0

            cal.lm.svm.spectra <- svmSpectraModel()
            svm.spectra.predict <- tryCatch(predict(cal.lm.svm.spectra, newdata=predict.frame.svm.spectra), error=function(e) rep(0, length(predict.frame.svm.spectra$Concentration)))
            svm.spectra.sum <- tryCatch(summary(lm(predict.frame.bayes.spectra$Concentration~svm.spectra.predict, na.action=na.exclude)), error=function(e) NULL)
            svm.spectra.r2 <- tryCatch(as.numeric(max(cal.lm.svm.spectra[["results"]]$Rsquared)), error=function(e) 0)
            svm.spectra.r2[!is.finite(svm.spectra.r2)] <- 0
            svm.spectra.slope <- tryCatch(as.numeric(svm.spectra.sum$coef[2]), error=function(e) 0)
            svm.spectra.slope[!is.finite(svm.spectra.slope)] <- 0


            
            model.frame <- data.frame(Model = c("Linear", "Non-Linear", "Lucas-Tooth", "Forest", "Rainforest", "Neural Intensities Shallow", "Neural Spectra Shallow", "XGBoost Intensities", "XGBoost Spectra", "Bayes Intensities", "Bayes Spectra", "SVM Intensities", "SVM Spectra"),
            valSlope = round(c(lm.slope, lm2.slope, lucas.slope, forest.slope, rainforest.slope, neural.shallow.intens.slope, neural.shallow.spectra.slope, xgboost.intens.slope, xgboost.spectra.slope, bayes.intens.slope, bayes.spectra.slope, svm.intens.slope, svm.spectra.slope), 2),
            R2 = round(c(lm.r2, lm2.r2, lucas.r2, forest.r2, rainforest.r2, neural.shallow.intens.r2, neural.shallow.spectra.r2, xgboost.intens.r2, xgboost.spectra.r2, bayes.intens.r2, bayes.spectra.r2, svm.intens.r2, svm.spectra.r2), 2),
            Score = round(c(lm.r2*lm.slope, lm2.r2*lm2.slope, lucas.r2*lucas.slope, forest.r2*forest.slope, rainforest.r2*rainforest.slope, neural.shallow.intens.r2*neural.shallow.intens.slope, neural.shallow.spectra.r2*neural.shallow.spectra.slope, xgboost.intens.r2*xgboost.intens.slope, xgboost.spectra.r2*xgboost.spectra.slope, bayes.intens.r2*bayes.intens.slope, bayes.spectra.r2*bayes.spectra.slope, svm.intens.r2*svm.intens.slope, svm.spectra.r2*svm.spectra.slope), 2),
            Rank = round(abs(1-c(lm.r2*lm.slope, lm2.r2*lm2.slope, lucas.r2*lucas.slope, forest.r2*forest.slope, rainforest.r2*rainforest.slope, neural.shallow.intens.r2*neural.shallow.intens.slope, neural.shallow.spectra.r2*neural.shallow.spectra.slope, xgboost.intens.r2*xgboost.intens.slope, xgboost.spectra.r2*xgboost.spectra.slope, bayes.intens.r2*bayes.intens.slope, bayes.spectra.r2*bayes.spectra.slope, svm.intens.r2*svm.intens.slope, svm.spectra.r2*svm.spectra.slope)), 2),
            Code=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
            stringsAsFactors=FALSE)
            
            for(i in 1:9){
                if(model.frame$valSlope[i] < 0.9 | model.frame$valSlope[i] > 1.1){
                    model.frame$Score[i] <- model.frame$Score[i]*0
                    model.frame$Rank[i] <- model.frame$Rank[i]*10
                }
            }
            
            model.frame[complete.cases(model.frame), ] %>%  arrange(Rank)
            
            #model.frame[order(model.frame, model.frame$Rank),]
            #model.frame
            
        })
        
        bestCalHold <- reactiveValues()
        
        
        
        bestCalType <- reactive({

            model.frame <- bestCalHold[[input$calcurveelement]]
            
            #model.frame <- subset(model.frame, !(model.frame$valSlope < 0.9 | model.frame$valSlope > 1.1))
            if(sum(model.frame$Score)==0){
                3
            } else if(sum(model.frame$Score)>0){
                model.frame[Closest(model.frame$Score, 1, which=TRUE),6]
            }
            
        })
        
        bestParameters <- reactive({
            
            if(bestCalType()==1){
                linearParameters()
            } else if(bestCalType()==2){
                nonlinearParameters()
            } else if(bestCalType()==3){
                lucasToothParameters()
            } else if(bestCalType()==4){
                forestParameters()
            } else if(bestCalType()==5){
                rainforestParameters()
            } else if(bestCalType()==6){
                neuralNetworkIntensityShallowParameters()
            } else if(bestCalType()==7){
                neuralNetworkSpectraShallowParameters()
            } else if(bestCalType()==8){
                xgboostIntensityParameters()
            } else if(bestCalType()==9){
                xgboostSpectraParameters()
            } else if(bestCalType()==10){
                neuralNetworkIntensityShallowParameters()
            } else if(bestCalType()==11){
                neuralNetworkSpectraShallowParameters()
            } else if(bestCalType()==12){
                xgboostIntensityParameters()
            } else if(bestCalType()==13){
                xgboostSpectraParameters()
            }
            
        })
        
        bestModel <- reactive({
            
            if(bestCalType()==1){
                linearModel()
            } else if(bestCalType()==2){
                nonlinearModel()
            } else if(bestCalType()==3){
                lucasToothModel()
            } else if(bestCalType()==4){
                forestModel()
            } else if(bestCalType()==5){
                rainforestModel()
            } else if(bestCalType()==6){
                neuralNetworkIntensityShallowModel()
            } else if(bestCalType()==7){
                neuralNetworkSpectraShallowModel()
            } else if(bestCalType()==8){
                xgboostIntensityModel()
            } else if(bestCalType()==9){
                xgboostSpectraModel()
            } else if(bestCalType()==10){
                bayesIntensityShallowModel()
            } else if(bestCalType()==11){
                bayesSpectraShallowModel()
            } else if(bestCalType()==12){
                svmIntensityModel()
            } else if(bestCalType()==13){
                svmSpectraModel()
            }
            
        })
        
        observeEvent(input$trainslopes, priority=95, {
            foresthold$foresttrain <- "cv"
            xgboosthold$xgbtype <- "Linear"
            calMemory$Calibration$calList[[input$calcurveelement]] <- list(Parameters=defaultCalConditions(element=input$calcurveelement, number.of.standards=length(holdFrame()$Spectrum)), Model=NULL)
            
            bestCalHold[[input$calcurveelement]] <- bestCalTypeFrame()
            calMemory$Calibration$calList[[input$calcurveelement]] <- isolate(modelPack(parameters=bestParameters(), model=bestModel(), compress=TRUE))
        })
        
        output$models <- renderDataTable({
            
            bestCalHold[[input$calcurveelement]]
            
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
        
        

        
        
        calTransformationPre <- reactive(label="calTransformationPre", {
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
        
        calDependentTransformationPre <- reactive(label="calDependentTransformationPre", {
            if(!"DepTrans" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calConditions$hold[["CalTable"]][["DepTrans"]]
            } else if("DepTrans" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calSettings$calList[[input$calcurveelement]][[1]]$CalTable$DepTrans[1]
            }
        })
        
        output$dependenttransformationui <- renderUI({
            req(input$radiocal)
            dependentTransformationUI(radiocal=input$radiocal, selection=dependentTransformation())
        })
        
        calEnergyRangePre <- reactive(label="calEnergyRangePre", {
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
        
        calCompressPre <- reactive(label="calCompressPre", {
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
        
        maxSample <- reactive(label="maxSample", {
            req(input$radiocal)
            if(input$radiocal==4){
                30
            } else if(input$radiocal==5){
                300
            }else if(input$radiocal==6){
                30
            } else if(input$radiocal==7){
                300
            } else if(input$radiocal==8){
                30
            } else if(input$radiocal==9){
                300
            }
            
        })
        
        
        defaultSample <- reactive({
            
            floor(sqrt(ncol(predictIntensity())))
            
        })
        
        calForestTrySelectionpre <- reactive(label="calForestTrySelectionpre", {
            if(!"ForestTry" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calConditions$hold[["CalTable"]]["ForestTry"]
            } else if("ForestTry" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calSettings$calList[[input$calcurveelement]][[1]]$CalTable$ForestTry[1]
            }
        })
        
        
        calForestMetricSelectionpre <- reactive(label="calForestMetricSelectionpre", {
            if(!"ForestMetric" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calConditions$hold[["CalTable"]]["ForestMetric"]
            } else if("ForestMetric" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calSettings$calList[[input$calcurveelement]][[1]]$CalTable$ForestMetric[1]
            }
        })
        
        calForestTCSelectionpre <- reactive(label="calForestTCSelectionpre", {
            if(!"ForestTC" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calConditions$hold[["CalTable"]]["ForestTC"]
            } else if("ForestTC" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calSettings$calList[[input$calcurveelement]][[1]]$CalTable$ForestTC[1]
            }
        })
        
        
        calForestNumberSelectionpre <- reactive(label="calForestNumberSelectionpre", {
            if(!"ForestNumber" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calConditions$hold[["CalTable"]]["ForestNumber"]
            } else if("ForestNumber" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calSettings$calList[[input$calcurveelement]][[1]]$CalTable$ForestNumber[1]
            }
        })
        
        calCVRepeatsSelectionpre <- reactive(label="calCVRepeatsSelectionpre", {
            if(!"CVRepeats" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calConditions$hold[["CalTable"]]["CVRepeats"]
            } else if("CVRepeats" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calSettings$calList[[input$calcurveelement]][[1]]$CalTable$CVRepeats[1]
            }
        })
        
        
        calForestTreeSelectionpre <- reactive(label="calForestTreeSelectionpre", {
            if(!"ForestTrees" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calConditions$hold[["CalTable"]]["ForestTrees"]
            } else if("ForestNumber" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calSettings$calList[[input$calcurveelement]][[1]]$CalTable$ForestTrees[1]
            }
        })
        
        calHiddenLayersSelectionpre <- reactive(label="calHiddenLayersSelectionpre", {
            if(!"NeuralHL" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calConditions$hold[["CalTable"]]["NeuralHL"]
            } else if("NeuralHL" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calSettings$calList[[input$calcurveelement]][[1]]$CalTable$NeuralHL[1]
            }
        })
        
        calHiddenUnitsSelectionpre <- reactive(label="calHiddenUnitsSelectionpre", {
            if(!"NeuralHU" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calConditions$hold[["CalTable"]]["NeuralHU"]), "-")))
            } else if("NeuralHU" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calSettings$calList[[input$calcurveelement]][[1]]$CalTable$NeuralHU[1]), "-")))
            }
        })
        
        calWeightDecaySelectionpre <- reactive(label="calWeightDecaySelectionpre", {
            if(!"NeuralWD" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calConditions$hold[["CalTable"]]["NeuralWD"]), "-")))
            } else if("NeuralWD" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calSettings$calList[[input$calcurveelement]][[1]]$CalTable$NeuralWD[1]), "-")))
            }
        })
        
        calMaxIterationsSelectionpre <- reactive(label="calMaxIterationsSelectionpre", {
            if(!"NeuralMI" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calConditions$hold[["CalTable"]]["NeuralMI"]
            } else if("NeuralMI" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calSettings$calList[[input$calcurveelement]][[1]]$CalTable$NeuralMI[1]
            }
        })
        
        calXGBTypeSelectionpre <- reactive(label="calXGBTypeSelectionpre", {
            if(!"xgbType" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.character(calConditions$hold[["CalTable"]]["xgbType"])
            } else if("xgbType" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                
                as.character(calSettings$calList[[input$calcurveelement]][[1]]$CalTable$xgbType[1])
            }
        })
        
        calTreeDepthSelectionpre <- reactive(label="calTreeDepthSelectionpre", {
            if(!"TreeDepth" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calConditions$hold[["CalTable"]]["TreeDepth"]), "-")))
            } else if("TreeDepth" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calSettings$calList[[input$calcurveelement]][[1]]$CalTable$TreeDepth[1]), "-")))
            }
        })
        
        calXGBAlphaSelectionpre <- reactive(label="calXGBAlphaSelectionpre", {
            if(!"xgbAlpha" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calConditions$hold[["CalTable"]]["xgbAlpha"]), "-")))
            } else if("xgbAlpha" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calSettings$calList[[input$calcurveelement]][[1]]$CalTable$xgbAlpha[1]), "-")))
            }
        })
        
        calXGBGammaSelectionpre <- reactive(label="calXGBGammaSelectionpre", {
            if(!"xgbGamma" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calConditions$hold[["CalTable"]]["xgbGamma"]), "-")))
            } else if("xgbGamma" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calSettings$calList[[input$calcurveelement]][[1]]$CalTable$xgbGamma[1]), "-")))
            }
        })
        
        calXGBEtaSelectionpre <- reactive(label="calXGBEtaSelectionpre", {
            if(!"xgbEta" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calConditions$hold[["CalTable"]]["xgbEta"]), "-")))
            } else if("xgbEta" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calSettings$calList[[input$calcurveelement]][[1]]$CalTable$xgbEta[1]), "-")))
            }
        })
        
        calxgboostLambdaSelectionpre <- reactive(label="calxgboostLambdaSelectionpre", {
            if(!"xgbLambda" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calConditions$hold[["CalTable"]]["xgbLambda"]), "-")))
            } else if("xgbLambda" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calSettings$calList[[input$calcurveelement]][[1]]$CalTable$xgbLambda[1]), "-")))
            }
        })
        
        calXGBSubSampleSelectionpre <- reactive(label="calXGBSubSampleSelectionpre", {
            if(!"xgbSubSample" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calConditions$hold[["CalTable"]]["xgbSubSample"]), "-")))
            } else if("xgbSubSample" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calSettings$calList[[input$calcurveelement]][[1]]$CalTable$xgbSubSample[1]), "-")))
            }
        })
        
        calXGBColSampleSelectionpre <- reactive(label="calXGBColSampleSelectionpre", {
            if(!"xgbColSample" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calConditions$hold[["CalTable"]]["xgbColSample"]), "-")))
            } else if("xgbColSample" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calSettings$calList[[input$calcurveelement]][[1]]$CalTable$xgbColSample[1]), "-")))
            }
        })
        
        calXGBMinChildSelectionpre <- reactive(label="calXGBMinChildSelectionpre", {
            if(!"xgbMinChild" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calConditions$hold[["CalTable"]]["xgbMinChild"]
            } else if("xgbMinChild" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                calSettings$calList[[input$calcurveelement]][[1]]$CalTable$xgbMinChild[1]
            }
        })
        
        calBARTKSelectionpre <- reactive(label="calBARTKSelectionpre", {
            if(!"bartK" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calConditions$hold[["CalTable"]]["bartK"]), "-")))
            } else if("bartK" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calSettings$calList[[input$calcurveelement]][[1]]$CalTable$bartK[1]), "-")))
            }
        })
        
        calBARTBetaSelectionpre <- reactive(label="calBARTBetaSelectionpre", {
            if(!"bartBeta" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calConditions$hold[["CalTable"]]["bartBeta"]), "-")))
            } else if("bartBeta" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calSettings$calList[[input$calcurveelement]][[1]]$CalTable$bartBeta[1]), "-")))
            }
        })
        
        calBARTNuSelectionpre <- reactive(label="calBARTNuSelectionpre", {
            if(!"bartNu" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calConditions$hold[["CalTable"]]["bartNu"]), "-")))
            } else if("bartNu" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calSettings$calList[[input$calcurveelement]][[1]]$CalTable$bartNu[1]), "-")))
            }
        })
        
        calSVMCSelectionpre <- reactive(label="calSVMCSelectionpre", {
            if(!"svmC" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calConditions$hold[["CalTable"]]["svmC"]), "-")))
            } else if("svmC" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calSettings$calList[[input$calcurveelement]][[1]]$CalTable$svmC[1]), "-")))
            }
        })
        
        calSVMDegreeSelectionpre <- reactive(label="calSVMDegreeSelectionpre", {
            if(!"svmDegree" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calConditions$hold[["CalTable"]]["svmDegree"]), "-")))
            } else if("svmDegree" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calSettings$calList[[input$calcurveelement]][[1]]$CalTable$svmDegree[1]), "-")))
            }
        })
        
        calSVMScaleSelectionpre <- reactive(label="calSVMScaleSelectionpre", {
            if(!"svmScale" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calConditions$hold[["CalTable"]]["svmScale"]), "-")))
            } else if("svmScale" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calSettings$calList[[input$calcurveelement]][[1]]$CalTable$svmScale[1]), "-")))
            }
        })
        
        calSVMSigmaSelectionpre <- reactive(label="calSVMSigmaSelectionpre", {
            if(!"svmSigma" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calConditions$hold[["CalTable"]]["svmSigma"]), "-")))
            } else if("svmSigma" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calSettings$calList[[input$calcurveelement]][[1]]$CalTable$svmSigma[1]), "-")))
            }
        })
        
        calSVMLengthSelectionpre <- reactive(label="calSVMLengthSelectionpre", {
            if(!"svmLength" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calConditions$hold[["CalTable"]]["svmLength"]), "-")))
            } else if("svmLength" %in% colnames(calSettings$calList[[input$calcurveelement]][[1]]$CalTable)){
                as.numeric(unlist(strsplit(as.character(calSettings$calList[[input$calcurveelement]][[1]]$CalTable$svmLength[1]), "-")))
            }
        })
        
        basichold <- reactiveValues()
        foresthold <- reactiveValues()
        lucashold <- reactiveValues()
        neuralhold <- reactiveValues()
        xgboosthold <- reactiveValues()
        barthold <- reactiveValues()
        svmhold <- reactiveValues()
        
        observeEvent(input$calcurveelement, {
            basichold$normtype <- calNormSelectionpre()
            basichold$normmin <- normMinPre()
            basichold$normmax <- normMaxPre()
            basichold$compress <- calCompressPre()
            basichold$transformation <- calTransformationPre()
            basichold$deptransformation <- calDependentTransformationPre()
            basichold$energyrange <- calEnergyRangePre()
            lucashold$intercept <- calInterceptSelectionPre()
            lucashold$slope <- calSlopeSelectionPre()
            foresthold$foresttry <- calForestTrySelectionpre()
            foresthold$forestmetric <- calForestMetricSelectionpre()
            foresthold$foresttrain <- calForestTCSelectionpre()
            foresthold$forestnumber <- calForestNumberSelectionpre()
            foresthold$cvrepeats <- calCVRepeatsSelectionpre()
            foresthold$foresttrees <- calForestTreeSelectionpre()
            neuralhold$neuralhiddenlayers <- calHiddenLayersSelectionpre()
            neuralhold$neuralhiddenunits <- calHiddenUnitsSelectionpre()
            neuralhold$neuralweightdecay <- calWeightDecaySelectionpre()
            neuralhold$neuralmaxiterations <- calMaxIterationsSelectionpre()
            xgboosthold$xgbtype <- calXGBTypeSelectionpre()
            xgboosthold$treedepth <- calTreeDepthSelectionpre()
            xgboosthold$xgbalpha <- calXGBAlphaSelectionpre()
            xgboosthold$xgbgamma <- calXGBGammaSelectionpre()
            xgboosthold$xgbeta <- calXGBEtaSelectionpre()
            xgboosthold$xgblambda <- calxgboostLambdaSelectionpre()
            xgboosthold$xgbsubsample <- calXGBSubSampleSelectionpre()
            xgboosthold$xgbcolsample <- calXGBColSampleSelectionpre()
            xgboosthold$xgbminchild <- calXGBMinChildSelectionpre()
            barthold$bartk <- calBARTKSelectionpre()
            barthold$bartbeta <- calBARTBetaSelectionpre()
            barthold$bartnu <- calBARTNuSelectionpre()
            svmhold$svmc <- calSVMCSelectionpre()
            svmhold$svmdegree <- calSVMDegreeSelectionpre()
            svmhold$svmscale <- calSVMScaleSelectionpre()
            svmhold$svmsigma <- calSVMSigmaSelectionpre()
            svmhold$svmlength <- calSVMLengthSelectionpre()
        })
        
        


        
        #observeEvent(input$trainslopes, {
            
            #    isolate(calhold$caltype <- bestCalType())
            
            #})
            
            
            
        basicNormType <- reactive(label="basicNormType", {
            basichold$normtype
        })
        
        basicNormMin <- reactive(label="basicNormMin", {
            basichold$normmin
        })
        
        basicNormMax <- reactive(label="basicNormMax", {
            basichold$normmax
        })
        
        basicCompress <- reactive(label="basicCompress", {
            basichold$compress
        })
        
        basicTransformation <- reactive(label="basicTransformation", {
            basichold$transformation
        })
        
        dependentTransformation <- reactive(label="dependentTransformation", {
            basichold$deptransformation
        })
        
        basicEnergyRange <- reactive(label="basicEnergyRange", {
            basichold$energyrange
        })
        
        lucasSlope <- reactive(label="lucasSlope", {
            lucashold$slope
        })
        
        lucasIntercept <- reactive(label="lucasIntercept", {
            lucashold$intercept
        })
        
        forestTrySelection <- reactive(label="forestTrySelection", {
            foresthold$foresttry
        })
        
        forestMetricSelection <- reactive(label="forestMetricSelection", {
            foresthold$forestmetric
        })
        
        forestTrainSelection <- reactive(label="forestTrainSelection", {
            foresthold$foresttrain
        })
        
        forestNumberSelection <- reactive(label="forestNumberSelection", {
            foresthold$forestnumber
        })
        
        cvRepeatsSelection <- reactive({
            foresthold$cvrepeats
        })
        
        forestTreeSelection <- reactive(label="forestTreeSelection", {
            foresthold$foresttrees
        })
        
        neuralHiddenLayersSelection <- reactive(label="neuralHiddenLayersSelection", {
            neuralhold$neuralhiddenlayers
        })
        
        neuralHiddenUnitsSelection <- reactive(label="neuralHiddenUnitsSelection", {
            neuralhold$neuralhiddenunits
        })
        
        neuralWeightDecaySelection <- reactive(label="neuralWeightDecaySelection", {
            neuralhold$neuralweightdecay
        })
        
        neuralMaxIterationsSelection <- reactive(label="neuralMaxIterationsSelection", {
            neuralhold$neuralmaxiterations
        })
        
        xgboostTypeSelection <- reactive(label="xgboostTypeSelection", {
            xgboosthold$xgbtype
        })
        
        xgboostTreeDepthSelection <- reactive(label="xgboostTreeDepthSelection", {
            xgboosthold$treedepth
        })
        
        xgboostAlphaSelection <- reactive(label="xgboostAlphaSelection", {
            xgboosthold$xgbalpha
        })
        
        xgboostGammaSelection <- reactive(label="xgboostGammaSelection", {
            xgboosthold$xgbgamma
        })
        
        xgboostEtaSelection <- reactive(label="xgboostEtaSelection", {
            xgboosthold$xgbeta
        })
        
        xgboostLambdaSelection <- reactive(label="xgboostLambdaSelection", {
            xgboosthold$xgblambda
        })
        
        xgboostSubSampleSelection <- reactive(label="xgboostSubSampleSelection", {
            xgboosthold$xgbsubsample
        })
        
        xgboostColSampleSelection <- reactive(label="xgboostColSampleSelection", {
            xgboosthold$xgbcolsample
        })
        
        xgboostMinChildSelection <- reactive(label="xgboostMinChildSelection", {
            xgboosthold$xgbminchild
        })
        
        bartKSelection <- reactive(label="bartKSelection", {
            barthold$bartk
        })
        
        bartBetaSelection <- reactive(label="bartBetaSelection", {
            barthold$bartbeta
        })
        
        bartNuSelection <- reactive(label="bartNuSelection", {
            barthold$bartnu
        })
        
        svmCSelection <- reactive(label="svmCSelection", {
            svmhold$svmc
        })
        
        svmDegreeSelection <- reactive(label="svmDegreeSelection", {
            svmhold$svmdegree
        })
        
        svmScaleSelection <- reactive(label="svmScaleSelection", {
            svmhold$svmscale
        })
        
        svmSigmaSelection <- reactive(label="svmSigmaSelection", {
            svmhold$svmsigma
        })
        
        svmLengthSelection <- reactive(label="svmLengthSelection", {
            svmhold$svmlength
        })
        
        observeEvent(input$normcal, {
            basichold$normtype <- input$normcal
        })
        
        observeEvent(input$comptonmin, {
            basichold$normmin <- input$comptonmin
        })
        
        observeEvent(input$comptonmax, {
            basichold$normmax <- input$comptonmax
        })
        
        observeEvent(input$compress, {
             basichold$compress <- input$compress
        })
        
        observeEvent(input$transformation, {
            basichold$transformation <- input$transformation
        })
        
        observeEvent(input$deptransformation, {
            basichold$deptransformation <- input$deptransformation
        })
        
        observeEvent(input$energyrange, {
            basichold$energyrange <- input$energyrange
        })
        
        observeEvent(input$slope_vars, {
            lucashold$slope <- input$slope_vars
        })
        
        observeEvent(input$addallslopes==TRUE, {
            lucashold$slope <- outVaralt()
        })
        
        observeEvent(input$removeallslopes==TRUE, {
            lucashold$slope <- input$calcurveelement
        })
        
        observeEvent(input$intercept_vars, {
            lucashold$intercept <- input$intercept_vars
        })
        
        observeEvent(input$foresttry, {
            foresthold$foresttry <- input$foresttry
        })
        
        observeEvent(input$forestmetric, {
            foresthold$forestmetric <- input$forestmetric
        })
        
        observeEvent(input$foresttrain, {
            foresthold$foresttrain <- input$foresttrain
        })
        
        observeEvent(input$forestnumber, {
            foresthold$forestnumber <- input$forestnumber
        })
        
        observeEvent(input$cvrepeats, {
            foresthold$cvrepeats <- input$cvrepeats
        })
        
        observeEvent(input$foresttrees, {
            foresthold$foresttrees <- input$foresttrees
        })
        
        observeEvent(input$neuralhiddenlayers, {
            neuralhold$neuralhiddenlayers <- input$neuralhiddenlayers
        })
        
        observeEvent(input$neuralhiddenunits, {
            neuralhold$neuralhiddenunits <- input$neuralhiddenunits
        })
        
        observeEvent(input$neuralweightdecay, {
            neuralhold$neuralweightdecay <- input$neuralweightdecay
        })
        
        observeEvent(input$neuralmaxiterations, {
            neuralhold$neuralmaxiterations <- input$neuralmaxiterations
        })
        
        observeEvent(input$xgbtype, {
            xgboosthold$xgbtype <- input$xgbtype
        })
        
        observeEvent(input$treedepth, {
            xgboosthold$treedepth <- input$treedepth
        })
        
        observeEvent(input$xgbalpha, {
            xgboosthold$xgbalpha <- input$xgbalpha
        })
        
        observeEvent(input$xgbgamma, {
            xgboosthold$xgbgamma <- input$xgbgamma
        })
        
        observeEvent(input$xgbeta, {
            xgboosthold$xgbeta <- input$xgbeta
        })
        
        
        observeEvent(input$xgblambda, {
            xgboosthold$xgblambda <- input$xgblambda
        })
        
        observeEvent(input$xgbsubsample, {
            xgboosthold$xgbsubsample <- input$xgbsubsample
        })
        
        observeEvent(input$xgbcolsample, {
            xgboosthold$xgbcolsample <- input$xgbcolsample
        })
        
        observeEvent(input$xgbminchild, {
            xgboosthold$xgbminchild <- input$xgbminchild
        })
        
        observeEvent(input$bartk, {
            barthold$bartk <- input$bartk
        })
        
        observeEvent(input$bartbeta, {
            barthold$bartbeta <- input$bartbeta
        })
        
        observeEvent(input$bartnu, {
            barthold$bartnu <- input$bartnu
        })
        
        observeEvent(input$svmc, {
            svmhold$svmc <- input$svmc
        })
        
        observeEvent(input$svmdegree, {
            svmhold$svmdegree <- input$svmdegree
        })
        
        observeEvent(input$svmscale, {
            svmhold$svmscale <- input$svmscale
        })
        
        observeEvent(input$svmsigma, {
            svmhold$svmsigma <- input$svmsigma
        })
        
        observeEvent(input$svmlength, {
            svmhold$svmlength <- input$svmlength
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
            } else if(input$radiocal==10){
                actionButton("mclrun", "Run Model")
            } else if(input$radiocal==11){
                actionButton("mclrun", "Run Model")
            } else if(input$radiocal==12){
                actionButton("mclrun", "Run Model")
            }   else if(input$radiocal==13){
                actionButton("mclrun", "Run Model")
            }
            
        })
        
        output$foresttryui <- renderUI({
            req(input$radiocal)
            if(input$radiocal==6 | input$radiocal==7){
                forestTryUI(radiocal=input$radiocal, neuralhiddenlayers=input$neuralhiddenlayers, selection=calForestTrySelectionpre(), maxsample=maxSample())
            } else {
                forestTryUI(radiocal=input$radiocal, neuralhiddenlayers=NULL, selection=calForestTrySelectionpre(), maxsample=maxSample())
            }
        })
        
        
        output$forestmetricui <- renderUI({
            req(input$radiocal)
            forestMetricUI(radiocal=input$radiocal, selection=calForestMetricSelectionpre())
        })
        
        
        output$foresttrainui <- renderUI({
            req(input$radiocal)
            forestTrainUI(radiocal=input$radiocal, selection=calForestTCSelectionpre())
        })
        
        output$forestnumberui <- renderUI({
            req(input$radiocal, input$foresttrain)
            forestNumberUI(radiocal=input$radiocal, selection=calForestNumberSelectionpre())
        })
        
        output$cvrepeatsui <- renderUI({
            req(input$radiocal)
            tryCatch(cvRepeatsUI(radiocal=input$radiocal, foresttrain=input$foresttrain, selection=calCVRepeatsSelectionpre()), error=function(e) NULL)
        })
        
        
        output$foresttreesui <- renderUI({
            req(input$radiocal)
            if(input$radiocal==4 | input$radiocal==5){
                forestTreesUI(radiocal=input$radiocal, selection=calForestTreeSelectionpre())
            } else if(input$radiocal==8 | input$radiocal==9){
                forestTreesUI(radiocal=input$radiocal, selection=calForestTreeSelectionpre(), xgbtype=input$xgbtype)
            } else if(input$radiocal==10 | input$radiocal==11){
                forestTreesUI(radiocal=input$radiocal, selection=calForestTreeSelectionpre(), xgbtype=input$xgbtype)
            }
            
        })
        
        output$neuralhiddenlayersui <- renderUI({
            req(input$radiocal)
            neuralHiddenLayersUI(radiocal=input$radiocal, selection=calHiddenLayersSelectionpre())
        })
        
        output$neuralhiddenunitsui <- renderUI({
            req(input$radiocal)
            if(input$radiocal==6){
                neuralHiddenUnitsUi(radiocal=input$radiocal, selection=calHiddenUnitsSelectionpre())
            } else if(input$radiocal==7){
                neuralHiddenUnitsUi(radiocal=input$radiocal, selection=calHiddenUnitsSelectionpre())
            } else if(input$radiocal==10){
                neuralHiddenUnitsUi(radiocal=input$radiocal, selection=calHiddenUnitsSelectionpre(), xgbtype=input$xgbtype)
            } else if(input$radiocal==11){
                neuralHiddenUnitsUi(radiocal=input$radiocal, selection=calHiddenUnitsSelectionpre(), xgbtype=input$xgbtype)
            } else {
                NULL
            }
            
        })
        
        output$neuralweightdecayui <- renderUI({
            req(input$radiocal)
            tryCatch(neuralWeightDecayUI(radiocal=input$radiocal, selection=calWeightDecaySelectionpre(), neuralhiddenlayers=input$neuralhiddenlayers), error=function(e) NULL)
        })
        
        output$neuralmaxiterationsui <- renderUI({
            req(input$radiocal)
            tryCatch(neuralMaxIterationsUI(radiocal=input$radiocal, selection=calMaxIterationsSelectionpre(), neuralhiddenlayers=input$neuralhiddenlayers), error=function(e) NULL)
        })
        
        output$treedepthui <- renderUI({
            req(input$radiocal)
            treeDepthUI(radiocal=input$radiocal, selection=calTreeDepthSelectionpre(), xgbtype=input$xgbtype)
        })
        
        output$xgbtypeui <- renderUI({
            req(input$radiocal)
            xgbTypeUI(radiocal=input$radiocal, selection=calXGBTypeSelectionpre())
        })
        
        output$xgbalphaui <- renderUI({
            req(input$radiocal, input$xgbtype)
            xgbAlphaUI(radiocal=input$radiocal, selection=calXGBAlphaSelectionpre(), xgbtype=input$xgbtype)
        })
        
        output$xgbgammaui <- renderUI({
            req(input$radiocal, input$xgbtype)
            xgbGammaUI(radiocal=input$radiocal, selection=calXGBGammaSelectionpre(), xgbtype=input$xgbtype)
        })
        
        output$xgbetaui <- renderUI({
            req(input$radiocal)
            xgbEtaUI(radiocal=input$radiocal, selection=calXGBEtaSelectionpre())
        })
        
        output$xgblambdaui <- renderUI({
            req(input$radiocal, input$xgbtype)
            xgbLambdaUI(radiocal=input$radiocal, selection=calxgboostLambdaSelectionpre(), xgbtype=input$xgbtype)
        })
        
        output$xgbsubsampleui <- renderUI({
            req(input$radiocal, input$xgbtype)
            xgbSubSampleUI(radiocal=input$radiocal, selection=calXGBSubSampleSelectionpre(), xgbtype=input$xgbtype)
        })
        
        output$xgbcolsampleui <- renderUI({
            req(input$radiocal, input$xgbtype)
            xgbColSampleUI(radiocal=input$radiocal, selection=calXGBColSampleSelectionpre(), xgbtype=input$xgbtype)
        })
        
        output$xgbminchildui <- renderUI({
            req(input$radiocal, input$xgbtype)
            xgbMinChildUI(radiocal=input$radiocal, selection=calXGBMinChildSelectionpre(), xgbtype=input$xgbtype)
        })
        
        output$bartkui <- renderUI({
            req(input$radiocal, input$xgbtype)
            bartKUI(radiocal=input$radiocal, selection=calBARTKSelectionpre(), xgbtype=input$xgbtype)
        })
        
        output$bartbetaui <- renderUI({
            req(input$radiocal, input$xgbtype)
            bartBetaUI(radiocal=input$radiocal, selection=calBARTKSelectionpre(), xgbtype=input$xgbtype)
        })
        
        output$bartnuui <- renderUI({
            req(input$radiocal, input$xgbtype)
            bartNuUI(radiocal=input$radiocal, selection=calBARTKSelectionpre(), xgbtype=input$xgbtype)
        })
        
        output$svmcui <- renderUI({
            req(input$radiocal, input$xgbtype)
            svmCUI(radiocal=input$radiocal, selection=calSVMCSelectionpre())
        })
        
        output$svmdegreeui <- renderUI({
            req(input$radiocal, input$xgbtype)
            svmDegreeUI(radiocal=input$radiocal, selection=calSVMDegreeSelectionpre(), xgbtype=input$xgbtype)
        })
        
        output$svmscaleui <- renderUI({
            req(input$radiocal, input$xgbtype)
            svmScaleUI(radiocal=input$radiocal, selection=calSVMScaleSelectionpre(), xgbtype=input$xgbtype)
        })
        
        output$svmsigmaui <- renderUI({
            req(input$radiocal, input$xgbtype)
            svmSigmaUI(radiocal=input$radiocal, selection=calSVMSigmaSelectionpre(), xgbtype=input$xgbtype)
        })
        
        output$svmlengthui <- renderUI({
            req(input$radiocal, input$xgbtype)
            svmLengthUI(radiocal=input$radiocal, selection=calSVMLengthSelectionpre(), xgbtype=input$xgbtype)
        })
        
        
        
        
        predictFramePre <- reactive({
            
            intensity <- holdFrame()$Intensity
            
            concentration <- holdFrame()$Concentration
            
            predict.frame <- data.frame(concentration, intensity)
            colnames(predict.frame) <- c("Concentration", "Intensity")
            
            
            predict.frame[complete.cases(predict.frame),]
            
            
        })
        
        
        
        predictIntensity <- reactive(label="predictIntensity",{
            #req(input$radiocal)
            predict.intensity <- if(input$radiocal==1){
                linearModelSet()$data
            } else if(input$radiocal==2){
                nonLinearModelSet()$data
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
            } else if(input$radiocal==10){
                bayesIntensityModelSet()$data[,!colnames(bayesIntensityModelSet()$data) %in% c("Spectrum", "Concentration")]
            } else if(input$radiocal==11){
                bayesSpectraModelSet()$data[,!colnames(bayesSpectraModelSet()$data) %in% c("Spectrum", "Concentration")]
            } else if(input$radiocal==12){
                svmIntensityModelSet()$data[,!colnames(svmIntensityModelSet()$data) %in% c("Spectrum", "Concentration")]
            } else if(input$radiocal==13){
                svmSpectraModelSet()$data[,!colnames(svmSpectraModelSet()$data) %in% c("Spectrum", "Concentration")]
            }
            predictFrameCheck(predict.intensity)
            
        })
        
        
        output$testingagain <- renderDataTable({
            predictFrame()
            
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
            } else if(input$radiocal==10){
                bayesIntensityModelSet()$data
            } else if(input$radiocal==11){
                bayesSpectraModelSet()$data
            } else if(input$radiocal==12){
                svmIntensityModelSet()$data
            } else if(input$radiocal==13){
                svmSpectraModelSet()$data
            }
        })
        
        modelParameters <- reactive({
            req(input$radiocal, input$calcurveelement)
            tryCatch(if(input$radiocal==1){
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
            } else if(input$radiocal==10){
                bayesIntensityModelSet()$parameters
            } else if(input$radiocal==11){
                bayesSpectraModelSet()$parameters
            } else if(input$radiocal==12){
                svmIntensityModelSet()$parameters
            } else if(input$radiocal==13){
                svmSpectraModelSet()$parameters
            }, error=function(e) NULL)
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
                tryCatch(linearModel(), error=function(e) NULL)
            } else if(input$radiocal==2){
                tryCatch(nonLinearModel(), error=function(e) NULL)
            } else if(input$radiocal==3){
                tryCatch(lucasToothModel(), error=function(e) NULL)
            } else if(input$radiocal==4){
                tryCatch(forestModel(), error=function(e) NULL)
            } else if(input$radiocal==5){
                tryCatch(rainforestModel(), error=function(e) NULL)
            } else if(input$radiocal==6){
                tryCatch(neuralNetworkIntensityModel(), error=function(e) NULL)
            } else if(input$radiocal==7){
                tryCatch(neuralNetworkSpectraModel(), error=function(e) NULL)
            } else if(input$radiocal==8){
                tryCatch(xgboostIntensityModel(), error=function(e) NULL)
            } else if(input$radiocal==9){
                tryCatch(xgboostSpectraModel(), error=function(e) NULL)
            } else if(input$radiocal==10){
                tryCatch(bayesIntensityModel(), error=function(e) NULL)
            } else if(input$radiocal==11){
                bayesSpectraModel()
            } else if(input$radiocal==12){
                svmIntensityModel()
            } else if(input$radiocal==13){
                svmSpectraModel()
            }
        })
        
        #observeEvent(input$calcurveelement, priority=101, {
        #    if(!input$calcurveelement %in% names(calMemory$Calibration$calList)){
        #       calMemory$Calibration$calList[[input$calcurveelement]] <- list(lucasToothModelSet()$parameters, lucasToothModel())

        #   }
        #})
        
        
        observeEvent(input$createcalelement, priority=100, {
            calMemory$Calibration$calList[[input$calcurveelement]] <- NULL
                calMemory$Calibration$calList[[input$calcurveelement]] <- isolate(modelPack(parameters=modelParameters(), model=elementModelGen(), compress=TRUE))
                calSettings$calList[[input$calcurveelement]] <- NULL
                    calSettings$calList[[input$calcurveelement]] <- isolate(modelPack(parameters=modelParameters(), model=NULL, compress=TRUE))
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
            req(input$calcurveelement, input$radiocal)
            
            val.frame <- tryCatch(mclValGen(model=elementModel(), data=predictIntensity(), predict.frame=predictFrame(), dependent.transformation=basichold$deptransformation), error=function(e) NULL)
            
            if(is.null(val.frame)){
                data.frame(Concentration=predictFrame()$Concentration, Intensity=rep(0, length(predictFrame()$Concentration), Prediction=rep(0, length(predictFrame()$Concentration))), stringsAsFactors=FALSE)
            } else if(!is.null(val.frame)){
                val.frame
            }
            
        })
        
        valFrameVal <- reactiveValues()
        valFrameVal$val.frame <- NULL
        
        #observeEvent(!is.null(values[["DF"]]), priority=75, {
        #    valFrameVal$val.frame <- valFrame()
        #})
        
        observeEvent(modelParameters(), priority=77, {
            #req(input$calcurvelement, input$radiocal)
            valFrameVal$val.frame <- tryCatch(valFrame(), error=function(e) NULL)
        })
        
        #observeEvent(input$radiocal, priority=-1, {
        #    valFrameVal$val.frame <- valFrame()
        #})
        
        observeEvent(input$trainslopes, priority=75, {
            valFrameVal$val.frame <- tryCatch(valFrame(), error=function(e) NULL)
        })

        observeEvent(input$createcalelement, priority=97, {
            tryCatch(valFrameVal$val.frame <- valFrame(), error=function(e) NULL)
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
            } else if(input$radiocal==10){
                3
            } else if(input$radiocal==11){
                5
            } else if(input$radiocal==12){
                3
            } else if(input$radiocal==13){
                5
            }
            
        })
        
    
        
        rangescalcurve <- reactiveValues(x = NULL, y = NULL)
        
        
        
        calCurvePlotPre <- reactive(label="calCurvePlotPre",{
            req(input$calcurveelement, input$radiocal)
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
                    annotate("text", label=lm_eqn_poly(lm(Concentration~Intensity + I(Intensity^2), data=predictFrame()[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                    stat_smooth(method="lm", formula=y~poly(x,2), fullrange = TRUE) +
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
                    geom_smooth() +
                    geom_point() +
                    geom_point(aes(Intensity, Concentration), data = valFrame()[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
                    scale_x_continuous(paste(element.name, norma), breaks=scales::pretty_breaks()) +
                    scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                    coord_cartesian(xlim = rangescalcurve$x, ylim = rangescalcurve$y, expand = TRUE), error=function(e) NULL)
                } else if(input$loglinear=="Log"){
                    tryCatch(ggplot(data=valFrame()[ vals$keeprows, , drop = FALSE], aes(Intensity, Concentration)) +
                    theme_light() +
                    annotate("text", label=lm_eqn(lm(Concentration~., valFrame()[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                    geom_smooth() +
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
            
            if(input$radiocal==10){
                
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
            
            if(input$radiocal==11){
                
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
            
            if(input$radiocal==12){
                
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
            
            if(input$radiocal==13){
                
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
        
        calCurvePlot <- reactive(label="calCurvePlot", {
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
        
        calCurvePlotex <- reactive(label="calCurvePlot",{
            calCurvePlotPre()
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
        
        valCurvePlot <- reactive(label="valCurvePlot", {
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
        
        valCurvePlotex <- reactive({
            valCurvePlotPre()
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
                number = parameters$ForestNumber)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                rf_model <- tryCatch(caret::train(Concentration~.,data=predict.frame, method="rf", type="Regression", trControl=tune_control, ntree=parameters$ForestTrees, prox=TRUE, importance=TRUE, metric=parameters$ForestMetric, tuneGrid=rf.grid, na.action=na.omit, trim=TRUE), error=function(e) NULL)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                rf_model <- tryCatch(caret::train(Concentration~.,data=predict.frame, method="rf", type="Regression", trControl=tune_control, ntree=parameters$ForestTrees, prox=TRUE,allowParallel=TRUE, importance=TRUE, metric=parameters$ForestMetric, tuneGrid=rf.grid, na.action=na.omit, trim=TRUE), error=function(e) NULL)
                stopCluster(cl)
            }
            rf_model
            
        })
        
        rainforestModelRandom <- reactive(label="rainforestModelRandom",{
            
            data <- rainforestModelSet()$data[randomizeData(),]
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
                number = parameters$ForestNumber)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                rf_model <- tryCatch(caret::train(Concentration~.,data=data[,-1], method="rf", type="Regression", trControl=tune_control, ntree=parameters$ForestTrees, prox=TRUE, importance=TRUE, metric=parameters$ForestMetric, tuneGrid=rf.grid, na.action=na.omit, trim=TRUE), error=function(e) NULL)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                rf_model <- tryCatch(caret::train(Concentration~.,data=data[,-1], method="rf", type="Regression", trControl=tune_control, ntree=parameters$ForestTrees, prox=TRUE,allowParallel=TRUE, importance=TRUE, metric=parameters$ForestMetric, tuneGrid=rf.grid, na.action=na.omit, trim=TRUE), error=function(e) NULL)
                stopCluster(cl)
            }
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
                number = parameters$ForestNumber)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats)
            }
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                nn_model <- tryCatch(caret::train(Concentration~.,data=predict.frame, method="nnet", linout=TRUE, trControl=tune_control, metric=parameters$ForestMetric, na.action=na.omit, importance=TRUE, tuneGrid=nn.grid, maxit=parameters$NeuralMI, trace=F, trim=TRUE), error=function(e) NULL)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                nn_model <- tryCatch(caret::train(Concentration~.,data=predict.frame, method="nnet", linout=TRUE, trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, na.action=na.omit, importance=TRUE, tuneGrid=nn.grid, maxit=parameters$NeuralMI, trace=F, trim=TRUE), error=function(e) NULL)
                stopCluster(cl)
            }
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            f <- as.formula(paste("Concentration ~", paste(names(predict.frame)[!names(predict.frame) %in% "Concentration"], collapse = " + ")))
            
            
            if(input$multicore_behavior=="Single Core"){
                nn_model <- tryCatch(caret::train(f,data=predict.frame, method="neuralnet", rep=parameters$ForestTry, trControl=tune_control, metric=parameters$ForestMetric, na.action=na.omit,  tuneGrid=nn.grid, linear.output=TRUE), error=function(e) NULL)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                nn_model <- tryCatch(caret::train(f,data=predict.frame, method="neuralnet", rep=parameters$ForestTry, trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, na.action=na.omit,  tuneGrid=nn.grid, linear.output=TRUE), error=function(e) NULL)
                stopCluster(cl)
            }
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
                number = parameters$ForestNumber)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats)
            }
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                nn_model <- tryCatch(caret::train(Concentration~.,data=data[,-1], method="nnet", linout=TRUE, trControl=tune_control, metric=parameters$ForestMetric, na.action=na.omit, importance=TRUE, tuneGrid=nn.grid, maxit=parameters$NeuralMI, trace=F, trim=TRUE), error=function(e) NULL)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                nn_model <- tryCatch(caret::train(Concentration~.,data=data[,-1], method="nnet", linout=TRUE, trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, na.action=na.omit, importance=TRUE, tuneGrid=nn.grid, maxit=parameters$NeuralMI, trace=F, trim=TRUE), error=function(e) NULL)
                stopCluster(cl)
            }
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            f <- as.formula(paste("Concentration ~", paste(names(data)[!names(data) %in% "Concentration"], collapse = " + ")))
            
            if(input$multicore_behavior=="Single Core"){
                nn_model <- tryCatch(caret::train(f,data=data[,-1], method="neuralnet", rep=parameters$ForestTry, trControl=tune_control, metric=parameters$ForestMetric, na.action=na.omit, tuneGrid=nn.grid, linear.output=TRUE), error=function(e) NULL)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                nn_model <- tryCatch(caret::train(f,data=data[,-1], method="neuralnet", rep=parameters$ForestTry, trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, na.action=na.omit, tuneGrid=nn.grid, linear.output=TRUE), error=function(e) NULL)
                stopCluster(cl)
            }
            nn_model
            
        })
        
        xgbtreeIntensityModelRandom <- reactive(label="xgbtreeIntensityModelRandom",{
            
            predict.frame <- xgbtreeIntensityModelSet()$data[randomizeData(),]
            parameters <- xgbtreeIntensityModelSet()$parameters$CalTable
            
            
            tree.depth.vec <- as.numeric(unlist(strsplit(as.character(parameters$TreeDepth), "-")))
            xgbeta.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbEta), "-")))
            xgbgamma.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbGamma), "-")))
            xgbsubsample.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbSubSample), "-")))
            xgbcolsample.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbColSample), "-")))
            
            metricModel <- if(parameters$ForestMetric=="RMSE" | parameters$ForestMetric=="Rsquared"){
                defaultSummary
            } else if(parameters$ForestMetric=="MAE"){
                maeSummary
            } else if(parameters$ForestMetric=="logMAE"){
                logmaeSummary
            } else if(parameters$ForestMetric=="SMAPE"){
                smapeSummary
            }
            
            
            xgbGrid <- expand.grid(
            nrounds = seq(50, parameters$ForestTrees, by=parameters$ForestTrees/5),
            max_depth = seq(tree.depth.vec[1], tree.depth.vec[2], by=5),
            eta = seq(xgbeta.vec[1], xgbeta.vec[2], by=0.1),
            gamma=seq(xgbgamma.vec[1], xgbgamma.vec[2], by=0.1),
            colsample_bytree = seq(xgbcolsample.vec[1], xgbcolsample.vec[2], by=0.1),
            subsample = seq(xgbsubsample.vec[1], xgbsubsample.vec[2], by=0.1),
            min_child_weight = parameters$xgbMinChild
            )
            
            tune_control <- if(parameters$ForestTC!="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
                
            
            if(input$multicore_behavior=="Single Core"){
                xgb_model <- caret::train(Concentration~., data=predict.frame, trControl = tune_control, tuneGrid = xgbGrid, metric=parameters$ForestMetric, method = "xgbTree", na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                xgb_model <- caret::train(Concentration~., data=predict.frame, trControl = tune_control, tuneGrid = xgbGrid, metric=parameters$ForestMetric, method = "xgbTree", na.action=na.omit, allowParallel=TRUE)
                stopCluster(cl)
            } else if(input$multicore_behavior=="OpenMP"){
                xgb_model <- caret::train(Concentration~., data=predict.frame, trControl = tune_control, tuneGrid = xgbGrid, metric=parameters$ForestMetric, method = "xgbTree", na.action=na.omit, nthread=as.numeric(cores.to.use))
            }
            
            xgb_model
            
        })
        
        xgblinearIntensityModelRandom <- reactive(label="xglinearIntensityModelRandom", {
            req(input$radiocal, input$calcurveelement)
            
            predict.frame <- xgblinearIntensityModelSet()$data[randomizeData(),]
            parameters <- xgblinearIntensityModelSet()$parameters$CalTable
            
            
            xgbalpha.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbAlpha), "-")))
            xgbeta.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbEta), "-")))
            xgblambda.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbLambda), "-")))
            
            xgbGrid <- expand.grid(
            nrounds = seq(50, parameters$ForestTrees, by=parameters$ForestTrees/5),
            alpha=seq(xgbalpha.vec[1], xgbalpha.vec[2], by=0.1),
            eta = seq(xgbeta.vec[1], xgbeta.vec[2], by=0.1),
            lambda = seq(xgblambda.vec[1], xgblambda.vec[2], by=0.1)
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
                
            
            if(input$multicore_behavior=="Single Core"){
                xgb_model <- caret::train(Concentration~., data=predict.frame, trControl = tune_control, tuneGrid = xgbGrid, metric=parameters$ForestMetric, method = "xgbLinear", na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                xgb_model <- caret::train(Concentration~., data=predict.frame, trControl = tune_control, tuneGrid = xgbGrid, metric=parameters$ForestMetric, method = "xgbLinear", na.action=na.omit, allowParallel=TRUE)
                stopCluster(cl)
            } else if(input$multicore_behavior=="OpenMP"){
                xgb_model <- caret::train(Concentration~., data=predict.frame, trControl = tune_control, tuneGrid = xgbGrid, metric=parameters$ForestMetric, method = "xgbLinear", na.action=na.omit, nthread=as.numeric(cores.to.use))
            }
            xgb_model
            
        })
        
        xgboostIntensityModelRandom <- reactive({
            
            if(input$xgbtype=="Tree"){
                xgbtreeIntensityModelRandom()
            } else if(input$xgbtype=="Linear"){
                xgblinearIntensityModelRandom()
            }
            
        })
        
        
        xgbtreeSpectraModelRandom <- reactive(label="xgbtreeSpectraModelRandom",{
            
            data <- xgbtreeSpectraModelSet()$data[randomizeData(),]
            parameters <- xgbtreeSpectraModelSet()$parameters$CalTable
            
            tree.depth.vec <- as.numeric(unlist(strsplit(as.character(parameters$TreeDepth), "-")))
            xgbeta.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbEta), "-")))
            xgbgamma.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbGamma), "-")))
            xgbsubsample.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbSubSample), "-")))
            xgbcolsample.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbColSample), "-")))
            
            metricModel <- if(parameters$ForestMetric=="RMSE" | parameters$ForestMetric=="Rsquared"){
                defaultSummary
            } else if(parameters$ForestMetric=="MAE"){
                maeSummary
            } else if(parameters$ForestMetric=="logMAE"){
                logmaeSummary
            } else if(parameters$ForestMetric=="SMAPE"){
                smapeSummary
            }
            
            
            xgbGrid <- expand.grid(
            nrounds = seq(50, parameters$ForestTrees, by=parameters$ForestTrees/5),
            max_depth = seq(tree.depth.vec[1], tree.depth.vec[2], by=5),
            eta = seq(xgbeta.vec[1], xgbeta.vec[2], by=0.1),
            gamma=seq(xgbgamma.vec[1], xgbgamma.vec[2], by=0.1),
            colsample_bytree = seq(xgbcolsample.vec[1], xgbcolsample.vec[2], by=0.1),
            subsample = seq(xgbsubsample.vec[1], xgbsubsample.vec[2], by=0.1),
            min_child_weight = parameters$xgbMinChild
            )
            
            tune_control <- if(parameters$ForestTC!="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
                
            
            if(input$multicore_behavior=="Single Core"){
                xgb_model <- caret::train(Concentration~., data=data[,-1], trControl = tune_control, tuneGrid = xgbGrid, metric=parameters$ForestMetric, method = "xgbTree", na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                xgb_model <- caret::train(Concentration~., data=data[,-1], trControl = tune_control, tuneGrid = xgbGrid, metric=parameters$ForestMetric, method = "xgbTree", na.action=na.omit, allowParallel=TRUE)
                stopCluster(cl)
            } else if(input$multicore_behavior=="OpenMP"){
                xgb_model <- caret::train(Concentration~., data=data[,-1], trControl = tune_control, tuneGrid = xgbGrid, metric=parameters$ForestMetric, method = "xgbTree", na.action=na.omit, nthread=as.numeric(cores.to.use))
            }
            
            xgb_model
            
            
        })
        
        xgblinearSpectraModelRandom <- reactive(label="xgblinearSpectraModelRandom", {
            req(input$radiocal, input$calcurveelement)
            
            data <- xgblinearSpectraModelSet()$data[randomizeData(),]
            parameters <- xgblinearSpectraModelSet()$parameters$CalTable
            
            xgbalpha.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbAlpha), "-")))
            xgbeta.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbEta), "-")))
            xgblambda.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbLambda), "-")))
            
            
            xgbGrid <- expand.grid(
            nrounds = seq(50, parameters$ForestTrees, by=parameters$ForestTrees/5),
            alpha=seq(xgbalpha.vec[1], xgbalpha.vec[2], by=0.1),
            eta = seq(xgbeta.vec[1], xgbeta.vec[2], by=0.1),
            lambda = seq(xgblambda.vec[1], xgblambda.vec[2], by=0.1)
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
                
            
            if(input$multicore_behavior=="Single Core"){
                xgb_model <- caret::train(Concentration~., data=data[,-1], trControl = tune_control, tuneGrid = xgbGrid, metric=parameters$ForestMetric, method = "xgbLinear", na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                xgb_model <- caret::train(Concentration~., data=data[,-1], trControl = tune_control, tuneGrid = xgbGrid, metric=parameters$ForestMetric, method = "xgbLinear", na.action=na.omit, allowParallel=TRUE)
                stopCluster(cl)
            } else if(input$multicore_behavior=="OpenMP"){
                xgb_model <- caret::train(Concentration~., data=data[,-1], trControl = tune_control, tuneGrid = xgbGrid, metric=parameters$ForestMetric, method = "xgbLinear", na.action=na.omit, nthread=as.numeric(cores.to.use))
            }
            
            xgb_model
            
        })
        
        xgboostSpectraModelRandom <- reactive({
            
            if(input$xgbtype=="Tree"){
                xgbtreeSpectraModelRandom()
            } else if(input$xgbtype=="Linear"){
                xgblinearSpectraModelRandom()
            }
            
        })
        
        bartMachineIntensityModelRandomized <- reactive(label="bartMachineIntensityModel", {
            req(input$radiocal, input$calcurveelement)
            predict.frame <- bartMachineIntensityModelSet()$data[randomizeData(),]
            parameters <- bartMachineIntensityModelSet()$parameters$CalTable
            
            xgbalpha.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbAlpha), "-")))
            k.vec <- dnorminv(1-(as.numeric(unlist(strsplit(as.character(parameters$bartK), "-")))/100))
            bartbeta.vec <- as.numeric(unlist(strsplit(as.character(parameters$bartBeta), "-")))
            bartnu.vec <- as.numeric(unlist(strsplit(as.character(parameters$bartNu), "-")))
            
            bart.grid <- expand.grid(
                num_trees=parameters$ForestTrees,
                alpha=seq(xgbalpha.vec[1], xgbalpha.vec[2], by=0.1),
                beta=seq(bartbeta.vec[1], bartbeta.vec[2], by=0.1),
                nu=seq(bartnu.vec[1], bartnu.vec[2], by=0.1),
                k=seq(k.vec[1], k.vec[2], by=0.5))
            
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
                verboseIter = TRUE,
                allowParallel = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE,
                allowParallel = TRUE)
            }
            
            
            
            bart_model <- tryCatch(caret::train(Concentration~., data=predict.frame, method="bartMachine", trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=bart.grid, serialize = TRUE), error=function(e) NULL)
            
            stopCluster(cl)
            bart_model
            
        })
        
        bayesLinearIntensityModelRandomized <- reactive(label="bayesLinearIntensityModel", {
            req(input$radiocal, input$calcurveelement)
            predict.frame <- bayesLinearIntensityModelSet()$data[randomizeData(),]
            parameters <- bayesLinearIntensityModelSet()$parameters$CalTable
            
            
            bart.grid <- NULL
            
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                bart_model <- tryCatch(caret::train(Concentration~., data=predict.frame, method="bayesglm", trControl=tune_control, metric=parameters$ForestMetric), error=function(e) NULL)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                bart_model <- tryCatch(caret::train(Concentration~., data=predict.frame, method="bayesglm", trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric), error=function(e) NULL)
                stopCluster(cl)
            }
            bart_model
            
        })
        
        bayesNeuralNetIntensityModelRandomized <- reactive(label="bayesNeuralNetIntensityModel", {
            req(input$radiocal, input$calcurveelement)
            predict.frame <- bartMachineIntensityModelSet()$data[randomizeData(),]
            parameters <- bartMachineIntensityModelSet()$parameters$CalTable
            
            
            hiddenunits.vec <- as.numeric(unlist(strsplit(as.character(parameters$NeuralHU), "-")))
            
            
            bart.grid <- expand.grid(
            neurons = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1))
            
            
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                bart_model <- tryCatch(caret::train(Concentration~.,data=predict.frame, method="brnn", trControl=tune_control, importance=TRUE, metric=parameters$ForestMetric, tuneGrid=bart.grid, na.action=na.omit), error=function(e) NULL)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                bart_model <- tryCatch(caret::train(Concentration~.,data=predict.frame, method="brnn", trControl=tune_control, allowParallel=TRUE, importance=TRUE, metric=parameters$ForestMetric, tuneGrid=bart.grid, na.action=na.omit), error=function(e) NULL)
                stopCluster(cl)
            }
            bart_model
            
        })
        
        bayesIntensityModelRandomized <- reactive(label="bayesIntensityModel", {
            
            if(xgboosthold$xgbtype=="Tree"){
                bartMachineIntensityModelRandomized()
            } else if(xgboosthold$xgbtype=="Linear"){
                bayesLinearIntensityModelRaandomized()
            } else if(xgboosthold$xgbtype=="Neural Net"){
                bayesNeuralNetIntensityModelRandomized()
            }
            
        })
        
        bartMachineSpectraModelRandomized <- reactive(label="bartMachineSpectraModel", {
            req(input$radiocal, input$calcurveelement)
            data <- bartMachineSpectraModelSet()$data[randomizeData(),]
            parameters <- bartMachineSpectraModelSet()$parameters$CalTable
            
            xgbalpha.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbAlpha), "-")))
            k.vec <- dnorminv(1-(as.numeric(unlist(strsplit(as.character(parameters$bartK), "-")))/100))
            bartbeta.vec <- as.numeric(unlist(strsplit(as.character(parameters$bartBeta), "-")))
            bartnu.vec <- as.numeric(unlist(strsplit(as.character(parameters$bartNu), "-")))
            
            bart.grid <- expand.grid(
                num_trees=parameters$ForestTrees,
                alpha=seq(xgbalpha.vec[1], xgbalpha.vec[2], by=0.1),
                beta=seq(bartbeta.vec[1], bartbeta.vec[2], by=0.1),
                nu=seq(bartnu.vec[1], bartnu.vec[2], by=0.1),
                k=seq(k.vec[1], k.vec[2], by=0.5))
            
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
                verboseIter = TRUE,
                allowParallel = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE,
                allowParallel = TRUE)
            }
            
            
            
            bart_model <- tryCatch(caret::train(Concentration~.,data=data[,-1], method="bartMachine", trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=bart.grid, na.action=na.omit, serialize = TRUE), error=function(e) NULL)
            
            
            stopCluster(cl)
            bart_model
            
        })
        
        bayesLinearSpectraModelRandomized <- reactive(label="bayesLinearSpectraModel", {
            req(input$radiocal, input$calcurveelement)
            data <- bayesLinearSpectraModelSet()$data[randomizeData(),]
            parameters <- bayesLinearSpectraModelSet()$parameters$CalTable
            
            
            bart.grid <- NULL
            
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                bart_model <- tryCatch(caret::train(Concentration~.,data=data[,-1], method="bayesglm", trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=bart.grid), error=function(e) NULL)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                bart_model <- tryCatch(caret::train(Concentration~.,data=data[,-1], method="bayesglm", trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=bart.grid), error=function(e) NULL)
                stopCluster(cl)
            }
            bart_model
            
        })
        
        bayesNeuralNetSpectraModelRandomized <- reactive(label="bayesNeuralNetSpectraModel", {
            req(input$radiocal, input$calcurveelement)
            data <- bayesNeuralNetSpectraModelSet()$data[randomizeData(),]
            parameters <- bayesNeuralNetSpectraModelSet()$parameters$CalTable
            
            
            hiddenunits.vec <- as.numeric(unlist(strsplit(as.character(parameters$NeuralHU), "-")))
            
            
            bart.grid <- expand.grid(
            neurons = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1))
            
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                bart_model <- tryCatch(caret::train(Concentration~.,data=data[,-1], method="brnn", trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=bart.grid), error=function(e) NULL)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                bart_model <- tryCatch(caret::train(Concentration~.,data=data[,-1], method="brnn", trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=bart.grid), error=function(e) NULL)
                stopCluster(cl)
            }
            bart_model
            
        })
        
        bayesSpectraModelRandomized <- reactive(label="bayesSpectraModel", {
            
            if(xgboosthold$xgbtype=="Tree"){
                bartMachineSpectraModelRandomized()
            } else if(xgboosthold$xgbtype=="Linear"){
                bayesLinearSpectraModelRandomized()
            } else if(xgboosthold$xgbtype=="Neural Net"){
                bayesNeuralNetSpectraModelRandomized()
            }
            
        })
        
        svmLinearIntensityModelRandomized <- reactive(label="svmLinearIntensityModel", {
            req(input$radiocal, input$calcurveelement)
            predict.frame <- svmLinearIntensityModelSet()$data[randomizeData(),]
            parameters <- svmLinearIntensityModelSet()$parameters$CalTable
            
            
            svmc.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmC), "-")))
            
            
            svm.grid <- expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1))
            
            
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                svm_model <- tryCatch(caret::train(Concentration~.,data=predict.frame, method="svmLinear", trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit), error=function(e) NULL)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                svm_model <- tryCatch(caret::train(Concentration~.,data=predict.frame, method="svmLinear", trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit), error=function(e) NULL)
                stopCluster(cl)
            }
            svm_model
            
        })
        
        svmPolyIntensityModelRandomized <- reactive(label="svmPolyIntensityModel", {
            req(input$radiocal, input$calcurveelement)
            predict.frame <- svmPolyIntensityModelSet()$data[randomizeData(),]
            parameters <- svmPolyIntensityModelSet()$parameters$CalTable
            
            
            svmc.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmC), "-")))
            svmdegree.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmDegree), "-")))
            svmscale.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmScale), "-")))

            
            svm.grid <- expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1),
            scale=seq(svmscale.vec[1], svmscale.vec[2], 1),
            degree=seq(svmdegree.vec[1], svmdegree.vec[2], 1))
            
            
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                svm_model <- tryCatch(caret::train(Concentration~.,data=predict.frame, method="svmPoly", trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit), error=function(e) NULL)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                svm_model <- tryCatch(caret::train(Concentration~.,data=predict.frame, method="svmPoly", trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit), error=function(e) NULL)
                stopCluster(cl)
            }
            svm_model
            
        })
        
        svmRadialIntensityModelRandomized <- reactive(label="svmRadialIntensityModel", {
            req(input$radiocal, input$calcurveelement)
            predict.frame <- svmRadialIntensityModelSet()$data[randomizeData(),]
            parameters <- svmRadialIntensityModelSet()$parameters$CalTable
            
            
            svmc.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmC), "-")))
            svmsigma.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmSigma), "-")))
            
            svm.grid <- if(parameters$xgbType!="Radial Cost"){
                expand.grid(
                C = seq(svmc.vec[1], svmc.vec[2], 1),
                sigma=seq(svmsigma.vec[1], svmsigma.vec[2], 1))
            } else if(parameters$xgbType=="Radial Cost"){
                expand.grid(
                C = seq(svmc.vec[1], svmc.vec[2], 1))
            }
            
            svm.flavor <- if(parameters$xgbType=="Radial"){
                "svmRadial"
            } else if(parameters$xgbType=="Radial Cost"){
                "svmRadialCost"
            } else if(parameters$xgbType=="Radial Sigma"){
                "svmRadialSigma"
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                svm_model <- tryCatch(caret::train(Concentration~.,data=predict.frame, method=svm.flavor, trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit), error=function(e) NULL)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                svm_model <- tryCatch(caret::train(Concentration~.,data=predict.frame, method=svm.flavor, trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit), error=function(e) NULL)
                stopCluster(cl)
            }
            svm_model
            
        })
        
        svmBoundrangeIntensityModelRandomized <- reactive(label="svmBoundrangeIntensityModel", {
            req(input$radiocal, input$calcurveelement)
            predict.frame <- svmBoundrangeIntensityModelSet()$data[randomizeData(),]
            parameters <- svmBoundrangeIntensityModelSet()$parameters$CalTable
            
            
            svmc.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmC), "-")))
            svmlength.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmLength), "-")))
            
            svm.grid <- expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1),
            length=seq(svmlength.vec[1], svmlength.vec[2], 1))
            
            
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                svm_model <- tryCatch(caret::train(Concentration~.,data=predict.frame, method="svmBoundrangeString", trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit), error=function(e) NULL)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                svm_model <- tryCatch(caret::train(Concentration~.,data=predict.frame, method="svmBoundrangeString", trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit), error=function(e) NULL)
                stopCluster(cl)
            }
            svm_model
            
        })
        
        svmExponentialIntensityModelRandomized <- reactive(label="svmExponentialIntensityModel", {
            req(input$radiocal, input$calcurveelement)
            predict.frame <- svmExponentialIntensityModelSet()$data[randomizeData(),]
            parameters <- svmExponentialIntensityModelSet()$parameters$CalTable
            
            
            svmc.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmC), "-")))
            xgblambda.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbLambda), "-")))
            
            svm.grid <- expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1),
            lambda=seq(xgblambda.vec[1], xgblambda.vec[2], 1))
            
            
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                svm_model <- tryCatch(caret::train(Concentration~.,data=predict.frame, method="svmExpoString", trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit), error=function(e) NULL)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                svm_model <- tryCatch(caret::train(Concentration~.,data=predict.frame, method="svmExpoString", trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit), error=function(e) NULL)
                stopCluster(cl)
            }
            svm_model
            
        })
        
        svmSpectrumIntensityModelRandomized <- reactive(label="svmSpectrumIntensityModel", {
            req(input$radiocal, input$calcurveelement)
            predict.frame <- svmSpectrumIntensityModelSet()$data[randomizeData(),]
            parameters <- svmSpectrumIntensityModelSet()$parameters$CalTable
            
            
            svmc.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmC), "-")))
            svmlength.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmLength), "-")))
            
            svm.grid <- expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1),
            length=seq(svmlength.vec[1], svmlength.vec[2], 1))
            
            
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                svm_model <- tryCatch(caret::train(Concentration~.,data=predict.frame, method="svmSpectrumString", trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit), error=function(e) NULL)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                svm_model <- tryCatch(caret::train(Concentration~.,data=predict.frame, method="svmSpectrumString", trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit), error=function(e) NULL)
                stopCluster(cl)
            }
            svm_model
            
        })
        
        svmIntensityModelRandomized <- reactive(label="svmIntensityModel", {
            
            if(xgboosthold$xgbtype=="Linear"){
                svmLinearIntensityModelRandomized()
            } else if(xgboosthold$xgbtype=="Polynomial"){
                svmPolyIntensityModelRandomized()
            } else if(xgboosthold$xgbtype=="Exponential"){
                svmExponentialIntensityModelRandomized()
            } else if(xgboosthold$xgbtype=="Radial" | xgboosthold$xgbtype=="Radial Cost" | xgboosthold$xgbtype=="Radial Sigma"){
                svmRadialIntensityModelRandomized()
            } else if(xgboosthold$xgbtype=="Boundrange String"){
                svmBoundrangeIntensityModelRandomized()
            } else if(xgboosthold$xgbtype=="Spectrum String"){
                svmSpectrumIntensityModelRandomized()
            }
            
        })
        
        svmLinearSpectraModelRandomized <- reactive(label="svmLinearSpectraModel", {
            req(input$radiocal, input$calcurveelement)
            data <- svmLinearSpectraModelSet()$data[randomizeData(),]
            parameters <- svmLinearSpectraModelSet()$parameters$CalTable
            
            
            svmc.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmC), "-")))
            
            
            svm.grid <- expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1))
            
            
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                svm_model <- caret::train(Concentration~.,data=data[,-1], method="svmLinear", trControl=tune_control,  metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                svm_model <- caret::train(Concentration~.,data=data[,-1], method="svmLinear", trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
                stopCluster(cl)
            }
            svm_model
            
        })
        
        svmPolySpectraModelRandomized <- reactive(label="svmPolyIntensityModel", {
            req(input$radiocal, input$calcurveelement)
            data <- svmPolySpectraModelSet()$data[randomizeData(),]
            parameters <- svmPolySpectraModelSet()$parameters$CalTable
            
            
            svmc.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmC), "-")))
            svmdegree.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmDegree), "-")))
            svmscale.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmScale), "-")))

            
            svm.grid <- expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1),
            scale=seq(svmscale.vec[1], svmscale.vec[2], 1),
            degree=seq(svmdegree.vec[1], svmdegree.vec[2], 1))
            
            
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                svm_model <- caret::train(Concentration~., data=data[,-1], method="svmPoly", trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                svm_model <- caret::train(Concentration~., data=data[,-1], method="svmPoly", trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
                stopCluster(cl)
            }
            svm_model
            
        })
        
        svmRadialSpectraModelRandomized <- reactive(label="svmRadialSpectraModel", {
            req(input$radiocal, input$calcurveelement)
            data <- svmRadialSpectraModelSet()$data[randomizeData(),]
            parameters <- svmRadialSpectraModelSet()$parameters$CalTable
            
            
            svmc.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmC), "-")))
            svmsigma.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmSigma), "-")))
            
            svm.grid <- if(parameters$xgbType!="Radial Cost"){
                expand.grid(
                C = seq(svmc.vec[1], svmc.vec[2], 1),
                sigma=seq(svmsigma.vec[1], svmsigma.vec[2], 1))
            } else if(parameters$xgbType=="Radial Cost"){
                expand.grid(
                C = seq(svmc.vec[1], svmc.vec[2], 1))
            }
            
            svm.flavor <- if(parameters$xgbType=="Radial"){
                "svmRadial"
            } else if(parameters$xgbType=="Radial Cost"){
                "svmRadialCost"
            } else if(parameters$xgbType=="Radial Sigma"){
                "svmRadialSigma"
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                svm_model <- caret::train(Concentration~.,data=data[,-1], method=svm.flavor, trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                svm_model <- caret::train(Concentration~.,data=data[,-1], method=svm.flavor, trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
                stopCluster(cl)
            }
            svm_model
            
        })
        
        svmBoundrangeSpectraModelRandomized <- reactive(label="svmBoundrangeSpectraModel", {
            req(input$radiocal, input$calcurveelement)
            data <- svmBoundrangeSpectraModelSet()$data[randomizeData(),]
            parameters <- svmBoundrangeSpectraModelSet()$parameters$CalTable
            
            
            svmc.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmC), "-")))
            svmlength.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmLength), "-")))
            
            svm.grid <- expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1),
            length=seq(svmlength.vec[1], svmlength.vec[2], 1))
            
            
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            x_train <- matrix(data[,-1], ncol=1)
            colnames(x_train) <- "data"
            y_train <- data$Concentration
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                svm_model <- caret::train(x_train, y_train, method="svmBoundrangeString", trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                svm_model <- caret::train(x_train, y_train, method="svmBoundrangeString", trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
                stopCluster(cl)
            }
            svm_model
            
        })
        
        svmExponentialSpectraModelRandomized <- reactive(label="svmExponentialSpectraModel", {
            req(input$radiocal, input$calcurveelement)
            data <- svmExponentialSpectraModelSet()$data[randomizeData(),]
            parameters <- svmExponentialSpectraModelSet()$parameters$CalTable
            
            
            svmc.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmC), "-")))
            xgblambda.vec <- as.numeric(unlist(strsplit(as.character(parameters$xgbLambda), "-")))
            
            svm.grid <- expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1),
            lambda=seq(xgblambda.vec[1], xgblambda.vec[2], 1))
            
            
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            x_train <- matrix(data[,-1], ncol=1)
            colnames(x_train) <- "data"
            y_train <- data$Concentration
            
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                svm_model <- caret::train(x_train, y_train, method="svmExpoString", type="Regression", trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                svm_model <- caret::train(x_train, y_train, method="svmExpoString", type="Regression", trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
                stopCluster(cl)
            }
            svm_model
            
        })
        
        svmSpectrumSpectraModelRandomized <- reactive(label="svmSpectrumSpectraModel", {
            req(input$radiocal, input$calcurveelement)
            data <- svmSpectrumSpectraModelSet()$data[randomizeData(),]
            parameters <- svmSpectrumSpectraModelSet()$parameters$CalTable
            
            
            svmc.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmC), "-")))
            svmlength.vec <- as.numeric(unlist(strsplit(as.character(parameters$svmLength), "-")))
            
            svm.grid <- expand.grid(
            C = seq(svmc.vec[1], svmc.vec[2], 1),
            length=seq(svmlength.vec[1], svmlength.vec[2], 1))
            
            
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
                verboseIter = TRUE)
            } else if(parameters$ForestTC=="repeatedcv"){
                caret::trainControl(
                method = parameters$ForestTC,
                number = parameters$ForestNumber,
                repeats=parameters$CVRepeats,
                verboseIter = TRUE)
            }
            
            x_train <- matrix(data[,-1], ncol=1)
            colnames(x_train) <- "data"
            y_train <- data$Concentration
            
            
            cores.to.use <- if(parameters$ForestTC=="repeatedcv"){
                if(parameters$ForestNumber*parameters$CVRepeats >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber*parameters$CVRepeats < as.numeric(my.cores)){
                    parameters$ForestNumber*parameters$CVRepeats
                }
            } else if(parameters$ForestTC!="repeatedcv"){
                if(parameters$ForestNumber >= as.numeric(my.cores)){
                    as.numeric(my.cores)
                } else  if(parameters$ForestNumber < as.numeric(my.cores)){
                    parameters$ForestNumber
                }
            }
            
            
            if(input$multicore_behavior=="Single Core"){
                svm_model <- caret::train(x_train, y_train, method="svmSpectrumString", trControl=tune_control, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
            } else if(input$multicore_behavior=="Fork" | input$multicore_behavior=="Serialize"){
                cl <- if(input$multicore_behavior=="Serialize"){
                    parallel::makePSOCKcluster(as.numeric(cores.to.use))
                } else if(input$multicore_behavior=="Fork"){
                    parallel::makeForkCluster(as.numeric(cores.to.use))
                }
                clusterEvalQ(cl, library(foreach))
                registerDoParallel(cl)
                
                svm_model <- caret::train(x_train, y_train, method="svmSpectrumString", trControl=tune_control, allowParallel=TRUE, metric=parameters$ForestMetric, tuneGrid=svm.grid, na.action=na.omit)
                stopCluster(cl)
            }
            svm_model
            
        })
        
        svmSpectraModelRandomized <- reactive(label="svmSpectraModel", {
            
            if(xgboosthold$xgbtype=="Linear"){
                svmLinearSpectraModelRandomized()
            } else if(xgboosthold$xgbtype=="Polynomial"){
                svmPolySpectraModelRandomized()
            } else if(xgboosthold$xgbtype=="Exponential"){
                svmExponentialSpectraModelRandomized()
            } else if(xgboosthold$xgbtype=="Radial" | xgboosthold$xgbtype=="Radial Cost" | xgboosthold$xgbtype=="Radial Sigma"){
                svmRadialSpectraModelRandomized()
            } else if(xgboosthold$xgbtype=="Boundrange String"){
                svmBoundrangeSpectraModelRandomized()
            } else if(xgboosthold$xgbtype=="Spectrum String"){
                svmSpectrumSpectraModelRandomized()
            }
            
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
            } else if(input$radiocal==10){
                bayesIntensityModelRandomized()
            } else if(input$radiocal==11){
                bayesSpectraModelRandomized()
            } else if(input$radiocal==12){
                svmIntensityModelRandomized()
            } else if(input$radiocal==13){
                svmSpectraModelRandomized()
            }
            
        })
        
        #randomHold <- reactiveValues()
        #randomHold$calList <- list()
        
        #observeEvent(input$createcalelement, priority=100, {
        #    randomHold$calList[[input$calcurveelement]] <- NULL
        #    randomHold$calList[[input$calcurveelement]] <- isolate(list(Parameters=modelParameters(), Model=elementModelRandom()))
        #})
        
        
        valFrameRandomized <- reactive(label="valFrameRandomized",{
            
            predict.intensity <- predictIntensity()[ vals$keeprows, , drop = FALSE]
            predict.frame <- predictFrame()[ vals$keeprows, , drop = FALSE]
            predict.frame.cal <- predict.frame[(randomizeData()), , drop = FALSE]
            predict.frame <- predict.frame[!(randomizeData()), , drop = FALSE]
            predict.frame <- subset(predict.frame, predict.frame$Concentration > my.min(predict.frame.cal[,"Concentration"]) & predict.frame$Concentration  < my.max(predict.frame.cal[,"Concentration"]))
            predict.intensity <- predict.frame[,!colnames(predict.frame) %in% c("Spectrum", "Concentration")]
            
            element.model <-  elementModelRandom()
            
            
            
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
            
            if (input$radiocal==10){
                
                cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity)
                #cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
                #cal.est.conc.luc <- cal.est.conc.tab$fit
                #cal.est.conc.luc.up <- cal.est.conc.tab$upr
                #cal.est.conc.luc.low <- cal.est.conc.tab$lwr
                
                
                val.frame <- data.frame(predict.frame$Concentration, as.vector(cal.est.conc.pred.luc), as.vector(cal.est.conc.pred.luc))
                colnames(val.frame) <- c("Concentration", "Intensity", "Prediction")
            }
            
            if (input$radiocal==11){
                
                cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity)
                #cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
                #cal.est.conc.luc <- cal.est.conc.tab$fit
                #cal.est.conc.luc.up <- cal.est.conc.tab$upr
                #cal.est.conc.luc.low <- cal.est.conc.tab$lwr
                
                
                val.frame <- data.frame(predict.frame$Concentration, as.vector(cal.est.conc.pred.luc), as.vector(cal.est.conc.pred.luc))
                colnames(val.frame) <- c("Concentration", "Intensity", "Prediction")
            }
            
            if (input$radiocal==12){
                
                cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity)
                #cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
                #cal.est.conc.luc <- cal.est.conc.tab$fit
                #cal.est.conc.luc.up <- cal.est.conc.tab$upr
                #cal.est.conc.luc.low <- cal.est.conc.tab$lwr
                
                
                val.frame <- data.frame(predict.frame$Concentration, as.vector(cal.est.conc.pred.luc), as.vector(cal.est.conc.pred.luc))
                colnames(val.frame) <- c("Concentration", "Intensity", "Prediction")
            }
            
            if (input$radiocal==13){
                
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
            element.model <-  elementModelRandom()
            
            
            
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
            
            if (input$radiocal==10){
                cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity, na.action=na.omit)
                
                val.frame <- data.frame(na.omit(predict.frame)$Concentration, as.vector(cal.est.conc.pred.luc), as.vector(cal.est.conc.pred.luc))
                colnames(val.frame) <- c("Concentration", "Intensity", "Prediction")
            }
            
            if (input$radiocal==11){
                cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity, na.action=na.omit)
                
                val.frame <- data.frame(na.omit(predict.frame)$Concentration, as.vector(cal.est.conc.pred.luc), as.vector(cal.est.conc.pred.luc))
                colnames(val.frame) <- c("Concentration", "Intensity", "Prediction")
            }
            
            if (input$radiocal==12){
                cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity, na.action=na.omit)
                
                val.frame <- data.frame(na.omit(predict.frame)$Concentration, as.vector(cal.est.conc.pred.luc), as.vector(cal.est.conc.pred.luc))
                colnames(val.frame) <- c("Concentration", "Intensity", "Prediction")
            }
            
            if (input$radiocal==13){
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
                geom_smooth() +
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
            
            if(input$radiocal==10){
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
            
            if(input$radiocal==11){
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
            
            if(input$radiocal==12){
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
            
            if(input$radiocal==13){
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
            modelParameters()$CalTable
            
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
            
            
            model <- elementModelGen()

            
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
            } else if(input$radiocal==10){
                forestLM()
            } else if(input$radiocal==11){
                forestLM()
            } else if(input$radiocal==12){
                forestLM()
            } else if(input$radiocal==13){
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
            if(input$modelcompress==TRUE){
                calBundle(filetype=input$filetype, units=input$unit, spectra=spectraExport(), intensities=spectraLineTableExport(), definitions=linevalues[["DF"]], values=values[["DF"]], notes=input$notes, calList=calListCompress(calMemory$Calibration$calList))
            } else if(input$modelcompress==FALSE){
                calBundle(filetype=input$filetype, units=input$unit, spectra=spectraExport(), intensities=spectraLineTableExport(), definitions=linevalues[["DF"]], values=values[["DF"]], notes=input$notes, calList=calMemory$Calibration$calList)
            }
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
            
            cal.list <- lapply(inFile$datapath, calRDS)
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
            clusterEvalQ(cl, library(foreach))
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
            clusterEvalQ(cl, library(foreach))
            registerDoParallel(cl)
            cal.lm <- lapply(quantNames(),function(x) caret::train(Concentration~., data=rainforestDataMulti()[[x]][vals_multi$keeprows[[x]],, drop=FALSE], method="rf", type="Regression",
            trControl=trainControl(method=input$foresttrain_multi, number=input$forestnumber_multi), ntree=input$foresttrees_multi,
            prox=TRUE,allowParallel=TRUE, metric=input$forestmetric_multi, na.action=na.omit, importance=TRUE, trim=TRUE))
            stopCluster(cl)
            names(cal.lm) <- quantNames()
            cal.lm
            
            
        })
        
        
        neuralNetworkIntensitShallowyModelMulti <- reactive({
            
            #randomForest(Concentration~., data=predictFrameForest()[vals$keeprows,, drop=FALSE], na.action=na.omit, ntree=1000, nPerm=100)
            
            weightdecay.vec <- as.numeric(unlist(strsplit(as.character(input$neuralweightdecay_multi), "-")))
            hiddenunits.vec <- as.numeric(unlist(strsplit(as.character(input$neuralhiddenunits_multi), "-")))
            
            nn.grid <- expand.grid(
            .decay = seq(weightdecay.vec[1], weightdecay.vec[2], 0.1),
            .size = seq(hiddenunits.vec[1], hiddenunits.vec[2], 1))
            
            metricModel <- if(input$ForestMetric=="RMSE" | parameters$ForestMetric=="Rsquared"){
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
            clusterEvalQ(cl, library(foreach))
            registerDoParallel(cl)
            cal.lm <- lapply(quantNames(), function(x) caret::train(Concentration~., data=predictFrameForestMulti()[[x]][vals_multi$keeprows[[x]],, drop=FALSE], method="rf", type="Regression",
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
            clusterEvalQ(cl, library(foreach))
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
                #annotate("text", label=lm_eqn(lm(Concentration~Intensity, predict.frame[ unlist(unlist(vals_multi$keeprows), use.names=FALSE), , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
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
                #annotate("text", label=lm_eqn_poly(lm(Concentration~Intensity + I(Intensity^2), predict.frame[ unlist(vals_multi$keeprows), , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
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
                #annotate("text", label=lm_eqn(lm(Concentration~., val.frame[ unlist(vals_multi$keeprows), , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
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
                #annotate("text", label=lm_eqn(lm(Concentration~., val.frame[ unlist(vals_multi$keeprows), , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
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
                #annotate("text", label=lm_eqn(lm(Concentration~., val.frame[ unlist(vals_multi$keeprows), , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
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
            #annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame[ unlist(vals_multi$keeprows), , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
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
                clusterEvalQ(cl, library(foreach))
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
                clusterEvalQ(cl, library(foreach))
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
                #annotate("text", label=lm_eqn(element.model), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                stat_smooth(method="lm", fullrange = TRUE, aes(fill=Instrument), alpha=0.1) +
                geom_point() +
                scale_x_continuous(paste(element.name, intens), breaks=scales::pretty_breaks()) +
                scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
                coord_cartesian(xlim = rangescalcurverandom_multi$x, ylim = rangescalcurverandom_multi$y, expand = TRUE)
                
            }
            
            if(input$radiocal_multi==2){
                
                calcurve.plot <- ggplot(data=predict.frame, aes(Intensity, Concentration, colour=Instrument, shape=Instrument)) +
                theme_light() +
                #annotate("text", label=lm_eqn_poly(element.model), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
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
                #annotate("text", label=lm_eqn(element.model), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
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
                #annotate("text", label=lm_eqn(lm(Concentration~., val.frame)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
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
                #annotate("text", label=lm_eqn(lm(Concentration~., val.frame)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
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
            #annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
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
            calListMulti[[x]][["calList"]][[input$calcurveelement_multi]] <<- list(isolate(calConditionsMulti), isolate(elementModelMulti()[[x]])))
            
            
            
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
        } else if(input$valfiletype=="Aggregate CSV File") {
            fileInput('loadvaldata', 'Choose CSV', multiple=FALSE,
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
            selectInput("valfiletype", label="Filetype", c("CSV", "Aggregate CSV File", "TXT", "Net", "Elio", "MCA", "SPX", "PDZ"), selected="CSV")
        } else if(!is.null(input$calfileinput2)){
            selectInput("valfiletype", label="Filetype", c("CSV", "Aggregate CSV File", "TXT", "Net", "Elio", "MCA", "SPX", "PDZ"), selected=calFileContents2()[["FileType"]])
        
        }
    
})
    
    calFileContents2 <- reactive({
        
        existingCalFile <- input$calfileinput2
        
        if (is.null(existingCalFile)) return(NULL)
        
        
        Calibration <- calRDS(existingCalFile$datapath, null.strip=TRUE)
        
        
        Calibration
        
    })
    
    

    
    
    observeEvent(input$processvalspectra, {
        
        fullValSpectra <- reactive({
            
            fullSpectraProcess(inFile=input$loadvaldata, gainshiftvalue=gainshiftHold())
                    
        })
        
        valImportedCSV <- reactive(label="importedCSV", {
            req(input$loadvaldata)
            
                inFile <- input$loadvaldata
                if (is.null(inFile)) return(NULL)
                
                importCSVFrame(filepath=inFile$datapath)
        })
        
        readValTXT <- reactive({
            
            readTXTProcess(inFile=input$loadvaldata, gainshiftvalue=gainshiftHold())

            
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
            
            readElioProcess(inFile=input$loadvaldata, gainshiftvalue=gainshiftHold())
            
            
        })
        
        readValMCA <- reactive({
            
            readMCAProcess(inFile=input$loadvaldata, gainshiftvalue=gainshiftHold())
            
        })
        
        readValSPX <- reactive({
            
            readSPXProcess(inFile=input$loadvaldata, gainshiftvalue=gainshiftHold())
            
        })
            
            
            readvalPDZ <- reactive({
                
            binaryshiftvalue <- tryCatch(binaryHold(), error=function(e) NULL)
                
            readPDZProcess(inFile=input$loadvaldata, gainshiftvalue=gainshiftHold(), advanced=input$advanced, binaryshift=binaryshiftvalue)
                
                
            })
            
            

        
        
        myValData <- reactive({
            
            data <- if(input$valfiletype=="CSV"){
                fullValSpectra()
            } else if(input$valfiletype=="Aggregate CSV File"){
                valImportedCSV()
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
            

                calFileContents2()$Intensities[!colnames(calFileContents2()$Intensities) %in% "Spectrum"]

            
            
        })
        
        calValElements <- reactive({
            calList <- calValHold()
            valelements <- names(calList)
            
            #valelements.simp <- gsub(".K.alpha", "", valelements)
            #valelements.simp <- gsub(".K.beta", "", valelements.simp)
            #valelements.simp <- gsub(".L.alpha", "", valelements.simp)
            #valelements.simp <- gsub(".L.beta", "", valelements.simp)
            #valelements.simp <- gsub(".M.line", "", valelements.simp)

            
            #valelements <- as.vector(as.character(valelements[match(as.character(fluorescence.lines$Symbol), valelements.simp)]))
            
            #valelements <- c(valelements, names(calList)[!(names(calList) %in% valelements)])

            
            order_elements(valelements)
        })
        
        calVariableElements <- reactive({
            variables <- calVariables()
            variableelements <- names(variables)
            
            #variableelements.simp <- gsub(".K.alpha", "", variableelements)
            #variableelements.simp <- gsub(".K.beta", "", variableelements)
            #variableelements.simp <- gsub(".L.alpha", "", variableelements)
            #variableelements.simp <- gsub(".L.beta", "", variableelements)
            #variableelements.simp <- gsub(".M.line", "", variableelements)
            
            #variableelements <- as.vector(as.character(na.omit(variableelements[match(as.character(fluorescence.lines$Symbol), variableelements.simp)])))

            order_elements(variableelements)
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
            } else if(input$valfiletype=="Aggregate CSV File"){
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
            } else if(input$valfiletype=="Spectra") {
                "Spectra"
            }
            
        })
        
        
  
  
        
        
        tableInputValCounts <- reactive({
            valelements <- calValElements()
            variableelements <- calVariableElements()
            val.data <- myValData()
            
            if(valDataType()=="Spectra"){spectra.line.list <- pblapply(cl=as.numeric(my.cores)/2, X=valelements, function(x) elementGrab(element.line=x, data=val.data, range.table=calDefinitions()))}
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
        
        lineTypePreference <- reactive({
            
             calFileContents2()[["LinePreference"]]
            
        })
        

        
        
        fullInputValCounts <- reactive({
            valelements <- calValElements()
            variableelements <- calVariableElements()
            val.data <- myValData()
            
            if(valDataType()=="Spectra"){spectra.line.list <- lapply(variableelements, function(x) elementGrab(element.line=x, data=val.data, range.table=calDefinitions()))}
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
        
        fullInputValCountsWide <- reactive({
            valelements <- calValElements()
            variableelements <- calVariableElements()
            val.data <- myValData()
            
            if(valDataType()=="Spectra"){spectra.line.list <- lapply(variableelements, function(x) wideElementGrab(element.line=x, data=val.data, range.table=calDefinitions()))}
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
        
        output$myvaltablewide <- renderDataTable({
            
            fullInputValCountsWide()
            
        })
        
        
        countList <- reactive({
             list(Narrow=fullInputValCounts(), Wide=fullInputValCountsWide())
        })
        
        
        tableInputValQuant <- reactive({
            suppressWarnings({
                cloudCalPredict(Calibration=calFileContents2(), count.list=countList(), elements.cal=calValElements(), variables=calVariableElements(), valdata=myValData(), rounding=input$resultrounding, multiplier=input$multiplier)
            })
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
