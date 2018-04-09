library(shiny)
library(ggplot2)
library(pbapply)
library(reshape2)
library(dplyr)
library(data.table)
library(DT)
library(gridExtra)
library(rhandsontable)
library(Cairo)
library(broom)
library(shinyjs)
library(formattable)
library(markdown)
library(rmarkdown)
library(XML)
library(corrplot)
library(scales)


options(shiny.maxRequestSize=9000000*1024^2)

options(warn=-1)
assign("last.warning", NULL, envir = baseenv())

shinyServer(function(input, output, session) {
    
    output$filegrab <- renderUI({
        
        if(input$filetype=="Spectra") {
            fileInput('file1', 'Choose CSV', multiple=TRUE,
            accept=c(".csv"))
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
    
    
    output$gainshiftui <- renderUI({
        
        if(input$advanced==TRUE){
           numericInput('gainshift', "Gain Shift (keV)", min=-1, max=1, value=0)
        } else {
            p()
        }
        
    })
    
    gainshiftHold <- reactive({
        
        if(input$advanced==TRUE){
            input$gainshift
        } else if(input$advanced==FALSE){
            0
        }
        
    })
    
    
    fullSpectra <- reactive({
        
        
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
    
    
    netCounts <- reactive({
        
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
    
    readElio <- reactive({
        
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
    
    
    readMCA <- reactive({
        
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
    
    
    readSPX <- reactive({
        
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
    
    
    readPDZ <- reactive({
        
        withProgress(message = 'Processing Data', value = 0, {
            
            inFile <- input$file1
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
   
    
    
    observeEvent(input$actionprocess, {
        

        myData <- reactive({
            
            data <- if(input$filetype=="Spectra"){
                fullSpectra()
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
        
        
calFileContents <- reactive({
            
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

dataHold <- reactive({
    data <- if(input$usecalfile==FALSE){
        myData()
    } else if(input$usecalfile==TRUE && input$usespectravalues==FALSE){
        calFileContents()$Spectra
    } else if (input$usecalfile==TRUE && input$usespectravalues==TRUE){
        myData()
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


dataCount <- reactive({
    inFile <- input$file1
    
    if(input$usecalfile==FALSE){
        length(inFile$datapath)
    }else if(input$usecalfile==TRUE && input$usespectravalues==FALSE){
        length(calFileContents()$Intensities)
    }else if(input$usecalfile==TRUE && input$usespectravalues==TRUE){
        length(inFile$datapath)
    }
})



        
        # Return the requested dataset
        datasetInput <- reactive({
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
        
        
        
         plotInput <- reactive({
             
             data <- dataHold()

             
             
             id.seq <- seq(1, 2048,1)
             
             n <- length(data$Energy)
             
             element <- datasetInput()
             intensity.norm <- (element$Intensity/max(element$Intensity))*max(data$CPS)
             intensity.base <- (element$Intensity/max(element$Intensity))
             
             
             
             spectral.plot <- qplot(data$Energy, data$CPS, xlab = "Energy (keV)", ylab = "Counts per Second", geom="line", colour=data$Spectrum) +
             theme_light()+
             theme(legend.position="bottom") +
             geom_segment(aes(x=element$Line, xend=element$Line, y = 0, yend=intensity.norm), colour="grey50", linetype=2)  +
             scale_colour_discrete("Spectrum") +
             coord_cartesian(xlim = ranges$x, ylim = ranges$y)
             
       

         })


        output$distPlot <- renderPlot({

print(plotInput())


        })
        
        # When a double-click happens, check if there's a brush on the plot.
        # If so, zoom to the brush bounds; if not, reset the zoom.
        observeEvent(input$plot1_dblclick, {
            data <- dataHold()
            brush <- input$plot1_brush
            if (!is.null(brush)) {
                ranges$x <- c(brush$xmin*mean(data$Energy), brush$xmax*max(data$Energy))
                ranges$y <- c(brush$ymin*mean(data$CPS), brush$ymax*max(data$CPS))
                
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
    
    spectra.line.table <- dataHold()
    
    
    if(input$usecalfile==FALSE && input$filetype=="Spectra"){
        standard
    } else if(input$usecalfile==FALSE && input$filetype=="Elio"){
        standard
    }  else if(input$usecalfile==FALSE && input$filetype=="MCA"){
        standard
    }  else if(input$usecalfile==FALSE && input$filetype=="SPX"){
        standard
    }  else if(input$usecalfile==FALSE && input$filetype=="PDZ"){
        standard
    } else if(input$usecalfile==FALSE && input$filetype=="Net"){
        colnames(spectra.line.table[2:4])
    } else if(input$usecalfile==TRUE){
        ls(calFileContents()$Intensities)
    }
    
})

standardLines <- reactive({
    
    spectra.line.table <- dataHold()
    
    n <- length(names(spectra.line.table))
    
    
    choices <- if(input$filetype=="Spectra"){
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

output$checkboxElements <-  renderUI({
    
    checkboxGroupInput("show_vars", label="Elemental lines to show:",
    choices = standardLines(), selected = standardElements())
    
})

output$checkboxElementsKalpha <-  renderUI({
    
    selectInput("show_vars_k_alpha", label="K-alpha",
    choices = kalphaLines, selected = c("Ca.K.alpha", "Ti.K.alpha", "Fe.K.alpha"), multiple=TRUE)
    
})

output$checkboxElementsKbeta <-  renderUI({
    
    selectInput("show_vars_k_beta", label="K-beta",
    choices = kbetaLines, selected = NULL, multiple=TRUE)
    
})

output$checkboxElementsLalpha <-  renderUI({
    
    selectInput("show_vars_l_alpha", label="L-alpha",
    choices = lalphaLines, selected = "Rh.L.alpha", multiple=TRUE)
    
})

output$checkboxElementsLbeta <-  renderUI({
    
    selectInput("show_vars_l_beta", label="L-beta",
    choices = lbetaLines, selected = NULL, multiple=TRUE)
    
})

output$checkboxElementsM <-  renderUI({
    
    selectInput("show_vars_m", label="M",
    choices = mLines, selected = "Pb.M.line", multiple=TRUE)
    
})

elementallinestouse <- reactive({
    
    #c(input$show_vars_k_alpha, input$show_vars_k_beta, input$show_vars_l_alpha, input$show_vars_l_beta, input$show_vars_m)
    
    input$show_vars
    
    
})





 
 spectraData <- reactive({
     
     data <- dataHold()
     
     elements <- elementallinestouse()





     spectra.line.list <- lapply(elements, function(x) elementGrab(element.line=x, data=data))
     element.count.list <- lapply(spectra.line.list, '[', 2)

     spectra.line.vector <- as.numeric(unlist(element.count.list))
     
     dim(spectra.line.vector) <- c(length(spectra.line.list[[1]]$Spectrum), length(elements))
     
     spectra.line.frame <- data.frame(spectra.line.list[[1]]$Spectrum, spectra.line.vector)
     
     colnames(spectra.line.frame) <- c("Spectrum", elements)
     
     spectra.line.frame <- as.data.frame(spectra.line.frame)
     
     spectra.line.frame <- spectra.line.frame[order(as.character(spectra.line.frame$Spectrum)),]

     spectra.line.frame$Spectrum <- gsub(".pdz", "", spectra.line.frame$Spectrum)
     spectra.line.frame$Spectrum <- gsub(".csv", "", spectra.line.frame$Spectrum)
     spectra.line.frame$Spectrum <- gsub(".CSV", "", spectra.line.frame$Spectrum)
     spectra.line.frame$Spectrum <- gsub(".spt", "", spectra.line.frame$Spectrum)
     spectra.line.frame$Spectrum <- gsub(".mca", "", spectra.line.frame$Spectrum)
     spectra.line.frame$Spectrum <- gsub(".spx", "", spectra.line.frame$Spectrum)


     spectra.line.frame
     
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
 
 
 
 

 
 
 
 tableInput <- reactive({
     
     elements <- elementallinestouse()


     select.line.table <- if(input$filetype=="Spectra"){
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
     
     rounded <- round(select.line.table[elements], digits=0)
     full <- data.frame(select.line.table$Spectrum, rounded)
     colnames(full) <- c("Spectrum", elements)
     
     full
 })


  output$mytable1 <- renderDataTable({
   
  tableInput()

  })
  
  output$covarianceplot <- renderPlot({
      
      data.table <- tableInput()
      correlations <- cor(data.table[,-1])
      corrplot(correlations, method="circle")
      
  })
  

  
  
  output$downloadData <- downloadHandler(
  filename = function() { paste(input$dataset, '.csv', sep=',') },
  content = function(file
  ) {
      write.csv(spectraData(), file)
  }
  )
  
  observeEvent(input$hotableprocess1, {
  })
  
  

hotableInputBlank <- reactive({
    
    elements <- elementallinestouse()




spectra.line.table <- if(input$filetype=="Spectra"){
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
    dataHold()
}

        empty.line.table <- spectra.line.table[,elements] * 0.0000

    #empty.line.table$Spectrum <- spectra.line.table$Spectrum
    
    hold.frame <- data.frame(spectra.line.table$Spectrum, empty.line.table)
    colnames(hold.frame) <- c("Spectrum", elements)
    
    hold.frame <- as.data.frame(hold.frame)
    
  
  
  hold.frame


})

hotableInputCal <- reactive({
    
    elements <- elementallinestouse()
    
    
    
    
    spectra.line.table <- if(input$filetype=="Spectra"){
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
        dataHold()
    }
    
    empty.line.table <- spectra.line.table[,elements] * 0.0000
    
    #empty.line.table$Spectrum <- spectra.line.table$Spectrum
    
    hold.frame <- data.frame(spectra.line.table$Spectrum, empty.line.table)
    colnames(hold.frame) <- c("Spectrum", elements)
    
    hold.frame <- as.data.frame(hold.frame)


    value.frame <- calFileContents()$Values
    
    #anna <- rbind(hold.frame, value.frame)
    
    #temp.table <- data.table(anna)[,list(result = sum(result)), elements]
    
    #as.data.frame(temp.table)
    
    #element.matches <- elements[elements %in% ls(value.frame)]
    
    #merge_Sum(.df1=hold.frame, .df2=value.frame, .id_Columns="Spectrum",  .match_Columns=element.matches)
    
    
    #data.frame(calFileContents()$Values, hold.frame[,! names(hold.frame) %in% names(calFileContents()$Values)])
    
    hold.frame.reduced <- hold.frame[2:length(hold.frame)]
    value.frame.reduced <- if(colnames(calFileContents()$Values)[1]=="Spectrum"){
        value.frame[2:length(value.frame)]
    }else if(colnames(calFileContents()$Values)[1]=="Include"){
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
    
    
    hotable.data <- if(input$usecalfile==FALSE){
        hotableInputBlank()
    }else if(input$usecalfile==TRUE){
        hotableInputCal()
    }
    
    

hotable.new <- if(input$usecalfile==FALSE){
    data.frame(Include=rep(TRUE, length(hotable.data$Spectrum)), hotable.data)
}else if(input$usecalfile==TRUE && colnames(calFileContents()$Values)[1]=="Spectrum"){
    data.frame(Include=rep(TRUE, length(hotable.data$Spectrum)), hotable.data)
}else if(input$usecalfile==TRUE && colnames(calFileContents()$Values)[1]=="Include"){
    data.frame(Include=calFileContents()$Values[,1], hotable.data)
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

eventReactive(input$linecommit,{
    
    values[["DF"]] <- hotableInput()
    
})


## Handsontable

output$hot <- renderRHandsontable({
    
    DF <- values[["DF"]]
    
    DF <- DF[order(as.character(DF$Spectrum)),]
    
    
    
    if (!is.null(DF))
    rhandsontable(DF) %>% hot_col(2:length(DF), renderer=htmlwidgets::JS("safeHtmlRenderer"))
    
    
})


observeEvent(input$resethotable, {
    
   values[["DF"]] <- NULL
   
   values[["DF"]] <- hotableInput()

    
})

output$covarianceplotvalues <- renderPlot({
    
    data.table <- values[["DF"]]
    correlations <- cor(data.table[,3:length(data.table)], use="pairwise.complete.obs")
    corrplot(correlations, method="circle")
    
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


outVar <- reactive({
    input$hotableprocess2

    myelements <- elementallinestouse()

    result <- if(is.null(myelements)){
        "Ca.K.alpha"
    }else{
        myelements
    }
    
    result


    })

outVaralt <- reactive({
    input$hotableprocess2
    
    
    myelements <- c(elementallinestouse())

    
    if(is.null(myelements)){
        paste("Ca.K.alpha")
    }else{
        myelements
    }
    
})

outVaralt2 <- reactive({
    input$hotableprocess2
    
    
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

inVar3Selected <- reactive({
    
    hold <- values[["DF"]]
    
    optionhold <- if(is.null(input$calcurveelement)){
        ls(hold)[2]
    }else{
        input$calcurveelement
    }
    
    if(input$usecalfile==FALSE && is.null(calList[[optionhold]])==TRUE){
        optionhold
    } else if(input$usecalfile==TRUE && is.null(calList[[optionhold]])==TRUE  && is.null(calFileContents()$calList[[optionhold]])==FALSE){
        calFileContents()$calList[[optionhold]][[1]]$Intercept
    } else if(input$usecalfile==FALSE && is.null(calList[[optionhold]])==FALSE){
        calList[[optionhold]][[1]]$Intercept
    } else if(input$usecalfile==TRUE && is.null(calList[[optionhold]])==FALSE){
        calList[[optionhold]][[1]]$Intercept
    } else if(input$usecalfile==TRUE && is.null(calList[[optionhold]])==TRUE && is.null(calFileContents()$calList[[optionhold]])==TRUE){
        optionhold
    } 
    
    
})


output$inVar3 <- renderUI({
    
    selectInput(inputId = "intercept_vars", label = h4("Intercept"), choices =  outVaralt2(), selected=inVar3Selected(), multiple=TRUE)
})

inVar4Selectedpre <- reactive({
    
    hold <- values[["DF"]]
    
    optionhold <- if(is.null(input$calcurveelement)){
        ls(hold)[2]
    }else{
        input$calcurveelement
    }
    
    
    if(input$usecalfile==FALSE && is.null(calList[[optionhold]])==TRUE){
        optionhold
    } else if(input$usecalfile==TRUE && is.null(calList[[optionhold]])==TRUE && is.null(calFileContents()$calList[[optionhold]])==FALSE){
        calFileContents()$calList[[optionhold]][[1]]$Slope
    } else if(input$usecalfile==FALSE && is.null(calList[[optionhold]])==FALSE){
        calList[[optionhold]][[1]]$Slope
    } else if(input$usecalfile==TRUE && is.null(calList[[optionhold]])==FALSE){
        calList[[optionhold]][[1]]$Slope
    }  else if(input$usecalfile==TRUE && is.null(calList[[optionhold]])==TRUE && is.null(calFileContents()$calList[[optionhold]])==TRUE){
        optionhold
    }
})





output$nvariablesui <- renderUI({
    
    if(input$trainslopes==TRUE){
        numericInput("nvariables", label = "# Elements", min=2, max=length(outVaralt()), value=3)
    } else if(input$trainslopes==FALSE){
        p()
    }
    
})

fishVector <- reactive({
    
    combos_mod <- function(a.vector){
        
        so <- seq(from=2, to=input$nvariables, by=1)
        
        long <- pblapply(so, function(x) gRbase::combnPrim(x=a.vector, m=x), cl=6L)
        and <- pblapply(long, function(x) plyr::alply(x, 2), cl=6L)
        thanks.for.all.the.fish <- do.call(list, unlist(and, recursive=FALSE))
        
        thanks.for.all.the.fish
        
    }
    
    
    
    combos_mod(ls(spectraLineTable()[ ,!colnames(spectraLineTable())==input$calcurveelement]))

})


bestSlopeVars <- reactive({
    
    choices <- outVaralt()
    
    
    optimal_r_chain(element=input$calcurveelement, intensities=spectraLineTable(), values= as.data.frame(values[["DF"]]), possible.slopes=fishVector())
    
})


inVar4Selected <- reactive({
    
    if(input$trainslopes==FALSE){
        inVar4Selectedpre()
    } else if(input$trainslopes==TRUE){
        bestSlopeVars()
    }
    
    
})


output$inVar4 <- renderUI({
    selectInput(inputId = "slope_vars", label = h4("Slope"), choices =  outVaralt(), selected=inVar4Selected(), multiple=TRUE)
})




calConditons <- reactiveValues()
calList <- reactiveValues()
calList <- NULL

observeEvent(input$hotableprocess2, {
    
    cal.condition <- 3
    norm.condition <- 1
    
    norm.min <- 18.5
    norm.max <- 19.5
    
    cal.table <- data.frame(cal.condition, norm.condition, norm.min, norm.max)
    colnames(cal.table) <- c("CalType", "NormType", "Min", "Max")
    
    slope.corrections <- input$slope_vars
    intercept.corrections <- input$intercept_vars
    
    standards.used <- vals$keeprows
    
    cal.mode.list <- list(cal.table, slope.corrections, intercept.corrections, standards.used)
    names(cal.mode.list) <- c("CalTable", "Slope", "Intercept", "StandardsUsed")
    
    calConditons <<- cal.mode.list
    
})



calTypeSelection <- reactive({
    
    hold <- values[["DF"]]
    
    optionhold <- if(is.null(input$calcurveelement)){
        ls(hold)[2]
    }else{
        input$calcurveelement
    }
    
    if(input$usecalfile==FALSE && is.null(calList[[optionhold]])==TRUE){
        calConditons[[1]][[1]]
    } else if(input$usecalfile==TRUE && is.null(calList[[optionhold]])==TRUE && is.null(calFileContents()$calList[[optionhold]])==FALSE){
        calFileContents()$calList[[optionhold]][[1]]$CalTable$CalType
    } else if(input$usecalfile==FALSE && is.null(calList[[optionhold]])==FALSE){
        calList[[optionhold]][[1]]$CalTable$CalType
    } else if(input$usecalfile==TRUE && is.null(calList[[optionhold]])==FALSE){
        calList[[optionhold]][[1]]$CalTable$CalType
    } else if(input$usecalfile==TRUE && is.null(calList[[optionhold]])==TRUE && is.null(calFileContents()$calList[[optionhold]])==TRUE){
        calConditons[[1]][[1]]
    }
    
})

calNormSelection <- reactive({
    
    hold <- values[["DF"]]
    
    optionhold <- if(is.null(input$calcurveelement)){
        ls(hold)[2]
    }else{
        input$calcurveelement
    }
    
    if(input$usecalfile==FALSE && is.null(calList[[optionhold]])==TRUE){
        calConditons[[1]][[2]]
    }else if(input$usecalfile==TRUE && is.null(calList[[optionhold]])==TRUE && is.null(calFileContents()$calList[[optionhold]])==FALSE){
        calFileContents()$calList[[optionhold]][[1]]$CalTable$NormType
    } else if(input$usecalfile==FALSE && is.null(calList[[optionhold]])==FALSE){
        calList[[optionhold]][[1]]$CalTable$NormType
    } else if(input$usecalfile==TRUE && is.null(calList[[optionhold]])==FALSE){
        calList[[optionhold]][[1]]$CalTable$NormType
    } else if(input$usecalfile==TRUE && is.null(calList[[optionhold]])==TRUE && is.null(calFileContents()$calList[[optionhold]])==TRUE){
        calConditons[[1]][[2]]
    }
    
})

normMinSelection <- reactive({
    
    hold <- values[["DF"]]
    
    optionhold <- if(is.null(input$calcurveelement)){
        ls(hold)[2]
    }else{
        input$calcurveelement
    }
    
    if(input$usecalfile==FALSE && is.null(calList[[optionhold]])==TRUE){
        calConditons[[1]][[3]]
    }else if(input$usecalfile==TRUE && is.null(calFileContents()$calList[[optionhold]])==FALSE && is.null(calList[[optionhold]])==TRUE){
        calFileContents()$calList[[optionhold]][[1]]$CalTable$Min
    } else if(input$usecalfile==FALSE && is.null(calList[[optionhold]])==FALSE){
        calList[[optionhold]][[1]]$CalTable$Min
    } else if(input$usecalfile==TRUE && is.null(calList[[optionhold]])==FALSE){
        calList[[optionhold]][[1]]$CalTable$Min
    } else if(input$usecalfile==TRUE && is.null(calList[[optionhold]])==TRUE && is.null(calFileContents()$calList[[optionhold]])==TRUE){
        calConditons[[1]][[3]]
    }
    
})

normMaxSelection <- reactive({
    
    hold <- values[["DF"]]
    
    optionhold <- if(is.null(input$calcurveelement)){
        ls(hold)[2]
    }else{
        input$calcurveelement
    }
    
    if(input$usecalfile==FALSE && is.null(calList[[optionhold]])==TRUE){
        calConditons[[1]][[4]]
    }else if(input$usecalfile==TRUE && is.null(calList[[optionhold]])==TRUE && is.null(calFileContents()$calList[[optionhold]])==FALSE){
        calFileContents()$calList[[optionhold]][[1]]$CalTable$Max
    } else if(input$usecalfile==FALSE && is.null(calList[[optionhold]])==FALSE){
        calList[[optionhold]][[1]]$CalTable$Max
    } else if(input$usecalfile==TRUE && is.null(calList[[optionhold]])==FALSE){
        calList[[optionhold]][[1]]$CalTable$Max
    } else if(input$usecalfile==TRUE && is.null(calList[[optionhold]])==TRUE && is.null(calFileContents()$calList[[optionhold]])==TRUE){
        calConditons[[1]][[4]]
    }
})





output$calTypeInput <- renderUI({
    
    selectInput("radiocal", label = "Calibration Curve",
    choices = list("Linear" = 1, "Non-Linear" = 2, "Lucas-Tooth" = 3),
    selected = calTypeSelection())
    
    
})


output$normTypeInput <- renderUI({
    
    selectInput("normcal", label = "Normalization",
    choices = list("Time" = 1, "Total Counts" = 2, "Compton" = 3),
    selected = calNormSelection())
    
    
})


output$comptonMinInput <- renderUI({
    
    numericInput('comptonmin', label=h6("Min"), step=0.001, value=normMinSelection(), min=0, max=50, width='30%')
    
})

output$comptonMaxInput <- renderUI({
    
    numericInput('comptonmax', label=h6("Max"), step=0.001, value=normMaxSelection(), min=0, max=50, width='30%')
    
})




elementHold <- reactive({
    
    if(is.null(input$calcurveelement)==TRUE){
        ls(dataHold())[1]
    } else{
        input$calcurveelement
    }
    
})



  
  calFileStandards <- reactive({

          
  
     if(input$usecalfile==TRUE && is.null(calList[[elementHold()]])==TRUE && is.null(calFileContents()$calList[[elementHold()]])==FALSE){
          calFileContents()$calList[[elementHold()]][[1]][[4]]
      } else if(input$usecalfile==FALSE && is.null(calList[[elementHold()]])==TRUE && is.null(calFileContents()$calList[[elementHold()]])==TRUE){
          rep(TRUE, dataCount())
      } else if(input$usecalfile==TRUE && is.null(calList[[elementHold()]])==FALSE && is.null(calFileContents()$calList[[elementHold()]])==TRUE){
          calList[[elementHold()]][[1]][[4]]
      } else if(input$usecalfile==TRUE && is.null(calList[[elementHold()]])==FALSE && is.null(calFileContents()$calList[[elementHold()]])==FALSE){
          calList[[elementHold()]][[1]][[4]]
      } else if(input$usecalfile==FALSE && is.null(calList[[elementHold()]])==FALSE && is.null(calFileContents()$calList[[elementHold()]])==TRUE){
          calList[[elementHold()]][[1]][[4]]
      } else if(input$usecalfile==FALSE && is.null(calList[[elementHold()]])==FALSE && is.null(calFileContents()$calList[[elementHold()]])==FALSE){
          calList[[elementHold()]][[1]][[4]]
      } else if(input$usecalfile==TRUE && is.null(calList[[elementHold()]])==TRUE && is.null(calFileContents()$calList[[elementHold()]])==TRUE){
          rep(TRUE, dataCount())
      }
      
      
      
      
  })
  
  
  
  
  vals <- reactiveValues()
  

vals$keeprows <- if(input$usecalfile==TRUE){
    calFileStandards()
}else{
    rep(TRUE, dataCount())
}


  
  #if(input$hotableprocess2){vals$keeprows <- vals$keeprows[dropStandard()]}


output$temp <- renderTable({
    
    as.data.frame(vals$keeprows)
    
})


dataType <- reactive({
    if(input$filetype=="Spectra"){
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
          spectraData()[values[["DF"]]$Include,]
      }else if(dataType()=="Net"){
          dataHold()[values[["DF"]]$Include,]
      }
      
      
      spectra.line.table <- spectra.line.table[order(as.character(spectra.line.table$Spectrum)),]
      spectra.line.table

      
      
  })
  
  
  holdFrame <- reactive({
      
      spectra.line.table <- spectraLineTable()
      
      concentration <- as.vector(as.numeric(unlist(concentrationTable()[,input$calcurveelement])))
      
      
      
      intensity <- as.vector(as.numeric(unlist(spectraLineTable()[,input$calcurveelement])))
      
      spectra.names <- spectra.line.table$Spectrum
      
      hold.frame <- data.frame(spectra.names, concentration, intensity)
      colnames(hold.frame) <- c("Spectrum", "Concentration", "Intensity")
      hold.frame <- na.omit(hold.frame)
      
      hold.frame <- hold.frame[order(as.character(hold.frame$Spectrum)),]

      
      hold.frame
      
      
  })
  
  dataNorm <- reactive({
      
      data <- dataHold()
      data[data$Spectrum %in% holdFrame()$Spectrum, ]
      
      
  })
  
  
  predictFramePre <- reactive({
      
      
      concentration <- holdFrame()$Concentration
      intensity <- holdFrame()$Intensity
      
      predict.frame <- data.frame(concentration, intensity)
      colnames(predict.frame) <- c("Concentration", "Intensity")
      
      
      predict.frame
      
      
  })
  
  
  
  predictIntensity <- reactive({
      
      spectra.line.table <- spectraLineTable()
      data <- dataNorm()
      
      
      spectra.line.table <- spectraLineTable()[spectraLineTable()$Spectrum %in% holdFrame()$Spectrum, ]

      
      if (input$radiocal!=3){
          
          if(input$normcal==1){
              predict.intensity <- if(dataType()=="Spectra"){
                  general.prep(spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
              } else if(dataType()=="Net"){
                  general.prep.net(spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
              }
          }
          
          if(input$normcal==2){
              predict.intensity <- if(dataType()=="Spectra"){
                  simple.tc.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
              } else if(dataType()=="Net"){
                  simple.tc.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
              }
          }
          
          if(input$normcal==3){
              predict.intensity <- if(dataType()=="Spectra"){
                  simple.comp.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, norm.min=input$comptonmin, norm.max=input$comptonmax)
              } else if(dataType()=="Net"){
                  simple.comp.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, norm.min=input$comptonmin, norm.max=input$comptonmax)
              }
          }
          
      }
      
      
      if (input$radiocal==3){
          
          if(input$normcal==1){
              predict.intensity <- if(dataType()=="Spectra"){
                  lucas.simp.prep(spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
              } else if(dataType()=="Net"){
                  lucas.simp.prep.net(spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
              }
          }
          
          if(input$normcal==2){
              predict.intensity <- if(dataType()=="Spectra"){
                  lucas.tc.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
              } else if(dataType()=="Net"){
                  lucas.tc.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
              }
          }
          
          if(input$normcal==3){
              predict.intensity <- if(dataType()=="Spectra"){
                  lucas.comp.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
              } else if(dataType()=="Net"){
                  lucas.comp.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
              }
          }
          
      }
      
      
      predict.intensity
      
      
  })
  
  
  predictFrame <- reactive({
      
      predict.frame <- predictFramePre()
      predict.intensity <- predictIntensity()
      

      
      predict.frame <- data.frame(predict.intensity, predict.frame$Concentration)
      colnames(predict.frame) <- c(names(predict.intensity), "Concentration")
      
      
      
      predict.frame
      
      
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
  
  
  elementModel <- reactive({
      
      predict.frame <- predictFrame()
      
      
      if (input$radiocal==1){
          cal.lm <- lm(Concentration~Intensity, data=predict.frame[ vals$keeprows, , drop = FALSE])
      }
      
      
      if (input$radiocal==2){
          cal.lm <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame[ vals$keeprows, , drop = FALSE])
      }
      
      if (input$radiocal==3){
          cal.lm <- lm(Concentration~., data=predict.frame[ vals$keeprows, , drop = FALSE])
      }
      
      cal.lm
      
  })
  
  
  valFrame <- reactive({
      
      predict.intensity <- predictIntensity()
      predict.frame <- predictFrame()
      element.model <- elementModel()
      
      
      if (input$radiocal!=3){
          cal.est.conc.pred <- predict(object=element.model, newdata=predict.intensity, interval='confidence')
          cal.est.conc.tab <- data.frame(cal.est.conc.pred)
          cal.est.conc <- cal.est.conc.tab$fit
          
          val.frame <- data.frame(predict.frame$Concentration, cal.est.conc)
          colnames(val.frame) <- c("Concentration", "Prediction")
      }
      
      if (input$radiocal==3){
          
          xmin = 0; xmax=10
          N = length(predict.frame$Concentration)
          means = colMeans(predict.frame)
          dummyDF = t(as.data.frame(means))
          for(i in 2:N){dummyDF=rbind(dummyDF,means)}
          xv=seq(xmin,xmax, length.out=N)
          dummyDF$Concentration = xv
          yv=predict(element.model, newdata=predict.intensity)
          
          
          lucas.x <- yv
          
          cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity, interval='confidence')
          cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
          cal.est.conc.luc <- cal.est.conc.tab$fit
          cal.est.conc.luc.up <- cal.est.conc.tab$upr
          cal.est.conc.luc.low <- cal.est.conc.tab$lwr
          
          
          val.frame <- data.frame(predict.frame$Concentration, predict.intensity$Intensity, lucas.x, cal.est.conc.luc, cal.est.conc.luc.up, cal.est.conc.luc.low)
          colnames(val.frame) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
      }
      
      
      
      
      val.frame
      
  })
  
  calValFrame <- reactive({
      
      valFrame()
      
  })
  
  rangescalcurve <- reactiveValues(x = NULL, y = NULL)

  
  
  calCurvePlot <- reactive({
      
      predict.frame <- predictFrame()
      element.model <- elementModel()
      val.frame <- valFrame()
      
      element.name <- gsub("[.]", "", substr(input$calcurveelement, 1, 2))
      intens <- " Counts per Second"
      norma <- " Normalized"
      norma.comp <- " Compton Normalized"
      norma.tc <- " Valid Counts Normalized"
      conen <- " (%)"
      predi <- " Estimate (%)"
      log <- "Log "

      
      intensity.name <- c(element.name, intens)
      concentration.name <- c(element.name, conen)
      prediction.name <- c(element.name, predi)
      
      
      if(input$radiocal==1){
          calcurve.plot <- ggplot(data=predict.frame[ vals$keeprows, , drop = FALSE], aes(Intensity, Concentration)) +
          theme_light() +
          annotate("text", label=lm_eqn(lm(Concentration~Intensity, predict.frame[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
          geom_point() +
          geom_point(data = predict.frame[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
          stat_smooth(method="lm", fullrange = TRUE) +
          scale_x_continuous(paste(element.name, intens)) +
          scale_y_continuous(paste(element.name, conen)) +
          coord_cartesian(xlim = rangescalcurve$x, ylim = rangescalcurve$y, expand = FALSE)

      }
      
      if(input$radiocal==2){
          calcurve.plot <- ggplot(data=predict.frame[ vals$keeprows, , drop = FALSE], aes(Intensity, Concentration)) +
          theme_light() +
          annotate("text", label=lm_eqn_poly(lm(Concentration~Intensity + I(Intensity^2), predict.frame[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
          geom_point() +
          geom_point(data = predict.frame[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
          stat_smooth(method="lm", formula=y~poly(x,2)) +
          scale_x_continuous(paste(element.name, intens)) +
          scale_y_continuous(paste(element.name, conen)) +
          coord_cartesian(xlim = rangescalcurve$x, ylim = rangescalcurve$y, expand = FALSE)

      }
      
      if(input$radiocal==3){
          calcurve.plot <- ggplot(data=val.frame[ vals$keeprows, , drop = FALSE], aes(IntensityNorm, Concentration)) +
          theme_light() +
          annotate("text", label=lm_eqn(lm(Concentration~., val.frame[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
          geom_point() +
          geom_point(aes(IntensityNorm, Concentration), data = val.frame[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
          geom_smooth(aes(x=IntensityNorm, y=Concentration, ymin = Lower, ymax = Upper)) +
          scale_x_continuous(paste(element.name, norma)) +
          scale_y_continuous(paste(element.name, conen)) +
          coord_cartesian(xlim = rangescalcurve$x, ylim = rangescalcurve$y, expand = FALSE)

      }
      
      
      
      calcurve.plot
      
      
  })
  
  observeEvent(input$plot_cal_dblclick, {
      brush <- input$plot_cal_brush
      if (!is.null(brush)) {
          rangescalcurve$x <- c(brush$xmin, brush$xmax)
          rangescalcurve$y <- c(brush$ymin, brush$ymax)
          
      } else {
          rangescalcurve$x <- NULL
          rangescalcurve$y <- NULL
      }
  })
  
  output$calcurveplots <- renderPlot({
      calCurvePlot()
  })
  
  
  rangesvalcurve <- reactiveValues(x = NULL, y = NULL)

  
  valCurvePlot <- reactive({
      
      predict.frame <- predictFrame()
      element.model <- elementModel()
      
      
      
      element.name <- gsub("[.]", "", substr(input$calcurveelement, 1, 2))
      intens <- " Counts per Second"
      norma <- " Normalized"
      norma.comp <- " Compton Normalized"
      norma.tc <- " Valid Counts Normalized"
      conen <- " (%)"
      predi <- " Estimate (%)"
      log <- "Log "
      
      intensity.name <- c(element.name, intens)
      concentration.name <- c(element.name, conen)
      prediction.name <- c(element.name, predi)
      val.frame <- valFrame()
      
      
          valcurve.plot <- ggplot(data=val.frame[ vals$keeprows, , drop = FALSE], aes(Prediction, Concentration)) +
          theme_bw() +
          annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
          geom_abline(intercept=0, slope=1, lty=2) +
          stat_smooth(method="lm") +
          geom_point() +
          geom_point(aes(Prediction, Concentration),  data = val.frame[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
          scale_x_continuous(paste(element.name, predi)) +
          scale_y_continuous(paste(element.name, conen)) +
          coord_cartesian(xlim = rangesvalcurve$x, ylim = rangesvalcurve$y, expand = FALSE)

      
      

      
      valcurve.plot
      
  })
  
  observeEvent(input$plot_val_dblclick, {
      brush <- input$plot_val_brush
      if (!is.null(brush)) {
          rangesvalcurve$x <- c(brush$xmin, brush$xmax)
          rangesvalcurve$y <- c(brush$ymin, brush$ymax)
          
      } else {
          rangesvalcurve$x <- NULL
          rangesvalcurve$y <- NULL
      }
  })
  
  
  output$valcurveplots <- renderPlot({
      valCurvePlot()
  })
  
  
  
  calPlotDownload <- reactive({
      
      grid.arrange(calCurvePlot(), valCurvePlot(), ncol=2)
      
  })
  
  
  output$downloadcloudplot <- downloadHandler(
  filename = function() { paste(paste(c(input$calname, "_", input$calcurveelement), collapse=''), '.tiff',  sep='') },
  content = function(file) {
      ggsave(file,calPlotDownload(), device="tiff", compression="lzw", type="cairo", dpi=300, width=12, height=7)
  }
  )
  
  
  calValTable <- reactive({
      
      standard.table <- valFrame()
      hold.frame <- holdFrame()
      
      standard.table.summary <- data.frame(hold.frame$Spectrum, standard.table$Concentration, standard.table$Prediction, standard.table$Concentration-standard.table$Prediction, ((standard.table$Concentration-standard.table$Prediction)/standard.table$Concentration))
      colnames(standard.table.summary) <- c("Standard", "Concentration", "Prediction", "Difference", "Relative")
      
      standard.table.summary[,-1] <-round(standard.table.summary[,-1],4)
      standard.table.summary[,5] <- as.character(percent(standard.table.summary[,5]))
      
      this.table <- DT::datatable(standard.table.summary)
      this.table
      
  })
  
  
  output$standardsperformance <- DT::renderDataTable({
      
      
      standard.table <- calValTable()
      standard.table
      
  }, options =list(aoColumnDefs = list(list(sClass="alignRight",aTargets=c(list(2), list(3),list(4),list(5))))  ))
  
  
  randomizeData <- reactive({
      
      cal.frame <- concentrationTable()
      cal.frame <- cal.frame[ vals$keeprows, , drop = FALSE]
      total.number <- length(cal.frame[,1])
      sample.number <- total.number-round(input$percentrandom*total.number, 0)
      
      hold <- cal.frame[sample(nrow(cal.frame), sample.number),]
      cal.frame$Spectrum %in% hold$Spectrum
      
  })
  
  
  
  calCurveFrameRandomized <- reactive({
      
      predict.frame <- predictFrame()
      predict.frame <- predict.frame[ vals$keeprows, , drop = FALSE]
      
      predict.frame[randomizeData(),]
      
  })
  
  
  elementModelRandom <- reactive({
      
      predict.frame <- calCurveFrameRandomized()
      
      
      if (input$radiocal==1){
          cal.lm <- lm(Concentration~Intensity, data=predict.frame)
      }
      
      
      if (input$radiocal==2){
          cal.lm <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame)
      }
      
      if (input$radiocal==3){
          cal.lm <- lm(Concentration~., data=predict.frame)
      }
      
      cal.lm
      
  })
  
  
  valFrameRandomized <- reactive({
      
      predict.intensity <- predictIntensity()[ vals$keeprows, , drop = FALSE]
      predict.frame <- predictFrame()[ vals$keeprows, , drop = FALSE]
      
      predict.intensity <- predict.intensity[!(randomizeData()), , drop = FALSE]
      predict.frame <- predict.frame[!(randomizeData()), , drop = FALSE]
      element.model <- elementModelRandom()
      
      
      
      if (input$radiocal!=3){
          cal.est.conc.pred <- predict(object=element.model, newdata=predict.intensity, interval='confidence')
          cal.est.conc.tab <- data.frame(cal.est.conc.pred)
          cal.est.conc <- cal.est.conc.tab$fit
          
          val.frame <- data.frame(predict.frame$Concentration, cal.est.conc)
          colnames(val.frame) <- c("Concentration", "Prediction")
      }
      
      if (input$radiocal==3){
          
          xmin = 0; xmax=10
          N = length(predict.frame$Concentration)
          means = colMeans(predict.frame)
          dummyDF = t(as.data.frame(means))
          for(i in 2:N){dummyDF=rbind(dummyDF,means)}
          xv=seq(xmin,xmax, length.out=N)
          dummyDF$Concentration = xv
          yv=predict(element.model, newdata=predict.intensity)
          
          
          lucas.x <- yv
          
          cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity, interval='confidence')
          cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
          cal.est.conc.luc <- cal.est.conc.tab$fit
          cal.est.conc.luc.up <- cal.est.conc.tab$upr
          cal.est.conc.luc.low <- cal.est.conc.tab$lwr
          
          
          val.frame <- data.frame(predict.frame$Concentration, predict.intensity$Intensity, lucas.x, cal.est.conc.luc, cal.est.conc.luc.up, cal.est.conc.luc.low)
          colnames(val.frame) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
      }
      
      
      
      
      val.frame
      
  })
  
  
  valFrameRandomizedRev <- reactive({
      
      predict.intensity <- predictIntensity()[ vals$keeprows, , drop = FALSE]
      predict.frame <- predictFrame()[ vals$keeprows, , drop = FALSE]
      
      predict.intensity <- predict.intensity[(randomizeData()), ]
      predict.frame <- predict.frame[(randomizeData()), ]
      element.model <- elementModelRandom()
      
      
      
      if (input$radiocal!=3){
          cal.est.conc.pred <- predict(object=element.model, newdata=predict.intensity, interval='confidence')
          cal.est.conc.tab <- data.frame(cal.est.conc.pred)
          cal.est.conc <- cal.est.conc.tab$fit
          
          val.frame <- data.frame(predict.frame$Concentration, cal.est.conc)
          colnames(val.frame) <- c("Concentration", "Prediction")
      }
      
      if (input$radiocal==3){
          
          xmin = 0; xmax=10
          N = length(predict.frame$Concentration)
          means = colMeans(predict.frame)
          dummyDF = t(as.data.frame(means))
          for(i in 2:N){dummyDF=rbind(dummyDF,means)}
          xv=seq(xmin,xmax, length.out=N)
          dummyDF$Concentration = xv
          yv=predict(element.model, newdata=predict.intensity)
          
          
          lucas.x <- yv
          
          cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity, interval='confidence')
          cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
          cal.est.conc.luc <- cal.est.conc.tab$fit
          cal.est.conc.luc.up <- cal.est.conc.tab$upr
          cal.est.conc.luc.low <- cal.est.conc.tab$lwr
          
          
          val.frame <- data.frame(predict.frame$Concentration, predict.intensity$Intensity, lucas.x, cal.est.conc.luc, cal.est.conc.luc.up, cal.est.conc.luc.low)
          colnames(val.frame) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
      }
      
      
      
      
      val.frame
      
  })
  
  rangescalcurverandom <- reactiveValues(x = NULL, y = NULL)

  
  calCurvePlotRandom <- reactive({
      
      predict.frame <- calCurveFrameRandomized()
      element.model <- elementModelRandom()
      
      
      element.name <- gsub("[.]", "", substr(input$calcurveelement, 1, 2))
      intens <- " Counts per Second"
      norma <- " Normalized"
      norma.comp <- " Compton Normalized"
      norma.tc <- " Valid Counts Normalized"
      conen <- " (%)"
      predi <- " Estimate (%)"
      
      intensity.name <- c(element.name, intens)
      concentration.name <- c(element.name, conen)
      prediction.name <- c(element.name, predi)
      
      
      if(input$radiocal==1){
          calcurve.plot <- ggplot(data=predict.frame, aes(Intensity, Concentration)) +
          theme_light() +
          annotate("text", label=lm_eqn(element.model), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
          geom_point() +
          geom_point(data = predict.frame, shape = 21, fill = "red", color = "black", alpha = 0.25) +
          stat_smooth(method="lm", fullrange = TRUE) +
          scale_x_continuous(paste(element.name, intens)) +
          scale_y_continuous(paste(element.name, conen)) +
          coord_cartesian(xlim = rangescalcurverandom$x, ylim = rangescalcurverandom$y, expand = FALSE)

      }
      
      if(input$radiocal==2){
          
          calcurve.plot <- ggplot(data=predict.frame, aes(Intensity, Concentration)) +
          theme_light() +
          annotate("text", label=lm_eqn_poly(element.model), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
          geom_point() +
          geom_point(data = predict.frame, shape = 21, fill = "red", color = "black", alpha = 0.25) +
          stat_smooth(method="lm", formula=y~poly(x,2)) +
          scale_x_continuous(paste(element.name, intens)) +
          scale_y_continuous(paste(element.name, conen)) +
          coord_cartesian(xlim = rangescalcurverandom$x, ylim = rangescalcurverandom$y, expand = FALSE)
      }
      
      if(input$radiocal==3){
          val.frame <- valFrameRandomizedRev()

          calcurve.plot <- ggplot(data=val.frame, aes(IntensityNorm, Concentration)) +
          theme_light() +
          annotate("text", label=lm_eqn(element.model), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
          geom_point() +
          geom_point(aes(IntensityNorm, Concentration), data = val.frame, shape = 21, fill = "red", color = "black", alpha = 0.25) +
          geom_smooth(aes(x=IntensityNorm, y=Concentration, ymin = Lower, ymax = Upper)) +
          scale_x_continuous(paste(element.name, norma)) +
          scale_y_continuous(paste(element.name, conen)) +
          coord_cartesian(xlim = rangescalcurverandom$x, ylim = rangescalcurverandom$y, expand = FALSE)
      }
      
      calcurve.plot
      
      
  })
  
  observeEvent(input$plot_cal_dblclick_random, {
      brush <- input$plot_cal_brush_random
      if (!is.null(brush)) {
          rangescalcurverandom$x <- c(brush$xmin, brush$xmax)
          rangescalcurverandom$y <- c(brush$ymin, brush$ymax)
          
      } else {
          rangescalcurverandom$x <- NULL
          rangescalcurverandom$y <- NULL
      }
  })
  
  
  
  output$calcurveplotsrandom <- renderPlot({
      calCurvePlotRandom()
  })
  
  
  rangesvalcurverandom <- reactiveValues(x = NULL, y = NULL)

  valCurvePlotRandom <- reactive({
      
      
      
      element.name <- gsub("[.]", "", substr(input$calcurveelement, 1, 2))
      intens <- " Counts per Second"
      norma <- " Normalized"
      norma.comp <- " Compton Normalized"
      norma.tc <- " Valid Counts Normalized"
      conen <- " (%)"
      predi <- " Estimate (%)"
      
      intensity.name <- c(element.name, intens)
      concentration.name <- c(element.name, conen)
      prediction.name <- c(element.name, predi)
      
      val.frame <- valFrameRandomized()

      valcurve.plot <- ggplot(data=val.frame, aes(Prediction, Concentration)) +
      theme_bw() +
      annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
      geom_abline(intercept=0, slope=1, lty=2) +
      stat_smooth(method="lm") +
      geom_point() +
      geom_point(aes(Prediction, Concentration),  data = val.frame, shape = 21, fill = "red", color = "black", alpha = 0.25) +
      scale_x_continuous(paste(element.name, predi)) +
      scale_y_continuous(paste(element.name, conen)) +
      coord_cartesian(xlim = rangesvalcurverandom$x, ylim = rangesvalcurverandom$y, expand = FALSE)
      
      valcurve.plot
      
  })
  
  observeEvent(input$plot_val_dblclick_random, {
      brush <- input$plot_val_brush_random
      if (!is.null(brush)) {
          rangesvalcurverandom$x <- c(brush$xmin, brush$xmax)
          rangesvalcurverandom$y <- c(brush$ymin, brush$ymax)
          
      } else {
          rangesvalcurverandom$x <- NULL
          rangesvalcurverandom$y <- NULL
      }
  })
  
  output$valcurveplotsrandom <- renderPlot({
      valCurvePlotRandom()
  })
  
  
  ####CalCurves
  
  # Float over info
  output$hover_infocal <- renderUI({
      
      point.table <- if(input$radiocal!=3){
          calCurveFrame()
      } else if(input$radiocal==3) {
          calValFrame()
      }
      
      concentration.table <- concentrationTable()
      hold.table <- concentration.table[,c("Spectrum", input$calcurveelement)]
      colnames(hold.table) <- c("Spectrum", "Selection")
      hold.table$Selection[hold.table$Selection==""] <- NA
      hold.table <- hold.table[complete.cases(hold.table), ]
      
      point.table$Spectrum <- hold.table["Spectrum"]
      
      hover <- input$plot_hovercal
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
  
  
  # Float over info
  output$hover_infocal_random <- renderUI({
      
      point.table <- if(input$radiocal!=3){
          calCurveFrame()
      } else if(input$radiocal==3) {
          valFrame()
      }
      
      randomized <- randomizeData()
      
      
      point.table <- point.table[ vals$keeprows, , drop = FALSE]
      point.table <- point.table[randomized,]

      
      concentration.table <- concentrationTable()
      
      concentration.table <- concentration.table[ vals$keeprows, , drop = FALSE]
      concentration.table <- concentration.table[randomized,]
      
      hold.table <- concentration.table[,c("Spectrum", input$calcurveelement)]
      colnames(hold.table) <- c("Spectrum", "Selection")

      
      point.table$Spectrum <- hold.table["Spectrum"]
      
      hover <- input$plot_hovercal_random
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
  observeEvent(input$plot_cal_click, {
      
      predict.frame <- if(input$radiocal!=3){
          calCurveFrame()
      } else if(input$radiocal==3) {
          calValFrame()
      }
      
      res <- nearPoints(predict.frame, input$plot_cal_click, allRows = TRUE)
      
      vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  
  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle, {
      
      predict.frame <- if(input$radiocal!=3){
          calCurveFrame()
      } else if(input$radiocal==3) {
          calValFrame()
      }
      res <- brushedPoints(predict.frame, input$plot_cal_brush, allRows = TRUE)
      
      vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Reset all points
  observeEvent(input$exclude_reset, {
      
      predict.frame <- if(input$radiocal!=3){
          calCurveFrame()
      } else if(input$radiocal==3) {
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
      
      point.table <- calValFrame()
      concentration.table <- concentrationTable()
      hold.table <- concentration.table[,c("Spectrum", input$calcurveelement)]
      colnames(hold.table) <- c("Spectrum", "Selection")
      hold.table$Selection[hold.table$Selection==""] <- NA
      hold.table <- hold.table[complete.cases(hold.table), ]
      
      point.table$Spectrum <- hold.table["Spectrum"]
      
      
      hover <- input$plot_hoverval
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
  
  
  output$hover_infoval_random <- renderUI({
      
      point.table <- calValFrame()
      
      randomized <- randomizeData()
      
      
      point.table <- point.table[ vals$keeprows, , drop = FALSE]
      point.table <- point.table[!(randomized),]
      
      concentration.table <- concentrationTable()
      
      concentration.table <- concentration.table[ vals$keeprows, , drop = FALSE]
      concentration.table <- concentration.table[!(randomized),]
      concentration.table.rev <- concentration.table[(randomized),]
      
      hold.table <- concentration.table[,c("Spectrum", input$calcurveelement)]
      colnames(hold.table) <- c("Spectrum", "Selection")

      
      point.table$Spectrum <- hold.table["Spectrum"]
      
      
      point.table <- point.table[point.table$Concentration > min(concentration.table.rev[,input$calcurveelement], na.rm = TRUE) & point.table$Concentration < max(concentration.table.rev[,input$calcurveelement], na.rm = TRUE), ]
      
      
      
      hover <- input$plot_hoverval_random
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
  observeEvent(input$plot_val_click, {
      
      predict.frame <- calValFrame()
      
      res <- nearPoints(predict.frame, input$plot_val_click, allRows = TRUE)
      
      vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  
  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle, {
      predict.frame <- calValFrame()
      
      res <- brushedPoints(predict.frame, input$plot_val_brush, allRows = TRUE)
      
      vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Reset all points
  observeEvent(input$exclude_reset, {
      predict.frame <- calValFrame()
      
      vals$keeprows <- rep(TRUE, nrow(predict.frame))
  })
  
  
  
  modelFrame <- reactive({
      

      
      model <- elementModel()
      
      model.frame <- as.data.frame(augment(model))
      
      model.frame$qq <- qqnorm(model.frame$.std.resid)[[1]]
      
      model.frame$sqrt.std.resid <- sqrt(abs(model.frame$.std.resid))
      
      model.frame$seq.cooksd <- seq_along(model.frame$.cooksd)
      
      #model.frame$Spectrum <- predictFrameName()$Spectrum
      
      
      
      model.frame
      
      
  })
  
  
  
  
  
  diagResidualsFitted <- reactive({
      
      model <- modelFrame()
      
      p1 <- ggplot(model[ vals$keeprows, , drop = FALSE], aes(.fitted, .resid)) +
      stat_smooth(method="loess") +
      geom_hline(yintercept=0, col="red", linetype="dashed") +
      xlab("Fitted values") +
      ylab("Residuals") +
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
      xlab("Theoretical Quantiles") +
      ylab("Standardized Residuals") +
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
      xlab("Fitted Value") +
      ylab(expression(sqrt("|Standardized residuals|"))) +
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
      xlab("Obs. Number") +
      ylab("Cook's distance") +
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
      xlab("Leverage") +
      ylab("Standardized Residuals") +
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
      xlab("Leverage hii") +
      ylab("Cook's Distance") +
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
  
  #########Diagnostic Plot Controls#######
  ####Residuals Fitted
  # Float over info
  output$hover_inforesidualsfitted <- renderUI({
      
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





calList <- reactiveValues()

observeEvent(input$actionprocess, {
    isolate(calList <- emptyList())
    calList <<- calList
})




                 observeEvent(input$createcalelement, {
                              
                              cal.condition <- input$radiocal
                              norm.condition <- input$normcal
                              
                              norm.min <- print(input$comptonmin)
                              norm.max <- print(input$comptonmax)
                              
                              cal.table <- data.frame(cal.condition, norm.condition, norm.min, norm.max)
                              colnames(cal.table) <- c("CalType", "NormType", "Min", "Max")
                              
                              slope.corrections <- input$slope_vars
                              intercept.corrections <- input$intercept_vars
                              
                              standards.used <- vals$keeprows
                              
                              cal.mode.list <- list(cal.table, slope.corrections, intercept.corrections, standards.used)
                              names(cal.mode.list) <- c("CalTable", "Slope", "Intercept", "StandardsUsed")
                              
                              calConditons <<- cal.mode.list
                              
                              })


calList <- reactiveValues()
observeEvent(input$createcalelement, {
    
    
    calList[[input$calcurveelement]] <- list(isolate(calConditons), isolate(strip_glm(elementModel())))

    calList <<- calList

})

calPlotList <- reactiveValues()
calPlotList <- emptyList()
observeEvent(input$createcalelement, {
    
    
    calPlotList[[input$calcurveelement]] <- isolate(calPlotDownload())
    
    calPlotList <<- calPlotList
    
})

diagPlotList <- reactiveValues()
diagPlotList <- emptyList()
#observeEvent(input$createcalelement, {
    
    
    #diagPlotList[[input$calcurveelement]] <- isolate(diagPlotDownload())
    
    #diagPlotList <<- diagPlotList
    
    #})

Calibration <- reactiveValues()
observeEvent(input$createcal, {
    
    
    spectra.line.table <- if(dataType()=="Spectra"){
        spectraData()
    } else if(dataType()=="Net"){
        dataHold()
    }
             cal.intensities <- spectra.line.table[elementallinestouse()]
             cal.values <- values[["DF"]]
             cal.data <- if(dataType()=="Spectra"){
                 dataHold()
             } else if(dataType()=="Net"){
                 myData()
             }
             
             
             dataHold()

             calibrationList <- NULL
             calibrationList <- list(input$filetype, input$calunits, cal.data, cal.intensities, cal.values, calList)
             names(calibrationList) <- c("FileType", "Units", "Spectra", "Intensities", "Values", "calList")
             
    Calibration <<- calibrationList

    
})

CalibrationPlots <- reactiveValues()
observeEvent(input$createcal, {
    
    CalibrationPlots$calCurves <<- calPlotList
    
    
})

#observeEvent(input$createcal, {
    
    #CalibrationPlots$diagPlots <<- diagPlotList
    
    
    #})



output$downloadModel <- downloadHandler(
filename <- function(){
    paste(input$calname, "quant", sep=".")
},

content = function(file) {
    saveRDS(Calibration, file = file, compress="xz")
}
)


output$downloadReport <- downloadHandler(
 function() { paste(paste(c(input$calname), collapse=''), '.pdf',  sep='') },
 content = function(file){
    ml = marrangeGrob(grobs=CalibrationPlots$calCurves, nrow=1, ncol=1)
    ggsave(file, ml, device="pdf", dpi=300, width=12, height=7)

dev.off()
})




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
            if(quantType()=="Spectra"){
                "Spectra"
            } else if(quantType()=="Elio"){
                "Spectra"
            }  else if(quantType()=="MCA"){
                "Spectra"
            }  else if(quantType()=="SPX"){
                "Spectra"
            } else if (quantType()=="Net"){
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
            
            
            names(quantCals()[[input$defaultcal]])
            
            
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
            
            hold <- quantValues()
            
            optionhold <- if(is.null(input$calcurveelement_multi)){
                ls(hold[[input$defaultcal]])[2]
            }else{
                input$calcurveelement_multi
            }
            
            calListMulti[[input$defaultcal]][["calList"]][[optionhold]][[1]]$Intercept
            
            
            
        })
        
        
        output$inVar3_multi <- renderUI({
            
            selectInput(inputId = "intercept_vars_multi", label = h4("Intercept"), choices =  outVaralt2Multi(), selected=inVar3SelectedMulti(), multiple=TRUE)
        })
        
        inVar4SelectedMulti <- reactive({
            
            hold <- quantValues()
            
            optionhold <- if(is.null(input$calcurveelement_multi)){
                ls(hold[[input$defaultcal]])[2]
            }else{
                input$calcurveelement_multi
            }
            
            
            calListMulti[[input$defaultcal]][["calList"]][[optionhold]][[1]]$Slope
            
        })
        
        output$inVar4_multi <- renderUI({
            selectInput(inputId = "slope_vars_multi", label = h4("Slope"), choices =  outVaraltMulti(), selected=inVar4SelectedMulti(), multiple=TRUE)
        })
        
        
        
        calTypeSelectionMulti <- reactive({
            
            hold <- quantValues()
            
            optionhold <- if(is.null(input$calcurveelement_multi)){
                ls(hold[[input$defaultcal]])[2]
            }else{
                input$calcurveelement_multi
            }
            
            calListMulti[[input$defaultcal]][["calList"]][[optionhold]][[1]]$CalTable$CalType
            
            
        })
        
        calNormSelectionMulti <- reactive({
            
            hold <- quantValues()
            
            optionhold <- if(is.null(input$calcurveelement_multi)){
                ls(hold[[input$defaultcal]])[2]
            }else{
                input$calcurveelement_multi
            }
            
            
            calListMulti[[input$defaultcal]][["calList"]][[optionhold]][[1]]$CalTable$NormType
            
            
        })
        
        normMinSelectionMulti <- reactive({
            
            hold <- quantValues()
            
            optionhold <- if(is.null(input$calcurveelement_multi)){
                ls(hold[[input$defaultcal]])[2]
            }else{
                input$calcurveelement_multi
            }
            
            
            calListMulti[[input$defaultcal]][["calList"]][[optionhold]][[1]]$CalTable$Min
            
            
        })
        
        normMaxSelectionMulti <- reactive({
            
            hold <- quantValues()
            
            optionhold <- if(is.null(input$calcurveelement_multi)){
                ls(hold[[input$defaultcal]])[2]
            }else{
                input$calcurveelement_multi
            }
            
            calListMulti[[input$defaultcal]][["calList"]][[optionhold]][[1]]$CalTable$Max
            
        })
        
        
        
        
        
        output$calTypeInput_multi <- renderUI({
            
            selectInput("radiocal_multi", label = "Calibration Curve",
            choices = list("Linear" = 1, "Non-Linear" = 2, "Lucas-Tooth" = 3),
            selected = calTypeSelectionMulti())
            
            
        })
        
        
        output$normTypeInput_multi <- renderUI({
            
            selectInput("normcal_multi", label = "Normalization",
            choices = list("Time" = 1, "Total Counts" = 2, "Compton" = 3),
            selected = calNormSelectionMulti())
            
            
        })
        
        
        output$comptonMinInput_multi <- renderUI({
            
            numericInput('comptonmin_multi', label=h6("Min"), step=0.001, value=normMinSelectionMulti(), min=0, max=50, width='30%')
            
        })
        
        output$comptonMaxInput_multi <- renderUI({
            
            numericInput('comptonmax_multi', label=h6("Max"), step=0.001, value=normMaxSelectionMulti(), min=0, max=50, width='30%')
            
        })
        
        

observeEvent(input$actionprocess2_multi, {

        
 
        
        
        
        
        elementHoldMulti <- reactive({
            
            if(is.null(input$calcurveelement_multi)==TRUE){
                 elementallinestouseMulti()[1]
            } else{
                input$calcurveelement_multi
            }
            
        })
        
        
        calFileStandardsMulti <- reactive({
            
           cal.list <- quantCals()
           
           cal.names <- quantNames()
           
           n <- length(cal.names)
           
           element.cal.meta <- lapply(cal.list, "[[", elementHoldMulti())
           element.cal.meta2 <- lapply(element.cal.meta, "[[", 1)
           element.cal.meta3 <- lapply(element.cal.meta2, "[[", "StandardsUsed")
           element.cal.meta4 <- lapply(element.cal.meta3, as.vector)

           
           names(element.cal.meta4) <- quantNames()
           element.cal.meta4
           
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
            
            spectra.line.list

            
        })
        

        
        
        holdFrameMulti <- reactive({
            
            spectra.line.table <- spectraLineTableMulti()
            
            cal.names <- names(spectra.line.table)
            
            index <- seq(from=1, to=length(cal.names), by=1)
            
            concentration <- lapply(quantNames(),function(x) as.vector(as.numeric(unlist(concentrationTableMulti()[[x]][input$calcurveelement_multi]))))
            names(concentration) <- quantNames()

            
            
            intensity <- lapply(quantNames(),function(x) as.vector(as.numeric(unlist(spectraLineTableMulti()[[x]][input$calcurveelement_multi]))))
            names(intensity) <- quantNames()

            
            spectra.names <- lapply(quantNames(),function(x) as.vector(spectra.line.table[[x]][,"Spectrum"]))
            names(spectra.names) <- quantNames()
            
            hold.list <- lapply(quantNames(),function(x) data.frame(Spectrum=spectra.names[[x]], Concentration=concentration[[x]], Intensity=intensity[[x]]))
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



        predictFramePreMulti <- reactive({
            

            
            concentration <- lapply(holdFrameMulti(), function(x) as.vector(x[,"Concentration"]))
            names(concentration) <- quantNames()
            
            intensity <- lapply(holdFrameMulti(), function(x) as.vector(x[,"Intensity"]))
            names(intensity) <- quantNames()

            predict.frame <- lapply(quantNames(),function(x) data.frame(Concentration=concentration[[x]], Intensity==intensity[[x]]))
            names(predict.frame) <- quantNames()

            #data.frame(concentration, intensity)
            #colnames(predict.frame) <- c("Concentration", "Intensity")
            
            
            predict.frame
            
            
        })
        


        predictIntensityMulti <- reactive({
            

            
            spectra.line.table <- spectraLineTableMulti()
            data <- dataNormMulti()
            
            
            #spectra.line.table <- spectra.line.table[paste0(spectra.line.table$Spectrum, spectra.line.table$Instrument) %in% paste0(holdFrameMulti()$Spectrum, holdFrameMulti()$Instrument), ]
            
            spectra.line.table <- lapply(quantNames(),function(x) spectra.line.table[[x]][spectra.line.table[[x]][,"Spectrum"] %in% holdFrameMulti()[[x]][,"Spectrum"], ])
            names(spectra.line.table) <- quantNames()

            
            
            if (input$radiocal_multi!=3){
                
                if(input$normcal_multi==1){
                    predict.intensity <- if(dataTypeMulti()=="Spectra"){
                        lapply(quantNames(),function(x) general.prep(spectra.line.table=spectra.line.table[[x]], element.line=input$calcurveelement_multi))
                    } else if(dataTypeMulti()=="Net"){
                        lapply(quantNames(),function(x) general.prep.net(spectra.line.table=spectra.line.table[[x]], element.line=input$calcurveelement_multi))
                    }
                }
                
                if(input$normcal_multi==2){
                    predict.intensity <- if(dataTypeMulti()=="Spectra"){
                        lapply(quantNames(),function(x) simple.tc.prep(data=data[[x]], spectra.line.table=spectra.line.table[[x]], element.line=input$calcurveelement_multi))
                    } else if(dataTypeMulti()=="Net"){
                        lapply(quantNames(),function(x) simple.tc.prep.net(data=data[[x]], spectra.line.table=spectra.line.table[[x]], element.line=input$calcurveelement_multi))
                    }
                }
                
                if(input$normcal_multi==3){
                    predict.intensity <- if(dataTypeMulti()=="Spectra"){
                        lapply(quantNames(),function(x) simple.comp.prep(data=data[[x]], spectra.line.table=spectra.line.table[[x]], element.line=input$calcurveelement_multi, norm.min=input$comptonmin_multi, norm.max=input$comptonmax_multi))
                    } else if(dataTypeMulti()=="Net"){
                        lapply(quantNames(),function(x) simple.comp.prep.net(data=data[[x]], spectra.line.table=spectra.line.table[[x]], element.line=input$calcurveelement_multi, norm.min=input$comptonmin_multi, norm.max=input$comptonmax_multi))
                    }
                }
                
            }
            
            
            if (input$radiocal_multi==3){
                
                if(input$normcal_multi==1){
                    predict.intensity <- if(dataTypeMulti()=="Spectra"){
                        lapply(quantNames(),function(x) lucas.simp.prep(spectra.line.table=spectra.line.table[[x]], element.line=input$calcurveelement_multi, slope.element.lines=input$slope_vars_multi, intercept.element.lines=input$intercept_vars_multi))
                    } else if(dataTypeMulti()=="Net"){
                        lapply(quantNames(),function(x) lucas.simp.prep.net(spectra.line.table=spectra.line.table[[x]], element.line=input$calcurveelement_multi, slope.element.lines=input$slope_vars_multi, intercept.element.lines=input$intercept_vars_multi))
                    }
                }
                
                if(input$normcal_multi==2){
                    predict.intensity <- if(dataTypeMulti()=="Spectra"){
                        lapply(quantNames(),function(x) lucas.tc.prep(data=data[[x]], spectra.line.table=spectra.line.table[[x]], element.line=input$calcurveelement_multi, slope.element.lines=input$slope_vars_multi, intercept.element.lines=input$intercept_vars_multi))
                    } else if(dataTypeMulti()=="Net"){
                        lapply(quantNames(),function(x) lucas.tc.prep.net(data=data[[x]], spectra.line.table=spectra.line.table[[x]], element.line=input$calcurveelement_multi, slope.element.lines=input$slope_vars_multi, intercept.element.lines=input$intercept_vars_multi))
                    }
                }
                
                if(input$normcal_multi==3){
                    predict.intensity <- if(dataTypeMulti()=="Spectra"){
                        lapply(quantNames(),function(x) lucas.comp.prep(data=data[[x]], spectra.line.table=spectra.line.table[[x]], element.line=input$calcurveelement_multi, slope.element.lines=input$slope_vars_multi, intercept.element.lines=input$intercept_vars_multi, norm.min=input$comptonmin_multi, norm.max=input$comptonmax_multi))
                    } else if(dataTypeMulti()=="Net"){
                        lapply(quantNames(),function(x) lucas.comp.prep.net(data=data[[x]], spectra.line.table=spectra.line.table[[x]], element.line=input$calcurveelement_multi, slope.element.lines=input$slope_vars_multi, intercept.element.lines=input$intercept_vars_multi, norm.min=input$comptonmin_multi, norm.max=input$comptonmax_multi))
                    }
                }
                
            }
            
            predict.intensity <- lapply(predict.intensity, as.data.frame)
            
            names(predict.intensity) <- quantNames()
            predict.intensity
            
            
        })
        

        
        predictFrameMulti <- reactive({
            

            
            predict.frame <- predictFramePreMulti()
            predict.intensity <- predictIntensityMulti()
            
            
            
            predict.frame <- lapply(quantNames(),function(x) data.frame(predict.intensity[[x]], Concentration=predict.frame[[x]][,"Concentration"]))
            predict.frame <- lapply(predict.frame, as.data.frame)

            names(predict.frame) <- quantNames()
            
            predict.frame
            
            #data.frame(predict.intensity, predict.frame$Concentration)
            #colnames(predict.frame) <- c(names(predict.intensity), "Concentration")
            
            #predict.frame$Instrument <- holdFrameMulti()$Instrument
            
            #predict.frame
            
            
        })
        
        
    
        
        
        
        calCurveFrameMulti <- reactive({
            

            
            cal.frame <- predictFrameMulti()
            
            cal.frame <- lapply(quantNames(),function(x) data.frame(cal.frame[[x]], Instrument=x))
            names(cal.frame) <- quantNames()
            
            do.call("rbind", cal.frame)
            
            
        })
        
        
        
        elementModelMulti <- reactive({
            
            

            
            predict.list <- predictFrameMulti()
            
            
            
            if (input$radiocal_multi==1){
                cal.lm <- lapply(quantNames(),function(x) lm(Concentration~Intensity, data=predict.list[[x]][ vals_multi$keeprows[[x]], , drop = FALSE]))
            }
            
            
            if (input$radiocal_multi==2){
                cal.lm <- lapply(quantNames(), function(x) lm(Concentration~Intensity + I(Intensity^2), data=predict.list[[x]][ vals_multi$keeprows[[x]], , drop = FALSE]))
            }
            
            if (input$radiocal_multi==3){
                cal.lm <- lapply(quantNames(),function(x) lm(Concentration~., data=predict.list[[x]][ vals_multi$keeprows[[x]], , drop = FALSE]))
            }
            
            names(cal.lm) <- quantNames()

            
            cal.lm
            
        })
        
        
        
        
        valFrameMulti <- reactive({
            
            

            
            predict.intensity <- predictIntensityMulti()
            predict.frame <- predictFrameMulti()
            element.model <- elementModelMulti()
            
            
            if (input$radiocal_multi!=3){
                cal.est.conc.pred <- lapply(quantNames(),function(x) predict(object=element.model[[x]], newdata=predict.intensity[[x]], interval='confidence'))
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
        
                yv=lapply(quantNames(),function(x) predict(element.model[[x]], newdata=predict.intensity[[x]]))
                names(yv) <- quantNames()

                
                lucas.x <- yv
                
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
                IntensityNorm=lucas.x[[x]],
                Prediction=cal.est.conc.luc[[x]],
                Upper=cal.est.conc.luc.up[[x]],
                Lower=cal.est.conc.luc.low[[x]]
                ))
                names(val.frame) <- quantNames()
                
                
                #val.frame <- data.frame(predict.frame$Concentration, predict.intensity$Intensity, lucas.x, cal.est.conc.luc, cal.est.conc.luc.up, cal.est.conc.luc.low)
                #colnames(val.frame) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
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
            
            element.name <- gsub("[.]", "", substr(input$calcurveelement_multi, 1, 2))
            intens <- " Counts per Second"
            norma <- " Normalized"
            norma.comp <- " Compton Normalized"
            norma.tc <- " Valid Counts Normalized"
            conen <- " (%)"
            predi <- " Estimate (%)"
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
                scale_x_continuous(paste(element.name, intens)) +
                scale_y_continuous(paste(element.name, conen)) +
                coord_cartesian(xlim = rangescalcurve_multi$x, ylim = rangescalcurve_multi$y, expand = FALSE)
                
            }
            
            if(input$radiocal_multi==2){
                calcurve.plot <- ggplot(data=predict.frame[ unlist(vals_multi$keeprows), , drop = FALSE], aes(Intensity, Concentration, colour=Instrument, shape=Instrument)) +
                theme_light() +
                annotate("text", label=lm_eqn_poly(lm(Concentration~Intensity + I(Intensity^2), predict.frame[ unlist(vals_multi$keeprows), , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                geom_point() +
                geom_point(data = predict.frame[!unlist(vals_multi$keeprows), , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
                stat_smooth(method="lm", formula=y~poly(x,2), aes(fill=Instrument), alpha=0.1) +
                scale_x_continuous(paste(element.name, intens)) +
                scale_y_continuous(paste(element.name, conen)) +
                coord_cartesian(xlim = rangescalcurve_multi$x, ylim = rangescalcurve_multi$y, expand = FALSE)
                
            }
            
            if(input$radiocal_multi==3){
                calcurve.plot <- ggplot(data=val.frame[ unlist(vals_multi$keeprows), , drop = FALSE], aes(IntensityNorm, Concentration, colour=Instrument, shape=Instrument)) +
                theme_light() +
                annotate("text", label=lm_eqn(lm(Concentration~., val.frame[ unlist(vals_multi$keeprows), , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                geom_point() +
                geom_point(aes(IntensityNorm, Concentration), data = val.frame[!unlist(vals_multi$keeprows), , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
                geom_smooth(aes(x=IntensityNorm, y=Concentration, ymin = Lower, ymax = Upper, fill=Instrument), alpha=0.1) +
                scale_x_continuous(paste(element.name, norma)) +
                scale_y_continuous(paste(element.name, conen)) +
                coord_cartesian(xlim = rangescalcurve_multi$x, ylim = rangescalcurve_multi$y, expand = FALSE)
                
            }
            
            
            
            calcurve.plot
            
            
        })
        
        
        
        observeEvent(input$plot_cal_dblclick_multi, {
            brush <- input$plot_cal_brush_multi
            if (!is.null(brush)) {
                rangescalcurve_multi$x <- c(brush$xmin, brush$xmax)
                rangescalcurve_multi$y <- c(brush$ymin, brush$ymax)
                
            } else {
                rangescalcurve_multi$x <- NULL
                rangescalcurve_multi$y <- NULL
            }
        })
        
        output$calcurveplots_multi <- renderPlot({
            calCurvePlotMulti()
        })
        
        
        rangesvalcurve_multi <- reactiveValues(x = NULL, y = NULL)
        
        
        valCurvePlotMulti <- reactive({
            
            predict.intensity <- predictIntensityMulti()
            predict.frame <- predictFrameMulti()
            element.model <- elementModelMulti()[[input$defaultcal]]
            
            
            
            element.name <- gsub("[.]", "", substr(input$calcurveelement_multi, 1, 2))
            intens <- " Counts per Second"
            norma <- " Normalized"
            norma.comp <- " Compton Normalized"
            norma.tc <- " Valid Counts Normalized"
            conen <- " (%)"
            predi <- " Estimate (%)"
            log <- "Log "
            
            intensity.name <- c(element.name, intens)
            concentration.name <- c(element.name, conen)
            prediction.name <- c(element.name, predi)
            val.frame <- valFrameMulti()
            
            
            valcurve.plot <- ggplot(data=val.frame[ unlist(vals_multi$keeprows), , drop = FALSE], aes(Prediction, Concentration, colour=Instrument, shape=Instrument)) +
            theme_bw() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame[ unlist(vals_multi$keeprows), , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_abline(intercept=0, slope=1, lty=2) +
            stat_smooth(method="lm", aes(fill=Instrument), alpha=0.1) +
            geom_point() +
            geom_point(aes(Prediction, Concentration),  data = val.frame[!unlist(vals_multi$keeprows), , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
            scale_x_continuous(paste(element.name, predi)) +
            scale_y_continuous(paste(element.name, conen)) +
            coord_cartesian(xlim = rangesvalcurve_multi$x, ylim = rangesvalcurve_multi$y, expand = FALSE)
            
            
            
            
            
            valcurve.plot
            
        })
        
        
        observeEvent(input$plot_val_dblclick_multi, {
            brush <- input$plot_val_brush_multi
            if (!is.null(brush)) {
                rangesvalcurve_multi$x <- c(brush$xmin, brush$xmax)
                rangesvalcurve_multi$y <- c(brush$ymin, brush$ymax)
                
            } else {
                rangesvalcurve_multi$x <- NULL
                rangesvalcurve_multi$y <- NULL
            }
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
            
            cal.frame <- spectraLineTableMulti()[[input$defaultcal]]
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
            
            
            
            if (input$radiocal_multi!=3){
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
                
                yv=lapply(quantNames(),function(x) predict(element.model[[x]], newdata=predict.intensity[[x]]))
                names(yv) <- quantNames()

                
                
                lucas.x <- yv
                
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
                IntensityNorm=lucas.x[[x]],
                Prediction=cal.est.conc.luc[[x]],
                Upper=cal.est.conc.luc.up[[x]],
                Lower=cal.est.conc.luc.low[[x]]
                ))
                
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
            
            
            
            if (input$radiocal_multi!=3){
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
                
                yv=lapply(quantNames(),function(x) predict(element.model[[x]], newdata=predict.intensity[[x]]))
                names(yv) <- quantNames()

                
                
                lucas.x <- yv
                
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
                IntensityNorm=lucas.x[[x]],
                Prediction=cal.est.conc.luc[[x]],
                Upper=cal.est.conc.luc.up[[x]],
                Lower=cal.est.conc.luc.low[[x]]
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
            
            
            
            element.name <- gsub("[.]", "", substr(input$calcurveelement_multi, 1, 2))
            intens <- " Counts per Second"
            norma <- " Normalized"
            norma.comp <- " Compton Normalized"
            norma.tc <- " Valid Counts Normalized"
            conen <- " (%)"
            predi <- " Estimate (%)"
            
            intensity.name <- c(element.name, intens)
            concentration.name <- c(element.name, conen)
            prediction.name <- c(element.name, predi)
            
            
            if(input$radiocal_multi==1){
                calcurve.plot <- ggplot(data=predict.frame, aes(Intensity, Concentration, colour=Instrument, shape=Instrument)) +
                theme_light() +
                annotate("text", label=lm_eqn(element.model), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                geom_point() +
                stat_smooth(method="lm", fullrange = TRUE, aes(fill=Instrument), alpha=0.1) +
                scale_x_continuous(paste(element.name, intens)) +
                scale_y_continuous(paste(element.name, conen)) +
                coord_cartesian(xlim = rangescalcurverandom_multi$x, ylim = rangescalcurverandom_multi$y, expand = FALSE)
                
            }
            
            if(input$radiocal_multi==2){
                
                calcurve.plot <- ggplot(data=predict.frame, aes(Intensity, Concentration, colour=Instrument, shape=Instrument)) +
                theme_light() +
                annotate("text", label=lm_eqn_poly(element.model), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                geom_point() +
                stat_smooth(method="lm", formula=y~poly(x,2), aes(fill=Instrument), alpha=0.1) +
                scale_x_continuous(paste(element.name, intens)) +
                scale_y_continuous(paste(element.name, conen)) +
                coord_cartesian(xlim = rangescalcurverandom_multi$x, ylim = rangescalcurverandom_multi$y, expand = FALSE)
            }
            
            if(input$radiocal_multi==3){
                val.frame <- valFrameRandomizedRevMulti()

                calcurve.plot <- ggplot(data=val.frame, aes(IntensityNorm, Concentration, colour=Instrument, shape=Instrument)) +
                theme_light() +
                annotate("text", label=lm_eqn(element.model), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
                geom_point() +
                geom_smooth(aes(x=IntensityNorm, y=Concentration, ymin = Lower, ymax = Upper, fill=Instrument), alpha=0.1) +
                scale_x_continuous(paste(element.name, norma)) +
                scale_y_continuous(paste(element.name, conen)) +
                coord_cartesian(xlim = rangescalcurverandom_multi$x, ylim = rangescalcurverandom_multi$y, expand = FALSE)
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
        
        
        observeEvent(input$plot_cal_dblclick_random_multi, {
            brush <- input$plot_cal_brush_random_multi
            if (!is.null(brush)) {
                rangescalcurverandom_multi$x <- c(brush$xmin, brush$xmax)
                rangescalcurverandom_multi$y <- c(brush$ymin, brush$ymax)
                
            } else {
                rangescalcurverandom_multi$x <- NULL
                rangescalcurverandom_multi$y <- NULL
            }
        })
        
        
        output$calcurveplotsrandom_multi <- renderPlot({
            calCurvePlotRandomMulti()
        })
        
        
        rangesvalcurverandom_multi <- reactiveValues(x = NULL, y = NULL)
        
        
        valCurvePlotRandomMulti <- reactive({
            
            
            
            element.name <- gsub("[.]", "", substr(input$calcurveelement_multi, 1, 2))
            intens <- " Counts per Second"
            norma <- " Normalized"
            norma.comp <- " Compton Normalized"
            norma.tc <- " Valid Counts Normalized"
            conen <- " (%)"
            predi <- " Estimate (%)"
            
            intensity.name <- c(element.name, intens)
            concentration.name <- c(element.name, conen)
            prediction.name <- c(element.name, predi)
            
            val.frame <- valFrameRandomizedMulti()
            
            valcurve.plot <- ggplot(data=val.frame, aes(Prediction, Concentration, colour=Instrument, shape=Instrument)) +
            theme_bw() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_abline(intercept=0, slope=1, lty=2) +
            stat_smooth(method="lm", aes(fill=Instrument), alpha=0.1) +
            geom_point() +
            scale_x_continuous(paste(element.name, predi)) +
            scale_y_continuous(paste(element.name, conen)) +
            coord_cartesian(xlim = rangesvalcurverandom_multi$x, ylim = rangesvalcurverandom_multi$y, expand = FALSE)
            
            valcurve.plot
            
        })
        
        observeEvent(input$plot_val_dblclick_random_multi, {
            brush <- input$plot_val_brush_random_multi
            if (!is.null(brush)) {
                rangesvalcurverandom_multi$x <- c(brush$xmin, brush$xmax)
                rangesvalcurverandom_multi$y <- c(brush$ymin, brush$ymax)
                
            } else {
                rangesvalcurverandom_multi$x <- NULL
                rangesvalcurverandom_multi$y <- NULL
            }
        })
        
        output$valcurveplotsrandom_multi <- renderPlot({
            valCurvePlotRandomMulti()
        })
        
        
        
        ####CalCurves
        
        # Float over info
        output$hover_infocal_multi <- renderUI({
            
            point.table <- if(input$radiocal_multi!=3){
                calCurveFrameMulti()
            } else if(input$radiocal_multi==3) {
                valFrameMulti()
            }
            
            concentration.table <- do.call("rbind", holdFrameMulti())
            
            hold.table <- as.vector(concentration.table[,"Spectrum"])
            
            
            point.table$Spectrum <- hold.table
           
            
            #point.table$Spectrum <- hold.table["Spectrum"]
            
            hover <- input$plot_hovercal_multi
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
            p(HTML(paste0(point$Instrument))),
            p(HTML(paste0(point$Spectrum)))

            )
        })
        
        
        
        # Float over info
        output$hover_infocal_random_multi <- renderUI({
            
            point.table <- if(input$radiocal_multi!=3){
                calCurveFrameRandomizedMulti()
            } else if(input$radiocal_multi==3) {
                valFrameRandomizedRevMulti()
            }
            
            
            
            predict.frame <- lapply(quantNames(),function(x) data.frame(spectraLineTableMulti()[[x]][ vals_multi$keeprows[[x]], ]))
            names(predict.frame) <- quantNames()
            
            
            predict.frame <- lapply(quantNames(),function(x) data.frame( predict.frame[[x]][(randomizeDataMulti()), ]))
            names(predict.frame) <- quantNames()
            
           
            
            concentration.table <- do.call("rbind", predict.frame)

            #concentration.table <- concentration.table[ unlist(vals_multi$keeprows), , drop = FALSE]
            #concentration.table <- concentration.table[(randomizeDataMulti()),]
            
            hold.table <- as.vector(concentration.table[,"Spectrum"])
            
            
            point.table$Spectrum <- hold.table

            
            
            
            #point.table$Spectrum <- hold.table["Spectrum"]
            
            hover <- input$plot_hovercal_random_multi
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
            p(HTML(paste0(point$Instrument))),
            p(HTML(paste0(point$Spectrum)))
            
            )
        })
        
        
        
        # Toggle points that are clicked
        observeEvent(input$plot_cal_click_multi, {
            
            predict.frame <- if(input$radiocal_multi!=3){
                calCurveFrameMulti()
            } else if(input$radiocal_multi==3) {
                calValFrameMulti()
            }
            
            res <- nearPoints(predict.frame, input$plot_cal_click_multi, allRows = TRUE)
            
            temprows <- xor(unlist(vals_multi$keeprows), res$selected_)
            
            vals_multi$keeprows <- relist(flesh=temprows, skeleton=vals_multi$keeprows)
        })
        
        
        
        # Toggle points that are brushed, when button is clicked
        observeEvent(input$exclude_toggle_multi, {
            
            predict.frame <- if(input$radiocal_multi!=3){
                calCurveFrameMulti()
            } else if(input$radiocal_multi==3) {
                calValFrameMulti()
            }
            res <- brushedPoints(predict.frame, input$plot_cal_brush_multi, allRows = TRUE)
            
            temprows <- xor(unlist(vals_multi$keeprows), res$selected_)
            
            vals_multi$keeprows <- relist(flesh=temprows, skeleton=vals_multi$keeprows)        })
        
        
        
        # Reset all points
        observeEvent(input$exclude_reset_multi, {
            
            predict.frame <- if(input$radiocal_multi!=3){
                calCurveFrameMulti()
            } else if(input$radiocal_multi==3) {
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
            
            hold.table <- as.vector(concentration.table[,"Spectrum"])
            
            
            point.table$Spectrum <- hold.table

            
            
            hover <- input$plot_hoverval_multi
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
            p(HTML(paste0(point$Instrument))),
            p(HTML(paste0(point$Spectrum)))
            
            )
        })
        
        
        
        
        output$hover_infoval_random_multi <- renderUI({
            
            point.table <- valFrameRandomizedMulti()
            
            concentration.table.rev <- predictFrameRandomMulti()

            
            predict.frame <- lapply(quantNames(),function(x) data.frame(holdFrameMulti()[[x]][ vals_multi$keeprows[[x]], , drop = FALSE]))
            names(predict.frame) <- quantNames()
            
            
            predict.frame <- lapply(quantNames(),function(x) data.frame( predict.frame[[x]][!(randomizeDataMulti()), , drop = FALSE]))
            names(predict.frame) <- quantNames()
            
            predict.frame <- lapply(quantNames(), function(x) as.data.frame(
            predict.frame[[x]][predict.frame[[x]][, "Concentration"] > min(concentration.table.rev[[x]][,"Concentration"], na.rm = TRUE) & predict.frame[[x]][, "Concentration"] < max(concentration.table.rev[[x]][,"Concentration"], na.rm = TRUE), ]))
            names(predict.frame) <- quantNames()
            
            hold.table <- do.call("rbind", predict.frame)
            
            
            point.table$Spectrum <- hold.table[,"Spectrum"]
            
            
            hover <- input$plot_hoverval_random_multi
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
            p(HTML(paste0(point$Instrument))),
            p(HTML(paste0(point$Spectrum)))
            
            )
        })
        
        
        
        # Toggle points that are clicked
        observeEvent(input$plot_val_click_multi, {
            
            predict.frame <- calValFrameMulti()
            
            res <- nearPoints(predict.frame, input$plot_val_click_multi, allRows = TRUE)
            
            temprows <- xor(unlist(vals_multi$keeprows), res$selected_)
            
            vals_multi$keeprows <- relist(flesh=temprows, skeleton=vals_multi$keeprows)        })
        
        
        
        # Toggle points that are brushed, when button is clicked
        observeEvent(input$exclude_toggle_multi, {
            predict.frame <- calValFrameMulti()
            
            res <- brushedPoints(predict.frame, input$plot_val_brush_multi, allRows = TRUE)
            
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
        
        
        output$downloadcloudplot_multi <- downloadHandler(
        filename = function() { paste(paste(c(input$calname, "_", input$calcurveelement_multi), collapse=''), '.tiff',  sep='') },
        content = function(file) {
            ggsave(file,calPlotDownloadMulti(), device="tiff", compression="lzw", type="cairo", dpi=300, width=18, height=7)
        }
        )





calConditonsMulti <- reactiveValues()


observeEvent(input$createcalelement_multi, {
    
    cal.condition <- input$radiocal_multi
    norm.condition <- input$normcal_multi
    
    norm.min <- print(input$comptonmin_multi)
    norm.max <- print(input$comptonmax_multi)
    
    cal.table <- data.frame(cal.condition, norm.condition, norm.min, norm.max)
    colnames(cal.table) <- c("CalType", "NormType", "Min", "Max")
    
    slope.corrections <- input$slope_vars_multi
    intercept.corrections <- input$intercept_vars_multi
    
    standards.used <- 1
    
    cal.mode.list <- list(cal.table, slope.corrections, intercept.corrections, standards.used)
    names(cal.mode.list) <- c("CalTable", "Slope", "Intercept", "StandardsUsed")
    
    calConditonsMulti <<- cal.mode.list
    
})




        


        
        observeEvent(input$createcalelement_multi, {
            
           lapply(quantNames(), function(x)
            calListMulti[[x]][["calList"]][[input$calcurveelement_multi]] <<- list(isolate(calConditonsMulti), isolate(strip_glm(elementModelMulti()[[x]]))))
            
            
            
        })

observeEvent(input$createcalelement_multi, {
    
    lapply(quantNames(), function(x)
           calListMulti[[x]][["calList"]][[input$calcurveelement_multi]][[1]][[4]] <<- isolate(as.vector(vals_multi$keeprows[[x]])))
      
    
    
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
    ggsave(file, ml, device="pdf", dpi=300, width=16, height=7)
    
    dev.off()
})






        

})
        
        
    })
    
    
    output$filevalgrab <- renderUI({
        
        if(input$valfiletype=="Spectra") {
            fileInput('loadvaldata', 'Choose CSV', multiple=TRUE,
            accept=c(".csv"))
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
    
    

    
    
    observeEvent(input$processvalspectra, {
        
        fullValSpectra <- reactive({
            
            
            withProgress(message = 'Processing Data', value = 0, {
                
                inFile <- input$loadvaldata
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
            
            data
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
            
            data <- if(input$valfiletype=="Spectra"){
                fullValSpectra()
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
        
        
        
        calFileContents2 <- reactive({
            
            existingCalFile <- input$calfileinput2
            
            if (is.null(existingCalFile)) return(NULL)
            
            
            Calibration <- readRDS(existingCalFile$datapath)
            
            Calibration
            
        })
        
        valdata <- myValData()

        
        
        output$contents2 <- renderTable({
            
            
            
            myValData()
            
        })
        
        calValHold <- reactive({
            

            calFileContents2()[[6]]
            
            
            


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
        
        valDataType <- reactive({
            
            if(input$valfiletype=="Spectra"){
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
            
            if(valDataType()=="Spectra"){spectra.line.list <- lapply(valelements, function(x) elementGrab(element.line=x, data=val.data))}
            if(valDataType()=="Spectra"){element.count.list <- lapply(spectra.line.list, '[', 2)}
            
            
            
            if(valDataType()=="Spectra"){spectra.line.vector <- as.numeric(unlist(element.count.list))}
            
            if(valDataType()=="Spectra"){dim(spectra.line.vector) <- c(length(spectra.line.list[[1]]$Spectrum), length(valelements))}
            
            if(valDataType()=="Spectra"){spectra.line.frame <- data.frame(spectra.line.list[[1]]$Spectrum, spectra.line.vector)}
            
            if(valDataType()=="Spectra"){colnames(spectra.line.frame) <- c("Spectrum", valelements)}
            
            if(valDataType()=="Spectra"){spectra.line.frame <- as.data.frame(spectra.line.frame)}
            
            if(valDataType()=="Spectra"){spectra.line.frame}
            
            if(valDataType()=="Spectra"){val.line.table <- data.table(spectra.line.frame[, c("Spectrum", valelements), drop = FALSE])}
            
            
            if(valDataType()=="Net"){val.line.table <- val.data[c("Spectrum", valelements), drop=FALSE]}
                
                
                val.line.table


        })
        
        
        
        fullInputValCounts <- reactive({
            valelements <- calValElements()
            variableelements <- calVariableElements()
            val.data <- myValData()
            
            if(valDataType()=="Spectra"){spectra.line.list <- lapply(variableelements, function(x) elementGrab(element.line=x, data=val.data))}
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
            
        count.table <- data.frame(fullInputValCounts())
        the.cal <- calValHold()
        elements.cal <- calValElements()
        elements <- elements.cal[!is.na(match(elements.cal, ls(count.table)))]
        variables <- calVariableElements()
        valdata <- myValData()
        
        #elements <- fluorescence.lines$Symbol[sort(order(fluorescence.lines$Symbol)[elements])]


            
            
        predicted.list <- lapply(elements, function (x)
            if(valDataType()=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                    object=the.cal[[x]][[2]],
                    newdata=general.prep(
                        spectra.line.table=as.data.frame(
                            count.table
                            ),
                        element.line=x)
                )
            } else if(valDataType()=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==2) {
                predict(
                    object=the.cal[[x]][[2]],
                    newdata=simple.tc.prep(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                            count.table
                            ),
                        element.line=x
                    )
                )
            } else if(valDataType()=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==3) {
                predict(
                    object=the.cal[[x]][[2]],
                        newdata=simple.comp.prep(
                            data=valdata,
                            spectra.line.table=as.data.frame(
                                count.table
                                ),
                            element.line=x,
                            norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                            norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                            )
                )
            } else if(valDataType()=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                 predict(
                    object=the.cal[[x]][[2]],
                    newdata=lucas.simp.prep(
                        spectra.line.table=as.data.frame(
                            count.table
                            ),
                        element.line=x,
                        slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                        )
                 )
            } else if(valDataType()=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==2){
                predict(
                    object=the.cal[[x]][[2]],
                    newdata=lucas.tc.prep(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                            count.table
                            ),
                        element.line=x,
                        slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                    )
                )
            } else if(valDataType()=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==3){
                predict(
                    object=the.cal[[x]][[2]],
                    newdata=lucas.comp.prep(
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
            }else if(valDataType()=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                    object=the.cal[[x]][[2]],
                    newdata=general.prep.net(
                        spectra.line.table=as.data.frame(
                            count.table
                            ),
                        element.line=x)
                )
            } else if(valDataType()=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==2) {
                predict(
                    object=the.cal[[x]][[2]],
                    newdata=simple.tc.prep.net(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                            count.table
                            ),
                            element.line=x
                            )
                )
            } else if(valDataType()=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==3) {
                predict(
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
            } else if(valDataType()=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                    object=the.cal[[x]][[2]],
                    newdata=lucas.simp.prep.net(
                        spectra.line.table=as.data.frame(
                            count.table
                            ),
                        element.line=x,
                        slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                        )
                )
            } else if(valDataType()=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==2){
                predict(
                    object=the.cal[[x]][[2]],
                    newdata=lucas.tc.prep.net(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                            count.table
                            ),
                        element.line=x,
                        slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                        )
                )
            } else if(valDataType()=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==3){
                predict(
                    object=the.cal[[x]][[2]],
                    newdata=lucas.comp.prep.net(
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
            }
            
            
            
            )
            
        predicted.vector <- unlist(predicted.list)
        
        dim(predicted.vector) <- c(length(count.table$Spectrum), length(elements))
        
        predicted.frame <- data.frame(count.table$Spectrum, predicted.vector)
        
        colnames(predicted.frame) <- c("Spectrum", elements)
        #elements <- elements[order(match(fluorescence.lines$Symbol, elements))]

        

        predicted.data.table <- data.table(predicted.frame)

        #predicted.values <- t(predicted.values)
        predicted.data.table
            
            
        })
        
        output$myvaltable2 <- renderDataTable({
            
            tableInputValQuant()
            
        })
        
        
        # valtest <- lapply(valelements, function(x) predict(calsList[[x]], as.data.frame(val.line.table[x])))
        
        output$downloadValData <- downloadHandler(
        filename = function() { paste(input$calname, "_ValData", '.csv', sep='', collapse='') },
        content = function(file
        ) {
            write.csv(tableInputValQuant(), file)
        }
        )
        
        
  


})

 })





