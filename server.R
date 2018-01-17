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



options(shiny.maxRequestSize=9000000*1024^2)

options(warn=-1)
assign("last.warning", NULL, envir = baseenv())

shinyServer(function(input, output, session) {
    
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
            }
            
                data


        })
        
        
calFileContents <- reactive({
            
    existingCalFile <- input$calfileinput
            
    if (is.null(existingCalFile)) return(NULL)


    Calibration <- readRDS(existingCalFile$datapath)
            
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
     
     spectra.line.frame
     
 })
 
 netData <- reactive({
     
     net.data <- dataHold()
     
     elements <- elementallinestouse()


     net.data.partial <- net.data[,elements]
     net.data <- data.frame(net.data$Spectrum ,net.data.partial)
     colnames(net.data) <- c("Spectrum", elements)
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
    value.frame.reduced <- value.frame[2:length(value.frame)]
    
    rownames(hold.frame.reduced) <- hold.frame$Spectrum
    rownames(value.frame.reduced) <- value.frame$Spectrum

    
    hotable.new = hold.frame.reduced %>% add_rownames %>%
    full_join(value.frame.reduced %>% add_rownames) %>%
    group_by(rowname) %>%
    summarise_all(funs(sum(., na.rm = FALSE)))
    
    colnames(hotable.new)[1] <- "Spectrum"
    
    hotable.new
    
    
})

hotableInput <- reactive({
    
    
    hotable.data <- if(input$usecalfile==FALSE){
        hotableInputBlank()
    }else if(input$usecalfile==TRUE){
        hotableInputCal()
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
    
    
    
    if (!is.null(DF))
    rhandsontable(DF) %>% hot_col(2:length(DF), renderer=htmlwidgets::JS("safeHtmlRenderer"))
    
    
})


observeEvent(input$resethotable, {
    
   values[["DF"]] <- NULL
   
   values[["DF"]] <- hotableInput()

    
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
    
    
    myelements <- c(elementallinestouse(), "None")

    
    if(is.null(myelements)){
        paste("Ca.K.alpha")
    }else{
        myelements
    }
    
})

outVaralt2 <- reactive({
    input$hotableprocess2
    
    
    myelements <- c(elementallinestouse(), "None")
    
    
    if(is.null(myelements)){
        paste("Ca.K.alpha")
    }else{
        myelements[! myelements %in% c(input$calcurveelement, "None")]
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

inVar4Selected <- reactive({
    
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

output$inVar4 <- renderUI({
    selectInput(inputId = "slope_vars", label = h4("Slope"), choices =  outVaralt(), selected=inVar4Selected(), multiple=TRUE)
})




calConditons <- reactiveValues()
calList <- reactiveValues()
calList <- NULL

observeEvent(input$hotableprocess2, {
    
    cal.condition <- 1
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
    
    radioButtons("radiocal", label = "Calibration Curve",
    choices = list("Linear" = 1, "Non-Linear" = 2, "Lucas-Tooth" = 3),
    selected = calTypeSelection())
    
    
})


output$normTypeInput <- renderUI({
    
    radioButtons("normcal", label = "Normalization",
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
          calList[[input$calcurveelement]][[1]][[4]]
      } else if(input$usecalfile==FALSE && is.null(calList[[elementHold()]])==FALSE && is.null(calFileContents()$calList[[elementHold()]])==TRUE){
          calList[[elementHold()]][[1]][[4]]
      } else if(input$usecalfile==TRUE && is.null(calList[[elementHold()]])==TRUE && is.null(calFileContents()$calList[[elementHold()]])==TRUE){
          rep(TRUE, dataCount())
      }
      
      
      
      
  })
  
  
  
  
  vals <- reactiveValues()
  

vals$keeprows <- if(is.null(calFileStandards())==FALSE){
    calFileStandards()
}else{
    rep(TRUE, dataCount())
}
  
vals$keeprows <- rep(TRUE, dataCount())

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
    } else if (input$filetype=="Net"){
        "Net"
    }
    
})

  
  
  predictFrame <- reactive({
      
      data <- dataHold()
      
      
      
      
      concentration.table <- as.data.frame(values[["DF"]], stringsAsFactors=FALSE)
      concentration.table[concentration.table==""] <- 999
      


      spectra.line.table <- if(dataType()=="Spectra"){
          spectraData()
      }else if(dataType()=="Net"){
          dataHold()
      }
      
      
      
      concentration <- as.vector(as.numeric(unlist(concentration.table[input$calcurveelement])))
      
      
      
      intensity <- as.vector(as.numeric(unlist(spectra.line.table[input$calcurveelement])))
      
      spectra.names <- spectra.line.table$Spectrum
      
      hold.frame <- data.frame(spectra.names, concentration, intensity)
      colnames(hold.frame) <- c("Spectrum", "Concentration", "Intensity")
      hold.frame[hold.frame==999] <- NA
      hold.frame <- na.omit(hold.frame)
      
      concentration <- hold.frame$Concentration
      intensity <- hold.frame$Intensity
      
      data <- data[data$Spectrum %in% hold.frame$Spectrum, ]
      
      spectra.line.table <- spectra.line.table[spectra.line.table$Spectrum %in% hold.frame$Spectrum, ]
      
      
      
      predict.frame <- data.frame(concentration, intensity)
      colnames(predict.frame) <- c("Concentration", "Intensity")
      
      
      predict.frame[ vals$keeprows, , drop = FALSE]
    

  })
  
  



calCurveFrame <- reactive({
    
    data <- dataHold()
    
    
    #if(input$usecalfile==TRUE && input$plot_cal_click==FALSE){vals$keeprows <- calFileContents()$calList[[input$calcurveelement]][[1]][[4]]}
    
    
    
    concentration.table <- as.data.frame(values[["DF"]], stringsAsFactors=FALSE)
    concentration.table[concentration.table==""] <- 999
    
    spectra.line.table <- if(dataType()=="Spectra"){
        spectraData()
    } else if(dataType()=="Net"){
        dataHold()
    }
    
    
    
    
    
    
    
    
    concentration <- as.vector(as.numeric(unlist(concentration.table[input$calcurveelement])))
    
    
    
    intensity <- as.vector(as.numeric(unlist(spectra.line.table[input$calcurveelement])))
    
    spectra.names <- spectra.line.table$Spectrum
    
    # intensity <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
    
    
    # concentration <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
    
    hold.frame <- data.frame(spectra.names, concentration, intensity)
    colnames(hold.frame) <- c("Spectrum", "Concentration", "Intensity")
    hold.frame[hold.frame==999] <- NA
    hold.frame <- na.omit(hold.frame)
    
    concentration <- hold.frame$Concentration
    intensity <- hold.frame$Intensity
    
    data <- data[data$Spectrum %in% hold.frame$Spectrum, ]
    
    spectra.line.table <- spectra.line.table[spectra.line.table$Spectrum %in% hold.frame$Spectrum, ]
    
    
    
    
    
    
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
    
    
    predict.frame <- data.frame(concentration, intensity)
    colnames(predict.frame) <- c("Concentration", "Intensity")
    
    predict.intensity <- if(input$filetype=="Spectra"){
        general.prep(spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
    } else if(input$filetype=="Net"){
        general.prep.net(spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
    }
    
    
    if(input$normcal==2){
        
        
        predict.intensity.tc <- if(dataType()=="Spectra"){
            simple.tc.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
        } else if(dataType()=="Net"){
            simple.tc.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
        }
        
        predict.frame.tc <- data.frame(predict.intensity.tc, predict.frame$Concentration)
        colnames(predict.frame.tc) <- c(names(predict.intensity.tc), "Concentration")
        
        
    }
    
    if(input$normcal==3){
        
        predict.intensity.comp <- if(dataType()=="Spectra"){
            simple.comp.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, norm.min=input$comptonmin, norm.max=input$comptonmax)
        } else if(dataType()=="Net"){
            simple.comp.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, norm.min=input$comptonmin, norm.max=input$comptonmax)
        }
        
        predict.frame.comp <- data.frame(predict.intensity.comp, predict.frame$Concentration)
        colnames(predict.frame.comp) <- c(names(predict.intensity.comp), "Concentration")
    }
    
    
    
    
    if (input$normcal==1){
        
        cal.lm <- lm(Concentration~Intensity, data=predict.frame[ vals$keeprows, , drop = FALSE])
        cal.lm.poly <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame[ vals$keeprows, , drop = FALSE])
        
        cal.est.conc.pred <- predict(object=cal.lm, newdata=predict.intensity, interval='confidence')
        cal.est.conc.tab <- data.frame(cal.est.conc.pred)
        cal.est.conc <- cal.est.conc.tab$fit
        
        cal.est.poly.conc.pred <- predict(object=cal.lm.poly, newdata=predict.intensity, interval='confidence')
        cal.est.poly.conc.tab <- data.frame(cal.est.poly.conc.pred)
        cal.est.poly.conc <- cal.est.poly.conc.tab$fit
        
        val.frame <- data.frame(predict.frame$Concentration, cal.est.conc)
        colnames(val.frame) <- c("Concentration", "Prediction")
        
        
        val.frame.poly <- data.frame(predict.frame$Concentration, cal.est.poly.conc)
        colnames(val.frame.poly) <- c("Concentration", "Prediction")
        
    }
    
    
    if (input$normcal==3){
        
        
        
        cal.lm.comp <- lm(Concentration~Intensity, data=predict.frame.comp[ vals$keeprows, , drop = FALSE])
        
        cal.lm.poly.comp <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame.comp[ vals$keeprows, , drop = FALSE])
        
        cal.est.conc.pred.comp <- predict(object=cal.lm.comp, newdata=predict.intensity.comp, interval='confidence')
        cal.est.conc.tab.comp <- data.frame(cal.est.conc.pred.comp)
        cal.est.conc.comp <- cal.est.conc.tab.comp$fit
        
        cal.est.poly.conc.pred.comp <- predict(object=cal.lm.poly.comp, newdata=predict.intensity.comp, interval='confidence')
        cal.est.poly.conc.tab.comp <- data.frame(cal.est.poly.conc.pred.comp)
        cal.est.poly.conc.comp <- cal.est.poly.conc.tab.comp$fit
        
        val.frame.comp <- data.frame(predict.frame$Concentration, cal.est.conc.comp)
        colnames(val.frame.comp) <- c("Concentration", "Prediction")
        
        
        
        val.frame.poly.comp <- data.frame(predict.frame$Concentration, cal.est.poly.conc.comp)
        colnames(val.frame.poly.comp) <- c("Concentration", "Prediction")
        
    }
    
    
    
    if (input$normcal==2){
        
        cal.lm.tc <- lm(Concentration~Intensity, data=predict.frame.tc[ vals$keeprows, , drop = FALSE])
        cal.lm.poly.tc <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame.tc[ vals$keeprows, , drop = FALSE])
        
        cal.est.conc.pred.tc <- predict(object=cal.lm.tc, newdata=predict.intensity.tc, interval='confidence')
        cal.est.conc.tab.tc <- data.frame(cal.est.conc.pred.tc)
        cal.est.conc.tc <- cal.est.conc.tab.tc$fit
        
        cal.est.poly.conc.pred.tc <- predict(object=cal.lm.poly.tc, newdata=predict.intensity.tc, interval='confidence')
        cal.est.poly.conc.tab.tc <- data.frame(cal.est.poly.conc.pred.tc)
        cal.est.poly.conc.tc <- cal.est.poly.conc.tab.tc$fit
        
        val.frame.tc <- data.frame(predict.frame$Concentration, cal.est.conc.tc)
        colnames(val.frame.tc) <- c("Concentration", "Prediction")
        
        
        
        val.frame.poly.tc <- data.frame(predict.frame$Concentration, cal.est.poly.conc.tc)
        colnames(val.frame.poly.tc) <- c("Concentration", "Prediction")
        
        
    }
    
    
    
    
    
    ####Fourth Iteration
    
    if (input$radiocal==3){
        
        
        
        if(input$normcal==1){
            
            predict.intensity.luc <- if(dataType()=="Spectra"){
                lucas.simp.prep(spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
            } else if(dataType()=="Net"){
                lucas.simp.prep.net(spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
            }
            
            predict.frame.luc <- data.frame(predict.intensity.luc, predict.frame$Concentration)
            colnames(predict.frame.luc) <- c(names(predict.intensity.luc), "Concentration")
            
            
            lucas.lm <- lm(Concentration~., data=predict.frame.luc[ vals$keeprows, , drop = FALSE])
            
            
            xmin = 0; xmax=10
            N = length(predict.frame.luc$Concentration)
            means = colMeans(predict.frame.luc)
            dummyDF = t(as.data.frame(means))
            for(i in 2:N){dummyDF=rbind(dummyDF,means)}
            xv=seq(xmin,xmax, length.out=N)
            dummyDF$Concentration = xv
            yv=predict(lucas.lm, newdata=predict.intensity.luc)
            
            
            
            lucas.x <- yv
            
            
            
            cal.est.conc.pred.luc <- predict(object=lucas.lm , newdata=predict.intensity.luc, interval='confidence')
            cal.est.conc.tab.luc <- data.frame(cal.est.conc.pred.luc)
            cal.est.conc.luc <- cal.est.conc.tab.luc$fit
            cal.est.conc.luc.up <- cal.est.conc.tab.luc$upr
            cal.est.conc.luc.low <- cal.est.conc.tab.luc$lwr
            
            
            val.frame.luc <- data.frame(predict.frame$Concentration, predict.intensity.luc$Intensity, lucas.x, cal.est.conc.luc, cal.est.conc.luc.up, cal.est.conc.luc.low)
            colnames(val.frame.luc) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
        }
        
        
        
        
        
        if(input$normcal==3){
            
            
            predict.intensity.luc.comp <- if(dataType()=="Spectra"){
                lucas.comp.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
            } else if(dataType()=="Net"){
                lucas.comp.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
            }
            
            predict.frame.luc.comp <- data.frame(predict.intensity.luc.comp, predict.frame$Concentration)
            colnames(predict.frame.luc.comp) <- c(names(predict.intensity.luc.comp), "Concentration")
            
            
            
            
            lucas.lm.comp <- lm(Concentration~., data=predict.frame.luc.comp[ vals$keeprows, , drop = FALSE])
            
            
            xmin = 0; xmax=10
            N = length(predict.frame.luc.comp$Concentration)
            means = colMeans(predict.frame.luc.comp)
            dummyDF = t(as.data.frame(means))
            for(i in 2:N){dummyDF=rbind(dummyDF,means)}
            xv=seq(xmin,xmax, length.out=N)
            dummyDF$Concentration = xv
            yv=predict(lucas.lm.comp, newdata=predict.intensity.luc.comp)
            
            
            lucas.x.comp <- yv
            
            cal.est.conc.pred.luc.comp <- predict(object=lucas.lm.comp , newdata=predict.intensity.luc.comp, interval='confidence')
            cal.est.conc.tab.luc.comp <- data.frame(cal.est.conc.pred.luc.comp)
            cal.est.conc.luc.comp <- cal.est.conc.tab.luc.comp$fit
            cal.est.conc.luc.up.comp <- cal.est.conc.tab.luc.comp$upr
            cal.est.conc.luc.low.comp <- cal.est.conc.tab.luc.comp$lwr
            
            
            val.frame.luc.comp <- data.frame(predict.frame$Concentration, predict.intensity.luc.comp$Intensity, lucas.x.comp, cal.est.conc.luc.comp, cal.est.conc.luc.up.comp, cal.est.conc.luc.low.comp)
            colnames(val.frame.luc.comp) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
            
        }
        
        
        
        
        if(input$normcal==2){
            
            
            predict.intensity.luc.tc <- if(dataType()=="Spectra"){
                lucas.tc.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
            } else if(dataType()=="Net"){
                lucas.tc.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
            }
            
            predict.frame.luc.tc <- data.frame(predict.intensity.luc.tc, predict.frame$Concentration)
            colnames(predict.frame.luc.tc) <- c(names(predict.intensity.luc.tc), "Concentration")
            
            
            lucas.lm.tc <- lm(Concentration~., data=predict.frame.luc.tc)
            
            xmin = 0; xmax=10
            N = length(predict.frame.luc.tc$Concentration)
            means = colMeans(predict.frame.luc.tc)
            dummyDF = t(as.data.frame(means))
            for(i in 2:N){dummyDF=rbind(dummyDF,means)}
            xv=seq(xmin,xmax, length.out=N)
            dummyDF$Concentration = xv
            yv=predict(lucas.lm.tc, newdata=predict.intensity.luc.tc)
            
            
            lucas.x.tc <- yv
            
            cal.est.conc.pred.luc.tc <- predict(object=lucas.lm.tc , newdata=predict.intensity.luc.tc, interval='confidence')
            cal.est.conc.tab.luc.tc <- data.frame(cal.est.conc.pred.luc.tc)
            cal.est.conc.luc.tc <- cal.est.conc.tab.luc.tc$fit
            cal.est.conc.luc.up.tc <- cal.est.conc.tab.luc.tc$upr
            cal.est.conc.luc.low.tc <- cal.est.conc.tab.luc.tc$lwr
            
            
            val.frame.luc.tc <- data.frame(predict.frame$Concentration, predict.intensity.luc.tc$Intensity, lucas.x.tc, cal.est.conc.luc.tc, cal.est.conc.luc.up.tc, cal.est.conc.luc.low.tc)
            colnames(val.frame.luc.tc) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
            
        }
        
    }

    

    
    
    
    
    if(input$radiocal==1 && input$normcal==1) {
        predict.frame
    } else if (input$radiocal==2 && input$normcal==1){
        predict.frame
    } else if (input$radiocal==1 && input$normcal==2){
        predict.frame.tc
    } else if (input$radiocal==2 && input$normcal==2) {
        predict.frame.tc
    } else if (input$radiocal==1 && input$normcal==3) {
        predict.frame.comp
    } else if (input$radiocal==2 && input$normcal==3) {
        predict.frame.comp
    } else if (input$radiocal==3 && input$normcal==1) {
        predict.frame.luc
    } else if (input$radiocal==3 && input$normcal==2){
        predict.frame.luc.tc
    } else if (input$radiocal==3 && input$normcal==3) {
        predict.frame.luc.comp
    } else if (is.na(input$calcurveelement)) {
        return()
    }
    
})



calValFrame <- reactive({
    
    data <- dataHold()
    
    
    #if(input$usecalfile==TRUE && input$plot_cal_click==FALSE){vals$keeprows <- calFileContents()$calList[[input$calcurveelement]][[1]][[4]]}
    
    
    
    concentration.table <- as.data.frame(values[["DF"]], stringsAsFactors=FALSE)
    concentration.table[concentration.table==""] <- 999
    
    spectra.line.table <- if(dataType()=="Spectra"){
        spectraData()
    } else if(dataType()=="Net"){
        dataHold()
    }
    
    
    
    
    
    
    
    
    concentration <- as.vector(as.numeric(unlist(concentration.table[input$calcurveelement])))
    
    
    
    intensity <- as.vector(as.numeric(unlist(spectra.line.table[input$calcurveelement])))
    
    spectra.names <- spectra.line.table$Spectrum
    
    # intensity <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
    
    
    # concentration <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
    
    hold.frame <- data.frame(spectra.names, concentration, intensity)
    colnames(hold.frame) <- c("Spectrum", "Concentration", "Intensity")
    hold.frame[hold.frame==999] <- NA
    hold.frame <- na.omit(hold.frame)
    
    concentration <- hold.frame$Concentration
    intensity <- hold.frame$Intensity
    
    data <- data[data$Spectrum %in% hold.frame$Spectrum, ]
    
    spectra.line.table <- spectra.line.table[spectra.line.table$Spectrum %in% hold.frame$Spectrum, ]
    
    
    
    
    
    
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
    
    
    predict.frame <- data.frame(concentration, intensity)
    colnames(predict.frame) <- c("Concentration", "Intensity")
    
    predict.intensity <- if(dataType()=="Spectra"){
        general.prep(spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
    } else if(dataType()=="Net"){
        general.prep.net(spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
    }
    
    
    if(input$normcal==2){
        
        
        predict.intensity.tc <- if(dataType()=="Spectra"){
            simple.tc.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
        } else if(dataType()=="Net"){
            simple.tc.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
        }
        
        predict.frame.tc <- data.frame(predict.intensity.tc, predict.frame$Concentration)
        colnames(predict.frame.tc) <- c(names(predict.intensity.tc), "Concentration")
        
        
    }
    
    if(input$normcal==3){
        
        predict.intensity.comp <- if(dataType()=="Spectra"){
            simple.comp.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, norm.min=input$comptonmin, norm.max=input$comptonmax)
        } else if(dataType()=="Net"){
            simple.comp.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, norm.min=input$comptonmin, norm.max=input$comptonmax)
        }
        
        predict.frame.comp <- data.frame(predict.intensity.comp, predict.frame$Concentration)
        colnames(predict.frame.comp) <- c(names(predict.intensity.comp), "Concentration")
    }
    
    
    
    
    if (input$normcal==1){
        
        cal.lm <- lm(Concentration~Intensity, data=predict.frame[ vals$keeprows, , drop = FALSE])
        cal.lm.poly <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame[ vals$keeprows, , drop = FALSE])
        
        cal.est.conc.pred <- predict(object=cal.lm, newdata=predict.intensity, interval='confidence')
        cal.est.conc.tab <- data.frame(cal.est.conc.pred)
        cal.est.conc <- cal.est.conc.tab$fit
        
        cal.est.poly.conc.pred <- predict(object=cal.lm.poly, newdata=predict.intensity, interval='confidence')
        cal.est.poly.conc.tab <- data.frame(cal.est.poly.conc.pred)
        cal.est.poly.conc <- cal.est.poly.conc.tab$fit
        
        val.frame <- data.frame(predict.frame$Concentration, cal.est.conc)
        colnames(val.frame) <- c("Concentration", "Prediction")
        
        
        val.frame.poly <- data.frame(predict.frame$Concentration, cal.est.poly.conc)
        colnames(val.frame.poly) <- c("Concentration", "Prediction")
        
    }
    
    
    if (input$normcal==3){
        
        
        
        cal.lm.comp <- lm(Concentration~Intensity, data=predict.frame.comp[ vals$keeprows, , drop = FALSE])
        
        cal.lm.poly.comp <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame.comp[ vals$keeprows, , drop = FALSE])
        
        cal.est.conc.pred.comp <- predict(object=cal.lm.comp, newdata=predict.intensity.comp, interval='confidence')
        cal.est.conc.tab.comp <- data.frame(cal.est.conc.pred.comp)
        cal.est.conc.comp <- cal.est.conc.tab.comp$fit
        
        cal.est.poly.conc.pred.comp <- predict(object=cal.lm.poly.comp, newdata=predict.intensity.comp, interval='confidence')
        cal.est.poly.conc.tab.comp <- data.frame(cal.est.poly.conc.pred.comp)
        cal.est.poly.conc.comp <- cal.est.poly.conc.tab.comp$fit
        
        val.frame.comp <- data.frame(predict.frame$Concentration, cal.est.conc.comp)
        colnames(val.frame.comp) <- c("Concentration", "Prediction")
        
        
        
        val.frame.poly.comp <- data.frame(predict.frame$Concentration, cal.est.poly.conc.comp)
        colnames(val.frame.poly.comp) <- c("Concentration", "Prediction")
        
    }
    
    
    
    if (input$normcal==2){
        
        cal.lm.tc <- lm(Concentration~Intensity, data=predict.frame.tc[ vals$keeprows, , drop = FALSE])
        cal.lm.poly.tc <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame.tc[ vals$keeprows, , drop = FALSE])
        
        cal.est.conc.pred.tc <- predict(object=cal.lm.tc, newdata=predict.intensity.tc, interval='confidence')
        cal.est.conc.tab.tc <- data.frame(cal.est.conc.pred.tc)
        cal.est.conc.tc <- cal.est.conc.tab.tc$fit
        
        cal.est.poly.conc.pred.tc <- predict(object=cal.lm.poly.tc, newdata=predict.intensity.tc, interval='confidence')
        cal.est.poly.conc.tab.tc <- data.frame(cal.est.poly.conc.pred.tc)
        cal.est.poly.conc.tc <- cal.est.poly.conc.tab.tc$fit
        
        val.frame.tc <- data.frame(predict.frame$Concentration, cal.est.conc.tc)
        colnames(val.frame.tc) <- c("Concentration", "Prediction")
        
        
        
        val.frame.poly.tc <- data.frame(predict.frame$Concentration, cal.est.poly.conc.tc)
        colnames(val.frame.poly.tc) <- c("Concentration", "Prediction")
        
        
    }
    
    
    
    
    
    ####Fourth Iteration
    
    if (input$radiocal==3){
        
        
        
        if(input$normcal==1){
            
            predict.intensity.luc <- if(dataType()=="Spectra"){
                lucas.simp.prep(spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
            } else if(dataType()=="Net"){
                lucas.simp.prep.net(spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
            }
            
            predict.frame.luc <- data.frame(predict.intensity.luc, predict.frame$Concentration)
            colnames(predict.frame.luc) <- c(names(predict.intensity.luc), "Concentration")
            
            
            lucas.lm <- lm(Concentration~., data=predict.frame.luc[ vals$keeprows, , drop = FALSE])
            
            
            xmin = 0; xmax=10
            N = length(predict.frame.luc$Concentration)
            means = colMeans(predict.frame.luc)
            dummyDF = t(as.data.frame(means))
            for(i in 2:N){dummyDF=rbind(dummyDF,means)}
            xv=seq(xmin,xmax, length.out=N)
            dummyDF$Concentration = xv
            yv=predict(lucas.lm, newdata=predict.intensity.luc)
            
            
            
            lucas.x <- yv
            
            
            
            cal.est.conc.pred.luc <- predict(object=lucas.lm , newdata=predict.intensity.luc, interval='confidence')
            cal.est.conc.tab.luc <- data.frame(cal.est.conc.pred.luc)
            cal.est.conc.luc <- cal.est.conc.tab.luc$fit
            cal.est.conc.luc.up <- cal.est.conc.tab.luc$upr
            cal.est.conc.luc.low <- cal.est.conc.tab.luc$lwr
            
            
            val.frame.luc <- data.frame(predict.frame$Concentration, predict.intensity.luc$Intensity, lucas.x, cal.est.conc.luc, cal.est.conc.luc.up, cal.est.conc.luc.low)
            colnames(val.frame.luc) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
        }
        
        
        
        
        
        if(input$normcal==3){
            
            
            predict.intensity.luc.comp <- if(dataType()=="Spectra"){
                lucas.comp.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
            } else if(dataType()=="Net"){
                lucas.comp.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
            }
            
            predict.frame.luc.comp <- data.frame(predict.intensity.luc.comp, predict.frame$Concentration)
            colnames(predict.frame.luc.comp) <- c(names(predict.intensity.luc.comp), "Concentration")
            
            
            
            
            lucas.lm.comp <- lm(Concentration~., data=predict.frame.luc.comp[ vals$keeprows, , drop = FALSE])
            
            
            xmin = 0; xmax=10
            N = length(predict.frame.luc.comp$Concentration)
            means = colMeans(predict.frame.luc.comp)
            dummyDF = t(as.data.frame(means))
            for(i in 2:N){dummyDF=rbind(dummyDF,means)}
            xv=seq(xmin,xmax, length.out=N)
            dummyDF$Concentration = xv
            yv=predict(lucas.lm.comp, newdata=predict.intensity.luc.comp)
            
            
            lucas.x.comp <- yv
            
            cal.est.conc.pred.luc.comp <- predict(object=lucas.lm.comp , newdata=predict.intensity.luc.comp, interval='confidence')
            cal.est.conc.tab.luc.comp <- data.frame(cal.est.conc.pred.luc.comp)
            cal.est.conc.luc.comp <- cal.est.conc.tab.luc.comp$fit
            cal.est.conc.luc.up.comp <- cal.est.conc.tab.luc.comp$upr
            cal.est.conc.luc.low.comp <- cal.est.conc.tab.luc.comp$lwr
            
            
            val.frame.luc.comp <- data.frame(predict.frame$Concentration, predict.intensity.luc.comp$Intensity, lucas.x.comp, cal.est.conc.luc.comp, cal.est.conc.luc.up.comp, cal.est.conc.luc.low.comp)
            colnames(val.frame.luc.comp) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
            
        }
        
        
        
        
        if(input$normcal==2){
            
            
            predict.intensity.luc.tc <- if(dataType()=="Spectra"){
                lucas.tc.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
            } else if(dataType()=="Net"){
                lucas.tc.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
            }
            
            predict.frame.luc.tc <- data.frame(predict.intensity.luc.tc, predict.frame$Concentration)
            colnames(predict.frame.luc.tc) <- c(names(predict.intensity.luc.tc), "Concentration")
            
            
            lucas.lm.tc <- lm(Concentration~., data=predict.frame.luc.tc)
            
            xmin = 0; xmax=10
            N = length(predict.frame.luc.tc$Concentration)
            means = colMeans(predict.frame.luc.tc)
            dummyDF = t(as.data.frame(means))
            for(i in 2:N){dummyDF=rbind(dummyDF,means)}
            xv=seq(xmin,xmax, length.out=N)
            dummyDF$Concentration = xv
            yv=predict(lucas.lm.tc, newdata=predict.intensity.luc.tc)
            
            
            lucas.x.tc <- yv
            
            cal.est.conc.pred.luc.tc <- predict(object=lucas.lm.tc , newdata=predict.intensity.luc.tc, interval='confidence')
            cal.est.conc.tab.luc.tc <- data.frame(cal.est.conc.pred.luc.tc)
            cal.est.conc.luc.tc <- cal.est.conc.tab.luc.tc$fit
            cal.est.conc.luc.up.tc <- cal.est.conc.tab.luc.tc$upr
            cal.est.conc.luc.low.tc <- cal.est.conc.tab.luc.tc$lwr
            
            
            val.frame.luc.tc <- data.frame(predict.frame$Concentration, predict.intensity.luc.tc$Intensity, lucas.x.tc, cal.est.conc.luc.tc, cal.est.conc.luc.up.tc, cal.est.conc.luc.low.tc)
            colnames(val.frame.luc.tc) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
            
        }
        
    }

    
    

    
    
    if(input$radiocal==1 && input$normcal==1) {
        val.frame
    } else if (input$radiocal==2 && input$normcal==1){
        val.frame.poly
    } else if (input$radiocal==1 && input$normcal==2){
        val.frame.poly.tc
    } else if (input$radiocal==2 && input$normcal==2) {
        val.frame.tc
    } else if (input$radiocal==1 && input$normcal==3) {
        val.frame.comp
    } else if (input$radiocal==2 && input$normcal==3) {
        val.frame.poly.comp
    } else if (input$radiocal==3 && input$normcal==1) {
        val.frame.luc
    } else if (input$radiocal==3 && input$normcal==2){
        val.frame.luc.tc
    } else if (input$radiocal==3 && input$normcal==3) {
        val.frame.luc.comp
    } else if (is.na(input$calcurveelement)) {
        return()
    }
    
})



randomizeData <- reactive({
    
    cal.frame <- as.data.frame(values[["DF"]], stringsAsFactors=FALSE)
    cal.frame <- cal.frame[ vals$keeprows, , drop = FALSE]
    total.number <- length(cal.frame[,1])
    sample.number <- total.number-round(input$percentrandom*total.number, 0)
    
    hold <- cal.frame[sample(nrow(cal.frame), sample.number),]
    cal.frame$Spectrum %in% hold$Spectrum
    
})

output$testing <- renderUI({
    tagList(as.data.frame(table(randomizeData()))[2,2])
})




calCurvePlot <- reactive({
    
    data <- dataHold()
    
    
    #if(input$usecalfile==TRUE && input$plot_cal_click==FALSE){vals$keeprows <- calFileContents()$calList[[input$calcurveelement]][[1]][[4]]}

    
    
    concentration.table <- as.data.frame(values[["DF"]], stringsAsFactors=FALSE)
    concentration.table[concentration.table==""] <- 999
    
    spectra.line.table <- if(dataType()=="Spectra"){
        spectraData()
    } else if(dataType()=="Net"){
        dataHold()
    }
    
    
    
    
    
    
    
    
    concentration <- as.vector(as.numeric(unlist(concentration.table[input$calcurveelement])))
    
    
    
    intensity <- as.vector(as.numeric(unlist(spectra.line.table[input$calcurveelement])))
    
    spectra.names <- spectra.line.table$Spectrum
    
    # intensity <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
    
    
    # concentration <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
    
    hold.frame <- data.frame(spectra.names, concentration, intensity)
    colnames(hold.frame) <- c("Spectrum", "Concentration", "Intensity")
    hold.frame[hold.frame==999] <- NA
    hold.frame <- na.omit(hold.frame)
    
    concentration <- hold.frame$Concentration
    intensity <- hold.frame$Intensity
    
    data <- data[data$Spectrum %in% hold.frame$Spectrum, ]
    
    spectra.line.table <- spectra.line.table[spectra.line.table$Spectrum %in% hold.frame$Spectrum, ]
    
    
    
    
    
    
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
    
    
    predict.frame <- data.frame(concentration, intensity)
    colnames(predict.frame) <- c("Concentration", "Intensity")
    
    predict.intensity <- if(dataType()=="Spectra"){
        general.prep(spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
    } else if(dataType()=="Net"){
        general.prep.net(spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
    }    
    
    
    if(input$normcal==2){

    
    predict.intensity.tc <- if(dataType()=="Spectra"){
        simple.tc.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
    } else if(dataType()=="Net"){
        simple.tc.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
    }
    
    predict.frame.tc <- data.frame(predict.intensity.tc, predict.frame$Concentration)
    colnames(predict.frame.tc) <- c(names(predict.intensity.tc), "Concentration")
    
    
    }
    
    if(input$normcal==3){
        
        predict.intensity.comp <- if(dataType()=="Spectra"){
            simple.comp.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, norm.min=input$comptonmin, norm.max=input$comptonmax)
        } else if(dataType()=="Net"){
            simple.comp.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, norm.min=input$comptonmin, norm.max=input$comptonmax)
        }
        
        predict.frame.comp <- data.frame(predict.intensity.comp, predict.frame$Concentration)
        colnames(predict.frame.comp) <- c(names(predict.intensity.comp), "Concentration")
    }
    
    


    if (input$normcal==1){
    cal.lm <- lm(Concentration~Intensity, data=predict.frame[ vals$keeprows, , drop = FALSE])
    cal.lm.poly <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame[ vals$keeprows, , drop = FALSE])
    
    cal.est.conc.pred <- predict(object=cal.lm, newdata=predict.intensity, interval='confidence')
    cal.est.conc.tab <- data.frame(cal.est.conc.pred)
    cal.est.conc <- cal.est.conc.tab$fit
    
    cal.est.poly.conc.pred <- predict(object=cal.lm.poly, newdata=predict.intensity, interval='confidence')
    cal.est.poly.conc.tab <- data.frame(cal.est.poly.conc.pred)
    cal.est.poly.conc <- cal.est.poly.conc.tab$fit
    
    val.frame <- data.frame(predict.frame$Concentration, cal.est.conc)
    colnames(val.frame) <- c("Concentration", "Prediction")
    
    
    val.frame.poly <- data.frame(predict.frame$Concentration, cal.est.poly.conc)
    colnames(val.frame.poly) <- c("Concentration", "Prediction")
    
    }
    
    
    if (input$normcal==3){


    
    cal.lm.comp <- lm(Concentration~Intensity, data=predict.frame.comp[ vals$keeprows, , drop = FALSE])
    
    cal.lm.poly.comp <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame.comp[ vals$keeprows, , drop = FALSE])
    
    cal.est.conc.pred.comp <- predict(object=cal.lm.comp, newdata=predict.intensity.comp, interval='confidence')
    cal.est.conc.tab.comp <- data.frame(cal.est.conc.pred.comp)
    cal.est.conc.comp <- cal.est.conc.tab.comp$fit
    
    cal.est.poly.conc.pred.comp <- predict(object=cal.lm.poly.comp, newdata=predict.intensity.comp, interval='confidence')
    cal.est.poly.conc.tab.comp <- data.frame(cal.est.poly.conc.pred.comp)
    cal.est.poly.conc.comp <- cal.est.poly.conc.tab.comp$fit
    
    val.frame.comp <- data.frame(predict.frame$Concentration, cal.est.conc.comp)
    colnames(val.frame.comp) <- c("Concentration", "Prediction")
    
    
    
    val.frame.poly.comp <- data.frame(predict.frame$Concentration, cal.est.poly.conc.comp)
    colnames(val.frame.poly.comp) <- c("Concentration", "Prediction")
    
    }
    
    
    
    if (input$normcal==2){
    
    cal.lm.tc <- lm(Concentration~Intensity, data=predict.frame.tc[ vals$keeprows, , drop = FALSE])
    cal.lm.poly.tc <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame.tc[ vals$keeprows, , drop = FALSE])
    
    cal.est.conc.pred.tc <- predict(object=cal.lm.tc, newdata=predict.intensity.tc, interval='confidence')
    cal.est.conc.tab.tc <- data.frame(cal.est.conc.pred.tc)
    cal.est.conc.tc <- cal.est.conc.tab.tc$fit
    
    cal.est.poly.conc.pred.tc <- predict(object=cal.lm.poly.tc, newdata=predict.intensity.tc, interval='confidence')
    cal.est.poly.conc.tab.tc <- data.frame(cal.est.poly.conc.pred.tc)
    cal.est.poly.conc.tc <- cal.est.poly.conc.tab.tc$fit
    
    val.frame.tc <- data.frame(predict.frame$Concentration, cal.est.conc.tc)
    colnames(val.frame.tc) <- c("Concentration", "Prediction")
    
    
    
    val.frame.poly.tc <- data.frame(predict.frame$Concentration, cal.est.poly.conc.tc)
    colnames(val.frame.poly.tc) <- c("Concentration", "Prediction")
    
    
    }
    


    
    
    ####Fourth Iteration
    
    if (input$radiocal==3){
        

        
        if(input$normcal==1){
            
            predict.intensity.luc <- if(dataType()=="Spectra"){
                lucas.simp.prep(spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
            } else if(dataType()=="Net"){
                lucas.simp.prep.net(spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
            }
            
            predict.frame.luc <- data.frame(predict.intensity.luc, predict.frame$Concentration)
            colnames(predict.frame.luc) <- c(names(predict.intensity.luc), "Concentration")
            
            
    lucas.lm <- lm(Concentration~., data=predict.frame.luc[ vals$keeprows, , drop = FALSE])
    
    
    xmin = 0; xmax=10
    N = length(predict.frame.luc$Concentration)
    means = colMeans(predict.frame.luc)
    dummyDF = t(as.data.frame(means))
    for(i in 2:N){dummyDF=rbind(dummyDF,means)}
    xv=seq(xmin,xmax, length.out=N)
    dummyDF$Concentration = xv
    yv=predict(lucas.lm, newdata=predict.intensity.luc)

    
    
    lucas.x <- yv



    cal.est.conc.pred.luc <- predict(object=lucas.lm , newdata=predict.intensity.luc, interval='confidence')
    cal.est.conc.tab.luc <- data.frame(cal.est.conc.pred.luc)
    cal.est.conc.luc <- cal.est.conc.tab.luc$fit
    cal.est.conc.luc.up <- cal.est.conc.tab.luc$upr
    cal.est.conc.luc.low <- cal.est.conc.tab.luc$lwr
    
    
    val.frame.luc <- data.frame(predict.frame$Concentration, predict.intensity.luc$Intensity, lucas.x, cal.est.conc.luc, cal.est.conc.luc.up, cal.est.conc.luc.low)
    colnames(val.frame.luc) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
        }
    

    
    
    
    if(input$normcal==3){
        
        
        predict.intensity.luc.comp <- if(dataType()=="Spectra"){
            lucas.comp.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
        } else if(dataType()=="Net"){
            lucas.comp.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
        }
        
        predict.frame.luc.comp <- data.frame(predict.intensity.luc.comp, predict.frame$Concentration)
        colnames(predict.frame.luc.comp) <- c(names(predict.intensity.luc.comp), "Concentration")
        


    
    lucas.lm.comp <- lm(Concentration~., data=predict.frame.luc.comp[ vals$keeprows, , drop = FALSE])
    
    
    xmin = 0; xmax=10
    N = length(predict.frame.luc.comp$Concentration)
    means = colMeans(predict.frame.luc.comp)
    dummyDF = t(as.data.frame(means))
    for(i in 2:N){dummyDF=rbind(dummyDF,means)}
    xv=seq(xmin,xmax, length.out=N)
    dummyDF$Concentration = xv
    yv=predict(lucas.lm.comp, newdata=predict.intensity.luc.comp)
    
    
    lucas.x.comp <- yv
    
    cal.est.conc.pred.luc.comp <- predict(object=lucas.lm.comp , newdata=predict.intensity.luc.comp, interval='confidence')
    cal.est.conc.tab.luc.comp <- data.frame(cal.est.conc.pred.luc.comp)
    cal.est.conc.luc.comp <- cal.est.conc.tab.luc.comp$fit
    cal.est.conc.luc.up.comp <- cal.est.conc.tab.luc.comp$upr
    cal.est.conc.luc.low.comp <- cal.est.conc.tab.luc.comp$lwr
    
    
    val.frame.luc.comp <- data.frame(predict.frame$Concentration, predict.intensity.luc.comp$Intensity, lucas.x.comp, cal.est.conc.luc.comp, cal.est.conc.luc.up.comp, cal.est.conc.luc.low.comp)
    colnames(val.frame.luc.comp) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
    
    }
    
    
    
    
    if(input$normcal==2){
        
        
    predict.intensity.luc.tc <- if(dataType()=="Spectra"){
        lucas.tc.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
    } else if(dataType()=="Net"){
        lucas.tc.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
    }
    
    predict.frame.luc.tc <- data.frame(predict.intensity.luc.tc, predict.frame$Concentration)
    colnames(predict.frame.luc.tc) <- c(names(predict.intensity.luc.tc), "Concentration")
    

    lucas.lm.tc <- lm(Concentration~., data=predict.frame.luc.tc)
    
    xmin = 0; xmax=10
    N = length(predict.frame.luc.tc$Concentration)
    means = colMeans(predict.frame.luc.tc)
    dummyDF = t(as.data.frame(means))
    for(i in 2:N){dummyDF=rbind(dummyDF,means)}
    xv=seq(xmin,xmax, length.out=N)
    dummyDF$Concentration = xv
    yv=predict(lucas.lm.tc, newdata=predict.intensity.luc.tc)
    
    
    lucas.x.tc <- yv
    
    cal.est.conc.pred.luc.tc <- predict(object=lucas.lm.tc , newdata=predict.intensity.luc.tc, interval='confidence')
    cal.est.conc.tab.luc.tc <- data.frame(cal.est.conc.pred.luc.tc)
    cal.est.conc.luc.tc <- cal.est.conc.tab.luc.tc$fit
    cal.est.conc.luc.up.tc <- cal.est.conc.tab.luc.tc$upr
    cal.est.conc.luc.low.tc <- cal.est.conc.tab.luc.tc$lwr
    
    
    val.frame.luc.tc <- data.frame(predict.frame$Concentration, predict.intensity.luc.tc$Intensity, lucas.x.tc, cal.est.conc.luc.tc, cal.est.conc.luc.up.tc, cal.est.conc.luc.low.tc)
    colnames(val.frame.luc.tc) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
    
    }
    
    }
    
    
    
    
    if (input$radiocal!=3){


    ####Linear Calibration Curve Model Display, Time Normalized
    if(input$normcal==1){
    calcurve.linear.plot <- ggplot(data=predict.frame[ vals$keeprows, , drop = FALSE], aes(Intensity, Concentration)) +
    theme_light() +
    annotate("text", label=lm_eqn(lm(Concentration~Intensity, predict.frame[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
    geom_point() +
    geom_point(data = predict.frame[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
    stat_smooth(method="lm", fullrange = TRUE) +
    scale_x_continuous(paste(element.name, intens)) +
    scale_y_continuous(paste(element.name, conen))

    
    calcurve.poly.plot <- ggplot(data=predict.frame[ vals$keeprows, , drop = FALSE], aes(Intensity, Concentration)) +
    theme_light() +
    annotate("text", label=lm_eqn_poly(lm(Concentration~Intensity + I(Intensity^2), predict.frame[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
    geom_point() +
    geom_point(data = predict.frame[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
    stat_smooth(method="lm", formula=y~poly(x,2)) +
    scale_x_continuous(paste(element.name, intens)) +
    scale_y_continuous(paste(element.name, conen))
    }

    

    ####Linear Calibration Curve Model Display, Total Count Normalized
    if(input$normcal==2){
    calcurve.linear.plot.tc <- ggplot(data=predict.frame.tc[ vals$keeprows, , drop = FALSE], aes(Intensity, Concentration)) +
    theme_light() +
    annotate("text", label=lm_eqn(lm(Concentration~Intensity, predict.frame.tc[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
    geom_point() +
    geom_point(data = predict.frame.tc[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
    stat_smooth(method="lm") +
    scale_x_continuous(paste(element.name, norma.tc)) +
    scale_y_continuous(paste(element.name, conen))
    
    calcurve.poly.plot.tc <- ggplot(data=predict.frame.tc, aes(Intensity, Concentration)) +
    theme_light() +
    annotate("text", label=lm_eqn_poly(lm(Concentration~Intensity + I(Intensity^2), predict.frame.tc[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
    geom_point() +
    geom_point(data = predict.frame.tc[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
    stat_smooth(method="lm", formula=y~poly(x,2)) +
    scale_x_continuous(paste(element.name, norma.tc)) +
    scale_y_continuous(paste(element.name, conen))
    }
    
    
    ####Linear Calibration Curve Model Display, Compton Normalized
    if(input$normcal==3){
    calcurve.linear.plot.comp <- ggplot(data=predict.frame.comp[ vals$keeprows, , drop = FALSE], aes(Intensity, Concentration)) +
    theme_light() +
    annotate("text", label=lm_eqn(lm(Concentration~Intensity, predict.frame.comp[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
    geom_point() +
    geom_point(data = predict.frame.comp[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
    stat_smooth(method="lm") +
    scale_x_continuous(paste(element.name, norma.comp)) +
    scale_y_continuous(paste(element.name, conen))
    
    calcurve.poly.plot.comp <- ggplot(data=predict.frame.comp[ vals$keeprows, , drop = FALSE], aes(Intensity, Concentration)) +
    theme_light() +
    annotate("text", label=lm_eqn_poly(lm(Concentration~Intensity + I(Intensity^2), predict.frame.comp)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
    geom_point() +
    geom_point(data = predict.frame.comp[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
    stat_smooth(method="lm", formula=y~poly(x,2)) +
    scale_x_continuous(paste(element.name, norma.comp)) +
    scale_y_continuous(paste(element.name, conen))
    }
    
    }
    
    
    if (input$radiocal==3){
    
    ####lucas-Tooth  Calibration Curve Model Display, Time Normalized
    
    if(input$normcal==1){
    calcurve.linear.plot.luc <- ggplot(data=val.frame.luc[ vals$keeprows, , drop = FALSE], aes(IntensityNorm, Concentration)) +
    theme_light() +
    annotate("text", label=lm_eqn(lm(Concentration~., val.frame.luc[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
    geom_point() +
    geom_point(aes(IntensityNorm, Concentration), data = val.frame.luc[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
    geom_smooth(aes(x=IntensityNorm, y=Concentration, ymin = Lower, ymax = Upper)) +
    scale_x_continuous(paste(element.name, norma)) +
    scale_y_continuous(paste(element.name, conen))
    }
    
    
    
    ####lucas-Tooth Calibration Curve Model Display, Total Count Normalized
    if(input$normcal==2){
        
        
    calcurve.linear.plot.luc.tc <- ggplot(data=val.frame.luc.tc[ vals$keeprows, , drop = FALSE], aes(IntensityNorm, Concentration)) +
    theme_light() +
    annotate("text", label=lm_eqn(lm(Concentration~., predict.frame.luc.tc[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
    geom_point() +
    geom_point(aes(IntensityNorm, Concentration), data = val.frame.luc.tc[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
    geom_smooth(aes(x=IntensityNorm, y=Concentration, ymin = Lower, ymax = Upper)) +
    scale_x_continuous(paste(element.name, norma.tc)) +
    scale_y_continuous(paste(element.name, conen))
    }
    

    
    ####lucas-Tooth Calibration Curve Model Display, Compton Normalized
    
    if(input$normcal==3){
    calcurve.linear.plot.luc.comp <- ggplot(data=val.frame.luc.comp[ vals$keeprows, , drop = FALSE], aes(IntensityNorm, Concentration)) +
    theme_light() +
    annotate("text", label=lm_eqn(lm(Concentration~., predict.frame.luc.comp[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
    geom_point() +
    geom_point(aes(IntensityNorm, Concentration), data = val.frame.luc.comp[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
    geom_smooth(aes(x=IntensityNorm, y=Concentration, ymin = Lower, ymax = Upper)) +
    scale_x_continuous(paste(element.name, norma.comp)) +
    scale_y_continuous(paste(element.name, conen))
    
    }
    
    }
    
    
    if(input$radiocal==1 && input$normcal==1) {
        calcurve.linear.plot
    } else if (input$radiocal==2 && input$normcal==1){
        calcurve.poly.plot
    } else if (input$radiocal==1 && input$normcal==2){
        calcurve.linear.plot.tc
    } else if (input$radiocal==2 && input$normcal==2) {
        calcurve.poly.plot.tc
    } else if (input$radiocal==1 && input$normcal==3) {
        calcurve.linear.plot.comp
    } else if (input$radiocal==2 && input$normcal==3) {
        calcurve.poly.plot.comp
    } else if (input$radiocal==3 && input$normcal==1) {
        calcurve.linear.plot.luc
    } else if (input$radiocal==3 && input$normcal==2){
        calcurve.linear.plot.luc.tc
    } else if (input$radiocal==3 && input$normcal==3) {
        calcurve.linear.plot.luc.comp
    } else if (is.na(input$calcurveelement)) {
        return()
    }
    
})



output$calcurveplots <- renderPlot({
    calCurvePlot()
})


valCurvePlot <- reactive({
    
    data <- dataHold()
    
    
    #if(input$usecalfile==TRUE && input$plot_cal_click==FALSE){vals$keeprows <- calFileContents()$calList[[input$calcurveelement]][[1]][[4]]}
    
    
    
    concentration.table <- as.data.frame(values[["DF"]], stringsAsFactors=FALSE)
    concentration.table[concentration.table==""] <- 999
    
    spectra.line.table <- if(dataType()=="Spectra"){
        spectraData()
    } else if(dataType()=="Net"){
        dataHold()
    }
    
    
    
    
    
    
    
    
    concentration <- as.vector(as.numeric(unlist(concentration.table[input$calcurveelement])))
    
    
    
    intensity <- as.vector(as.numeric(unlist(spectra.line.table[input$calcurveelement])))
    
    spectra.names <- spectra.line.table$Spectrum
    
    # intensity <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
    
    
    # concentration <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
    
    hold.frame <- data.frame(spectra.names, concentration, intensity)
    colnames(hold.frame) <- c("Spectrum", "Concentration", "Intensity")
    hold.frame[hold.frame==999] <- NA
    hold.frame <- na.omit(hold.frame)
    
    concentration <- hold.frame$Concentration
    intensity <- hold.frame$Intensity
    
    data <- data[data$Spectrum %in% hold.frame$Spectrum, ]
    
    spectra.line.table <- spectra.line.table[spectra.line.table$Spectrum %in% hold.frame$Spectrum, ]
    
    
    
    
    
    
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
    
    
    predict.frame <- data.frame(concentration, intensity)
    colnames(predict.frame) <- c("Concentration", "Intensity")
    
    predict.intensity <- if(dataType()=="Spectra"){
        general.prep(spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
    } else if(dataType()=="Net"){
        general.prep.net(spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
    }
    
    
    if(input$normcal==2){
        
        
        predict.intensity.tc <- if(dataType()=="Spectra"){
            simple.tc.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
        } else if(dataType()=="Net"){
            simple.tc.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
        }
        
        predict.frame.tc <- data.frame(predict.intensity.tc, predict.frame$Concentration)
        colnames(predict.frame.tc) <- c(names(predict.intensity.tc), "Concentration")
        
        
    }
    
    if(input$normcal==3){
        
        predict.intensity.comp <- if(dataType()=="Spectra"){
            simple.comp.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, norm.min=input$comptonmin, norm.max=input$comptonmax)
        } else if(dataType()=="Net"){
            simple.comp.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, norm.min=input$comptonmin, norm.max=input$comptonmax)
        }
        
        predict.frame.comp <- data.frame(predict.intensity.comp, predict.frame$Concentration)
        colnames(predict.frame.comp) <- c(names(predict.intensity.comp), "Concentration")
    }
    
    
    
    
    if (input$normcal==1){
        
        cal.lm <- lm(Concentration~Intensity, data=predict.frame[ vals$keeprows, , drop = FALSE])
        cal.lm.poly <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame[ vals$keeprows, , drop = FALSE])
        
        cal.est.conc.pred <- predict(object=cal.lm, newdata=predict.intensity, interval='confidence')
        cal.est.conc.tab <- data.frame(cal.est.conc.pred)
        cal.est.conc <- cal.est.conc.tab$fit
        
        cal.est.poly.conc.pred <- predict(object=cal.lm.poly, newdata=predict.intensity, interval='confidence')
        cal.est.poly.conc.tab <- data.frame(cal.est.poly.conc.pred)
        cal.est.poly.conc <- cal.est.poly.conc.tab$fit
        
        val.frame <- data.frame(predict.frame$Concentration, cal.est.conc)
        colnames(val.frame) <- c("Concentration", "Prediction")
        
        
        val.frame.poly <- data.frame(predict.frame$Concentration, cal.est.poly.conc)
        colnames(val.frame.poly) <- c("Concentration", "Prediction")
        
    }
    
    
    if (input$normcal==3){
        
        
        
        cal.lm.comp <- lm(Concentration~Intensity, data=predict.frame.comp[ vals$keeprows, , drop = FALSE])
        
        cal.lm.poly.comp <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame.comp[ vals$keeprows, , drop = FALSE])
        
        cal.est.conc.pred.comp <- predict(object=cal.lm.comp, newdata=predict.intensity.comp, interval='confidence')
        cal.est.conc.tab.comp <- data.frame(cal.est.conc.pred.comp)
        cal.est.conc.comp <- cal.est.conc.tab.comp$fit
        
        cal.est.poly.conc.pred.comp <- predict(object=cal.lm.poly.comp, newdata=predict.intensity.comp, interval='confidence')
        cal.est.poly.conc.tab.comp <- data.frame(cal.est.poly.conc.pred.comp)
        cal.est.poly.conc.comp <- cal.est.poly.conc.tab.comp$fit
        
        val.frame.comp <- data.frame(predict.frame$Concentration, cal.est.conc.comp)
        colnames(val.frame.comp) <- c("Concentration", "Prediction")
        
        
        
        val.frame.poly.comp <- data.frame(predict.frame$Concentration, cal.est.poly.conc.comp)
        colnames(val.frame.poly.comp) <- c("Concentration", "Prediction")
        
    }
    
    
    
    if (input$normcal==2){
        
        cal.lm.tc <- lm(Concentration~Intensity, data=predict.frame.tc[ vals$keeprows, , drop = FALSE])
        cal.lm.poly.tc <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame.tc[ vals$keeprows, , drop = FALSE])
        
        cal.est.conc.pred.tc <- predict(object=cal.lm.tc, newdata=predict.intensity.tc, interval='confidence')
        cal.est.conc.tab.tc <- data.frame(cal.est.conc.pred.tc)
        cal.est.conc.tc <- cal.est.conc.tab.tc$fit
        
        cal.est.poly.conc.pred.tc <- predict(object=cal.lm.poly.tc, newdata=predict.intensity.tc, interval='confidence')
        cal.est.poly.conc.tab.tc <- data.frame(cal.est.poly.conc.pred.tc)
        cal.est.poly.conc.tc <- cal.est.poly.conc.tab.tc$fit
        
        val.frame.tc <- data.frame(predict.frame$Concentration, cal.est.conc.tc)
        colnames(val.frame.tc) <- c("Concentration", "Prediction")
        
        
        
        val.frame.poly.tc <- data.frame(predict.frame$Concentration, cal.est.poly.conc.tc)
        colnames(val.frame.poly.tc) <- c("Concentration", "Prediction")
        
        
    }
    
    
    
    
    
    ####Fourth Iteration
    
    if (input$radiocal==3){
        
        
        
        if(input$normcal==1){
            
            predict.intensity.luc <- if(dataType()=="Spectra"){
                lucas.simp.prep(spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
            } else if(dataType()=="Net"){
                lucas.simp.prep.net(spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
            }
            
            predict.frame.luc <- data.frame(predict.intensity.luc, predict.frame$Concentration)
            colnames(predict.frame.luc) <- c(names(predict.intensity.luc), "Concentration")
            
            
            lucas.lm <- lm(Concentration~., data=predict.frame.luc[ vals$keeprows, , drop = FALSE])
            
            
            
            
            
            xmin = 0; xmax=10
            N = length(predict.frame.luc$Concentration)
            means = colMeans(predict.frame.luc)
            dummyDF = t(as.data.frame(means))
            for(i in 2:N){dummyDF=rbind(dummyDF,means)}
            xv=seq(xmin,xmax, length.out=N)
            dummyDF$Concentration = xv
            yv=predict(lucas.lm, newdata=predict.intensity.luc)
            
            
            
            lucas.x <- yv
            
            
            
            cal.est.conc.pred.luc <- predict(object=lucas.lm , newdata=predict.intensity.luc, interval='confidence')
            cal.est.conc.tab.luc <- data.frame(cal.est.conc.pred.luc)
            cal.est.conc.luc <- cal.est.conc.tab.luc$fit
            cal.est.conc.luc.up <- cal.est.conc.tab.luc$upr
            cal.est.conc.luc.low <- cal.est.conc.tab.luc$lwr
            
            
            val.frame.luc <- data.frame(predict.frame$Concentration, predict.intensity.luc$Intensity, lucas.x, cal.est.conc.luc, cal.est.conc.luc.up, cal.est.conc.luc.low)
            colnames(val.frame.luc) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
        }
        
        
        
        
        
        if(input$normcal==3){
            
            
            predict.intensity.luc.comp <- if(dataType()=="Spectra"){
                lucas.comp.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
            } else if(dataType()=="Net"){
                lucas.comp.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
            }
            
            predict.frame.luc.comp <- data.frame(predict.intensity.luc.comp, predict.frame$Concentration)
            colnames(predict.frame.luc.comp) <- c(names(predict.intensity.luc.comp), "Concentration")
            
            
            
            
            lucas.lm.comp <- lm(Concentration~., data=predict.frame.luc.comp[ vals$keeprows, , drop = FALSE])
            
            
            xmin = 0; xmax=10
            N = length(predict.frame.luc.comp$Concentration)
            means = colMeans(predict.frame.luc.comp)
            dummyDF = t(as.data.frame(means))
            for(i in 2:N){dummyDF=rbind(dummyDF,means)}
            xv=seq(xmin,xmax, length.out=N)
            dummyDF$Concentration = xv
            yv=predict(lucas.lm.comp, newdata=predict.intensity.luc.comp)
            
            
            lucas.x.comp <- yv
            
            cal.est.conc.pred.luc.comp <- predict(object=lucas.lm.comp , newdata=predict.intensity.luc.comp, interval='confidence')
            cal.est.conc.tab.luc.comp <- data.frame(cal.est.conc.pred.luc.comp)
            cal.est.conc.luc.comp <- cal.est.conc.tab.luc.comp$fit
            cal.est.conc.luc.up.comp <- cal.est.conc.tab.luc.comp$upr
            cal.est.conc.luc.low.comp <- cal.est.conc.tab.luc.comp$lwr
            
            
            val.frame.luc.comp <- data.frame(predict.frame$Concentration, predict.intensity.luc.comp$Intensity, lucas.x.comp, cal.est.conc.luc.comp, cal.est.conc.luc.up.comp, cal.est.conc.luc.low.comp)
            colnames(val.frame.luc.comp) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
            
        }
        
        
        
        
        if(input$normcal==2){
            
            
            predict.intensity.luc.tc <- if(dataType()=="Spectra"){
                lucas.tc.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
            } else if(dataType()=="Net"){
                lucas.tc.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
            }
            
            predict.frame.luc.tc <- data.frame(predict.intensity.luc.tc, predict.frame$Concentration)
            colnames(predict.frame.luc.tc) <- c(names(predict.intensity.luc.tc), "Concentration")
            
            
            lucas.lm.tc <- lm(Concentration~., data=predict.frame.luc.tc)
            
            xmin = 0; xmax=10
            N = length(predict.frame.luc.tc$Concentration)
            means = colMeans(predict.frame.luc.tc)
            dummyDF = t(as.data.frame(means))
            for(i in 2:N){dummyDF=rbind(dummyDF,means)}
            xv=seq(xmin,xmax, length.out=N)
            dummyDF$Concentration = xv
            yv=predict(lucas.lm.tc, newdata=predict.intensity.luc.tc)
            
            
            lucas.x.tc <- yv
            
            cal.est.conc.pred.luc.tc <- predict(object=lucas.lm.tc , newdata=predict.intensity.luc.tc, interval='confidence')
            cal.est.conc.tab.luc.tc <- data.frame(cal.est.conc.pred.luc.tc)
            cal.est.conc.luc.tc <- cal.est.conc.tab.luc.tc$fit
            cal.est.conc.luc.up.tc <- cal.est.conc.tab.luc.tc$upr
            cal.est.conc.luc.low.tc <- cal.est.conc.tab.luc.tc$lwr
            
            
            val.frame.luc.tc <- data.frame(predict.frame$Concentration, predict.intensity.luc.tc$Intensity, lucas.x.tc, cal.est.conc.luc.tc, cal.est.conc.luc.up.tc, cal.est.conc.luc.low.tc)
            colnames(val.frame.luc.tc) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
            
        }
        
    }
    
    if (input$radiocal!=3){
        
        
        ####Linear Calibration Curve Model Display, Time Normalized
        if(input$normcal==1){
            
            calval.linear.plot <- ggplot(data=val.frame[ vals$keeprows, , drop = FALSE], aes(Prediction, Concentration)) +
            theme_bw() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            stat_smooth(method="lm") +
            geom_abline(intercept=0, slope=1, lty=2) +
            geom_point() +
            geom_point(aes(Prediction, Concentration), data = val.frame[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
            scale_x_continuous(paste(element.name, predi)) +
            scale_y_continuous(paste(element.name, conen))
            
            calval.poly.plot <- ggplot(data=val.frame.poly[ vals$keeprows, , drop = FALSE], aes(Prediction, Concentration)) +
            theme_bw() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.poly[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_abline(intercept=0, slope=1, lty=2) +
            stat_smooth(method="lm") +
            geom_point() +
            geom_point(aes(Prediction, Concentration), data = val.frame.poly[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
            scale_x_continuous(paste(element.name, predi)) +
            scale_y_continuous(paste(element.name, conen))
        }
        
        
        
        ####Linear Calibration Curve Model Display, Total Count Normalized
        if(input$normcal==2){
            
            calval.linear.plot.tc <- ggplot(data=val.frame.tc[ vals$keeprows, , drop = FALSE], aes(Prediction, Concentration)) +
            theme_bw() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.tc[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_abline(intercept=0, slope=1, lty=2) +
            stat_smooth(method="lm") +
            geom_point() +
            geom_point(aes(Prediction, Concentration), data = val.frame.tc[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
            scale_x_continuous(paste(element.name, predi)) +
            scale_y_continuous(paste(element.name, conen))
            
            calval.poly.plot.tc <- ggplot(data=val.frame.poly.tc[ vals$keeprows, , drop = FALSE], aes(Prediction, Concentration)) +
            theme_bw() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.poly.tc[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_abline(intercept=0, slope=1, lty=2) +
            stat_smooth(method="lm") +
            geom_point() +
            geom_point(aes(Prediction, Concentration), data = val.frame.poly.tc[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
            scale_x_continuous(paste(element.name, predi)) +
            scale_y_continuous(paste(element.name, conen))
        }
        
        
        ####Linear Calibration Curve Model Display, Compton Normalized
        if(input$normcal==3){
            
            calval.linear.plot.comp <- ggplot(data=val.frame.comp[ vals$keeprows, , drop = FALSE], aes(Prediction, Concentration)) +
            theme_bw() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.comp[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_abline(intercept=0, slope=1, lty=2) +
            stat_smooth(method="lm") +
            geom_point() +
            geom_point(aes(Prediction, Concentration), data = val.frame.comp[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
            scale_x_continuous(paste(element.name, predi)) +
            scale_y_continuous(paste(element.name, conen))
            
            calval.poly.plot.comp <- ggplot(data=val.frame.poly.comp[ vals$keeprows, , drop = FALSE], aes(Prediction, Concentration)) +
            theme_bw() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.poly.comp[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_abline(intercept=0, slope=1, lty=2) +
            stat_smooth(method="lm") +
            geom_point() +
            geom_point(aes(Prediction, Concentration), data = val.frame.poly.comp[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
            scale_x_continuous(paste(element.name, predi)) +
            scale_y_continuous(paste(element.name, conen))
            
        }
        
    }

    
    
    if (input$radiocal==3){
        
        ####lucas-Tooth  Calibration Curve Model Display, Time Normalized
        
        if(input$normcal==1){
            
            calval.linear.plot.luc <- ggplot(data=val.frame.luc[ vals$keeprows, , drop = FALSE], aes(Prediction, Concentration)) +
            theme_bw() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.luc[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            stat_smooth(method="lm") +
            geom_abline(intercept=0, slope=1, lty=2) +
            geom_point() +
            geom_point(aes(Prediction, Concentration),  data = val.frame.luc[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
            scale_x_continuous(paste(element.name, predi)) +
            scale_y_continuous(paste(element.name, conen))
        }
        
        
        
        ####lucas-Tooth Calibration Curve Model Display, Total Count Normalized
        if(input$normcal==2){

            calval.linear.plot.luc.tc <- ggplot(data=val.frame.luc.tc[ vals$keeprows, , drop = FALSE], aes(Prediction, Concentration)) +
            theme_bw() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.luc.tc[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            stat_smooth(method="lm") +
            geom_abline(intercept=0, slope=1, lty=2) +
            geom_point() +
            geom_point(aes(Prediction, Concentration),  data = val.frame.luc.tc[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
            scale_x_continuous(paste(element.name, predi)) +
            scale_y_continuous(paste(element.name, conen))

        }
        
        
        
        ####lucas-Tooth Calibration Curve Model Display, Compton Normalized
        
        if(input$normcal==3){
            
            calval.linear.plot.luc.comp <- ggplot(data=val.frame.luc.comp[ vals$keeprows, , drop = FALSE], aes(Prediction, Concentration)) +
            theme_bw() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.luc.comp[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_abline(intercept=0, slope=1, lty=2) +
            stat_smooth(method="lm") +
            geom_point() +
            geom_point(aes(Prediction, Concentration),  data = val.frame.luc.comp[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
            scale_x_continuous(paste(element.name, predi)) +
            scale_y_continuous(paste(element.name, conen))
            
        }
        
    }
    
    
    if(input$radiocal==1 && input$normcal==1) {
        calval.linear.plot
    } else if (input$radiocal==2 && input$normcal==1){
        calval.poly.plot
    } else if (input$radiocal==1 && input$normcal==2){
        calval.linear.plot.tc
    } else if (input$radiocal==2 && input$normcal==2) {
        calval.poly.plot.tc
    } else if (input$radiocal==1 && input$normcal==3) {
        calval.linear.plot.comp
    } else if (input$radiocal==2 && input$normcal==3) {
        calval.poly.plot.comp
    } else if (input$radiocal==3 && input$normcal==1) {
        calval.linear.plot.luc
    } else if (input$radiocal==3 && input$normcal==2){
        calval.linear.plot.luc.tc
    } else if (input$radiocal==3 && input$normcal==3) {
        calval.linear.plot.luc.comp
    } else if (is.na(input$calcurveelement)) {
        return()
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




calCurvePlotRandom <- reactive({
    
    data <- dataHold()[ vals$keeprows, , drop = FALSE]
    randomized <- randomizeData()
    
    #if(input$usecalfile==TRUE && input$plot_cal_click==FALSE){vals$keeprows <- calFileContents()$calList[[input$calcurveelement]][[1]][[4]]}
    
    
    
    concentration.table <- as.data.frame(values[["DF"]], stringsAsFactors=FALSE)[ vals$keeprows, , drop = FALSE]
    concentration.table[concentration.table==""] <- 999
    
    spectra.line.table <- if(dataType()=="Spectra"){
        spectraData()[ vals$keeprows, , drop = FALSE]
    } else if(dataType()=="Net"){
        dataHold()[ vals$keeprows, , drop = FALSE]
    }
    
    
    
    
    
    
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
    
    
    
    concentration <- as.vector(as.numeric(unlist(concentration.table[input$calcurveelement])))
    
    
    
    intensity <- as.vector(as.numeric(unlist(spectra.line.table[input$calcurveelement])))
    
    spectra.names <- spectra.line.table$Spectrum
    
    # intensity <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
    
    
    # concentration <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
    
    hold.frame <- data.frame(spectra.names, concentration, intensity)
    colnames(hold.frame) <- c("Spectrum", "Concentration", "Intensity")
    hold.frame[hold.frame==999] <- NA
    hold.frame <- na.omit(hold.frame)
    
    concentration <- hold.frame$Concentration
    intensity <- hold.frame$Intensity
    
    data <- data[data$Spectrum %in% hold.frame$Spectrum, ]
    
    spectra.line.table <- spectra.line.table[spectra.line.table$Spectrum %in% hold.frame$Spectrum, ]
    
        spectra.line.table <- spectra.line.table[randomized, ]
        hold.frame <- hold.frame[randomized, ]
        data <- data[data$Spectrum %in% spectra.line.table$Spectrum, ]

    
    
    
    predict.frame <- data.frame(concentration, intensity)
    colnames(predict.frame) <- c("Concentration", "Intensity")
    predict.frame <- predict.frame[randomized,]
    
   
   

    predict.intensity <- if(dataType()=="Spectra"){
        general.prep(spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
    } else if(dataType()=="Net"){
        general.prep.net(spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
    }
    
    
    if(input$normcal==2){
        
        
        predict.intensity.tc <- if(dataType()=="Spectra"){
            simple.tc.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
        } else if(dataType()=="Net"){
            simple.tc.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
        }
        
        predict.frame.tc <- data.frame(predict.intensity.tc, predict.frame$Concentration)
        colnames(predict.frame.tc) <- c(names(predict.intensity.tc), "Concentration")
        
        
    }
    
    if(input$normcal==3){
        
        predict.intensity.comp <- if(dataType()=="Spectra"){
            simple.comp.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, norm.min=input$comptonmin, norm.max=input$comptonmax)
        } else if(dataType()=="Net"){
            simple.comp.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, norm.min=input$comptonmin, norm.max=input$comptonmax)
        }
        
        predict.frame.comp <- data.frame(predict.intensity.comp, predict.frame$Concentration)
        colnames(predict.frame.comp) <- c(names(predict.intensity.comp), "Concentration")
    }
    
    
    
    
    if (input$normcal==1){
        cal.lm <- lm(Concentration~Intensity, data=predict.frame)
        cal.lm.poly <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame)
        
        cal.est.conc.pred <- predict(object=cal.lm, newdata=predict.intensity, interval='confidence')
        cal.est.conc.tab <- data.frame(cal.est.conc.pred)
        cal.est.conc <- cal.est.conc.tab$fit
        
        cal.est.poly.conc.pred <- predict(object=cal.lm.poly, newdata=predict.intensity, interval='confidence')
        cal.est.poly.conc.tab <- data.frame(cal.est.poly.conc.pred)
        cal.est.poly.conc <- cal.est.poly.conc.tab$fit
        
        val.frame <- data.frame(predict.frame$Concentration, cal.est.conc)
        colnames(val.frame) <- c("Concentration", "Prediction")
        
        
        val.frame.poly <- data.frame(predict.frame$Concentration, cal.est.poly.conc)
        colnames(val.frame.poly) <- c("Concentration", "Prediction")
        
    }
    
    
    if (input$normcal==3){
        
        
        
        cal.lm.comp <- lm(Concentration~Intensity, data=predict.frame.comp)
        
        cal.lm.poly.comp <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame.comp)
        
        cal.est.conc.pred.comp <- predict(object=cal.lm.comp, newdata=predict.intensity.comp, interval='confidence')
        cal.est.conc.tab.comp <- data.frame(cal.est.conc.pred.comp)
        cal.est.conc.comp <- cal.est.conc.tab.comp$fit
        
        cal.est.poly.conc.pred.comp <- predict(object=cal.lm.poly.comp, newdata=predict.intensity.comp, interval='confidence')
        cal.est.poly.conc.tab.comp <- data.frame(cal.est.poly.conc.pred.comp)
        cal.est.poly.conc.comp <- cal.est.poly.conc.tab.comp$fit
        
        val.frame.comp <- data.frame(predict.frame$Concentration, cal.est.conc.comp)
        colnames(val.frame.comp) <- c("Concentration", "Prediction")
        
        
        
        val.frame.poly.comp <- data.frame(predict.frame$Concentration, cal.est.poly.conc.comp)
        colnames(val.frame.poly.comp) <- c("Concentration", "Prediction")
        
    }
    
    
    
    if (input$normcal==2){
        
        cal.lm.tc <- lm(Concentration~Intensity, data=predict.frame.tc)
        cal.lm.poly.tc <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame.tc)
        
        cal.est.conc.pred.tc <- predict(object=cal.lm.tc, newdata=predict.intensity.tc, interval='confidence')
        cal.est.conc.tab.tc <- data.frame(cal.est.conc.pred.tc)
        cal.est.conc.tc <- cal.est.conc.tab.tc$fit
        
        cal.est.poly.conc.pred.tc <- predict(object=cal.lm.poly.tc, newdata=predict.intensity.tc, interval='confidence')
        cal.est.poly.conc.tab.tc <- data.frame(cal.est.poly.conc.pred.tc)
        cal.est.poly.conc.tc <- cal.est.poly.conc.tab.tc$fit
        
        val.frame.tc <- data.frame(predict.frame$Concentration, cal.est.conc.tc)
        colnames(val.frame.tc) <- c("Concentration", "Prediction")
        
        
        
        val.frame.poly.tc <- data.frame(predict.frame$Concentration, cal.est.poly.conc.tc)
        colnames(val.frame.poly.tc) <- c("Concentration", "Prediction")
        
        
    }
    
    
    
    
    
    ####Fourth Iteration
    
    if (input$radiocal==3){
        
        
        
        if(input$normcal==1){
            
            predict.intensity.luc <- if(dataType()=="Spectra"){
                lucas.simp.prep(spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
            } else if(dataType()=="Net"){
                lucas.simp.prep.net(spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
            }
            
            predict.frame.luc <- data.frame(predict.intensity.luc, predict.frame$Concentration)
            colnames(predict.frame.luc) <- c(names(predict.intensity.luc), "Concentration")
            
            
            lucas.lm <- lm(Concentration~., data=predict.frame.luc)
            
            
            xmin = 0; xmax=10
            N = length(predict.frame.luc$Concentration)
            means = colMeans(predict.frame.luc)
            dummyDF = t(as.data.frame(means))
            for(i in 2:N){dummyDF=rbind(dummyDF,means)}
            xv=seq(xmin,xmax, length.out=N)
            dummyDF$Concentration = xv
            yv=predict(lucas.lm, newdata=predict.intensity.luc)
            
            
            
            lucas.x <- yv
            
            
            
            cal.est.conc.pred.luc <- predict(object=lucas.lm , newdata=predict.intensity.luc, interval='confidence')
            cal.est.conc.tab.luc <- data.frame(cal.est.conc.pred.luc)
            cal.est.conc.luc <- cal.est.conc.tab.luc$fit
            cal.est.conc.luc.up <- cal.est.conc.tab.luc$upr
            cal.est.conc.luc.low <- cal.est.conc.tab.luc$lwr
            
            
            val.frame.luc <- data.frame(predict.frame$Concentration, predict.intensity.luc$Intensity, lucas.x, cal.est.conc.luc, cal.est.conc.luc.up, cal.est.conc.luc.low)
            colnames(val.frame.luc) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
        }
        
        
        
        
        
        if(input$normcal==3){
            
            
            predict.intensity.luc.comp <- if(dataType()=="Spectra"){
                lucas.comp.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
            } else if(dataType()=="Net"){
                lucas.comp.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
            }
            
            predict.frame.luc.comp <- data.frame(predict.intensity.luc.comp, predict.frame$Concentration)
            colnames(predict.frame.luc.comp) <- c(names(predict.intensity.luc.comp), "Concentration")
            
            
            
            
            lucas.lm.comp <- lm(Concentration~., data=predict.frame.luc.comp)
            
            
            xmin = 0; xmax=10
            N = length(predict.frame.luc.comp$Concentration)
            means = colMeans(predict.frame.luc.comp)
            dummyDF = t(as.data.frame(means))
            for(i in 2:N){dummyDF=rbind(dummyDF,means)}
            xv=seq(xmin,xmax, length.out=N)
            dummyDF$Concentration = xv
            yv=predict(lucas.lm.comp, newdata=predict.intensity.luc.comp)
            
            
            lucas.x.comp <- yv
            
            cal.est.conc.pred.luc.comp <- predict(object=lucas.lm.comp , newdata=predict.intensity.luc.comp, interval='confidence')
            cal.est.conc.tab.luc.comp <- data.frame(cal.est.conc.pred.luc.comp)
            cal.est.conc.luc.comp <- cal.est.conc.tab.luc.comp$fit
            cal.est.conc.luc.up.comp <- cal.est.conc.tab.luc.comp$upr
            cal.est.conc.luc.low.comp <- cal.est.conc.tab.luc.comp$lwr
            
            
            val.frame.luc.comp <- data.frame(predict.frame$Concentration, predict.intensity.luc.comp$Intensity, lucas.x.comp, cal.est.conc.luc.comp, cal.est.conc.luc.up.comp, cal.est.conc.luc.low.comp)
            colnames(val.frame.luc.comp) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
            
        }
        
        
        
        
        if(input$normcal==2){
            
            
            predict.intensity.luc.tc <- if(dataType()=="Spectra"){
                lucas.tc.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
            } else if(dataType()=="Net"){
                lucas.tc.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
            }
            
            predict.frame.luc.tc <- data.frame(predict.intensity.luc.tc, predict.frame$Concentration)
            colnames(predict.frame.luc.tc) <- c(names(predict.intensity.luc.tc), "Concentration")
            
            
            lucas.lm.tc <- lm(Concentration~., data=predict.frame.luc.tc)
            
            xmin = 0; xmax=10
            N = length(predict.frame.luc.tc$Concentration)
            means = colMeans(predict.frame.luc.tc)
            dummyDF = t(as.data.frame(means))
            for(i in 2:N){dummyDF=rbind(dummyDF,means)}
            xv=seq(xmin,xmax, length.out=N)
            dummyDF$Concentration = xv
            yv=predict(lucas.lm.tc, newdata=predict.intensity.luc.tc)
            
            
            lucas.x.tc <- yv
            
            cal.est.conc.pred.luc.tc <- predict(object=lucas.lm.tc , newdata=predict.intensity.luc.tc, interval='confidence')
            cal.est.conc.tab.luc.tc <- data.frame(cal.est.conc.pred.luc.tc)
            cal.est.conc.luc.tc <- cal.est.conc.tab.luc.tc$fit
            cal.est.conc.luc.up.tc <- cal.est.conc.tab.luc.tc$upr
            cal.est.conc.luc.low.tc <- cal.est.conc.tab.luc.tc$lwr
            
            
            val.frame.luc.tc <- data.frame(predict.frame$Concentration, predict.intensity.luc.tc$Intensity, lucas.x.tc, cal.est.conc.luc.tc, cal.est.conc.luc.up.tc, cal.est.conc.luc.low.tc)
            colnames(val.frame.luc.tc) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
            
        }
        
    }
    
    
    
    
    if (input$radiocal!=3){
        
        
        ####Linear Calibration Curve Model Display, Time Normalized
        if(input$normcal==1){
            calcurve.linear.plot <- ggplot(data=predict.frame, aes(Intensity, Concentration)) +
            theme_light() +
            annotate("text", label=lm_eqn(lm(Concentration~Intensity, predict.frame)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_point() +
            geom_point(data = predict.frame, shape = 21, fill = "green", color = "black", alpha = 0.25) +
            stat_smooth(method="lm", fullrange = TRUE) +
            scale_x_continuous(paste(element.name, intens)) +
            scale_y_continuous(paste(element.name, conen))
            
            
            calcurve.poly.plot <- ggplot(data=predict.frame, aes(Intensity, Concentration)) +
            theme_light() +
            annotate("text", label=lm_eqn_poly(lm(Concentration~Intensity + I(Intensity^2), predict.frame)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_point() +
            geom_point(data = predict.frame, shape = 21, fill = "green", color = "black", alpha = 0.25) +
            stat_smooth(method="lm", formula=y~poly(x,2)) +
            scale_x_continuous(paste(element.name, intens)) +
            scale_y_continuous(paste(element.name, conen))
        }
        
        
        
        ####Linear Calibration Curve Model Display, Total Count Normalized
        if(input$normcal==2){
            calcurve.linear.plot.tc <- ggplot(data=predict.frame.tc, aes(Intensity, Concentration)) +
            theme_light() +
            annotate("text", label=lm_eqn(lm(Concentration~Intensity, predict.frame.tc)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_point() +
            geom_point(data = predict.frame.tc, shape = 21, fill = "green", color = "black", alpha = 0.25) +
            stat_smooth(method="lm") +
            scale_x_continuous(paste(element.name, norma.tc)) +
            scale_y_continuous(paste(element.name, conen))
            
            calcurve.poly.plot.tc <- ggplot(data=predict.frame.tc, aes(Intensity, Concentration)) +
            theme_light() +
            annotate("text", label=lm_eqn_poly(lm(Concentration~Intensity + I(Intensity^2), predict.frame.tc)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_point() +
            geom_point(data = predict.frame.tc, shape = 21, fill = "green", color = "black", alpha = 0.25) +
            stat_smooth(method="lm", formula=y~poly(x,2)) +
            scale_x_continuous(paste(element.name, norma.tc)) +
            scale_y_continuous(paste(element.name, conen))
        }
        
        
        ####Linear Calibration Curve Model Display, Compton Normalized
        if(input$normcal==3){
            calcurve.linear.plot.comp <- ggplot(data=predict.frame.comp, aes(Intensity, Concentration)) +
            theme_light() +
            annotate("text", label=lm_eqn(lm(Concentration~Intensity, predict.frame.comp)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_point() +
            geom_point(data = predict.frame.comp, shape = 21, fill = "green", color = "black", alpha = 0.25) +
            stat_smooth(method="lm") +
            scale_x_continuous(paste(element.name, norma.comp)) +
            scale_y_continuous(paste(element.name, conen))
            
            calcurve.poly.plot.comp <- ggplot(data=predict.frame.comp, aes(Intensity, Concentration)) +
            theme_light() +
            annotate("text", label=lm_eqn_poly(lm(Concentration~Intensity + I(Intensity^2), predict.frame.comp)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_point() +
            geom_point(data = predict.frame.comp, shape = 21, fill = "green", color = "black", alpha = 0.25) +
            stat_smooth(method="lm", formula=y~poly(x,2)) +
            scale_x_continuous(paste(element.name, norma.comp)) +
            scale_y_continuous(paste(element.name, conen))
        }
        
    }
    
    
    if (input$radiocal==3){
        
        ####lucas-Tooth  Calibration Curve Model Display, Time Normalized
        
        if(input$normcal==1){
            calcurve.linear.plot.luc <- ggplot(data=val.frame.luc, aes(IntensityNorm, Concentration)) +
            theme_light() +
            annotate("text", label=lm_eqn(lm(Concentration~., val.frame.luc)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_point() +
            geom_point(aes(IntensityNorm, Concentration), data = val.frame.luc, shape = 21, fill = "green", color = "black", alpha = 0.25) +
            geom_smooth(aes(x=IntensityNorm, y=Concentration, ymin = Lower, ymax = Upper)) +
            scale_x_continuous(paste(element.name, norma)) +
            scale_y_continuous(paste(element.name, conen))
        }
        
        
        
        ####lucas-Tooth Calibration Curve Model Display, Total Count Normalized
        if(input$normcal==2){
            
            
            calcurve.linear.plot.luc.tc <- ggplot(data=val.frame.luc.tc, aes(IntensityNorm, Concentration)) +
            theme_light() +
            annotate("text", label=lm_eqn(lm(Concentration~., predict.frame.luc.tc)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_point() +
            geom_point(aes(IntensityNorm, Concentration), data = val.frame.luc.tc, shape = 21, fill = "green", color = "black", alpha = 0.25) +
            geom_smooth(aes(x=IntensityNorm, y=Concentration, ymin = Lower, ymax = Upper)) +
            scale_x_continuous(paste(element.name, norma.tc)) +
            scale_y_continuous(paste(element.name, conen))
        }
        
        
        
        ####lucas-Tooth Calibration Curve Model Display, Compton Normalized
        
        if(input$normcal==3){
            calcurve.linear.plot.luc.comp <- ggplot(data=val.frame.luc.comp, aes(IntensityNorm, Concentration)) +
            theme_light() +
            annotate("text", label=lm_eqn(lm(Concentration~., predict.frame.luc.comp)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_point() +
            geom_point(aes(IntensityNorm, Concentration), data = val.frame.luc.comp, shape = 21, fill = "green", color = "black", alpha = 0.25) +
            geom_smooth(aes(x=IntensityNorm, y=Concentration, ymin = Lower, ymax = Upper)) +
            scale_x_continuous(paste(element.name, norma.comp)) +
            scale_y_continuous(paste(element.name, conen))
            
        }
        
    }
    
    
    if(input$radiocal==1 && input$normcal==1) {
        calcurve.linear.plot
    } else if (input$radiocal==2 && input$normcal==1){
        calcurve.poly.plot
    } else if (input$radiocal==1 && input$normcal==2){
        calcurve.linear.plot.tc
    } else if (input$radiocal==2 && input$normcal==2) {
        calcurve.poly.plot.tc
    } else if (input$radiocal==1 && input$normcal==3) {
        calcurve.linear.plot.comp
    } else if (input$radiocal==2 && input$normcal==3) {
        calcurve.poly.plot.comp
    } else if (input$radiocal==3 && input$normcal==1) {
        calcurve.linear.plot.luc
    } else if (input$radiocal==3 && input$normcal==2){
        calcurve.linear.plot.luc.tc
    } else if (input$radiocal==3 && input$normcal==3) {
        calcurve.linear.plot.luc.comp
    } else if (is.na(input$calcurveelement)) {
        return()
    }
    
})



output$calcurveplotsrandom <- renderPlot({
    calCurvePlotRandom()
})



valCurvePlotRandom <- reactive({
    
    data <- dataHold()[ vals$keeprows, , drop = FALSE]
    randomized <- randomizeData()
    
    #if(input$usecalfile==TRUE && input$plot_cal_click==FALSE){vals$keeprows <- calFileContents()$calList[[input$calcurveelement]][[1]][[4]]}
    
    
    
    concentration.table <- as.data.frame(values[["DF"]], stringsAsFactors=FALSE)[ vals$keeprows, , drop = FALSE]
    concentration.table[concentration.table==""] <- 999
    
    spectra.line.table <- if(dataType()=="Spectra"){
        spectraData()[ vals$keeprows, , drop = FALSE]
    } else if(dataType()=="Net"){
        dataHold()[ vals$keeprows, , drop = FALSE]
    }
    
    
    
    
    
    
    
    
    concentration <- as.vector(as.numeric(unlist(concentration.table[input$calcurveelement])))
    
    
    
    intensity <- as.vector(as.numeric(unlist(spectra.line.table[input$calcurveelement])))
    
    spectra.names <- spectra.line.table$Spectrum
    
    # intensity <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
    
    
    # concentration <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
    
    hold.frame <- data.frame(spectra.names, concentration, intensity)
    colnames(hold.frame) <- c("Spectrum", "Concentration", "Intensity")
    hold.frame[hold.frame==999] <- NA
    hold.frame <- na.omit(hold.frame)
    
    concentration <- hold.frame$Concentration
    intensity <- hold.frame$Intensity
    
    data <- data[data$Spectrum %in% hold.frame$Spectrum, ]
    
    spectra.line.table <- spectra.line.table[spectra.line.table$Spectrum %in% hold.frame$Spectrum, ]
    
    spectra.line.table.invert <- spectra.line.table[!(randomized), ]
    hold.frame.invert <- hold.frame[!(randomized), ]
    data.invert <- data[(data$Spectrum %in% spectra.line.table.invert$Spectrum), ]
    
    
    spectra.line.table <- spectra.line.table[(randomized), ]
    hold.frame <- hold.frame[(randomized), ]
    data <- data[(data$Spectrum %in% spectra.line.table$Spectrum), ]
    

    
    
    
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
    
    predict.frame.invert <- data.frame(concentration, intensity)
    colnames(predict.frame.invert) <- c("Concentration", "Intensity")
    predict.frame.invert <- predict.frame.invert[!(randomized), ]

    
    
    predict.frame <- data.frame(concentration, intensity)
    colnames(predict.frame) <- c("Concentration", "Intensity")
    predict.frame <- predict.frame[(randomized), ]
    

    
    predict.intensity <- if(dataType()=="Spectra"){
        general.prep(spectra.line.table=spectra.line.table.invert, element.line=input$calcurveelement)
    } else if(dataType()=="Net"){
        general.prep.net(spectra.line.table=spectra.line.table.invert, element.line=input$calcurveelement)
    }
    
    predict.intensity.lm <- if(dataType()=="Spectra"){
        general.prep(spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
    } else if(dataType()=="Net"){
        general.prep.net(spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
    }
    
    
    if(input$normcal==2){
        
        
        predict.intensity.tc <- if(dataType()=="Spectra"){
            simple.tc.prep(data=data.invert, spectra.line.table=spectra.line.table.invert, element.line=input$calcurveelement)
        } else if(dataType()=="Net"){
            simple.tc.prep.net(data=data.invert, spectra.line.table=spectra.line.table.invert, element.line=input$calcurveelement)
        }
        
        predict.intensity.tc.lm <- if(dataType()=="Spectra"){
            simple.tc.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
        } else if(dataType()=="Net"){
            simple.tc.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
        }
        
        predict.frame.tc <- data.frame(predict.intensity.tc.lm, predict.frame$Concentration)
        colnames(predict.frame.tc) <- c(names(predict.intensity.tc), "Concentration")
        
        
    }
    
    if(input$normcal==3){
        
        predict.intensity.comp <- if(dataType()=="Spectra"){
            simple.comp.prep(data=data.invert, spectra.line.table=spectra.line.table.invert, element.line=input$calcurveelement, norm.min=input$comptonmin, norm.max=input$comptonmax)
        } else if(dataType()=="Net"){
            simple.comp.prep.net(data=data.invert, spectra.line.table=spectra.line.table.invert, element.line=input$calcurveelement, norm.min=input$comptonmin, norm.max=input$comptonmax)
        }
        
        predict.intensity.comp.lm <- if(dataType()=="Spectra"){
            simple.comp.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, norm.min=input$comptonmin, norm.max=input$comptonmax)
        } else if(dataType()=="Net"){
            simple.comp.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, norm.min=input$comptonmin, norm.max=input$comptonmax)
        }
        
        predict.frame.comp <- data.frame(predict.intensity.comp.lm, predict.frame$Concentration)
        colnames(predict.frame.comp) <- c(names(predict.intensity.comp), "Concentration")
    }
    
    
    
    
    if (input$normcal==1){
        
        cal.lm <- lm(Concentration~Intensity, data=predict.frame)
        cal.lm.poly <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame)
        
        cal.est.conc.pred <- predict(object=cal.lm, newdata=predict.intensity, interval='confidence')
        cal.est.conc.tab <- data.frame(cal.est.conc.pred)
        cal.est.conc <- cal.est.conc.tab$fit
        
        cal.est.poly.conc.pred <- predict(object=cal.lm.poly, newdata=predict.intensity, interval='confidence')
        cal.est.poly.conc.tab <- data.frame(cal.est.poly.conc.pred)
        cal.est.poly.conc <- cal.est.poly.conc.tab$fit
        
        val.frame <- data.frame(predict.frame.invert$Concentration, cal.est.conc)
        colnames(val.frame) <- c("Concentration", "Prediction")
        val.frame <- val.frame[val.frame$Concentration > min(predict.frame$Concentration, na.rm = TRUE) & val.frame$Concentration < max(predict.frame$Concentration, na.rm = TRUE), ]

        
        
        val.frame.poly <- data.frame(predict.frame.invert$Concentration, cal.est.poly.conc)
        colnames(val.frame.poly) <- c("Concentration", "Prediction")
        val.frame.poly <- val.frame.poly[val.frame.poly$Concentration > min(predict.frame$Concentration, na.rm = TRUE) & val.frame.poly$Concentration < max(predict.frame$Concentration, na.rm = TRUE), ]

    }
    
    
    if (input$normcal==3){
        
        
        
        cal.lm.comp <- lm(Concentration~Intensity, data=predict.frame.comp)
        
        cal.lm.poly.comp <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame.comp)
        
        cal.est.conc.pred.comp <- predict(object=cal.lm.comp, newdata=predict.intensity.comp, interval='confidence')
        cal.est.conc.tab.comp <- data.frame(cal.est.conc.pred.comp)
        cal.est.conc.comp <- cal.est.conc.tab.comp$fit
        
        cal.est.poly.conc.pred.comp <- predict(object=cal.lm.poly.comp, newdata=predict.intensity.comp, interval='confidence')
        cal.est.poly.conc.tab.comp <- data.frame(cal.est.poly.conc.pred.comp)
        cal.est.poly.conc.comp <- cal.est.poly.conc.tab.comp$fit
        
        val.frame.comp <- data.frame(predict.frame.invert$Concentration, cal.est.conc.comp)
        colnames(val.frame.comp) <- c("Concentration", "Prediction")
        val.frame.comp <- val.frame.comp[val.frame.comp$Concentration > min(predict.frame.comp$Concentration, na.rm = TRUE) & val.frame.comp$Concentration < max(predict.frame.comp$Concentration, na.rm = TRUE), ]

        
        
        val.frame.poly.comp <- data.frame(predict.frame.invert$Concentration, cal.est.poly.conc.comp)
        colnames(val.frame.poly.comp) <- c("Concentration", "Prediction")
        val.frame.poly.comp <- val.frame.poly.comp[val.frame.poly.comp$Concentration > min(predict.frame.comp$Concentration, na.rm = TRUE) & val.frame.poly.comp$Concentration < max(predict.frame.comp$Concentration, na.rm = TRUE), ]

    }
    
    
    
    if (input$normcal==2){
        
        cal.lm.tc <- lm(Concentration~Intensity, data=predict.frame.tc)
        cal.lm.poly.tc <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame.tc)
        
        cal.est.conc.pred.tc <- predict(object=cal.lm.tc, newdata=predict.intensity.tc, interval='confidence')
        cal.est.conc.tab.tc <- data.frame(cal.est.conc.pred.tc)
        cal.est.conc.tc <- cal.est.conc.tab.tc$fit
        
        cal.est.poly.conc.pred.tc <- predict(object=cal.lm.poly.tc, newdata=predict.intensity.tc, interval='confidence')
        cal.est.poly.conc.tab.tc <- data.frame(cal.est.poly.conc.pred.tc)
        cal.est.poly.conc.tc <- cal.est.poly.conc.tab.tc$fit
        
        val.frame.tc <- data.frame(predict.frame.invert$Concentration, cal.est.conc.tc)
        colnames(val.frame.tc) <- c("Concentration", "Prediction")
        val.frame.tc <- val.frame.tc[val.frame.tc$Concentration > min(predict.frame.tc$Concentration, na.rm = TRUE) & val.frame.tc$Concentration < max(predict.frame.tc$Concentration, na.rm = TRUE), ]

        
        
        val.frame.poly.tc <- data.frame(predict.frame.invert$Concentration, cal.est.poly.conc.tc)
        colnames(val.frame.poly.tc) <- c("Concentration", "Prediction")
        val.frame.poly.tc <- val.frame.poly.tc[val.frame.poly.tc$Concentration > min(predict.frame.tc$Concentration, na.rm = TRUE) & val.frame.poly.tc$Concentration < max(predict.frame.tc$Concentration, na.rm = TRUE), ]

        
    }
    
    
    
    
    
    ####Fourth Iteration
    
    if (input$radiocal==3){
        
        
        
        if(input$normcal==1){
            
            predict.intensity.luc <- if(dataType()=="Spectra"){
                lucas.simp.prep(spectra.line.table=spectra.line.table.invert, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
            } else if(dataType()=="Net"){
                lucas.simp.prep.net(spectra.line.table=spectra.line.table.invert, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
            }
            
            predict.intensity.luc.lm <- if(dataType()=="Spectra"){
                lucas.simp.prep(spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
            } else if(dataType()=="Net"){
                lucas.simp.prep.net(spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
            }

            
            predict.frame.luc <- data.frame(predict.intensity.luc.lm, predict.frame$Concentration)
            colnames(predict.frame.luc) <- c(names(predict.intensity.luc), "Concentration")
            
            
            lucas.lm <- lm(Concentration~., data=predict.frame.luc)
            
            
            
            
            
            xmin = 0; xmax=10
            N = length(predict.frame.luc$Concentration)
            means = colMeans(predict.frame.luc)
            dummyDF = t(as.data.frame(means))
            for(i in 2:N){dummyDF=rbind(dummyDF,means)}
            xv=seq(xmin,xmax, length.out=N)
            dummyDF$Concentration = xv
            yv=predict(lucas.lm, newdata=predict.intensity.luc)
            
            
            
            lucas.x <- yv
            
            
            
            cal.est.conc.pred.luc <- predict(object=lucas.lm , newdata=predict.intensity.luc, interval='confidence')
            cal.est.conc.tab.luc <- data.frame(cal.est.conc.pred.luc)
            cal.est.conc.luc <- cal.est.conc.tab.luc$fit
            cal.est.conc.luc.up <- cal.est.conc.tab.luc$upr
            cal.est.conc.luc.low <- cal.est.conc.tab.luc$lwr
            
            
            val.frame.luc <- data.frame(predict.frame.invert$Concentration, predict.intensity.luc$Intensity, lucas.x, cal.est.conc.luc, cal.est.conc.luc.up, cal.est.conc.luc.low)
            colnames(val.frame.luc) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
            val.frame.luc <- val.frame.luc[val.frame.luc$Concentration > min(predict.frame.luc$Concentration, na.rm = TRUE) & val.frame.luc$Concentration < max(predict.frame.luc$Concentration, na.rm = TRUE), ]

        }
        
        
        
        
        
        if(input$normcal==3){
            
            
            predict.intensity.luc.comp <- if(dataType()=="Spectra"){
                lucas.comp.prep(data=data.invert, spectra.line.table=spectra.line.table.invert, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
            } else if(dataType()=="Net"){
                lucas.comp.prep.net(data=data.invert, spectra.line.table=spectra.line.table.invert, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
            }
            
            predict.intensity.luc.comp.lm <- if(dataType()=="Spectra"){
                lucas.comp.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
            } else if(dataType()=="Net"){
                lucas.comp.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
            }
            
            predict.frame.luc.comp <- data.frame(predict.intensity.luc.comp.lm, predict.frame$Concentration)
            colnames(predict.frame.luc.comp) <- c(names(predict.intensity.luc.comp), "Concentration")
            
            
            
            
            lucas.lm.comp <- lm(Concentration~., data=predict.frame.luc.comp)
            
            
            xmin = 0; xmax=10
            N = length(predict.frame.luc.comp$Concentration)
            means = colMeans(predict.frame.luc.comp)
            dummyDF = t(as.data.frame(means))
            for(i in 2:N){dummyDF=rbind(dummyDF,means)}
            xv=seq(xmin,xmax, length.out=N)
            dummyDF$Concentration = xv
            yv=predict(lucas.lm.comp, newdata=predict.intensity.luc.comp)
            
            
            lucas.x.comp <- yv
            
            cal.est.conc.pred.luc.comp <- predict(object=lucas.lm.comp , newdata=predict.intensity.luc.comp, interval='confidence')
            cal.est.conc.tab.luc.comp <- data.frame(cal.est.conc.pred.luc.comp)
            cal.est.conc.luc.comp <- cal.est.conc.tab.luc.comp$fit
            cal.est.conc.luc.up.comp <- cal.est.conc.tab.luc.comp$upr
            cal.est.conc.luc.low.comp <- cal.est.conc.tab.luc.comp$lwr
            
            
            val.frame.luc.comp <- data.frame(predict.frame.invert$Concentration, predict.intensity.luc.comp$Intensity, lucas.x.comp, cal.est.conc.luc.comp, cal.est.conc.luc.up.comp, cal.est.conc.luc.low.comp)
            colnames(val.frame.luc.comp) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
            val.frame.luc.comp <- val.frame.luc.comp[val.frame.luc.comp$Concentration > min(predict.frame.luc.comp$Concentration, na.rm = TRUE) & val.frame.luc.comp$Concentration < max(predict.frame.luc.comp$Concentration, na.rm = TRUE), ]

        }
        
        
        
        
        if(input$normcal==2){
            
            
            predict.intensity.luc.tc <- if(dataType()=="Spectra"){
                lucas.tc.prep(data=data.invert, spectra.line.table=spectra.line.table.invert, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
            } else if(dataType()=="Net"){
                lucas.tc.prep.net(data=data.invert, spectra.line.table=spectra.line.table.invert, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
            }
            
            predict.intensity.luc.tc.lm <- if(dataType()=="Spectra"){
                lucas.tc.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
            } else if(dataType()=="Net"){
                lucas.tc.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
            }
            
            predict.frame.luc.tc <- data.frame(predict.intensity.luc.tc.lm, predict.frame$Concentration)
            colnames(predict.frame.luc.tc) <- c(names(predict.intensity.luc.tc), "Concentration")
            
            
            lucas.lm.tc <- lm(Concentration~., data=predict.frame.luc.tc)
            
            xmin = 0; xmax=10
            N = length(predict.frame.luc.tc$Concentration)
            means = colMeans(predict.frame.luc.tc)
            dummyDF = t(as.data.frame(means))
            for(i in 2:N){dummyDF=rbind(dummyDF,means)}
            xv=seq(xmin,xmax, length.out=N)
            dummyDF$Concentration = xv
            yv=predict(lucas.lm.tc, newdata=predict.intensity.luc.tc)
            
            
            lucas.x.tc <- yv
            
            cal.est.conc.pred.luc.tc <- predict(object=lucas.lm.tc , newdata=predict.intensity.luc.tc, interval='confidence')
            cal.est.conc.tab.luc.tc <- data.frame(cal.est.conc.pred.luc.tc)
            cal.est.conc.luc.tc <- cal.est.conc.tab.luc.tc$fit
            cal.est.conc.luc.up.tc <- cal.est.conc.tab.luc.tc$upr
            cal.est.conc.luc.low.tc <- cal.est.conc.tab.luc.tc$lwr
            
            
            val.frame.luc.tc <- data.frame(predict.frame.invert$Concentration, predict.intensity.luc.tc$Intensity, lucas.x.tc, cal.est.conc.luc.tc, cal.est.conc.luc.up.tc, cal.est.conc.luc.low.tc)
            colnames(val.frame.luc.tc) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
            val.frame.luc.tc <- val.frame.luc.tc[val.frame.luc.tc$Concentration > min(predict.frame.luc.tc$Concentration, na.rm = TRUE) & val.frame.luc.tc$Concentration < max(predict.frame.luc.tc$Concentration, na.rm = TRUE), ]

            
        }
        
    }
    
    if (input$radiocal!=3){
        
        
        ####Linear Calibration Curve Model Display, Time Normalized
        if(input$normcal==1){
            
            calval.linear.plot <- ggplot(data=val.frame, aes(Prediction, Concentration)) +
            theme_bw() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            stat_smooth(method="lm") +
            geom_abline(intercept=0, slope=1, lty=2) +
            geom_point() +
            geom_point(aes(Prediction, Concentration), data = val.frame, shape = 21, fill = "green", color = "black", alpha = 0.25) +
            scale_x_continuous(paste(element.name, predi)) +
            scale_y_continuous(paste(element.name, conen))
            
            calval.poly.plot <- ggplot(data=val.frame.poly, aes(Prediction, Concentration)) +
            theme_bw() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.poly)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_abline(intercept=0, slope=1, lty=2) +
            stat_smooth(method="lm") +
            geom_point() +
            geom_point(aes(Prediction, Concentration), data = val.frame.poly, shape = 21, fill = "green", color = "black", alpha = 0.25) +
            scale_x_continuous(paste(element.name, predi)) +
            scale_y_continuous(paste(element.name, conen))
        }
        
        
        
        ####Linear Calibration Curve Model Display, Total Count Normalized
        if(input$normcal==2){
            
            calval.linear.plot.tc <- ggplot(data=val.frame.tc, aes(Prediction, Concentration)) +
            theme_bw() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.tc)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_abline(intercept=0, slope=1, lty=2) +
            stat_smooth(method="lm") +
            geom_point() +
            geom_point(aes(Prediction, Concentration), data = val.frame.tc, shape = 21, fill = "red", color = "black", alpha = 0.25) +
            scale_x_continuous(paste(element.name, predi)) +
            scale_y_continuous(paste(element.name, conen))
            
            calval.poly.plot.tc <- ggplot(data=val.frame.poly.tc, aes(Prediction, Concentration)) +
            theme_bw() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.poly.tc)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_abline(intercept=0, slope=1, lty=2) +
            stat_smooth(method="lm") +
            geom_point() +
            geom_point(aes(Prediction, Concentration), data = val.frame.poly.tc, shape = 21, fill = "green", color = "black", alpha = 0.25) +
            scale_x_continuous(paste(element.name, predi)) +
            scale_y_continuous(paste(element.name, conen))
        }
        
        
        ####Linear Calibration Curve Model Display, Compton Normalized
        if(input$normcal==3){
            
            calval.linear.plot.comp <- ggplot(data=val.frame.comp, aes(Prediction, Concentration)) +
            theme_bw() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.comp)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_abline(intercept=0, slope=1, lty=2) +
            stat_smooth(method="lm") +
            geom_point() +
            geom_point(aes(Prediction, Concentration), data = val.frame.comp, shape = 21, fill = "green", color = "black", alpha = 0.25) +
            scale_x_continuous(paste(element.name, predi)) +
            scale_y_continuous(paste(element.name, conen))
            
            calval.poly.plot.comp <- ggplot(data=val.frame.poly.comp, aes(Prediction, Concentration)) +
            theme_bw() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.poly.comp)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_abline(intercept=0, slope=1, lty=2) +
            stat_smooth(method="lm") +
            geom_point() +
            geom_point(aes(Prediction, Concentration), data = val.frame.poly.comp, shape = 21, fill = "green", color = "black", alpha = 0.25) +
            scale_x_continuous(paste(element.name, predi)) +
            scale_y_continuous(paste(element.name, conen))
            
        }
        
    }
    
    
    
    if (input$radiocal==3){
        
        ####lucas-Tooth  Calibration Curve Model Display, Time Normalized
        
        if(input$normcal==1){
            
            calval.linear.plot.luc <- ggplot(data=val.frame.luc, aes(Prediction, Concentration)) +
            theme_bw() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.luc)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            stat_smooth(method="lm") +
            geom_abline(intercept=0, slope=1, lty=2) +
            geom_point() +
            geom_point(aes(Prediction, Concentration),  data = val.frame.luc, shape = 21, fill = "green", color = "black", alpha = 0.25) +
            scale_x_continuous(paste(element.name, predi)) +
            scale_y_continuous(paste(element.name, conen))
        }
        
        
        
        ####lucas-Tooth Calibration Curve Model Display, Total Count Normalized
        if(input$normcal==2){
            
            calval.linear.plot.luc.tc <- ggplot(data=val.frame.luc.tc, aes(Prediction, Concentration)) +
            theme_bw() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.luc.tc)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            stat_smooth(method="lm") +
            geom_abline(intercept=0, slope=1, lty=2) +
            geom_point() +
            geom_point(aes(Prediction, Concentration),  data = val.frame.luc.tc, shape = 21, fill = "green", color = "black", alpha = 0.25) +
            scale_x_continuous(paste(element.name, predi)) +
            scale_y_continuous(paste(element.name, conen))
            
        }
        
        
        
        ####lucas-Tooth Calibration Curve Model Display, Compton Normalized
        
        if(input$normcal==3){
            
            calval.linear.plot.luc.comp <- ggplot(data=val.frame.luc.comp, aes(Prediction, Concentration)) +
            theme_bw() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.luc.comp)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_abline(intercept=0, slope=1, lty=2) +
            stat_smooth(method="lm") +
            geom_point() +
            geom_point(aes(Prediction, Concentration),  data = val.frame.luc.comp, shape = 21, fill = "green", color = "black", alpha = 0.25) +
            scale_x_continuous(paste(element.name, predi)) +
            scale_y_continuous(paste(element.name, conen))
            
        }
        
    }
    
    
    if(input$radiocal==1 && input$normcal==1) {
        calval.linear.plot
    } else if (input$radiocal==2 && input$normcal==1){
        calval.poly.plot
    } else if (input$radiocal==1 && input$normcal==2){
        calval.linear.plot.tc
    } else if (input$radiocal==2 && input$normcal==2) {
        calval.poly.plot.tc
    } else if (input$radiocal==1 && input$normcal==3) {
        calval.linear.plot.comp
    } else if (input$radiocal==2 && input$normcal==3) {
        calval.poly.plot.comp
    } else if (input$radiocal==3 && input$normcal==1) {
        calval.linear.plot.luc
    } else if (input$radiocal==3 && input$normcal==2){
        calval.linear.plot.luc.tc
    } else if (input$radiocal==3 && input$normcal==3) {
        calval.linear.plot.luc.comp
    } else if (is.na(input$calcurveelement)) {
        return()
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
    
    concentration.table <- as.data.frame(values[["DF"]], stringsAsFactors=FALSE)
    hold.table <- concentration.table[,c("Spectrum", input$calcurveelement)]
    colnames(hold.table) <- c("Spectrum", "Selection")
    hold.table$Selection[hold.table$Selection==""] <- 999
    hold.table$Selection[hold.table$Selection==999] <- NA
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
        calValFrame()
    }
    
    randomized <- randomizeData()
    
    
    point.table <- point.table[ vals$keeprows, , drop = FALSE]
    point.table <- point.table[randomized,]
    
    concentration.table <- as.data.frame(values[["DF"]], stringsAsFactors=FALSE)
    
    concentration.table <- concentration.table[ vals$keeprows, , drop = FALSE]
    concentration.table <- concentration.table[randomized,]
    
    hold.table <- concentration.table[,c("Spectrum", input$calcurveelement)]
    colnames(hold.table) <- c("Spectrum", "Selection")
    hold.table$Selection[hold.table$Selection==""] <- 999
    hold.table$Selection[hold.table$Selection==999] <- NA
    hold.table <- hold.table[complete.cases(hold.table), ]
    
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
    concentration.table <- as.data.frame(values[["DF"]], stringsAsFactors=FALSE)
    hold.table <- concentration.table[,c("Spectrum", input$calcurveelement)]
    colnames(hold.table) <- c("Spectrum", "Selection")
    hold.table$Selection[hold.table$Selection==""] <- 999
    hold.table$Selection[hold.table$Selection==999] <- NA
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
    
    concentration.table <- as.data.frame(values[["DF"]], stringsAsFactors=FALSE)
    
    concentration.table <- concentration.table[ vals$keeprows, , drop = FALSE]
    concentration.table <- concentration.table[!(randomized),]
    concentration.table.rev <- concentration.table[(randomized),]

    hold.table <- concentration.table[,c("Spectrum", input$calcurveelement)]
    colnames(hold.table) <- c("Spectrum", "Selection")
    hold.table$Selection[hold.table$Selection==""] <- 999
    hold.table$Selection[hold.table$Selection==999] <- NA
    hold.table <- hold.table[complete.cases(hold.table), ]
    
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





 calValTable <- reactive({
     
     data <- dataHold()
     
     
     #if(input$usecalfile==TRUE && input$plot_cal_click==FALSE){vals$keeprows <- calFileContents()$calList[[input$calcurveelement]][[1]][[4]]}
     
     
     
     concentration.table <- as.data.frame(values[["DF"]], stringsAsFactors=FALSE)
     concentration.table[concentration.table==""] <- 999
     
     spectra.line.table <- if(dataType()=="Spectra"){
         spectraData()
     }else if(dataType()=="Net"){
         dataHold()
     }
     
     
     
     
     
     
     
     
     concentration <- as.vector(as.numeric(unlist(concentration.table[input$calcurveelement])))
     
     
     
     intensity <- as.vector(as.numeric(unlist(spectra.line.table[input$calcurveelement])))
     
     spectra.names <- spectra.line.table$Spectrum
     
     # intensity <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
     
     
     # concentration <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
     
     hold.frame <- data.frame(spectra.names, concentration, intensity)
     colnames(hold.frame) <- c("Spectrum", "Concentration", "Intensity")
     hold.frame[hold.frame==999] <- NA
     hold.frame <- na.omit(hold.frame)
     
     concentration <- hold.frame$Concentration
     intensity <- hold.frame$Intensity
     
     data <- data[data$Spectrum %in% hold.frame$Spectrum, ]
     
     spectra.line.table <- spectra.line.table[spectra.line.table$Spectrum %in% hold.frame$Spectrum, ]
     
     
     
     
     
     
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
     
     
     predict.frame <- data.frame(concentration, intensity)
     colnames(predict.frame) <- c("Concentration", "Intensity")
     
     predict.intensity <- if(dataType()=="Spectra"){
         general.prep(spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
     } else if(dataType()=="Net"){
         general.prep.net(spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
     }
     
     
     if(input$normcal==2){
         
         
         predict.intensity.tc <- if(dataType()=="Spectra"){
             simple.tc.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
         } else if(dataType()=="Net"){
             simple.tc.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
         }
         
         predict.frame.tc <- data.frame(predict.intensity.tc, predict.frame$Concentration)
         colnames(predict.frame.tc) <- c(names(predict.intensity.tc), "Concentration")
         
         
     }
     
     if(input$normcal==3){
         
         predict.intensity.comp <- if(dataType()=="Spectra"){
             simple.comp.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, norm.min=input$comptonmin, norm.max=input$comptonmax)
         } else if(dataType()=="Net"){
             simple.comp.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, norm.min=input$comptonmin, norm.max=input$comptonmax)
         }
         
         predict.frame.comp <- data.frame(predict.intensity.comp, predict.frame$Concentration)
         colnames(predict.frame.comp) <- c(names(predict.intensity.comp), "Concentration")
     }
     
     
     
     
     if (input$normcal==1){
         
         cal.lm <- lm(Concentration~Intensity, data=predict.frame[ vals$keeprows, , drop = FALSE])
         cal.lm.poly <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame[ vals$keeprows, , drop = FALSE])
         
         cal.est.conc.pred <- predict(object=cal.lm, newdata=predict.intensity, interval='confidence')
         cal.est.conc.tab <- data.frame(cal.est.conc.pred)
         cal.est.conc <- cal.est.conc.tab$fit
         
         cal.est.poly.conc.pred <- predict(object=cal.lm.poly, newdata=predict.intensity, interval='confidence')
         cal.est.poly.conc.tab <- data.frame(cal.est.poly.conc.pred)
         cal.est.poly.conc <- cal.est.poly.conc.tab$fit
         
         val.frame <- data.frame(predict.frame$Concentration, cal.est.conc)
         colnames(val.frame) <- c("Concentration", "Prediction")
         
         
         val.frame.poly <- data.frame(predict.frame$Concentration, cal.est.poly.conc)
         colnames(val.frame.poly) <- c("Concentration", "Prediction")
         
     }
     
     
     if (input$normcal==3){
         
         
         
         cal.lm.comp <- lm(Concentration~Intensity, data=predict.frame.comp[ vals$keeprows, , drop = FALSE])
         
         cal.lm.poly.comp <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame.comp[ vals$keeprows, , drop = FALSE])
         
         cal.est.conc.pred.comp <- predict(object=cal.lm.comp, newdata=predict.intensity.comp, interval='confidence')
         cal.est.conc.tab.comp <- data.frame(cal.est.conc.pred.comp)
         cal.est.conc.comp <- cal.est.conc.tab.comp$fit
         
         cal.est.poly.conc.pred.comp <- predict(object=cal.lm.poly.comp, newdata=predict.intensity.comp, interval='confidence')
         cal.est.poly.conc.tab.comp <- data.frame(cal.est.poly.conc.pred.comp)
         cal.est.poly.conc.comp <- cal.est.poly.conc.tab.comp$fit
         
         val.frame.comp <- data.frame(predict.frame$Concentration, cal.est.conc.comp)
         colnames(val.frame.comp) <- c("Concentration", "Prediction")
         
         
         
         val.frame.poly.comp <- data.frame(predict.frame$Concentration, cal.est.poly.conc.comp)
         colnames(val.frame.poly.comp) <- c("Concentration", "Prediction")
         
     }
     
     
     
     if (input$normcal==2){
         
         cal.lm.tc <- lm(Concentration~Intensity, data=predict.frame.tc[ vals$keeprows, , drop = FALSE])
         cal.lm.poly.tc <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame.tc[ vals$keeprows, , drop = FALSE])
         
         cal.est.conc.pred.tc <- predict(object=cal.lm.tc, newdata=predict.intensity.tc, interval='confidence')
         cal.est.conc.tab.tc <- data.frame(cal.est.conc.pred.tc)
         cal.est.conc.tc <- cal.est.conc.tab.tc$fit
         
         cal.est.poly.conc.pred.tc <- predict(object=cal.lm.poly.tc, newdata=predict.intensity.tc, interval='confidence')
         cal.est.poly.conc.tab.tc <- data.frame(cal.est.poly.conc.pred.tc)
         cal.est.poly.conc.tc <- cal.est.poly.conc.tab.tc$fit
         
         val.frame.tc <- data.frame(predict.frame$Concentration, cal.est.conc.tc)
         colnames(val.frame.tc) <- c("Concentration", "Prediction")
         
         
         
         val.frame.poly.tc <- data.frame(predict.frame$Concentration, cal.est.poly.conc.tc)
         colnames(val.frame.poly.tc) <- c("Concentration", "Prediction")
         
         
     }
     
     
     
     
     
     ####Fourth Iteration
     
     if (input$radiocal==3){
         
         
         
         if(input$normcal==1){
             
             predict.intensity.luc <- if(dataType()=="Spectra"){
                 lucas.simp.prep(spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
             } else if(dataType()=="Net"){
                 lucas.simp.prep.net(spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
             }
             
             predict.frame.luc <- data.frame(predict.intensity.luc, predict.frame$Concentration)
             colnames(predict.frame.luc) <- c(names(predict.intensity.luc), "Concentration")
             
             
             lucas.lm <- lm(Concentration~., data=predict.frame.luc[ vals$keeprows, , drop = FALSE])
             
             
             xmin = 0; xmax=10
             N = length(predict.frame.luc$Concentration)
             means = colMeans(predict.frame.luc)
             dummyDF = t(as.data.frame(means))
             for(i in 2:N){dummyDF=rbind(dummyDF,means)}
             xv=seq(xmin,xmax, length.out=N)
             dummyDF$Concentration = xv
             yv=predict(lucas.lm, newdata=predict.intensity.luc)
             
             
             
             lucas.x <- yv
             
             
             
             cal.est.conc.pred.luc <- predict(object=lucas.lm , newdata=predict.intensity.luc, interval='confidence')
             cal.est.conc.tab.luc <- data.frame(cal.est.conc.pred.luc)
             cal.est.conc.luc <- cal.est.conc.tab.luc$fit
             cal.est.conc.luc.up <- cal.est.conc.tab.luc$upr
             cal.est.conc.luc.low <- cal.est.conc.tab.luc$lwr
             
             
             val.frame.luc <- data.frame(predict.frame$Concentration, predict.intensity.luc$Intensity, lucas.x, cal.est.conc.luc, cal.est.conc.luc.up, cal.est.conc.luc.low)
             colnames(val.frame.luc) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
         }
         
         
         
         
         
         if(input$normcal==3){
             
             
             predict.intensity.luc.comp <- if(dataType()=="Spectra"){
                 lucas.comp.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
             } else if(dataType()=="Net"){
                 lucas.comp.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
             }
             
             predict.frame.luc.comp <- data.frame(predict.intensity.luc.comp, predict.frame$Concentration)
             colnames(predict.frame.luc.comp) <- c(names(predict.intensity.luc.comp), "Concentration")
             
             
             
             
             lucas.lm.comp <- lm(Concentration~., data=predict.frame.luc.comp[ vals$keeprows, , drop = FALSE])
             
             
             xmin = 0; xmax=10
             N = length(predict.frame.luc.comp$Concentration)
             means = colMeans(predict.frame.luc.comp)
             dummyDF = t(as.data.frame(means))
             for(i in 2:N){dummyDF=rbind(dummyDF,means)}
             xv=seq(xmin,xmax, length.out=N)
             dummyDF$Concentration = xv
             yv=predict(lucas.lm.comp, newdata=predict.intensity.luc.comp)
             
             
             lucas.x.comp <- yv
             
             cal.est.conc.pred.luc.comp <- predict(object=lucas.lm.comp , newdata=predict.intensity.luc.comp, interval='confidence')
             cal.est.conc.tab.luc.comp <- data.frame(cal.est.conc.pred.luc.comp)
             cal.est.conc.luc.comp <- cal.est.conc.tab.luc.comp$fit
             cal.est.conc.luc.up.comp <- cal.est.conc.tab.luc.comp$upr
             cal.est.conc.luc.low.comp <- cal.est.conc.tab.luc.comp$lwr
             
             
             val.frame.luc.comp <- data.frame(predict.frame$Concentration, predict.intensity.luc.comp$Intensity, lucas.x.comp, cal.est.conc.luc.comp, cal.est.conc.luc.up.comp, cal.est.conc.luc.low.comp)
             colnames(val.frame.luc.comp) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
             
         }
         
         
         
         
         if(input$normcal==2){
             
             
             predict.intensity.luc.tc <- if(dataType()=="Spectra"){
                 lucas.tc.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
             } else if(dataType()=="Net"){
                 lucas.tc.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
             }
             
             predict.frame.luc.tc <- data.frame(predict.intensity.luc.tc, predict.frame$Concentration)
             colnames(predict.frame.luc.tc) <- c(names(predict.intensity.luc.tc), "Concentration")
             
             
             lucas.lm.tc <- lm(Concentration~., data=predict.frame.luc.tc)
             
             xmin = 0; xmax=10
             N = length(predict.frame.luc.tc$Concentration)
             means = colMeans(predict.frame.luc.tc)
             dummyDF = t(as.data.frame(means))
             for(i in 2:N){dummyDF=rbind(dummyDF,means)}
             xv=seq(xmin,xmax, length.out=N)
             dummyDF$Concentration = xv
             yv=predict(lucas.lm.tc, newdata=predict.intensity.luc.tc)
             
             
             lucas.x.tc <- yv
             
             cal.est.conc.pred.luc.tc <- predict(object=lucas.lm.tc , newdata=predict.intensity.luc.tc, interval='confidence')
             cal.est.conc.tab.luc.tc <- data.frame(cal.est.conc.pred.luc.tc)
             cal.est.conc.luc.tc <- cal.est.conc.tab.luc.tc$fit
             cal.est.conc.luc.up.tc <- cal.est.conc.tab.luc.tc$upr
             cal.est.conc.luc.low.tc <- cal.est.conc.tab.luc.tc$lwr
             
             
             val.frame.luc.tc <- data.frame(predict.frame$Concentration, predict.intensity.luc.tc$Intensity, lucas.x.tc, cal.est.conc.luc.tc, cal.est.conc.luc.up.tc, cal.est.conc.luc.low.tc)
             colnames(val.frame.luc.tc) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
             
         }
         
     }

     

     
     
     
    standard.table <- if(input$radiocal==1 && input$normcal==1) {
         val.frame
     } else if (input$radiocal==2 && input$normcal==1){
         val.frame.poly
     } else if (input$radiocal==1 && input$normcal==2){
         val.frame.tc
     } else if (input$radiocal==2 && input$normcal==2) {
         val.frame.poly.tc
     } else if (input$radiocal==1 && input$normcal==3) {
         val.frame.comp
     } else if (input$radiocal==2 && input$normcal==3) {
         val.frame.poly.comp
     } else if (input$radiocal==3 && input$normcal==1) {
         val.frame.luc
     } else if (input$radiocal==3 && input$normcal==2){
         val.frame.luc.tc
     } else if (input$radiocal==3 && input$normcal==3) {
         val.frame.luc.comp
     }
     
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
 

 nullElementModel <- reactive({
     
     
 })


 
 elementModel <- reactive({
     
     data <- dataHold()
     
     
     #if(input$usecalfile==TRUE && input$plot_cal_click==FALSE){vals$keeprows <- calFileContents()$calList[[input$calcurveelement]][[1]][[4]]}
     
     
     
     concentration.table <- as.data.frame(values[["DF"]], stringsAsFactors=FALSE)
     concentration.table[concentration.table==""] <- 999
     
     spectra.line.table <- if(dataType()=="Spectra"){
         spectraData()
     }else if(dataType()=="Net"){
         dataHold()
     }
     
     
     
     
     
     
     
     
     concentration <- as.vector(as.numeric(unlist(concentration.table[input$calcurveelement])))
     
     
     
     intensity <- as.vector(as.numeric(unlist(spectra.line.table[input$calcurveelement])))
     
     spectra.names <- spectra.line.table$Spectrum
     
     # intensity <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
     
     
     # concentration <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
     
     hold.frame <- data.frame(spectra.names, concentration, intensity)
     colnames(hold.frame) <- c("Spectrum", "Concentration", "Intensity")
     hold.frame[hold.frame==999] <- NA
     hold.frame <- na.omit(hold.frame)
     
     concentration <- hold.frame$Concentration
     intensity <- hold.frame$Intensity
     
     data <- data[data$Spectrum %in% hold.frame$Spectrum, ]
     
     spectra.line.table <- spectra.line.table[spectra.line.table$Spectrum %in% hold.frame$Spectrum, ]
     
     
     
     
     
     
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
     
     
     predict.frame <- data.frame(concentration, intensity)
     colnames(predict.frame) <- c("Concentration", "Intensity")
     
     predict.intensity <- if(dataType()=="Spectra"){
         general.prep(spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
     } else if(dataType()=="Net"){
         general.prep.net(spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
     }
     
     
     if(input$normcal==2){
         
         
         predict.intensity.tc <- if(dataType()=="Spectra"){
             simple.tc.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
         } else if(dataType()=="Net"){
             simple.tc.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement)
         }
         
         predict.frame.tc <- data.frame(predict.intensity.tc, predict.frame$Concentration)
         colnames(predict.frame.tc) <- c(names(predict.intensity.tc), "Concentration")
         
         
     }
     
     if(input$normcal==3){
         
         predict.intensity.comp <- if(dataType()=="Spectra"){
             simple.comp.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, norm.min=input$comptonmin, norm.max=input$comptonmax)
         } else if(dataType()=="Net"){
             simple.comp.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, norm.min=input$comptonmin, norm.max=input$comptonmax)
         }
         
         predict.frame.comp <- data.frame(predict.intensity.comp, predict.frame$Concentration)
         colnames(predict.frame.comp) <- c(names(predict.intensity.comp), "Concentration")
     }
     
     
     
     
     if (input$normcal==1){
         
         cal.lm <- lm(Concentration~Intensity, data=predict.frame[ vals$keeprows, , drop = FALSE])
         cal.lm.poly <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame[ vals$keeprows, , drop = FALSE])
         
         cal.est.conc.pred <- predict(object=cal.lm, newdata=predict.intensity, interval='confidence')
         cal.est.conc.tab <- data.frame(cal.est.conc.pred)
         cal.est.conc <- cal.est.conc.tab$fit
         
         cal.est.poly.conc.pred <- predict(object=cal.lm.poly, newdata=predict.intensity, interval='confidence')
         cal.est.poly.conc.tab <- data.frame(cal.est.poly.conc.pred)
         cal.est.poly.conc <- cal.est.poly.conc.tab$fit
         
         val.frame <- data.frame(predict.frame$Concentration, cal.est.conc)
         colnames(val.frame) <- c("Concentration", "Prediction")
         
         
         val.frame.poly <- data.frame(predict.frame$Concentration, cal.est.poly.conc)
         colnames(val.frame.poly) <- c("Concentration", "Prediction")
         
     }
     
     
     if (input$normcal==3){
         
         
         
         cal.lm.comp <- lm(Concentration~Intensity, data=predict.frame.comp[ vals$keeprows, , drop = FALSE])
         
         cal.lm.poly.comp <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame.comp[ vals$keeprows, , drop = FALSE])
         
         cal.est.conc.pred.comp <- predict(object=cal.lm.comp, newdata=predict.intensity.comp, interval='confidence')
         cal.est.conc.tab.comp <- data.frame(cal.est.conc.pred.comp)
         cal.est.conc.comp <- cal.est.conc.tab.comp$fit
         
         cal.est.poly.conc.pred.comp <- predict(object=cal.lm.poly.comp, newdata=predict.intensity.comp, interval='confidence')
         cal.est.poly.conc.tab.comp <- data.frame(cal.est.poly.conc.pred.comp)
         cal.est.poly.conc.comp <- cal.est.poly.conc.tab.comp$fit
         
         val.frame.comp <- data.frame(predict.frame$Concentration, cal.est.conc.comp)
         colnames(val.frame.comp) <- c("Concentration", "Prediction")
         
         
         
         val.frame.poly.comp <- data.frame(predict.frame$Concentration, cal.est.poly.conc.comp)
         colnames(val.frame.poly.comp) <- c("Concentration", "Prediction")
         
     }
     
     
     
     if (input$normcal==2){
         
         cal.lm.tc <- lm(Concentration~Intensity, data=predict.frame.tc[ vals$keeprows, , drop = FALSE])
         cal.lm.poly.tc <- lm(Concentration~Intensity + I(Intensity^2), data=predict.frame.tc[ vals$keeprows, , drop = FALSE])
         
         cal.est.conc.pred.tc <- predict(object=cal.lm.tc, newdata=predict.intensity.tc, interval='confidence')
         cal.est.conc.tab.tc <- data.frame(cal.est.conc.pred.tc)
         cal.est.conc.tc <- cal.est.conc.tab.tc$fit
         
         cal.est.poly.conc.pred.tc <- predict(object=cal.lm.poly.tc, newdata=predict.intensity.tc, interval='confidence')
         cal.est.poly.conc.tab.tc <- data.frame(cal.est.poly.conc.pred.tc)
         cal.est.poly.conc.tc <- cal.est.poly.conc.tab.tc$fit
         
         val.frame.tc <- data.frame(predict.frame$Concentration, cal.est.conc.tc)
         colnames(val.frame.tc) <- c("Concentration", "Prediction")
         
         
         
         val.frame.poly.tc <- data.frame(predict.frame$Concentration, cal.est.poly.conc.tc)
         colnames(val.frame.poly.tc) <- c("Concentration", "Prediction")
         
         
     }
     
     
     
     
     
     ####Fourth Iteration
     
     if (input$radiocal==3){
         
         
         
         if(input$normcal==1){
             
             predict.intensity.luc <- if(dataType()=="Spectra"){
                 lucas.simp.prep(spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
             } else if(dataType()=="Net"){
                 lucas.simp.prep.net(spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
             }
             
             predict.frame.luc <- data.frame(predict.intensity.luc, predict.frame$Concentration)
             colnames(predict.frame.luc) <- c(names(predict.intensity.luc), "Concentration")
             
             
             lucas.lm <- lm(Concentration~., data=predict.frame.luc[ vals$keeprows, , drop = FALSE])
             
             
             xmin = 0; xmax=10
             N = length(predict.frame.luc$Concentration)
             means = colMeans(predict.frame.luc)
             dummyDF = t(as.data.frame(means))
             for(i in 2:N){dummyDF=rbind(dummyDF,means)}
             xv=seq(xmin,xmax, length.out=N)
             dummyDF$Concentration = xv
             yv=predict(lucas.lm, newdata=predict.intensity.luc)
             
             
             
             lucas.x <- yv
             
             
             
             cal.est.conc.pred.luc <- predict(object=lucas.lm , newdata=predict.intensity.luc, interval='confidence')
             cal.est.conc.tab.luc <- data.frame(cal.est.conc.pred.luc)
             cal.est.conc.luc <- cal.est.conc.tab.luc$fit
             cal.est.conc.luc.up <- cal.est.conc.tab.luc$upr
             cal.est.conc.luc.low <- cal.est.conc.tab.luc$lwr
             
             
             val.frame.luc <- data.frame(predict.frame$Concentration, predict.intensity.luc$Intensity, lucas.x, cal.est.conc.luc, cal.est.conc.luc.up, cal.est.conc.luc.low)
             colnames(val.frame.luc) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
         }
         
         
         
         
         
         if(input$normcal==3){
             
             
             predict.intensity.luc.comp <- if(dataType()=="Spectra"){
                 lucas.comp.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
             } else if(dataType()=="Net"){
                 lucas.comp.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars, norm.min=input$comptonmin, norm.max=input$comptonmax)
             }
             
             predict.frame.luc.comp <- data.frame(predict.intensity.luc.comp, predict.frame$Concentration)
             colnames(predict.frame.luc.comp) <- c(names(predict.intensity.luc.comp), "Concentration")
             
             
             
             
             lucas.lm.comp <- lm(Concentration~., data=predict.frame.luc.comp[ vals$keeprows, , drop = FALSE])
             
             
             xmin = 0; xmax=10
             N = length(predict.frame.luc.comp$Concentration)
             means = colMeans(predict.frame.luc.comp)
             dummyDF = t(as.data.frame(means))
             for(i in 2:N){dummyDF=rbind(dummyDF,means)}
             xv=seq(xmin,xmax, length.out=N)
             dummyDF$Concentration = xv
             yv=predict(lucas.lm.comp, newdata=predict.intensity.luc.comp)
             
             
             lucas.x.comp <- yv
             
             cal.est.conc.pred.luc.comp <- predict(object=lucas.lm.comp , newdata=predict.intensity.luc.comp, interval='confidence')
             cal.est.conc.tab.luc.comp <- data.frame(cal.est.conc.pred.luc.comp)
             cal.est.conc.luc.comp <- cal.est.conc.tab.luc.comp$fit
             cal.est.conc.luc.up.comp <- cal.est.conc.tab.luc.comp$upr
             cal.est.conc.luc.low.comp <- cal.est.conc.tab.luc.comp$lwr
             
             
             val.frame.luc.comp <- data.frame(predict.frame$Concentration, predict.intensity.luc.comp$Intensity, lucas.x.comp, cal.est.conc.luc.comp, cal.est.conc.luc.up.comp, cal.est.conc.luc.low.comp)
             colnames(val.frame.luc.comp) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
             
         }
         
         
         
         
         if(input$normcal==2){
             
             
             predict.intensity.luc.tc <- if(dataType()=="Spectra"){
                 lucas.tc.prep(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
             } else if(dataType()=="Net"){
                 lucas.tc.prep.net(data=data, spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=input$slope_vars, intercept.element.lines=input$intercept_vars)
             }
             
             predict.frame.luc.tc <- data.frame(predict.intensity.luc.tc, predict.frame$Concentration)
             colnames(predict.frame.luc.tc) <- c(names(predict.intensity.luc.tc), "Concentration")
             
             
             lucas.lm.tc <- lm(Concentration~., data=predict.frame.luc.tc)
             
             xmin = 0; xmax=10
             N = length(predict.frame.luc.tc$Concentration)
             means = colMeans(predict.frame.luc.tc)
             dummyDF = t(as.data.frame(means))
             for(i in 2:N){dummyDF=rbind(dummyDF,means)}
             xv=seq(xmin,xmax, length.out=N)
             dummyDF$Concentration = xv
             yv=predict(lucas.lm.tc, newdata=predict.intensity.luc.tc)
             
             
             lucas.x.tc <- yv
             
             cal.est.conc.pred.luc.tc <- predict(object=lucas.lm.tc , newdata=predict.intensity.luc.tc, interval='confidence')
             cal.est.conc.tab.luc.tc <- data.frame(cal.est.conc.pred.luc.tc)
             cal.est.conc.luc.tc <- cal.est.conc.tab.luc.tc$fit
             cal.est.conc.luc.up.tc <- cal.est.conc.tab.luc.tc$upr
             cal.est.conc.luc.low.tc <- cal.est.conc.tab.luc.tc$lwr
             
             
             val.frame.luc.tc <- data.frame(predict.frame$Concentration, predict.intensity.luc.tc$Intensity, lucas.x.tc, cal.est.conc.luc.tc, cal.est.conc.luc.up.tc, cal.est.conc.luc.low.tc)
             colnames(val.frame.luc.tc) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
             
         }
         
     }

     


   model <- if(input$radiocal==1 && input$normcal==1) {
         cal.lm
     } else if (input$radiocal==2 && input$normcal==1) {
         cal.lm.poly
     } else if (input$radiocal==1 && input$normcal==2) {
         cal.lm.tc
     } else if (input$radiocal==2 && input$normcal==2) {
         cal.lm.poly.tc
     } else if (input$radiocal==1 && input$normcal==3) {
         cal.lm.comp
     } else if (input$radiocal==2 && input$normcal==3) {
         cal.lm.poly.comp
     } else if (input$radiocal==3 && input$normcal==1) {
         lucas.lm
     } else if (input$radiocal==3 && input$normcal==2) {
         lucas.lm.tc
     } else if (input$radiocal==3 && input$normcal==3) {
         lucas.lm.comp
     }
     
     
     
     model

     

 })
 
 
 
 
 modelFrame <- reactive({
     
     table <- predictFrame()
     
     
     model <- elementModel()
     
     model.frame <- as.data.frame(augment(model))
     
     model.frame$qq <- qqnorm(model.frame$.std.resid)[[1]]
     
     model.frame$sqrt.std.resid <- sqrt(abs(model.frame$.std.resid))
     
     model.frame$seq.cooksd <- seq_along(model.frame$.cooksd)
     
     model.frame$Spectrum <- table$Spectrum



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
      geom_point(data=model[ !vals$keeprows, , drop = FALSE], aes(.hat, .cooksd), shape = 21, fill = "red", color = "black", alpha = 0.25) 
     
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
            valelements
        })
        
        calVariableElements <- reactive({
            variables <- calVariables()
            variableelements <- ls(variables)
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


            
            
            
        predicted.list <- pblapply(elements, function (x)
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





