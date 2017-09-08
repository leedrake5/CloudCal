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



options(shiny.maxRequestSize=90*1024^2)

options(warn=-1)
assign("last.warning", NULL, envir = baseenv())

shinyServer(function(input, output, session) {
    
    
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
 
    
    
    observeEvent(input$actionprocess, {
        

        myData <- reactive({
            
            data <- if(input$filetype=="Spectra"){
                fullSpectra()
            } else if(input$filetype=="Net"){
                netCounts()
            }
            
                data


        })
        
        
calFileContents <- reactive({
            
    existingCalFile <- input$calfileinput
            
    if (is.null(existingCalFile)) return(NULL)


    load(existingCalFile$datapath, verbose=TRUE)
            
    Calibration
            
        })

dataHold <- reactive({
    data <- if(input$usecalfile==FALSE){
        myData()
    }else if(input$usecalfile==TRUE){
        calFileContents()$Spectra
    }
    
    data
    
})


dataCount <- reactive({
    inFile <- input$file1
    
    if(input$usecalfile==FALSE){
        length(inFile$datapath)
    }else if(input$usefile==TRUE){
        length(calFileContents()$Spectra)
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
    }else if(input$usecalfile==FALSE && input$filetype=="Net"){
        colnames(spectra.line.table[2:4])
    }else if(input$usecalfile==TRUE){
        ls(calFileContents()$Intensities)
    }
    
})

standardLines <- reactive({
    
    spectra.line.table <- dataHold()
    
    n <- length(names(spectra.line.table))
    
    
    choices <- if(input$filetype=="Spectra"){
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






 
 spectraData <- reactive({
     
     data <- dataHold()





     spectra.line.list <- lapply(input$show_vars, function(x) elementGrab(element.line=x, data=data))
     element.count.list <- lapply(spectra.line.list, `[`, 2)

     spectra.line.vector <- as.numeric(unlist(element.count.list))
     
     dim(spectra.line.vector) <- c(length(spectra.line.list[[1]]$Spectrum), length(input$show_vars))
     
     spectra.line.frame <- data.frame(spectra.line.list[[1]]$Spectrum, spectra.line.vector)
     
     colnames(spectra.line.frame) <- c("Spectrum", input$show_vars)
     
     spectra.line.frame <- as.data.frame(spectra.line.frame)
     
     spectra.line.frame
     
 })
 
 netData <- reactive({
     
     net.data <- dataHold()
     
     net.data.partial <- net.data[input$show_vars]
     net.data <- data.frame(net.data$Spectrum ,net.data.partial)
     colnames(net.data) <- c("Spectrum", input$show_vars)
     net.data
     
 })
 
 
 

 
 
 
 tableInput <- reactive({
     select.line.table <- if(input$filetype=="Spectra"){
         spectraData()
     }else if(input$filetype=="Net"){
         netData()
     }
     
     rounded <- round(select.line.table[input$show_vars], digits=0)
     full <- data.frame(select.line.table$Spectrum, rounded)
     colnames(full) <- c("Spectrum", input$show_vars)
     
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
  
  

hotableInput <- reactive({
    


spectra.line.table <- if(input$filetype=="Spectra"){
    spectraData()
}else if(input$filetype=="Net"){
    dataHold()
}
        empty.line.table <- spectra.line.table[input$show_vars] * 0.0000

    #empty.line.table$Spectrum <- spectra.line.table$Spectrum
    
    hold.frame <- data.frame(spectra.line.table$Spectrum, empty.line.table)
    colnames(hold.frame) <- c("Spectrum", input$show_vars)
    
    hold.frame <- as.data.frame(hold.frame)
    
  
  hotable.data <- if(input$usecalfile==FALSE){
      hold.frame
  }else if(input$usecalfile==TRUE){
      
      data.frame(calFileContents()$Values, hold.frame[! names(hold.frame) %in% names(calFileContents()$Values)])

  }
  
  hotable.data


})

values <- reactiveValues()


observe({
    if (!is.null(input$hot)) {
        DF <- hot_to_r(input$hot)
    } else {
        if (is.null(values[["DF"]]) && input$linecommit)
        DF <- hotableInput()
        else
        DF <- values[["DF"]]
    }
    values[["DF"]] <- DF
})


## Handsontable

output$hot <- renderRHandsontable({

    DF <- values[["DF"]]
    
    
    
    if (!is.null(DF))
    rhandsontable(DF) %>% hot_col(2:length(DF),type="numeric")
    

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

    myelements <- input$show_vars

    result <- if(is.null(myelements)){
        "Ca.K.alpha"
    }else{
        myelements
    }
    
    result


    })

outVaralt <- reactive({
    input$hotableprocess2
    
    
    myelements <- c(input$show_vars, "None")

    
    if(is.null(myelements)){
        paste("Ca.K.alpha")
    }else{
        myelements
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
    
    if(input$usecalfile==FALSE){
        input$calcurveelement
    }else if(input$usecalfile==TRUE){
        calFileContents()$calList[[optionhold]][[1]]$Intercept
    }
    
    
})


output$inVar3 <- renderUI({
    
    checkboxGroupInput(inputId = "intercept_vars", label = h4("Intercept"), choices =  outVaralt(), selected=inVar3Selected())
})

inVar4Selected <- reactive({
    
    hold <- values[["DF"]]
    
    optionhold <- if(is.null(input$calcurveelement)){
        ls(hold)[2]
    }else{
        input$calcurveelement
    }
    
    
    if(input$usecalfile==FALSE){
        input$calcurveelement
    }else if(input$usecalfile==TRUE){
        calFileContents()$calList[[optionhold]][[1]]$Slope
        
    }
})

output$inVar4 <- renderUI({
    checkboxGroupInput(inputId = "slope_vars", label = h4("Slope"), choices =  outVaralt(), selected=inVar4Selected())
})




calConditons <- reactiveValues()

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
    
    if(input$usecalfile==FALSE){
        calConditons[[1]][[1]]
    } else if(input$usecalfile==TRUE){
        calFileContents()$calList[[optionhold]][[1]]$CalTable$CalType
    }
    
})

calNormSelection <- reactive({
    
    hold <- values[["DF"]]
    
    optionhold <- if(is.null(input$calcurveelement)){
        ls(hold)[2]
    }else{
        input$calcurveelement
    }
    
    if(input$usecalfile==FALSE){
        calConditons[[1]][[2]]
    }else if(input$usecalfile==TRUE){
        calFileContents()$calList[[optionhold]][[1]]$CalTable$NormType
    }
    
})

normMinSelection <- reactive({
    
    hold <- values[["DF"]]
    
    optionhold <- if(is.null(input$calcurveelement)){
        ls(hold)[2]
    }else{
        input$calcurveelement
    }
    
    if(input$usecalfile==FALSE){
        calConditons[[1]][[3]]
    }else if(input$usecalfile==TRUE){
        calFileContents()$calList[[optionhold]][[1]]$CalTable$Min
    }
    
})

normMaxSelection <- reactive({
    
    hold <- values[["DF"]]
    
    optionhold <- if(is.null(input$calcurveelement)){
        ls(hold)[2]
    }else{
        input$calcurveelement
    }
    
    if(input$usecalfile==FALSE){
        calConditons[[1]][[4]]
    }else if(input$usecalfile==TRUE){
        calFileContents()$calList[[optionhold]][[1]]$CalTable$Max
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






  
  calFileStandards <- reactive({

          
      elementHold <- if(is.null(input$calcurveelement)==TRUE){
          ls(dataHold())[1]
      } else{
          input$calcurveelement
      }
      
      standards <- if(input$usecalfile==TRUE){
          calFileContents()$calList[[elementHold]][[1]][[4]]
      } else if(input$usecalfile==FALSE){
          rep(TRUE, dataCount())
      }
      
      standards
      
      
  })
  
  
  
  
  vals <- reactiveValues()
  
  vals$keeprows <- vals$keeprows[ vals$keeprows != TRUE]
  vals$keeprows <- vals$keeprows[ vals$keeprows != FALSE]
  vals$keeprows = calFileStandards()

  


  
  
  predictFrame <- reactive({
      
      data <- dataHold()
      
      
      
      
      concentration.table <- as.data.frame(values[["DF"]], stringsAsFactors=FALSE)
      concentration.table[concentration.table==""] <- 999
      


      spectra.line.table <- if(input$filetype=="Spectra"){
          spectraData()
      }else if(input$filetype=="Net"){
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
      
      
      predict.frame
    

  })
  
  



calCurveFrame <- reactive({
    
    data <- dataHold()
    
    
    
    
    concentration.table <- as.data.frame(values[["DF"]], stringsAsFactors=FALSE)
    concentration.table[concentration.table==""] <- 999
    
    spectra.line.table <- if(input$filetype=="Spectra"){
        spectraData()
    }else if(input$filetype=="Net"){
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
    
    predict.intensity <- data.frame(predict.frame$Intensity)
    colnames(predict.intensity) <- c("Intensity")
    
    if(input$normcal==2){
        
        if(input$filetype=="Spectra"){total.counts <- aggregate(CPS~Spectrum, data=data, sum)}
        if(input$filetype=="Spectra"){colnames(total.counts) <- c("Spectrum", "CPS")}
        
        if(input$filetype=="Net"){total.counts.net <- rowSums(spectra.line.table[2:length(spectra.line.table)])}
        if(input$filetype=="Net"){total.counts <- data.frame(spectra.line.table$Spectrum, total.counts.net)}
        if(input$filetype=="Net"){colnames(total.counts) <- c("Spectrum", "CPS")}
        
        predict.frame.tc <- data.frame(predict.frame$Concentration, intensity/total.counts$CPS)
        colnames(predict.frame.tc) <- c("Concentration", "Intensity")
        
        
        
        predict.intensity.tc <- data.frame(predict.frame.tc$Intensity)
        colnames(predict.intensity.tc) <- c("Intensity")
    }
    
    if(input$normcal==3){
        if(input$filetype=="Spectra"){compton.norm <- subset(data$CPS, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))}
        if(input$filetype=="Spectra"){compton.file <- subset(data$Spectrum, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))}
        if(input$filetype=="Spectra"){compton.frame <- data.frame(is.0(compton.norm, compton.file))}
        if(input$filetype=="Spectra"){colnames(compton.frame) <- c("Compton", "Spectrum")}
        if(input$filetype=="Spectra"){compton.frame.ag.hold <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")}
        if(input$filetype=="Spectra"){colnames(compton.frame.ag.hold) <- c("Spectrum", "Compton")}
        
        compton.ag.fake.Spectrum <- hold.frame$Spectrum
        compton.ag.fake.Compton <- rep(1, length(hold.frame$Spectrum))
        compton.ag.fake <- data.frame(compton.ag.fake.Spectrum,compton.ag.fake.Compton)
        colnames(compton.ag.fake) <- c("Spectrum", "Compton")
        
        
        compton.frame.ag <- if(input$filetype=="Spectra"){
            compton.frame.ag.hold
        }else if(input$filetype=="Net"){
            compton.ag.fake
        }
        
        
        
        
        
        predict.frame.comp <- data.frame(predict.frame$Concentration, intensity/compton.frame.ag$Compton)
        colnames(predict.frame.comp) <- c("Concentration", "Intensity")
        
        
        
        predict.intensity.comp <- data.frame(predict.frame.comp$Intensity)
        colnames(predict.intensity.comp) <- c("Intensity")
    }
    
    
    
    
        if (input$normcal==1){
            
            cal.lm <- lm(predict.frame$Concentration~predict.frame$Intensity)
            cal.lm.poly <- lm(predict.frame$Concentration~poly(predict.frame$Intensity, 2))
            
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
            
            
            
            cal.lm.comp <- lm(predict.frame.comp$Concentration~predict.frame.comp$Intensity)
            cal.lm.poly.comp <- lm(predict.frame.comp$Concentration~poly(predict.frame.comp$Intensity, 2))
            
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
            
            cal.lm.tc <- lm(predict.frame.tc$Concentration~predict.frame.tc$Intensity)
            cal.lm.poly.tc <- lm(predict.frame.tc$Concentration~poly(predict.frame.tc$Intensity, 2))
            
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
        
        intercept.none <- rep(0, length(spectra.line.table$Spectrum))
        lukas.intercept.table <- data.frame(spectra.line.table[input$show_vars], intercept.none)
        colnames(lukas.intercept.table) <- c(names(spectra.line.table[input$show_vars]), "None")
        
        
        
        slope.none <- rep(1, length(spectra.line.table$Spectrum))
        lukas.slope.table <- data.frame(spectra.line.table[input$show_vars], slope.none)
        colnames(lukas.slope.table) <- c(names(spectra.line.table[input$show_vars]), "None")
        
        if(input$normcal==1){
            lukas.intercept <- data.frame(rowSums(lukas.intercept.table[input$intercept_vars]))
            lukas.slope <- data.frame(lukas.slope.table[input$slope_vars])
            
            
            predict.frame.luk <- data.frame(predict.frame$Concentration, (intensity*lukas.intercept),lukas.slope)
            colnames(predict.frame.luk) <- c("Concentration", "Intensity", names(lukas.slope))
            
            
            
            predict.intensity.luk <- data.frame(predict.frame.luk$Intensity, lukas.slope)
            colnames(predict.intensity.luk) <- c("Intensity", names(lukas.slope))
            
            lukas.lm <- lm(Concentration~., data=predict.frame.luk)
            
            cal.est.conc.pred.luk <- predict(object=lukas.lm , newdata=predict.intensity.luk, interval='confidence')
            cal.est.conc.tab.luk <- data.frame(cal.est.conc.pred.luk)
            cal.est.conc.luk <- cal.est.conc.tab.luk$fit
            cal.est.conc.luk.up <- cal.est.conc.tab.luk$upr
            cal.est.conc.luk.low <- cal.est.conc.tab.luk$lwr
            
            
            val.frame.luk <- data.frame(predict.frame$Concentration, intensity, intensity/sum(intensity), cal.est.conc.luk, cal.est.conc.luk.up, cal.est.conc.luk.low)
            colnames(val.frame.luk) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
            
        }
        
        
        
        if(input$normcal==3){
            
            lukas.intercept.comp <- data.frame(rowSums(lukas.intercept.table[input$intercept_vars]))/compton.frame.ag$Compton
            
            
            
            lukas.slope.comp <- data.frame(rowSums(lukas.slope.table[input$slope_vars]))/compton.frame.ag$Compton
            
            
            
            
            lukas.intensity.comp <- (predict.frame.comp$Intensity+lukas.intercept.comp)*lukas.slope.comp
            
            
            predict.frame.luk.comp <- data.frame(predict.frame$Concentration, lukas.intensity.comp)
            colnames(predict.frame.luk.comp) <- c("Concentration", "Intensity")
            
            
            
            predict.intensity.luk.comp <- data.frame(predict.frame.luk.comp$Intensity)
            colnames(predict.intensity.luk.comp) <- c("Intensity")
            
            cal.lm.luk.comp <- lm(predict.frame.luk.comp$Concentration~predict.frame.luk.comp$Intensity)
            
            cal.est.conc.pred.luk.comp <- predict(object=cal.lm.luk.comp, newdata=predict.intensity.luk.comp, interval='confidence')
            cal.est.conc.tab.luk.comp <- data.frame(cal.est.conc.pred.luk.comp)
            cal.est.conc.luk.comp <- cal.est.conc.tab.luk.comp$fit
            
            val.frame.luk.comp <- data.frame(predict.frame$Concentration, cal.est.conc.luk.comp)
            colnames(val.frame.luk.comp) <- c("Concentration", "Prediction")
            
            
            
            
            
            lukas.intercept.comp <- data.frame(rowSums(lukas.intercept.table[input$intercept_vars]))/compton.frame.ag$Compton
            
            
            
            lukas.slope.comp <- data.frame(lukas.slope.table[input$slope_vars])/compton.frame.ag$Compton
            
            
            
            predict.frame.luk.comp <- data.frame(predict.frame$Concentration, (intensity/compton.frame.ag$Compton*lukas.intercept.comp),lukas.slope.comp)
            colnames(predict.frame.luk.comp) <- c("Concentration", "Intensity", names(lukas.slope.comp))
            
            
            
            predict.intensity.luk.comp <- data.frame(predict.frame.luk.comp$Intensity, lukas.slope.comp)
            colnames(predict.intensity.luk.comp) <- c("Intensity", names(lukas.slope.comp))
            
            
            
            lukas.lm.comp <- lm(Concentration~., data=predict.frame.luk.comp)
            
            cal.est.conc.pred.luk.comp <- predict(object=lukas.lm.comp , newdata=predict.intensity.luk.comp, interval='confidence')
            cal.est.conc.tab.luk.comp <- data.frame(cal.est.conc.pred.luk.comp)
            cal.est.conc.luk.comp <- cal.est.conc.tab.luk.comp$fit
            cal.est.conc.luk.up.comp <- cal.est.conc.tab.luk.comp$upr
            cal.est.conc.luk.low.comp <- cal.est.conc.tab.luk.comp$lwr
            
            
            val.frame.luk.comp <- data.frame(predict.frame$Concentration, intensity/compton.frame.ag$Compton, (intensity/compton.frame.ag$Compton)/sum(intensity/compton.frame.ag$Compton), cal.est.conc.luk.comp, cal.est.conc.luk.up.comp, cal.est.conc.luk.low.comp)
            colnames(val.frame.luk.comp) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
            
        }
        
        
        
        
        if(input$normcal==2){
            
            
            lukas.intercept.tc <- data.frame(rowSums(lukas.intercept.table[input$intercept_vars]))/total.counts$CPS
            lukas.slope.tc <- data.frame(lukas.slope.table[input$slope_vars])/total.counts$CPS
            
            
            predict.frame.luk.tc <- data.frame(predict.frame$Concentration, (intensity/total.counts$CPS*lukas.intercept.tc),lukas.slope.tc)
            colnames(predict.frame.luk.tc) <- c("Concentration", "Intensity", names(lukas.slope.tc))
            
            
            
            predict.intensity.luk.tc <- data.frame(predict.frame.luk.tc$Intensity, lukas.slope.tc)
            colnames(predict.intensity.luk.tc) <- c("Intensity", names(lukas.slope.tc))
            
            lukas.lm.tc <- lm(Concentration~., data=predict.frame.luk.tc)
            
            cal.est.conc.pred.luk.tc <- predict(object=lukas.lm.tc , newdata=predict.intensity.luk.tc, interval='confidence')
            cal.est.conc.tab.luk.tc <- data.frame(cal.est.conc.pred.luk.tc)
            cal.est.conc.luk.tc <- cal.est.conc.tab.luk.tc$fit
            cal.est.conc.luk.up.tc <- cal.est.conc.tab.luk.tc$upr
            cal.est.conc.luk.low.tc <- cal.est.conc.tab.luk.tc$lwr
            
            
            val.frame.luk.tc <- data.frame(predict.frame$Concentration, intensity/total.counts$CPS, (intensity/total.counts$CPS)/sum(intensity/total.counts$CPS), cal.est.conc.luk.tc, cal.est.conc.luk.up.tc, cal.est.conc.luk.low.tc)
            colnames(val.frame.luk.tc) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
            
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
        predict.frame.luk
    } else if (input$radiocal==3 && input$normcal==2){
        predict.frame.luk.tc
    } else if (input$radiocal==3 && input$normcal==3) {
        predict.frame.luk.comp
    } else if (is.na(input$calcurveelement)) {
        return()
    }
    
})



calValFrame <- reactive({
    
    data <- dataHold()
    
    
    concentration.table <- as.data.frame(values[["DF"]], stringsAsFactors=FALSE)
    concentration.table[concentration.table==""] <- 999
    
    spectra.line.table <- if(input$filetype=="Spectra"){
        spectraData()
    }else if(input$filetype=="Net"){
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
    
    predict.intensity <- data.frame(predict.frame$Intensity)
    colnames(predict.intensity) <- c("Intensity")
    
    if(input$normcal==2){
        
        if(input$filetype=="Spectra"){total.counts <- aggregate(CPS~Spectrum, data=data, sum)}
        if(input$filetype=="Spectra"){colnames(total.counts) <- c("Spectrum", "CPS")}
        
        if(input$filetype=="Net"){total.counts.net <- rowSums(spectra.line.table[2:length(spectra.line.table)])}
        if(input$filetype=="Net"){total.counts <- data.frame(spectra.line.table$Spectrum, total.counts.net)}
        if(input$filetype=="Net"){colnames(total.counts) <- c("Spectrum", "CPS")}
        
        predict.frame.tc <- data.frame(predict.frame$Concentration, intensity/total.counts$CPS)
        colnames(predict.frame.tc) <- c("Concentration", "Intensity")
        
        
        
        predict.intensity.tc <- data.frame(predict.frame.tc$Intensity)
        colnames(predict.intensity.tc) <- c("Intensity")
    }
    
    if(input$normcal==3){
        if(input$filetype=="Spectra"){compton.norm <- subset(data$CPS, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))}
        if(input$filetype=="Spectra"){compton.file <- subset(data$Spectrum, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))}
        if(input$filetype=="Spectra"){compton.frame <- data.frame(is.0(compton.norm, compton.file))}
        if(input$filetype=="Spectra"){colnames(compton.frame) <- c("Compton", "Spectrum")}
        if(input$filetype=="Spectra"){compton.frame.ag.hold <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")}
        if(input$filetype=="Spectra"){colnames(compton.frame.ag.hold) <- c("Spectrum", "Compton")}
        
        compton.ag.fake.Spectrum <- hold.frame$Spectrum
        compton.ag.fake.Compton <- rep(1, length(hold.frame$Spectrum))
        compton.ag.fake <- data.frame(compton.ag.fake.Spectrum,compton.ag.fake.Compton)
        colnames(compton.ag.fake) <- c("Spectrum", "Compton")
        
        
        compton.frame.ag <- if(input$filetype=="Spectra"){
            compton.frame.ag.hold
        }else if(input$filetype=="Net"){
            compton.ag.fake
        }
        
        
        
        
        
        predict.frame.comp <- data.frame(predict.frame$Concentration, intensity/compton.frame.ag$Compton)
        colnames(predict.frame.comp) <- c("Concentration", "Intensity")
        
        
        
        predict.intensity.comp <- data.frame(predict.frame.comp$Intensity)
        colnames(predict.intensity.comp) <- c("Intensity")
    }
    
    
    
    
        if (input$normcal==1){
            
            cal.lm <- lm(predict.frame$Concentration~predict.frame$Intensity)
            cal.lm.poly <- lm(predict.frame$Concentration~poly(predict.frame$Intensity, 2))
            
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
            
            
            
            cal.lm.comp <- lm(predict.frame.comp$Concentration~predict.frame.comp$Intensity)
            cal.lm.poly.comp <- lm(predict.frame.comp$Concentration~poly(predict.frame.comp$Intensity, 2))
            
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
            
            cal.lm.tc <- lm(predict.frame.tc$Concentration~predict.frame.tc$Intensity)
            cal.lm.poly.tc <- lm(predict.frame.tc$Concentration~poly(predict.frame.tc$Intensity, 2))
            
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
        
        intercept.none <- rep(0, length(spectra.line.table$Spectrum))
        lukas.intercept.table <- data.frame(spectra.line.table[input$show_vars], intercept.none)
        colnames(lukas.intercept.table) <- c(names(spectra.line.table[input$show_vars]), "None")
        
        
        
        slope.none <- rep(1, length(spectra.line.table$Spectrum))
        lukas.slope.table <- data.frame(spectra.line.table[input$show_vars], slope.none)
        colnames(lukas.slope.table) <- c(names(spectra.line.table[input$show_vars]), "None")
        
        if(input$normcal==1){
            lukas.intercept <- data.frame(rowSums(lukas.intercept.table[input$intercept_vars]))
            lukas.slope <- data.frame(lukas.slope.table[input$slope_vars])
            
            
            predict.frame.luk <- data.frame(predict.frame$Concentration, (intensity*lukas.intercept),lukas.slope)
            colnames(predict.frame.luk) <- c("Concentration", "Intensity", names(lukas.slope))
            
            
            
            predict.intensity.luk <- data.frame(predict.frame.luk$Intensity, lukas.slope)
            colnames(predict.intensity.luk) <- c("Intensity", names(lukas.slope))
            
            lukas.lm <- lm(Concentration~., data=predict.frame.luk)
            
            cal.est.conc.pred.luk <- predict(object=lukas.lm , newdata=predict.intensity.luk, interval='confidence')
            cal.est.conc.tab.luk <- data.frame(cal.est.conc.pred.luk)
            cal.est.conc.luk <- cal.est.conc.tab.luk$fit
            cal.est.conc.luk.up <- cal.est.conc.tab.luk$upr
            cal.est.conc.luk.low <- cal.est.conc.tab.luk$lwr
            
            
            val.frame.luk <- data.frame(predict.frame$Concentration, intensity, intensity/sum(intensity), cal.est.conc.luk, cal.est.conc.luk.up, cal.est.conc.luk.low)
            colnames(val.frame.luk) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
            
        }
        
        
        
        if(input$normcal==3){
            
            lukas.intercept.comp <- data.frame(rowSums(lukas.intercept.table[input$intercept_vars]))/compton.frame.ag$Compton
            
            
            
            lukas.slope.comp <- data.frame(rowSums(lukas.slope.table[input$slope_vars]))/compton.frame.ag$Compton
            
            
            
            
            lukas.intensity.comp <- (predict.frame.comp$Intensity+lukas.intercept.comp)*lukas.slope.comp
            
            
            predict.frame.luk.comp <- data.frame(predict.frame$Concentration, lukas.intensity.comp)
            colnames(predict.frame.luk.comp) <- c("Concentration", "Intensity")
            
            
            
            predict.intensity.luk.comp <- data.frame(predict.frame.luk.comp$Intensity)
            colnames(predict.intensity.luk.comp) <- c("Intensity")
            
            cal.lm.luk.comp <- lm(predict.frame.luk.comp$Concentration~predict.frame.luk.comp$Intensity)
            
            cal.est.conc.pred.luk.comp <- predict(object=cal.lm.luk.comp, newdata=predict.intensity.luk.comp, interval='confidence')
            cal.est.conc.tab.luk.comp <- data.frame(cal.est.conc.pred.luk.comp)
            cal.est.conc.luk.comp <- cal.est.conc.tab.luk.comp$fit
            
            val.frame.luk.comp <- data.frame(predict.frame$Concentration, cal.est.conc.luk.comp)
            colnames(val.frame.luk.comp) <- c("Concentration", "Prediction")
            
            
            
            
            
            lukas.intercept.comp <- data.frame(rowSums(lukas.intercept.table[input$intercept_vars]))/compton.frame.ag$Compton
            
            
            
            lukas.slope.comp <- data.frame(lukas.slope.table[input$slope_vars])/compton.frame.ag$Compton
            
            
            
            predict.frame.luk.comp <- data.frame(predict.frame$Concentration, (intensity/compton.frame.ag$Compton*lukas.intercept.comp),lukas.slope.comp)
            colnames(predict.frame.luk.comp) <- c("Concentration", "Intensity", names(lukas.slope.comp))
            
            
            
            predict.intensity.luk.comp <- data.frame(predict.frame.luk.comp$Intensity, lukas.slope.comp)
            colnames(predict.intensity.luk.comp) <- c("Intensity", names(lukas.slope.comp))
            
            
            
            lukas.lm.comp <- lm(Concentration~., data=predict.frame.luk.comp)
            
            cal.est.conc.pred.luk.comp <- predict(object=lukas.lm.comp , newdata=predict.intensity.luk.comp, interval='confidence')
            cal.est.conc.tab.luk.comp <- data.frame(cal.est.conc.pred.luk.comp)
            cal.est.conc.luk.comp <- cal.est.conc.tab.luk.comp$fit
            cal.est.conc.luk.up.comp <- cal.est.conc.tab.luk.comp$upr
            cal.est.conc.luk.low.comp <- cal.est.conc.tab.luk.comp$lwr
            
            
            val.frame.luk.comp <- data.frame(predict.frame$Concentration, intensity/compton.frame.ag$Compton, (intensity/compton.frame.ag$Compton)/sum(intensity/compton.frame.ag$Compton), cal.est.conc.luk.comp, cal.est.conc.luk.up.comp, cal.est.conc.luk.low.comp)
            colnames(val.frame.luk.comp) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
            
        }
        
        
        
        
        if(input$normcal==2){
            
            
            lukas.intercept.tc <- data.frame(rowSums(lukas.intercept.table[input$intercept_vars]))/total.counts$CPS
            lukas.slope.tc <- data.frame(lukas.slope.table[input$slope_vars])/total.counts$CPS
            
            
            predict.frame.luk.tc <- data.frame(predict.frame$Concentration, (intensity/total.counts$CPS*lukas.intercept.tc),lukas.slope.tc)
            colnames(predict.frame.luk.tc) <- c("Concentration", "Intensity", names(lukas.slope.tc))
            
            
            
            predict.intensity.luk.tc <- data.frame(predict.frame.luk.tc$Intensity, lukas.slope.tc)
            colnames(predict.intensity.luk.tc) <- c("Intensity", names(lukas.slope.tc))
            
            lukas.lm.tc <- lm(Concentration~., data=predict.frame.luk.tc)
            
            cal.est.conc.pred.luk.tc <- predict(object=lukas.lm.tc , newdata=predict.intensity.luk.tc, interval='confidence')
            cal.est.conc.tab.luk.tc <- data.frame(cal.est.conc.pred.luk.tc)
            cal.est.conc.luk.tc <- cal.est.conc.tab.luk.tc$fit
            cal.est.conc.luk.up.tc <- cal.est.conc.tab.luk.tc$upr
            cal.est.conc.luk.low.tc <- cal.est.conc.tab.luk.tc$lwr
            
            
            val.frame.luk.tc <- data.frame(predict.frame$Concentration, intensity/total.counts$CPS, (intensity/total.counts$CPS)/sum(intensity/total.counts$CPS), cal.est.conc.luk.tc, cal.est.conc.luk.up.tc, cal.est.conc.luk.low.tc)
            colnames(val.frame.luk.tc) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
            
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
        val.frame.luk
    } else if (input$radiocal==3 && input$normcal==2){
        val.frame.luk.tc
    } else if (input$radiocal==3 && input$normcal==3) {
        val.frame.luk.comp
    } else if (is.na(input$calcurveelement)) {
        return()
    }
    
})




calCurvePlot <- reactive({
    
    data <- dataHold()
    
    
    #if(input$usecalfile==TRUE && input$plot_cal_click==FALSE){vals$keeprows <- calFileContents()$calList[[input$calcurveelement]][[1]][[4]]}

    
    
    concentration.table <- as.data.frame(values[["DF"]], stringsAsFactors=FALSE)
    concentration.table[concentration.table==""] <- 999
    
    spectra.line.table <- if(input$filetype=="Spectra"){
        spectraData()
    }else if(input$filetype=="Net"){
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
    
    predict.intensity <- data.frame(predict.frame$Intensity)
    colnames(predict.intensity) <- c("Intensity")
    
    
    
    if(input$normcal==2){

    if(input$filetype=="Spectra"){total.counts <- aggregate(CPS~Spectrum, data=data, sum)}
    if(input$filetype=="Spectra"){colnames(total.counts) <- c("Spectrum", "CPS")}
    
    if(input$filetype=="Net"){total.counts.net <- rowSums(spectra.line.table[2:length(spectra.line.table)])}
    if(input$filetype=="Net"){total.counts <- data.frame(spectra.line.table$Spectrum, total.counts.net)}
    if(input$filetype=="Net"){colnames(total.counts) <- c("Spectrum", "CPS")}
    
    predict.frame.tc <- data.frame(predict.frame$Concentration, intensity/total.counts$CPS)
    colnames(predict.frame.tc) <- c("Concentration", "Intensity")
    
    
    
    predict.intensity.tc <- data.frame(predict.frame.tc$Intensity)
    colnames(predict.intensity.tc) <- c("Intensity")
    }
    
    if(input$normcal==3){
        if(input$filetype=="Spectra"){compton.norm <- subset(data$CPS, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))}
        if(input$filetype=="Spectra"){compton.file <- subset(data$Spectrum, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))}
        if(input$filetype=="Spectra"){compton.frame <- data.frame(is.0(compton.norm, compton.file))}
        if(input$filetype=="Spectra"){colnames(compton.frame) <- c("Compton", "Spectrum")}
        if(input$filetype=="Spectra"){compton.frame.ag.hold <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")}
        if(input$filetype=="Spectra"){colnames(compton.frame.ag.hold) <- c("Spectrum", "Compton")}
        
        compton.ag.fake.Spectrum <- hold.frame$Spectrum
        compton.ag.fake.Compton <- rep(1, length(hold.frame$Spectrum))
        compton.ag.fake <- data.frame(compton.ag.fake.Spectrum,compton.ag.fake.Compton)
        colnames(compton.ag.fake) <- c("Spectrum", "Compton")
        
        
        compton.frame.ag <- if(input$filetype=="Spectra"){
            compton.frame.ag.hold
        }else if(input$filetype=="Net"){
            compton.ag.fake
        }
        
        
        
        
        
        predict.frame.comp <- data.frame(predict.frame$Concentration, intensity/compton.frame.ag$Compton)
        colnames(predict.frame.comp) <- c("Concentration", "Intensity")
        
        
        
        predict.intensity.comp <- data.frame(predict.frame.comp$Intensity)
        colnames(predict.intensity.comp) <- c("Intensity")
    }
    
    
    

    if (input$normcal==1){

    cal.lm <- lm(predict.frame$Concentration~predict.frame$Intensity)
    cal.lm.poly <- lm(predict.frame$Concentration~poly(predict.frame$Intensity, 2))
    
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


    
    cal.lm.comp <- lm(predict.frame.comp$Concentration~predict.frame.comp$Intensity)
    cal.lm.poly.comp <- lm(predict.frame.comp$Concentration~poly(predict.frame.comp$Intensity, 2))
    
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
    
    cal.lm.tc <- lm(predict.frame.tc$Concentration~predict.frame.tc$Intensity)
    cal.lm.poly.tc <- lm(predict.frame.tc$Concentration~poly(predict.frame.tc$Intensity, 2))
    
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
        
        intercept.none <- rep(0, length(spectra.line.table$Spectrum))
        lukas.intercept.table <- data.frame(spectra.line.table[input$show_vars], intercept.none)
        colnames(lukas.intercept.table) <- c(names(spectra.line.table[input$show_vars]), "None")
        
        
        
        slope.none <- rep(1, length(spectra.line.table$Spectrum))
        lukas.slope.table <- data.frame(spectra.line.table[input$show_vars], slope.none)
        colnames(lukas.slope.table) <- c(names(spectra.line.table[input$show_vars]), "None")
        
        if(input$normcal==1){
    lukas.intercept <- data.frame(rowSums(lukas.intercept.table[input$intercept_vars]))
    lukas.slope <- data.frame(lukas.slope.table[input$slope_vars])
    
    
    predict.frame.luk <- data.frame(predict.frame$Concentration, (intensity*lukas.intercept),lukas.slope)
    colnames(predict.frame.luk) <- c("Concentration", "Intensity", names(lukas.slope))
    
    
    
    predict.intensity.luk <- data.frame(predict.frame.luk$Intensity, lukas.slope)
    colnames(predict.intensity.luk) <- c("Intensity", names(lukas.slope))
    
    lukas.lm <- lm(Concentration~., data=predict.frame.luk)
    
    cal.est.conc.pred.luk <- predict(object=lukas.lm , newdata=predict.intensity.luk, interval='confidence')
    cal.est.conc.tab.luk <- data.frame(cal.est.conc.pred.luk)
    cal.est.conc.luk <- cal.est.conc.tab.luk$fit
    cal.est.conc.luk.up <- cal.est.conc.tab.luk$upr
    cal.est.conc.luk.low <- cal.est.conc.tab.luk$lwr
    
    
    val.frame.luk <- data.frame(predict.frame$Concentration, intensity, intensity/sum(intensity), cal.est.conc.luk, cal.est.conc.luk.up, cal.est.conc.luk.low)
    colnames(val.frame.luk) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
    
        }
    
    
    
    if(input$normcal==3){
    
    lukas.intercept.comp <- data.frame(rowSums(lukas.intercept.table[input$intercept_vars]))/compton.frame.ag$Compton
    
    
    
    lukas.slope.comp <- data.frame(rowSums(lukas.slope.table[input$slope_vars]))/compton.frame.ag$Compton
    
    
    
    
    lukas.intensity.comp <- (predict.frame.comp$Intensity+lukas.intercept.comp)*lukas.slope.comp
    
    
    predict.frame.luk.comp <- data.frame(predict.frame$Concentration, lukas.intensity.comp)
    colnames(predict.frame.luk.comp) <- c("Concentration", "Intensity")
    
    
    
    predict.intensity.luk.comp <- data.frame(predict.frame.luk.comp$Intensity)
    colnames(predict.intensity.luk.comp) <- c("Intensity")
    
    cal.lm.luk.comp <- lm(predict.frame.luk.comp$Concentration~predict.frame.luk.comp$Intensity)
    
    cal.est.conc.pred.luk.comp <- predict(object=cal.lm.luk.comp, newdata=predict.intensity.luk.comp, interval='confidence')
    cal.est.conc.tab.luk.comp <- data.frame(cal.est.conc.pred.luk.comp)
    cal.est.conc.luk.comp <- cal.est.conc.tab.luk.comp$fit
    
    val.frame.luk.comp <- data.frame(predict.frame$Concentration, cal.est.conc.luk.comp)
    colnames(val.frame.luk.comp) <- c("Concentration", "Prediction")
    
    
    
    
    
    lukas.intercept.comp <- data.frame(rowSums(lukas.intercept.table[input$intercept_vars]))/compton.frame.ag$Compton
    
    
    
    lukas.slope.comp <- data.frame(lukas.slope.table[input$slope_vars])/compton.frame.ag$Compton
    
    
    
    predict.frame.luk.comp <- data.frame(predict.frame$Concentration, (intensity/compton.frame.ag$Compton*lukas.intercept.comp),lukas.slope.comp)
    colnames(predict.frame.luk.comp) <- c("Concentration", "Intensity", names(lukas.slope.comp))
    
    
    
    predict.intensity.luk.comp <- data.frame(predict.frame.luk.comp$Intensity, lukas.slope.comp)
    colnames(predict.intensity.luk.comp) <- c("Intensity", names(lukas.slope.comp))
    
    
    
    lukas.lm.comp <- lm(Concentration~., data=predict.frame.luk.comp)
    
    cal.est.conc.pred.luk.comp <- predict(object=lukas.lm.comp , newdata=predict.intensity.luk.comp, interval='confidence')
    cal.est.conc.tab.luk.comp <- data.frame(cal.est.conc.pred.luk.comp)
    cal.est.conc.luk.comp <- cal.est.conc.tab.luk.comp$fit
    cal.est.conc.luk.up.comp <- cal.est.conc.tab.luk.comp$upr
    cal.est.conc.luk.low.comp <- cal.est.conc.tab.luk.comp$lwr
    
    
    val.frame.luk.comp <- data.frame(predict.frame$Concentration, intensity/compton.frame.ag$Compton, (intensity/compton.frame.ag$Compton)/sum(intensity/compton.frame.ag$Compton), cal.est.conc.luk.comp, cal.est.conc.luk.up.comp, cal.est.conc.luk.low.comp)
    colnames(val.frame.luk.comp) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
    
    }
    
    
    
    
    if(input$normcal==2){
        
        
    lukas.intercept.tc <- data.frame(rowSums(lukas.intercept.table[input$intercept_vars]))/total.counts$CPS
    lukas.slope.tc <- data.frame(lukas.slope.table[input$slope_vars])/total.counts$CPS
    
    
    predict.frame.luk.tc <- data.frame(predict.frame$Concentration, (intensity/total.counts$CPS*lukas.intercept.tc),lukas.slope.tc)
    colnames(predict.frame.luk.tc) <- c("Concentration", "Intensity", names(lukas.slope.tc))
    
    
    
    predict.intensity.luk.tc <- data.frame(predict.frame.luk.tc$Intensity, lukas.slope.tc)
    colnames(predict.intensity.luk.tc) <- c("Intensity", names(lukas.slope.tc))
    
    lukas.lm.tc <- lm(Concentration~., data=predict.frame.luk.tc)
    
    cal.est.conc.pred.luk.tc <- predict(object=lukas.lm.tc , newdata=predict.intensity.luk.tc, interval='confidence')
    cal.est.conc.tab.luk.tc <- data.frame(cal.est.conc.pred.luk.tc)
    cal.est.conc.luk.tc <- cal.est.conc.tab.luk.tc$fit
    cal.est.conc.luk.up.tc <- cal.est.conc.tab.luk.tc$upr
    cal.est.conc.luk.low.tc <- cal.est.conc.tab.luk.tc$lwr
    
    
    val.frame.luk.tc <- data.frame(predict.frame$Concentration, intensity/total.counts$CPS, (intensity/total.counts$CPS)/sum(intensity/total.counts$CPS), cal.est.conc.luk.tc, cal.est.conc.luk.up.tc, cal.est.conc.luk.low.tc)
    colnames(val.frame.luk.tc) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
    
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
    annotate("text", label=lm_eqn_poly(lm(Concentration~poly(Intensity, 2), predict.frame.comp)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
    geom_point() +
    geom_point(data = predict.frame.comp[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
    stat_smooth(method="lm", formula=y~poly(x,2)) +
    scale_x_continuous(paste(element.name, norma.comp)) +
    scale_y_continuous(paste(element.name, conen))
    }
    
    }
    
    
    if (input$radiocal==3){
    
    ####Lukas-Tooth  Calibration Curve Model Display, Time Normalized
    
    if(input$normcal==1){
    calcurve.linear.plot.luk <- ggplot(data=val.frame.luk[ vals$keeprows, , drop = FALSE], aes(IntensityNorm, Concentration)) +
    theme_light() +
    annotate("text", label=lm_eqn(lm(Concentration~IntensityNorm, val.frame.luk[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
    geom_point() +
    geom_point(aes(IntensityNorm, Concentration), data = val.frame.luk[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
    geom_smooth(aes(x=IntensityNorm, y=Concentration, ymin = Lower, ymax = Upper)) +
    scale_x_continuous(paste(element.name, norma)) +
    scale_y_continuous(paste(element.name, conen))
    }
    
    
    
    ####Lukas-Tooth Calibration Curve Model Display, Total Count Normalized
    if(input$normcal==2){
        
        
    calcurve.linear.plot.luk.tc <- ggplot(data=val.frame.luk.tc[ vals$keeprows, , drop = FALSE], aes(IntensityNorm, Concentration)) +
    theme_light() +
    annotate("text", label=lm_eqn(lm(Concentration~Intensity, predict.frame.luk.tc[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
    geom_point() +
    geom_point(aes(IntensityNorm, Concentration), data = val.frame.luk.tc[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
    geom_smooth(aes(x=IntensityNorm, y=Concentration, ymin = Lower, ymax = Upper)) +
    scale_x_continuous(paste(element.name, norma.tc)) +
    scale_y_continuous(paste(element.name, conen))
    }
    

    
    ####Lukas-Tooth Calibration Curve Model Display, Compton Normalized
    
    if(input$normcal==3){
    calcurve.linear.plot.luk.comp <- ggplot(data=val.frame.luk.comp[ vals$keeprows, , drop = FALSE], aes(IntensityNorm, Concentration)) +
    theme_light() +
    annotate("text", label=lm_eqn(lm(Concentration~Intensity, predict.frame.luk.comp[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
    geom_point() +
    geom_point(aes(IntensityNorm, Concentration), data = val.frame.luk.comp[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
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
        calcurve.linear.plot.luk
    } else if (input$radiocal==3 && input$normcal==2){
        calcurve.linear.plot.luk.tc
    } else if (input$radiocal==3 && input$normcal==3) {
        calcurve.linear.plot.luk.comp
    } else if (is.na(input$calcurveelement)) {
        return()
    }
    
})

observeEvent(input$createcalplot, {
})

output$calcurveplots <- renderPlot({
    calCurvePlot()
})


valCurvePlot <- reactive({
    
    data <- dataHold()
    
    
    #if(input$usecalfile==TRUE && input$plot_val_click==FALSE){vals$keeprows <- calFileContents()$calList[[input$calcurveelement]][[1]][[4]]}
    
    concentration.table <- as.data.frame(values[["DF"]], stringsAsFactors=FALSE)
    concentration.table[concentration.table==""] <- 999
    
    spectra.line.table <- if(input$filetype=="Spectra"){
        spectraData()
    }else if(input$filetype=="Net"){
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
    
    predict.intensity <- data.frame(predict.frame$Intensity)
    colnames(predict.intensity) <- c("Intensity")
    
    if(input$normcal==2){
        
        if(input$filetype=="Spectra"){total.counts <- aggregate(CPS~Spectrum, data=data, sum)}
        if(input$filetype=="Spectra"){colnames(total.counts) <- c("Spectrum", "CPS")}
        
        if(input$filetype=="Net"){total.counts.net <- rowSums(spectra.line.table[2:length(spectra.line.table)])}
        if(input$filetype=="Net"){total.counts <- data.frame(spectra.line.table$Spectrum, total.counts.net)}
        if(input$filetype=="Net"){colnames(total.counts) <- c("Spectrum", "CPS")}
        
        predict.frame.tc <- data.frame(predict.frame$Concentration, intensity/total.counts$CPS)
        colnames(predict.frame.tc) <- c("Concentration", "Intensity")
        
        
        
        predict.intensity.tc <- data.frame(predict.frame.tc$Intensity)
        colnames(predict.intensity.tc) <- c("Intensity")
    }
    
    if(input$normcal==3){
        if(input$filetype=="Spectra"){compton.norm <- subset(data$CPS, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))}
        if(input$filetype=="Spectra"){compton.file <- subset(data$Spectrum, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))}
        if(input$filetype=="Spectra"){compton.frame <- data.frame(is.0(compton.norm, compton.file))}
        if(input$filetype=="Spectra"){colnames(compton.frame) <- c("Compton", "Spectrum")}
        if(input$filetype=="Spectra"){compton.frame.ag.hold <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")}
        if(input$filetype=="Spectra"){colnames(compton.frame.ag.hold) <- c("Spectrum", "Compton")}
        
        compton.ag.fake.Spectrum <- hold.frame$Spectrum
        compton.ag.fake.Compton <- rep(1, length(hold.frame$Spectrum))
        compton.ag.fake <- data.frame(compton.ag.fake.Spectrum,compton.ag.fake.Compton)
        colnames(compton.ag.fake) <- c("Spectrum", "Compton")
        
        
        compton.frame.ag <- if(input$filetype=="Spectra"){
            compton.frame.ag.hold
        }else if(input$filetype=="Net"){
            compton.ag.fake
        }
        
        
        
        
        
        predict.frame.comp <- data.frame(predict.frame$Concentration, intensity/compton.frame.ag$Compton)
        colnames(predict.frame.comp) <- c("Concentration", "Intensity")
        
        
        
        predict.intensity.comp <- data.frame(predict.frame.comp$Intensity)
        colnames(predict.intensity.comp) <- c("Intensity")
    }
    
    
    
    
        if (input$normcal==1){
            
            cal.lm <- lm(predict.frame$Concentration~predict.frame$Intensity)
            cal.lm.poly <- lm(predict.frame$Concentration~poly(predict.frame$Intensity, 2))
            
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
            
            
            
            cal.lm.comp <- lm(predict.frame.comp$Concentration~predict.frame.comp$Intensity)
            cal.lm.poly.comp <- lm(predict.frame.comp$Concentration~poly(predict.frame.comp$Intensity, 2))
            
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
            
            cal.lm.tc <- lm(predict.frame.tc$Concentration~predict.frame.tc$Intensity)
            cal.lm.poly.tc <- lm(predict.frame.tc$Concentration~poly(predict.frame.tc$Intensity, 2))
            
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
        
        intercept.none <- rep(0, length(spectra.line.table$Spectrum))
        lukas.intercept.table <- data.frame(spectra.line.table[input$show_vars], intercept.none)
        colnames(lukas.intercept.table) <- c(names(spectra.line.table[input$show_vars]), "None")
        
        
        
        slope.none <- rep(1, length(spectra.line.table$Spectrum))
        lukas.slope.table <- data.frame(spectra.line.table[input$show_vars], slope.none)
        colnames(lukas.slope.table) <- c(names(spectra.line.table[input$show_vars]), "None")
        
        if(input$normcal==1){
            lukas.intercept <- data.frame(rowSums(lukas.intercept.table[input$intercept_vars]))
            lukas.slope <- data.frame(lukas.slope.table[input$slope_vars])
            
            
            predict.frame.luk <- data.frame(predict.frame$Concentration, (intensity*lukas.intercept),lukas.slope)
            colnames(predict.frame.luk) <- c("Concentration", "Intensity", names(lukas.slope))
            
            
            
            predict.intensity.luk <- data.frame(predict.frame.luk$Intensity, lukas.slope)
            colnames(predict.intensity.luk) <- c("Intensity", names(lukas.slope))
            
            lukas.lm <- lm(Concentration~., data=predict.frame.luk)
            
            cal.est.conc.pred.luk <- predict(object=lukas.lm , newdata=predict.intensity.luk, interval='confidence')
            cal.est.conc.tab.luk <- data.frame(cal.est.conc.pred.luk)
            cal.est.conc.luk <- cal.est.conc.tab.luk$fit
            cal.est.conc.luk.up <- cal.est.conc.tab.luk$upr
            cal.est.conc.luk.low <- cal.est.conc.tab.luk$lwr
            
            
            val.frame.luk <- data.frame(predict.frame$Concentration, intensity, intensity/sum(intensity), cal.est.conc.luk, cal.est.conc.luk.up, cal.est.conc.luk.low)
            colnames(val.frame.luk) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
            
        }
        
        
        
        if(input$normcal==3){
            
            lukas.intercept.comp <- data.frame(rowSums(lukas.intercept.table[input$intercept_vars]))/compton.frame.ag$Compton
            
            
            
            lukas.slope.comp <- data.frame(rowSums(lukas.slope.table[input$slope_vars]))/compton.frame.ag$Compton
            
            
            
            
            lukas.intensity.comp <- (predict.frame.comp$Intensity+lukas.intercept.comp)*lukas.slope.comp
            
            
            predict.frame.luk.comp <- data.frame(predict.frame$Concentration, lukas.intensity.comp)
            colnames(predict.frame.luk.comp) <- c("Concentration", "Intensity")
            
            
            
            predict.intensity.luk.comp <- data.frame(predict.frame.luk.comp$Intensity)
            colnames(predict.intensity.luk.comp) <- c("Intensity")
            
            cal.lm.luk.comp <- lm(predict.frame.luk.comp$Concentration~predict.frame.luk.comp$Intensity)
            
            cal.est.conc.pred.luk.comp <- predict(object=cal.lm.luk.comp, newdata=predict.intensity.luk.comp, interval='confidence')
            cal.est.conc.tab.luk.comp <- data.frame(cal.est.conc.pred.luk.comp)
            cal.est.conc.luk.comp <- cal.est.conc.tab.luk.comp$fit
            
            val.frame.luk.comp <- data.frame(predict.frame$Concentration, cal.est.conc.luk.comp)
            colnames(val.frame.luk.comp) <- c("Concentration", "Prediction")
            
            
            
            
            
            lukas.intercept.comp <- data.frame(rowSums(lukas.intercept.table[input$intercept_vars]))/compton.frame.ag$Compton
            
            
            
            lukas.slope.comp <- data.frame(lukas.slope.table[input$slope_vars])/compton.frame.ag$Compton
            
            
            
            predict.frame.luk.comp <- data.frame(predict.frame$Concentration, (intensity/compton.frame.ag$Compton*lukas.intercept.comp),lukas.slope.comp)
            colnames(predict.frame.luk.comp) <- c("Concentration", "Intensity", names(lukas.slope.comp))
            
            
            
            predict.intensity.luk.comp <- data.frame(predict.frame.luk.comp$Intensity, lukas.slope.comp)
            colnames(predict.intensity.luk.comp) <- c("Intensity", names(lukas.slope.comp))
            
            
            
            lukas.lm.comp <- lm(Concentration~., data=predict.frame.luk.comp)
            
            cal.est.conc.pred.luk.comp <- predict(object=lukas.lm.comp , newdata=predict.intensity.luk.comp, interval='confidence')
            cal.est.conc.tab.luk.comp <- data.frame(cal.est.conc.pred.luk.comp)
            cal.est.conc.luk.comp <- cal.est.conc.tab.luk.comp$fit
            cal.est.conc.luk.up.comp <- cal.est.conc.tab.luk.comp$upr
            cal.est.conc.luk.low.comp <- cal.est.conc.tab.luk.comp$lwr
            
            
            val.frame.luk.comp <- data.frame(predict.frame$Concentration, intensity/compton.frame.ag$Compton, (intensity/compton.frame.ag$Compton)/sum(intensity/compton.frame.ag$Compton), cal.est.conc.luk.comp, cal.est.conc.luk.up.comp, cal.est.conc.luk.low.comp)
            colnames(val.frame.luk.comp) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
            
        }
        
        
        
        
        if(input$normcal==2){
            
            
            lukas.intercept.tc <- data.frame(rowSums(lukas.intercept.table[input$intercept_vars]))/total.counts$CPS
            lukas.slope.tc <- data.frame(lukas.slope.table[input$slope_vars])/total.counts$CPS
            
            
            predict.frame.luk.tc <- data.frame(predict.frame$Concentration, (intensity/total.counts$CPS*lukas.intercept.tc),lukas.slope.tc)
            colnames(predict.frame.luk.tc) <- c("Concentration", "Intensity", names(lukas.slope.tc))
            
            
            
            predict.intensity.luk.tc <- data.frame(predict.frame.luk.tc$Intensity, lukas.slope.tc)
            colnames(predict.intensity.luk.tc) <- c("Intensity", names(lukas.slope.tc))
            
            lukas.lm.tc <- lm(Concentration~., data=predict.frame.luk.tc)
            
            cal.est.conc.pred.luk.tc <- predict(object=lukas.lm.tc , newdata=predict.intensity.luk.tc, interval='confidence')
            cal.est.conc.tab.luk.tc <- data.frame(cal.est.conc.pred.luk.tc)
            cal.est.conc.luk.tc <- cal.est.conc.tab.luk.tc$fit
            cal.est.conc.luk.up.tc <- cal.est.conc.tab.luk.tc$upr
            cal.est.conc.luk.low.tc <- cal.est.conc.tab.luk.tc$lwr
            
            
            val.frame.luk.tc <- data.frame(predict.frame$Concentration, intensity/total.counts$CPS, (intensity/total.counts$CPS)/sum(intensity/total.counts$CPS), cal.est.conc.luk.tc, cal.est.conc.luk.up.tc, cal.est.conc.luk.low.tc)
            colnames(val.frame.luk.tc) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
            
        }
        
    }
    
    
    if (input$radiocal!=3){


    ####Linear Calibration Curve Model Display, Time Normalized
    if(input$normcal==1){
        
        calval.linear.plot <- ggplot(data=val.frame[ vals$keeprows, , drop = FALSE], aes(Prediction, Concentration)) +
        theme_bw() +
        annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        stat_smooth(method="lm") +
        geom_point() +
        geom_point(data = val.frame[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
        scale_x_continuous(paste(element.name, predi)) +
        scale_y_continuous(paste(element.name, conen))
        
        calval.poly.plot <- ggplot(data=val.frame.poly[ vals$keeprows, , drop = FALSE], aes(Prediction, Concentration)) +
        theme_bw() +
        annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.poly[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        stat_smooth(method="lm") +
        geom_point() +
        geom_point(data = val.frame.poly[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
        scale_x_continuous(paste(element.name, predi)) +
        scale_y_continuous(paste(element.name, conen))
    }
    
    
    
    ####Linear Calibration Curve Model Display, Total Count Normalized
    if(input$normcal==2){
        
        calval.linear.plot.tc <- ggplot(data=val.frame.tc[ vals$keeprows, , drop = FALSE], aes(Prediction, Concentration)) +
        theme_bw() +
        annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.tc[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        stat_smooth(method="lm") +
        geom_point() +
        geom_point(data = val.frame.tc[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
        scale_x_continuous(paste(element.name, predi)) +
        scale_y_continuous(paste(element.name, conen))
        
        calval.poly.plot.tc <- ggplot(data=val.frame.poly.tc[ vals$keeprows, , drop = FALSE], aes(Prediction, Concentration)) +
        theme_bw() +
        annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.poly.tc[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        stat_smooth(method="lm") +
        geom_point() +
        geom_point(data = val.frame.poly.tc[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
        scale_x_continuous(paste(element.name, predi)) +
        scale_y_continuous(paste(element.name, conen))
    }
    
    
    ####Linear Calibration Curve Model Display, Compton Normalized
    if(input$normcal==3){
        
        calval.linear.plot.comp <- ggplot(data=val.frame.comp[ vals$keeprows, , drop = FALSE], aes(Prediction, Concentration)) +
        theme_bw() +
        annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.comp[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        stat_smooth(method="lm") +
        geom_point() +
        geom_point(data = val.frame.comp[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
        scale_x_continuous(paste(element.name, predi)) +
        scale_y_continuous(paste(element.name, conen))
        
        calval.poly.plot.comp <- ggplot(data=val.frame.poly.comp[ vals$keeprows, , drop = FALSE], aes(Prediction, Concentration)) +
        theme_bw() +
        annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.poly.comp[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        stat_smooth(method="lm") +
        geom_point() +
        geom_point(data = val.frame.poly.comp[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
        scale_x_continuous(paste(element.name, predi)) +
        scale_y_continuous(paste(element.name, conen))

    }
    
    }
    
    
    if (input$radiocal==3){
        
        ####Lukas-Tooth  Calibration Curve Model Display, Time Normalized
        
        if(input$normcal==1){
            
            calval.linear.plot.luk <- ggplot(data=val.frame.luk[ vals$keeprows, , drop = FALSE], aes(Prediction, Concentration)) +
            theme_bw() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.luk[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            stat_smooth(method="lm") +
            geom_point() +
            geom_point(aes(IntensityNorm, Concentration), data = val.frame.luk[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
            scale_x_continuous(paste(element.name, predi)) +
            scale_y_continuous(paste(element.name, conen))
        }
        
        
        
        ####Lukas-Tooth Calibration Curve Model Display, Total Count Normalized
        if(input$normcal==2){

            calval.linear.plot.luk.tc <- ggplot(data=val.frame.luk.tc[ vals$keeprows, , drop = FALSE], aes(Prediction, Concentration)) +
            theme_bw() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.luk.tc[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            stat_smooth(method="lm") +
            geom_point() +
            geom_point(aes(IntensityNorm, Concentration), data = val.frame.luk.tc[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
            scale_x_continuous(paste(element.name, predi)) +
            scale_y_continuous(paste(element.name, conen))

        }
        
        
        
        ####Lukas-Tooth Calibration Curve Model Display, Compton Normalized
        
        if(input$normcal==3){
            
            calval.linear.plot.luk.comp <- ggplot(data=val.frame.luk.comp[ vals$keeprows, , drop = FALSE], aes(Prediction, Concentration)) +
            theme_bw() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.luk.comp[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            stat_smooth(method="lm") +
            geom_point() +
            geom_point(aes(IntensityNorm, Concentration), data = val.frame.luk.comp[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
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
        calval.linear.plot.luk
    } else if (input$radiocal==3 && input$normcal==2){
        calval.linear.plot.luk.tc
    } else if (input$radiocal==3 && input$normcal==3) {
        calval.linear.plot.luk.comp
    } else if (is.na(input$calcurveelement)) {
        return()
    }
    
})

output$valcurveplots <- renderPlot({
    valCurvePlot()
})



plotInput2 <- reactive({
    
    data <- dataHold()
    
    
    
    
    concentration.table <- as.data.frame(values[["DF"]], stringsAsFactors=FALSE)
    concentration.table[concentration.table==""] <- 999
    
    spectra.line.table <- if(input$filetype=="Spectra"){
        spectraData()
    }else if(input$filetype=="Net"){
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
    
    predict.intensity <- data.frame(predict.frame$Intensity)
    colnames(predict.intensity) <- c("Intensity")
    
    if(input$normcal==1){
        
        cal.lm <- lm(predict.frame$Concentration~predict.frame$Intensity)
        cal.lm.poly <- lm(predict.frame$Concentration~poly(predict.frame$Intensity, 2))
        
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


    if(input$normcal==2){
        
        if(input$filetype=="Spectra"){total.counts <- aggregate(CPS~Spectrum, data=data, sum)}
        if(input$filetype=="Spectra"){colnames(total.counts) <- c("Spectrum", "CPS")}
        
        if(input$filetype=="Net"){total.counts.net <- rowSums(spectra.line.table[2:length(spectra.line.table)])}
        if(input$filetype=="Net"){total.counts <- data.frame(spectra.line.table$Spectrum, total.counts.net)}
        if(input$filetype=="Net"){colnames(total.counts) <- c("Spectrum", "CPS")}
        
        predict.frame.tc <- data.frame(predict.frame$Concentration, intensity/total.counts$CPS)
        colnames(predict.frame.tc) <- c("Concentration", "Intensity")
        
        
        
        predict.intensity.tc <- data.frame(predict.frame.tc$Intensity)
        colnames(predict.intensity.tc) <- c("Intensity")
    }
    
    if(input$normcal==3){
        if(input$filetype=="Spectra"){compton.norm <- subset(data$CPS, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))}
        if(input$filetype=="Spectra"){compton.file <- subset(data$Spectrum, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))}
        if(input$filetype=="Spectra"){compton.frame <- data.frame(is.0(compton.norm, compton.file))}
        if(input$filetype=="Spectra"){colnames(compton.frame) <- c("Compton", "Spectrum")}
        if(input$filetype=="Spectra"){compton.frame.ag.hold <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")}
        if(input$filetype=="Spectra"){colnames(compton.frame.ag.hold) <- c("Spectrum", "Compton")}
        
        compton.ag.fake.Spectrum <- hold.frame$Spectrum
        compton.ag.fake.Compton <- rep(1, length(hold.frame$Spectrum))
        compton.ag.fake <- data.frame(compton.ag.fake.Spectrum,compton.ag.fake.Compton)
        colnames(compton.ag.fake) <- c("Spectrum", "Compton")
        
        
        compton.frame.ag <- if(input$filetype=="Spectra"){
            compton.frame.ag.hold
        }else if(input$filetype=="Net"){
            compton.ag.fake
        }
        
        
        
        
        
        predict.frame.comp <- data.frame(predict.frame$Concentration, intensity/compton.frame.ag$Compton)
        colnames(predict.frame.comp) <- c("Concentration", "Intensity")
        
        
        
        predict.intensity.comp <- data.frame(predict.frame.comp$Intensity)
        colnames(predict.intensity.comp) <- c("Intensity")
    }
    
    
    
    if (input$radiocal!=3){
        
        if (input$normcal==1){
            
            cal.lm <- lm(predict.frame$Concentration~predict.frame$Intensity)
            cal.lm.poly <- lm(predict.frame$Concentration~poly(predict.frame$Intensity, 2))
            
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
            
            
            
            cal.lm.comp <- lm(predict.frame.comp$Concentration~predict.frame.comp$Intensity)
            cal.lm.poly.comp <- lm(predict.frame.comp$Concentration~poly(predict.frame.comp$Intensity, 2))
            
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
            
            cal.lm.tc <- lm(predict.frame.tc$Concentration~predict.frame.tc$Intensity)
            cal.lm.poly.tc <- lm(predict.frame.tc$Concentration~poly(predict.frame.tc$Intensity, 2))
            
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
        
    }
    
    
    
    
    ####Fourth Iteration
    
    if (input$radiocal==3){
        
        intercept.none <- rep(0, length(spectra.line.table$Spectrum))
        lukas.intercept.table <- data.frame(spectra.line.table[input$show_vars], intercept.none)
        colnames(lukas.intercept.table) <- c(names(spectra.line.table[input$show_vars]), "None")
        
        
        
        slope.none <- rep(1, length(spectra.line.table$Spectrum))
        lukas.slope.table <- data.frame(spectra.line.table[input$show_vars], slope.none)
        colnames(lukas.slope.table) <- c(names(spectra.line.table[input$show_vars]), "None")
        
        if(input$normcal==1){
            lukas.intercept <- data.frame(rowSums(lukas.intercept.table[input$intercept_vars]))
            lukas.slope <- data.frame(lukas.slope.table[input$slope_vars])
            
            
            predict.frame.luk <- data.frame(predict.frame$Concentration, (intensity*lukas.intercept),lukas.slope)
            colnames(predict.frame.luk) <- c("Concentration", "Intensity", names(lukas.slope))
            
            
            
            predict.intensity.luk <- data.frame(predict.frame.luk$Intensity, lukas.slope)
            colnames(predict.intensity.luk) <- c("Intensity", names(lukas.slope))
            
            lukas.lm <- lm(Concentration~., data=predict.frame.luk)
            
            cal.est.conc.pred.luk <- predict(object=lukas.lm , newdata=predict.intensity.luk, interval='confidence')
            cal.est.conc.tab.luk <- data.frame(cal.est.conc.pred.luk)
            cal.est.conc.luk <- cal.est.conc.tab.luk$fit
            cal.est.conc.luk.up <- cal.est.conc.tab.luk$upr
            cal.est.conc.luk.low <- cal.est.conc.tab.luk$lwr
            
            
            val.frame.luk <- data.frame(predict.frame$Concentration, intensity, intensity/sum(intensity), cal.est.conc.luk, cal.est.conc.luk.up, cal.est.conc.luk.low)
            colnames(val.frame.luk) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
            
        }
        
        
        
        if(input$normcal==3){
            
            lukas.intercept.comp <- data.frame(rowSums(lukas.intercept.table[input$intercept_vars]))/compton.frame.ag$Compton
            
            
            
            lukas.slope.comp <- data.frame(rowSums(lukas.slope.table[input$slope_vars]))/compton.frame.ag$Compton
            
            
            
            
            lukas.intensity.comp <- (predict.frame.comp$Intensity+lukas.intercept.comp)*lukas.slope.comp
            
            
            predict.frame.luk.comp <- data.frame(predict.frame$Concentration, lukas.intensity.comp)
            colnames(predict.frame.luk.comp) <- c("Concentration", "Intensity")
            
            
            
            predict.intensity.luk.comp <- data.frame(predict.frame.luk.comp$Intensity)
            colnames(predict.intensity.luk.comp) <- c("Intensity")
            
            cal.lm.luk.comp <- lm(predict.frame.luk.comp$Concentration~predict.frame.luk.comp$Intensity)
            
            cal.est.conc.pred.luk.comp <- predict(object=cal.lm.luk.comp, newdata=predict.intensity.luk.comp, interval='confidence')
            cal.est.conc.tab.luk.comp <- data.frame(cal.est.conc.pred.luk.comp)
            cal.est.conc.luk.comp <- cal.est.conc.tab.luk.comp$fit
            
            val.frame.luk.comp <- data.frame(predict.frame$Concentration, cal.est.conc.luk.comp)
            colnames(val.frame.luk.comp) <- c("Concentration", "Prediction")
            
            
            
            
            
            lukas.intercept.comp <- data.frame(rowSums(lukas.intercept.table[input$intercept_vars]))/compton.frame.ag$Compton
            
            
            
            lukas.slope.comp <- data.frame(lukas.slope.table[input$slope_vars])/compton.frame.ag$Compton
            
            
            
            predict.frame.luk.comp <- data.frame(predict.frame$Concentration, (intensity/compton.frame.ag$Compton*lukas.intercept.comp),lukas.slope.comp)
            colnames(predict.frame.luk.comp) <- c("Concentration", "Intensity", names(lukas.slope.comp))
            
            
            
            predict.intensity.luk.comp <- data.frame(predict.frame.luk.comp$Intensity, lukas.slope.comp)
            colnames(predict.intensity.luk.comp) <- c("Intensity", names(lukas.slope.comp))
            
            
            
            lukas.lm.comp <- lm(Concentration~., data=predict.frame.luk.comp)
            
            cal.est.conc.pred.luk.comp <- predict(object=lukas.lm.comp , newdata=predict.intensity.luk.comp, interval='confidence')
            cal.est.conc.tab.luk.comp <- data.frame(cal.est.conc.pred.luk.comp)
            cal.est.conc.luk.comp <- cal.est.conc.tab.luk.comp$fit
            cal.est.conc.luk.up.comp <- cal.est.conc.tab.luk.comp$upr
            cal.est.conc.luk.low.comp <- cal.est.conc.tab.luk.comp$lwr
            
            
            val.frame.luk.comp <- data.frame(predict.frame$Concentration, intensity/compton.frame.ag$Compton, (intensity/compton.frame.ag$Compton)/sum(intensity/compton.frame.ag$Compton), cal.est.conc.luk.comp, cal.est.conc.luk.up.comp, cal.est.conc.luk.low.comp)
            colnames(val.frame.luk.comp) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
            
        }
        
        
        
        
        if(input$normcal==2){
            
            
            lukas.intercept.tc <- data.frame(rowSums(lukas.intercept.table[input$intercept_vars]))/total.counts$CPS
            lukas.slope.tc <- data.frame(lukas.slope.table[input$slope_vars])/total.counts$CPS
            
            
            predict.frame.luk.tc <- data.frame(predict.frame$Concentration, (intensity/total.counts$CPS*lukas.intercept.tc),lukas.slope.tc)
            colnames(predict.frame.luk.tc) <- c("Concentration", "Intensity", names(lukas.slope.tc))
            
            
            
            predict.intensity.luk.tc <- data.frame(predict.frame.luk.tc$Intensity, lukas.slope.tc)
            colnames(predict.intensity.luk.tc) <- c("Intensity", names(lukas.slope.tc))
            
            lukas.lm.tc <- lm(Concentration~., data=predict.frame.luk.tc)
            
            cal.est.conc.pred.luk.tc <- predict(object=lukas.lm.tc , newdata=predict.intensity.luk.tc, interval='confidence')
            cal.est.conc.tab.luk.tc <- data.frame(cal.est.conc.pred.luk.tc)
            cal.est.conc.luk.tc <- cal.est.conc.tab.luk.tc$fit
            cal.est.conc.luk.up.tc <- cal.est.conc.tab.luk.tc$upr
            cal.est.conc.luk.low.tc <- cal.est.conc.tab.luk.tc$lwr
            
            
            val.frame.luk.tc <- data.frame(predict.frame$Concentration, intensity/total.counts$CPS, (intensity/total.counts$CPS)/sum(intensity/total.counts$CPS), cal.est.conc.luk.tc, cal.est.conc.luk.up.tc, cal.est.conc.luk.low.tc)
            colnames(val.frame.luk.tc) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
            
        }
        
    }
    
    
    
    
    
    
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
        
        calval.linear.plot <- ggplot(data=val.frame, aes(Prediction, Concentration)) +
        theme_bw() +
        annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        stat_smooth(method="lm") +
        geom_point() +
        scale_x_continuous(paste(element.name, predi)) +
        scale_y_continuous(paste(element.name, conen))
        
        
        calcurve.poly.plot <- ggplot(data=predict.frame[ vals$keeprows, , drop = FALSE], aes(Intensity, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn_poly(lm(Concentration~poly(Intensity, 2), predict.frame)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        geom_point() +
        geom_point(data = predict.frame[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
        stat_smooth(method="lm", formula=y~poly(x,2)) +
        scale_x_continuous(paste(element.name, intens)) +
        scale_y_continuous(paste(element.name, conen))
        
        calval.poly.plot <- ggplot(data=val.frame.poly, aes(Prediction, Concentration)) +
        theme_bw() +
        annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.poly)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        stat_smooth(method="lm") +
        geom_point() +
        scale_x_continuous(paste(element.name, predi)) +
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
        
        calval.linear.plot.tc <- ggplot(data=val.frame.tc, aes(Prediction, Concentration)) +
        theme_bw() +
        annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.tc)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        stat_smooth(method="lm") +
        geom_point() +
        scale_x_continuous(paste(element.name, predi)) +
        scale_y_continuous(paste(element.name, conen))
        
        calcurve.poly.plot.tc <- ggplot(data=predict.frame.tc, aes(Intensity, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn_poly(lm(Concentration~poly(Intensity, 2), predict.frame.tc[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        geom_point() +
        geom_point(data = predict.frame.tc[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
        stat_smooth(method="lm", formula=y~poly(x,2)) +
        scale_x_continuous(paste(element.name, norma.tc)) +
        scale_y_continuous(paste(element.name, conen))
        
        calval.poly.plot.tc <- ggplot(data=val.frame.poly.tc, aes(Prediction, Concentration)) +
        theme_bw() +
        annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.poly.tc)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        stat_smooth(method="lm") +
        geom_point() +
        scale_x_continuous(paste(element.name, predi)) +
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
        
        calval.linear.plot.comp <- ggplot(data=val.frame.comp, aes(Prediction, Concentration)) +
        theme_bw() +
        annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.comp)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        stat_smooth(method="lm") +
        geom_point() +
        scale_x_continuous(paste(element.name, predi)) +
        scale_y_continuous(paste(element.name, conen))
        
        calcurve.poly.plot.comp <- ggplot(data=predict.frame.comp[ vals$keeprows, , drop = FALSE], aes(Intensity, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn_poly(lm(Concentration~poly(Intensity, 2), predict.frame.comp)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        geom_point() +
        geom_point(data = predict.frame.comp[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
        stat_smooth(method="lm", formula=y~poly(x,2)) +
        scale_x_continuous(paste(element.name, norma.comp)) +
        scale_y_continuous(paste(element.name, conen))
        
        calval.poly.plot.comp <- ggplot(data=val.frame.poly.comp, aes(Prediction, Concentration)) +
        theme_bw() +
        annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.poly.comp)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        stat_smooth(method="lm") +
        geom_point() +
        scale_x_continuous(paste(element.name, predi)) +
        scale_y_continuous(paste(element.name, conen))
    }
    
    
    if (input$radiocal==3){
        
        ####Lukas-Tooth  Calibration Curve Model Display, Time Normalized
        
        if(input$normcal==1){
            calcurve.linear.plot.luk <- ggplot(data=val.frame.luk[ vals$keeprows, , drop = FALSE], aes(IntensityNorm, Concentration)) +
            theme_light() +
            annotate("text", label=lm_eqn(lm(Concentration~IntensityNorm, val.frame.luk[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_point() +
            geom_point(aes(IntensityNorm, Concentration), data = val.frame.luk[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
            geom_smooth(aes(x=IntensityNorm, y=Concentration, ymin = Lower, ymax = Upper)) +
            scale_x_continuous(paste(element.name, norma)) +
            scale_y_continuous(paste(element.name, conen))
            
            calval.linear.plot.luk <- ggplot(data=val.frame.luk, aes(Prediction, Concentration)) +
            theme_bw() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.luk)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            stat_smooth(method="lm") +
            geom_point() +
            scale_x_continuous(paste(element.name, predi)) +
            scale_y_continuous(paste(element.name, conen))
        }
        
        
        
        ####Lukas-Tooth Calibration Curve Model Display, Total Count Normalized
        if(input$normcal==2){
            
            
            calcurve.linear.plot.luk.tc <- ggplot(data=val.frame.luk.tc[ vals$keeprows, , drop = FALSE], aes(IntensityNorm, Concentration)) +
            theme_light() +
            annotate("text", label=lm_eqn(lm(Concentration~Intensity, predict.frame.luk.tc[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_point() +
            geom_point(aes(IntensityNorm, Concentration), data = val.frame.luk.tc[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
            geom_smooth(aes(x=IntensityNorm, y=Concentration, ymin = Lower, ymax = Upper)) +
            scale_x_continuous(paste(element.name, norma.tc)) +
            scale_y_continuous(paste(element.name, conen))
            
            calval.linear.plot.luk.tc <- ggplot(data=val.frame.luk.tc, aes(Prediction, Concentration)) +
            theme_bw() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.luk.tc)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            stat_smooth(method="lm") +
            geom_point() +
            scale_x_continuous(paste(element.name, predi)) +
            scale_y_continuous(paste(element.name, conen))
        }
        
        
        
        ####Lukas-Tooth Calibration Curve Model Display, Compton Normalized
        
        if(input$normcal==3){
            calcurve.linear.plot.luk.comp <- ggplot(data=val.frame.luk.comp[ vals$keeprows, , drop = FALSE], aes(IntensityNorm, Concentration)) +
            theme_light() +
            annotate("text", label=lm_eqn(lm(Concentration~Intensity, predict.frame.luk.comp[ vals$keeprows, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            geom_point() +
            geom_point(aes(IntensityNorm, Concentration), data = val.frame.luk.comp[!vals$keeprows, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
            geom_smooth(aes(x=IntensityNorm, y=Concentration, ymin = Lower, ymax = Upper)) +
            scale_x_continuous(paste(element.name, norma.comp)) +
            scale_y_continuous(paste(element.name, conen))
            
            calval.linear.plot.luk.comp <- ggplot(data=val.frame.luk.comp, aes(Prediction, Concentration)) +
            theme_bw() +
            annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.luk.comp)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
            stat_smooth(method="lm") +
            geom_point() +
            scale_x_continuous(paste(element.name, predi)) +
            scale_y_continuous(paste(element.name, conen))
            
        }
        
    }
    
    
    if(input$radiocal==1 && input$normcal==1) {
        grid.arrange(calcurve.linear.plot,calval.linear.plot, ncol=2)
    } else if (input$radiocal==2 && input$normcal==1){
        grid.arrange(calcurve.poly.plot,calval.poly.plot, ncol=2)
    } else if (input$radiocal==1 && input$normcal==2){
        grid.arrange(calcurve.linear.plot.tc,calval.linear.plot.tc, ncol=2)
    } else if (input$radiocal==2 && input$normcal==2) {
        grid.arrange(calcurve.poly.plot.tc,calval.poly.plot.tc, ncol=2)
    } else if (input$radiocal==1 && input$normcal==3) {
        grid.arrange(calcurve.linear.plot.comp,calval.linear.plot.comp, ncol=2)
    } else if (input$radiocal==2 && input$normcal==3) {
        grid.arrange(calcurve.poly.plot.comp,calval.poly.plot.comp, ncol=2)
    } else if (input$radiocal==3 && input$normcal==1) {
        grid.arrange(calcurve.linear.plot.luk,calval.linear.plot.luk, ncol=2)
    } else if (input$radiocal==3 && input$normcal==2){
        grid.arrange(calcurve.linear.plot.luk.tc,calval.linear.plot.luk.tc, ncol=2)
    } else if (input$radiocal==3 && input$normcal==3) {
        grid.arrange(calcurve.linear.plot.luk.comp,calval.linear.plot.luk.comp, ncol=2)
    } else if (is.na(input$calcurveelement)) {
        return()
    }
    
})






####CalCurves
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

observeEvent(input$createcalplot, {
    

    vals$keeprows <- calFileStandards()
})



####ValCurves
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





 tableInput2 <- reactive({
     
     data <- dataHold()
     
     
     



     concentration.table <- as.data.frame(values[["DF"]], stringsAsFactors=FALSE)
     concentration.table[concentration.table==""] <- 999
     
     spectra.line.table <- if(input$filetype=="Spectra"){
         spectraData()
     }else if(input$filetype=="Net"){
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
     
     
     
     predict.intensity <- data.frame(predict.frame$Intensity)
     colnames(predict.intensity) <- c("Intensity")
     
     cal.lm <- lm(predict.frame$Concentration~predict.frame$Intensity)
     cal.lm.poly <- lm(predict.frame$Concentration~poly(predict.frame$Intensity, 2))
     
     cal.est.conc.pred <- predict(object=cal.lm, newdata=predict.intensity, interval='confidence')
     cal.est.conc.tab <- data.frame(cal.est.conc.pred)
     cal.est.conc <- cal.est.conc.tab$fit
     
     cal.est.poly.conc.pred <- predict(object=cal.lm.poly, newdata=predict.intensity, interval='confidence')
     cal.est.poly.conc.tab <- data.frame(cal.est.poly.conc.pred)
     cal.est.poly.conc <- cal.est.poly.conc.tab$fit
     
     val.difference <-concentration-cal.est.conc
     val.relative <- val.difference/concentration
     
     val.frame <- data.frame(as.vector(as.character(spectra.line.table$Spectrum)), concentration, cal.est.conc, val.difference, val.relative)
     colnames(val.frame) <- c("Standard", "Concentration", "Prediction", "Difference", "Relative")
     
     
     val.frame.poly <- data.frame(spectra.line.table$Spectrum, concentration, cal.est.poly.conc, (concentration-cal.est.poly.conc), ((concentration-cal.est.poly.conc)/concentration))
     colnames(val.frame.poly) <- c("Standard", "Concentration", "Prediction", "Difference", "Relative")
     
     
     
     
     
     if(input$filetype=="Spectra"){compton.norm <- subset(data$CPS, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))}
     if(input$filetype=="Spectra"){compton.file <- subset(data$Spectrum, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))}
     if(input$filetype=="Spectra"){compton.frame <- data.frame(is.0(compton.norm, compton.file))}
     if(input$filetype=="Spectra"){colnames(compton.frame) <- c("Compton", "Spectrum")}
     if(input$filetype=="Spectra"){compton.frame.ag.hold <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")}
     if(input$filetype=="Spectra"){colnames(compton.frame.ag.hold) <- c("Spectrum", "Compton")}
     
     compton.ag.fake.Spectrum <- hold.frame$Spectrum
     compton.ag.fake.Compton <- rep(1, length(hold.frame$Spectrum))
     compton.ag.fake <- data.frame(compton.ag.fake.Spectrum,compton.ag.fake.Compton)
     colnames(compton.ag.fake) <- c("Spectrum", "Compton")
     
     compton.frame.ag <- if(input$filetype=="Spectra"){
         compton.frame.ag.hold
     }else if(input$filetype=="Net"){
         compton.ag.fake
     }
     
     predict.frame.comp <- data.frame(predict.frame$Concentration, intensity/compton.frame.ag$Compton)
     colnames(predict.frame.comp) <- c("Concentration", "Intensity")
     
     
     
     predict.intensity.comp <- data.frame(predict.frame.comp$Intensity)
     colnames(predict.intensity.comp) <- c("Intensity")
     
     cal.lm.comp <- lm(predict.frame.comp$Concentration~predict.frame.comp$Intensity)
     cal.lm.poly.comp <- lm(predict.frame.comp$Concentration~poly(predict.frame.comp$Intensity, 2))
     
     cal.est.conc.pred.comp <- predict(object=cal.lm.comp, newdata=predict.intensity.comp, interval='confidence')
     cal.est.conc.tab.comp <- data.frame(cal.est.conc.pred.comp)
     cal.est.conc.comp <- cal.est.conc.tab.comp$fit
     
     cal.est.poly.conc.pred.comp <- predict(object=cal.lm.poly.comp, newdata=predict.intensity.comp, interval='confidence')
     cal.est.poly.conc.tab.comp <- data.frame(cal.est.poly.conc.pred.comp)
     cal.est.poly.conc.comp <- cal.est.poly.conc.tab.comp$fit
     
     val.frame.comp <- data.frame(spectra.line.table$Spectrum, concentration, cal.est.conc.comp, concentration-cal.est.conc.comp, (concentration-cal.est.conc.comp)/concentration)
     colnames(val.frame.comp) <- c("Standard", "Concentration", "Prediction", "Difference", "Relative")
     
     
     
     val.frame.poly.comp <- data.frame(spectra.line.table$Spectrum, concentration, cal.est.poly.conc.comp, concentration-cal.est.poly.conc.comp, (concentration-cal.est.poly.conc.comp)/concentration)
     colnames(val.frame.poly.comp) <- c("Standard", "Concentration", "Prediction", "Difference", "Relative")
     
     
     
     
     if(input$filetype=="Spectra"){total.counts <- aggregate(CPS~Spectrum, data=data, sum)}
     if(input$filetype=="Spectra"){colnames(total.counts) <- c("Spectrum", "CPS")}
     
     if(input$filetype=="Net"){total.counts.net <- rowSums(spectra.line.table[2:length(spectra.line.table)])}
     if(input$filetype=="Net"){total.counts <- data.frame(spectra.line.table$Spectrum, total.counts.net)}
     if(input$filetype=="Net"){colnames(total.counts) <- c("Spectrum", "CPS")}
     
     predict.frame.tc <- data.frame(predict.frame$Concentration, intensity/total.counts$CPS)
     colnames(predict.frame.tc) <- c("Concentration", "Intensity")
     
     
     
     predict.intensity.tc <- data.frame(predict.frame.tc$Intensity)
     colnames(predict.intensity.tc) <- c("Intensity")
     
     cal.lm.tc <- lm(predict.frame.tc$Concentration~predict.frame.tc$Intensity)
     cal.lm.poly.tc <- lm(predict.frame.tc$Concentration~poly(predict.frame.tc$Intensity, 2))
     
     cal.est.conc.pred.tc <- predict(object=cal.lm.tc, newdata=predict.intensity.tc, interval='confidence')
     cal.est.conc.tab.tc <- data.frame(cal.est.conc.pred.tc)
     cal.est.conc.tc <- cal.est.conc.tab.tc$fit
     
     cal.est.poly.conc.pred.tc <- predict(object=cal.lm.poly.tc, newdata=predict.intensity.tc, interval='confidence')
     cal.est.poly.conc.tab.tc <- data.frame(cal.est.poly.conc.pred.tc)
     cal.est.poly.conc.tc <- cal.est.poly.conc.tab.tc$fit
     
     val.frame.tc <- data.frame(spectra.line.table$Spectrum, concentration, cal.est.conc.tc, concentration-cal.est.conc.tc, (concentration-cal.est.conc.tc)/concentration)
     colnames(val.frame.tc) <- c("Standard", "Concentration", "Prediction", "Difference", "Relative")
     
     
     
     val.frame.poly.tc <- data.frame(spectra.line.table$Spectrum, concentration, cal.est.poly.conc.tc, concentration-cal.est.poly.conc.tc, (concentration-cal.est.poly.conc.tc)/concentration)
     colnames(val.frame.poly.tc) <- c("Standard", "Concentration", "Prediction", "Difference", "Relative")
     
     
     
     intercept.none <- rep(0, length(spectra.line.table$Spectrum))
     lukas.intercept.table.x <- data.frame(spectra.line.table[input$show_vars], intercept.none)
     colnames(lukas.intercept.table.x) <- c(names(spectra.line.table[input$show_vars]), "None")
     
     
     
     slope.none <- rep(1, length(spectra.line.table$Spectrum))
     lukas.slope.table <- data.frame(spectra.line.table[input$show_vars], slope.none)
     colnames(lukas.slope.table) <- c(names(spectra.line.table[input$show_vars]), "None")
     
     


     ####Fourth Iteration
     lukas.intercept.table <- data.frame(rowSums(lukas.intercept.table.x[input$intercept_vars]))
     colnames(lukas.intercept.table) <- c("first")
     lukas.intercept <- lukas.intercept.table$first
     lukas.slope <- data.frame(lukas.slope.table[input$slope_vars])
     
     
     predict.frame.luk <- data.frame(predict.frame$Concentration, (intensity*lukas.intercept),lukas.slope)
     colnames(predict.frame.luk) <- c("Concentration", "Intensity", names(lukas.slope))
     
     
     
     predict.intensity.luk <- data.frame(predict.frame.luk$Intensity, lukas.slope)
     colnames(predict.intensity.luk) <- c("Intensity", names(lukas.slope))
     
     lukas.lm <- lm(Concentration~., data=predict.frame.luk)
     
     cal.est.conc.pred.luk <- predict(object=lukas.lm , newdata=predict.intensity.luk, interval='confidence')
     cal.est.conc.tab.luk <- data.frame(cal.est.conc.pred.luk)
     cal.est.conc.luk <- cal.est.conc.tab.luk$fit
     cal.est.conc.luk.up <- cal.est.conc.tab.luk$upr
     cal.est.conc.luk.low <- cal.est.conc.tab.luk$lwr
     
     
     val.frame.luk <- data.frame(spectra.line.table$Spectrum, concentration, cal.est.conc.luk, concentration-cal.est.conc.luk, (concentration-cal.est.conc.luk)/concentration)
     colnames(val.frame.luk) <- c("Standard", "Concentration", "Prediction", "Difference", "Relative")
     
     
     
    
     
     
     lukas.intercept.table.comp <- data.frame(rowSums(lukas.intercept.table.x[input$intercept_vars]))/compton.frame.ag$Compton
     colnames(lukas.intercept.table.comp) <- c("first")
     lukas.intercept.comp <- lukas.intercept.table.comp$first
     lukas.slope.comp <- data.frame(lukas.slope.table[input$slope_vars])/compton.frame.ag$Compton
     
     
     predict.frame.luk.comp <- data.frame(predict.frame$Concentration, (intensity/compton.frame.ag$Compton*lukas.intercept.comp),lukas.slope.comp)
     colnames(predict.frame.luk.comp) <- c("Concentration", "Intensity", names(lukas.slope.comp))
     
     
     
     predict.intensity.luk.comp <- data.frame(predict.frame.luk.comp$Intensity, lukas.slope.comp)
     colnames(predict.intensity.luk.comp) <- c("Intensity", names(lukas.slope.comp))
     
     lukas.lm.comp <- lm(Concentration~., data=predict.frame.luk.comp)
     
     cal.est.conc.pred.luk.comp <- predict(object=lukas.lm.comp , newdata=predict.intensity.luk.comp, interval='confidence')
     cal.est.conc.tab.luk.comp <- data.frame(cal.est.conc.pred.luk.comp)
     cal.est.conc.luk.comp <- cal.est.conc.tab.luk.comp$fit
     cal.est.conc.luk.up.comp <- cal.est.conc.tab.luk.comp$upr
     cal.est.conc.luk.low.comp <- cal.est.conc.tab.luk.comp$lwr
     
     
     val.frame.luk.comp <- data.frame(spectra.line.table$Spectrum, concentration, cal.est.conc.luk.comp, concentration-cal.est.conc.luk.comp, (concentration-cal.est.conc.luk.comp)/concentration)
     colnames(val.frame.luk.comp) <- c("Standard", "Concentration", "Prediction", "Difference", "Relative")
     
     
     
     
     lukas.intercept.table.tc <- data.frame(rowSums(lukas.intercept.table.x[input$intercept_vars]))/total.counts$CPS
     colnames(lukas.intercept.table.tc) <- c("first")
     lukas.intercept.tc <- lukas.intercept.table.tc$first
     lukas.slope.tc <- data.frame(lukas.slope.table[input$slope_vars])/total.counts$CPS
     
     
     predict.frame.luk.tc <- data.frame(predict.frame$Concentration, (intensity/total.counts$CPS*lukas.intercept.tc),lukas.slope.tc)
     colnames(predict.frame.luk.tc) <- c("Concentration", "Intensity", names(lukas.slope.tc))
     
     
     
     predict.intensity.luk.tc <- data.frame(predict.frame.luk.tc$Intensity, lukas.slope.tc)
     colnames(predict.intensity.luk.tc) <- c("Intensity", names(lukas.slope.tc))
     
     lukas.lm.tc <- lm(Concentration~., data=predict.frame.luk.tc)
     
     cal.est.conc.pred.luk.tc <- predict(object=lukas.lm.tc , newdata=predict.intensity.luk.tc, interval='confidence')
     cal.est.conc.tab.luk.tc <- data.frame(cal.est.conc.pred.luk.tc)
     cal.est.conc.luk.tc <- cal.est.conc.tab.luk.tc$fit
     cal.est.conc.luk.up.tc <- cal.est.conc.tab.luk.tc$upr
     cal.est.conc.luk.low.tc <- cal.est.conc.tab.luk.tc$lwr
     
     
     val.frame.luk.tc <- data.frame(spectra.line.table$Spectrum, concentration, cal.est.conc.luk.tc, concentration-cal.est.conc.luk.tc, (concentration-cal.est.conc.luk.tc)/concentration)
     colnames(val.frame.luk.tc) <- c("Standard", "Concentration", "Prediction", "Difference", "Relative")
     
     
     
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
         val.frame.luk
     } else if (input$radiocal==3 && input$normcal==2){
         val.frame.luk.tc
     } else if (input$radiocal==3 && input$normcal==3) {
         val.frame.luk.comp
     }
     
     this.table <- DT::datatable(standard.table)
     this.table

 })
 
 

 
 output$standardsperformance <- DT::renderDataTable({
    

     tableInput2()
    
 
 })
 

 nullElementModel <- reactive({
     
     
 })


 
 elementModel <- reactive({
     
     data <- dataHold()
     
     
     


     concentration.table <- as.data.frame(values[["DF"]], stringsAsFactors=FALSE)
     concentration.table[concentration.table==""] <- 999
     
     spectra.line.table <- if(input$filetype=="Spectra"){
         spectraData()
     }else if(input$filetype=="Net"){
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
     
     
     
     predict.intensity <- data.frame(predict.frame$Intensity)
     colnames(predict.intensity) <- c("Intensity")
     
     cal.lm <- lm(Concentration~Intensity, data=predict.frame)
     cal.lm.poly <- lm(Concentration~poly(Intensity, 2), data=predict.frame)
     
     
     
     
     if(input$filetype=="Spectra"){compton.norm <- subset(data$CPS, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))}
     if(input$filetype=="Spectra"){compton.file <- subset(data$Spectrum, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))}
     if(input$filetype=="Spectra"){compton.frame <- data.frame(is.0(compton.norm, compton.file))}
     if(input$filetype=="Spectra"){colnames(compton.frame) <- c("Compton", "Spectrum")}
     if(input$filetype=="Spectra"){compton.frame.ag.hold <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")}
     if(input$filetype=="Spectra"){colnames(compton.frame.ag.hold) <- c("Spectrum", "Compton")}
     
     compton.ag.fake.Spectrum <- hold.frame$Spectrum
     compton.ag.fake.Compton <- rep(1, length(hold.frame$Spectrum))
     compton.ag.fake <- data.frame(compton.ag.fake.Spectrum,compton.ag.fake.Compton)
     colnames(compton.ag.fake) <- c("Spectrum", "Compton")
     
     compton.frame.ag <- if(input$filetype=="Spectra"){
         compton.frame.ag.hold
     }else if(input$filetype=="Net"){
         compton.ag.fake
     }
     
     predict.frame.comp <- data.frame(predict.frame$Concentration, intensity/compton.frame.ag$Compton)
     colnames(predict.frame.comp) <- c("Concentration", "Intensity")
     
     
     
     predict.intensity.comp <- data.frame(predict.frame.comp$Intensity)
     colnames(predict.intensity.comp) <- c("Intensity")
     
     cal.lm.comp <- lm(Concentration~Intensity, data=predict.frame.comp)
     cal.lm.poly.comp <- lm(Concentration~poly(Intensity, 2), data=predict.frame.comp)
     
     
     if(input$filetype=="Spectra"){total.counts <- aggregate(CPS~Spectrum, data=data, sum)}
     if(input$filetype=="Spectra"){colnames(total.counts) <- c("Spectrum", "CPS")}
     
     if(input$filetype=="Net"){total.counts.net <- rowSums(spectra.line.table[2:length(spectra.line.table)])}
     if(input$filetype=="Net"){total.counts <- data.frame(spectra.line.table$Spectrum, total.counts.net)}
     if(input$filetype=="Net"){colnames(total.counts) <- c("Spectrum", "CPS")}
     
     predict.frame.tc <- data.frame(predict.frame$Concentration, intensity/total.counts$CPS)
     colnames(predict.frame.tc) <- c("Concentration", "Intensity")
     
     
     
     predict.intensity.tc <- data.frame(predict.frame.tc$Intensity)
     colnames(predict.intensity.tc) <- c("Intensity")
     
     cal.lm.tc <- lm(Concentration~Intensity, data=predict.frame.tc)
     cal.lm.poly.tc <- lm(Concentration~poly(Intensity, 2), data=predict.frame.tc)
     
     
     intercept.none <- rep(0, length(spectra.line.table$Spectrum))
     lukas.intercept.table.x <- data.frame(spectra.line.table[input$show_vars], intercept.none)
     colnames(lukas.intercept.table.x) <- c(names(spectra.line.table[input$show_vars]), "None")
     
     
     
     slope.none <- rep(1, length(spectra.line.table$Spectrum))
     lukas.slope.table <- data.frame(spectra.line.table[input$show_vars], slope.none)
     colnames(lukas.slope.table) <- c(names(spectra.line.table[input$show_vars]), "None")
     
     
    
     
     ####Fourth Iteration
     lukas.intercept.table <- data.frame(rowSums(lukas.intercept.table.x[input$intercept_vars]))
     colnames(lukas.intercept.table) <- c("first")
     
     
     
     lukas.intercept <- lukas.intercept.table$first
     lukas.slope <- data.frame(lukas.slope.table[input$slope_vars])
     
     
     predict.frame.luk <- data.frame(predict.frame$Concentration, (intensity*lukas.intercept),lukas.slope)
     colnames(predict.frame.luk) <- c("Concentration", "Intensity", names(lukas.slope))
     
     
     
     predict.intensity.luk <- data.frame(predict.frame.luk$Intensity, lukas.slope)
     colnames(predict.intensity.luk) <- c("Intensity", names(lukas.slope))
     
     lukas.lm <- lm(Concentration~., data=predict.frame.luk)
     
     
     lukas.intercept.table.comp <- data.frame(rowSums(lukas.intercept.table.x[input$intercept_vars]))/compton.frame.ag$Compton
     colnames(lukas.intercept.table.comp) <- c("first")
     
     
     lukas.intercept.comp <- lukas.intercept.table.comp$first
     lukas.slope.comp <- data.frame(lukas.slope.table[input$slope_vars])/compton.frame.ag$Compton
     
     
     predict.frame.luk.comp <- data.frame(predict.frame$Concentration, (intensity/compton.frame.ag$Compton*lukas.intercept.comp),lukas.slope.comp)
     colnames(predict.frame.luk.comp) <- c("Concentration", "Intensity", names(lukas.slope.comp))
     
     
     
     predict.intensity.luk.comp <- data.frame(predict.frame.luk.comp$Intensity, lukas.slope.comp)
     colnames(predict.intensity.luk.comp) <- c("Intensity", names(lukas.slope.comp))
     
     lukas.lm.comp <- lm(Concentration~., data=predict.frame.luk.comp)
     
        lukas.intercept.table.tc <- data.frame(rowSums(lukas.intercept.table.x[input$intercept_vars]))/total.counts$CPS
     colnames(lukas.intercept.table.tc) <- c("first")
     
     
     lukas.intercept.tc <- lukas.intercept.table.tc$first
     lukas.slope.tc <- data.frame(lukas.slope.table[input$slope_vars])/total.counts$CPS
     
     
     predict.frame.luk.tc <- data.frame(predict.frame$Concentration, (intensity/total.counts$CPS*lukas.intercept.tc),lukas.slope.tc)
     colnames(predict.frame.luk.tc) <- c("Concentration", "Intensity", names(lukas.slope.tc))
     
     
     
     predict.intensity.luk.tc <- data.frame(predict.frame.luk.tc$Intensity, lukas.slope.tc)
     colnames(predict.intensity.luk.tc) <- c("Intensity", names(lukas.slope.tc))
     
     lukas.lm.tc <- lm(Concentration~., data=predict.frame.luk.tc)
     
     
     input$createcalelement



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
         lukas.lm
     } else if (input$radiocal==3 && input$normcal==2) {
         lukas.lm.tc
     } else if (input$radiocal==3 && input$normcal==3) {
         lukas.lm.comp
     }
     

     
strip_glm(model)

 })
 
 
 modelFrame <- reactive({
     
     
     model <- elementModel()
     
     model.frame <- as.data.frame(augment(model))
     
     model.frame$qq <- qqnorm(model.frame$.std.resid)[[1]]
     
     model.frame$sqrt.std.resid <- sqrt(abs(model.frame$.std.resid))
     
     model.frame$seq.cooksd <- seq_along(model.frame$.cooksd)


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
 
 #########Diagnostic Plot Controls#######
 ####Residuals Fitted
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

    cal.vector <- input$show_vars
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

observeEvent(input$hotableprocess2, {
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
    
    
    calList[[input$calcurveelement]] <- list(isolate(calConditons), isolate(elementModel()))

    calList <<- calList

})

Calibration <- reactiveValues()
observeEvent(input$createcal, {
    
    
    spectra.line.table <- if(input$filetype=="Spectra"){
        spectraData()
    }else if(input$filetype=="Net"){
        dataHold()
    }
             cal.intensities <- spectra.line.table[input$show_vars]
             cal.values <- values[["DF"]]
             cal.data <- dataHold()

             calibrationList <- NULL
             calibrationList <- list(input$filetype, input$calunits, cal.data, cal.intensities, cal.values, calList)
             names(calibrationList) <- c("FileType", "Units", "Spectra", "Intensities", "Values", "calList")
             
    Calibration <<- calibrationList

    
})


output$downloadModel <- downloadHandler(
filename <- function(){
    paste(input$calname, "quant", sep=".")
},

content = function(file) {
    saveRDS(Calibration, file = file)
}
)



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
        
        
        myValData <- reactive({
            
            data <- if(input$valfiletype=="Spectra"){
                fullValSpectra()
            } else if(input$valfiletype=="Net"){
                netValCounts()
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
        
        
  
  
        
        
        tableInputValCounts <- reactive({
            valelements <- calValElements()
            variableelements <- calVariableElements()
            val.data <- myValData()
            
            if(input$valfiletype=="Spectra"){spectra.line.list <- lapply(valelements, function(x) elementGrab(element.line=x, data=val.data))}
            if(input$valfiletype=="Spectra"){element.count.list <- lapply(spectra.line.list, '[', 2)}
            
            
            
            if(input$valfiletype=="Spectra"){spectra.line.vector <- as.numeric(unlist(element.count.list))}
            
            if(input$valfiletype=="Spectra"){dim(spectra.line.vector) <- c(length(spectra.line.list[[1]]$Spectrum), length(valelements))}
            
            if(input$valfiletype=="Spectra"){spectra.line.frame <- data.frame(spectra.line.list[[1]]$Spectrum, spectra.line.vector)}
            
            if(input$valfiletype=="Spectra"){colnames(spectra.line.frame) <- c("Spectrum", valelements)}
            
            if(input$valfiletype=="Spectra"){spectra.line.frame <- as.data.frame(spectra.line.frame)}
            
            if(input$valfiletype=="Spectra"){spectra.line.frame}
            
            if(input$valfiletype=="Spectra"){val.line.table <- data.table(spectra.line.frame[, c("Spectrum", valelements), drop = FALSE])}
            
            
            if(input$valfiletype=="Net"){val.line.table <- val.data[c("Spectrum", valelements), drop=FALSE]}
                
                
                val.line.table


        })
        
        
        
        fullInputValCounts <- reactive({
            valelements <- calValElements()
            variableelements <- calVariableElements()
            val.data <- myValData()
            
            if(input$valfiletype=="Spectra"){spectra.line.list <- lapply(variableelements, function(x) elementGrab(element.line=x, data=val.data))}
            if(input$valfiletype=="Spectra"){element.count.list <- lapply(spectra.line.list, `[`, 2)}
            
            
            if(input$valfiletype=="Spectra"){spectra.line.vector <- as.numeric(unlist(element.count.list))}
            
            if(input$valfiletype=="Spectra"){dim(spectra.line.vector) <- c(length(spectra.line.list[[1]]$Spectrum), length(variableelements))}
            
            if(input$valfiletype=="Spectra"){spectra.line.frame <- data.frame(spectra.line.list[[1]]$Spectrum, spectra.line.vector)}
            
            if(input$valfiletype=="Spectra"){colnames(spectra.line.frame) <- c("Spectrum", variableelements)}
            
            if(input$valfiletype=="Spectra"){spectra.line.frame <- as.data.frame(spectra.line.frame)}
            
            if(input$valfiletype=="Spectra"){val.line.table <- spectra.line.frame[c("Spectrum", variableelements)]}
            
            if(input$valfiletype=="Net"){val.line.table <- val.data}
            
            val.line.table
        })

        
        
        output$myvaltable1 <- renderDataTable({
            
            fullInputValCounts()
            
        })
        
        
        
        
        tableInputValQuant <- reactive({
            
        count.table <- data.frame(fullInputValCounts())
        the.cal <- calValHold()
        elements <- calValElements()
        variables <- calVariableElements()
        valdata <- myValData()


            
            
            
        predicted.list <- pblapply(elements, function (x)
            if(input$valfiletype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                    object=the.cal[[x]][[2]],
                    newdata=general.prep(
                        spectra.line.table=as.data.frame(
                            count.table
                            ),
                        element.line=x)
                )
            } else if(input$valfiletype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==2) {
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
            } else if(input$valfiletype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==3) {
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
            } else if(input$valfiletype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                 predict(
                    object=the.cal[[x]][[2]],
                    newdata=lukas.simp.prep(
                        spectra.line.table=as.data.frame(
                            count.table
                            ),
                        element.line=x,
                        slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                        )
                 )
            } else if(input$valfiletype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==2){
                predict(
                    object=the.cal[[x]][[2]],
                    newdata=lukas.tc.prep(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                            count.table
                            ),
                        element.line=x,
                        slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                    )
                )
            } else if(input$valfiletype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==3){
                predict(
                    object=the.cal[[x]][[2]],
                    newdata=lukas.comp.prep(
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
            }else if(input$valfiletype=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                    object=the.cal[[x]][[2]],
                    newdata=general.prep.net(
                        spectra.line.table=as.data.frame(
                            count.table
                            ),
                        element.line=x)
                )
            } else if(input$valfiletype=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==2) {
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
            } else if(input$valfiletype=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==3) {
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
            } else if(input$valfiletype=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
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
            } else if(input$valfiletype=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==2){
                predict(
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
            } else if(input$valfiletype=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==3){
                predict(
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





