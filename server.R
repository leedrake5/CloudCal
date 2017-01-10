library(shiny)
library(shinysky)
library(ggplot2)
library(pbapply)
library(reshape2)
library(TTR)
library(dplyr)
library(shinyIncubator)
library(data.table)
library(ggtern)
library(gridExtra)
library(data.table)
library(DT)








shinyServer(function(input, output, session) {
    
    
    
 
    
    observeEvent(input$actionprocess, {
        

        myData <- reactive({
            
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


        output$contents <- renderTable({
            
            

           myData()
          
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
             


             data <- myData()
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
            ggsave(file,plotInput(), width=10, height=7)
        }
        )
        
        
        

        
      
   
    
         })
         
         
 data <- myData()
 
 spectra.line.table <- spectra.line.fn(data)
 select.line.table <- datatable(spectra.line.table[, input$show_vars, drop = FALSE])
 select.line.table
 concentration.names <- input$show_vars

  
 fordownload <- spectra.line.table[input$show_vars]
 
 tableInput <- reactive({
     spectra.line.table <- spectra.line.fn(data)
     select.line.table <- datatable(spectra.line.table[, input$show_vars, drop = FALSE])
     select.line.table
 })


  output$mytable1 <- renderDataTable({
   
  tableInput()

  })
  

  
 spectra.line.table[input$show_vars]
  
  output$downloadData <- downloadHandler(
  filename = function() { paste(input$dataset, '.csv', sep=',') },
  content = function(file
  ) {
      write.csv(fordownload, file)
  }
  )
  
  observeEvent(input$hotableprocess1, {
  })

hotableInput <- reactive({
  empty.line.table <-  spectra.line.table[input$show_vars] * 0
  empty.line.table$Spectrum <- spectra.line.table$Spectrum
  
  empty.line.table
  
})

output$hotable1 <- renderHotable(exp={
    input$hotableprocess1
    isolate(print(hotableInput()))}, readOnly=F)


observeEvent(input$hotableprocess2, {
})


      renderHotable <- reactive({

         hot.to.df(input$hotable1) # this will convert your input into a data.frame
          
          
          
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
intensities <- spectra.line.table[input$show_vars]
n.inten <- length(intensities)
myelements <- names(intensities[2:n.inten])

if(is.null(myelements)){
    return("Ca.K.alpha")
}else{
    myelements
}


})

outVaralt <- reactive({
    input$hotableprocess2
    
    
    none <- rep(0, length(spectra.line.table$Spectrum))
    hold.table <- data.frame(spectra.line.table[input$show_vars], none)
    colnames(hold.table) <- c(names(spectra.line.table[input$show_vars]), "None")
    
    
    n.inten <- length(hold.table)
    myelements <- names(hold.table[2:n.inten])
    
    if(is.null(myelements)){
        paste(n.inten)
    }else{
        myelements
    }
    
})

output$inVar2 <- renderUI({
    selectInput(inputId = "calcurveelement", label = h4("Element"), choices =  outVar())
})


output$inVar3 <- renderUI({
    checkboxGroupInput(inputId = "intercept_vars", label = h4("Intercept"), choices =  outVaralt(), selected=input$calcurveelement)
})

output$inVar4 <- renderUI({
    checkboxGroupInput(inputId = "slope_vars", label = h4("Slope"), choices =  outVaralt(), selected=input$calcurveelement)
})



concentration.table <- renderHotable()
concentration.hold <- concentration.table







  plotInput2 <- reactive({
      
      
      concentration.table <- renderHotable()
      concentration.hold <- concentration.table






          concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[input$calcurveelement]))))
          
          
          intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[input$calcurveelement]))))
          
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
      
        cal.lm <- lm(predict.frame$Concentration~predict.frame$Intensity)
        cal.lm.poly <- lm(predict.frame$Concentration~poly(predict.frame$Intensity, 2))
        
        cal.est.conc.pred <- predict(object=cal.lm, newdata=predict.intensity, interval='confidence')
        cal.est.conc.tab <- data.frame(cal.est.conc.pred)
        cal.est.conc <- cal.est.conc.tab$fit
        
        cal.est.poly.conc.pred <- predict(object=cal.lm.poly, newdata=predict.intensity, interval='confidence')
        cal.est.poly.conc.tab <- data.frame(cal.est.poly.conc.pred)
        cal.est.poly.conc <- cal.est.poly.conc.tab$fit
        
        val.frame <- data.frame(concentration, cal.est.conc)
        colnames(val.frame) <- c("Concentration", "Prediction")
        
        val.frame.poly <- data.frame(concentration, cal.est.poly.conc)
        colnames(val.frame.poly) <- c("Concentration", "Prediction")
        
    
        
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
        cal.lm.poly.comp <- lm(predict.frame.comp$Concentration~poly(predict.frame.comp$Intensity, 2))
        
        cal.est.conc.pred.comp <- predict(object=cal.lm.comp, newdata=predict.intensity.comp, interval='confidence')
        cal.est.conc.tab.comp <- data.frame(cal.est.conc.pred.comp)
        cal.est.conc.comp <- cal.est.conc.tab.comp$fit
        
        cal.est.poly.conc.pred.comp <- predict(object=cal.lm.poly.comp, newdata=predict.intensity.comp, interval='confidence')
        cal.est.poly.conc.tab.comp <- data.frame(cal.est.poly.conc.pred.comp)
        cal.est.poly.conc.comp <- cal.est.poly.conc.tab.comp$fit
        
        val.frame.comp <- data.frame(concentration, cal.est.conc.comp)
        colnames(val.frame.comp) <- c("Concentration", "Prediction")
        
        val.frame.poly.comp <- data.frame(concentration, cal.est.poly.conc.comp)
        colnames(val.frame.poly.comp) <- c("Concentration", "Prediction")


    
        total.counts <- aggregate(CPS~Spectrum, data=data, sum)
        colnames(total.counts) <- c("Spectrum", "CPS")
        
        predict.frame.tc <- data.frame(concentration, intensity/total.counts$CPS)
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
        
        val.frame.tc <- data.frame(concentration, cal.est.conc.tc)
        colnames(val.frame.tc) <- c("Concentration", "Prediction")
        
        val.frame.poly.tc <- data.frame(concentration, cal.est.poly.conc.tc)
        colnames(val.frame.poly.tc) <- c("Concentration", "Prediction")
        
        intercept.none <- rep(0, length(spectra.line.table$Spectrum))
        lukas.intercept.table <- data.frame(spectra.line.table[input$show_vars], intercept.none)
        colnames(lukas.intercept.table) <- c(names(spectra.line.table[input$show_vars]), "None")
        
        slope.none <- rep(1, length(spectra.line.table$Spectrum))
        lukas.slope.table <- data.frame(spectra.line.table[input$show_vars], slope.none)
        colnames(lukas.slope.table) <- c(names(spectra.line.table[input$show_vars]), "None")
        
        
     
      
      ####Fourth Iteration
      lukas.intercept <- data.frame(rowSums(lukas.intercept.table[input$intercept_vars]))
      lukas.slope <- data.frame(lukas.slope.table[input$slope_vars])
      
      
      predict.frame.luk <- data.frame(concentration, (intensity*lukas.intercept),lukas.slope)
      colnames(predict.frame.luk) <- c("Concentration", "Intensity", names(lukas.slope))
      predict.intensity.luk <- data.frame(predict.frame.luk$Intensity, lukas.slope)
      colnames(predict.intensity.luk) <- c("Intensity", names(lukas.slope))
      
      lukas.lm <- lm(Concentration~., data=predict.frame.luk)
      
      cal.est.conc.pred.luk <- predict(object=lukas.lm , newdata=predict.intensity.luk, interval='confidence')
      cal.est.conc.tab.luk <- data.frame(cal.est.conc.pred.luk)
      cal.est.conc.luk <- cal.est.conc.tab.luk$fit
      cal.est.conc.luk.up <- cal.est.conc.tab.luk$upr
      cal.est.conc.luk.low <- cal.est.conc.tab.luk$lwr


      val.frame.luk <- data.frame(concentration, intensity, intensity/sum(intensity), cal.est.conc.luk, cal.est.conc.luk.up, cal.est.conc.luk.low)
      colnames(val.frame.luk) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
      

        
        lukas.intercept.comp <- data.frame(rowSums(lukas.intercept.table[input$intercept_vars]))/compton.frame.ag$Compton
        lukas.slope.comp <- data.frame(rowSums(lukas.slope.table[input$slope_vars]))/compton.frame.ag$Compton
        
        
        lukas.intensity.comp <- (predict.frame.comp$Intensity+lukas.intercept.comp)*lukas.slope.comp


        predict.frame.luk.comp <- data.frame(concentration, lukas.intensity.comp)
        colnames(predict.frame.luk.comp) <- c("Concentration", "Intensity")
        predict.intensity.luk.comp <- data.frame(predict.frame.luk.comp$Intensity)
        colnames(predict.intensity.luk.comp) <- c("Intensity")
        
        cal.lm.luk.comp <- lm(predict.frame.luk.comp$Concentration~predict.frame.luk.comp$Intensity)
        
        cal.est.conc.pred.luk.comp <- predict(object=cal.lm.luk.comp, newdata=predict.intensity.luk.comp, interval='confidence')
        cal.est.conc.tab.luk.comp <- data.frame(cal.est.conc.pred.luk.comp)
        cal.est.conc.luk.comp <- cal.est.conc.tab.luk.comp$fit
        
        val.frame.luk.comp <- data.frame(concentration, cal.est.conc.luk.comp)
        colnames(val.frame.luk.comp) <- c("Concentration", "Prediction")
        
        
        
        
        lukas.intercept.comp <- data.frame(rowSums(lukas.intercept.table[input$intercept_vars]))/compton.frame.ag$Compton
        lukas.slope.comp <- data.frame(lukas.slope.table[input$slope_vars])/compton.frame.ag$Compton
        
        
        predict.frame.luk.comp <- data.frame(concentration, (intensity/compton.frame.ag$Compton*lukas.intercept.comp),lukas.slope.comp)
        colnames(predict.frame.luk.comp) <- c("Concentration", "Intensity", names(lukas.slope.comp))
        predict.intensity.luk.comp <- data.frame(predict.frame.luk.comp$Intensity, lukas.slope.comp)
        colnames(predict.intensity.luk.comp) <- c("Intensity", names(lukas.slope.comp))
        
        lukas.lm.comp <- lm(Concentration~., data=predict.frame.luk.comp)
        
        cal.est.conc.pred.luk.comp <- predict(object=lukas.lm.comp , newdata=predict.intensity.luk.comp, interval='confidence')
        cal.est.conc.tab.luk.comp <- data.frame(cal.est.conc.pred.luk.comp)
        cal.est.conc.luk.comp <- cal.est.conc.tab.luk.comp$fit
        cal.est.conc.luk.up.comp <- cal.est.conc.tab.luk.comp$upr
        cal.est.conc.luk.low.comp <- cal.est.conc.tab.luk.comp$lwr
        
        
        val.frame.luk.comp <- data.frame(concentration, intensity/compton.frame.ag$Compton, (intensity/compton.frame.ag$Compton)/sum(intensity/compton.frame.ag$Compton), cal.est.conc.luk.comp, cal.est.conc.luk.up.comp, cal.est.conc.luk.low.comp)
        colnames(val.frame.luk.comp) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")
        
        
        
        lukas.intercept.tc <- data.frame(rowSums(lukas.intercept.table[input$intercept_vars]))/total.counts$CPS
        lukas.slope.tc <- data.frame(lukas.slope.table[input$slope_vars])/total.counts$CPS
        
        
        predict.frame.luk.tc <- data.frame(concentration, (intensity/total.counts$CPS*lukas.intercept.tc),lukas.slope.tc)
        colnames(predict.frame.luk.tc) <- c("Concentration", "Intensity", names(lukas.slope.tc))
        predict.intensity.luk.tc <- data.frame(predict.frame.luk.tc$Intensity, lukas.slope.tc)
        colnames(predict.intensity.luk.tc) <- c("Intensity", names(lukas.slope.tc))
        
        lukas.lm.tc <- lm(Concentration~., data=predict.frame.luk.tc)
        
        cal.est.conc.pred.luk.tc <- predict(object=lukas.lm.tc , newdata=predict.intensity.luk.tc, interval='confidence')
        cal.est.conc.tab.luk.tc <- data.frame(cal.est.conc.pred.luk.tc)
        cal.est.conc.luk.tc <- cal.est.conc.tab.luk.tc$fit
        cal.est.conc.luk.up.tc <- cal.est.conc.tab.luk.tc$upr
        cal.est.conc.luk.low.tc <- cal.est.conc.tab.luk.tc$lwr
        
        
        val.frame.luk.tc <- data.frame(concentration, intensity/total.counts$CPS, (intensity/total.counts$CPS)/sum(intensity/total.counts$CPS), cal.est.conc.luk.tc, cal.est.conc.luk.up.tc, cal.est.conc.luk.low.tc)
        colnames(val.frame.luk.tc) <- c("Concentration", "Intensity", "IntensityNorm", "Prediction", "Upper", "Lower")


        
        
        


      ####Linear Calibration Curve Model Display, Time Normalized
      calcurve.linear.plot <- ggplot(data=predict.frame, aes(Intensity, Concentration)) +
      theme_light() +
      annotate("text", label=lm_eqn(lm(Concentration~Intensity, predict.frame)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
      geom_point() +
      stat_smooth(method="lm") +
      scale_x_continuous(paste(element.name, intens)) +
      scale_y_continuous(paste(element.name, conen))
      
       calval.linear.plot <- ggplot(data=val.frame, aes(Prediction, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        stat_smooth(method="lm") +
        geom_point() +
        scale_x_continuous(paste(element.name, predi)) +
        scale_y_continuous(paste(element.name, conen))
        
        calcurve.poly.plot <- ggplot(data=predict.frame, aes(Intensity, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn_poly(lm(Concentration~poly(Intensity, 2), predict.frame)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        geom_point() +
        stat_smooth(method="lm", formula=y~poly(x,2)) +
        scale_x_continuous(paste(element.name, intens)) +
        scale_y_continuous(paste(element.name, conen))
        
        calval.poly.plot <- ggplot(data=val.frame.poly, aes(Prediction, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.poly)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        stat_smooth(method="lm") +
        geom_point() +
        scale_x_continuous(paste(element.name, predi)) +
        scale_y_continuous(paste(element.name, conen))
        
        
        ####Linear Calibration Curve Model Display, Total Count Normalized
        calcurve.linear.plot.tc <- ggplot(data=predict.frame.tc, aes(Intensity, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn(lm(Concentration~Intensity, predict.frame.tc)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        geom_point() +
        stat_smooth(method="lm") +
        scale_x_continuous(paste(element.name, norma.tc)) +
        scale_y_continuous(paste(element.name, conen))
        
        calval.linear.plot.tc <- ggplot(data=val.frame.tc, aes(Prediction, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.tc)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        stat_smooth(method="lm") +
        geom_point() +
        scale_x_continuous(paste(element.name, predi)) +
        scale_y_continuous(paste(element.name, conen))
        
        calcurve.poly.plot.tc <- ggplot(data=predict.frame.tc, aes(Intensity, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn_poly(lm(Concentration~poly(Intensity, 2), predict.frame.tc)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        geom_point() +
        stat_smooth(method="lm", formula=y~poly(x,2)) +
        scale_x_continuous(paste(element.name, norma.tc)) +
        scale_y_continuous(paste(element.name, conen))
        
        calval.poly.plot.tc <- ggplot(data=val.frame.poly.tc, aes(Prediction, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.poly.tc)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        stat_smooth(method="lm") +
        geom_point() +
        scale_x_continuous(paste(element.name, predi)) +
        scale_y_continuous(paste(element.name, conen))
        
        ####Linear Calibration Curve Model Display, Compton Normalized
        calcurve.linear.plot.comp <- ggplot(data=predict.frame.comp, aes(Intensity, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn(lm(Concentration~Intensity, predict.frame.comp)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        geom_point() +
        stat_smooth(method="lm") +
        scale_x_continuous(paste(element.name, norma.comp)) +
        scale_y_continuous(paste(element.name, conen))
        
        calval.linear.plot.comp <- ggplot(data=val.frame.comp, aes(Prediction, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.comp)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        stat_smooth(method="lm") +
        geom_point() +
        scale_x_continuous(paste(element.name, predi)) +
        scale_y_continuous(paste(element.name, conen))
        
        calcurve.poly.plot.comp <- ggplot(data=predict.frame.comp, aes(Intensity, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn_poly(lm(Concentration~poly(Intensity, 2), predict.frame.comp)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        geom_point() +
        stat_smooth(method="lm", formula=y~poly(x,2)) +
        scale_x_continuous(paste(element.name, norma.comp)) +
        scale_y_continuous(paste(element.name, conen))
        
        calval.poly.plot.comp <- ggplot(data=val.frame.poly.comp, aes(Prediction, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.poly.comp)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        stat_smooth(method="lm") +
        geom_point() +
        scale_x_continuous(paste(element.name, predi)) +
        scale_y_continuous(paste(element.name, conen))
        
        ####Lukas-Tooth  Calibration Curve Model Display, Time Normalized
        calcurve.linear.plot.luk <- ggplot(data=val.frame.luk, aes(IntensityNorm, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn(lm(Concentration~Intensity, predict.frame.luk)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        geom_point() +
        geom_smooth(aes(x=(Intensity/sum(Intensity)), y=Concentration, ymin = Lower, ymax = Upper)) +
        scale_x_continuous(paste(element.name, norma)) +
        scale_y_continuous(paste(element.name, conen))
        
        
        calval.linear.plot.luk <- ggplot(data=val.frame.luk, aes(Prediction, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.luk)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        stat_smooth(method="lm") +
        geom_point() +
        scale_x_continuous(paste(element.name, predi)) +
        scale_y_continuous(paste(element.name, conen))

        
        ####Lukas-Tooth Calibration Curve Model Display, Total Count Normalized
        calcurve.linear.plot.luk.tc <- ggplot(data=val.frame.luk.tc, aes(IntensityNorm, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn(lm(Concentration~Intensity, predict.frame.luk.tc)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        geom_point() +
        geom_smooth(aes(x=(Intensity/sum(Intensity)), y=Concentration, ymin = Lower, ymax = Upper)) +
        scale_x_continuous(paste(element.name, norma.tc)) +
        scale_y_continuous(paste(element.name, conen))
        
        calval.linear.plot.luk.tc <- ggplot(data=val.frame.luk.tc, aes(Prediction, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.luk.tc)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        stat_smooth(method="lm") +
        geom_point() +
        scale_x_continuous(paste(element.name, predi)) +
        scale_y_continuous(paste(element.name, conen))
        
        ####Lukas-Tooth Calibration Curve Model Display, Compton Normalized
        calcurve.linear.plot.luk.comp <- ggplot(data=val.frame.luk.comp, aes(IntensityNorm, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn(lm(Concentration~Intensity, predict.frame.luk.comp)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        geom_point() +
        geom_smooth(aes(x=(Intensity/sum(Intensity)), y=Concentration, ymin = Lower, ymax = Upper)) +
        scale_x_continuous(paste(element.name, norma.comp)) +
        scale_y_continuous(paste(element.name, conen))
        
        calval.linear.plot.luk.comp <- ggplot(data=val.frame.luk.comp, aes(Prediction, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame.luk.comp)), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        stat_smooth(method="lm") +
        geom_point() +
        scale_x_continuous(paste(element.name, predi)) +
        scale_y_continuous(paste(element.name, conen))

        
        
    
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

observeEvent(input$createcalplot, {
})


output$calcurveplots <- renderPlot({
    input$createcalplot
    isolate(print(plotInput2()))
    
    
})



plotInput3 <- reactive({
    
    concentration.table <- renderHotable()
    concentration.hold <- concentration.table
    
    
    
    
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[input$calcurveelement]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[input$calcurveelement]))))
    

    
    
    predict.frame <- data.frame(concentration, intensity)
    colnames(predict.frame) <- c("Concentration", "Intensity")
    predict.intensity <- data.frame(predict.frame$Intensity)
    colnames(predict.intensity) <- c("Intensity")
    
    cal.lm <- lm(predict.frame$Concentration~predict.frame$Intensity)
    cal.lm.poly <- lm(predict.frame$Concentration~poly(predict.frame$Intensity, 2))
    
    
    
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
    cal.lm.poly.comp <- lm(predict.frame.comp$Concentration~poly(predict.frame.comp$Intensity, 2))
    

    
    total.counts <- aggregate(CPS~Spectrum, data=data, sum)
    colnames(total.counts) <- c("Spectrum", "CPS")
    
    predict.frame.tc <- data.frame(concentration, intensity/total.counts$CPS)
    colnames(predict.frame.tc) <- c("Concentration", "Intensity")
    predict.intensity.tc <- data.frame(predict.frame.tc$Intensity)
    colnames(predict.intensity.tc) <- c("Intensity")
    
    cal.lm.tc <- lm(predict.frame.tc$Concentration~predict.frame.tc$Intensity)
    cal.lm.poly.tc <- lm(predict.frame.tc$Concentration~poly(predict.frame.tc$Intensity, 2))
    
    intercept.none <- rep(0, length(spectra.line.table$Spectrum))
    lukas.intercept.table <- data.frame(spectra.line.table[input$show_vars], intercept.none)
    colnames(lukas.intercept.table) <- c(names(spectra.line.table[input$show_vars]), "None")
    
    slope.none <- rep(1, length(spectra.line.table$Spectrum))
    lukas.slope.table <- data.frame(spectra.line.table[input$show_vars], slope.none)
    colnames(lukas.slope.table) <- c(names(spectra.line.table[input$show_vars]), "None")
    
    
    
    
    ####Fourth Iteration
    lukas.intercept <- data.frame(rowSums(lukas.intercept.table[input$intercept_vars]))
    lukas.slope <- data.frame(lukas.slope.table[input$slope_vars])
    
    
    predict.frame.luk <- data.frame(concentration, (intensity*lukas.intercept),lukas.slope)
    colnames(predict.frame.luk) <- c("Concentration", "Intensity", names(lukas.slope))
    predict.intensity.luk <- data.frame(predict.frame.luk$Intensity, lukas.slope)
    colnames(predict.intensity.luk) <- c("Intensity", names(lukas.slope))
    
    lukas.lm <- lm(Concentration~., data=predict.frame.luk)
    
    cal.est.conc.pred.luk <- predict(object=lukas.lm , newdata=predict.intensity.luk, interval='confidence')
    cal.est.conc.tab.luk <- data.frame(cal.est.conc.pred.luk)
    cal.est.conc.luk <- cal.est.conc.tab.luk$fit
    cal.est.conc.luk.up <- cal.est.conc.tab.luk$upr
    cal.est.conc.luk.low <- cal.est.conc.tab.luk$lwr
    
    
    val.frame.luk <- data.frame(concentration, intensity, cal.est.conc.luk, cal.est.conc.luk.up, cal.est.conc.luk.low)
    colnames(val.frame.luk) <- c("Concentration", "Intensity", "Prediction", "Upper", "Lower")
    
    
    
    lukas.intercept.comp <- data.frame(rowSums(lukas.intercept.table[input$intercept_vars]))/compton.frame.ag$Compton
    lukas.slope.comp <- data.frame(rowSums(lukas.slope.table[input$slope_vars]))/compton.frame.ag$Compton
    
    
    lukas.intensity.comp <- (predict.frame.comp$Intensity+lukas.intercept.comp)*lukas.slope.comp
    
    
    predict.frame.luk.comp <- data.frame(concentration, lukas.intensity.comp)
    colnames(predict.frame.luk.comp) <- c("Concentration", "Intensity")
    predict.intensity.luk.comp <- data.frame(predict.frame.luk.comp$Intensity)
    colnames(predict.intensity.luk.comp) <- c("Intensity")
    
    cal.lm.luk.comp <- lm(predict.frame.luk.comp$Concentration~predict.frame.luk.comp$Intensity)
    

    lukas.intercept.comp <- data.frame(rowSums(lukas.intercept.table[input$intercept_vars]))/compton.frame.ag$Compton
    lukas.slope.comp <- data.frame(lukas.slope.table[input$slope_vars])/compton.frame.ag$Compton
    
    
    predict.frame.luk.comp <- data.frame(concentration, (intensity/compton.frame.ag$Compton*lukas.intercept.comp),lukas.slope.comp)
    colnames(predict.frame.luk.comp) <- c("Concentration", "Intensity", names(lukas.slope.comp))
    predict.intensity.luk.comp <- data.frame(predict.frame.luk.comp$Intensity, lukas.slope.comp)
    colnames(predict.intensity.luk.comp) <- c("Intensity", names(lukas.slope.comp))
    
    lukas.lm.comp <- lm(Concentration~., data=predict.frame.luk.comp)
    
  
    lukas.intercept.tc <- data.frame(rowSums(lukas.intercept.table[input$intercept_vars]))/total.counts$CPS
    lukas.slope.tc <- data.frame(lukas.slope.table[input$slope_vars])/total.counts$CPS
    
    
    predict.frame.luk.tc <- data.frame(concentration, (intensity/total.counts$CPS*lukas.intercept.tc),lukas.slope.tc)
    colnames(predict.frame.luk.tc) <- c("Concentration", "Intensity", names(lukas.slope.tc))
    predict.intensity.luk.tc <- data.frame(predict.frame.luk.tc$Intensity, lukas.slope.tc)
    colnames(predict.intensity.luk.tc) <- c("Intensity", names(lukas.slope.tc))
    
    lukas.lm.tc <- lm(Concentration~., data=predict.frame.luk.tc)
    


    lm.model <- if(input$radiocal==1 && input$normcal==1) {
        cal.lm
    } else if (input$radiocal==2 && input$normcal==1){
        cal.lm.poly
    } else if (input$radiocal==1 && input$normcal==2){
        cal.lm.tc
    } else if (input$radiocal==2 && input$normcal==2) {
        cal.lm.poly.tc
    } else if (input$radiocal==1 && input$normcal==3) {
        cal.lm.comp
    } else if (input$radiocal==2 && input$normcal==3) {
        cal.lm.poly.comp
    } else if (input$radiocal==3 && input$normcal==1) {
        lukas.lm
    } else if (input$radiocal==3 && input$normcal==2){
        lukas.lm.tc
    } else if (input$radiocal==3 && input$normcal==3) {
        lukas.lm.comp
    } else if (is.na(input$calcurveelement)) {
        return()
    }
    
    
    diagPlts<-diagPlot(lm.model)
    
    do.call(grid.arrange, c(diagPlts, ncol=2, nrow=3))


})


output$calcurvediag <- renderPlot({
    input$createcalplot
    isolate(print(plotInput3()))
    
    
})



 tableInput2 <- reactive({
     
     
     concentration.table <- renderHotable()
     concentration.hold <- concentration.table
     
     
     
     
     
     
     concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[input$calcurveelement]))))
     
     
     intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[input$calcurveelement]))))

     
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
     
     
     
     total.counts <- aggregate(CPS~Spectrum, data=data, sum)
     colnames(total.counts) <- c("Spectrum", "CPS")
     
     predict.frame.tc <- data.frame(concentration, intensity/total.counts$CPS)
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
     
     
     predict.frame.luk <- data.frame(concentration, (intensity*lukas.intercept),lukas.slope)
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
     
     
     predict.frame.luk.comp <- data.frame(concentration, (intensity/compton.frame.ag$Compton*lukas.intercept.comp),lukas.slope.comp)
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
     
     
     predict.frame.luk.tc <- data.frame(concentration, (intensity/total.counts$CPS*lukas.intercept.tc),lukas.slope.tc)
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
 
 intensities <- spectra.line.table[input$show_vars]
 n.inten <- length(intensities)
 myelements <- names(intensities[2:n.inten])
 cal.list <- vector("list", length(myelements))
 names(cal.list) <- myelements
 
 nullElementModel <- reactive({
     
     
 })


 
 elementModel <- reactive({
     
     concentration.table <- renderHotable()
     concentration.hold <- concentration.table
     
     
     
     
     
     
     concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[input$calcurveelement]))))
     
     
     intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[input$calcurveelement]))))
     
     
     predict.frame <- data.frame(concentration, intensity)
     colnames(predict.frame) <- c("Concentration", "Intensity")
     predict.intensity <- data.frame(predict.frame$Intensity)
     colnames(predict.intensity) <- c("Intensity")
     
     cal.lm <- lm(predict.frame$Concentration~predict.frame$Intensity)
     cal.lm.poly <- lm(predict.frame$Concentration~poly(predict.frame$Intensity, 2))
     
     
     
     
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
     cal.lm.poly.comp <- lm(predict.frame.comp$Concentration~poly(predict.frame.comp$Intensity, 2))
     
     
     total.counts <- aggregate(CPS~Spectrum, data=data, sum)
     colnames(total.counts) <- c("Spectrum", "CPS")
     
     predict.frame.tc <- data.frame(concentration, intensity/total.counts$CPS)
     colnames(predict.frame.tc) <- c("Concentration", "Intensity")
     predict.intensity.tc <- data.frame(predict.frame.tc$Intensity)
     colnames(predict.intensity.tc) <- c("Intensity")
     
     cal.lm.tc <- lm(predict.frame.tc$Concentration~predict.frame.tc$Intensity)
     cal.lm.poly.tc <- lm(predict.frame.tc$Concentration~poly(predict.frame.tc$Intensity, 2))
     
     
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
     
     
     predict.frame.luk <- data.frame(concentration, (intensity*lukas.intercept),lukas.slope)
     colnames(predict.frame.luk) <- c("Concentration", "Intensity", names(lukas.slope))
     predict.intensity.luk <- data.frame(predict.frame.luk$Intensity, lukas.slope)
     colnames(predict.intensity.luk) <- c("Intensity", names(lukas.slope))
     
     lukas.lm <- lm(Concentration~., data=predict.frame.luk)
     
     
     lukas.intercept.table.comp <- data.frame(rowSums(lukas.intercept.table.x[input$intercept_vars]))/compton.frame.ag$Compton
     colnames(lukas.intercept.table.comp) <- c("first")
     lukas.intercept.comp <- lukas.intercept.table.comp$first
     lukas.slope.comp <- data.frame(lukas.slope.table[input$slope_vars])/compton.frame.ag$Compton
     
     
     predict.frame.luk.comp <- data.frame(concentration, (intensity/compton.frame.ag$Compton*lukas.intercept.comp),lukas.slope.comp)
     colnames(predict.frame.luk.comp) <- c("Concentration", "Intensity", names(lukas.slope.comp))
     predict.intensity.luk.comp <- data.frame(predict.frame.luk.comp$Intensity, lukas.slope.comp)
     colnames(predict.intensity.luk.comp) <- c("Intensity", names(lukas.slope.comp))
     
     lukas.lm.comp <- lm(Concentration~., data=predict.frame.luk.comp)
     
        lukas.intercept.table.tc <- data.frame(rowSums(lukas.intercept.table.x[input$intercept_vars]))/total.counts$CPS
     colnames(lukas.intercept.table.tc) <- c("first")
     lukas.intercept.tc <- lukas.intercept.table.tc$first
     lukas.slope.tc <- data.frame(lukas.slope.table[input$slope_vars])/total.counts$CPS
     
     
     predict.frame.luk.tc <- data.frame(concentration, (intensity/total.counts$CPS*lukas.intercept.tc),lukas.slope.tc)
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
     
model
     
     
 })
 






})

})








