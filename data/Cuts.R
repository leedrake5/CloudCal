###CUTS
     
     
     observe({
         input$createcal
     intensities <- spectra.line.table[input$show_vars]
     n.inten <- length(intensities)
     myelements <- names(intensities[2:n.inten])
     cal.name.list <- paste(myelements, "cal", sep=".")
     
     cal.list <- list(get(cal.name.list))
     })
     
     return(cal.list)

     })
 
 
 # {paste(paste(as.character(input$calcurveelement)), "cal", sep=".")} <- renderPrint({
    
    #   input$createcalelement
    #   formModel()
     
     #})

 paste(as.character(input$calcurvelemement), "cal", sep=".") 
 
 
 attach(bout)
 
 bout.frame <- data.frame(year, d13c, atmosphere)
 bout.melt <- melt(bout.frame, id="year")
 qplot(year, value, data=bout.melt, colour=variable, shape=variable) + theme_light() + stat_smooth()
 
  attach(allnmcen)
 
allnmcen.frame <- data.frame(Year, d13c3, atmosphere)
allnmcen.melt <- melt(allnmcen.frame, id="Year")
 qplot(Year, value, data= allnmcen.melt, colour=variable, shape=variable) + theme_light() + stat_smooth()
