## ------------------------------------------------------------------------------------ ##
## Define server logic required to run scripts
## ------------------------------------------------------------------------------------ ##
defaultVals<-c(0.1,.1,.3,.5,.5)
NumberOfOptions <- 2
library(ggplot2)
library(ggthemes)
library(here)
library(gridExtra)
library(tidyverse)
library(rmarkdown)
library(knitr)
library(DT)
library(ggrepel)
library(ggradar)
library(grid)

ffiles <- list.files(path='Functions/', pattern="^.*\\.R$",full.names=TRUE, recursive=TRUE)
invisible(sapply(ffiles, source))

shinyServer(function(input, output, session){
  
  print("launching app...")
  data<-read.csv(here("Data/shiny_data_jj_update.csv"))

  output$dynamicRho<-renderUI({
    selectInput("rho", "Rho-adjustment scenario:", unique(data$Rho[data$OM %in% input$om]), selected = "No rho-adjustment")
    })
  
  output$dynamicFreq<-renderUI({
    selectInput("freq", "Stock assessment frequency:", unique(data$Frequency[data$OM %in% input$om & data$Rho %in% input$rho]), selected ="Two year updates")
  })
  
  output$dynamicRho2<-renderUI({
    selectInput("rho2", "Rho-adjustment scenario:", unique(data$Rho[data$OM %in% input$om2]), selected = "No rho-adjustment")
  })
  
  output$dynamicFreq2<-renderUI({
    selectInput("freq2", "Stock assessment frequency:", unique(data$Frequency[data$OM %in% input$om2 & data$Rho %in% input$rho2]), selected = "Two year updates")
  })
  
  output$dynamicRho7<-renderUI({
    selectInput("rho7", "Rho-adjustment scenario:", unique(data$Rho[data$OM %in% input$om7]), selected = "No rho-adjustment")
  })
  
  output$dynamicFreq7<-renderUI({
    selectInput("freq7", "Stock assessment frequency:", unique(data$Frequency[data$OM %in% input$om7 & data$Rho %in% input$rho7]), selected = "Two year updates")
  })
  

  
output$metrics<- renderDataTable(metrics())
output$scenarios<-renderDataTable(scenarios())
output$glossary<-renderDataTable(Glossary())

#render gifs
observeEvent(input$do3, {
output$trajectorygif<- renderImage(
  list(src=here("HCR_app/www/trajectory.gif"), align="left", height="300px", width="600px"), deleteFile = FALSE)
   })

observeEvent(input$do4,{
output$boxplotsgif<- renderImage(
  list(src=here("HCR_app/www/boxplots.gif"), align="left", height="300px", width="600px"), deleteFile = FALSE)
  })

observeEvent(input$do5,{
output$kobegif<- renderImage(
  list(src=here("HCR_app/www/kobe.gif"), align="left", height="300px", width="500px"), deleteFile = FALSE)
  })

observeEvent(input$do6,{
output$radargif<- renderImage(
  list(src=here("HCR_app/www/radar.gif"), align="left", height="300px", width="300px"), deleteFile = FALSE)
  })
   

  # Do a control rule 
  observeEvent(input$do,{
    output$SSBplot<-renderPlot(plotSSB(input$om,input$rho,input$freq))
    output$Fplot<-renderPlot(plotF(input$om, input$rho, input$freq))
    output$Catchplot<-renderPlot(plotCatch(input$om, input$rho, input$freq))
    output$Rplot<-renderPlot(plotR(input$om, input$rho, input$freq))
    
    output$FCI<-renderPlot(plotFCI(input$om, input$rho, input$freq))
    output$SSBCI<-renderPlot(plotSSBCI(input$om, input$rho, input$freq))
    output$CatchCI<-renderPlot(plotCatchCI(input$om, input$rho, input$freq))
    output$RCI<-renderPlot(plotRCI(input$om, input$rho, input$freq))
    output$CatchBox<-renderPlot(plotCatchBox(input$om, input$rho, input$freq))
    
    })
  
  observeEvent(input$do2, {
    output$REESSB<-renderPlot(plotREESSB(input$om2, input$rho2, input$freq2))
    output$REEF<-renderPlot(plotREEF(input$om2, input$rho2, input$freq2))
    output$rhoSSB<-renderPlot(plotrhoSSB(input$om2, input$rho2, input$freq2))
    output$rhoF<-renderPlot(plotrhoF(input$om2, input$rho2, input$freq2))
    output$SSBerror<-renderPlot(plotSSBerror(input$om2, input$rho2, input$freq2))
    output$Ferror<-renderPlot(plotFerror(input$om2, input$rho2, input$freq2))
  })
  
  observeEvent(input$do7, {
    output$SSBratio<- renderPlot(plotSSBratioBox(input$om7, input$rho7, input$freq7))
    output$Fratio<- renderPlot(plotFratioBox(input$om7, input$rho7, input$freq7))
    output$kobe<-renderPlot(plotkobe(input$om7,input$rho7, input$freq7))
    output$shortradar<-renderPlot(plotshortradar(input$om7, input$rho7, input$freq7))
    output$mediumradar<-renderPlot(plotmediumradar(input$om7, input$rho7, input$freq7))
    output$longradar<-renderPlot(plotlongradar(input$om7, input$rho7, input$freq7))
    output$estimatedkobe<-renderPlot(plotestimatedkobe(input$om7, input$rho7, input$freq7))
    output$terminalkobe<-renderPlot(plotterminalkobe(input$om7, input$rho7, input$freq7))
    
      })

 observeEvent(input$do8, {
     output$CompareMis<- renderPlot(plotCompareMis(input$comp, input$miss, input$hcr, input$plottype))
     
       })
 

})  # End of ShinyServer

## ------------------------------------------------------------------------------------ ##





