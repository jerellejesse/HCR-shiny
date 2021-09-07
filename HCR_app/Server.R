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

ffiles <- list.files(path='Functions/', pattern="^.*\\.R$",full.names=TRUE, recursive=TRUE)
invisible(sapply(ffiles, source))
shinyServer(function(input, output, session) {
  
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
  
  output$dynamicRho3<-renderUI({
    selectInput("rho3", "Rho-adjustment scenario:", unique(data$Rho[data$OM %in% input$om3]), selected = "No rho-adjustment")
  })
  
  output$dynamicFreq3<-renderUI({
    selectInput("freq3", "Stock assessment frequency:", unique(data$Frequency[data$OM %in% input$om3 & data$Rho %in% input$rho3]), selected = "Two year updates")
  })
  
output$metrics<- renderDataTable(metrics())
output$scenarios<-renderDataTable(scenarios())
output$glossary<-renderDataTable(Glossary())

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
    
    })
  
  observeEvent(input$do2, {
    output$REESSB<-renderPlot(plotREESSB(input$om2, input$rho2, input$freq2))
    output$REEF<-renderPlot(plotREEF(input$om2, input$rho2, input$freq2))
    output$rhoSSB<-renderPlot(plotrhoSSB(input$om2, input$rho2, input$freq2))
    output$rhoF<-renderPlot(plotrhoF(input$om2, input$rho2, input$freq2))
  })
})  # End of ShinyServer
## ------------------------------------------------------------------------------------ ##





