## ------------------------------------------------------------------------------------ ##
## Define server logic required to run scripts
## ------------------------------------------------------------------------------------ ##
defaultVals<-c(0.1,.1,.3,.5,.5)
NumberOfOptions <- 1
library(ggplot2)
library(ggthemes)
library(here)
library(gridExtra)
ffiles <- list.files(path='Functions/', pattern="^.*\\.R$",full.names=TRUE, recursive=TRUE)
invisible(sapply(ffiles, source))
shinyServer(function(input, output, session) {
  
  print("launching app...")
  # Do a control rule 
  observeEvent(input$do,{
    output$SSBplot<-renderPlot(plotSSB(input$om,input$rho,input$freq))
    output$Fplot<-renderPlot(plotF(input$om, input$rho, input$freq))
    output$Catchplot<-renderPlot(plotCatch(input$om, input$rho, input$freq))
    output$Rplot<-renderPlot(plotR(input$om, input$rho, input$freq))
    output$REESSB<-renderPlot(plotREESSB(input$om, input$rho, input$freq))
    output$REEF<-renderPlot(plotREEF(input$om, input$rho, input$freq))
    })
})  # End of ShinyServer
## ------------------------------------------------------------------------------------ ##





