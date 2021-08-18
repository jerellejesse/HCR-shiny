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
    output$plts<-renderPlot(plotSSB(input$om,input$rho,input$freq))})
})  # End of ShinyServer
## ------------------------------------------------------------------------------------ ##





