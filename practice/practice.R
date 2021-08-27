library(shiny)

ui <- basicPage(
  headerPanel("Test Shiny App"),
  sidebarPanel(
    selectInput(inputId = "site", label = "Select Site", choices = unique(mtcars$gear)),
    uiOutput("dynamicui")),
  mainPanel()
)

server <- function(input, output, session){
  output$dynamicui <- renderUI({
    selectInput(inputId = "duct", label = "Select Duct", choices = rownames(mtcars)[mtcars$gear %in% input$site])
  })
  
}
runApp(list(ui = ui, server = server))