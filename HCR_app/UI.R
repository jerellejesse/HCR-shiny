library(shiny)
library(markdown)
library(ggmap)
library(here)
# ----------------------------------------#
# MAIN USER INTERFACE FOR THE APPLICATION #
# ----------------------------------------#

defaultVals<-c(0.1,.1,.3,.5,.5)

shinyUI(fluidPage(navbarPage("New England Groundfish MSE",
                             # Enter specifications
                             tabPanel("Specifications",
                                      sidebarLayout(
                                        sidebarPanel(width=3,
                                                     div(helpText("Specifcations"), style = "font-size:100%"),
                                                     tags$div(selectInput("om", "Operating Model and Stock Assessment Misspecification Scenario:",
                                                                          c("Base Case Overfished Scenario" = "1",
                                                                            "Base Case Not Overfished Scenario" = "2",
                                                                            "Overfished Mortality Misspecified Scenario" = "3",
                                                                            "Overfished Recruitment Misspecified Scenario" = "4",
                                                                            "Overfished Mortality and Recruitment Misspecified Scenario" = "5",
                                                                            "Not Overfished Catchability Misspecified Scenario" = "6")), 
                                                              style = "font-size:100%"),
                                                     tags$div(selectInput("rho", "Rho-adjustment Scenario:",
                                                                          c("No rho-adjustment" = "1",
                                                                            "Rho-adjustment" = "2")),
                                                              style = "font-size:100%"), 
                                                     tags$div(selectInput("freq", "Stock assessment frequency:",
                                                                          c("Two year updates" = "2",
                                                                            "Annual updates" = "1")),
                                                              style = "font-size:100%")),
                                        mainPanel(
                                          plotOutput("CTL2",width="400px",height="300px"),
                                          actionButton("do", "Create plots"),
                                          plotOutput("plts"))),
                             ))))

