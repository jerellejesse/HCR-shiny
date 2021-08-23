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
                                                     tags$div(selectInput("om0", "Operating Model and Stock Assessment Misspecification Scenario:",
                                                                          c("Base Case Overfished Scenario" = "1",
                                                                            "Base Case Not Overfished Scenario" = "2",
                                                                            "Overfished Mortality Misspecified Scenario" = "3",
                                                                            "Overfished Recruitment Misspecified Scenario" = "4",
                                                                            "Overfished Mortality and Recruitment Misspecified Scenario" = "5",
                                                                            "Not Overfished Catchability Misspecified Scenario" = "6")), 
                                                              style = "font-size:100%"),
                                                     tags$div(selectInput("rho0", "Rho-adjustment Scenario:",
                                                                          c("No rho-adjustment" = "2",
                                                                            "Rho-adjustment" = "1")),
                                                              style = "font-size:100%"), 
                                                     tags$div(selectInput("freq0", "Stock assessment frequency:",
                                                                          c("Two year updates" = "1",
                                                                            "Annual updates" = "2")),
                                                              style = "font-size:100%")),
                                       mainPanel() # close main panel
                                      )), #close tab panel
                             
                                        tabPanel("Stock Performance",
                                          sidebarLayout(
                                            sidebarPanel(width=3,
                                                     div(helpText("Specifications"), style = "font-size:100%"),
                                                     tags$div(selectInput("om", "Operating Model and Stock Assessment Misspecification Scenario:",
                                                                          c("Base Case Overfished Scenario" = "1",
                                                                            "Base Case Not Overfished Scenario" = "2",
                                                                            "Overfished Mortality Misspecified Scenario" = "3",
                                                                            "Overfished Recruitment Misspecified Scenario" = "4",
                                                                            "Overfished Mortality and Recruitment Misspecified Scenario" = "5",
                                                                            "Not Overfished Catchability Misspecified Scenario" = "6")), 
                                                              style = "font-size:100%"),
                                                     tags$div(selectInput("rho", "Rho-adjustment Scenario:",
                                                                          c("No rho-adjustment" = "2",
                                                                            "Rho-adjustment" = "1")),
                                                              style = "font-size:100%"), 
                                                     tags$div(selectInput("freq", "Stock assessment frequency:",
                                                                          c("Two year updates" = "1",
                                                                            "Annual updates" = "2")),
                                                              style = "font-size:100%")),
                                        mainPanel(
                                          actionButton("do", "Create plots"),
                                          tabsetPanel(
                                            tabPanel("Estimated/ True", 
                                                     fluidRow(
                                                       splitLayout(plotOutput("SSBplot"), plotOutput("Fplot"))),
                                                     fluidRow(
                                                       splitLayout(plotOutput("Catchplot"), plotOutput("Rplot")))),
                                            tabPanel("True 95% confidence interval",
                                                     fluidRow(
                                                       splitLayout(plotOutput("SSBCI"), plotOutput("FCI"))),
                                                     fluidRow(
                                                       splitLayout(plotOutput("CatchCI"), plotOutput("RCI")))),
                                            tabPanel("Catch", plotOutput("CatchBox"))
                                          )) #close main panel
                                        )), #close tab panel
                                        tabPanel("Assessment Performance",
                                              sidebarLayout(
                                                 sidebarPanel(width=3,
                                                              div(helpText("Specifications"), style = "font-size:100%"),
                                                              tags$div(selectInput("om2", "Operating Model and Stock Assessment Misspecification Scenario:",
                                                                                   c("Base Case Overfished Scenario" = "1",
                                                                                     "Base Case Not Overfished Scenario" = "2",
                                                                                     "Overfished Mortality Misspecified Scenario" = "3",
                                                                                     "Overfished Recruitment Misspecified Scenario" = "4",
                                                                                     "Overfished Mortality and Recruitment Misspecified Scenario" = "5",
                                                                                     "Not Overfished Catchability Misspecified Scenario" = "6")), 
                                                                       style = "font-size:100%"),
                                                              tags$div(selectInput("rho2", "Rho-adjustment Scenario:",
                                                                                   c("No rho-adjustment" = "2",
                                                                                     "Rho-adjustment" = "1")),
                                                                       style = "font-size:100%"), 
                                                              tags$div(selectInput("freq2", "Stock assessment frequency:",
                                                                                   c("Two year updates" = "1",
                                                                                     "Annual updates" = "2")),
                                                                       style = "font-size:100%")),
                                                 mainPanel(
                                                   actionButton("do2", "Create plots"),
                                                   tabsetPanel(
                                                     tabPanel("REE",
                                                              fluidRow(
                                                                splitLayout(plotOutput("REESSB"), plotOutput("REEF")))),
                                                     tabPanel("Mohns Rho",
                                                              fluidRow(
                                                                splitLayout(plotOutput("rhoSSB"), plotOutput("rhoF")))),
                                                     tabPanel("Reference Point Error",
                                                              fluidRow(
                                                                splitLayout(plotOutput("SSBerror"), plotOutput("Ferror"))))
                                                     
                                                   ))# close main panel
                             )), #close tab panel
                             tabPanel("Management Performance",
                                      sidebarLayout(
                                        sidebarPanel(width=3,
                                                     div(helpText("Specifications"), style = "font-size:100%"),
                                                     tags$div(selectInput("om3", "Operating Model and Stock Assessment Misspecification Scenario:",
                                                                          c("Base Case Overfished Scenario" = "1",
                                                                            "Base Case Not Overfished Scenario" = "2",
                                                                            "Overfished Mortality Misspecified Scenario" = "3",
                                                                            "Overfished Recruitment Misspecified Scenario" = "4",
                                                                            "Overfished Mortality and Recruitment Misspecified Scenario" = "5",
                                                                            "Not Overfished Catchability Misspecified Scenario" = "6")), 
                                                              style = "font-size:100%"),
                                                     tags$div(selectInput("rho3", "Rho-adjustment Scenario:",
                                                                          c("No rho-adjustment" = "2",
                                                                            "Rho-adjustment" = "1")),
                                                              style = "font-size:100%"), 
                                                     tags$div(selectInput("freq3", "Stock assessment frequency:",
                                                                          c("Two year updates" = "1",
                                                                            "Annual updates" = "2")),
                                                              style = "font-size:100%")),
                                        mainPanel(actionButton("do3", "Create plots"),
                                                  tabsetPanel(
                                                    tabPanel("Stock Trajectory", plotOutput("kobe")),
                                                    tabPanel("Ratio",
                                                             fluidRow(
                                                               splitLayout(plotOutput("SSBratio"), plotOutput("Fratio")))),
                                                    tabPanel("Spider",
                                                             fluidRow(
                                                               splitLayout(plotOutput("short"), plotOutput("medium"), plotOutput("long"))))
                                         ))# close main panel
                                      )), #close tab panel
                             tabPanel("Compare Scenarios",
                                     mainPanel(
                                        )# close main panel
                                      ) #close tab panel
                             )))# close navbar

