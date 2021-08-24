library(shiny)
library(markdown)
library(ggmap)
library(here)
# ----------------------------------------#
# MAIN USER INTERFACE FOR THE APPLICATION #
# ----------------------------------------#

defaultVals<-c(0.1,.1,.3,.5,.5)
data<-read.csv(here('Data/shiny_data_jj.csv'))
shinyUI(fluidPage(navbarPage("New England Groundfish MSE",
                             # Enter specifications
                             tabPanel("Home",
                                      mainPanel(
                                         fluidRow(column(12,"About",
                                         fluidRow(column(12,
                                          tabsetPanel(
                                            tabPanel("Harvest Control Rules"),
                                           tabPanel("Operating Models"),
                                           tabPanel("Performance Metrics")
                                         )#close tabset
                                         ))))# close rows
                                         ) # close main panel
                                      ), #close tab panel
                             
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
                                                     tags$div(uiOutput("dynamicRho")), tags$div(uiOutput("dynamicFreq"))),
                                        mainPanel(
                                          fluidRow(column(12,"Reading Plots",
                                         fluidRow(column(12, actionButton("do", "Create plots"),
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
                                          )#close tabset
                                          ))))#close rows
                                         )#close main panel
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
                                                   fluidRow(column(12, "Reading Plots",
                                                   fluidRow(column(12,
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
                                                     
                                                   )#close tabset
                                                   ))))#close rows
                                                   )# close main panel
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
                                        mainPanel(
                                          fluidRow(column(12,"Reading Plots",
                                          fluidRow(column(12,
                                          actionButton("do3", "Create plots"),
                                                  tabsetPanel(
                                                    tabPanel("Stock Trajectory", plotOutput("kobe")),
                                                    tabPanel("Ratio",
                                                             fluidRow(
                                                               splitLayout(plotOutput("SSBratio"), plotOutput("Fratio")))),
                                                    tabPanel("Spider",
                                                             fluidRow(
                                                               splitLayout(plotOutput("short"), plotOutput("medium"), plotOutput("long"))))
                                         )#close tabset
                                         ))))#close rows
                                         )# close main panel
                                      )), #close tab panel
                             tabPanel("Compare Scenarios",
                                      sidebarLayout(
                                        sidebarPanel(width=3,
                                                     div(helpText("Specifications"), style = "font-size:100%"),
                                                     tags$div(checkboxGroupInput("om4", "Operating Model and Stock Assessment Misspecification Scenario:",
                                                                          c("Base Case Overfished Scenario" = "1",
                                                                            "Base Case Not Overfished Scenario" = "2",
                                                                            "Overfished Mortality Misspecified Scenario" = "3",
                                                                            "Overfished Recruitment Misspecified Scenario" = "4",
                                                                            "Overfished Mortality and Recruitment Misspecified Scenario" = "5",
                                                                            "Not Overfished Catchability Misspecified Scenario" = "6")), 
                                                              style = "font-size:100%"),
                                                     tags$div(checkboxGroupInput("rho4", "Rho-adjustment Scenario:",
                                                                          c("No rho-adjustment" = "2",
                                                                            "Rho-adjustment" = "1")),
                                                              style = "font-size:100%"), 
                                                     tags$div(checkboxGroupInput("freq4", "Stock assessment frequency:",
                                                                          c("Two year updates" = "1",
                                                                            "Annual updates" = "2")),
                                                              style = "font-size:100%"),
                                                     tags$div(checkboxGroupInput("hcr", "Harvest Control Rule:",
                                                                                 c("Ramp" = "1",
                                                                                   "Pstar" = "2",
                                                                                   "F-step"="3",
                                                                                   "Contrained ramp"="4")),
                                                              style = "font-size:100%"),
                                                     actionButton("do4", "Compare!")),
                                       mainPanel("Hello"
                                        ))# close main panel
                                      ) #close tab panel
                             )))# close navbar

