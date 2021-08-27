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
                                         fluidRow(column(12,includeMarkdown("About.Rmd"),
                                         fluidRow(column(12,
                                          tabsetPanel(
                                           tabPanel("Harvest Control Rules",includeMarkdown("HCR.Rmd"),img(src="HCR.png")),
                                           tabPanel("Scenarios",includeMarkdown("Scenarios.Rmd"),dataTableOutput("scenarios")),
                                           tabPanel("Performance Metrics", dataTableOutput("metrics")),
                                           tabPanel("Glossary", dataTableOutput("glossary"))
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
                                          fluidRow(column(12,"Reading Plots- will add info about how to read plots",
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
                                                              tags$div(uiOutput("dynamicRho2")), tags$div(uiOutput("dynamicFreq2"))),
                                                 mainPanel(
                                                   fluidRow(column(12, "Reading Plots- will add info about how to read plots",
                                                   fluidRow(column(12,
                                                   actionButton("do2", "Create plots"),
                                                   tabsetPanel(
                                                     tabPanel("REE",
                                                              fluidRow(
                                                                splitLayout(plotOutput("REESSB", width="90%"), plotOutput("REEF", width="90%")))),
                                                     tabPanel("Mohns Rho",
                                                              fluidRow(
                                                                splitLayout(plotOutput("rhoSSB", width="90%"), plotOutput("rhoF", width="90%")))),
                                                     tabPanel("Reference Point Error",
                                                              fluidRow(
                                                                splitLayout(plotOutput("SSBerror", width="90%"), plotOutput("Ferror", width="90%"))))
                                                     
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
                                                     tags$div(uiOutput("dynamicRho3")), tags$div(uiOutput("dynamicFreq3"))),
                                        mainPanel(
                                          fluidRow(column(12,"Reading Plots- will add info about how to read plots",
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
                                       mainPanel("Figuring out how to compare scenarios with the most flexibility"
                                        ))# close main panel
                                      ) #close tab panel
                             ),
                  hr(),
                  img(src="gmri.jpg"), img(src="smast.jpg"), img(src="noaa.jpg"), img(src="nefmc.jpg")
                         ))# close navbar

