library(shiny)
library(markdown)
library(ggmap)
library(here)
# ----------------------------------------#
# MAIN USER INTERFACE FOR THE APPLICATION #
# ----------------------------------------#

defaultVals<-c(0.1,.1,.3,.5,.5)
data<-read.csv(here('Data/shiny_data_jj_update.csv'))
shinyUI(fluidPage(navbarPage("New England Groundfish MSE",
                             # Enter specifications
                             tabPanel("Home",
                                      mainPanel(
                                         fluidRow(column(12,includeMarkdown("About.Rmd"),
                                         fluidRow(column(2, tags$a(href="https://s3.amazonaws.com/nefmc.org/1aii_Summary_HCR_Evaluation_Report.pdf",
                                                              target="_blank", "Full Report", icon("fas fa-link"), style="font-size:125%;")),
                                                  column(3, tags$a(href="https://s3.amazonaws.com/nefmc.org/1aii_Summary_HCR_Evaluation_Report.pdf",
                                                                   target="_blank", "Executive Summary", icon("fas fa-link"), style="font-size:125%;")),
                                                  column(2, tags$a(href="https://www.gmri.org/projects/evaluating-alternative-harvest-control-rules-new-england-groundfish/",
                                                                   target="_blank", "Project site", icon("fas fa-link"), style="font-size:125%;")),
                                          fluidRow(tags$div(style="margin-bottom:50px;")),
                                         fluidRow(column(12,
                                          tabsetPanel(
                                           tabPanel("Harvest Control Rules",includeMarkdown("HCR.Rmd"),img(src="HCR.png")),
                                           tabPanel("Scenarios",includeMarkdown("Scenarios.Rmd"),dataTableOutput("scenarios")),
                                           tabPanel("Performance Metrics", dataTableOutput("metrics")),
                                           tabPanel("Glossary", dataTableOutput("glossary")),
                                           tabPanel("Acknowledgments", includeMarkdown("Acknowledgments.Rmd"))
                                         )#close tabset
                                         )))))# close rows
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
                                          fluidRow(column(4,img(src="trajectory.png")), 
                                                   column(4,img(src="boxplots.png")),
                                                   column(4,img(src="boxplot.png")),
                                         fluidRow(column(12, actionButton("do", "Create plots"),
                                          tabsetPanel(
                                            tabPanel("Trajectories", 
                                                     fluidRow(
                                                       splitLayout(plotOutput("SSBplot"), plotOutput("Fplot"))),
                                                     fluidRow(
                                                       splitLayout(plotOutput("Catchplot"), plotOutput("Rplot")))),
                                            tabPanel(" 95% Trajectories",
                                                     fluidRow(
                                                       splitLayout(plotOutput("SSBCI"), plotOutput("FCI"))),
                                                     fluidRow(
                                                       splitLayout(plotOutput("CatchCI"), plotOutput("RCI")))),
                                            tabPanel("Catch", plotOutput("CatchBox"))
                                          )#close tabset
                                          )))#close rows
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
                                                   fluidRow(column(4,img(src="trajectory.png")),
                                                            column(4, img(src="boxplots.png")),
                                                   fluidRow(column(12,
                                                   actionButton("do2", "Create plots"),
                                                   tabsetPanel(
                                                     tabPanel("Relative Error",
                                                              fluidRow(
                                                                splitLayout(plotOutput("REESSB", width="90%"), plotOutput("REEF", width="90%")))),
                                                     tabPanel("Retrospective Patterns",
                                                              fluidRow(
                                                                splitLayout(plotOutput("rhoSSB", width="90%"), plotOutput("rhoF", width="90%")))),
                                                     tabPanel("Reference Point Accuracy",
                                                              fluidRow(
                                                                splitLayout(plotOutput("SSBerror", width="90%"), plotOutput("Ferror", width="90%"))))
                                                     
                                                   )#close tabset
                                                   )))#close rows
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
                                          fluidRow(column(4,img(src="kobe2.png")),
                                                   column(4, img(src="boxplots.png")),
                                                   column(4, img(src="radar.png")),
                                          fluidRow(column(12,
                                          actionButton("do3", "Create plots"),
                                                  tabsetPanel(
                                                    tabPanel("Stock Status Trajectory", plotOutput("kobe")),
                                                    tabPanel("Stock Status Ratio",
                                                             fluidRow(
                                                               splitLayout(plotOutput("SSBratio"), plotOutput("Fratio")))),
                                                    tabPanel("Radar",
                                                             fluidRow(
                                                               splitLayout(plotOutput("short"), plotOutput("medium"), plotOutput("long"))))
                                         )#close tabset
                                         )))#close rows
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

