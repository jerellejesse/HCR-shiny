library(shiny)
library(markdown)
library(ggmap)
library(here)
library(shinycssloaders)
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
                                                  column(3, tags$a(href="chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/viewer.html?pdfurl=https%3A%2F%2Fs3.amazonaws.com%2Fnefmc.org%2F1ai_HCR-Evaluation-Executive-Summary.pdf&clen=855666&chunk=true",
                                                                   target="_blank", "Executive Summary", icon("fas fa-link"), style="font-size:125%;")),
                                                  column(2, tags$a(href="https://www.gmri.org/projects/evaluating-alternative-harvest-control-rules-new-england-groundfish/",
                                                                   target="_blank", "Project site", icon("fas fa-link"), style="font-size:125%;")),
                                          fluidRow(tags$div(style="margin-bottom:50px;")),
                                         fluidRow(column(12,
                                          tabsetPanel(
                                           tabPanel("Harvest Control Rules",includeMarkdown("HCR.Rmd"),img(src="HCR.png")),
                                           tabPanel("Scenarios",includeMarkdown("Scenarios.Rmd"),DT::dataTableOutput("scenarios")),
                                           tabPanel("Performance Metrics", includeMarkdown("metrics.Rmd"),DT::dataTableOutput("metrics"), includeMarkdown("plots.Rmd"), 
                                                   fluidRow(
                                                    column(6, includeMarkdown("trajectory.Rmd")),
                                                    column(6, img(src="trajectory.PNG"))),
                                                   fluidRow(
                                                     column(6, includeMarkdown("boxplots.Rmd")),
                                                     column(6, img(src="boxplots.PNG"))),
                                                   fluidRow(
                                                     column(6, includeMarkdown("kobe.Rmd")),
                                                     column(6, img(src="kobe.PNG"))),
                                                   fluidRow(
                                                     column(6, includeMarkdown("radar.Rmd")),
                                                     column(6, img(src="radar.PNG")))),
                                           tabPanel("Glossary", DT::dataTableOutput("glossary")),
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
                                                                            "Overfished Natural Mortality Misspecified Scenario" = "3",
                                                                            "Overfished Recruitment Misspecified Scenario" = "4",
                                                                            "Overfished Natural Mortality and Recruitment Misspecified Scenario" = "5",
                                                                            "Not Overfished Survey Catchability Misspecified Scenario" = "6")),  
                                                              style = "font-size:100%"),
                                                     tags$div(uiOutput("dynamicRho")), tags$div(uiOutput("dynamicFreq")),
                                                     tags$div(actionButton("do", "Create plots"))),
                                        mainPanel(
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
                                         )#close main panel
                                        )), #close tab panel
                             
                                            tabPanel("Assessment Performance",
                                              sidebarLayout(
                                                 sidebarPanel(width=3,
                                                              div(helpText("Specifications"), style = "font-size:100%"),
                                                              tags$div(selectInput("om2", "Operating Model and Stock Assessment Misspecification Scenario:",
                                                                                   c("Base Case Overfished Scenario" = "1",
                                                                                     "Base Case Not Overfished Scenario" = "2",
                                                                                     "Overfished Natural Mortality Misspecified Scenario" = "3",
                                                                                     "Overfished Recruitment Misspecified Scenario" = "4",
                                                                                     "Overfished Natural Mortality and Recruitment Misspecified Scenario" = "5",
                                                                                     "Not Overfished Survey Catchability Misspecified Scenario" = "6")), 
                                                                       style = "font-size:100%"),
                                                              tags$div(uiOutput("dynamicRho2")), tags$div(uiOutput("dynamicFreq2")),
                                                               tags$div(actionButton("do2", "Create plots"))),
                                                 mainPanel(
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
                                                   )# close main panel
                                             )), #close tab panel
                             
                             tabPanel("Management Performance",
                                      sidebarLayout(
                                        sidebarPanel(width=3,
                                                     div(helpText("Specifications"), style = "font-size:100%"),
                                                     tags$div(selectInput("om7", "Operating Model and Stock Assessment Misspecification Scenario:",
                                                                          c("Base Case Overfished Scenario" = "1",
                                                                            "Base Case Not Overfished Scenario" = "2",
                                                                            "Overfished Natural Mortality Misspecified Scenario" = "3",
                                                                            "Overfished Recruitment Misspecified Scenario" = "4",
                                                                            "Overfished Natural Mortality and Recruitment Misspecified Scenario" = "5",
                                                                            "Not Overfished Survey Catchability Misspecified Scenario" = "6")), 
                                                              style = "font-size:100%"),
                                                     tags$div(uiOutput("dynamicRho7")), tags$div(uiOutput("dynamicFreq7")),
                                                     tags$div(actionButton("do7", "Create plots"))),
                                        mainPanel(
                                                tabsetPanel(
                                                    tabPanel("Stock Status Trajectory",
                                                             fluidRow(
                                                               splitLayout(plotOutput("kobe", width="75%")))),
                                                    tabPanel("Stock Status Ratio",
                                                             fluidRow(
                                                               splitLayout(plotOutput("SSBratio"), plotOutput("Fratio")))),
                                                    tabPanel("Radar",
                                                             fluidRow(
                                                               plotOutput("shortradar"), plotOutput("mediumradar"), plotOutput("longradar")))
                                         )#close tabset
                                       )# close main panel
                                      )), #close tab panel
                             tabPanel("Misspecifications",
                                      sidebarLayout(
                                        sidebarPanel(width=4,
                                                     div(helpText("Specifications"), style = "font-size:100%"),
                                                     tags$div(selectInput("comp","Compare:",c("Misspecified and correctly specified stock assessment", "Rho-adjusted and not rho-adjusted (GOM cod) with natural mortality and recruitment misspecifications"= "Rho-adjusted and not rho-adjusted", "Two-year and annual stock assessment updates (GOM cod) with natural mortality and recruitment misspecifications"="Two-year and annual stock assessment updates")),
                                                              conditionalPanel(
                                                                condition="input.comp =='Misspecified and correctly specified stock assessment'",
                                                                selectInput("miss", "Misspecification",
                                                                            c("Natural mortality (GoM cod)"="1", "Recruitment (GoM cod)"="2", "Natural Mortality & Recruitment (GoM cod)"="3", "Survey Catchability (GB haddock)"="4"))),
                                                                    style="font-size:100%" ),  
                                                     tags$div(radioButtons("hcr", "Harvest Control Rule:",
                                                                                 c("Ramp"="1" ,
                                                                                   "P*"="2" ,
                                                                                   "F-step"="3",
                                                                                   "Contrained ramp"="4"), inline=TRUE, selected = '"Ramp"="1"')),
                                                     tags$div(actionButton("do8", "Compare!"))),
                                       mainPanel(width = 8,
                                                 fluidRow(
                                                   conditionalPanel(
                                                     condition = "input.do8 > 0",
                                                     style = "display: none;",
                                                  withSpinner(plotOutput("CompareMis"), type=1), plotOutput("CompareAssess"), plotOutput("CompareManage"), plotOutput("CompareCatch")))
                                      )  # close main panel 
                                      )) # close tab panel
                                ),#close navbar
                  hr(),
                  img(src="gmri.jpg"), img(src="smast.jpg"), img(src="noaa.jpg"), img(src="nefmc.jpg")
                         ))# close fluidpage and shiny ui
                        


