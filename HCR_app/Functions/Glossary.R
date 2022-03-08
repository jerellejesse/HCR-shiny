Glossary<-function(){
  library(ggplot2)
  library(ggthemes)
  library(here)
  library(gridExtra)
  library(tidyverse)
  library(rmarkdown)
  library(knitr)
  library(DT)
  library(ggrepel)
  library(ggradar)
  library(grid)
  library(plotly)
  
  Term=c("Management strategy evaluation", 
         "Harvest control rule",
         "Operating Model",
         "Scenario",
         "Management Procedure",
         "Stock assessment",
         "Overfished",
         "Overfishing",
         "Catchability",
         "Spawning stock biomass",
         "Recruitment",
         "Fishing mortality",
         "Natural mortality",
         "Mohn's rho",
         "Retrospective pattern",
         "Maximum sustainable yield",
         "Spawning sotck biomass at MSY",
         "Fishing mortality at MSY",
         "Relative error",
         "Kobe plot",
         "Performance metric",
         "Ramp",
         "Constrained ramp",
         "P*",
         "F-step",
         "Short-term",
         "Medium-term",
         "Long-term" )
  
  Abbreviation=c("MSE",
                 "HCR",
                 "OM","",
                 "MP",
                 "","","","",
                 "SSB",
                 "R",
                 "F",
                 "M",
                 "","",
                 "MSY",
                 "SSBmsy",
                 "Fmsy",
                 "REE",
                 "","","","","","","","","")
  
  Definition=c("Iterative process of simulation testing alternate managment procedures and assessing performance against management goals",
               "Pre-agreed guidlines that determine the level of fishing that can take place, based on preceived stock status",
               "Mathematical model used to describe the dynamics of the fish and fishery",
               "Combination of OMs and MPs including overfishing, misspecification, frequency of assessment, and rho-adjustment",
               "Combination of monitoring, assessment, harvest control rule, and management action that is designed to meet objectives of the fishery and has been perfomrance tested.",
               "Process of estimating stock abundance and the impact of fishing on the stock",
               "Biomass of stock is below target level",
               "Fishing mortality rate is above target level",
               "Efficiency of survey of fishery",
               "Biomass of females in the stock. Used to address conservation concerns of maintaing the reprodutive component of the stock",
               "Number of fish born in a year and that survive to a certain age to join the stock",
               "Rate of deaths due to fishing",
               "Rate of death due to natural causes",
               "Measure of severity of retrospective patterns and can be used to adjust estimates",
               "Systemic trend in stock assessment estimates when additional years of data are added",
               "The largest yield that can be taken continuously and sustainably from a stock",
               "The equilibrium SSB that results from fishing at Fmsy",
               "The fishing mortality rate that achieves MSY",
               "Measure of stock assssment accuracy",
               "Plot that shows the trajectory of stock status over time",
               "Set a statistics used to evaluate the performance of MPs",
               "HCR that decreases F gradually with SSB if SSB is below the overfished threshold",
               "HCR that is emulates the ramp HCR with a catch variation constraint",
               "HCR that rampsdown F as SSB decreases below the overfished threshold but avoids overfishing by accounting for uncertainty with a probabilistic approach",
               "HCR that has a step decrease in F from 75% Fmsy to 70% Fmsy based on SSB",
               "1-5 years",
               "6-10 years",
               "11-21 years")
    
  table<-data.frame(Term, Abbreviation, Definition) 
  datatable(table)
  
  
}
