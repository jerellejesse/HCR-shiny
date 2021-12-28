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
         "Operating Model")
  
  Abbreviation=c("MSE",
                 "HCR",
                 "OM")
  
  Definition=c("Iterative process of simulation testing alternate harvest strategies and assessing performance against management goals",
               "Pre-agreed guidlines that determine the level of fishing that can take place, based on preceived stock status",
               "Model used to describe the dynamics of the fish and fishery")
    
  table<-data.frame(Term, Abbreviation, Definition) 
  datatable(table)
  
  
}