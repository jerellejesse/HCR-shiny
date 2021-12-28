plotshortradar <- function(om7,rho7,freq7)
{library(ggplot2)
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
  
  df<-read.csv(here('Data/radar_data_jj.csv'))[-c(1:2),]
  df<-df[df$OM==om7,]
  df<-df[df$Rho==rho7,]
  df<-df[df$Frequency==freq7,]
  df$HCR[df$HCR==1]<-'Ramp'
  df$HCR[df$HCR==2]<-'P*'
  df$HCR[df$HCR==3]<-'F-step'
  df$HCR[df$HCR==4]<-'Constrained ramp'
  df$HCR<-as.factor(df$HCR)
  
  short<-select(df,c(HCR,ShorttermSSB, ShorttermCatch,Catchstab, Ffreq, Bfreq))


 ggradar(short,
          values.radar=c("0", "0.5", "1"),
          grid.min=0, grid.mid=0.5, grid.max=1,
          group.colours = c("#000000","#E69F00","#56B4E9","#009E73","#F0E442"),
          background.circle.colour="white",
          gridline.mid.colour = "grey",
          legend.position = "top",
          axis.labels=c("Short term SSB", "Short term\nCatch", "Catch Stability", "Frequency\nnot overfishing", "Frequency\nnot\noverfished"))
  
 
 
}
