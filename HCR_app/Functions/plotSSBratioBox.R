plotSSBratioBox <- function(om7,rho7,freq7)
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
  
  df<-read.csv(here('Data/boxdata2_jj.csv'))
  df<-df[df$OM==om7,]
  df<-df[df$Rho==rho7,]
  df<-df[df$Frequency==freq7,]
  df$HCR[df$HCR==1]<-'Ramp'
  df$HCR[df$HCR==2]<-'P*'
  df$HCR[df$HCR==3]<-'F-step'
  df$HCR[df$HCR==4]<-'Constrained ramp'
  df$HCR<-as.factor(df$HCR)
  
  ggplot(df)+
    geom_boxplot(aes(x=Time, y=SSBratio, fill=HCR)) +
    theme_classic()+
    ylab('SSB/SSBMSY')+
    xlab('Time')+
    geom_hline(yintercept=0.5, linetype="dashed", color = "black", size=1)+
    theme(text=element_text(size=18),legend.position='bottom')+
    scale_fill_colorblind()
  
}