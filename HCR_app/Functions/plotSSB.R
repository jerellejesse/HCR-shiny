plotSSB <- function(om,rho,freq)
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
  
df<-read.csv(here('Data/shiny_data_jj_update.csv'))
df<-df[df$OM==om,]
df<-df[df$Rho==rho,]
df<-df[df$Frequency==freq,]
df$HCR[df$HCR==1]<-'Ramp'
df$HCR[df$HCR==2]<-'P*'
df$HCR[df$HCR==3]<-'F-step'
df$HCR[df$HCR==4]<-'Constrained ramp'
df$HCR<-as.factor(df$HCR)
df<-df[df$Year>1987,]
ggplot(df)+geom_line(aes(x=Year,y=SSBest,color=HCR))+geom_point(aes(x=Year,y=SSB,color=HCR))+
  theme_classic()+theme(text=element_text(size=18),legend.position='top')+
  ylab('SSB (mt)')+geom_vline(xintercept=2019, linetype='dotted')+
  scale_color_colorblind()
}

