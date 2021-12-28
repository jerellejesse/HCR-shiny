plotCatchCI <- function(om,rho,freq)
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
  
  df<-read.csv(here('Data/Table_update.csv'))
  df<-df[df$OM==om,]
  df<-df[df$Rho==rho,]
  df<-df[df$Frequency==freq,]
  df$HCR[df$HCR==1]<-'Ramp'
  df$HCR[df$HCR==2]<-'P*'
  df$HCR[df$HCR==3]<-'F-step'
  df$HCR[df$HCR==4]<-'Constrained ramp'
  df$HCR<-as.factor(df$HCR)
  df<-df[df$Year>2019,]
  ggplot(df)+geom_line(aes(x=Year,y=Catchsim,color=HCR))+
    geom_ribbon(aes(y=Catchsim,x=Year,ymin=CatchCI_Lower,ymax=CatchCI_Upper,fill=HCR),alpha=0.2)+
    theme_classic()+theme(text=element_text(size=18),legend.position='none')+
    ylab('Catch (mt)')+
    scale_color_colorblind()+ scale_fill_colorblind()
}