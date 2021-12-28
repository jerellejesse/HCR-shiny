plotrhoSSB <- function(om2,rho2,freq2)
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
  df<-df[df$OM==om2,]
  df<-df[df$Rho==rho2,]
  df<-df[df$Frequency==freq2,]
  df$HCR[df$HCR==1]<-'Ramp'
  df$HCR[df$HCR==2]<-'P*'
  df$HCR[df$HCR==3]<-'F-step'
  df$HCR[df$HCR==4]<-'Constrained ramp'
  df$HCR<-as.factor(df$HCR)
  df<-df[df$Year>2019,]
  
  miny<-min(-1,min(df$rhoSSB))
  maxy<-max(1, max(df$rhoSSB))
  
  ggplot(df)+geom_line(aes(x=Year,y=rhoSSB,color=HCR),size=1)+
    theme_classic()+theme(text=element_text(size=18),legend.position='top')+
    ylab('Monhs Rho for SSB')+
    ylim(miny, maxy)+
    scale_color_colorblind()
}
