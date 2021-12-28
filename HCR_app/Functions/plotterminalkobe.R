plotterminalkobe <- function(om7,rho7,freq7)
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
  
  df<-read.csv(here('Data/kobeterminal_data_jj.csv'))
  df<-df[df$OM==om7,]
  df<-df[df$Rho==rho7,]
  df<-df[df$Frequency==freq7,]
  df$HCR[df$HCR==1]<-'Ramp'
  df$HCR[df$HCR==2]<-'P*'
  df$HCR[df$HCR==3]<-'F-step'
  df$HCR[df$HCR==4]<-'Constrained ramp'
  df$HCR<-as.factor(df$HCR)

maxSSBest<-max(1.1,max(df$SSBestratioreal))
maxF<-max(1.1,max(df$Fratioreal))

df$HCR<-as.factor(df$HCR)

kobe <- ggplot(df, aes(x = SSBestratioreal, y = Fratioreal)) +
  theme_bw() 
kobe <- kobe + annotate(geom = "rect", xmin = 1, xmax = maxSSBest, ymin = 0, ymax = 1, fill = "green", colour = "green", alpha = 0.5) +
  annotate(geom = "rect", xmin = 0, xmax = 1, ymin = 1, ymax = maxF, fill = "red", colour = "red", alpha = 0.5) +
  annotate(geom = "rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "yellow", colour = "yellow", alpha = 0.5) +
  annotate(geom = "rect", xmin = 1, xmax = maxSSBest, ymin = 1, ymax = maxF, fill = "yellow", colour = "yellow", alpha = 0.5) +
  geom_path(aes(linetype = HCR,colour=HCR), size = 0.3) +
  geom_point(aes(colour=HCR)) + # colour = yr
  labs(x = 'SSB/SSB MSY',
       y = 'F/F MSY') +
  xlim(0,maxSSBest)+
  ylim(0,maxF)+
  scale_color_colorblind()+
  geom_vline(xintercept=0.5, linetype='dotted')+
  theme(text=element_text(size=16),legend.position='bottom')+
  ggtitle("Terminal")+
  geom_text_repel(data=subset(df, Year > 2039 | Year < 2020),aes(x = SSBestratioreal, y = Fratioreal, label = Year))
kobe
}