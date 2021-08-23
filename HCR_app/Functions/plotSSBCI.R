plotSSBCI <- function(om,rho,freq)
{
  df<-read.csv(here('Data/Table.csv'))
  df<-df[df$OM==om,]
  df<-df[df$Rho==rho,]
  df<-df[df$Frequency==freq,]
  df$HCR[df$HCR==1]<-'Ramp'
  df$HCR[df$HCR==2]<-'Feco'
  df$HCR[df$HCR==3]<-'F-step'
  df$HCR[df$HCR==4]<-'Constrained ramp'
  df$HCR<-as.factor(df$HCR)
  df<-df[df$Year>2019,]
  ggplot(df)+geom_line(aes(x=Year,y=SSB,color=HCR))+
    geom_ribbon(aes(y=SSB,x=Year,ymin=SSBCI_Lower,ymax=SSBCI_Upper,fill=HCR),alpha=0.2)+
    theme_classic()+theme(text=element_text(size=18),legend.position='top')+
    ylab('SSB (mt)')+
    scale_color_colorblind()+ scale_fill_colorblind()
}