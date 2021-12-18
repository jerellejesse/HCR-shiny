plotREESSB <- function(om2,rho2,freq2)
{
  df<-read.csv(here('Data/ree_new.csv'))
  df<-df[df$OM==om2,]
  df<-df[df$Rho==rho2,]
  df<-df[df$Frequency==freq2,]
  df$HCR[df$HCR==1]<-'Ramp'
  df$HCR[df$HCR==2]<-'P*'
  df$HCR[df$HCR==3]<-'F-step'
  df$HCR[df$HCR==4]<-'Constrained ramp'
  df$HCR<-as.factor(df$HCR)
  df<-df[df$Year>2019,]
  
  miny<-min(-10,min(df$REESSB))
  maxy<-max(10, max(df$REESSB))
  
  ggplot(df)+geom_line(aes(x=Year,y=REESSB,color=HCR),size=1)+
    theme_classic()+theme(text=element_text(size=18),legend.position='top')+
    ylab('% REE SSB')+
    ylim(miny,maxy)+
    scale_color_colorblind()
}