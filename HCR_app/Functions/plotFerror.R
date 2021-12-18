plotFerror <- function(om2,rho2,freq2)
{
  df<-read.csv(here('Data/boxdata_jj.csv'))
  df<-df[df$OM==om2,]
  df<-df[df$Rho==rho2,]
  df<-df[df$Frequency==freq2,]
  df$HCR[df$HCR==1]<-'Ramp'
  df$HCR[df$HCR==2]<-'P*'
  df$HCR[df$HCR==3]<-'F-step'
  df$HCR[df$HCR==4]<-'Constrained ramp'
  df$HCR<-as.factor(df$HCR)
  
  ggplot(df)+
  geom_boxplot(aes(x=Time, y=Fratiot, fill=HCR))+
  theme_classic()+
  ylab('Estimated/True Fmsy')+
  xlab('Time')+
  theme(text=element_text(size=18),legend.position='bottom')+
  scale_fill_colorblind()+
  geom_hline(yintercept=1, linetype="dashed", color = "black", size=1)
  
}
