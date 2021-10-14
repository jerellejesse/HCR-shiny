plotCatchBox <- function(om,rho,freq)
{
  df<-read.csv(here('Data/boxdata_jj.csv'))
  df<-df[df$OM==om,]
  df<-df[df$Rho==rho,]
  df<-df[df$Frequency==freq,]
  df$HCR[df$HCR==1]<-'Ramp'
  df$HCR[df$HCR==2]<-'P*'
  df$HCR[df$HCR==3]<-'F-step'
  df$HCR[df$HCR==4]<-'Constrained ramp'
  df$HCR<-as.factor(df$HCR)
  
  ggplot(df)+
    geom_boxplot(aes(x=Time, y=catch, fill=HCR)) +
    theme_classic()+
    ylab('Catch (mt)')+
    xlab('Time')+
    theme(text=element_text(size=18),legend.position='bottom')+
    scale_fill_colorblind()
    
}