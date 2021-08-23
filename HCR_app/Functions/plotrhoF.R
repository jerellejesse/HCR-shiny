plotrhoF <- function(om,rho,freq)
{
  df<-read.csv(here('Data/shiny_data_jj.csv'))
  df<-df[df$OM==om,]
  df<-df[df$Rho==rho,]
  df<-df[df$Frequency==freq,]
  df$HCR[df$HCR==1]<-'Ramp'
  df$HCR[df$HCR==2]<-'Feco'
  df$HCR[df$HCR==3]<-'F-step'
  df$HCR[df$HCR==4]<-'Constrained ramp'
  df$HCR<-as.factor(df$HCR)
  df<-df[df$Year>2019,]
  ggplot(df)+geom_line(aes(x=Year,y=rhoF,color=HCR))+
    theme_classic()+theme(text=element_text(size=18),legend.position='top')+
    ylab('Monhs Rho for F')+
    scale_y_continuous(limits = c(-0.50, 1))+
    scale_color_colorblind()
}
