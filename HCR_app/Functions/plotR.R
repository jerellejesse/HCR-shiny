plotR <- function(om,rho,freq)
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
  df<-df[df$Year>1987,]
  ggplot(df)+geom_line(aes(x=Year,y=Rest,color=HCR))+geom_point(aes(x=Year,y=R,color=HCR))+
    theme_classic()+theme(text=element_text(size=18),legend.position='none')+
    ylab('Recruitment')+geom_vline(xintercept=2019, linetype='dotted')+
    scale_color_colorblind()
}
