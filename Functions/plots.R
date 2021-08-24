plots <- function(om,rho,freq)
{
df<-read.csv('Table.csv')
df<-df[df$OM==om,]
df<-df[df$Rho==rho,]
df<-df[df$Frequency==freq,]
df<- df %>% drop_na(HCR)
df$HCR[df$HCR==1]<-'Ramp'
df$HCR[df$HCR==2]<-'P*'
df$HCR[df$HCR==3]<-'F-step'
df$HCR[df$HCR==4]<-'Constrained ramp'
df$HCR<-as.factor(df$HCR)
df<-df[df$Year>1987,]
plot1<-ggplot(df)+geom_line(aes(x=Year,y=SSBest,color=HCR))+geom_point(aes(x=Year,y=SSB,color=HCR))+
  theme_classic()+theme(text=element_text(size=18),legend.position='right')+
  ylab('SSB (mt)')+geom_vline(xintercept=2019, linetype='dotted')+
  scale_color_colorblind()
plot2<-ggplot(df)+geom_line(aes(x=Year,y=Catchest,color=HCR))+geom_point(aes(x=Year,y=Catchsim,color=HCR))+
  theme_classic()+theme(text=element_text(size=18),legend.position='right')+
  ylab('Catch (mt)')+geom_vline(xintercept=2019, linetype='dotted')+
  scale_color_colorblind()
require(gridExtra)
grid.arrange(plot1, plot2, ncol=1)
}