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
plot3<-ggplot(df)+geom_line(aes(x=Year,y=Fest,color=HCR))+geom_point(aes(x=Year,y=F_full,color=HCR))+
  theme_classic()+theme(text=element_text(size=18),legend.position='right')+
  ylab('F')+geom_vline(xintercept=2019, linetype='dotted')+
  scale_color_colorblind()
plot4<-ggplot(df)+geom_line(aes(x=Year,y=REEF,color=HCR),size=1)+
  theme_classic()+theme(text=element_text(size=18),legend.position='right')+
  ylab('%REE F')+ ylim(min(-15,min(df$Catchsim)),15)+
  scale_color_colorblind()+
  scale_x_continuous(limits = c(2020,2040))
require(gridExtra)
grid.arrange(plot1, plot2, plot3, plot4, ncol=1)
}