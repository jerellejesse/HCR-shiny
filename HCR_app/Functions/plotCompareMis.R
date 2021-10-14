plotCompareMis <- function(comp,miss,hcr)
{
  if(comp == "Misspecified and correctly specified stock assessment"){
  df<-read.csv(here::here('Data/shiny_data_jj_update.csv'))
  df<-df[df$Compare_Mis==comp,]
  df<-df[df$Misspecification==miss,]
  df<-df[df$HCR==hcr,]
   df$HCR[df$HCR==1]<-'Ramp'
   df$HCR[df$HCR==2]<-'P*'
   df$HCR[df$HCR==3]<-'F-step'
   df$HCR[df$HCR==4]<-'Constrained ramp'
  df$HCR<-as.factor(df$HCR)
  df<-df[df$Year>1987,]
  
  if(miss == 4){
  data<-read.csv(here::here('Data/shiny_data_jj_update.csv'))
  Df<-filter(data, Scenario %in% c(25,26,27,28))

  Df<-Df[Df$HCR==hcr,]
  Df$HCR[Df$HCR==1]<-'Ramp'
  Df$HCR[Df$HCR==2]<-'P*'
  Df$HCR[Df$HCR==3]<-'F-step'
  Df$HCR[Df$HCR==4]<-'Constrained ramp'
  Df$HCR<-as.factor(Df$HCR)
  Df<-Df[Df$Year>1987,]


  ggplot()+geom_line(data=df,aes(x=Year,y=SSBest, color=HCR))+geom_point(data=df,aes(x=Year,y=SSB, color=HCR))+
    geom_line(data=Df,aes(x=Year,y=SSBest, color=HCR))+geom_point(data=Df,aes(x=Year,y=SSB, color=HCR))+
    theme_classic()+theme(text=element_text(size=18),legend.position='top')+
    ylab('SSB')+
    geom_vline(xintercept=2019, linetype='dotted')+
    scale_color_colorblind()
  
  } else {
    data<-read.csv(here::here('Data/shiny_data_jj_update.csv'))
    Df<-filter(data, Scenario %in% c(1,2,3,4))

    Df<-Df[Df$HCR==hcr,]
    Df$HCR[Df$HCR==1]<-'Ramp'
    Df$HCR[Df$HCR==2]<-'P*'
    Df$HCR[Df$HCR==3]<-'F-step'
    Df$HCR[Df$HCR==4]<-'Constrained ramp'
    Df$HCR<-as.factor(Df$HCR)
    Df<-Df[Df$Year>1987,]
    
    
    ggplot()+geom_line(data=df,aes(x=Year,y=SSBest, color=HCR))+geom_point(data=df,aes(x=Year,y=SSB, color=HCR))+
      geom_line(data=Df,aes(x=Year,y=SSBest, color=HCR))+geom_point(data=Df,aes(x=Year,y=SSB, color=HCR))+
      theme_classic()+theme(text=element_text(size=18),legend.position='top')+
      ylab('SSB')+
      geom_vline(xintercept=2019, linetype='dotted')+
      scale_color_colorblind()
  }
  }else if (comp== "Rho-adjusted and not rho-adjusted"){
    df<-read.csv(here::here('Data/shiny_data_jj_update.csv'))
    df<-df[df$Compare_Rho==comp,]
    df<-df[df$HCR==hcr,]
    df$HCR[df$HCR==1]<-'Ramp'
    df$HCR[df$HCR==2]<-'P*'
    df$HCR[df$HCR==3]<-'F-step'
    df$HCR[df$HCR==4]<-'Constrained ramp'
    df$HCR<-as.factor(df$HCR)
    df<-df[df$Year>2019,]
    
    data<-read.csv(here::here('Data/shiny_data_jj_update.csv'))
    Df<-filter(data, Scenario %in% c(13,14,15,16))
    Df<-Df[Df$HCR==hcr,]
    Df$HCR[Df$HCR==1]<-'Ramp'
    Df$HCR[Df$HCR==2]<-'P*'
    Df$HCR[Df$HCR==3]<-'F-step'
    Df$HCR[Df$HCR==4]<-'Constrained ramp'
    Df$HCR<-as.factor(Df$HCR)
    Df<-Df[Df$Year>2019,]
    
    ggplot()+geom_line(data=df,aes(x=Year,y=rhoSSB, color=HCR))+
      geom_line(data=Df, aes(x=Year, y=rhoSSB, color=HCR))+
      theme_classic()+theme(text=element_text(size=18),legend.position='top')+
      ylab('Mohns rho for SSB')+
      scale_color_colorblind()
    
  }else {
    df<-read.csv(here::here('Data/shiny_data_jj_update.csv'))
    df<-df[df$Compare_Freq==comp,]
    df<-df[df$HCR==hcr,]
    df$HCR[df$HCR==1]<-'Ramp'
    df$HCR[df$HCR==2]<-'P*'
    df$HCR[df$HCR==3]<-'F-step'
    df$HCR[df$HCR==4]<-'Constrained ramp'
    df$HCR<-as.factor(df$HCR)
    df<-df[df$Year>1987,]
    
    data<-read.csv(here::here('Data/shiny_data_jj_update.csv'))
    Df<-filter(data, Scenario %in% c(13,14,15,16))
    Df<-Df[Df$HCR==hcr,]
    Df$HCR[Df$HCR==1]<-'Ramp'
    Df$HCR[Df$HCR==2]<-'P*'
    Df$HCR[Df$HCR==3]<-'F-step'
    Df$HCR[Df$HCR==4]<-'Constrained ramp'
    Df$HCR<-as.factor(Df$HCR)
    Df<-Df[Df$Year>1987,]
    
    ggplot()+geom_line(data=df,aes(x=Year,y=SSBest, color=HCR))+geom_point(data=df,aes(x=Year,y=SSB, color=HCR))+
      geom_line(data=Df,aes(x=Year,y=SSBest, color=HCR))+geom_point(data=Df,aes(x=Year,y=SSB, color=HCR))+
      theme_classic()+theme(text=element_text(size=18),legend.position='top')+
      ylab('SSB')+
      geom_vline(xintercept=2019, linetype='dotted')+
      scale_color_colorblind()
    
  }
    
}
