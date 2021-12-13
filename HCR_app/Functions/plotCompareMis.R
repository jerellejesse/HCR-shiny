plotCompareMis <- function(comp,miss,hcr, plottype)
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
  df$Misspecification[df$Misspecification==1]<-"Mortality misspecified"
  df$Misspecification[df$Misspecification==2]<-"Recruitment misspecified"
  df$Misspecification[df$Misspecification==3]<-"Mortality & recruitment misspecified"
  df$Misspecification[df$Misspecification==4]<-"Catchability misspecified"
  df$Misspecification<-as.factor(df$Misspecification)
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
  Df$Misspecification[is.na(Df$Misspecification)]<-"Not misspecified"
  Df$Misspecification<-as.factor(Df$Misspecification)
  Df<-Df[Df$Year>1987,]


  ssb<-ggplot()+geom_line(data=df,aes(x=Year,y=SSBest, color=HCR,linetype=Misspecification, alpha="Estimated"), size=1)+
    geom_line(data=df,aes(x=Year,y=SSB, color=HCR, linetype=Misspecification, alpha="True"), size=1)+
    geom_line(data=Df,aes(x=Year,y=SSBest, color=HCR,linetype=Misspecification, alpha="Estimated"), size=1)+
    geom_line(data=Df,aes(x=Year,y=SSB, color=HCR, linetype=Misspecification, alpha="True"), size=1)+
    ylab('SSB')+
    geom_vline(xintercept=2019, linetype='dotted')+
    scale_alpha_manual("Estimated or True", values=c(0.3, 1))+
    scale_color_colorblind()+
    theme_classic()+theme(text=element_text(size=18),legend.position='right')+
    guides(alpha = guide_legend(override.aes = list(size = 2)), color=guide_legend(override.aes = list(size = 2)))
  
  f<-ggplot()+geom_line(data=df,aes(x=Year,y=Fest, color=HCR,linetype=Misspecification, alpha="Estimated"), size=1)+
    geom_line(data=df,aes(x=Year,y=F_full, color=HCR, linetype=Misspecification, alpha="True"), size=1)+
    geom_line(data=Df,aes(x=Year,y=Fest, color=HCR,linetype=Misspecification, alpha="Estimated"), size=1)+
    geom_line(data=Df,aes(x=Year,y=F_full, color=HCR, linetype=Misspecification, alpha="True"), size=1)+
    ylab('F')+
    geom_vline(xintercept=2019, linetype='dotted')+
    scale_alpha_manual("Estimated or True", values=c(0.3, 1))+
    scale_color_colorblind()+
    theme_classic()+theme(text=element_text(size=18),legend.position='right')+
    guides(alpha = guide_legend(override.aes = list(size = 2)), color=guide_legend(override.aes = list(size = 2)))
  

  c<-ggplot()+geom_line(data=df,aes(x=Year,y=Catchest, color=HCR,linetype=Misspecification, alpha="Estimated"), size=1)+
    geom_line(data=df,aes(x=Year,y=Catchsim, color=HCR, linetype=Misspecification, alpha="True"), size=1)+
    geom_line(data=Df,aes(x=Year,y=Catchest, color=HCR,linetype=Misspecification, alpha="Estimated"), size=1)+
    geom_line(data=Df,aes(x=Year,y=Catchsim, color=HCR, linetype=Misspecification, alpha="True"), size=1)+
    ylab('Catch')+
    geom_vline(xintercept=2019, linetype='dotted')+
    scale_alpha_manual("Estimated or True", values=c(0.3, 1))+
    scale_color_colorblind()+
    theme_classic()+theme(text=element_text(size=18),legend.position='right')+
    guides(alpha = guide_legend(override.aes = list(size = 2)), color=guide_legend(override.aes = list(size = 2)))
  
    } else {
    data<-read.csv(here::here('Data/shiny_data_jj_update.csv'))
    Df<-filter(data, Scenario %in% c(1,2,3,4))

    Df<-Df[Df$HCR==hcr,]
    Df$HCR[Df$HCR==1]<-'Ramp'
    Df$HCR[Df$HCR==2]<-'P*'
    Df$HCR[Df$HCR==3]<-'F-step'
    Df$HCR[Df$HCR==4]<-'Constrained ramp'
    Df$HCR<-as.factor(Df$HCR)
    Df$Misspecification[is.na(Df$Misspecification)]<-"Not misspecified"
    Df<-Df[Df$Year>1987,]
    
    ssb<-ggplot()+geom_line(data=df,aes(x=Year,y=SSBest, color=HCR,linetype=Misspecification, alpha="Estimated"), size=1)+
      geom_line(data=df,aes(x=Year,y=SSB, color=HCR, linetype=Misspecification, alpha="True"), size=1)+
      geom_line(data=Df,aes(x=Year,y=SSBest, color=HCR,linetype=Misspecification, alpha="Estimated"), size=1)+
      geom_line(data=Df,aes(x=Year,y=SSB, color=HCR, linetype=Misspecification, alpha="True"), size=1)+
      ylab('SSB')+
      geom_vline(xintercept=2019, linetype='dotted')+
      scale_alpha_manual("Estimated or True", values=c(0.3, 1))+
      scale_color_colorblind()+
      theme_classic()+theme(text=element_text(size=18),legend.position='right')+
      guides(alpha = guide_legend(override.aes = list(size = 2)), color=guide_legend(override.aes = list(size = 2)))
    
    
    f<-ggplot()+geom_line(data=df,aes(x=Year,y=Fest, color=HCR,linetype=Misspecification, alpha="Estimated"), size=1)+
      geom_line(data=df,aes(x=Year,y=F_full, color=HCR, linetype=Misspecification, alpha="True"), size=1)+
      geom_line(data=Df,aes(x=Year,y=Fest, color=HCR,linetype=Misspecification, alpha="Estimated"), size=1)+
      geom_line(data=Df,aes(x=Year,y=F_full, color=HCR, linetype=Misspecification, alpha="True"), size=1)+
      ylab('F')+
      geom_vline(xintercept=2019, linetype='dotted')+
      scale_alpha_manual("Estimated or True", values=c(0.3, 1))+
      scale_color_colorblind()+
      theme_classic()+theme(text=element_text(size=18),legend.position='right')+
      guides(alpha = guide_legend(override.aes = list(size = 2)), color=guide_legend(override.aes = list(size = 2)))
    
    c<-ggplot()+geom_line(data=df,aes(x=Year,y=Catchest, color=HCR,linetype=Misspecification, alpha="Estimated"), size=1)+
      geom_line(data=df,aes(x=Year,y=Catchsim, color=HCR, linetype=Misspecification, alpha="True"), size=1)+
      geom_line(data=Df,aes(x=Year,y=Catchest, color=HCR,linetype=Misspecification, alpha="Estimated"), size=1)+
      geom_line(data=Df,aes(x=Year,y=Catchsim, color=HCR, linetype=Misspecification, alpha="True"), size=1)+
      ylab('Catch')+
      geom_vline(xintercept=2019, linetype='dotted')+
      scale_alpha_manual("Estimated or True", values=c(0.3, 1))+
      scale_color_colorblind()+
      theme_classic()+theme(text=element_text(size=18),legend.position='right')+
      guides(alpha = guide_legend(override.aes = list(size = 2)), color=guide_legend(override.aes = list(size = 2)))
 
    r<-ggplot()+geom_line(data=df,aes(x=Year,y=Rest, color=HCR,linetype=Misspecification, alpha="Estimated"), size=1)+
      geom_line(data=df,aes(x=Year,y=R, color=HCR, linetype=Misspecification, alpha="True"), size=1)+
      geom_line(data=Df,aes(x=Year,y=Rest, color=HCR,linetype=Misspecification, alpha="Estimated"), size=1)+
      geom_line(data=Df,aes(x=Year,y=R, color=HCR, linetype=Misspecification, alpha="True"), size=1)+
      ylab('Recruitment')+
      geom_vline(xintercept=2019, linetype='dotted')+
      scale_alpha_manual("Estimated or True", values=c(0.3, 1))+
      scale_color_colorblind()+
      theme_classic()+theme(text=element_text(size=18),legend.position='right')+
      guides(alpha = guide_legend(override.aes = list(size = 2)), color=guide_legend(override.aes = list(size = 2)))
    
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
    
  ssb<- ggplot()+geom_line(data=df,aes(x=Year,y=SSBest, color=HCR,linetype=Rho, alpha="Estimated"), size=1)+
      geom_line(data=df,aes(x=Year,y=SSB, color=HCR, linetype=Rho, alpha="True"), size=1)+
      geom_line(data=Df,aes(x=Year,y=SSBest, color=HCR,linetype=Rho, alpha="Estimated"), size=1)+
      geom_line(data=Df,aes(x=Year,y=SSB, color=HCR, linetype=Rho, alpha="True"), size=1)+
      ylab('SSB')+
      geom_vline(xintercept=2019, linetype='dotted')+
      scale_alpha_manual("Estimated or True", values=c(0.3, 1))+
      scale_color_colorblind()+
      theme_classic()+theme(text=element_text(size=18),legend.position='right')+
      guides(alpha = guide_legend(override.aes = list(size = 2)), color=guide_legend(override.aes = list(size = 2)))
    
  
  f<- ggplot()+geom_line(data=df,aes(x=Year,y=Fest, color=HCR,linetype=Rho, alpha="Estimated"), size=1)+
    geom_line(data=df,aes(x=Year,y=F_full, color=HCR, linetype=Rho, alpha="True"), size=1)+
    geom_line(data=Df,aes(x=Year,y=Fest, color=HCR,linetype=Rho, alpha="Estimated"), size=1)+
    geom_line(data=Df,aes(x=Year,y=F_full, color=HCR, linetype=Rho, alpha="True"), size=1)+
    ylab('F')+
    geom_vline(xintercept=2019, linetype='dotted')+
    scale_alpha_manual("Estimated or True", values=c(0.3, 1))+
    scale_color_colorblind()+
    theme_classic()+theme(text=element_text(size=18),legend.position='right')+
    guides(alpha = guide_legend(override.aes = list(size = 2)), color=guide_legend(override.aes = list(size = 2)))
  
  c<- ggplot()+geom_line(data=df,aes(x=Year,y=Catchest, color=HCR,linetype=Rho, alpha="Estimated"), size=1)+
    geom_line(data=df,aes(x=Year,y=Catchsim, color=HCR, linetype=Rho, alpha="True"), size=1)+
    geom_line(data=Df,aes(x=Year,y=Catchest, color=HCR,linetype=Rho, alpha="Estimated"), size=1)+
    geom_line(data=Df,aes(x=Year,y=Catchsim, color=HCR, linetype=Rho, alpha="True"), size=1)+
    ylab('Catch')+
    geom_vline(xintercept=2019, linetype='dotted')+
    scale_alpha_manual("Estimated or True", values=c(0.3, 1))+
    scale_color_colorblind()+
    theme_classic()+theme(text=element_text(size=18),legend.position='right')+
    guides(alpha = guide_legend(override.aes = list(size = 2)), color=guide_legend(override.aes = list(size = 2)))
  
  r<- ggplot()+geom_line(data=df,aes(x=Year,y=Rest, color=HCR,linetype=Rho, alpha="Estimated"), size=1)+
    geom_line(data=df,aes(x=Year,y=R, color=HCR, linetype=Rho, alpha="True"), size=1)+
    geom_line(data=Df,aes(x=Year,y=Rest, color=HCR,linetype=Rho, alpha="Estimated"), size=1)+
    geom_line(data=Df,aes(x=Year,y=R, color=HCR, linetype=Rho, alpha="True"), size=1)+
    ylab('Recruitment')+
    geom_vline(xintercept=2019, linetype='dotted')+
    scale_alpha_manual("Estimated or True", values=c(0.3, 1))+
    scale_color_colorblind()+
    theme_classic()+theme(text=element_text(size=18),legend.position='right')+
    guides(alpha = guide_legend(override.aes = list(size = 2)), color=guide_legend(override.aes = list(size = 2)))
  
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
  
 ssb<- ggplot()+geom_line(data=df,aes(x=Year,y=SSBest, color=HCR,linetype=Frequency, alpha="Estimated"), size=1)+
    geom_line(data=df,aes(x=Year,y=SSB, color=HCR, linetype=Frequency, alpha="True"), size=1)+
    geom_line(data=Df,aes(x=Year,y=SSBest, color=HCR,linetype=Frequency, alpha="Estimated"), size=1)+
    geom_line(data=Df,aes(x=Year,y=SSB, color=HCR, linetype=Frequency, alpha="True"), size=1)+
    ylab('SSB')+
    geom_vline(xintercept=2019, linetype='dotted')+
    scale_alpha_manual("Estimated or True", values=c(0.3, 1))+
    scale_color_colorblind()+
    theme_classic()+theme(text=element_text(size=18),legend.position='right')+
    guides(alpha = guide_legend(override.aes = list(size = 2)), color=guide_legend(override.aes = list(size = 2)))

 f<- ggplot()+geom_line(data=df,aes(x=Year,y=Fest, color=HCR,linetype=Frequency, alpha="Estimated"), size=1)+
   geom_line(data=df,aes(x=Year,y=F_full, color=HCR, linetype=Frequency, alpha="True"), size=1)+
   geom_line(data=Df,aes(x=Year,y=Fest, color=HCR,linetype=Frequency, alpha="Estimated"), size=1)+
   geom_line(data=Df,aes(x=Year,y=F_full, color=HCR, linetype=Frequency, alpha="True"), size=1)+
   ylab('F')+
   geom_vline(xintercept=2019, linetype='dotted')+
   scale_alpha_manual("Estimated or True", values=c(0.3, 1))+
   scale_color_colorblind()+
   theme_classic()+theme(text=element_text(size=18),legend.position='right')+
   guides(alpha = guide_legend(override.aes = list(size = 2)), color=guide_legend(override.aes = list(size = 2)))
 
 c<- ggplot()+geom_line(data=df,aes(x=Year,y=Catchest, color=HCR,linetype=Frequency, alpha="Estimated"), size=1)+
   geom_line(data=df,aes(x=Year,y=Catchsim, color=HCR, linetype=Frequency, alpha="True"), size=1)+
   geom_line(data=Df,aes(x=Year,y=Catchest, color=HCR,linetype=Frequency, alpha="Estimated"), size=1)+
   geom_line(data=Df,aes(x=Year,y=Catchsim, color=HCR, linetype=Frequency, alpha="True"), size=1)+
   ylab('Catch')+
   geom_vline(xintercept=2019, linetype='dotted')+
   scale_alpha_manual("Estimated or True", values=c(0.3, 1))+
   scale_color_colorblind()+
   theme_classic()+theme(text=element_text(size=18),legend.position='right')+
   guides(alpha = guide_legend(override.aes = list(size = 2)), color=guide_legend(override.aes = list(size = 2)))
 
 r<- ggplot()+geom_line(data=df,aes(x=Year,y=Rest, color=HCR,linetype=Frequency, alpha="Estimated"), size=1)+
   geom_line(data=df,aes(x=Year,y=R, color=HCR, linetype=Frequency, alpha="True"), size=1)+
   geom_line(data=Df,aes(x=Year,y=Rest, color=HCR,linetype=Frequency, alpha="Estimated"), size=1)+
   geom_line(data=Df,aes(x=Year,y=R, color=HCR, linetype=Frequency, alpha="True"), size=1)+
   ylab('Recruitment')+
   geom_vline(xintercept=2019, linetype='dotted')+
   scale_alpha_manual("Estimated or True", values=c(0.3, 1))+
   scale_color_colorblind()+
   theme_classic()+theme(text=element_text(size=18),legend.position='right')+
   guides(alpha = guide_legend(override.aes = list(size = 2)), color=guide_legend(override.aes = list(size = 2)))
 
 
}   
  if (plottype== "SSB"){ssb}
  else if (plottype=="F"){f} 
  else if (plottype =="Catch"){c}
  else if (plottype== "R"){r}
  
  

  } 

    