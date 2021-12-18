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
  df$Misspecification[df$Misspecification==1]<-"Mortality misspecified"
  df$Misspecification[df$Misspecification==2]<-"Recruitment misspecified"
  df$Misspecification[df$Misspecification==3]<-"Mortality & recruitment misspecified"
  df$Misspecification[df$Misspecification==4]<-"Catchability misspecified"
  df$Misspecification<-as.factor(df$Misspecification)
  df<-df[df$Year>1987,]
  
  
  df2<-read.csv(here::here('Data/Table_update.csv'))
  df2<-df2[df2$Compare_Mis==comp,]
  df2<-df2[df2$Misspecification==miss,]
  df2<-df2[df2$HCR==hcr,]
  df2$HCR[df2$HCR==1]<-'Ramp'
  df2$HCR[df2$HCR==2]<-'P*'
  df2$HCR[df2$HCR==3]<-'F-step'
  df2$HCR[df2$HCR==4]<-'Constrained ramp'
  df2$HCR<-as.factor(df2$HCR)
  df2$Misspecification[df2$Misspecification==1]<-"Mortality misspecified"
  df2$Misspecification[df2$Misspecification==2]<-"Recruitment misspecified"
  df2$Misspecification[df2$Misspecification==3]<-"Mortality & recruitment misspecified"
  df2$Misspecification[df2$Misspecification==4]<-"Catchability misspecified"
  df2$Misspecification<-as.factor(df2$Misspecification)
  df2<-df2[df2$Year>2019,]
  
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

  data2<-read.csv(here::here('Data/Table_update.csv'))
  Df2<-filter(data2, Scenario %in% c(25,26,27,28))
  Df2<-Df2[Df2$HCR==hcr,]
  Df2$HCR[Df2$HCR==1]<-'Ramp'
  Df2$HCR[Df2$HCR==2]<-'P*'
  Df2$HCR[Df2$HCR==3]<-'F-step'
  Df2$HCR[Df2$HCR==4]<-'Constrained ramp'
  Df2$HCR<-as.factor(Df2$HCR)
  Df2$Misspecification[is.na(Df2$Misspecification)]<-"Not misspecified"
  Df2$Misspecification<-as.factor(Df2$Misspecification)
  Df2<-Df2[Df2$Year>2019,]

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
  
  ssbci<- ggplot()+geom_line(data=df2,aes(x=Year,y=SSB, color=HCR,linetype=Misspecification), size=1)+
    geom_ribbon(data=df2, aes(x=Year,y=SSB,ymin=SSBCI_Lower, ymax=SSBCI_Upper, fill=HCR, linetype=Misspecification), alpha=0.2)+
    geom_line(data=Df2,aes(x=Year,y=SSB, color=HCR,linetype=Misspecification), size=1)+
    geom_ribbon(data=Df2, aes(x=Year, y=SSB,ymin=SSBCI_Lower, ymax=SSBCI_Upper, fill=HCR, linetype=Misspecification), alpha=0.2)+
    ylab('SSB')+
    scale_color_colorblind()+ scale_fill_colorblind()+
    theme_classic()+theme(text=element_text(size=18),legend.position='none')
   
  fci<- ggplot()+geom_line(data=df2,aes(x=Year,y=F_full, color=HCR,linetype=Misspecification), size=1)+
    geom_ribbon(data=df2, aes(x=Year,y=F_full,ymin=FCI_Lower, ymax=FCI_Upper, fill=HCR, linetype=Misspecification), alpha=0.2)+
    geom_line(data=Df2,aes(x=Year,y=F_full, color=HCR,linetype=Misspecification), size=1)+
    geom_ribbon(data=Df2, aes(x=Year, y=F_full,ymin=FCI_Lower, ymax=FCI_Upper, fill=HCR, linetype=Misspecification), alpha=0.2)+
    ylab('F')+
    scale_color_colorblind()+ scale_fill_colorblind()+
    theme_classic()+theme(text=element_text(size=18),legend.position='none')
  
  
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
    
    data2<-read.csv(here::here('Data/Table_update.csv'))
    Df2<-filter(data2, Scenario %in% c(1,2,3,4))
    Df2<-Df2[Df2$HCR==hcr,]
    Df2$HCR[Df2$HCR==1]<-'Ramp'
    Df2$HCR[Df2$HCR==2]<-'P*'
    Df2$HCR[Df2$HCR==3]<-'F-step'
    Df2$HCR[Df2$HCR==4]<-'Constrained ramp'
    Df2$HCR<-as.factor(Df2$HCR)
    Df2$Misspecification[is.na(Df2$Misspecification)]<-"Not misspecified"
    Df2$Misspecification<-as.factor(Df2$Misspecification)
    Df2<-Df2[Df2$Year>2019,]
    
    ssb<-ggplot()+
      geom_line(data=df,aes(x=Year,y=SSBest, color=HCR,linetype=Misspecification, alpha="Estimated"), size=1)+
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
    
    ssbci<- ggplot()+geom_line(data=df2,aes(x=Year,y=SSB, color=HCR,linetype=Misspecification), size=1)+
      geom_ribbon(data=df2, aes(x=Year,y=SSB,ymin=SSBCI_Lower, ymax=SSBCI_Upper, fill=HCR, linetype=Misspecification), alpha=0.2)+
      geom_line(data=Df2,aes(x=Year,y=SSB, color=HCR,linetype=Misspecification), size=1)+
      geom_ribbon(data=Df2, aes(x=Year, y=SSB,ymin=SSBCI_Lower, ymax=SSBCI_Upper, fill=HCR, linetype=Misspecification), alpha=0.2)+
      ylab('SSB')+
      scale_color_colorblind()+scale_fill_colorblind()+
      theme_classic()+theme(text=element_text(size=18),legend.position='none')
    
    fci<- ggplot()+geom_line(data=df2,aes(x=Year,y=F_full, color=HCR,linetype=Misspecification), size=1)+
      geom_ribbon(data=df2, aes(x=Year,y=F_full,ymin=FCI_Lower, ymax=FCI_Upper, fill=HCR, linetype=Misspecification), alpha=0.2)+
      geom_line(data=Df2,aes(x=Year,y=F_full, color=HCR,linetype=Misspecification), size=1)+
      geom_ribbon(data=Df2, aes(x=Year, y=F_full,ymin=FCI_Lower, ymax=FCI_Upper, fill=HCR, linetype=Misspecification), alpha=0.2)+
      ylab('F')+
      scale_color_colorblind()+scale_fill_colorblind()+
      theme_classic()+theme(text=element_text(size=18),legend.position='none')
    
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
    
    df2<-read.csv(here::here('Data/Table_update.csv'))
    df2<-df2[df2$Compare_Rho==comp,]
    df2<-df2[df2$HCR==hcr,]
    df2$HCR[df2$HCR==1]<-'Ramp'
    df2$HCR[df2$HCR==2]<-'P*'
    df2$HCR[df2$HCR==3]<-'F-step'
    df2$HCR[df2$HCR==4]<-'Constrained ramp'
    df2$HCR<-as.factor(df2$HCR)
    df2<-df2[df2$Year>2019,]
    
    data<-read.csv(here::here('Data/shiny_data_jj_update.csv'))
    Df<-filter(data, Scenario %in% c(13,14,15,16))
    Df<-Df[Df$HCR==hcr,]
    Df$HCR[Df$HCR==1]<-'Ramp'
    Df$HCR[Df$HCR==2]<-'P*'
    Df$HCR[Df$HCR==3]<-'F-step'
    Df$HCR[Df$HCR==4]<-'Constrained ramp'
    Df$HCR<-as.factor(Df$HCR)
    Df<-Df[Df$Year>1987,]
    
    
    data2<-read.csv(here::here('Data/Table_update.csv'))
    Df2<-filter(data2, Scenario %in% c(13,14,15,16))
    Df2<-Df2[Df2$HCR==hcr,]
    Df2$HCR[Df2$HCR==1]<-'Ramp'
    Df2$HCR[Df2$HCR==2]<-'P*'
    Df2$HCR[Df2$HCR==3]<-'F-step'
    Df2$HCR[Df2$HCR==4]<-'Constrained ramp'
    Df2$HCR<-as.factor(Df2$HCR)
    Df2$Misspecification[is.na(Df2$Misspecification)]<-"Not misspecified"
    Df2$Misspecification<-as.factor(Df2$Misspecification)
    Df2<-Df2[Df2$Year>2019,]
    
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
  
  ssbci<- ggplot()+geom_line(data=df2,aes(x=Year,y=SSB, color=HCR,linetype=Rho), size=1)+
    geom_ribbon(data=df2, aes(x=Year,y=SSB,ymin=SSBCI_Lower, ymax=SSBCI_Upper, fill=HCR, linetype=Rho), alpha=0.2)+
    geom_line(data=Df2,aes(x=Year,y=SSB, color=HCR,linetype=Rho), size=1)+
    geom_ribbon(data=Df2, aes(x=Year, y=SSB,ymin=SSBCI_Lower, ymax=SSBCI_Upper, fill=HCR, linetype=Rho), alpha=0.2)+
    ylab('SSB')+
    scale_color_colorblind()+scale_fill_colorblind()+
    theme_classic()+theme(text=element_text(size=18),legend.position='none')
  
  fci<- ggplot()+geom_line(data=df2,aes(x=Year,y=F_full, color=HCR,linetype=Rho), size=1)+
    geom_ribbon(data=df2, aes(x=Year,y=F_full,ymin=FCI_Lower, ymax=FCI_Upper, fill=HCR, linetype=Rho), alpha=0.2)+
    geom_line(data=Df2,aes(x=Year,y=F_full, color=HCR,linetype=Rho), size=1)+
    geom_ribbon(data=Df2, aes(x=Year, y=F_full,ymin=FCI_Lower, ymax=FCI_Upper, fill=HCR, linetype=Rho), alpha=0.2)+
    ylab('F')+
    scale_color_colorblind()+scale_fill_colorblind()+
    theme_classic()+theme(text=element_text(size=18),legend.position='none')
  
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
  
  df2<-read.csv(here::here('Data/Table_update.csv'))
  df2<-df2[df2$Compare_Freq==comp,]
  df2<-df2[df2$HCR==hcr,]
  df2$HCR[df2$HCR==1]<-'Ramp'
  df2$HCR[df2$HCR==2]<-'P*'
  df2$HCR[df2$HCR==3]<-'F-step'
  df2$HCR[df2$HCR==4]<-'Constrained ramp'
  df2$HCR<-as.factor(df2$HCR)
  df2<-df2[df2$Year>2019,]
  
  
  data<-read.csv(here::here('Data/shiny_data_jj_update.csv'))
  Df<-filter(data, Scenario %in% c(13,14,15,16))
  Df<-Df[Df$HCR==hcr,]
  Df$HCR[Df$HCR==1]<-'Ramp'
  Df$HCR[Df$HCR==2]<-'P*'
  Df$HCR[Df$HCR==3]<-'F-step'
  Df$HCR[Df$HCR==4]<-'Constrained ramp'
  Df$HCR<-as.factor(Df$HCR)
  Df<-Df[Df$Year>1987,]
  
  data2<-read.csv(here::here('Data/Table_update.csv'))
  Df2<-filter(data2, Scenario %in% c(13,14,15,16))
  Df2<-Df2[Df2$HCR==hcr,]
  Df2$HCR[Df2$HCR==1]<-'Ramp'
  Df2$HCR[Df2$HCR==2]<-'P*'
  Df2$HCR[Df2$HCR==3]<-'F-step'
  Df2$HCR[Df2$HCR==4]<-'Constrained ramp'
  Df2$HCR<-as.factor(Df2$HCR)
  Df2$Misspecification[is.na(Df2$Misspecification)]<-"Not misspecified"
  Df2$Misspecification<-as.factor(Df2$Misspecification)
  Df2<-Df2[Df2$Year>2019,]
  
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
 
 ssbci<- ggplot()+geom_line(data=df2,aes(x=Year,y=SSB, color=HCR,linetype=Frequency), size=1)+
   geom_ribbon(data=df2, aes(x=Year,y=SSB,ymin=SSBCI_Lower, ymax=SSBCI_Upper, fill=HCR, linetype=Frequency), alpha=0.2)+
   geom_line(data=Df2,aes(x=Year,y=SSB, color=HCR,linetype=Frequency), size=1)+
   geom_ribbon(data=Df2, aes(x=Year, y=SSB,ymin=SSBCI_Lower, ymax=SSBCI_Upper, fill=HCR, linetype=Frequency), alpha=0.2)+
 ylab('SSB')+
   scale_color_colorblind()+scale_fill_colorblind()+
   theme_classic()+theme(text=element_text(size=18),legend.position='none')
 
 fci<- ggplot()+geom_line(data=df2,aes(x=Year,y=F_full, color=HCR,linetype=Frequency), size=1)+
   geom_ribbon(data=df2, aes(x=Year,y=F_full,ymin=FCI_Lower, ymax=FCI_Upper, fill=HCR, linetype=Frequency), alpha=0.2)+
   geom_line(data=Df2,aes(x=Year,y=F_full, color=HCR,linetype=Frequency), size=1)+
   geom_ribbon(data=Df2, aes(x=Year, y=F_full,ymin=FCI_Lower, ymax=FCI_Upper, fill=HCR, linetype=Frequency), alpha=0.2)+
   ylab('F')+
   scale_color_colorblind()+scale_fill_colorblind()+
   theme_classic()+theme(text=element_text(size=18),legend.position='none')
}   
  grid.arrange(ssbci,fci, nrow=1)

  
  

  } 

    