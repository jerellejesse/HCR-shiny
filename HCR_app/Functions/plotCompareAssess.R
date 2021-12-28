plotCompareAssess <- function(comp,miss,hcr)
{library(ggplot2)
  library(ggthemes)
  library(here)
  library(gridExtra)
  library(tidyverse)
  library(rmarkdown)
  library(knitr)
  library(DT)
  library(ggrepel)
  library(ggradar)
  library(grid)
  library(plotly)
  
  if(comp == "Misspecified and correctly specified stock assessment"){
    df<-read.csv(here::here('Data/ree_new.csv'))
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
    df<-df[df$Year>2019,]
    
    df2<-read.csv(here('Data/shiny_data_jj_update.csv'))
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
    
    df3<-read.csv(here('Data/boxdata_jj.csv'))
    df3<-df3[df3$Compare_Mis==comp,]
    df3<-df3[df3$Misspecification==miss,]
    df3<-df3[df3$HCR==hcr,]
    df3$HCR[df3$HCR==1]<-'Ramp'
    df3$HCR[df3$HCR==2]<-'P*'
    df3$HCR[df3$HCR==3]<-'F-step'
    df3$HCR[df3$HCR==4]<-'Constrained ramp'
    df3$HCR<-as.factor(df3$HCR)
    df3$Misspecification[df3$Misspecification==1]<-"Mortality misspecified"
    df3$Misspecification[df3$Misspecification==2]<-"Recruitment misspecified"
    df3$Misspecification[df3$Misspecification==3]<-"Mortality & recruitment misspecified"
    df3$Misspecification[df3$Misspecification==4]<-"Catchability misspecified"
    df3$Misspecification<-as.factor(df3$Misspecification)
    
    
    if(miss == 4){
      data<-read.csv(here::here('Data/ree_new.csv'))
      Df<-filter(data, Scenario %in% c(25,26,27,28))
      Df<-Df[Df$HCR==hcr,]
      Df$HCR[Df$HCR==1]<-'Ramp'
      Df$HCR[Df$HCR==2]<-'P*'
      Df$HCR[Df$HCR==3]<-'F-step'
      Df$HCR[Df$HCR==4]<-'Constrained ramp'
      Df$HCR<-as.factor(Df$HCR)
      Df$Misspecification[is.na(Df$Misspecification)]<-"Not misspecified"
      Df$Misspecification<-as.factor(Df$Misspecification)
      Df<-Df[Df$Year>2019,]
      
      data2<-read.csv(here::here('Data/shiny_data_jj_update.csv'))
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
      
      data3<-read.csv(here::here('Data/boxdata_jj.csv'))
      Df3<-filter(data3, Scenario %in% c(25,26,27,28))
      Df3<-Df3[Df3$HCR==hcr,]
      Df3$HCR[Df3$HCR==1]<-'Ramp'
      Df3$HCR[Df3$HCR==2]<-'P*'
      Df3$HCR[Df3$HCR==3]<-'F-step'
      Df3$HCR[Df3$HCR==4]<-'Constrained ramp'
      Df3$HCR<-as.factor(Df3$HCR)
      Df3$Misspecification[is.na(Df3$Misspecification)]<-"Not misspecified"
      Df3$Misspecification<-as.factor(Df3$Misspecification)
      
      
      
      reessb<-ggplot()+geom_line(data=df,aes(x=Year,y=REESSB, color=HCR,linetype=Misspecification ), size=1)+
       geom_line(data=Df,aes(x=Year,y=REESSB, color=HCR, linetype=Misspecification), size=1)+
        ylab('% REE SSB')+
        scale_color_colorblind()+
        theme_classic()+theme(text=element_text(size=18),legend.position='none')
      
      reef<-ggplot()+geom_line(data=df,aes(x=Year,y=REEF, color=HCR,linetype=Misspecification ), size=1)+
        geom_line(data=Df,aes(x=Year,y=REEF, color=HCR, linetype=Misspecification), size=1)+
        ylab('% REE F')+
        scale_color_colorblind()+
        theme_classic()+theme(text=element_text(size=18),legend.position='none')
      
      miny<-min(c(-1,min(df2$rhoSSB, na.rm=TRUE), min(Df2$rhoSSB, na.rm=TRUE)))
      maxy<-max(c(1, max(df2$rhoSSB, na.rm=TRUE), max(Df2$rhoSSB, na.rm=TRUE)))
      
      rhoSSB<-ggplot()+geom_line(data=df2,aes(x=Year,y=rhoSSB,color=HCR, linetype=Misspecification),size=1)+
        geom_line(data=Df2,aes(x=Year,y=rhoSSB, color=HCR, linetype=Misspecification), size=1)+
        theme_classic()+theme(text=element_text(size=18),legend.position='none')+
        ylab('Monhs Rho for SSB')+
        scale_color_colorblind()+
        ylim(miny,maxy)
      
      min<-min(c(-1,min(df2$rhoF, na.rm=TRUE), min(Df2$rhoF, na.rm=TRUE)))
      max<-max(c(1, max(df2$rhoF, na.rm=TRUE), max(Df2$rhoF, na.rm=TRUE)))
      
      rhoF<-ggplot()+geom_line(data=df2,aes(x=Year,y=rhoF,color=HCR, linetype=Misspecification),size=1)+
        geom_line(data=Df2,aes(x=Year,y=rhoF, color=HCR, linetype=Misspecification), size=1)+
        theme_classic()+theme(text=element_text(size=18),legend.position='none')+
        ylab('Monhs Rho for F')+
        scale_color_colorblind()+
        ylim(min,max)
      
      SSBerror<-ggplot()+
        geom_boxplot(data=df3,aes(x=Time, y=SSBratiot, fill=HCR, linetype=Misspecification))+
        geom_boxplot(data=Df3,aes(x=Time, y=SSBratiot, fill=HCR, linetype=Misspecification))+
        theme_classic()+
        ylab('Estimated/True SSBmsy')+
        xlab('Time')+
        theme(text=element_text(size=18),legend.position='bottom')+
        scale_fill_colorblind()+
        geom_hline(yintercept=1, linetype="dashed", color = "black", size=1)
      
      Ferror<-ggplot()+
        geom_boxplot(data=df3,aes(x=Time, y=Fratiot, fill=HCR, linetype=Misspecification))+
        geom_boxplot(data=Df3,aes(x=Time, y=Fratiot, fill=HCR, linetype=Misspecification))+
        theme_classic()+
        ylab('Estimated/True SSBmsy')+
        xlab('Time')+
        theme(text=element_text(size=18),legend.position='bottom')+
        scale_fill_colorblind()+
        geom_hline(yintercept=1, linetype="dashed", color = "black", size=1)
      
      
    } else {
      data<-read.csv(here::here('Data/ree_new.csv'))
      Df<-filter(data, Scenario %in% c(1,2,3,4))
      
      Df<-Df[Df$HCR==hcr,]
      Df$HCR[Df$HCR==1]<-'Ramp'
      Df$HCR[Df$HCR==2]<-'P*'
      Df$HCR[Df$HCR==3]<-'F-step'
      Df$HCR[Df$HCR==4]<-'Constrained ramp'
      Df$HCR<-as.factor(Df$HCR)
      Df$Misspecification[is.na(Df$Misspecification)]<-"Not misspecified"
      Df<-Df[Df$Year>2019,]
      
      data2<-read.csv(here::here('Data/shiny_data_jj_update.csv'))
      Df2<-filter(data2, Scenario %in% c(1,2,3,4))
      Df2<-Df2[Df2$HCR==hcr,]
      Df2$HCR[Df2$HCR==1]<-'Ramp'
      Df2$HCR[Df2$HCR==2]<-'P*'
      Df2$HCR[Df2$HCR==3]<-'F-step'
      Df2$HCR[Df2$HCR==4]<-'Constrained ramp'
      Df2$HCR<-as.factor(Df2$HCR)
      Df2$Misspecification[is.na(Df2$Misspecification)]<-"Not misspecified"
      Df2<-Df2[Df2$Year>2019,]
      
      data3<-read.csv(here::here('Data/boxdata_jj.csv'))
      Df3<-filter(data3, Scenario %in% c(1,2,3,4))
      Df3<-Df3[Df3$HCR==hcr,]
      Df3$HCR[Df3$HCR==1]<-'Ramp'
      Df3$HCR[Df3$HCR==2]<-'P*'
      Df3$HCR[Df3$HCR==3]<-'F-step'
      Df3$HCR[Df3$HCR==4]<-'Constrained ramp'
      Df3$HCR<-as.factor(Df3$HCR)
      Df3$Misspecification[is.na(Df3$Misspecification)]<-"Not misspecified"
      Df3$Misspecification<-as.factor(Df3$Misspecification)
     
      reessb<-ggplot()+geom_line(data=df,aes(x=Year,y=REESSB, color=HCR,linetype=Misspecification ), size=1)+
        geom_line(data=Df,aes(x=Year,y=REESSB, color=HCR, linetype=Misspecification), size=1)+
        ylab('% REE SSB')+
        scale_color_colorblind()+
        theme_classic()+theme(text=element_text(size=18),legend.position='none')
      
      reef<-ggplot()+geom_line(data=df,aes(x=Year,y=REEF, color=HCR,linetype=Misspecification ), size=1)+
        geom_line(data=Df,aes(x=Year,y=REEF, color=HCR, linetype=Misspecification), size=1)+
        ylab('% REE F')+
        scale_color_colorblind()+
        theme_classic()+theme(text=element_text(size=18),legend.position='none')
      
      miny<-min(c(-1,min(df2$rhoSSB, na.rm=TRUE), min(Df2$rhoSSB, na.rm=TRUE)))
      maxy<-max(c(1, max(df2$rhoSSB, na.rm=TRUE), max(Df2$rhoSSB, na.rm=TRUE)))
      
      rhoSSB<-ggplot()+geom_line(data=df2,aes(x=Year,y=rhoSSB,color=HCR, linetype=Misspecification),size=1)+
        geom_line(data=Df2,aes(x=Year,y=rhoSSB, color=HCR, linetype=Misspecification), size=1)+
        theme_classic()+theme(text=element_text(size=18),legend.position='none')+
        ylab('Monhs Rho for SSB')+
        scale_color_colorblind()+
        ylim(miny,maxy)
      
      min<-min(c(-1,min(df2$rhoF, na.rm=TRUE), min(Df2$rhoF, na.rm=TRUE)))
      max<-max(c(1, max(df2$rhoF, na.rm=TRUE), max(Df2$rhoF, na.rm=TRUE)))
      
      rhoF<-ggplot()+geom_line(data=df2,aes(x=Year,y=rhoF,color=HCR, linetype=Misspecification),size=1)+
        geom_line(data=Df2,aes(x=Year,y=rhoF, color=HCR, linetype=Misspecification), size=1)+
        theme_classic()+theme(text=element_text(size=18),legend.position='none')+
        ylab('Monhs Rho for F')+
        scale_color_colorblind()+
        ylim(min,max)
      
      
      SSBerror<-ggplot()+
        geom_boxplot(data=df3,aes(x=Time, y=SSBratiot, fill=HCR, linetype=Misspecification))+
        geom_boxplot(data=Df3,aes(x=Time, y=SSBratiot, fill=HCR, linetype=Misspecification))+
        theme_classic()+
        ylab('Estimated/True SSBmsy')+
        xlab('Time')+
        theme(text=element_text(size=18),legend.position='bottom')+
        scale_fill_colorblind()+
        geom_hline(yintercept=1, linetype="dashed", color = "black", size=1)
      
      Ferror<-ggplot()+
        geom_boxplot(data=df3,aes(x=Time, y=Fratiot, fill=HCR, linetype=Misspecification))+
        geom_boxplot(data=Df3,aes(x=Time, y=Fratiot, fill=HCR, linetype=Misspecification))+
        theme_classic()+
        ylab('Estimated/True SSBmsy')+
        xlab('Time')+
        theme(text=element_text(size=18),legend.position='bottom')+
        scale_fill_colorblind()+
        geom_hline(yintercept=1, linetype="dashed", color = "black", size=1)
      
    }
    
  }else if (comp== "Rho-adjusted and not rho-adjusted"){
    df<-read.csv(here::here('Data/ree_new.csv'))
    df<-df[df$Compare_Rho==comp,]
    df<-df[df$HCR==hcr,]
    df$HCR[df$HCR==1]<-'Ramp'
    df$HCR[df$HCR==2]<-'P*'
    df$HCR[df$HCR==3]<-'F-step'
    df$HCR[df$HCR==4]<-'Constrained ramp'
    df$HCR<-as.factor(df$HCR)
    df<-df[df$Year>2019,]
    
    data<-read.csv(here::here('Data/ree_new.csv'))
    Df<-filter(data, Scenario %in% c(13,14,15,16))
    Df<-Df[Df$HCR==hcr,]
    Df$HCR[Df$HCR==1]<-'Ramp'
    Df$HCR[Df$HCR==2]<-'P*'
    Df$HCR[Df$HCR==3]<-'F-step'
    Df$HCR[Df$HCR==4]<-'Constrained ramp'
    Df$HCR<-as.factor(Df$HCR)
    Df<-Df[Df$Year>2019,]
    
    df2<-read.csv(here::here('Data/shiny_data_jj_update.csv'))
    df2<-df2[df2$Compare_Rho==comp,]
    df2<-df2[df2$HCR==hcr,]
    df2$HCR[df2$HCR==1]<-'Ramp'
    df2$HCR[df2$HCR==2]<-'P*'
    df2$HCR[df2$HCR==3]<-'F-step'
    df2$HCR[df2$HCR==4]<-'Constrained ramp'
    df2$HCR<-as.factor(df2$HCR)
    df2<-df2[df2$Year>2019,]
    
    data2<-read.csv(here::here('Data/shiny_data_jj_update.csv'))
    Df2<-filter(data2, Scenario %in% c(13,14,15,16))
    Df2<-Df2[Df2$HCR==hcr,]
    Df2$HCR[Df2$HCR==1]<-'Ramp'
    Df2$HCR[Df2$HCR==2]<-'P*'
    Df2$HCR[Df2$HCR==3]<-'F-step'
    Df2$HCR[Df2$HCR==4]<-'Constrained ramp'
    Df2$HCR<-as.factor(Df2$HCR)
    Df2<-Df2[Df2$Year>2019,]
    
    df3<-read.csv(here::here('Data/boxdata_jj.csv'))
    df3<-df3[df3$Compare_Rho==comp,]
    df3<-df3[df3$HCR==hcr,]
    df3$HCR[df3$HCR==1]<-'Ramp'
    df3$HCR[df3$HCR==2]<-'P*'
    df3$HCR[df3$HCR==3]<-'F-step'
    df3$HCR[df3$HCR==4]<-'Constrained ramp'
    df3$HCR<-as.factor(df3$HCR)
  
    
    data3<-read.csv(here::here('Data/boxdata_jj.csv'))
    Df3<-filter(data3, Scenario %in% c(13,14,15,16))
    Df3<-Df3[Df3$HCR==hcr,]
    Df3$HCR[Df3$HCR==1]<-'Ramp'
    Df3$HCR[Df3$HCR==2]<-'P*'
    Df3$HCR[Df3$HCR==3]<-'F-step'
    Df3$HCR[Df3$HCR==4]<-'Constrained ramp'
    Df3$HCR<-as.factor(Df3$HCR)
    
    
    
    reessb<-ggplot()+geom_line(data=df,aes(x=Year,y=REESSB, color=HCR,linetype=Rho ), size=1)+
      geom_line(data=Df,aes(x=Year,y=REESSB, color=HCR, linetype=Rho), size=1)+
      ylab('% REE SSB')+
      scale_color_colorblind()+
      theme_classic()+theme(text=element_text(size=18),legend.position='none')
    
    reef<-ggplot()+geom_line(data=df,aes(x=Year,y=REEF, color=HCR,linetype=Rho), size=1)+
      geom_line(data=Df,aes(x=Year,y=REEF, color=HCR, linetype=Rho), size=1)+
      ylab('% REE F')+
      scale_color_colorblind()+
      theme_classic()+theme(text=element_text(size=18),legend.position='none')
    
    miny<-min(c(-1,min(df2$rhoSSB, na.rm=TRUE), min(Df2$rhoSSB, na.rm=TRUE)))
    maxy<-max(c(1, max(df2$rhoSSB, na.rm=TRUE), max(Df2$rhoSSB, na.rm=TRUE)))
    
    rhoSSB<-ggplot()+geom_line(data=df2,aes(x=Year,y=rhoSSB,color=HCR, linetype=Rho),size=1)+
      geom_line(data=Df2,aes(x=Year,y=rhoSSB, color=HCR, linetype=Rho), size=1)+
      theme_classic()+theme(text=element_text(size=18),legend.position='none')+
      ylab('Monhs Rho for SSB')+
      scale_color_colorblind()+
      ylim(miny,maxy)
    
    min<-min(c(-1,min(df2$rhoF, na.rm=TRUE), min(Df2$rhoF, na.rm=TRUE)))
    max<-max(c(1, max(df2$rhoF, na.rm=TRUE), max(Df2$rhoF, na.rm=TRUE)))
    
    rhoF<-ggplot()+geom_line(data=df2,aes(x=Year,y=rhoF,color=HCR, linetype=Rho),size=1)+
      geom_line(data=Df2,aes(x=Year,y=rhoF, color=HCR, linetype=Rho), size=1)+
      theme_classic()+theme(text=element_text(size=18),legend.position='none')+
      ylab('Monhs Rho for F')+
      scale_color_colorblind()+
      ylim(min,max)
    
    
    SSBerror<-ggplot()+
      geom_boxplot(data=df3,aes(x=Time, y=SSBratiot, fill=HCR, linetype=Rho))+
      geom_boxplot(data=Df3,aes(x=Time, y=SSBratiot, fill=HCR, linetype=Rho))+
      theme_classic()+
      ylab('Estimated/True SSBmsy')+
      xlab('Time')+
      theme(text=element_text(size=18),legend.position='bottom')+
      scale_fill_colorblind()+
      geom_hline(yintercept=1, linetype="dashed", color = "black", size=1)
    
    Ferror<-ggplot()+
      geom_boxplot(data=df3,aes(x=Time, y=Fratiot, fill=HCR, linetype=Rho))+
      geom_boxplot(data=Df3,aes(x=Time, y=Fratiot, fill=HCR, linetype=Rho))+
      theme_classic()+
      ylab('Estimated/True SSBmsy')+
      xlab('Time')+
      theme(text=element_text(size=18),legend.position='bottom')+
      scale_fill_colorblind()+
      geom_hline(yintercept=1, linetype="dashed", color = "black", size=1)
    
  }else {
    df<-read.csv(here::here('Data/ree_new.csv'))
    df<-df[df$Compare_Freq==comp,]
    df<-df[df$HCR==hcr,]
    df$HCR[df$HCR==1]<-'Ramp'
    df$HCR[df$HCR==2]<-'P*'
    df$HCR[df$HCR==3]<-'F-step'
    df$HCR[df$HCR==4]<-'Constrained ramp'
    df$HCR<-as.factor(df$HCR)
    df<-df[df$Year>2019,]
    
    data<-read.csv(here::here('Data/ree_new.csv'))
    Df<-filter(data, Scenario %in% c(13,14,15,16))
    Df<-Df[Df$HCR==hcr,]
    Df$HCR[Df$HCR==1]<-'Ramp'
    Df$HCR[Df$HCR==2]<-'P*'
    Df$HCR[Df$HCR==3]<-'F-step'
    Df$HCR[Df$HCR==4]<-'Constrained ramp'
    Df$HCR<-as.factor(Df$HCR)
    Df<-Df[Df$Year>2019,]
    
    
    df2<-read.csv(here::here('Data/shiny_data_jj_update.csv'))
    df2<-df2[df2$Compare_Freq==comp,]
    df2<-df2[df2$HCR==hcr,]
    df2$HCR[df2$HCR==1]<-'Ramp'
    df2$HCR[df2$HCR==2]<-'P*'
    df2$HCR[df2$HCR==3]<-'F-step'
    df2$HCR[df2$HCR==4]<-'Constrained ramp'
    df2$HCR<-as.factor(df2$HCR)
    df2<-df2[df2$Year>2019,]
    
    data2<-read.csv(here::here('Data/shiny_data_jj_update.csv'))
    Df2<-filter(data2, Scenario %in% c(13,14,15,16))
    Df2<-Df2[Df2$HCR==hcr,]
    Df2$HCR[Df2$HCR==1]<-'Ramp'
    Df2$HCR[Df2$HCR==2]<-'P*'
    Df2$HCR[Df2$HCR==3]<-'F-step'
    Df2$HCR[Df2$HCR==4]<-'Constrained ramp'
    Df2$HCR<-as.factor(Df2$HCR)
    Df2<-Df2[Df2$Year>2019,]
    
    df3<-read.csv(here::here('Data/boxdata_jj.csv'))
    df3<-df3[df3$Compare_Rho==comp,]
    df3<-df3[df3$HCR==hcr,]
    df3$HCR[df3$HCR==1]<-'Ramp'
    df3$HCR[df3$HCR==2]<-'P*'
    df3$HCR[df3$HCR==3]<-'F-step'
    df3$HCR[df3$HCR==4]<-'Constrained ramp'
    df3$HCR<-as.factor(df3$HCR)
    
    
    data3<-read.csv(here::here('Data/boxdata_jj.csv'))
    Df3<-filter(data3, Scenario %in% c(13,14,15,16))
    Df3<-Df3[Df3$HCR==hcr,]
    Df3$HCR[Df3$HCR==1]<-'Ramp'
    Df3$HCR[Df3$HCR==2]<-'P*'
    Df3$HCR[Df3$HCR==3]<-'F-step'
    Df3$HCR[Df3$HCR==4]<-'Constrained ramp'
    Df3$HCR<-as.factor(Df3$HCR)
    
    
    reessb<-ggplot()+geom_line(data=df,aes(x=Year,y=REESSB, color=HCR,linetype=Frequency ), size=1)+
      geom_line(data=Df,aes(x=Year,y=REESSB, color=HCR, linetype=Frequency), size=1)+
      ylab('% REE SSB')+
      scale_color_colorblind()+
      theme_classic()+theme(text=element_text(size=18),legend.position='none')
    
    
    reef<-ggplot()+geom_line(data=df,aes(x=Year,y=REEF, color=HCR,linetype=Frequency), size=1)+
      geom_line(data=Df,aes(x=Year,y=REEF, color=HCR, linetype=Frequency), size=1)+
      ylab('% REE F')+
      scale_color_colorblind()+
      theme_classic()+theme(text=element_text(size=18),legend.position='none')
    
    miny<-min(c(-1,min(df2$rhoSSB, na.rm=TRUE), min(Df2$rhoSSB, na.rm=TRUE)))
    maxy<-max(c(1, max(df2$rhoSSB, na.rm=TRUE), max(Df2$rhoSSB, na.rm=TRUE)))
    
    rhoSSB<-ggplot()+geom_line(data=df2,aes(x=Year,y=rhoSSB,color=HCR, linetype=Frequency),size=1)+
      geom_line(data=Df2,aes(x=Year,y=rhoSSB, color=HCR, linetype=Frequency), size=1)+
      theme_classic()+theme(text=element_text(size=18),legend.position='none')+
      ylab('Monhs Rho for SSB')+
      scale_color_colorblind()+
      ylim(miny,maxy)
    
    min<-min(c(-1,min(df2$rhoF, na.rm=TRUE), min(Df2$rhoF, na.rm=TRUE)))
    max<-max(c(1, max(df2$rhoF, na.rm=TRUE), max(Df2$rhoF, na.rm=TRUE)))
    
    rhoF<-ggplot()+geom_line(data=df2,aes(x=Year,y=rhoF,color=HCR, linetype=Frequency),size=1)+
      geom_line(data=Df2,aes(x=Year,y=rhoF, color=HCR, linetype=Frequency), size=1)+
      theme_classic()+theme(text=element_text(size=18),legend.position='none')+
      ylab('Monhs Rho for F')+
      scale_color_colorblind()+
      ylim(min,max)
    
    
    SSBerror<-ggplot()+
      geom_boxplot(data=df3,aes(x=Time, y=SSBratiot, fill=HCR, linetype=Frequency))+
      geom_boxplot(data=Df3,aes(x=Time, y=SSBratiot, fill=HCR, linetype=Frequency))+
      theme_classic()+
      ylab('Estimated/True SSBmsy')+
      xlab('Time')+
      theme(text=element_text(size=18),legend.position='bottom')+
      scale_fill_colorblind()+
      geom_hline(yintercept=1, linetype="dashed", color = "black", size=1)
    
    Ferror<-ggplot()+
      geom_boxplot(data=df3,aes(x=Time, y=Fratiot, fill=HCR, linetype=Frequency))+
      geom_boxplot(data=Df3,aes(x=Time, y=Fratiot, fill=HCR, linetype=Frequency))+
      theme_classic()+
      ylab('Estimated/True SSBmsy')+
      xlab('Time')+
      theme(text=element_text(size=18),legend.position='bottom')+
      scale_fill_colorblind()+
      geom_hline(yintercept=1, linetype="dashed", color = "black", size=1)
  }   
  
  grid.arrange(rhoSSB, rhoF, ncol=2)
 
  
} 