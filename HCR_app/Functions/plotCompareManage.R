plotCompareManage <- function(comp,miss,hcr)
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
    df<-read.csv(here::here('Data/kobeterminal_data_jj.csv'))
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
    
    
    df2<-read.csv(here('Data/kobeestimated_data_jj.csv'))
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
    
    
    df3<-read.csv(here('Data/boxdata2_jj.csv'))
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
    
    df4<-read.csv(here('Data/radar_data_jj.csv'))[-c(1:2),]
    df4<-df4[df4$Compare_Mis==comp,]
    df4<-df4[df4$Misspecification==miss,]
    df4<-df4[df4$HCR==hcr,]
    df4$HCR[df4$HCR==1]<-'Ramp'
    df4$HCR[df4$HCR==2]<-'P*'
    df4$HCR[df4$HCR==3]<-'F-step'
    df4$HCR[df4$HCR==4]<-'Constrained ramp'
    df4$HCR<-as.factor(df4$HCR)
    df4$Misspecification[df4$Misspecification==1]<-"Mortality misspecified"
    df4$Misspecification[df4$Misspecification==2]<-"Recruitment misspecified"
    df4$Misspecification[df4$Misspecification==3]<-"Mortality & recruitment misspecified"
    df4$Misspecification[df4$Misspecification==4]<-"Catchability misspecified"
    df4$Misspecification<-as.factor(df4$Misspecification)
    
    
    if(miss == 4){
      data<-read.csv(here::here('Data/kobeterminal_data_jj.csv'))
      Df<-filter(data, Scenario %in% c(25,26,27,28))
      Df<-Df[Df$HCR==hcr,]
      Df$HCR[Df$HCR==1]<-'Ramp'
      Df$HCR[Df$HCR==2]<-'P*'
      Df$HCR[Df$HCR==3]<-'F-step'
      Df$HCR[Df$HCR==4]<-'Constrained ramp'
      Df$HCR<-as.factor(Df$HCR)
      Df$Misspecification[is.na(Df$Misspecification)]<-"Not misspecified"
      Df$Misspecification<-as.factor(Df$Misspecification)
      
      
      data2<-read.csv(here::here('Data/kobeestimated_data_jj.csv'))
      Df2<-filter(data2, Scenario %in% c(25,26,27,28))
      Df2<-Df2[Df2$HCR==hcr,]
      Df2$HCR[Df2$HCR==1]<-'Ramp'
      Df2$HCR[Df2$HCR==2]<-'P*'
      Df2$HCR[Df2$HCR==3]<-'F-step'
      Df2$HCR[Df2$HCR==4]<-'Constrained ramp'
      Df2$HCR<-as.factor(Df2$HCR)
      Df2$Misspecification[is.na(Df2$Misspecification)]<-"Not misspecified"
      Df2$Misspecification<-as.factor(Df2$Misspecification)
      
      
      data3<-read.csv(here::here('Data/boxdata2_jj.csv'))
      Df3<-filter(data3, Scenario %in% c(25,26,27,28))
      Df3<-Df3[Df3$HCR==hcr,]
      Df3$HCR[Df3$HCR==1]<-'Ramp'
      Df3$HCR[Df3$HCR==2]<-'P*'
      Df3$HCR[Df3$HCR==3]<-'F-step'
      Df3$HCR[Df3$HCR==4]<-'Constrained ramp'
      Df3$HCR<-as.factor(Df3$HCR)
      Df3$Misspecification[is.na(Df3$Misspecification)]<-"Not misspecified"
      Df3$Misspecification<-as.factor(Df3$Misspecification)
      
      
      maxSSBest<-max(1.1,max(df$SSBestratioreal, na.rm=TRUE), max(Df$SSBestratioreal, na.rm=TRUE))
      maxF<-max(1.1,max(df$Fratioreal, na.rm=TRUE), max(Df$Fratioreal, na.rm=TRUE))
      
      kobeterm <- ggplot() +
        theme_bw()+
        annotate(geom = "rect", xmin = 1, xmax = maxSSBest, ymin = 0, ymax = 1, fill = "green", colour = "green", alpha = 0.5) +
        annotate(geom = "rect", xmin = 0, xmax = 1, ymin = 1, ymax = maxF, fill = "red", colour = "red", alpha = 0.5) +
        annotate(geom = "rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "yellow", colour = "yellow", alpha = 0.5) +
        annotate(geom = "rect", xmin = 1, xmax = maxSSBest, ymin = 1, ymax = maxF, fill = "yellow", colour = "yellow", alpha = 0.5) +
        geom_path(data=df, aes(x = SSBestratioreal, y = Fratioreal, linetype = Misspecification,colour=HCR), size = 0.3) +
        geom_point(data=df, aes(x = SSBestratioreal, y = Fratioreal,shape=Misspecification, colour=HCR)) + 
        geom_path(data=Df, aes(x = SSBestratioreal, y = Fratioreal, linetype = Misspecification,colour=HCR), size = 0.3) +
        geom_point(data=Df, aes(x = SSBestratioreal, y = Fratioreal, shape=Misspecification, colour=HCR)) + 
        labs(x = 'SSB/SSB MSY',
             y = 'F/F MSY') +
        xlim(0,maxSSBest)+
        ylim(0,maxF)+
        scale_color_colorblind()+
        geom_vline(xintercept=0.5, linetype='dotted')+
        theme(text=element_text(size=16),legend.position='none')+
        geom_text_repel(data=subset(df, Year > 2039 | Year < 2020),aes(x = SSBestratioreal, y = Fratioreal, label = Year))
      
      
      maxSSB<-max(c(1.1,max(df2$SSBestratioreal, na.rm=TRUE), max(Df2$SSBratioreal, na.rm=TRUE)))
      maxF<-max(c(1.1,max(df2$Fratioreal, na.rm=TRUE), max(Df2$Fratioreal, na.rm=TRUE)))
      
      kobeest <- ggplot() +
        theme_bw()+
        annotate(geom = "rect", xmin = 1, xmax = maxSSB, ymin = 0, ymax = 1, fill = "green", colour = "green", alpha = 0.5) +
        annotate(geom = "rect", xmin = 0, xmax = 1, ymin = 1, ymax = maxF, fill = "red", colour = "red", alpha = 0.5) +
        annotate(geom = "rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "yellow", colour = "yellow", alpha = 0.5) +
        annotate(geom = "rect", xmin = 1, xmax = maxSSB, ymin = 1, ymax = maxF, fill = "yellow", colour = "yellow", alpha = 0.5) +
        geom_path(data=df2, aes(x = SSBestratioreal, y = Fratioreal, linetype = Misspecification,colour=HCR), size = 1) +
        geom_point(data=df2, aes(x = SSBestratioreal, y = Fratioreal,shape=Misspecification, color=HCR)) + 
        geom_path(data=Df2, aes(x = SSBestratioreal, y = Fratioreal, linetype = Misspecification,colour=HCR), size = 1) +
        geom_point(data=Df2, aes(x = SSBestratioreal, y = Fratioreal, shape=Misspecification, color=HCR)) +
        labs(x = 'SSB/SSB MSY',
             y = 'F/F MSY') +
        xlim(0,maxSSB)+
        ylim(0,maxF)+
        scale_color_colorblind()+
        geom_vline(xintercept=0.5, linetype='dotted')+
        theme(text=element_text(size=16),legend.position='none')+
        geom_text_repel(data=subset(df2, Year > 2037 | Year < 2020),aes(x = SSBestratioreal, y = Fratioreal, label = Year))+
        geom_text_repel(data=subset(Df2, Year > 2037 | Year < 2020),aes(x = SSBestratioreal, y = Fratioreal, label = Year))
      
       
     SSBratio<- ggplot()+
        geom_boxplot(data=df3,aes(x=Time, y=SSBratio, fill=HCR, linetype=Misspecification)) +
        geom_boxplot(data=Df3,aes(x=Time, y=SSBratio, fill=HCR, linetype=Misspecification)) +
        theme_classic()+
        ylab('SSB/SSBMSY')+
        xlab('Time')+
        theme(text=element_text(size=18),legend.position='none')+
        scale_fill_colorblind()
     
     Fratio<- ggplot()+
       geom_boxplot(data=df3,aes(x=Time, y=Fratio, fill=HCR, linetype=Misspecification)) +
       geom_boxplot(data=Df3,aes(x=Time, y=Fratio, fill=HCR, linetype=Misspecification)) +
       theme_classic()+
       ylab('F/FMSY')+
       xlab('Time')+
       theme(text=element_text(size=18),legend.position='none')+
       scale_fill_colorblind()
     
      
      
    } else {
      data<-read.csv(here::here('Data/kobeterminal_data_jj.csv'))
      Df<-filter(data, Scenario %in% c(1,2,3,4))
      
      Df<-Df[Df$HCR==hcr,]
      Df$HCR[Df$HCR==1]<-'Ramp'
      Df$HCR[Df$HCR==2]<-'P*'
      Df$HCR[Df$HCR==3]<-'F-step'
      Df$HCR[Df$HCR==4]<-'Constrained ramp'
      Df$HCR<-as.factor(Df$HCR)
      Df$Misspecification[is.na(Df$Misspecification)]<-"Not misspecified"
      Df$Misspecification<-as.factor(Df$Misspecification)
      
      data2<-read.csv(here::here('Data/kobeestimated_data_jj.csv'))
      Df2<-filter(data2, Scenario %in% c(1,2,3,4))
      Df2<-Df2[Df2$HCR==hcr,]
      Df2$HCR[Df2$HCR==1]<-'Ramp'
      Df2$HCR[Df2$HCR==2]<-'P*'
      Df2$HCR[Df2$HCR==3]<-'F-step'
      Df2$HCR[Df2$HCR==4]<-'Constrained ramp'
      Df2$HCR<-as.factor(Df2$HCR)
      Df2$Misspecification[is.na(Df2$Misspecification)]<-"Not misspecified"
      Df2$Misspecification<-as.factor(Df2$Misspecification)
      
      data3<-read.csv(here::here('Data/boxdata2_jj.csv'))
      Df3<-filter(data3, Scenario %in% c(1,2,3,4))
      Df3<-Df3[Df3$HCR==hcr,]
      Df3$HCR[Df3$HCR==1]<-'Ramp'
      Df3$HCR[Df3$HCR==2]<-'P*'
      Df3$HCR[Df3$HCR==3]<-'F-step'
      Df3$HCR[Df3$HCR==4]<-'Constrained ramp'
      Df3$HCR<-as.factor(Df3$HCR)
      Df3$Misspecification[is.na(Df3$Misspecification)]<-"Not misspecified"
      Df3$Misspecification<-as.factor(Df3$Misspecification)
      
      maxSSBest<-max(1.1,max(df$SSBestratioreal, na.rm=TRUE), max(Df$SSBestratioreal, na.rm=TRUE))
      maxF<-max(1.1,max(df$Fratioreal, na.rm=TRUE), max(Df$Fratioreal, na.rm=TRUE))
      
      kobeterm <- ggplot() +
        theme_bw()+
        annotate(geom = "rect", xmin = 1, xmax = maxSSBest, ymin = 0, ymax = 1, fill = "green", colour = "green", alpha = 0.5) +
        annotate(geom = "rect", xmin = 0, xmax = 1, ymin = 1, ymax = maxF, fill = "red", colour = "red", alpha = 0.5) +
        annotate(geom = "rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "yellow", colour = "yellow", alpha = 0.5) +
        annotate(geom = "rect", xmin = 1, xmax = maxSSBest, ymin = 1, ymax = maxF, fill = "yellow", colour = "yellow", alpha = 0.5) +
        geom_path(data=df, aes(x = SSBestratioreal, y = Fratioreal, linetype = Misspecification,colour=HCR), size = 0.3) +
        geom_point(data=df, aes(x = SSBestratioreal, y = Fratioreal,shape=Misspecification, colour=HCR)) + 
        geom_path(data=Df, aes(x = SSBestratioreal, y = Fratioreal, linetype = Misspecification,colour=HCR), size = 0.3) +
        geom_point(data=Df, aes(x = SSBestratioreal, y = Fratioreal, shape=Misspecification, colour=HCR)) + 
        labs(x = 'SSB/SSB MSY',
             y = 'F/F MSY') +
        xlim(0,maxSSBest)+
        ylim(0,maxF)+
        scale_color_colorblind()+
        geom_vline(xintercept=0.5, linetype='dotted')+
        theme(text=element_text(size=16),legend.position='none')+
        geom_text_repel(data=subset(df, Year > 2039 | Year < 2020),aes(x = SSBestratioreal, y = Fratioreal, label = Year))
      
      
      maxSSB<-max(1.1,max(df2$SSBestratioreal, na.rm=TRUE), max(Df2$SSBratioreal, na.rm=TRUE))
      maxF<-max(1.1,max(df2$Fratioreal, na.rm=TRUE), max(Df2$Fratioreal, na.rm=TRUE))
      
      kobeest <- ggplot() +
        theme_bw()+
        annotate(geom = "rect", xmin = 1, xmax = maxSSB, ymin = 0, ymax = 1, fill = "green", colour = "green", alpha = 0.5) +
        annotate(geom = "rect", xmin = 0, xmax = 1, ymin = 1, ymax = maxF, fill = "red", colour = "red", alpha = 0.5) +
        annotate(geom = "rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "yellow", colour = "yellow", alpha = 0.5) +
        annotate(geom = "rect", xmin = 1, xmax = maxSSB, ymin = 1, ymax = maxF, fill = "yellow", colour = "yellow", alpha = 0.5) +
        geom_path(data=df2, aes(x = SSBestratioreal, y = Fratioreal, linetype = Misspecification,colour=HCR), size = 1) +
        geom_point(data=df2, aes(x = SSBestratioreal, y = Fratioreal,shape=Misspecification, color=HCR)) + 
        geom_path(data=Df2, aes(x = SSBestratioreal, y = Fratioreal, linetype = Misspecification,colour=HCR), size = 1) +
        geom_point(data=Df2, aes(x = SSBestratioreal, y = Fratioreal, shape=Misspecification, color=HCR)) +
        labs(x = 'SSB/SSB MSY',
             y = 'F/F MSY') +
        xlim(0,maxSSB)+
        ylim(0,maxF)+
        scale_color_colorblind()+
        geom_vline(xintercept=0.5, linetype='dotted')+
        theme(text=element_text(size=16),legend.position='none')+
        geom_text_repel(data=subset(df2, Year > 2037 | Year < 2020),aes(x = SSBestratioreal, y = Fratioreal, label = Year))+
        geom_text_repel(data=subset(Df2, Year > 2037 | Year < 2020),aes(x = SSBestratioreal, y = Fratioreal, label = Year))
      
      
      SSBratio<- ggplot()+
        geom_boxplot(data=df3,aes(x=Time, y=SSBratio, fill=HCR, linetype=Misspecification)) +
        geom_boxplot(data=Df3,aes(x=Time, y=SSBratio, fill=HCR, linetype=Misspecification)) +
        theme_classic()+
        ylab('SSB/SSBMSY')+
        xlab('Time')+
        theme(text=element_text(size=18),legend.position='none')+
        scale_fill_colorblind()
      
      Fratio<- ggplot()+
        geom_boxplot(data=df3,aes(x=Time, y=Fratio, fill=HCR, linetype=Misspecification)) +
        geom_boxplot(data=Df3,aes(x=Time, y=Fratio, fill=HCR, linetype=Misspecification)) +
        theme_classic()+
        ylab('F/FMSY')+
        xlab('Time')+
        theme(text=element_text(size=18),legend.position='none')+
        scale_fill_colorblind()
      
      
     
      
    }
    
  }else if (comp== "Rho-adjusted and not rho-adjusted"){
    df<-read.csv(here::here('Data/kobeterminal_data_jj.csv'))
    df<-df[df$Compare_Rho==comp,]
    df<-df[df$HCR==hcr,]
    df$HCR[df$HCR==1]<-'Ramp'
    df$HCR[df$HCR==2]<-'P*'
    df$HCR[df$HCR==3]<-'F-step'
    df$HCR[df$HCR==4]<-'Constrained ramp'
    df$HCR<-as.factor(df$HCR)
    
    
    data<-read.csv(here::here('Data/kobeterminal_data_jj.csv'))
    Df<-filter(data, Scenario %in% c(13,14,15,16))
    Df<-Df[Df$HCR==hcr,]
    Df$HCR[Df$HCR==1]<-'Ramp'
    Df$HCR[Df$HCR==2]<-'P*'
    Df$HCR[Df$HCR==3]<-'F-step'
    Df$HCR[Df$HCR==4]<-'Constrained ramp'
    Df$HCR<-as.factor(Df$HCR)
    
    
    df2<-read.csv(here::here('Data/kobeestimated_data_jj.csv'))
    df2<-df2[df2$Compare_Rho==comp,]
    df2<-df2[df2$HCR==hcr,]
    df2$HCR[df2$HCR==1]<-'Ramp'
    df2$HCR[df2$HCR==2]<-'P*'
    df2$HCR[df2$HCR==3]<-'F-step'
    df2$HCR[df2$HCR==4]<-'Constrained ramp'
    df2$HCR<-as.factor(df2$HCR)
    
    
    data2<-read.csv(here::here('Data/kobeestimated_data_jj.csv'))
    Df2<-filter(data2, Scenario %in% c(13,14,15,16))
    Df2<-Df2[Df2$HCR==hcr,]
    Df2$HCR[Df2$HCR==1]<-'Ramp'
    Df2$HCR[Df2$HCR==2]<-'P*'
    Df2$HCR[Df2$HCR==3]<-'F-step'
    Df2$HCR[Df2$HCR==4]<-'Constrained ramp'
    Df2$HCR<-as.factor(Df2$HCR)
    
    
    df3<-read.csv(here::here('Data/boxdata2_jj.csv'))
    df3<-df3[df3$Compare_Rho==comp,]
    df3<-df3[df3$HCR==hcr,]
    df3$HCR[df3$HCR==1]<-'Ramp'
    df3$HCR[df3$HCR==2]<-'P*'
    df3$HCR[df3$HCR==3]<-'F-step'
    df3$HCR[df3$HCR==4]<-'Constrained ramp'
    df3$HCR<-as.factor(df3$HCR)
    
    
    data3<-read.csv(here::here('Data/boxdata2_jj.csv'))
    Df3<-filter(data3, Scenario %in% c(13,14,15,16))
    Df3<-Df3[Df3$HCR==hcr,]
    Df3$HCR[Df3$HCR==1]<-'Ramp'
    Df3$HCR[Df3$HCR==2]<-'P*'
    Df3$HCR[Df3$HCR==3]<-'F-step'
    Df3$HCR[Df3$HCR==4]<-'Constrained ramp'
    Df3$HCR<-as.factor(Df3$HCR)
    
    
    maxSSBest<-max(1.1,max(df$SSBestratioreal, na.rm=TRUE), max(Df$SSBestratioreal, na.rm=TRUE))
    maxF<-max(1.1,max(df$Fratioreal, na.rm=TRUE), max(Df$Fratioreal, na.rm=TRUE))
    
    kobeterm <- ggplot() +
      theme_bw()+
      annotate(geom = "rect", xmin = 1, xmax = maxSSBest, ymin = 0, ymax = 1, fill = "green", colour = "green", alpha = 0.5) +
      annotate(geom = "rect", xmin = 0, xmax = 1, ymin = 1, ymax = maxF, fill = "red", colour = "red", alpha = 0.5) +
      annotate(geom = "rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "yellow", colour = "yellow", alpha = 0.5) +
      annotate(geom = "rect", xmin = 1, xmax = maxSSBest, ymin = 1, ymax = maxF, fill = "yellow", colour = "yellow", alpha = 0.5) +
      geom_path(data=df, aes(x = SSBestratioreal, y = Fratioreal, linetype = Rho,colour=HCR), size = 0.3) +
      geom_point(data=df, aes(x = SSBestratioreal, y = Fratioreal,shape=Rho, colour=HCR)) + 
      geom_path(data=Df, aes(x = SSBestratioreal, y = Fratioreal, linetype = Rho,colour=HCR), size = 0.3) +
      geom_point(data=Df, aes(x = SSBestratioreal, y = Fratioreal, shape=Rho, colour=HCR)) + 
      labs(x = 'SSB/SSB MSY',
           y = 'F/F MSY') +
      xlim(0,maxSSBest)+
      ylim(0,maxF)+
      scale_color_colorblind()+
      geom_vline(xintercept=0.5, linetype='dotted')+
      theme(text=element_text(size=16),legend.position='none')+
      geom_text_repel(data=subset(df, Year > 2039 | Year < 2020),aes(x = SSBestratioreal, y = Fratioreal, label = Year))
    
    
    maxSSB<-max(1.1,max(df2$SSBestratioreal, na.rm=TRUE), max(Df2$SSBratioreal, na.rm=TRUE))
    maxF<-max(1.1,max(df2$Fratioreal, na.rm=TRUE), max(Df2$Fratioreal), na.rm=TRUE)
    
    kobeest <- ggplot() +
      theme_bw()+
      annotate(geom = "rect", xmin = 1, xmax = maxSSB, ymin = 0, ymax = 1, fill = "green", colour = "green", alpha = 0.5) +
      annotate(geom = "rect", xmin = 0, xmax = 1, ymin = 1, ymax = maxF, fill = "red", colour = "red", alpha = 0.5) +
      annotate(geom = "rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "yellow", colour = "yellow", alpha = 0.5) +
      annotate(geom = "rect", xmin = 1, xmax = maxSSB, ymin = 1, ymax = maxF, fill = "yellow", colour = "yellow", alpha = 0.5) +
      geom_path(data=df2, aes(x = SSBestratioreal, y = Fratioreal, linetype = Rho,colour=HCR), size = 1) +
      geom_point(data=df2, aes(x = SSBestratioreal, y = Fratioreal,shape=Rho, color=HCR)) + 
      geom_path(data=Df2, aes(x = SSBestratioreal, y = Fratioreal, linetype = Rho,colour=HCR), size = 1) +
      geom_point(data=Df2, aes(x = SSBestratioreal, y = Fratioreal, shape=Rho, color=HCR)) +
      labs(x = 'SSB/SSB MSY',
           y = 'F/F MSY') +
      xlim(0,maxSSB)+
      ylim(0,maxF)+
      scale_color_colorblind()+
      geom_vline(xintercept=0.5, linetype='dotted')+
      theme(text=element_text(size=16),legend.position='none')+
      geom_text_repel(data=subset(df2, Year > 2037 | Year < 2020),aes(x = SSBestratioreal, y = Fratioreal, label = Year))+
      geom_text_repel(data=subset(Df2, Year > 2037 | Year < 2020),aes(x = SSBestratioreal, y = Fratioreal, label = Year))
    
    
    SSBratio<- ggplot()+
      geom_boxplot(data=df3,aes(x=Time, y=SSBratio, fill=HCR, linetype=Rho)) +
      geom_boxplot(data=Df3,aes(x=Time, y=SSBratio, fill=HCR, linetype=Rho)) +
      theme_classic()+
      ylab('SSB/SSBMSY')+
      xlab('Time')+
      theme(text=element_text(size=18),legend.position='none')+
      scale_fill_colorblind()
    
    Fratio<- ggplot()+
      geom_boxplot(data=df3,aes(x=Time, y=Fratio, fill=HCR, linetype=Rho)) +
      geom_boxplot(data=Df3,aes(x=Time, y=Fratio, fill=HCR, linetype=Rho)) +
      theme_classic()+
      ylab('F/FMSY')+
      xlab('Time')+
      theme(text=element_text(size=18),legend.position='none')+
      scale_fill_colorblind()
    
    
  }else {
    df<-read.csv(here::here('Data/kobeterminal_data_jj.csv'))
    df<-df[df$Compare_Freq==comp,]
    df<-df[df$HCR==hcr,]
    df$HCR[df$HCR==1]<-'Ramp'
    df$HCR[df$HCR==2]<-'P*'
    df$HCR[df$HCR==3]<-'F-step'
    df$HCR[df$HCR==4]<-'Constrained ramp'
    df$HCR<-as.factor(df$HCR)
    
    
    data<-read.csv(here::here('Data/kobeterminal_data_jj.csv'))
    Df<-filter(data, Scenario %in% c(13,14,15,16))
    Df<-Df[Df$HCR==hcr,]
    Df$HCR[Df$HCR==1]<-'Ramp'
    Df$HCR[Df$HCR==2]<-'P*'
    Df$HCR[Df$HCR==3]<-'F-step'
    Df$HCR[Df$HCR==4]<-'Constrained ramp'
    Df$HCR<-as.factor(Df$HCR)
    
    
    
    df2<-read.csv(here::here('Data/kobeestimated_data_jj.csv'))
    df2<-df2[df2$Compare_Freq==comp,]
    df2<-df2[df2$HCR==hcr,]
    df2$HCR[df2$HCR==1]<-'Ramp'
    df2$HCR[df2$HCR==2]<-'P*'
    df2$HCR[df2$HCR==3]<-'F-step'
    df2$HCR[df2$HCR==4]<-'Constrained ramp'
    df2$HCR<-as.factor(df2$HCR)
    
    
    data2<-read.csv(here::here('Data/kobeestimated_data_jj.csv'))
    Df2<-filter(data2, Scenario %in% c(13,14,15,16))
    Df2<-Df2[Df2$HCR==hcr,]
    Df2$HCR[Df2$HCR==1]<-'Ramp'
    Df2$HCR[Df2$HCR==2]<-'P*'
    Df2$HCR[Df2$HCR==3]<-'F-step'
    Df2$HCR[Df2$HCR==4]<-'Constrained ramp'
    Df2$HCR<-as.factor(Df2$HCR)
    
    
    df3<-read.csv(here::here('Data/boxdata2_jj.csv'))
    df3<-df3[df3$Compare_Rho==comp,]
    df3<-df3[df3$HCR==hcr,]
    df3$HCR[df3$HCR==1]<-'Ramp'
    df3$HCR[df3$HCR==2]<-'P*'
    df3$HCR[df3$HCR==3]<-'F-step'
    df3$HCR[df3$HCR==4]<-'Constrained ramp'
    df3$HCR<-as.factor(df3$HCR)
    
    
    data3<-read.csv(here::here('Data/boxdata2_jj.csv'))
    Df3<-filter(data3, Scenario %in% c(13,14,15,16))
    Df3<-Df3[Df3$HCR==hcr,]
    Df3$HCR[Df3$HCR==1]<-'Ramp'
    Df3$HCR[Df3$HCR==2]<-'P*'
    Df3$HCR[Df3$HCR==3]<-'F-step'
    Df3$HCR[Df3$HCR==4]<-'Constrained ramp'
    Df3$HCR<-as.factor(Df3$HCR)
    
    
    maxSSBest<-max(1.1,max(df$SSBestratioreal, na.rm=TRUE), max(Df$SSBestratioreal, na.rm=TRUE))
    maxF<-max(1.1,max(df$Fratioreal, na.rm=TRUE), max(Df$Fratioreal, na.rm=TRUE))
    
    kobeterm <- ggplot() +
      theme_bw()+
      annotate(geom = "rect", xmin = 1, xmax = maxSSBest, ymin = 0, ymax = 1, fill = "green", colour = "green", alpha = 0.5) +
      annotate(geom = "rect", xmin = 0, xmax = 1, ymin = 1, ymax = maxF, fill = "red", colour = "red", alpha = 0.5) +
      annotate(geom = "rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "yellow", colour = "yellow", alpha = 0.5) +
      annotate(geom = "rect", xmin = 1, xmax = maxSSBest, ymin = 1, ymax = maxF, fill = "yellow", colour = "yellow", alpha = 0.5) +
      geom_path(data=df, aes(x = SSBestratioreal, y = Fratioreal, linetype = Frequency,colour=HCR), size = 0.3) +
      geom_point(data=df, aes(x = SSBestratioreal, y = Fratioreal,shape=Frequency, colour=HCR)) + 
      geom_path(data=Df, aes(x = SSBestratioreal, y = Fratioreal, linetype = Frequency,colour=HCR), size = 0.3) +
      geom_point(data=Df, aes(x = SSBestratioreal, y = Fratioreal, shape=Frequency, colour=HCR)) + 
      labs(x = 'SSB/SSB MSY',
           y = 'F/F MSY') +
      xlim(0,maxSSBest)+
      ylim(0,maxF)+
      scale_color_colorblind()+
      geom_vline(xintercept=0.5, linetype='dotted')+
      theme(text=element_text(size=16),legend.position='none')+
      geom_text_repel(data=subset(df, Year > 2039 | Year < 2020),aes(x = SSBestratioreal, y = Fratioreal, label = Year))
    
    
    maxSSB<-max(1.1,max(df2$SSBestratioreal, na.rm=TRUE), max(Df2$SSBestratioreal, na.rm=TRUE))
    maxF<-max(1.1,max(df2$Fratioreal, na.rm=TRUE), max(Df2$Fratioreal, na.rm=TRUE))
    
    kobeest <- ggplot() +
      theme_bw()+
      annotate(geom = "rect", xmin = 1, xmax = maxSSB, ymin = 0, ymax = 1, fill = "green", colour = "green", alpha = 0.5) +
      annotate(geom = "rect", xmin = 0, xmax = 1, ymin = 1, ymax = maxF, fill = "red", colour = "red", alpha = 0.5) +
      annotate(geom = "rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "yellow", colour = "yellow", alpha = 0.5) +
      annotate(geom = "rect", xmin = 1, xmax = maxSSB, ymin = 1, ymax = maxF, fill = "yellow", colour = "yellow", alpha = 0.5) +
      geom_path(data=df2, aes(x = SSBestratioreal, y = Fratioreal, linetype =Frequency,colour=HCR), size = 1) +
      geom_point(data=df2, aes(x = SSBestratioreal, y = Fratioreal,shape=Frequency, color=HCR)) + 
      geom_path(data=Df2, aes(x = SSBestratioreal, y = Fratioreal, linetype = Frequency,colour=HCR), size = 1) +
      geom_point(data=Df2, aes(x = SSBestratioreal, y = Fratioreal, shape=Frequency, color=HCR)) +
      labs(x = 'SSB/SSB MSY',
           y = 'F/F MSY') +
      xlim(0,maxSSB)+
      ylim(0,maxF)+
      scale_color_colorblind()+
      geom_vline(xintercept=0.5, linetype='dotted')+
      theme(text=element_text(size=16),legend.position='none')+
      geom_text_repel(data=subset(df2, Year > 2037 | Year < 2020),aes(x = SSBestratioreal, y = Fratioreal, label = Year),size=10)+
     geom_text_repel(data=subset(Df2, Year > 2037 | Year < 2020),aes(x = SSBestratioreal, y = Fratioreal, label = Year),size=10)
    
    
    SSBratio<- ggplot()+
      geom_boxplot(data=df3,aes(x=Time, y=SSBratio, fill=HCR, linetype=Frequency)) +
      geom_boxplot(data=Df3,aes(x=Time, y=SSBratio, fill=HCR, linetype=Frequency)) +
      theme_classic()+
      ylab('SSB/SSBMSY')+
      xlab('Time')+
      theme(text=element_text(size=18),legend.position='none')+
      scale_fill_colorblind()
    
    Fratio<- ggplot()+
      geom_boxplot(data=df3,aes(x=Time, y=Fratio, fill=HCR, linetype=Frequency)) +
      geom_boxplot(data=Df3,aes(x=Time, y=Fratio, fill=HCR, linetype=Frequency)) +
      theme_classic()+
      ylab('F/FMSY')+
      xlab('Time')+
      theme(text=element_text(size=18),legend.position='none')+
      scale_fill_colorblind()
    

  }   
  kobeest
  
  
  
} 