plotCompareCatch <- function(comp,miss,hcr)
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
    df<-read.csv(here::here('Data/boxdata_jj.csv'))%>%
      filter(! is.na(HCR))%>%
      filter(! is.na(Time))
    
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
    
    

    
    if(miss == 4){
      data<-read.csv(here::here('Data/boxdata_jj.csv'))%>%
        filter(! is.na(HCR))%>%
        filter(! is.na(Time))
      Df<-filter(data, Scenario %in% c(25,26,27,28))
      Df<-Df[Df$HCR==hcr,]
      Df$HCR[Df$HCR==1]<-'Ramp'
      Df$HCR[Df$HCR==2]<-'P*'
      Df$HCR[Df$HCR==3]<-'F-step'
      Df$HCR[Df$HCR==4]<-'Constrained ramp'
      Df$HCR<-as.factor(Df$HCR)
      Df$Misspecification[is.na(Df$Misspecification)]<-"Not misspecified"
      Df$Misspecification<-as.factor(Df$Misspecification)
      

      
      catch <- ggplot()+
        geom_boxplot(data=df, aes(x=Time, y=catch, fill=HCR, linetype=Misspecification),alpha=0.3) +
        geom_boxplot(data=Df, aes(x=Time, y=catch, fill=HCR, linetype=Misspecification),alpha=0.3)+
        theme_classic()+
        ylab('Catch (mt)')+
        xlab('Time')+
        theme(text=element_text(size=18),legend.position='none')+
        scale_fill_colorblind()
      
      
    } else {
      data<-read.csv(here::here('Data/boxdata_jj.csv'))%>%
        filter(! is.na(HCR))%>%
        filter(! is.na(Time))
      Df<-filter(data, Scenario %in% c(1,2,3,4))
      Df<-Df[Df$HCR==hcr,]
      Df$HCR[Df$HCR==1]<-'Ramp'
      Df$HCR[Df$HCR==2]<-'P*'
      Df$HCR[Df$HCR==3]<-'F-step'
      Df$HCR[Df$HCR==4]<-'Constrained ramp'
      Df$HCR<-as.factor(Df$HCR)
      Df$Misspecification[is.na(Df$Misspecification)]<-"Not misspecified"
      Df$Misspecification<-as.factor(Df$Misspecification)
      

      
      catch <- ggplot()+
        geom_boxplot(data=df, aes(x=Time, y=catch, fill=HCR, linetype=Misspecification),alpha=0.3) +
        geom_boxplot(data=Df, aes(x=Time, y=catch, fill=HCR, linetype=Misspecification),alpha=0.3)+
        theme_classic()+
        ylab('Catch (mt)')+
        xlab('Time')+
        theme(text=element_text(size=18),legend.position='none')+
        scale_fill_colorblind()
      
     
      
    }
    
  }else if (comp== "Rho-adjusted and not rho-adjusted"){
    df<-read.csv(here::here('Data/boxdata_jj.csv'))%>%
      filter(! is.na(HCR))%>%
      filter(! is.na(Time))
    
    df<-df[df$Compare_Rho==comp,]
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

    
    data<-read.csv(here::here('Data/boxdata_jj.csv'))%>%
      filter(! is.na(HCR))%>%
      filter(! is.na(Time))
    Df<-filter(data, Scenario %in% c(13,14,15,16))
    Df<-Df[Df$HCR==hcr,]
    Df$HCR[Df$HCR==1]<-'Ramp'
    Df$HCR[Df$HCR==2]<-'P*'
    Df$HCR[Df$HCR==3]<-'F-step'
    Df$HCR[Df$HCR==4]<-'Constrained ramp'
    Df$HCR<-as.factor(Df$HCR)
    Df$Misspecification[is.na(Df$Misspecification)]<-"Not misspecified"
    Df$Misspecification<-as.factor(Df$Misspecification)
    
    

    
    catch <- ggplot()+
      geom_boxplot(data=df, aes(x=Time, y=catch, fill=HCR, linetype=Rho),alpha=0.3) +
      geom_boxplot(data=Df, aes(x=Time, y=catch, fill=HCR, linetype=Rho),alpha=0.3)+
      theme_classic()+
      ylab('Catch (mt)')+
      xlab('Time')+
      theme(text=element_text(size=18),legend.position='none')+
      scale_fill_colorblind()
   
    
    
  }else {
    df<-read.csv(here::here('Data/boxdata_jj.csv'))%>%
      filter(!is.na(HCR))%>%
      filter(! is.na(Time))
    df<-df[df$Compare_Freq==comp,]
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
    

    
    data<-read.csv(here::here('Data/boxdata_jj.csv'))%>%
      filter(! is.na(HCR))%>%
      filter(! is.na(Time))
    Df<-filter(data, Scenario %in% c(13,14,15,16))
    Df<-Df[Df$HCR==hcr,]
    Df$HCR[Df$HCR==1]<-'Ramp'
    Df$HCR[Df$HCR==2]<-'P*'
    Df$HCR[Df$HCR==3]<-'F-step'
    Df$HCR[Df$HCR==4]<-'Constrained ramp'
    Df$HCR<-as.factor(Df$HCR)
    Df$Misspecification[is.na(Df$Misspecification)]<-"Not misspecified"
    Df$Misspecification<-as.factor(Df$Misspecification)

    
    catch <- ggplot()+
      geom_boxplot(data=df, aes(x=Time, y=catch, fill=HCR, linetype=Frequency),alpha=0.3) +
      geom_boxplot(data=Df, aes(x=Time, y=catch, fill=HCR, linetype=Frequency),alpha=0.3)+
      theme_classic()+
      ylab('Catch (mt)')+
      xlab('Time')+
      theme(text=element_text(size=18),legend.position='none')+
      scale_fill_colorblind()
    
  }   
 catch
  
  
  
  
}