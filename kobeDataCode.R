#kobe data pull estimated 
setwd("C:/Users/jjesse/Box/HCR_Sims")#change this accordingly 
wd<-getwd()

####Set up files####
library(matrixStats)
library(dplyr)
library(ggrepel)
library(ggthemes)

setwd(paste(wd,"/Sim_21","/sim",sep=""))

sims <- list.files()

Freal<-matrix(NA,ncol=length(sims),nrow=20)
Fproxy<-matrix(NA,ncol=length(sims),nrow=20)
SSBestreal<-matrix(NA,ncol=length(sims),nrow=20)
SSBestproxy<-matrix(NA,ncol=length(sims),nrow=20)

for (k in 1:length(sims)){
  if (file.size(sims[k])==0){
    sims[k]<-NA}
}
sims<-na.omit(sims)

for (k in 1:length(sims)){
  load(sims[k])
  Freal[,k]<-na.omit(tail(omvalGlobal[[1]]$Fest[length(omvalGlobal[[1]]$sumCW),],22))
  SSBestreal[,k]<-na.omit(tail(omvalGlobal[[1]]$SSBest[length(omvalGlobal[[1]]$sumCW),],22))
  Fproxy[,k]<-omvalGlobal[[1]]$FPROXY[169:(length(omvalGlobal[[1]]$sumCW)-2)]
  SSBestproxy[,k]<-omvalGlobal[[1]]$SSBPROXY[169:(length(omvalGlobal[[1]]$sumCW)-2)]
}

Freal<-rowMedians(Freal,na.rm=T)
Fproxy<-rowMedians(Fproxy,na.rm=T)
Fratioreal<-Freal/Fproxy

SSBestreal<-rowMedians(SSBestreal,na.rm=T)
SSBestproxy<-rowMedians(SSBestproxy,na.rm=T)
SSBestratioreal<-SSBestreal/SSBestproxy
Year<-2019:((length(omvalGlobal[[1]]$sumCW)-169)+2017)
Dftrue<-as.data.frame(cbind(SSBestratioreal,Fratioreal,Year))
Dftrue$Scenario<-21


#df<-full_join(df, Dftrue)





for (i in 2:20){
  setwd(paste(wd,"/Sim_",i,"/sim",sep=""))
  
  sims <- list.files()
  
  for (k in 1:length(sims)){
    if (file.size(sims[k])==0){
      sims[k]<-NA}
  }
  sims<-na.omit(sims)
  
  Freal<-matrix(NA,ncol=length(sims),nrow=20)
  Fproxy<-matrix(NA,ncol=length(sims),nrow=20)
  SSBestreal<-matrix(NA,ncol=length(sims),nrow=20)
  SSBestproxy<-matrix(NA,ncol=length(sims),nrow=20)
  
  for (k in 1:length(sims)){
    load(sims[k])
    Freal[,k]<-na.omit(tail(omvalGlobal[[1]]$Fest[length(omvalGlobal[[1]]$sumCW),],22))
    SSBestreal[,k]<-na.omit(tail(omvalGlobal[[1]]$SSBest[length(omvalGlobal[[1]]$sumCW),],22))
    Fproxy[,k]<-omvalGlobal[[1]]$FPROXY[169:(length(omvalGlobal[[1]]$sumCW)-2)]
    SSBestproxy[,k]<-omvalGlobal[[1]]$SSBPROXY[169:(length(omvalGlobal[[1]]$sumCW)-2)]
  }
  
  Freal<-rowMedians(Freal,na.rm=T)
  Fproxy<-rowMedians(Fproxy,na.rm=T)
  Fratioreal<-Freal/Fproxy
  
  SSBestreal<-rowMedians(SSBestreal,na.rm=T)
  SSBestproxy<-rowMedians(SSBestproxy,na.rm=T)
  SSBestratioreal<-SSBestreal/SSBestproxy
  Year<-2019:((length(omvalGlobal[[1]]$sumCW)-169)+2017)
  Dftrue2<-as.data.frame(cbind(SSBestratioreal,Fratioreal,Year))
  Dftrue2$Scenario<-i
  Dftrue<-dplyr::full_join(Dftrue, Dftrue2)
}


##### save data 
scenarios<-read.csv(here("Data/scenarios.csv"))

#change rho and frequency inputs to make coding shiny easier
scenarios$Rho[scenarios$Rho==2]<-"No rho-adjustment"
scenarios$Rho[scenarios$Rho==1]<-"Rho-adjustment"
scenarios$Frequency[scenarios$Frequency==1]<-"Two year updates"
scenarios$Frequency[scenarios$Frequency==2]<-"Annual updates"

full_data<-full_join(df,scenarios, by=c("Scenario"))



write.csv(full_data, here("Data/kobeestimated_data_jj.csv"))


##### Kobe data terminal
####Set up files####
library(matrixStats)
library(dplyr)
library(ggrepel)
library(ggthemes)

#First simulation
setwd(paste(wd,"/Sim_1","/sim",sep=""))

sims <- list.files()

Freal<-matrix(NA,ncol=length(sims),nrow=22)
Fproxy<-matrix(NA,ncol=length(sims),nrow=22)
SSBestreal<-matrix(NA,ncol=length(sims),nrow=22)
SSBestproxy<-matrix(NA,ncol=length(sims),nrow=22)

for (k in 1:length(sims)){
  if (file.size(sims[k])==0){
    sims[k]<-NA}
}
sims<-na.omit(sims)

for (k in 1:length(sims)){
  load(sims[k])
  for (x in 168:190){
    Freal[(x-168),k]<-tail(na.omit(omvalGlobal[[1]]$Fest[x,]),1)
    SSBestreal[(x-168),k]<-tail(na.omit(omvalGlobal[[1]]$SSBest[x,]),1)
  }
  Fproxy[,k]<-omvalGlobal[[1]]$FPROXY[169:190]
  SSBestproxy[,k]<-omvalGlobal[[1]]$SSBPROXY[169:190]
}

Freal<-rowMedians(Freal,na.rm=T)
Fproxy<-rowMedians(Fproxy,na.rm=T)
Fratioreal<-Freal/Fproxy

SSBestreal<-rowMedians(SSBestreal,na.rm=T)
SSBestproxy<-rowMedians(SSBestproxy,na.rm=T)
SSBestratioreal<-SSBestreal/SSBestproxy
Year<-2019:2040
df<-as.data.frame(cbind(SSBestratioreal,Fratioreal,Year))
df$Scenario<-1

#Other simulations
for (i in 2:32){
  setwd(paste(wd,"/Sim_",i,"/sim",sep=""))
  
  sims <- list.files()
  
  Freal<-matrix(NA,ncol=length(sims),nrow=22)
  Fproxy<-matrix(NA,ncol=length(sims),nrow=22)
  SSBestreal<-matrix(NA,ncol=length(sims),nrow=22)
  SSBestproxy<-matrix(NA,ncol=length(sims),nrow=22)
  
  for (k in 1:length(sims)){
    if (file.size(sims[k])==0){
      sims[k]<-NA}
  }
  sims<-na.omit(sims)
  
  for (k in 1:length(sims)){
    load(sims[k])
    for (x in 168:190){
      Freal[(x-168),k]<-tail(na.omit(omvalGlobal[[1]]$Fest[x,]),1)
      SSBestreal[(x-168),k]<-tail(na.omit(omvalGlobal[[1]]$SSBest[x,]),1)
    }
    Fproxy[,k]<-omvalGlobal[[1]]$FPROXY[169:190]
    SSBestproxy[,k]<-omvalGlobal[[1]]$SSBPROXY[169:190]
  }
  
  Freal<-rowMedians(Freal,na.rm=T)
  Fproxy<-rowMedians(Fproxy,na.rm=T)
  Fratioreal<-Freal/Fproxy
  
  SSBestreal<-rowMedians(SSBestreal,na.rm=T)
  SSBestproxy<-rowMedians(SSBestproxy,na.rm=T)
  SSBestratioreal<-SSBestreal/SSBestproxy
  Year<-2019:2040
  Dftrue2<-as.data.frame(cbind(SSBestratioreal,Fratioreal,Year))
  Dftrue2$Scenario<-i
  df<-full_join(df,Dftrue2)
}

# save data
scenarios<-read.csv(here("Data/scenarios.csv"))

#change rho and frequency inputs to make coding shiny easier
scenarios$Rho[scenarios$Rho==2]<-"No rho-adjustment"
scenarios$Rho[scenarios$Rho==1]<-"Rho-adjustment"
scenarios$Frequency[scenarios$Frequency==1]<-"Two year updates"
scenarios$Frequency[scenarios$Frequency==2]<-"Annual updates"

full_data<-full_join(df,scenarios, by=c("Scenario"))



write.csv(full_data, here("Data/kobeterminal_data_jj.csv"))
