#kobe data pull
setwd("C:/Users/jjesse/Box/HCR_Sims")#change this accordingly 
wd<-getwd()

####Set up files####
library(matrixStats)
library(dplyr)
library(ggrepel)
library(ggthemes)
setwd(paste(wd,"/Sim_1","/sim",sep=""))

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
Dftrue$Scenario<-1

for (m in 2:32){
  setwd(paste(wd,"/Sim_",m,"/sim",sep=""))
  
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
  Dftrue2<-as.data.frame(cbind(SSBestratioreal,Fratioreal,Year))
  Dftrue2$Scenario<-m
  df<-full_join(df,Dftrue2)
}




