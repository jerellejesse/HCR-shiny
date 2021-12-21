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

Freal<-matrix(NA,ncol=length(sims),nrow=22)
Fproxy<-matrix(NA,ncol=length(sims),nrow=22)
SSBreal<-matrix(NA,ncol=length(sims),nrow=22)
SSBproxy<-matrix(NA,ncol=length(sims),nrow=22)

for (k in 1:length(sims)){
  if (file.size(sims[k])==0){
    sims[k]<-NA}
}
sims<-na.omit(sims)

for (k in 1:length(sims)){
  load(sims[k])
  Freal[,k]<-omvalGlobal[[1]]$F_full[168:(length(omvalGlobal[[1]]$sumCW)-2)]
  SSBreal[,k]<-omvalGlobal[[1]]$SSB[168:(length(omvalGlobal[[1]]$sumCW)-2)]
  Fproxy[,k]<-omvalGlobal[[1]]$FPROXYT[169:(length(omvalGlobal[[1]]$sumCW)-2)]
  SSBproxy[,k]<-omvalGlobal[[1]]$SSBPROXYT[169:(length(omvalGlobal[[1]]$sumCW)-2)]
}

Freal<-rowMedians(Freal,na.rm=T)
Fproxy<-rowMedians(Fproxy,na.rm=T)
Fratioreal<-Freal/Fproxy

SSBreal<-rowMedians(SSBreal,na.rm=T)
SSBproxy<-rowMedians(SSBproxy,na.rm=T)
SSBratioreal<-SSBreal/SSBproxy
Year<-2019:((length(omvalGlobal[[1]]$sumCW)-169)+2017)
Dftrue<-as.data.frame(cbind(SSBratioreal,Fratioreal,Year))
Dftrue$Scenario<-1
df<-Dftrue

for (m in 2:32){
  setwd(paste(wd,"/Sim_",m,"/sim",sep=""))
  
  sims <- list.files()
  
  Freal<-matrix(NA,ncol=length(sims),nrow=20)
  Fproxy<-matrix(NA,ncol=length(sims),nrow=20)
  SSBreal<-matrix(NA,ncol=length(sims),nrow=20)
  SSBproxy<-matrix(NA,ncol=length(sims),nrow=20)
  
  if (m>20 & m<25){
    Freal<-matrix(NA,ncol=length(sims),nrow=21)
    Fproxy<-matrix(NA,ncol=length(sims),nrow=21)
    SSBreal<-matrix(NA,ncol=length(sims),nrow=21)
    SSBproxy<-matrix(NA,ncol=length(sims),nrow=21)
  }
  
  for (k in 1:length(sims)){
    if (file.size(sims[k])==0){
      sims[k]<-NA}
  }
  sims<-na.omit(sims)
  
  if (m>20 & m<25){
    for (k in 1:length(sims)){
      tryCatch({
        load(sims[k])
        Freal[,k]<-na.omit(tail(omvalGlobal[[1]]$F_full[length(omvalGlobal[[1]]$sumCW),],22))
        SSBreal[,k]<-na.omit(tail(omvalGlobal[[1]]$SSB[length(omvalGlobal[[1]]$sumCW),],22))
        Fproxy[,k]<-omvalGlobal[[1]]$FPROXYT[169:(length(omvalGlobal[[1]]$sumCW)-1)]
        SSBproxy[,k]<-omvalGlobal[[1]]$SSBPROXYT[169:(length(omvalGlobal[[1]]$sumCW)-1)]
      },error=function(e){})
    }
  }
  else {
    for (k in 1:length(sims)){
      tryCatch({
        load(sims[k])
        Freal[,k]<-na.omit(tail(omvalGlobal[[1]]$F_full[length(omvalGlobal[[1]]$sumCW),],22))
        SSBreal[,k]<-na.omit(tail(omvalGlobal[[1]]$SSB[length(omvalGlobal[[1]]$sumCW),],22))
        Fproxy[,k]<-omvalGlobal[[1]]$FPROXYT[169:(length(omvalGlobal[[1]]$sumCW)-2)]
        SSBproxy[,k]<-omvalGlobal[[1]]$SSBPROXYT[169:(length(omvalGlobal[[1]]$sumCW)-2)]
      },error=function(e){})
    }
  }
  
  Freal<-rowMedians(Freal,na.rm=T)
  Fproxy<-rowMedians(Fproxy,na.rm=T)
  Fratioreal<-Freal/Fproxy
  
  SSBreal<-rowMedians(SSBreal,na.rm=T)
  SSBproxy<-rowMedians(SSBproxy,na.rm=T)
  SSBratioreal<-SSBreal/SSBproxy
  Year<-2019:((length(omvalGlobal[[1]]$sumCW)-169)+2017)
  Dftrue2<-as.data.frame(cbind(SSBratioreal,Fratioreal,Year))
  Dftrue2$Scenario<-m
  df<-full_join(df,Dftrue2)
}


write.csv(df,here("Data/kobetrue_data_jj.csv"))

