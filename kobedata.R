#kobe data pull
setwd("C:/Users/jjesse/Box/Kerr Lab/Fisheries Science Lab/HCR_Evaluation")#change this accordingly 
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
  Freal[,k]<-omvalGlobal[[1]]$F_full[168:189]
  Fproxy[,k]<-omvalGlobal[[1]]$FPROXYT[169:190]
  SSBreal[,k]<-omvalGlobal[[1]]$SSB[168:189]
  SSBproxy[,k]<-omvalGlobal[[1]]$SSBPROXYT[169:190]
}

Freal<-rowMedians(Freal,na.rm=T)
Fproxy<-rowMedians(Fproxy,na.rm=T)
Fratioreal<-Freal/Fproxy

SSBreal<-rowMedians(SSBreal,na.rm=T)
SSBproxy<-rowMedians(SSBproxy,na.rm=T)
SSBratioreal<-SSBreal/SSBproxy
Year<-2019:2040
df<-as.data.frame(cbind(SSBestratioreal,Fratioreal,Year))
df$Scenario<-1


for (i in 2:32){
  setwd(paste(wd,"/Sim_",i,"/sim",sep=""))
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
    Freal[,k]<-omvalGlobal[[1]]$F_full[168:189]
    Fproxy[,k]<-omvalGlobal[[1]]$FPROXYT[169:190]
    SSBreal[,k]<-omvalGlobal[[1]]$SSB[168:189]
    SSBproxy[,k]<-omvalGlobal[[1]]$SSBPROXYT[169:190]
  }
  
  Freal<-rowMedians(Freal,na.rm=T)
  Fproxy<-rowMedians(Fproxy,na.rm=T)
  Fratioreal<-Freal/Fproxy
  
  SSBreal<-rowMedians(SSBreal,na.rm=T)
  SSBproxy<-rowMedians(SSBproxy,na.rm=T)
  SSBratioreal<-SSBreal/SSBproxy
  Year<-2019:2040
  df2<-as.data.frame(cbind(SSBratioreal,Fratioreal,Year))
  df2$Scenario<-i

  df<-full_join(df,df2)
}


write.csv(df,here("Data/kobetrue.csv"))
