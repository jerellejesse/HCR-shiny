library('fmsb')
library(matrixStats)
library(dplyr)

#Load data and change to numeric

wd<-setwd("C:/Users/jjesse/Box/HCR_Sims")

setwd(paste(wd,"/Sim_1","/sim",sep=""))
sims <- list.files()
for (k in 1:length(sims)){
  if (file.size(sims[k])==0){
    sims[k]<-NA}
}
sims<-na.omit(sims)
SSB<-matrix(NA,nrow=21,ncol=length(sims))
Catch<-matrix(NA,nrow=21,ncol=length(sims))
F_mort<-matrix(NA,nrow=21,ncol=length(sims))
Fproxy<-matrix(NA,nrow=21,ncol=length(sims))
SSBproxy<-matrix(NA,nrow=21,ncol=length(sims))

for (k in 1:length(sims)){
  load(sims[k])
  SSB[,k]<-omvalGlobal[[1]]$SSB[170:190]
  Catch[,k]<-omvalGlobal[[1]]$sumCW[169:189]
  F_mort[,k]<-omvalGlobal[[1]]$F_full[170:190]
  Fproxy[,k]<-omvalGlobal[[1]]$FPROXYT2[170:190]
  SSBproxy[,k]<-omvalGlobal[[1]]$SSBPROXYT2[170:190]
}

SSBFinal<-rowMedians(SSB[1:5,],na.rm=T)
ShorttermSSB<-median(SSBFinal)
Catchmed<-rowMedians(Catch[1:5,],na.rm=T)
ShorttermCatch<-median(Catchmed,na.rm=T)
Catch<-Catch[1:5,]
catchdiff<-matrix(NA,4,length(Catch[1,]))
Catchstab<-rep(NA,length(Catch[1,]))
for (i in 1:length(Catch[1,])){
  for (j in 1:(length(Catch[,1])-1)){
    catchdiff[j,i]<-(Catch[j+1,i]-Catch[j,i])
  }
  Catchstab[i]<-sqrt((1/(length(Catch[,i])-1))*sum(catchdiff[,i])^2)/((1/(length(Catch[,i])))*sum(Catch[,i]))
}
Catchstab<-1/median(Catchstab,na.rm=T)
Fprop<-F_mort/Fproxy
Fprop<-rowMeans(Fprop[1:5,])
Fprop[Fprop<1]<-1
Fprop[Fprop>1]<-0
Ffreq<-mean(Fprop)
Bprop<-SSB/(SSBproxy*0.5)
Bprop<-rowMeans(Bprop[1:5,])
Bprop[Bprop<1]<-0
Bprop[Bprop>1]<-1
Bfreq<-mean(Bprop)

x<-1
d<-as.data.frame(cbind(x,ShorttermSSB,ShorttermCatch,Catchstab,Ffreq,Bfreq))

for (m in 2:32){
  setwd(paste(wd,"/Sim_",m,"/sim",sep=""))
  
sims <- list.files()
for (k in 1:length(sims)){
  if (file.size(sims[k])==0){
    sims[k]<-NA}
}
sims<-na.omit(sims)
SSB<-matrix(NA,nrow=21,ncol=length(sims))
Catch<-matrix(NA,nrow=21,ncol=length(sims))
F_mort<-matrix(NA,nrow=21,ncol=length(sims))
Fproxy<-matrix(NA,nrow=21,ncol=length(sims))
SSBproxy<-matrix(NA,nrow=21,ncol=length(sims))

for (k in 1:length(sims)){
  load(sims[k])
  SSB[,k]<-omvalGlobal[[1]]$SSB[170:190]
  Catch[,k]<-omvalGlobal[[1]]$sumCW[169:189]
  F_mort[,k]<-omvalGlobal[[1]]$F_full[170:190]
  Fproxy[,k]<-omvalGlobal[[1]]$FPROXYT2[170:190]
  SSBproxy[,k]<-omvalGlobal[[1]]$SSBPROXYT2[170:190]
}

SSBFinal<-rowMedians(SSB[1:5,],na.rm=T)
ShorttermSSB<-median(SSBFinal)
Catchmed<-rowMedians(Catch[1:5,],na.rm=T)
ShorttermCatch<-median(Catchmed,na.rm=T)
Catch<-Catch[1:5,]
catchdiff<-matrix(NA,4,length(Catch[1,]))
Catchstab<-rep(NA,length(Catch[1,]))
for (i in 1:length(Catch[1,])){
  for (j in 1:(length(Catch[,1])-1)){
    catchdiff[j,i]<-(Catch[j+1,i]-Catch[j,i])
  }
  Catchstab[i]<-sqrt((1/(length(Catch[,i])-1))*sum(catchdiff[,i])^2)/((1/(length(Catch[,i])))*sum(Catch[,i]))
}
Catchstab<-1/median(Catchstab,na.rm=T)
Fprop<-F_mort/Fproxy
Fprop<-rowMeans(Fprop[1:5,])
Fprop[Fprop<1]<-1
Fprop[Fprop>1]<-0
Ffreq<-mean(Fprop)
Bprop<-SSB/(SSBproxy*0.5)
Bprop<-rowMeans(Bprop[1:5,])
Bprop[Bprop<1]<-0
Bprop[Bprop>1]<-1
Bfreq<-mean(Bprop)

x<-m
d2<-as.data.frame(cbind(x,ShorttermSSB,ShorttermCatch,Catchstab,Ffreq,Bfreq))

d<-full_join(d,d2)
}

d$ShorttermSSB<-as.numeric(d$ShorttermSSB)
d$ShorttermCatch<-as.numeric(d$ShorttermCatch)
d$Catchstab<-as.numeric(d$Catchstab)
d$Ffreq<-as.numeric(d$Ffreq)
d$Bfreq<-as.numeric(d$Bfreq)
d$ShorttermSSB<-d$ShorttermSSB/max(d$ShorttermSSB)
d$ShorttermCatch<-d$ShorttermCatch/max(d$ShorttermCatch)
d$Catchstab<-d$Catchstab/max(d$Catchstab)
d$Ffreq<-d$Ffreq/max(d$Ffreq +0.001)
d$Bfreq<-d$Bfreq/max(d$Bfreq +0.001)
maxs<-rep(1,5)
mins<-rep(0,5)
#Add maximum and minimum values to dataframe
d<-rbind(mins,d)
d<-rbind(maxs,d)
rownames(d)<-c('max','min',d$x[3:length(d$x)])
d$x<-NULL

