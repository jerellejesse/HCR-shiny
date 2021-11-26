##### short term plot#####

library('fmsb')
library(matrixStats)
library(dplyr)
library(here)

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

short<-d


##### Medium term plot#####
library('fmsb')
library(matrixStats)
library(dplyr)

#Load data and change to numeric
wd<-getwd()
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

SSBFinal<-rowMedians(SSB[6:10,],na.rm=T)
ShorttermSSB<-median(SSBFinal)
Catchmed<-rowMedians(Catch[6:10,],na.rm=T)
ShorttermCatch<-median(Catchmed,na.rm=T)
Catch<-Catch[6:10,]
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
Fprop<-rowMeans(Fprop[6:10,])
Fprop[Fprop<1]<-1
Fprop[Fprop>1]<-0
Ffreq<-mean(Fprop)
Bprop<-SSB/(SSBproxy*0.5)
Bprop<-rowMeans(Bprop[6:10,])
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

SSBFinal<-rowMedians(SSB[6:10,],na.rm=T)
ShorttermSSB<-median(SSBFinal)
Catchmed<-rowMedians(Catch[6:10,],na.rm=T)
ShorttermCatch<-median(Catchmed,na.rm=T)
Catch<-Catch[6:10,]
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
Fprop<-rowMeans(Fprop[6:10,])
Fprop[Fprop<1]<-1
Fprop[Fprop>1]<-0
Ffreq<-mean(Fprop)
Bprop<-SSB/(SSBproxy*0.5)
Bprop<-rowMeans(Bprop[6:10,])
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

medium <-d
medium_data<-rename(medium, MediumtermSSB = ShorttermSSB)%>%
rename(MediumtermCatch = ShorttermCatch)%>%
  rename(MediumCatchstab = Catchstab)%>%
  rename(MediumFfreq = Ffreq)%>%
  rename(MediumBfreq = Bfreq)

##### Long term plot #####
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
SSB<-matrix(NA,nrow=22,ncol=length(sims))
Catch<-matrix(NA,nrow=22,ncol=length(sims))
F_mort<-matrix(NA,nrow=22,ncol=length(sims))
Fproxy<-matrix(NA,nrow=22,ncol=length(sims))
SSBproxy<-matrix(NA,nrow=22,ncol=length(sims))

for (k in 1:length(sims)){
  load(sims[k])
  SSB[,k]<-omvalGlobal[[1]]$SSB[169:190]
  Catch[,k]<-omvalGlobal[[1]]$sumCW[168:189]
  F_mort[,k]<-omvalGlobal[[1]]$F_full[169:190]
  Fproxy[,k]<-omvalGlobal[[1]]$FPROXYT2[169:190]
  SSBproxy[,k]<-omvalGlobal[[1]]$SSBPROXYT2[169:190]
}

SSBFinal<-rowMedians(SSB[11:21,],na.rm=T)
ShorttermSSB<-median(SSBFinal)
Catchmed<-rowMedians(Catch[11:21,],na.rm=T)
ShorttermCatch<-median(Catchmed,na.rm=T)
Catch<-Catch[11:21,]
catchdiff<-matrix(NA,10,length(Catch[1,]))
Catchstab<-rep(NA,length(Catch[1,]))
for (i in 1:length(Catch[1,])){
  for (j in 1:(length(Catch[,1])-1)){
    catchdiff[j,i]<-(Catch[j+1,i]-Catch[j,i])
  }
  Catchstab[i]<-sqrt((1/(length(Catch[,i])-1))*sum(catchdiff[,i])^2)/((1/(length(Catch[,i])))*sum(Catch[,i]))
}
Catchstab<-1/median(Catchstab)
Fprop<-F_mort/Fproxy
Fprop<-rowMedians(Fprop[11:21,])
Fprop[Fprop<1]<-1
Fprop[Fprop>1]<-0
Ffreq<-mean(Fprop)
Bprop<-SSB/(SSBproxy*0.5)
Bprop<-rowMedians(Bprop[11:21,])
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
SSB<-matrix(NA,nrow=22,ncol=length(sims))
Catch<-matrix(NA,nrow=22,ncol=length(sims))
F_mort<-matrix(NA,nrow=22,ncol=length(sims))
Fproxy<-matrix(NA,nrow=22,ncol=length(sims))
SSBproxy<-matrix(NA,nrow=22,ncol=length(sims))

for (k in 1:length(sims)){
  load(sims[k])
  SSB[,k]<-omvalGlobal[[1]]$SSB[169:190]
  Catch[,k]<-omvalGlobal[[1]]$sumCW[168:189]
  F_mort[,k]<-omvalGlobal[[1]]$F_full[169:190]
  Fproxy[,k]<-omvalGlobal[[1]]$FPROXYT2[169:190]
  SSBproxy[,k]<-omvalGlobal[[1]]$SSBPROXYT2[169:190]
}

SSBFinal<-rowMedians(SSB[11:21,],na.rm=T)
ShorttermSSB<-median(SSBFinal)
Catchmed<-rowMedians(Catch[11:21,],na.rm=T)
ShorttermCatch<-median(Catchmed,na.rm=T)
Catch<-Catch[11:21,]
catchdiff<-matrix(NA,10,length(Catch[1,]))
Catchstab<-rep(NA,length(Catch[1,]))
for (i in 1:length(Catch[1,])){
  for (j in 1:(length(Catch[,1])-1)){
    catchdiff[j,i]<-(Catch[j+1,i]-Catch[j,i])
  }
  Catchstab[i]<-sqrt((1/(length(Catch[,i])-1))*sum(catchdiff[,i])^2)/((1/(length(Catch[,i])))*sum(Catch[,i]))
}
Catchstab<-1/median(Catchstab)
Fprop<-F_mort/Fproxy
Fprop<-rowMedians(Fprop[11:21,])
Fprop[Fprop<1]<-1
Fprop[Fprop>1]<-0
Ffreq<-mean(Fprop)
Bprop<-SSB/(SSBproxy*0.5)
Bprop<-rowMedians(Bprop[11:21,])
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

long<-d
long_data<-rename(long, LongtermSSB = ShorttermSSB)%>%
rename(LongtermCatch = ShorttermCatch)%>%
rename(LongCatchstab = Catchstab)%>%
  rename(LongFfreq = Ffreq)%>%
  rename(LongBfreq = Bfreq)

##### bind all together #####

radar_data<-full_join(short[-c(1,2),], medium_data[-c(1,2),], by="x")%>%
full_join(long_data[-c(1,2),], by="x")%>%
  rename(Scenario= x)


#save data
scenarios<-read.csv(here("Data/scenarios.csv"))

#change rho and frequency inputs to make coding shiny easier
scenarios$Rho[scenarios$Rho==2]<-"No rho-adjustment"
scenarios$Rho[scenarios$Rho==1]<-"Rho-adjustment"
scenarios$Frequency[scenarios$Frequency==1]<-"Two year updates"
scenarios$Frequency[scenarios$Frequency==2]<-"Annual updates"

full_data<-full_join(radar_data,scenarios, by=c("Scenario"))

maxs<-rep(1,16)
mins<-rep(0,16)
#Add maximum and minimum values to dataframe
full_data<-rbind(mins,full_data)
full_data<-rbind(maxs,full_data)
full_data$Scenario<-as.numeric(full_data$Scenario)

write.csv(full_data, here("Data/radar_data_jj.csv"))
