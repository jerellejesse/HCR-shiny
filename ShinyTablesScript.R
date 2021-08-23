library(matrixStats)
library(dplyr)
library(here)

setwd("C:/Users/jjesse/Box/HCR_Sims")
wd<-getwd()

setwd(paste(wd,"/Sim_1/sim",sep=""))

sims <- list.files()

for (k in 1:length(sims)){
  if (file.size(sims[k])==0){
    sims[k]<-NA}
}
sims<-na.omit(sims)

Catchsim<-matrix(NA,nrow=52,ncol=length(sims))

for (k in 1:length(sims)){
  load(sims[k])
  Catchsim[,k]<-omvalGlobal[[1]]$R[136:187]
}

Catchsim<-rowMedians(Catchsim,na.rm=T)
Year<-1986:2037
df<-as.data.frame(cbind(Catchsim,Year))

Catchest<-matrix(NA,nrow=52,ncol=length(sims))

for (k in 1:length(sims)){
  load(sims[k])
  Catchest[,k]<-omvalGlobal[[1]]$Rest[190,1:52]
}

Catchest<-rowMedians(Catchest,na.rm=T)
Catchest<-na.omit(Catchest)

df$Catchest<-Catchest

df$Scenario<-1

for (i in 2:32){
  setwd(paste(wd,"/Sim_",i,"/sim",sep=""))
  
  sims <- list.files()
  
  for (k in 1:length(sims)){
    if (file.size(sims[k])==0){
      sims[k]<-NA}
  }
  sims<-na.omit(sims)
  
  Catchsim<-matrix(NA,nrow=52,ncol=length(sims))
  
  for (k in 1:length(sims)){
    load(sims[k])
    Catchsim[,k]<-omvalGlobal[[1]]$R[136:187]
  }
  
  Catchsim<-rowMedians(Catchsim,na.rm=T)
  Year<-1986:2037
  df2<-as.data.frame(cbind(Catchsim,Year))
  
  Catchest<-matrix(NA,nrow=52,ncol=length(sims))
  
  for (k in 1:length(sims)){
    load(sims[k])
    Catchest[,k]<-omvalGlobal[[1]]$Rest[190,1:52]
  }
  
  Catchest<-rowMedians(Catchest,na.rm=T)
  Catchest<-na.omit(Catchest)
  
  df2$Catchest<-Catchest
  df2$Scenario<-i
  df<-full_join(df,df2)
}

df<-rename(df, R=Catchsim)%>%
 rename(Rest=Catchest)

setwd("C:/Users/jjesse/Box/Jerelle Jesse/MSE/shiny")
write.csv(df,here("Data/R.csv"))


#REE calculations--------------------------------------------
setwd("C:/Users/jjesse/Box/HCR_Sims")
wd<-getwd()

setwd(paste(wd,"/Sim_1/sim",sep=""))
sims <- list.files()

for (k in 1:length(sims)){
  if (file.size(sims[k])==0){
    sims[k]<-NA}
}
sims<-na.omit(sims)

Catchsim<-matrix(NA,nrow=21,ncol=length(sims))

for (k in 1:length(sims)){
  load(sims[k])
  Ftrue<-omvalGlobal[[1]]$F_full[169:190]
  for (i in seq(170,190,1)){
    Fest<-omvalGlobal[[1]]$Fest[i,]
    Fest<-na.omit(Fest)
    Fest<-tail(Fest,1)
    Catchsim[(i-169),k]<-((Fest-Ftrue[i-169])/Ftrue[i-169])*100
  }
}

Catchsim<-rowMedians(Catchsim,na.rm=T)
Year<-seq(2019,2039,1)
df<-as.data.frame(cbind(Catchsim,Year))
df$Scenario<-1


for (j in 2:32){
  setwd(paste(wd,"/Sim_",j,"/sim",sep=""))
  
  sims <- list.files()
  
  for (k in 1:length(sims)){
    if (file.size(sims[k])==0){
      sims[k]<-NA}
  }
  sims<-na.omit(sims)
  
  Catchsim<-matrix(NA,nrow=21,ncol=length(sims))
  
  for (k in 1:length(sims)){
  load(sims[k])
  Ftrue<-omvalGlobal[[1]]$F_full[169:190]
  for (i in seq(170,190,1)){
    Fest<-omvalGlobal[[1]]$Fest[i,]
    Fest<-na.omit(Fest)
    Fest<-tail(Fest,1)
    Catchsim[(i-169),k]<-((Fest-Ftrue[i-169])/Ftrue[i-169])*100
    }
  }
  
  Catchsim<-rowMedians(Catchsim,na.rm=T)
  Year<-2019:2039
  df2<-as.data.frame(cbind(Catchsim,Year))
  df2$Scenario<-j
  
  df<-full_join(df,df2)
}

  
df<-rename(df, REEF=Catchsim)

setwd("C:/Users/jjesse/Box/Jerelle Jesse/MSE/shiny")
write.csv(df,here("Data/REE_F.csv"))

# Mohn's rho----------------------------
setwd("C:/Users/jjesse/Box/HCR_Sims")
wd<-getwd()

setwd(paste(wd,"/Sim_1/sim",sep=""))
sims <- list.files()

for (k in 1:length(sims)){
  if (file.size(sims[k])==0){
    sims[k]<-NA}
}
sims<-na.omit(sims)

Catchsim<-matrix(NA,nrow=56,ncol=length(sims))

for (k in 1:length(sims)){
  load(sims[k])
  Catchsim[,k]<-omvalGlobal[[1]]$Mohns_Rho_F[134:189]
}

Catchsim<-rowMedians(Catchsim,na.rm=T)
Year<-1984:2039
df<-as.data.frame(cbind(Catchsim,Year))
df$Scenario<-1



for (i in 2:32){
  setwd(paste(wd,"/Sim_",i,"/sim",sep=""))
  
  sims <- list.files()
  
  for (k in 1:length(sims)){
    if (file.size(sims[k])==0){
      sims[k]<-NA}
  }
  sims<-na.omit(sims)
  
  Catchsim<-matrix(NA,nrow=56,ncol=length(sims))
  
  for (k in 1:length(sims)){
    load(sims[k])
    Catchsim[,k]<-omvalGlobal[[1]]$Mohns_Rho_F[134:189]
  }
  
  Catchsim<-rowMedians(Catchsim,na.rm=T)
  Year<-1984:2039
  df2<-as.data.frame(cbind(Catchsim,Year))
  
  df2$Scenario<-i
  df<-full_join(df,df2)
}

df<-rename(df, rhoF=Catchsim)

write.csv(df,here("Data/RhoF.csv"))



##### merge together #####
library(here)
library(dplyr)
SSB<-read.csv(here("Data/SSB.csv"))[-1]
F<-read.csv(here("Data/F.csv"))[-1]
R<-read.csv(here("Data/R.csv"))[-1]
Catch<-read.csv(here("Data/Catch.csv"))[-1]
REESSB<-read.csv(here("Data/REE_SSB.csv"))[-1]
REEF<-read.csv(here("Data/REE_F.csv"))[-1]
RhoSSB<-read.csv(here("Data/RhoSSB.csv"))[-1]
RhoF<-read.csv(here("Data/RhoF.csv"))[-1]

all_data<-full_join(SSB,F, by=c("Year","Scenario"))%>%
  full_join(R, by=c("Year", "Scenario"))%>%
  full_join(Catch, by=c("Year", "Scenario"))%>%
  full_join(REESSB, by=c("Year", "Scenario"))%>%
  full_join(REEF, by=c("Year", "Scenario"))%>%
  full_join(RhoSSB, by=c("Year", "Scenario"))%>%
  full_join(RhoF, by=c("Year","Scenario"))

#reorder columns
data<-all_data[, c(2,4,1,3,5,6,7,8,9,10,11,12,13,14)]

#export as csv
write.csv(data,here("Data/shiny_data.csv"))

plot(data$Year[data$Scenario==4], data$F_full[data$Scenario==4])
 
# add scenario descriptions
data<-read.csv(here("Data/shiny_data.csv"))[-1]
scenarios<-read.csv(here("Data/scenarios.csv"))

full_data<-full_join(data,scenarios, by=c("Scenario"))[, c(1,2,15:18,3:14)]
write.csv(full_data, here("Data/shiny_data_jj.csv"))
