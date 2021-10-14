####Box Plot Data####
####Set directory####
require(dplyr)
require(matrixStats)
setwd("C:/Users/jjesse/Box/HCR_Sims")#change this accordingly 
wd<-getwd()

####Catch Boxplot####
setwd(paste(wd,"/Sim_1/sim",sep=""))
sims <- list.files()
for (k in 1:length(sims)){
  if (file.size(sims[k])==0){
    sims[k]<-NA}
}
sims<-na.omit(sims)
catch<-matrix(NA,nrow=length(sims),ncol=22)

for (k in 1:length(sims)){
  load(sims[k])
  catch[k,]<-omvalGlobal[[1]]$sumCW[169:190]
}

catchs<-rowMedians(catch[,1:5])
Df<-as.data.frame(catchs)
Df$catch<-catchs
Df$catchs<-NULL
Df$Time<-'Short-term'

catchm<-rowMedians(catch[,6:10])
Df2<-as.data.frame(catchm)
Df2$catch<-catchm
Df2$catchm<-NULL
Df2$Time<-'Medium-term'
Df<-full_join(Df,Df2)

catchl<-rowMedians(catch[,11:21])
Df2<-as.data.frame(catchl)
Df2$catch<-catchl
Df2$catchl<-NULL
Df2$Time<-'Long-term'
Df<-full_join(Df,Df2)
Df$Scenario<-1


for (i in 2:32){
  setwd(paste(wd,"/Sim_",i,"/sim",sep=""))

  sims <- list.files()

  for (k in 1:length(sims)){
    if (file.size(sims[k])==0){
      sims[k]<-NA}
  }
  sims<-na.omit(sims)
  catch<-matrix(NA,nrow=length(sims),ncol=22)
  
  for (k in 1:length(sims)){
    load(sims[k])
    catch[k,]<-omvalGlobal[[1]]$sumCW[169:190]
  }
  
catchs<-rowMedians(catch[,1:5])
Df2<-as.data.frame(catchs)
Df2$catch<-catchs
Df2$catchs<-NULL
Df2$Time<-'Short-term'
  
catchm<-rowMedians(catch[,6:10])
Df3<-as.data.frame(catchm)
Df3$catch<-catchm
Df3$catchm<-NULL
Df3$Time<-'Medium-term'
Df2<-full_join(Df2,Df3)
  
catchl<-rowMedians(catch[,11:21])
Df3<-as.data.frame(catchl)
Df3$catch<-catchl
Df3$catchl<-NULL
Df3$Time<-'Long-term'
Df2<-full_join(Df2,Df3)
Df2$Scenario<-i
Df<-full_join(Df,Df2)
}

write.csv(Df,here("Data/Catch_box.csv"))



##### estimated/true SSBMSY boxplot ####
setwd("C:/Users/jjesse/Box/HCR_Sims")#change this accordingly 
wd<-getwd()

setwd(paste(wd,"/Sim_1/sim",sep=""))
sims <- list.files()
for (k in 1:length(sims)){
  if (file.size(sims[k])==0){
    sims[k]<-NA}
}
sims<-na.omit(sims)

Fratiot<-matrix(NA,nrow=length(sims),ncol=22)

for (k in 1:length(sims)){
  load(sims[k])
  Fratiot[k,]<-omvalGlobal[[1]]$SSBPROXY[169:190]/omvalGlobal[[1]]$SSBPROXYT2[169:190]
}

Fratiots<-rowMedians(Fratiot[,1:5])
Df<-as.data.frame(Fratiots)
Df$Fratiot<-Fratiots
Df$Fratiots<-NULL
Df$Time<-'Short-term'

Fratiotm<-rowMedians(Fratiot[,6:10])
Df2<-as.data.frame(Fratiotm)
Df2$Fratiot<-Fratiotm
Df2$Fratiotm<-NULL
Df2$Time<-'Medium-term'
Df<-full_join(Df,Df2)

Fratiotl<-rowMedians(Fratiot[,11:21])
Df2<-as.data.frame(Fratiotl)
Df2$Fratiot<-Fratiotl
Df2$Fratiotl<-NULL
Df2$Time<-'Long-term'
Df<-full_join(Df,Df2)
Df$Scenario<-1


for (i in 2:32){
  setwd(paste(wd,"/Sim_",i,"/sim",sep=""))
  
  sims <- list.files()
  
  for (k in 1:length(sims)){
    if (file.size(sims[k])==0){
      sims[k]<-NA}
  }
  sims<-na.omit(sims)
  
  Fratiot<-matrix(NA,nrow=length(sims),ncol=22)
  
  for (k in 1:length(sims)){
    load(sims[k])
    Fratiot[k,]<-omvalGlobal[[1]]$SSBPROXY[169:190]/omvalGlobal[[1]]$SSBPROXYT2[169:190]
  }
  
Fratiots<-rowMedians(Fratiot[,1:5])
Df2<-as.data.frame(Fratiots)
Df2$Fratiot<-Fratiots
Df2$Fratiots<-NULL
Df2$Time<-'Short-term'
  
Fratiotm<-rowMedians(Fratiot[,6:10])
Df3<-as.data.frame(Fratiotm)
Df3$Fratiot<-Fratiotm
Df3$Fratiotm<-NULL
Df3$Time<-'Medium-term'
Df2<-full_join(Df2,Df3)
  
Fratiotl<-rowMedians(Fratiot[,11:21])
Df3<-as.data.frame(Fratiotl)
Df3$Fratiot<-Fratiotl
Df3$Fratiotl<-NULL
Df3$Time<-'Long-term'
Df2<-full_join(Df2,Df3)

Df2$Scenario<-i
Df<-full_join(Df,Df2)
}

Df<-rename(Df, SSBratiot=Fratiot)
write.csv(Df,here("Data/SSBMSY_box.csv"))


##### estimated/true FMSY boxplot #####
setwd("C:/Users/jjesse/Box/HCR_Sims")#change this accordingly 
wd<-getwd()

setwd(paste(wd,"/Sim_1/sim",sep=""))
sims <- list.files()
for (k in 1:length(sims)){
  if (file.size(sims[k])==0){
    sims[k]<-NA}
}
sims<-na.omit(sims)

Fratiot<-matrix(NA,nrow=length(sims),ncol=22)

for (k in 1:length(sims)){
  load(sims[k])
  Fratiot[k,]<-omvalGlobal[[1]]$FPROXY[169:190]/omvalGlobal[[1]]$FPROXYT2[169:190]
}

Fratiots<-rowMedians(Fratiot[,1:5])
Df<-as.data.frame(Fratiots)
Df$Fratiot<-Fratiots
Df$Fratiots<-NULL
Df$Time<-'Short-term'

Fratiotm<-rowMedians(Fratiot[,6:10])
Df2<-as.data.frame(Fratiotm)
Df2$Fratiot<-Fratiotm
Df2$Fratiotm<-NULL
Df2$Time<-'Medium-term'
Df<-full_join(Df,Df2)

Fratiotl<-rowMedians(Fratiot[,11:21])
Df2<-as.data.frame(Fratiotl)
Df2$Fratiot<-Fratiotl
Df2$Fratiotl<-NULL
Df2$Time<-'Long-term'
Df<-full_join(Df,Df2)
Df$Scenario<-1


for (i in 2:32){
  setwd(paste(wd,"/Sim_",i,"/sim",sep=""))
  
  sims <- list.files()
  
  for (k in 1:length(sims)){
    if (file.size(sims[k])==0){
      sims[k]<-NA}
  }
  sims<-na.omit(sims)
  
  Fratiot<-matrix(NA,nrow=length(sims),ncol=22)
  
  for (k in 1:length(sims)){
    load(sims[k])
    Fratiot[k,]<-omvalGlobal[[1]]$FPROXY[169:190]/omvalGlobal[[1]]$FPROXYT2[169:190]
  }
  
  Fratiots<-rowMedians(Fratiot[,1:5])
  Df2<-as.data.frame(Fratiots)
  Df2$Fratiot<-Fratiots
  Df2$Fratiots<-NULL
  Df2$Time<-'Short-term'
  
  Fratiotm<-rowMedians(Fratiot[,6:10])
  Df3<-as.data.frame(Fratiotm)
  Df3$Fratiot<-Fratiotm
  Df3$Fratiotm<-NULL
  Df3$Time<-'Medium-term'
  Df2<-full_join(Df2,Df3)
  
  Fratiotl<-rowMedians(Fratiot[,11:21])
  Df3<-as.data.frame(Fratiotl)
  Df3$Fratiot<-Fratiotl
  Df3$Fratiotl<-NULL
  Df3$Time<-'Long-term'
  Df2<-full_join(Df2,Df3)
  
  Df2$Scenario<-i
  Df<-full_join(Df,Df2)
}

write.csv(Df,here("Data/FMSY_box.csv"))


##### merge code #####
boxdata<-read.csv(here("Data/Catch_box.csv"))%>%
  full_join(read.csv(here("Data/SSBMSY_box.csv")))%>%
  full_join(read.csv(here("Data/FMSY_box.csv")))

#bring in scenario info
scenarios<-read.csv(here("Data/scenarios.csv"))

#change rho and frequency inputs to make coding shiny easier
scenarios$Rho[scenarios$Rho==2]<-"No rho-adjustment"
scenarios$Rho[scenarios$Rho==1]<-"Rho-adjustment"
scenarios$Frequency[scenarios$Frequency==1]<-"Two year updates"
scenarios$Frequency[scenarios$Frequency==2]<-"Annual updates"

full_data<-full_join(boxdata,scenarios, by=c("Scenario"))

write.csv(full_data, here("Data/boxdata_jj.csv"))  
