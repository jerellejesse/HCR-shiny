library(matrixStats)
library(dplyr)
library(here)

setwd("C:/Users/mmazur/Box/HCR_Sims")
wd<-getwd()

setwd(paste(wd,"/Sim_1/sim",sep=""))

sims <- list.files()

for (k in 1:length(sims)){
  if (file.size(sims[k])==0){
    sims[k]<-NA}
}
sims<-na.omit(sims)

Catchsim<-matrix(NA,nrow=55,ncol=length(sims))

for (k in 1:length(sims)){
  load(sims[k])
  Catchsim[,k]<-omvalGlobal[[1]]$sumCW[136:190]
}

Catchsim<-rowMedians(Catchsim,na.rm=T)
Year<-1986:2040
df<-as.data.frame(cbind(Catchsim,Year))

Catchest<-matrix(NA,nrow=55,ncol=length(sims))

for (k in 1:length(sims)){
  load(sims[k])
  Catchest[,k]<-omvalGlobal[[1]]$Catchest[190,1:55]
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

  Catchsim<-matrix(NA,nrow=55,ncol=length(sims))

  for (k in 1:length(sims)){
    load(sims[k])
    Catchsim[,k]<-omvalGlobal[[1]]$R[136:190]
  }

Catchsim<-rowMedians(Catchsim,na.rm=T)
Year<-1986:2040
df2<-as.data.frame(cbind(Catchsim,Year))

Catchest<-matrix(NA,nrow=55,ncol=length(sims))

for (k in 1:length(sims)){
  load(sims[k])
  Catchest[,k]<-omvalGlobal[[1]]$Rest[190,1:55]
}

Catchest<-rowMedians(Catchest,na.rm=T)
Catchest<-na.omit(Catchest)

df2$Catchest<-Catchest
df2$Scenario<-i
df<-full_join(df,df2)
}

df<-rename(df, Catchsim=Catchsim)%>%
 rename(Catchest=Catchest)

setwd("C:/Users/mmazur/Box/Jerelle Jesse/MSE/shiny")
write.csv(df,here("Data/Catch.csv"))


#REE calculations--------------------------------------------
setwd("C:/Users/mmazur/Box/Mackenzie_Mazur/HCR_Sims")
wd<-getwd()

setwd(paste(wd,"/Sim_1/sim",sep=""))
sims <- list.files()

for (k in 1:length(sims)){
  if (file.size(sims[k])==0){
    sims[k]<-NA}
}
sims<-na.omit(sims)

Catchsim<-matrix(NA,nrow=11,ncol=length(sims))

for (k in 1:length(sims)){
  load(sims[k])
  SSBtrue<-omvalGlobal[[1]]$SSB[168:188]
  for (i in seq(170,190,2)){
    SSBest<-omvalGlobal[[1]]$SSBest[i,]
    SSBest<-na.omit(SSBest)
    SSBest<-tail(SSBest,1)
    Catchsim[((i-168)/2),k]<-((SSBest-SSBtrue[i-169])/SSBtrue[i-169])*100
  }
}
Catchsim<-rowMedians(Catchsim,na.rm=T)
Year<-seq(2019,2039,2)
df<-as.data.frame(cbind(Catchsim,Year))
df$Scenario<-1


for (j in 2:20){
  setwd(paste(wd,"/Sim_",j,"/sim",sep=""))

  sims <- list.files()

  for (k in 1:length(sims)){
    if (file.size(sims[k])==0){
      sims[k]<-NA}
  }
  sims<-na.omit(sims)

  Catchsim<-matrix(NA,nrow=11,ncol=length(sims))

  for (k in 1:length(sims)){
    load(sims[k])
    SSBtrue<-omvalGlobal[[1]]$SSB[168:188]
    for (i in seq(170,190,2)){
      SSBest<-omvalGlobal[[1]]$SSBest[i,]
      SSBest<-na.omit(SSBest)
      SSBest<-tail(SSBest,1)
      Catchsim[((i-168)/2),k]<-((SSBest-SSBtrue[i-169])/SSBtrue[i-169])*100
    }
  }

  Catchsim<-rowMedians(Catchsim,na.rm=T)
  Year<-seq(2019,2039,2)
  df2<-as.data.frame(cbind(Catchsim,Year))
  df2$Scenario<-j

  df<-dplyr::full_join(df,df2)
}

for (j in 21:24){
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
    SSBtrue<-omvalGlobal[[1]]$SSB[169:190]
    for (i in seq(170,190,1)){
      SSBest<-omvalGlobal[[1]]$SSBest[i,]
      SSBest<-na.omit(SSBest)
      SSBest<-tail(SSBest,1)
      Catchsim[(i-169),k]<-((SSBest-SSBtrue[i-169])/SSBtrue[i-169])*100
    }
  }
  
  Catchsim<-rowMedians(Catchsim,na.rm=T)
  Year<-2019:2039
  df2<-as.data.frame(cbind(Catchsim,Year))
  df2$Scenario<-j
  
  df<-dplyr::full_join(df,df2)
}

for (j in 25:32){
  setwd(paste(wd,"/Sim_",j,"/sim",sep=""))
  
  sims <- list.files()
  
  for (k in 1:length(sims)){
    if (file.size(sims[k])==0){
      sims[k]<-NA}
  }
  sims<-na.omit(sims)
  
  Catchsim<-matrix(NA,nrow=11,ncol=length(sims))
  
  for (k in 1:length(sims)){
    load(sims[k])
    SSBtrue<-omvalGlobal[[1]]$SSB[168:188]
    for (i in seq(170,190,2)){
      SSBest<-omvalGlobal[[1]]$SSBest[i,]
      SSBest<-na.omit(SSBest)
      SSBest<-tail(SSBest,1)
      Catchsim[((i-168)/2),k]<-((SSBest-SSBtrue[i-169])/SSBtrue[i-169])*100
    }
  }
  
  Catchsim<-rowMedians(Catchsim,na.rm=T)
  Year<-seq(2019,2039,2)
  df2<-as.data.frame(cbind(Catchsim,Year))
  df2$Scenario<-j
  
  df<-dplyr::full_join(df,df2)
}

df<-rename(df, REESSB=Catchsim)

setwd("C:/Users/mmazur/Box/Jerelle Jesse/MSE/shiny")
write.csv(df,here("Data/REE_SSB22_32.csv"))

# Mohn's rho----------------------------
setwd("C:/Users/mmazur/Box/HCR_Sims")
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
  Catchsim[,k]<-omvalGlobal[[1]]$Mohns_Rho_SSB[134:189]
}

Catchsim<-rowMedians(Catchsim,na.rm=T)
Year<-1984:2039
df<-as.data.frame(cbind(Catchsim,Year))
df$Scenario<-1



# for (i in 2:32){
#   setwd(paste(wd,"/Sim_",i,"/sim",sep=""))
#   
#   sims <- list.files()
#   
#   for (k in 1:length(sims)){
#     if (file.size(sims[k])==0){
#       sims[k]<-NA}
#   }
#   sims<-na.omit(sims)
#   
#   Catchsim<-matrix(NA,nrow=56,ncol=length(sims))
#   
#   for (k in 1:length(sims)){
#     load(sims[k])
#     Catchsim[,k]<-omvalGlobal[[1]]$Mohns_Rho_F[134:189]
#   }
#   
#   Catchsim<-rowMedians(Catchsim,na.rm=T)
#   Year<-1984:2039
#   df2<-as.data.frame(cbind(Catchsim,Year))
#   
#   df2$Scenario<-i
#   df<-full_join(df,df2)
# }

df<-rename(df, rhoSSB=Catchsim)

write.csv(df,here("Data/RhoSSB1.csv"))



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

#change rho and frequency inputs to make coding shiny easier
scenarios$Rho[scenarios$Rho==2]<-"No rho-adjustment"
scenarios$Rho[scenarios$Rho==1]<-"Rho-adjustment"
scenarios$Frequency[scenarios$Frequency==1]<-"Two year updates"
scenarios$Frequency[scenarios$Frequency==2]<-"Annual updates"

full_data<-full_join(data,scenarios, by=c("Scenario"))[, c(1,2,15:18,3:14)]
write.csv(full_data, here("Data/shiny_data_jj.csv"))

#Change inputs in datatable from Mackenzie
Table<-read.csv(here("Data/Table.csv"))[-1]

#rename
Table$Rho[Table$Rho==2]<-"No rho-adjustment"
Table$Rho[Table$Rho==1]<-"Rho-adjustment"
Table$Frequency[Table$Frequency==1]<-"Two year updates"
Table$Frequency[Table$Frequency==2]<-"Annual updates"

write.csv(Table, "Table.csv")



##### Update Scenario 1 and %REE #####
data_old<-read.csv(here("Data/shiny_data_jj.csv"))[-1]
#take out old scenario 1
data_update<-filter(data_old, Scenario !=1)

#merge scenario 1 metrics
scenario1<-read.csv(here("Data/SSB1.csv"))[-1]%>%
  full_join(read.csv(here("Data/F1.csv"))[-1])%>%
  full_join(read.csv(here("Data/Catch1.csv"))[-1])%>%
  full_join(read.csv(here("Data/R1.csv"))[-1])%>%
  full_join(read.csv(here("Data/REESSB1.csv"))[-1])%>%
  full_join(read.csv(here("Data/REEF1.csv"))[-1])%>%
  full_join(read.csv(here("Data/RhoF1.csv"))[-1])%>%
  full_join(read.csv(here("Data/RhoSSB1.csv"))[-1])

write.csv(scenario1,here("Data/scenario1.csv"))

#Add scenario info
scenarios<-read.csv(here("Data/scenarios.csv"))

#change rho and frequency inputs to make coding shiny easier
scenarios$Rho[scenarios$Rho==2]<-"No rho-adjustment"
scenarios$Rho[scenarios$Rho==1]<-"Rho-adjustment"
scenarios$Frequency[scenarios$Frequency==1]<-"Two year updates"
scenarios$Frequency[scenarios$Frequency==2]<-"Annual updates"

update_data<-left_join(scenario1,scenarios, by=c("Scenario"))[,c(2,4,15:18,1,3,5:12,14,13)]

#add to old data
data_new<-bind_rows(update_data, data_update)
write.csv(data_new, here("Data/shiny_data_jj_update.csv"))



##### New REE data #####
f<-read.csv(here("Data/REE rerun/REE_F1_19.csv"))[-1]%>%
  bind_rows(read.csv(here("Data/REE rerun/REE_F20.csv"))[-1])%>%
  bind_rows(read.csv(here("Data/REE rerun/REE_F21.csv"))[-1])%>%
  bind_rows(read.csv(here("Data/REE rerun/REE_F22_32.csv"))[-1])

ssb<-read.csv(here("Data/REE rerun/REE_SSB1_20.csv"))[-1]%>%
  bind_rows(read.csv(here("Data/REE rerun/REE_SSB21.csv"))[-1])%>%
  bind_rows(read.csv(here("Data/REE rerun/REE_SSB22_32.csv"))[-1])

ree_new<-full_join(f,ssb, by=c("Year", "Scenario"))  

scenarios<-read.csv(here("Data/scenarios.csv"))
scenarios$Rho[scenarios$Rho==2]<-"No rho-adjustment"
scenarios$Rho[scenarios$Rho==1]<-"Rho-adjustment"
scenarios$Frequency[scenarios$Frequency==1]<-"Two year updates"
scenarios$Frequency[scenarios$Frequency==2]<-"Annual updates"

update_ree<-left_join(ree_new,scenarios, by=c("Scenario"))

write.csv(update_ree, here("Data/ree_new.csv"))  
  
  
  
