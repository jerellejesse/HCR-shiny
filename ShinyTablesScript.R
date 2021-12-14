library(matrixStats)
library(dplyr)
library(here)
library(plyr)
library(DescTools)

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

  Catchsim<-matrix(NA,nrow=3,ncol=length(sims))

  for (k in 1:length(sims)){
    load(sims[k])
    Catchsim[,k]<-omvalGlobal[[1]]$F_full[188:190]
  }

Catchsim<-rowMedians(Catchsim,na.rm=T)
Year<-2038:2040
df2<-as.data.frame(cbind(Catchsim,Year))

Catchest<-matrix(NA,nrow=3,ncol=length(sims))

for (k in 1:length(sims)){
  load(sims[k])
  Catchest[,k]<-omvalGlobal[[1]]$Fest[190,1:3]
}

Catchest<-rowMedians(Catchest,na.rm=T)
Catchest<-na.omit(Catchest)

df2$Catchest<-Catchest
df2$Scenario<-i
df<-full_join(df,df2)
}

df<-rename(df, R=Catchsim)%>%
 rename(Rest=Catchest)


write.csv(df,here("Data/R1.csv"))



#REE calculations--------------------------------------------
#SSB
setwd("C:/Users/jjesse/Box/HCR_Sims")
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
  df<-full_join(df,df2)
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

write.csv(df,here("Data/REE_SSB.csv"))


#F
setwd("C:/Users/jjesse/Box/HCR_Sims")
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
  
  Ftrue<-omvalGlobal[[1]]$F_full[167:187]
  for (i in seq(170,190,2)){
    Fest<-omvalGlobal[[1]]$Fest[i,]
    Fest<-na.omit(Fest)
    Fest<-tail(Fest,1)
    Catchsim[((i-168)/2),k]<-((Fest-Ftrue[i-169])/Ftrue[i-169])*100
    
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
    Ftrue<-omvalGlobal[[1]]$F_full[167:187]
    for (i in seq(170,190,2)){
      Fest<-omvalGlobal[[1]]$Fest[i,]
      Fest<-na.omit(Fest)
      Fest<-tail(Fest,1)
      Catchsim[((i-168)/2),k]<-((Fest-Ftrue[i-169])/Ftrue[i-169])*100
      
    }
  }
  
  Catchsim<-rowMedians(Catchsim,na.rm=T)
  Year<-seq(2019,2039,2)
  df2<-as.data.frame(cbind(Catchsim,Year))
  df2$Scenario<-j
  df<-full_join(df,df2)
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
    Ftrue<-omvalGlobal[[1]]$F_full[168:189]
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
    Ftrue<-omvalGlobal[[1]]$F_full[167:187]
    for (i in seq(170,190,2)){
      Fest<-omvalGlobal[[1]]$Fest[i,]
      Fest<-na.omit(Fest)
      Fest<-tail(Fest,1)
      Catchsim[((i-168)/2),k]<-((Fest-Ftrue[i-169])/Ftrue[i-169])*100
    }
  }
  
  Catchsim<-rowMedians(Catchsim,na.rm=T)
  Year<-seq(2019,2039,2)
  df2<-as.data.frame(cbind(Catchsim,Year))
  df2$Scenario<-j
  
  df<-dplyr::full_join(df,df2)
}

df<-rename(df, REEF=Catchsim)

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

df<-rename(df, rhoF=Catchsim)

write.csv(df,here("Data/RhoF1.csv"))


#####Confidence interval plots ####
setwd("C:/Users/jjesse/Box/HCR_Sims")
wd<-getwd()

setwd(paste(wd,"/Sim_1/sim",sep=""))
sims <- list.files()

for (k in 1:length(sims)){
  if (file.size(sims[k])==0){
    sims[k]<-NA}
}

sims<-na.omit(sims)

SSBratiots<-matrix(NA,nrow=22,ncol=length(sims))

for (k in 1:length(sims)){
  load(sims[k])
  SSBratiots[,k]<-omvalGlobal[[1]]$R[169:190]
}

Df<-as.data.frame(SSBratiots)
Df$Year<-2019:2040
Df<- Df %>% gather(Year, R, 1:(length(Df)-1))
Df$Year<-rep(2019:2040,(length(Df)-1))
Df <- ddply(Df, "Year", summarise, median = MedianCI(R)[1], CI_lower=MedianCI(R)[2], CI_upper=MedianCI(R)[3])
Df$Scenario<-1

df<-dplyr::rename(Df, RCI_Upper=CI_upper)%>%
  dplyr::rename(RCI_Lower=CI_lower)%>%
  select(-median)

write.csv(df, here::here("Data/RCI1.csv"))

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



##### Update Scenario 1 and extended time series and %REE #####
data_old<-read.csv(here("Data/shiny_data_jj.csv"))[-1]
#take out old scenario 1
data_remove<-filter(data_old, Scenario !=1)

#merge scenario 1 metrics
scenario1<-read.csv(here("Data/SSB1.csv"))[-1]%>%
  full_join(read.csv(here("Data/F1.csv"))[-1])%>%
  full_join(read.csv(here("Data/Catch1.csv"))[-1])%>%
  full_join(read.csv(here("Data/R1.csv"))[-1])%>%
  full_join(read.csv(here("Data/REE_SSB1.csv"))[-1])%>%
  full_join(read.csv(here("Data/REE_F1.csv"))[-1])%>%
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
data_new<-bind_rows(update_data, data_remove)
write.csv(data_new, here("Data/shiny_data_jj_update.csv"))

#####extended time series for confidence interval plots 
data<-read.csv(here("Data/Table.csv"))

extend<-read.csv(here("Data/SSB_2040.csv"))[-1]%>%
  full_join(read.csv(here("Data/F_2040.csv"))[-1])%>%
  full_join(read.csv(here("Data/Catch_2040.csv"))[-1])%>%
  full_join(read.csv(here("Data/R_2040.csv"))[-1])

#Add scenario info
scenarios<-read.csv(here("Data/scenarios.csv"))

#change rho and frequency inputs to make coding shiny easier
scenarios$Rho[scenarios$Rho==2]<-"No rho-adjustment"
scenarios$Rho[scenarios$Rho==1]<-"Rho-adjustment"
scenarios$Frequency[scenarios$Frequency==1]<-"Two year updates"
scenarios$Frequency[scenarios$Frequency==2]<-"Annual updates"

update_data<-left_join(extend,scenarios, by=c("Scenario"))[,c(2,4,11:14,1,3,5:10)]

#merge in extended data
data_new<-bind_rows(update_data, data)
write.csv(data_new, here("Data/Table_update.csv"))





##### update scenario 1 CI data #####
data_old<-read.csv(here::here("Data/Table_update.csv"))[-1]
data_left<-data_old[,1:17, 26:28]

#take out old scenario 1
data_remove<-select(data_old,Year, Scenario, SSBCI_Lower, SSBCI_Upper, FCI_Lower, FCI_Upper, CatchCI_Lower, CatchCI_Upper, RCI_Lower, RCI_Upper)%>%
  filter(Scenario !=1)

#merge scenario 1 metrics
scenario1_CI<-read.csv(here::here("Data/SSBCI1.csv"))[-1]%>%
  full_join(read.csv(here::here("Data/FCI1.csv"))[-1])%>%
  full_join(read.csv(here::here("Data/CatchCI1.csv"))[-1])%>%
  full_join(read.csv(here::here("Data/RCI1.csv"))[-1])

write.csv(scenario1,here::here("Data/scenario1_CI.csv"))


#add to old data
data_merge<-full_join(scenario1_CI, data_remove)
data_new<-full_join( data_left, data_merge, by=c("Year", "Scenario"))


#need the new medians in the table_update file
scenario1<-read.csv(here("Data/scenario1.csv"))%>%
  select(Year, Scenario,SSB, F_full, Catchsim, R)

data_new_remove<-select(data_new, Year, Scenario, SSB, F_full, Catchsim, R)%>%
  filter(Scenario!= 1)
data_new_replace<-full_join(scenario1, data_new_remove)

data_new_left<-data_new[, c(1:6,8,10,12,14:25)]

data_new2<-full_join(data_new_left, data_new_replace, by=c("Year", "Scenario"))
  
write.csv(data_new2, here::here("Data/Table_update.csv"))





##### New REE data #####
# f<-read.csv(here("Data/REE rerun/REE_F1_19.csv"))[-1]%>%
#   bind_rows(read.csv(here("Data/REE rerun/REE_F20.csv"))[-1])%>%
#   bind_rows(read.csv(here("Data/REE rerun/REE_F21_24.csv"))[-1])%>%
#   bind_rows(read.csv(here("Data/REE rerun/REE_F25_32.csv"))[-1])
# 
# ssb<-read.csv(here("Data/REE rerun/REE_SSB1_20.csv"))[-1]%>%
#   bind_rows(read.csv(here("Data/REE rerun/REE_SSB21_24.csv"))[-1])%>%
#   bind_rows(read.csv(here("Data/REE rerun/REE_SSB25_32.csv"))[-1])
# ree_new<-full_join(f,ssb, by=c("Year", "Scenario"))  

f<-read.csv(here("Data/REE_F.csv"))[-1]
ssb<-read.csv(here("Data/REE_SSB.csv"))[-1]

ree_new<-full_join(f, ssb, by=c("Year", "Scenario"))
ree_new_remove<-filter(ree_new, Scenario !=1)

scenario1_ree<-read.csv(here("Data/REE_SSB1.csv"))[-1]%>%
  full_join(read.csv(here("Data/REE_F1.csv"))[-1])
  
ree_new<-bind_rows(scenario1_ree, ree_new_remove)

scenarios<-read.csv(here("Data/scenarios.csv"))
scenarios$Rho[scenarios$Rho==2]<-"No rho-adjustment"
scenarios$Rho[scenarios$Rho==1]<-"Rho-adjustment"
scenarios$Frequency[scenarios$Frequency==1]<-"Two year updates"
scenarios$Frequency[scenarios$Frequency==2]<-"Annual updates"

update_ree<-left_join(data,scenarios, by=c("Scenario"))

write.csv(update_ree, here("Data/ree_new.csv"))  
  
  
##### add columns for comparison tab #####
data<-read.csv(here::here("Data/Table_update.csv"))
scenarios<-read.csv(here::here("Data/scenarios.csv"))

data$Misspecification[data$Scenario %in% c(5,6,7,8)]<-1 #cod M
data$Misspecification[data$Scenario %in% c(9, 10,11,12)]<-2 #cod R
data$Misspecification[data$Scenario %in% c(13,14,15,16)]<-3 #cod M + R
data$Misspecification[data$Scenario %in% c(29,30,31,32)]<-4 #haddock catchability

data$Compare_Mis[data$Scenario %in% c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,25,26,27,28,29,30,31,32)]<-"Misspecified and correctly specified stock assessment"
data$Compare_Rho[data$Scenario %in% c(17,18,19,20)]<-"Rho-adjusted and not rho-adjusted"
data$Compare_Freq[data$Scenario %in% c(21,22,23,24)]<-"Two-year and annual stock assessment updates"

write.csv(data, here::here("Data/Table_update.csv"))


data$Rho[data$Rho==2]<-"No rho-adjustment"
data$Rho[data$Rho==1]<-"Rho-adjustment"
data$Frequency[data$Frequency==1]<-"Two year updates"
data$Frequency[data$Frequency==2]<-"Annual updates"
