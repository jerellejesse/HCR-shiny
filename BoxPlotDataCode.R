####Box Plot Data####
####Set directory####
require(dplyr)
require(matrixStats)
setwd("C:/Users/mmazur/Box/Mackenzie_Mazur/HCR_Sims")#change this accordingly 
wd<-getwd()

####Read in Data####
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

