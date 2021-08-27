scenarios<-function()
{
  OM=c("Base Case Overfished", "Base Case Not Overfished ", "Overfished Mortality Misspecified", "Overfished Recruitment Misspecified", "Overfished Mortality and Recruitment Misspecified", "Overfished Mortality and Recruitment Misspecified", "Overfished Mortality and Recruitment Misspecified", "Not Overfished Catchability Misspecified")
  Rho=c("No Rho-adjustment","No Rho-adjustment","No Rho-adjustment", "No Rho-adjustment","No Rho-adjustment","Rho-adjustment", "No Rho-adjustment", "No Rho-adjustment")
  Frequency=c("Two Year Updates", "Two Year Updates", "Two Year Updates", "Two Year Updates", "Two Year Updates", "Two Year Updates", "Annual Updates", "Two Year Updates")
  
  scenarios<-data.frame(OM, Rho, Frequency) 
  colnames(scenarios)[colnames(scenarios)=="OM"]<-"Operating Model/ Misspecification"
  datatable(scenarios)
  
  
}