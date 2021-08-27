metrics<-function()
{
Metric=c("Estimated/ true", "True 95% ", "catch", "% REE", "Mohns rho", "Refernce point error", "Kobe", "Ratio ", "Radar")
Type=c("Stock performance","Stock performance","Stock performance", "Assessment performance","Assessment performance","Assessment performance", "Management performance", "Management performance", "Management performance")
Description=c("trajectory for SSB, F, Catch, R", "trajectory for SSB, F, Catch, R with 95% confidence interval", "true median catch boxplot", "for terminal estimated SSB and F", "for SSB and F", "Median ratios of estimated / true SSBMSY and FMSY", "true stock trajectories- F/FMSY versus SSB/SSBMSY", "true median ratios of SSB/SSBMSY and F/FMSY", "using SSB, Freq. not overfished, freq not overfishing, catch stability, catch")

metrics<-data.frame(Type, Metric, Description) 
datatable(metrics)

}
