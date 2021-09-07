metrics<-function()
{
Metric=c("Trajectories", "95% Trajectories ", "Catch boxplot", "Relative error", "Retrospective patterns", "Refernce point accuracy", "Stock status trajectory", "Stock status ratio ", "Radar")
Type=c("Stock performance","Stock performance","Stock performance", "Assessment performance","Assessment performance","Assessment performance", "Management performance", "Management performance", "Management performance")
Description=c("trajectory for SSB, F, Catch, R", "trajectory for SSB, F, Catch, R with 95% confidence interval", "true median catch boxplot for short-, medium-, and long-term", "% REE for terminal estimated SSB and F", "Mohn's Rho for SSB and F", "Median estimated / true SSBMSY and FMSY", "true stock trajectories- F/FMSY versus SSB/SSBMSY", "true median ratios of SSB/SSBMSY and F/FMSY", "using SSB, Freq. not overfished, freq not overfishing, catch stability, catch")

metrics<-data.frame(Type, Metric, Description) 
datatable(metrics)

}
