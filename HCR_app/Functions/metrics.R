metrics<-function()
{
Metric=c("Trajectories", "95% Trajectories ", "Catch boxplot", "Relative error", "Retrospective patterns", "Refernce point accuracy", "Stock status trajectory", "Stock status ratio ", "Radar")
Type=c("Stock performance","Stock performance","Stock performance", "Assessment performance","Assessment performance","Assessment performance", "Management performance", "Management performance", "Management performance")
Description=c("trajectories for spawning stock biomass, fishing mortality, catch, recruitment", "trajectories for spawning stock biomass, fishing mortality, catch, recruitment with 95% confidence interval", "true median catch boxplot for short-, medium-, and long-term", "% relative error for terminal estimated SSB and F", "Mohn's Rho for spawning stock biomass and fishing mortality", "median estimated / true SSBMSY and FMSY for short-, medium-, and long-term", "kobe plots: true stock trajectories- F/FMSY versus SSB/SSBMSY", "true median ratios of SSB/SSBMSY and F/FMSY for short-, medium-, and long-term", "radar plots using spawning stock biomass, frequency not overfished, frequency not overfishing, catch stability, catch")

metrics<-data.frame(Type, Metric, Description) 
datatable(metrics)

}
