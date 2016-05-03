# Internal function
# Determine quantile of mixture of beta distributions

# Author: Gwenael G.R. Leday

.quantileBetabar <- function(df.bar, df.error, wt.bar, alpha){
	statbis <- 0.5
	stp <- 0.499999
	vals <- rep(NA, 100)
	bool <- T
	ct <- 0
	while(bool){
		ct <- ct + 1
		vals[ct] <- statbis
		pvalbis <- 1 - pbetabar(statbis, df1=df.bar/2, df2=df.error/2, wt=wt.bar)
		if(pvalbis>alpha){
			statbis <- statbis + stp
		}else{
			statbis <- statbis - stp
		}
		stp <- stp / 2
		if(ct>1) if(abs(pvalbis-alpha)<=.Machine$double.eps | abs(vals[ct-1]-vals[ct])<=1e-8) bool <- F
	}
	vals[ct]
}