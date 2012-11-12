# Estimate knots

# Author: Gwenael G.R. Leday

.estimateKnots <- function(cghseg, cghcall, probloss=NULL, probnorm=NULL, probgain=NULL, probamp=NULL){

	val <- sort(unique(cghcall))
	nval <- length(val)
	knots <- NULL
	
	# Midpoints of intermediate intervals
	if(is.null(probloss) || is.null(probnorm) || is.null(probgain)){
		if(nval>1){
			for(k in 1:(nval-1)){
				knots <- c(knots,(max(cghseg[cghcall==val[k]]) +  min(cghseg[cghcall==val[k+1]]))/2)
			}
		}
	}
	# Estimate using call probabilities
	else{
		p <- function(alpha, cghseg, leftState, rightState){
			ind1 <- as.numeric(cghseg <= alpha)
			ind2 <- 1-ind1
			as.numeric(ind1%*%leftState + ind2%*%rightState)
		}
		for(j in 1:(length(val)-1)){
			# Reduce search to adjacent states range
			lb <- min(cghseg[cghcall==val[j]])
			ub <- max(cghseg[cghcall==val[j+1]])

			# Search maximum
			xx <- seq(from=lb, to=ub, length.out=1000)
			if(val[j]==-1) tomax <- sapply(xx, p, cghseg=cghseg, leftState=probloss, rightState=probnorm)
			if(val[j]==0) tomax <- sapply(xx, p, cghseg=cghseg, leftState=probnorm, rightState=probgain)
			if(val[j]==1) tomax <- sapply(xx, p, cghseg=cghseg, leftState=probgain, rightState=probamp)

			# Take midpoints of interval
			optvals <- xx[which(tomax==max(tomax))]
			leftpts <- max(cghseg[cghseg<=min(optvals)])
			rightpts <- min(cghseg[cghseg>min(optvals)])

			knots <- c(knots, mean(c(leftpts, rightpts)))
		}
	}
	return(knots)
}