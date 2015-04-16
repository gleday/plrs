# Internal function
# Build design matrix with truncated power basis functions

# Author: Gwenael G.R. Leday

.Bmat <- function(cghseg, knots, continuous=TRUE, general.intercept=TRUE){

	nKnots <- length(knots)
	nval <- nKnots + 1
	xx <- cghseg
	mat <- matrix(cghseg)
	if(general.intercept) mat <- cbind(rep(1, length(xx)) ,mat)

	# Truncated p-th power function
	tpower <- function(x, t, p) (x - t) ^ p * (x > t)

	if(continuous){
		if(nKnots>=1) mat <- cbind(mat, tpower(xx, knots[1], 1))
		if(nKnots>=2) mat <- cbind(mat, tpower(xx, knots[2], 1))
		if(nKnots>=3) mat <- cbind(mat, tpower(xx, knots[3], 1))
		nval2 <- 0
	}
	else{
		if(nKnots>=1){
			mat <- cbind(mat, tpower(xx, knots[1], 0))
			mat <- cbind(mat, tpower(xx, knots[1], 1))
		}
		if(nKnots>=2){
			mat <- cbind(mat, tpower(xx, knots[2], 0))
			mat <- cbind(mat, tpower(xx, knots[2], 1))
		}
		if(nKnots>=3){
			mat <- cbind(mat, tpower(xx, knots[3], 0))
			mat <- cbind(mat, tpower(xx, knots[3], 1))
		}
		nval2 <- nval - 1
	}

	# Labels
	theta.1 <- paste(paste(paste("theta",0:(nval-1),sep=""),".",sep=""),1,sep="")
	if(!general.intercept & nval2==0) theta.0 <- NULL
	else{ theta.0 <- paste(paste(paste("theta",0:nval2,sep=""),".",sep=""),0,sep="")}
	colnames(mat) <- sort(c(theta.0, theta.1))

	return(mat)
}

