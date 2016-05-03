# Internal function
# Confidence bands for a given x, optimization with csdp().

# Author: Gwenael G.R. Leday

.respCI <- function(newx, object, alpha){

	# Test including intercept
	cov.mat <- object@test$unconstr@QP$vcov/object@test$unconstr@QP$S2
	x <- rep(0, length(coef(object)))
	newstat <- as.numeric(t(coef(object)-x) %*% solve(cov.mat, coef(object)-x))
	newstat <- newstat/(object@test$unconstr@QP$df.error*object@test$unconstr@QP$S2+newstat)
	qbetabar <- .quantileBetabar(df.bar=object@test$df.bar+1, df.error=object@test$unconstr@QP$df.error, wt.bar=object@test$wt.bar, alpha=alpha)
	lambda <- (object@test$unconstr@QP$df.error*object@test$unconstr@QP$S2)*(qbetabar/(1-qbetabar))

	R <- solve(cov.mat)
	M <- chol(R)
	ML <- coef(object)
	bb <- -M%*%ML
	constr <- t(object@QP$Amat)
	npar <- length(ML)

	# myC
	myC <- list(
		-1*rbind(cbind(diag(npar), bb), cbind(t(bb), lambda)),
		matrix(0, npar-1, npar-1)
		)

	# myA
	A11 <- rbind(cbind(matrix(0,npar,npar), matrix(M[,1],npar,1)), cbind(matrix(M[,1],1,npar),0))
	A12 <- rbind(cbind(matrix(0,npar,npar), matrix(M[,2],npar,1)), cbind(matrix(M[,2],1,npar),0))
	if(npar>2) A13 <- rbind(cbind(matrix(0,npar,npar), matrix(M[,3],npar,1)), cbind(matrix(M[,3],1,npar),0))
	if(npar>2) A14 <- rbind(cbind(matrix(0,npar,npar), matrix(M[,4],npar,1)), cbind(matrix(M[,4],1,npar),0))
	if(npar>4) A15 <- rbind(cbind(matrix(0,npar,npar), matrix(M[,5],npar,1)), cbind(matrix(M[,5],1,npar),0))
	if(npar>4) A16 <- rbind(cbind(matrix(0,npar,npar), matrix(M[,6],npar,1)), cbind(matrix(M[,6],1,npar),0))
	if(npar>6) A17 <- rbind(cbind(matrix(0,npar,npar), matrix(M[,7],npar,1)), cbind(matrix(M[,7],1,npar),0))
	if(npar>6) A18 <- rbind(cbind(matrix(0,npar,npar), matrix(M[,8],npar,1)), cbind(matrix(M[,8],1,npar),0))

	if(npar==2){
		A21 <- constr[,1]
		A22 <- constr[,2]
	}
	else{
		A21 <- diag(constr[,1])
		A22 <- diag(constr[,2])
	}
	if(npar>2) A23 <- diag(constr[,3])
	if(npar>2) A24 <- diag(constr[,4])
	if(npar>4) A25 <- diag(constr[,5])
	if(npar>4) A26 <- diag(constr[,6])
	if(npar>6) A27 <- diag(constr[,7])
	if(npar>6) A28 <- diag(constr[,8])

	myA <- NULL
	for(i in 1:npar){
		myA <- c(myA, list(list(get(paste("A1",i,sep="")), get(paste("A2",i,sep="")))))
	}

	#myK
	if(npar!=2){
		myK <- list(type=c("s","s"), size=c(npar+1,npar-1))
	}
	else{
		myK <- list(type=c("s","l"), size=c(npar+1,npar-1))
	}

	#myb
	myb <- as.vector(.Bmat(newx, knots=knots(object), object@call.arg$continuous, TRUE))

	# Semidefinite prog for inf
	res <- csdp(myC,myA,myb,myK)
	parinf <- res$y
	inf <- res$dobj

	#myb
	myb <- -as.vector(.Bmat(newx, knots=knots(object), object@call.arg$continuous, TRUE))

	# Semidefinite prog for sup
	res <- csdp(myC,myA,myb,myK)
	parsup <- res$y
	sup <- -res$dobj

	out <- c(inf,sup)
	names(out) <- c("inf","sup")

	return(out)
}

