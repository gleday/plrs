# Internal function
# Fit a (constrained) piecewise linear regression spline

# Author: Gwenael G.R. Leday

.plrs.fit <- function(X, matconstr, mytype, modelFull, callArg){

	nval <- length(modelFull@mdata$mconf)
	constr <- ifelse(is.null(matconstr), FALSE, TRUE)
	expr <- modelFull@mdata$mexpr

	### Fit (constrained) plrs
	if(nval!=1){
		# Unconstrained model
		if(!constr){
			lm.obj <- lm(expr~.-1, data=as.data.frame(cbind(expr,X)))
			sol <- coef(lm.obj)
			cov.mat <- vcov(lm.obj)
			S2 <- summary(lm.obj)$sigma^2
			df.error <- lm.obj$df.res
		}
		# Constrained model
		else{
			# Input solve.QP
			Dmat <- crossprod(X,X)
			dvec <- crossprod(X,expr)

			# Input
			Amat <- t(matconstr)
			bvec <- rep(0, nrow(matconstr))

			## Quadratic programming
			sol <- solve.QP(Dmat, as.vector(dvec), Amat, bvec, meq = 0)$sol
			names(sol) <- colnames(X)
		}
	}
	### Fit simple linear model
	else{
		# Unconstrained
		if(!constr){
			lm.obj <- lm(expr~.-1, data=as.data.frame(cbind(expr,X)))
			sol <- coef(lm.obj)
			cov.mat <- vcov(lm.obj)
			S2 <- summary(lm.obj)$sigma^2
			df.error <- lm.obj$df.res
		}
		# Constrain slope to be positive or null
		else{
			# Input
			Dmat <- crossprod(X,X)
			dvec <- crossprod(X,expr)
			Amat <- t(t(c(0,1)))
			bvec <- 0

			## Quadratic programming
			sol <- solve.QP(Dmat, as.vector(dvec), Amat, bvec, meq = 0)$sol

			names(sol) <- colnames(X)
		}
		chg.pt <- numeric()
	}

	### Output plrs object
	confModel <- function(modeFull, mylabs){

		theconf <- modelFull@mdata$mconf
		theknots <- modeFull@mdata$mknots
		idx1 <- substr(mylabs, 6,6)
		idx2 <- substr(mylabs, 8,8)
		myidx <- as.numeric(unique(idx1))

		if(sum(idx2=="0")==1){
			cont <- TRUE
		}
		else{
			cont <- FALSE
		}
		if(length(mylabs)==1){
			myconf <- 0
			myknots <- numeric()
		}
		else{
			myconf <- theconf[myidx+1]
			deleted.knots <- !(1:length(theknots))%in%myidx
			if(all(deleted.knots)){
				myknots <- numeric()
			}
			else{
				myknots <- theknots[!deleted.knots]
			}			
		}
		return(list(conf=myconf, knots=myknots, conti=cont))
	}
	tp <- confModel(modelFull, colnames(X))
	chg.pt <- tp$knots
	continuous <- tp$conti
	mdata <- modelFull@mdata
	mdata$mconf <- tp$conf
	mdata$mknots <- tp$knots

	fit.val <- as.vector(crossprod(t(X),sol)) #as.vector(X %*% sol)
	names(fit.val) <- names(expr)
	if(constr){
		qp <- list("Dmat" = Dmat, "dvec" = dvec, "Amat" = Amat,
					"bvec" = bvec, "meq" = 0)
	}
	else{
		qp <- list("vcov" = cov.mat, "S2" = S2, "df.error" = df.error)
	}

	call.arg <- list("continuous" = callArg$continuous,
			     "constr" = callArg$constr,
			     "constr.intercepts" = modelFull@call.arg$constr.intercepts,
			     "constr.slopes" = modelFull@call.arg$constr.slopes,
			     "config" = paste(as.character(mdata$mconf),collapse=","))

	out <- new("plrs",
			coefficients = sol,
			fitted.values = fit.val,
			residuals = expr - fit.val,
			X = X,
			data = modelFull@data,
			mdata = mdata,
			QP = qp,
			test = list(),
			cb = list(),
			selected = TRUE,
			type = mytype,
			call.arg = call.arg)

	return(out)
}
