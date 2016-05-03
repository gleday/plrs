# Likelihood ratio test for (constrained) plrs models

# Unconstrained:
# H0: Intercept model
# H1: Full model

# Constrained:
# H0: All constraints are actives (=)
# H1: At least one constraint is strict (>)

# Authors: Gwenael G.R. Leday based on Ulrike Gromping code from package ic.infer

plrs.test <- function(object, alpha=0.05){

	if(class(object)!="plrs") stop("An object of class \"plrs\" is required")
	if(object@selected){
		obj <- plrs(expr=object@mdata$mexpr, cghseg=object@mdata$mcghseg, cghcall=object@mdata$mcghcall,
				probloss = object@mdata$mprobloss,
				probnorm = object@mdata$mprobnorm,
				probgain = object@mdata$mprobgain,
				probamp = object@mdata$mprobamp,
				knots = NULL,
				continuous = object@call.arg$continuous,
				constr = object@call.arg$constr,
				constr.slopes = object@call.arg$constr.slopes,
				constr.intercepts = object@call.arg$constr.intercepts)
	}else{
		obj <- object
	}
	if(length(coef(obj))!=1){
		if(obj@call.arg$constr){ # Test whether all inequalities are equalities
			# Aknowledgement: Taken from package ic.infer
			if(obj@QP$meq == nrow(t(obj@QP$Amat))){
				stop("Test not possible: only equality constraints")
			}

			library(mvtnorm)

			# Unconstrained model
			unconstr <- .plrs.fit(X=obj@X, matconstr=NULL, mytype=obj@type, modelFull=obj, callArg=obj@call.arg)
			cov.mat <- unconstr@QP$vcov/unconstr@QP$S2
			Dmat <- obj@QP$Dmat
			dvec <- as.vector(obj@QP$dvec)
			uiw <- t(obj@QP$Amat)
			bvec <- obj@QP$bvec
			meq <- obj@QP$meq

			# If intercepts are not constrained set them to 0
			if(!obj@call.arg$constr.intercepts & !obj@call.arg$continuous){
			     para <- c(3,5,7)[c(3,5,7) < ncol(model.matrix(obj))]
			     mat <- matrix(0,length(para),ncol(model.matrix(obj)))
			     mat[1:length(para),para] <- 1
			     uiw <- rbind(mat,uiw)
			     bvec <- c(bvec,rep(0,nrow(mat)))
			     meq <- meq + length(para)
			}

			# Weights
			if(meq==0){
				wt.bar <- ic.weights(crossprod(t(crossprod(t(uiw),cov.mat)),t(uiw)))
			}
			else{
				wt.bar <- ic.weights(solve(solve(crossprod(t(crossprod(t(uiw),cov.mat)),t(uiw)))[-(1:obj@QP$meq), -(1:obj@QP$meq)]))
			}

			# Stat, quantile and pval
			b.eq  <- solve.QP(Dmat, dvec, t(uiw), bvec, meq = nrow(uiw))$sol
			b.eq <- round(b.eq,8)
			names(b.eq) <- names(coef(obj))
			stat <- as.vector(crossprod(coef(obj)-b.eq, solve(cov.mat, coef(obj)-b.eq)))
			stat <- stat/(unconstr@QP$df.error*unconstr@QP$S2+stat)
			df.bar <- (nrow(uiw)-meq):0
			pval <- 1 - pbetabar(stat, df1=df.bar/2, df2=unconstr@QP$df.error/2, wt=wt.bar)
			qbetabar <- .quantileBetabar(df.bar=df.bar, df.error=unconstr@QP$df.error, wt.bar=wt.bar, alpha=alpha)
			test <- list("stat" = stat, "pvalue" = pval, "wt.bar" = wt.bar, "df.bar" = df.bar, "unconstr" = unconstr, "qbetabar" = qbetabar, "alpha" = alpha)
		}
		else{ # intercept vs given model
			myX <- matrix(1, nrow(obj@X), 1, dimnames=list(NULL,"theta0.0"))
			modelNull <- .plrs.fit(X=myX, matconstr=NULL, mytype="Intercept", modelFull=obj)

			rss0 <- sum(residuals(modelNull)^2)
			rss1 <- sum(residuals(obj)^2)

			# Degree of freedom
			N <- length(obj@data$expr)
			df0  <- N - 1
			df1  <- N - ncol(model.matrix(obj))
			dfnum <- df0 - df1
			dfden <- df1

			# P-value
			stat  <- ((rss0 - rss1)/dfnum)/(rss1/dfden)
			pval <- 1 - pf(stat, dfnum, dfden)
			test <- list("stat" = stat, "pvalue" = pval)
		}
		object@test <- test
		return(object)
	}
	else{
		return(object)
	}
}

