# Extract AIC, AICC, BIC and OSAIC from a 'plrs' object

# Author: Gwenael G.R. Leday

criteria <- function(obj, crit = "all"){

	# Error handling
	if(class(obj)!="plrs") stop("An object of class \"plrs\" is required")
	if(length(crit)==0) stop("No criterion")
	if(length(crit)==1) if(crit=="all") crit <- c("aic","aicc","bic","osaic")
	if(!is.character(crit)) stop("'crit' has to be a character")
	if(!"all"%in%crit & !"aic"%in%crit & !"aicc"%in%crit &
         !"bic"%in%crit & !"osaic"%in%crit) stop("Wrong criterion")
	bool <- TRUE
	if("osaic"%in%crit){
		if(!obj@call.arg$constr) bool <- FALSE
		else{
			if(length(coef(obj))!=1){
				if(obj@QP$meq == nrow(t(obj@QP$Amat))) bool <- FALSE
			}
			else{
				bool <- FALSE
			}
		}
	}

	rss <- sum(residuals(obj)^2)
	n <- length(obj@data$expr)

	# Compute criteria for the segmented model
	if(rss != 0){
		if(obj@call.arg$constr & length(coef(obj))>1) p <- length(coef(obj)) - obj@QP$meq
		else p <- length(coef(obj))

		if("aic" %in% crit || "aicc" %in% crit || !bool) aic <- n*log(rss/n) + 2*p
		if("aicc" %in% crit) aicc <- aic + ((2*p)*(p+1))/(n-p-1)
		if("bic" %in% crit) bic <- n*log(rss/n) + log(n)*p
		if("osaic" %in% crit){ # As defined in Hughes et al (2003)
			if(bool){
				library(mvtnorm)
				# Calculate weights
				obj.unconstr <- .plrs.fit(X=obj@X, matconstr=NULL, mytype=obj@type, modelFull=obj, callArg=obj@call.arg)
				cov.mat <- obj.unconstr@QP$vcov/obj.unconstr@QP$S2
				uiw <- t(obj@QP$Amat)

				# Aknowledgement: Taken from package ic.infer
				if(obj@QP$meq==0){
					#wt.bar <- ic.weights(uiw %*% cov.mat %*% t(uiw))
					wt.bar <- ic.weights(crossprod(t(crossprod(t(uiw),cov.mat)),t(uiw)))
				}
          	 	 	else{
					wt.bar <- ic.weights(solve(solve(crossprod(t(crossprod(t(uiw),cov.mat)),t(uiw)))[-(1:obj@QP$meq), -(1:obj@QP$meq)]))
				}
				wt.bar <- wt.bar[sort(names(wt.bar))]
				r <- (length(wt.bar)-1)
				osaic.npar <- sum(wt.bar*(p-r+0:r))
				osaic <- n*log(rss/n) + 2*osaic.npar
			}
			else{
				osaic <- aic
			}
		}
	}
	else{
		if("aic" %in% crit) aic <- -Inf
		if("aicc" %in% crit) aicc <- -Inf
		if("bic" %in% crit) bic <- -Inf
		if("osaic" %in% crit) osaic <- -Inf
	}

	out <- NULL
	if("aic" %in% crit) out <- c(out, list("aic" = aic))
	if("aicc" %in% crit) out <- c(out, list("aicc" = aicc))
	if("bic" %in% crit) out <- c(out, list("bic" = bic))
	if("osaic" %in% crit) out <- c(out, list("osaic" = osaic))
	return(out)
}
