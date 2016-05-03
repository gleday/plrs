######################
## Methods for "plrs"
######################

setMethod(
	f = "coef",
	signature = "plrs",
	definition = function(object,...) object@coefficients
)
setMethod(
	f = "fitted",
	signature = "plrs",
	definition = function(object,...) object@fitted.values
)
setMethod(
	f = "residuals",
	signature = "plrs",
	definition = function(object,...) object@residuals
)
setMethod(
	f = "model.matrix",
	signature = "plrs",
	definition = function(object,...) object@X
)
setMethod(
	f = "plot",
	signature = "plrs",
	definition = function(x,y,...) plot.plrs(x,...)
)
setMethod(
	f = "knots",
	signature = "plrs",
	definition = function(Fn,...) Fn@mdata$mknots
)
setMethod(
	f = "print",
	signature = "plrs",
	definition = function(x,...){
		# Display
		cat("\n")
		cat("Object of class \"plrs\"\n\n")
		if(x@selected){
			cat("Selected spline coefficients:\n")
		}
		else{
			cat("Spline coefficients:\n")
		}
		print(round(coef(x),5))
		cat("\n")

		if(x@call.arg$constr){
			cat("Model is constrained:\n")
			cat("constr.slopes =", x@call.arg$constr.slopes,"\n")
			if(!is.null(x@call.arg$constr.slopes.to.zero)) cat("constr.slopes.to.zero =", x@call.arg$constr.slopes.to.zero,"\n")
			cat("constr.intercepts =", x@call.arg$constr.intercepts,"\n\n")
		}
		if(length(x@test)!=0){
			cat("Testing:\n")
			cat("stat =", x@test$stat, "\n")
			cat("quantile = ", x@test$qbetabar,"  (alpha = ", x@test$alpha, ")","\n", sep="")
			cat("p-value = ", format.pval(x@test$pvalue),"\n\n")
		}
	}
)
setMethod(
	f = "show",
	signature = "plrs",
	definition = function(object) print(object)
)
setMethod(
	f = "summary",
	signature = "plrs",
	definition = function(object,...){
		print(object)
		cat("Configuration:\n")
		print(effects(object))
	}
)
setMethod(
	f = "predict",
	signature = "plrs",
	definition = function(object, ...) predict.plrs(object, ...)
)

setMethod(
	f = "effects",
	signature = "plrs",
	definition = function(object,...){
		mat <- table(object@mdata$mcghcall)
		confMat <- matrix(NA,3,4)
		colnames(confMat) <- c("Loss","Normal","Gain","Ampl.")
		rownames(confMat) <- c("n.obs","I.effect", "S.effect")
		labs <- as.numeric(dimnames(mat)[[1]])
		vals <- as.numeric(mat)
		confMat[1,c(-1,0,1,2)%in%labs] <- vals[labs%in%c(-1,0,1,2)]
		if(length(labs)!=1){
			if(-1%in%labs){
				confMat[2,1] <- round(coef(object)["theta1.0"],3)
				confMat[3,1] <- round(coef(object)["theta0.1"],3)
			}
			if(1%in%labs){
				if(-1%in%labs){
					confMat[2,3] <- round(coef(object)["theta2.0"],3)
					confMat[3,3] <- round(coef(object)["theta2.1"],3)
				}
				else{	
					confMat[2,3] <- round(coef(object)["theta1.0"],3)
					confMat[3,3] <- round(coef(object)["theta1.1"],3)
				}
			}
			if(2%in%labs){
				if(-1%in%labs){
					confMat[2,4] <- round(coef(object)["theta3.0"],3)
					confMat[3,4] <- round(coef(object)["theta3.1"],3)
				}
				else{
					confMat[2,4] <- round(coef(object)["theta2.0"],3)
					confMat[3,4] <- round(coef(object)["theta2.1"],3)
				}
			}
		}
		else{
			confMat[2,2] <- 0
			confMat[3,2] <- round(coef(object)["theta0.1"],3)
		}
		return(t(confMat))
	}
)
