################################
## Methods for "plrs.series"
################################

setMethod(
	f = "print",
	signature = "plrs.series",
	definition = function(x,...){
		cat("\n")
		cat("Object of class \"plrs.series\"\n\n")
		cat(nrow(x@coefficients),"genes\n")
		if(is.null(x@call.arg$control.select)){
			cat("\n")
			cat("Type of fitted model:\n")
			cat("continuous =", x@call.arg$control.model$continuous,"\n")
			if(x@call.arg$control.model$constr){
				cat("constr.slopes =", x@call.arg$control.model$constr.slopes,"\n")
				cat("constr.intercepts =", x@call.arg$control.model$constr.intercepts,"\n\n")
			}
			else{
				cat("constr =", x@call.arg$control.model$constr,"\n\n")
			}
		}else{
			cat("Models selected with", toupper(x@call.arg$control.select$crit), "\n\n")
		}
	}
)
setMethod(
	f = "show",
	signature = "plrs.series",
	definition = function(object){
		print(object)
	}
)
setMethod(
	f = "summary",
	signature = "plrs.series",
	definition = function(object,...){
		# Display
		print(object)
		cat("Configuration:\n")
		print(object@general)
		cat("\n")
		cat("Nb of models regarding their types:\n")
		cat("Intercept        ",object@modelsType$summary[1],"\n")
		cat("Simple linear    ",object@modelsType$summary[2],"\n")
		if(nrow(object@general)>1){
			cat("Piecewise level  ",object@modelsType$summary[3],"\n")
			cat("Piecewise linear ",object@modelsType$summary[4],"\n\n")
		}
		else{
			cat("\n")
		}
		if(length(object@test)!=0){
			cat("Testing:\n")
			cat("Number of rejected null hypothesis:", sum(object@test[,3]<.1, na.rm=T), "genes\n")
			cat("(at 0.1 significance level based on \n Benjamini-Hochberg corrected p-values)\n\n")
		}
	}
)


