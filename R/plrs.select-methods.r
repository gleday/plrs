############################
## Methods for "plrs.select"
############################

setMethod(
	f = "plot",
	signature = "plrs.select",
	definition = function(x,y,...) plot.plrs(x@model,...)
)
setMethod(
	f = "print",
	signature = "plrs.select",
	definition = function(x,...){
		# Display
		cat("\n")
		cat("Object of class \"plrs.select\"\n\n")
		cat(toupper(x@crit), "model selection procedure", "\n\n")

		cat("Coefficients of selected spline:\n")
		print(round(coef(x@model),5))
		cat("\n")
		if(x@model@call.arg$constr){
			cat("Model is constrained:\n")
			cat("constr.slopes =", x@model@call.arg$constr.slopes,"\n")
			if(!is.null(x@model@call.arg$constr.slopes.to.zero)) cat("constr.slopes.to.zero =", x@model@call.arg$constr.slopes.to.zero,"\n")
			cat("constr.intercepts =", x@model@call.arg$constr.intercepts,"\n\n")
		}
	}
)
setMethod(
	f = "show",
	signature = "plrs.select",
	definition = function(object) print(object)
)
setMethod(
	f = "summary",
	signature = "plrs.select",
	definition = function(object,...){
		# Display
		print(object)
		cat("Model selection table:\n")
		if(nrow(object@table)>5){
			print(head(object@table))
			cat("...\n\n")
		}
		else{
			print(object@table)
			cat("\n")
		}
	}
)