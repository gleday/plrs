# Likelihood-based confidence bands for (constrained) plrs

# Author: Gwenael G.R. Leday

plrs.cb <- function(object, alpha=0.05, newcgh=NULL){

	if(!class(object)%in%c("plrs","plrs.select")) stop("An object of class \"plrs\" or \"plrs.select\" is required")
	if(length(object@test)==0 & !object@selected) stop("Likelihood ratio test using plrs.test() is required before computing bounds")
	if(is.null(newcgh)){
		if(!is.null(knots(object))) newcgh <- sort(c(knots(object)+1e-5,knots(object)-1e-5))
		xx <- seq(from=floor(min(object@data$cghseg))-0.5, to=ceiling(max(object@data$cghseg))+0.5, length=100-length(knots(object)))
		newcgh <- sort(c(newcgh, xx))
	}
	if(!object@call.arg$constr || !object@call.arg$constr.intercepts) stop("Model has to be fully constrained")
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
		obj <- plrs.test(obj, alpha=alpha)
	}else{
		obj <- object
	}

	allCI <- sapply(newcgh, .respCI, object=obj, alpha=alpha)

	object@cb <- list(inf=allCI[1,], sup=allCI[2,], x=newcgh)

	return(object)
}

