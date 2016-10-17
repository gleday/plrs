# Select the best model based on information criteria
# AIC, AICC, BIC or OSAIC

# Author: Gwenael G.R. Leday

plrs.select <- function(object, crit = ifelse(object@call.arg$constr,"osaic","aic")){

	criter <- crit
	if(is.null(criter)) stop("No criterion")
	if(length(criter)>1) stop("Multiple criteria are not allowed")
	if(!is.character(criter)) stop("Criterion has to be a character")
	if(!criter%in%c("aic","aicc","bic","osaic")) stop("Wrong criterion")

	# Generate all design and constraint matrices
	allsubmodels <- .plrs.submodels(object)

	# Fit all submodels
	allModels <- rep(list(NULL),length(allsubmodels$allMats))
	for(k in 1:length(allsubmodels$allMats)){
		allModels[[k]] <- .plrs.fit(X = allsubmodels$allMats[[k]],
                                        matconstr = allsubmodels$allConstr[[k]],
                                        mytype = allsubmodels$allTypes[k],
                                        modelFull = object, callArg=object@call.arg)
	}
	allModels <- c(allModels, object)
	allTypes <- c(allsubmodels$allTypes, "Piecewise linear")

	# Compute criterion
	f <- function(x) criteria(x, criter)[criter]
	crits <- unlist(lapply(allModels, f))
	crits <- matrix(crits, length(crits), 1)
	rownames(crits) <- paste("model",1:length(allModels), sep="")
	colnames(crits) <- criter

	# Output
	best <- which.min(crits)
	bmodel <- allModels[[best]]
	bmodel@selected <- TRUE
	bmodel@type <- allTypes[best]

	out <- new("plrs.select",
                 "table" = crits,
                 "model" = bmodel,
                 "crit" = criter)
	return(out)
}



