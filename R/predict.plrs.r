# Make predictions from a given "plrs" model

# Author: Gwenael G.R. Leday

predict.plrs <- function(object, newcghseg, ...){

	if(class(object)!="plrs") stop("An object of class \"plrs\" is required")
	if(!is.vector(newcghseg)) stop("Object \"newcghseg\" has to be a vector")

	X <- .Bmat(newcghseg, knots = object@data$knots,
		    continuous = object@call.arg$continuous,
		    general.intercept = T)

	if(object@selected) X <- X[,colnames(X)%in%names(coef(object))]

	fit.val <- as.vector(crossprod(t(X), coef(object)))
	return(fit.val)
}
