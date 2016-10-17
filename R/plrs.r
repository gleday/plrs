# Fit a (constrained) piecewise linear regression spline

# Author: Gwenael G.R. Leday

plrs <- function(expr, cghseg, cghcall=NULL,
                 probloss = NULL,
                 probnorm = NULL,
                 probgain = NULL,
                 probamp = NULL,
                 knots = NULL,
                 continuous = FALSE,
                 constr = TRUE,
                 constr.slopes = 2,
                 constr.intercepts = TRUE,
                 min.obs=3,
                 discard.obs=TRUE){

	#### Error handling
	## check input data
	if(!is.vector(expr)) stop("expr must be a vector")
	if(!is.vector(cghseg)) stop("cghseg must be a vector")
	if(any(is.na(expr))) stop("Missing values are not allowed in expr")
	if(any(is.na(cghseg))) stop("Missing values are not allowed in cghseg")
	if(is.null(cghcall)) cghcall <- rep(0,length(cghseg))
	else{
		if(!is.vector(cghcall)){
			stop("cghcall must be a vector")
		}
		else{
			if(any(is.na(cghcall))) stop("Missing values are not allowed in cghcall")
			if(2%in%cghcall & !1%in%cghcall){
				cghcall[cghcall==2] <- 1
				warning("No gains: observations labelled amplifications are changed to gains")
			}
		}
	}
	if(length(expr)!=length(cghseg))	stop("expr and cghseg are incompatible")
	if(length(expr)!=length(cghcall)) 	stop("expr and cghcall are incompatible")
	if(length(cghseg)!=length(cghcall)) stop("cghseg and cghcall are incompatible")
	if(!is.null(probloss) & !is.vector(probloss)) 	stop("probloss must be a vector")
	if(!is.null(probnorm) & !is.vector(probnorm)) 	stop("probnorm must be a vector")
	if(!is.null(probgain) & !is.vector(probgain)) 	stop("probgain must be a vector")
	if(!is.null(probamp) & !is.vector(probamp)) 	stop("probamp must be a vector")

	val <- sort(unique(cghcall))
	conf <- val
	nval <- length(val)
	if(nval>4) stop("Configurations with more than 4 states are not allowed")
	out <- NULL

	## Check configuration
	# min.obs
	if(!is.numeric(min.obs)) stop("'min.obs' must be a numeric")
	min.obs <- round(min.obs)
	if(min.obs<1) stop("'min.obs' must be a positive integer")
	# discard.obs
	if(!is.logical(discard.obs)) stop("'discard.obs' must be a logical")
	# Nb of obs
	mcghcall <- modify.conf(cghcall, min.obs=min.obs, discard=discard.obs)

	mexpr <- expr
	mcghseg <- cghseg
	mprobloss <- probloss
	mprobnorm <- probnorm
	mprobgain <- probgain
	mprobamp <- probamp
	if(any(is.na(mcghcall))){
		mexpr <- mexpr[!is.na(mcghcall)]
		mcghseg <- mcghseg[!is.na(mcghcall)]
		if(!is.null(mprobloss)) mprobloss <- probloss[!is.na(mcghcall)]
		if(!is.null(mprobnorm)) mprobnorm <- mprobnorm[!is.na(mcghcall)]
		if(!is.null(mprobgain)) mprobgain <- mprobgain[!is.na(mcghcall)]
		if(!is.null(mprobamp)) mprobamp <- mprobamp[!is.na(mcghcall)]
		mcghcall <- mcghcall[!is.na(mcghcall)]
	}
	val <- sort(unique(mcghcall))
	nval <- length(val)

	if(nval!=1){
		# knots
		if(is.null(knots)){
			chg.pt <- .estimateKnots(cghseg = mcghseg, cghcall = mcghcall,
     	                                     probloss = mprobloss, probnorm = mprobnorm,
                                           probgain = mprobgain, probamp = mprobamp)
			if(sum(order(chg.pt)==1:length(chg.pt))!=length(chg.pt)){
				warning("Cannot fit a plrs model: estimated knots are non-ordered (this may be caused by strong outliers).")
			}
			# Recalling
			cl <- rep(val[1],length(mcghcall))
			if(length(chg.pt)>=1) cl[mcghseg>=chg.pt[1]] <- val[2]
			if(length(chg.pt)>=2) cl[mcghseg>=chg.pt[2]] <- val[3]
			if(length(chg.pt)>=3) cl[mcghseg>=chg.pt[3]] <- val[4]
			names(cl) <- names(mcghcall)
			# Reconfigure
			tp <- modify.conf(cl, min.obs=min.obs, discard=discard.obs)
			# If not identical, this means that min obs is not fulfilled for a state after recalling, hence: 
			if(!identical(cl,tp)){
				mcghcall <- tp
				if(any(is.na(mcghcall))){
					mexpr <- mexpr[!is.na(mcghcall)]
					mcghseg <- mcghseg[!is.na(mcghcall)]
					if(!is.null(mprobloss)) mprobloss <- probloss[!is.na(mcghcall)]
					if(!is.null(mprobnorm)) mprobnorm <- mprobnorm[!is.na(mcghcall)]
					if(!is.null(mprobgain)) mprobgain <- mprobgain[!is.na(mcghcall)]
					if(!is.null(mprobamp)) mprobamp <- mprobamp[!is.na(mcghcall)]
					mcghcall <- mcghcall[!is.na(mcghcall)]
				}
				val <- sort(unique(mcghcall))
				nval <- length(val)
				chg.pt <- .estimateKnots(cghseg = mcghseg, cghcall = mcghcall,
     	                                     probloss = mprobloss, probnorm = mprobnorm,
                                           probgain = mprobgain, probamp = mprobamp)
				if(sum(order(chg.pt)==1:length(chg.pt))!=length(chg.pt)){
					warning("Cannot fit a plrs model: estimated knots are non-ordered (this may be caused by strong outliers).")
				}
				# Recalling
				cl <- rep(val[1],length(mcghcall))
				if(length(chg.pt)>=1) cl[mcghseg>=chg.pt[1]] <- val[2]
				if(length(chg.pt)>=2) cl[mcghseg>=chg.pt[2]] <- val[3]
				if(length(chg.pt)>=3) cl[mcghseg>=chg.pt[3]] <- val[4]
				names(cl) <- names(mcghcall)
			}
			mcghcall <- cl
			rm(cl)
		}
		else{
			if(!is.numeric(knots)) stop("'knots' is not correct")
			if(length(knots)!=(nval-1))  stop("wrong number of knots")
			chg.pt <- knots
		}

		# continuous
		if(is.null(continuous) || !is.logical(continuous) ||
              (is.vector(continuous) & length(continuous)>1)) stop("'continuous' is not correct")

		# constr
		if(!(is.logical(constr) & length(constr)==1)) stop("'constr' must be a logical")
		else{
			# constr.slopes
			if(!is.null(constr.slopes)){
				if(!(is.numeric(constr.slopes) & length(constr.slopes)==1))
					stop("'constr.slopes' must be a numeric")
				if(sum(constr.slopes%in%c(1,2))!=length(constr.slopes))
					stop("wrong type of constraints for slopes")
			}
			else{
				constr.slopes <- 2
			}

			# constr.intercepts
			if(!is.null(constr.intercepts)){
				if(!(is.logical(constr.intercepts) & length(constr.intercepts)==1))
					stop("'constr.intercepts' must be a logical")
			}
			else{
				constr.intercepts <- FALSE
			}
			if(constr & !constr.intercepts & is.null(constr.slopes)) constr <- FALSE
		}
	}


	### Fit (constrained) plrs
	if(nval!=1){
		# Unconstrained model
		if(!constr){
			# Design matrix
			X <- .Bmat(mcghseg, knots = chg.pt, 
				    continuous = continuous,
				    general.intercept = TRUE)

			# fit
			#sol <- lm.fit(x=X, y=em)$coef
			lm.obj <- lm(mexpr~.-1, data=as.data.frame(cbind(mexpr,X)))
			sol <- coef(lm.obj)
			cov.mat <- vcov(lm.obj)
			S2 <- summary(lm.obj)$sigma^2
			df.error <- lm.obj$df.res
		}
		# Constrained model
		else{
			# Design matrix
			X <- .Bmat(mcghseg, knots=chg.pt, 
				    continuous=continuous,
				    general.intercept=TRUE)

			# Input solve.QP
			Dmat <- crossprod(X,X)
			dvec <- crossprod(X,mexpr)

			# Get matrix of constraints
			get.mat <- .make.mat.constr(constr.intercepts=constr.intercepts, constr.slopes=constr.slopes, continuous=continuous, val=val)

			# Input
			Amat <- t(get.mat)
			bvec <- rep(0, nrow(get.mat))

			## Quadratic programming
			sol <- solve.QP(Dmat, as.vector(dvec), Amat, bvec, meq = 0)$sol
			names(sol) <- colnames(X)
		}
	}
	### Fit simple linear model
	else{
		# Design matrix
		X <- cbind(1,mcghseg)
		colnames(X) <- c("theta0.0","theta0.1")

		# Unconstrained
		if(!constr){
			#sol <- lm.fit(x=X, y=mexpr)$coef
			lm.obj <- lm(mexpr~.-1, data=as.data.frame(cbind(mexpr,X)))
			sol <- coef(lm.obj)
			cov.mat <- vcov(lm.obj)
			S2 <- summary(lm.obj)$sigma^2
			df.error <- lm.obj$df.res
		}
		# Constrain slope to be positive or null
		else{
			# Input
			Dmat <- crossprod(X,X)
			dvec <- crossprod(X,mexpr)
			Amat <- t(t(c(0,1)))
			bvec <- 0
			meq <- 0
			
			## Quadratic programming
			sol <- solve.QP(Dmat, as.vector(dvec), Amat, bvec, meq = meq)$sol

			names(sol) <- colnames(X)
		}
		chg.pt <- numeric()
	}


	### Output plrs object
	fit.val <- as.vector(crossprod(t(X),sol))
	names(fit.val) <- names(mexpr)
	dat <- list("expr" = expr, "cghseg" = cghseg, "cghcall" = cghcall,
                  "probloss" = probloss, "probnorm" = probnorm, "probgain" = "probgain", "probamp" = probamp,
                  "conf" = conf, "knots" = chg.pt)

	mdat <- list("mexpr" = mexpr, "mcghseg" = mcghseg, "mcghcall" = mcghcall,
                   "mprobloss" = mprobloss, "mprobnorm" = mprobnorm, "mprobgain" = mprobgain, "mprobamp" = mprobamp,
                   "mconf" = val, "mknots" = chg.pt)

	if(constr){
		qp <- list("Dmat" = Dmat, "dvec" = dvec, "Amat" = Amat,
					"bvec" = bvec, "meq" = 0)
	}
	else{
		qp <- list("vcov" = cov.mat, "S2" = S2, "df.error" = df.error)
	}

	## Type of model: Functions needed
	# Identify type of model based on estimated parameters.
	is.intercept <- function(x){
		if(("theta0.0" %in% names(x)) & length(x)==1){
			return(TRUE)
		}
		else{
			return(FALSE)
		}
	}
	is.linear <- function(x){
		if(all(c("theta0.0","theta0.1") %in% names(x)) & length(x)==2){
			return(TRUE)
		}
		else{
			return(FALSE)
		}
	}
	is.piecewise <- function(x){
		if(!is.intercept(x) & !is.linear(x)){
			return(TRUE)
		}
		else{
			return(FALSE)
		}
	}
	is.piecewise.level <- function(x){
		IDintercpts <- as.numeric(substr(names(x),8,9))
		if(is.piecewise(x) & sum(IDintercpts)==0){
			return(TRUE)
		}
		else{
			return(FALSE)
		}
	}
	# Def: is piecewise linear if it is piecewise and not level
	is.piecewise.linear <- function(x){
		if(is.piecewise(x) & !is.piecewise.level(x)){
			return(TRUE)
		}
		else{
			return(FALSE)
		}
	}
	if(is.intercept(sol)) model.type <- "Intercept"
	if(is.linear(sol)) model.type <- "Linear"
	if(is.piecewise.level(sol)) model.type <- "Piecewise level"
	if(is.piecewise.linear(sol)) model.type <- "Piecewise linear"

	call.arg <- list("continuous" = continuous,
			     "constr" = constr,
			     "constr.intercepts" = constr.intercepts,
			     "constr.slopes" = constr.slopes,
			     "config" = paste(as.character(val),collapse=","))
	out <- new("plrs",
			coefficients = sol,
			fitted.values = fit.val,
			residuals = mexpr - fit.val,
			X = X,
			data = dat,
			mdata = mdat,
			QP = qp,
			test = list(),
			cb = list(),
			selected = FALSE,
			type = model.type,
			call.arg = call.arg)

	return(out)
}
