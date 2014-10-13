# Fit plrs models for a series of arrays

# Author: Gwenael G.R. Leday

plrs.series <- function(expr, cghseg, cghcall=NULL,
				probloss = NULL,
				probnorm = NULL,
				probgain = NULL,
				probamp = NULL,
				control.model = list(continuous=FALSE, constr=TRUE, constr.slopes=2, constr.intercepts=TRUE, min.obs=3, discard.obs=TRUE),
				control.select = list(crit = ifelse(control.model$constr,"osaic","aic")),
				control.test = list(testing=TRUE, cb=FALSE, alpha=0.05),
				control.output = list(save.models = FALSE, save.plots = FALSE, plot.lin = FALSE, type = "jpeg")){

	#### Check input
	if(class(expr)=="ExpressionSet"){
		expr <- exprs(expr)
	}
	if(class(cghseg)=="cghSeg"){
		cghseg <- segmented(cghseg)
		cghcall <- NULL
	}
	if(class(cghseg)=="cghCall"){
		cghcall <- CGHbase:::calls(cghseg)
		if(sum(dim(cghcall)==c(0,0))!=0){
			cghcall <- NULL
			probloss <- probnorm <- probgain <- probamp <- NULL
			cghseg <- segmented(cghseg)
		}
		else{
			probloss <- probloss(cghseg)
			probnorm <- probnorm(cghseg)
			probgain <- probgain(cghseg)
			probamp <- probamp(cghseg)
			cghseg <- segmented(cghseg)
		}
	}
	if(!(is.matrix(expr) & is.matrix(cghseg))){
		stop("input data have to be in matrix format")
	}
	else{
		if(any(is.na(expr))) stop("Missing values are not allowed in expr")
		if(!sum(dim(expr)==dim(cghseg))==2)
			stop("dimensions of input data are not the same")
	}
	if(is.null(cghcall)) cghcall <- matrix(0, nrow(expr), ncol(expr), dimnames=dimnames(expr))
	if(!is.matrix(cghcall)){
		stop("input data have to be in matrix format")
	}
	else{
		if(!sum(dim(expr)==dim(cghcall))==2)
			stop("dimensions of input data are not the same")
	}
	# control.model
	if(!is.list(control.model)) stop("control.model is not a list")
	if(!"continuous"%in%names(control.model)) control.model$continuous <- FALSE
	if(!"constr"%in%names(control.model)) control.model$constr <- TRUE
	if(!"constr.slopes"%in%names(control.model)) control.model$constr.slopes <- 2
	if(!"constr.intercepts"%in%names(control.model)) control.model$constr.intercepts <- TRUE
	if(!"min.obs"%in%names(control.model)) control.model$min.obs <- 3
	if(!"discard.obs"%in%names(control.model)) control.model$discard.obs <- TRUE
	# control.select
	if(!is.list(control.select) & !is.null(control.select)) stop("control.select is not a list")
	if(length(control.select)>0){
		if(!"crit"%in%names(control.select)) control.select$crit <- ifelse(control.model$constr,"osaic","aic")
	}
	# control.test
	if(!is.list(control.test)) stop("control.test is not a list")
	if(!"testing"%in%names(control.test)) control.test$testing <- TRUE
	if(!"cb"%in%names(control.test)) control.test$cb <- FALSE
	if(!"alpha"%in%names(control.test)) control.test$alpha <- FALSE
	if(!control.test$testing & control.test$cb) control.test$testing <- TRUE
	# control.output
	if(!is.list(control.output)) stop("control.output is not a list")
	if(!"save.models"%in%names(control.output)) control.output$save.models <- FALSE
	if(!"save.plots"%in%names(control.output)) control.output$save.plots <- FALSE
	if(!"plot.lin"%in%names(control.output)) control.output$plot.lin <- FALSE
	if(!"type"%in%names(control.output)) control.output$type <- "jpeg"


	# Matrix of coefficients
	ngenes <- nrow(expr)
	est <- matrix(NA, ngenes, 8)
	nam1 <- colnames(est) <- c("theta0.0", "theta0.1", "theta1.0", "theta1.1", "theta2.0", "theta2.1", "theta3.0", "theta3.1")
	rownames(est) <- paste("gene",1:ngenes,sep="")

	# Matrix of model types
	modelTypes <- matrix(F, ngenes, 4)
	colnames(modelTypes) <- c("Intercept", "Linear", "Piecewise level", "Piecewise linear")
	rownames(modelTypes) <- rownames(est)

	# Matrix of effects
	Ieff <- Seff <- matrix(F, ngenes, 4)
	nam3 <- colnames(Ieff) <- colnames(Seff) <- c("Loss", "Norm.", "Gain", "Ampl.")
	rownames(modelTypes) <- rownames(est)

	# Testing
	if(control.test$testing){
		res.test <- matrix(NA, ngenes, 3, dimnames=list(rownames(est), c("stat","raw.pval","BH.adj.pval")))
	}

	# Summary matrix on aberrations
	config <- matrix(NA, ngenes, 4)
	colnames(config) <- as.character(-1:2)
	rownames(config) <- rownames(est)

	# Create directories for output
	if(control.output$save.plots) dir.create("plrsSeriesPlots")
	if(control.output$save.models) dir.create("plrsSeriesObjects")
	if(is.null(rownames(expr))){
		lab <- 1:nrow(expr)
	}
	else{
		lab <- rownames(expr)
	}

	# Relabel rownames duplicates
	ind <- duplicated(lab)
	cp <- 1
	while(sum(ind)!=0){
		lab[ind] <- paste(lab[ind], "_", cp, sep="")
		ind <- duplicated(lab)
		if(sum(ind)!=0) lab[ind] <- substr(lab[ind], 1, nchar(lab[ind])-2)
		cp <- cp + 1
	}

	if(ngenes>100){
		cat("In progress... \n\n")
		toprint <- round(quantile(1:ngenes, probs=seq(0, 1, 0.10))[-1])
		t <- proc.time()
		ct <- 1
	}

	for(x in 1:ngenes){
		if(ngenes>100){
			if(x==toprint[ct]){
				xx <- (proc.time() - t)[3]
				cat(names(toprint[ct]), " done (", toprint[ct]," genes),\t  ",
					"time elapsed = ", .convertToTime(xx),"\n", sep="")
				ct <- ct + 1
			}
		}
#cat("i = ", x, "\n")
		error <- try(
		model <- plrs(expr=expr[x,], cghseg=cghseg[x,], cghcall=cghcall[x,],
                          probloss = probloss[x,],
                          probnorm = probnorm[x,],
                          probgain = probgain[x,],
                          probamp = probamp[x,],
                          continuous = control.model$continuous,
                          constr = control.model$constr,
                          constr.slopes = control.model$constr.slopes,
                          constr.intercepts = control.model$constr.intercepts,
                          min.obs = control.model$min.obs,
                          discard.obs = control.model$discard.obs)
		,T)
		if(!inherits(error, "try-error")){
			vec <- as.vector(table(model@data$cghcall))
			config[x,colnames(config)%in%as.character(model@data$conf)] <- vec
			if(!is.null(control.select)) model <- plrs.select(model, crit = control.select$crit)@model
			est[x,nam1%in%names(coef(model))] <- coef(model)
			modelTypes[x,colnames(modelTypes)%in%model@type] <- TRUE
			tabb <- effects(model)
			indeff <- !is.na(tabb[,2])
			Ieff[x,indeff] <- tabb[indeff,2]
			Seff[x,indeff] <- tabb[indeff,3]

			if(control.test$testing || control.test$cb){
				model <- plrs.test(model)
				if(length(model@test)!=0){
					res.test[x,1] <- model@test$stat
					res.test[x,2] <- model@test$pvalue
					if(control.test$cb){
						model <- plrs.cb(model, alpha=0.05)
					}
				}
			}
			if(control.output$save.models){
				save(model, file = paste("plrsSeriesObjects/gene_",lab[x],".RData",sep=""))
			}
			if(control.output$plot.lin || control.output$save.plots){
				if(control.output$save.plots){
					if(control.output$type=="pdf") pdf(paste("plrsSeriesPlots/gene",lab[x],".pdf",sep=""))
					if(control.output$type=="jpeg") jpeg(paste("plrsSeriesPlots/gene",lab[x],".jpeg",sep=""))
				}
				plot(model, lin=control.output$plot.lin)
				if(control.output$save.plots) dev.off()
			}
		}
	}
	est <- round(est,8)

	### Distribution of nb observations
	general <- apply(config, 2, function(x)sum(!is.na(x)))
	tp <- matrix(NA, 4, 4)
	for(d in 1:ncol(config)){
		if(general[d]!=0) tp[,d] <- summary(config[,d])[c(1,3,4,6)]
	}
	general <- rbind(general, tp)
	colnames(general) <- colnames(config)
	rownames(general) <- c("Nb genes","Min. obs","Median. obs","Mean. obs","Max. obs")
	if(sum(general[1,]!=0)==1){
		general <- t(as.matrix(general[1,]))
		rownames(general) <- "Nb genes"
	}

	### Type of models
	type.model <- as.matrix(colSums(modelTypes))
	rownames(type.model) <- c("Intercept", "Simple linear", "Piecewise level", "Piecewise linear")
	colnames(type.model) <- "NbModels"

	### Testing
	if(control.test$testing) res.test[,3] <- p.adjust(res.test[,2], method = "BH")
	else{
		res.test <- matrix(nrow=0,ncol=0)
	}

	cat("\n")
	out <- new("plrs.series",
			coefficients = est,
			effects = list(I=Ieff, S=Seff),
			test = res.test,
			general = general,
			modelsType = list(summary=type.model, all=modelTypes),
			call.arg = list(control.model=control.model, control.select=control.select))
	return(out)
}

