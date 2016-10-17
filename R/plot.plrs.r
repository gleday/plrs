# Plot data points, model fit and confidence bands

# Author: Gwenael G.R. Leday

plot.plrs <- function(x,
                      col.line = "black",
                      col.pts = c("red", "blue", "green2", "green4"),
                      col.cb = "yellow",
                      xlim = c(floor(min(x@data$cghseg)),ceiling(max(x@data$cghseg))),
                      ylim = c(floor(min(x@data$expr)),ceiling(max(x@data$expr))),
                      pch = 16, lwd=4, cex = 1.2,
                      xlab="", ylab="", main = "",                      
                      add = FALSE,
                      lty = 1,
                      lin = FALSE,...){

	# Error handling
	if(class(x)!="plrs") stop("An object of class \"plrs\" is required")

	# Get data and set colors per state
	seg <- x@data$cghseg
	rna <- x@data$expr
	called <- x@data$cghcall
	val  <- x@data$conf
	nval <- length(val)
	pl <- FALSE
	if(x@type=="Linear" || x@type=="Intercept") pl <- TRUE
	if(!is.vector(col.pts)) stop("col.pts is not correct")
	if(length(col.pts)!=1 & length(col.pts)!=4){
		warning("col.pts length wrong: set to default")
		col.pts <- c("red", "blue", "green2", "green4")
	}
	colr <- rep(col.pts[1],length(called))
	if(length(col.pts)>1){
		for(i in val) colr[called==i] <- col.pts[c(-1,0,1,2)==i]
	}
	if(length(pch)>1){
		if(length(pch)!=4) stop("Either one or four values as input")
		pchtp <- rep(pch[1],length(called))
		for(i in val) pchtp[called==i] <- pch[c(-1,0,1,2)==i]
	}
	else{
		pchtp <- rep(pch,length(called))
	}
	if(length(x@cb)!=0){
		xCB <- x@cb$x
		ylbCB <- x@cb$inf
		yubCB <- x@cb$sup
	}

	# Main plot
	if(!add){
		plot(seg, rna,
                 main = main,
                 xlab = xlab, ylab = ylab,
                 ylim = ylim, xlim = xlim,
                 pch = pchtp, cex = cex, lwd=lwd,
                 col = colr)
	}

	# Add model
	if(!pl){
		xx <- NULL
		if(!is.null(knots(x))) xx <- c(knots(x)+1e-5,knots(x)-1e-5)
		xx <- c(xx, seq(from=xlim[1]-0.5, to=xlim[2]+0.5, length=50-length(knots(x))))
		if(!is.null(knots(x))) xx[xx%in%knots(x)] <- xx[xx%in%knots(x)] + 1e-4
		xx <- sort(xx)
		yy <- predict(x, newcghseg=xx)
		nknots <- length(knots(x))
		xLine <- xx
		yLine <- yy
		xPts <- seg
		yPts <- rna
		ct <- 0
		bool <- TRUE
		while(bool){
			ct <- ct + 1
			if(ct<=nknots){
				# cb
				if(length(x@cb)!=0){
					indCB <- (xCB<knots(x)[ct]) & (!is.na(xCB))
					polygon(c(xCB[indCB],xCB[indCB][length(xCB[indCB]):1]), c(ylbCB[indCB], yubCB[indCB][length(yubCB[indCB]):1]), col = col.cb, border = NA)
					xCB[indCB] <- NA
					ylbCB[indCB] <- NA
					yubCB[indCB] <- NA
				}

				# Data points
				indPts <- (xPts<knots(x)[ct]) & (!is.na(xPts))
				points(xPts[indPts], yPts[indPts], pch = pchtp[indPts], cex = cex, lwd=lwd, col = colr[indPts])
				xPts[indPts] <- NA
				yPts[indPts] <- NA

				# Line
				indLine <- (xLine<knots(x)[ct]) & (!is.na(xLine))
				lines(xLine[indLine], yLine[indLine], col=col.line, lwd=lwd, lty=lty)
				xLine[indLine] <- NA
				yLine[indLine] <- NA
			}
			else{
				# cb
				if(length(x@cb)!=0){
					indCB <- !is.na(xCB)
					polygon(c(xCB[indCB],xCB[indCB][length(xCB[indCB]):1]), c(ylbCB[indCB], yubCB[indCB][length(yubCB[indCB]):1]), col = col.cb, border = NA)
					xCB[indCB] <- NA
					ylbCB[indCB] <- NA
					yubCB[indCB] <- NA
				}

				# Data points
				indPts <- !is.na(xPts)
				points(xPts[indPts], yPts[indPts], pch = pchtp[indPts], cex = cex, lwd=lwd, col = colr[indPts])
				xPts[indPts] <- NA
				yPts[indPts] <- NA

				# Line
				indLine <- !is.na(xLine)
				lines(xLine[indLine], yLine[indLine], col=col.line, lwd=lwd, lty=lty)
				xLine[indLine] <- NA
				yLine[indLine] <- NA
			}
			if(all(is.na(xLine))){
				bool <- FALSE
				box()
			}
		}
		if(lin){
			# Plot simple line
			obj <- plrs(expr=rna, cghseg=seg, cghcall=NULL,
			            constr = x@call.arg$constr)
			plot(obj, add=TRUE, col.line=col.line, lty=2, lwd=lwd)
		}
	}
	else{
		if(length(x@cb)!=0){
			polygon(c(xCB,xCB[length(xCB):1]), c(ylbCB, yubCB[length(yubCB):1]), col = col.cb, border = NA)
			points(seg, rna, pch = pchtp, cex = cex, lwd=lwd, col = colr)
			box()
		}
		if(length(coef(x))==2) abline(a=coef(x)[1] , b=coef(x)[2], lwd=lwd, col=col.line, lty=lty)
		if(length(coef(x))==1) abline(a=coef(x)[1] , b=0, lwd=lwd, col=col.line, lty=lty)
	}
}
