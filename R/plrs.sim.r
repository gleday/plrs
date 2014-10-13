# Generate PLRS relationships

plrs.sim <- function(n=80, states=4, sigma=1, x=NULL){

	if(states==1){
		int <- 1
		slp <- 1
		knot <- NULL
		if(is.null(x)){
			x <- runif(round(n), min = 0, max = 1)
			x <- x[sample(1:length(x))]
		}
		y <- int[1] + slp[1]*x + rnorm(n, mean = 0, sd = sigma)
		cal <- rep(0,length(x))

		xtrue <- seq(0, 1, length=100)
		ytrue <- int[1] + slp[1]*xtrue
	}
	if(states==2){
		int <- c(1,1.5)
		slp <- c(0,1)
		knot <- 0.5
		if(is.null(x)){
			x1 <- runif(round(n/2), min = 0, max = 0.5)
			x2 <- runif(round(n/2), min = 0.5, max = 1)
			x <- c(x1,x2)
		}
		y <- x
		y[x<=0.5] <- slp[1]*x[x<=0.5] + int[1]
		y[x>0.5] <- slp[2]*x[x>0.5] + int[2] - slp[2]*0.5
		y <- y + rnorm(n, mean = 0, sd = sigma)
		cal <- rep(0,n)
		cal[x>=0.5] <- 1
		
		xtrue <- seq(0, 1, length=100)
		ytrue <- y
		ytrue[xtrue<=0.5] <- slp[1]*xtrue[xtrue<=0.5] + int[1]
		ytrue[xtrue>0.5] <- slp[2]*xtrue[xtrue>0.5] + int[2] - slp[2]*0.5
	}
	if(states==3){
		int <- c(1,1.5,1.5)
		slp <- c(1,0,1)
		knot <- c(0.5,1)
		if(is.null(x)){
			x1 <- runif(round(n/3), min = 0, max = 0.5)
			x2 <- runif(round(n/3), min = 0.5, max = 1)
			x3 <- runif(round(n/3), min = 1, max = 1.5)
			x <- c(x1,x2,x3)
		}

		y <- x
		y[x<=0.5] <- slp[1]*x[x<=0.5] + int[1]
		y[x>0.5&x<=1] <- slp[2]*x[x>0.5&x<=1] + int[2] - slp[2]*0.5
		y[x>1] <- slp[3]*x[x>1] + int[3] - slp[3]*0.5
		y <- y + rnorm(n, mean = 0, sd = sigma)
		cal <- rep(-1,n)
		cal[x>0.5&x<=1] <- 0
		cal[x>1] <- 1

		xtrue <- seq(0, 1.5, length=100)
		ytrue <- y
		ytrue[xtrue<=0.5] <- slp[1]*xtrue[xtrue<=0.5] + int[1]
		ytrue[xtrue>0.5&xtrue<=1] <- slp[2]*xtrue[xtrue>0.5&xtrue<=1] + int[2] - slp[2]*0.5
		ytrue[xtrue>1] <- slp[3]*xtrue[xtrue>1] + int[3] - slp[3]*0.5
	}
	if(states==4){
		int <- c(1,1.5,1.5,1.75)
		slp <- c(1,0,1,2)
		knot <- c(0.5,1,1.5)
		if(is.null(x)){
			x1 <- runif(round(n/4), min = 0, max = 0.5)
			x2 <- runif(round(n/4), min = 0.5, max = 1)
			x3 <- runif(round(n/4), min = 1, max = 1.5)
			x4 <- runif(round(n/4), min = 1.5, max = 2)
			x <- c(x1,x2,x3,x4)
		}

		y <- x
		y[x<=0.5] <- slp[1]*x[x<=0.5] + int[1]
		y[x>0.5&x<=1] <- slp[2]*x[x>0.5&x<=1] + int[2]- slp[2]*0.5
		y[x>1&x<=1.5] <- slp[3]*x[x>1&x<=1.5] + int[3] - slp[3]*0.5
		y[x>1.5] <- slp[4]*x[x>1.5] + int[4] - slp[4]*1
		y <- y + rnorm(n, mean = 0, sd = sigma)
		cal <- rep(-1,n)
		cal[x>0.5&x<=1] <- 0
		cal[x>1&x<=1.5] <- 1
		cal[x>1.5] <- 2

		xtrue <- seq(0, 2, length=100) #c(seq(from=0.3, to=0.7, length=10),seq(from=0.8, to=1.2, length=10),seq(from=1.3, to=1.7, length=10))

		ytrue <- rep(0, length(xtrue))
		ytrue[xtrue<=0.5] <- slp[1]*xtrue[xtrue<=0.5] + int[1]
		ytrue[xtrue>0.5&xtrue<=1] <- slp[2]*xtrue[xtrue>0.5&xtrue<=1] + int[2] - slp[2]*0.5
		ytrue[xtrue>1&xtrue<=1.5] <- slp[3]*xtrue[xtrue>1&xtrue<=1.5] + int[3] - slp[3]*0.5
		ytrue[xtrue>1.5] <- slp[4]*xtrue[xtrue>1.5] + int[4] - slp[4]*1
	}

	out <- list(seg=x, expr=y, cal=cal, knots=knot, xtrue=xtrue, ytrue=ytrue)
	return(out)
}