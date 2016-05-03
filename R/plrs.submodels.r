# Internal function
# Generate all submodels (all design and constraint matrices)

# Author: Gwenael G.R. Leday

.plrs.submodels <- function(object){

	constr.slopes <- object@call.arg$constr.slopes
	constr.intercepts <- object@call.arg$constr.intercepts
	continuous <- object@call.arg$continuous
	val <- object@mdata$mconf
	nval <- length(val)

	if(!object@call.arg$constr){ # If no constraints make standard search
		constr.slopes <- 1
		allConstr <- NULL
	}
	if(nval==1){
		allLabs <- list("theta0.0")
		allConstr <- list(NULL)
		allTypes <- "Intercept"
	}
	if(nval==2){
		if(all(val==c(-1,1))|all(val==c(-1,2))|all(val==c(1,2))) constr.slopes <- 1
		if(constr.slopes==2){
			if(all(val==c(-1,0))){
				if(!continuous){
					allLabs <- list("theta0.0", c("theta0.0", "theta0.1"), c("theta0.0", "theta1.0"),
                                            c("theta0.0", "theta0.1", "theta1.0"), c("theta0.0", "theta0.1", "theta1.1"))
					allTypes <- c("Intercept", "Linear", "Piecewise level", "Piecewise linear", "Piecewise linear")
					if(constr.intercepts){
						# loss-normal: 5 submodels
						allConstr <- list(NULL,
									matrix(c(0,1),1,2),
									matrix(c(0,1),1,2),
									matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
									matrix(c(0,1,1,0,0,-1),2,3, byrow=TRUE))
					}
					else{
						# loss-normal: 5 submodels
						allConstr <- list(NULL,
									matrix(c(0,1),1,2),
									NULL,
									matrix(c(0,1,0),1,3, byrow=TRUE),
									matrix(c(0,1,1,0,0,-1),2,3, byrow=TRUE))
					}
				}
				else{
					# loss-normal: 3 submodels
					allLabs <- list("theta0.0", c("theta0.0", "theta0.1"), c("theta0.0", "theta0.1", "theta1.1"))
					allConstr <- list(NULL,
								matrix(c(0,1),1,2),
								matrix(c(0,1,1,0,0,-1),2,3, byrow=TRUE))
					allTypes <- c("Intercept", "Linear", "Piecewise linear")
				}
			}
			if(all(val==c(0,1))|all(val==c(0,2))){
				if(!continuous){
					allLabs <- list("theta0.0", c("theta0.0", "theta0.1"), c("theta0.0", "theta1.0"),
                                            c("theta0.0", "theta1.1"), c("theta0.0", "theta0.1", "theta1.0"),
                                            c("theta0.0", "theta0.1", "theta1.1"), c("theta0.0", "theta1.0", "theta1.1"))
					allTypes <- c("Intercept", "Linear", "Piecewise level", "Piecewise linear",
                                            "Piecewise linear", "Piecewise linear", "Piecewise linear")
					if(constr.intercepts){
						# normal-gain: 7 submodels
						allConstr <- list(NULL,
									matrix(c(0,1),1,2),
									matrix(c(0,1),1,2),
									matrix(c(0,1),1,2),
									matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
									matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
									matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE))
					}
					else{
						# normal-gain: 7 submodels
						allConstr <- list(NULL,
									matrix(c(0,1),1,2),
									NULL,
									matrix(c(0,1),1,2),
									matrix(c(0,1,0),1,3, byrow=TRUE),
									matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
									matrix(c(0,0,1),1,3, byrow=TRUE))
					}
				}
				else{
					# normal-gain: 4 submodels
					allLabs <- list("theta0.0", c("theta0.0", "theta0.1"), c("theta0.0", "theta1.1"), c("theta0.0", "theta0.1", "theta1.1"))
					allConstr <- list(NULL,
								matrix(c(0,1),1,2),
								matrix(c(0,1),1,2),
								matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE))
					allTypes <- c("Intercept", "Linear", "Piecewise linear", "Piecewise linear")
				}
			}
		}
		if(constr.slopes==1){
			if(!continuous){
				allLabs <- list("theta0.0", c("theta0.0", "theta0.1"), c("theta0.0", "theta1.0"),
                                      c("theta0.0", "theta1.1"), c("theta0.0", "theta0.1", "theta1.0"),
                                      c("theta0.0", "theta0.1", "theta1.1"), c("theta0.0", "theta1.0", "theta1.1"))
				allTypes <- c("Intercept", "Linear", "Piecewise level", "Piecewise linear",
                                      "Piecewise linear", "Piecewise linear", "Piecewise linear")
				if(object@call.arg$constr){
					if(constr.intercepts){
						# gain-amp: 7 submodels
						allConstr <- list(NULL,
									matrix(c(0,1),1,2),
									matrix(c(0,1),1,2),
									matrix(c(0,1),1,2),
									matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
									matrix(c(0,1,0,0,1,1),2,3, byrow=TRUE),
									matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE))
					}
					else{
						# gain-amp: 7 submodels
						allConstr <- list(NULL,
									matrix(c(0,1),1,2),
									NULL,
									matrix(c(0,1),1,2),
									matrix(c(0,1,0),1,3, byrow=TRUE),
									matrix(c(0,1,0,0,1,1),2,3, byrow=TRUE),
									matrix(c(0,0,1),1,3, byrow=TRUE))
					}
				}
			}
			else{
				# gain-amp: 4 submodels
				allLabs <- list("theta0.0", c("theta0.0", "theta0.1"), c("theta0.0", "theta1.1"), c("theta0.0", "theta0.1", "theta1.1"))
				if(object@call.arg$constr) allConstr <- list(NULL,
							matrix(c(0,1),1,2),
							matrix(c(0,1),1,2),
							matrix(c(0,1,0,0,1,1),2,3, byrow=TRUE))
				allTypes <- c("Intercept", "Linear", "Piecewise linear", "Piecewise linear", "Piecewise linear")
			}
		}
	}
	if(nval==3){
		if(all(val==c(-1,1,2))) constr.slopes <- 1 
		if(constr.slopes==2){
			if(all(val==c(-1,0,1))|all(val==c(-1,0,2))){
				if(!continuous){
					# loss-normal-gain: 23 models
					allLabs <- list("theta0.0",
						c("theta0.0", "theta0.1"),
						c("theta0.0", "theta1.0"),
						c("theta0.0", "theta2.0"),
						c("theta0.0", "theta2.1"),
						c("theta0.0", "theta0.1", "theta1.0"),
						c("theta0.0", "theta0.1", "theta1.1"),
						c("theta0.0", "theta0.1", "theta2.0"),
						c("theta0.0", "theta0.1", "theta2.1"),
						c("theta0.0", "theta1.0", "theta2.0"),
						c("theta0.0", "theta1.0", "theta2.1"),
						c("theta0.0", "theta2.0", "theta2.1"),
						c("theta0.0", "theta0.1", "theta1.0", "theta1.1"),
						c("theta0.0", "theta0.1", "theta1.0", "theta2.0"),
						c("theta0.0", "theta0.1", "theta1.0", "theta2.1"),
						c("theta0.0", "theta0.1", "theta1.1", "theta2.0"),
						c("theta0.0", "theta0.1", "theta1.1", "theta2.1"),
						c("theta0.0", "theta0.1", "theta2.0", "theta2.1"),
						c("theta0.0", "theta1.0", "theta2.0", "theta2.1"),
						c("theta0.0", "theta0.1", "theta1.0", "theta1.1", "theta2.0"),
						c("theta0.0", "theta0.1", "theta1.0", "theta1.1", "theta2.1"),
						c("theta0.0", "theta0.1", "theta1.0", "theta2.0", "theta2.1"),
						c("theta0.0", "theta0.1", "theta1.1", "theta2.0", "theta2.1"))
					allTypes <- c("Intercept", "Linear", "Piecewise level", "Piecewise level", "Piecewise linear",
                                               "Piecewise linear", "Piecewise linear", "Piecewise linear", "Piecewise linear",
                                               "Piecewise level", "Piecewise linear", "Piecewise linear", "Piecewise linear",
                                               "Piecewise linear", "Piecewise linear", "Piecewise linear", "Piecewise linear",
                                               "Piecewise linear", "Piecewise linear", "Piecewise linear", "Piecewise linear",
                                               "Piecewise linear", "Piecewise linear")
					if(constr.intercepts){
						allConstr <- list(NULL,
							matrix(c(0,1),1,2),
							matrix(c(0,1),1,2),
							matrix(c(0,1),1,2),
							matrix(c(0,1),1,2),
							matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
							matrix(c(0,1,1,0,0,-1),2,3, byrow=TRUE),
							matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
							matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
							matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
							matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
							matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
							matrix(c(0,1,0,1,0,0,0,-1,0,0,1,0),3,4, byrow=TRUE),
							matrix(c(0,1,0,0,0,0,1,0,0,0,0,1),3,4, byrow=TRUE),
							matrix(c(0,1,0,0,0,0,1,0,0,0,0,1),3,4, byrow=TRUE),
							matrix(c(0,1,1,0,0,0,-1,0,0,0,0,1),3,4, byrow=TRUE),
							matrix(c(0,1,1,0,0,0,-1,0,0,0,0,1),3,4, byrow=TRUE),
							matrix(c(0,1,0,0,0,0,1,0,0,0,0,1),3,4, byrow=TRUE),
							matrix(c(0,1,0,0,0,0,1,0,0,0,0,1),3,4, byrow=TRUE),
							matrix(c(0,1,0,1,0, 0,0,0,-1,0, 0,0,1,0,0, 0,0,0,0,1),4,5, byrow=TRUE),
							matrix(c(0,1,0,1,0, 0,0,0,-1,0, 0,0,1,0,0, 0,0,0,0,1),4,5, byrow=TRUE),
							matrix(c(0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,0,0,1),4,5, byrow=TRUE),		
							matrix(c(0,1,1,0,0, 0,0,-1,0,0, 0,0,0,1,0, 0,0,0,0,1),4,5, byrow=TRUE))
					}
					else{
						allConstr <- list(NULL,
							matrix(c(0,1),1,2),
							NULL,
							NULL,
							matrix(c(0,1),1,2),
							matrix(c(0,1,0),1,3, byrow=TRUE),
							matrix(c(0,1,1,0,0,-1),2,3, byrow=TRUE),
							matrix(c(0,1,0),1,3, byrow=TRUE),
							matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
							NULL,
							matrix(c(0,0,1),1,3, byrow=TRUE),
							matrix(c(0,0,1),1,3, byrow=TRUE),
							matrix(c(0,1,0,1,0,0,0,-1),2,4, byrow=TRUE),
							matrix(c(0,1,0,0),1,4, byrow=TRUE),
							matrix(c(0,1,0,0,0,0,0,1),2,4, byrow=TRUE),
							matrix(c(0,1,1,0,0,0,-1,0),2,4, byrow=TRUE),
							matrix(c(0,1,1,0,0,0,-1,0,0,0,0,1),3,4, byrow=TRUE),
							matrix(c(0,1,0,0,0,0,0,1),2,4, byrow=TRUE),
							matrix(c(0,0,0,1),2,4, byrow=TRUE),
							matrix(c(0,1,0,1,0, 0,0,0,-1,0),2,5, byrow=TRUE),
							matrix(c(0,1,0,1,0, 0,0,0,-1,0, 0,0,1,0,0),3,5, byrow=TRUE),
							matrix(c(0,1,0,0,0, 0,0,0,0,1),2,5, byrow=TRUE),		
							matrix(c(0,1,1,0,0, 0,0,-1,0,0, 0,0,0,0,1),3,5, byrow=TRUE))
					}
				}
				else{
					# loss-normal-gain: 6 models
					allLabs <- list("theta0.0",
                                            c("theta0.0", "theta0.1"),
                                            c("theta0.0", "theta2.1"),
                                            c("theta0.0", "theta0.1", "theta1.1"),
                                            c("theta0.0", "theta0.1", "theta2.1"),
                                            c("theta0.0", "theta0.1", "theta1.1", "theta2.1"))
					allTypes <- c("Intercept", "Linear", "Piecewise linear", "Piecewise linear", "Piecewise linear", "Piecewise linear")
					allConstr <- list(NULL,
                                                matrix(c(0,1),1,2),
                                                matrix(c(0,1),1,2),
                                                matrix(c(0,1,1,0,0,-1),2,3, byrow=TRUE),
                                                matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
                                                matrix(c(0,1,1,0,0,0,-1,0,0,0,0,1),3,4, byrow=TRUE))
				}
			}
			if(all(val==c(0,1,2))){
				if(!continuous){
					# normal-gain-amp: 31 models
					allLabs <- list("theta0.0",
                                              c("theta0.0", "theta0.1"),
                                              c("theta0.0", "theta1.0"),
                                              c("theta0.0", "theta1.1"),
                                              c("theta0.0", "theta2.0"),
                                              c("theta0.0", "theta2.1"),
                                              c("theta0.0", "theta0.1", "theta1.0"),
                                              c("theta0.0", "theta0.1", "theta1.1"),
                                              c("theta0.0", "theta0.1", "theta2.0"),
                                              c("theta0.0", "theta0.1", "theta2.1"),
                                              c("theta0.0", "theta1.0", "theta1.1"),
                                              c("theta0.0", "theta1.0", "theta2.0"),
                                              c("theta0.0", "theta1.0", "theta2.1"),
                                              c("theta0.0", "theta1.1", "theta2.0"),
                                              c("theta0.0", "theta1.1", "theta2.1"),
                                              c("theta0.0", "theta2.0", "theta2.1"),
                                              c("theta0.0", "theta0.1", "theta1.0", "theta1.1"),
                                              c("theta0.0", "theta0.1", "theta1.0", "theta2.0"),
                                              c("theta0.0", "theta0.1", "theta1.0", "theta2.1"),
                                              c("theta0.0", "theta0.1", "theta1.1", "theta2.0"),
                                              c("theta0.0", "theta0.1", "theta1.1", "theta2.1"),
                                              c("theta0.0", "theta0.1", "theta2.0", "theta2.1"),
                                              c("theta0.0", "theta1.0", "theta1.1", "theta2.0"),
                                              c("theta0.0", "theta1.0", "theta1.1", "theta2.1"),
                                              c("theta0.0", "theta1.0", "theta2.0", "theta2.1"),
                                              c("theta0.0", "theta1.1", "theta2.0", "theta2.1"),
                                              c("theta0.0", "theta0.1", "theta1.0", "theta1.1", "theta2.0"),
                                              c("theta0.0", "theta0.1", "theta1.0", "theta1.1", "theta2.1"),
                                              c("theta0.0", "theta0.1", "theta1.0", "theta2.0", "theta2.1"),
                                              c("theta0.0", "theta0.1", "theta1.1", "theta2.0", "theta2.1"),
                                              c("theta0.0", "theta1.0", "theta1.1", "theta2.0", "theta2.1"))
					allTypes <- c("Intercept", "Linear", "Piecewise level", "Piecewise linear", "Piecewise level",
                                            "Piecewise linear", "Piecewise linear", "Piecewise linear", "Piecewise linear",
                                            "Piecewise linear", "Piecewise linear", "Piecewise level", "Piecewise linear",
                                            "Piecewise linear", "Piecewise linear", "Piecewise linear", "Piecewise linear",
                                            "Piecewise linear", "Piecewise linear", "Piecewise linear", "Piecewise linear",
                                            "Piecewise linear", "Piecewise linear", "Piecewise linear", "Piecewise linear",
                                            "Piecewise linear", "Piecewise linear", "Piecewise linear", "Piecewise linear",
                                            "Piecewise linear", "Piecewise linear")
					if(constr.intercepts){
						allConstr <- list(NULL,
			                                    matrix(c(0,1),1,2),
			                                    matrix(c(0,1),1,2),
			                                    matrix(c(0,1),1,2),
			                                    matrix(c(0,1),1,2),
			                                    matrix(c(0,1),1,2),
			                                    matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
			                                    matrix(c(0,1,0,0,1,1),2,3, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0,0,1,0,0,0,0,1),3,4, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0,0,1,0,0,0,0,1),3,4, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0,0,1,0,0,0,0,1),3,4, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0,0,1,0,0,0,0,1),3,4, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0,0,1,0,0,0,1,1),3,4, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0,0,1,0,0,0,0,1),3,4, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0,0,1,0,0,0,0,1),3,4, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0,0,1,0,0,0,1,1),3,4, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0,0,1,0,0,0,0,1),3,4, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0,0,1,0,0,1,0,1),3,4, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,0,0,1),4,5, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,0,1,1),4,5, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,0,0,1),4,5, byrow=TRUE),		
			                                    matrix(c(0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,1,0,1),4,5, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,1,0,1),4,5, byrow=TRUE))
					}
					else{
						allConstr <- list(NULL,
			                                    matrix(c(0,1),1,2),
			                                    NULL,
			                                    matrix(c(0,1),1,2),
			                                    NULL,
			                                    matrix(c(0,1),1,2),
			                                    matrix(c(0,1,0),1,3, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
			                                    matrix(c(0,1,0),1,3, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
			                                    matrix(c(0,0,1),1,3, byrow=TRUE),
			                                    NULL,
			                                    matrix(c(0,0,1),1,3, byrow=TRUE),
			                                    matrix(c(0,1,0),1,3, byrow=TRUE),
			                                    matrix(c(0,1,0,0,1,1),2,3, byrow=TRUE),
			                                    matrix(c(0,0,1),1,3, byrow=TRUE),
			                                    matrix(c(0,1,0,0, 0,0,0,1),2,4, byrow=TRUE),
			                                    matrix(c(0,1,0,0),1,4, byrow=TRUE),
			                                    matrix(c(0,1,0,0, 0,0,0,1),2,4, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0,0,1,0),2,4, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0,0,1,0,0,0,1,1),3,4, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0,0,0,1),2,4, byrow=TRUE),
			                                    matrix(c(0,0,1,0),1,4, byrow=TRUE),
			                                    matrix(c(0,0,1,0,0,0,1,1),2,4, byrow=TRUE),
			                                    matrix(c(0,0,0,1),1,4, byrow=TRUE),
			                                    matrix(c(0,1,0,0, 0,1,0,1),2,4, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0, 0,0,0,1,0),2,5, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0, 0,0,0,1,0, 0,0,0,1,1),3,5, byrow=TRUE),
			                                    matrix(c(0,1,0,0,0, 0,0,0,0,1),2,5, byrow=TRUE),		
			                                    matrix(c(0,1,0,0,0, 0,0,1,0,0, 0,0,1,0,1),3,5, byrow=TRUE),
			                                    matrix(c(0,0,1,0,0, 0,0,1,0,1),2,5, byrow=TRUE))
					}
				}
				else{
					# normal-gain-amp: 8 models
					allLabs <- list("theta0.0",
                                            c("theta0.0", "theta0.1"),
                                            c("theta0.0", "theta1.1"),
                                            c("theta0.0", "theta2.1"),
                                            c("theta0.0", "theta0.1", "theta1.1"),
                                            c("theta0.0", "theta0.1", "theta2.1"),
                                            c("theta0.0", "theta1.1", "theta2.1"),
                                            c("theta0.0", "theta0.1", "theta1.1", "theta2.1"))
					allTypes <- c("Intercept", "Linear", "Piecewise linear", "Piecewise linear", "Piecewise linear",
                                            "Piecewise linear", "Piecewise linear", "Piecewise linear")
					allConstr <- list(NULL,
		                                    matrix(c(0,1),1,2),
		                                    matrix(c(0,1),1,2),
		                                    matrix(c(0,1),1,2),
		                                    matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
		                                    matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
		                                    matrix(c(0,1,0,0,1,1),2,3, byrow=TRUE),
		                                    matrix(c(0,1,0,0,0,0,1,0,0,0,1,1),3,4, byrow=TRUE))
				}
			}
		}
		if(constr.slopes==1){
			if(!continuous){
				# 31 models
				allLabs <- list(c("theta0.0"),
                                        c("theta0.0", "theta0.1"),
                                        c("theta0.0", "theta1.0"),
                                        c("theta0.0", "theta1.1"),
                                        c("theta0.0", "theta2.0"),
                                        c("theta0.0", "theta2.1"),
                                        c("theta0.0", "theta0.1", "theta1.0"),
                                        c("theta0.0", "theta0.1", "theta1.1"),
                                        c("theta0.0", "theta0.1", "theta2.0"),
                                        c("theta0.0", "theta0.1", "theta2.1"),
                                        c("theta0.0", "theta1.0", "theta1.1"),
                                        c("theta0.0", "theta1.0", "theta2.0"),
                                        c("theta0.0", "theta1.0", "theta2.1"),
                                        c("theta0.0", "theta1.1", "theta2.0"),
                                        c("theta0.0", "theta1.1", "theta2.1"),
                                        c("theta0.0", "theta2.0", "theta2.1"),
                                        c("theta0.0", "theta0.1", "theta1.0", "theta1.1"),
                                        c("theta0.0", "theta0.1", "theta1.0", "theta2.0"),
                                        c("theta0.0", "theta0.1", "theta1.0", "theta2.1"),
                                        c("theta0.0", "theta0.1", "theta1.1", "theta2.0"),
                                        c("theta0.0", "theta0.1", "theta1.1", "theta2.1"),
                                        c("theta0.0", "theta0.1", "theta2.0", "theta2.1"),
                                        c("theta0.0", "theta1.0", "theta1.1", "theta2.0"),
                                        c("theta0.0", "theta1.0", "theta1.1", "theta2.1"),
                                        c("theta0.0", "theta1.0", "theta2.0", "theta2.1"),
                                        c("theta0.0", "theta1.1", "theta2.0", "theta2.1"),
                                        c("theta0.0", "theta0.1", "theta1.0", "theta1.1", "theta2.0"),
                                        c("theta0.0", "theta0.1", "theta1.0", "theta1.1", "theta2.1"),
                                        c("theta0.0", "theta0.1", "theta1.0", "theta2.0", "theta2.1"),
                                        c("theta0.0", "theta0.1", "theta1.1", "theta2.0", "theta2.1"),
                                        c("theta0.0", "theta1.0", "theta1.1", "theta2.0", "theta2.1"))
				allTypes <- c("Intercept", "Linear", "Piecewise level", "Piecewise linear", "Piecewise level",
                                      rep("Piecewise linear", 6), "Piecewise level", rep("Piecewise linear", 19))
				if(object@call.arg$constr){
					if(constr.intercepts){
						allConstr <- list(NULL,
		                                          matrix(c(0,1),1,2),
		                                          matrix(c(0,1),1,2),
		                                          matrix(c(0,1),1,2),
      		                                    matrix(c(0,1),1,2),
      		                                    matrix(c(0,1),1,2),
      		                                    matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
      		                                    matrix(c(0,1,0,0,1,1),2,3, byrow=TRUE),
      		                                    matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
      		                                    matrix(c(0,1,0,0,1,1),2,3, byrow=TRUE),
      		                                    matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
      		                                    matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
      		                                    matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
      		                                    matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
      		                                    matrix(c(0,1,0,0,1,1),2,3, byrow=TRUE),
      		                                    matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
      		                                    matrix(c(0,1,0,0, 0,1,0,1, 0,0,1,0),3,4, byrow=TRUE),
      		                                    matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1),3,4, byrow=TRUE),
      		                                    matrix(c(0,1,0,0, 0,1,0,1, 0,0,1,0),3,4, byrow=TRUE),
      		                                    matrix(c(0,1,0,0, 0,1,1,0, 0,0,0,1),3,4, byrow=TRUE),
      		                                    matrix(c(0,1,0,0, 0,1,1,0, 0,1,1,1),3,4, byrow=TRUE),
      		                                    matrix(c(0,1,0,0, 0,1,0,1, 0,0,1,0),3,4, byrow=TRUE),
      		                                    matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1),3,4, byrow=TRUE),
      		                                    matrix(c(0,1,0,0, 0,0,1,0, 0,0,1,1),3,4, byrow=TRUE),
      		                                    matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1),3,4, byrow=TRUE),
      		                                    matrix(c(0,1,0,0, 0,1,0,1, 0,0,1,0),3,4, byrow=TRUE),
      		                                    matrix(c(0,1,0,0,0, 0,1,0,1,0, 0,0,1,0,0, 0,0,0,0,1),4,5, byrow=TRUE),
      		                                    matrix(c(0,1,0,0,0, 0,1,0,1,0, 0,1,0,1,1, 0,0,1,0,0),4,5, byrow=TRUE),
      		                                    matrix(c(0,1,0,0,0, 0,1,0,0,1, 0,0,1,0,0, 0,0,0,1,0),4,5, byrow=TRUE),
      		                                    matrix(c(0,1,0,0,0, 0,1,1,0,0, 0,1,1,0,1, 0,0,0,1,0),4,5, byrow=TRUE),
      		                                    matrix(c(0,1,0,0,0, 0,0,1,0,0, 0,0,1,0,1, 0,0,0,1,0),4,5, byrow=TRUE))
					}
					else{
						allConstr <- list(NULL,
      		                                    matrix(c(0,1),1,2),
      		                                    NULL,
      		                                    matrix(c(0,1),1,2),
      		                                    NULL,
      		                                    matrix(c(0,1),1,2),
      		                                    matrix(c(0,1,0),1,3, byrow=TRUE),
      		                                    matrix(c(0,1,0,0,1,1),2,3, byrow=TRUE),
      		                                    matrix(c(0,1,0),1,3, byrow=TRUE),
      		                                    matrix(c(0,1,0,0,1,1),2,3, byrow=TRUE),
      		                                    matrix(c(0,0,1),1,3, byrow=TRUE),
      		                                    NULL,
      		                                    matrix(c(0,0,1),1,3, byrow=TRUE),
      		                                    matrix(c(0,1,0),1,3, byrow=TRUE),
      		                                    matrix(c(0,1,0,0,1,1),2,3, byrow=TRUE),
      		                                    matrix(c(0,0,1),1,3, byrow=TRUE),
      		                                    matrix(c(0,1,0,0, 0,1,0,1),2,4, byrow=TRUE),
      		                                    matrix(c(0,1,0,0),1,4, byrow=TRUE),
      		                                    matrix(c(0,1,0,0, 0,1,0,1),2,4, byrow=TRUE),
      		                                    matrix(c(0,1,0,0, 0,1,1,0),2,4, byrow=TRUE),
      		                                    matrix(c(0,1,0,0, 0,1,1,0, 0,1,1,1),3,4, byrow=TRUE),
      		                                    matrix(c(0,1,0,0, 0,1,0,1),2,4, byrow=TRUE),
      		                                    matrix(c(0,0,1,0),1,4, byrow=TRUE),
      		                                    matrix(c(0,0,1,0, 0,0,1,1),2,4, byrow=TRUE),
      		                                    matrix(c(0,0,0,1),1,4, byrow=TRUE),
      		                                    matrix(c(0,1,0,0, 0,1,0,1),2,4, byrow=TRUE),
      		                                    matrix(c(0,1,0,0,0, 0,1,0,1,0),2,5, byrow=TRUE),
      		                                    matrix(c(0,1,0,0,0, 0,1,0,1,0, 0,1,0,1,1),3,5, byrow=TRUE),
      		                                    matrix(c(0,1,0,0,0, 0,1,0,0,1),2,5, byrow=TRUE),
      		                                    matrix(c(0,1,0,0,0, 0,1,1,0,0, 0,1,1,0,1),3,5, byrow=TRUE),
      		                                    matrix(c(0,0,1,0,0, 0,0,1,0,1, 0,0,0,1,0),3,5, byrow=TRUE))
					}
				}
			}
			else{
				allLabs <- list(c("theta0.0"),
                                        c("theta0.0", "theta0.1"),
                                        c("theta0.0", "theta1.1"),
                                        c("theta0.0", "theta2.1"),
                                        c("theta0.0", "theta0.1", "theta1.1"),
                                        c("theta0.0", "theta0.1", "theta2.1"),
                                        c("theta0.0", "theta1.1", "theta2.1"),
                                        c("theta0.0", "theta0.1", "theta1.1", "theta2.1"))
				if(object@call.arg$constr) allConstr <- list(NULL,
		                              matrix(c(0,1),1,2),
		                              matrix(c(0,1),1,2),
		                              matrix(c(0,1),1,2),
		                              matrix(c(0,1,0,0,1,1),2,3, byrow=TRUE),
		                              matrix(c(0,1,0,0,1,1),2,3, byrow=TRUE),
		                              matrix(c(0,1,0,0,1,1),2,3, byrow=TRUE),
		                              matrix(c(0,1,0,0, 0,1,1,0, 0,1,1,1),3,4, byrow=TRUE))
				allTypes <- c("Intercept", "Linear", "Piecewise linear", "Piecewise linear", 
                                      "Piecewise linear", "Piecewise linear", "Piecewise linear", "Piecewise linear")
			}
		}
	}
	if(nval==4){
		if(constr.slopes==2){
			if(!continuous){
				# 95 models
				allLabs <- list(
					c("theta0.0"),
					c("theta0.0", "theta0.1"),
					c("theta0.0", "theta1.0"),
					c("theta0.0", "theta2.0"),
					c("theta0.0", "theta2.1"),
					c("theta0.0", "theta3.0"),
					c("theta0.0", "theta3.1"),
					c("theta0.0", "theta0.1", "theta1.0"),
					c("theta0.0", "theta0.1", "theta1.1"),
					c("theta0.0", "theta0.1", "theta2.0"),
					c("theta0.0", "theta0.1", "theta2.1"),
					c("theta0.0", "theta0.1", "theta3.0"),
					c("theta0.0", "theta0.1", "theta3.1"),
					c("theta0.0", "theta1.0", "theta2.0"),
					c("theta0.0", "theta1.0", "theta2.1"),
					c("theta0.0", "theta1.0", "theta3.0"),
					c("theta0.0", "theta1.0", "theta3.1"),
					c("theta0.0", "theta2.0", "theta2.1"),
					c("theta0.0", "theta2.0", "theta3.0"),
					c("theta0.0", "theta2.0", "theta3.1"),
					c("theta0.0", "theta2.1", "theta3.0"),
					c("theta0.0", "theta2.1", "theta3.1"),
					c("theta0.0", "theta3.0", "theta3.1"),
					c("theta0.0", "theta0.1", "theta1.0", "theta1.1"),
					c("theta0.0", "theta0.1", "theta1.0", "theta2.0"),
					c("theta0.0", "theta0.1", "theta1.0", "theta2.1"),
					c("theta0.0", "theta0.1", "theta1.0", "theta3.0"),
					c("theta0.0", "theta0.1", "theta1.0", "theta3.1"),
					c("theta0.0", "theta0.1", "theta1.1", "theta2.0"),
					c("theta0.0", "theta0.1", "theta1.1", "theta2.1"),
					c("theta0.0", "theta0.1", "theta1.1", "theta3.0"),
					c("theta0.0", "theta0.1", "theta1.1", "theta3.1"),
					c("theta0.0", "theta0.1", "theta2.0", "theta2.1"),
					c("theta0.0", "theta0.1", "theta2.0", "theta3.0"),
					c("theta0.0", "theta0.1", "theta2.0", "theta3.1"),
					c("theta0.0", "theta0.1", "theta2.1", "theta3.0"),
					c("theta0.0", "theta0.1", "theta2.1", "theta3.1"),
					c("theta0.0", "theta0.1", "theta3.0", "theta3.1"),
					c("theta0.0", "theta1.0", "theta2.0", "theta2.1"),
					c("theta0.0", "theta1.0", "theta2.0", "theta3.0"),
					c("theta0.0", "theta1.0", "theta2.0", "theta3.1"),
					c("theta0.0", "theta1.0", "theta2.1", "theta3.0"),
					c("theta0.0", "theta1.0", "theta2.1", "theta3.1"),
					c("theta0.0", "theta1.0", "theta3.0", "theta3.1"),
					c("theta0.0", "theta2.0", "theta2.1", "theta3.0"),
					c("theta0.0", "theta2.0", "theta2.1", "theta3.1"),
					c("theta0.0", "theta2.0", "theta3.0", "theta3.1"),
					c("theta0.0", "theta2.1", "theta3.0", "theta3.1"),
					c("theta0.0", "theta0.1", "theta1.0", "theta1.1", "theta2.0"),
					c("theta0.0", "theta0.1", "theta1.0", "theta1.1", "theta2.1"),
					c("theta0.0", "theta0.1", "theta1.0", "theta1.1", "theta3.0"),
					c("theta0.0", "theta0.1", "theta1.0", "theta1.1", "theta3.1"),
					c("theta0.0", "theta0.1", "theta1.0", "theta2.0", "theta2.1"),
					c("theta0.0", "theta0.1", "theta1.0", "theta2.0", "theta3.0"),
					c("theta0.0", "theta0.1", "theta1.0", "theta2.0", "theta3.1"),
					c("theta0.0", "theta0.1", "theta1.0", "theta2.1", "theta3.0"),
					c("theta0.0", "theta0.1", "theta1.0", "theta2.1", "theta3.1"),
					c("theta0.0", "theta0.1", "theta1.0", "theta3.0", "theta3.1"),
					c("theta0.0", "theta0.1", "theta1.1", "theta2.0", "theta2.1"),
					c("theta0.0", "theta0.1", "theta1.1", "theta2.0", "theta3.0"),
					c("theta0.0", "theta0.1", "theta1.1", "theta2.0", "theta3.1"),
					c("theta0.0", "theta0.1", "theta1.1", "theta2.1", "theta3.0"),
					c("theta0.0", "theta0.1", "theta1.1", "theta2.1", "theta3.1"),
					c("theta0.0", "theta0.1", "theta1.1", "theta3.0", "theta3.1"),
					c("theta0.0", "theta0.1", "theta2.0", "theta2.1", "theta3.0"),
					c("theta0.0", "theta0.1", "theta2.0", "theta2.1", "theta3.1"),
					c("theta0.0", "theta0.1", "theta2.0", "theta3.0", "theta3.1"),
					c("theta0.0", "theta0.1", "theta2.1", "theta3.0", "theta3.1"),
					c("theta0.0", "theta1.0", "theta2.0", "theta2.1", "theta3.0"),
					c("theta0.0", "theta1.0", "theta2.0", "theta2.1", "theta3.1"),
					c("theta0.0", "theta1.0", "theta2.0", "theta3.0", "theta3.1"),
					c("theta0.0", "theta1.0", "theta2.1", "theta3.0", "theta3.1"),
					c("theta0.0", "theta2.0", "theta2.1", "theta3.0", "theta3.1"),
					c("theta0.0", "theta0.1", "theta1.0", "theta1.1", "theta2.0", "theta2.1"),
					c("theta0.0", "theta0.1", "theta1.0", "theta1.1", "theta2.0", "theta3.0"),
					c("theta0.0", "theta0.1", "theta1.0", "theta1.1", "theta2.0", "theta3.1"),
					c("theta0.0", "theta0.1", "theta1.0", "theta1.1", "theta2.1", "theta3.0"),
					c("theta0.0", "theta0.1", "theta1.0", "theta1.1", "theta2.1", "theta3.1"),
					c("theta0.0", "theta0.1", "theta1.0", "theta1.1", "theta3.0", "theta3.1"),
					c("theta0.0", "theta0.1", "theta1.0", "theta2.0", "theta2.1", "theta3.0"),
					c("theta0.0", "theta0.1", "theta1.0", "theta2.0", "theta2.1", "theta3.1"),
					c("theta0.0", "theta0.1", "theta1.0", "theta2.0", "theta3.0", "theta3.1"),
					c("theta0.0", "theta0.1", "theta1.0", "theta2.1", "theta3.0", "theta3.1"),
					c("theta0.0", "theta0.1", "theta1.1", "theta2.0", "theta2.1", "theta3.0"),
					c("theta0.0", "theta0.1", "theta1.1", "theta2.0", "theta2.1", "theta3.1"),
					c("theta0.0", "theta0.1", "theta1.1", "theta2.0", "theta3.0", "theta3.1"),
					c("theta0.0", "theta0.1", "theta1.1", "theta2.1", "theta3.0", "theta3.1"),
					c("theta0.0", "theta0.1", "theta2.0", "theta2.1", "theta3.0", "theta3.1"),
					c("theta0.0", "theta1.0", "theta2.0", "theta2.1", "theta3.0", "theta3.1"),
					c("theta0.0", "theta0.1", "theta1.0", "theta1.1", "theta2.0", "theta2.1", "theta3.0"),
					c("theta0.0", "theta0.1", "theta1.0", "theta1.1", "theta2.0", "theta2.1", "theta3.1"),
					c("theta0.0", "theta0.1", "theta1.0", "theta1.1", "theta2.0", "theta3.0", "theta3.1"),
					c("theta0.0", "theta0.1", "theta1.0", "theta1.1", "theta2.1", "theta3.0", "theta3.1"),
					c("theta0.0", "theta0.1", "theta1.0", "theta2.0", "theta2.1", "theta3.0", "theta3.1"),
					c("theta0.0", "theta0.1", "theta1.1", "theta2.0", "theta2.1", "theta3.0", "theta3.1"))
				allTypes <- c("Intercept", "Linear",
                                  rep("Piecewise level",2),
                                  "Piecewise linear",
                                  "Piecewise level",
                                  rep("Piecewise linear",7),
                                  "Piecewise level",
                                  "Piecewise linear",
                                  "Piecewise level",
                                  rep("Piecewise linear",2),
                                  "Piecewise level",
                                  rep("Piecewise linear",20),
                                  "Piecewise level",
                                  rep("Piecewise linear",55))
				if(constr.intercepts){
					allConstr <- list(
						NULL,
						matrix(c(0,1),1,2),
						matrix(c(0,1),1,2),
						matrix(c(0,1),1,2),
						matrix(c(0,1),1,2),
						matrix(c(0,1),1,2),
						matrix(c(0,1),1,2),
						matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
						matrix(c(0,1,1,0,0,-1),2,3, byrow=TRUE),
						matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
						matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
						matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
						matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
						matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
						matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
						matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
						matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
						matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
						matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
						matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
						matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
						matrix(c(0,1,0,0,1,1),2,3, byrow=TRUE),
						matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
						matrix(c(0,1,0,1, 0,0,1,0, 0,0,0,-1),3,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1),3,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1),3,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1),3,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1),3,4, byrow=TRUE),
						matrix(c(0,1,1,0, 0,0,-1,0, 0,0,0,1),3,4, byrow=TRUE),
						matrix(c(0,1,1,0, 0,0,-1,0, 0,0,0,1),3,4, byrow=TRUE),
						matrix(c(0,1,1,0, 0,0,-1,0, 0,0,0,1),3,4, byrow=TRUE),
						matrix(c(0,1,1,0, 0,0,-1,0, 0,0,0,1),3,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1),3,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1),3,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1),3,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1),3,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,1,0, 0,0,1,1),3,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1),3,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1),3,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1),3,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1),3,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1),3,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,1,0, 0,0,1,1),3,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1),3,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1),3,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,1,0, 0,0,1,1),3,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1),3,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1),3,4, byrow=TRUE),
						matrix(c(0,1,0,1,0, 0,0,1,0,0, 0,0,0,-1,0, 0,0,0,0,1),4,5, byrow=TRUE),
						matrix(c(0,1,0,1,0, 0,0,1,0,0, 0,0,0,-1,0, 0,0,0,0,1),4,5, byrow=TRUE),
						matrix(c(0,1,0,1,0, 0,0,1,0,0, 0,0,0,-1,0, 0,0,0,0,1),4,5, byrow=TRUE),
						matrix(c(0,1,0,1,0, 0,0,1,0,0, 0,0,0,-1,0, 0,0,0,0,1),4,5, byrow=TRUE),
						matrix(c(0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,0,0,1),4,5, byrow=TRUE),
						matrix(c(0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,0,0,1),4,5, byrow=TRUE),
						matrix(c(0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,0,0,1),4,5, byrow=TRUE),
						matrix(c(0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,0,0,1),4,5, byrow=TRUE),
						matrix(c(0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,0,1,1),4,5, byrow=TRUE),
						matrix(c(0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,0,0,1),4,5, byrow=TRUE),
						matrix(c(0,1,1,0,0, 0,0,-1,0,0, 0,0,0,1,0, 0,0,0,0,1),4,5, byrow=TRUE),
						matrix(c(0,1,1,0,0, 0,0,-1,0,0, 0,0,0,1,0, 0,0,0,0,1),4,5, byrow=TRUE),
						matrix(c(0,1,1,0,0, 0,0,-1,0,0, 0,0,0,1,0, 0,0,0,0,1),4,5, byrow=TRUE),
						matrix(c(0,1,1,0,0, 0,0,-1,0,0, 0,0,0,1,0, 0,0,0,0,1),4,5, byrow=TRUE),
						matrix(c(0,1,1,0,0, 0,0,-1,0,0, 0,0,0,1,0, 0,0,0,1,1),4,5, byrow=TRUE),
						matrix(c(0,1,1,0,0, 0,0,-1,0,0, 0,0,0,1,0, 0,0,0,0,1),4,5, byrow=TRUE),
						matrix(c(0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,0,0,1),4,5, byrow=TRUE),
						matrix(c(0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,0,1,1),4,5, byrow=TRUE),
						matrix(c(0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,0,0,1),4,5, byrow=TRUE),
						matrix(c(0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,1,0,1),4,5, byrow=TRUE),
						matrix(c(0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,0,0,1),4,5, byrow=TRUE),
						matrix(c(0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,0,1,1),4,5, byrow=TRUE),
						matrix(c(0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,0,0,1),4,5, byrow=TRUE),
						matrix(c(0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,1,0,1),4,5, byrow=TRUE),
						matrix(c(0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,1,0,1),4,5, byrow=TRUE),
						matrix(c(0,1,0,1,0,0, 0,0,1,0,0,0, 0,0,0,-1,0,0, 0,0,0,0,1,0, 0,0,0,0,0,1),5,6, byrow=TRUE),
						matrix(c(0,1,0,1,0,0, 0,0,1,0,0,0, 0,0,0,-1,0,0, 0,0,0,0,1,0, 0,0,0,0,0,1),5,6, byrow=TRUE),
						matrix(c(0,1,0,1,0,0, 0,0,1,0,0,0, 0,0,0,-1,0,0, 0,0,0,0,1,0, 0,0,0,0,0,1),5,6, byrow=TRUE),
						matrix(c(0,1,0,1,0,0, 0,0,1,0,0,0, 0,0,0,-1,0,0, 0,0,0,0,1,0, 0,0,0,0,0,1),5,6, byrow=TRUE),
						matrix(c(0,1,0,1,0,0, 0,0,1,0,0,0, 0,0,0,-1,0,0, 0,0,0,0,1,0, 0,0,0,0,1,1),5,6, byrow=TRUE),
						matrix(c(0,1,0,1,0,0, 0,0,1,0,0,0, 0,0,0,-1,0,0, 0,0,0,0,1,0, 0,0,0,0,0,1),5,6, byrow=TRUE),
						matrix(c(0,1,0,0,0,0, 0,0,1,0,0,0, 0,0,0,1,0,0, 0,0,0,0,1,0, 0,0,0,0,0,1),5,6, byrow=TRUE),
						matrix(c(0,1,0,0,0,0, 0,0,1,0,0,0, 0,0,0,1,0,0, 0,0,0,0,1,0, 0,0,0,0,1,1),5,6, byrow=TRUE),
						matrix(c(0,1,0,0,0,0, 0,0,1,0,0,0, 0,0,0,1,0,0, 0,0,0,0,1,0, 0,0,0,0,0,1),5,6, byrow=TRUE),
						matrix(c(0,1,0,0,0,0, 0,0,1,0,0,0, 0,0,0,1,0,0, 0,0,0,0,1,0, 0,0,0,1,0,1),5,6, byrow=TRUE),
						matrix(c(0,1,1,0,0,0, 0,0,-1,0,0,0, 0,0,0,1,0,0, 0,0,0,0,1,0, 0,0,0,0,0,1),5,6, byrow=TRUE),
						matrix(c(0,1,1,0,0,0, 0,0,-1,0,0,0, 0,0,0,1,0,0, 0,0,0,0,1,0, 0,0,0,0,1,1),5,6, byrow=TRUE),
						matrix(c(0,1,1,0,0,0, 0,0,-1,0,0,0, 0,0,0,1,0,0, 0,0,0,0,1,0, 0,0,0,0,0,1),5,6, byrow=TRUE),
						matrix(c(0,1,1,0,0,0, 0,0,-1,0,0,0, 0,0,0,1,0,0, 0,0,0,0,1,0, 0,0,0,1,0,1),5,6, byrow=TRUE),
						matrix(c(0,1,0,0,0,0, 0,0,1,0,0,0, 0,0,0,1,0,0, 0,0,0,0,1,0, 0,0,0,1,0,1),5,6, byrow=TRUE),
						matrix(c(0,1,0,0,0,0, 0,0,1,0,0,0, 0,0,0,1,0,0, 0,0,0,0,1,0, 0,0,0,1,0,1),5,6, byrow=TRUE),
						matrix(c(0,1,0,1,0,0,0, 0,0,1,0,0,0,0, 0,0,0,-1,0,0,0, 0,0,0,0,1,0,0, 0,0,0,0,0,1,0, 0,0,0,0,0,0,1),6,7, byrow=TRUE),
						matrix(c(0,1,0,1,0,0,0, 0,0,1,0,0,0,0, 0,0,0,-1,0,0,0, 0,0,0,0,1,0,0, 0,0,0,0,0,1,0, 0,0,0,0,0,1,1),6,7, byrow=TRUE),
						matrix(c(0,1,0,1,0,0,0, 0,0,1,0,0,0,0, 0,0,0,-1,0,0,0, 0,0,0,0,1,0,0, 0,0,0,0,0,1,0, 0,0,0,0,0,0,1),6,7, byrow=TRUE),
						matrix(c(0,1,0,1,0,0,0, 0,0,1,0,0,0,0, 0,0,0,-1,0,0,0, 0,0,0,0,1,0,0, 0,0,0,0,0,1,0, 0,0,0,0,1,0,1),6,7, byrow=TRUE),
						matrix(c(0,1,0,0,0,0,0, 0,0,1,0,0,0,0, 0,0,0,1,0,0,0, 0,0,0,0,1,0,0, 0,0,0,0,0,1,0, 0,0,0,0,1,0,1),6,7, byrow=TRUE),
						matrix(c(0,1,1,0,0,0,0, 0,0,-1,0,0,0,0, 0,0,0,1,0,0,0, 0,0,0,0,1,0,0, 0,0,0,0,0,1,0, 0,0,0,0,1,0,1),6,7, byrow=TRUE))
				}
				else{
					allConstr <- list(
						NULL,
						matrix(c(0,1),1,2),
						NULL,
						NULL,
						matrix(c(0,1),1,2),
						NULL,
						matrix(c(0,1),1,2),
						matrix(c(0,1,0),1,3, byrow=TRUE),
						matrix(c(0,1,1,0,0,-1),2,3, byrow=TRUE),
						matrix(c(0,1,0),1,3, byrow=TRUE),
						matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
						matrix(c(0,1,0),1,3, byrow=TRUE),
						matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
						NULL,
						matrix(c(0,0,1),1,3, byrow=TRUE),
						NULL,
						matrix(c(0,0,1),1,3, byrow=TRUE),
						matrix(c(0,0,1),1,3, byrow=TRUE),
						NULL,
						matrix(c(0,0,1),1,3, byrow=TRUE),
						matrix(c(0,1,0),1,3, byrow=TRUE),
						matrix(c(0,1,0,0,1,1),2,3, byrow=TRUE),
						matrix(c(0,0,1),1,3, byrow=TRUE),
						matrix(c(0,1,0,1, 0,0,0,-1),2,4, byrow=TRUE),
						matrix(c(0,1,0,0),1,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,0,1),2,4, byrow=TRUE),
						matrix(c(0,1,0,0),1,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,0,1),2,4, byrow=TRUE),
						matrix(c(0,1,1,0, 0,0,-1,0),2,4, byrow=TRUE),
						matrix(c(0,1,1,0, 0,0,-1,0, 0,0,0,1),3,4, byrow=TRUE),
						matrix(c(0,1,1,0, 0,0,-1,0),2,4, byrow=TRUE),
						matrix(c(0,1,1,0, 0,0,-1,0, 0,0,0,1),3,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,0,1),2,4, byrow=TRUE),
						matrix(c(0,1,0,0),1,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,0,1),2,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,1,0),2,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,1,0, 0,0,1,1),3,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,0,1),2,4, byrow=TRUE),
						matrix(c(0,0,0,1),1,4, byrow=TRUE),
						NULL,
						matrix(c(0,0,0,1),1,4, byrow=TRUE),
						matrix(c(0,0,1,0),1,4, byrow=TRUE),
						matrix(c(0,0,1,0, 0,0,1,1),2,4, byrow=TRUE),
						matrix(c(0,0,0,1),2,4, byrow=TRUE),
						matrix(c(0,0,1,0),1,4, byrow=TRUE),
						matrix(c(0,0,1,0, 0,0,1,1),2,4, byrow=TRUE),
						matrix(c(0,0,0,1),1,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,0,1),2,4, byrow=TRUE),
						matrix(c(0,1,0,1,0, 0,0,0,-1,0),2,5, byrow=TRUE),
						matrix(c(0,1,0,1,0, 0,0,0,-1,0, 0,0,0,0,1),3,5, byrow=TRUE),
						matrix(c(0,1,0,1,0, 0,0,0,-1,0),2,5, byrow=TRUE),
						matrix(c(0,1,0,1,0, 0,0,0,-1,0, 0,0,0,0,1),3,5, byrow=TRUE),
						matrix(c(0,1,0,0,0, 0,0,0,0,1),2,5, byrow=TRUE),
						matrix(c(0,1,0,0,0),1,5, byrow=TRUE),
						matrix(c(0,1,0,0,0, 0,0,0,0,1),2,5, byrow=TRUE),
						matrix(c(0,1,0,0,0, 0,0,0,1,0),2,5, byrow=TRUE),
						matrix(c(0,1,0,0,0, 0,0,0,1,0, 0,0,0,1,1),3,5, byrow=TRUE),
						matrix(c(0,1,0,0,0, 0,0,0,0,1),2,5, byrow=TRUE),
						matrix(c(0,1,1,0,0, 0,0,-1,0,0, 0,0,0,0,1),3,5, byrow=TRUE),
						matrix(c(0,1,1,0,0, 0,0,-1,0,0),2,5, byrow=TRUE),
						matrix(c(0,1,1,0,0, 0,0,-1,0,0, 0,0,0,0,1),3,5, byrow=TRUE),
						matrix(c(0,1,1,0,0, 0,0,-1,0,0, 0,0,0,1,0),3,5, byrow=TRUE),
						matrix(c(0,1,1,0,0, 0,0,-1,0,0, 0,0,0,1,0, 0,0,0,1,1),4,5, byrow=TRUE),
						matrix(c(0,1,1,0,0, 0,0,-1,0,0, 0,0,0,0,1),3,5, byrow=TRUE),
						matrix(c(0,1,0,0,0, 0,0,0,1,0),2,5, byrow=TRUE),
						matrix(c(0,1,0,0,0, 0,0,0,1,0, 0,0,0,1,1),3,5, byrow=TRUE),
						matrix(c(0,1,0,0,0, 0,0,0,0,1),2,5, byrow=TRUE),
						matrix(c(0,1,0,0,0, 0,0,1,0,0, 0,0,1,0,1),3,5, byrow=TRUE),
						matrix(c(0,0,0,1,0),1,5, byrow=TRUE),
						matrix(c(0,0,0,1,0, 0,0,0,1,1),2,5, byrow=TRUE),
						matrix(c(0,0,0,0,1),1,5, byrow=TRUE),
						matrix(c(0,0,1,0,0, 0,0,1,0,1),2,5, byrow=TRUE),
						matrix(c(0,0,1,0,0, 0,0,1,0,1),2,5, byrow=TRUE),
						matrix(c(0,1,0,1,0,0, 0,0,0,-1,0,0, 0,0,0,0,0,1),3,6, byrow=TRUE),
						matrix(c(0,1,0,1,0,0, 0,0,0,-1,0,0),2,6, byrow=TRUE),
						matrix(c(0,1,0,1,0,0, 0,0,0,-1,0,0, 0,0,0,0,0,1),3,6, byrow=TRUE),
						matrix(c(0,1,0,1,0,0, 0,0,0,-1,0,0, 0,0,0,0,1,0),3,6, byrow=TRUE),
						matrix(c(0,1,0,1,0,0, 0,0,0,-1,0,0, 0,0,0,0,1,0, 0,0,0,0,1,1),4,6, byrow=TRUE),
						matrix(c(0,1,0,1,0,0, 0,0,0,-1,0,0, 0,0,0,0,0,1),3,6, byrow=TRUE),
						matrix(c(0,1,0,0,0,0, 0,0,0,0,1,0),2,6, byrow=TRUE),
						matrix(c(0,1,0,0,0,0, 0,0,0,0,1,0, 0,0,0,0,1,1),3,6, byrow=TRUE),
						matrix(c(0,1,0,0,0,0, 0,0,0,0,0,1),2,6, byrow=TRUE),
						matrix(c(0,1,0,0,0,0, 0,0,0,1,0,0, 0,0,0,1,0,1),3,6, byrow=TRUE),
						matrix(c(0,1,1,0,0,0, 0,0,-1,0,0,0, 0,0,0,0,1,0),3,6, byrow=TRUE),
						matrix(c(0,1,1,0,0,0, 0,0,-1,0,0,0, 0,0,0,0,1,0, 0,0,0,0,1,1),4,6, byrow=TRUE),
						matrix(c(0,1,1,0,0,0, 0,0,-1,0,0,0, 0,0,0,0,0,1),3,6, byrow=TRUE),
						matrix(c(0,1,1,0,0,0, 0,0,-1,0,0,0, 0,0,0,1,0,0, 0,0,0,1,0,1),4,6, byrow=TRUE),
						matrix(c(0,1,0,0,0,0, 0,0,0,1,0,0, 0,0,0,1,0,1),3,6, byrow=TRUE),
						matrix(c(0,0,0,1,0,0, 0,0,0,1,0,1),2,6, byrow=TRUE),
						matrix(c(0,1,0,1,0,0,0, 0,0,0,-1,0,0,0, 0,0,0,0,0,1,0),3,7, byrow=TRUE),
						matrix(c(0,1,0,1,0,0,0, 0,0,0,-1,0,0,0, 0,0,0,0,0,1,0, 0,0,0,0,0,1,1),4,7, byrow=TRUE),
						matrix(c(0,1,0,1,0,0,0, 0,0,0,-1,0,0,0, 0,0,0,0,0,0,1),3,7, byrow=TRUE),
						matrix(c(0,1,0,1,0,0,0, 0,0,0,-1,0,0,0, 0,0,0,0,1,0,0, 0,0,0,0,1,0,1),4,7, byrow=TRUE),
						matrix(c(0,1,0,0,0,0,0, 0,0,0,0,1,0,0, 0,0,0,0,1,0,1),3,7, byrow=TRUE),
						matrix(c(0,1,1,0,0,0,0, 0,0,-1,0,0,0,0, 0,0,0,0,1,0,0, 0,0,0,0,1,0,1),4,7, byrow=TRUE))
				}
			}
			else{
				# 11 models
				allLabs <- list(
					"theta0.0",
					c("theta0.0", "theta0.1"),
					c("theta0.0", "theta2.1"),
					c("theta0.0", "theta3.1"),
					c("theta0.0", "theta0.1", "theta1.1"),
					c("theta0.0", "theta0.1", "theta2.1"),
					c("theta0.0", "theta0.1", "theta3.1"),
					c("theta0.0", "theta2.1", "theta3.1"),
					c("theta0.0", "theta0.1", "theta1.1", "theta2.1"),
					c("theta0.0", "theta0.1", "theta1.1", "theta3.1"),
					c("theta0.0", "theta0.1", "theta2.1", "theta3.1"))
				allConstr <- list(
						NULL,
						matrix(c(0,1),1,2),
						matrix(c(0,1),1,2),
						matrix(c(0,1),1,2),
						matrix(c(0,1,1,0,0,-1),2,3, byrow=TRUE),
						matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
						matrix(c(0,1,0,0,0,1),2,3, byrow=TRUE),
						matrix(c(0,1,0,0,1,1),2,3, byrow=TRUE),
						matrix(c(0,1,1,0, 0,0,-1,0, 0,0,0,1),3,4, byrow=TRUE),
						matrix(c(0,1,1,0, 0,0,-1,0, 0,0,0,1),3,4, byrow=TRUE),
						matrix(c(0,1,0,0, 0,0,1,0, 0,0,1,1),3,4, byrow=TRUE))
				allTypes <- c("Intercept", "Linear", rep("Piecewise linear",9))
			}
		}
		if(constr.slopes==1){
			constrmat <- t(object@QP$Amat)
			if(!continuous){
				mylab <- c("theta0.0", "theta0.1", "theta1.0", "theta1.1", "theta2.0", "theta2.1", "theta3.0", "theta3.1")
				searchAll <- list(NULL,t(combn(1:7,1))+1,t(combn(1:7,2))+1,t(combn(1:7,3))+1,t(combn(1:7,4))+1,t(combn(1:7,5))+1,t(combn(1:7,6))+1)

				nbmodels <- sum(unlist(lapply(searchAll,nrow)))+1
				ct <- 1
				allLabs <- allConstr <- rep(list(NULL),nbmodels)
				for(i in 1:length(searchAll)){
					if(i==1){
						allLabs[[ct]] <- "theta0.0"
						ct <- ct + 1
					}
					else{
						for(j in 1:nrow(searchAll[[i]])){
							tokeep <- searchAll[[i]][j,]
							allLabs[[ct]] <- mylab[c(1,tokeep)]
							if(object@call.arg$constr){
								if(length(tokeep)==1) allConstr[[ct]] <- matrix(c(0,1),1,2)
								else{
									tp <- constrmat[,c(1,tokeep)]
									tp <- tp[rowSums(tp)!=0,]
									if(!is.matrix(tp)) tp <- matrix(tp, 1:length(tp), byrow=TRUE)
									else{
										tp <- tp[!duplicated(tp),]
										if(!is.matrix(tp)) tp <- matrix(tp, 1:length(tp), byrow=TRUE)
										allConstr[[ct]] <- tp
									}
								}
							}
							ct <- ct + 1
						}
					}
				}
				allTypes <- c("Intercept", "Linear",
						"Piecewise level",
						"Piecewise linear",
						"Piecewise level",
						"Piecewise linear",
						"Piecewise level",
						rep("Piecewise linear",8),
						"Piecewise level",
						"Piecewise linear",
						"Piecewise level",
						rep("Piecewise linear",6),
						"Piecewise level",
						rep("Piecewise linear",24),
						"Piecewise level",
						rep("Piecewise linear",77))
			}
			else{
					allLabs <- list(
							"theta0.0",
							c("theta0.0", "theta0.1"),
							c("theta0.0", "theta1.1"),
							c("theta0.0", "theta2.1"),
							c("theta0.0", "theta3.1"),
							c("theta0.0", "theta0.1", "theta1.1"),
							c("theta0.0", "theta0.1", "theta2.1"),
							c("theta0.0", "theta0.1", "theta3.1"),
							c("theta0.0", "theta1.1", "theta2.1"),
							c("theta0.0", "theta1.1", "theta3.1"),
							c("theta0.0", "theta2.1", "theta3.1"),
							c("theta0.0", "theta0.1", "theta1.1", "theta2.1"),
							c("theta0.0", "theta0.1", "theta1.1", "theta3.1"),
							c("theta0.0", "theta0.1", "theta2.1", "theta3.1"),
							c("theta0.0", "theta1.1", "theta2.1", "theta3.1"))
					if(object@call.arg$constr) allConstr <- list(
							NULL,
							matrix(c(0,1),1,2, byrow=TRUE),
							matrix(c(0,1),1,2, byrow=TRUE),
							matrix(c(0,1),1,2, byrow=TRUE),
							matrix(c(0,1),1,2, byrow=TRUE),
							matrix(c(0,1,0, 0,0,1),2,3, byrow=TRUE),
							matrix(c(0,1,0, 0,1,1),2,3, byrow=TRUE),
							matrix(c(0,1,0, 0,0,1),2,3, byrow=TRUE),
							matrix(c(0,1,0, 0,0,1),2,3, byrow=TRUE),
							matrix(c(0,1,0, 0,0,1),2,3, byrow=TRUE),
							matrix(c(0,1,0, 0,0,1),2,3, byrow=TRUE),
							matrix(c(0,1,0,0, 0,0,1,0, 0,1,0,1),3,4, byrow=TRUE),
							matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1),3,4, byrow=TRUE),
							matrix(c(0,1,0,0, 0,1,1,0, 0,0,0,1),3,4, byrow=TRUE),
							matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1),3,4, byrow=TRUE))
					allTypes <- c("Intercept", "Linear", rep("Piecewise linear", 13))
			}
		}
	}
	f <- function(labs, x){
		if(length(labs)==1){
			m <- matrix(1,nrow(x),1)
			colnames(m) <- "theta0.0"
		}
		else{
			m <- x[,colnames(x)%in%labs]
		}
		m
	}
	allMats <- lapply(allLabs, f, x=object@X)
	return(list(allLabs=allLabs, allConstr=allConstr, allTypes=allTypes, allMats=allMats))
}


