# Internal function
# Build matrix of constraints

# Author: Gwenael G.R. Leday

.make.mat.constr <- function(constr.slopes,
                             constr.intercepts,
                             continuous,
                             val){

	nval <- length(val)
	
	if(nval==2){
		if(all(val==c(-1,1))|all(val==c(-1,2))|all(val==c(1,2))) constr.slopes <- 1 
		if(constr.slopes==2){
			if(all(val==c(-1,0))){
				if(!continuous){
					if(constr.intercepts){
						mat <- matrix(c(0,1,0,1,
	                                              0,0,0,-1,
	                                              0,0,1,0),3,4, byrow=TRUE)
					}
					else{
						mat <- matrix(c(0,1,0,1,
	                                              0,0,0,-1),2,4, byrow=TRUE)
					}
				}
				else{
						mat <- matrix(c(0,1,1,
	                                              0,0,-1),2,3, byrow=TRUE)
				}
			}
			if(all(val==c(0,1))|all(val==c(0,2))){
				if(!continuous){
					if(constr.intercepts){
						mat <- matrix(c(0,1,0,0,
	                                              0,0,0,1,
	                                              0,0,1,0),3,4, byrow=TRUE)
					}
					else{
						mat <- matrix(c(0,1,0,0,
	                                              0,0,0,1),2,4, byrow=TRUE)
					}
				}
				else{
						mat <- matrix(c(0,1,0,
	                                              0,0,1),2,3, byrow=TRUE)
				}
			}
		}
		if(constr.slopes==1){
			if(!continuous){
				if(constr.intercepts){
					mat <- matrix(c(0,1,0,0,
                                              0,1,0,1,
                                              0,0,1,0),3,4, byrow=TRUE)
				}
				else{
					mat <- matrix(c(0,1,0,0,
                                              0,1,0,1),2,4, byrow=TRUE)
				}
			}
			else{
				mat <- matrix(c(0,1,0,
                                        0,1,1),2,3, byrow=TRUE)
			}
		}
	}
	if(nval==3){
		if(all(val==c(-1,1,2))) constr.slopes <- 1 
		if(constr.slopes==2){
			if(all(val==c(-1,0,1))|all(val==c(-1,0,2))){
				if(!continuous){
					if(constr.intercepts){
						mat <- matrix(c(0,1,0,1,0,0,
	                                              0,0,0,0,0,1,
	                                              0,0,0,-1,0,0,
	                                              0,0,1,0,0,0,
	                                              0,0,0,0,1,0),5,6, byrow=TRUE)
					}
					else{
						mat <- matrix(c(0,1,0,1,0,0,
	                                              0,0,0,0,0,1,
	                                              0,0,0,-1,0,0),3,6, byrow=TRUE)
					}
				}
				else{
						mat <- matrix(c(0,1,1,0,
	                                              0,0,0,1,
	                                              0,0,-1,0),3,4, byrow=TRUE)
				}
			}
			if(all(val==c(0,1,2))){
				if(!continuous){
					if(constr.intercepts){
						mat <- matrix(c(0,1,0,0,0,0,
	                                              0,0,0,1,0,0,
	                                              0,0,0,1,0,1,
	                                              0,0,1,0,0,0,
	                                              0,0,0,0,1,0),5,6, byrow=TRUE)
					}
					else{
						mat <- matrix(c(0,1,0,0,0,0,
	                                              0,0,0,1,0,0,
	                                              0,0,0,1,0,1),3,6, byrow=TRUE)
					}
				}
				else{
						mat <- matrix(c(0,1,0,0,
	                                              0,0,1,0,
	                                              0,0,1,1),3,4, byrow=TRUE)
				}
			}
		}
		if(constr.slopes==1){
			if(!continuous){
				if(constr.intercepts){
					mat <- matrix(c(0,1,0,0,0,0,
                                              0,1,0,1,0,0,
                                              0,1,0,1,0,1,
                                              0,0,1,0,0,0,
                                              0,0,0,0,1,0),5,6, byrow=TRUE)
				}
				else{
					mat <- matrix(c(0,1,0,0,0,0,
                                              0,1,0,1,0,0,
                                              0,1,0,1,0,1),3,6, byrow=TRUE)
				}
			}
			else{
					mat <- matrix(c(0,1,0,0,
                                              0,1,1,0,
                                              0,1,1,1),3,4, byrow=TRUE)
			}
		}
	}
	if(nval==4){
		if(constr.slopes==2){
			if(!continuous){
				if(constr.intercepts){
					mat <- matrix(c(0,1,0,1,0,0,0,0,
                                              0,0,0,0,0,1,0,0,
                                              0,0,0,0,0,1,0,1,
                                              0,0,0,-1,0,0,0,0,
                                              0,0,1,0,0,0,0,0,
                                              0,0,0,0,1,0,0,0,
                                              0,0,0,0,0,0,1,0),7,8, byrow=TRUE)
				}
				else{
					mat <- matrix(c(0,1,0,1,0,0,0,0,
                                              0,0,0,0,0,1,0,0,
                                              0,0,0,0,0,1,0,1,
                                              0,0,0,-1,0,0,0,0),4,8, byrow=TRUE)
				}
			}
			else{
					mat <- matrix(c(0,1,1,0,0,
                                              0,0,0,1,0,
                                              0,0,0,1,1,
                                              0,0,-1,0,0),4,5, byrow=TRUE)
			}
		}
		if(constr.slopes==1){
			if(!continuous){
				if(constr.intercepts){
					mat <- matrix(c(0,1,0,0,0,0,0,0,
                                              0,1,0,1,0,0,0,0,
                                              0,1,0,1,0,1,0,0,
                                              0,1,0,1,0,1,0,1,
                                              0,0,1,0,0,0,0,0,
                                              0,0,0,0,1,0,0,0,
                                              0,0,0,0,0,0,1,0),7,8, byrow=TRUE)
				}
				else{
					mat <- matrix(c(0,1,0,0,0,0,0,0,
                                              0,1,0,1,0,0,0,0,
                                              0,1,0,1,0,1,0,0,
                                              0,1,0,1,0,1,0,1),4,8, byrow=TRUE)
				}
			}
			else{
					mat <- matrix(c(0,1,0,0,0,
                                              0,1,1,0,0,
                                              0,1,1,1,0,
                                              0,1,1,1,1),4,5, byrow=TRUE)
			}
		}
	}
	return(mat)
}