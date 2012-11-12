# Rearrange calls to avoid low number of measurement for each state

# Author: Gwenael G.R. Leday

################## Details
## discard=FALSE:
# 1. Reference label is normals (coded 0)
# 2. If not enough observations:
# - 'losses' merge to 'normals'
# - 'gains' merge to 'normals'
# - 'amplifications' merge to 'gains' or 'normals' (if no 'gains'
#   or not enough observations as well)
#
## discard=TRUE:
# If number of observations is lower than min.obs,
# these observations are discarded (replaced by NAs)

modify.conf <- function(cghcall, min.obs=3, discard=TRUE){

	val <- sort(unique(cghcall))
	nval <- length(val)
	distr <- as.matrix(table(cghcall))
	val.ref <- which(val==0)
	out <- cghcall

	if(nval!=1){
		if(discard){
			ind <- distr<min.obs
			if(any(ind)){
				xx <- val[which(ind)]
				for(i in 1:length(xx)) out[out==xx[i]] <- NA
			}
		}
		else{
			if(-1%in%val){
				if(distr[val==-1]<min.obs){
					out[out==-1] <- 0
					distr <- as.matrix(table(out))
					val <- sort(unique(out))
				}
			}
			if(2%in%val){
				if(distr[val==2]<min.obs){
					out[out==2] <- 1
					distr <- as.matrix(table(out))
					val <- sort(unique(out))
				}
			}
			if(1%in%val){
				if(distr[val==1]<min.obs){
					if(2%in%out) out[out==2] <- 1
					else{
						out[out==1] <- 0
					}	
					distr <- as.matrix(table(out))
					val <- sort(unique(out))
				}
			}
			if(0%in%val){
				if(distr[val==0]<min.obs){
					if(1%in%out) out[out==0] <- 1
					else{
						if(-1%in%out) out[out==0] <- -1
					}
					distr <- as.matrix(table(out))
					val <- sort(unique(out))
				}
			}
			if(length(val)==1){
				out <- rep(0,length(cghcall))
				names(out) <- names(cghcall)
			}
		}
	}
	out
}
