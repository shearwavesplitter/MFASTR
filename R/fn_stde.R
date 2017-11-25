#' @title Weighted standard error
#' @description A bootstrapped weighted standard error for fast polarisations
#' @param summ Dataframe containing Castelazzi graded events (CZ_*.summ)
#' @param weights A vector containing the weights with length equal to the number of filters used (usually 3) in order with the first corresponding to F1
#' @param seed A random number seed
#' @param iter Number of iterations
#' @return The circular standard error in degrees 
#' @details This function can also be run with a custom weight for each measurement by setting them with weights. Or, for the unweighted version, set weights=rep(1,length(summ$fast)).
#' @export
stde.weighted <- function(summ,weights=c(1,2,3),seed=NULL,iter=9999) {
source("~/paper/R/weightedmean.R")
	require(parallel)
	if (is.null(seed)) {
}else{
		set.seed(seed)
	}
	n <- length(summ$fast)

	for (j in 1:iter){
		samp <- sample(1:n,size=n,replace=TRUE)

		fast <- as.data.frame(summ$fast[c(samp)])
		finalgrade <- as.data.frame(summ$finalgrade[c(samp)])
		fsumm <- cbind(fast,finalgrade)
		colnames(fsumm) <- c("fast","finalgrade")
		m <- fast.weighted(fsumm,weights=weights)$mean
		if (j == 1) {
			means <- m
		} else {
			means <- c(means,m)
		}
	}

	sd <- sd.circular(means*2)
	#sd <- asin(sd)
sd <- deg(sd/2)
return(sd)
}