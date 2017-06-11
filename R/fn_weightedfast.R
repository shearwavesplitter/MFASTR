#' @title Mean fast polarisation
#' @description Determine the mean weighted fast polarisation 
#' @param summ Dataframe containing Castelazzi graded events (CZ_*.summ)
#' @param weights A vector containing the weights with length equal to the number of filters used (usually 3) in order with the first corresponding to F1 or a weight for each measurement 
#' @return A list containing the weighted mean polarisation, its pythagorean length, and the (weighted) p-value from the Rayleigh test
#' @export
fast.weighted <- function(summ,weights=c(1,2,3)){
fast <- summ$fast*2
fast <- circular(rad(fast),units="radians")
grades <- summ$finalgrade
if(length(weights) == length(summ$fast)){
	grades <- weights
}else{
	if(is.null(grades)){stop("No finalgrades found. Use CZ graded summary file (with F1, F2 etc grades)")}
	grades <- as.character(grades)
	ung <- unique(grades)
	ung <- sort(ung)
	if(length(weights) != length(ung)){stop("Weight vector is wrong length")}
	for (j in 1:length(ung)){
		grades[grades == ung[j]] <- weights[j]
	}
}
grades <- as.numeric(grades)
	vec <- fast
	n <- sum(grades)
	C <- (1/n)*sum(grades*cos(vec))
	S <- (1/n)*sum(grades*sin(vec))
	m <- atan2(S,C)
	m2 <- m/2
	m2 <- circular(deg(m2),units="degrees")
	R <- sqrt(C^2+S^2)
 	Z <- n*R^2
	pval <- exp(-Z)
	if(n < 50){pval <- exp(-Z)*(1+((2*Z-Z^2)/(4*n))-((24*Z-132*Z^2+76*Z^3-9*Z^4)/(288*n^2)))}
	ret <- list()
	ret$mean <-m2
	ret$R <- R
	ret$pval <- pval
	ret$var <- 2*(1-R)
	##Standard error
	alpha <- n^(-1)*sum(2*grades*cos(fast-m))
	se2 <- sqrt((1-alpha)/(2*n*R^2))
	#ret$SE <- deg(asin(se2))/2
return(ret)
}
