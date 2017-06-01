#' @title Mean delay time
#' @description Determine the mean weighted delay time
#' @param summ Dataframe containing Castelazzi graded events (CZ_*.summ)
#' @param weights A vector containing the weights with length equal to the number of filters used (usually 3) in order with the first corresponding to F1
#' @return A list containing the weighted mean delay time, and mean delay time per kilometre (straightline) path length as well as their respective standard deviations and standard errors. 
#' @export
dt.weighted <- function(summ,weights=c(1,2,3)) {
	grades <- summ$finalgrade
	if(is.null(grades)){stop("No finalgrades found. Use CZ graded summary file (with F1, F2 etc grades)")}
	grades <- as.character(grades)
	ung <- unique(grades)
	ung <- sort(ung)
	if(length(weights) != length(ung)){stop("Weight vector is wrong length")}
	for (j in 1:length(ung)){
		grades[grades == ung[j]] <- weights[j]
	}
	grades <- as.numeric(grades)
	dist <- sqrt(summ$distevstat^2+summ$depthkm^2)
	#delay time
	dt <- summ$tlag
	wdt <- grades*dt
	n <- sum(grades)
	m <- sum(wdt)/n
	sd <- sd(rep(dt, grades)) 
	se <- sd/sqrt(n)
	res <- list()
	res$mean <- m
	res$sd <- sd
	res$se<- se

	#Delay time per kilometere straight line path length
	dtpkm <- dt/dist
	wdtpkm <- grades*dtpkm
	n <- sum(grades)
	mpkm <- sum(wdtpkm)/n
	sdpkm <- sd(rep(dtpkm, grades)) 
	sepkm <- sdpkm/sqrt(n)

	res$meanpkm <- mpkm
	res$sdpkm <- sdpkm
	res$sepkm <- sepkm

return(res)

}