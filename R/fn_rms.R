#' @title Root mean square
#' @description Simple routine to determine root mean square value of a signal
#' @param x Vector signal
#' @return RMS value
#' @export
rms <- function(x){
	x <- x^2
	depmen <- mean(x) #I need to check were else depmen is used and potentially recalculate it
	if(depmen > 0){
		se <- sqrt(depmen)
	}else{
		se <- 0.00001
	}
return(se)
}