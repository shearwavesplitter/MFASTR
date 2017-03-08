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