#' @title Weighted axial mean
#' @description The mean of a weighted axial variable
#' @param vec A vector of axis (degrees)
#' @param weights A vector of weights of the same length as vec
#' @return The mean axis (degrees) and the Pythagorean length
#' @export
mean.weighted <- function(vec,weights=NULL){
	if(is.null(weights)){weights <- rep(1,length(vec))}
	vec <- vec*2
	vec <- vec*pi/180
	n <- sum(weights)
	C <- (1/n)*sum(weights*cos(vec))
	S <- (1/n)*sum(weights*sin(vec))
	m <- atan2(S,C)
	m2 <- m/2
	m2 <- m2*(180/pi)
	R <- sqrt(C^2+S^2)
return(c(m2,R))
}