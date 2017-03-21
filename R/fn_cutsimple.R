#allow b to be set incase we need to add this functionality in the future
#' @title Simple cut
#' @description A simple routine to cuts out portion of a vector signal
#' @param x vector signal
#' @param dt sample interval
#' @param t1 Begin cut time
#' @param t2 End cut time
#' @return A cut vector signal
#' @export
cut_simple <- function(x,dt,t1,t2,b=0){
	e <- (length(x)-1)*dt-b
	fcut <- t1
	scut <- t2
	dt <- as.integer(dt*1000000)/1000000 #fixing floatintg point error
	if (fcut < b){fcut <- b}
	if (scut > e){scut <- e}
	#Delete samples
	fdel <- as.integer(fcut/dt) 
	sdel <- as.integer((e-scut)/dt) 
	if(sdel > 0){x <- head(x,-sdel)}
	if(fdel > 0){x <- tail(x,-fdel)}
return(x)
}