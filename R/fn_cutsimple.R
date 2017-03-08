#allow b to be set incase we need to add this functionality in the future
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