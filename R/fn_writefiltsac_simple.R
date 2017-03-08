#' @export
writesac_filtsmp <- function(path,trip,name,low,high,E=".e",N=".n",Z=".z",n=1) {
	setwd(path)
	trip0 <- trip
	
		trip <- trip0
		#for (j in 1:3){ #dont need z component
		#print(paste0("Writing ",name,".",low,"-",high,".fb",n))
		print(paste0("Writing ",name))
		for (j in 1:2){
			comp <- c(E,N,Z)
				if(is.null(low) | is.null(high)){
					sm.write1sac(trip[[j]],paste0(name,comp[j]))
				}else{
					trip[[j]]$amp <- butfilt(trip[[j]]$amp, fl=low, fh=high, deltat=trip[[j]]$dt, type="BP" , proto="BU",npoles=2,zp=FALSE) #zp=FALSE so filter isn't zero phase (one pass)
					sm.write1sac(trip[[j]],paste0(name,".",low,"-",high,".fb",n,comp[j]))
				}
			}
}
	
