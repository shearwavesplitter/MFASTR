writesac_filt <- function(path,trip,name,filtlist,number=3,E=".e",N=".n",Z=".z") {
	setwd(path)
	trip0 <- trip
	if (number > length(filtlist$high)){
		number <- length(filtlist$high)
		print(paste0("Writing all available filters to file"))
	}else{
		print(paste0("Writing ",number," best filters to file"))
	}
	for (i in 1:number){
		trip <- trip0
		filter <- filtlist[i,]
		#for (j in 1:3){ #dont need z component
		print(paste0("Writing ",name,".",filter$low,"-",filter$high,".fb",i))
		for (j in 1:2){
			comp <- c(E,N,Z)
			trip[[j]]$amp <- butfilt(trip[[j]]$amp, fl=filter$low, fh=filter$high, deltat=trip[[j]]$dt, type="BP" , proto="BU",npoles=2,zp=FALSE) #zp=FALSE so filter isn't zero phase (one pass)
			sm.write1sac(trip[[j]],paste0(name,".",filter$low,"-",filter$high,".fb",i,comp[j]))
		}
	
	}
	f<- filtlist[1:number,]
return(f)
}