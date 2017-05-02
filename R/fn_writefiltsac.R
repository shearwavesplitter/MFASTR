#' @title Write filtered SAC files
#' @description Writes out filtered waveforms ready to have shear wave splitting measured
#' @param path Path to folder
#' @param trip Event triplet (output of readtriplet)
#' @param name Name of the event
#' @param filtlist Dataframe of the best filters (output of filter_spread)
#' @param number Number of best filters to use
#' @param E Suffix of the east component
#' @param N Suffix of the north component
#' @param Z Suffix of the vertical component
#' #return A dataframe of the filters that have been written
#' @export
#' @examples
#' # Write out three best filters for event 2002.054.09.47.lhor2
#' pathto <- "~/mfast/sample_data/raw_data"
#' event <- "2002.054.09.47.lhor2"
#' write_sample(pathto)
#' triplet <- readtriplet(event)
#' bestfilt <- filter_spread(triplet)
#' f <- writesac_filt(pathto,triplet,event,bestfilt)
writesac_filt <- function(path,trip,name,filtlist,number=3,E=".e",N=".n",Z=".z",zerophase=FALSE) {
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
			trip[[j]]$amp <- butfilt(trip[[j]]$amp, fl=filter$low, fh=filter$high, deltat=trip[[j]]$dt, type="BP" , proto="BU",npoles=2,zp=zerophase) #zerophase=FALSE so filter isn't zero phase (one pass)
			sm.write1sac(trip[[j]],paste0(name,".",filter$low,"-",filter$high,".fb",i,comp[j]))
		}
	
	}
	f<- filtlist[1:number,]
return(f)
}