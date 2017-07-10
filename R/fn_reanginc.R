#' @title Redetermine incidence angles
#' @description Redetermine incidence angles for events in summary file
#' @param summpath Path to the .summ file
#' @param tvel Veloctity model read in by readtvel or a stored model (ak135_alp, ak135_taupo)
#' @param overwrite Should the original summfile be overwritten?
#' @param mfast Is the summfile from the original MFAST?
#' @param mc.cores Number of cores to run the calculations on (defaults to maximum)
#' @return A summary file with redetermined incidence angles and ray parameters
#' @export
#' @examples
#' # Redetermine the angle of incidences for a summary file
#' pathto <- "~/mfast/sample_data/summ_files/WPRZ.127.CZ.summ"
#' nsumm <- reanginc(pathto,tvel=ak135_alp)
reanginc <- function(summpath,tvel=ak135_taupo,overwrite=FALSE,mfast=FALSE,mc.cores=NULL){
	if(mfast){summ <- readmfast(summpath)}else{
		summ <- read.csv(summpath)
	}
	
	require(parallel)

	if(is.null(mc.cores)){
		mc.cores <- detectCores()
	}
	print(paste0("Determining incidence angles on ",mc.cores," cores"))
	anghorse <- function(nline,tvel,summfile){
		line <- summfile[nline,]
		stla <- line$slat
		stlo <-  line$slon
		evla <-  line$evla
		evlo <-  line$evlo
		evdp <-  line$depthkm
		if(evdp < 0){return(c(NA,NA))}
		if(evla == -12345){return(c(NA,NA))}
		if(evlo == -12345){return(c(NA,NA))}
		if(stla == -12345){return(c(NA,NA))}
		if(stlo == -12345){return(c(NA,NA))}

		dist <- GreatDist(stlo,stla,evlo,evla)

		mod <- tvel
		ray <- Traveltime('S',dist$ddeg,evdp,mod)
		rayf <- which(ray$tt == min(ray$t))
		rayp <- ray$p[rayf]
		p <- rayp*360 /2 /3.1415927
		R <- mod$rp
		c <- mod$vs[1]
		i <- atan2(p*c/(R*sqrt(1-(p*c/R)^2)),1) /2 /3.1415927 * 360
		out <- c(i,rayp)
	return(out)
	}
	
	angs <- mclapply2(1:length(summ$cuspid),anghorse,summfile=summ,tvel=tvel,mc.cores=mc.cores)

	angs <- do.call(rbind.data.frame, angs)
	colnames(angs) <- c("anginc","rayp")
	angs$anginc <- round(angs$anginc,2)
	angs$rayp <- round(angs$rayp,2)
	
	summ$anginc <- angs$anginc
	summ$rayp <- angs$rayp

	if(overwrite){
		write.table(summ,file=summpath,quote=FALSE,row.names=FALSE,sep=",")
	}
	
return(summ)

}