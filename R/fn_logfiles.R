#' @title Parse results
#' @description Parses output of shear wave splitting measurement for a set of filters (used to build .summ files)
#' @param path Path to folder
#' @param name Name of event
#' @param trip Seismogram triplet (output of readtriplet)
#' @param filtlist Dataframe of the best filters to be used (output of writesac_filt)
#' @param maxfreqv Vector of dominant frequency in the S-wave (maxfreq) for each filter(output of create_ini) 
#' @param comment Optional comment
#' @param anginc Angle of indidence (output of anginc)
#' @return A dataframe containing the results for that event
#' @export
#' @examples
#' # Run shear wave splitting measurement on event 2002.054.09.47.lhor2 and parse the results
#' pathto <- "~/mfast/sample_data/raw_data"
#' write_sample(pathto)
#' event <- "2002.054.09.47.lhor2"
#' triplet <- readtriplet(event,path=pathto)
#' a <- anginc(ak135_alp,triplet)
#' bestfilt <- filter_spread(triplet)
#' maxfreq <- createini(pathto,triplet,bestfilt,event)
#' f <- writesac_filt(pathto,triplet,event,bestfilt)
#' run_mfast(pathto,event,f)
#' res <- logfiles(pathto,event,triplet,f,maxfreq,anginc=a)

logfiles <- function(path,name,trip,filtlist,maxfreqv,comment="MFASTR",anginc) {
	setwd(path)
	east <- trip[[1]]
	east$HEAD$values <- as.character(east$HEAD$values)
	oanginc <- anginc
	if(is.null(filtlist)){filtlist <- cbind(NA,NA,NA,NA);filtlist <- as.data.frame(filtlist);colnames(filtlist) <- c("low","high","snrv","nyq")}
	for (i in 1:length(filtlist$high)){
		filter <- filtlist[i,]
		if(is.na(filter$low)){cmpname <- name}else{cmpname <- paste0(name,".",filter$low,"-",filter$high,".fb",i)}
		logname <- paste0(cmpname,".ilognew.ass")
		ex <- file.exists(logname)
		if(!ex){print(paste0(logname," does not exist"))}else{
		log <- read.table(logname,header=TRUE)
		###Set up components of .summ file
		event <- as.character(log$event)
		stat <- east$sta
		slat <- round(as.numeric(east$HEAD$values[[which(east$HEAD$names == "stla")]]),4)
		slon <- round(as.numeric(east$HEAD$values[[which(east$HEAD$names == "stlo")]]),3)
		cuspid <- name
		year <- east$DATTIM$yr
		o <- as.numeric(east$HEAD$values[[which(east$HEAD$names == "o")]])
		ocorr <- as.numeric(east$HEAD$values[[which(east$HEAD$names == "user9")]])
		if(ocorr == -12345){ocorr <- 0}
		if(o == -12345){o <- 0}
		doy_det <- round(east$DATTIM$jd + east$DATTIM$hr/24 + (east$DATTIM$mi/60)/24 + (((east$DATTIM$sec+ocorr)/60)/60)/24,4)
		evla <- round(as.numeric(east$HEAD$values[[which(east$HEAD$names == "evla")]]),2)
		evlo <- round(as.numeric(east$HEAD$values[[which(east$HEAD$names == "evlo")]]),2)
		distevstat <- round(as.numeric(east$HEAD$values[[which(east$HEAD$names == "dist")]]),4)
		depthkm <- round(as.numeric(east$HEAD$values[[which(east$HEAD$names == "evdp")]]),1)
		if(depthkm > 900){depthkm <- depthkm/1000}
		mag <- -12345
		baz <- round(as.numeric(east$HEAD$values[[which(east$HEAD$names == "baz")]]),3)
		spol <- round(log$spol,3)
		Dspol <- round(log$dspol,3)
		wbeg <- log$wbeg_best
		wend <- log$wend_best 
		dist_ruap_km <- -12345
		dist_ruap_deg <- -12345
		SNR <- round(filter$snrv,5)
		tlag <- round(log$tlag,6)
		Dtlag <- round(log$dtlag,6)
		fast <- round(log$fast,3)
		Dfast <- round(log$dfast,3)
		rayp <- round(oanginc[2],2)
		anginc <- round(oanginc[1],1) 		
		anginc_corr <- "anginc_corr" 
		ini <- read.table(paste0(cmpname,".ini"))
		type_ini <- paste0("ass_",ini$V1[3],"_",ini$V1[4])
		timestamp <- log$time
		comment <- comment
		nyquist <- round(filter$nyq,0)
		gradeABCNR <- as.character(log$grade)
		filt_lo <- filter$low
		filt_HI <- filter$high
		spolfast <- (360-abs(as.numeric(as.character(log$fast))*2-as.numeric(as.character(log$spol))*2))
		if(spolfast > 180){spolfast <- spolfast-360}
		spolfast <- abs(spolfast/2)
		spolfast <- round(abs(spolfast),3)
		null <- FALSE
		if(spolfast < 20 | spolfast > 70){null <- TRUE}
		bandang <- -12345 #Not calculated yet
		pickgrade <- trimws(east$HEAD$values[[which(east$HEAD$names == "kt5")]])
		error <- read.table(paste0(cmpname,".error"))
		lambdamax <- round(max(error$V3),9)
		ndf <- log$ndf
		lambda_min <- round(log$lambda2_min,0)
		s <- as.numeric(east$HEAD$values[[which(east$HEAD$names == "t5")]])
		o <- as.numeric(east$HEAD$values[[which(east$HEAD$names == "o")]])
		ttime <- round(s-o,5)
		if(o == -12345){ttime <- -12345;print("Origin time not set")}
		maxfreq <-round(maxfreqv[i],5)
		
		line <- cbind(event,stat,slat,slon,cuspid,year,doy_det,evla,evlo,distevstat,depthkm,mag,baz,spol,Dspol,wbeg,wend,dist_ruap_km,dist_ruap_deg,SNR,tlag,Dtlag,fast,Dfast,anginc,anginc_corr,type_ini,timestamp,comment,nyquist,gradeABCNR,filt_lo,filt_HI,spolfast,bandang,pickgrade,lambdamax,ndf,lambda_min,ttime,maxfreq,rayp,null)
		line <- as.data.frame(line)
		print("##################################")
		print(event)
		print(paste0("fast = ",fast," +/- ",Dfast))
		print(paste0("tlag = ",tlag," +/- ",Dtlag))
		print("##################################")
		if(!exists('fline')){fline <- line}else{fline <- rbind(fline,line)}
		###Create all six plots
		#all6(path=path,name=cmpname,trip=trip,low=filter$low,high=filter$high) #all6 is not a priority
		}  ## ilognew check '}'
		## Clean files
		if (dir.exists("output")){}else{dir.create("output")}
		pat <- paste0(cmpname,".*")
		ls2 <- list.files(path,pattern=pat)
		inif <- list.files(pattern="*.ini$")
		l2f <- ls2 %in% inif
		ls2 <- ls2[!l2f]
		for (j in 1:length(ls2)){file.copy(ls2[j],"output",overwrite=TRUE);file.remove(ls2[j])}
		
	}
	file.remove("df.txt")

if(exists('fline')){return(fline)}else{return(NULL)}


}