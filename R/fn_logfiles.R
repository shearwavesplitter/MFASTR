logfiles <- function(path,name,trip,filtlist,maxfreqv,comment="MFASTR",tvelpath="~/MFASTR/velocity/ak135_taupo.tvel") {
	setwd(path)
	east <- trip[[1]]
	east$HEAD$values <- as.character(east$HEAD$values)
	if(is.null(filtlist)){filtlist <- cbind(NA,NA,NA,NA);filtlist <- as.data.frame(filtlist);colnames(filtlist) <- c("low","high","snrv","nyq")}
	for (i in 1:length(filtlist$high)){
		filter <- filtlist[i,]
		if(is.na(filter$low)){cmpname <- name}else{cmpname <- paste0(name,".",filter$low,"-",filter$high,".fb",i)}
		logname <- paste0(cmpname,".ilognew.ass")
		log <- read.table(logname,header=TRUE)
		###Set up components of .summ file
		event <- as.character(log$event)
		stat <- east$sta
		slat <- round(as.numeric(east$HEAD$values[[which(east$HEAD$names == "stla")]]),4)
		slon <- round(as.numeric(east$HEAD$values[[which(east$HEAD$names == "stlo")]]),3)
		cuspid <- name
		year <- east$DATTIM$yr
		o <- as.numeric(east$HEAD$values[[which(east$HEAD$names == "o")]])
		doy_det <- round(east$DATTIM$jd + east$DATTIM$hr/24 + (east$DATTIM$mi/60)/24 + (((east$DATTIM$sec+o)/60)/60)/24,4)
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
		wend <- log$wend_best #wend is a little bit longer than expected
		dist_ruap_km <- -12345
		dist_ruap_deg <- -12345
		SNR <- round(filter$snrv,5)
		tlag <- round(log$tlag,6)
		Dtlag <- round(log$dtlag,6)
		fast <- round(log$fast,3)
		Dfast <- round(log$dfast,3)
		anginc <- round(anginc(tvelpath,trip),1) #Since we approximate with a layered earth model instead of tauP this may be a few degrees out. Not working quite as expected
		anginc_corr <- "anginc_corr" 
		ini <- read.table(paste0(cmpname,".ini"))
		type_ini <- paste0("ass_",ini$V1[3],"_",ini$V1[4])
		timestamp <- log$time
		comment <- comment
		nyquist <- round(filter$nyq,0)
		gradeABCNR <- as.character(log$grade)
		filt_lo <- filter$low
		filt_HI <- filter$high
		spolfast <- round(abs(spol - fast),2)
		if(spolfast < 20 | spolfast > 70){gradeABCNR <- "NULL"}
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
		
		line <- cbind(event,stat,slat,slon,cuspid,year,doy_det,evla,evlo,distevstat,depthkm,mag,baz,spol,Dspol,wbeg,wend,dist_ruap_km,dist_ruap_deg,SNR,tlag,Dtlag,fast,Dfast,anginc,anginc_corr,type_ini,timestamp,comment,nyquist,gradeABCNR,filt_lo,filt_HI,spolfast,bandang,pickgrade,lambdamax,ndf,lambda_min,ttime,maxfreq)
		line <- as.data.frame(line)
		print("##################################")
		print(event)
		print(paste0("fast = ",fast," +/- ",Dfast))
		print(paste0("tlag = ",tlag," +/- ",Dtlag))
		print("##################################")
		if(i == 1){fline <- line}else{fline <- rbind(fline,line)}
		###Create all six plots
		#all6(path=path,name=cmpname,trip=trip,low=filter$low,high=filter$high) #all6 is not a priority
		## Clean files
		if (dir.exists("output")){}else{dir.create("output")}
		pat <- paste0("*.fb",i,".*")
		ls2 <- list.files(path,pattern=pat)
		for (j in 1:length(ls2)){file.copy(ls2[j],"output");file.remove(ls2[j])}

	}
	file.remove("df.txt")



return(fline)
}