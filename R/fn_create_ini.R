#This has been created with a combination of T2_local and create_event_ini.sh. Need to check for differences
#' @export
createini <- function(path,trip,filts,name,number=3,E=".e",N=".n",Z=".z",nwbeg=5,fdmin=0.3,fdmax=8,t_win_freq=3,tlagscale=1){
	print("Creating .ini files")
	p <- as.numeric(as.character(trip[[3]]$HEAD$values[[which(trip[[3]]$HEAD$names == "a")]]))
	s <- as.numeric(as.character(trip[[1]]$HEAD$values[[which(trip[[1]]$HEAD$names == "t5")]]))
 	b <- as.numeric(as.character(trip[[1]]$HEAD$values[[which(trip[[1]]$HEAD$names == "b")]]))
	#depmax <- as.numeric(as.character(trip[[1]]$HEAD$values[[which(trip[[1]]$HEAD$names == "depmax")]]))
	if (p == -12345){p <- 0;print("Ppick not found")}
	depmax <- max(trip[[1]]$amp)
	if (depmax < 1e9){nyq_fr <- 1/2/trip[[3]]$dt}
	sminp <- (s-p)/2
#### Signal to noise windows
	signalbeg <- s
	if (s < b){signalbeg <- b} #Not sure what this does, maybe if b is negative?
	signalend <- signalbeg + t_win_freq


# cut a 3 sec window before (noise) and after (signal) S-Pick..
# the spectrum of the signal window should give an estimate of the signal period length (T)
#


#
### Save original values so they can be reset after one loop
	nwbeg0 <- nwbeg
	fdmin0 <- fdmin
	fdmax0 <- fdmax
	t_win_freq0 <- t_win_freq
	tlagscale0 <- tlagscale	
	signalbeg0 <- signalbeg
	signalend0 <- signalend
	sminp0 <- sminp


	
	for(i in 1:number){
		#Reset constants
		nwbeg <- nwbeg0
		fdmin <- fdmin0
		fdmax <- fdmax0
		t_win_freq <- t_win_freq0
		tlagscale <- tlagscale0
		signalbeg <- signalbeg0
		signalend <- signalend0
		sminp <- sminp0
######Calculate the dominant frequency
		if (is.null(filts)){Ewav <- trip[[1]]$amp;Nwav <- trip[[2]]$amp}else{
		filter <- filts[i,]
		Ewav <- butfilt(trip[[1]]$amp, fl=filter$low, fh=filter$high, deltat=trip[[1]]$dt, type="BP" , proto="BU",npoles=2,zp=FALSE) #zp=FALSE so filter isn't zero phase (one pass)
		Nwav <- butfilt(trip[[2]]$amp, fl=filter$low, fh=filter$high, deltat=trip[[2]]$dt, type="BP" , proto="BU",npoles=2,zp=FALSE) #zp=FALSE so filter isn't zero phase (one pass)
		}

		ecut <- cut_simple(Ewav,trip[[1]]$dt,signalbeg,signalend)
		ncut <- cut_simple(Nwav,trip[[2]]$dt,signalbeg,signalend)
		
		ecut <- ecut-mean(ecut)
		ecut <- detrend(ecut)
		ncut <- ncut-mean(ncut)
		ncut <- detrend(ncut)

		ecut <- jadjust.length(ecut) #zeropad (same as sac). This is very important! 
		ncut <- jadjust.length(ncut)
		

		fte <- FRWDft(ecut$signal,length(ecut$signal),0,trip[[1]]$dt)
		ftn <- FRWDft(ncut$signal,length(ncut$signal),0,trip[[2]]$dt)
		ge <- abs(fte$G)
		gn <- abs(ftn$G)
		freq <- ftn$f
		ge <- subset(ge, freq < 10)
		gn <- subset(gn, freq < 10)
		freq <- subset(freq, freq < 10)
		ge <- subset(ge, freq > 0.5)
		gn <- subset(gn, freq > 0.5)
		freq <- subset(freq, freq > 0.5)


		#fte <- fft(ecut)
		#ftn <- fft(ncut)
		#Fs <- 1/(as.integer(trip[[1]]$dt*1000000)/1000000)
		#freq <- seq(0,Fs/2,Fs/length(ecut))
		#fte <- head(fte,length(ecut)/2+1)
		#ftn <- head(ftn,length(ncut)/2+1)
		#fte <- subset(fte, freq < 10)
		#ftn <- subset(ftn, freq < 10)
		#freq <- subset(freq, freq < 10)
		#fte <- subset(fte, freq > 0.5)
		#ftn <- subset(ftn, freq > 0.5)
		#freq <- subset(freq, freq > 0.5)
		maxfe <- freq[which(ge == max(ge))]
		maxfn <- freq[which(gn == max(gn))]
	

		#Limit frequencies to reasonable values
		f1 <- maxfn
		f2 <- maxfe 
		if (f1 < fdmin){f1 <- fdmin}
		if (f2 < fdmin){f2 <- fdmin}
		if (f1 > fdmax){f1 <- fdmax}
		if (f2 > fdmax){f2 <- fdmax}
		fre <- 2*f1*f2/(f1+f2) #harmonic mean to emphasis lower frequencies
		t1 <- 1/f1
		t2 <- 1/f2
		tmid <- 1/fre
		w3 <- tmid/1.2+0.15
		w4 <- tmid*2.5+0.15
		dtend <- 0.08
		nwend <- as.integer((w4-w3)/dtend)

		#Make sure there are sufficent windows
		while(nwend > 25){
			dtend <- dtend*1.1
			nwend <- as.integer((w4-w3)/dtend)
		}
		while (nwend < 15) {
			dtend <- dtend/1.1
			nwend <- as.integer((w4-w3)/dtend)
		}
		maxnoclusters <- 15
		nmin <- 5
		halfsminp <- sminp
 		if ((0.3 + nwbeg*0.2) < halfsminp) {
    			dtbeg <- 0.2
    			toffbeg <- 0.3
    		} else {
			toffbeg <- (halfsminp/(nwbeg+1))
			dtbeg <- toffbeg
		if (toffbeg > 0.1) {toffbeg <- 0.1}
		}
		toffend=w3
	
		t3 <- toffbeg+toffend
		if(t3 > tlagscale){t3 <- tlagscale}
		smallestwind <- t3
		tlagscale <- smallestwind
		dtlagmax <- tlagscale/4
		dfastmax <- 40



		## round some values
		dtend <- round(dtend,7)
		dtlagmax <- round(dtlagmax,6)
		toffend <- round(toffend,6)
		tlagscale <- round(tlagscale,6)
		##Create output
		l1 <- cbind(paste0("'",E,"'"),"ext1")
		l2 <- cbind(paste0("'",N,"'"),             "ext2")
		l3 <- cbind(nwbeg ,            "nwbeg")
		l4 <- cbind(nwend   ,          "nwend")
		l5 <- cbind(dtbeg    ,         "dt_beg")
		l6 <- cbind(dtend    ,         "dt_end")
		l7 <- cbind(dtlagmax   ,         "dtlag_max")
		l8 <- cbind(dfastmax   ,         "dfast_max")
		l9 <- cbind(toffbeg    ,    "t_off_beg")
		l10 <- cbind(toffend    ,    "t_off_end")
		l11 <- cbind(tlagscale  ,     "tlag_scale")
		l12 <- cbind(180     ,    "fast_scale")
		l13 <- cbind(maxnoclusters   ,     "max_no_clusters")
		l14 <- cbind(nmin    ,      "nmin")
		l15 <- cbind(".true." , "OPT_verbose")
		l16 <- cbind(".true." , "OPT_outfile")
	
		out <- rbind(l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16)
		out <- as.data.frame(out)
		if(is.null(filts)){cmpname <- name}else{cmpname <- paste0(name,".",filter$low,"-",filter$high,".fb",i)}
		write.table(out,file=paste0(path,"/",cmpname,".ini"),col.names=FALSE,row.names=FALSE,sep=" ",quote=FALSE)
		if(i == 1){frelist <- fre}else{frelist <- rbind(frelist,fre)}
	}
	frelist <- as.numeric(frelist)
return(frelist)
}