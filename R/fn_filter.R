filter_spread <- function(trip,type="normal",filter=NULL,t_win_snr=3,t_err=0.05,snrmax=3){
## Need to add normal and local filters
	if (type == "normal"){
		f1 <- cbind(0.4,4)
		f2 <- cbind(0.5,5)
		f3 <- cbind(0.2,3)
		f4 <- cbind(0.3,3)
		f5 <- cbind(0.5,4)
		f6 <- cbind(0.6,3)
		f7 <- cbind(0.8,6)
		f8 <- cbind(1,3)
		f9 <- cbind(1,5)
		f10 <- cbind(1,8)
		f11 <- cbind(2,3)
		f12 <- cbind(2,6)
		f13 <- cbind(3,8)
		f14 <- cbind(4,10)
		
		f <- rbind(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14)
		f <- as.data.frame(f)
	}
	if (type == "verylocal"){
		f1 <- cbind(1,5)
		f2 <- cbind(1,8)
		f3 <- cbind(1,15)
		f4 <- cbind(1,30)
		f5 <- cbind(3,5)
		f6 <- cbind(2,8)
		f7 <- cbind(3,15)
		f8 <- cbind(3,30)
		f9 <- cbind(5,10)
		f10 <- cbind(5,15)
		f11 <- cbind(5,30)
		f12 <- cbind(5,45)
		f13 <- cbind(10,20)
		f14 <- cbind(10,45)
		
		f <- rbind(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14)
		f <- as.data.frame(f)
	}
	
	if (is.null(filter)){print(paste0("Finding best filters for ",type))}else{f <- filter; print(paste0("Finding best filters from user defined list"))}
	
	colnames(f) <- c("low","high")
	bandwidth <- f$high/(f$low*2)
	
	prod <- rep(0,length(bandwidth))
	snrv <- rep(0,length(bandwidth))
	nyq <- rep(0,length(bandwidth))
	f <- cbind(f,prod,snrv)
	#### Test all filters
	for (i in 1:length(bandwidth)){
		#Apply filter to record
		s <- as.numeric(as.character(trip[[1]]$HEAD$values[[which(trip[[1]]$HEAD$names == "t5")]]))
		
		low <- f$low[i]
		high <- f$high[i]
		band <- bandwidth[i]
		#plot(trip[[1]]$amp,type="l")
		Efilt <- butfilt(trip[[1]]$amp, fl=low, fh=high, deltat=trip[[1]]$dt, type="BP" , proto="BU",npoles=2,zp=FALSE) #zp=FALSE so filter isn't zero phase (one pass)
		Nfilt <- butfilt(trip[[2]]$amp, fl=low, fh=high, deltat=trip[[2]]$dt, type="BP" , proto="BU",npoles=2,zp=FALSE)
		#Calculate SNR
		dt <- as.integer(trip[[1]]$dt*1000000)/1000000
		nyq_fr <- (1/2/dt)
		while(high >= nyq_fr){
			high <- high - 2
		}
		band <- high/(2*low)
		snr <- snr(Efilt,Nfilt,s,dt,t_win_snr=t_win_snr,t_err=t_err,b=0)
		snrprod <- band*snr
		#if (snr < snrmax){print(paste0("SNR less than ",snrmax))}
		f$prod[i] <- snrprod
		f$snrv[i] <- snr
		f$nyq[i] <- nyq_fr
	}
	f <- subset(f, snrv > snrmax)
	f <- f[order(-f$prod),]
	if (length(f$high > 0)){print(paste0(length(f$high)," filters (SNR > ",snrmax,") found"))}
return(f)
}