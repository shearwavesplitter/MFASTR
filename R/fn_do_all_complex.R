#' @title Run MFAST on multiple stations with more options
#' @description Run shear wave splitting measurements on multiple folders/stations
#' @param path Path to folder containing folders with events 
#' @param sheader SAC header the S-wave pick is stored in
#' @param nwbeg number of start times tested
#' @param fdmin Minimum allowed dominant frequency
#' @param fdmax Maximum allowed dominant frequency 
#' @param t_win_freq Window to calculate the dominant frequency (s)
#' @param tlagmax Maximum allowed time delay (s)
#' @param Ncmin Minimum number of points in an acceptable cluster 
#' @param Mmax maximum number of clusters
#' @param snrmax Minimum snr allowed for a good filter
#' @param t_win_snr Window for SNR  (s)
#' @param t_err Modification to t_win_snr to account for error in S-pick  (s)
#' @param filter User defined set of filters (this overrides the filter selected with type).
#' @param type Which of the MFAST default settings and filters to use. If a P-wave pick is present, type="verylocal" uses it to set t_win_snr
#' @param filtnum Number of filters to test
#' @param tvelpath Path to a .tvel file containing the velocity model (overrides tvel)
#' @param tvel A tvel file read with readtvel (ak135_alp and ak135_taupo are already loaded)
#' @param suffe Suffix of east component 
#' @param suffn Suffix of north component 
#' @param suffz Suffix of vertical component 
#' @param no_threads Number of threads to run measurements on. Set to 1 for verbose mode. Defaults to the number of cores
#' @return A dataframe containing a summary of all the stations
#' @export
#' @examples
#' # Run on measurements three folders of the normal sample data
#' write_sample("~/mfast/sample_data/raw_data")
#' write_sample("~/mfast/sample_data/raw_data2")
#' write_sample("~/mfast/sample_data/raw_data3")
#' do_all_complex(path="~/mfast/sample_data")


do_all_complex <- function(path,sheader="t0",nwbeg=5,fdmin=0.3,fdmax=8,t_win_freq=3,tlagmax=1,Ncmin=5,Mmax=15,snrmax=3,t_win_snr=3,t_err=0.02,filtnum=3,type="normal",filter=NULL,tvelpath=NULL,tvel=ak135_alp,suffe=".e",suffn=".n",suffz=".z",zerophase=TRUE,no_threads=NULL) {
	ls <- list.dirs(path,recursive=FALSE)

	del <- list.files(path,recursive=TRUE,pattern="*.summ$$")
	if(length(del) > 0){file.remove(paste0(path,"/",del))}
	st <- Sys.time()
	k <- lapply(ls,do_station_complex,sheader=sheader,nwbeg=nwbeg,fdmin=fdmin,fdmax=fdmax,t_win_freq=t_win_freq,tlagmax=tlagmax,Ncmin=Ncmin,Mmax=Mmax,snrmax=snrmax,t_win_snr=t_win_snr,t_err=t_err,type=type,filtnum=filtnum,filter=filter,tvelpath=tvelpath,tvel=tvel,zerophase=zerophase,no_threads=no_threads)



			
		k <- k[!is.null(k)]
		
	fsumm <-  do.call(rbind.data.frame, k)



	date <- Sys.Date()
	yr <- as.numeric(substring(date,0,4))
	mn <- as.numeric(substring(date,6,7))
	dy <- as.numeric(substring(date,9,10))
	jday <- tojul(yr,mn,dy)-tojul(yr,1,1)+1

	dirn <- paste0(path,"/",basename(path))
	write.table(fsumm,file=paste0(dirn,".",jday,".summ"),quote=FALSE,row.names=FALSE,sep=",")

	
	czname <- list.files(path,recursive=TRUE,pattern=glob2rx("CZ_*.summ"))
	
	fn <- function(x) try(read.csv(x))

	cz <- lapply(paste0(path,"/",czname),fn)
	czall <-  do.call(rbind.data.frame, cz)
	write.table(czall,file=paste0(dirn,".",jday,".CZ.summ"),quote=FALSE,row.names=FALSE,sep=",")

	abname <- list.files(path,recursive=TRUE,pattern=glob2rx("AB_*.summ"))
	ab <- lapply(paste0(path,"/",abname),fn)
	aball <-  do.call(rbind.data.frame, ab)
	write.table(aball,file=paste0(dirn,".",jday,".AB.summ"),quote=FALSE,row.names=FALSE,sep=",")

mean.axial <- function(vec){
	vec <- vec*2
	vec <- vec*pi/180
	n <- length(vec)
	C <- (1/n)*sum(cos(vec))
	S <- (1/n)*sum(sin(vec))
	m <- atan2(S,C)
	m2 <- m/2
	m2 <- m2*(180/pi)
return(m2)
}

	unstat <- as.character(unique(fsumm$stat))
	fsumm$fast <- as.numeric(as.character(fsumm$fast))
	fsumm$tlag<- as.numeric(as.character(fsumm$tlag))
	for (i in 1:length(unstat)){
		sub <- subset(fsumm, stat == unstat[i])
		n <- length(sub$fast)
		if (n > 1){
			m <- mean.axial(sub$fast)
		}else{ 
			m <- NA
		}
		t <- mean(sub$tlag)
		subAB <- subset(aball, stat == unstat[i])
		nAB <- length(subAB$fast)
		
		if (nAB > 1){
			mAB <- mean.axial(subAB$fast)
		}else{ 
			mAB <- subAB$fast[1]
		}
		if(nAB == 0){mAB <- NA}
		if(nAB > 0){tAB <- mean(subAB$tlag)}else{tAB <- NA}

		subCZ <- subset(czall, stat == unstat[i])
		nCZ <- length(subCZ$fast)
		
		if (nAB > 1){
			mCZ <- mean.axial(subCZ$fast)
		}else{ 
			mCZ <- subCZ$fast[1]
		}
		if(nCZ == 0){mCZ <- NA}
		if(nCZ > 0){tCZ <- mean(subCZ$tlag)}else{tCZ <- NA}
		
		vec <- cbind(unstat[i],round(m,2),round(t,2),n,round(mAB,2),round(tAB,2),nAB,round(mCZ,2),round(tCZ,2),nCZ)
		if(i == 1){fvec <- vec}else{fvec <- rbind(fvec,vec)}
	
	}
	
	fvec <- as.data.frame(fvec)
	colnames(fvec) <- c("Station","fast","tlag","#","ABfast","ABtlag","AB#","CZfast","CZtlag","CZ#")
	write.table(fvec,file=paste0(dirn,".",jday,".means"),quote=FALSE,row.names=FALSE,sep=",")
	et <- Sys.time()
	print("Total run time for all stations:")
	print(et-st)
return(fvec)
}

