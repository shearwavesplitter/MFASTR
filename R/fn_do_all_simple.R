#### Runs MFAST on a directory with either the normal or very local default settings
#' @title Run MFAST on multiple folders
#' @description Run shear wave splitting measurements on more than one folder/station
#' @param path Path to folder containing folders with events
#' @param sheader SAC header the S-wave pick is stored in
#' @param type Which of the MFAST default settings and filters to use
#' @param filtnum Number of filters to test
#' @param tvelpath Path to a .tvel file containing the velocity model (overrides tvel)
#' @param tvel A tvel file read with readtvel (ak135_alp and ak135_taupo are already loaded)	
#' @param no_cores Number of cores to run measurements on. Set to 1 for verbose mode. Defaults to maximum available.
#' @details Component suffixes are determined automatically
#' @return A dataframe containing a summary of all the stations
#' @export
#' @examples
#' # Run on measurements three folders of the normal sample data
#' write_sample("~/mfast/sample_data/raw_data")
#' write_sample("~/mfast/sample_data/raw_data2")
#' write_sample("~/mfast/sample_data/raw_data3")
#' do_all_simple(path="~/mfast/sample_data")

do_all_simple <- function(path,sheader="t0",type="normal",filtnum=3,tvelpath=NULL,tvel=ak135_alp,zerophase=FALSE,no_cores=Inf) {
	ls <- list.dirs(path,recursive=FALSE)

	del <- list.files(path,recursive=TRUE,pattern="*.summ$$")
	if(length(del) > 0){file.remove(paste0(path,"/",del))}

	k <- lapply(ls,do_station_simple,sheader=sheader,type=type,filtnum=filtnum,tvelpath=tvelpath,tvel=tvel,zerophase=zerophase,no_cores=no_cores)



			
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
		print(vec)
		if(i == 1){fvec <- vec}else{fvec <- rbind(fvec,vec)}
	
	}
	
	fvec <- as.data.frame(fvec)
	colnames(fvec) <- c("Station","fast","tlag","#","ABfast","ABtlag","AB#","CZfast","CZtlag","CZ#")
	write.table(fvec,file=paste0(dirn,".",jday,".means"),quote=FALSE,row.names=FALSE,sep=",")
return(fvec)
}

