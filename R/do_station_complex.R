#### Runs MFAST on a directory with mpre options
#' @title Run MFAST with more options
#' @description Run shear wave splitting measurements on a folder of events with more options
#' @param path Path to folder 
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
#' @param downsample Downsample if sampling rate is less than 0.01s (Defaults to FALSE, originally used to decrease computational loads)
#' @param biglong logical, TRUE=long=8 bytes
#' @param Iendian Endian-ness of the data: 1,2,3: "little", "big", "swap". Default = 1 (little)
#' @return A dataframe containing the summary file
#' @export
#' @examples
#' # Run on measurements the normal sample data with defaults
#' write_sample("~/mfast/sample_data/raw_data")
#' do_station_complex(path="~/mfast/sample_data/raw_data")
#'
#' # Run measurements with your own defined filters
#' filt_low <- c(0.1,0.2,0.5)
#' filt_high <- c(1,2,3)
#' filts <- cbind(filt_low,filt_high)
#' write_sample("~/mfast/sample_data/raw_data")
#' do_station_complex(path="~/mfast/sample_data/raw_data",filter=filts)
do_station_complex <- function(path,sheader="t0",nwbeg=5,fdmin=0.3,fdmax=8,t_win_freq=3,tlagmax=1,Ncmin=5,Mmax=15,snrmax=3,t_win_snr=3,t_err=0.02,filtnum=3,type="normal",filter=NULL,tvelpath=NULL,tvel=ak135_alp,suffe=".e",suffn=".n",suffz=".z",zerophase=TRUE,no_threads=NULL,mc.preschedule=TRUE,downsample=FALSE,biglong=FALSE,Iendian=1) {
	setwd(path)
	tlagscale <- tlagmax
	if(file.exists("output")){print("WARNING: This folder already contains an output folder and will be over written")}
	
	print(paste0("Running MFAST with do_station_complex"))
	print(paste0("File suffixes are set to ",suffe," for East, ",suffn," for North and ",suffz," for vertical"))
	print(paste0("Values selected are:"))
	print(paste0("nwbeg = ",nwbeg))
	print(paste0("fdmin = ",fdmin))
	print(paste0("fdmax = ",fdmax))
	print(paste0("t_win_freq = ",t_win_freq))
	print(paste0("tlagmax = ",tlagscale))
	print(paste0("snrmax = ",snrmax))
	print(paste0("t_win_snr = ",t_win_snr))
	print(paste0("t_err = ",t_err))
	
	ls_east <- list.files(pattern=paste0("\\",suffe,"$"))
	if(length(ls_east) < 1){warning("No East components found");return(NULL)}

	check1 <- checkcomp(path,E=suffe,N=suffn,Z=suffz)
	if (check1){
		warning("All events have components missing");return(NULL)
	}
	check2 <- checkspick(path,header=sheader,E=suffe,N=suffn,Z=suffz)
	if (check2){
				warning(paste0("All events have Spicks missing in the ",sheader," header"));return(NULL)
	}




	if(is.null(tvelpath)){print("Using stored velocity model")}else{tvel <- readtvel(tvelpath)}
	ls_east <- list.files(pattern=paste0("\\",suffe,"$"))
	ls_all <- gsub(paste0(" *",suffe),"",ls_east)

parallel2 <- function(event,suffe,suffn,suffz,sheader,filtnum,tvel,type,nwbeg,fdmin,fdmax,t_win_freq,tlagscale,snrmax,t_win_snr,t_err,zerophase){
		trip <- readtriplet(event,E=suffe,N=suffn,Z=suffz,header=sheader,downsample=downsample,biglong=biglong,Iendian=Iendian)
		filts <- filter_spread(trip,type=type,snrmax=snrmax,t_win_snr=t_win_snr,t_err=t_err,zerophase=zerophase)
		filts <- subset(filts, snrv > 2); print("Removing filters with SNR < 2")
		#stname <- as.character(trip[[1]]$sta)
		if (length(filts$high > 0)){
			anginc <- anginc(tvel,trip)
			maxfreq <- createini(path,trip,filts,event,filtnum,E=suffe,N=suffn,Z=suffz,nwbeg=nwbeg,fdmin=fdmin,fdmax=fdmax,t_win_freq=t_win_freq,tlagmax=tlagscale,zerophase=zerophase)
			f <- writesac_filt(path,trip,event,filts,number=filtnum,E=suffe,N=suffn,Z=suffz,zerophase=zerophase)
			run_mfast(path,event,f)
			summline <- logfiles(path,event,trip,f,maxfreq,anginc=anginc)
			return(summline)
		}else{print("No good filters found")}

	if(exists('summary1',inherits=FALSE)){return(summary1)}else{return(NULL)}
}



nc <- parallel::detectCores()
if(is.null(no_threads)){no_threads <- speed(path)[1]}
st <- Sys.time()
print(paste0("Start time: ",st))


if(no_threads > length(ls_all)){no_threads <- length(ls_all)}
if(no_threads > 1){
	silent <- TRUE
	print("For verbose mode set no_threads=1")

}else{silent <- FALSE}
		print(paste0("Running ", length(ls_all)," events on ",no_threads," threads across ",nc," cores"))
		summary1 <- mclapply2(ls_all,parallel2,suffe=suffe,suffn=suffn,suffz=suffz,sheader=sheader,filtnum=filtnum,tvel=tvel,type=type,nwbeg=nwbeg,fdmin=fdmin,fdmax=fdmax,t_win_freq=t_win_freq,tlagscale=tlagscale,snrmax=snrmax,t_win_snr=t_win_snr,t_err=t_err,zerophase=zerophase,mc.cores =no_threads,mc.silent=silent,mc.preschedule=mc.preschedule)

############ This section is for windows parallelisations if this package were to be made windows compatible. We will use mclapply for now instead. This also has potential to parallelise over multiple systems
#if(no_cores == 1){
#	summary1 <- lapply(ls_all,parallel2,suffe=suffe,suffn=suffn,suffz=suffz,sheader=sheader,filtnum=filtnum,tvel=tvel,type=type,nwbeg=nwbeg,fdmin=fdmin,fdmax=fdmax,t_win_freq=t_win_freq,tlagscale=tlagscale,snrmax=snrmax,t_win_snr=t_win_snr,t_err=t_err,zerophase=zerophase)
#}else{
#	print(paste0("Running ", length(ls_all)," events on ",no_cores," cores"))
#	print("For verbose mode set no_cores=1")
#	cl <- makeCluster(no_cores)
#	clusterEvalQ(cl, library(MFASTR))
#		summary1 <- parLapply(cl,ls_all,parallel2,suffe=suffe,suffn=suffn,suffz=suffz,sheader=sheader,filtnum=filtnum,tvel=tvel,type=type,nwbeg=nwbeg,fdmin=fdmin,fdmax=fdmax,t_win_freq=t_win_freq,tlagscale=tlagscale,snrmax=snrmax,t_win_snr=t_win_snr,t_err=t_err,zerophase=zerophase)
#	stopCluster(cl)
#}
#######
summary1 <- do.call(rbind.data.frame, summary1)

if(exists('summary1',inherits=FALSE)){summary <- summary1}else{print("No good filters for all events");return(NULL)}

## Zip output folder -- doesn't work if there is no program or it isn't where R looks for it
stat <- basename(getwd())
inilist <- list.files(path,pattern=".ini$")
inidir <- paste0(stat,".ini_files")
if(dir.exists(inidir)){}else{dir.create(inidir)}
for (j in 1:length(inilist)){file.copy(inilist[j],inidir,overwrite=TRUE);file.remove(inilist[j])}
#print("Zipping output")
list <- list.files(paste0(path,"/output"))
#zip("output",paste0("output/",list),extras=">/dev/null")
#unlink("output",recursive=TRUE)

#Create summ file
date <- Sys.Date()
yr <- as.numeric(substring(date,0,4))
mn <- as.numeric(substring(date,6,7))
dy <- as.numeric(substring(date,9,10))
n <- tojul(yr,mn,dy)-tojul(yr,1,1)+1
summname <- paste0(stat,".",n,".summ")
write.table(summary,file=summname,quote=FALSE,row.names=FALSE,sep=",")
summdir <- paste0(stat,".summ_files")
if(dir.exists(summdir)){}else{dir.create(summdir)}
file.copy(summname,summdir,overwrite=TRUE)
file.remove(summname)
grade(paste0(summdir,"/",summname),minsnr=3,tlagmax=tlagscale)
print(paste0(stat," done"))
et <- Sys.time()
print("Total run time:")
print(et-st)
summ <- read.csv(paste0(summdir,"/",summname))

return(summ)
}

