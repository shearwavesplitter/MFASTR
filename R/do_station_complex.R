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
do_station_complex <- function(path,sheader="t0",nwbeg=5,fdmin=0.3,fdmax=8,t_win_freq=3,tlagmax=1,Ncmin=5,Mmax=15,snrmax=3,t_win_snr=3,t_err=0.02,filtnum=3,type="normal",filter=NULL,tvelpath=NULL,tvel=ak135_alp,suffe=".e",suffn=".n",suffz=".z") {
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
	if(length(ls_east) < 1){stop("No East components found")}

	check1 <- checkcomp(path,E=suffe,N=suffn,Z=suffz)
	if (check1){
		stop("All events have components missing")	
	}
	check2 <- checkspick(path,header=sheader,E=suffe,N=suffn,Z=suffz)
	if (check2){
		stop("All events have Spicks missing")	
	}



	if(is.null(tvelpath)){print("Using stored velocity model")}else{tvel <- readtvel(tvelpath)}
	ls_east <- list.files(pattern=paste0("\\",suffe,"$"))
	ls_all <- gsub(paste0(" *",suffe),"",ls_east)
	for (i in 1:length(ls_all)){
		event <- ls_all[i]
		trip <- readtriplet(event,E=suffe,N=suffn,Z=suffz,header=sheader)
		filts <- filter_spread(trip,type=type,filter=filter,snrmax=snrmax,t_win_snr=t_win_snr,t_err=t_err)
		filts <- subset(filts, snrv > 2); print("Removing filters with SNR < 2")
		if (length(filts$high > 0)){
			anginc <- anginc(tvel,trip)
			maxfreq <- createini(path,trip,filts,event,filtnum,E=suffe,N=suffn,Z=suffz,nwbeg=nwbeg,fdmin=fdmin,fdmax=fdmax,t_win_freq=t_win_freq,tlagmax=tlagscale,Ncmin=Ncmin,Mmax=Mmax)
			f <- writesac_filt(path,trip,event,filts,number=filtnum,E=suffe,N=suffn,Z=suffz)
			run_mfast(path,event,f)
			summline <- logfiles(path,event,trip,f,maxfreq,anginc=anginc)
		if(!is.null(summline)){
			if(!exists('summary1')){summary1 <- summline}else{summary1 <- rbind(summary1,summline)}
		}
			rm(event,trip,filts,anginc,maxfreq,f,summline)
		}else{print("No good filters found")}
	}

if(exists('summary1')){summary <- summary1}else{print("No good filters for all events");return(NULL)}

## Zip output folder -- doesn't work if there is no program or it isn't where R looks for it
stat <- as.character(trip[[1]]$sta)
inilist <- list.files(paste0(path,"/output"),pattern=".ini")
inidir <- paste0(stat,".ini_files")
if(dir.exists(inidir)){}else{dir.create(inidir)}
for (j in 1:length(inilist)){file.copy(paste0("output/",inilist[j]),inidir,overwrite=TRUE)}
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
print(paste0("Station ",stat," done"))
return(summary)
}

