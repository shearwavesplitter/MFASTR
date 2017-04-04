#### Runs MFAST on a directory with either the normal or very local default settings
#' @title Run MFAST
#' @description Run shear wave splitting measurements on a folder of events
#' @param path Path to folder 
#' @param sheader SAC header the S-wave pick is stored in
#' @param type Which of the MFAST default settings and filters to use
#' @param filtnum Number of filters to test
#' @param tvelpath Path to a .tvel file containing the velocity model (overrides tvel)
#' @param tvel A tvel file read with readtvel (ak135_alp and ak135_taupo are already loaded)	
#' $details Component suffixes are determined automatically
#' @export
#' @examples
#' # Run on measurements the normal sample data
#' write_sample("~/mfast/sample_data/raw_data")
#' do_station_simple(path="~/mfast/sample_data/raw_data")
#' 
#' # Run on measurements the verylocal sample data where the S-pick is stored in the t5 header
#' write_sample("~/mfast/sample_data/raw_data",type="verylocal")
#' do_station_simple(path="~/mfast/sample_data/raw_data",type="verylocal",sheader="t5")
do_station_simple <- function(path,sheader="t0",type="normal",filtnum=3,tvelpath=NULL,tvel=ak135_alp) {
	setwd(path)
	if(file.exists("output")){print("WARNING: This folder already contains an output folder and will be over written")}
### Determine suffixes
 	fls <- setdiff(list.files(),list.dirs(recursive=FALSE,full.names=FALSE))
	sfx <- sfx <- unique(gsub(".*[[:punct:]]","",fls,perl=TRUE))
	if(length(sfx) < 3){stop("Components missing")}
	cmpz <- fixcomps(sfx)
	if(length(which(cmpz == "E")) > 1){stop("Inconsistent suffixes")}
	if(length(which(cmpz == "N")) > 1){stop("Inconsistent suffixes")}
	if(length(which(cmpz == "V")) > 1){stop("Inconsistent suffixes")}
	if(length(which(cmpz == "1")) > 1){stop("Inconsistent suffixes")}
	if(length(which(cmpz == "2")) > 1){stop("Inconsistent suffixes")}
	punctl <- nchar(sfx[1])
	sl <- nchar(fls[1])
	punct <- substring(fls[1],sl-punctl,sl-punctl)
	suffe <- paste0(punct,sfx[which(cmpz == "E" | cmpz == "1")])
	suffn <- paste0(punct,sfx[which(cmpz == "N" | cmpz == "2")])
	suffz <- paste0(punct,sfx[which(cmpz == "V")])

##
	if(type=="normal"){nwbeg=5;fdmin=0.3;fdmax=8;t_win_freq=3;tlagscale=1;snrmax=3;t_win_snr=3;t_err=0.05}
	if(type=="verylocal"){nwbeg=5;fdmin=0.3;fdmax=16;t_win_freq=0.75;tlagscale=0.4;snrmax=1.5;t_win_snr=0.75;t_err=0.02}
	
	print(paste0("Running MFAST with ",type," filters"))
	print(paste0("File suffixes are set to ",suffe," for East, ",suffn," for North and ",suffz," for vertical"))
	print(paste0("Default values for ",type," have been selected"))
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



	if(is.null(tvelpath)){print("Using stored velocity model")}else{tvel <- readtvel(path)}
	ls_east <- list.files(pattern=paste0("\\",suffe,"$"))
	ls_all <- gsub(paste0(" *",suffe),"",ls_east)
	for (i in 1:length(ls_all)){
		event <- ls_all[i]
		trip <- readtriplet(event,E=suffe,N=suffn,Z=suffz,header=sheader)
		filts <- filter_spread(trip,type=type,snrmax=snrmax,t_win_snr=t_win_snr,t_err=t_err)
		filts <- subset(filts, snrv > 2); print("Removing filters with SNR < 2")
		anginc <- anginc(tvel,trip)
		if (length(filts$high > 0)){
			maxfreq <- createini(path,trip,filts,event,filtnum,E=suffe,N=suffn,Z=suffz,nwbeg=nwbeg,fdmin=fdmin,fdmax=fdmax,t_win_freq=t_win_freq,tlagmax=tlagscale)
			f <- writesac_filt(path,trip,event,filts,number=filtnum,E=suffe,N=suffn,Z=suffz)
			run_mfast(path,event,f)
			summline <- logfiles(path,event,trip,f,maxfreq,anginc=anginc)
			if(i == 1){summary <- summline}else{summary <- rbind(summary,summline)}
		}else{print("No good filters found")}
	}

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

