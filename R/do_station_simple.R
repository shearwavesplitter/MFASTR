#### Runs MFAST on a directory with either the normal or very local default settings
#' @export
do_station_simple <- function(path="/home/stefan/mfast_package_v2.2/sample_data",sheader="t0",type="normal",filtnum=3,tvelpath="~/Dropbox/MFASTR_dev/velocity/ak135_taupo.tvel",suffe=".e",suffn=".n",suffz=".z") {
	setwd(path)

	if(file.exists("output.zip")){print("WARNING: This folder already contains an output.zip folder and will be over written")}
##
	if(type=="normal"){nwbeg=5;fdmin=0.3;fdmax=8;t_win_freq=3;tlagscale=1;snrmax=3;t_win_snr=3;t_err=0.05}
	if(type=="verylocal"){nwbeg=3;fdmin=0.3;fdmax=16;t_win_freq=0.75;tlagscale=0.4;snrmax=1.5;t_win_snr=0.75;t_err=0.02}
	
	print(paste0("Running MFAST with ",type," filters"))
	print(paste0("File suffixes are set to ",suffe," for East, ",suffn," for North and ",suffz," for vertical"))
	print(paste0("Default values for ",type," have been selected"))
	print(paste0("nwbeg = ",nwbeg))
	print(paste0("fdmin = ",fdmin))
	print(paste0("fdmax = ",fdmax))
	print(paste0("t_win_freq = ",t_win_freq))
	print(paste0("tlagmax/tlagscale = ",tlagscale))
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




	ls_east <- list.files(pattern=paste0("\\",suffe,"$"))
	ls_all <- gsub(paste0(" *",suffe),"",ls_east)
	for (i in 1:length(ls_all)){
		event <- ls_all[i]
		trip <- readtriplet(event,E=suffe,N=suffn,Z=suffz,header=sheader)
		filts <- filter_spread(trip,type=type,snrmax=snrmax,t_win_snr=t_win_snr,t_err=t_err)
		if (length(filts$high > 0)){
			maxfreq <- createini(path,trip,filts,event,filtnum,E=suffe,N=suffn,Z=suffz,nwbeg=nwbeg,fdmin=fdmin,fdmax=fdmax,t_win_freq=t_win_freq,tlagscale=tlagscale)
			f <- writesac_filt(path,trip,event,filts,number=filtnum,E=suffe,N=suffn,Z=suffz)
			run_mfast(path,event,f)
			summline <- logfiles(path,event,trip,f,maxfreq,tvelpath=tvelpath)
			if(i == 1){summary <- summline}else{summary <- rbind(summary,summline)}
		}else{print("No good filters found")}
	}

## Zip output folder
stat <- as.character(trip[[1]]$sta)
inilist <- list.files(paste0(path,"/output"),pattern=".ini")
inidir <- paste0(stat,".ini_files")
if(dir.exists(inidir)){}else{dir.create(inidir)}
for (j in 1:length(inilist)){file.copy(paste0("output/",inilist[j]),inidir)}
print("Zipping output")
list <- list.files(paste0(path,"/output"))
zip("output",paste0("output/",list),extras=">/dev/null")
unlink("output",recursive=TRUE)

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
file.copy(summname,summdir)
file.remove(summname)
print(paste0("Station ",stat," done"))
return(summary)
}

