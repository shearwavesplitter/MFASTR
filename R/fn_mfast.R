#' @title Run splitting measurement
#' @description Runs shearwave splitting measurements on a set of filtered SAC files
#' @param path Path to folder
#' @param name Name of event
#' @param filtlist A dataframe of the best filters to be used (output of writesac_filt)
#' @export
#' @examples
#' # Run shear wave splitting measurements on event 2002.054.09.47.lhor2
#' pathto <- "~/mfast/sample_data/raw_data"
#' write_sample(pathto)
#' event <- "2002.054.09.47.lhor2"
#' triplet <- readtriplet(event,path=pathto)
#' bestfilt <- filter_spread(triplet)
#' maxfreq <- createini(pathto,triplet,bestfilt,event)
#' f <- writesac_filt(pathto,triplet,event,bestfilt)
#' run_mfast(pathto,event,f)
run_mfast <- function(path,name,filtlist) {
	setwd(path)
	if(is.null(filtlist)){
			cmpname <- name
			#file.rename(paste0(cmpname,".ini"), "ass.ini")
			print(paste0("Running MFAST on ",cmpname))
			system(paste0("(echo ",cmpname," | ~/MFASTR/split_mfm/bin/ass_mfm && touch ",cmpname,".assdone) >/dev/null"))
			#file.rename( "ass.ini",paste0(cmpname,".ini"))
		}else{
		for (i in 1:length(filtlist$high)){
			filter <- filtlist[i,]
			cmpname <- paste0(name,".",filter$low,"-",filter$high,".fb",i)
			#file.rename(paste0(cmpname,".ini"), "ass.ini")
			print(paste0("Running MFAST on ",cmpname))
			#Find the location of ass_mfm
			j <- 0
			t <- 1
			while (t == 1){
				j <- j+1
				path <- .libPaths()[j]
				if(file.exists(paste0(path,"/MFASTR/exec/ass_mfm"))){pf <- paste0(path,"/MFASTR/exec/ass_mfm");t <- 2}
				if(is.na(path)){stop("Missing ass_mfm")}
			}
			#Run executable
			system(paste0("(echo ",cmpname," | ",pf," && touch ",cmpname,".assdone) >/dev/null"))
			#file.rename( "ass.ini",paste0(cmpname,".ini"))
		}
	}
}