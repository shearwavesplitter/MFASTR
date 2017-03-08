run_mfast <- function(path,name,filtlist) {
	setwd(path)
	if(is.null(filtlist)){
			cmpname <- name
			file.rename(paste0(cmpname,".ini"), "ass.ini")
			print(paste0("Running MFAST on ",cmpname))
			system(paste0("(echo ",cmpname," | ~/MFASTR/split_mfm/bin/ass_mfm && touch ",cmpname,".assdone) >/dev/null"))
			file.rename( "ass.ini",paste0(cmpname,".ini"))
		}else{
		for (i in 1:length(filtlist$high)){
			filter <- filtlist[i,]
			cmpname <- paste0(name,".",filter$low,"-",filter$high,".fb",i)
			file.rename(paste0(cmpname,".ini"), "ass.ini")
			print(paste0("Running MFAST on ",cmpname))
			system(paste0("(echo ",cmpname," | ~/MFASTR/split_mfm/bin/ass_mfm && touch ",cmpname,".assdone) >/dev/null"))
			file.rename( "ass.ini",paste0(cmpname,".ini"))
		}
	}
}