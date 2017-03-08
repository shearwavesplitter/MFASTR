#' @export
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
			file.rename( "ass.ini",paste0(cmpname,".ini"))
		}
	}
}