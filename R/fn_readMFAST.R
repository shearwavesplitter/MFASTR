#' @title Read MFAST .summ file
#' @description Reads a .summ output from the original MFAST codes
#' @param path The path the the summary file
#' @return A dataframe containing the summary file
#' @details This function is used with grade() to grade .summ files produced using the original MFAST codes (by setting mfast=TRUE).
#' @export
readmfast <- function(path){
	tab <- read.csv(path,skip=1,header=F)
	colnames(tab) <-  c("event","stat","slat","slon","cuspid","year","doy_det","evla","evlo","distevstat","depthkm","mag","baz","spol","Dspol","wbeg","wend","dist_ruap_km","dist_ruap_deg","SNR","tlag","Dtlag","fast","Dfast","anginc","anginc_corr","type_ini","timestamp","comment","nyquist","gradeABCNR","filt_lo","filt_HI","spolfast","bandang","pickgrade","lambdamax","ndf","lambda_min","ttime","maxfreq")
	null <- rep(FALSE,length(tab$fast))
	for(i in 1:length(tab$fast)){
		if(tab$spolfast[i] < 20 | tab$spolfast[i] > 70){null[i] <- TRUE}
	}
	tab <- cbind(tab,null)
	return(tab)

}