#' @title Plot all6
#' @description A wrapper for the all6plot function to plot all events from a station
#' @param path Path to folder containing raw events and output folder
#' @param filter Which filter to plot (e.g. c(1,2,3) for fb1, fb2 & fb3)
#' @param zerophase Were the filters applied zero phase?
#' @param E Vector signal of the east component
#' @param N Vector signal of the north component
#' @param Z Vector signal of the vertical component
#' @export


all6_station <- function(path,filter=c(1,2,3),zerophase=TRUE,E=".e",N=".n",Z=".z") {
	setwd(path)
	namelist <- list.files(pattern=paste0("\\",E,"$"))
	namelist2 <- gsub(paste0("\\",E),"",namelist)
	
	for (i in 1:length(filter)){
		for (j in 1:length(namelist2)){
		namelistsub <- list.files("output",pattern=paste0("fb",filter[i],E,"$"))
		n <- grep(namelist2[j],namelistsub)	
			if (length(n) !=0){
			 all6plot(path=path,filter=filter[i],cuspid=namelist2[j],zerophase=zerophase,E=E,N=N,Z=Z,auto=TRUE)
			}else{print(paste0("Skipping ",namelist2[j],", does not exist for filter ",filter[i]))}
		}
	}
}