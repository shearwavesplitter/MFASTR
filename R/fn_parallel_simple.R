#' @title Parallel 
#' @description Run shear wave splitting measurements on multiple stations in parallel
#' @param path Path to folder containing the folders for each station
#' @param cores Number of cores to use. Defaults to one less than the total
#' @param sheader SAC header the S-wave pick is stored in
#' @param type Which of the MFAST default settings and filters to use
#' @param filtnum Number of filters to test
#' @param tvelpath Path to a .tvel file containing the velocity model (overrides tvel)
#' @param tvel A tvel file read with readtvel (ak135_alp and ak135_taupo are already loaded)	
#' @details Component suffixes are determined automatically
#' @return A list containing the summary files for all stations
#' @export
#' @examples
#' # Run on measurements the normal sample data in parallel
#' write_sample("~/mfast/sample_data/raw_data1")
#' write_sample("~/mfast/sample_data/raw_data2")
#' parallel_simple(path="~/mfast/sample_data")
parallel_simple <- function(path,cores=NULL,sheader="t0",type="normal",filtnum=3,tvelpath=NULL,tvel=ak135_alp) {
	require(parallel)
	if(is.null(cores)){no_cores <- detectCores()-1}else{no_cores <- cores}
	ls <- list.dirs(path,recursive=FALSE)
	if(no_cores > length(ls)){no_cores <- length(ls)}
	print(paste0("Running ",length(ls)," stations on ",no_cores," cores..."))
	cl <- makeCluster(no_cores)
	clusterEvalQ(cl, library(MFASTR))
	res <- parLapply(cl,ls,do_station_simple,sheader=sheader,type=type,filtnum=filtnum,tvelpath=tvelpath,tvel=tvel)
	stopCluster(cl)
	print("Done")
}