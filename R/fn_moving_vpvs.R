#' @title vP/vS moving average
#' @description A moving average of vP/Vs 
#' @param vpvs A vector of vP/vS ratios
#' @param year A vector of years for each vP/vS
#' @param doy_det A vector of julian days for each vP/vS
#' @param windowlength Size of the averaging window (in days)
#' @param windowspeed Speed of advancing window (days per sample)
#' @return A dataframe containing the end days of each window along with its mean, standard deviation (of the mean), median, upper and lower 95% confidence intervals of the median, and the number of samples
#' @export
moving_vpvs <- function(vpvs,year,doy_det,windowlength,windowspeed){
	tlag <- vpvs #Just incase things are stored as factors (why R, why?)
	fakesumm <- cbind(tlag,year,doy_det)
	fakesumm <- as.data.frame(fakesumm,stringsAsFactors=FALSE)
	fakesumm$year <- as.numeric(as.character(fakesumm$year))
	fakesumm$doy_det <- as.numeric(as.character(fakesumm$doy_det))
	fakesumm$tlag <- as.numeric(as.character(fakesumm$tlag))
	clc <- moving_dt(fakesumm,windowlength-1,windowspeed,norm=FALSE) #minus 1 windowlength due to bug in moving_dt (0 equal 1 day window)
return(clc)
}