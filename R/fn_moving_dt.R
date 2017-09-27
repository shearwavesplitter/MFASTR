#' @title Delay time moving average
#' @description A moving average of delay time 
#' @param summfile A dataframe containing a summary file (i.e. from readmfast)
#' @param windowlength Size of the averaging window (in days)
#' @param windowspeed Speed of advancing window (in days)
#' @return A dataframe containing the end days of each window along with its mean and median delay time
moving_dt <- function(summfile,windowlength,windowspeed){

	yr <- summfile$year-min(summfile$year)
	day <- summfile$doy_det+yr*365

	maxday <- max(day)
	minday <- min(day)

	wns <- floor((maxday-(minday+windowlength))/windowspeed)+1

	m <- rep(NA,wns)
	med <- rep(NA,wns)
	d <- rep(NA,wns)

	for(i in 1:wns){
		startday <- minday+(i-1)*windowspeed
		endday <- startday+windowlength
		d[i] <- endday
		sub <- subset(summfile, day >= startday & day <= endday)
		if(length(sub$tlag) > 0){
			m[i] <- mean(sub$tlag)
			med[i] <- median(sub$tlag)
		}
	}

ret <- as.data.frame(cbind(d,m,med),stringsAsFactors=FALSE)
colnames(ret) <- c("day","mean","median")
return(ret)
}