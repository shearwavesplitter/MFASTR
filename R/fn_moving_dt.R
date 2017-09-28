#' @title Delay time moving average
#' @description A moving average of delay time 
#' @param summfile A dataframe containing a summary file (i.e. from readmfast)
#' @param windowlength Size of the averaging window (in days)
#' @param windowspeed Speed of advancing window (days per sample)
#' @return A dataframe containing the end days of each window along with its mean, standard deviation (of the mean), median, upper and lower 95% confidence intervals of the median, and the number of samples
#' @export
moving_dt <- function(summfile,windowlength,windowspeed){


	yr <- summfile$year-min(summfile$year)
	day <- summfile$doy_det+yr*365

	maxday <- max(day)
	minday <- min(day)

	wns <- floor((maxday-(minday+windowlength))/windowspeed)+1

	m <- rep(NA,wns)
	med <- rep(NA,wns)
	d <- rep(NA,wns)
	sdm <- rep(NA,wns)
	umed <- rep(NA,wns)
	lmed <- rep(NA,wns)
	n <- rep(0,wns)
	for(i in 1:wns){
		startday <- minday+(i-1)*windowspeed
		endday <- startday+windowlength
		d[i] <- endday
		sub <- subset(summfile, day >= startday & day <= endday)
		if(length(sub$tlag) > 0){
			m[i] <- mean(sub$tlag)
			sdm[i] <- sd(sub$tlag)
			med[i] <- median(sub$tlag)
			vl <- round(length(sub$tlag)/2-(1.96*sqrt(length(sub$tlag)))/2,0)
			vu <- round(1+length(sub$tlag)/2+(1.96*sqrt(length(sub$tlag)))/2,0)
				if(vu %in% order(sub$tlag)){umed[i] <- sub$tlag[order(sub$tlag)][vu]}else{umed[i] <- NA}
				if(vl %in% order(sub$tlag)){lmed[i] <- sub$tlag[order(sub$tlag)][vl]}else{lmed[i] <- NA}
			n[i] <- length(sub$tlag)
		}
	}

ret <- as.data.frame(cbind(d,m,med,sdm,umed,lmed,n),stringsAsFactors=FALSE)
colnames(ret) <- c("day","mean","median","meansd","upper95median","lower95median","n")
return(ret)
}