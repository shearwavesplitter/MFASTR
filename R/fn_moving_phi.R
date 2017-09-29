#' @title Fast polarisation moving average
#' @description A moving average of fast polarisation
#' @param summfile A dataframe containing a summary file (i.e. from readmfast)
#' @param windowlength Size of the averaging window (in days)
#' @param windowspeed Speed of advancing window (days per sample)
#' @return A dataframe containing the end days of each window along with its mean, median fast polarisation, and 95% confidence intervals of the mean (from a bootstrapped mle von Mises fit)
#' @export
moving_phi <- function(summfile,windowlength,windowspeed){

	yr <- summfile$year-min(summfile$year)
	day <- summfile$doy_det+yr*365

	maxday <- max(day)
	minday <- min(day)

	wns <- floor((maxday-(minday+windowlength))/windowspeed)+1

	m <- rep(NA,wns)
	med <- rep(NA,wns)
	d <- rep(NA,wns)
	lwr <- rep(NA,wns)
	hir <- rep(NA,wns)
	for(i in 1:wns){
		startday <- minday+(i-1)*windowspeed
		endday <- startday+windowlength
		d[i] <- endday
		sub <- subset(summfile, day >= startday & day <= endday)
		if(length(sub$tlag) > 0){
			m[i] <- as.numeric(deg(mean.circular(circular(rad(sub$fast)*2))/2))
			med[i] <- as.numeric(deg(median.circular(circular(rad(sub$fast)*2))/2))
			rs <- mle.vonmises.bootstrap.ci(circular(rad(sub$fast*2)))
			lwr[i] <- deg(as.numeric(rs$mu.ci[1]))/2
			hir[i] <- deg(as.numeric(rs$mu.ci[2]))/2
		}
	}

	for(i in 1:length(lwr)){
		if(!is.na(lwr[i])){if(lwr[i] > 90){lwr[i] <- lwr[i]-180}}
		if(!is.na(lwr[i])){if(lwr[i] < -90){lwr[i] <- lwr[i]+180}}
		if(!is.na(hir[i])){if(hir[i] > 90){hir[i] <- hir[i]-180}}
		if(!is.na(hir[i])){if(hir[i] < -90){hir[i] <- hir[i]+180}}
	}

ret <- as.data.frame(cbind(d,m,med,lwr,hir),stringsAsFactors=FALSE)
ret$m <- circular(ret$m,template="geographics",units="degrees")
ret$lwr <- circular(ret$lwr,units="degrees")
ret$hir <- circular(ret$hir,units="degrees")
ret$med <- circular(ret$med,template="geographics",units="degrees")
colnames(ret) <- c("day","mean","median","lower95mean","upper95mean")
return(ret)
}