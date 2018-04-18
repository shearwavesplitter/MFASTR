#' @title Fast polarisation moving average
#' @description A moving average of fast polarisation
#' @param summfile A dataframe containing a summary file (i.e. from readmfast)
#' @param windowlength Size of the averaging window (in days)
#' @param windowspeed Speed of advancing window (days per sample)
#' @return A dataframe containing the end days of each window along with its mean, median fast polarisation, and 95% confidence intervals of the mean (from a bootstrapped mle von Mises fit)
#' @export
moving_phi <- function(summfile,windowlength,windowspeed){
	windowlength=windowlength-1 #Actual window length appears to be windowlength+1 so fixing that here
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

	for(i in 1:length(m)){
		if(!is.na(m[i])){if(m[i] > 90){
            m[i] <- m[i]-180
            lwr[i] <- lwr[i]-180
            hir[i] <- hir[i] -180
        }}
		if(!is.na(m[i])){if(m[i] < -90){
            m[i] <- m[i]+180
            lwr[i] <- lwr[i]+180
            hir[i] <- hir[i]+180
        }}

	}

ret <- as.data.frame(cbind(d,m,med,hir,lwr),stringsAsFactors=FALSE)
ret$m <- circular(ret$m,template="geographics",units="degrees")
ret$lwr <- circular(ret$lwr,units="degrees")
ret$hir <- circular(ret$hir,units="degrees")
ret$med <- circular(ret$med,template="geographics",units="degrees")
colnames(ret) <- c("day","mean","median","upper95mean","lower95mean")
return(ret)
}


#m <- M$mean
#m[is.na(m)] <- 0
#m[m < 0] <- m[m < 0]+180
#up <- M$upper95mean
#low <- M$lower95mean
#low[is.na(low)] <- 0
#up[is.na(up)] <- 0
#up[up < m]  <- up[up < m]+180
#low[low > m]  <- low[low > m]-180
#low[is.na(M$lower95mean)] <- NA
#up[is.na(M$upper95mean)] <- NA
#m[is.na(M$mean)] <- NA
#plot(M$day,as.numeric(m),type="l",ylim=c(min(low,na.rm=T),max(up,na.rm=T)))
##arrows(x0=M$day,y0=up,x1=M$day,y1=low,code=3,angle=90)
#lines(M$day,up,col="red")
#lines(M$day,low,col="red")
#lines(M$day,as.numeric(m)-180)
#lines(M$day,as.numeric(m)+180)


#plot(M$day,as.numeric(M$mean),type="p",ylim=c(-90,90))
#points(M$day,M$lower95mean,col="blue")
#points(M$day,M$upper95mean,col="red")

