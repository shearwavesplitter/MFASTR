#' @title Fast polarisation moving average
#' @description A moving average of fast polarisation
#' @param summfile A dataframe containing a summary file (i.e. from readmfast)
#' @param windowlength Size of the averaging window (in days)
#' @param windowspeed Speed of advancing window (days per sample)
#' @param reps Number of interations for the bootstrap
#' @return A dataframe containing the end days of each window along with its mean, median fast polarisation, and 95% confidence intervals of the mean (from a bootstrapped mle von Mises fit)
#' @export
moving_phi <- function(summfile,windowlength,windowspeed,reps=9999){
	windowlength=windowlength-1 #Actual window length appears to be windowlength+1 so fixing that here
	yr <- summfile$year-min(summfile$year)
	day <- summfile$doy_det+yr*365

	maxday <- max(day)
	minday <- min(day)

    corrphi <- function(actual,upper,lower,ch=2*pi){
        act <- actual
        difu <- upper-actual
        difl <- actual-lower
        it <- 0
        while (difu < 0 | difl < 0){
            if(difu < 0 ){actual <- actual-ch}
            if(difl < 0){actual <- actual+ch}
            difu <- upper-actual
            difl <- actual-lower

            it <- it+1
            if(it >10){print("Median does not fall into interval; returning original value");return(act)}

        }


        return(actual)
    }

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
			#m[i] <- as.numeric(deg(mean.circular(circular(rad(sub$fast)*2))))
			medreal <- as.numeric(median.circular(circular(rad(sub$fast)*2)))
            biastf <- FALSE
            if(length(sub$fast) < 16){biastf <- TRUE}
            if(length(sub$fast) == 1){biastf <- FALSE}
			rs <- mle.vonmises.bootstrap.ci(circular(rad(sub$fast*2)),bias=biastf,reps=reps)
            mclose <- (as.numeric(rs$mu.ci[2])-as.numeric(rs$mu.ci[1]))/2+as.numeric(rs$mu.ci[1])
            mreal <- as.numeric(mean.circular(circular(rad(sub$fast)*2)))
			lwr[i] <- as.numeric(rs$mu.ci[1])#/2
			hir[i] <- as.numeric(rs$mu.ci[2])#/2
            m[i] <- corrphi(mreal,hir[i],lwr[i],ch=2*pi)
            #print("####")
            #print(hir[i])
            #print(m[i])
            #print(lwr[i])
			med[i] <- corrphi(medreal,hir[i],lwr[i],ch=2*pi)

		}
	}


m <- as.numeric(deg(m))/2
med <- as.numeric(deg(med))/2
lwr <- as.numeric(deg(lwr))/2
hir <- as.numeric(deg(hir))/2

	for(i in 1:length(m)){
		if(!is.na(m[i])){if(m[i] > 90){
            m[i] <- m[i]-180
            lwr[i] <- lwr[i]-180
            hir[i] <- hir[i] -180
            med[i] <- med[i]-180
        }}
		if(!is.na(m[i])){if(m[i] < -90){
            m[i] <- m[i]+180
            lwr[i] <- lwr[i]+180
            hir[i] <- hir[i]+180
            med[i] <- med[i]+180
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

