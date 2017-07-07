#' @title Classify events
#' @description Determines which type an event can be classified as
#' @param summ A summary file (usually from a single station)
#' @param cutoff Maximum propgation angle allowed for a Type 1 event
#' @param cutoff2 Maximum propgation angle allowed for a Type 2 event
#' @param minper Percentage of the path that must be below the maximum to be classified into that type
#' @param depthcutoff What percentage of the straight line path must be below cutoff2 to be classified as Type 2
#' @param sww The shear wave window cutoff
#' @param tvel The velocity model to be used (this must be the same as the one used to determine the ray parameters)
#' @param plot Should the result be plotted?
#' @details This function takes the events in a summary file and classifies each of them (based on their propogation angles) as Type 1 or Type 3 (full delay time and correct polarisation), Type 2 (correct polarisation) or Type NULL (low delay time and incorrect polarisation)
#' @return A dataframe containing the summary file with the type for each event
#' @export
det.type <- function(summ,cutoff=15,cutoff2=35,minper=0.9,depthcutoff=0.17,sww=45,tvel=ak135_taupo,plot=TRUE){
	type <- rep("NULL",length(summ$fast))
	summ <- cbind(summ,type)
	summ$type <- as.character(summ$type)
	summ <- summ[!is.na(summ$rayp),]
	summ <- subset(summ, anginc < sww)
	summ$type <- type
	x <- tvel$z
	y <- tvel$vs
	wh <- min(which(y == 0))
	y <- subset(y, x < x[wh])
	x <- subset(x, x < x[wh])

	for (j in 1:length(summ$fast)){
		line <- summ[j,]
		rayp <- line$rayp
		depth <- line$depthkm
		p <- rayp*360 /2 /3.1415927
		R <- tvel$rp
		upper <- sqrt(0.9999)*R/p
		lower <- y[1]
		f <- function(x) (atan2(p*x/(R*sqrt(1-(p*x/R)^2)),1) /2 /3.1415927 * 360)-cutoff
		root <- try(uniroot(f,c(lower,upper)))
		if(class(root) == "try-error"){
			summ[j,]$type <- "NULL"
		}else{
			vsf <- root$root
			if(vsf %in% y){
				df <- x[min(which(y == vsf))]
			}else{
 				if(vsf > max(y)){
					df <- 999
				}else{
					up <- min(which(y == y[ y >= vsf ][1]))
					low <- max(which(y == y[ y <= vsf ][length(y[ y <= vsf ])]))
					vel <- approx(x = c(y[low],y[up]), y = c(x[low],x[up]), xout =vsf)
					df <- vel$y
				}
			}
			topdist <- line$distevstat
			ang <- atan(depth/topdist)
			ndist <- df/sin(ang)
			fdist <- sqrt(topdist^2+depth^2)
			per <- ndist/fdist
			if(per > minper){
				summ[j,]$type <- "T1"
			}else{
				#if(per > depthcutoff){
					#summ[j,]$type <- "T2"
				#}
			}
		}
	}


	for (j in 1:length(summ$fast)){
		line <- summ[j,]
		rayp <- line$rayp
		depth <- line$depthkm
		p <- rayp*360 /2 /3.1415927
		R <- tvel$rp
		upper <- sqrt(0.9999)*R/p
		lower <- y[1]
		if(line$type == "NULL"){
		f <- function(x) (atan2(p*x/(R*sqrt(1-(p*x/R)^2)),1) /2 /3.1415927 * 360)-cutoff2
		root <- try(uniroot(f,c(lower,upper)))
		if(class(root) == "try-error"){
			summ[j,]$type <- "NULL"
		}else{
			vsf <- root$root
			if(vsf %in% y){
				df <- x[min(which(y == vsf))]
			}else{
				if(vsf > max(y)){
					df <- 999
				}else{
					up <- min(which(y == y[ y >= vsf ][1]))
					low <- max(which(y == y[ y <= vsf ][length(y[ y <= vsf ])]))
					vel <- approx(x = c(y[low],y[up]), y = c(x[low],x[up]), xout =vsf)
					df <- vel$y
				}
			}
			topdist <- line$distevstat
			ang <- atan(depth/topdist)
			ndist <- df/sin(ang)
			fdist <- sqrt(topdist^2+depth^2)
			per <- ndist/fdist
			if(per > minper){
				summ[j,]$type <- "T2"
			}else{
				if(per > depthcutoff){
					summ[j,]$type <- "T2"
				}
			}
		}
	}
	}


	hcut <- cos(rad(90-cutoff))
	dirsub <- subset(summ, type == "T1")
	dir2sub <- subset(summ, type == "T2")
	dsub <- rbind(dirsub,dir2sub)
	source("~/paper/R/weightedmean.R")
	source("~/paper/R/dt_mean.R")
	if(length(dsub$fast) < 2){return(NULL)}
	m <- fast.weighted(dsub)
	if(m$pval < 0.05){
		for (j in 1:length(summ$fast)){
			line <- summ[j,]
			if(line$type == "T2" | line$type == "NULL"){
				baz <- line$baz-as.numeric(m$mean)
				rayp <- line$rayp
				p <- rayp*360 /2 /3.1415927
				R <- tvel$rp
				depth <- line$depthkm
				if(depth %in% x){vs <- y[min(which(x == depth))]}else{
					up <- min(which(x == x[ x >= depth ][1]))
					low <-  max(which(x == x[ x <= depth ][length(x[ x <= depth ])]))
					vel <- approx(x = c(x[low],x[up]), y = c(y[low],y[up]), xout =depth)
					vs <- vel$y
				}
				takeoff <- atan2(p*vs/(R*sqrt(1-(p*vs/R)^2)),1) /2 /3.1415927 * 360
				h <- cos(rad(90-takeoff))
				xb <- h*sin(rad(baz))
				if(abs(xb) <= hcut){summ[j,]$type <- "T3"}

			}
		}
	}
	
	if(m$pval < 0.05){
		hcut <- cos(rad(90-cutoff))
		for (j in 1:length(summ$fast)){
			line <- summ[j,]
			if(line$type == "T2" | line$type == "NULL"){
				rayp <- line$rayp
				p <- rayp*360 /2 /3.1415927
				R <- tvel$rp
				baz <- line$baz-as.numeric(m$mean)
				upper <- sqrt(0.9999)*R/p
				lower <- y[1]
				f <- function(x) cos(rad(90-(atan2(p*x/(R*sqrt(1-(p*x/R)^2)),1) /2 /3.1415927 * 360)))*sin(rad(baz))-hcut
				root <- try(uniroot(f,c(lower,upper)))
					if(class(root) == "try-error"){
					}else{
						vsf <- root$root
						if(vsf %in% y){
							df <- x[min(which(y == vsf))]
						}else{
							if(vsf > max(y)){
							}else{
								up <- min(which(y == y[ y >= vsf ][1]))
								low <- max(which(y == y[ y <= vsf ][length(y[ y <= vsf ])]))
								vel <- approx(x = c(y[low],y[up]), y = c(x[low],x[up]), xout =vsf)
								df <- vel$y
							}
						}
						topdist <- line$distevstat
						ang <- atan(depth/topdist)
						ndist <- df/sin(ang)
						fdist <- sqrt(topdist^2+depth^2)
						per <- ndist/fdist
						if(per > minper){summ[j,]$type <- "T2"}else{if(per > depthcutoff){summ[j,]$type <- "T2"}}
					}
					f <- function(x) cos(rad(90-(atan2(p*x/(R*sqrt(1-(p*x/R)^2)),1) /2 /3.1415927 * 360)))*sin(rad(baz))+hcut
					root <- try(uniroot(f,c(lower,upper)))
					if(class(root) == "try-error"){
					}else{
						vsf <- root$root
						if(vsf %in% y){
							df <- x[min(which(y == vsf))]
						}else{
							if(vsf > max(y)){
							}else{
								up <- min(which(y == y[ y >= vsf ][1]))
								low <- max(which(y == y[ y <= vsf ][length(y[ y <= vsf ])]))
								vel <- approx(x = c(y[low],y[up]), y = c(x[low],x[up]), xout =vsf)
								df <- vel$y
							}
						}
						topdist <- line$distevstat
						ang <- atan(depth/topdist)
						ndist <- df/sin(ang)
						fdist <- sqrt(topdist^2+depth^2)
						per <- ndist/fdist
						if(per > minper){summ[j,]$type <- "T2"}else{if(per > depthcutoff){summ[j,]$type <- "T2"}}
					}
				}
			}
		}

		if(m$pval >= 0.05){
			gr <-  as.numeric(dsub$finalgrade)
			vals <- rep(as.numeric(rad(dsub$fast*2)),gr)
			k <- kuiper.test(vals,alpha=0.05)
			cap <- capture.output(k)
			if(cap[6] == "Do Not Reject Null Hypothesis "){
				bazs <- rep(as.numeric(rad(dsub$baz*2)),gr)
				cc <- cor.circular(bazs,vals,test=T)
				if(cc$p.value >= 0.05){	print(paste0("Discarding ",as.character(dsub$stat[1])));return(NULL)}

			}
		}
	
	n <- subset(summ, type == "NULL")
	t1 <- subset(summ, type == "T1")
	t2 <- subset(summ, type == "T2")
	t3 <- subset(summ, type == "T3")

#plottem
p <- summ$rayp*360 /2 /3.1415927
R <- tvel$rp
##Start vs 
for(k in 1:length(p)){
depth <- summ$depthkm[k]
if(depth %in% x){vs <- y[min(which(x == depth))]}else{
	up <- min(which(x == x[ x >= depth ][1]))
	low <-  max(which(x == x[ x <= depth ][length(x[ x <= depth ])]))
	vel <- approx(x = c(x[low],x[up]), y = c(y[low],y[up]), xout =depth)
	vs <- vel$y
	}
	if(k == 1){vst <- vs}else{vst <- rbind(vst,vs)}
}

vst <- as.numeric(vst)
takeoff <- atan2(p*vst/(R*sqrt(1-(p*vst/R)^2)),1) /2 /3.1415927 * 360
h <- cos(rad(90-takeoff))
xvec <- h*sin(rad(summ$baz))
yvec <- h*cos(rad(summ$baz))

t1x <- subset(xvec, summ$type == "T1")
t1y <- subset(yvec, summ$type == "T1")

t2x <- subset(xvec, summ$type == "T2")
t2y <- subset(yvec, summ$type == "T2")

t3x <- subset(xvec, summ$type == "T3")
t3y <- subset(yvec, summ$type == "T3")

tnx <- subset(xvec, summ$type == "NULL")
tny <- subset(yvec, summ$type == "NULL")

if (plot){
	plot(t1x,t1y,ylim=c(-1,1),xlim=c(-1,1),col="blue")
	points(t3x,t3y,ylim=c(-1,1),xlim=c(-1,1),col="green")
	points(t2x,t2y,ylim=c(-1,1),xlim=c(-1,1),col="orange")
	points(tnx,tny,ylim=c(-1,1),xlim=c(-1,1),col="red")
}
#tg <- rbind(t1,t3)

#ndt <- weighted.dt(n)
#nfast <- mean.weighted(n)
#tgdt <- weighted.dt(tg)
#tgfast <- mean.weighted(tg)
#t2dt <- weighted.dt(t2)
#t2fast <- mean.weighted(t2)

return(summ)


}
