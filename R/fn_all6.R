#This is a work in progress. Will need to use an external script to stitch these together
#' @export
all6 <- function(path,name,trip,low,high,zerophase=FALSE) {
setwd(path)
		if (dir.exists("all6plots")){}else{dir.create("all6plots")}
	print("Plotting all6")

	gmtpresets <- read.table(paste0(name,".gmt"))
	tlagscale <- gmtpresets$V2[which(gmtpresets$V1 == "tlag_scale")]
	s <- gmtpresets$V2[which(gmtpresets$V1 == "spick")]
	wbeg <- gmtpresets$V2[which(gmtpresets$V1 == "wbeg")]
	wend <- gmtpresets$V2[which(gmtpresets$V1 == "wend")]
	fname <- paste0(name,".clustxy")
	r <- read.table(fname)
	bwintab <- read.table(paste0(name,".soln"))
	bwin <- bwintab$V1
	win <- r$V1
	phi <- r$V4
	Dphi <- r$V5
	dt <- r$V6
	Ddt <- r$V7
	philab <- "Phi (degrees)"
	dtlab <- "dt (s)"


	fname <- paste0("all6plots/all6_",name,".eps")
	postscript(file=fname, onefile=FALSE, horizontal=FALSE,width=8.27,height=11.69,paper='special')

par(mar=c(1,1,1,1))

layout(matrix(c(13,13,13,19,9,9,9,14,14,14,19,10,10,10,15,15,15,19,11,11,11,16,16,16,19,12,12,12,20,1,1,19,3,3,17,20,2,2,19,3,3,17,20,7,8,19,4,4,17,20,5,6,19,4,4,17,20,18,18,19,18,18,17), 9, 7, byrow = TRUE))

#	13 13 9 9
#	14 14 10 10	
#	15 15 11 11
#	16 16 12 12
#	1  1   3 3
#	2  2   3 3
#	7  8   4 4
#       5  6   4 4
	

#plot clusters
		#1
		plot(win,phi,ylim=c(-90,90),pch=19,xlab="Window number",ylab=philab, yaxp  = c(-90, 90, 6))
		arrows(win, phi-Dphi, win, phi+Dphi, length=0.05, angle=90, code=3)
		points(bwin,phi[bwin],pch=4,col="blue",cex=5,lwd=3)

		#2
		plot(win,dt,ylim=c(0,tlagscale),pch=19,xlab="Window number",ylab=dtlab, yaxp  = c(0, tlagscale, tlagscale/0.1))
		arrows(win, dt-Ddt, win, dt+Ddt, length=0.05, angle=90, code=3)
		points(bwin,dt[bwin],pch=4,col="blue",cex=5,lwd=3)
		dev <- 	(tlagscale/2)/9
	
		#3
		plot(dt,phi,ylim=c(-90,90),xlim=c(0,tlagscale),xlab=dtlab,ylab=philab, yaxp  = c(-90, 90, 6),xaxp  = c(0, tlagscale, tlagscale/0.1),pch=19,cex=0.5)
		points(dt[bwin],phi[bwin],pch=4,col="blue",cex=5,lwd=3)
		#points(r$V2,r$V3,pch=2)	

#plot error surface
	image.maker <- function(coords, value){  #handy function to get points into correct format. I could make this faster with lapply?
 		 N1 <- length(unique(coords[,1]))
		N2 <- length(unique(coords[,2])) 
 		 image.out <- matrix(NA, nrow = N1, ncol = N2) 
  		#coords[,1] <- as.numeric(factor(coords[,1])) 
		coords[,1] <- as.numeric((factor(coords[,1]*1000000)))
 		 coords[,2] <- as.numeric((factor(coords[,2])))
 		 for (i in 1:nrow(coords)) {
    		image.out[coords[i,1], coords[i,2]] <- value[i] }
 		 return(image.out) 
		} 
 	er <- read.table(paste0(name,".error"))
	cont <- image.maker(er[c("V1","V2")],er$V3)

		#4
		contour(unique(er$V1),unique(er$V2),cont,nlevels=2,xlab=dtlab,ylab=philab, yaxp  = c(-90, 90, 6),xaxp  = c(0, tlagscale, tlagscale/0.1))
		contour(unique(er$V1),unique(er$V2),cont,levels=1,lwd=3,add=TRUE)
		points(dt[bwin],phi[bwin],pch=4,col="blue",cex=5,lwd=3)

#plot particle motion
	pm <- read.table(paste0(name,".pm"))
	pmc <- read.table(paste0(name,".pmc"))
		
		#5
		plot(pm,type="l",xlab="Slow Rel. Amp.",ylab="Fast Rel. Amp",yaxp  = c(-1, 1, 4),xaxp  = c(-1, 1, 4),xlim=c(-1,1),ylim=c(-1,1))

		
		#6
		plot(pmc,type="l",xlab="Slow Rel. Amp.",ylab="Fast Rel. Amp",yaxp  = c(-1, 1, 4),xaxp  = c(-1, 1, 4),xlim=c(-1,1),ylim=c(-1,1))

#corrected S and S
	sf <- read.table(paste0(name,".sf"))
	ss <-  read.table(paste0(name,".ss"))
	ssc <- read.table(paste0(name,".ssc"))

		#7
		plot(sf$V1,sf$V2,type="l",xlab="Seconds",ylab="",ylim=c(-1,1))
		lines(ss$V1,ss$V2,type="l",col="blue",lty=5)
		abline(v=s)
		text(s-0.05,1,"S")


		#8
		plot(sf$V1,sf$V2,type="l",xlab="Seconds",ylab="",ylim=c(-1,1))
		lines(ss$V1,ssc$V2,type="l",col="blue",lty=5)
		abline(v=s)
		text(s-0.05,1,"S")
	
#Plot top right
	src <- read.table(paste0(name,".src")) #parallel corrected
	sro <- read.table(paste0(name,".sro")) #parellel original
	stc <- read.table(paste0(name,".stc")) #perpedicular corrected
	sto <- read.table(paste0(name,".sto")) #perpendicular original

		#9
		plot(sro,type="l",xlim=c(s-2.3,s+2.3),xlab="Seconds",ylab="")
		p <- par('usr')
		#rect(wbeg,p[3]+30,wend,p[4]-30,density=50,col="lightgrey")
		#lines(sro,type="l")
		abline(v=s,col="red")
		#abline(v=wbeg,lty=5)
		#abline(v=wend,lty=5)
		text(s-0.1,max(sro$V2*3/4),"S")

		#10
		plot(src,type="l",xlim=c(s-2.3,s+2.3),xlab="Seconds",ylab="")
		p <- par('usr')
		#rect(wbeg,p[3]+30,wend,p[4]-30,density=50,col="lightgrey")
		#lines(sro,type="l")
		abline(v=s,col="red")
		#abline(v=wbeg,lty=5)
		#abline(v=wend,lty=5)
		text(s-0.1,max(sro$V2*3/4),"S")


		#11
		plot(stc,type="l",xlim=c(s-2.3,s+2.3),xlab="Seconds",ylab="")
		p <- par('usr')
		#rect(wbeg,p[3]+30,wend,p[4]-30,density=50,col="lightgrey")
		#lines(sro,type="l")
		abline(v=s,col="red")
		#abline(v=wbeg,lty=5)
		#abline(v=wend,lty=5)
		text(s-0.1,max(sro$V2*3/4),"S")


		#12
		plot(sto,type="l",xlim=c(s-2.3,s+2.3),xlab="Seconds",ylab="")
		p <- par('usr')
		#rect(wbeg,p[3]+30,wend,p[4]-30,density=50,col="lightgrey")
		#lines(sro,type="l")
		abline(v=s,col="red")
		#abline(v=wbeg,lty=5)
		#abline(v=wend,lty=5)
		text(s-0.1,max(sro$V2*3/4),"S")

#original wave forms
	for (j in 1:3){
		trip[[j]]$amp <- butfilt(trip[[j]]$amp, fl=low, fh=high, deltat=trip[[j]]$dt, type="BP" , proto="BU",npoles=2,zp=zerophase)
		trip[[j]]$amp <- detrend(trip[[j]]$amp)
		trip[[j]]$amp <- trip[[j]]$amp-mean(trip[[j]]$amp)
	}
	tvec <- seq(0,(length(trip[[1]]$amp)-1)*trip[[1]]$dt,trip[[1]]$dt) #this might be important for cutting scripts?

		#13
		plot(tvec,trip[[1]]$amp,type="l",xlim=c(s-4,s+4),xlab="Seconds",ylab="")
		abline(v=s)
		text(s-0.1,max(trip[[1]]$amp*3/4),"S")
		text(s+3,max(trip[[1]]$amp*3/4),trip[[1]]$comp)

		#14
		plot(tvec,trip[[2]]$amp,type="l",xlim=c(s-4,s+4),xlab="Seconds",ylab="")
		abline(v=s)
		text(s-0.1,max(trip[[2]]$amp*3/4),"S")
		text(s+3,max(trip[[2]]$amp*3/4),trip[[2]]$comp)

		#15
		plot(tvec,trip[[3]]$amp,type="l",xlim=c(s-4,s+4),xlab="Seconds",ylab="")
		abline(v=s)
		text(s-0.1,max(trip[[3]]$amp*3/4),"S")
		text(s+3,max(trip[[3]]$amp*3/4),trip[[3]]$comp)

	dev.off()

}