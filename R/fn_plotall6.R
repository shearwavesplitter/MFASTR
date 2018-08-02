#' @title Create all6 plot
#' @description Create an all6 plot for a specific event
#' @param path Path to folder containing raw events and output folder
#' @param filter Which filter to plot (e.g. 1 for fb1)
#' @param zerophase Were the filters applied zero phase?
#' @param E Vector signal of the east component
#' @param N Vector signal of the north component
#' @param Z Vector signal of the vertical component
#' @param auto Select the first event if multiple are available?
#' @param biglong logical, TRUE=long=8 bytes (original sac files written on 32bit machine)
#' @param Iendian Endian-ness of the original data: 1,2,3: "little", "big", "swap". Default = 1 (little)
#' @export
all6plot <- function(path,cuspid,filter=1,zerophase=TRUE,E=".e",N=".n",Z=".z",auto=FALSE,biglong=TRUE,Iendian=1) {
setwd(path)
		if (dir.exists("all6plots")){}else{dir.create("all6plots")}
	namelist <- list.files("output",glob2rx(pattern=paste0(cuspid,"*.fb",filter,E,"$")))
	namelist3 <- gsub(paste0("\\",E,"$"),"",namelist)
	wh <- 1
	wh2 <- wh
	if(length(wh) > 1){
		if(auto){wh2 <- wh[1];warning("Events are repeated with different filter parameters")}else{
			print("Event is repeated for that filter")
			print("Select which event to plot (Enter 1-n)")
			ptab <- as.data.frame(namelist[wh])
			colnames(ptab) <- "Events"
			print(ptab)
			nsel <- readline(prompt="Enter an numer of event to plot: ")
			wh2 <- wh[as.numeric(nsel)]
			 if (is.na(wh2)){stop("Invalid input")}
    				}	
  			}			
	
	name <- paste0("output/",namelist3[wh2])
	log <- capture.output({
  		trip <- readtriplet(cuspid,biglong=biglong,Iendian=Iendian)
	})

	print(paste0("Plotting all6 for event ",namelist3[wh2]))
	### Determine low and high filters
		suff <- gsub(paste0("\\.fb",filter,E,"$"),"",namelist[wh2])
		pref <- sub(paste0(cuspid,"."),"",suff)
		split <- strsplit(pref,"-")
		low <- split[[1]][1]
		high <- split[[1]][2]

#num.plots <- 7
#my.plots <- vector(num.plots, mode='list')

	gmtpresets <- read.table(paste0(name,".gmt"))
	tlagscale <- gmtpresets$V2[which(gmtpresets$V1 == "tlag_scale")]
	s <- gmtpresets$V2[which(gmtpresets$V1 == "spick")]
	wbeg <- gmtpresets$V2[which(gmtpresets$V1 == "wbeg")]
	wend <- gmtpresets$V2[which(gmtpresets$V1 == "wend")]
	fname <- paste0(name,".clustxy")
	clusname <- paste0(name,".clustnew")
	r <- read.table(fname)
	clusnew <- read.table(clusname)
	bwintab <- read.table(paste0(name,".soln"))
	bwin <- bwintab$V1
	win <- r$V1
	phi <- r$V4
	Dphi <- r$V5
	dt <- r$V6
	Ddt <- r$V7
	philab <- "Phi (°)"
	dtlab <- "dt (s)"


	#fname <- paste0("all6plots/all6_",name,".eps")
	#postscript(file=fname, onefile=FALSE, horizontal=FALSE,width=8.27,height=11.69,paper='special')

		############ Gather event details
			ini <- read.table(paste0(dir(pattern="ini_files"),"/",namelist3[wh2],".ini"))
			ilog <- read.table(paste0("output/",namelist3[wh2],".ilognew.ass"),header=T)

			trip[[1]]$HEAD$values <- as.character(trip[[1]]$HEAD$values)
			depth <-round(as.numeric(trip[[1]]$HEAD$values[[which(trip[[1]]$HEAD$names == "evdp")]]),1)
			dist <-round(as.numeric(trip[[1]]$HEAD$values[[which(trip[[1]]$HEAD$names == "dist")]]),4)
			mag <- "-" #Currently I do not record magnitude
			wbeg2 <- -round(as.numeric(as.character(ini$V1[9])),2) ########start window closest to the S-pick
			wbeg1 <- round((as.numeric(as.character(ini$V1[9]))+(as.numeric(as.character(ini$V1[3]))-1)*as.numeric(as.character(ini$V1[5])))*-1,2)########start window furtherest from the S-pick
			wbegn <- paste0("(",as.character(ini$V1[3]),")")
			
			wend1 <- round(as.numeric(as.character(ini$V1[10])),2)########end window closest to the S-pick
			wend2 <-  round((as.numeric(as.character(ini$V1[10]))+(as.numeric(as.character(ini$V1[4]))-1)*as.numeric(as.character(ini$V1[6]))),2)########end window furtherest from the S-pick
			wendn <- paste0("(",as.character(ini$V1[4]),")")
			sel1 <- wbeg
			sel2 <- wend
	
	savepath <- paste0("all6plots/",namelist3[wh2],".pdf")
	pdf(savepath, onefile=TRUE)

#original wave forms
	for (j in 1:3){
		trip[[j]]$amp <- butfilt(trip[[j]]$amp, fl=low, fh=high, deltat=trip[[j]]$dt, type="BP" , proto="BU",npoles=2,zp=zerophase)
		trip[[j]]$amp <- detrend(trip[[j]]$amp)
		trip[[j]]$amp <- trip[[j]]$amp-mean(trip[[j]]$amp)
	}
	tvec <- seq(0,(length(trip[[1]]$amp)-1)*trip[[1]]$dt,trip[[1]]$dt) #this might be important for cutting scripts?


		#dev.new()
		par(mfrow = c(3,1),
          oma = c(5,4,0,0) + 0.1,
          mar = c(0,0,1,1) + 0.1)
	
		#13
		plot(tvec,trip[[1]]$amp,type="l",xlim=c(s-4,s+4),xlab="Seconds",ylab="",xaxt='n',col="white")
		p <- par('usr')
		rect(wbeg,p[3]+30,wend,p[4]-30,density=50,col="lightgrey")
		lines(tvec,trip[[1]]$amp,type="l")
		#lines(sro,type="l")
		abline(v=s,col="red")
		abline(v=s+wend2,lty=5)
		abline(v=s+wbeg1,lty=5)

		#abline(v=s)
		text(s-0.1,max(trip[[1]]$amp*3/4),"S")
		text(s+3,max(trip[[1]]$amp*3/4),trip[[1]]$comp)
	
		#14
		plot(tvec,trip[[2]]$amp,type="l",xlim=c(s-4,s+4),xlab="Seconds",ylab="",xaxt='n',col="white")
		p <- par('usr')
		rect(wbeg,p[3]+30,wend,p[4]-30,density=50,col="lightgrey")
		lines(tvec,trip[[1]]$amp,type="l")
		#lines(sro,type="l")
		abline(v=s,col="red")
		abline(v=s+wend2,lty=5)
		abline(v=s+wbeg1,lty=5)
		abline(v=s,col="red")
		text(s-0.1,max(trip[[2]]$amp*3/4),"S")
		text(s+3,max(trip[[2]]$amp*3/4),trip[[2]]$comp)

		#15
		plot(tvec,trip[[3]]$amp,type="l",xlim=c(s-4,s+4),xlab="Seconds",ylab="")
		abline(v=s,col="red")
		text(s-0.1,max(trip[[3]]$amp*3/4),"S")
		text(s+3,max(trip[[3]]$amp*3/4),trip[[3]]$comp)

	title(xlab = "Seconds",
     		 outer = TRUE, line = 3)

		pl <- 7
		#my.plots[[pl]] <- recordPlot()

#Plot top right
	src <- read.table(paste0(name,".src")) #parallel corrected
	sro <- read.table(paste0(name,".sro")) #parellel original
	stc <- read.table(paste0(name,".stc")) #perpedicular corrected
	sto <- read.table(paste0(name,".sto")) #perpendicular original

		#dev.new()
		par(mfrow = c(4,1),
          oma = c(5,4,0,0) + 0.1,
          mar = c(0,0,1,1) + 0.1)
	

		#9
		plot(sro,type="l",xlim=c(s-2.3,s+2.3),xlab="Seconds",ylab="",xaxt='n')
		p <- par('usr')
		rect(wbeg,p[3]+30,wend,p[4]-30,density=50,col="lightgrey")
		lines(sro,type="l")
		#lines(sro,type="l")
		abline(v=s,col="red")
		abline(v=s+wend1,lty=5)
		abline(v=s+wend2,lty=5)
		abline(v=s+wbeg1,lty=5)
		abline(v=s+wbeg2,lty=5)

		text(s+1,max(sro$V2*9/10),"p")
		text(s-0.1,max(sro$V2*3/4),"S")

		#10
		plot(src,type="l",xlim=c(s-2.3,s+2.3),xlab="Seconds",ylab="",xaxt='n',col="white")
		p <- par('usr')
		rect(wbeg,p[3]+30,wend,p[4]-30,density=50,col="lightgrey")
		lines(src,type="l")
		#lines(sro,type="l")
		abline(v=s,col="red")
		abline(v=s+wend1,lty=5)
		abline(v=s+wend2,lty=5)
		abline(v=s+wbeg1,lty=5)
		abline(v=s+wbeg2,lty=5)
		text(s+1,max(sro$V2*9/10),"p-perp")
		text(s-0.1,max(sro$V2*3/4),"S")


		#11
		plot(stc,type="l",xlim=c(s-2.3,s+2.3),xlab="Seconds",ylab="",xaxt='n')
		p <- par('usr')
		#rect(wbeg,p[3]+30,wend,p[4]-30,density=50,col="lightgrey")
		#lines(sro,type="l")
		abline(v=s,col="red")
		#abline(v=wbeg,lty=5)
		#abline(v=wend,lty=5)
		text(s+1,max(sro$V2*9/10),"corrected p")
		text(s-0.1,max(sro$V2*3/4),"S")


		#12
		plot(sto,type="l",xlim=c(s-2.3,s+2.3),xlab="Seconds",ylab="")
		p <- par('usr')
		#rect(wbeg,p[3]+30,wend,p[4]-30,density=50,col="lightgrey")
		#lines(sro,type="l")
		abline(v=s,col="red")
		#abline(v=wbeg,lty=5)
		#abline(v=wend,lty=5)
		text(s+1,max(sro$V2*8/10),"corrected p-perp")
		text(s-0.1,max(sro$V2*3/4),"S")

	title(xlab = "Seconds",
     		 outer = TRUE, line = 3)

		pl <- 6
		#my.plots[[pl]] <- recordPlot()


#plot clusters
		par(mfrow = c(2,1),
          oma = c(5,4,0,0) + 0.1,
          mar = c(0,0,1,1) + 0.1)
		#1
		plot(win,phi,ylim=c(-90,90),pch=19,xlab="",ylab=philab, yaxp  = c(-90, 90, 6),xaxt='n')
		arrows(win, phi-Dphi, win, phi+Dphi, length=0.05, angle=90, code=3)
		points(bwin,phi[bwin],pch=4,col="blue",cex=5,lwd=3)

		#2
		plot(win,dt,ylim=c(0,tlagscale),pch=19,xlab="Window number",ylab=dtlab, yaxp  = c(0, tlagscale, tlagscale/0.1))
		arrows(win, dt-Ddt, win, dt+Ddt, length=0.05, angle=90, code=3)
		points(bwin,dt[bwin],pch=4,col="blue",cex=5,lwd=3)
		dev <- 	(tlagscale/2)/9

		title(xlab = "Window Number",
			ylab=paste(dtlab,"                                                                  ",philab),
     		 outer = TRUE, line = 3)

		pl <- 1
		#my.plots[[pl]] <- recordPlot()


	#dev.new()
		#3

	par(mfrow = c(1,1),
          oma = c(5,4,0,0) + 0.1,
          mar = c(0,0,1,1) + 0.1)

		plot(dt,phi,ylim=c(-90,90),xlim=c(0,tlagscale),xlab=dtlab,ylab=philab, yaxp  = c(-90, 90, 6),xaxp  = c(0, tlagscale, tlagscale/0.1),pch=19,cex=0.5,xaxs="i",yaxs="i")
		points(clusnew$V1,clusnew$V2,pch=2,col="red")	
		points(dt[bwin],phi[bwin],pch=4,col="blue",cex=5,lwd=3)

	pl <- 2
		#my.plots[[pl]] <- recordPlot()

#plot error surface
	#dev.new()
	image.maker <- function(coords, value){  #handy function to get points into correct format. I could make this faster with lapply?
 		 N1 <- length(unique(coords[,1]))
		N2 <- length(unique(coords[,2])) 
 		 image.out <- matrix(NA, nrow = N1, ncol = N2) 
  		#coords[,1] <- as.numeric(factor(coords[,1])) 
		coords[,1] <- as.numeric((factor(coords[,1]*1000000)))
 		 coords[,2] <- as.numeric((factor(coords[,2])))

		
		image.out <- matrix(data=value, nrow=N1, ncol=N2)


 		 #for (i in 1:nrow(coords)) {
    		#image.out[coords[i,1], coords[i,2]] <- value[i] }


 			 return(image.out) 
		} 

 	er <- read.table(paste0(name,".error"))
	cont <- image.maker(er[c("V1","V2")],er$V3)

		#4
		contour(unique(er$V1),unique(er$V2),cont,nlevels=2,xlab=dtlab,ylab=philab, yaxp  = c(-90, 90, 6),xaxp  = c(0, tlagscale, tlagscale/0.1),xaxs="i",yaxs="i")
		contour(unique(er$V1),unique(er$V2),cont,levels=1,lwd=3,add=TRUE)
		points(dt[bwin],phi[bwin],pch=4,col="blue",cex=5,lwd=3)

		pl <- 3
		#my.plots[[pl]] <- recordPlot()

#corrected S and S
		#dev.new()
		par(mfrow = c(2,1),
          oma = c(5,4,0,0) + 0.1,
          mar = c(0,0,1,1) + 0.1)
	sf <- read.table(paste0(name,".sf"))
	ss <-  read.table(paste0(name,".ss"))
	ssc <- read.table(paste0(name,".ssc"))

		#7
		plot(sf$V1,sf$V2,type="l",xlab="Seconds",ylab="",ylim=c(-1,1),xaxt='n',col="white",xaxs="i")
		p <- par('usr')
		rect(wbeg,p[3],wend,p[4],density=50,col="lightgrey")
		lines(sf$V1,sf$V2,type="l")
		lines(ss$V1,ss$V2,type="l",col="blue",lty=5)
		abline(v=s)
		text(s-0.05,1,"S")


		#8
		plot(sf$V1,sf$V2,type="l",xlab="Seconds",ylab="",ylim=c(-1,1),col="white",xaxs="i")
		p <- par('usr')
		rect(wbeg,p[3],wend,p[4],density=50,col="lightgrey")
		lines(sf$V1,sf$V2,type="l")
		lines(ss$V1,ssc$V2,type="l",col="blue",lty=5)
		abline(v=s)
		text(s-0.05,1,"S")

	title(xlab = "Seconds",
     		 outer = TRUE, line = 3)

			pl <- 5
		#my.plots[[pl]] <- recordPlot()
	

#plot particle motion



	
 

	par(mfrow = c(2,2),
          oma = c(5,4,0,0) + 0.1,
          mar = c(0,0,1,1) + 0.1)

	pm <- read.table(paste0(name,".pm"))
	pmc <- read.table(paste0(name,".pmc"))
		
		#5
		plot(pm,type="l",xlab="Slow Rel. Amp.",ylab="Fast Rel. Amp",yaxp  = c(-1, 1, 4),xaxp  = c(-1, 1, 4),xlim=c(-1,1),ylim=c(-1,1))

		
		#6
		plot(pmc,type="l",xlab="Slow Rel. Amp.",ylab="Fast Rel. Amp",yaxp  = c(-1, 1, 4),xaxp  = c(-1, 1, 4),xlim=c(-1,1),ylim=c(-1,1),yaxt='n')
		
		#Event info
		plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')



		

			text(x = 0, y = 0.3, paste("event", namelist3[wh2] ,"\n",
                             "-------------------------\n",
                             "Depth:",depth,"km\n",
				"Distance:",dist,"km\n",
				"Magnitude:",mag,
				"\n-------------------------\n", "Splitting windows (relative to S-pick)\n",
				"wbeg:",wbeg1,"to",wbeg2,wbegn,"\n",
				"wend:",wend1,"to",wend2,wendn,"\n",
				"Selected:",sel1,"to",sel2,"\n",
				"Length:",sel2-sel1), 
 			    cex = 1, col = "black",pos=4)


				plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')

						text(x = 0, y = 0.3, paste("results: GRADE", ilog$grade ,"\n",
                             "-------------------------\n",
                             "fast:",ilog$fast,"+/-",ilog$dfast,"(°)\n\n",
				"dt:",ilog$tlag,"+/-",ilog$dtlag,"(s)\n\n",
				"spol:",ilog$spol,"+/-",ilog$dspol,"(°)\n"), 
 			    cex = 1, col = "black",pos=4)






		title(ylab="                                                                       Fast Rel. Amp",
     		 outer = TRUE, line = 3)

	title(xlab = "Slow Rel. Amp.",
     		 outer = TRUE, line = -16.5)


		pl <- 4
		#my.plots[[pl]] <- recordPlot()


	graphics.off()
	
	#order <- c(7,6,1,2,3,5,4)
	

		
}