#' @title Path cluster
#' @description Clusters measurements by their station to event paths
#' @param summ An MFASTR summary file
#' @param savepath Path to save plots and files
#' @param hvec A vector of station elevations 
#' @param kmax Maximum number of clusters
#' @param runs Number of runs for the clustering
#' @param minsamples Minimum number of measurements for that station
#' @param seed Random number seed
#' @param plot Create plots?
#' @param palette Vector of user defined colours for plotting if the number of clusters is greater than 12
#' @param rot Degrees to rotate 3D lower hemisphere plot
#' @return Creates folders containing the cuspids of events in each cluster along with the p-value of the Rayleigh test for polarisations in that cluster. 
#' @details Uses the movMF package to fit mixtures of von Mises Fisher distributions to the station to event paths projected onto a unit hemisphere below each station.
#' @export
#' @examples
#' # Run for all stations and save to clustest folder
#' cz <- summ.cz("~/summfiles")
#' pathclus(cz,savepath="~/clustest",plot=TRUE)
pathclus <- function(summ,savepath,hvec=NULL,kmax=7,runs=20,minsample=55,seed=NULL,plot=TRUE,rot=180,palette=NULL) {
path <- savepath
data <- summ
mM <- require(movMF)
require(MFASTR)
if(!mM){stop("Please install movMF package")}

#reference points for converting to x,y co-ordinates
ymin <- -38.6346
xmin <- 176.021

ymax <- -38.5134
xmax <- 176.51


uniquest <- unique(data$stat)
sn <- 0
#Vector of station elevations
if (is.null(hvec)){
	hvec <- rep(0,length(uniquest))
}
for (s in uniquest) {
	sn <- sn+1
	station <- s
	print(s)
	statn <- subset(data, stat == station)
		line <- cbind(s,1,NA)
	if (length(statn$fast) < minsample) {
		n <- length(statn$fast)
		rtest <- rayleigh.test(circular(rad(statn$fast*2)))
		print(paste0("Less than ", minsample," measurements"))
		print(rtest$p)

		pval <- round(rtest$p,digits=3)
		textname <- paste0(station,"_cluster",1,"_p-val_",pval)
		filen <- paste0(path,"/",textname)
		cuspids <- statn$cuspid
		write.table(cuspids,file=filen,col.names=FALSE,row.names=FALSE,quote=FALSE)
	}
	if (length(statn$fast) >= minsample) {
		evlols <- statn$evlo
		evlals <- statn$evla
		evdp <- statn$depthkm
		val <- hvec[sn]
		evdp <- evdp+val
		la <- statn$slat[[1]]
		lo <- statn$slon[[1]]

		dx <- (evlols-xmin)*111.320*cos(rad(evlals))
		dy <- (evlals-ymin)*110.574

		sx <- (lo-xmin)*111.320*cos(rad(la))
		sy <- (la-ymin)*110.574

		diffx <- sx-dx
		diffy <- sy-dy

		#convert to spherical
		r <- sqrt(diffx^2+diffy^2+evdp^2)
		theta <- acos(evdp/r)
		phi <- atan2(diffy,diffx)

		r2 <- 1 
		x <- r2*sin(theta)*cos(phi)
		y <- r2*sin(theta)*sin(phi)
		z <- r2*cos(theta)

		m <- cbind(x,y,z)
		if (is.null(seed)){
		}else{
			set.seed(seed)
		}


		vMFs <- lapply(1:kmax, function(K)
		movMF(m,k=K,control=list(nruns=runs)))
#
		bics <- sapply(vMFs, BIC)

		print(bics)

		pr <- which.min(bics)
		it <- 0 #iterations for later tests
		line[3] <- round(bics[pr],2)
		line[2] <- pr
		fMF <- vMFs[[pr]]


		if (plot){
			brw <- require(RColorBrewer)
			p3d <- require(plot3D)
			if(!brw){warning("RColorBrewer package required to create plots")}
			if(!p3d){warning("plot3D package required to create 3D plots")}

		}
		clus <- predict(fMF)
		if (plot){	
		if(is.null(palette) & pr > 12){warning("You need to define your own colours (clusters > 12)")}else{	
			if(brw){
			if(is.null(palette)){if(pr > 8){colz <- brewer.pal(pr,"Paired")}else{colz <- brewer.pal(pr,"Dark2")}}else{colz <- palette[1:pr]}
			cols <- colz[clus]

			postscript(file=paste0(path,"/",station,"_2D.eps"), onefile=FALSE, horizontal=FALSE,width=7,height=7,paper='special')
				par(pty="s")
				plot(dx,dy,col=cols,xlab="x (km)",ylab="y (km)",asp=1,cex.axis=1.5,cex.lab=1.5)
				points(sx,sy,pch=17)
			dev.off()

			g <- 0
			if(brw & p3d){
			postscript(file=paste0(path,"/",station,"_3D.eps"), onefile=FALSE, horizontal=FALSE,width=7,height=7,paper='special')
				par(mfrow = c(1, 1))
				M <- mesh(seq(0, 2*pi, length.out = 100),
				seq(0, pi/2, length.out = 100))
				u <- M$x ; v <- M$y
				x2 <- cos(u)*sin(v)
				y2 <- sin(u)*sin(v)
				z2 <- cos(v)
				# full panels of box are drawn (bty = "f")
				#scatter3D(x2, y2, -z2, pch = ".", col = "lightgrey", bty = "f", cex = 2, colkey = FALSE,zlim=c(-1,1),theta=0)
				scatter3D(x2, y2, -z2, pch = ".", col = "lightgrey", bty = "f", cex = 2, colkey = FALSE,zlim=c(-1,1),theta=rot)

				points3D(x,y,-z,colvar=clus,col=colz,zlim=c(-1,1),add=TRUE,pch = ".",cex=5,colkey=FALSE,cex.axis=1.5,cex.lab=1.5)
			dev.off()
		}
		}
		}
		}

		for (i in 1:pr) {
			cluster <- subset(statn$fast, clus == i)
			cluster2 <- subset(statn, clus == i)
			n1 <- paste0(path,"/",station,"_azclusraw_",i)
			#n2 <- paste0(path,"/",station,"_azclusdub_",i)
				if (plot){	
			if(is.null(palette) & pr > 12){warning("You need to define your own colours (clusters > 12)")}else{	
			if(is.null(palette)){if(pr > 8){colz <- brewer.pal(pr,"Paired")}else{colz <- brewer.pal(pr,"Dark2")}}else{colz <- palette[1:pr]}
					if(brw){	
						if(pr > 8){colz <- brewer.pal(pr,"Paired")}else{colz <- brewer.pal(pr,"Dark2")}
						cols <- colz[clus]
						cls <- colz[[i]]
						plotrose(path=path,summ=cluster2,name=n1,cols=cls,antipodal="lightgrey")
					}
				}
			}
			n <- length(cluster)
			rtest <- rayleigh.test(circular(rad(cluster*2)))
			print(rtest$p)

			pval <- round(rtest$p,digits=3)
			textname <- paste0(station,"_cluster",i,"_p-val_",pval)
			filen <- paste0(path,"/",textname)
			cuspids <- subset(statn$cuspid, clus == i)
			write.table(cuspids,file=filen,col.names=FALSE,row.names=FALSE,quote=FALSE)

			}
		
		}
	if(sn == 1){lines <- line}else{lines <- rbind(lines,line)}
	}
	lines <- as.data.frame(lines)
	colnames(lines) <- c("Station","Clusters","BIC")
	filen2 <- paste0(path,"/summary")
	write.table(lines,file=filen2,col.names=TRUE,row.names=FALSE,quote=FALSE,sep=",")
	return(lines)
}
