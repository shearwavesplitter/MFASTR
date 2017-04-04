#' @export
plot.rose <- function(path,summ,bins=16,arrow=TRUE,kd=FALSE,sym=16,cols="blue",antipodal="lightblue"){
	req <- require(circular)
	if(req){}else{print("plot.rose requires the 'circular' package to be installed");return()}
	
	setwd(path)
	data <- summ$fast
	#Convert to radians
	data <- data*pi/180

	#Map data to between -90 and 90 degrees
	for (i in 1:length(data)){
		if (data[i] > pi/2) {
			data[i] <- data[i]-pi
		}
		if (data[i] <= -pi/2) {
			data[i] <- data[i]+pi
		}	
	}	
	for (i in 1:length(data)){
		if (data[i] > pi/2) {
			data[i] <- data[i]-pi
		}
		if (data[i] <= -pi/2) {
			data[i] <- data[i]+pi
		}
	}
	#Convert back to degrees for mean function
	data <- data*(180/pi)
	if (is.null(summ$finalgrade)){m <- mean.weighted(data)}else{
		fgrade <- as.numeric(gsub("F","",summ$finalgrade))
		maxgrade <- max(fgrade)
		weight <- fgrade/maxgrade
		m <- mean.weighted(data,weights=weight)
	}

	R <- m[2]
	tm <- circular(m[1]*pi/180,type="angles",units="radians",template="geographics")
	data <- circular(data*pi/180,type="angles",units="radians",template="geographics")
	ty <- "p"
	#Save axial plot 
	postscript(file="rose.eps", onefile=FALSE, horizontal=FALSE,width=9,height=9,paper='special')
		smrc <- data

		plot(smrc,pch=sym,col=cols,stack=T,shrink=1.2,bins=180,ticks=T,type=ty)
		rose.diag(smrc,bins=bins,col="darkgrey",prop=1.3,add=TRUE,shrink=1.2,pch=sym)
		points(smrc+pi,pch=sym,col=antipodal,stack=T,shrink=1.2,bins=180,type=ty)
		#rose.diag(smrc,bins=bins,col="darkgrey",prop=1.3,add=TRUE,shrink=1.2,pch=sym)
	
		#visualise axial data in roseplot
		sma <- smrc+pi
		rose.diag(sma,bins=bins,col="lightgrey",prop=1.3,add=TRUE,shrink=1.2,pch=sym)
		#Kernal density for axial data
		if (kd == TRUE){
			smapp <- append(sma,smrc)
			lines(density.circular(smapp,bw=10),lwd=2,lty=3)
		}

		#Plotmean
		if (arrow == TRUE){
			arrows.circular(tm,col="red",lwd=2,shrink=R)
			arrows.circular(tm+pi,col="red",lwd=2,shrink=R)
		}
		#if (medarrow == TRUE){
	#		arrows.circular(tmed,col="darkgreen",lwd=2,shrink=R) #median arrows
	#		arrows.circular(tmed+pi,col="darkgreen",lwd=2,shrink=R) 
	#	}
	dev.off()

}