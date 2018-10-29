#' @title Plot rose diagram
#' @description Plots a rose diagram of data from a .summ file
#' @param path Path to folder to save plots
#' @param name Name of plot
#' @param bins Number of bins
#' @param kd Kernal density?
#' @param sym Symbol for outer points (select a blank symbol for no outer points)
#' @param prop Scale length of rose diagram bins
#' @param bincol Colour of bins
#' @param bincol Colour of antipodal bins
#' @param cols Colour of points
#' @param antipodal Colour of antipodal points
#' @param axes Plot axes?
#' @param arrow Options; "mean", "median", "mean&median", or anything else for no arrow
#' @param arwcol Arrow colour
#' @param arwlty Arrow line type (lty)
#' @param arwlwd Arrow line thickness (lwd)
#' @param arwlwd Arrow line thickness (lwd)
#' @param arwlwd Arrow length scale (0-1). Defaults to mean resultant length
#' @export
plotrose <- function(path,summ,name="rose.eps",bins=16,kd=FALSE,sym=16,prop=1.3,bincol="darkgrey",antibincol="lightgrey",cols="blue",antipodal="lightblue",axes=TRUE,arrow="mean",arwcol="red",arwlty=1,arwlwd=2,arwscale=NA){
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
	if (is.null(summ$finalgrade)){m <- meanaxial(data)}else{
		fgrade <- as.numeric(gsub("F","",summ$finalgrade))
		maxgrade <- max(fgrade)
		weight <- fgrade/maxgrade
		m <- meanaxial(data,weights=weight)
	}

	R <- m[2]
	tm <- circular(m[1]*pi/180,type="angles",units="radians",template="geographics")
	data <- circular(data*pi/180,type="angles",units="radians",template="geographics")
	ty <- "p"
	#Save axial plot 
	ltyval <- 1
	if(!axes){ltyval <- 0}
	postscript(file=name, onefile=FALSE, horizontal=FALSE,width=9,height=9,paper='special')
		smrc <- data

		plot(smrc,pch=sym,col=cols,stack=T,shrink=1.2,bins=180,ticks=axes,type=ty,axes=axes,control.circle=circle.control(lty=ltyval))
		rose.diag(smrc,bins=bins,col=bincol,prop=prop,add=TRUE,shrink=1.2,axes=axes,ticks=axes)
		points(smrc+pi,pch=sym,col=antipodal,stack=T,shrink=1.2,bins=180,type=ty)
		#rose.diag(smrc,bins=bins,col="darkgrey",prop=1.3,add=TRUE,shrink=1.2,pch=sym)
	
		#visualise axial data in roseplot
		sma <- smrc+pi
		rose.diag(sma,bins=bins,col=antibincol,prop=prop,add=TRUE,shrink=1.2,axes=axes,ticks=axes)
		#Kernal density for axial data
		if (kd == TRUE){
			smapp <- append(sma,smrc)
			lines(density.circular(smapp,bw=10),lwd=2,lty=3)
		}

        if (is.na(arwscale)){arwscale=R}

		#Plotmean
		if (grepl("mean",arrow,ignore.case=T)){
			arrows.circular(tm,col=arwcol,shrink=arwscale,lwd=arwlwd,lty=arwlty)
			arrows.circular(tm+pi,col=arwcol,shrink=arwscale,lwd=arwlwd,lty=arwlty)
		}
		if (grepl("median",arrow,ignore.case=T)){
			arrows.circular(tmed,col=arwcol,shrink=arwscale,lwd=arwlwd,lty=arwlty) #median arrows
			arrows.circular(tmed+pi,col=arwcol,shrink=arwscale,lwd=arwlwd,lty=arwlty) 
		}
	dev.off()

}