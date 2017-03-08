#This potentially can account for layers above sea level
anginc <- function(path="~/MFASTR/velocity/ak135_taupo.tvel",trip){
	print(paste0("Determining angle of incidence using ",path))
	v <- read.table(path,skip=2)
		#Approximating velocity model with layers
	for (i in 1:(length(v$V1)-1)){
		one <- v[i,]
		two <- v[(i+1),]
		if(one$V1 == two$V1){v[i,] <- -12345}
	}

	for (i in 0:(length(v$V1)-2)){
		l <- length(v$V1)-i
		one <- v[l,]
		two <- v[(l-1),]
		if(one$V3 == two$V3){v[i,] <- -12345}
	}

	
	stla <- as.numeric(as.character(trip[[1]]$HEAD$values[[which(trip[[1]]$HEAD$names == "stla")]]))
	stlo <-  as.numeric(as.character(trip[[1]]$HEAD$values[[which(trip[[1]]$HEAD$names == "stlo")]]))
	stel <-  as.numeric(as.character(trip[[1]]$HEAD$values[[which(trip[[1]]$HEAD$names == "stel")]]))
	evla <-  as.numeric(as.character(trip[[1]]$HEAD$values[[which(trip[[1]]$HEAD$names == "evla")]]))
	evlo <-  as.numeric(as.character(trip[[1]]$HEAD$values[[which(trip[[1]]$HEAD$names == "evlo")]]))
	evdp <-  as.numeric(as.character(trip[[1]]$HEAD$values[[which(trip[[1]]$HEAD$names == "evdp")]]))
	if(stel == -12345){stel <- 0}
	v <- subset(v,V1 != -12345)

	v <- subset(v,V1 < evdp)
	top <- v$V1
	vel <- v$V3

	distance <- rdistaz(stla,stlo,evla,evlo)
	d <- distance$dist

	r <- Ray.time1D(d,evdp,stel,length(top),top,vel)

	ang <- r$angle
	if(ang > 90){ang <- ang-90}else{
	if(ang < 90){ang <- 90-ang}}
return(ang)
}