#This potentially can account for layers above sea level
#' @export
anginc <- function(tvel,trip){
	print(paste0("Determining angle of incidence"))

	stla <- as.numeric(as.character(trip[[1]]$HEAD$values[[which(trip[[1]]$HEAD$names == "stla")]]))
	stlo <-  as.numeric(as.character(trip[[1]]$HEAD$values[[which(trip[[1]]$HEAD$names == "stlo")]]))
	stel <-  as.numeric(as.character(trip[[1]]$HEAD$values[[which(trip[[1]]$HEAD$names == "stel")]]))
	evla <-  as.numeric(as.character(trip[[1]]$HEAD$values[[which(trip[[1]]$HEAD$names == "evla")]]))
	evlo <-  as.numeric(as.character(trip[[1]]$HEAD$values[[which(trip[[1]]$HEAD$names == "evlo")]]))
	evdp <-  as.numeric(as.character(trip[[1]]$HEAD$values[[which(trip[[1]]$HEAD$names == "evdp")]]))
	if(stel == -12345){stel <- 0}

	dist <- GreatDist(stlo,stla,evlo,evla)

	z <- tvel$V1
	vp <- tvel$V2
	vs <- tvel$V3
	rho <- tvel$V4
	rp <- 6371
	conr <- 20
	moho <- 35
	d410 <- 410
	d660 <- 660
	cmb <- 2891.5
	icb <- 5153.5
	qp <- rep(NaN, length(z))
	qs <- rep(NaN, length(z))
	mod <- list(z,vp,vs,rho,qp,qs,rp,conr,moho,d410,d660,cmb,icb)
	
	names(mod) <- c("z","vp","vs","rho","qp","qs","rp","conr","moho","d410","d660","cmb","icb")
	ray <- Traveltime('S',dist$ddeg,evdp,mod)
	rayf <- which(ray$tt == min(ray$t))
	rayp <- ray$p[rayf]
	p <- rayp*360 /2 /3.1415927
	R <- rp
	c <- vs[1]
	i <- atan2(p*c/(R*sqrt(1-(p*c/R)^2)),1) /2 /3.1415927 * 360

return(i)
}