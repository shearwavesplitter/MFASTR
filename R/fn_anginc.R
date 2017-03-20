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

	mod <- tvel
	ray <- Traveltime('S',dist$ddeg,evdp,mod)
	rayf <- which(ray$tt == min(ray$t))
	rayp <- ray$p[rayf]
	p <- rayp*360 /2 /3.1415927
	R <- mod$rp
	c <- mod$vs[1]
	i <- atan2(p*c/(R*sqrt(1-(p*c/R)^2)),1) /2 /3.1415927 * 360

return(i)
}