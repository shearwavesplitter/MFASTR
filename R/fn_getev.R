#' @title Get events
#' @description A handy function to retrieve specific events from a summary dataframe
#' @param summ Dataframe containing the summary file
#' @param events A vector containing the rquired event names
#' @param station Defaults to events on all stations
#' @export
getevents <- function(summ,events,station=NULL) {
	dat <- summ
	if (is.null(station[1])) {
	}else{
		dat <- subset(dat, dat$stat == station)
	}
	if (is.data.frame(events)){
		ev <- as.character(events[,1])
	} else{
		ev <- events
	}
	datev  <- dat$cuspid
	evlist <- intersect(ev,datev)
	dat <- subset(dat, cuspid %in% evlist)
return(dat)
}