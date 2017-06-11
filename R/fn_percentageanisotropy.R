#' @title Weighted percentage anisotropy
#' @description Determine the weighted percentage anisotropy and shear wave anisotropy for each stations in a summary file
#' @param summ Dataframe containing MFASt summary file
#' @param weights A vector containing the desired weights 
#' @return A dataframe containing each station and their corresponding percentage anistropy and shear wave anisotropy
#' @export
perani <- function(summ,weights=NULL){
	if(is.null(weights)){
		weights <- rep(1,length(summ$fast))
	}
	summ <- cbind(summ,weights)
	unstat <- unique(summ$stat)
	for (i in 1:length(unstat)){
		sub <- subset(summ, stat == unstat[i])
		tlag <- sub$tlag
		topdist <- sub$distevstat
		depth <- sub$depthkm
		d <- sqrt(topdist^2+depth^2)
		vf <- d/sub$ttime
		vs <- d/(sub$ttime+sub$tlag)
		vavg <- (vf+vs)/2
		rav <- ((vf-vs)/vavg)*100
		name <- as.character(unstat[i])
		SWA <- ((vf-vs)/vf)*100
		mrav <- sum(rav*sub$weights)/sum(sub$weights)
		mswa <- sum(SWA*sub$weights)/sum(sub$weights)
		vec <- cbind(name,round(mrav,2),round(mswa,2))
		if (exists('table2')){
			table2 <- rbind(table2,vec)
		} else {
			table2 <- vec
		}

	}
	table2 <- as.data.frame(table2)
	colnames(table2) <- c("Station", "panisot","swa")
	table2 <- table2[order(table2$Station),]
return(table2)
}