###
###
# This function checks to see if there is a value other than -12345 in the spick header.
##
#' @title Check S-wave picks
#' @description Checks a folder to make sure all events have S-wave picks and moves those with missing picks to a subdirectory
#' @param path Path to folder 
#' @param suffix Which component to look for the S-pick in (E, N, or Z)
#' @param header Header name of where the S-pick is stored
#' @param E Suffix of the east component
#' @param N Suffix of the north component
#' @param Z Suffix of the vertical component
#' @export
checkspick <- function(path,suffix="E",header="t0",E=".e",N=".n",Z=".z"){
	setwd(path)
	print("Checking for S-wave picks")
	c <- 0
	suffix <- get(suffix)
	ls_east <- list.files(pattern=paste0("\\",suffix,"$"))
	heads <- JSAC.seis(ls_east,HEADONLY=TRUE)

	for (i in 1:length(ls_east)){
		test <- heads[[i]]
		test2 <- test$HEAD
		wh <- which(test2$names == header)
		if (test2$values[[wh]] == "-12345"){
			print(paste0("No Spick for ",test$fn))
			c <- c+1
			if (dir.exists("nospicks")){}else{dir.create("nospicks")}
			name <- gsub(paste0(" *",suffix),"",test$fn)
			file.copy(paste0(name,E),"nospicks")
			file.remove(paste0(name,E))
			file.copy(paste0(name,N),"nospicks")
			file.remove(paste0(name,N))
			file.copy(paste0(name,Z),"nospicks")
			file.remove(paste0(name,Z))
		}
	}
	if (c == length(ls_east)){
		print("No events have Spicks")
		return(TRUE)
	} else {
		return(FALSE)	
	}

}