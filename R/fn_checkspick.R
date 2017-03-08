###
###
# This function checks to see if there is a value other than -12345 in the spick header.
##
checkspick <- function(path,suffix="E",header="t0",E=".e",N=".n",Z=".z"){
	setwd(path)
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