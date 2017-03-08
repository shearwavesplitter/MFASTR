#cut record 15 seconds before the S (or 3.5 seconds before the P) and 15 seconds after the S
cutrecord <- function(wav,pheader="a",sheader="t0"){
	#Convert relevant entries to type numeric
	wav$HEAD$values <- as.character(wav$HEAD$values)
	a <- wav$HEAD$values[[which(wav$HEAD$names == pheader)]] #This should always be a when running full script
	s <- wav$HEAD$values[[which(wav$HEAD$names == sheader)]]
	b <- wav$HEAD$values[[which(wav$HEAD$names == "b")]] #I'm not sure how this works if b isn't zero
	e <- wav$HEAD$values[[which(wav$HEAD$names == "e")]]
	o <- wav$HEAD$values[[which(wav$HEAD$names == "o")]]
	e <- as.numeric(e)
	a <- as.numeric(a)
	s <- as.numeric(s)
	b <- as.numeric(b)
	o <- as.numeric(o)
	if(b != 0){print("WARNING: B is not equal to zero, this functionality has not been tested")}
	if(a == -12345){
		fcut <- s-15
	}else{
		fcut <- a-3.5	
	}
	scut <- s+15
	
	samprate <- as.integer(wav$dt*1000000)/1000000 #fixing floating point error
	#Make sure we're not cutting outside of trace
	if (fcut < b){fcut <- b}
	if (scut > e){scut <- e}
	print(paste0("Cutting between ",round(fcut,2),"s and ",round(scut,2),"s"))
	#Delete samples
	fdel <- as.integer(fcut/samprate)
	sdel <- as.integer((e-scut)/samprate)
	if(sdel > 0){wav$amp <- head(wav$amp,-sdel)}
	if(fdel > 0){wav$amp <- tail(wav$amp,-fdel)}
	#if cut files are still too long cut to 10 seconds after S-pick (cut another 5 seconds off
	if (length(wav$amp) > 9999){
		sdel2 <- as.integer((e-(scut-5))/samprate)-sdel
		if(sdel2 > 0){wav$amp <- head(wav$amp,-sdel2)}
		sdel <- as.integer((e-(scut-5))/samprate)
	}
	if (length(wav$amp) > 9999){
		print("File too long");return(NULL)
	}
	#Update headers 
	ftime <- fdel*samprate
	stime <- sdel*samprate
	wav$HEAD$values[[which(wav$HEAD$names == "e")]] <- as.character(e-(ftime+stime))
	wav$DATTIM$t2 <- wav$DATTIM$t2-(ftime+stime)
	for (i in 1:10){
		nam <- paste0("t",i-1)
		if(as.numeric(as.character(wav$HEAD$values[[which(wav$HEAD$names == nam)]]) != -12345)){wav$HEAD$values[[which(wav$HEAD$names == nam)]] <- as.character(as.numeric(as.character(wav$HEAD$values[[which(wav$HEAD$names == nam)]]))-ftime)}
	}
	if(a != -12345){wav$HEAD$values[[which(wav$HEAD$names == "a")]] <- as.character(a-ftime)}
	if(o != -12345){wav$HEAD$values[[which(wav$HEAD$names == "o")]] <- as.character(o-ftime)}
	wav$HEAD$values[[which(wav$HEAD$names == "npts")]]  <- as.character(length(wav$amp))
	wav$N  <- length(wav$amp)
#For now I'm not updating KZtime etc
return(wav)
}