##
# This functions reads in a single triplet and applies corrections to the header per mfast_check
### ASSUMES that the east component contains the S-wave pick
### Missing: sac syncs the files
### Missing: doesnt fix B header being different. 
#####
# REQUIRES PICKS TO BE RELATIVE TO START OF TRACE (iztype IB) -- check this
####
#' @export
readtriplet <- function(event,path=".",E=".e",N=".n",Z=".z",header="t0",pheader="a"){
	setwd(path)
	print(paste0("Reading event ",event))
	Esac <- JSAC.seis(paste0(event,E))[[1]]
	Nsac <- JSAC.seis(paste0(event,N))[[1]]
	Zsac <- JSAC.seis(paste0(event,Z))[[1]]
	trip <- list(Esac,Nsac,Zsac)
	val <- as.character(Esac$HEAD$values[[which(Esac$HEAD$names == header)]])
	val2 <- as.character(Esac$HEAD$values[[which(Esac$HEAD$names == paste0("k",header))]])
	val3 <- as.character(Zsac$HEAD$values[[which(Zsac$HEAD$names == pheader)]])
	if (val == -12345){print(paste0(event," has no Spick on the East component"));return(NULL)}
	for (i in 1:3){
		trip[[i]]$HEAD$values <- as.character(trip[[i]]$HEAD$values) ###Convert factors to characters
		trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "t5")]] <- val
		trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "t0")]]  <- val
		trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "a")]]  <- val3
		trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "kt0")]] <- val2
		trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "kt5")]] <- val2
		trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "resp0")]]  <- -12345
		trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "resp1")]]  <- -12345
		trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "resp2")]]<- -12345
		trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "resp3")]] <- -12345
		trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "kuser0")]] <- -12345
		trip[[i]]$amp <- trip[[i]]$amp-mean(trip[[i]]$amp) #Is this the correct way to remove the mean? (rmean from sac)
		trip[[i]]$amp <- detrend(trip[[i]]$amp) #Detrend waveform
	}

	

	# downsample broadband data to prevent program crash
	#
	# Can't use interpolate to downsample data though--need to use decimate!
	#Sac appears to apply a FIR filter here. We do not
	for (i in 1:3){
		if (trip[[i]]$dt < 0.0099){
			r <- as.integer(0.01/trip[[i]]$dt) #r should be within 2 and 7
			if (r >= 2 && r <= 7){
				print(paste0("Downsampling data rate by factor of ", r))
				print("WARNING: downsampling has not been tested and may not work correctly")
				for (j in (1:length(trip[[i]]$amp))[seq(r, length(trip[[i]]$amp), r)]){
					trip[[i]]$amp[j] <- NA 
				}
			trip[[i]]$amp <- trip[[i]]$amp[!is.na(trip[[i]]$amp)]		
			trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "npts")]]  <- length(trip[[i]]$amp)
			trip[[i]]$N  <- length(trip[[i]]$amp)
			samprate <- trip[[i]]$dt*r
			trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "delta")]]  <- samprate
			trip[[i]]$dt <- samprate
			}else{print("factor is out of bounds")}
		}
	}


	
	#cut all files 15 sec after S-Pick and 15 sec before (or 3.5 before P-Pick),looks in a header for P-pick
	for (i in 1:3){
		trip[[i]] <- cutrecord(trip[[i]],sheader=header) #KZtime etc not updated yet. Is it required?
		trip[[i]]$amp <- trip[[i]]$amp-mean(trip[[i]]$amp) #Is this the correct way to remove the mean? (rmean from sac)
		trip[[i]]$amp <- detrend(trip[[i]]$amp) #Detrend waveform
	}


	#check to make sure B is equal for all components
	b1 <- trip[[1]]$HEAD$values[[which(trip[[1]]$HEAD$names == "b")]]
	b2 <- trip[[2]]$HEAD$values[[which(trip[[2]]$HEAD$names == "b")]]
	b3 <- trip[[3]]$HEAD$values[[which(trip[[3]]$HEAD$names == "b")]]

	if(b1 != b2 | b2 != b3){print("WARNING: B header is different. Contact developers");return(NULL)}

	for (i in 1:3){
		trip[[i]]$HEAD$values <- as.factor(trip[[i]]$HEAD$values) ###Convert characters back to factors
	}

	return(trip)
}