##
# This functions reads in a single triplet and applies corrections to the header per mfast_check
### ASSUMES that the east component contains the S-wave pick
### Missing: sac syncs the files
### Missing: doesnt fix B header being different. 
#####
# REQUIRES PICKS TO BE RELATIVE TO START OF TRACE (iztype IB) -- check this
####
#' @title Read a SAC format siesmogram triplet
#' @description Reads, cuts, and loads S-wave pick into the t5 header using RSEIS/JSAC.seis as a workhorse
#' @param event Event name
#' @param path Path to folder
#' @param E Suffix of east component 
#' @param N Suffix of north component 
#' @param Z Suffix of vertical component 
#' @param header Name of header containing the S-wave pick
#' @param pheader Name of header containing the P-wave pick
#' @param downsample Downsample if sampling rate is less than 0.01s (Defaults to FALSE, originally used to decrease computational loads)
#' @param biglong logical, TRUE=long=8 bytes (sac files written on 64bit machine)
#' @param Iendian Endian-ness of the data: 1,2,3: "little", "big", "swap". Default = 1 (little)
#' @return A list containing dataframes for each of the three components with signal and header information
#' @importFrom signal decimate
#' @export
#' @details The S-wave pick must be stored on at least the east component and the P-wave pick (if present) must be stored on the vertical component
#' @examples
#' # Read in 2002.054.09.47.lhor2
#' pathto <- "~/mfast/sample_data/raw_data"
#' write_sample(pathto)
#' event <- "2002.054.09.47.lhor2"
#' triplet <- readtriplet(event,path=pathto)
readtriplet <- function(event,path=".",E=".e",N=".n",Z=".z",header="t0",pheader="a",downsample=FALSE,biglong=TRUE,Iendian=1){
	setwd(path)
	print(paste0("Reading event ",event))
	Esac <- JSAC.seis(paste0(event,E,BIGLONG=biglong,Iendian=Iendian))[[1]]
	Nsac <- JSAC.seis(paste0(event,N,BIGLONG=biglong,Iendian=Iendian))[[1]]
	Zsac <- JSAC.seis(paste0(event,Z,BIGLONG=biglong,Iendian=Iendian))[[1]]
	trip <- list(Esac,Nsac,Zsac)
	for (i in 1:3){
	trip[[i]]$HEAD$values <- as.character(trip[[i]]$HEAD$values) ###Convert factors to characters
	###### Adjust B to zero
	b <- as.numeric(trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "b")]])
	b <- round(b,9)
	t <- as.numeric(trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == header)]])
	if(t != -12345){trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == header)]] <- as.character(t-b)}
	a  <- as.numeric(trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == pheader)]])
	if(a != -12345){trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == header)]] <- as.character(a-b)}
	o  <- as.numeric(trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == pheader)]])
	if(o != -12345){trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "o")]] <- as.character(o-b)}
	e  <- as.numeric(trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "e")]])
	if(e != -12345){trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "e")]] <- as.character(e-b)}
	trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "b")]] <- as.character(0)

	##############
	}
	
	Esac <- trip[[1]]
	Nsac <- trip[[2]]
	Zsac <- trip[[3]]

	val <- Esac$HEAD$values[[which(Esac$HEAD$names == header)]]
	val2 <- Esac$HEAD$values[[which(Esac$HEAD$names == paste0("k",header))]]
	val3 <- Zsac$HEAD$values[[which(Zsac$HEAD$names == pheader)]]
	valor <- Esac$HEAD$values[[which(Esac$HEAD$names == "o")]]
	if (as.numeric(val) == -12345){print(paste0(event," has no Spick on the East component"));return(NULL)}
	for (i in 1:3){
		#trip[[i]]$HEAD$values <- as.character(trip[[i]]$HEAD$values) ###Convert factors to characters
		trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "t5")]] <- val
		trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "t0")]]  <- val
		trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "a")]]  <- val3
		trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "kt0")]] <- val2
		trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "kt5")]] <- val2
		trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "resp0")]]  <- -12345
		trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "resp1")]]  <- -12345
		trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "resp2")]]<- -12345
		trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "resp3")]] <- -12345
		trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "user9")]] <- valor
		trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "kuser0")]] <- -12345
		trip[[i]]$amp <- trip[[i]]$amp-mean(trip[[i]]$amp) #Is this the correct way to remove the mean? (rmean from sac)
		trip[[i]]$amp <- detrend(trip[[i]]$amp) #Detrend waveform



	}

	# downsample broadband data to prevent program crash
	#
	# Can't use interpolate to downsample data though--need to use decimate! -- importing signal 
	if(downsample){
	for (i in 1:3){	
		trip[[i]]$dt <- round(trip[[i]]$dt,6)
		if (trip[[i]]$dt < 0.0099){
			r <- ceiling(0.01/trip[[i]]$dt) #r should be within 2 and 7 (SAC has issues with filters for r=7)
			if (r >= 2 && r <= 7){
				print(paste0("Downsampling data rate by factor of ", r))
				samprate <- trip[[i]]$dt*r
				#nyq <- 1/2/samprate
				#cutoff <- (1/r)*(1/2/trip[[i]]$dt) 
				#cutoff <- (1/2/trip[[i]]$dt)
				#trip[[i]]$amp <- butfilt(trip[[i]]$amp,deltat=trip[[i]]$dt,fh=cutoff,type="LP",npoles=2,proto="C1",zp=FALSE,RM=TRUE)
				#del <- !((1:length(trip[[i]]$amp)) %in% seq(r, length(trip[[i]]$amp), r))
				#for (j in (1:length(trip[[i]]$amp))){
				#	if(del[j]){trip[[i]]$amp[j] <- NA}
				#}
				print("WARNING: The order of the decimating FIR filter used by SAC is not precisely known. Currently MFASTR uses order 20.")
				trip[[i]]$amp <- decimate(trip[[i]]$amp,r,n=20,ftype="fir")

			trip[[i]]$amp <- trip[[i]]$amp[!is.na(trip[[i]]$amp)]		
			trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "npts")]]  <- length(trip[[i]]$amp)
			trip[[i]]$N  <- length(trip[[i]]$amp)
			samprate <- trip[[i]]$dt*r
			samprate <- round(samprate,6)
			trip[[i]]$HEAD$values[[which(trip[[i]]$HEAD$names == "delta")]]  <- samprate
			trip[[i]]$dt <- samprate
			}else{print("factor is out of bounds")}
		}
	}
	}

	#cut all files 15 sec after S-Pick and 15 sec before (or 3.5 before P-Pick),looks in a header for P-pick
	for (i in 1:3){
		trip[[i]] <- cutrecord(trip[[i]],sheader=header) #KZtime etc not updated yet. Is it required?
		trip[[i]]$amp <- trip[[i]]$amp-mean(trip[[i]]$amp) #Is this the correct way to remove the mean? (rmean from sac)
		trip[[i]]$amp <- detrend(trip[[i]]$amp) #Detrend waveform
	}






	for (i in 1:3){
		trip[[i]]$HEAD$values <- as.factor(trip[[i]]$HEAD$values) ###Convert characters back to factors
	}

	return(trip)
}