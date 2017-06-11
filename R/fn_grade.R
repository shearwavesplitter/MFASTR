#' @title Grade .summ file
#' @description Grades a .summ file (do_station automatically grades)
#' @param path Path to .summ file to be graded
#' @param minsnr Minimum SNR allowed for an AB+ grade
#' @param tlagmax Maximum time delay allowed for an AB+ grade
#' @param minl Minimum lambdamax allowed for a AB+ grade
#' @param mfast Set to TRUE to grade a .summ file produced by the original MFAST
#' @export
#' @examples
#' # (Re)grade LHOR2.75.summ
#' write_sample("~/mfast/sample_data/raw_data")
#' do_station_simple(path="~/mfast/sample_data/raw_data")
#' pathto <- "~/mfast/sample_data/raw_data/LHOR2.summ_files/LHOR2.75.summ"
#' grade(pathto)
#Castelazzi grading is based on castelazzi however, to expand to potentially more than 3 filters, all values must be within 10 degrees of their mean. If they are not then the one furtherest from the mean is removed (favouring removal of worse filters) and the test is repeated 
#Grading of very local in the MFAST sample data uses the normal defaults (e.g. SNR > 3 for AB measurement). Makes more sense to use maxnsr (minsnr) from processing. However, in the do_station scripts, we follow MFAST's approach. 
grade <- function(path,minsnr=3,tlagmax=1,minl=0,mfast=FALSE){
	print("Grading measurements")
	print(paste0("Miniumum SNR = ",minsnr))
	print(paste0("Maximum delay time = ",tlagmax*0.8))
	if(mfast){
	 summ <- readmfast(path)
	}else{
		summ <- read.csv(path)
	}
	if(is.null(summ$null)){stop("No null vector found, try setting mfast=TRUE if using original .summ file")}
	summ <- summ[summ$gradeABCNR %in% c("ACl","BCl"), ]
	nulls <- subset(summ, null == TRUE)


	summ <- subset(summ, null != TRUE)
	dir <- dirname(path)
	nam <- basename(path)
	if(length(nulls$fast) > 0){
		write.table(nulls,file=paste0(dir,"/NULL_",nam),quote=FALSE,row.names=FALSE,sep=",")
	}
	subs <- summ
	finalgrade <- rep("n",length(subs$fast))
	if(length(subs$fast) == 0){return()}
for (i in 1:length(subs$fast)){
	if ((subs$tlag[i] < (0.8*tlagmax)) && (subs$SNR[i] > minsnr) && (subs$Dfast[i] < 10)){
			finalgrade[i] <- "AB"
	} else {

			finalgrade[i] <- "NA"
	}
}


subs <- subset(subs, finalgrade == "AB")

subs <- subset(subs, lambdamax > minl)

drops2 <- c("null")
subs2 <- subs[ , !(names(subs) %in% drops2)]
if(length(subs2$fast) == 0){return()}
write.table(subs2,file=paste0(dir,"/AB_",nam),quote=FALSE,row.names=FALSE,sep=",")

#Castelazzi filtering. At least two filters have to give a similar result

###Small function to determine the mean of the fast azimuths in degrees
mean.axial <- function(vec){
	vec <- vec*2
	vec <- vec*pi/180
	n <- length(vec)
	C <- (1/n)*sum(cos(vec))
	S <- (1/n)*sum(sin(vec))
	m <- atan2(S,C)
	m2 <- m/2
	m2 <- m2*(180/pi)
return(m2)
}

if(length(subs$cuspid) > 1){

unev <- unique(subs$cuspid)
filt <- as.numeric(gsub("^.*?fb","",subs$event))
maxf <- max(filt)
subs <- cbind(subs,filt)
	for (i in 1:length(unev)){
		add <- 1 #To supress error
		rm('add')
		eventn <- unev[i]
		fsub <- subset(subs, cuspid == eventn)
		
			if (length(fsub$fast) == 1){
				add <- fsub
				add$finalgrade <- "F1"
			}else{sw <- FALSE
				while(sw == FALSE){
					ln <- length(fsub$fast)
					m <- mean.axial(fsub$fast)
					dif <- abs(fsub$fast*2-m*2)
					for (k in 1:length(dif)){
						if(dif[k] > 180){dif[k] <- 360-abs(dif[k])}
					}
					dif <- dif/2
	
					difsub <- subset(dif, dif < 10*2)
					if(length(difsub) == ln){nmin <- which.min(fsub$Dfast); add <- fsub[nmin,];add$finalgrade <- paste0("F",ln);sw <- TRUE}else{del <- max(which.max(dif)); fsub <- fsub[-del,]}
					if(length(difsub) == 1){sw <- TRUE}
				}

			}
		if(exists('add',inherits=FALSE)){
			if(exists('uniquev',inherits=FALSE)){
				uniquev <- rbind(uniquev,add)
			}else{
				uniquev <- add
			}
		}
	}
drops <- c("filt","null")
uniquev <- uniquev[ , !(names(uniquev) %in% drops)]
write.table(uniquev,file=paste0(dir,"/CZ_",nam),quote=FALSE,row.names=FALSE,sep=",")
}
}