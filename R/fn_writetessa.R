#' @title Write TESSA .summ file
#' @description Writes out a .summ file in the format required for TESSA
#' @param summ Dataframe containing the summary file of measurements to be run in TESSA
#' @param name Name of the file including path and .summ suffix (defaults to current working directory)
#' @export
#' @examples
#' # Create a .summ file for TESSA from all F1, F2 and F3 graded measurements
#' cz <- summ.cz("~/path/to/summfiles")
#' writetessa(cz,"~/TESSA/summfiles/cz.summ")
writetessa <- function(summ,name) {
	dat <- summ
	bl <- rep(NA,length(dat$fast))
	summ <- cbind(bl,bl,dat$slat,dat$slon,bl,dat$year,dat$doy_det,dat$evla,dat$evlo,bl,dat$depthkm,bl,dat$baz,dat$spol,dat$Dspol,dat$wbeg,dat$wend,bl,bl,dat$SNR,dat$tlag,dat$Dtlag,deg(dat$fast),dat$Dfast,dat$anginc,bl,bl,dat$timestamp,bl,dat$nyquist,dat$gradeABCNR,dat$filt_lo,dat$filt_HI,bl,bl,bl,dat$lambdamax,dat$ndf,dat$lambda_min)
	summ <- as.data.frame(summ)
	write.table(summ,file=name,na="",row.names=FALSE,col.names=F,quote=FALSE,sep=",")
}