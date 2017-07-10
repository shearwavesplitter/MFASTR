#' @title Read .tvel
#' @description Reads a .tvel file and saves it in an RSEIS compatible format
#' @param name Name and path of .tvel file
#' @return RSEIS compatible dataframe containing the velocity model
#' @export
#' @examples
#' path <- "~/mfast/velocity/ak135_taupo.tvel"
#' model <- readtvel(path)
#' write_sample("~/mfast/sample_data/raw_data")
#' do_station_simple(path="~/mfast/sample_data/raw_data",tvel=model)
readtvel <- function(name){
	print(paste0("Reading ",name))
	tvel <- read.table(name,skip=2)
	z <- tvel$V1
	if(min(z) < 0){warning(".tvel files must (currently) not have negative depths")}
	vp <- tvel$V2
	vs <- tvel$V3
	rho <- tvel$V4
	rp <- 6371
	conr <- 20
	moho <- 35
	d410 <- 410
	d660 <- 660
	cmb <- 2891.5
	icb <- 5153.5
	qp <- rep(NaN, length(z))
	qs <- rep(NaN, length(z))
	mod <- list(z,vp,vs,rho,qp,qs,rp,conr,moho,d410,d660,cmb,icb)
	
	names(mod) <- c("z","vp","vs","rho","qp","qs","rp","conr","moho","d410","d660","cmb","icb")

	
return(mod)
}