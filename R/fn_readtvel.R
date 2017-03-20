#' @export
readtvel <- function(name){
	print(paste0("Reading ",name))
	tvel <- read.table(name,skip=2)
	z <- tvel$V1
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