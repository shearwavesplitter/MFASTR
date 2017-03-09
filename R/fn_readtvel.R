#' @export
readtvel <- function(name){
	print(paste0("Reading ",name))
	v <- read.table(name,skip=2)
return(v)
}