#' @export
summ.read <- function(path,pattern){
ls <- list.files(path,recursive=TRUE,pattern=pattern)
require(parallel)
ls <- paste0(path,"/",ls)
m <- mclapply2(ls,read.csv)
m <- do.call(rbind.data.frame, m)
return(m)
}
#' @export
summ.null <- function(path){
pat <- paste0("^NULL_*")
s <- summ.read(path=path,pattern=pat)
return(s)
}
#' @export
summ.cz <- function(path){
pat <- paste0("^CZ_*")
s <- summ.read(path=path,pattern=pat)
return(s)
}
#' @export
summ.ab <- function(path){
pat <- paste0("^AB_*")
s <- summ.read(path=path,pattern=pat)
return(s)
}