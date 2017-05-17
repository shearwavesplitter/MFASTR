#' @export
summ.read <- function(path,pattern){
pat <- paste0("^",type,"_*")
ls <- list.files(path,recursive=TRUE,pattern=pattern)
require(parallel)
ls <- paste0(path,"/",ls)
m <- mclapply2(ls,read.csv)
m <- do.call(rbind.data.frame, m)
return(m)
}

summ.null <- function(path){
pat <- paste0("^NULL_*")
s <- summ.read(path=path,pattern=pat)
return(s)
}

summ.cz <- function(path,type="CZ"){
pat <- paste0("^CZ_*")
s <- summ.read(path=path,pattern=pat)
return(s)
}

summ.ab <- function(path,type="AB"){
pat <- paste0("^AB_*")
s <- summ.read(path=path,pattern=pat)
return(s)
}