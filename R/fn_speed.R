#' @export
speed <- function(path,mult=1){
setwd(path)
require(parallel)
print("Determining fastest number of threads")
nc <- detectCores()*mult

st <- ceiling(nc/4)

dir.create("speed")
#write_sample(paste0(path,"/speed"))
for (i in 1:ceiling(nc/2)){
write_sample(paste0(path,"/speed"))
ls <- list.files(paste0(path,"/speed"))
ls2 <- substring(list.files(paste0(path,"/speed")),0,4)
wh <- which(ls2 == "2002")
name <- ls[wh]
file.rename(paste0(path,"/speed/",name), paste0(path,"/speed/",i,"_",name))
}

fn <- function(event) readtriplet(event,path=paste0(path,"/speed"))

s <- seq(st,nc)

f <- function(i){ 
ls_east <- list.files(pattern=paste0("\\.e$"))
ls_all <- gsub(paste0(" *.e"),"",ls_east)
time <- system.time(mclapply(ls_all,fn,mc.silent=TRUE,mc.cores=i))
return(time)
}

t <- sapply(s,f)
setwd(path)
m <- which(t[3,] == min(t[3,]))+(st-1)[[1]]
setwd(path)
unlink(paste0(path,"/speed"),recursive=TRUE)
return(as.numeric(m))
}