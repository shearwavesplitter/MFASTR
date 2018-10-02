#' @export
summ.read <- function(path,pattern=".summ$"){
ls <- list.files(path,recursive=TRUE,pattern=pattern)
if(pattern == ".summ"){
	nulls <- list.files(path,recursive=TRUE,pattern="^NULL_*")
	czs <- 	list.files(path,recursive=TRUE,pattern="^CZ_*")
	abs <- list.files(path,recursive=TRUE,pattern="^AB_*")
	ls <- ls[!(ls %in% nulls)]
	ls <- ls[!(ls %in% czs)]
	ls <- ls[!(ls %in% abs)]
}	
require(parallel)
ls <- paste0(path,"/",ls)
m <- mclapply2(ls,read.csv,stringsAsFactors=FALSE)
m <- do.call(rbind.data.frame, m)
m$fast <- circular(m$fast,units="degrees",template="geographic")
m <- subset(m, !is.na(m$fast)) ## Some rows appear to be all NA. I have to check to see where this is coming from. Probably when a filter fails?

########### This part is for RK/NM project. It won't affect your data if you station names are different


m$stat[m$stat == "NS09"] <- "NS16"
m$stat[m$stat == "NS08"] <- "NS18"

#reorient borhole stations
m$fast[m$stat == "NS12"] <- m$fast[m$stat == "NS12"]-(360-285.73)
m$fast[m$stat == "NS13"] <- m$fast[m$stat == "NS13"]-(360-292.03)
m$fast[m$stat == "NS14"] <- m$fast[m$stat == "NS14"]-(360-65.31)
#spol is not unwound in this script
m$spol[m$stat == "NS12"] <- m$spol[m$stat == "NS12"]-(360-285.73)
m$spol[m$stat == "NS13"] <- m$spol[m$stat == "NS13"]-(360-292.03)
m$spol[m$stat == "NS14"] <- m$spol[m$stat == "NS14"]-(360-65.31)

#unwind values after potential borehole reorientation
for (i in 1:length(m$fast)){
	if (m$fast[i] > 180/2) {
		m$fast[i] <- m$fast[i]-180
	}
	if (m$fast[i] <= -180/2) {
		m$fast[i] <- m$fast[i]+180
	}
}

for (i in 1:length(m$fast)){
	if (m$fast[i] > 180/2) {
		m$fast[i] <- m$fast[i]-180
	}
	if (m$fast[i] <= -180/2) {
		m$fast[i] <- m$fast[i]+180
	}
}



return(m)
}


#' @title Read null
#' @description Reads in multiple null graded .summ files
#' @param path The path to the folder containing the .summ files
#' @return A dataframe containing all the .summ files
#' @export
summ.null <- function(path){
pat <- paste0("^NULL_*")
s <- summ.read(path=path,pattern=pat)
return(s)
}
#' @title Read cz
#' @description Reads in multiple CZ graded .summ files
#' @param path The path to the folder containing the .summ files
#' @return A dataframe containing all the .summ files
#' @export
summ.cz <- function(path){
pat <- paste0("^CZ_*")
s <- summ.read(path=path,pattern=pat)
return(s)
}
#' @title Read AB
#' @description Reads in multiple AB graded .summ files
#' @param path The path to the folder containing the .summ files
#' @return A dataframe containing all the .summ files
#' @export
summ.ab <- function(path){
pat <- paste0("^AB_*")
s <- summ.read(path=path,pattern=pat)
return(s)
}