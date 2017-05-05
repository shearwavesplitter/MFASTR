#' @title Check components
#' @description Checks a folder to make sure all three components are present and moves those with missing components to a subdirectory
#' @param path Path to folder 
#' @param E Suffix of the east component
#' @param N Suffix of the north component
#' @param Z Suffix of the vertical component
#' @export
checkcomp <- function(path,E=".e",N=".n",Z=".z"){
	setwd(path)
	print("Checking for all components")
	substrRight <- function(x, n){
  		substr(x, nchar(x)-n+1, nchar(x))
	}
	ls <- list.files() #All files
	ls2 <- substrRight(ls,nchar(E))
	comp1 <- subset(ls2, ls2 == E)
	comp2 <- subset(ls2, ls2 == N)
	comp3 <- subset(ls2, ls2 == Z)

	ls_east <- list.files(pattern=paste0("\\",E,"$"))
	
	if (length(comp1) == 0 && length(comp2) == 0 && length(comp3) == 0){
		print(paste0(path," has no files"))
		return(TRUE)
	}else{

		if (length(comp1) != length(comp2) | length(comp2) != length(comp3) | length(comp1) != length(comp3)){
			c2 <- 0
			for (i in 1:length(ls_east)){
				c <- 0
				name <- gsub(paste0(" *",E),"",ls_east[i])
				if(file.exists(paste0(name,E))){c <- c+1}
				if(file.exists(paste0(name,N))){c <- c+1}
				if(file.exists(paste0(name,Z))){c <- c+1}
				if(c !=3){c2 <- c2+1
					if (dir.exists("missingcomp")){}else{dir.create("missingcomp")}
					if(file.exists(paste0(name,E))){file.copy(paste0(name,E),"missingcomp");file.remove(paste0(name,E))}
					if(file.exists(paste0(name,N))){file.copy(paste0(name,N),"missingcomp");file.remove(paste0(name,N))}
					if(file.exists(paste0(name,Z))){file.copy(paste0(name,Z),"missingcomp");file.remove(paste0(name,Z))}
				}
			}
		if(c2 == length(ls_east)){return(TRUE)}else{return(FALSE)}
		}else{return(FALSE)}
	}
}