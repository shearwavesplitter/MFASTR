#' @export
write_sample <- function(path,type="normal"){
setwd(path)
	if(type == "normal"){
		for (i in 1:length(sample_normal)){
			write <- sample_normal[[i]][[1]]
			writename <- sample_normal[[i]][[1]]$fn
			sm.write1sac(write,writename)
		}
	}else{

	if(type == "verylocal"){
		for (i in 1:length(sample_verylocal)){
			write <- sample_verylocal[[i]][[1]]
			writename <- sample_verylocal[[i]][[1]]$fn
			sm.write1sac(write,writename)
		}
	}else{print("type not found. Use verylocal or normal")}
	}
}