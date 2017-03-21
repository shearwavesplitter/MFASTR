#Currently, b (B sac header) should always equal zero. Adding it as an option incase functionality is added to allow b to change
#' @title S-wave SNR
#' @description Determine the signal to noise ratio around the S-wave pick (workhorse of filter_spread)
#' @param E Vector signal of the east component
#' @param N Vector signal of the north component
#' @param s S-wave pick time
#' @param p P-wave pick time
#' @param dt Sample interval 
#' @param t_win_snr Window for SNR (s)
#' @param t_err Modification to t_win_snr to account for error in S-pick (s)
#' @param type If type is set to "verylocal" then the P-wave pick (if present) is used to set t_win_snr
#' @return Signal to noise ratio around the S-wave pick
#' @export
snr <- function(E,N,s,p=-12345,dt,t_win_snr=3,t_err=0.05,b=0,type="normal"){
	if (type == "verylocal"){
		if (p != -12345){
			halfsminp <- (s-p)/2
				if(halfsminp < t_win_snr){
					t_win_snr <- halfsminp
				}
		}
		
	}
	signalbeg <- s + t_err
	noisebeg <- s-t_win_snr
	if (noisebeg < b){noisebeg <- b} #shouldn't be used currently
	if (signalbeg < b){signalbeg <- b} #"
	signalend <- signalbeg + t_win_snr
	noiseend <- noisebeg + t_win_snr
	#Cut records to noise and signal portions
	Esignal <- cut_simple(E,dt,signalbeg,signalend,b=b)
	Nsignal <- cut_simple(N,dt,signalbeg,signalend,b=b)
	Enoise <- cut_simple(E,dt,noisebeg,noiseend,b=b)
	Nnoise <- cut_simple(N,dt,noisebeg,noiseend,b=b)
	#calculate their rms, MFAST does not detrend or demean at this step. This was causing us to overestimate SNR. Now it gives a slight underestimate
	#Esignal <- detrend(Esignal)
	#Nsignal <- detrend(Nsignal)
	#Enoise <- detrend(Enoise)
	#Nnoise <- detrend(Nnoise)

	#Esignal <- Esignal- mean(Esignal)
	#Nsignal <- Nsignal- mean(Nsignal)
	#Enoise <- Enoise- mean(Enoise)
	#Nnoise <- Nnoise- mean(Nnoise)
	esig <- rms(Esignal)
	nsig <- rms(Nsignal)
	enoi <- rms(Enoise)
	nnoi <- rms(Nnoise)
	rmse <- esig/enoi
	#print(rmse)
	rmsn <- nsig/nnoi
	#signal to noise ratio
	sn <- (rmse+rmsn)/2
return(sn)
}