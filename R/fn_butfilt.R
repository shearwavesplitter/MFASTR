#RSEIS like wrapper for signal butterworth filter
#' export
butfilt <- function(a,fl=0,fh=0.5,deltat=1,type="BP",proto="BU",npoles=5,zp=TRUE,chebstop=30,trbndw=0.3,RM=FALSE){
    if(RM){
        a <- a-mean(a)  
    }

    rtype=c("BP","BR","LP","HP")
    stype=c("pass","stop","low","high")
    type2=stype[which(rtype == type)]


    fs=1/deltat
    nyq <- 0.5*fs
    fls=fl/nyq
    fhs=fh/nyq

    if (type == "BP" | type == "BR"){
        efilt <- c(fls,fhs)
    }

    if (type == "HP"){
        efilt <- fls
    }

    if (type == "LP"){
        efilt <- fhs
    }

    if (proto == "BU"){
        bf <- butter(npoles, efilt,type=type2,plane="z")
    }
    
    if (proto == "C1"){
        bf <- cheby1(npoles, W=efilt,type=type2,plane="z",Rs=chebstop,Rp=trbndw)
    }

    if (proto == "C2"){
        bf <- cheby2(npoles, W=efilt,type=type2,plane="z",Rs=chebstop,Rp=trbndw)
    }

    if (zp){
        r=filtfilt(bf,a)
    }else{
        r=as.numeric(filter(bf,a))
    }
    return(r)
}

