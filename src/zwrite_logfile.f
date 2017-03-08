c-----------------------------------------------------------------------
	subroutine zwrite_logfile(event,lu,file_log,
     >	ev_x,ev_y,ev_z,ev_time,
     >	wbeg_best,wend_best,tlag_best,dtlag_best,
     >	fast_best,dfast_best,spol_best,dspol_best,
     >	fastSTR,dfastSTR,fastDIP,dfastDIP,
     >	spolSTR,dspolSTR,spolDIP,dspolDIP)
c-----------------------------------------------------------------------
c	write out log file
c-----------------------------------------------------------------------
c	n.teanby	9-9-02	original code
c-----------------------------------------------------------------------

	implicit none
	integer lu
	real wbeg_best,wend_best
	real tlag_best,dtlag_best
	real fast_best,dfast_best,spol_best,dspol_best
	real fastSTR,dfastSTR,fastDIP,dfastDIP
	real spolSTR,dspolSTR,spolDIP,dspolDIP
	real ev_x,ev_y,ev_z,ev_time
	character*50 file_log,event

c  ** write log file **
	open(lu,file=file_log,status='unknown')
	write(lu,1021)
	write(lu,1020) event,ev_x,ev_y,ev_z,ev_time,
     >   wbeg_best,wend_best,tlag_best,dtlag_best,
     >   fast_best,dfast_best,spol_best,dspol_best,
     >   fastSTR,dfastSTR,fastDIP,dfastDIP,
     >   spolSTR,dspolSTR,spolDIP,dspolDIP
	close(lu)

c  ** formats **
1020	format(a50,2x,e15.7,2x,e15.7,2x,e15.7,2x,e15.7,2x,
     >   f15.6,2x,f15.6,2x,f10.6,2x,f10.6,2x,
     >   f8.3,2x,f8.3,2x,f8.3,2x,f8.3,2x,
     >   f8.3,2x,f8.3,2x,f8.3,2x,f8.3,2x,
     >   f8.3,2x,f8.3,2x,f8.3,2x,f8.3)
1021	format('event',61x,'x',16x,'y',16x,'z',13x,'time',8x,
     >   'wbeg_best',8x,'wend_best',8x,
     >   'tlag',7x,'dtlag',6x,
     >   'fast ',4x,'dfast',7x,'spol',5x,'dspol',3x,
     >   'fastSTR',2x,'dfastSTR',3x,'fastDIP',2x,'dfastDIP',3x,
     >   'spolSTR',2x,'dspolSTR',3x,'spolDIP',2x,'dspolDIP')
	
	return
	end
