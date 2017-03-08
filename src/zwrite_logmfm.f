c-----------------------------------------------------------------------
	subroutine zwrite_logmfm(event,lu,file_log,
     >	ev_x,ev_y,ev_z,ev_time,
     >	wbeg_best,wend_best,tlag_best,dtlag_best,
     >	fast_best,dfast_best,spol_best,dspol_best,
     >	fastSTR,dfastSTR,fastDIP,dfastDIP,
     >	spolSTR,dspolSTR,spolDIP,dspolDIP,grade,ndf,lambda2_min)
c-----------------------------------------------------------------------
c	write out log file
c-----------------------------------------------------------------------
c	n.teanby	9-9-02	original code
c      mks 30 May 2008 modifying to print out grade
c      mks 31 March 2010 modifying to print out number of degrees of freedom
c      mks 21 July 2010 modifying to print out smallest eigenvalue lambda2_min
c-----------------------------------------------------------------------

	implicit none
	integer lu,ndf
	real wbeg_best,wend_best
	real tlag_best,dtlag_best
	real fast_best,dfast_best,spol_best,dspol_best
	real fastSTR,dfastSTR,fastDIP,dfastDIP
	real spolSTR,dspolSTR,spolDIP,dspolDIP
	real ev_x,ev_y,ev_z,ev_time,lambda2_min
	character*50 file_log,event
        character*5 grade

c  ** write log file **
	open(lu,file=file_log,status='unknown')
	write(lu,1021)
	write(lu,1020) event,ev_x,ev_y,ev_z,ev_time,
     >   wbeg_best,wend_best,tlag_best,dtlag_best,
     >   fast_best,dfast_best,spol_best,dspol_best,
     >   fastSTR,dfastSTR,fastDIP,dfastDIP,
     >   spolSTR,dspolSTR,spolDIP,dspolDIP,grade,ndf,lambda2_min
	close(lu)

c  ** formats **
1020	format(a50,2x,e15.7,2x,e15.7,2x,e15.7,2x,e15.7,2x,
     >   f15.6,2x,f15.6,2x,f10.6,2x,f10.6,2x,
     >   f8.3,2x,f8.3,2x,f8.3,2x,f8.3,2x,
     >   f8.3,2x,f8.3,2x,f8.3,2x,f8.3,2x,
     >   f8.3,2x,f8.3,2x,f8.3,2x,f8.3,2x,a5,i4,e15.7)
1021	format('event',61x,'x',16x,'y',16x,'z',13x,'time',8x,
     >   'wbeg_best',8x,'wend_best',8x,
     >   'tlag',7x,'dtlag',6x,
     >   'fast ',4x,'dfast',7x,'spol',5x,'dspol',3x,
     >   'fastSTR',2x,'dfastSTR',3x,'fastDIP',2x,'dfastDIP',3x,
     >   'spolSTR',2x,'dspolSTR',3x,'spolDIP',2x,'dspolDIP',2x,'grade',
     >    2x,'ndf',2x,'lambda2_min')
	
	return
	end
