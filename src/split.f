c-----------------------------------------------------------------------
	program split
c-----------------------------------------------------------------------
	implicit none
	integer n,np,npc,np1,np2,np2int,f,ndf,lu
	include "SIZE_np.h"
	include "SIZE_npc.h"
	include "SIZE_np12int.h"
	logical OPT_outfiles
	real delta,b,as,fs,ppick,spick
	real fast,dfast,tlag,dtlag
	real spol,dspol,phi,theta,ev_x,ev_y,ev_z,ev_time
	real lambda2_min
	real error(np1,np2),error_int(np1,np2int)
	real x0(np),y0(np),fast_scale,tlag_scale
	real fastSTR,dfastSTR,fastDIP,dfastDIP
	real spolSTR,dspolSTR,spolDIP,dspolDIP
	character*50 event,ext1,ext2,file1,file2,ext
	character*50 inifile,file_log
	character*50 fstrcat
	external fstrcat

	lu=20
	inifile='split.ini'
	
	print*,'------------------------------------------------------'
	print*,'ENTER EVENT TO RUN ANALYSIS ON:'
	read (*,'(a)'),event
	print*,'=====> processing file: ',event
	ext ='.ilog_nt'
	file_log=fstrcat(event,ext)

c  ** read initialisation paramters from split.ini **
	call zsplit_ini(inifile,lu,
     >	ext1,ext2,fast_scale,tlag_scale,OPT_outfiles)
	
c  ** read in data **
	file1=fstrcat(event,ext1)
	file2=fstrcat(event,ext2)
	call zreaddataSAC(file1,file2,lu,.false.,
     >x0,y0,n,np,b,delta,as,fs,ppick,spick,phi,theta,
     >ev_x,ev_y,ev_z,ev_time)
	
c  ** do splitting analysis **
	call zsplit(x0,y0,n,as,fs,
     >   delta,b,tlag_scale,np1,np2,np2int,
     >   fast,dfast,tlag,dtlag,
     >   spol,dspol,error,error_int,f,
     >   lambda2_min,ndf)

c  ** write output files for gmt plots **
	if (OPT_outfiles) then
	   call zwrite_outfiles(event,lu,tlag_scale,fast_scale,
     >   	x0,y0,n,b,delta,ppick,spick,
     >   	as,fs,
     >   	fast,tlag,spol,error_int)
	endif

c  ** rotate results into the enz frame **
	call zrotabc2enz(phi,theta,fast,dfast,
     >   fastSTR,dfastSTR,fastDIP,dfastDIP)
	call zrotabc2enz(phi,theta,spol,dspol,
     >   spolSTR,dspolSTR,spolDIP,dspolDIP)

c  ** write log file **
	call zwrite_logfile(event,lu,file_log,
     >   ev_x,ev_y,ev_z,ev_time,
     >   as,fs,tlag,dtlag,
     >   fast,dfast,spol,dspol,
     >   fastSTR,dfastSTR,fastDIP,dfastDIP,
     >   spolSTR,dspolSTR,spolDIP,dspolDIP)

c  ** print output message **	   
	print*,'------------------------------------------------------'
	print*,'----: ',event
	print*,'<- s-wave window =',as,' - ',fs,' seconds'
	print*,'<- fast_scale    =',fast_scale,' degrees'
	print*,'<- tlag_scale    =',tlag_scale,' seconds'
	print*,'->'
	print*,'lag      =',tlag,'+/-',dtlag,' seconds'
	print*,' in abc frame'
	print*,'fast     =',fast,'+/-',dfast,' degrees'
	print*,'spol     =',spol,'+/-',dspol,' degrees'
	print*,' in enz frame'
	print*,'fastSTR  =',fastSTR,'+/-',dfastSTR,' degrees'
	print*,'spolSTR  =',spolSTR,'+/-',dspolSTR,' degrees'
	print*,'------------------------------------------------------'

666	continue

	end

