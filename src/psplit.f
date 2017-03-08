c-----------------------------------------------------------------------
	program psplit
c-----------------------------------------------------------------------
c	read in the log files for each event
c	extract wbeg and wend (s-wave window)
c	read in data
c	do splitting
c	write output files for gmt
c-----------------------------------------------------------------------
c	n.teanby	9-9-02	original code
c	n.teanby	12-5-03	update
c-----------------------------------------------------------------------
	
	implicit none
	integer n,np,npc,f,ndf,ierr,lu,np1,np2,np2int
	include "SIZE_np.h"
	include "SIZE_npc.h"
	include "SIZE_np12int.h"
	logical OPT_outfiles,OPT_verbose
	real wbeg_best,wend_best,delta,b,as,fs,ppick,spick
	real fast_best,dfast_best,tlag_best,dtlag_best
	real spol_best,dspol_best,tlag_scale,fast_scale
	real x0(np),y0(np)
	real lambda2_min,phi,theta
	real error(np1,np2),error_int(np1,np2int)
	character*50 event,ext1,ext2,file1,file2
	integer i
c	real fastSTR,dfastSTR,fastDIP,dfastDIP
c	real spolSTR,dspolSTR,spolDIP,dspolDIP
	integer nwbeg,nwend
	integer nmin,max_no_clusters
	real dtlag_max,dfast_max,dt_beg,dt_end
	real t_off_beg,t_off_end
	real ev_x,ev_y,ev_z,ev_time
	character*50 dummy,file_log,ext
	character*50 fstrcat
	external fstrcat

	lu=20
	ext='.ilog.ass'

c  ** read initialisation paramters from ass.ini **
	call zass_ini('ass.ini',lu,ext1,ext2,nwbeg,nwend,
     >dt_beg,dt_end,dtlag_max,dfast_max,t_off_beg,t_off_end,
     >tlag_scale,fast_scale,max_no_clusters,nmin,
     >OPT_verbose,OPT_outfiles)

	open(10,file='event.list',status='old')
	do i=1,np
	   read(10,*,end=999),event
	   file_log=fstrcat(event,ext)
	   open(lu,file=file_log,status='old',iostat=ierr)
	   if (ierr.eq.0) then
c	   ** read wbeg_best and wend_best **
		read(lu,*),dummy
		read(lu,*),dummy,ev_x,ev_y,ev_z,ev_time,wbeg_best,wend_best
		close(lu)

c        ** read in data **
	      file1=fstrcat(event,ext1)
	      file2=fstrcat(event,ext2)
	      call zreaddataSAC(file1,file2,lu,.true.,
     >	   x0,y0,n,np,b,delta,as,fs,ppick,spick,phi,theta,
     >	   ev_x,ev_y,ev_z,ev_time)

c  	   ** do splitting analysis **
	      call zsplit(x0,y0,n,wbeg_best,wend_best,
     >   	   delta,b,tlag_scale,np1,np2,np2int,
     >   	   fast_best,dfast_best,tlag_best,dtlag_best,
     >   	   spol_best,dspol_best,error,error_int,f,
     >   	   lambda2_min,ndf)

c	   ** write output files for gmt plots **
	      call zwrite_outfiles(event,lu,tlag_scale,fast_scale,
     >	   x0,y0,n,b,delta,ppick,spick,
     >	   wbeg_best,wend_best,
     >	   fast_best,tlag_best,spol_best,error_int)

c	   ** print output message **	   
	      print*,'wbeg=',wbeg_best
	      print*,'wend=',wend_best
            print*,'---------------------------------------------------'
	      print*,'----: ',event
	      print*,'lag      =',tlag_best,'+/-',dtlag_best,' s'
	      print*,' in abc frame'
	      print*,'fast     =',fast_best,'+/-',dfast_best,' degrees'
	      print*,'spol     =',spol_best,'+/-',dspol_best,' degrees'
	      print *,'------------------------------------------------'
	   else
	      close(lu)
	   endif
	enddo
999	continue
	close(10)

	end

