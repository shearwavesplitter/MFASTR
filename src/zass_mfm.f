c-----------------------------------------------------------------------
	subroutine zass(event,ext1,ext2,lu,
     >nwbeg,nwend,dt_beg,dt_end,t_off_beg,t_off_end,
     >dtlag_max,dfast_max,tlag_scale,fast_scale,max_no_clusters,nmin,
     >OPT_verbose,OPT_outfiles,
     >wbeg_best,wend_best,ierr)
c-----------------------------------------------------------------------
c
c   mks 30 May 2008 modifying to print out more information
c   mks 31 march 2009 modifying to print out even more information
c	AUTOMATIC SEISMIC SPLITTING (ASS)
c
c	subroutine to automatically find the optimum shear wave window for
c	use in shear wave splitting.
c
c	the method is based on a grid search of a range of windows. for each 
c	trial window shear wave splitting is performed. the resulting set of  
c	tlag and fast directions is subjected to cluster analysis in order to  
c	find stable regions of the window space. the optimum window is the  
c	one used in the best measurement from the best cluster.
c
c	windowing:
c	||-------|------------||--------|---------------------------||
c	wbeg spick-t_off_beg  spick    spick + t_off_end            wend
c	
c	-The s-wave window is from wbeg to wend (in seconds)
c	-The position of the window is based on the S-wave onset time (spick)
c	which is read from the SAC header (T5)
c	-The analysis is cycled over nwbeg window beginnings and nwend window
c	endings (nwindows=nwbeg*nwend in total). The end of the window is most
c	critical so nwend>nwbeg normally.
c	-steps wbeg by dt_beg, and wend by dt_end seconds
c
c	-splitting analysis is done for each of the windows
c
c	-cluster analysis used to cluster the data, find optimum no. of clusters
c	and the best measurement within the best cluster
c
c	variables
c	---------
c    in:
c	event		char*50	name of event to analyse
c	ext1/2	char*50	extention of the two components to do analysis on
c	lu		int		logical unit to open files on
c	nwbeg		int		number of start positions for S-wave window
c	nwend		int		number of end positions for S-wave window
c	dt_beg	real		increment for start of window (in seconds)
c	dt_end	real		increment for end of window (in seconds)
c	t_off_beg	real		maximum window beginning (relative to spick)
c	t_off_end	real		minimum window end (relative to spick)
c	dtlag_max	real		max allowable error in lag time for inclusion 
c					in clustering
c	dfast_max	real		" fast direction "
c	tlag_scale	real		range of tlag scale in seconds
c	fast_scale	real		range of fast direction scale in degrees
c	max_no_clusters	int	max. number of clusters
c	nmin		int		minimum number of points in an acceptable cluster
c	OPT_verbose	logical	true for verbose output
c	OPT_outfiles logical	true if write outfiles for gmt plots
c    out:
c	wbeg_best	real		optimum start of S-wave window
c	wend_best	real		optimum end of S-wave window
c	ierr		int		=0 if there is an acceptable solution
c					=1 if no acceptable solutions
c    other:
c	x0/y0(np)	real		x and y seismic data
c	n		int		number of data points
c	ppick		real		p-wave pick read from sac header
c	spick		real		s-wave pick read from sac header
c	delta		real		sampling interval (s) read from sac header
c	as		real		beginning of s-wave window (hand pick, not used)
c	fs		real		end of s-wave window (hand pick, not used)
c	phi		real		rotation angle (clock from north) read from header
c	theta		real		rotation angle from vertical read from header
c		phi/theta define the frame relative to the ENZ reference frame
c	fast(npc)	real		fast direction for ith window
c	dfast(npc)	real		s.d. of fast direction for ith window
c	tlag(npc)	real		tlag direction for ith window
c	dtlag(npc)	real		s.d. of tlag direction for ith window
c	grade		character	grading of event
c
c-----------------------------------------------------------------------
c	n.teanby	20-8-02	original code
c-----------------------------------------------------------------------
	
	implicit none
	integer n,np,npc,np1,np2,np2int,f,ndf,nwindows,nwbeg
        integer nwend,ierr,lu
	include "SIZE_np.h"
	include "SIZE_npc.h"
	include "SIZE_np12int.h"
	logical OPT_verbose,OPT_outfiles
	real wbeg_best,wend_best,wbeg(npc),wend(npc)
	real delta,b,as,fs,ppick,spick
	real fast(npc),dfast(npc),tlag(npc),dtlag(npc)
	real fast_best,dfast_best,tlag_best,dtlag_best
	real spol_best,dspol_best
	real spol,dspol,phi,theta,ev_x,ev_y,ev_z,ev_time
	real lambda2_min,tlag_min,fast_min,dtlag_max,dfast_max
	real error(np1,np2),error_int(np1,np2int)
	real x0(np),y0(np),tlag_scale,fast_scale
	real xc0(npc),yc0(npc),vxc0(npc),vyc0(npc)
	real fastSTR,dfastSTR,fastDIP,dfastDIP
	real spolSTR,dspolSTR,spolDIP,dspolDIP
	character*50 event,ext1,ext2,file1,file2
	character*5 grade
	integer i,j,l,ibest,kbest,k
	integer nmin,max_no_clusters
	real dt_beg,dt_end
	integer cluster(npc,npc)
	real t_off_beg,t_off_end
	character*50 file_clustxy,file_clusters,file_soln,file_log,ext
	character*50 fstrcat,file_clusternew,file_lognew
	external fstrcat
        integer nc(npc)
        real var_overall(npc),tdifmax,phidifmax
        print *,'In program zass_mfm'
	ext='.clustxy'
	file_clustxy   = fstrcat(event,ext)
	ext='.clusters'
	file_clusters  = fstrcat(event,ext)
	ext='.clustnew'
	file_clusternew  = fstrcat(event,ext)
	ext='.soln'
	file_soln      = fstrcat(event,ext)
	ext='.ilog.ass'
	file_log       = fstrcat(event,ext)
	ext='.ilognew.ass'
	file_lognew       = fstrcat(event,ext)
	
c  ** calc the grid spacing **
	tlag_min = tlag_scale/real(np2int)
	fast_min = fast_scale/real(np1)
	
c  ** read in data **
	file1=fstrcat(event,ext1)
	file2=fstrcat(event,ext2)
	call zreaddataSAC(file1,file2,lu,.true.,
     >x0,y0,n,np,b,delta,as,fs,ppick,spick,phi,theta,
     >ev_x,ev_y,ev_z,ev_time)

c  ** calc number of windows and check it's not too many **
	nwindows=nwend*nwbeg
	print*,'no. windows to cycle = ',nwindows
	if (nwindows.gt.npc) then
	   pause 'ERROR: zass: number of windows to search is too large'
	endif
c  ** loop over window end points **
	do i=1,nwend
	   do j=1,nwbeg
	      l = (i-1)*nwbeg + j
	      wbeg(l) = spick - real(j-1)*dt_beg - t_off_beg
	      wend(l) = spick + real(i-1)*dt_end + t_off_end
	      call zsplit(x0,y0,n,wbeg(l),wend(l),delta,b,tlag_scale,
     >np1,np2,np2int,
     >fast(l),dfast(l),tlag(l),dtlag(l),spol,dspol,error,error_int,f,
     >lambda2_min,ndf)
	 if (OPT_verbose) then
	 write(*,99) l,wbeg(l),wend(l),tlag(l),dtlag(l),fast(l),dfast(l)
99	   format(i3,2x,f8.3,' -',f8.3,'s    tlag =',f8.5,' +/-',f8.5,
     >'    fast =',f8.3,' +/-',f8.3)
	 endif
	   enddo
	enddo

        print *," calling zpackresults. num data, limits*", 
     >     nwindows, dtlag_max, dfast_max
	call zpackresults(wbeg,wend,tlag,dtlag,fast,dfast,nwindows,npc,
     >dtlag_max,dfast_max)
        print *," back from zpackresults. new_num data ", nwindows 

c  ** do cluster analysis and find number of clusters **
	call zcluster(tlag,dtlag,fast,dfast,nwindows,
     >	tlag_scale,fast_scale,tlag_min,fast_min,max_no_clusters,
     >	xc0,yc0,vxc0,vyc0,cluster,k)

c  ** find best cluster **
c  mks 30 may 2008 call new cluster routine
c	call zselect_cluster(dtlag,dfast,vxc0,vyc0,nwindows,
c     >	tlag_scale,fast_scale,cluster,nmin,k,
c     >	kbest)
	call zselect_cluster_mfm(dtlag,dfast,vxc0,vyc0,nwindows,
     >	tlag_scale,fast_scale,cluster,nmin,k,
     >	kbest,nc,var_overall)

c  ** find best measurement **
	call zselect_measurement(dtlag,dfast,nwindows,
     >	wbeg,wend,delta,spick,tlag_scale,fast_scale,cluster,k,kbest,
     >	ibest)

c  ** grade measurement
        tdifmax = tlag_scale/4
	phidifmax = 45
        print *,'About to call zgrade_mfm kbest, tdifmax,phidifmax are'
        print *, kbest,tdifmax,phidifmax
	call zgrade_mfm(xc0,yc0,k,kbest,nmin,nc,var_overall,tdifmax,
     >    phidifmax,grade)
        print *," back from zgrade_mfm. Grade is ",grade


c  ** write out measurements to file **
	call zwrite_clustxy(lu,file_clustxy,nwindows,npc,
     >	wbeg,wend,fast,dfast,tlag,dtlag)

c  ** write out clusters to file **
	call zwrite_clusters(lu,file_clusters,k,npc,
     >	xc0,yc0,vxc0,vyc0)
	call zwrite_clusternew(lu,file_clusternew,k,npc,
     >	xc0,yc0,vxc0,vyc0,nc,var_overall)

c  ** do splitting and write log file **	
	if (kbest.eq.0) then
	   ierr = 1
	   wbeg_best=0.
	   wend_best=0.
	   print*,'NO CLUSTERS MEET THE CRITERIA'
	   print*,'NO SOLUTION FOUND FOR: ',event
	else
	   ierr=0
	   wbeg_best=wbeg(ibest)
	   wend_best=wend(ibest)
c  	** do splitting analysis **
	   call zsplit(x0,y0,n,wbeg_best,wend_best,
     >   	delta,b,tlag_scale,np1,np2,np2int,
     >   	fast_best,dfast_best,tlag_best,dtlag_best,
     >   	spol_best,dspol_best,error,error_int,f,
     >   	lambda2_min,ndf)
c	** write output files for gmt plots **
	   if (OPT_outfiles) then
	      call zwrite_outfiles(event,lu,tlag_scale,fast_scale,
     >	   x0,y0,n,b,delta,ppick,spick,
     >	   wbeg_best,wend_best,
     >	   fast_best,tlag_best,spol_best,error_int)
	   endif

c  	** write solution for gmt plot **
	   open(lu,file=file_soln,status='unknown')
	   write(lu,*) ibest,tlag_best,fast_best
	   close(lu)

c	** rotate results into the enz frame **
	   call zrotabc2enz(phi,theta,fast_best,dfast_best,
     >	fastSTR,dfastSTR,fastDIP,dfastDIP)
	   call zrotabc2enz(phi,theta,spol_best,dspol_best,
     >	spolSTR,dspolSTR,spolDIP,dspolDIP)

c	** write log file **
	   call zwrite_logfile(event,lu,file_log,
     >	ev_x,ev_y,ev_z,ev_time,
     >	wbeg_best,wend_best,tlag_best,dtlag_best,
     >	fast_best,dfast_best,spol_best,dspol_best,
     >	fastSTR,dfastSTR,fastDIP,dfastDIP,
     >	spolSTR,dspolSTR,spolDIP,dspolDIP)
	   call zwrite_logmfm(event,lu,file_lognew,
     >	ev_x,ev_y,ev_z,ev_time,
     >	wbeg_best,wend_best,tlag_best,dtlag_best,
     >	fast_best,dfast_best,spol_best,dspol_best,
     >	fastSTR,dfastSTR,fastDIP,dfastDIP,
     >	spolSTR,dspolSTR,spolDIP,dspolDIP,grade,ndf,lambda2_min)

c	** print output message **	   
	  print*,'------------------------------------------------------'
	  print*,'----: ',event
	  print*,'<- s-wave winw =',wbeg_best,' - ',wend_best,' seconds'
	  print*,'<- fast_scale    =',fast_scale,' degrees'
	  print*,'<- tlag_scale    =',tlag_scale,' seconds'
	  print*,'->'
	  print*,'lag      =',tlag_best,'+/-',dtlag_best,' seconds'
	  print*,' in abc frame'
	  print*,'fast     =',fast_best,'+/-',dfast_best,' degrees'
	  print*,'spol     =',spol_best,'+/-',dspol_best,' degrees'
	  print*,' in enz frame'
	  print*,'fastSTR  =',fastSTR,'+/-',dfastSTR,' degrees'
	  print*,'spolSTR  =',spolSTR,'+/-',dspolSTR,' degrees'
	  print*,'------------------------------------------------------'
	endif

	return
	end

c-----------------------------------------------------------------------
	subroutine zwrite_clustxy(lu,file_clustxy,n,np,
     >	wbeg,wend,fast,dfast,tlag,dtlag)
c-----------------------------------------------------------------------
c	write out measurements to file
c-----------------------------------------------------------------------
c	n.teanby	9-9-02	original code
c-----------------------------------------------------------------------

	implicit none
	integer lu,n,np,i
	real wbeg(np),wend(np),fast(np),dfast(np),tlag(np),dtlag(np)
	character*50 file_clustxy

c  ** write file **
	open(lu,file=file_clustxy,status='unknown')
	do i=1,n
	 write(lu,1) i,wbeg(i),wend(i),fast(i),dfast(i),tlag(i),dtlag(i)
	enddo
	close(lu)
1	format(i6,6(x,e15.5))	
	return
	end

c-----------------------------------------------------------------------
	subroutine zwrite_clusters(lu,file_clusters,n,np,
     >	xc0,yc0,vxc0,vyc0)
c-----------------------------------------------------------------------
c	write out clusters to file
c-----------------------------------------------------------------------
c	n.teanby	9-9-02	original code
c-----------------------------------------------------------------------

	implicit none
	integer lu,n,np,i
	real xc0(np),yc0(np),vxc0(np),vyc0(np)
	character*50 file_clusters

c  ** write file **
	open(lu,file=file_clusters,status='unknown')
	do i=1,n
	   write(lu,*) xc0(i),yc0(i),sqrt(vxc0(i)),sqrt(vyc0(i))
	enddo
	close(lu)
	
	return
	end

c-----------------------------------------------------------------------
	subroutine zwrite_clusternew(lu,file_clusters,n,np,
     >	xc0,yc0,vxc0,vyc0,nc,var_overall)
c-----------------------------------------------------------------------
c	write out clusters to file
c-----------------------------------------------------------------------
c	n.teanby	9-9-02	original code
c  mks 30 may 2008 making to write out a new file
c-----------------------------------------------------------------------

	implicit none
	integer lu,n,np,i,nc(np)
	real xc0(np),yc0(np),vxc0(np),vyc0(np),var_overall(np)
	character*50 file_clusters

c  ** write file **
	open(lu,file=file_clusters,status='unknown')
	do i=1,n
	   write(lu,*) xc0(i),yc0(i),sqrt(vxc0(i)),sqrt(vyc0(i)),nc(i),
     >     var_overall(i)
	enddo
	close(lu)
	
	return
	end

