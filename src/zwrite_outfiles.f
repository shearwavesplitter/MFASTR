c-----------------------------------------------------------------------
	subroutine zwrite_outfiles(event,lu,tlag_scale,fast_scale,
     >x0,y0,n,b,delta,ppick,spick,
     >wbeg,wend,
     >fast,tlag,spol,error_int)
c-----------------------------------------------------------------------
c	write output files 
c-----------------------------------------------------------------------
c	N. Teanby	6-8-02	original code
c-----------------------------------------------------------------------
	
	implicit none
	integer n,np,np1,np2,np2int,ninterp,noverlap,noverlap_resamp
        integer nw,lu
	include "SIZE_np.h"
	include "SIZE_np12int.h"
	real wbeg,wend,delta,b,fast,tlag,spol,ppick,spick
	real error_int(np1,np2int),tlag_scale,fast_scale
c  ** 
	character*50 event
	integer f,iwbeg,iwend,ilag,itlag_step
	real x(np),y(np),x0(np),y0(np)
	real xinterp(np),yinterp(np),xw(np),yw(np)
	real xrot(np),yrot(np),xlag(np),ylag(np)
	real time(np),timew(np),sto(np),sro(np),stc(np),src(np)
	real stc_resamp(np),src_resamp(np)
	real ssw(np),sfw(np),sscw(np),sfcw(np)
	real sswn(np),sfwn(np),sscwn(np),sfcwn(np)
	real xwn(np),ywn(np),xcw(np),ycw(np),xcwn(np),ycwn(np)
c  ** filenames **
	character*50 file_sto,file_sro,file_stc,file_src,file_ss,file_sf
	character*50 file_ssc,file_sfc,file_pm,file_pmc,file_error
	character*50 file_gmt
c  ** other **
	real test_tlag,test_fast,norm_pm,cov(2,2),ftick
	real max_xw,max_yw,max_xcw,max_ycw
	real max_ssw,max_sfw,max_sscw,max_sfcw
	real max_sto,max_sro,max_stc,max_src,max_rt
	character*50 fmt2
	character*50 fstrcat,ext
	external fstrcat
	integer i,j

c  ** set filenames **
	ext='.sto'
	file_sto   = fstrcat(event,ext)
	ext='.sro'
	file_sro   = fstrcat(event,ext)
	ext='.stc'
	file_stc   = fstrcat(event,ext)
	ext='.src'
	file_src   = fstrcat(event,ext)
	ext='.ss'
	file_ss    = fstrcat(event,ext)
	ext='.sf'
	file_sf    = fstrcat(event,ext)
	ext='.ssc'
	file_ssc   = fstrcat(event,ext)
	ext='.sfc'
	file_sfc   = fstrcat(event,ext)
	ext='.pm'
	file_pm    = fstrcat(event,ext)
	ext='.pmc'
	file_pmc   = fstrcat(event,ext)
	ext='.error'
	file_error = fstrcat(event,ext)
	ext='.gmt'
	file_gmt   = fstrcat(event,ext)


c  ** format for 2 column output **
	fmt2='(e15.7,2x,e15.7)'

c--------------------- DATA ---------------------
c  ** calculateinterpolation factor for error surface from np2 and np2int **
c  ** this factor is used to interpolate the error surface in the tlag dirn **
	f = (np2int-1)/(np2-1)
c  ** f needs to be an integer for zsplint to work **
	if (mod(np2int-1,np2-1).ne.0) then
	   pause 'ERROR: zwrite_outfiles: f not a whole number'
	endif
c  ** calc itlag_step from tlag_scale **
c  ** itlag_step is the grid spacing in tlag for the grid search **
c  ** it must be an integer greater than 1 **
	itlag_step = nint( tlag_scale/(real(np2-1)*delta) )
	
c  ** detrend the data **
	call zdetrend(x0,n,np,x)
	call zdetrend(y0,n,np,y)
c  ** set up an array with the times in **
	do i=1,n
	   time(i) = b + real(i-1)*delta
	enddo
c  ** interpolate the time series by the factor f **
	call zsplint(x,n,f,xinterp,ninterp)
	call zsplint(y,n,f,yinterp,ninterp)

c------------------- OUTFILES -------------------
c*****UNCORRECTED RADIAL AND TRANSVERSE **
c  ** rotate into the radial and transverse directions **
c  ** RADIAL     [sro] = parallel to the source polarisation (spol) **
c  ** TRANSVERSE [sto] = perpendicular to the source polarisation (spol) **
	call zrotate2d(x,y,n,np,spol,sto,sro)
	call zwrite2(file_sto,lu,fmt2,time,sto,n,np)
	call zwrite2(file_sro,lu,fmt2,time,sro,n,np)

c*****CORRECTED RADIAL AND TRANSVERSE **
c  ** rotate into fast and slow direction and apply lag, then rotate
c	into radial and transverse directions **
c  ** because tlag may not be an integer number of samples, interpolation
c	is required **
c  ** calc the lag in terms of the interpolated index **
	ilag = 1 + nint(tlag*real(f)/delta)
c  ** rotate and lag (positive lag means that both time series still begin
c	at b, however the time series will be tlag shorter than the 
c	uncorrected ones**
	call zrotate2d(xinterp,yinterp,ninterp,np,fast,xrot,yrot)
	call zlag(xrot,yrot,ninterp,np,ilag,0,xlag,ylag,noverlap)
c  ** rotate into spol dirn (radial and transverse) **
	call zrotate2d(xlag,ylag,noverlap,np,spol-fast,stc,src)
c  ** note these time series are interpolated by the factor f **
	call zresample(stc,noverlap,np,f,stc_resamp,noverlap_resamp)
	call zresample(src,noverlap,np,f,src_resamp,noverlap_resamp)
	call zwrite2(file_stc,lu,fmt2,time,stc_resamp,noverlap_resamp,np)
	call zwrite2(file_src,lu,fmt2,time,src_resamp,noverlap_resamp,np)
c  ** find maximum of r/t comps for the perposes of plotting the graphs **
	call zabs_max(sto,noverlap,np,max_sto)
	call zabs_max(sro,noverlap,np,max_sro)
	call zabs_max(stc,noverlap,np,max_stc)
	call zabs_max(src,noverlap,np,max_src)
	max_rt=max(max_sto,max_sro,max_stc,max_src)
        print *," max comps ",max_sto,max_sro,max_stc,max_src,max_rt

c*****FAST AND SLOW COMPONENTS (WINDOWED)**
c  ** window the interpolated data and set up windowed time matrix **
	iwbeg = nint((wbeg-b)/delta)+1
	iwend = nint((wend-b)/delta)+1
	iwbeg=f*(iwbeg-1) + 1
	iwend=f*(iwend-1) + 1 + f*np2*itlag_step
	call zwindow(xinterp,yinterp,ninterp,np,iwbeg,iwend,xw,yw,nw)
	do i=1,nw
	   timew(i) = wbeg + delta*real(i-1)/real(f)
	enddo
c*****CALC UNCORRECTED FAST AND SLOW COMPONENTS **
	call zrotate2d(xw,yw,nw,np,fast,ssw,sfw)
c*****CALC CORRECTED FAST AND SLOW COMPONENT**
	call zlag(ssw,sfw,nw,np,ilag,np2int,sscw,sfcw,noverlap)

c  ** WRITE OUT PARTICLE MOTION **
c  ** rotate corrected fast and slow waves back to original coords **
	call zrotate2d(sscw,sfcw,noverlap,np,-fast,xcw,ycw)
c  ** the points we required are numbered 1-noverlap ** 
c  ** find normalising factor for the particle motion and normalise to 
c	a maximum value of 1**
	call zabs_max(xw,noverlap,np,max_xw)
	call zabs_max(yw,noverlap,np,max_yw)
	call zabs_max(xcw,noverlap,np,max_xcw)
	call zabs_max(ycw,noverlap,np,max_ycw)
	norm_pm=1./max(max_xw,max_yw,max_xcw,max_ycw)
	call zmultiply(xw,noverlap,np,norm_pm,xwn)
	call zmultiply(yw,noverlap,np,norm_pm,ywn)
	call zmultiply(xcw,noverlap,np,norm_pm,xcwn)
	call zmultiply(ycw,noverlap,np,norm_pm,ycwn)
c*****UNCORRECTED PARTICLE MOTION IN S-WAVE WINDOW **
c  ** particle motion is composed of xwn,ywn in window wbeg-wend **
	call zwrite2(file_pm,lu,fmt2,xwn,ywn,noverlap,np)
c*****CORRECTED PARTICLE MOTION IN S-WAVE WINDOW **
c  ** particle motion is composed of xcwnw,ycwn in window wbeg-wend **
	call zwrite2(file_pmc,lu,fmt2,xcwn,ycwn,noverlap,np)
	
c  ** WRITE OUT FAST AND SLOW WAVEFORMS **
c  ** normalise the S-waves **
	call zabs_max(ssw,noverlap,np,max_ssw)
	call zabs_max(sfw,noverlap,np,max_sfw)
	call zabs_max(sscw,noverlap,np,max_sscw)
	call zabs_max(sfcw,noverlap,np,max_sfcw)
	call zmultiply(sfw,noverlap,np,1./max_sfw,sfwn)
	call zmultiply(sfcw,noverlap,np,1./max_sfcw,sfcwn)
	call zcovariance(sscw,sfcw,noverlap,np,cov)
c  ** if cross correlation is negative then flip the slow wave so that
c	the match of the waveforms is more obvious **
	if (cov(1,2).ge.0.) then
	   call zmultiply(ssw,noverlap,np,1./max_ssw,sswn)
	   call zmultiply(sscw,noverlap,np,1./max_sscw,sscwn)
	else
	   call zmultiply(ssw,noverlap,np,-1./max_ssw,sswn)
	   call zmultiply(sscw,noverlap,np,-1./max_sscw,sscwn)
	endif	
c*****UNCORRECTED FAST AND SLOW COMPONENTS **
	call zwrite2(file_ss,lu,fmt2,timew,sswn,noverlap,np)
	call zwrite2(file_sf,lu,fmt2,timew,sfwn,noverlap,np)
c*****CORRECTED FAST AND SLOW COMPONENTS **
	call zwrite2(file_ssc,lu,fmt2,timew,sscwn,noverlap,np)
	call zwrite2(file_sfc,lu,fmt2,timew,sfcwn,noverlap,np)

c*****INTERPOLATED ERROR SURFACE **
	open(lu,file=file_error)
	do i=1,np1
	   do j=1,np2int
	     test_tlag = delta*real((j-1)*itlag_step)/real(f)
	     test_fast = -90. + 180.*real(i-1)/real(np1-1)
	     write(lu,*) test_tlag,test_fast,error_int(i,j)
	   enddo
	enddo
	close(lu)

c--------- PLOTTING PARAMETERS REQUIRED BY GMT --------
	open(lu,file=file_gmt)
	write(lu,1000) 'ppick ',ppick
	write(lu,1000) 'spick ',spick
	write(lu,1000) 'wbeg ',wbeg
	write(lu,1000) 'wend ',wend
	write(lu,1000) 'waveform_minortick ',ftick((wend-wbeg)/4.)
	write(lu,1000) 'waveform_majortick ',ftick(wend-wbeg)
	write(lu,1000) 'tlag_scale ',tlag_scale
	write(lu,1000) 'tlag_minortick ',ftick(tlag_scale/4.)
	write(lu,1000) 'tlag_majortick ',ftick(tlag_scale)
	write(lu,1000) 'fast_scale ',fast_scale
	write(lu,1000) 'delta ',delta
	write(lu,1000) 'error_grid_tlag_int ',itlag_step*delta/real(f)
	write(lu,1000) 'rt_max_y ',max_rt
	write(lu,1000) 'rt_minortick_y ',ftick(max_rt/4.)
	write(lu,1000) 'rt_majortick_y ',ftick(max_rt)
	write(lu,1000) 'rt_b_x ',b
	write(lu,1000) 'rt_e_x ',b + real(n-1)*delta
	write(lu,1000) 'rt_minortick_x ',ftick((real(n-1)*delta)/4.)
	write(lu,1000) 'rt_majortick_x ',ftick(real(n-1)*delta)
	close(lu)

1000	format(a25,1x,e15.5)
	return
	end

c-----------------------------------------------------------------------
	function ftick(range)
c-----------------------------------------------------------------------
	implicit none
	real range,ftick
	if (range.le.0.0078125) then
	  ftick=0.001
	else if (range.le.0.015625) then
	  ftick=0.0025
	else if (range.le.0.03125) then
	  ftick=0.005
	else if (range.le.0.0625) then
	  ftick=0.01
	else if (range.le.0.125) then
	  ftick=0.025
	else if (range.le.0.25) then
	  ftick=0.05
	else if (range.le.0.5) then
	  ftick=0.1
	else if (range.le.1.0) then
	  ftick=0.2
	else if (range.le.2.0) then
	  ftick=0.5
	else if (range.le.4.0) then
	  ftick=1.
	else if (range.le.8.0) then
	  ftick=2.
	else if (range.le.16.0) then
	  ftick=5.
	else if (range.le.32.0) then
	  ftick=10.
	else if (range.le.64.0) then
	  ftick=20.
	else if (range.le.128.0) then
	  ftick=50.
	else if (range.le.256.0) then
	  ftick=50.
	else if (range.le.512.0) then
	  ftick=100.
	else if (range.le.1024.0) then
	  ftick=200.
	else if (range.le.2048.0) then
	  ftick=500.
	else if (range.le.4096.0) then
	  ftick=1000.
	else if (range.le.8192.0) then
	  ftick=2000.
	else if (range.le.16384.0) then
	  ftick=5000.
	else if (range.le.32768.0) then
	  ftick=10000.
	else
	  ftick=range
	endif
	return
	end
	
