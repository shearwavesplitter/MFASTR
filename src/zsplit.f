c-----------------------------------------------------------------------
	subroutine zsplit(x0,y0,n,wbeg,wend,delta,b,tlag_scale,
     >np1,np2,np2int,
     >fast,dfast,tlag,dtlag,spol,dspol,error,error_int,f,
     >lambda2_min,ndf)
c-----------------------------------------------------------------------
c
c	subroutine to perform splitting correction of Silver and Chan 1991
c	on two orthogonal components contained in file1/2.
c
c	this method performs a grid search over a range of fast directions
c	and lag times in order to find the fast direction lag time which 
c	gives the minimum second eigenvalue of the particle motion covaraince
c	matrix
c
c	interpolation in the tlag direction is used to increase resolution
c
c	Method
c	------
c	-x0 and y0 are two orthogonal components to do the analysis on
c	 (ideally orthogonal to ray direction).
c	-The S-wave window, delta, and b are specified in the subroutine call.
c	-Detrend the data (using a least squares fit to datapoints)
c	-Window the data. The data window has 'iwextra' points after the specified
c	 end point. this is so that when lagging is done the number of points
c	 within the window is constant. Hence, data points outside the window are
c	 also used.
c	-Map out the error surface over fast=-90 to 90, 
c	 lag=0-40 delta * itlag_step. The error surface is the
c	 value of lambda2 (smallest eigenvalue from partical motion
c	 covariance matrix).
c	-Interpolate error surface in tlag direction by the factor
c	 f=(np2int-1)/(np1-1).
c	-Grid search to find minimum lambda2 of interpolated error surface.
c	 This point corresponds to the fast direction and tlag which best
c	 correct for the splitting.
c	-Interpolate the time series to same resolution as error surface, so that 
c	 the correct value of lag can be used in the calculation of ndf, and
c	 source polarisation
c	-Window the interpolated time series.
c	-Rotate, lag, then calc covariance matrix, eigenvalues, and source
c	 polarisation
c	-Use source polarisation and fast direction to rotate seismograms into the
c	 noise direction and use noise to calc ndf.
c	-Use ndf and lambda2_min to calc the value of the 95% confidence contour
c	 and normalise interpolated error surface so 1 = 95% confidence interval.
c	 (return  both interpolated and uninterpolated error surfaces because
c	 uninterpolated one is used for stacking).
c
c	Variables
c	---------
c    input:
c	x0(np)/y0(np)	real		two orthogonal seismogram componentstim
c	n			int		number of points in entire time series
c	wbeg/wend		real		S-wave window to use in analysis
c	delta			real		sampling interval, read from SAC header
c	b			real		start time of time series
c	tlag_scale		real		max lag time of error surface
c						(reset to an integer multiple of 
c						(np2-1)*delta, where integer is =>1)
c	np1			int		number of grid points for fast direction
c	np2			int		number of grid points for lag time
c	np2int		int		number of grid points for lag time
c						 (after interp)
c
c    output:
c	fast			real		fast direction (deg clock from N)
c	dfast			real		1.s.d.
c	tlag			real		lag time in seconds
c	dtlag			real		1 s.d.
c	spol			real		s-wave source polarisation(deg clock from N)
c	dspol			real		Maximum Angular Deviation (MAD)
c	error(np1,np2)	real		uninterpolated error surface
c	error_int(np1,np2int)	real	interpolated error surface
c	f			int		interpolation factor=(np2int-1)/(np1-1)
c						 MUST BE and integer for zsplint to work
c	lambda2_min		real		minimum lambda2 (corresponds to solution)
c	ndf			real		no. deg. freedom
c
c    local:
c	np			int		array dimension
c						 (read from SIZE_np.h at compile time)
c	norig			int		original (uninterpolated) number of points
c						 in the shear wave window
c	nwindow		int		number of points in the S-wave window
c						 (including the extra points to allow for 
c						 the lag)
c	noverlap		int		number of overlapping points after lag has 
c						 been applied
c	ninterp		int		number of points after interpolation
c
c	iwbeg/iwend		int		indecices of beginning and end of s-wave
c						 analysis window
c	iwbegx/iwendx	int		indecices of beginning and end of s-wave
c						 total window
c	iwextra		int		number of extra points to include on the
c						 end of the window to allow for lagging
c						 MKS 27 Feb. 2011 changing from 
c               (= max_lag = np2 for uniterpolated case to (iwend-iwbeg))
c
c	x(np)/y(np)		real		time series after trend removal
c	x/ywindow(np)	real		windowed time series
c	x/yrot(np)		real		rotated time series
c	x/ylag(np)		real		lagged time series
c	x/yinterp(np)	real		interpolated time series
c	x/ynoise(np)	real		seismograms roated into noise (polarisation)
c						 direction. xnoise is the noise, ynoise is
c						 the S-wave signal
c	ndf			int		number of degrees of freedom in noise signal
c						 (used to calc 95% confidence level)
c	cov(2,2)		real		covariance matrix of particle motion
c	lambda1/2		real		eignevalues of cov (lambda1 is largest)
c	vec1/2		real		eignevectors of cov (vec1 corresponds to
c						 lambda1)
c	itlag			int		index of lag corresponding to lambda2_min
c	idtlag		real		1s.d. in lag in units of interp grid spacing
c	itlag_step		int		gridding step on error surface
c
c-----------------------------------------------------------------------
c	N. Teanby	5-8-02	original code
c	N. Teanby	12-5-03	itlag_step introduced so that tlag_scale can
c					be independent of number of grid points.
c       M. Savage       27-2-11 changing iwextra, the number of points allowed to overlapc                               so that we don't get most of the energy pushed
c                               out of the window
c-----------------------------------------------------------------------
	
	implicit none
	integer n,np,np1,np2,np2int,ninterp,norig,noverlap,nwindow
	include "SIZE_np.h"
	real wbeg,wend,delta,b,tlag_scale,fast,dfast,tlag,dtlag,spol
	real dspol,error(np1,np2),error_int(np1,np2int),idtlag,idfast
	integer f,iwbeg,iwend,ndf,itlag,ifast,itlag_step
	real x(np),y(np),x0(np),y0(np),xinterp(np),yinterp(np)
	real xnoise(np),ynoise(np)
	real xwindow(np),ywindow(np),xrot(np),yrot(np),xlag(np),ylag(np)
	real lambda1,lambda2,lambda2_min,vec1(2),vec2(2),cov(2,2)
	integer iwextra,iwbegx,iwendx

c  ** calc itlag_step from tlag_scale **
c  ** itlag_step is the grid spacing in tlag for the grid search **
c  ** it must be an integer greater than 1 **
c  ** use itlag_step to redefine tlag_scale **
	itlag_step = max(1 ,  nint( tlag_scale/(real(np2-1)*delta) )   )
c  ** redefine tlag_scale **
c 	PRINT*,tlag_scale,itlag_step,np2,delta	
	tlag_scale = real(itlag_step*(np2-1))*delta
c	PRINT*,tlag_scale	
c  ** calculateinterpolation factor for error surface from np2 and np2int **
c  ** this factor is used to interpolate the error surface in the tlag dirn **
	f = (np2int-1)/(np2-1)
c  ** f needs to be an integer for zsplint to work **
	if (mod(np2int-1,np2-1).ne.0) then
	   pause 'ERROR: zsplit: f not a whole number'
	endif

c  ** calculate the index of the S wave window **
	iwbeg = nint((wbeg-b)/delta)+1
	iwend = nint((wend-b)/delta)+1
c  ** calc the extra to add to end of the window to allow for lagging 
c	iwextra = maximum lag = np2*itlag_step **
c  MKS 27 Feb 2011 changing iwextra
c	iwextra = np2*itlag_step
c	iwextra = int(tlag_scale_orig/2.)
        iwextra = iwend - iwbeg
c        iwextra = (iwend - iwbeg)/2
	iwbegx = iwbeg
	iwendx = iwend + iwextra
	if (iwendx.gt.n) then
	   pause 'ERROR: zsplit: window out of range of data'
	endif
	
c  ** calc number of points originally in the window **
	norig=iwend-iwbeg+1

c  ** detrend the data **
	call zdetrend(x0,n,np,x)
	call zdetrend(y0,n,np,y)

c  ** window the data **
	call zwindow(x,y,n,np,iwbegx,iwendx,xwindow,ywindow,nwindow)

c  ** map out the error surface **
	call zgrid_lambda2(xwindow,ywindow,nwindow,iwextra,itlag_step,
     >				error,np1,np2)

c  ** interpolate error surface in tlag direction **
	call zerror_interp(error,np1,np2,np2int,error_int)

c  ** find the interpolated minimum position **
	call zerror_min(error_int,np1,np2int,ifast,itlag,lambda2_min)

c  ** convert indecies itlag/ifast into tlag and fast **
	tlag  = delta*real(itlag_step*(itlag-1))/real(f)
	fast = -90. + 180.*real(ifast-1)/real(np1-1)
	
c  ** interpolate the time series to be the same or as high resolution 
c	as error surface
c	because lag index is now in terms of interpolated times **
	call zsplint(x,n,f,xinterp,ninterp)
	call zsplint(y,n,f,yinterp,ninterp)

c  ** error surface resolution = delta *  itlag_step / f **
c  ** time series resolution   = delta *  1 / f **
c  ** itlag is in terms of error surface index **
c  ** convert itlag to time series index for use by zlag**
	itlag = itlag * itlag_step

c  ** window the interpolated data **
	iwbegx=f*(iwbegx-1)+1
	iwendx=f*(iwendx-1)+1
	iwextra=f*iwextra
	call zwindow(xinterp,yinterp,ninterp,np,iwbegx,iwendx,
     >xwindow,ywindow,nwindow)

c  ** rotate, lag, calc the covariance and eigenvalues **
	call zrotate2d(xwindow,ywindow,nwindow,np,fast,xrot,yrot)
	call zlag(xrot,yrot,nwindow,np,itlag,iwextra,xlag,ylag,noverlap)
	call zcovariance(xlag,ylag,noverlap,np,cov)
	call zeigen2x2(cov,lambda1,lambda2,vec1,vec2)
	call zsourcepol(fast,lambda1,lambda2,vec1,vec2,spol,dspol)

c  ** calc the number of degrees of freedom **
c  ** first rotate into spol-fast (so y is signal and x is noise) **
	call zrotate2d(xlag,ylag,noverlap,np,spol-fast,xnoise,ynoise)
	call zndf(xnoise,noverlap,norig,ndf)

c  ** normalise error surface and calc errors in fast and lag**
	call zerror95(error_int,np1,np2int,ndf,lambda2_min,idfast,idtlag)
	dtlag = delta * idtlag * itlag_step / real(f)
	dfast = 180.  * idfast / real(np1-1)
	
	return
	
	end
