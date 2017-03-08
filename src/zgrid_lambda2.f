c-----------------------------------------------------------------------
	subroutine zgrid_lambda2(x,y,n,iwextra,itlag_step,
     >					lambda2grid,np1,np2)
c-----------------------------------------------------------------------
c
c	calculate the second eigenvalue of the particle motion covaraince 
c	matrix over fast direction from -90 - 90 deg (1deg grid spacing) 
c	and lags of 0 to 40 (1 sample point grid spacing).
c
c	variables
c    in:
c	x(np)			real		time series (local east component)
c	y(np)			real		time series (local north component)
c	n			int		number of points
c	iwextra		int		number of extra points included in window
c						to allow for the lagging
c	itlag_step		int		gridding step in for lag time
c	[np			int		array dimension
c					(read from SIZE_np.h at compile time)]
c	np1/2			int		array dimension
c    out:
c	lambda2grid(np1,np2)	real	gridded value of lambda2
c    local:
c	fast			real		fast direction in degrees
c	lag			int		lag time in sample spacings
c
c
c-----------------------------------------------------------------------
c	N. Teanby	4-8-02	Original code
c-----------------------------------------------------------------------

	implicit none
	integer n,np,np1,np2,i,j,noverlap
	include "SIZE_np.h"
	real x(np),y(np),lambda2grid(np1,np2)
	real fast
	integer lag,iwextra,itlag_step
	real cov(2,2),lambda1,lambda2,vec1(2),vec2(2)
	real xrot(np),yrot(np),xlag(np),ylag(np)

c  ** map out the lambda2 surface **
	do 1 i=1,np1
	   do 2 j = 1,np2
c	   ** set fast direction (range is -90 to 90deg) **
	      fast = -90. + 180.*real(i-1)/real(np1-1)
c	   ** set lag **
		lag = (j - 1)*itlag_step
c	   ** rotate, lag, calc the covariance and eigenvalues **
		call zrotate2d(x,y,n,np,fast,xrot,yrot)
		call zlag(xrot,yrot,n,np,lag,iwextra,xlag,ylag,noverlap)
		call zcovariance(xlag,ylag,noverlap,np,cov)
		call zeigen2x2(cov,lambda1,lambda2,vec1,vec2)
		lambda2grid(i,j)=lambda2
2	   continue
1	continue
	return
	end
