c-----------------------------------------------------------------------
	subroutine zlag(x,y,n,np,lag,iwextra,xlag,ylag,noverlap)
c-----------------------------------------------------------------------
c
c	-LAG MUST BE +VE
c	-adds a lag to the x and y components
c	-positive lag => x has been shifted forward in time by lag points
c	-so y is the fast wave if lag is positive
c	-data from outside the analysis window are used such that the overlap
c	is constant (=analysis window length).
c
c	input data
c	<------analysis window------------>
c	|----------------------------------|-------|
c	1				     n-iwextra     n
c
c	output data
c	<------analysis window------------>
c	|----------------------------------|
c	1                                noverlap
c
c	variables
c	x(np)			real	time series
c	y(np)			real	time series
c	n			int	number of points
c	np			int	array dimension
c	lag			int	lag (x shifted forward wrt y by lag points)
c	iwextra		int	number of extra points included in window
c
c	xlag(np)		real	lagged time series
c	ylag(np)		real	lagged time series
c	noverlap		int	length of lagged time series
c
c-----------------------------------------------------------------------
c	N. Teanby	30-7-02	Original code
c-----------------------------------------------------------------------

	implicit none
	integer n,np,lag,noverlap,noverlap_max,i,iwextra
	real x(np),y(np),xlag(np),ylag(np)

c  ** calc max number of overlapping points **
	noverlap_max = n - iwextra
	noverlap=0

c  ** lag the time series by lag, new time series are noverlap long 
c	and the extra points have been omitted **
	if (lag.eq.0) then
	   do i=1,noverlap_max
	     xlag(i)=x(i)
	     ylag(i)=y(i)
	   enddo
	   noverlap=noverlap_max
	else if (lag.gt.0) then
	   do i=1,noverlap_max
	      if (i+lag.le.n) then
	         xlag(i)=x(i+lag)
	         ylag(i)=y(i)
		   noverlap=noverlap+1
		endif
	   enddo
	else
	   pause 'ERROR: zlag: negative lag not supported'
	endif

	return
	end



