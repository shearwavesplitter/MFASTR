c-----------------------------------------------------------------------
	subroutine zwindow(x,y,n,np,wbeg,wend,xwindow,ywindow,nwindow)
c-----------------------------------------------------------------------
c
c	window time series x and y between index wbeg and wend
c
c	variables
c	x(np)				real	time series
c	y(np)				real	time series
c	n				int	number of points
c	np				int	array dimension
c	wbeg				int	begin window
c	wend				int	end window
c
c	xwindow(np)			real	windowed time series
c	ywindow(np)			real	windowed time series
c	nwindow			int	length of windowed times series
c
c-----------------------------------------------------------------------
c	N. Teanby	30-7-02	Original code
c-----------------------------------------------------------------------

	implicit none
	integer n,np,i,nwindow,wbeg,wend
	real x(np),y(np),xwindow(np),ywindow(np)

	nwindow = wend - wbeg + 1

c  ** window time series **
	do 1 i=1,nwindow
	   xwindow(i) = x(i+wbeg-1)
	   ywindow(i) = y(i+wbeg-1)
1	continue

	return
	end



