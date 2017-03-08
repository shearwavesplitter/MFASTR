c-----------------------------------------------------------------------
	subroutine zrotate2d(x,y,n,np,angle,xrot,yrot)
c-----------------------------------------------------------------------
c
c	rotate a 2 seismogram (x and y comps) by angle.
c	angle is in degrees measured clockwise
c
c	variables
c	x(np)				real	time series
c	y(np)				real	time series
c	n				int	number of points
c	np				int	array dimension
c	angle				real	rotation angle (clockwise from North (y))
c
c	xrot(np)			real	rotated time series
c	yrot(np)			real	rotated time series
c
c-----------------------------------------------------------------------
c	N. Teanby	30-7-02	Original code
c-----------------------------------------------------------------------

	implicit none
	integer n,np,i
	real pi
	parameter (pi=3.141592654)
	real x(np),y(np),xrot(np),yrot(np)
	real angle,angler

c  ** convert to radians **
	angler=angle*pi/180.

c  ** rotate time series x and y by angle **
	do 1 i=1,n
	   xrot(i) = x(i)*cos(angler) - y(i)*sin(angler)
	   yrot(i) = x(i)*sin(angler) + y(i)*cos(angler)
1	continue

	return
	end



