
c-----------------------------------------------------------------------
	subroutine zlinint(y,n,np,ninterp,yint)
c-----------------------------------------------------------------------
c
c	linear interpolate the series y from n points to ninterp points
c
c	variables
c	input:
c	y(np)				real	series to interpolate
c	n				int	number of points
c	np				int	array dimension
c	ninterp			int	number of points to interpolate to
c
c	yint(np)			real	interpolated series
c
c-----------------------------------------------------------------------
c	N. Teanby	31-7-02	Original code
c     N. Teanby	31-10-03	Bug corrected
c-----------------------------------------------------------------------

	implicit none
	integer i,n,np,ninterp,index1,index2
	real y(np),yint(np)
	real x,dx

c  ** assume spacing of orignal series (y) = 1 **
c  ** spacing of interpolated series is then dx, where **
	dx = (n-1.)/(ninterp-1.)
	
c  ** interpolation **
c  ** set beginning and end points **
	yint(1)=y(1)
      yint(ninterp)=y(n)
c  ** set intermeadiate points **
	do 1 i=2,ninterp-1
c	** calc index of the two points used to do the interpolation **
	   x      = ( i - 1.) * dx
	   index1 = int( x ) + 1
	   index2 = index1   + 2
c	** interpolate **
	   yint(i)=y(index1) + (y(index2)-y(index1))*(x-aint(x))
1	continue

	return
	end
	
