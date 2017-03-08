c-----------------------------------------------------------------------
	subroutine zabs_max(a,n,np,amax)
c-----------------------------------------------------------------------
c
c	subroutine to return the maximum _absolute_ value of array a
c
c    in:
c	a(np)		real		array to be searched
c	n		int		number of data points
c	np		int		array dimension
c    out:
c	amax		real		maximum
c
c-----------------------------------------------------------------------
c	N. Teanby	6-8-02	Original code
c-----------------------------------------------------------------------

	implicit none
	integer n,np,i
	real a(np),amax
	
	amax=0.
	do 1 i=1,n
	   amax = max(amax,abs(a(i)))
1	continue
		
	return
	end
