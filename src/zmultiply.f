c-----------------------------------------------------------------------
	subroutine zmultiply(a,n,np,scalar,axscalar)
c-----------------------------------------------------------------------
c
c	subroutine to multiply each element of 'a' by 'scalar' to
c	give 'axscalar'
c
c    in:
c	a(np)		real		array to be multiplied
c	n		int		number of data points
c	np		int		array dimension#
c	scalar	real		multiplication factor
c    out:
c	axscalar(np)real		scaled array
c
c-----------------------------------------------------------------------
c	N. Teanby	4-8-02	Original code
c-----------------------------------------------------------------------

	implicit none
	integer n,np,i
	real a(np),scalar,axscalar(np)
	
	do 1 i=1,n
	   axscalar(i) = a(i) * scalar
1	continue
		
	return
	end
