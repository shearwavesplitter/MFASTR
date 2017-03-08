c-----------------------------------------------------------------------
	subroutine zerror_min(error,np1,np2,ifast,itlag,lambda2_min)
c-----------------------------------------------------------------------
c
c	grid search for the minimum value of lambda2 on the interpolated
c	error surface
c
c	variables
c    in:
c	error(np1,np2)	real	error surface (i.e. lambda2)
c	np1/2			int	array dimensions
c    out:
c	lambda2_min		real	minimum value of lambda2
c	itlag			int	index of lag corresponding to lambda2_min
c	ifast			int	index of fast dirn corresponding to lambda2_min
c
c-----------------------------------------------------------------------
c	N. Teanby	4-8-02	Original code
c-----------------------------------------------------------------------

	implicit none
	integer np1,np2
	real error(np1,np2),lambda2_min
	integer i,j,itlag,ifast
	
c  ** find the minimum lambda2 position **
	lambda2_min=error(1,1)
	itlag=1
	ifast=1
	do 1 i=1,np1
	  do 2 j = 1,np2
	     if (error(i,j).lt.lambda2_min) then
	        lambda2_min = error(i,j)
		  itlag = j
		  ifast= i
	     endif
2	   continue
1	continue	

	return
	end
