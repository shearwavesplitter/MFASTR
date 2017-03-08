c-----------------------------------------------------------------------
	subroutine zerror95(error,np1,np2,ndf,lambda2_min,ierror,jerror)
c-----------------------------------------------------------------------
c
c	subroutine to calculate normalise contours so that 1=95% confidence
c	contour. Also calc the errorbars on tlag and fast direction
c
c	calculated confidence interval according to appendix in
c	Silver and Chan 1991 using the formula:
c
c	lambda2    <=    1   +   k   f_(k,ndf-k) (1-alpha)
c	-------               -------
c	lambda2_min           ndf - k
c
c	where:
c	k = number of parameters = 2
c	alpha = confindence level = 0.05 for 95% confidence
c	f is the inverse of the F distribution
c	ndf = number of degrees of freedom
c	lambda2 = second eigen value of covaraiance matrix (= elements of error)
c	lambda2_min = minimum lambda2
c
c	calc errorbars on tlag and fast by finding the bounding rectangle of the
c	95% confidence contour.
c
c	variables
c    in:
c	error(np1,np2)		real	array of lambda2 (altered by routine)
c	np1/2				int	array dimension
c	ndf				int	number of degrees of freedom
c	lambda2_min			real	minimum lambda2 (corresponds to solution)
c    out:
c	error(np1,np2)		real	returned normalised array of lambda2
c	ierror			real	extent (half width) of contour in i dirn
c	jerror			real	extent (half width) of contour in j dirn
c    other:
c	lambda2_max			real	lambda2 corresponding to 95% confidence
c
c-----------------------------------------------------------------------
c	N. Teanby	1-8-02	Original code
c-----------------------------------------------------------------------

	implicit none
	integer ndf,np1,np2,npc
	integer i,j,jmin,jmax,irange,jrange
	include "SIZE_npc.h"
	integer line(npc)
	real ierror,jerror
	integer k,k1,irange_min,irange_max,istart,line_test(npc)
	real error(np1,np2),lambda2_min,fftable,lambda_max
	external fftable

c  ** check that npc is beg enough **	
	if (npc.lt.np1) then
	   pause 'ERROR: zerror95: npc < np1'
	endif
	
c  ** calc value of lambda at 95% confidence limit from tabulated values **
	if (ndf.ge.3) then
	   lambda_max = lambda2_min*( 1. + 2.*fftable(ndf-2)/real(ndf-2))
	else
	   print*,'WARNING: zerror95: ndf <=2, set to 3)'
	   lambda_max = lambda2_min*( 1. + 2.*fftable(1))
	endif
	
c  ** normalise errors by lambda_max, so that and error of 1 = 95% confidence **
	do i=1,np1
	   do j=1,np2
	      error(i,j)=error(i,j)/lambda_max
	   enddo
	enddo

c  ** find i/j error (half width of 95% confidence contour) **
c  ** find min and max j, simply search the array **
	jmin=np2
	jmax=1
	do i=1,np1
	   do j=1,np2
	      if (error(i,j).le.1.0) then
		   jmin = min0(jmin,j)
		   jmax = max0(jmax,j)
		endif
	   enddo
	enddo
	jrange=jmax-jmin
c  ** finding min max i is more difficult because of cyclicity of angles **
c  ** sweep a line over all j, set point on line equal to 1 if it falls within the 95% convidence contour for any j. The height of the bounding rectangle is defined by the shortest line which includes all points with a value of line(i)=1. This line is found by searching all line lengths from the minimum = sum_i linr(i) to maximum = np1**
	do i=1,np1
	   line(i)=0
	enddo
	do j=1,np2
	   do i=1,np1
	      if (error(i,j).le.1.0) then
		   line(i)=1
		endif
	   enddo
	enddo
c  ** min line length **
	irange_min = 0
	do i=1,np1
	   irange_min = irange_min + line(i)
	enddo
c  ** max line length **
	irange_max = np1
c  ** search all line length and starting points to find irange **
	do i = irange_min , irange_max
	  do istart = 1,np1
	    do k = 1,np1
	      line_test(k)=0
	    enddo
	    do k = istart,istart+i
	      if (k.gt.np1) then
		   k1 = k - np1
		else
		   k1 = k
		endif
		line_test(k1) = 1
	    enddo
	    do k = 1,np1
	      if ((line(k).eq.1).and.(line_test(k).ne.1)) then
		  goto 1
		endif
	    enddo
	    irange = i
	    goto 11
1	    continue
	  enddo
	enddo
11	continue
	
c  ** one standard deviation = 0.5*95% error half width 
c	(so x full width of 95% contour by 0.25 to get 1s.d.)**
	ierror = 0.25*real(irange)
	jerror = 0.25*real(jrange)
		
	return
	end
