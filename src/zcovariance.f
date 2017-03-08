c-----------------------------------------------------------------------
	subroutine zcovariance(x,y,n,np,cov)
c-----------------------------------------------------------------------
c
c	calculate the 2x2 covariance matrix of the vectors x and y
c	variables
c	x(np)				real	time series
c	y(np)				real	time series
c	n				int	number of points
c	np				int	array dimension
c	cov(2,2)			real	covariance matrix
c
c-----------------------------------------------------------------------
c	N. Teanby	16-7-02	Original code
c  M. Savage August 25 2012  Modifying to change covariance calculation to
c   be the standard format, as suggested by Richard Arnold.
c-----------------------------------------------------------------------

	implicit none
	integer n,np,i
	real x(np),y(np),cov(2,2),xmean,ymean

c  ** calc cross correlation matrix **
	cov(1,1) = 0.
	cov(2,2) = 0.
	cov(1,2) = 0.
	cov(2,1) = 0.
c  mks 25/8/2012 adding in calculation of mean of x and y
           xmean = 0.
           ymean = 0.
        do 10 i=1,n
           xmean=xmean + x(i)
           ymean=ymean + y(i)
 10     continue
        xmean = xmean / real(n)
        ymean = ymean / real(n)
c   mks 25/8/2012 end of addition for mean
	do 1 i=1,n
c  mks 25/8/2012 changin formula for covariance to include means
c  next 3 lines original Teanby code
c	   cov(1,1) = cov(1,1) + x(i)**2
c	   cov(2,2) = cov(2,2) + y(i)**2
c	   cov(1,2) = cov(1,2) + x(i)*y(i)
c  next 3 lines change to include means
	   cov(1,1) = cov(1,1) + (x(i) - xmean)**2
	   cov(2,2) = cov(2,2) + (y(i) - ymean)**2
	   cov(1,2) = cov(1,2) + (x(i) - xmean)*(y(i) - ymean)
1	continue
	cov(2,1) = cov(1,2)

c  ** normalise **
c  mks 25/8/2012 uncomment the normalization here
	cov(1,1) = cov(1,1)/real(n)
	cov(1,2) = cov(1,2)/real(n)
	cov(2,1) = cov(2,1)/real(n)
	cov(2,2) = cov(2,2)/real(n)

	return
	end



