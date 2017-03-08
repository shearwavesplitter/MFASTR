c-----------------------------------------------------------------------
	subroutine zdetrend(y,n,np,ydetrend)
c-----------------------------------------------------------------------
c
c	subroutine to remove trend from a dataset using a least squares
c	line of best fit (see squires p38 for theory)
c
c	variables
c    in:
c	y(np)		real		series to detrend
c	n		int		number of data points
c	np		int		array dimension
c    out:
c	ydetrend(np)real		detrended dataset
c    local:
c	m		real		grad of line of best fit
c	c		real		intercept of line of best fit
c
c-----------------------------------------------------------------------
c	N. Teanby	5-8-02	Original code
c-----------------------------------------------------------------------

	implicit none
	integer n,np,i
	real y(np),ydetrend(np)
	real xmean,ymean,sumy,sum1,sum2,trend,m,c,TINY
	parameter (TINY=1.e-10)
	
	sum1=0.
	sum2=0.
	sumy=0.
	
c  ** calc mean of x, which goes from 1 to n, so mean is **
	xmean=real(n+1)/2.
c  ** calc the mean of y **
	do 1 i=1,n
	   sumy = sumy + y(i)
1	continue
	ymean=sumy/real(n)
	
c  ** calc grad of line according to formulat in squires **
	do 2 i=1,n
	   sum1 = sum1 + (real(i)-xmean)*y(i)
	   sum2 = sum2 + (real(i)-xmean)**2	
2	continue

c  ** calc grad and intercept **
	if (sum1.lt.TINY) then
	   m = 0.
	   c = ymean
	else
	  m = sum1/sum2
	  c = ymean - m*xmean
	endif
	
c  ** dtrend the data **
	do 3 i=1,n
	   trend = m*real(i) + c
	   ydetrend(i) = y(i) - trend
3	continue


	return
	end
