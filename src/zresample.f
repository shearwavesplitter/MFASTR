c-----------------------------------------------------------------------
	subroutine zresample(a,n,np,f,ar,nr)
c-----------------------------------------------------------------------
c
c	subroutine to reduce the sampling of 'a' by a factor of f
c
c    in:
c	a(np)		real		array to be resampled
c	n		int		number of data points
c	np		int		array dimension
c	f		int		resampling factor
c    out:
c	ar(np)	real		resampled array
c	nr		int		number of resampled points
c
c-----------------------------------------------------------------------
c	N. Teanby	6-8-02	Original code
c-----------------------------------------------------------------------

	implicit none
	integer n,np,i,nr,f
	real a(np),ar(np)

	if (f.lt.1) then
	   pause 'ERROR: zresample: resampling factor f.lt.1'
	endif

c  ** number of resampled points **
	nr = int(real(n-1)/real(f))+1
        print *, 'npts before & after resample: ', np, nr
	
	do 1 i=1,nr
	   ar(i) = a(1+(i-1)*f)
1	continue
		
	return
	end
