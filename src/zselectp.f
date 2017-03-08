c-----------------------------------------------------------------------
	subroutine zselectp(d1,np,npts,delta,a,f,pwave,npwave)
c-----------------------------------------------------------------------
c
c	select the p-wave based on A and F from sac header
c
c	variables:
c input: d1		dble(np)	trace data (dependant varaible)
c	   np		int		dimensions of array
c	   npts	int		number of data points
c	   delta	dble		spacing (in seconds) of data points
c	   a		dble		start of p-wave
c	   f		dble		end of p-wave
c output:pwave	dble(np)	p-wave data only
c	   npwave	int		number of p-wave data
c
c-----------------------------------------------------------------------
c  modifications:
c	11-07-01	N. Teanby	Original code
c-----------------------------------------------------------------------
	
	implicit none
	integer np,npts,npwave
	double precision d1(np),delta,a,f,pwave(np)
	integer i,indexA,indexF

c  ** initialise **	
	npwave=0
	do 1 i=1,np
	   pwave(i)=0.0
1	continue

c  ** index of start and finish of p-wave **
	indexA=int(a/delta)+1
	indexF=int(f/delta)+2
	
c  ** assign p-wave to data range of d1 **
	do 2 i=indexA,indexF
	   npwave=npwave+1
	   pwave(npwave)=d1(i)
2	continue

	return
	end

