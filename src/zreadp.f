c-----------------------------------------------------------------------
	subroutine zreadp(file,lu,np,
     >	d1,npts,pwave,npwave,a,f,delta)
c-----------------------------------------------------------------------
c
c	variables:
c input: file	char*50	file to read
c	   lu		int		logical unit to open file on
c	   np		int		dimension of array
c output:d1		dble(np)	all data
c	   npts	int		number of data
c	   pwave	dble(np)	pwave component
c	   npwave	int		number of pwave data
c	   a		dble		start of pwave
c	   f		dble		end of pwave
c	   delta	dble		time spacing
c
c-----------------------------------------------------------------------
c  modifications:
c	11-07-01	N. Teanby	Original code
c-----------------------------------------------------------------------
	
	implicit none
	integer np,lu
	character file*50
c  ** header **
	double precision h1(14,5)
	integer h2(8,5)
	character h3a*8, h3b*16
	character*8 h4(7,3)
c  ** all data **
	double precision d1(np)
	integer npts
c  ** pwave data **
	double precision pwave(np)
	integer npwave
c  ** header varaibles that are used get a label **
	double precision a,f,delta
c  ** zgethdr **
	character c16*16

c  ** read in data file **
	call zreadsac(file,lu,np,h1,h2,h3a,h3b,h4,d1,npts)

c  ** get a, f, and delta from header arrays **
c  **
	call zgethdr('delta ',h1,h2,h3a,h3b,h4,c16)
	read(c16,*) delta
c  ** start of p-wave stored in T0 **
	call zgethdr('t0 ',h1,h2,h3a,h3b,h4,c16)
	read(c16,*) a
c  ** end of p-wave stored in T1 **
	call zgethdr('t1 ',h1,h2,h3a,h3b,h4,c16)
	read(c16,*) f

C  ** check that p-wave is defined **
	if ((a.eq.-12345.).or.(f.eq.-12345.)) then 
	   print*,'ERROR:- zreadp'
	   print*,'ERROR:   p-wave not defined in header varaibles T0/T1'
	   stop
	endif

c  ** select the pwave from the complete data array, d1 **
	call zselectp(d1,np,npts,delta,a,f,pwave,npwave)

	return
	end

