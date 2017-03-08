c-----------------------------------------------------------------------
	subroutine zwritesac(file,lu,np,h1,h2,h3a,h3b,h4,d1,npts)
c-----------------------------------------------------------------------
c
c	write a sac ascii data file
c	  write header info (in 4 parts)
c	  write data part1
c	  (may need to add a data part2 if unevenly spaced data is ever used)
c
c	variables:
c input: file	char*50		file to write output to
c	   lu		int			logical unit to read file to
c	   np		int			dimension of d1 array
c	   h1		dble(14,5)		header part1
c	   h2		int(8,5)		header part2
c	   h3a	char*8		header part3a
c	   h3b	char*16		header part3b
c	   h4		char*8 (7,3)	header part4
c	   d1		dble(np)		data points
c	   npts	int			number of data points (check with h2(2,5))
c
c-----------------------------------------------------------------------
c  modifications:
c	11-07-01	N. Teanby	Original code
c-----------------------------------------------------------------------
	
	implicit none
	integer lu,np
	double precision h1(14,5)
	integer h2(8,5)
	character h3a*8, h3b*16
	character*8 h4(7,3)
	double precision d1(np)
	integer i,npts,npts_mult5,npts_rem
	character*50 file

c  ** open data file **	
	open(lu,file=file,status='unknown')

c  ** write header info **
	do 1 i=1,14
	   write(lu,1001) h1(i,1),h1(i,2),h1(i,3),h1(i,4),h1(i,5)
1	continue
	do 2 i=1,8
	   write(lu,1002) h2(i,1),h2(i,2),h2(i,3),h2(i,4),h2(i,5)
2	continue
	write(lu,1003) h3a,h3b
	do 4 i=1,7
	   write(lu,1004) h4(i,1),h4(i,2),h4(i,3)
4	continue

c  ** check npts with npts from header **
	if (npts.ne.h2(2,5)) then
   	   print*,'WARNING:zsacwrite'
   	   print*,'WARNING:  npts supplied .ne. npts from header'
	endif
c  ** check npts does not exceed array dimension **
	if (npts.gt.np) then
	   print*,'WARNING:zsacwrite'
	   print*,'WARNING:  npts.gt.np, data missed out'
	endif

c  ** remove v small numbers or SAC complains !!! **
	do 50 i=1,npts
	   if (abs(d1(i)).le.1.e-14) then
	      d1(i)=0.
	   endif
50	continue

c  ** write data **
	npts_mult5 = 5*int(npts/5)
	do i=1,npts_mult5,5
	   write(lu,1010),d1(i),d1(i+1),d1(i+2),d1(i+3),d1(i+4)
	enddo
	npts_rem = mod(npts,5)
	if (npts_rem.eq.0) then
c	  do nothing, all data read in already
	else if (npts_rem.eq.1) then
	   write(lu,1011),d1(npts_mult5+1)
	else if (npts_rem.eq.2) then
	   write(lu,1012),d1(npts_mult5+1),d1(npts_mult5+2)
	else if (npts_rem.eq.3) then
	   write(lu,1013),d1(npts_mult5+1),d1(npts_mult5+2),
     >		d1(npts_mult5+3)
	else if (npts_rem.eq.4) then
	   write(lu,1014),d1(npts_mult5+1),d1(npts_mult5+2),
     >		d1(npts_mult5+3),d1(npts_mult5+4)
	endif	

	close(lu)
	return

c **	format statements **
1001	format(5g15.7)
1002	format(5i10)
1003	format(a8,a16)
1004	format(3a8)
1010	format(5g15.7)
1011	format(1g15.7)
1012	format(2g15.7)
1013	format(3g15.7)
1014	format(4g15.7)

	end

