c-----------------------------------------------------------------------
	subroutine zreadsac(file,lu,np,h1,h2,h3a,h3b,h4,d1,npts)
c-----------------------------------------------------------------------
c
c	read in a sac ascii data file
c	  read in header info (in 4 parts)
c	  read in data part1
c	  (may need to add a data part2 if unevenly spaced data is ever used)
c
c	variables:
c input: file	char*50		file to read in
c	   lu		int			logical unit to read file to
c	   np		int			dimension of d1 array
c output:h1		dble(14,5)		header part1
c	   h2		int(8,5)		header part2
c	   h3a	char*8		header part3a
c	   h3b	char*16		header part3b
c	   h4		char*8 (7,3)	header part4
c	   d1		dble(np)		data points
c	   npts	int			number of data points (read from h2(2,5))
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
	open(lu,file=file,status='old')

c  ** read header info **
	do 1 i=1,14
	   read(lu,1001) h1(i,1),h1(i,2),h1(i,3),h1(i,4),h1(i,5)
ccc	   write(6,1001) h1(i,1),h1(i,2),h1(i,3),h1(i,4),h1(i,5)
1	continue
	do 2 i=1,8
	   read(lu,1002) h2(i,1),h2(i,2),h2(i,3),h2(i,4),h2(i,5)
ccc	   write(6,1002) h2(i,1),h2(i,2),h2(i,3),h2(i,4),h2(i,5)
2	continue
	read(lu,1003) h3a,h3b
ccc	write(6,1003) h3a,h3b
	do 4 i=1,7
	   read(lu,1004) h4(i,1),h4(i,2),h4(i,3)
ccc	   write(6,1004) h4(i,1),h4(i,2),h4(i,3)
4	continue

c  ** read npts from header **
	npts=h2(2,5)
c  ** check npts does not exceed array dimension **
	if (npts.gt.np) then
	   print*,'WARNING:zsacread'
	   print*,'WARNING:  npts.gt.np, data missed out'
	endif
	
c  ** read in data, multiple of 5, then remainder**
	npts_mult5 = 5*int(npts/5)
	do i=1,npts_mult5,5
	   read(lu,1010),d1(i),d1(i+1),d1(i+2),d1(i+3),d1(i+4)
ccc	   write(6,1010),d1(i),d1(i+1),d1(i+2),d1(i+3),d1(i+4)
	enddo
	npts_rem = mod(npts,5)
	if (npts_rem.eq.0) then
c	  do nothing, all data read in already
	else if (npts_rem.eq.1) then
	   read(lu,1011),d1(npts_mult5+1)
	else if (npts_rem.eq.2) then
	   read(lu,1012),d1(npts_mult5+1),d1(npts_mult5+2)
	else if (npts_rem.eq.3) then
	   read(lu,1013),d1(npts_mult5+1),d1(npts_mult5+2),
     >		d1(npts_mult5+3)
	else if (npts_rem.eq.4) then
	   read(lu,1014),d1(npts_mult5+1),d1(npts_mult5+2),
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

